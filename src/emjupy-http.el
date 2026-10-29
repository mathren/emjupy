;;; emjupy-http.el --- HTTP transport, and what WebSockets share with it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The REST half of the Jupyter transport, and what the WebSockets share
;; with it: a server's address, and its token, sent in a header.  Handles
;; per-server XSRF cookies and base paths, so a server behind an ssh tunnel
;; or a reverse proxy works unchanged.  A server's address is parsed once,
;; in emjupy-core.el.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'emjupy-core)

;;;; Addresses

(defun emjupy--server-parts (server)
  "Return a plist describing SERVER's address, from its parsed parts.
Keys are :scheme, :ws-scheme, :host, :port and :path; the address was
parsed once, when the server was made (see `emjupy--parse-base-url')."
  (let ((scheme (emjupy-server-scheme server)))
    (list :scheme scheme
          :ws-scheme (if (equal scheme "https") "wss" "ws")
          :host (emjupy-server-host server) :port (emjupy-server-port server)
          :path (emjupy-server-path server))))

;;;; The XSRF cookie

(defconst emjupy--xsrf-endpoints '("/login" "/tree" "/lab" "/")
  "Paths that hand out an `_xsrf' cookie, tried in order.
Only HTML pages set it, not /api.  /login first: on a server wanting
credentials, /tree only redirects there, without a cookie.")

(defun emjupy--server-root-url (server)
  "Return SERVER's base-url as a URL, with http:// unless it has a scheme.
Only http:// and https:// are schemes: a host named, say, httpfoo is not."
  (let ((base-url (emjupy-server-base-url server)))
    (if (string-match-p "\\`https?://" base-url)
        base-url
      (concat "http://" base-url))))

(defun emjupy--harvest-xsrf (server)
  "Fetch an `_xsrf' cookie for SERVER and remember it.  Return it, or nil.
Raw, not through `emjupy--http-request': these answer with HTML."
  (let* ((root (emjupy--server-root-url server))
         (token (emjupy-server-token server))
         (found nil))
    (cl-loop for path in emjupy--xsrf-endpoints
             until found
             do (condition-case nil
                    ;; An endpoint that is not there, or a server that has
                    ;; gone, fails to connect -- `file-error' -- and the next
                    ;; is tried.  Anything else is a fault, and is not hidden.
                    (let* ((url-request-method "GET")
                           (url-request-data nil)
                           ;; No redirects: /tree redirects to /login, and the
                           ;; cookie is on neither the redirect nor, reliably,
                           ;; what url.el keeps.  Each endpoint is asked on its
                           ;; own.
                           (url-max-redirections 0)
                           (url-cookie-storage nil)
                           (url-cookie-secure-storage nil)
                           (url-request-extra-headers
                            (when (and token (not (string-empty-p token)))
                              `(("Authorization" .
                                 ,(format "token %s" token)))))
                           (buffer (url-retrieve-synchronously
                                    (concat root path) t nil 5)))
                      (when buffer
                        (with-current-buffer buffer
                          (goto-char (point-min))
                          (when (re-search-forward
                                 "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
                            (setq found
                                  (emjupy--http-bytes
                                   (match-string-no-properties 1)))))
                        (kill-buffer buffer)))
                  (file-error nil)))
    (when found (setf (emjupy-server-xsrf server) found))
    found))

;;;; Errors

(define-error 'emjupy-http-error "Jupyter server request failed" 'file-error)
(define-error 'emjupy-http-status "Jupyter server refused the request"
              'emjupy-http-error)
(define-error 'emjupy-http-xsrf "Jupyter server rejected the XSRF cookie"
  'emjupy-http-status)

;; A type of its own, so a caller catches the failure it expects -- a missing
;; file, an unreachable server -- and lets the rest through.  Under
;; `file-error', the family url.el and TRAMP signal for an unreachable resource,
;; whose messages read as plain text.

(defun emjupy--http-status-of (err)
  "Return the HTTP status carried by ERR, an `emjupy-http-status', or nil."
  (let ((msg (cadr err)))
    (and (stringp msg)
         (string-match "\\`\\[Jupyter HTTP \\([0-9]+\\)\\]" msg)
         (string-to-number (match-string 1 msg)))))

;;;; Requests and responses

(defun emjupy--contents-path (path)
  "Return the Contents API address of PATH, each segment percent-encoded.
In a URL \"#\" and \"?\" mean something, and a space is not allowed.
Separators stay; \"..\" is sent as written, for the server to judge."
  (concat "/api/contents/"
          (mapconcat #'url-hexify-string (split-string (or path "") "/") "/")))

(defun emjupy--http-exists-p (server path)
  "Return non-nil if PATH exists in SERVER's contents.

Nil for a 404, which is the answer to the question; any other failure
is not an answer, and is signalled."
  (condition-case err
      (progn (emjupy--http-request "GET" server (emjupy--contents-path path)) t)
    (emjupy-http-status
     (if (eql (emjupy--http-status-of err) 404) nil (signal (car err)
                                                            (cdr err))))))

(defun emjupy--http-interpret (server method path status body)
  "Turn a reply into a value, or signal.
SERVER, METHOD and PATH identify the request; STATUS and BODY are what
came back.  Apart from the request, so it is tested without a server."
  (cond
   ((>= status 400)
    (signal 'emjupy-http-status
            (list (format "[Jupyter HTTP %d] %s" status method) body)))
   ;; 204 No Content: there is nothing to say, and saying nothing is the
   ;; answer.  Interrupting a kernel replies this way, and parsing it as
   ;; JSON reported a syntax error for a request that had succeeded.
   ;; Done, with nothing to say: 204, or 201 with no body, as trusting a
   ;; notebook answers.
   ((or (= status 204)
        (and (= status 201) (string-empty-p (string-trim (or body "")))))
    nil)
   ;; Any other empty success is what a forwarded port gives with
   ;; nothing listening: blame the tunnel, not the JSON.
   ((string-empty-p (string-trim (or body "")))
    (signal 'emjupy-http-error
            (list (format "%s returned nothing for %s"
                          (emjupy--server-label server) path)
                  "is it reachable?")))
   (t
    (condition-case err
        (json-parse-string body :object-type 'hash-table :array-type 'array)
      (json-error
       (signal 'emjupy-http-error
               (list (format "JSON Parse Error on %s" path)
                     (error-message-string err))))))))

(defconst emjupy--secret-query-keys '("token" "_xsrf" "password")
  "Query parameters whose values must never be shown.")

(defun emjupy--redact-url (url)
  "Return URL with the value of any secret query parameter hidden.
URLs reach error messages and bug reports: the server is what helps
there, not its token."
  (if (not (stringp url))
      url
    (let ((out url))
      (dolist (key emjupy--secret-query-keys out)
        (setq out (replace-regexp-in-string
                   (concat "\\([?&]" (regexp-quote key) "=\\)[^&]*")
                   "\\1<redacted>" out t))))))

(defun emjupy--http-bytes (string)
  "Return STRING as UTF-8 bytes: a unibyte string, unchanged if ASCII.
url.el refuses a request that is multibyte, and an ASCII value in a
multibyte string makes it so once the body has anything else."
  (if (multibyte-string-p string)
      (encode-coding-string string 'utf-8)
    string))

(defun emjupy--http-bytes-alist (alist)
  "Return the header ALIST with every name and value as UTF-8 bytes."
  (mapcar (lambda (h) (cons (emjupy--http-bytes (car h))
                            (emjupy--http-bytes (cdr h))))
          alist))

(defun emjupy--http-read-response (buffer server)
  "Read BUFFER, an HTTP response, as a cons of its status and its body.
The status is nil if BUFFER holds no response at all.  Remember the XSRF
cookie it sets, onto SERVER, and kill BUFFER.  Shared by requests that
wait for their answer and those that do not, so both read it alike."
  (with-current-buffer buffer
    (goto-char (point-min))
    (let ((status nil))
      (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
        (setq status (string-to-number (match-string 1))))
      ;; Harvest XSRF cookie from response, onto THIS server
      (goto-char (point-min))
      (when (re-search-forward "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
        (setf (emjupy-server-xsrf server) (match-string 1)))
      (goto-char (point-min))
      (re-search-forward "\r?\n\r?\n" nil t)
      (prog1 (cons status (buffer-substring-no-properties (point) (point-max)))
        (kill-buffer buffer)))))

(defun emjupy--http-request-async (method server path body on-done on-fail)
  "Send a request to SERVER without waiting for it.
METHOD, PATH and BODY are as for `emjupy--http-request'.  ON-DONE is
called with the parsed answer, ON-FAIL with the error.  For a remote
server, the difference between Emacs waiting and not."
  (emjupy--http-request
   method server path body
   (lambda (_status)
     (let ((buffer (current-buffer)) (answer nil) (failure nil))
       (condition-case err
           (let ((response (emjupy--http-read-response buffer server)))
             ;; An error status still comes with a response: only none at
             ;; all is the network failing.
             (if (not (car response))
                 (signal 'emjupy-http-error
                         (list "Network error: no answer from"
                               (emjupy-server-base-url server)))
               (setq answer (emjupy--http-interpret server method path
                                                    (car response)
                                                    (cdr response)))))
         ;; What a request can fail with -- emjupy-http-error is a file
         ;; error -- handed on; ON-DONE runs outside, its errors its own.
         (file-error (setq failure err)))
       (if failure (funcall on-fail failure) (funcall on-done answer))))))

(defun emjupy--http-headers (server)
  "Return the headers a request to SERVER carries, as bytes."
  (let ((token (emjupy-server-token server))
        (xsrf (emjupy-server-xsrf server)))
    (emjupy--http-bytes-alist
     (append `(("Content-Type" . "application/json"))
             (when (and token (not (string-empty-p token)))
               `(("Authorization" . ,(format "token %s" token))))
             ;; XSRF tokens added from THIS server's cookie: another
             ;; server's earns a 403.
             (when xsrf
               `(("X-XSRFToken" . ,xsrf)
                 ("Cookie" . ,(format "_xsrf=%s" xsrf))))))))

(defun emjupy--http-url (server method path)
  "Return the address of a METHOD request for PATH on SERVER, as bytes.
A write carries the XSRF token as an argument too: Tornado reads an
`_xsrf' argument or the X-XSRFToken header.  A GET carries the time, so
no cache answers it."
  (let* ((separator (if (string-match-p "\\?" path) "&" "?"))
         (xsrf-arg (if (and (not (string= method "GET"))
                            (emjupy-server-xsrf server))
                       (format "%s_xsrf=%s" separator
                               (url-hexify-string (emjupy-server-xsrf server)))
                     ""))
         (cache-buster (if (string= method "GET")
                           (format "%s_t=%s" separator (float-time))
                         "")))
    (emjupy--http-bytes
     (concat (emjupy--server-root-url server) path xsrf-arg cache-buster))))

(cl-defun emjupy--http-send (method server path body callback &key final)
  "Send one METHOD request for PATH, with BODY, to SERVER.
Asynchronous if CALLBACK is given.  A request refused for its XSRF
cookie signals `emjupy-http-xsrf', unless FINAL: then it is reported as
any refusal is."
  ;; A write needs an XSRF cookie unless the token authenticates us, and the
  ;; REST endpoints never hand one out -- so go and get one first.
  (unless (or (string= method "GET")
              (emjupy-server-xsrf server)
              (let ((tok (emjupy-server-token server)))
                (and tok (not (string-empty-p tok)))))
    (emjupy--harvest-xsrf server))
  (let* ((url-request-method method)
         (url-request-data (when body (encode-coding-string body 'utf-8)))
         (url-automatic-caching nil)
         ;; url.el's cookie jar is emptied for the request: its own XSRF cookie
         ;; would go beside our X-XSRFToken, two values, and the server would
         ;; refuse the pair.
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (url-request-extra-headers (emjupy--http-headers server))
         (full-url (emjupy--http-url server method path)))
    (if callback
        (url-retrieve full-url callback)
      (let ((buffer (url-retrieve-synchronously full-url t nil 5)))
        (unless buffer
          (signal 'emjupy-http-error
                  (list "Network error: Could not reach"
                        (emjupy--redact-url full-url))))
        (let* ((response (emjupy--http-read-response buffer server))
               (status (or (car response) 200))
               (json-str (cdr response)))
          (if (and (= status 403)
                   (not final)
                   (let ((case-fold-search t))
                     (string-match-p "xsrf" json-str)))
              (signal 'emjupy-http-xsrf
                      (list (format "[Jupyter HTTP %d] %s" status method)
                            json-str))
            (emjupy--http-interpret server method path status json-str)))))))

(defun emjupy--http-request (method server path &optional body callback)
  "Send a request to SERVER and return the parsed JSON response.
METHOD is an HTTP method string, PATH the API path, BODY an optional
request body.  CALLBACK, if given, makes the request asynchronous.
Reports the HTTP status on failure, and carries SERVER's XSRF cookie."
  (condition-case err
      (emjupy--http-send method server path body callback)
    ;; A missing or rotated cookie is recoverable: fetch a fresh one and try
    ;; once more, rather than making the user reconnect over a cookie that
    ;; has simply aged out.
    (emjupy-http-xsrf
     (setf (emjupy-server-xsrf server) nil)
     (if (emjupy--harvest-xsrf server)
         (emjupy--http-send method server path body callback :final t)
       (signal 'emjupy-http-status (cdr err))))))

;;;; WebSocket authentication

(defvar emjupy--token-in-ws-address nil
  "Servers, by address, whose WebSocket refused the token in a header alone.
For them it goes in a WebSocket's address too; elsewhere not, as an
address is what proxies and servers log.")

(defun emjupy--ws-token-param (server separator)
  "Return the token part of a WebSocket address to SERVER, or \"\".
SEPARATOR, \"?\" or \"&\", starts it.  Empty but for a server that
refused the token in a header alone."
  (let ((token (emjupy-server-token server)))
    (if (and token (not (string-empty-p token))
             (member (emjupy-server-base-url server)
                     emjupy--token-in-ws-address))
        (concat separator "token=" (url-hexify-string token))
      "")))

(defun emjupy--note-ws-refusal (server err)
  "Note SERVER's refusal of a WebSocket token in a header alone, from ERR.
Return non-nil if noted now: its next WebSocket has the token in its
address too."
  (let ((token (emjupy-server-token server)))
    (when (and (eq (car-safe err) 'websocket-received-error-http-response)
               (memq (cadr err) '(401 403))
               token (not (string-empty-p token))
               (not (member (emjupy-server-base-url server)
                            emjupy--token-in-ws-address)))
      (push (emjupy-server-base-url server) emjupy--token-in-ws-address)
      t)))

(defun emjupy--websocket-auth-headers (server)
  "Return the headers a WebSocket to SERVER should carry.
The token in a header, which is accepted where a query string may not
be, and the XSRF cookie, which some servers check on upgrade."
  (let ((token (or (emjupy-server-token server) ""))
        (xsrf (emjupy-server-xsrf server)))
    (append
     (unless (string-empty-p token)
       (list (cons "Authorization" (format "token %s" token))))
     (when xsrf
       (list (cons "Cookie" (format "_xsrf=%s" xsrf)))))))

(provide 'emjupy-http)
;;; emjupy-http.el ends here
