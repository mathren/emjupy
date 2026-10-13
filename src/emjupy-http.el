;;; emjupy-http.el --- HTTP transport and server URL parsing for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The REST half of the Jupyter transport, plus the URL parsing shared with
;; the WebSocket layer.  Handles per-server XSRF cookies and base paths, so
;; a server behind an ssh tunnel or a reverse proxy works unchanged.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'emjupy-core)

;;;; Addresses

(defun emjupy--server-parts (&optional server)
  "Return a plist describing SERVER's base-url.
Keys are :scheme, :ws-scheme, :host, :port and :path.  Accepts a bare
port (\"8888\"), host:port, or an http(s) URL, with an optional base
path that the WebSocket URL keeps too, as for a JupyterHub server at
localhost:8888/user/alice."
  (let* ((server (or server emjupy--current-server))
         (raw (emjupy-server-base-url server))
         (secure (string-prefix-p "https" raw))
         (stripped (replace-regexp-in-string "\\`https?://" "" raw))
         (slash (string-match-p "/" stripped))
         (hostport (if slash (substring stripped 0 slash) stripped))
         (path (if slash
                   (string-trim-right (substring stripped slash) "/+")
                 ""))
         (parts (split-string hostport ":"))
         (host (if (string-empty-p (or (car parts) "")) "localhost"
                 (car parts)))
         (port (or (cadr parts) (if secure "443" "80"))))
    (list :scheme (if secure "https" "http")
          :ws-scheme (if secure "wss" "ws")
          :host host :port port :path path)))

(defun emjupy--server-host-port ()
  "Return (HOST . PORT) parsed from the current server's base-url."
  (let ((p (emjupy--server-parts)))
    (cons (plist-get p :host) (plist-get p :port))))

;;;; The XSRF cookie

(defconst emjupy--xsrf-endpoints '("/login" "/tree" "/lab" "/")
  "Paths that hand out an `_xsrf' cookie, tried in order.
Only HTML pages set it, not /api.  /login first: on a server wanting
credentials, /tree only redirects there, without a cookie.")

(defun emjupy--harvest-xsrf (server)
  "Fetch an `_xsrf' cookie for SERVER and remember it.  Return it, or nil.
Raw, not through `emjupy--http-request': these answer with HTML."
  (let* ((base-url (emjupy-server-base-url server))
         (token (emjupy-server-token server))
         (prefix (if (string-prefix-p "http" base-url) "" "http://"))
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
                                    (concat prefix base-url path) t nil 5)))
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
   ((= status 204) nil)
   ;; An empty success that is not 204 is what a forwarded port gives with
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

(defun emjupy--http-request (method server path &optional body
                                    callback retrying)
  "Send a request to SERVER and return the parsed JSON response.
METHOD is an HTTP method string, PATH the API path, BODY an optional
request body.  CALLBACK, if given, makes the request asynchronous.
Reports the HTTP status on failure, and carries SERVER's XSRF cookie."
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
         (token (emjupy-server-token server))
         (url-request-extra-headers
          (emjupy--http-bytes-alist
           (append `(("Content-Type" . "application/json"))
                   (when (and token (not (string-empty-p token)))
                     `(("Authorization" . ,(format "token %s" token))))
                   ;; XSRF tokens added from THIS server's cookie: another
                   ;; server's earns a 403.
                   (when (emjupy-server-xsrf server)
                     `(("X-XSRFToken" . ,(emjupy-server-xsrf server))
                       ("Cookie" . ,(format "_xsrf=%s"
                                            (emjupy-server-xsrf server))))))))
         (base-url (emjupy-server-base-url server))
         ;; Tornado reads an `_xsrf' argument or the X-XSRFToken header; both
         ;; are sent.
         (xsrf-arg (if (and (not (string= method "GET"))
                            (emjupy-server-xsrf server))
                       (format "%s_xsrf=%s"
                               (if (string-match-p "\\?" path) "&" "?")
                               (url-hexify-string (emjupy-server-xsrf server)))
                     ""))
         (cache-buster (if (string= method "GET")
                           (format (if (string-match-p "\\?" path) "&_t=%s"
                                     "?_t=%s")
                                   (float-time))
                         ""))
         (full-url (emjupy--http-bytes
                    (concat (if (string-prefix-p "http" base-url) "" "http://")
                            base-url path xsrf-arg cache-buster))))

    (if callback
        (url-retrieve full-url callback)
      (let ((buffer (url-retrieve-synchronously full-url t nil 5)))
        (if (not buffer)
            (signal 'emjupy-http-error
                    (list "Network error: Could not reach"
                          (emjupy--redact-url full-url)))
          (let* ((response (emjupy--http-read-response buffer server))
                 (status (or (car response) 200))
                 (json-str (cdr response)))
            (cond
             ;; A missing or rotated cookie is recoverable: fetch a fresh
             ;; one and try once more, rather than making the user
             ;; reconnect over a cookie that has simply aged out.
             ((and (= status 403)
                   (not retrying)
                   (let ((case-fold-search t))
                     (string-match-p "xsrf" json-str)))
              (setf (emjupy-server-xsrf server) nil)
              (if (emjupy--harvest-xsrf server)
                  (emjupy--http-request method server path body callback t)
                (signal 'emjupy-http-status
                        (list (format "[Jupyter HTTP %d] %s" status method)
                              json-str))))
             (t (emjupy--http-interpret server method path
                                        status json-str)))))))))

;;;; WebSocket authentication

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
