;;; emjupy-http.el --- HTTP transport and server URL parsing for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The REST half of the Jupyter transport, plus the URL parsing shared with
;; the WebSocket layer.  Handles per-server XSRF cookies and base paths, so
;; a server behind an ssh tunnel or a reverse proxy works unchanged.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'emjupy-core)

(defun emjupy--server-parts (&optional server)
  "Return a plist describing SERVER's base-url.

Keys are :scheme, :ws-scheme, :host, :port and :path.  Accepts a bare
port (\"8888\"), a host:port pair, a full http(s) URL, and an optional
trailing base path -- the last of which matters for servers reached
through an SSH tunnel into a proxied setup (e.g. a JupyterHub
single-user server at localhost:8888/user/alice), where the prefix has
to survive into the WebSocket URL as well as the REST calls."
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
         (host (if (string-empty-p (or (car parts) "")) "localhost" (car parts)))
         (port (or (cadr parts) (if secure "443" "80"))))
    (list :scheme (if secure "https" "http")
          :ws-scheme (if secure "wss" "ws")
          :host host :port port :path path)))

(defun emjupy--server-host-port ()
  "Return (HOST . PORT) parsed from the current server's base-url."
  (let ((p (emjupy--server-parts)))
    (cons (plist-get p :host) (plist-get p :port))))

(defconst emjupy--xsrf-endpoints '("/login" "/tree" "/lab" "/")
  "Paths that hand out an `_xsrf' cookie, tried in order.

Deliberately NOT \"/api\": the REST endpoints do not set the cookie at
all.  emjupy used to prime itself with a GET of /api and so never had
one -- harmless against a token-authenticated server, which skips the
XSRF check entirely, but fatal against a token-less one, where every
POST came back

  [Jupyter HTTP 403] POST: {\"message\": \"'_xsrf' argument missing from POST\"}

Only the HTML pages issue it, and /login comes first because it is the
one that always answers directly: on a server that wants credentials
/tree merely redirects there, and the redirect itself carries no
cookie.")

(defun emjupy--harvest-xsrf (server)
  "Fetch an `_xsrf' cookie for SERVER and remember it.  Return it, or nil.

Deliberately raw rather than going through `emjupy--http-request': these
endpoints answer with HTML, which the JSON parse there would reject.
Only the headers matter."
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
                         ;; Do not chase redirects: /tree answers 302 to
                         ;; /login and the cookie is on neither the 302 nor,
                         ;; reliably, whatever url.el leaves in the buffer
                         ;; afterwards.  Each endpoint is asked on its own.
                         (url-max-redirections 0)
                         (url-cookie-storage nil)
                         (url-cookie-secure-storage nil)
                         (url-request-extra-headers
                          (when (and token (not (string-empty-p token)))
                            `(("Authorization" . ,(format "token %s" token)))))
                         (buffer (url-retrieve-synchronously
                                  (concat prefix base-url path) t nil 5)))
                    (when buffer
                      (with-current-buffer buffer
                        (goto-char (point-min))
                        (when (re-search-forward
                               "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
                          (setq found (emjupy--http-bytes (match-string-no-properties 1)))))
                      (kill-buffer buffer)))
                (file-error nil)))
    (when found (setf (emjupy-server-xsrf server) found))
    found))

(define-error 'emjupy-http-error "Jupyter server request failed" 'file-error)
(define-error 'emjupy-http-status "Jupyter server refused the request" 'emjupy-http-error)

;; Why a type of its own, and why under `file-error'.  Every failure used to
;; be a bare `error', so a caller could not tell a missing file from a dead
;; server from a bug in emjupy, and the only way to tolerate the first two
;; was to swallow all three.  With a type, a caller catches exactly the
;; failure it expects and lets the rest through.  `file-error' is the
;; family Emacs itself uses for a resource that cannot be reached -- url.el
;; and TRAMP both signal it -- and its messages read as plain text, where
;; any other custom error prints its strings in quotes.

(defun emjupy--http-status-of (err)
  "Return the HTTP status carried by ERR, an `emjupy-http-status\=', or nil."
  (let ((msg (cadr err)))
    (and (stringp msg)
         (string-match "\\`\\[Jupyter HTTP \\([0-9]+\\)\\]" msg)
         (string-to-number (match-string 1 msg)))))

(defun emjupy--contents-path (path)
  "Return the Contents API address of PATH, each segment percent-encoded.

PATH is relative to what the server serves, and goes into a URL, where
some characters mean something: \"#\" starts a fragment and \"?\" a query,
so a notebook called a#b.ipynb was asked for as \"a\" -- and saved there.
A space is not allowed at all.  The separators stay as they are; \"..\"
is sent as written, for the server to judge."
  (concat "/api/contents/"
          (mapconcat #'url-hexify-string (split-string (or path "") "/") "/")))

(defun emjupy--http-exists-p (server path)
  "Return non-nil if PATH exists in SERVER\='s contents.

Nil for a 404, which is the answer to the question; any other failure
is not an answer, and is signalled."
  (condition-case err
      (progn (emjupy--http-request "GET" server (emjupy--contents-path path)) t)
    (emjupy-http-status
     (if (eql (emjupy--http-status-of err) 404) nil (signal (car err) (cdr err))))))

(defun emjupy--http-interpret (server method path status body)
  "Turn a reply into a value, or signal.

SERVER, METHOD and PATH identify the request; STATUS and BODY are what
came back.  Separate from the request itself so that the decisions --
which of these is success, and what each failure should say -- can be
tested without a server."
  (cond
   ((>= status 400)
    (signal 'emjupy-http-status
            (list (format "[Jupyter HTTP %d] %s" status method) body)))
   ;; 204 No Content: there is nothing to say, and saying nothing is the
   ;; answer.  Interrupting a kernel replies this way, and parsing it as
   ;; JSON reported a syntax error for a request that had succeeded.
   ((= status 204) nil)
   ;; A success with an empty body that is not 204 is not an answer.  It is
   ;; what a forwarded port gives when nothing is listening at the far end,
   ;; and calling it malformed JSON points at the parser rather than at the
   ;; tunnel.
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

A URL carries the token and the XSRF cookie, and URLs end up in error
messages, in diagnostics, and from there in bug reports and pasted
terminal output.  What is useful in all of those is which server was
being talked to, not the credential that authenticated it."
  (if (not (stringp url))
      url
    (let ((out url))
      (dolist (key emjupy--secret-query-keys out)
        (setq out (replace-regexp-in-string
                   (concat "\\([?&]" (regexp-quote key) "=\\)[^&]*")
                   "\\1<redacted>" out t))))))

(defun emjupy--http-bytes (string)
  "Return STRING as UTF-8 bytes: a unibyte string, unchanged if ASCII.

url.el joins the request line, the headers and the body into one string
and refuses it if the result is multibyte.  The body is encoded to bytes
already, but a URL or header value that is merely MULTIBYTE -- plain
ASCII held in a multibyte string, as `match-string\=' returns from a
response buffer and the minibuffer returns for a typed token -- turns
the whole request multibyte as soon as the body holds anything outside
ASCII.  Any output with such a character then made the notebook
impossible to save: \"Multibyte text in HTTP request\"."
  (if (multibyte-string-p string)
      (encode-coding-string string 'utf-8)
    string))

(defun emjupy--http-bytes-alist (alist)
  "Return the header ALIST with every name and value as UTF-8 bytes."
  (mapcar (lambda (h) (cons (emjupy--http-bytes (car h)) (emjupy--http-bytes (cdr h))))
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
METHOD, PATH and BODY are as for `emjupy--http-request\='.  ON-DONE is
called with the parsed answer, ON-FAIL with the error -- a refusal or a
network failure.  For a remote server, where each request costs a round
trip, this is the difference between Emacs waiting and not."
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
                         (list "Network error: no answer from" (emjupy-server-base-url server)))
               (setq answer (emjupy--http-interpret server method path
                                                    (car response) (cdr response)))))
         ;; What a request can fail with -- emjupy-http-error is a file
         ;; error -- handed on; ON-DONE runs outside, its errors its own.
         (file-error (setq failure err)))
       (if failure (funcall on-fail failure) (funcall on-done answer))))))

(defun emjupy--http-request (method server path &optional body callback retrying)
  "Send a request to SERVER and return the parsed JSON response.
METHOD is an HTTP method string, PATH the API path, BODY an optional
request body.  CALLBACK, if given, makes the request asynchronous.
Reports the exact HTTP status on failure and carries SERVER's own XSRF
cookie."
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
         ;; url.el keeps its own cookie jar and adds a Cookie header from it.
         ;; With one of its own in there, the server saw that cookie next to
         ;; our X-XSRFToken -- two different values -- and answered "XSRF
         ;; cookie does not match POST argument". Emptying the jar for the
         ;; duration leaves our header the only one, so the two always agree.
         ;; This is why the failure depended on the user's session: a fresh
         ;; `emacs -Q' has an empty jar and never hits it.
         (url-cookie-storage nil)
         (url-cookie-secure-storage nil)
         (token (emjupy-server-token server))
         (url-request-extra-headers
          (emjupy--http-bytes-alist
           (append `(("Content-Type" . "application/json"))
                  (when (and token (not (string-empty-p token)))
                    `(("Authorization" . ,(format "token %s" token))))
                  ;; Automatically inject XSRF tokens to bypass Jupyter 403 CSRF blocks.
                  ;; Read from THIS server: a cookie issued by another server
                  ;; (a second tunnel, say) would just earn a 403.
                  (when (emjupy-server-xsrf server)
                    `(("X-XSRFToken" . ,(emjupy-server-xsrf server))
                      ("Cookie" . ,(format "_xsrf=%s" (emjupy-server-xsrf server))))))))
         (base-url (emjupy-server-base-url server))
         ;; Tornado's check reads an `_xsrf' ARGUMENT or the X-XSRFToken
         ;; header -- "'_xsrf' argument missing from POST" is what it says
         ;; when it finds neither.  Sending both costs nothing and makes that
         ;; message impossible whenever we hold a cookie at all.
         (xsrf-arg (if (and (not (string= method "GET"))
                            (emjupy-server-xsrf server))
                       (format "%s_xsrf=%s"
                               (if (string-match-p "\\?" path) "&" "?")
                               (url-hexify-string (emjupy-server-xsrf server)))
                     ""))
         (cache-buster (if (string= method "GET")
                           (format (if (string-match-p "\\?" path) "&_t=%s" "?_t=%s")
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
                       (let ((case-fold-search t)) (string-match-p "xsrf" json-str)))
                  (setf (emjupy-server-xsrf server) nil)
                  (if (emjupy--harvest-xsrf server)
                      (emjupy--http-request method server path body callback t)
                    (signal 'emjupy-http-status
                            (list (format "[Jupyter HTTP %d] %s" status method)
                                  json-str))))
                 (t (emjupy--http-interpret server method path status json-str)))))))))

(defun emjupy--websocket-auth-headers (server)
  "Return the headers a WebSocket to SERVER should carry.

The same ones the kernel socket sends, and for the same reason: a token
in the query string is not accepted everywhere a token in a header is,
and the XSRF cookie is checked on upgrade requests by some server
versions.  The kernel socket has always sent both; this one sent
neither, which is why a notebook could execute cells over a WebSocket
while the language server on the same host refused to connect."
  (let ((token (or (emjupy-server-token server) ""))
        (xsrf (emjupy-server-xsrf server)))
    (append
     (unless (string-empty-p token)
       (list (cons "Authorization" (format "token %s" token))))
     (when xsrf
       (list (cons "Cookie" (format "_xsrf=%s" xsrf)))))))

(provide 'emjupy-http)
;;; emjupy-http.el ends here
