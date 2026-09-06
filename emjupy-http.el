;;; emjupy-http.el --- HTTP transport and server URL parsing for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
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

Keys are :scheme, :ws-scheme, :host, :port and :path. Accepts a bare
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

(defconst emjupy--xsrf-endpoints '("/tree" "/lab" "/")
  "Paths that hand out an `_xsrf' cookie, tried in order.

Deliberately NOT \"/api\": the REST endpoints do not set the cookie at
all.  emjupy used to prime itself with a GET of /api and so never had
one -- harmless against a token-authenticated server, which skips the
XSRF check entirely, but fatal against a token-less one, where every
POST came back

  [Jupyter HTTP 403] POST: {\"message\": \"'_xsrf' argument missing from POST\"}

Only the HTML pages issue it.")

(defun emjupy--harvest-xsrf (server)
  "Fetch an `_xsrf' cookie for SERVER and remember it.  Returns it, or nil.

Deliberately raw rather than going through `emjupy--http-request': these
endpoints answer with HTML, which the JSON parse there would reject.
Only the headers matter."
  (let* ((base-url (emjupy-server-base-url server))
         (token (emjupy-server-token server))
         (prefix (if (string-prefix-p "http" base-url) "" "http://"))
         (found nil))
    (cl-loop for path in emjupy--xsrf-endpoints
             until found
             do (ignore-errors
                  (let* ((url-request-method "GET")
                         (url-request-data nil)
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
                          (setq found (match-string 1))))
                      (kill-buffer buffer)))))
    (when found (setf (emjupy-server-xsrf server) found))
    found))

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
          (append `(("Content-Type" . "application/json"))
                  (when (and token (not (string-empty-p token)))
                    `(("Authorization" . ,(format "token %s" token))))
                  ;; Automatically inject XSRF tokens to bypass Jupyter 403 CSRF blocks.
                  ;; Read from THIS server: a cookie issued by another server
                  ;; (a second tunnel, say) would just earn a 403.
                  (when (emjupy-server-xsrf server)
                    `(("X-XSRFToken" . ,(emjupy-server-xsrf server))
                      ("Cookie" . ,(format "_xsrf=%s" (emjupy-server-xsrf server)))))))
         (base-url (emjupy-server-base-url server))
         (cache-buster (if (string= method "GET")
                           (format (if (string-match-p "\\?" path) "&_t=%s" "?_t=%s")
                                   (float-time))
                         ""))
         (full-url (concat (if (string-prefix-p "http" base-url) "" "http://")
                           base-url path cache-buster)))

    (if callback
        (url-retrieve full-url callback)
      (let ((buffer (url-retrieve-synchronously full-url t nil 5)))
        (if (not buffer)
            (error "Network error: Could not reach %s" full-url)
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((status 200))
              (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                (setq status (string-to-number (match-string 1))))

              ;; Harvest XSRF cookie from response, onto THIS server
              (goto-char (point-min))
              (when (re-search-forward "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
                (setf (emjupy-server-xsrf server) (match-string 1)))

              (goto-char (point-min))
              (re-search-forward "\r?\n\r?\n" nil t)
              (let ((json-str (buffer-substring-no-properties (point) (point-max))))
                (kill-buffer buffer)
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
                    (error "[Jupyter HTTP %d] %s: %s" status method json-str)))
                 ((>= status 400)
                  (error "[Jupyter HTTP %d] %s: %s" status method json-str))
                 (t
                  (condition-case err
                      (json-parse-string json-str :object-type 'hash-table :array-type 'array)
                    (error (error "JSON Parse Error on %s: %s" path err)))))))))))))

(provide 'emjupy-http)
;;; emjupy-http.el ends here
