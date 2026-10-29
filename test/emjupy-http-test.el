;;; emjupy-http-test.el --- Unit tests: emjupy-http.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-http.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-server-parts-handles-tunnel-urls ()
  "A tunnelled server may be given as a bare port, a host:port, a full
https URL, or a proxied base path -- all of which have to survive into
the WebSocket URL, not just the REST calls."
  (cl-flet ((parts (url)
              (emjupy--server-parts (make-emjupy-server :base-url url))))
    (let ((p (parts "localhost:18888")))
      (should (equal (plist-get p :host) "localhost"))
      (should (equal (plist-get p :port) "18888"))
      (should (equal (plist-get p :ws-scheme) "ws"))
      (should (equal (plist-get p :path) "")))
    ;; https must become wss, or the handshake is plaintext against a TLS port
    (let ((p (parts "https://example.org:8443")))
      (should (equal (plist-get p :ws-scheme) "wss"))
      (should (equal (plist-get p :port) "8443")))
    ;; a proxy / JupyterHub prefix must be kept
    (let ((p (parts "localhost:8888/user/alice")))
      (should (equal (plist-get p :host) "localhost"))
      (should (equal (plist-get p :port) "8888"))
      (should (equal (plist-get p :path) "/user/alice")))))

;;;; Login: one port, one kernel

(ert-deftest emjupy-test-login-adopts-single-running-kernel ()
  "A port with exactly one kernel behind it is adopted silently -- no
prompt, and crucially no second kernel spawned next to the one the
user already started on the remote host."
  (let* ((server (make-emjupy-server :base-url "localhost:18888" :token ""))
         (posted nil)
         (k (make-hash-table :test 'equal)))
    (puthash "id" "kern-existing" k)
    (puthash "name" "python3" k)
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (method _server path &rest _)
                 (cond
                  ((and (string= method "POST")
                        (string-match-p "/api/kernels" path))
                   (setq posted t) (make-hash-table :test 'equal))
                  ((string-match-p "/api/kernels" path) (vector k))
                  (t (make-hash-table :test 'equal)))))
              ((symbol-function 'completing-read)
               (lambda (&rest _)
                 (error "must not prompt for a single kernel"))))
      (should (equal (emjupy--bind-server-kernel server) "kern-existing")))
    (should-not posted)
    (should (equal (emjupy-server-kernel-id server) "kern-existing"))))

(ert-deftest emjupy-test-login-spawns-only-when-port-has-none ()
  "An empty server does get a fresh kernel -- otherwise the workflow
dead-ends on a server you just started."
  (let ((server (make-emjupy-server :base-url "localhost:18888" :token ""))
        (spawned (make-hash-table :test 'equal)))
    (puthash "id" "kern-new" spawned)
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (method _server path &rest _)
                 (cond
                  ((and (string= method "POST")
                        (string-match-p "/api/kernels" path)) spawned)
                  ((string-match-p "/api/kernels" path) (vector))
                  (t (make-hash-table :test 'equal))))))
      (should (equal (emjupy--bind-server-kernel server) "kern-new")))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-xsrf-not-harvested-from-api ()
  "The `_xsrf' cookie is fetched from an HTML page, never from /api.

The REST endpoints do not set the cookie at all, so priming with a GET
of /api left emjupy without one -- harmless against a token-
authenticated server, which skips the XSRF check, but fatal against a
token-less one, where every POST came back 403 \"'_xsrf' argument
missing from POST\"."
  (should-not (member "/api" emjupy--xsrf-endpoints))
  (should (member "/tree" emjupy--xsrf-endpoints))
  ;; and the login path no longer primes itself from /api
  (let ((asked nil))
    (cl-letf (((symbol-function 'emjupy--harvest-xsrf)
               (lambda (&rest _) (setq asked t) "cookie")))
      (should (emjupy--harvest-xsrf nil))
      (should asked))))

(ert-deftest emjupy-test-xsrf-fetched-before-a-write ()
  "A write on a token-less server fetches a cookie first; a GET does not
bother, and neither does a server whose token authenticates it."
  (let* ((server (make-emjupy-server :base-url "localhost:9" :token ""))
         (harvests 0))
    (cl-letf (((symbol-function 'emjupy--harvest-xsrf)
               (lambda (s) (setq harvests (1+ harvests))
                 (setf (emjupy-server-xsrf s) "c") "c"))
              ((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _) nil)))
      ;; GET: no cookie needed
      (ignore-errors (emjupy--http-request "GET" server "/api/status"))
      (should (= harvests 0))
      ;; POST with no token and no cookie: fetch one
      (ignore-errors (emjupy--http-request "POST" server "/api/kernels" "{}"))
      (should (= harvests 1))
      ;; already have one: do not fetch again
      (ignore-errors (emjupy--http-request "POST" server "/api/kernels" "{}"))
      (should (= harvests 1)))
    ;; a token authenticates the write, so no cookie is needed at all
    (let ((tokened (make-emjupy-server :base-url "localhost:9" :token "abc"))
          (n 0))
      (cl-letf (((symbol-function 'emjupy--harvest-xsrf)
                 (lambda (&rest _) (setq n (1+ n)) nil))
                ((symbol-function 'url-retrieve-synchronously)
                 (lambda (&rest _) nil)))
        (ignore-errors (emjupy--http-request "POST" tokened "/api/kernels"
                                             "{}"))
        (should (= n 0))))))

(ert-deftest emjupy-test-url-cookie-jar-is-emptied-per-request ()
  "url.el keeps its own cookie jar and adds a Cookie header from it.  With
one of its own in there the server saw that cookie beside our
X-XSRFToken -- two different values -- and answered \"XSRF cookie does
not match POST argument\".  This is why the failure depended on the
session: a fresh `emacs -Q' has an empty jar and never hits it."
  (let* ((server (make-emjupy-server :base-url "localhost:9" :token "abc"))
         (seen 'unset)
         ;; A jar with something in it, or the check below passes whether or
         ;; not the binding is there: in batch the jar is empty anyway, and
         ;; the first version of this test did exactly that.
         (url-cookie-storage '(("localhost" . [stale])))
         (url-cookie-secure-storage '(("localhost" . [stale]))))
    (should url-cookie-storage)
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (&rest _)
                 (setq seen (list url-cookie-storage url-cookie-secure-storage))
                 nil)))
      (ignore-errors (emjupy--http-request "GET" server "/api/status")))
    (should (equal seen '(nil nil)))))

(ert-deftest emjupy-test-xsrf-harvest-tries-login-first ()
  "/login comes first because it is the endpoint that always answers
directly: on a server wanting credentials /tree merely redirects there,
and the redirect itself carries no cookie -- which left emjupy with no
cookie and every write answered 403 \"'_xsrf' argument missing\"."
  (should (equal (car emjupy--xsrf-endpoints) "/login"))
  (should-not (member "/api" emjupy--xsrf-endpoints)))

(ert-deftest emjupy-test-xsrf-sent-as-argument-on-writes ()
  "Tornado's check reads an `_xsrf' argument or the X-XSRFToken header;
sending both makes \"argument missing\" impossible whenever we hold a
cookie.  A GET carries neither."
  (let ((server (make-emjupy-server :base-url "localhost:9" :token "abc"))
        (seen nil))
    (setf (emjupy-server-xsrf server) "COOKIEVAL")
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _) (setq seen url) nil)))
      (ignore-errors (emjupy--http-request "POST" server "/api/kernels" "{}"))
      (should (string-match-p "_xsrf=COOKIEVAL" seen))
      (ignore-errors (emjupy--http-request "GET" server "/api/status"))
      (should-not (string-match-p "_xsrf=" seen)))))

(ert-deftest emjupy-test-restart-kernel-asks-first ()
  "A restart throws away every variable in the session and there is no
undo, so it asks -- and declining leaves the kernel alone."
  (let ((posted nil))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                  :source "x"
                                  :outputs [] :metadata (make-hash-table)))
        buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel emjupy--buffer-notebook)
              (make-emjupy-kernel :id "k1" :name "python3"))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
                  ((symbol-function 'emjupy--http-request)
                   (lambda (&rest _) (setq posted t))))
          (should-error (emjupy-restart-kernel) :type 'user-error))
        (should-not posted)))))

(ert-deftest emjupy-test-reachability-probes-a-protected-endpoint ()
  "Reachability must be judged from an endpoint that needs credentials.

/api/status is a health endpoint and several jupyter_server versions
answer it without authentication, so a tokenless probe returned 200,
emjupy concluded no token was needed, and every write was then refused
with 403 -- without ever asking for the token."
  (let ((probed nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (_m _s path &rest _) (setq probed path) t)))
      (emjupy--server-reachable-p
       (make-emjupy-server :base-url "localhost:8888"))
      (should-not (equal probed "/api/status"))
      (should (equal probed "/api/contents?content=0")))))

(ert-deftest emjupy-test-interrupt-posts-to-the-interrupt-endpoint ()
  "Interrupting leaves the session alone -- it is not a restart."
  (let ((sent nil))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                  :source "x"
                                  :outputs [] :metadata (make-hash-table)))
        buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel emjupy--buffer-notebook)
              (make-emjupy-kernel :id "k1" :name "python3"))
        (cl-letf (((symbol-function 'emjupy--http-request)
                   (lambda (method _s path &rest _) (setq sent
                                                          (list method path)))))
          (emjupy-interrupt-kernel))
        (should (equal sent '("POST" "/api/kernels/k1/interrupt")))))))

(ert-deftest emjupy-test-tokenless-probe-is-quiet ()
  "The probe for a token-less server does not report its 403.

A 403 there is the expected answer for a server that DOES want a token,
not a failure worth showing."
  (let ((shown nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               ;; as the real request raises it
               (lambda (&rest _) (signal 'emjupy-http-status
                                         (list "[Jupyter HTTP 403] GET: Forbidden"))))
              ((symbol-function 'message)
               (lambda (f &rest args) (when f
                                        (push (apply #'format
                                                     f args) shown)) nil)))
      (should-not (emjupy--server-reachable-p
                   (make-emjupy-server :base-url "h" :token "")))
      (should-not (cl-some (lambda (m) (string-match-p "403" m)) shown)))))

(ert-deftest emjupy-test-reopening-leaves-no-stray-rules ()
  "Re-opening a notebook into its existing buffer leaves no stray borders.

Re-opening reuses the buffer but parses fresh cell structs, so the
previous structs' overlays become unreachable.  Erasing the text without
deleting them would leave each collapsed to zero width, still drawing its
rule -- which is the stack of borders at the top
of a notebook opened again, the case that happens when the notebook was
left running on the server.

Drives `emjupy-open-notebook' rather than imitating it, so that it fails
if the open path stops going through the redraw."
  (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"))
         (ipynb (json-serialize
                 (let ((nb (make-hash-table :test 'equal))
                       (cell (make-hash-table :test 'equal))
                       (content (make-hash-table :test 'equal))
                       (top (make-hash-table :test 'equal)))
                   (puthash "cell_type" "code" cell)
                   (puthash "source" "a = 1" cell)
                   (puthash "outputs" [] cell)
                   (puthash "metadata" (make-hash-table :test 'equal) cell)
                   (puthash "cells" (vector cell) nb)
                   (puthash "metadata" (make-hash-table :test 'equal) nb)
                   (puthash "nbformat" 4 nb)
                   (puthash "nbformat_minor" 5 nb)
                   (puthash "content" nb content)
                   (puthash "type" "notebook" content)
                   (setq top content)
                   top)))
         (buffer nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _) (json-parse-string ipynb
                                                    :object-type 'hash-table)))
              ((symbol-function 'switch-to-buffer) (lambda (b &rest _) b))
              ((symbol-function 'emjupy-connect-kernel) (lambda (&rest _) nil)))
      (unwind-protect
          (progn
            ;; open the same notebook three times into the same buffer
            (dotimes (_ 3)
              (setq buffer (emjupy-open-notebook "reopen.ipynb" server)))
            (with-current-buffer buffer
              (let ((strays (seq-filter
                             (lambda (o)
                               (and (memq (overlay-get o 'emjupy-overlay)
                                          '(cell output))
                                    (= (overlay-start o)
                                       (overlay-end o))))
                             (overlays-in (point-min) (point-max))))
                    (tagged (seq-filter (lambda (o)
                                          (memq (overlay-get o
                                                             'emjupy-overlay)
                                                '(cell output)))
                                        (overlays-in (point-min) (point-max)))))
                (should (= (length strays) 0))
                ;; one cell, so one overlay, however many times it was opened
                (should (= (length tagged) 1))
                (should (string-match-p "a = 1" (buffer-string))))))
        (when (buffer-live-p buffer)
          (let ((kill-buffer-query-functions nil)) (kill-buffer buffer))))))
  (emjupy-test--companions-all-owned))

(ert-deftest emjupy-test-lsp-failure-names-the-cause ()
  "A refused WebSocket says nothing useful, so /lsp/status is asked.

Its answer separates the three causes, which look identical from the
outside: no `jupyter-lsp' at all, a refused token, or the endpoint
present but listing no language server -- the last being the usual
surprise, since `jupyterlab-lsp' is only the plumbing and installs
none."
  (let ((server (make-emjupy-server :base-url "box:9999" :token "t")))
    (cl-flet ((said (stub)
                (let ((out nil))
                  (cl-letf (((symbol-function 'emjupy--http-request) stub)
                            ((symbol-function 'message)
                             (lambda (f &rest args)
                               (when f (push (apply #'format f args) out))
                               nil)))
                    (emjupy--lsp-explain-failure server))
                  (car out))))
      ;; the endpoint is not there
      (should (string-match-p
               "jupyter-lsp"
               (said (lambda (&rest _)
                       (signal 'emjupy-http-status
                               (list "[Jupyter HTTP 404] GET: nope"))))))
      ;; the token is wrong -- which is a 403, not a missing endpoint
      (should (string-match-p
               "token"
               (said (lambda (&rest _)
                       (signal 'emjupy-http-status
                               (list "[Jupyter HTTP 403] GET: Forbidden"))))))
      ;; present, but nothing installed to run
      (should (string-match-p
               "no language server"
               (said (lambda (&rest _)
                       (let ((h (make-hash-table :test 'equal)))
                         (puthash "sessions" (make-hash-table :test 'equal) h)
                         h)))))
      ;; present and listing one, so the fault is elsewhere
      (should (string-match-p
               "pylsp"
               (said (lambda (&rest _)
                       (let ((h (make-hash-table :test 'equal))
                             (s (make-hash-table :test 'equal)))
                         (puthash "pylsp" t s)
                         (puthash "sessions" s h)
                         h))))))))

(ert-deftest emjupy-test-definition-in-a-server-file-is-fetched ()
  "A definition on the server opens over HTTP, not as a local path.

The language server runs beside the kernel and answers with paths on
THAT machine.  Handing one to `xref-make-file-location' opened it as if
it were local, so a jump the server had answered correctly still landed
nowhere."
  (let* ((server (make-emjupy-server :base-url "box:9999" :token "t"
                                     :root "/srv/nb"))
         (fetched nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (_m _s path &rest _)
                 (setq fetched path)
                 (let ((h (make-hash-table :test 'equal)))
                   (puthash "content" "def helper(x):\n    return x\n" h)
                   h))))
      (let ((buf (emjupy-open-server-file "/srv/nb/lib/mod.py" server 2)))
        (unwind-protect
            (progn
              ;; asked the Contents API for the right thing, as text
              (should (string-match-p "/api/contents/lib/mod.py" fetched))
              (should (string-match-p "format=text" fetched))
              (with-current-buffer buf
                (should buffer-read-only)
                (should (eq major-mode 'python-mode))
                (should (string-match-p "def helper" (buffer-string)))
                ;; and it went to the line asked for
                (should (= (line-number-at-pos) 2))
                ;; named so two servers holding the same path do not collide
                (should (string-match-p "box:9999" (buffer-name)))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
    ;; a path the server does not serve is refused, not invented
    (cl-letf (((symbol-function 'emjupy--http-request) (lambda (&rest _) nil)))
      (should-error (emjupy-open-server-file "/etc/passwd" server)
                    :type 'user-error))))

;;;; Performance

(ert-deftest emjupy-test-204-is-success-not-a-parse-error ()
  "A 204 reply carries no body, and that is the answer.

Interrupting a kernel replies 204 No Content with zero bytes -- measured
against a running server.  Parsing that as JSON reported a syntax error
for a request that had in fact succeeded."
  (let ((server (make-emjupy-server :base-url "h:1" :token "t")))
    ;; 204: no value, and no error
    (should-not (emjupy--http-interpret server "POST"
                                        "/api/kernels/k/interrupt" 204 ""))
    ;; ordinary success still parses
    (let ((parsed (emjupy--http-interpret server "GET" "/api/kernels" 200
                                          "{\"id\": \"k\"}")))
      (should (equal (gethash "id" parsed) "k")))
    ;; an empty body that is NOT 204 is not an answer: that is what a
    ;; forwarded port gives when nothing is listening at the far end, and
    ;; calling it malformed JSON points at the parser rather than the tunnel
    (should (string-match-p
             "returned nothing"
             (condition-case err
                 (progn (emjupy--http-interpret server "GET" "/api/kernels"
                                                200 "") "no error")
               (error (error-message-string err)))))
    ;; and a real failure still reports its status
    (should (string-match-p
             "404"
             (condition-case err
                 (progn (emjupy--http-interpret server "GET" "/api/nope" 404
                                                "{}") "no error")
               (error (error-message-string err)))))
    ;; malformed JSON is still malformed
    (should (string-match-p
             "JSON Parse Error"
             (condition-case err
                 (progn (emjupy--http-interpret server "GET" "/api/kernels"
                                                200 "{oops") "no error")
               (error (error-message-string err)))))))

(ert-deftest emjupy-test-secrets-are-redacted-from-urls ()
  "A URL shown to anyone has its credentials hidden.

The token and the XSRF cookie travel in the query string, and URLs reach
error messages and the diagnostic report -- which is written to be
pasted into a bug report.  What is useful there is which server was
being talked to, not what authenticated it."
  (should (equal (emjupy--redact-url
                  "ws://localhost:9999/lsp/ws/pylsp?token=s3cret")
                 "ws://localhost:9999/lsp/ws/pylsp?token=<redacted>"))
  (should (equal (emjupy--redact-url
                  "http://localhost:8888/api/kernels?_xsrf=abc123&_t=1")
                 "http://localhost:8888/api/kernels?_xsrf=<redacted>&_t=1"))
  ;; the rest of the URL is left alone: it is the useful part
  (should (equal (emjupy--redact-url "http://box:8888/api/contents/a.ipynb")
                 "http://box:8888/api/contents/a.ipynb"))
  ;; and the cache-buster is not a secret, so it stays legible
  (should (string-match-p "_t=1"
                          (emjupy--redact-url
                           "http://h/api?token=x&_t=1"))))

;;;; The undo guard

(ert-deftest emjupy-test-both-websockets-authenticate-alike ()
  "The language server's socket carries what the kernel's socket carries.

A token in the query string is not accepted everywhere a token in a
header is -- the kernel socket has always sent both, with a comment
saying why: a proxy in front of a tunnel may strip the query.  The
language server's socket sent neither, which is how a notebook could
execute cells over a WebSocket while the language server on the same
host refused to connect, and why the fallback to a LOCAL server kept
happening."
  (let ((server (make-emjupy-server :base-url "localhost:9999"
                                    :token "s3cret")))
    (should (equal (emjupy--websocket-auth-headers server)
                   '(("Authorization" . "token s3cret"))))
    ;; the XSRF cookie travels too, where there is one
    (setf (emjupy-server-xsrf server) "abc123")
    (should (equal (emjupy--websocket-auth-headers server)
                   '(("Authorization" . "token s3cret")
                     ("Cookie" . "_xsrf=abc123"))))
    ;; and a server with no token asks for nothing
    (should-not (emjupy--websocket-auth-headers
                 (make-emjupy-server :base-url "h" :token "")))))

(ert-deftest emjupy-test-request-is-bytes-with-a-multibyte-cookie ()
  "A request carrying non-ASCII output is sent as bytes throughout.

url.el joins the request line, headers and body and refuses the result
if it is multibyte.  An XSRF cookie read from a response with
`match-string' is ASCII but multibyte, and with any character outside
ASCII in the body -- a progress bar drawn in U+2588 -- the notebook could
not be saved: \"Multibyte text in HTTP request\"."
  (let* ((server (make-emjupy-server :base-url "127.0.0.1:9" :token ""))
         (seen nil))
    (setf (emjupy-server-xsrf server) (string-to-multibyte "2|abc|def|123"))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _)
                 (setq seen (list url url-request-extra-headers
                                  url-request-data))
                 nil)))
      (ignore (condition-case nil
                  (emjupy--http-request "PUT" server "/api/contents/x.ipynb"
                                        (json-serialize
                                         (list :text "100%|\u2588\u2588|")))
                (emjupy-http-error nil))))
    (pcase-let ((`(,url ,headers ,data) seen))
      (should-not (multibyte-string-p url))
      (dolist (h headers)
        (should-not (multibyte-string-p (car h)))
        (should-not (multibyte-string-p (cdr h))))
      (should-not (multibyte-string-p data))
      ;; the join url.el makes is bytes, which is what it requires
      (should-not (multibyte-string-p
                   (apply #'concat url data (mapcar #'cdr headers)))))))

(ert-deftest emjupy-test-reachability-takes-only-a-failed-request-for-no ()
  "A failed request means unreachable; a bug in emjupy is raised, not taken for it."
  (let ((server (make-emjupy-server :base-url "localhost:9" :token "")))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _) (signal 'emjupy-http-error
                                         (list "no server")))))
      (should-not (emjupy--server-reachable-p server)))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _) (signal 'wrong-type-argument
                                         (list 'stringp 1)))))
      (should-error (emjupy--server-reachable-p server)
                    :type 'wrong-type-argument))))

(ert-deftest emjupy-test-a-refused-session-says-why ()
  "A session the server would not start is reported with its reason."
  (let ((server (make-emjupy-server :base-url "localhost:8888" :token "t"))
        (said nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _) (signal 'emjupy-http-status
                                         (list "[Jupyter HTTP 403] POST: token refused"))))
              ((symbol-function 'message)
               (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
      (should-not (emjupy--start-kernel-session
                   (make-emjupy-notebook :server server :path "nb.ipynb")))
      (should (string-match-p "Could not start a session for nb.ipynb.*token refused" said)))))

(ert-deftest emjupy-test-http-responses-are-read-alike ()
  "A response is read the same whether it was waited for or not."
  (let ((server (make-emjupy-server :base-url "localhost:9" :token "t")))
    (should (equal (emjupy--http-read-response
                    (with-current-buffer (generate-new-buffer " r")
                      (insert "HTTP/1.1 404 Not Found\r\nSet-Cookie: _xsrf=abc; Path=/\r\n\r\n{\"message\": \"gone\"}")
                      (current-buffer))
                    server)
                   '(404 . "{\"message\": \"gone\"}")))
    (should (equal (emjupy-server-xsrf server) "abc"))
    ;; no response at all
    (should-not (car (emjupy--http-read-response
                      (with-current-buffer (generate-new-buffer " r")
                        (current-buffer)) server)))))

(ert-deftest emjupy-test-a-host-named-like-a-scheme-is-a-host ()
  "Only http:// and https:// are schemes: a host named httpfoo is a host.
Checked by its first letters, httpfoo:8888 was taken to have a scheme,
and asked for as httpfoo:8888/api/..., which url.el cannot fetch; and
httpsfoo:8888 was taken to be served over TLS."
  (should (equal (emjupy--server-root-url
                  (make-emjupy-server :base-url "httpfoo:8888"))
                 "http://httpfoo:8888"))
  (should (equal (plist-get (emjupy--server-parts
                             (make-emjupy-server :base-url "httpsfoo:8888"))
                            :scheme)
                 "http"))
  (should (equal (emjupy--server-root-url
                  (make-emjupy-server :base-url "https://hub.example.org/u/a"))
                 "https://hub.example.org/u/a")))


(ert-deftest emjupy-test-a-server-answering-is-asked-once-an-opening ()
  "A yes from the reachability probe holds for a moment; a no does not.
One opening asked three times, a round trip each."
  (let ((server (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
        (asked 0) (answer t)
        (emjupy--reachable-at (make-hash-table :test 'eq)))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _)
                 (cl-incf asked)
                 (if answer (make-hash-table)
                   (signal 'emjupy-http-error '("refused"))))))
      (setq answer nil)
      (should-not (emjupy--server-reachable-p server))
      (should-not (emjupy--server-reachable-p server))
      (should (= asked 2))
      (setq answer t)
      (should (emjupy--server-reachable-p server))
      (should (emjupy--server-reachable-p server))
      (should (= asked 3)))))

(provide 'emjupy-http-test)

;;; emjupy-http-test.el ends here
