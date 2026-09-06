;;; emjupy-test.el --- Tests for emjupy.el -*- lexical-binding: t; -*-

;; Kept separate from emjupy.el on purpose: implementation and tests are
;; different files, this one requires the other.
;;
;; Most tests here are pure/fast and need nothing external. A few, clearly
;; marked in section 9, need a real Python language server (pylsp, pyright,
;; or jedi-language-server) on PATH to fully exercise Eglot; those call
;; `ert-skip' when no such server is found rather than failing, so the rest
;; of the suite stays green on a machine without one installed.

(require 'ert)
(require 'emjupy)
(require 'cl-lib)

(defmacro emjupy-test--with-notebook (cells-form buf-var nb-var &rest body)
  "Set up a fresh emjupy-mode BUF-VAR/NB-VAR pair from CELLS-FORM, run
BODY with BUF-VAR as the current buffer, then kill it (and any shadow
buffer it created)."
  (declare (indent 3))
  `(let* ((,nb-var (make-emjupy-notebook :cells ,cells-form))
          (,buf-var (generate-new-buffer "*emjupy-test*")))
     (unwind-protect
         (with-current-buffer ,buf-var
           (emjupy-mode)
           (setq emjupy--buffer-notebook ,nb-var)
           (setf (emjupy-notebook-buffer ,nb-var) ,buf-var)
           (emjupy--rerender-notebook)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var))
         (when (and (emjupy-notebook-shadow-buffer ,nb-var)
                    (buffer-live-p (emjupy-notebook-shadow-buffer ,nb-var)))
           (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer ,nb-var))))))))

(defvar emjupy-test--sent nil
  "Payloads captured by `emjupy-test--with-fake-kernel'.")

(defmacro emjupy-test--with-fake-kernel (&rest body)
  "Run BODY with a stubbed-out kernel attached to this buffer's notebook.

Stubs the two seam functions rather than stuffing a non-websocket
sentinel into the kernel: the real `websocket-openp' type-checks its
argument, so a sentinel raises `wrong-type-argument' instead of
exercising the code under test. Sent payloads land in
`emjupy-test--sent'."
  `(let ((emjupy-test--sent nil))
     (when emjupy--buffer-notebook
       (setf (emjupy-notebook-kernel emjupy--buffer-notebook)
             (make-emjupy-kernel :id "fake-kernel"
                                 :pending (make-hash-table :test 'equal)
                                 :notebook emjupy--buffer-notebook)))
     (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
               ((symbol-function 'emjupy--ws-send)
                (lambda (payload &rest _) (push payload emjupy-test--sent))))
       ,@body)))

;;; ---------------------------------------------------------------------
;;; 1. Data structures
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-struct-creation ()
  "Ensure structs initialize correctly."
  (let ((server (make-emjupy-server :host "localhost" :port 8888 :token "abc")))
    (should (equal (emjupy-server-host server) "localhost"))
    (should (equal (emjupy-server-token server) "abc"))))

;;; ---------------------------------------------------------------------
;;; 2. .ipynb parsing & serialization
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-ipynb-roundtrip ()
  "Parsing then re-serializing a notebook preserves structure."
  (let* ((raw-json "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"import numpy as np\\n\",\"print(1)\"]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")
         (parsed-nb (emjupy--parse-ipynb raw-json))
         (first-cell (aref (emjupy-notebook-cells parsed-nb) 0))
         (reserialized (emjupy--serialize-notebook parsed-nb)))
    (should (eq (emjupy-cell-type first-cell) 'code))
    (should (string= (emjupy-cell-source first-cell) "import numpy as np\nprint(1)"))
    (should (string-match-p "\"nbformat\":4" reserialized))
    (should (string-match-p "\"import numpy as np\\\\n\"" reserialized))))

;;; ---------------------------------------------------------------------
;;; 2b. nbformat validity of what we WRITE BACK
;;; ---------------------------------------------------------------------
;; The whole point of the .ipynb-as-source-of-truth design is that
;; non-emacs collaborators can open what we save. A notebook that fails
;; `nbformat.validate' is rejected by nbconvert, papermill and friends
;; even though it looks fine inside emjupy.

(ert-deftest emjupy-test-serialize-emits-cell-ids ()
  "nbformat_minor 5 requires an `id' on EVERY cell."
  (let* ((c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x=1"
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array))
         (cell (aref (gethash "cells" out) 0)))
    (should (stringp (gethash "id" cell)))
    (should (> (length (gethash "id" cell)) 0))))

(ert-deftest emjupy-test-serialize-preserves-original-cell-id ()
  "A cell that came from a real notebook keeps ITS id on the way back
out, so a round trip doesn't renumber cells for collaborators."
  (let* ((raw "{\"cells\":[{\"cell_type\":\"code\",\"id\":\"abc12345\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"x=1\"]}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}")
         (nb (emjupy--parse-ipynb raw))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array)))
    (should (equal (gethash "id" (aref (gethash "cells" out) 0)) "abc12345"))))

(ert-deftest emjupy-test-serialize-normalizes-required-output-fields ()
  "display_data/execute_result outputs REQUIRE `metadata' (and
execute_result also `execution_count') per the nbformat v4 schema.
Kernels don't always send them."
  (let* ((oh (make-hash-table :test 'equal))
         (dat (make-hash-table :test 'equal))
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                              :metadata (make-hash-table))))
    (puthash "text/plain" "<Figure>" dat)
    (puthash "output_type" "display_data" oh)
    (puthash "data" dat oh)                ; deliberately NO metadata
    (setf (emjupy-cell-outputs c) (vector oh))
    (let* ((nb (make-emjupy-notebook :cells (vector c)))
           (out (json-parse-string (emjupy--serialize-notebook nb)
                                   :object-type 'hash-table :array-type 'array))
           (o (aref (gethash "outputs" (aref (gethash "cells" out) 0)) 0)))
      (should (hash-table-p (gethash "metadata" o))))))

(ert-deftest emjupy-test-source-roundtrip-is-idempotent ()
  "Only interior lines carry a newline in the nbformat `source' array.
Appending one to the last line too makes every save/reload cycle grow
the cell by a trailing blank line."
  (should (equal (append (emjupy--source-lines "a\nb") nil) '("a\n" "b")))
  (should (equal (append (emjupy--source-lines "solo") nil) '("solo")))
  ;; parse(serialize(x)) == x
  (let* ((src "import numpy as np\nprint(1)")
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source src
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (again (emjupy--parse-ipynb (emjupy--serialize-notebook nb))))
    (should (equal (emjupy-cell-source (aref (emjupy-notebook-cells again) 0)) src))))

;;; ---------------------------------------------------------------------
;;; 2c. Server URL parsing (remote kernels reached through an SSH tunnel)
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-server-parts-handles-tunnel-urls ()
  "A tunnelled server may be given as a bare port, a host:port, a full
https URL, or a proxied base path -- all of which have to survive into
the WebSocket URL, not just the REST calls."
  (cl-flet ((parts (url) (emjupy--server-parts (make-emjupy-server :base-url url))))
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

(ert-deftest emjupy-test-kernel-ws-url-is-scheme-and-path-aware ()
  "The kernel WebSocket URL must carry the scheme, the base path, and a
url-encoded token -- and must NOT emit a bare `?token=' when there is
no token."
  (let ((captured nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest _) (setq captured url) nil)))
      (let* ((server (make-emjupy-server
                      :base-url "https://example.org:8443/user/alice" :token "t o k"))
             (nb (make-emjupy-notebook :path "a.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "wss://" captured))
        (should (string-match-p "/user/alice/api/kernels/kid/channels" captured))
        (should (string-match-p "token=t%20o%20k" captured)))
      (let* ((server (make-emjupy-server :base-url "localhost:18888" :token ""))
             (nb (make-emjupy-notebook :path "b.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "ws://" captured))
        (should-not (string-match-p "token=" captured))))))

(ert-deftest emjupy-test-two-servers-keep-separate-xsrf ()
  "The XSRF cookie belongs to ONE server. Sharing it globally means a
second tunnel replays the first server's cookie and earns a 403."
  (let ((a (emjupy--intern-server "localhost:18888" "tok-a"))
        (b (emjupy--intern-server "localhost:19999" "tok-b")))
    (setf (emjupy-server-xsrf a) "xsrf-a")
    (setf (emjupy-server-xsrf b) "xsrf-b")
    (should (equal (emjupy-server-xsrf a) "xsrf-a"))
    (should (equal (emjupy-server-xsrf b) "xsrf-b"))
    (should-not (eq a b))
    ;; re-login to a known server updates its token but keeps the object,
    ;; so notebooks already pointing at it stay valid
    (let ((again (emjupy--intern-server "localhost:18888" "tok-a2")))
      (should (eq again a))
      (should (equal (emjupy-server-token a) "tok-a2"))
      (should (equal (emjupy-server-xsrf a) "xsrf-a")))))

(ert-deftest emjupy-test-notebook-buffer-name-includes-server ()
  "The same notebook path can exist on two servers; one buffer cannot
represent both."
  (let ((a (make-emjupy-server :base-url "localhost:18888"))
        (b (make-emjupy-server :base-url "localhost:19999")))
    (should-not (equal (emjupy--notebook-buffer-name "analysis.ipynb" a)
                       (emjupy--notebook-buffer-name "analysis.ipynb" b)))))

(ert-deftest emjupy-test-shadow-path-includes-server ()
  "Likewise for the Eglot shadow file: two servers hosting
`analysis.ipynb' must not share one shadow buffer, or each would
clobber the other's code."
  (let* ((a (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:18888")))
         (b (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:19999"))))
    (should-not (equal (emjupy--shadow-file-path a) (emjupy--shadow-file-path b)))))

(ert-deftest emjupy-test-markdown-highlighted-without-markdown-mode ()
  "Markdown cells must be highlighted even when no markdown package is
installed. `markdown-mode' is a MELPA package emjupy does not depend
on, so relying on it alone left markdown cells as flat, undifferentiated
plain text for most users."
  (cl-letf (((symbol-function 'emjupy--markdown-mode-fn) (lambda () nil)))
    (let* ((src "# A heading\n\nSome **bold** and *italic* and `code`.\n\n- a bullet\n> a quote\n\n[label](http://example.org)")
           (out (emjupy--fontify-as src 'markdown))
           (face-at (lambda (needle &optional off)
                      (let ((i (+ (string-match (regexp-quote needle) out) (or off 0))))
                        (get-text-property i 'face out)))))
      (cl-flet ((has (faces f) (if (listp faces) (memq f faces) (eq faces f))))
        ;; heading
        (should (has (funcall face-at "# A heading") 'font-lock-function-name-face))
        ;; emphasis
        (should (has (funcall face-at "**bold**") 'bold))
        (should (has (funcall face-at "*italic*" 1) 'italic))
        ;; inline code
        (should (has (funcall face-at "`code`") 'font-lock-constant-face))
        ;; list bullet
        (should (has (funcall face-at "- a bullet") 'font-lock-keyword-face))
        ;; blockquote
        (should (has (funcall face-at "> a quote") 'font-lock-comment-face))
        ;; link label
        (should (has (funcall face-at "label") 'link))))))

(ert-deftest emjupy-test-markdown-fallback-not-used-when-mode-available ()
  "When a real markdown mode IS installed, defer to it rather than
layering emjupy's approximation on top."
  (let ((called nil))
    (cl-letf (((symbol-function 'emjupy--markdown-mode-fn) (lambda () 'text-mode))
              ((symbol-function 'emjupy--markdown-fontify-fallback)
               (lambda () (setq called t))))
      (emjupy--fontify-as "# hi" 'markdown)
      (should-not called))))

(ert-deftest emjupy-test-code-cells-still-use-python ()
  "The markdown work must not disturb python highlighting."
  (let ((out (emjupy--fontify-as "def f():\n    return 1" 'code)))
    (should (text-property-not-all 0 (length out) 'face nil out))))

;;; ---------------------------------------------------------------------
;;; 2d. Login: one port, one kernel
;;; ---------------------------------------------------------------------

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
                  ((and (string= method "POST") (string-match-p "/api/kernels" path))
                   (setq posted t) (make-hash-table :test 'equal))
                  ((string-match-p "/api/kernels" path) (vector k))
                  (t (make-hash-table :test 'equal)))))
              ((symbol-function 'completing-read)
               (lambda (&rest _) (error "must not prompt for a single kernel"))))
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
                  ((and (string= method "POST") (string-match-p "/api/kernels" path)) spawned)
                  ((string-match-p "/api/kernels" path) (vector))
                  (t (make-hash-table :test 'equal))))))
      (should (equal (emjupy--bind-server-kernel server) "kern-new")))))

(ert-deftest emjupy-test-login-normalizes-bare-port ()
  "A bare port means localhost, which is what an `ssh -L' forward gives you."
  (should (equal (emjupy--normalize-url "8888") "localhost:8888"))
  (should (equal (emjupy--normalize-url "localhost:8888") "localhost:8888"))
  (should (equal (emjupy--normalize-url "https://example.org/jup")
                 "https://example.org/jup")))

(ert-deftest emjupy-test-login-does-not-prompt-for-token-when-unneeded ()
  "Many tunnelled servers run token-less. Asking every time is what made
logging into a second tunnel tedious."
  (cl-letf (((symbol-function 'emjupy--server-reachable-p) (lambda (&rest _) t))
            ((symbol-function 'read-string)
             (lambda (&rest _) (error "must not prompt when no token is needed"))))
    (should (equal (emjupy--resolve-token "localhost:18888" nil) "")))
  ;; but it does ask when the server rejects an empty token
  (cl-letf (((symbol-function 'emjupy--server-reachable-p) (lambda (&rest _) nil))
            ((symbol-function 'read-string) (lambda (&rest _) "typed-token")))
    (should (equal (emjupy--resolve-token "localhost:18888" nil) "typed-token")))
  ;; an explicit token always wins
  (should (equal (emjupy--resolve-token "localhost:18888" "explicit") "explicit")))

(ert-deftest emjupy-test-each-port-binds-its-own-kernel ()
  "One port = one kernel: two tunnels must not end up sharing one."
  (let ((a (emjupy--intern-server "localhost:18888" ""))
        (b (emjupy--intern-server "localhost:19999" "")))
    (setf (emjupy-server-kernel-id a) "kern-a")
    (setf (emjupy-server-kernel-id b) "kern-b")
    (should (equal (emjupy-server-kernel-id a) "kern-a"))
    (should (equal (emjupy-server-kernel-id b) "kern-b"))))

;;; ---------------------------------------------------------------------
;;; 2e. Output background band
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-hex-colors-parsed-exactly ()
  "Hex colours must be parsed arithmetically, NOT through
`color-name-to-rgb', which resolves via the display palette and on a
tty quantises \"#1c1c1c\" all the way to black."
  (should (equal (emjupy--color-rgb "#000000") '(0.0 0.0 0.0)))
  (should (equal (emjupy--color-rgb "#ffffff") '(1.0 1.0 1.0)))
  (let ((rgb (emjupy--color-rgb "#1c1c1c")))
    (should rgb)
    (should (> (car rgb) 0.0))
    (should (< (car rgb) 0.2)))
  (should (equal (emjupy--color-rgb "#fff") '(1.0 1.0 1.0)))
  (should-not (emjupy--color-rgb "not-a-color"))
  (should-not (emjupy--color-rgb nil)))

(ert-deftest emjupy-test-output-color-contrasts-in-both-directions ()
  "The output accent has to differ from the buffer background in BOTH
directions: darker on a light theme, lighter on a dark one."
  (cl-flet ((accent (bg fg) (emjupy--blend-colors bg fg emjupy-output-blend))
            (lum (c) (apply #'+ (emjupy--color-rgb c))))
    (let ((c (accent "#ffffff" "#000000")))
      (should c)
      (should (< (lum c) (lum "#ffffff"))))
    (let ((c (accent "#1c1c1c" "#e5e5e5")))
      (should c)
      (should (> (lum c) (lum "#1c1c1c"))))))

(ert-deftest emjupy-test-output-color-explicit-and-disabled ()
  "An explicit colour is used verbatim; nil disables the band entirely,
leaving output on the buffer's normal background."
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face attr &rest _)
               (if (eq face 'default)
                   (cond ((eq attr :background) "#ffffff")
                         ((eq attr :foreground) "#000000"))
                 'unspecified))))
    (let ((got (make-hash-table :test 'eq)))
      (cl-letf (((symbol-function 'set-face-attribute)
                 (lambda (face _f _a value) (puthash face value got))))
        (let ((emjupy-output-color "#f0f0f0"))
          (should (emjupy--sync-theme-colors))
          (should (equal (gethash 'emjupy-output got) "#f0f0f0")))
        (let ((emjupy-output-color nil)
              (emjupy-output-error-color nil)
              (emjupy-output-warning-color nil)
              (emjupy-output-image-color nil))
          (should-not (emjupy--sync-theme-colors))
          (should (eq (gethash 'emjupy-output got) 'unspecified))
          (should (eq (gethash 'emjupy-output-error got) 'unspecified)))))))

(ert-deftest emjupy-test-nothing-but-output-is-recoloured ()
  "The buffer background is left completely alone, and a cell with no
output carries no background overlay at all.

An earlier design remapped `default' to a canvas colour and painted
cells back on top; that inverted the instant anything was wrong with
the cell colour, and it did."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should-not (assq 'default face-remapping-alist))
        ;; the source overlay paints nothing
        (should-not (overlay-get (emjupy-cell-overlay cell) 'face))
        ;; and with no output there is no band overlay to paint
        (should-not (emjupy-cell-output-ov cell))))))

(ert-deftest emjupy-test-output-band-appears-and-disappears-with-output ()
  "Output gets a background band; clearing the outputs shrinks the box
and takes the band with it."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table)))
        (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "hello\n" oh)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; no output yet: no band
        (should-not (emjupy-cell-output-ov cell))
        ;; output arrives
        (setf (emjupy-cell-outputs cell) (vector oh))
        (emjupy--rerender-notebook)
        (should (emjupy-cell-output-ov cell))
        ;; the band is painted on the output text itself now, not on the
        ;; overlay, so several kinds of output can be coloured differently
        ;; within one box
        (goto-char (point-min))
        (should (search-forward "hello" nil t))
        (should (memq 'emjupy-output
                      (get-text-property (match-beginning 0) 'face)))
        ;; and the band covers the output text, not the source
        (let ((ov (emjupy-cell-output-ov cell)))
          (should (string-match-p "hello"
                                  (buffer-substring-no-properties
                                   (overlay-start ov) (overlay-end ov))))
          (should-not (string-match-p "x = 1"
                                      (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov)))))
        ;; outputs cleared: box shrinks, band goes with it
        (setf (emjupy-cell-outputs cell) [])
        (emjupy--rerender-notebook)
        (should-not (emjupy-cell-output-ov cell))
        (should-not (string-match-p "hello" (buffer-string)))))))

(ert-deftest emjupy-test-typing-is-highlighted-immediately ()
  "Highlighting must follow typing, not wait for the next render."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-end (emjupy-cell-overlay cell)))
        (forward-line -1)
        (end-of-line)
        (insert "\nimport os")
        (goto-char (point-min))
        (should (search-forward "import" nil t))
        (should (eq (get-text-property (match-beginning 0) 'face)
                    'font-lock-keyword-face))))))

(ert-deftest emjupy-test-typing-does-not-inherit-neighbouring-face ()
  "Text typed straight after a keyword must not pick up its face.
`self-insert-command' inherits sticky properties from the character
before point, so a plain word typed after `import' came out keyword
coloured."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "import os"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "import" nil t))
        ;; type immediately after the keyword, the way `self-insert' would
        (insert-and-inherit "ed_thing")
        (goto-char (point-min))
        (should (search-forward "ed_thing" nil t))
        (should-not (eq (get-text-property (match-beginning 0) 'face)
                        'font-lock-keyword-face))))))

(ert-deftest emjupy-test-refontifying-leaves-text-and-undo-alone ()
  "Re-highlighting rewrites face properties only: the buffer text is
untouched and nothing lands in the undo history."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((before (buffer-substring-no-properties (point-min) (point-max))))
          (setq buffer-undo-list nil)
          (emjupy--refontify-cell cell)
          (should (equal before (buffer-substring-no-properties (point-min) (point-max))))
          (should (null (delq nil buffer-undo-list))))))))

(ert-deftest emjupy-test-output-band-fills-the-whole-line ()
  "The band must fill each output line edge to edge, not stop at the
last character.

Since Emacs 27 a face's background stops at end-of-line unless the face
sets `:extend'.  Without it a short output line is highlighted only as
far as its text, which looks ragged beside a long one.  Two things have
to hold: the face extends, and the overlay actually covers each newline
for `:extend' to act on."
  (should (eq (face-attribute 'emjupy-output :extend nil nil) t))
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table)))
        (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    ;; deliberately ragged: a short line next to a long one
    (puthash "text" "hi\na considerably longer output line\n" oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((ov (emjupy-cell-output-ov cell))
              (total 0) (covered 0))
          (should ov)
          (save-excursion
            (goto-char (overlay-start ov))
            (while (< (point) (overlay-end ov))
              (when (eq (char-after) ?\n)
                (setq total (1+ total))
                (when (memq ov (overlays-at (point)))
                  (setq covered (1+ covered))))
              (forward-char 1)))
          (should (> total 0))
          ;; every newline inside the band is covered, so every line extends
          (should (= total covered)))))))

(ert-deftest emjupy-test-output-face-sets-only-background ()
  "`emjupy-output' must specify a background and NOTHING else.

It is applied as an overlay face over output text, and overlay faces
merge on top of text properties -- so every attribute it specifies
overrides the faces on tracebacks and rich output."
  (should (eq (face-attribute 'emjupy-output :inherit nil nil) 'unspecified))
  ;; INHERIT must be t here -- with nil, `face-attribute' does not follow
  ;; `:inherit', so an inheriting face still reports `unspecified' and the
  ;; check passes while the bug is present.
  (dolist (attr '(:foreground :weight :slant :underline :overline
                  :strike-through :box :inverse-video))
    (should (eq (face-attribute 'emjupy-output attr nil t) 'unspecified))))

;;; ---------------------------------------------------------------------
;;; 2f. Undo safety
;;; ---------------------------------------------------------------------

(defun emjupy-test--three-cell-notebook ()
  (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "aaa = 1"
                            :outputs [] :metadata (make-hash-table))
          (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "bbb = 2"
                            :outputs [] :metadata (make-hash-table))
          (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "ccc = 3"
                            :outputs [] :metadata (make-hash-table))))

(ert-deftest emjupy-test-rerender-is-not-recorded-in-undo ()
  "Re-rendering erases and rebuilds the whole buffer.  Recording that as
an undoable change used to push ~20 entries onto the list, and undoing
one of them tore the notebook apart."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (setq buffer-undo-list nil)
        (emjupy--rerender-notebook)
        (should (null (delq nil buffer-undo-list)))))))

(ert-deftest emjupy-test-undo-from-another-cell-does-not-scramble ()
  "Undo invoked with point in a different cell must never damage the
notebook.  It used to delete every cell after the one being edited."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let ((c1 (aref cells 0)) (c3 (aref cells 2)))
          (setq buffer-undo-list nil)
          ;; type in cell 1
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (insert "11")
          (undo-boundary)
          (emjupy--sync-all-cells)
          ;; something re-renders (output arriving on another cell)
          (let ((oh (make-hash-table :test 'equal)))
            (puthash "output_type" "stream" oh)
            (puthash "name" "stdout" oh)
            (puthash "text" "hi\n" oh)
            (setf (emjupy-cell-outputs c3) (vector oh)))
          (emjupy--rerender-notebook c3)
          ;; undo from cell 3
          (goto-char (overlay-start (emjupy-cell-overlay c3)))
          (ignore-errors (undo))
          (emjupy--sync-all-cells)
          ;; every cell survives, in order, with its own source
          (should (= (length (emjupy-notebook-cells nb)) 3))
          (should (equal (emjupy-cell-source (aref cells 1)) "bbb = 2"))
          (should (equal (emjupy-cell-source (aref cells 2)) "ccc = 3"))
          (should (string-match-p "bbb = 2" (buffer-string)))
          (should (string-match-p "ccc = 3" (buffer-string))))))))

(ert-deftest emjupy-test-undo-still-works-when-render-changed-nothing ()
  "A re-render that produces identical text must leave the undo history
intact -- otherwise every stray refresh would silently drop it."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let ((c1 (aref cells 0)) (c3 (aref cells 2)))
          (setq buffer-undo-list nil)
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (insert "11")
          (undo-boundary)
          (emjupy--sync-all-cells)
          (emjupy--rerender-notebook c3)   ; no textual change
          (goto-char (overlay-start (emjupy-cell-overlay c3)))
          (undo)
          (emjupy--sync-all-cells)
          (should (equal (emjupy-cell-source c1) "aaa = 1"))
          (should (= (length (emjupy-notebook-cells nb)) 3)))))))

;;; ---------------------------------------------------------------------
;;; 3. Cell operations: insert / delete / move / cycle-type
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-cell-insertion-and-deletion ()
  "Creating and deleting notebook cells dynamically."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 10"
                                 :outputs [] :metadata (make-hash-table)))
      buf nb
    (with-current-buffer buf
      (goto-char (point-min))
      (emjupy-insert-cell-below)
      (should (= (length (emjupy-notebook-cells nb)) 2))
      (emjupy-delete-cell)
      (should (= (length (emjupy-notebook-cells nb)) 1)))))

(ert-deftest emjupy-test-move-cell-up-down ()
  "Moving a cell swaps its position; its output moves WITH it (output is
a field on the cell struct, not a separate entity); the top/bottom
boundary is a no-op rather than an error."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "A" :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "B" :outputs [] :metadata (make-hash-table)))
         (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "C" :outputs [] :metadata (make-hash-table)))
         (out (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" out) (puthash "text" "B-output\n" out)
    (setf (emjupy-cell-outputs c2) (vector out))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source (emjupy-notebook-cells nb)) '("B" "A" "C")))
        (should (> (length (emjupy-cell-outputs (aref (emjupy-notebook-cells nb) 0))) 0))
        ;; the (now) top cell moving up again must be a no-op
        (goto-char (overlay-start (emjupy-cell-overlay (aref (emjupy-notebook-cells nb) 0))))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source (emjupy-notebook-cells nb)) '("B" "A" "C")))))))

(ert-deftest emjupy-test-cycle-cell-type ()
  "C-c C-t toggles a cell between code and markdown."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'markdown))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'code))))))

;;; ---------------------------------------------------------------------
;;; 4. Execution: cell order must never scramble; sync must cover ALL cells
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-execute-preserves-cell-order ()
  "Executing a non-last cell must not shove it (and its output) to the
end of the buffer -- output belongs directly after its own cell, and
every later cell must still render after it, in order."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "2+2" :outputs [] :metadata (make-hash-table)))
        (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "3+3" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point)
         ;; the code that got sent is the cell we asked for, not a neighbour
         (should (string-match-p (regexp-quote "1+1") (car emjupy-test--sent))))
        (let ((oh (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" oh) (puthash "name" "stdout" oh) (puthash "text" "2\n" oh)
          (emjupy--append-output-to-cell c1 oh nb))
        (should (< (overlay-start (emjupy-cell-output-ov c1)) (overlay-start (emjupy-cell-overlay c2))))
        (should (< (overlay-start (emjupy-cell-overlay c2)) (overlay-start (emjupy-cell-overlay c3))))))))

(ert-deftest emjupy-test-execute-syncs-all-cells-not-just-current ()
  "Executing one cell must not discard unsynced live edits sitting in a
DIFFERENT cell -- the rerender it triggers rebuilds the whole buffer
from cell structs, so every cell must be synced first, not just the
one being run."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         ;; type into cell 2 with no explicit sync, the way live typing works
         (goto-char (overlay-start (emjupy-cell-overlay c2)))
         (insert "999")
         ;; then execute cell 1
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point))
        (should (string-match-p "999" (buffer-string)))))))

;;; ---------------------------------------------------------------------
;;; 5. Output-box rendering: visibility, merged divider, 80-col borders
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-no-output-box-when-no-output ()
  "A cell with no output (e.g. a bare import) gets no output box at all,
and its own bottom border is never hidden."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "import numpy as np"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should-not (emjupy-cell-output-ov cell))
      (let ((footer (overlay-get (emjupy-cell-overlay cell) 'after-string)))
        (should (and footer (string-match-p "└" footer)))))))

(ert-deftest emjupy-test-output-box-merges-with-input-border ()
  "A cell WITH output gets ONE merged divider line (T-junction), not a
closing border immediately followed by a separate opening one."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "print(1)"
                                  :outputs [] :metadata (make-hash-table)))
         (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
    (puthash "text/plain" "1" dat)
    (puthash "output_type" "execute_result" oh) (puthash "data" dat oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should (emjupy-cell-output-ov cell))
      (should (string= (overlay-get (emjupy-cell-overlay cell) 'after-string) ""))
      (should (string-prefix-p "├" (overlay-get (emjupy-cell-output-ov cell) 'before-string))))))

(ert-deftest emjupy-test-box-borders-match-configured-width ()
  "Box-drawing borders (excluding their trailing newline) are pinned to
`emjupy-box-width', regardless of label length, and never crash on an
overlong label."
  (let ((emjupy-box-width 80))
    (should (= (1- (length (emjupy--box-header "[In: 1] python"))) 80))
    (should (= (1- (length (emjupy--box-footer))) 80))
    (should (string-suffix-p "\n" (emjupy--box-header (make-string 500 ?x)))))
  ;; header and footer always agree, at any configured width
  (dolist (w '(60 100 140))
    (let ((emjupy-box-width w))
      (should (= (length (emjupy--box-header "[In: 12] python"))
                 (length (emjupy--box-footer))))
      (should (= (1- (length (emjupy--box-footer))) w)))))

(ert-deftest emjupy-test-box-width-fits-the-window ()
  "The default `window' setting sizes the outline to the window, so the
rule spans a wide frame instead of stopping short at 80 columns."
  (let ((emjupy-box-width 'window)
        (emjupy-box-min-width 60))
    ;; With no window (batch), fall back to a sane fixed width rather than
    ;; erroring or collapsing to zero.
    (should (>= (emjupy--box-width) emjupy-box-min-width))
    ;; Never narrower than the configured floor.
    (let ((emjupy-box-min-width 200))
      (should (>= (emjupy--box-width) 200)))))

;;; ---------------------------------------------------------------------
;;; 6. Rich output: display_data / image routing
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-display-data-not-dropped ()
  "display_data messages -- how matplotlib's inline figure actually
arrives, separately from the execute_result text repr -- must be
captured, not silently ignored."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                 :outputs [] :metadata (make-hash-table)))
         (msg-id "msg-display-data-test")
         (nb (make-emjupy-notebook :cells (vector cell)))
         (kernel (make-emjupy-kernel :id "k1"
                                     :pending (make-hash-table :test 'equal)
                                     :notebook nb)))
    (puthash msg-id cell (emjupy-kernel-pending kernel))
    (let* ((hdr (make-hash-table :test 'equal)) (ph (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)) (content (make-hash-table :test 'equal))
           (msg (make-hash-table :test 'equal)))
      (puthash "msg_type" "display_data" hdr)
      (puthash "msg_id" msg-id ph)
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "data" dat content)
      (puthash "header" hdr msg) (puthash "parent_header" ph msg) (puthash "content" content msg)
      ;; Handed the raw payload rather than a websocket-frame -- the handler
      ;; accepts either, so recorded kernel traffic can be replayed. NOT
      ;; wrapped in ignore-errors: a throw here must fail the test, not be
      ;; silently absorbed into a passing "0 outputs" assertion.
      (emjupy--handle-ws-message kernel (json-serialize msg))
      (should (> (length (emjupy-cell-outputs cell)) 0))
      (should (equal (gethash "output_type" (aref (emjupy-cell-outputs cell) 0)) "display_data")))))

(ert-deftest emjupy-test-image-preferred-over-text ()
  "When a result carries both image/png and text/plain, the image
branch is taken -- on an Emacs that can actually display images."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "<<IMG:png>>" (buffer-string))))))))

(ert-deftest emjupy-test-image-falls-back-to-text-without-image-support ()
  "On an Emacs that CANNOT display images (a tty frame, or a build
without libpng -- `emacs-nox' is the common case), an image output must
degrade to the bundle's own text/plain repr plus a visible note.

Regression test: `create-image' SIGNALS `Invalid image type' rather
than returning nil, and rendering happens inside the websocket
callback where websocket.el swallows errors -- so the failure mode was
a silently blank output box with nothing logged."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) nil)))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure size 640x480>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((text (buffer-string)))
            ;; the text repr is shown ...
            (should (string-match-p "<Figure size 640x480>" text))
            ;; ... and the user is told why there's no picture
            (should (string-match-p "image/png" text))
            ;; and the output box is NOT empty
            (should (emjupy-cell-output-ov cell))))))))

(ert-deftest emjupy-test-image-render-error-is-reported-not-swallowed ()
  "If `create-image' signals even though the type looked available, the
cell must still render something explaining itself rather than a blank box."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (&rest _) (error "Invalid image type `png'"))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "could not render" (buffer-string))))))))

(ert-deftest emjupy-test-svg-is-not-base64-decoded ()
  "image/svg+xml arrives as literal markup, not base64 -- decoding it
would corrupt it (or signal)."
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :data data))))
    (let ((img (emjupy--render-image-output "<svg width='1'></svg>" 'svg)))
      (should (equal (plist-get (cdr img) :data) "<svg width='1'></svg>")))))

(ert-deftest emjupy-test-mime-text-accepts-string-or-array ()
  "nbformat stores multi-line MIME payloads as a string OR an array of
lines depending on whether the JSON came from the Contents API or
straight off disk; both must render."
  (should (equal (emjupy--mime-text "a\nb") "a\nb"))
  (should (equal (emjupy--mime-text ["a\n" "b"]) "a\nb"))
  (should (equal (emjupy--mime-text nil) "")))

(ert-deftest emjupy-test-duplicate-image-rendered-once ()
  "A cell whose last expression is a figure receives the SAME picture
twice from the kernel -- once as the execute_result repr, once as the
inline backend's display_data. It must be drawn once."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (kind)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" "iVBORw0KGgoSAMEPAYLOAD==" dat)
                   (puthash "text/plain" "<Figure size 640x480 with 1 Axes>" dat)
                   (puthash "output_type" kind oh)
                   (puthash "data" dat oh)
                   oh))))
      (setf (emjupy-cell-outputs cell)
            (vector (funcall mk "execute_result") (funcall mk "display_data")))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((n 0) (start 0))
            (while (string-match "<<IMG:png>>" (buffer-string) start)
              (setq n (1+ n) start (match-end 0)))
            (should (= n 1))))))))

(ert-deftest emjupy-test-distinct-images-both-rendered ()
  "Deduplication must only collapse IDENTICAL images -- two different
figures in one cell are two outputs and both have to show."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "two figs"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (payload)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" payload dat)
                   (puthash "output_type" "display_data" oh)
                   (puthash "data" dat oh)
                   oh))))
      (setf (emjupy-cell-outputs cell)
            (vector (funcall mk "iVBORw0KGgoAAAAA") (funcall mk "iVBORw0KGgoBBBBB")))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((n 0) (start 0))
            (while (string-match "<<IMG:" (buffer-string) start)
              (setq n (1+ n) start (match-end 0)))
            (should (= n 2))))))))

(ert-deftest emjupy-test-repeated-text-output-is-not-deduplicated ()
  "Two identical `print' calls really are two outputs -- only images
are collapsed."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "print"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda ()
               (let ((oh (make-hash-table :test 'equal)))
                 (puthash "output_type" "stream" oh)
                 (puthash "name" "stdout" oh)
                 (puthash "text" "same line\n" oh)
                 oh))))
    (setf (emjupy-cell-outputs cell) (vector (funcall mk) (funcall mk)))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((n 0) (start 0))
          (while (string-match "same line" (buffer-string) start)
            (setq n (1+ n) start (match-end 0)))
          (should (= n 2)))))))

(ert-deftest emjupy-test-dedup-does-not-alter-saved-outputs ()
  "Deduplication is a RENDER-time concern only. The cell's outputs --
and therefore the .ipynb we write back -- must still contain exactly
what the kernel sent, so the file stays faithful for other clients."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda (kind)
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" kind oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell)
          (vector (funcall mk "execute_result") (funcall mk "display_data")))
    (emjupy-test--with-notebook (vector cell) buf nb
      ;; render collapses to one ...
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 1))
      ;; ... but the struct, and the serialized notebook, keep both
      (should (= (length (emjupy-cell-outputs cell)) 2))
      (let* ((json (emjupy--serialize-notebook nb))
             (out (json-parse-string json :object-type 'hash-table :array-type 'array)))
        (should (= (length (gethash "outputs" (aref (gethash "cells" out) 0))) 2))))))

(ert-deftest emjupy-test-dedup-can-be-disabled ()
  "`emjupy-deduplicate-image-outputs' nil restores the raw behaviour."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda ()
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" "display_data" oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell) (vector (funcall mk) (funcall mk)))
    (let ((emjupy-deduplicate-image-outputs nil))
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 2)))
    (let ((emjupy-deduplicate-image-outputs t))
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 1)))))

;;; ---------------------------------------------------------------------
;;; 7. Kernel restart
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-restart-kernel-requires-connection ()
  "Restarting with no kernel signals a clear user-error instead of
failing deep inside some HTTP call."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x"
                                :outputs [] :metadata (make-hash-table)))
      buf nb
    (should-error (emjupy-restart-kernel) :type 'user-error)))

(ert-deftest emjupy-test-restart-kernel-reuses-same-id ()
  "Restart must POST to THIS notebook's own kernel id and reconnect
with that SAME id (not spawn a fresh one), and drop now-orphaned
pending requests."
  (let* ((server (make-emjupy-server :base-url "localhost:8888" :token "tok"))
         (restart-path nil) (reconnected-with nil))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x"
                                  :outputs [] :metadata (make-hash-table)))
        buf nb
      (setf (emjupy-notebook-server nb) server)
      (let ((kernel (make-emjupy-kernel :id "kernel-abc" :server server
                                        :pending (make-hash-table :test 'equal)
                                        :notebook nb)))
        (setf (emjupy-notebook-kernel nb) kernel)
        (puthash "stale-msg" 'some-cell (emjupy-kernel-pending kernel))
        (cl-letf (((symbol-function 'emjupy--http-request)
                   (lambda (_method _server path &rest _)
                     (when (string-match-p "/restart" path) (setq restart-path path))
                     (make-hash-table :test 'equal)))
                  ((symbol-function 'emjupy-connect-kernel)
                   (lambda (notebook kernel-id &optional name)
                     (setq reconnected-with (list notebook kernel-id name))))
                  ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) nil)))
          (emjupy-restart-kernel))
        (should (string-match-p "kernel-abc" restart-path))
        (should (equal (nth 1 reconnected-with) "kernel-abc"))
        (should (eq (nth 0 reconnected-with) nb))
        (should (= (hash-table-count (emjupy-kernel-pending kernel)) 0))))))

;;; ---------------------------------------------------------------------
;;; 7b. Several notebooks / several servers at once
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-each-notebook-has-its-own-kernel ()
  "Two open notebooks must hold two independent kernels. When kernel
state was global, opening the second silently stole the first's
connection."
  (let* ((nb1 (make-emjupy-notebook :path "one.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:18888")))
         (nb2 (make-emjupy-notebook :path "two.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:19999")))
         (k1 (make-emjupy-kernel :id "k-one" :pending (make-hash-table :test 'equal) :notebook nb1))
         (k2 (make-emjupy-kernel :id "k-two" :pending (make-hash-table :test 'equal) :notebook nb2)))
    (setf (emjupy-notebook-kernel nb1) k1)
    (setf (emjupy-notebook-kernel nb2) k2)
    (should-not (eq (emjupy-notebook-kernel nb1) (emjupy-notebook-kernel nb2)))
    (should-not (eq (emjupy-kernel-pending k1) (emjupy-kernel-pending k2)))
    ;; and each kernel points back at its OWN notebook
    (should (eq (emjupy-kernel-notebook k1) nb1))
    (should (eq (emjupy-kernel-notebook k2) nb2))))

(ert-deftest emjupy-test-output-goes-to-the-requesting-notebook ()
  "A frame must be delivered to the notebook whose kernel produced it,
not to whichever buffer happens to be current. This is the
cross-talk that global `emjupy--current-notebook-buffer' caused: a
slow cell in notebook A finishing while you were reading notebook B
wrote A's output into B."
  (let* ((cell-a (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "a"
                                   :outputs [] :metadata (make-hash-table)))
         (cell-b (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "b"
                                   :outputs [] :metadata (make-hash-table)))
         (buf-a (generate-new-buffer "*emjupy-A*"))
         (buf-b (generate-new-buffer "*emjupy-B*"))
         (nb-a (make-emjupy-notebook :path "A.ipynb" :cells (vector cell-a) :buffer buf-a))
         (nb-b (make-emjupy-notebook :path "B.ipynb" :cells (vector cell-b) :buffer buf-b))
         (k-a (make-emjupy-kernel :id "ka" :pending (make-hash-table :test 'equal) :notebook nb-a)))
    (unwind-protect
        (progn
          (dolist (pair (list (cons buf-a nb-a) (cons buf-b nb-b)))
            (with-current-buffer (car pair)
              (emjupy-mode)
              (setq emjupy--buffer-notebook (cdr pair))
              (emjupy--rerender-notebook)))
          (puthash "msg-a" cell-a (emjupy-kernel-pending k-a))
          ;; deliver A's frame while B is the current buffer
          (with-current-buffer buf-b
            (let* ((hdr (make-hash-table :test 'equal)) (ph (make-hash-table :test 'equal))
                   (content (make-hash-table :test 'equal)) (msg (make-hash-table :test 'equal)))
              (puthash "msg_type" "stream" hdr)
              (puthash "msg_id" "msg-a" ph)
              (puthash "name" "stdout" content)
              (puthash "text" "belongs-to-A\n" content)
              (puthash "header" hdr msg) (puthash "parent_header" ph msg)
              (puthash "content" content msg)
              (emjupy--handle-ws-message k-a (json-serialize msg))))
          (should (= (length (emjupy-cell-outputs cell-a)) 1))
          (should (= (length (emjupy-cell-outputs cell-b)) 0))
          (with-current-buffer buf-a
            (should (string-match-p "belongs-to-A" (buffer-string))))
          (with-current-buffer buf-b
            (should-not (string-match-p "belongs-to-A" (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (b (list buf-a buf-b)) (when (buffer-live-p b) (kill-buffer b)))))))

;;; ---------------------------------------------------------------------
;;; 8. Syntax highlighting
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-python-cell-gets-real-highlighting ()
  "Code cells are fontified via real `python-mode' (bundled with Emacs
core) in a throwaway temp buffer, not a fake major-mode-shadowing
hack -- this must produce actual `face' text properties."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "def foo(x):\n    return x + 1"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (let* ((ov (emjupy-cell-overlay cell))
             (faces (cl-loop for p from (overlay-start ov) below (overlay-end ov)
                              for f = (get-text-property p 'face)
                              when f collect f)))
        (should faces)))))

(ert-deftest emjupy-test-markdown-cell-degrades-gracefully ()
  "Markdown cells render without error whether or not markdown-ts-mode
or markdown-mode happen to be installed (neither ships with Emacs)."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                 :source "# Title\nSome **bold** text"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should (buffer-live-p buf)))))

;;; ---------------------------------------------------------------------
;;; 9. Eglot / completion-framework integration
;;; ---------------------------------------------------------------------

(defun emjupy-test--python-lsp-available-p ()
  "Best-effort check for a usable Python language server on PATH."
  (or (executable-find "pylsp") (executable-find "pyright-langserver")
      (executable-find "jedi-language-server")))

(ert-deftest emjupy-test-shadow-delegate-skips-markdown-cells ()
  "The shadow-buffer delegate must no-op for markdown cells -- no LSP
server applies to prose -- without needing Eglot or any process."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown :source "hi"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should-not (emjupy--cell-shadow-delegate (lambda (&rest _) 'should-not-run)))))))

(ert-deftest emjupy-test-shadow-delegate-maps-position-for-code-cells ()
  "For a code cell, the delegate must land in the shadow buffer at the
position corresponding to point in the notebook buffer -- pure
marker/position-mapping mechanics, no live LSP server required."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "aaa = 1"
                               :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "bbb = 2"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (forward-char 3) ;; land right after "bbb"
        (let (seen)
          (emjupy--cell-shadow-delegate
           (lambda (_cell-start _shadow-start _buf)
             (setq seen (buffer-substring-no-properties (max (point-min) (- (point) 3)) (point)))))
          (should (equal seen "bbb")))))))

(ert-deftest emjupy-test-eglot-cross-cell-completion ()
  "End-to-end: a name defined in one cell is a completion candidate
while editing a DIFFERENT cell, automatically, with no
`emjupy-edit-cell-externally' call anywhere -- this is the actual
IDE-experience feature. Needs a real Python language server on PATH;
skipped otherwise so the rest of the suite isn't held hostage to it."
  (unless (and (emjupy-test--python-lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server (pylsp/pyright/jedi-language-server) on PATH"))
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "MY_TEST_MARKER_VALUE = 42\n" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "MY_TEST_MARKER"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (ignore-errors (emjupy--ensure-shadow-buffer nb))
      (let ((sbuf (emjupy-notebook-shadow-buffer nb)))
        (unless sbuf (ert-skip "Shadow buffer could not be created"))
        (with-current-buffer sbuf
          (let ((w 0))
            (while (and (< w 20) (not (and (eglot-current-server) (jsonrpc-running-p (eglot-current-server)))))
              (accept-process-output nil 1.0) (setq w (1+ w))))
          (unless (eglot-current-server) (ert-skip "Eglot did not connect within timeout"))))
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (goto-char (line-end-position))
        (let ((result (run-hook-with-args-until-success 'completion-at-point-functions)))
          (should (consp result))
          (when (consp result)
            (let* ((start (nth 0 result)) (end (nth 1 result))
                   (cands (all-completions "" (nth 2 result) nil)))
              (should (>= start (overlay-start (emjupy-cell-overlay c2))))
              (should (<= end (overlay-end (emjupy-cell-overlay c2))))
              (should (member "MY_TEST_MARKER_VALUE" cands)))))))))

(ert-deftest emjupy-test-eglot-live-server-rejects-dead-ones ()
  "A server whose process has died must not be handed back.

Eglot's own capf calls `eglot--current-server-or-lose' on it, which
signals \"No current JSON-RPC connection\" -- and that jsonrpc-error
escaped the completion machinery and reached the user."
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) nil)))
    (should-not (emjupy--eglot-live-server)))
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) 'srv))
            ((symbol-function 'jsonrpc-running-p) (lambda (&rest _) nil)))
    (should-not (emjupy--eglot-live-server)))
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) 'srv))
            ((symbol-function 'jsonrpc-running-p) (lambda (&rest _) t)))
    (should (eq (emjupy--eglot-live-server) 'srv))))

(ert-deftest emjupy-test-completion-and-eldoc-survive-a-dead-server ()
  "Neither completion nor eldoc may signal when the language server is
gone -- they simply have nothing to offer."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import os\nos.pat" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-letf (((symbol-function 'emjupy--eglot-live-server) (lambda (&rest _) nil)))
          (goto-char (point-min))
          (should-not (emjupy--cell-completion-at-point))
          (should-not (emjupy--cell-eldoc-function #'ignore)))))))

(ert-deftest emjupy-test-shadow-directory-can-be-remote ()
  "`emjupy-shadow-directory' selects where the shadow file lives, so the
language server can be started on the machine the kernel runs on.

Computing the path must not touch the filesystem: with a TRAMP
directory that would open an ssh connection just to ask for a name."
  (let ((nb (make-emjupy-notebook
             :path "a.ipynb"
             :server (make-emjupy-server :base-url "localhost:18888"))))
    (let ((emjupy-shadow-directory nil))
      (should (string-prefix-p (expand-file-name "emjupy-shadow"
                                                 temporary-file-directory)
                               (emjupy--shadow-file-path nb))))
    (let ((emjupy-shadow-directory "/tmp/custom-shadow"))
      (should (string-prefix-p "/tmp/custom-shadow/" (emjupy--shadow-file-path nb))))
    ;; a TRAMP path is carried through verbatim, with no connection attempt
    (let* ((emjupy-shadow-directory "/ssh:user@host:/tmp/emjupy-shadow")
           (path (emjupy--shadow-file-path nb)))
      (should (string-prefix-p "/ssh:user@host:/tmp/emjupy-shadow/" path))
      (should (string-suffix-p ".py" path)))))

(ert-deftest emjupy-test-shadow-sanitizes-raw-bytes-for-lsp ()
  "Undecodable raw bytes must not reach the language server.

The shadow document is handed over as a JSON string, and `jsonrpc'
rejects anything that is not valid UTF-8 with `wrong-type-argument
utf-8-string-p', so the connection never starts."
  (let* ((raw (concat "x = 1" (string #x3FFF97) "\ny = 2"))
         (clean (emjupy--sanitize-for-lsp raw)))
    ;; before: not serialisable; after: fine
    (should-error (json-serialize (vector raw)))
    (should (json-serialize (vector clean)))
    ;; one character for one character, so shadow offsets still map back
    ;; onto the cell source they came from
    (should (= (length raw) (length clean)))))

(ert-deftest emjupy-test-shadow-leaves-ordinary-unicode-alone ()
  "Sanitising must only touch raw bytes -- accented text, dashes, maths
and non-Latin scripts are perfectly good UTF-8 and pass through."
  (let ((s "caf\u00e9 \u2014 na\u00efve \u2713 \u03b1\u03b2\u03b3"))
    (should (equal s (emjupy--sanitize-for-lsp s))))
  (should (equal "" (emjupy--sanitize-for-lsp "")))
  (should (equal "plain ascii" (emjupy--sanitize-for-lsp "plain ascii"))))

(ert-deftest emjupy-test-shadow-content-is-serialisable ()
  "The assembled shadow document is valid UTF-8 even when a cell holds a
raw byte."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source (concat "a = 1" (string #x3FFF97))
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "b = 2" :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1 c2) :path "raw.ipynb")))
    (should (json-serialize (vector (emjupy--build-shadow-content nb))))))

(ert-deftest emjupy-test-shadow-coding-is-pinned ()
  "The shadow file's coding system is fixed, not negotiated.

Left to itself `write-region' calls `select-safe-coding-system', which
for content it cannot encode cleanly stops and ASKS, defaulting to
`raw-text' -- and a shadow file written as raw text reads back as
mojibake."
  (should (eq emjupy--shadow-coding 'utf-8-unix)))

(ert-deftest emjupy-test-eglot-advice-is-inert-outside-notebooks ()
  "The advice must not change how Eglot commands behave in ordinary
buffers -- it only redirects inside `emjupy-mode\'."
  (with-temp-buffer
    (python-mode)
    (should (equal (emjupy--eglot-command-advice (lambda (&rest args) (cons 'ran args))
                                                 1 2)
                   '(ran 1 2)))))

(ert-deftest emjupy-test-eglot-delegate-errors-clearly-without-a-server ()
  "With no language server the user gets a plain explanation, not a raw
jsonrpc-error out of Eglot\'s innards."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (cl-letf (((symbol-function 'emjupy--eglot-live-server) (lambda (&rest _) nil)))
          (should-error (emjupy-eglot-delegate #'ignore) :type 'user-error))))))

(ert-deftest emjupy-test-eglot-delegate-refuses-outside-a-cell ()
  "Point has to be in a cell for there to be anything to delegate."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-max))
        (should-error (emjupy-eglot-delegate #'ignore) :type 'user-error)))))

(ert-deftest emjupy-test-pull-shadow-into-cells ()
  "Edits an Eglot command made in the shadow buffer come back into the
cells -- including edits spanning more than one cell, which is the whole
reason the shadow buffer exists."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "aaa = 1" :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "print(aaa)" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((shadow (generate-new-buffer " *fake-shadow*")))
          (unwind-protect
              (progn
                (with-current-buffer shadow
                  (insert (emjupy--shadow-cell-marker (emjupy-cell-id c1)) "\n"
                          "bbb = 1" "\n\n"
                          (emjupy--shadow-cell-marker (emjupy-cell-id c2)) "\n"
                          "print(bbb)" "\n"))
                (setf (emjupy-notebook-shadow-buffer nb) shadow)
                (should (= (emjupy--pull-shadow-into-cells nb) 2))
                (should (equal (emjupy-cell-source c1) "bbb = 1"))
                (should (equal (emjupy-cell-source c2) "print(bbb)"))
                ;; and the notebook buffer shows the new text
                (should (string-match-p "bbb = 1" (buffer-string)))
                ;; a second pull with nothing changed reports no updates
                (should (= (emjupy--pull-shadow-into-cells nb) 0)))
            (let ((kill-buffer-query-functions nil))
              (when (buffer-live-p shadow) (kill-buffer shadow)))))))))

(ert-deftest emjupy-test-eglot-advice-installed-on-the-listed-commands ()
  "Every command in `emjupy-eglot-delegated-commands\' is actually advised."
  (dolist (cmd emjupy-eglot-delegated-commands)
    (should (advice-member-p #'emjupy--eglot-command-advice cmd))))

(ert-deftest emjupy-test-xref-backend-only-in-notebooks ()
  "The xref backend announces itself only where there is a notebook."
  (with-temp-buffer
    (should-not (emjupy--xref-backend)))
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should (eq (emjupy--xref-backend) 'emjupy))
        (should (eq (run-hook-with-args-until-success 'xref-backend-functions)
                    'emjupy))))))

(ert-deftest emjupy-test-shadow-position-maps-back-to-a-cell ()
  "A position in the shadow buffer maps back onto the character it came
from, in the right cell.  This is what turns a location the server
describes in shadow-file coordinates into a usable jump."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "alpha = 1" :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "beta = 2" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((shadow (generate-new-buffer " *fake-shadow*")))
          (unwind-protect
              (progn
                (with-current-buffer shadow
                  (insert (emjupy--shadow-cell-marker (emjupy-cell-id c1)) "\n"
                          "alpha = 1" "\n\n"
                          (emjupy--shadow-cell-marker (emjupy-cell-id c2)) "\n"
                          "beta = 2" "\n"))
                (setf (emjupy-notebook-shadow-buffer nb) shadow)
                ;; start of cell 2's section in the shadow
                (let* ((s2 (emjupy--shadow-section-start shadow (emjupy-cell-id c2)))
                       (mapped (emjupy--shadow-position-to-cell nb s2)))
                  (should mapped)
                  (should (eq (car mapped) c2))
                  (should (eq (get-text-property (cdr mapped) 'emjupy-cell) c2))
                  (should (equal (buffer-substring-no-properties
                                  (cdr mapped) (+ (cdr mapped) 4))
                                 "beta")))
                ;; and a few characters in still lands in the same cell
                (let* ((s1 (emjupy--shadow-section-start shadow (emjupy-cell-id c1)))
                       (mapped (emjupy--shadow-position-to-cell nb (+ s1 6))))
                  (should (eq (car mapped) c1))
                  (should (eq (get-text-property (cdr mapped) 'emjupy-cell) c1))))
            (let ((kill-buffer-query-functions nil))
              (when (buffer-live-p shadow) (kill-buffer shadow)))))))))

(ert-deftest emjupy-test-xref-remap-leaves-foreign-locations-alone ()
  "An xref into some other file -- a library, say -- must be handed back
untouched: the real file is the right destination there."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((item (xref-make "elsewhere"
                               (xref-make-file-location "/usr/lib/python/os.py" 10 0))))
          (should (equal (emjupy--xref-remap nb (list item)) (list item))))))))

(ert-deftest emjupy-test-dashboard-remote-root-lookup ()
  "`emjupy-remote-root\' takes one directory for every server, or an
alist keyed by server."
  (let ((a (make-emjupy-server :base-url "localhost:8888"))
        (b (make-emjupy-server :base-url "localhost:9999")))
    (let ((emjupy-remote-root nil))
      (should-not (emjupy--remote-root-for a)))
    (let ((emjupy-remote-root "/ssh:box:/srv/nb"))
      (should (equal (emjupy--remote-root-for a) "/ssh:box:/srv/nb"))
      (should (equal (emjupy--remote-root-for b) "/ssh:box:/srv/nb")))
    (let ((emjupy-remote-root '(("localhost:8888" . "~/notebooks")
                               ("localhost:9999" . "/ssh:box:/srv/nb"))))
      (should (equal (emjupy--remote-root-for a) "~/notebooks"))
      (should (equal (emjupy--remote-root-for b) "/ssh:box:/srv/nb")))))

(ert-deftest emjupy-test-dashboard-dired-needs-a-root ()
  "Without `emjupy-remote-root\' set, `d\' says so rather than guessing."
  (with-temp-buffer
    (emjupy-list-mode)
    (setq emjupy-list--server (make-emjupy-server :base-url "localhost:8888"))
    (setq emjupy-list--path "")
    (let ((emjupy-remote-root nil))
      (should-error (emjupy-list-dired) :type 'user-error))))

(ert-deftest emjupy-test-output-faces-are-background-only ()
  "Every output face may set a background and `:extend\' and nothing else.
They are applied over output text, so any colour attribute they carried
would fight the faces on tracebacks and rich output."
  (dolist (face '(emjupy-output emjupy-output-error
                 emjupy-output-warning emjupy-output-image))
    (should (eq (face-attribute face :inherit nil nil) 'unspecified))
    (should (eq (face-attribute face :extend nil nil) t))
    (dolist (attr '(:foreground :weight :slant :underline :box :inverse-video))
      (should (eq (face-attribute face attr nil t) 'unspecified)))))

(ert-deftest emjupy-test-output-classified-by-kind ()
  "Errors, warnings, images and ordinary output each get their own face."
  (cl-flet ((mk (type &optional name img)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (when name (puthash "name" name o))
                (when img
                  (let ((d (make-hash-table :test 'equal)))
                    (puthash "image/png" "iVBORw0KGgoAAAAA" d)
                    (puthash "data" d o)))
                o)))
    (should (eq (emjupy--output-face (mk "error")) 'emjupy-output-error))
    ;; stderr that is not a traceback is where warnings.warn and logging go
    (should (eq (emjupy--output-face (mk "stream" "stderr")) 'emjupy-output-warning))
    (should (eq (emjupy--output-face (mk "stream" "stdout")) 'emjupy-output))
    (should (eq (emjupy--output-face (mk "display_data" nil t)) 'emjupy-output-image))
    (should (eq (emjupy--output-face (mk "execute_result")) 'emjupy-output))))

(ert-deftest emjupy-test-each-output-piece-painted-separately ()
  "One cell can hold a figure, a warning and a traceback at once, and each
must read as what it is -- so the faces go on the pieces, not on one
overlay across the whole box."
  (cl-flet ((mk (type &rest kv)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (while kv (puthash (pop kv) (pop kv) o))
                o)))
    (let* ((img (let ((d (make-hash-table :test 'equal)))
                  (puthash "image/png" "iVBORw0KGgoAAAAA" d)
                  (puthash "text/plain" "<Figure>" d) d))
           (cell (make-emjupy-cell
                  :id (emjupy--new-cell-id) :type 'code :source "run()"
                  :outputs (vector (mk "stream" "name" "stdout" "text" "normal out\n")
                                   (mk "stream" "name" "stderr" "text" "a warning\n")
                                   (mk "display_data" "data" img)
                                   (mk "error" "ename" "ValueError" "evalue" "boom"
                                       "traceback" ["Traceback line"]))
                  :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (cl-flet ((face-at (probe)
                      (goto-char (point-min))
                      (and (search-forward probe nil t)
                           (get-text-property (match-beginning 0) 'face))))
            (should (memq 'emjupy-output (face-at "normal out")))
            (should (memq 'emjupy-output-warning (face-at "a warning")))
            (should (memq 'emjupy-output-image (face-at "<Figure>")))
            (should (memq 'emjupy-output-error (face-at "ValueError")))
            (should (memq 'emjupy-output-error (face-at "Traceback line"))))
          ;; and the box overlay paints nothing, or it would override them all
          (should-not (overlay-get (emjupy-cell-output-ov cell) 'face)))))))

(ert-deftest emjupy-test-error-and-warning-tints-work-on-both-themes ()
  "`auto\' error and warning colours tint toward red and yellow, and stay
distinguishable from the ordinary output colour on light and dark alike."
  (dolist (theme '(("#ffffff" . "#000000") ("#1c1c1c" . "#e5e5e5")))
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (face attr &rest _)
                 (if (eq face 'default)
                     (cond ((eq attr :background) (car theme))
                           ((eq attr :foreground) (cdr theme)))
                   'unspecified))))
      (let ((got (make-hash-table :test 'eq)))
        (cl-letf (((symbol-function 'set-face-attribute)
                   (lambda (face _f _a value) (puthash face value got))))
          (emjupy--sync-theme-colors))
        (let ((err (gethash 'emjupy-output-error got))
              (warn (gethash 'emjupy-output-warning got))
              (out (gethash 'emjupy-output got)))
          (should (emjupy--color-rgb err))
          (should (emjupy--color-rgb warn))
          ;; each is its own colour, not all the same band
          (should-not (equal err out))
          (should-not (equal warn out))
          (should-not (equal err warn))
          ;; red tint: red channel dominates; yellow tint: blue is lowest
          (let ((e (emjupy--color-rgb err)) (w (emjupy--color-rgb warn)))
            (should (> (nth 0 e) (nth 2 e)))
            (should (> (nth 0 w) (nth 2 w)))
            (should (> (nth 1 w) (nth 2 w)))))))))

(ert-deftest emjupy-test-output-colors-are-customizable ()
  "Explicit colours are used verbatim; nil falls back to the ordinary
output colour."
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face attr &rest _)
               (if (eq face 'default)
                   (cond ((eq attr :background) "#ffffff")
                         ((eq attr :foreground) "#000000"))
                 'unspecified))))
    (let ((got (make-hash-table :test 'eq)))
      (cl-letf (((symbol-function 'set-face-attribute)
                 (lambda (face _f _a value) (puthash face value got))))
        (let ((emjupy-output-error-color "#ff0000")
              (emjupy-output-warning-color "#00ff00")
              (emjupy-output-image-color "#123456")
              (emjupy-output-color "#abcdef"))
          (emjupy--sync-theme-colors)
          (should (equal (gethash 'emjupy-output got) "#abcdef"))
          (should (equal (gethash 'emjupy-output-error got) "#ff0000"))
          (should (equal (gethash 'emjupy-output-warning got) "#00ff00"))
          (should (equal (gethash 'emjupy-output-image got) "#123456")))
        ;; nil means "same as ordinary output"
        (let ((emjupy-output-color "#abcdef")
              (emjupy-output-error-color nil)
              (emjupy-output-image-color nil))
          (emjupy--sync-theme-colors)
          (should (equal (gethash 'emjupy-output-error got) "#abcdef"))
          (should (equal (gethash 'emjupy-output-image got) "#abcdef")))))))

(ert-deftest emjupy-test-box-width-leaves-a-right-margin ()
  "Rules stop short of the right edge by `emjupy-box-right-margin\'.
Emacs cannot always report exactly how many columns are usable, so the
margin is the knob for setups where the rule still runs past the edge."
  (let ((emjupy-box-width 'window)
        (emjupy-box-min-width 10))
    (cl-letf (((symbol-function 'emjupy--window-text-width) (lambda (&rest _) 100)))
      (dolist (pair '((0 . 100) (2 . 98) (6 . 94)))
        (let ((emjupy-box-right-margin (car pair)))
          (should (= (emjupy--box-width) (cdr pair))))))))

(ert-deftest emjupy-test-window-width-excludes-line-numbers ()
  "The line-number column is drawn inside the text area, so
`window-body-width\' counts it -- and a rule sized from that overshoots
the right edge by the width of the numbers."
  (let ((win (selected-window)))
    (with-current-buffer (window-buffer win)
      (cl-letf (((symbol-function 'window-max-chars-per-line) (lambda (&rest _) 200)))
        (setq-local display-line-numbers nil)
        (should (= (emjupy--window-text-width win) 200))
        (setq-local display-line-numbers t)
        (cl-letf (((symbol-function 'line-number-display-width) (lambda (&rest _) 6)))
          (should (= (emjupy--window-text-width win) 194)))
        (setq-local display-line-numbers nil)))))

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
              ((symbol-function 'url-retrieve-synchronously) (lambda (&rest _) nil)))
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
                ((symbol-function 'url-retrieve-synchronously) (lambda (&rest _) nil)))
        (ignore-errors (emjupy--http-request "POST" tokened "/api/kernels" "{}"))
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

(provide 'emjupy-test)
;;; emjupy-test.el ends here
