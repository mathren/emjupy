;;; emjupy-integration-test.el --- Live-server tests for emjupy -*- lexical-binding: t; -*-

;; These tests talk to a REAL Jupyter server over a REAL connection and run
;; REAL Python in a REAL kernel. They are opt-in: without the environment
;; variables below they all `ert-skip', so `emjupy-run-tests.el' stays fast
;; and hermetic by default.
;;
;;   EMJUPY_TEST_URL     host:port of a running Jupyter server. Point this at
;;                       the LOCAL end of an ssh tunnel to exercise the remote
;;                       path (e.g. 127.0.0.1:18888 for
;;                       `ssh -N -L 18888:127.0.0.1:8888 user@host').
;;   EMJUPY_TEST_TOKEN   its token.
;;
;; Rationale for having these at all: the pure/mocked suite cannot catch the
;; things that actually broke here -- an image arriving over the wire that the
;; running Emacs can't display, a WebSocket URL that drops its base path, a
;; saved notebook that no other tool will open. Those only show up against a
;; live server.

(require 'ert)
(require 'emjupy)
(require 'cl-lib)

;;; --------------------------------------------------------------------
;;; Harness
;;; --------------------------------------------------------------------

(defvar emjupy-int--server nil)
(defvar emjupy-int--buffer nil
  "The notebook buffer the current test is driving.")
(defvar emjupy-int--notebook "emjupy-integration.ipynb")

(defun emjupy-int--url () (getenv "EMJUPY_TEST_URL"))
(defun emjupy-int--token () (or (getenv "EMJUPY_TEST_TOKEN") ""))

(defun emjupy-int--skip-unless-live ()
  "Skip the calling test unless a live Jupyter server is reachable."
  (unless (emjupy-int--url)
    (ert-skip "EMJUPY_TEST_URL not set -- live-server tests are opt-in"))
  (setq emjupy--current-server
        (make-emjupy-server :base-url (emjupy-int--url) :token (emjupy-int--token)))
  (setq emjupy-int--server emjupy--current-server)
  (unless (condition-case nil
              (emjupy--http-request "GET" emjupy--current-server "/api/status")
            (error nil))
    (ert-skip (format "No Jupyter server reachable at %s" (emjupy-int--url)))))

(defun emjupy-int--pump (seconds &optional until)
  "Run the event loop for SECONDS, stopping early when UNTIL returns non-nil.
Batch Emacs does not process WebSocket traffic unless something waits
on process output, so every assertion about kernel replies has to pump."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (< (float-time) deadline)
                (not (and until (funcall until))))
      (accept-process-output nil 0.1))
    (and until (funcall until))))

(defun emjupy-int--fresh-notebook ()
  "Create (or overwrite) the scratch notebook on the server and open it."
  (let ((payload (make-hash-table :test 'equal))
        (content (make-hash-table :test 'equal)))
    (puthash "cells" (vector) content)
    (puthash "metadata" (make-hash-table :test 'equal) content)
    (puthash "nbformat" 4 content)
    (puthash "nbformat_minor" 5 content)
    (puthash "type" "notebook" payload)
    (puthash "format" "json" payload)
    (puthash "content" content payload)
    (emjupy--http-request "PUT" emjupy--current-server
                          (concat "/api/contents/" emjupy-int--notebook)
                          (json-serialize payload)))
  (setq emjupy-int--buffer (emjupy-open-notebook emjupy-int--notebook)))

(defun emjupy-int--connect-kernel (&optional buffer)
  "Spawn a kernel for BUFFER's notebook and wait for its WebSocket."
  (with-current-buffer (or buffer emjupy-int--buffer)
    (emjupy--spawn-and-connect-kernel (emjupy--notebook))
    (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p)))))

(defun emjupy-int--run (code &optional seconds)
  "Put CODE in the notebook's first cell, execute it, wait for the reply.
Returns the cell. Writes through the BUFFER, because
`emjupy-execute-cell-at-point' re-syncs every cell from buffer text
first -- setting the struct alone would be silently overwritten."
  (with-current-buffer emjupy-int--buffer
    (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
           (cell (aref cells 0)))
      (setf (emjupy-cell-source cell) code)
      (setf (emjupy-cell-outputs cell) [])
      (setf (emjupy-cell-exec-count cell) nil)
      (emjupy--rerender-notebook cell)
      (goto-char (overlay-start (emjupy-cell-overlay cell)))
      (emjupy-execute-cell-at-point)
      ;; execute_reply sets exec-count; that's our completion signal.
      (emjupy-int--pump (or seconds 60)
                        (lambda () (numberp (emjupy-cell-exec-count cell))))
      cell)))

(defun emjupy-int--output-types (cell)
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           collect (gethash "output_type" o)))

(defun emjupy-int--mime-keys (cell)
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           append (when (gethash "data" o)
                    (cl-loop for k being the hash-keys of (gethash "data" o) collect k))))

(defmacro emjupy-int--with-live-kernel (&rest body)
  "Skip unless live; set up a notebook + kernel; run BODY; tear down."
  `(progn
     (emjupy-int--skip-unless-live)
     (emjupy-int--fresh-notebook)
     (with-current-buffer emjupy-int--buffer
       (goto-char (point-min))
       (emjupy-insert-cell-below))
     (unless (emjupy-int--connect-kernel)
       (ert-skip "Kernel WebSocket did not open in time"))
     (unwind-protect (progn ,@body)
       (when (buffer-live-p emjupy-int--buffer)
         (with-current-buffer emjupy-int--buffer
           (emjupy--disconnect-kernel (emjupy--notebook))))
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p emjupy-int--buffer)
           (kill-buffer emjupy-int--buffer))))))

;;; --------------------------------------------------------------------
;;; 1. Remote kernel through an SSH tunnel
;;; --------------------------------------------------------------------

(ert-deftest emjupy-int-tunnel-rest-api ()
  "The REST half of the transport works through whatever is in front of
the server (here: an ssh -L tunnel)."
  (emjupy-int--skip-unless-live)
  (let ((status (emjupy--http-request "GET" emjupy--current-server "/api/status")))
    (should (hash-table-p status))
    (should (gethash "started" status)))
  (let ((contents (emjupy--http-request "GET" emjupy--current-server "/api/contents")))
    (should (gethash "content" contents))))

(ert-deftest emjupy-int-tunnel-websocket-and-execute ()
  "A kernel spawned over the tunnel accepts an execute_request and
streams stdout back -- the end-to-end claim the README makes about
using the browser's transport instead of ZMQ."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "print('hello from the tunnel')")))
     (should (member "stream" (emjupy-int--output-types cell)))
     (should (string-match-p
              "hello from the tunnel"
              (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                         (append (emjupy-cell-outputs cell) nil) ""))))))

(ert-deftest emjupy-int-kernel-session-is-persistent ()
  "State set by one execution is visible to the next -- the whole point
of a persistent kernel (load a big dataset once, reuse it)."
  (emjupy-int--with-live-kernel
   (emjupy-int--run "PERSISTED = 4242")
   (let ((cell (emjupy-int--run "print(PERSISTED)")))
     (should (string-match-p
              "4242"
              (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                         (append (emjupy-cell-outputs cell) nil) ""))))))

(ert-deftest emjupy-int-kernel-survives-websocket-reconnect ()
  "Dropping the WebSocket and reconnecting to the SAME kernel keeps the
session state -- this is the `resilient to connection drops' claim
(remote kernel keeps running, reconnect emacs)."
  (emjupy-int--with-live-kernel
   (emjupy-int--run "SURVIVOR = 7")
   (with-current-buffer emjupy-int--buffer
     (let* ((nb (emjupy--notebook))
            (kernel-id (emjupy-kernel-id (emjupy-notebook-kernel nb))))
       (websocket-close (emjupy-kernel-ws (emjupy-notebook-kernel nb)))
       (emjupy-int--pump 3 (lambda () (not (emjupy--ws-live-p))))
       (should-not (emjupy--ws-live-p))
       ;; reconnect to the SAME kernel, as emjupy-reconnect-kernel does
       (emjupy-reconnect-kernel)
       (should (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))
       (should (equal (emjupy-kernel-id (emjupy-notebook-kernel nb)) kernel-id))))
   ;; state set before the drop is still there afterwards
   (let ((cell (emjupy-int--run "print(SURVIVOR)")))
     (should (string-match-p
              "7" (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                             (append (emjupy-cell-outputs cell) nil) ""))))))

;;; --------------------------------------------------------------------
;;; 2. Python support: results, tracebacks, execution counts
;;; --------------------------------------------------------------------

(ert-deftest emjupy-int-python-execute-result ()
  "A bare expression comes back as execute_result with a text/plain repr."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "40 + 2")))
     (should (member "execute_result" (emjupy-int--output-types cell)))
     (should (member "text/plain" (emjupy-int--mime-keys cell)))
     (should (numberp (emjupy-cell-exec-count cell))))))

(ert-deftest emjupy-int-python-traceback ()
  "An exception renders as an error output carrying the real traceback."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "1/0")))
     (should (member "error" (emjupy-int--output-types cell)))
     (let ((err (cl-find-if (lambda (o) (string= (gethash "output_type" o) "error"))
                            (append (emjupy-cell-outputs cell) nil))))
       (should (equal (gethash "ename" err) "ZeroDivisionError"))
       (should (> (length (gethash "traceback" err)) 0)))
     ;; and it must actually appear in the buffer, ANSI colour codes stripped
     (with-current-buffer emjupy-int--buffer
       (should (string-match-p "ZeroDivisionError" (buffer-string)))
       (should-not (string-match-p "\033\\[" (buffer-string)))))))

(ert-deftest emjupy-int-python-stdlib-and-multiline ()
  "Multi-line code with imports executes as one unit."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "import json, sys\nd = {'a': [1,2,3]}\nprint(json.dumps(d))")))
     (should (string-match-p
              "{\"a\": \\[1, 2, 3\\]}"
              (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                         (append (emjupy-cell-outputs cell) nil) ""))))))

;;; --------------------------------------------------------------------
;;; 3. Inline images
;;; --------------------------------------------------------------------

(ert-deftest emjupy-int-matplotlib-emits-png ()
  "A matplotlib figure arrives as image/png in a display_data message
and is captured into the cell's outputs."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run
                (concat "%matplotlib inline\n"
                        "import matplotlib.pyplot as plt\n"
                        "fig, ax = plt.subplots()\n"
                        "ax.plot([1,2,3],[2,4,9])\n"
                        "plt.show()")
                120)))
     (should (member "display_data" (emjupy-int--output-types cell)))
     (should (member "image/png" (emjupy-int--mime-keys cell)))
     ;; a real PNG, not an empty placeholder
     (let* ((out (cl-find-if (lambda (o) (and (gethash "data" o)
                                              (gethash "image/png" (gethash "data" o))))
                             (append (emjupy-cell-outputs cell) nil)))
            (b64 (gethash "image/png" (gethash "data" out))))
       (should (> (length b64) 1000))
       (should (string-prefix-p "\211PNG" (base64-decode-string (emjupy--mime-text b64))))))))

(ert-deftest emjupy-int-image-output-is-never-a-blank-box ()
  "However this Emacs is built, an image output must leave something
visible in the buffer.

This is the regression that motivated the fix: on a no-image build
`create-image' SIGNALS, the error is swallowed by websocket.el's
callback guard, and the user gets an empty output box with nothing
logged. Asserted for BOTH capability outcomes so it holds on a
graphical Emacs and on emacs-nox alike."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run
                (concat "%matplotlib inline\n"
                        "import matplotlib.pyplot as plt\n"
                        "fig, ax = plt.subplots()\n"
                        "ax.plot([3,1,4])\n"
                        "plt.show()")
                120)))
     (should (member "image/png" (emjupy-int--mime-keys cell)))
     (should (emjupy-cell-output-ov cell))
     (with-current-buffer emjupy-int--buffer
       (let* ((ov (emjupy-cell-output-ov cell))
              (body (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
              (has-image
               (cl-loop for p from (overlay-start ov) below (overlay-end ov)
                        for d = (get-text-property p 'display)
                        thereis (and (consp d) (eq (car d) 'image)))))
         (if (emjupy--image-displayable-p 'png)
             (should has-image)
           ;; no image support: must explain itself, not render nothing
           (should (string-match-p "image/png" body)))
         ;; either way the output region is non-empty
         (should (> (length (string-trim body)) 0)))))))

;;; --------------------------------------------------------------------
;;; 4. Saving: what we write back must open elsewhere
;;; --------------------------------------------------------------------

(ert-deftest emjupy-int-saved-notebook-is-valid-nbformat ()
  "Round-trip a notebook WITH outputs through the server and validate it
with nbformat itself. Guards the README's core promise of staying
compatible with standard .ipynb for non-emacs users."
  (emjupy-int--with-live-kernel
   (emjupy-int--run "print('saved output')")
   (with-current-buffer emjupy-int--buffer
     (emjupy-save-notebook))
   (let* ((py (or (executable-find "python3") (executable-find "python"))))
     (unless py (ert-skip "No python on PATH to validate with"))
     (let* ((path (expand-file-name emjupy-int--notebook
                                    (or (getenv "EMJUPY_TEST_ROOT") default-directory)))
            (_ (unless (file-exists-p path)
                 (ert-skip (format "Notebook not on this filesystem: %s" path))))
            (out (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process py nil t nil "-c"
                                   (format "import nbformat,sys
nb = nbformat.read(%S, as_version=4)
nbformat.validate(nb)
print('VALID')" path))))))
       (should (string-match-p "VALID" out))))))

;;; --------------------------------------------------------------------
;;; 5. Eglot against a real language server
;;; --------------------------------------------------------------------

(defun emjupy-int--lsp-available-p ()
  (or (executable-find "pylsp") (executable-find "pyright-langserver")
      (executable-find "jedi-language-server")))

(defun emjupy-int--wait-for-eglot (nb)
  "Ensure NB's shadow buffer has a live Eglot server; return it or nil."
  (ignore-errors (emjupy--ensure-shadow-buffer nb))
  (let ((sbuf (emjupy-notebook-shadow-buffer nb)))
    (when sbuf
      (with-current-buffer sbuf
        (emjupy-int--pump 30 (lambda () (and (fboundp 'eglot-current-server)
                                             (eglot-current-server))))
        (and (fboundp 'eglot-current-server) (eglot-current-server))))))

(ert-deftest emjupy-int-eglot-completes-stdlib-in-notebook-buffer ()
  "Completion inside an ordinary notebook cell reaches a real language
server via the shadow buffer -- no `C-c '' required."
  (unless (and (emjupy-int--lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server on PATH"))
  (emjupy-int--skip-unless-live)
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "import os\nos.pat" :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1) :path "eglot-int.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-eglot*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--rerender-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (goto-char (line-end-position 2))   ; end of "os.pat"
          (let ((result (run-hook-with-args-until-success 'completion-at-point-functions)))
            (should (consp result))
            (should (member "path" (all-completions "" (nth 2 result) nil)))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (and (emjupy-notebook-shadow-buffer nb)
                   (buffer-live-p (emjupy-notebook-shadow-buffer nb)))
          (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer nb))))))))

(ert-deftest emjupy-int-eglot-eldoc-returns-hover-info ()
  "eldoc in a code cell gets hover text back from the server. Exercises
`emjupy--cell-eldoc-function', which cannot use Eglot's own eldoc
function (that one only fires when its buffer is visible, and the
shadow buffer deliberately never is)."
  (unless (and (emjupy-int--lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server on PATH"))
  (emjupy-int--skip-unless-live)
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "import json\njson.dumps" :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1) :path "eldoc-int.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-eldoc*"))
         (got nil))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--rerender-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (goto-char (line-end-position 2))
          ;; A cold language server can take a few seconds to index before it
          ;; answers hover, and `emjupy--cell-eldoc-function' deliberately
          ;; swallows errors (eldoc must never throw on a keystroke) -- so
          ;; retry rather than assume the first request lands.
          (cl-loop repeat 15
                   until got
                   do (emjupy--cell-eldoc-function
                       (lambda (doc &rest _) (setq got doc)))
                      (unless got (emjupy-int--pump 1)))
          (should (stringp got))
          (should (string-match-p "dump" got)))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (and (emjupy-notebook-shadow-buffer nb)
                   (buffer-live-p (emjupy-notebook-shadow-buffer nb)))
          (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer nb))))))))

(ert-deftest emjupy-int-eglot-works-for-second-notebook-in-session ()
  "A notebook opened AFTER another one in the same session must get a
working language server too.

Regression test: once any server is running for the shadow project,
Eglot auto-manages a newly visited shadow file from `find-file-hook'
and sends didOpen immediately -- so filling the buffer after visiting
it left the server holding an empty document. `eglot--managed-mode'
reported t and a server was live, so the failure was invisible: hover
just silently returned nothing for every notebook after the first."
  (unless (and (emjupy-int--lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server on PATH"))
  (let (buffers notebooks)
    (unwind-protect
        (let ((results
               (cl-loop for spec in '(("first-nb.ipynb"  "import os\nos.getcwd")
                                      ("second-nb.ipynb" "import json\njson.dumps"))
                        collect
                        (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                                       :source (nth 1 spec) :outputs []
                                                       :metadata (make-hash-table)))
                               (nb (make-emjupy-notebook :cells (vector cell) :path (nth 0 spec)))
                               (buf (generate-new-buffer (format "*emjupy-int-%s*" (nth 0 spec))))
                               (got nil))
                          (push buf buffers) (push nb notebooks)
                          (with-current-buffer buf
                            (emjupy-mode)
                            (setq emjupy--buffer-notebook nb)
                            (setf (emjupy-notebook-buffer nb) buf)
                                              (emjupy--rerender-notebook)
                            (unless (emjupy-int--wait-for-eglot nb)
                              (ert-skip "Eglot did not connect"))
                            (goto-char (overlay-start (emjupy-cell-overlay cell)))
                            (goto-char (line-end-position 2))
                            (cl-loop repeat 15 until got
                                     do (emjupy--cell-eldoc-function
                                         (lambda (doc &rest _) (setq got doc)))
                                        (unless got (emjupy-int--pump 1)))
                            got)))))
          ;; BOTH notebooks, not just the first one opened
          (should (stringp (nth 0 results)))
          (should (stringp (nth 1 results)))
          (should (string-match-p "getcwd" (nth 0 results)))
          (should (string-match-p "dump" (nth 1 results))))
      (let ((kill-buffer-query-functions nil))
        (dolist (b buffers) (when (buffer-live-p b) (kill-buffer b)))
        (dolist (n notebooks)
          (when (and (emjupy-notebook-shadow-buffer n)
                     (buffer-live-p (emjupy-notebook-shadow-buffer n)))
            (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer n)))))))))

(ert-deftest emjupy-int-duplicate-figure-renders-once ()
  "A cell ending in a bare figure gets the SAME png twice from the
kernel -- once as the execute_result repr, once as the inline
backend's display_data. Both must be kept in the cell's outputs (so
the saved .ipynb matches what the kernel sent) but drawn only once."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run
                 (concat "%matplotlib inline\n"
                         "import matplotlib.pyplot as plt\n"
                         "fig, ax = plt.subplots()\n"
                         "ax.plot([1,2,3])\n"
                         "fig")
                 120))
          (with-png (cl-remove-if-not
                     (lambda (o) (and (gethash "data" o)
                                      (gethash "image/png" (gethash "data" o))))
                     (append (emjupy-cell-outputs cell) nil))))
     ;; The kernel really does send it twice -- if it ever stops, this test
     ;; is no longer exercising anything and should be revisited.
     (unless (> (length with-png) 1)
       (ert-skip "Kernel did not emit a duplicate figure; nothing to deduplicate"))
     ;; identical payloads
     (should (equal (gethash "image/png" (gethash "data" (nth 0 with-png)))
                    (gethash "image/png" (gethash "data" (nth 1 with-png)))))
     ;; raw outputs keep both ...
     (should (> (length (emjupy-cell-outputs cell)) 1))
     ;; ... the render collapses them to one
     (should (= 1 (length (cl-remove-if-not
                           #'emjupy--output-image-key
                           (emjupy--outputs-for-render (emjupy-cell-outputs cell))))))
     ;; and on a graphical Emacs exactly one image is actually inserted
     (when (emjupy--image-displayable-p 'png)
       (with-current-buffer emjupy-int--buffer
         (let* ((ov (emjupy-cell-output-ov cell))
                (n 0) (prev nil))
           (cl-loop for p from (overlay-start ov) below (overlay-end ov)
                    for d = (get-text-property p 'display)
                    do (when (and (consp d) (eq (car d) 'image) (not (eq d prev)))
                         (setq n (1+ n)))
                       (setq prev d))
           (should (= n 1))))))))

;;; --------------------------------------------------------------------
;;; 6. Several notebooks, and several servers, at once
;;; --------------------------------------------------------------------

(defun emjupy-int--shutdown-all-kernels (server)
  "Shut down every kernel on SERVER.
Leftovers from earlier tests would otherwise make \"exactly one kernel
is running behind this tunnel\" false, and the login tests would be
asserting against noise."
  (cl-loop for k across (emjupy--http-request "GET" server "/api/kernels")
           do (ignore-errors
                (emjupy--http-request "DELETE" server
                                      (concat "/api/kernels/" (gethash "id" k))))))

(defun emjupy-int--blank-notebook-json ()
  "Return the JSON body for creating an empty notebook."
  (let ((payload (make-hash-table :test 'equal))
        (content (make-hash-table :test 'equal)))
    (puthash "cells" (vector) content)
    (puthash "metadata" (make-hash-table :test 'equal) content)
    (puthash "nbformat" 4 content)
    (puthash "nbformat_minor" 5 content)
    (puthash "type" "notebook" payload)
    (puthash "format" "json" payload)
    (puthash "content" content payload)
    (json-serialize payload)))

(defun emjupy-int--open-with-kernel (server path)
  "Create PATH on SERVER, open it, attach a kernel. Returns the buffer."
  (let ((payload (make-hash-table :test 'equal))
        (content (make-hash-table :test 'equal)))
    (puthash "cells" (vector) content)
    (puthash "metadata" (make-hash-table :test 'equal) content)
    (puthash "nbformat" 4 content)
    (puthash "nbformat_minor" 5 content)
    (puthash "type" "notebook" payload)
    (puthash "format" "json" payload)
    (puthash "content" content payload)
    (emjupy--http-request "PUT" server (concat "/api/contents/" path)
                          (json-serialize payload)))
  (let ((buf (emjupy-open-notebook path server)))
    (with-current-buffer buf
      (goto-char (point-min))
      (emjupy-insert-cell-below)
      (emjupy--spawn-and-connect-kernel (emjupy--notebook))
      (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))
    buf))

(defun emjupy-int--run-in (buf code &optional seconds)
  "Run CODE in BUF's first cell and wait for the reply. Returns the cell."
  (with-current-buffer buf
    ;; A notebook just fetched from the server may legitimately have no
    ;; cells; give it one so there is somewhere to type.
    (when (zerop (length (emjupy-notebook-cells emjupy--buffer-notebook)))
      (goto-char (point-min))
      (emjupy-insert-cell-below))
    (let ((cell (aref (emjupy-notebook-cells emjupy--buffer-notebook) 0)))
      (setf (emjupy-cell-source cell) code)
      (setf (emjupy-cell-outputs cell) [])
      (setf (emjupy-cell-exec-count cell) nil)
      (emjupy--rerender-notebook cell)
      (goto-char (overlay-start (emjupy-cell-overlay cell)))
      (emjupy-execute-cell-at-point)
      (emjupy-int--pump (or seconds 60)
                        (lambda () (numberp (emjupy-cell-exec-count cell))))
      cell)))

(defun emjupy-int--stdout (cell)
  (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
             (append (emjupy-cell-outputs cell) nil) ""))

(ert-deftest emjupy-int-two-notebooks-have-independent-kernels ()
  "Two notebooks open at once from the SAME server must each own a
kernel. A variable defined in one must not be visible in the other,
and finishing a cell in one must not write output into the other."
  (emjupy-int--skip-unless-live)
  (let* ((server emjupy--current-server)
         (a (emjupy-int--open-with-kernel server "multi-a.ipynb"))
         (b (emjupy-int--open-with-kernel server "multi-b.ipynb")))
    (unwind-protect
        (progn
          ;; distinct kernels
          (let ((ka (with-current-buffer a (emjupy-kernel-id (emjupy--kernel))))
                (kb (with-current-buffer b (emjupy-kernel-id (emjupy--kernel)))))
            (should (stringp ka))
            (should (stringp kb))
            (should-not (equal ka kb)))
          ;; both sockets live simultaneously -- opening B must not have
          ;; stolen A's connection
          (should (with-current-buffer a (emjupy--ws-live-p)))
          (should (with-current-buffer b (emjupy--ws-live-p)))
          ;; isolated interpreter state
          (emjupy-int--run-in a "ONLY_IN_A = 111")
          (let ((cell (emjupy-int--run-in b "print('B sees', 'ONLY_IN_A' in dir())")))
            (should (string-match-p "B sees False" (emjupy-int--stdout cell))))
          ;; and A still works after B ran
          (let ((cell (emjupy-int--run-in a "print(ONLY_IN_A)")))
            (should (string-match-p "111" (emjupy-int--stdout cell))))
          ;; output landed only in its own buffer
          (with-current-buffer a (should (string-match-p "111" (buffer-string))))
          (with-current-buffer b (should-not (string-match-p "111" (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (buf (list a b))
          (when (buffer-live-p buf)
            (with-current-buffer buf (emjupy--disconnect-kernel (emjupy--notebook)))
            (kill-buffer buf)))))))

(ert-deftest emjupy-int-two-servers-on-separate-tunnels ()
  "Two Jupyter servers -- typically two ssh tunnels on different local
ports -- must be usable at the same time, each with its own token,
its own XSRF cookie and its own kernels.

Needs EMJUPY_TEST_URL2 (and optionally EMJUPY_TEST_TOKEN2) pointing at
a SECOND server; skipped otherwise."
  (emjupy-int--skip-unless-live)
  (let ((url2 (getenv "EMJUPY_TEST_URL2")))
    (unless url2 (ert-skip "EMJUPY_TEST_URL2 not set -- no second server to test against"))
    (let* ((s1 (emjupy--intern-server (emjupy-int--url) (emjupy-int--token)))
           (s2 (emjupy--intern-server url2 (or (getenv "EMJUPY_TEST_TOKEN2")
                                               (emjupy-int--token))))
           a b)
      (should-not (eq s1 s2))
      ;; each server hands out its own XSRF cookie
      (emjupy--http-request "GET" s1 "/api")
      (emjupy--http-request "GET" s2 "/api")
      (unwind-protect
          (progn
            ;; SAME notebook path on both servers -- the buffers must not collide
            (setq a (emjupy-int--open-with-kernel s1 "same-name.ipynb"))
            (setq b (emjupy-int--open-with-kernel s2 "same-name.ipynb"))
            (should-not (eq a b))
            (should-not (equal (buffer-name a) (buffer-name b)))
            ;; each notebook points at its own server
            (should (eq (with-current-buffer a (emjupy-notebook-server (emjupy--notebook))) s1))
            (should (eq (with-current-buffer b (emjupy-notebook-server (emjupy--notebook))) s2))
            ;; both live at once
            (should (with-current-buffer a (emjupy--ws-live-p)))
            (should (with-current-buffer b (emjupy--ws-live-p)))
            ;; and their kernels are genuinely different processes
            (let ((pid-a (emjupy-int--stdout
                          (emjupy-int--run-in a "import os; print('PID', os.getpid())")))
                  (pid-b (emjupy-int--stdout
                          (emjupy-int--run-in b "import os; print('PID', os.getpid())"))))
              (should (string-match-p "PID" pid-a))
              (should (string-match-p "PID" pid-b))
              (should-not (equal pid-a pid-b)))
            ;; shadow files (and therefore Eglot documents) are distinct too
            (should-not (equal (with-current-buffer a (emjupy--shadow-file-path (emjupy--notebook)))
                               (with-current-buffer b (emjupy--shadow-file-path (emjupy--notebook))))))
        (let ((kill-buffer-query-functions nil))
          (dolist (buf (list a b))
            (when (and buf (buffer-live-p buf))
              (with-current-buffer buf (emjupy--disconnect-kernel (emjupy--notebook)))
              (kill-buffer buf))))))))

(ert-deftest emjupy-int-restart-affects-only-its-own-notebook ()
  "Restarting one notebook's kernel must leave every other notebook's
kernel -- and its interpreter state -- untouched."
  (emjupy-int--skip-unless-live)
  (let* ((server emjupy--current-server)
         (a (emjupy-int--open-with-kernel server "restart-a.ipynb"))
         (b (emjupy-int--open-with-kernel server "restart-b.ipynb")))
    (unwind-protect
        (progn
          (emjupy-int--run-in a "KEEP_A = 1")
          (emjupy-int--run-in b "KEEP_B = 2")
          (with-current-buffer b
            (emjupy-restart-kernel)
            (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))
          ;; B lost its state (it restarted) ...
          (let ((cell (emjupy-int--run-in b "print('KEEP_B' in dir())")))
            (should (string-match-p "False" (emjupy-int--stdout cell))))
          ;; ... but A did not
          (let ((cell (emjupy-int--run-in a "print(KEEP_A)")))
            (should (string-match-p "1" (emjupy-int--stdout cell)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (buf (list a b))
          (when (buffer-live-p buf)
            (with-current-buffer buf (emjupy--disconnect-kernel (emjupy--notebook)))
            (kill-buffer buf)))))))

(ert-deftest emjupy-int-login-adopts-the-running-kernel ()
  "The headline workflow: a kernel is already running behind the tunnel,
`emjupy-login' on that port adopts it, and opening a notebook lands in
that LIVE REPL -- state defined before login is still there.

Also asserts no second kernel is created: spawning one next to the
kernel the user started on the remote host is the failure this is
guarding against."
  (emjupy-int--skip-unless-live)
  (let* ((server emjupy--current-server)
         (path "login-repl.ipynb")
         (_ (emjupy-int--shutdown-all-kernels server))
         (setup (emjupy-int--open-with-kernel server path))
         before-count kernel-id buf)
    (unwind-protect
        (progn
          ;; Establish live state in the kernel behind this port.
          (emjupy-int--run-in setup "SET_BEFORE_LOGIN = 31337")
          (setq kernel-id (with-current-buffer setup (emjupy-kernel-id (emjupy--kernel))))
          ;; Drop our client entirely: the kernel keeps running server-side,
          ;; exactly as it would if Emacs had never connected yet.
          (with-current-buffer setup (emjupy--disconnect-kernel (emjupy--notebook)))
          (let ((kill-buffer-query-functions nil)) (kill-buffer setup))
          (setq setup nil)
          (setq before-count (length (emjupy--http-request "GET" server "/api/kernels")))
          ;; Fresh login on that port.
          (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) path)))
            (setq buf (emjupy-login (emjupy-int--url) (emjupy-int--token))))
          (should (buffer-live-p buf))
          ;; no extra kernel was spawned
          (should (= (length (emjupy--http-request "GET" server "/api/kernels")) before-count))
          (with-current-buffer buf
            ;; attached automatically, to the kernel that was already running
            (should (emjupy--kernel))
            (should (equal (emjupy-kernel-id (emjupy--kernel)) kernel-id))
            (should (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p)))))
          ;; ... and it is the SAME live REPL
          (let ((cell (emjupy-int--run-in buf "print(SET_BEFORE_LOGIN)")))
            (should (string-match-p "31337" (emjupy-int--stdout cell)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (b (list setup buf))
          (when (and b (buffer-live-p b))
            (with-current-buffer b (emjupy--disconnect-kernel (emjupy--notebook)))
            (kill-buffer b)))))))

(ert-deftest emjupy-int-login-on-two-ports-gives-two-kernels ()
  "One port = one kernel. Logging into two tunnels in the same Emacs
must leave two independent REPLs, each reachable from its own notebook.

Needs EMJUPY_TEST_URL2; skipped otherwise."
  (emjupy-int--skip-unless-live)
  (let ((url2 (getenv "EMJUPY_TEST_URL2")))
    (unless url2 (ert-skip "EMJUPY_TEST_URL2 not set -- no second server"))
    (let ((s1 (emjupy--intern-server (emjupy-int--url) (emjupy-int--token)))
          (s2 (emjupy--intern-server url2 (or (getenv "EMJUPY_TEST_TOKEN2")
                                              (emjupy-int--token))))
          buf1 buf2)
      (emjupy-int--shutdown-all-kernels s1)
      (emjupy-int--shutdown-all-kernels s2)
      (unwind-protect
          (progn
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "port-one.ipynb")))
              (emjupy--http-request
               "PUT" (emjupy--intern-server (emjupy-int--url) (emjupy-int--token))
               "/api/contents/port-one.ipynb" (emjupy-int--blank-notebook-json))
              (setq buf1 (emjupy-login (emjupy-int--url) (emjupy-int--token))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "port-two.ipynb")))
              (emjupy--http-request
               "PUT" (emjupy--intern-server url2 (or (getenv "EMJUPY_TEST_TOKEN2")
                                                     (emjupy-int--token)))
               "/api/contents/port-two.ipynb" (emjupy-int--blank-notebook-json))
              (setq buf2 (emjupy-login url2 (or (getenv "EMJUPY_TEST_TOKEN2")
                                                (emjupy-int--token)))))
            (should (buffer-live-p buf1))
            (should (buffer-live-p buf2))
            (dolist (b (list buf1 buf2))
              (with-current-buffer b
                (should (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))))
            ;; two distinct kernels ...
            (let ((k1 (with-current-buffer buf1 (emjupy-kernel-id (emjupy--kernel))))
                  (k2 (with-current-buffer buf2 (emjupy-kernel-id (emjupy--kernel)))))
              (should (stringp k1)) (should (stringp k2))
              (should-not (equal k1 k2)))
            ;; ... in genuinely separate interpreters
            (emjupy-int--run-in buf1 "PORT_ONE_ONLY = 1")
            (let ((cell (emjupy-int--run-in buf2 "print('PORT_ONE_ONLY' in dir())")))
              (should (string-match-p "False" (emjupy-int--stdout cell)))))
        (let ((kill-buffer-query-functions nil))
          (dolist (b (list buf1 buf2))
            (when (and b (buffer-live-p b))
              (with-current-buffer b (emjupy--disconnect-kernel (emjupy--notebook)))
              (kill-buffer b))))))))

(ert-deftest emjupy-int-eglot-rename-works-from-a-notebook ()
  "`eglot-rename\' run in a notebook buffer renames through the shadow
buffer, across every cell -- rather than failing with \"No current
JSON-RPC connection\" because the notebook buffer is not the one Eglot
manages."
  (unless (and (emjupy-int--lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server on PATH"))
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "my_variable = 41" :outputs []
                               :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "print(my_variable + 1)" :outputs []
                               :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1 c2) :path "int-rename.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-rename*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--rerender-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (should (search-forward "my_var" nil t))
          (goto-char (match-beginning 0))
          (eglot-rename "renamed_var")
          ;; both cells updated, not just the one point was in
          (should (equal (emjupy-cell-source c1) "renamed_var = 41"))
          (should (equal (emjupy-cell-source c2) "print(renamed_var + 1)"))
          (should (string-match-p "renamed_var" (buffer-string)))
          (should-not (string-match-p "my_variable" (buffer-string))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (and (emjupy-notebook-shadow-buffer nb)
                   (buffer-live-p (emjupy-notebook-shadow-buffer nb)))
          (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer nb))))))))

(ert-deftest emjupy-int-xref-finds-definitions-across-cells ()
  "M-. from a use in one cell jumps to the definition in another, landing
in the notebook buffer rather than in the shadow file."
  (unless (and (emjupy-int--lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server on PATH"))
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "def my_function(a):\n    return a + 1"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "result = my_function(2)"
                               :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1 c2) :path "int-xref.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-xref*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--rerender-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c2)))
          (should (search-forward "my_function" nil t))
          (goto-char (match-beginning 0))
          (should (eq (run-hook-with-args-until-success 'xref-backend-functions) 'emjupy))
          (let* ((id (xref-backend-identifier-at-point 'emjupy))
                 (defs (xref-backend-definitions 'emjupy id)))
            (should defs)
            (let ((loc (xref-item-location (car defs))))
              ;; remapped into the notebook, not left pointing at the shadow file
              (should (cl-typep loc 'xref-buffer-location))
              (should (eq (xref-buffer-location-buffer loc) buf))
              (let ((pos (xref-buffer-location-position loc)))
                ;; and it lands on the definition, which lives in the OTHER cell
                (should (eq (get-text-property pos 'emjupy-cell) c1))
                (should (string-prefix-p "my_function"
                                         (buffer-substring-no-properties
                                          pos (min (point-max) (+ pos 11)))))))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p buf) (kill-buffer buf))
        (when (and (emjupy-notebook-shadow-buffer nb)
                   (buffer-live-p (emjupy-notebook-shadow-buffer nb)))
          (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer nb))))))))

(ert-deftest emjupy-int-dashboard-lists-kernels-and-notebooks ()
  "The dashboard shows what the server has, and descends into folders."
  (emjupy-int--skip-unless-live)
  (let ((server emjupy--current-server) dash)
    (emjupy--http-request "PUT" server "/api/contents/int-dash.ipynb"
                          (emjupy-int--blank-notebook-json))
    (let ((p (make-hash-table :test 'equal)))
      (puthash "name" "python3" p)
      (emjupy--http-request "POST" server "/api/kernels" (json-serialize p)))
    (unwind-protect
        (with-current-buffer (setq dash (emjupy-server-dashboard server))
          (should (eq major-mode 'emjupy-list-mode))
          (let ((kinds (mapcar (lambda (e) (plist-get (car e) :kind))
                               tabulated-list-entries))
                (names (mapcar (lambda (e) (aref (cadr e) 1))
                               tabulated-list-entries)))
            ;; kernels AND notebooks, in one place
            (should (memq 'kernel kinds))
            (should (memq 'notebook kinds))
            (should (member "int-dash.ipynb" names)))
          ;; opening a notebook row gives a real notebook buffer
          (goto-char (point-min))
          (let ((found nil))
            (while (and (not found) (not (eobp)))
              (if (and (eq (plist-get (tabulated-list-get-id) :kind) 'notebook)
                       (equal (aref (tabulated-list-get-entry) 1) "int-dash.ipynb"))
                  (setq found t)
                (forward-line 1)))
            (should found)
            (let ((nbbuf (emjupy-list-open)))
              (should (bufferp nbbuf))
              (with-current-buffer nbbuf
                (should (equal (emjupy-notebook-path (emjupy--notebook))
                               "int-dash.ipynb")))
              (let ((kill-buffer-query-functions nil)) (kill-buffer nbbuf)))))
      (let ((kill-buffer-query-functions nil))
        (when (buffer-live-p dash) (kill-buffer dash))))))

(provide 'emjupy-integration-test)
;;; emjupy-integration-test.el ends here
