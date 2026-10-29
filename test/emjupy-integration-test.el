;;; emjupy-integration-test.el --- Live-server tests for emjupy -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; These tests run real Python in a real kernel, over a real connection to a
;; Jupyter server.  Opt-in: without the variables below they skip.
;;
;;   EMJUPY_TEST_URL    host:port of a running Jupyter server; the local
;;                      end of an ssh tunnel tests the remote path
;;   EMJUPY_TEST_TOKEN  its token
;;
;; They catch what mocks cannot: an image the running Emacs cannot show, a
;; WebSocket URL losing its base path, a saved notebook other tools refuse.

;;; Code:

(require 'ert)
(require 'emjupy)
(require 'cl-lib)

;;;; Harness

(defvar emjupy-int--server nil
  "The Jupyter server the live tests talk to.")
(defvar emjupy-int--buffer nil
  "The notebook buffer the current test is driving.")
(defvar emjupy-int--notebook "emjupy-integration.ipynb"
  "The scratch notebook, on the server, that the live tests open.")

(defun emjupy-int--url () (getenv "EMJUPY_TEST_URL"))
(defun emjupy-int--token () (or (getenv "EMJUPY_TEST_TOKEN") ""))

(defun emjupy-int--skip-unless-live ()
  "Skip the calling test unless a live Jupyter server is reachable."
  (unless (emjupy-int--url)
    (ert-skip "EMJUPY_TEST_URL not set -- live-server tests are opt-in"))
  (setq emjupy--current-server
        (make-emjupy-server :base-url (emjupy-int--url)
                            :token (emjupy-int--token)))
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

(defun emjupy-int--shows (regexp &optional seconds)
  "Wait up to SECONDS, 30 by default, for this buffer to show REGEXP.
Output is drawn from a timer, after it arrives: checked at once, a slow
machine had not drawn it yet."
  (emjupy-int--pump (or seconds 30)
                    (lambda () (string-match-p regexp (buffer-string)))))

(defun emjupy-int--wait-finished (cell &optional seconds)
  "Wait up to SECONDS, 60 by default, for CELL's run to finish; return CELL.
Finished is the reply and the idle after its last output both come: the
request then leaves the pending table.  The reply alone is not enough --
it can overtake the output it describes.  Called in CELL's notebook."
  (let ((pending (emjupy-kernel-pending
                  (emjupy-notebook-kernel emjupy--buffer-notebook))))
    (emjupy-int--pump (or seconds 60)
                      (lambda () (numberp (emjupy-cell-exec-count cell))))
    (emjupy-int--pump (or seconds 60)
                      (lambda ()
                        (not (cl-loop for v being the hash-values of pending
                                      thereis (eq v cell))))))
  cell)

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
      (emjupy--redraw-notebook cell)
      (goto-char (overlay-start (emjupy-cell-overlay cell)))
      (emjupy-execute-cell-at-point)
      (emjupy-int--wait-finished cell seconds))))

(defun emjupy-int--output-types (cell)
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           collect (gethash "output_type" o)))

(defun emjupy-int--mime-keys (cell)
  (cl-loop for o across (or (emjupy-cell-outputs cell) [])
           append (when (gethash "data" o)
                    (cl-loop for k being the hash-keys of (gethash "data" o)
                             collect k))))

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

;;;; 1. Remote kernel through an SSH tunnel

(ert-deftest emjupy-int-tunnel-rest-api ()
  "The REST half of the transport works through whatever is in front of
the server (here: an ssh -L tunnel)."
  (emjupy-int--skip-unless-live)
  (let ((status (emjupy--http-request "GET" emjupy--current-server
                                      "/api/status")))
    (should (hash-table-p status))
    (should (gethash "started" status)))
  (let ((contents (emjupy--http-request "GET" emjupy--current-server
                                        "/api/contents")))
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
       (should (equal (emjupy-kernel-id (emjupy-notebook-kernel nb))
                      kernel-id))))
   ;; state set before the drop is still there afterwards
   (let ((cell (emjupy-int--run "print(SURVIVOR)")))
     (should (string-match-p
              "7" (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                             (append (emjupy-cell-outputs cell) nil) ""))))))

;;;; 2. Python support: results, tracebacks, execution counts

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
     (let ((err (cl-find-if (lambda (o) (string= (gethash "output_type" o)
                                                 "error"))
                            (append (emjupy-cell-outputs cell) nil))))
       (should (equal (gethash "ename" err) "ZeroDivisionError"))
       (should (> (length (gethash "traceback" err)) 0)))
     ;; and it must actually appear in the buffer, ANSI colour codes stripped
     (with-current-buffer emjupy-int--buffer
       (should (emjupy-int--shows "ZeroDivisionError"))
       (should-not (string-match-p "\033\\[" (buffer-string)))))))

(ert-deftest emjupy-int-python-stdlib-and-multiline ()
  "Multi-line code with imports executes as one unit."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "import json, sys\nd = {'a': [1,2,3]}\nprint(json.dumps(d))")))
     (should (string-match-p
              "{\"a\": \\[1, 2, 3\\]}"
              (mapconcat (lambda (o) (emjupy--mime-text (gethash "text" o)))
                         (append (emjupy-cell-outputs cell) nil) ""))))))

;;;; 3. Inline images

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
                                              (gethash "image/png"
                                                       (gethash "data" o))))
                             (append (emjupy-cell-outputs cell) nil)))
            (b64 (gethash "image/png" (gethash "data" out))))
       (should (> (length b64) 1000))
       (should (string-prefix-p
                "\211PNG" (base64-decode-string (emjupy--mime-text b64))))))))

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
              (body (buffer-substring-no-properties (overlay-start ov)
                                                    (overlay-end ov)))
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

;;;; 4. Saving: what we write back must open elsewhere

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
                                    (or (getenv "EMJUPY_TEST_ROOT")
                                        default-directory)))
            (_ (unless (file-exists-p path)
                 (ert-skip
                  (format "Notebook not on this filesystem: %s" path))))
            (out (with-output-to-string
                   (with-current-buffer standard-output
                     (call-process py nil t nil "-c"
                                   (format "import nbformat,sys
nb = nbformat.read(%S, as_version=4)
nbformat.validate(nb)
print('VALID')" path))))))
       (should (string-match-p "VALID" out))))))

;;;; 5. Eglot against a real language server

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
                               :source "import os\nos.pat" :outputs []
                               :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1) :path "eglot-int.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-eglot*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--redraw-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (goto-char (line-end-position 2))   ; end of "os.pat"
          (let ((result (run-hook-with-args-until-success
                         'completion-at-point-functions)))
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
                               :source "import json\njson.dumps" :outputs []
                               :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1) :path "eldoc-int.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-eldoc*"))
         (got nil))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--redraw-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (goto-char (line-end-position 2))
          ;; A cold language server may take seconds to answer hover, and
          ;; `emjupy--cell-eldoc-function' swallows errors, as eldoc must: so
          ;; retried.
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
                                      ("second-nb.ipynb"
                                       "import json\njson.dumps"))
                        collect
                        (let* ((cell (make-emjupy-cell
                                      :id (emjupy--new-cell-id) :type 'code
                                      :source (nth 1 spec) :outputs []
                                      :metadata (make-hash-table)))
                               (nb (make-emjupy-notebook :cells (vector cell)
                                                         :path (nth 0 spec)))
                               (buf (generate-new-buffer
                                     (format "*emjupy-int-%s*" (nth 0 spec))))
                               (got nil))
                          (push buf buffers) (push nb notebooks)
                          (with-current-buffer buf
                            (emjupy-mode)
                            (setq emjupy--buffer-notebook nb)
                            (setf (emjupy-notebook-buffer nb) buf)
                            (emjupy--redraw-notebook)
                            (unless (emjupy-int--wait-for-eglot nb)
                              (ert-skip "Eglot did not connect"))
                            (goto-char
                             (overlay-start (emjupy-cell-overlay cell)))
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
            (ignore-errors (kill-buffer
                            (emjupy-notebook-shadow-buffer n)))))))))

(ert-deftest emjupy-int-duplicate-figure-renders-once ()
  "A figure the kernel sends twice is kept twice but drawn once.

A cell ending in a bare figure can get it both as the execute_result
repr and as the inline backend's display_data.  Both are kept in the
cell's outputs, so the saved .ipynb matches what the kernel sent, and
each distinct picture is drawn once.

The rule tested is emjupy's, not the kernel's: whether the two copies
are byte-identical depends on matplotlib, matplotlib-inline and the
user's IPython configuration, and two different pictures rightly draw
as two.  So the payloads are compared first, a run with no real
duplicate is skipped as having nothing to test, and
what is asserted is that every distinct picture appears exactly once."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run
                 (concat "%matplotlib inline\n"
                         "import matplotlib.pyplot as plt\n"
                         "fig, ax = plt.subplots()\n"
                         "ax.plot([1,2,3])\n"
                         "fig")
                 120))
          (outputs (emjupy-cell-outputs cell))
          (keys (delq nil (mapcar #'emjupy--output-image-key
                                  (append outputs nil))))
          (distinct (delete-dups (copy-sequence keys))))
     (unless (< (length distinct) (length keys))
       (ert-skip (format "No repeated picture to deduplicate: %d image output(s), %d distinct"
                         (length keys) (length distinct))))
     ;; the render draws each distinct picture exactly once ...
     (let ((rendered (delq nil (mapcar #'emjupy--output-image-key
                                       (emjupy--outputs-to-draw outputs)))))
       (should (equal (sort (mapcar #'cdr rendered) #'string<)
                      (sort (mapcar #'cdr distinct) #'string<)))
       ;; ... while the cell's outputs keep every copy the kernel sent
       (should (> (length keys) (length rendered))))
     ;; and on a graphical Emacs that many images are actually inserted
     (when (emjupy--image-displayable-p 'png)
       (with-current-buffer emjupy-int--buffer
         (let* ((ov (emjupy-cell-output-ov cell))
                (n 0) (prev nil))
           (cl-loop for p from (overlay-start ov) below (overlay-end ov)
                    for d = (get-text-property p 'display)
                    do (when (and (consp d) (eq (car d) 'image)
                                  (not (eq d prev)))
                         (setq n (1+ n)))
                    (setq prev d))
           (should (= n (length distinct)))))))))

;;;; 6. Several notebooks, and several servers, at once

(defun emjupy-int--shutdown-all-kernels (server)
  "Shut down every kernel on SERVER.
Leftovers from earlier tests would otherwise make \"exactly one kernel
is running behind this tunnel\" false, and the login tests would be
asserting against noise."
  (cl-loop for k across (emjupy--http-request "GET" server "/api/kernels")
           do (ignore-errors
                (emjupy--http-request "DELETE" server
                                      (concat "/api/kernels/"
                                              (gethash "id" k))))))

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
      (emjupy--redraw-notebook cell)
      (goto-char (overlay-start (emjupy-cell-overlay cell)))
      (emjupy-execute-cell-at-point)
      (emjupy-int--wait-finished cell seconds))))

(defun emjupy-int--await-ready (buf &optional tries)
  "Run a trivial cell in BUF until the kernel answers, or give up.

A kernel that has just restarted accepts a connection before it will
execute anything: the socket is live, the request is sent, and the reply
that describes it arrives while the output it produced does not.  Waiting
longer does not help -- that output was never sent.  So readiness is
established by asking for something and seeing it come back."
  (let ((left (or tries 10))
        (ready nil))
    (while (and (> left 0) (not ready))
      (setq left (1- left))
      (let ((cell (emjupy-int--run-in buf "print('emjupy-ready')" 20)))
        (when (string-match-p "emjupy-ready" (emjupy-int--stdout cell))
          (setq ready t))))
    ready))

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
                (kb (with-current-buffer b
                      (emjupy-kernel-id (emjupy--kernel)))))
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
          (with-current-buffer a (should (emjupy-int--shows "111")))
          (with-current-buffer b (should-not (string-match-p "111"
                                                             (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (buf (list a b))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (emjupy--disconnect-kernel (emjupy--notebook)))
            (kill-buffer buf)))))))

(ert-deftest emjupy-int-two-servers-on-separate-tunnels ()
  "Two Jupyter servers -- typically two ssh tunnels on different local
ports -- must be usable at the same time, each with its own token,
its own XSRF cookie and its own kernels.

Needs EMJUPY_TEST_URL2 (and optionally EMJUPY_TEST_TOKEN2) pointing at
a SECOND server; skipped otherwise."
  (emjupy-int--skip-unless-live)
  (let ((url2 (getenv "EMJUPY_TEST_URL2")))
    (unless url2
      (ert-skip "EMJUPY_TEST_URL2 not set -- no second server to test against"))
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
            ;; SAME notebook path on both servers -- the buffers must not
            ;; collide
            (setq a (emjupy-int--open-with-kernel s1 "same-name.ipynb"))
            (setq b (emjupy-int--open-with-kernel s2 "same-name.ipynb"))
            (should-not (eq a b))
            (should-not (equal (buffer-name a) (buffer-name b)))
            ;; each notebook points at its own server
            (should (eq (with-current-buffer a
                          (emjupy-notebook-server (emjupy--notebook))) s1))
            (should (eq (with-current-buffer b
                          (emjupy-notebook-server (emjupy--notebook))) s2))
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
            (should-not (equal (with-current-buffer a
                                 (emjupy--shadow-file-path (emjupy--notebook)))
                               (with-current-buffer b (emjupy--shadow-file-path (emjupy--notebook))))))
        (let ((kill-buffer-query-functions nil))
          (dolist (buf (list a b))
            (when (and buf (buffer-live-p buf))
              (with-current-buffer buf
                (emjupy--disconnect-kernel (emjupy--notebook)))
              (kill-buffer buf))))))))

(ert-deftest emjupy-int-restart-affects-only-its-own-notebook ()
  :tags '(:unstable)
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
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
              (emjupy-restart-kernel))
            (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))
          ;; and wait until it will actually run something
          (should (emjupy-int--await-ready b))
          ;; B lost its state (it restarted) ...
          (let ((cell (emjupy-int--run-in b "print('KEEP_B' in dir())")))
            (should (string-match-p "False" (emjupy-int--stdout cell))))
          ;; ... but A did not
          (let ((cell (emjupy-int--run-in a "print(KEEP_A)")))
            (should (string-match-p "1" (emjupy-int--stdout cell)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (buf (list a b))
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (emjupy--disconnect-kernel (emjupy--notebook)))
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
          (setq kernel-id (with-current-buffer setup
                            (emjupy-kernel-id (emjupy--kernel))))
          ;; Drop our client entirely: the kernel keeps running server-side,
          ;; exactly as it would if Emacs had never connected yet.
          (with-current-buffer setup
            (emjupy--disconnect-kernel (emjupy--notebook)))
          (let ((kill-buffer-query-functions nil)) (kill-buffer setup))
          (setq setup nil)
          (setq before-count (length (emjupy--http-request "GET" server
                                                           "/api/kernels")))
          ;; Fresh login on that port.
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (&rest _) path)))
            (setq buf (emjupy-login (emjupy-int--url) (emjupy-int--token))))
          (should (buffer-live-p buf))
          ;; no extra kernel was spawned
          (should (= (length (emjupy--http-request "GET" server
                                                   "/api/kernels"))
                     before-count))
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
            (with-current-buffer b
              (emjupy--disconnect-kernel (emjupy--notebook)))
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
               "PUT" (emjupy--intern-server (emjupy-int--url)
                                            (emjupy-int--token))
               "/api/contents/port-one.ipynb" (emjupy-int--blank-notebook-json))
              (setq buf1 (emjupy-login (emjupy-int--url) (emjupy-int--token))))
            (cl-letf (((symbol-function 'completing-read)
                       (lambda (&rest _) "port-two.ipynb")))
              (emjupy--http-request
               "PUT" (emjupy--intern-server url2
                                            (or (getenv "EMJUPY_TEST_TOKEN2")
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
            (let ((k1 (with-current-buffer buf1
                        (emjupy-kernel-id (emjupy--kernel))))
                  (k2 (with-current-buffer buf2
                        (emjupy-kernel-id (emjupy--kernel)))))
              (should (stringp k1)) (should (stringp k2))
              (should-not (equal k1 k2)))
            ;; ... in genuinely separate interpreters
            (emjupy-int--run-in buf1 "PORT_ONE_ONLY = 1")
            (let ((cell (emjupy-int--run-in buf2
                                            "print('PORT_ONE_ONLY' in dir())")))
              (should (string-match-p "False" (emjupy-int--stdout cell)))))
        (let ((kill-buffer-query-functions nil))
          (dolist (b (list buf1 buf2))
            (when (and b (buffer-live-p b))
              (with-current-buffer b
                (emjupy--disconnect-kernel (emjupy--notebook)))
              (kill-buffer b))))))))

(ert-deftest emjupy-int-eglot-rename-works-from-a-notebook ()
  "`eglot-rename' run in a notebook buffer renames through the shadow
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
         (nb (make-emjupy-notebook :cells (vector c1 c2)
                                   :path "int-rename.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-rename*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--redraw-notebook)
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
         (nb (make-emjupy-notebook :cells (vector c1 c2)
                                   :path "int-xref.ipynb"))
         (buf (generate-new-buffer "*emjupy-int-xref*")))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--redraw-notebook)
          (unless (emjupy-int--wait-for-eglot nb)
            (ert-skip "Eglot did not connect"))
          (goto-char (overlay-start (emjupy-cell-overlay c2)))
          (should (search-forward "my_function" nil t))
          (goto-char (match-beginning 0))
          (should (eq (run-hook-with-args-until-success 'xref-backend-functions)
                      'emjupy))
          (let* ((id (xref-backend-identifier-at-point 'emjupy))
                 (defs (xref-backend-definitions 'emjupy id)))
            (should defs)
            (let ((loc (xref-item-location (car defs))))
              ;; remapped into the notebook, not left pointing at the shadow
              ;; file
              (should (cl-typep loc 'xref-buffer-location))
              (should (eq (xref-buffer-location-buffer loc) buf))
              (let ((pos (xref-buffer-location-position loc)))
                ;; and it lands on the definition, which lives in the OTHER cell
                (should (eq (get-text-property pos 'emjupy-cell) c1))
                (should (string-prefix-p "my_function"
                                         (buffer-substring-no-properties
                                          pos (min (point-max)
                                                   (+ pos 11)))))))))
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
                       (equal (aref (tabulated-list-get-entry) 1)
                              "int-dash.ipynb"))
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

(ert-deftest emjupy-int-websocket-server-manages-the-shadow ()
  "Connecting over the WebSocket leaves the shadow buffer managed by it.

Connecting is not enough.  Eglot decides which buffers a server manages
through hooks tied to the project a buffer belongs to, and a shadow
buffer in a transient project is not always recognised: on Emacs 30 the
connection can succeed with the buffer unmanaged, every request then
falling through to a local language server, which answers about the
wrong files.

The class of the attached server is what is asserted, not merely that
one is attached: a local server is still a server, and that is exactly
the state this is here to catch."
  (emjupy-int--skip-unless-live)
  (emjupy-int--with-live-kernel
   (with-current-buffer emjupy-int--buffer
     (let ((nb (emjupy--notebook)))
       ;; the kernel reports its directory a moment after connecting
       (emjupy-int--pump 10 (lambda () (emjupy-notebook-kernel-cwd nb)))
       (should (emjupy-notebook-kernel-cwd nb))
       (emjupy-start-language-support nb)
       (emjupy-int--pump
        20 (lambda ()
             (let ((sb (emjupy-notebook-shadow-buffer nb)))
               (and (buffer-live-p sb)
                    (with-current-buffer sb
                      (ignore-errors (eglot-current-server)))))))
       (let ((sb (emjupy-notebook-shadow-buffer nb)))
         (should (buffer-live-p sb))
         (with-current-buffer sb
           (let ((attached (ignore-errors (eglot-current-server))))
             (should attached)
             (should (object-of-class-p attached 'emjupy-eglot-server)))))))))

(ert-deftest emjupy-int-listing-is-sorted-by-recency ()
  "The dashboard, against a real server, lists newest first per kind."
  (emjupy-int--skip-unless-live)
  (let* ((dir "emjupy-int-sort")
         (server emjupy--current-server))
    (cl-flet ((put-file (name body)
                (let ((payload (make-hash-table :test 'equal)))
                  (puthash "type" "file" payload)
                  (puthash "format" "text" payload)
                  (puthash "content" body payload)
                  (emjupy--http-request "PUT" server
                                        (concat "/api/contents/" dir "/" name)
                                        (json-serialize payload)))))
      ;; a directory, then three files written oldest to newest
      (let ((mk (make-hash-table :test 'equal)))
        (puthash "type" "directory" mk)
        (ignore-errors
          (emjupy--http-request "PUT" server (concat "/api/contents/" dir)
                                (json-serialize mk))))
      (dolist (name '("zulu.py" "middle.py" "alpha.py"))
        (put-file name "x = 1\n")
        (sleep-for 1.1))
      ;; alpha was written last, so it comes first despite the name
      (let* ((rows (emjupy--list-entries server dir))
             (files (seq-filter (lambda (r)
                                  (eq (plist-get (car r) :kind) 'file)) rows))
             (names (mapcar (lambda (r)
                              (substring-no-properties (aref (cadr r) 1)))
                            files)))
        (should (member "alpha.py" names))
        (should (equal (seq-take names 3) '("alpha.py" "middle.py"
                                            "zulu.py")))))))

(ert-deftest emjupy-int-progress-bar-fits-the-cell ()
  "A progress bar written without any width drawn inside the cell.

The kernel is told the box width in COLUMNS, which is how tqdm and
anything using `shutil.get_terminal_size' learn how wide to draw when
their output is not a terminal; and whatever still comes out wider is
folded where the box ends."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run
                 (concat "import os, shutil\n"
                         "print('COLUMNS', os.environ.get('COLUMNS'))\n"
                         "print('TERM', shutil.get_terminal_size().columns)\n"
                         "import sys\n"
                         "sys.stderr.write('100%|' + '\u2588' * 300 + '| 9/9\\n')")
                 60))
          (out (emjupy-int--stdout cell)))
     (with-current-buffer emjupy-int--buffer
       (let ((width (emjupy--box-width)))
         ;; the kernel was told
         (should (string-match-p (format "COLUMNS %d" width) out))
         (should (string-match-p (format "TERM %d" width) out))
         ;; and nothing drawn is wider than the box
         (let ((ov (emjupy-cell-output-ov cell)) (widest 0))
           (save-excursion
             (goto-char (overlay-start ov))
             (while (< (point) (overlay-end ov))
               (let ((w 0) (p (line-beginning-position)))
                 (while (< p (line-end-position))
                   (unless (get-text-property p 'emjupy-pad)
                     (setq w (+ w (char-width (char-after p)))))
                   (setq p (1+ p)))
                 (setq widest (max widest w)))
               (forward-line 1)))
           (should (<= widest width))))))))

(ert-deftest emjupy-int-closing-a-notebook-releases-its-connections ()
  "Closing a notebook closes the connections it opened, quickly.

Every notebook had its own language-server connection and shadow
buffer, and closing it left both running -- a WebSocket, a placeholder
process and a jupyter-lsp session on the server for every notebook ever
opened.  Closing released nothing until this; then, done naively, it
waited seconds for processes to drain, and Eglot started a replacement
for each server it saw die."
  (skip-unless (getenv "EMJUPY_TEST_URL"))
  (let* ((server (emjupy--intern-server (getenv "EMJUPY_TEST_URL")
                                        (or (getenv "EMJUPY_TEST_TOKEN") "")))
         (root (or (getenv "EMJUPY_TEST_ROOT") default-directory))
         (count (lambda ()
                  (list (length (seq-filter #'process-live-p (process-list)))
                        (length (seq-filter (lambda (b)
                                              (string-match-p "emjupy_"
                                                              (buffer-name b)))
                                            (buffer-list)))))))
    ;; Settle first: a test before this one may still be starting its
    ;; notebook's language server -- the kernel says where it runs after the
    ;; test has finished -- and that would be counted as this one's.
    (let ((last nil) (stable 0))
      ;; steady for a second and a half, or fifteen at most
      (cl-loop repeat 30 until (>= stable 3)
               do (accept-process-output nil 0.5)
               (let ((now (funcall count)))
                 (setq stable (if (equal now last) (1+ stable) 0)
                       last now))))
    (let ((before (funcall count)))
      (dotimes (i 3)
        (let* ((c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                    :source "import os"
                                    :outputs []
                                    :metadata (make-hash-table :test 'equal)))
               (nb (make-emjupy-notebook :cells (vector c)
                                         :path (format "release-%d.ipynb" i)
                                         :server server :kernel-cwd root))
               (buf (generate-new-buffer (format "*release-%d*" i))))
          (with-current-buffer buf
            (emjupy-mode)
            (setq emjupy--buffer-notebook nb)
            (setf (emjupy-notebook-buffer nb) buf)
            (emjupy--redraw-notebook)
            (emjupy-start-language-support nb)
            (ignore-errors (emjupy--ensure-shadow-buffer nb))
            (dotimes (_ 50) (accept-process-output nil 0.1)))
          (let ((t0 (float-time)))
            (kill-buffer buf)
            (should (< (- (float-time) t0) 0.5)))
          (dotimes (_ 20) (accept-process-output nil 0.1))))
      (should (equal (funcall count) before)))))

;;;; Network faults, through tools/faultproxy.py

(defconst emjupy-int--proxy-script
  (expand-file-name "../tools/faultproxy.py"
                    (file-name-directory (or load-file-name buffer-file-name)))
  "The proxy that misbehaves on command.")

(defvar emjupy-int--proxy nil
  "The running fault proxy, as (PROCESS PORT CONTROL-PORT).")

(defun emjupy-int--start-proxy ()
  "Start the fault proxy in front of the test server; return its port."
  (let* ((target (split-string (emjupy-int--url) ":"))
         (buf (generate-new-buffer " *faultproxy*"))
         (proc (make-process :name "faultproxy" :buffer buf :noquery t
                             :command (list "python3" emjupy-int--proxy-script
                                            (car target) (cadr target)))))
    (with-timeout (10 (error "Fault proxy did not start"))
      (while (not (with-current-buffer buf
                    (goto-char (point-min))
                    (re-search-forward "listening \\([0-9]+\\) control \\([0-9]+\\)" nil t)))
        (accept-process-output proc 0.1)))
    (with-current-buffer buf
      (setq emjupy-int--proxy (list proc (string-to-number (match-string 1))
                                    (string-to-number (match-string 2)))))
    (nth 1 emjupy-int--proxy)))

(defun emjupy-int--proxy-say (command)
  "Send COMMAND to the fault proxy and wait for it to be done."
  (let* ((out (generate-new-buffer " *faultproxy-ctl*"))
         (s (open-network-stream "faultproxy-ctl" out "127.0.0.1"
                                 (nth 2 emjupy-int--proxy))))
    (process-send-string s (concat command "\n"))
    (with-timeout (5 (error "Fault proxy did not answer %s" command))
      (while (not (with-current-buffer out (string-match-p "ok"
                                                           (buffer-string))))
        (accept-process-output s 0.05)))
    (delete-process s)
    (kill-buffer out)))

(defmacro emjupy-int--through-proxy (&rest body)
  "Run BODY with the test server reached through the fault proxy."
  (declare (indent 0))
  `(progn
     (unless (and (emjupy-int--url) (executable-find "python3"))
       (ert-skip "Needs a live server and python3"))
     (let* ((port (emjupy-int--start-proxy))
            (process-environment (cons (format "EMJUPY_TEST_URL=127.0.0.1:%d"
                                               port)
                                       process-environment)))
       (unwind-protect (progn ,@body)
         (when (process-live-p (car emjupy-int--proxy))
           (kill-process (car emjupy-int--proxy)))))))

(ert-deftest emjupy-int-fault-stalled-request-fails-in-time ()
  "A request to a server that has stopped answering fails, in time.

Emacs waits for a synchronous request, so one that never returns is a
frozen Emacs.  It must give up, with the error callers expect."
  (emjupy-int--through-proxy
    (emjupy-int--skip-unless-live)
    (emjupy-int--proxy-say "stall")
    (let ((t0 (float-time)))
      (should-error (emjupy--http-request "GET" emjupy--current-server
                                          "/api/contents")
                    :type 'emjupy-http-error)
      (should (< (- (float-time) t0) 10)))
    (emjupy-int--proxy-say "reset")))

(ert-deftest emjupy-int-fault-fragmented-slow-output-arrives-whole ()
  "Output split into tiny, delayed pieces arrives exactly as sent.

WebSocket frames cut mid-way, and in several pieces, must be put back
together: nothing lost, nothing repeated, nothing out of order."
  (emjupy-int--through-proxy
    (emjupy-int--with-live-kernel
     (emjupy-int--proxy-say "chunk 7")
     (emjupy-int--proxy-say "delay 2")
     (let ((cell (emjupy-int--run "print('é' * 300)\nprint('end')" 60)))
       (should (equal (emjupy-int--stdout cell)
                      (concat (make-string 300 ?é) "\nend\n"))))
     (emjupy-int--proxy-say "reset"))))

(ert-deftest emjupy-int-fault-tunnel-drop-mid-cell-then-reconnect ()
  "The tunnel dropping while a cell runs neither errs nor hangs Emacs.

Then reconnecting reaches the same kernel, its state intact: the tunnel
went, not the kernel."
  (emjupy-int--through-proxy
    (emjupy-int--with-live-kernel
     (emjupy-int--run "keep = 42" 30)
     (with-current-buffer emjupy-int--buffer
       (let* ((nb (emjupy--notebook))
              (cell (aref (emjupy-notebook-cells nb) 0)))
         (setf (emjupy-cell-source cell)
               "import time\nfor i in range(30):\n    print(i, flush=True)\n    time.sleep(0.1)")
         (emjupy--redraw-notebook cell)
         (goto-char (overlay-start (emjupy-cell-overlay cell)))
         (emjupy-execute-cell-at-point)
         (emjupy-int--pump 1)
         (emjupy-int--proxy-say "sever")
         (emjupy-int--pump 2)
         ;; the socket is known to be gone, and Emacs is still responsive
         (should-not (emjupy--ws-live-p (emjupy-notebook-kernel nb)))
         (let ((t0 (float-time)))
           (emjupy--sync-all-cells)
           (should (< (- (float-time) t0) 0.5)))
         ;; the tunnel comes back; so does the same kernel
         (emjupy-int--proxy-say "reset")
         (emjupy-reconnect-kernel)
         (emjupy-int--pump 30
                           (lambda ()
                             (emjupy--ws-live-p (emjupy-notebook-kernel nb))))
         (should (emjupy--ws-live-p (emjupy-notebook-kernel nb)))))
     (let ((cell (emjupy-int--run "print(keep)" 30)))
       (should (equal (emjupy-int--stdout cell) "42\n"))))))

(ert-deftest emjupy-int-fault-a-cell-run-just-after-a-drop-runs ()
  "A cell run in the moments after a dropped connection runs once it is back.
It was refused, the notebook said to have no kernel."
  (emjupy-int--through-proxy
    (emjupy-int--with-live-kernel
     (emjupy-int--run "keep = 7" 30)
     (with-current-buffer emjupy-int--buffer
       (let* ((nb (emjupy--notebook))
              (cell (aref (emjupy-notebook-cells nb) 0)))
         (setf (emjupy-cell-source cell) "print('after the drop', keep)")
         (emjupy--redraw-notebook cell)
         (goto-char (overlay-start (emjupy-cell-overlay cell)))
         (emjupy-int--proxy-say "sever")
         (emjupy-int--pump 0.5)
         (emjupy-execute-cell-at-point)
         (should (emjupy-int--pump
                  30 (lambda ()
                       (string-match-p "after the drop 7"
                                       (emjupy-int--stdout cell))))))))))

(ert-deftest emjupy-int-a-dead-tunnel-is-reopened-mid-run ()
  "A notebook's SSH tunnel dying mid-run is reopened, and nothing is lost.
emjupy reopens the tunnel it opened, on the same port, and the server
replays what the cell printed meanwhile.  Same settings as
`emjupy-int-a-notebook-through-tramp-opens-on-its-machine'."
  (let ((file (getenv "EMJUPY_TEST_SSH_NOTEBOOK"))
        (env (getenv "EMJUPY_TEST_SSH_ENV")))
    (unless (and file env)
      (ert-skip "EMJUPY_TEST_SSH_NOTEBOOK and EMJUPY_TEST_SSH_ENV not set"))
    (let* ((emjupy-language-support nil)
           (buf (emjupy-int--open-through-tramp file env)))
      (unwind-protect
          (with-current-buffer buf
            (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
            (let ((cell (seq-find (lambda (c) (eq (emjupy-cell-type c) 'code))
                                  (emjupy-notebook-cells (emjupy--notebook)))))
              (setf (emjupy-cell-source cell)
                    "import time\nfor i in range(40):\n    print('t%d' % i, flush=True)\n    time.sleep(0.15)")
              (emjupy--redraw-notebook cell)
              (goto-char (overlay-start (emjupy-cell-overlay cell)))
              (emjupy-execute-cell-at-point)
              (emjupy-int--pump 2)
              (dolist (tunnel emjupy--remote-tunnels)
                (when (process-live-p (plist-get tunnel :process))
                  (signal-process (plist-get tunnel :process) 'KILL)))
              (should (emjupy-int--pump
                       60 (lambda ()
                            (string-match-p "t39" (emjupy-int--stdout cell)))))
              (should (equal (mapcar (lambda (l)
                                       (string-to-number (substring l 1)))
                                     (split-string (emjupy-int--stdout cell)
                                                   "\n" t))
                             (number-sequence 0 39)))))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer buf))))))

(ert-deftest emjupy-int-a-kernel-shut-down-elsewhere-ends-its-cell ()
  "A kernel shut down by someone else, mid-cell, its connection left open.
The server keeps the WebSocket open, so nothing drops; asked after once
quiet, the kernel is gone: the cell ends, and the notebook is left
without a kernel.  In its own notebook, so the shared kernel lives on."
  (emjupy-int--skip-unless-live)
  (let* ((emjupy-kernel-check-interval 2)
         (emjupy-language-support nil)
         (buf (emjupy-int--open-with-kernel
               emjupy--current-server
               (format "emjupy-int-shutdown-%d.ipynb" (random 100000)))))
    (unwind-protect
        (with-current-buffer buf
          (let* ((nb (emjupy--notebook))
                 (cell (aref (emjupy-notebook-cells nb) 0))
                 (id (emjupy-kernel-id (emjupy-notebook-kernel nb))))
            (setf (emjupy-cell-source cell) "import time\ntime.sleep(20)")
            (emjupy--redraw-notebook cell)
            (goto-char (overlay-start (emjupy-cell-overlay cell)))
            (emjupy-execute-cell-at-point)
            (emjupy-int--pump 1)
            (emjupy--http-request "DELETE" (emjupy-notebook-server nb)
                                  (concat "/api/kernels/" id))
            (should (emjupy-int--pump
                     12 (lambda () (null (emjupy-notebook-kernel nb)))))))
      (let ((kill-buffer-query-functions nil))
        (kill-buffer buf)))))

(ert-deftest emjupy-int-a-notebook-reopened-through-a-new-tunnel-is-the-same ()
  "Opening a TRAMP notebook again through a new tunnel gives back its buffer.
The same machine, port and path are the same notebook: its buffer is
moved onto the new tunnel, connected, its kernel's state there."
  (let ((file (getenv "EMJUPY_TEST_SSH_NOTEBOOK"))
        (env (getenv "EMJUPY_TEST_SSH_ENV")))
    (unless (and file env)
      (ert-skip "EMJUPY_TEST_SSH_NOTEBOOK and EMJUPY_TEST_SSH_ENV not set"))
    (let* ((emjupy-language-support nil)
           (buf (emjupy-int--open-through-tramp file env))
           (again nil))
      (unwind-protect
          (progn
            (with-current-buffer buf
              (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
              (let ((cell (seq-find (lambda (c)
                                      (eq (emjupy-cell-type c) 'code))
                                    (emjupy-notebook-cells
                                     (emjupy--notebook)))))
                (setf (emjupy-cell-source cell) "kept = 41 + 1")
                (emjupy--redraw-notebook cell)
                (goto-char (overlay-start (emjupy-cell-overlay cell)))
                (emjupy-execute-cell-at-point)
                (emjupy-int--pump
                 10 (lambda () (numberp (emjupy-cell-exec-count cell))))))
            (dolist (tunnel emjupy--remote-tunnels)
              (when (process-live-p (plist-get tunnel :process))
                (signal-process (plist-get tunnel :process) 'KILL)))
            (emjupy-int--pump 0.5)
            (setq again (emjupy-int--open-through-tramp file env))
            (should (eq again buf))
            (with-current-buffer buf
              (should (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p))))
              (let ((cell (seq-find (lambda (c)
                                      (eq (emjupy-cell-type c) 'code))
                                    (emjupy-notebook-cells
                                     (emjupy--notebook)))))
                (setf (emjupy-cell-source cell) "print('kept is', kept)")
                (emjupy--redraw-notebook cell)
                (goto-char (overlay-start (emjupy-cell-overlay cell)))
                (emjupy-execute-cell-at-point)
                (should (emjupy-int--pump
                         20 (lambda ()
                              (string-match-p "kept is 42"
                                              (emjupy-int--stdout cell))))))))
        (let ((kill-buffer-query-functions nil))
          (dolist (b (delete-dups (list buf again)))
            (when (buffer-live-p b) (kill-buffer b))))))))

(ert-deftest emjupy-int-an-untrusted-notebook-is-trusted-through-the-server ()
  "A notebook saved elsewhere, a script in its output, is trusted as Jupyter.
Unsigned, the server marks its cell untrusted, and the output does not
open; trusted, the server signs it, and marks it trusted from then on."
  (emjupy-int--skip-unless-live)
  (let* ((server emjupy--current-server)
         (path (format "emjupy-int-trust-%d.ipynb" (random 100000)))
         (out (let ((o (make-hash-table :test 'equal))
                    (d (make-hash-table :test 'equal)))
                (puthash "text/html" "<p>x</p><script>1</script>" d)
                (puthash "text/plain" "x" d)
                (puthash "output_type" "display_data" o)
                (puthash "data" d o)
                (puthash "metadata" (make-hash-table) o)
                o))
         (cell (let ((c (make-hash-table :test 'equal)))
                 (puthash "cell_type" "code" c) (puthash "id" "c1" c)
                 ;; Unique: Jupyter trusts by content, what it once trusted
                 ;; trusted under any name.
                 (puthash "source" (format "x = %d" (random 1000000000)) c)
                 (puthash "metadata" (make-hash-table) c)
                 (puthash "outputs" (vector out) c)
                 (puthash "execution_count" 1 c) c))
         (content (let ((n (make-hash-table :test 'equal)))
                    (puthash "cells" (vector cell) n)
                    (puthash "metadata" (make-hash-table) n)
                    (puthash "nbformat" 4 n) (puthash "nbformat_minor" 5 n) n))
         (payload (let ((p (make-hash-table :test 'equal)))
                    (puthash "type" "notebook" p) (puthash "format" "json" p)
                    (puthash "content" content p) p))
         (open (lambda () (emjupy-open-notebook path server)))
         (first-cell (lambda (buf)
                       (aref (emjupy-notebook-cells
                              (buffer-local-value 'emjupy--buffer-notebook buf))
                             0)))
         (kill (lambda (buf) (let ((kill-buffer-query-functions nil))
                               (kill-buffer buf))))
         (buf nil))
    (emjupy--http-request "PUT" server (emjupy--contents-path path)
                          (json-serialize payload))
    (setq buf (funcall open))
    (should-not (emjupy--cell-trusted-p (funcall first-cell buf)))
    (with-current-buffer buf
      (emjupy--goto-cell (funcall first-cell buf) 'start)
      (should-error (emjupy-open-output (emjupy--openable-data
                                         (funcall first-cell buf)))
                    :type 'user-error)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (emjupy-trust-notebook)))
    (funcall kill buf)
    (setq buf (funcall open))
    (unwind-protect
        (should (emjupy--cell-trusted-p (funcall first-cell buf)))
      (funcall kill buf))))

(ert-deftest emjupy-int-a-kernel-the-server-lacks-is-said-gone ()
  "Connecting to a kernel the server does not have -- as after a restart --
is never taken as connected: the server's refusal is seen, and the
notebook is told its kernel is gone."
  (emjupy-int--skip-unless-live)
  (let* ((said nil)
         (buf (emjupy-int--open-with-kernel
               emjupy--current-server
               (format "emjupy-int-gone-%d.ipynb" (random 100000)))))
    (unwind-protect
        (with-current-buffer buf
          (let ((nb (emjupy--notebook)))
            (cl-letf* ((real (symbol-function 'message))
                       ((symbol-function 'message)
                        (lambda (fmt &rest args)
                          (when (stringp fmt)
                            (push (apply #'format-message fmt args) said))
                          (apply real fmt args))))
              (emjupy--disconnect-kernel nb)
              (emjupy-connect-kernel nb "00000000-0000-0000-0000-000000000000")
              (should (emjupy-int--pump
                       15 (lambda () (null (emjupy-notebook-kernel nb))))))
            (should (seq-some (lambda (m) (string-match-p "is gone from the server" m))
                              said))
            (should-not (seq-some (lambda (m)
                                    (string-match-p "connected to kernel 00000000" m))
                                  said))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest emjupy-int-cells-run-while-one-runs-keep-their-order ()
  "Cells run while a slow one runs are queued and run in the order sent.
As in Jupyter: the kernel runs them in that order, execution counts rise
with it, each cell gets its own output in order -- a loop's figures
between its labels -- and a queued cell shows [In: *] meanwhile."
  (emjupy-int--skip-unless-live)
  (let* ((server emjupy--current-server)
         (path (format "emjupy-int-order-%d.ipynb" (random 100000)))
         (sources
          (append
           (list "import time\nimport matplotlib.pyplot as plt\norder = []"
                 "order.append('slow')\nfor i in range(6):\n    print('slow', i, flush=True)\n    time.sleep(0.3)")
           (cl-loop for k below 4 collect
                    (format "order.append('f%d')\nfor j in range(3):\n    print('f%d-%%d' %% j, flush=True)\n    plt.figure(figsize=(2, 1)); plt.plot([j, %d]); plt.show()" k k k))
           (cl-loop for k below 8 collect
                    (format "order.append('q%d')\nprint('q%d', sum(range(%d)))" k k (* k 1000)))
           (list "print(order)")))
         (last-cell (1- (length sources)))
         (sent (let ((rest (number-sequence 2 (1- last-cell))) (shuffled nil))
                 (random "emjupy-order")
                 (while rest
                   (let ((pick (nth (random (length rest)) rest)))
                     (push pick shuffled) (setq rest (delete pick rest))))
                 (append '(1) shuffled (list last-cell))))
         (n 0)
         (notebook
          `((cells . ,(vconcat
                       (mapcar (lambda (src)
                                 `((cell_type . "code")
                                   (id . ,(format "c%d" (cl-incf n)))
                                   (source . ,src)
                                   (metadata . ,(make-hash-table))
                                   (outputs . [])
                                   (execution_count . nil)))
                               sources)))
            (metadata . ,(make-hash-table))
            (nbformat . 4) (nbformat_minor . 5)))
         (buf nil))
    (emjupy--http-request "PUT" server (emjupy--contents-path path)
                          (json-encode `((type . "notebook") (format . "json")
                                         (content . ,notebook))))
    (setq buf (emjupy-open-notebook path server))
    (unwind-protect
        (with-current-buffer buf
          (let* ((nb (emjupy--notebook))
                 (cells (emjupy-notebook-cells nb))
                 (run (lambda (i)
                        (emjupy--goto-cell (aref cells i) 'start)
                        (emjupy-execute-cell-at-point)))
                 (done (lambda (i)
                         (and (numberp (emjupy-cell-exec-count (aref cells i)))
                              (not (memq (emjupy-cell-id (aref cells i))
                                         emjupy--running-cells))))))
            (emjupy--spawn-and-connect-kernel nb)
            (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
            (funcall run 0)
            (should (emjupy-int--pump 60 (lambda () (funcall done 0))))
            (funcall run 1)
            (emjupy-int--pump 0.3)
            (dolist (i (cdr sent)) (funcall run i))
            (emjupy-int--pump 0.3)
            (should (string-match-p "\\[In: \\*\\]"
                                    (emjupy--cell-label
                                     (aref cells (car (last sent 2))))))
            (should (emjupy-int--pump
                     90 (lambda () (cl-every done sent))))
            (let ((tag (lambda (i)
                         (cond ((= i 1) "slow")
                               ((< i 6) (format "f%d" (- i 2)))
                               (t (format "q%d" (- i 6))))))
                  (ran (butlast sent)))
              ;; the kernel ran them in the order sent
              (should (equal (string-trim (emjupy-int--stdout
                                           (aref cells last-cell)))
                             (format "[%s]"
                                     (mapconcat
                                      (lambda (i)
                                        (format "'%s'" (funcall tag i)))
                                      ran ", "))))
              ;; their counts rise in that order
              (let ((counts (mapcar (lambda (i)
                                      (emjupy-cell-exec-count (aref cells i)))
                                    sent)))
                (should (equal counts (sort (copy-sequence counts) #'<))))
              ;; the slow cell has all its lines; each loop its figures in order
              (should (equal (split-string (emjupy-int--stdout (aref cells 1))
                                           "\n" t)
                             (mapcar (lambda (i) (format "slow %d" i))
                                     (number-sequence 0 5))))
              (dolist (i (number-sequence 2 5))
                (should (equal (mapcar (lambda (o)
                                         (if (equal (gethash "output_type" o)
                                                    "stream")
                                             (string-trim
                                              (emjupy--mime-text
                                               (gethash "text" o)))
                                           (and (gethash "image/png"
                                                         (gethash "data" o))
                                                'figure)))
                                       (append (emjupy-cell-outputs
                                                (aref cells i))
                                               nil))
                               (cl-loop for j below 3
                                        append (list (format "%s-%d"
                                                             (funcall tag i) j)
                                                     'figure))))))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
(defun emjupy-int--file-notebook (source)
  "Make a fresh notebook whose one code cell holds SOURCE; return its path."
  ;; Unique across runs: a rename refuses a name a run before left there.
  (let ((path (format "emjupy-int-file-%s-%d.ipynb"
                      (format-time-string "%s%3N") (emacs-pid))))
    (emjupy--http-request
     "PUT" emjupy--current-server (emjupy--contents-path path)
     (json-encode
      `((type . "notebook") (format . "json")
        (content . ((cells . [((cell_type . "code") (id . "c1")
                                (source . ,source)
                                (metadata . ,(make-hash-table))
                                (outputs . []) (execution_count . nil))])
                    (metadata . ,(make-hash-table))
                    (nbformat . 4) (nbformat_minor . 5))))))
    path))

(defun emjupy-int--saved-source (path)
  "The first cell's source in the notebook at PATH, as the server has it."
  (let ((model (emjupy--http-request
                "GET" emjupy--current-server (emjupy--contents-path path))))
    (emjupy--mime-text
     (gethash "source" (aref (gethash "cells" (gethash "content" model)) 0)))))

(defmacro emjupy-int--with-file-notebook (source &rest body)
  "Run BODY in a fresh notebook holding SOURCE; PATH and BUF are bound."
  (declare (indent 1))
  `(progn
     (emjupy-int--skip-unless-live)
     (let* ((path (emjupy-int--file-notebook ,source))
            (buf (emjupy-open-notebook path emjupy--current-server))
            (kill-buffer-query-functions nil))
       (unwind-protect
           (with-current-buffer buf
             (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
               ,@body))
         (when (buffer-live-p buf) (kill-buffer buf))
         ;; its file goes too, under whatever name it ended with
         (ignore-errors
           (emjupy--http-request "DELETE" emjupy--current-server
                                 (emjupy--contents-path path)))))))

(defun emjupy-int--retype (text)
  "Make the first cell of this notebook hold TEXT, unsaved."
  (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0)))
    (setf (emjupy-cell-source cell) text)
    (emjupy--redraw-notebook cell)))

(ert-deftest emjupy-int-revert-to-the-checkpoint-a-save-made ()
  "Saving makes a checkpoint; reverting to it brings the saved notebook back."
  (emjupy-int--with-file-notebook "x = 1"
    (emjupy-save-notebook)
    (emjupy-int--retype "x = 2  # not saved")
    (emjupy-revert-to-checkpoint)
    (should (equal (emjupy-cell-source
                    (aref (emjupy-notebook-cells (emjupy--notebook)) 0))
                   "x = 1"))))

(ert-deftest emjupy-int-rename-save-as-and-duplicate ()
  "Rename, save as and duplicate a notebook, as JupyterLab does."
  (emjupy-int--with-file-notebook "y = 1"
    (let ((renamed (concat "renamed-" path))
          (other (concat "other-" path)))
      ;; rename: the file moves, the buffer follows, saving writes there
      (emjupy-rename-notebook renamed)
      (should-not (emjupy--http-exists-p emjupy--current-server path))
      (should (emjupy--http-exists-p emjupy--current-server renamed))
      (emjupy-int--retype "y = 2")
      (emjupy-save-notebook)
      (should (equal (emjupy-int--saved-source renamed) "y = 2"))
      ;; save as: a new file, the old one as it was
      (emjupy-int--retype "y = 3")
      (emjupy-save-notebook-as other)
      (should (equal (emjupy-int--saved-source other) "y = 3"))
      (should (equal (emjupy-int--saved-source renamed) "y = 2"))
      (should (equal (emjupy-notebook-path (emjupy--notebook)) other))
      ;; duplicate: a copy beside it, opened in a buffer of its own
      (let ((copy (emjupy-duplicate-notebook))
            (copy-path nil))
        (unwind-protect
            (with-current-buffer copy
              (setq copy-path (emjupy-notebook-path (emjupy--notebook)))
              (should-not (equal copy-path other))
              (should (equal (emjupy-cell-source
                              (aref (emjupy-notebook-cells (emjupy--notebook))
                                    0))
                             "y = 3")))
          (kill-buffer copy)
          ;; what it made goes: a run after would trip on it
          (dolist (made (list renamed other copy-path))
            (when made
              (ignore-errors
                (emjupy--http-request "DELETE" emjupy--current-server
                                      (emjupy--contents-path made))))))))))

(ert-deftest emjupy-int-a-cell-shows-how-long-it-ran ()
  "A cell run shows how long it took beside its count, kept as JupyterLab
keeps it: under \"execution\" in the saved notebook."
  (emjupy-int--with-file-notebook "import time; time.sleep(0.4)"
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0)))
      (emjupy--spawn-and-connect-kernel (emjupy--notebook))
      (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-execute-cell-at-point)
      (should (emjupy-int--pump
               30 (lambda () (numberp (emjupy-cell-exec-count cell)))))
      (emjupy-int--pump 0.5)
      (let ((seconds (emjupy--cell-execution-seconds cell)))
        (should (and seconds (< 0.35 seconds 3))))
      (should (string-match-p "\\] [0-9.]+ m?s python"
                              (emjupy--overlay-header
                               (emjupy-cell-overlay cell))))
      (emjupy-save-notebook)
      (let* ((model (emjupy--http-request "GET" emjupy--current-server
                                          (emjupy--contents-path path)))
             (saved (aref (gethash "cells" (gethash "content" model)) 0)))
        (should (gethash "shell.execute_reply"
                         (gethash "execution" (gethash "metadata" saved))))))))

(ert-deftest emjupy-int-the-debugger-stops-steps-and-shows ()
  "A breakpoint stops the cell there; variables, evaluation, stepping work.
JupyterLab's debugger, through the kernel: the cell's line marked, its
variables and an expression read in the frame, a step, and on to the
end of the cell."
  (emjupy-int--with-file-notebook "a = 1\nb = a + 1\nc = b * 3\nprint(c)"
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0))
          (said nil))
      (emjupy--spawn-and-connect-kernel (emjupy--notebook))
      (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
      (emjupy-debug-mode 1)
      (unwind-protect
          (progn
            (emjupy--goto-cell cell 'beginning)
            (forward-line 2)
            (emjupy-debug-toggle-breakpoint)
            (emjupy--goto-cell cell 'beginning)
            (emjupy-execute-cell-at-point)
            (should (emjupy-int--pump 20 (lambda () emjupy--debug-arrow)))
            (should (equal (string-trim
                            (buffer-substring
                             (overlay-start emjupy--debug-arrow)
                             (overlay-end emjupy--debug-arrow)))
                           "c = b * 3"))
            (emjupy-debug-variables)
            (with-current-buffer "*emjupy variables*"
              (should (string-match-p "a = 1" (buffer-string)))
              (should (string-match-p "b = 2" (buffer-string))))
            (cl-letf (((symbol-function 'message)
                       (lambda (fmt &rest args)
                         (push (apply #'format fmt args) said))))
              (emjupy-debug-evaluate "b*10"))
            (should (equal (car said) "20"))
            (emjupy-debug-next)
            (should (emjupy-int--pump 20 (lambda () emjupy--debug-arrow)))
            (should (string-match-p "print(c)"
                                    (buffer-substring
                                     (overlay-start emjupy--debug-arrow)
                                     (overlay-end emjupy--debug-arrow))))
            (emjupy-debug-continue)
            (should (emjupy-int--pump
                     20 (lambda () (numberp (emjupy-cell-exec-count cell)))))
            (should (equal (string-trim (emjupy-int--stdout cell)) "6")))
        (emjupy-debug-mode -1)))))

(defun emjupy-int--debug-start (cell)
  "Start debugging this notebook's kernel, CELL's notebook, once it is up."
  (ignore cell)
  (emjupy--spawn-and-connect-kernel (emjupy--notebook))
  (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
  (emjupy-debug-mode 1))

(defun emjupy-int--debug-said (thunk)
  "Run THUNK; return what it said with `message', newest first."
  (let ((said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args)
                 (push (apply #'format fmt args) said))))
      (funcall thunk))
    said))

(ert-deftest emjupy-int-the-debugger-stops-only-when-the-condition-holds ()
  "A breakpoint with a condition stops only when it is true."
  (emjupy-int--with-file-notebook
      "total = 0\nfor i in range(5):\n    total += i\nprint(total)"
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0)))
      (emjupy-int--debug-start cell)
      (unwind-protect
          (progn
            (emjupy--goto-cell cell 'beginning)
            (forward-line 2)
            (emjupy-debug-toggle-breakpoint "i == 3")
            (emjupy-execute-cell-at-point)
            (should (emjupy-int--pump 20 (lambda () emjupy--debug-arrow)))
            (should (equal (car (emjupy-int--debug-said
                                 (lambda () (emjupy-debug-evaluate "i"))))
                           "3"))
            (emjupy-debug-continue)
            (should (emjupy-int--pump
                     20 (lambda () (numberp (emjupy-cell-exec-count cell)))))
            (should (equal (string-trim (emjupy-int--stdout cell)) "10")))
        (emjupy-debug-mode -1)))))

(ert-deftest emjupy-int-the-debugger-on-an-exception-frames-and-nesting ()
  "Stopped on a raised exception: its message said; the outer frame chosen,
its variable evaluated there and what that variable holds shown."
  (emjupy-int--with-file-notebook
      "def inner(d):\n    raise ValueError('boom')\ndef outer():\n    data = {'a': {'b': 1}}\n    inner(data)\nouter()"
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0))
          (said nil))
      (emjupy-int--debug-start cell)
      (unwind-protect
          (progn
            (emjupy-debug-break-on-exceptions '("raised"))
            (emjupy--goto-cell cell 'beginning)
            (setq said (emjupy-int--debug-said
                        (lambda ()
                          (emjupy-execute-cell-at-point)
                          (emjupy-int--pump 20 (lambda () emjupy--debug-arrow))
                          (emjupy-int--pump 1))))
            (should emjupy--debug-arrow)
            (should (seq-some (lambda (m) (string-match-p "exception: boom" m))
                              said))
            (let ((outer (seq-find (lambda (f)
                                     (equal (gethash "name" f) "outer"))
                                   emjupy--debug-frames)))
              (should outer)
              (emjupy-debug-frame outer))
            (should (equal (car (emjupy-int--debug-said
                                 (lambda () (emjupy-debug-evaluate "data"))))
                           "{'a': {'b': 1}}"))
            (cl-letf (((symbol-function 'display-buffer) #'ignore))
              (emjupy-debug-variables))
            (with-current-buffer "*emjupy variables*"
              (goto-char (point-min))
              (search-forward "+ data = ")
              (button-activate (button-at (match-beginning 0)))
              (should (string-match-p "'a' = {'b': 1}" (buffer-string))))
            (emjupy-debug-continue))
        (emjupy-debug-mode -1)))))

(ert-deftest emjupy-int-the-mode-line-follows-the-kernel ()
  "The mode line shows the server's port, and the kernel busy, then idle."
  (emjupy-int--with-file-notebook "import time; time.sleep(1.5)"
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0))
          (port (plist-get (emjupy--server-parts emjupy--current-server)
                           :port)))
      (emjupy--spawn-and-connect-kernel (emjupy--notebook))
      (should (emjupy-int--pump
               60 (lambda () (string-match-p "idle"
                                             (emjupy--mode-line-status)))))
      (should (string-match-p (format ":%s\\]" port)
                              (emjupy--mode-line-status)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-execute-cell-at-point)
      (should (emjupy-int--pump
               10 (lambda () (string-match-p "busy"
                                             (emjupy--mode-line-status)))))
      (should (emjupy-int--pump
               20 (lambda () (string-match-p "idle"
                                             (emjupy--mode-line-status))))))))

(ert-deftest emjupy-int-restart-run-all-a-region-and-shutting-down ()
  "Restart and run all: a fresh kernel runs every cell.  A region runs its
cells.  Shut down, the server has the kernel no more."
  (emjupy-int--with-file-notebook "x = 41"
    (let* ((nb (emjupy--notebook)) (cells nil))
      (emjupy-insert-cell-below)
      (insert "print(x + 1)")
      (emjupy--sync-all-cells)
      (setq cells (emjupy-notebook-cells nb))
      (emjupy--spawn-and-connect-kernel nb)
      (should (emjupy-int--pump 60 (lambda () (emjupy--ws-live-p))))
      (cl-flet ((done ()
                  (cl-every (lambda (c) (numberp (emjupy-cell-exec-count c)))
                            cells)))
        ;; a region over both
        (let ((transient-mark-mode t))
          (emjupy--goto-cell (aref cells 0) 'beginning)
          (push-mark (point) t t)
          (emjupy--goto-cell (aref cells 1) 'end)
          (emjupy-execute-cell-at-point))
        (should (emjupy-int--pump 30 #'done))
        (should (equal (string-trim (emjupy-int--stdout (aref cells 1))) "42"))
        ;; restart and run all: a fresh kernel counts from 1 again, where
        ;; the old one would go on at 3
        (dolist (c (append cells nil)) (setf (emjupy-cell-exec-count c) nil))
        (emjupy-restart-kernel-and-run-all)
        (should (emjupy-int--pump 60 #'done))
        (should (equal (mapcar #'emjupy-cell-exec-count (append cells nil))
                       '(1 2)))
        (should (equal (string-trim (emjupy-int--stdout (aref cells 1))) "42")))
      (let ((kernel-id (emjupy-kernel-id (emjupy-notebook-kernel nb))))
        (emjupy-shutdown-kernel)
        (should-not (emjupy-notebook-kernel nb))
        (should-not (cl-find kernel-id
                             (append (emjupy--http-request
                                      "GET" emjupy--current-server "/api/kernels")
                                     nil)
                             :key (lambda (k) (gethash "id" k))
                             :test #'equal))))))

(ert-deftest emjupy-int-a-notebook-gets-the-kernel-it-records ()
  "A notebook recording a kernelspec the server has gets a kernel of it, as
the server says; one recording a kernel it lacks gets python3."
  (emjupy-int--skip-unless-live)
  (let* ((specs (gethash "kernelspecs"
                         (emjupy--http-request "GET" emjupy--current-server
                                               "/api/kernelspecs")))
         (other (seq-find (lambda (n) (not (equal n "python3")))
                          (hash-table-keys specs))))
    (dolist (case (append (and other (list (cons other other)))
                          (list (cons "no-such-kernel" "python3"))))
      (emjupy-int--with-file-notebook "x = 1"
        (let ((nb (emjupy--notebook)))
          (setf (emjupy-notebook-metadata nb)
                (emjupy--json-object "kernelspec"
                                     (emjupy--json-object "name" (car case))))
          (cl-letf (((symbol-function 'message) #'ignore))
            (emjupy--spawn-and-connect-kernel nb))
          (let ((id (emjupy-kernel-id (emjupy-notebook-kernel nb))))
            (should (equal (gethash "name"
                                    (emjupy--http-request
                                     "GET" emjupy--current-server
                                     (format "/api/kernels/%s" id)))
                           (cdr case))))
          (emjupy-shutdown-kernel))))))

(ert-deftest emjupy-int-language-support-off-does-not-warm-up-on-opening ()
  "Opening a notebook with `emjupy-language-support' nil starts no language support.
Opening one builds the shadow buffer and starts a server in the
background, so that completion is ready by the first keystroke -- and did
so whatever the switch said, then reported \"Language-server setup
failed\" on a machine with no server.  Here the shadow buffer is counted
instead of built: not asked for with the switch off, asked for once with
it on."
  (emjupy-int--skip-unless-live)
  (dolist (case '((nil . 0) (t . 1)))
    (let ((asked 0))
      (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                 (lambda (&rest _) (cl-incf asked) nil)))
        (let ((emjupy-language-support (car case)))
          (emjupy-int--fresh-notebook)))
      (unwind-protect
          (ert-info ((format "switch %S" (car case)))
            (should (= asked (cdr case))))
        (let ((kill-buffer-query-functions nil))
          (when (buffer-live-p emjupy-int--buffer)
            (with-current-buffer emjupy-int--buffer (set-buffer-modified-p nil))
            (kill-buffer emjupy-int--buffer)))))))

(defun emjupy-int--open-through-tramp (file env)
  "Open FILE, a notebook through TRAMP, with C-c C-o, the environment ENV."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) env)))
    (with-current-buffer (find-file-noselect file)
      (emjupy-open-this-notebook))))

(ert-deftest emjupy-int-a-notebook-through-tramp-opens-on-its-machine ()
  "An .ipynb opened through TRAMP opens on its own machine, and stays there.
Set EMJUPY_TEST_SSH_NOTEBOOK to a TRAMP name of a notebook -- hops are
welcome -- and EMJUPY_TEST_SSH_ENV to an environment there with
jupyter-server.  The notebook opens on a server there, through a tunnel;
its kernel runs in its folder; closing it closes the tunnel and leaves
the server and its kernel running; opening it again gets them back."
  (let ((file (getenv "EMJUPY_TEST_SSH_NOTEBOOK"))
        (env (getenv "EMJUPY_TEST_SSH_ENV")))
    (unless (and file env)
      (ert-skip "EMJUPY_TEST_SSH_NOTEBOOK and EMJUPY_TEST_SSH_ENV not set"))
    (let* ((emjupy-language-support nil)
           (dir (file-remote-p (file-name-directory file) 'localname))
           (nb (emjupy-int--open-through-tramp file env))
           (server (emjupy-notebook-server
                    (buffer-local-value 'emjupy--buffer-notebook nb)))
           (answer nil))
      (should (eq (buffer-local-value 'major-mode nb) 'emjupy-mode))
      (should (equal (emjupy--remote-root-for server)
                     (directory-file-name
                      (concat (emjupy--remote-prefix file) dir))))
      (with-current-buffer nb
        (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p)))
        (emjupy--kernel-eval (emjupy-notebook-kernel emjupy--buffer-notebook)
                             "import os; emjupy_int_left = 42; print(os.getcwd())"
                             (lambda (out) (setq answer (string-trim out))))
        (emjupy-int--pump 20 (lambda () answer))
        (should (equal (file-name-as-directory answer)
                       (file-name-as-directory dir)))
        (set-buffer-modified-p nil))
      (let ((kill-buffer-query-functions nil)) (kill-buffer nb))
      (should-not emjupy--remote-tunnels)
      ;; again: the server, and its kernel, are where they were left
      (let* ((again (emjupy-int--open-through-tramp file env)) (left nil))
        (with-current-buffer again
          (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p)))
          (emjupy--kernel-eval (emjupy-notebook-kernel emjupy--buffer-notebook)
                               "print(emjupy_int_left)"
                               (lambda (out) (setq left (string-trim out))))
          (emjupy-int--pump 20 (lambda () left))
          (should (equal left "42"))
          (set-buffer-modified-p nil))
        (let ((kill-buffer-query-functions nil)) (kill-buffer again))))))

(ert-deftest emjupy-int-fault-kernel-dying-mid-cell-ends-the-run ()
  "A kernel that dies while a cell runs does not leave it running for ever.

The kernel exits without replying; the server restarts it.  The cell
must stop showing as running, rather than wait for a reply that will
never come.

Expected to fail.  The server restarts the kernel about
four seconds later but sends nothing over the notebook's connection --
no \"restarting\" status, no close -- and its REST API reports the new
kernel as \"starting\", so nothing that arrives says the run is over.
Ending it needs a check made while a cell runs: the kernel's session
changing, or the kernel polled.  A connection that does close is
handled: the cells running on it are ended."
  :expected-result :failed
  (emjupy-int--with-live-kernel
   (with-current-buffer emjupy-int--buffer
     (let* ((nb (emjupy--notebook))
            (cell (aref (emjupy-notebook-cells nb) 0))
            (pending (lambda () (cl-loop for v being the hash-values of
                                         (emjupy-kernel-pending (emjupy-notebook-kernel nb))
                                         thereis (eq v cell)))))
       (setf (emjupy-cell-source cell) "import os\nos._exit(1)")
       (emjupy--redraw-notebook cell)
       (goto-char (overlay-start (emjupy-cell-overlay cell)))
       (emjupy-execute-cell-at-point)
       (emjupy-int--pump 20 (lambda () (not (funcall pending))))
       (should-not (funcall pending))))))

(ert-deftest emjupy-int-fault-save-during-stall-reports-failure ()
  "Saving while the server has stopped answering reports that it failed.

Not a hang, and not a silent success: the edits are not on the server,
and the user must be told."
  (emjupy-int--through-proxy
    (emjupy-int--with-live-kernel
     (emjupy-int--proxy-say "stall")
     (with-current-buffer emjupy-int--buffer
       (let ((t0 (float-time)))
         (should-error (emjupy-save-notebook))
         (should (< (- (float-time) t0) 15))))
     (emjupy-int--proxy-say "reset"))))

(ert-deftest emjupy-int-plotly-figure-opens-with-the-kernels-plotly-js ()
  "A plotly figure shows as a line, and opens as a page that draws it.

The figure arrives as JSON only: the page is built here, with plotly.js
asked of the kernel -- near 5 MB, which has to arrive whole -- and kept.
The viewer is the one thing stubbed: it records the page it would show."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run "import plotly.graph_objects as go
go.Figure(go.Scatter(x=[1, 2, 3], y=[3, 1, 2])).show()" 60))
          (data (gethash "data" (aref (emjupy-cell-outputs cell) 0)))
          (shown nil))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "plotly is not installed where the kernel runs"))
     (should (gethash emjupy--plotly-mime data))
     (with-current-buffer emjupy-int--buffer
       ;; the output is the line that opens it, not "Figure"
       (should (emjupy-int--shows "Interactive plotly figure"))
       (cl-letf (((symbol-function 'emjupy--show-file) (lambda (file)
                                                         (setq shown file))))
         (emjupy-open-output data)
         (emjupy-int--pump 60 (lambda () shown))))
     (should shown)
     (let* ((page (with-temp-buffer (insert-file-contents shown)
                                    (buffer-string)))
            (script (and (string-match "<script src=\"file://\\([^\"]+\\)\""
                                       page)
                         (match-string 1 page))))
       (should (string-match-p "Plotly.newPlot" page))
       (should (string-match-p "\"type\":\"scatter\"" page))
       (should (and script (file-exists-p script)))
       ;; the kernel's whole plotly.js, not its first piece
       (should (> (file-attribute-size (file-attributes script)) 1000000))
       (should (string-match-p "plotly\\.js v[0-9]"
                               (with-temp-buffer
                                 (insert-file-contents script nil 0 2000)
                                 (buffer-string))))))))

(ert-deftest emjupy-int-interact-slider-redraws-its-figure-in-place ()
  "An interact slider shows as a control, and moving it redraws the figure.

The figure comes back as ordinary output answering the slider's message,
not the cell's execution: it must reach the cell, replace the figure
there rather than add a second, and leave the control in place."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run "import matplotlib
matplotlib.use('module://matplotlib_inline.backend_inline')
import matplotlib.pyplot as plt
from ipywidgets import interact
@interact(n=(1, 5))
def f(n=3):
    plt.plot(range(n)); plt.title(f'n={n}'); plt.show()" 60))
          (images (lambda ()
                    (cl-loop for o across (emjupy-cell-outputs cell)
                             for d = (and (hash-table-p o) (gethash "data" o))
                             when (and d (gethash "image/png" d)) collect
                             (gethash "image/png" d)))))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "ipywidgets or matplotlib is not installed where the kernel runs"))
     (with-current-buffer emjupy-int--buffer
       ;; the slider, not "interactive(children=...)"
       (should (emjupy-int--shows "n ◀ 3 ▶"))
       (should-not (string-match-p "interactive(children" (buffer-string)))
       (let ((before (funcall images)))
         (should (= (length before) 1))
         (goto-char (point-min))
         (search-forward "n ◀ 3 ▶")
         (backward-char)
         (emjupy-widget-increase)
         (emjupy-int--pump 30 (lambda () (let ((now (funcall images)))
                                           (and (= (length now) 1)
                                                (not (equal now before))))))
         ;; one figure, the new one, and the control showing the new value
         (should (= (length (funcall images)) 1))
         (should-not (equal (funcall images) before))
         (emjupy-flush-output (current-buffer))
         (should (string-match-p "n ◀ 4 ▶" (buffer-string))))))))

(ert-deftest emjupy-int-widgets-are-driven-by-the-shape-of-their-state ()
  "Every kind of widget control sends the kernel what it was given.

The controls are not written per widget type: each is drawn and driven
by the shape of the widget's state.  One widget per kind of control,
each used once -- the minibuffer's answers given -- and the kernel asked
what it now holds."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run "import ipywidgets as w, datetime
clicks = [0]
b = w.Button(description='Press')
b.on_click(lambda _: clicks.__setitem__(0, clicks[0] + 1))
W = dict(step=w.IntSlider(description='Step', value=3, min=0, max=10),
         number=w.IntText(description='Number', value=1),
         range=w.IntRangeSlider(description='Range', value=(2, 8), min=0, max=10),
         choose=w.Dropdown(description='Choose', options=['a', 'b', 'c'], value='a'),
         several=w.SelectMultiple(description='Several', options=['x', 'y', 'z']),
         toggle=w.Checkbox(description='Toggle', value=False),
         text=w.Text(description='Text', value='old'),
         date=w.DatePicker(description='Date'),
         tags=w.TagsInput(description='Tags', value=['p']),
         color=w.ColorPicker(description='Colour', value='black'))
w.VBox(list(W.values()) + [b])" 60))
          (kernel (with-current-buffer emjupy-int--buffer
                    (emjupy-notebook-kernel emjupy--buffer-notebook))))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "ipywidgets is not installed where the kernel runs"))
     (with-current-buffer emjupy-int--buffer
       (emjupy-flush-output (current-buffer))
       (cl-flet ((use (label glyph)
                   ;; the control after LABEL: GLYPH starts it
                   (goto-char (overlay-start (emjupy-cell-output-ov cell)))
                   (re-search-forward (if (string-empty-p label)
                                          (concat "\\(" (regexp-quote glyph)
                                                  "\\)")
                                        (concat (regexp-quote label) " .*?\\("
                                                (regexp-quote glyph) "\\)")))
                   (goto-char (match-beginning 1))
                   (emjupy-widget-activate)
                   (emjupy-int--pump 0.5)))
         (cl-letf (((symbol-function 'read-number) (lambda (&rest _) 7))
                   ((symbol-function 'read-string)
                    (lambda (prompt &rest _)
                      (cond ((string-match-p "Date" prompt) "2024-03-05")
                            ((string-match-p "commas" prompt) "p, q")
                            (t "new"))))
                   ((symbol-function 'completing-read) (lambda (&rest _) "c"))
                   ((symbol-function 'completing-read-multiple)
                    (lambda (&rest _) '("x" "z")))
                   ((symbol-function 'read-color) (lambda (&rest _) "red")))
           (use "Step" "▶")
           (use "Number" "[")
           (use "Range" "2")
           (use "Choose" "a")
           (use "Several" "(none)")
           (use "Toggle" "[ ]")
           (use "Text" "[")
           (use "Date" "[")
           (use "Tags" "[")
           (use "Colour" "[")
           (use "" "[ Press ]"))))
     (let ((answer nil))
       (emjupy--kernel-eval
        kernel "print(repr((W['step'].value, W['number'].value, W['range'].value, W['choose'].value, W['several'].value, W['toggle'].value, W['text'].value, str(W['date'].value), W['tags'].value, W['color'].value, clicks[0])))"
        (lambda (out) (setq answer (string-trim out))))
       (emjupy-int--pump 15 (lambda () answer))
       (should (equal answer
                      "(4, 7, (7, 8), 'c', ('x', 'z'), True, 'new', '2024-03-05', ['p', 'q'], '#ff0000', 1)"))))))

(ert-deftest emjupy-int-every-ipywidgets-widget-is-drawn ()
  "Every widget ipywidgets defines is drawn by a rule, bar a known few.

Built from ipywidgets' own list, so a widget it adds is checked the day
it appears: drawn, if its state has a shape a rule knows, else named
here as an exception.  The one exception: a gamepad, shown by name."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run "import ipywidgets as w, inspect
ws = []
for n, c in inspect.getmembers(w, inspect.isclass):
    if issubclass(c, w.DOMWidget) and c is not w.DOMWidget and not n.startswith(('Layout', 'Style')):
        try:
            ws.append(c(description=n) if 'description' in c.class_trait_names() else c())
        except Exception:
            pass
w.VBox(ws)" 60)))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "ipywidgets is not installed where the kernel runs"))
     (with-current-buffer emjupy-int--buffer
       (emjupy-flush-output (current-buffer))
       (let* ((ov (emjupy-cell-output-ov cell))
              (text (buffer-substring-no-properties (overlay-start ov)
                                                    (overlay-end ov)))
              (named nil) (start 0))
         (while (string-match "\\[widget: \\([A-Za-z]+\\)\\]" text start)
           (push (match-string 1 text) named)
           (setq start (match-end 0)))
         (should (equal named '("Controller")))
         ;; the binary ones are read now: none goes unshown
         (should-not (cl-some (lambda (l) (string-match-p "\\`\\[widget\\]" l))
                              (split-string text "\n")))
         ;; and the rest are drawn as themselves: a few to be sure
         (dolist (shown '("IntSlider ◀ 0 ▶" "IntRangeSlider 25 – 75"
                          "IntProgress ░"
                          "Checkbox [ ]" "Valid ✗" "SelectMultiple (none) ▾"
                          "DatePicker [--]"
                          "FloatLogSlider ◀ 1 ▶   1 … 10000" "Password []"
                          "[ Button ]"))
           (ert-info (shown)
             (should (string-match-p (regexp-quote shown) text)))))))))

(ert-deftest emjupy-int-binary-widgets-image-audio-upload ()
  "Widgets whose data is binary: an image drawn and redrawn, a sound, an upload.

Their data comes, and goes, in binary WebSocket frames, which were not
read: an image, a sound or a video showed as [widget], and a file could
not be uploaded.  The image is replaced from the kernel and drawn anew;
the file chosen reaches the kernel byte for byte."
  (emjupy-int--with-live-kernel
   (let* ((file (make-temp-file "emjupy-upload" nil ".bin"))
          (bytes (apply #'unibyte-string (number-sequence 0 255)))
          (cell (emjupy-int--run "import ipywidgets as w, base64
png1 = base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==')
png2 = base64.b64decode('iVBORw0KGgoAAAANSUhEUgAAAAIAAAACCAYAAABytg0kAAAAFklEQVR42mP8z8DwnwEJMDKQIgEA9b0D/6m0jAgAAAAASUVORK5CYII=')
img = w.Image(value=png1, format='png')
snd = w.Audio(value=b'RIFF' + bytes(40), format='wav', autoplay=False)
up = w.FileUpload(description='Up')
w.VBox([img, snd, up])" 60)))
     (unwind-protect
         (progn
           (let ((coding-system-for-write 'no-conversion)) (write-region
                                                            bytes
                                                            nil
                                                            file nil 'quiet))
           (when (string-match-p "No module named"
                                 (format "%S" (emjupy-cell-outputs cell)))
             (ert-skip "ipywidgets is not installed where the kernel runs"))
           (with-current-buffer emjupy-int--buffer
             (emjupy-flush-output (current-buffer))
             (let ((shown (lambda ()
                            (let ((ov (emjupy-cell-output-ov cell)))
                              (buffer-substring-no-properties
                               (overlay-start ov) (overlay-end ov))))))
               ;; the image's bytes arrived; this Emacs draws no images, so it
               ;; is a line
               (should (string-match-p "▶ Image (png, 70)" (funcall shown)))
               (should (string-match-p "▶ Audio (wav" (funcall shown)))
               ;; replaced from the kernel: drawn anew
               (let ((kernel (emjupy-notebook-kernel emjupy--buffer-notebook))
                     (done nil))
                 (emjupy--kernel-eval kernel "img.value = png2; print('ok')"
                                      (lambda (_) (setq done t)))
                 (emjupy-int--pump
                  15 (lambda ()
                       (and done
                            (string-match-p "▶ Image (png, 77)"
                                            (funcall shown))))))
               (should (string-match-p "▶ Image (png, 77)" (funcall shown)))
               ;; an upload: the file is chosen, then nothing more
               (goto-char (overlay-start (emjupy-cell-output-ov cell)))
               (search-forward "[ Upload… ]") (backward-char 3)
               (let ((answers (list file "")))
                 (cl-letf (((symbol-function 'read-file-name)
                            (lambda (&rest _) (pop answers))))
                   (emjupy-widget-activate)))
               (should (string-match-p (regexp-quote (file-name-nondirectory file))
                                       (funcall shown)))))
           (let ((answer nil)
                 (kernel (with-current-buffer emjupy-int--buffer
                           (emjupy-notebook-kernel emjupy--buffer-notebook))))
             (emjupy-int--pump 1)
             (emjupy--kernel-eval
              kernel "print(len(up.value), up.value[0].name.endswith('.bin'), bytes(up.value[0].content) == bytes(range(256)))"
              (lambda (out) (setq answer (string-trim out))))
             (emjupy-int--pump 15 (lambda () answer))
             (should (equal answer "1 True True"))))
       (delete-file file)))))

(ert-deftest emjupy-int-widget-page-bridge-relays-both-ways ()
  "A page, through the bridge, gets the widget's models and moves the widget.

Emacs stands in for the page here -- a WebSocket to the bridge, saying
what a page says -- so no browser is needed: the models the page would
draw arrive, and a value the page sends reaches the kernel."
  (emjupy-int--with-live-kernel
   (let* ((cell (emjupy-int--run "import anywidget, traitlets
class Counter(anywidget.AnyWidget):
    _esm = 'export default { render() {} }'
    value = traitlets.Int(0).tag(sync=True)
c = Counter(); c" 60))
          (received nil))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "anywidget is not installed where the kernel runs"))
     (with-current-buffer emjupy-int--buffer
       (emjupy-flush-output (current-buffer))
       (should (string-match-p "▶ Interactive widget: Counter (anywidget)"
                               (buffer-string)))
       (let* ((id (catch 'found
                    (maphash (lambda (k v)
                               (when (equal (gethash "_anywidget_id" (cdr v)
                                                     "") "__main__.Counter")
                                 (throw 'found k)))
                             emjupy--widget-models)))
              (token "test-page")
              (kernel (emjupy-notebook-kernel emjupy--buffer-notebook)))
         (should id)
         (puthash token (list :kernel kernel :cell cell :view id
                              :models (emjupy--widget-tree id))
                  emjupy--bridge-pages)
         (let ((ws (websocket-open
                    (format "ws://127.0.0.1:%d/"
                            (emjupy--bridge-port))
                    :on-message (lambda (_ws frame)
                                  (push (json-parse-string
                                         (websocket-frame-text frame)
                                         :object-type 'hash-table)
                                        received)))))
           (unwind-protect
               (progn
                 (emjupy-int--pump 5 (lambda () (websocket-openp ws)))
                 (websocket-send-text ws (format "{\"type\": \"hello\", \"page\": \"%s\"}" token))
                 (emjupy-int--pump 10 (lambda () received))
                 ;; the models a page draws from
                 (let ((init (car received)))
                   (should (equal (gethash "type" init) "init"))
                   (should (equal (gethash "view" init) id))
                   (should (cl-some (lambda (m) (equal (gethash "id" m) id))
                                    (gethash "models" init))))
                 ;; a value the page sends reaches the kernel
                 (websocket-send-text
                  ws (format "{\"type\": \"comm_msg\", \"comm_id\": \"%s\", \"data\": {\"method\": \"update\", \"state\": {\"value\": 5}, \"buffer_paths\": []}, \"buffers\": []}" id))
                 (emjupy-int--pump 2)
                 (let ((answer nil))
                   (emjupy--kernel-eval
                    kernel "print(c.value)" (lambda (out)
                                              (setq answer (string-trim out))))
                   (emjupy-int--pump 10 (lambda () answer))
                   (should (equal answer "5"))))
             (websocket-close ws)
             (remhash token emjupy--bridge-pages))))))))

(ert-deftest emjupy-int-real-tqdm-is-one-bar-updated-in-place ()
  "A real tqdm bar, with prints in its loop, is one line redrawn in place.

The other progress-bar tests write the carriage returns themselves; this
one runs tqdm, whose timing and width are its own.  Watched while it
runs: never more than one bar line, its percentage only rising.  At the
end: one bar at 100%, each printed line once and in order below it,
nothing wider than the cell.  And what is kept, and saved, is the bar
as it last stood, not every frame tqdm drew over."
  (emjupy-int--with-live-kernel
   (with-current-buffer emjupy-int--buffer
     (let* ((cell (aref (emjupy-notebook-cells emjupy--buffer-notebook) 0))
            (inhibit-read-only t)
            (bar-lines
             (lambda ()
               (let ((ov (emjupy-cell-output-ov cell)))
                 (when (overlayp ov)
                   (cl-remove-if-not
                    (lambda (l) (string-match-p "[0-9]+%|" l))
                    (split-string (buffer-substring-no-properties
                                   (overlay-start ov) (overlay-end ov))
                                  "\n"))))))
            (percent (lambda (line) (and line
                                         (string-match "\\([0-9]+\\)%|" line)
                                         (string-to-number (match-string 1 line)))))
            (seen nil))
       (let ((ov (emjupy-cell-overlay cell)))
         (delete-region (overlay-start ov) (1- (overlay-end ov)))
         (goto-char (overlay-start ov))
         (insert "import time
from tqdm import tqdm
for i in tqdm(range(50)):
    if i % 10 == 0:
        print('step', i)
    time.sleep(0.05)
print('done')"))
       (emjupy-execute-cell-at-point)
       ;; watched while it runs
       (let ((deadline (+ (float-time) 30)))
         (while (and (< (float-time) deadline)
                     (not (member (emjupy-cell-id cell) emjupy--running-cells))
                     (not (funcall bar-lines)))
           (accept-process-output nil 0.05))
         (while (and (< (float-time) deadline)
                     (member (emjupy-cell-id cell) emjupy--running-cells))
           (emjupy-flush-output (current-buffer))
           (let ((bars (funcall bar-lines)))
             (should (<= (length bars) 1))
             (when bars (push (funcall percent (car bars)) seen)))
           (accept-process-output nil 0.1)))
       (emjupy-int--pump 2)
       (emjupy-flush-output (current-buffer))
       (when (string-match-p "No module named"
                             (format "%S" (emjupy-cell-outputs cell)))
         (ert-skip "tqdm is not installed where the kernel runs"))
       ;; it was seen moving, and only forward
       (setq seen (nreverse (delq nil seen)))
       (should (> (length (delete-dups (copy-sequence seen))) 2))
       (should (equal seen (sort (copy-sequence seen) #'<=)))
       ;; one bar, finished
       (let ((bars (funcall bar-lines)))
         (should (= (length bars) 1))
         (should (string-match-p "100%|" (car bars)))
         (should (string-match-p "50/50" (car bars))))
       ;; every printed line once, in order
       (let ((text (buffer-substring-no-properties
                    (overlay-start (emjupy-cell-output-ov cell))
                    (overlay-end (emjupy-cell-output-ov cell)))))
         (should (equal (cl-loop with start = 0
                                 while
                                 (string-match "^\\(step [0-9]+\\|done\\)"
                                               text start)
                                 collect (match-string 1 text)
                                 do (setq start (match-end 0)))
                        '("step 0" "step 10" "step 20" "step 30" "step 40"
                          "done"))))
       ;; nothing wider than the cell
       (let ((width (emjupy--box-width)))
         (dolist (l (funcall bar-lines))
           (should (<= (string-width (string-trim-right l)) width))))
       ;; and kept as its last state: the frames tqdm drew over are not
       ;; saved, as JupyterLab saves them, so the notebook holds one bar
       (let ((stderr (cl-loop for o across (emjupy-cell-outputs cell)
                              when (equal (gethash "name" o) "stderr")
                              concat (emjupy--mime-text (gethash "text" o)))))
         (should (= (cl-count ?% stderr) 1))
         (should (string-match-p "100%|.*50/50" stderr)))))))

(ert-deftest emjupy-int-autosave-saves-and-never-overwrites-elsewhere ()
  "Auto-save writes what was typed to the server, but never over a change made elsewhere.
Against a real server: a save fetches when the file last changed and
compares it with what was opened, so a notebook saved from JupyterLab
meanwhile is left alone, and auto-saving stops for it."
  (emjupy-int--with-live-kernel
   (with-current-buffer emjupy-int--buffer
     (let* ((emjupy-autosave-interval 0)
            (emjupy-recovery-directory (make-temp-file "emjupy-recovery" t))
            (nb emjupy--buffer-notebook)
            (server (emjupy-notebook-server nb))
            (path (emjupy-notebook-path nb))
            (fetch (lambda ()
                     (json-serialize
                      (gethash
                       "content" (emjupy--http-request
                                  "GET"
                                  server (emjupy--contents-path path)))))))
       (unwind-protect
           (progn
             (setq emjupy--autosave-paused nil emjupy--autosave-last nil)
             (setf (emjupy-notebook-last-modified nb)
                   (emjupy--server-last-modified nb))
             ;; typed, then saved on its own
             (goto-char
              (1- (overlay-end (emjupy-cell-overlay
                                (aref (emjupy-notebook-cells nb) 0)))))
             (let ((inhibit-read-only t)) (insert "  # autosaved"))
             (emjupy--idle-save)
             ;; not waited for by Emacs; the test waits for it
             (emjupy-int--pump 10 (lambda () (not emjupy--autosave-in-flight)))
             (should-not (buffer-modified-p))
             (should (string-match-p "# autosaved" (funcall fetch)))
             ;; written meanwhile by another client
             (let ((body (make-hash-table :test 'equal))
                   (content (gethash "content" (emjupy--http-request
                                                "GET" server
                                                (emjupy--contents-path path)))))
               (puthash "source" "# from elsewhere"
                        (aref (gethash "cells" content) 0))
               (puthash "type" "notebook" body) (puthash "format" "json" body)
               (puthash "content" content body)
               (sleep-for 1.1)                ; a later last_modified
               (emjupy--http-request "PUT" server (emjupy--contents-path path)
                                     (json-serialize body)))
             (let ((inhibit-read-only t)) (insert "  # mine"))
             (setq emjupy--autosave-last nil)
             (emjupy--idle-save)
             ;; not waited for by Emacs; the test waits for it
             (emjupy-int--pump 10 (lambda () (not emjupy--autosave-in-flight)))
             (should emjupy--autosave-paused)
             (should (buffer-modified-p))
             (should (string-match-p "# from elsewhere" (funcall fetch)))
             (should-not (string-match-p "# mine" (funcall fetch))))
         (set-buffer-modified-p nil)
         (delete-directory emjupy-recovery-directory t))))))

(ert-deftest emjupy-int-a-notebook-file-opened-as-text-opens-on-a-local-kernel
    ()
  "An .ipynb file shown as text opens as a notebook, on a server started for it.
The environment.yml beside it names the environment offered; a server is
started there, the text replaced by the notebook, its kernel runs in the
notebook's directory, and the server stops when the notebook is closed."
  (let* ((jupyter (executable-find "jupyter"))
         (dir (make-temp-file "emjupy-here" t))
         (file (expand-file-name "analysis.ipynb" dir))
         (offered nil))
    (unless jupyter (ert-skip "No jupyter on the PATH"))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "{\"cells\": [{\"cell_type\": \"code\", \"execution_count\": null, \"id\": \"a1\", \"metadata\": {}, \"outputs\": [], \"source\": [\"x = 6 * 7\"]}], \"metadata\": {}, \"nbformat\": 4, \"nbformat_minor\": 5}"))
          (with-temp-file (expand-file-name "environment.yml" dir)
            (insert "name: analysis\n"))
          (cl-letf (((symbol-function 'completing-read)
                     (lambda (_p _c &rest args)
                       (setq offered (nth 4 args))
                       ;; the environment jupyter is in: its bin's parent
                       (file-name-directory
                        (directory-file-name (file-name-directory jupyter)))))
                    ((symbol-function 'emjupy--offer-recovery) #'ignore))
            (let* ((emjupy-language-support nil)
                   (text (find-file-noselect file))
                   (notebook (with-current-buffer text
                               (switch-to-buffer text)
                               (emjupy-open-this-notebook)))
                   (process (plist-get (car emjupy--local-servers) :process))
                   (answer nil))
              (should (equal offered "analysis"))
              (should-not (buffer-live-p text))
              (should (eq (buffer-local-value 'major-mode notebook)
                          'emjupy-mode))
              (with-current-buffer notebook
                (emjupy-int--pump 30 (lambda () (emjupy--ws-live-p)))
                (emjupy--kernel-eval
                 (emjupy-notebook-kernel emjupy--buffer-notebook)
                 "import os; print(os.path.basename(os.getcwd()), 6 * 7)"
                 (lambda (out) (setq answer
                                     (string-trim out))))
                (emjupy-int--pump 20 (lambda () answer))
                (should (equal answer (format "%s 42"
                                              (file-name-nondirectory dir))))
                (set-buffer-modified-p nil))
              (let ((kill-buffer-query-functions nil)) (kill-buffer notebook))
              (emjupy-int--pump 1)
              (should-not (process-live-p process)))))
      (delete-directory dir t))))

(defun emjupy-int--run-answering (code answers &optional password-answer)
  "Run CODE in the first cell, answering the kernel's prompts with ANSWERS.
ANSWERS are given in turn to `read-string', or `quit' to press C-g; a
password prompt gets PASSWORD-ANSWER.  Return the cell's output text and
the prompts asked, once the cell has finished."
  (let ((prompts nil))
    (cl-letf (((symbol-function 'read-string)
               (lambda (prompt &rest _)
                 (push prompt prompts)
                 (let ((a (pop answers)))
                   (if (eq a 'quit) (signal 'quit nil) a))))
              ((symbol-function 'read-passwd)
               (lambda (prompt &rest _) (push prompt prompts) password-answer)))
      (let ((cell (emjupy-int--run code 30)))
        (with-current-buffer emjupy-int--buffer
          (emjupy-int--pump 20 (lambda () (not (member (emjupy-cell-id cell)
                                                       emjupy--running-cells))))
          (emjupy-flush-output (current-buffer))
          (list (mapconcat
                 (lambda (o) (if (gethash "text" o)
                                 (emjupy--mime-text (gethash "text" o))
                               (or (gethash "ename" o) "")))
                 (append (emjupy-cell-outputs cell) nil) "")
                (reverse prompts)
                (member (emjupy-cell-id cell) emjupy--running-cells)))))))

(ert-deftest emjupy-int-a-cell-asking-for-input-is-answered ()
  "input(), getpass, pdb and breakpoint() ask in the minibuffer, and are answered.
They failed at once -- StdinNotImplementedError -- as emjupy told the
kernel it took no input and answered no request for it."
  (emjupy-int--with-live-kernel
   (let ((emjupy--input-history nil))
     ;; input()
     (pcase-let ((`(,out ,prompts ,running) (emjupy-int--run-answering
                                             "x = input('name? ')\nprint('hi', x)" '("Ada"))))
       (should (string-match-p "hi Ada" out))
       (should (string-match-p "name\\? \\'" (car prompts)))
       (should-not running))
     ;; getpass: a password prompt, not kept
     (pcase-let ((`(,out ,_ ,_) (emjupy-int--run-answering
                                 "import getpass\np = getpass.getpass('pw: ')\nprint('length', len(p))"
                                 nil "s3cret")))
       (should (string-match-p "length 6" out))
       (should-not (member "s3cret" emjupy--input-history)))
     ;; pdb: a command evaluated, then continue
     (pcase-let ((`(,out ,prompts ,running) (emjupy-int--run-answering
                                             "import pdb\nz = 6\npdb.set_trace()\nprint('after', z)"
                                             '("p z * 7" "c"))))
       (should (string-match-p "42" out))
       (should (string-match-p "after 6" out))
       (should (cl-every (lambda (p)
                           (string-match-p "(Pdb)\\|ipdb>" p)) prompts))
       (should-not running))
     ;; breakpoint()
     (pcase-let ((`(,out ,_ ,running) (emjupy-int--run-answering
                                       "w = 2\nbreakpoint()\nprint('done', w)"
                                       '("c"))))
       (should (string-match-p "done 2" out))
       (should-not running))
     ;; C-g at the prompt: the kernel interrupted, not left waiting
     (pcase-let ((`(,out ,_ ,running) (emjupy-int--run-answering
                                       "y = input('never answered: ')"
                                       '(quit))))
       (should (string-match-p "KeyboardInterrupt" out))
       (should-not running)))))

(ert-deftest emjupy-int-output-survives-a-dropped-connection ()
  "A cell running when the connection drops keeps all its output.
The server keeps a session's messages while its connection is down and
replays them when the same session connects again: emjupy keeps the
cells, reconnects on its own under the same session, and the output
arrives."
  (emjupy-int--with-live-kernel
   (with-current-buffer emjupy-int--buffer
     (let* ((emjupy-reconnect-window 30)
            (cell (aref (emjupy-notebook-cells emjupy--buffer-notebook) 0))
            (inhibit-read-only t))
       (let ((ov (emjupy-cell-overlay cell)))
         (delete-region (overlay-start ov) (1- (overlay-end ov)))
         (goto-char (overlay-start ov))
         (insert "import time\nfor i in range(25):\n    print('tick', i, flush=True); time.sleep(0.2)"))
       (emjupy-execute-cell-at-point)
       (emjupy-int--pump 1.2)
       ;; the link drops: the connection gone, not closed by emjupy
       (let ((kernel (emjupy-notebook-kernel emjupy--buffer-notebook)))
         (delete-process (websocket-conn (emjupy-kernel-ws kernel))))
       (emjupy-int--pump 1)
       ;; kept running meanwhile, not ended
       (should (member (emjupy-cell-id cell) emjupy--running-cells))
       ;; reconnected on its own; the run finishes, every tick there
       (emjupy-int--pump 25 (lambda () (not (member (emjupy-cell-id cell)
                                                    emjupy--running-cells))))
       (emjupy-flush-output (current-buffer))
       (should (emjupy--ws-live-p))
       (should-not (member (emjupy-cell-id cell) emjupy--running-cells))
       (let ((text (emjupy--mime-text
                    (gethash "text" (aref (emjupy-cell-outputs cell) 0)))))
         (dotimes (i 25)
           (ert-info ((format "tick %d" i))
             (should (string-match-p (format "^tick %d$" i) text)))))))))

(ert-deftest emjupy-int-nested-tqdm-leaves-one-bar ()
  "Nested tqdm bars end as a terminal shows them: one finished outer bar.
The inner bar, leave=False, is cleared each pass, leaving no stale copy
behind."
  (emjupy-int--with-live-kernel
   (let ((cell (emjupy-int--run "from tqdm import tqdm\nimport time
for i in tqdm(range(3), desc='outer', ncols=60):
    for j in tqdm(range(10), desc='inner', leave=False, ncols=60): time.sleep(0.005)" 60)))
     (when (string-match-p "No module named"
                           (format "%S" (emjupy-cell-outputs cell)))
       (ert-skip "tqdm is not installed where the kernel runs"))
     (with-current-buffer emjupy-int--buffer
       (emjupy-flush-output (current-buffer))
       (let* ((ov (emjupy-cell-output-ov cell))
              (shown (split-string (string-trim (buffer-substring-no-properties
                                                 (overlay-start ov)
                                                 (overlay-end ov)))
                                   "\n")))
         (should (= (cl-count-if (lambda (l)
                                   (string-match-p "outer" l)) shown) 1))
         (should (string-match-p "outer: 100%"
                                 (car (seq-filter (lambda (l) (string-match-p "outer" l)) shown))))
         (should-not (seq-some (lambda (l)
                                 (string-match-p "inner" l)) shown)))))))

(provide 'emjupy-integration-test)
;;; emjupy-integration-test.el ends here
