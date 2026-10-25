;;; emjupy-eglot-test.el --- Unit tests: emjupy-eglot.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-eglot.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-shadow-path-includes-server ()
  "Likewise for the Eglot shadow file: two servers hosting
`analysis.ipynb' must not share one shadow buffer, or each would
clobber the other's code."
  (let* ((a (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:18888")))
         (b (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:19999"))))
    (should-not (equal (emjupy--shadow-file-path a)
                       (emjupy--shadow-file-path b)))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-shadow-delegate-skips-markdown-cells ()
  "The shadow-buffer delegate must no-op for markdown cells -- no LSP
server applies to prose -- without needing Eglot or any process."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "hi"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should-not (emjupy--cell-shadow-delegate (lambda (&rest _)
                                                    'should-not-run)))))))

(ert-deftest emjupy-test-shadow-delegate-maps-position-for-code-cells ()
  "For a code cell, the delegate must land in the shadow buffer at the
position corresponding to point in the notebook buffer -- pure
marker/position-mapping mechanics, no live LSP server required."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "aaa = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "bbb = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (forward-char 3) ;; land right after "bbb"
        (let (seen)
          (emjupy--cell-shadow-delegate
           (lambda (_cell-start _shadow-start _buf)
             (setq seen (buffer-substring-no-properties (max (point-min)
                                                             (- (point) 3))
                                                        (point)))))
          (should (equal seen "bbb")))))))

(ert-deftest emjupy-test-eglot-cross-cell-completion ()
  "End-to-end: a name defined in one cell is a completion candidate
while editing a DIFFERENT cell, automatically, with no
`emjupy-edit-cell-externally' call anywhere -- this is the actual
IDE-experience feature. Needs a real Python language server on PATH;
skipped otherwise so the rest of the suite isn't held hostage to it."
  (unless (and (emjupy-test--python-lsp-available-p) (require 'eglot
                                                              nil 'noerror))
    (ert-skip "No Python language server (pylsp/pyright/jedi-language-server) on PATH"))
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "MY_TEST_MARKER_VALUE = 42\n"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "MY_TEST_MARKER"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (ignore-errors (emjupy--ensure-shadow-buffer nb))
      (let ((sbuf (emjupy-notebook-shadow-buffer nb)))
        (unless sbuf (ert-skip "Shadow buffer could not be created"))
        (with-current-buffer sbuf
          (let ((w 0))
            (while (and (< w 20)
                        (not (and (eglot-current-server)
                                  (jsonrpc-running-p (eglot-current-server)))))
              (accept-process-output nil 1.0) (setq w (1+ w))))
          (unless (eglot-current-server)
            (ert-skip "Eglot did not connect within timeout"))))
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (goto-char (line-end-position))
        (let ((result (run-hook-with-args-until-success
                       'completion-at-point-functions)))
          (should (consp result))
          (when (consp result)
            (let* ((start (nth 0 result)) (end (nth 1 result))
                   (cands (all-completions "" (nth 2 result) nil)))
              (should (>= start (overlay-start (emjupy-cell-overlay c2))))
              (should (<= end (overlay-end (emjupy-cell-overlay c2))))
              (should (member "MY_TEST_MARKER_VALUE" cands)))))))))

(ert-deftest emjupy-test-completion-and-eldoc-survive-a-dead-server ()
  "Neither completion nor eldoc may signal when the language server is
gone -- they simply have nothing to offer."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import os\nos.pat" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-letf (((symbol-function 'emjupy--eglot-live-server)
                   (lambda (&rest _) nil)))
          (goto-char (point-min))
          (should-not (emjupy--cell-completion-at-point))
          (should-not (emjupy--cell-eldoc-function #'ignore)))))))

(ert-deftest emjupy-test-eglot-advice-is-inert-outside-notebooks ()
  "The advice must not change how Eglot commands behave in ordinary
buffers -- it only redirects inside `emjupy-mode'."
  (with-temp-buffer
    (python-mode)
    (should (equal (emjupy--eglot-command-advice (lambda (&rest args)
                                                   (cons 'ran args))
                                                 1 2)
                   '(ran 1 2)))))

(ert-deftest emjupy-test-eglot-delegate-errors-clearly-without-a-server ()
  "With no language server the user gets a plain explanation, not a raw
jsonrpc-error out of Eglot's innards."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (cl-letf (((symbol-function 'emjupy--eglot-live-server)
                   (lambda (&rest _) nil)))
          (should-error (emjupy-eglot-delegate #'ignore) :type 'user-error))))))

(ert-deftest emjupy-test-eglot-delegate-refuses-outside-a-cell ()
  "Point has to be in a cell for there to be anything to delegate."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-max))
        (should-error (emjupy-eglot-delegate #'ignore) :type 'user-error)))))

(ert-deftest emjupy-test-pull-shadow-into-cells ()
  "Edits an Eglot command made in the shadow buffer come back into the
cells -- including edits spanning more than one cell, which is the whole
reason the shadow buffer exists."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "aaa = 1" :outputs []
                               :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "print(aaa)" :outputs []
                               :metadata (make-hash-table))))
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
  "Every command in `emjupy-eglot-delegated-commands' is actually advised."
  (dolist (cmd emjupy-eglot-delegated-commands)
    (should (advice-member-p #'emjupy--eglot-command-advice cmd))))

(ert-deftest emjupy-test-xref-backend-only-in-notebooks ()
  "The xref backend announces itself only where there is a notebook."
  (with-temp-buffer
    (should-not (emjupy--xref-backend)))
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should (eq (emjupy--xref-backend) 'emjupy))
        (should (eq (run-hook-with-args-until-success 'xref-backend-functions)
                    'emjupy))))))

(ert-deftest emjupy-test-xref-remap-leaves-foreign-locations-alone ()
  "An xref into some other file -- a library, say -- must be handed back
untouched: the real file is the right destination there."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((item (xref-make "elsewhere"
                               (xref-make-file-location
                                "/usr/lib/python/os.py" 10 0))))
          (should (equal (emjupy--xref-remap nb (list item)) (list item))))))))

(ert-deftest emjupy-test-shadow-refresh-does-no-remote-io ()
  "Refreshing the shadow buffer must not touch the file system.

It runs on the completion, eldoc and xref paths -- once per keystroke --
and over TRAMP each of these is an ssh round trip.  Measured against ssh
on localhost, before any network: `write-region' 214 ms, and a lock file
per modification another 100 ms."
  (let ((calls nil))
    (cl-letf* ((real-write (symbol-function 'write-region))
               ((symbol-function 'write-region)
                (lambda (&rest args) (push 'write calls)
                  (apply real-write args)))
               ((symbol-function 'make-directory)
                (lambda (&rest _) (push 'mkdir calls) t)))
      (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                    :source "x = 1"
                                    :outputs [] :metadata (make-hash-table))))
        (emjupy-test--with-notebook (vector cell) buf nb
          (with-current-buffer buf
            (ignore-errors (emjupy--ensure-shadow-buffer nb))
            ;; creation may do I/O; what follows must not
            (setq calls nil)
            (dotimes (i 3)
              (setf (emjupy-cell-source cell) (format "x = %d" i))
              (ignore-errors (emjupy--ensure-shadow-buffer nb)))
            (should-not (memq 'write calls))
            (should-not (memq 'mkdir calls))))))))

(ert-deftest emjupy-test-shadow-buffer-has-no-lock-files ()
  "Lock files are off in the shadow buffer.

Emacs writes a `.#name' lock beside a visited file whenever the buffer
becomes modified.  On a remote file that is remote I/O per keystroke --
121 ms against localhost ssh, versus 20 with locking off -- and the lock
guards against a second editor that cannot exist for a generated file."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((shadow (ignore-errors
                        (emjupy--ensure-shadow-buffer nb))))
          (when (buffer-live-p shadow)
            (with-current-buffer shadow
              (should (local-variable-p 'create-lockfiles))
              (should-not create-lockfiles))))))))

(ert-deftest emjupy-test-shadow-sync-to-disk-option ()
  "`always' restores the old write-on-every-edit behaviour."
  (should (eq emjupy-shadow-sync-to-disk 'lazy))
  (let ((writes 0))
    (cl-letf* ((real-write (symbol-function 'write-region))
               ((symbol-function 'write-region)
                (lambda (&rest args) (setq writes (1+ writes))
                  (apply real-write args))))
      (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                    :source "x = 1"
                                    :outputs [] :metadata (make-hash-table))))
        (emjupy-test--with-notebook (vector cell) buf nb
          (with-current-buffer buf
            (ignore-errors (emjupy--ensure-shadow-buffer nb))
            (setq writes 0)
            (let ((emjupy-shadow-sync-to-disk 'always))
              (setf (emjupy-cell-source cell) "x = 2")
              (ignore-errors (emjupy--ensure-shadow-buffer nb)))
            (should (> writes 0))))))))

(ert-deftest emjupy-test-shadow-buffer-follows-a-changed-path ()
  "The shadow buffer moves when its path changes.

The kernel reports its working directory a moment AFTER it connects, so
the first shadow buffer of a session is created before that answer
arrives and lands in the fallback temp directory.  Reusing it thereafter
left the language server reading a local scratch file, and imports of
the user's own modules stayed unresolvable however
`emjupy-shadow-host' was set."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x = 1"
                                 :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector cell) :path "nb.ipynb"))
         (buf (generate-new-buffer "*move*"))
         (later (expand-file-name "emjupy-cwd-test" temporary-file-directory)))
    (make-directory later t)
    (unwind-protect
        (with-current-buffer buf
          (emjupy-mode)
          (setq emjupy--buffer-notebook nb)
          (setf (emjupy-notebook-buffer nb) buf)
          (emjupy--rerender-notebook)
          (let ((emjupy-shadow-directory nil) (emjupy-remote-root nil))
            (ignore-errors (emjupy--ensure-shadow-buffer nb))
            (let ((first (buffer-local-value
                          'buffer-file-name
                          (emjupy-notebook-shadow-buffer nb))))
              ;; the kernel now answers
              (setf (emjupy-notebook-kernel-cwd nb) later)
              (ignore-errors (emjupy--ensure-shadow-buffer nb))
              (let ((second (buffer-local-value
                             'buffer-file-name
                             (emjupy-notebook-shadow-buffer nb))))
                (should-not (equal first second))
                (should (string-prefix-p
                         (file-name-as-directory later) second))))))
      (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))

(ert-deftest emjupy-test-shadow-buffer-killed-without-asking ()
  "Relocating the shadow buffer must not stop to ask.

It visits a file but is a regenerated copy of the cells, and since edits
are no longer written through it is always \"modified\" -- so `kill-buffer'
would prompt every time the path changed."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x = 1"
                                 :outputs [] :metadata (make-hash-table)))
         (asked nil)
         (later (expand-file-name "emjupy-kill-test" temporary-file-directory)))
    (make-directory later t)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
        (let ((emjupy-shadow-directory nil) (emjupy-remote-root nil))
          (ignore-errors (emjupy--ensure-shadow-buffer emjupy--buffer-notebook))
          (let ((shadow (emjupy-notebook-shadow-buffer nb)))
            (when (buffer-live-p shadow)
              ;; make it dirty, as ordinary use does
              (with-current-buffer shadow (insert "\n# edited"))
              (cl-letf (((symbol-function 'y-or-n-p)
                         (lambda (&rest _) (setq asked t) t)))
                (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
                      later)
                (ignore-errors (emjupy--ensure-shadow-buffer nb)))
              (should-not asked))))))))

(ert-deftest emjupy-test-shadow-is-built-for-eglot-either-way ()
  "The shadow buffer is what Eglot manages, on either transport.

Eglot rides the WebSocket too, so the shadow is the buffer it attaches
to -- local, and built whichever server is underneath."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((built nil))
          (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                     (lambda (&rest _) (setq built t) nil)))
            ;; the delegate needs a real buffer to hand on to; what matters
            ;; here is only that it asked for one
            (ignore-errors
              (emjupy--cell-shadow-delegate (lambda (&rest _) t)))
            (should built)))
        ;; the exception stands: a kernel this Emacs cannot reach, with no
        ;; way given to reach it, would have the server describe the wrong
        ;; machine, so nothing is built
        (let ((emjupy-shadow-host nil)
              (emjupy-shadow-when-kernel-unreachable nil))
          (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
                "/srv/nowhere")
          (let ((built nil))
            (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                       (lambda (&rest _) (setq built t) nil)))
              (ignore-errors (emjupy--cell-shadow-delegate (lambda (&rest _) t)))
              (should-not built))))))))

(ert-deftest emjupy-test-eldoc-never-blocks ()
  "Asking the language server must not hold the editor.

eldoc runs after EVERY command: a blocking wait there costs the full
request timeout per cursor movement while the server is slow or silent,
indistinguishable from a hang.

The request is asserted to have been SENT, not only to have returned
quickly: a call failing at once would pass the timing having asked
nothing."
  :tags '(:timing)
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "os.pa"
                                :outputs [] :metadata (make-hash-table)))
        (requests 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (cl-letf (((symbol-function 'emjupy--cell-shadow-delegate)
                   (lambda (fn) (funcall fn 1 1 (current-buffer))))
                  ((symbol-function 'emjupy--eglot-live-server)
                   (lambda (&rest _) 'a-server))
                  ((symbol-function 'emjupy--eglot-capable-p)
                   (lambda (&rest _) t))
                  ((symbol-function 'eglot--TextDocumentPositionParams)
                   (lambda () nil))
                  ;; a server that never answers
                  ((symbol-function 'jsonrpc-async-request)
                   (lambda (&rest _) (setq requests (1+ requests)) nil)))
          (let ((start (float-time)))
            (dotimes (_ 5) (emjupy--cell-eldoc-function #'ignore))
            ;; five ticks, well under a second in total
            (should (< (- (float-time) start) 1.0))
            ;; and each one really did ask
            (should (= requests 5))))))))

(ert-deftest emjupy-test-language-support-switch ()
  "`emjupy-language-support' nil silences every per-command language path.

One switch, so a slowdown can be attributed in a single setq: with it
off nothing runs after a command except emjupy's own bookkeeping, and
the notebook still works."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import os"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((emjupy-language-support nil)
              (asked nil))
          (cl-letf (((symbol-function 'emjupy--cell-completion-at-point)
                     (lambda (&rest _) (setq asked t) nil))
                    ((symbol-function 'emjupy--cell-eldoc-function)
                     (lambda (&rest _) (setq asked t) nil)))
            (emjupy--capf)
            (emjupy--eldoc #'ignore)
            (should-not asked)))
        ;; and the notebook itself is unaffected
        (should (string-match-p "import os" (buffer-string)))))))

(ert-deftest emjupy-test-language-support-off-attaches-no-server ()
  "With `emjupy-language-support' nil no server is started for a notebook.
The switch was read by completion, eldoc and `emjupy-start-language-support'
only.  The warm-up when a notebook opens, and the commands that ask a
server for something, all came through `emjupy--ensure-shadow-buffer',
which attached Eglot whatever the switch said -- so a notebook opened with
it off still tried to start a server, and failed, and said so.  The shadow
buffer is still built: editing all the code in one buffer needs no server."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import os"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((emjupy-language-support nil))
          (should (= 0 (emjupy-test--counting-server-starts starts
                         ;; the buffer is built, and nothing is attached
                         (should (buffer-live-p
                                  (emjupy--ensure-shadow-buffer nb)))))))
        ;; a command asking a server for something says there is none, and
        ;; starts none
        (let ((emjupy-language-support nil))
          (should (= 0 (emjupy-test--counting-server-starts starts
                         (should-error (emjupy-eglot-delegate #'ignore)
                                       :type 'user-error)))))
        ;; The control: the same, switched on, does start one -- so the counts
        ;; above are of something that happens.
        (let ((emjupy-language-support t))
          (should (= 1 (emjupy-test--counting-server-starts starts
                         (emjupy--ensure-shadow-buffer nb)))))))))

(ert-deftest emjupy-test-language-support-off-leaves-m-dot-to-other-backends ()
  "M-. is not taken by emjupy when `emjupy-language-support' is nil.
Its xref backend was registered for every notebook and never looked at the
switch, so M-. went through the shadow buffer and started a server."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((emjupy-language-support nil))
          (should-not (emjupy--xref-backend)))
        (let ((emjupy-language-support t))
          (should (eq (emjupy--xref-backend) 'emjupy)))))
    (with-temp-buffer (should-not (emjupy--xref-backend)))))

(ert-deftest emjupy-test-the-edit-message-does-not-claim-a-server-that-is-off ()
  "Editing all the code in one buffer says Eglot is active only when it can be."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (dolist (case '((nil . nil) (t . t)))
          (let ((emjupy-language-support (car case)) (said nil))
            (save-window-excursion
              (cl-letf (((symbol-function 'message)
                         (lambda (f &rest args) (when f
                                                  (push (apply #'format
                                                               f args) said))))
                        ((symbol-function 'eglot--connect) #'ignore)
                        ((symbol-function 'eglot--guess-contact)
                         (lambda (&rest _) nil))
                        ((symbol-function 'emjupy--warn-local-language-server)
                         #'ignore))
                (emjupy-edit-cell-externally)))
            (should (cl-some (lambda (m)
                               (string-match-p "Editing notebook code" m))
                             said))
            (should (eq (cdr case)
                        (and (cl-some (lambda (m)
                                        (string-match-p "Eglot active" m))
                                      said) t)))))))))

(ert-deftest emjupy-test-start-language-support-by-hand-says-when-it-is-off ()
  "`M-x emjupy-start-language-support' in a notebook with the switch off says so."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((emjupy-language-support nil))
          (let ((err (should-error (call-interactively
                                    #'emjupy-start-language-support)
                                   :type 'user-error)))
            (should (string-match-p "Language support is off" (cadr err)))))))))

(ert-deftest emjupy-test-language-support-starts-eagerly ()
  "Language support begins when the notebook learns where its kernel is.

Waiting for the first completion means the first request of a session
pays for the handshake, and a server that will never start stays silent
until something asks -- by which time the user is mid-task."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table)))
        (built 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
              temporary-file-directory)
        (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                   (lambda (&rest _) (setq built (1+ built)) nil)))
          (emjupy-start-language-support emjupy--buffer-notebook)
          (should (= built 1)))
        ;; and it respects the master switch
        (let ((emjupy-language-support nil)
              (asked 0))
          (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                     (lambda (&rest _) (setq asked (1+ asked)) nil)))
            (should-not (emjupy-start-language-support emjupy--buffer-notebook))
            (should (= asked 0))))))))

(ert-deftest emjupy-test-shadow-works-without-a-language-server ()
  "The shadow buffer is built and moved on a machine with no language server.

The back-off exists for work that can hang -- a remote directory, a
language server that will not start.  It briefly covered this whole
function, so where no language server is installed the first attempt
failed, set the back-off, and every later call returned early: the
buffer was never built, and never moved when its path changed.  That is
how the build broke, since a build machine has no pylsp."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x = 1"
                                 :outputs [] :metadata (make-hash-table)))
         (later (expand-file-name "emjupy-noserver-test"
                                  temporary-file-directory)))
    (make-directory later t)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
        (cl-letf (((symbol-function 'eglot--connect)
                   (lambda (&rest _) (error "no language server here"))))
          (let ((emjupy-shadow-directory nil) (emjupy-remote-root nil))
            ;; built, despite the failed attach
            (ignore-errors (emjupy--ensure-shadow-buffer nb))
            (let ((first (buffer-local-value
                          'buffer-file-name
                          (emjupy-notebook-shadow-buffer nb))))
              (should first)
              ;; and still moved when the kernel reports a directory
              (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook) later)
              (ignore-errors (emjupy--ensure-shadow-buffer nb))
              (let ((second (buffer-local-value
                             'buffer-file-name
                             (emjupy-notebook-shadow-buffer nb))))
                (should-not (equal first second))
                (should (string-prefix-p
                         (file-name-as-directory later) second))))))))))

(ert-deftest emjupy-test-websocket-claims-the-shadow-from-plain-eglot ()
  "A plain Eglot server does not keep the WebSocket one out.

Anything asking for completion or documentation builds the shadow
buffer, and that path starts a server the ordinary way.  On a notebook
whose kernel reports its directory a moment later, the ordinary server
therefore wins the race, and skipping when ANY server was attached meant
the WebSocket one was never tried -- leaving a language server reading a
scratch file over TRAMP, which is what the WebSocket transport exists to
avoid.  What matters is whether the attached server is ours."
  (let ((shut 0))
    (with-temp-buffer
      ;; nothing attached: free to claim
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&rest _) nil)))
        (should (emjupy--claim-shadow-for-websocket (current-buffer))))
      ;; a plain server: shut down, then claimed
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&rest _) 'a-plain-server))
                ((symbol-function 'object-of-class-p) (lambda (&rest _) nil))
                ((symbol-function 'eglot-shutdown)
                 (lambda (&rest _) (setq shut (1+ shut)))))
        (should (emjupy--claim-shadow-for-websocket (current-buffer)))
        (should (= shut 1)))
      ;; already ours: left alone, and not claimed again
      (cl-letf (((symbol-function 'eglot-current-server)
                 (lambda (&rest _) 'our-server))
                ((symbol-function 'object-of-class-p) (lambda (&rest _) t))
                ((symbol-function 'eglot-shutdown)
                 (lambda (&rest _) (setq shut (1+ shut)))))
        (should-not (emjupy--claim-shadow-for-websocket (current-buffer)))
        (should (= shut 1))))))

(ert-deftest emjupy-test-locality-warning-only-when-it-applies ()
  "The message about the kernel running elsewhere waits until it is true.

It fired whenever the WebSocket transport was not yet in charge, which
is the case every time a notebook is opened -- so opening a second
notebook announced that completions describe this machine and advised
installing something already installed and about to be used."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table)))
        (warned 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (cl-letf (((symbol-function 'emjupy--warn-shadow-is-local)
                   (lambda (&rest _) (setq warned (1+ warned))))
                  ((symbol-function 'emjupy--ensure-shadow-buffer)
                   (lambda (&rest _) nil))
                  ((symbol-function 'emjupy--lsp-in-charge-p)
                   (lambda (&rest _) nil)))
          ;; still connecting: nothing said
          (let ((emjupy-lsp-enabled t))
            (setq emjupy--shadow-blocked-until nil)
            (ignore-errors (emjupy--cell-shadow-delegate (lambda (&rest _) t)))
            (should (= warned 0)))
          ;; the attempt has failed, so the shadow really is what answers
          (let ((emjupy-lsp-enabled t))
            (setq emjupy--shadow-blocked-until (time-add (current-time) 30))
            (ignore-errors (emjupy--cell-shadow-delegate (lambda (&rest _) t)))
            (should (= warned 1)))
          ;; or the transport was never wanted
          (let ((emjupy-lsp-enabled nil))
            (setq emjupy--shadow-blocked-until nil)
            (ignore-errors (emjupy--cell-shadow-delegate (lambda (&rest _) t)))
            (should (= warned 2))))))))

(ert-deftest emjupy-test-diagnose-reads-the-attached-server ()
  "The report describes the server Eglot has attached, not a stale slot.

A slot of its own could go stale, and say the socket never opened however
well it works."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-server emjupy--buffer-notebook)
              (make-emjupy-server :base-url "h:1" :token "t"))
        (let ((shadow (generate-new-buffer " *shadow*")))
          (setf (emjupy-notebook-shadow-buffer emjupy--buffer-notebook) shadow)
          (unwind-protect
              (cl-letf (((symbol-function 'eglot-current-server)
                         (lambda (&rest _) 'a-server))
                        ((symbol-function 'object-of-class-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'jsonrpc-running-p)
                         (lambda (&rest _) t))
                        ((symbol-function 'display-buffer) #'ignore))
                (emjupy-lsp-diagnose)
                (with-current-buffer "*emjupy language server*"
                  (should (string-match-p "over the Jupyter WebSocket"
                                          (buffer-string)))
                  (should (string-match-p "connection      running"
                                          (buffer-string)))
                  (should-not (string-match-p "never opened" (buffer-string)))))
            (let ((kill-buffer-query-functions nil)) (kill-buffer shadow))))))))

;;;; Performance

(ert-deftest emjupy-test-per-command-work-is-small ()
  "What runs after every command must not depend on the notebook's size.

eldoc and completion run on every keystroke.  Anything proportional to
the whole notebook there is felt directly, and anything that touches the
network is felt tenfold."
  :tags '(:timing)
  (let ((cells (emjupy-test--big-notebook 60 20)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 30))))
        ;; no language server in a unit test: this measures emjupy's own share
        ;; -- finding the cell, syncing, reaching the delegate.  Once first,
        ;; untimed: the first call creates the shadow buffer, once per notebook,
        ;; which spread over fifty calls dominated the average.
        (ignore-errors (emjupy--cell-shadow-delegate (lambda (&rest _) nil)))
        (let ((per-call (/ (emjupy-test--elapsed
                             (dotimes (_ 50)
                               (ignore-errors (emjupy--cell-shadow-delegate
                                               (lambda (&rest _) nil)))))
                           50.0)))
          (should (< per-call 0.05)))
        (let ((per-move (/ (emjupy-test--elapsed
                             (dotimes (_ 200)
                               (run-hooks 'pre-command-hook)
                               (ignore-errors (forward-line 1))))
                           200.0)))
          (should (< per-move 0.01)))))))

(ert-deftest emjupy-test-shadow-file-is-hidden ()
  "The shadow file is hidden.

It belongs to emjupy rather than to the user, and it may sit in the
directory they are working in, so it stays out of their listings."
  (let ((nb (make-emjupy-notebook
             :cells [] :path "analysis.ipynb"
             :server (make-emjupy-server :base-url "box:8888" :token "t"))))
    (let ((name (file-name-nondirectory (emjupy--shadow-file-path nb))))
      (should (string-prefix-p ".emjupy_" name))
      ;; still tells you which server and notebook it belongs to
      (should (string-match-p "analysis" name))
      (should (string-match-p "8888" name)))))

(ert-deftest emjupy-test-diagnose-does-not-print-the-token ()
  "The language server report is safe to paste.

It prints the WebSocket URL, which carries the token."
  (let ((cell (emjupy-test--cell-like 'code "x = 1")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-server emjupy--buffer-notebook)
              (make-emjupy-server :base-url "localhost:9999" :token "s3cret"))
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (emjupy-lsp-diagnose))
        (with-current-buffer "*emjupy language server*"
          (should-not (string-match-p "s3cret" (buffer-string)))
          (should (string-match-p "<redacted>" (buffer-string)))
          ;; still says which server, which is the point of the line
          (should (string-match-p "localhost:9999" (buffer-string))))))))

;;;; The undo guard

(ert-deftest emjupy-test-diagnose-reports-why-the-socket-was-not-used ()
  "The report says why the WebSocket route was not taken.

Without it the report described a healthy notebook -- lsp enabled, no
back-off -- while a local language server was running, because the
branch where the socket opens and Eglot then refuses to attach recorded
nothing.  A message in the echo area during startup is gone by the time
anyone asks."
  (let ((cell (emjupy-test--cell-like 'code "x = 1")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-server emjupy--buffer-notebook)
              (make-emjupy-server :base-url "h:1" :token "t"))
        (cl-letf (((symbol-function 'display-buffer) #'ignore))
          (let ((emjupy--lsp-last-failure nil))
            (emjupy-lsp-diagnose)
            (with-current-buffer "*emjupy language server*"
              (should (string-match-p "socket failure  none recorded"
                                      (buffer-string)))))
          (let ((emjupy--lsp-last-failure "Eglot would not attach: wrong number of arguments"))
            (emjupy-lsp-diagnose)
            (with-current-buffer "*emjupy language server*"
              (should (string-match-p "wrong number of arguments"
                                      (buffer-string))))))))))

(ert-deftest emjupy-test-definitions-point-at-the-kernels-machine ()
  "A definition outside the notebook is named on the machine that has it.

The language server sees the kernel's filesystem and answers with paths
as it sees them.  Taken literally here they name files on this machine,
usually files that do not exist -- so jumping to a definition opened an
empty buffer at a plausible path instead of the file eldoc had just been
quoting from."
  (cl-letf (((symbol-function 'emjupy--answer-is-from-the-kernels-machine-p)
             (lambda (_) t)))
    (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"
                                       :root "/home/me/project"))
           (nb (make-emjupy-notebook :cells [] :path "n.ipynb" :server server
                                     :kernel-cwd "/home/me/project"))
           (emjupy-remote-root "/ssh:box:/home/me/project"))
      ;; a file under the server's root is renamed onto the remote
      (should (equal (emjupy--remote-name-for nb "/home/me/project/library.py")
                     "/ssh:box:/home/me/project/library.py"))
      ;; the standard library too: /usr/lib/python3.13/glob.py exists on both
      ;; machines and they are different files, so opening the local one shows
      ;; the wrong source with nothing to say so
      (should (equal (emjupy--remote-name-for nb "/usr/lib/python3.13/glob.py")
                     "/ssh:box:/usr/lib/python3.13/glob.py"))
      ;; a name already on another machine is left alone
      (should-not (emjupy--remote-name-for nb "/ssh:elsewhere:/x/y.py"))
      ;; and the xref item carries the new name, keeping line and column
      (let* ((item (xref-make "def f" (xref-make-file-location
                                       "/home/me/project/library.py" 12 4)))
             (loc (xref-item-location
                   (emjupy--xref-on-the-right-machine nb item))))
        (should (equal (xref-location-group loc)
                       "/ssh:box:/home/me/project/library.py"))
        (should (= (xref-location-line loc) 12))))))

(ert-deftest emjupy-test-definitions-left-alone-when-the-kernel-is-here ()
  "No rewriting when the kernel runs on this machine."
  (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"
                                     :root "/home/me/project"))
         (nb (make-emjupy-notebook :cells [] :path "n.ipynb" :server server
                                   :kernel-cwd "/home/me/project"))
         (emjupy-remote-root nil))
    (should-not (emjupy--remote-name-for nb "/home/me/project/library.py"))))

(ert-deftest emjupy-test-definition-elsewhere-says-so-rather-than-inventing ()
  "A definition on the kernel's machine that cannot be reached says so.

With no way to build a remote name -- a tunnelled server and nothing
configured -- the local name is a file this machine has not got.
Opening it created an empty buffer at a plausible path, which reads as
\"the definition is here and is blank\" rather than \"I cannot reach that
machine\"."
  (cl-letf (((symbol-function 'emjupy--answer-is-from-the-kernels-machine-p)
             (lambda (_) t)))
    (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"
                                       :root "/home/me/project"))
           (nb (make-emjupy-notebook :cells [] :path "n.ipynb" :server server
                                     :kernel-cwd "/home/me/project"))
           (emjupy-remote-root nil)
           (emjupy-ssh-host nil)
           (emjupy-probe-environment nil)
           (item (xref-make "def f" (xref-make-file-location
                                     "/home/me/project/library.py" 3 0))))
      (let ((err (should-error (emjupy--xref-on-the-right-machine nb item)
                               :type 'user-error)))
        (should (string-match-p "emjupy-remote-root"
                                (error-message-string err))))
      ;; a file that IS here is opened, not refused: the kernel may be local
      ;; or the directory shared
      (let* ((here (expand-file-name "emjupy-here.py"
                                     temporary-file-directory)))
        (unwind-protect
            (progn
              (write-region "x = 1\n" nil here nil 'silent)
              (let* ((server2 (make-emjupy-server
                               :base-url "localhost:9999" :token "t"
                               :root (directory-file-name temporary-file-directory)))
                     (nb2 (make-emjupy-notebook :cells [] :path "n.ipynb"
                                                :server server2))
                     (item2 (xref-make "def f" (xref-make-file-location here
                                                                        1 0))))
                (should (emjupy--xref-on-the-right-machine nb2 item2))))
          (ignore-errors (delete-file here)))))))

(ert-deftest emjupy-test-local-server-paths-are-left-local ()
  "A definition from a language server on THIS machine stays a local path.

The fallback server reads this machine's files, so its paths are this
machine's.  Sending them to the kernel's host would point at files
that may not be there."
  (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"
                                     :root "/home/me/project"))
         (nb (make-emjupy-notebook :cells [] :path "n.ipynb" :server server
                                   :kernel-cwd "/home/me/project"))
         (emjupy-remote-root "/ssh:box:/home/me/project")
         (item (xref-make "def f" (xref-make-file-location
                                   "/usr/lib/python3.13/glob.py" 5 0))))
    (cl-letf (((symbol-function 'emjupy--answer-is-from-the-kernels-machine-p)
               (lambda (_) nil)))
      (should (equal (xref-location-group
                      (xref-item-location
                       (emjupy--xref-on-the-right-machine nb item)))
                     "/usr/lib/python3.13/glob.py")))))

(ert-deftest emjupy-test-remote-names-are-not-asked-for-their-true-name ()
  "A remote file's true name is not asked of the other machine.
`file-truename' on a TRAMP name asks the remote machine, per link it
follows -- twice for each item a search returns."
  (cl-letf (((symbol-function 'file-truename)
             (lambda (f) (if (file-remote-p f)
                             (error "Asked the remote machine for %s" f) f))))
    (should (equal (emjupy--local-truename "/ssh:box:/tmp/emjupy-shadow/x.py")
                   "/ssh:box:/tmp/emjupy-shadow/x.py"))
    (should (equal (emjupy--local-truename "/tmp/here.py") "/tmp/here.py"))))

(provide 'emjupy-eglot-test)

;;; emjupy-eglot-test.el ends here
