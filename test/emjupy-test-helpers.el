;;; emjupy-test-helpers.el --- What the unit tests share  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Fixtures, macros and constants the unit tests share: a notebook in a
;; buffer, fake servers and kernels, the docs directory.

;;; Code:

(require 'dabbrev)
(require 'org)
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
           (emjupy--redraw-notebook)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var))
         (when (and (emjupy-notebook-shadow-buffer ,nb-var)
                    (buffer-live-p (emjupy-notebook-shadow-buffer ,nb-var)))
           (ignore-errors (kill-buffer
                           (emjupy-notebook-shadow-buffer ,nb-var))))))))

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

(defun emjupy-test--three-cell-notebook ()
  (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                            :source "aaa = 1"
                            :outputs [] :metadata (make-hash-table))
          (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                            :source "bbb = 2"
                            :outputs [] :metadata (make-hash-table))
          (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                            :source "ccc = 3"
                            :outputs [] :metadata (make-hash-table))))

(defun emjupy-test--python-lsp-available-p ()
  "Best-effort check for a usable Python language server on PATH."
  (or (executable-find "pylsp") (executable-find "pyright-langserver")
      (executable-find "jedi-language-server")))

(defun emjupy-test--companions-all-owned ()
  "Check that every overlay showing a rule belongs to a live cell or output.
The boxes are counted by their own overlays; the header and footer
companions that draw their rules must each have an owner that is still
there, or they are a rule with no cell."
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (dolist (o (overlays-in (point-min) (point-max)))
        (let ((owner (overlay-get o 'emjupy-rules-of)))
          (when owner
            (should (overlay-buffer owner))
            (should (memq o (overlay-get owner 'emjupy-companions)))))))))

(defconst emjupy-test--make-directory (symbol-function 'make-directory)
  "The real `make-directory', for stubs that fail only for some directories.
A stub failing every call broke whatever else made one -- TRAMP, loaded
for the first time inside the test, makes one as it loads.")

(defun emjupy-test--failing-make-directory (dirs error-symbol message
                                                 &optional on-failure)
  "Return a `make-directory' failing for directories matching DIRS, a regexp.
They fail with ERROR-SYMBOL and MESSAGE; any other directory is made.
ON-FAILURE, a function, is called at each failure -- to count them, say."
  (lambda (dir &rest args)
    (if (string-match-p dirs dir)
        (progn (when on-failure (funcall on-failure))
               (signal error-symbol (list message)))
      (apply emjupy-test--make-directory dir args))))

(defun emjupy-test--append-and-flush (cell output &optional nb)
  "Append OUTPUT to CELL and redraw at once.
Output is coalesced behind a short timer in normal use; a test wants the
buffer to match the cells before it looks at it."
  (emjupy--append-output-to-cell cell output nb)
  (when (and nb (emjupy-notebook-buffer nb))
    (emjupy-flush-output (emjupy-notebook-buffer nb))))

(defmacro emjupy-test--counting-server-starts (var &rest body)
  "Run BODY with Eglot\\'s connecting counted in VAR, and no server really started."
  (declare (indent 1))
  `(let ((,var 0))
     (cl-letf (((symbol-function 'eglot--connect) (lambda (&rest _)
                                                    (cl-incf ,var) nil))
               ((symbol-function 'eglot--guess-contact)
                (lambda (&rest _) '(python-mode "pylsp")))
               ((symbol-function 'emjupy--warn-local-language-server) #'ignore))
       ,@body)
     ,var))

(defun emjupy-test--stream (text)
  "Return a stream output carrying TEXT."
  (let ((o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o)
    (puthash "name" "stdout" o)
    (puthash "text" text o)
    o))

(defun emjupy-test--cell-like (type source &optional outputs exec metadata)
  "Return a cell of TYPE holding SOURCE, OUTPUTS, EXEC and METADATA."
  (make-emjupy-cell :id (emjupy--new-cell-id) :type type :source source
                    :outputs (or outputs [])
                    :exec-count exec
                    :metadata (or metadata (make-hash-table :test 'equal))))

(defun emjupy-test--stream-output (text)
  "Return a stream output carrying TEXT."
  (let ((o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o)
    (puthash "name" "stdout" o)
    (puthash "text" text o)
    o))

(defun emjupy-test--assert-invariants (label)
  "Fail with LABEL if this buffer and its cells disagree."
  (let ((problems (emjupy--check-invariants)))
    (should (equal (cons label problems) (cons label nil)))))

(defun emjupy-test--undo-case (operation)
  "Type into the first cell, run OPERATION, and return whether undo works.

Returns a plist: :undone whether the typing was taken back, :intact
whether the first cell survived, :problems any invariant violations."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'code "b = 2\nc = 3")
                       (emjupy-test--cell-like 'code "d = 4")))
        (result nil))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        (funcall operation nb buf)
        (goto-char (point-min))
        (let ((worked (condition-case nil
                          (progn
                            (undo)
                            ;; Deleting and inserting a cell are undo steps of
                            ;; their own, each taking a step back before the
                            ;; typing; anything else is undone with it.
                            (let ((steps 0))
                              (while (and (string-match-p "\\+ 7"
                                                          (buffer-string))
                                          (< steps 4))
                                (undo-more 1)
                                (setq steps (1+ steps))))
                            t)
                        (error nil))))
          ;; Undo changes the buffer; the cells catch up at the next sync,
          ;; which every path that reads them -- saving included -- performs
          ;; first.  Syncing here is what a user's next action would do.
          (emjupy--sync-all-cells)
          (setq result
                (list :worked worked
                      :undone (not (string-match-p "\\+ 7" (buffer-string)))
                      :intact (and (string-match-p "a = 1" (buffer-string)) t)
                      :saved-undone (not (string-match-p
                                          "\\+ 7"
                                          (mapconcat
                                           (lambda (c)
                                             (or (emjupy-cell-source c) ""))
                                           (append (emjupy-notebook-cells nb) nil)
                                           "\n")))
                      :problems (emjupy--check-invariants))))))
    result))

(defun emjupy-test--big-notebook (cells-count output-lines)
  "Return CELLS-COUNT code cells, each with OUTPUT-LINES of output."
  (vconcat
   (cl-loop for i from 0 below cells-count
            collect (let ((cell (emjupy-test--cell-like
                                 'code
                                 (format "def f%d(a):\n    return a + %d"
                                         i i))))
                      (when (> output-lines 0)
                        (setf (emjupy-cell-outputs cell)
                              (vector (emjupy-test--stream-output
                                       (mapconcat (lambda (n)
                                                    (format "line %d" n))
                                                  (number-sequence 1
                                                                   output-lines)
                                                  "\n")))))
                      cell))))

(defmacro emjupy-test--elapsed (&rest body)
  "Return the seconds BODY took, garbage collection left out.

What is measured is emjupy's own work.  A collection triggered by the
garbage earlier tests left -- the randomised ones make a great deal --
would otherwise land inside the timing, and a test that passed alone
failed in the full suite one run in two."
  (declare (indent 0))
  `(progn
     (garbage-collect)
     (let ((gc-cons-threshold most-positive-fixnum)
           (start (float-time)))
       ,@body
       (- (float-time) start))))

(defconst emjupy-test--menu-exempt
  '(emjupy-menu emjupy-indent-or-cycle emjupy-latex-unrender-or-delete)
  "Commands that belong on a key but not in the menu.

The menu itself; TAB, which indents and is not a thing one goes looking
for; and DEL, which reveals a rendered formula and only makes sense with
point already on one.")

(defun emjupy-test--menu-commands ()
  "Return the commands offered by `emjupy-menu'."
  (let (found)
    (letrec ((walk (lambda (node)
                     (cond
                      ((vectorp node) (mapc walk (append node nil)))
                      ((consp node)
                       (let ((spec (plist-get node :command)))
                         (when (and spec (symbolp spec)) (push spec found)))
                       (mapc (lambda (x) (when (or (consp x) (vectorp x))
                                           (funcall walk x)))
                             node))))))
      (funcall walk (get 'emjupy-menu 'transient--layout)))
    found))

(defconst emjupy-test--undo-exempt
  '(;; need a kernel or a server: covered by the integration tests
    emjupy-execute-cell-at-point emjupy-execute-cell-and-goto-next
    emjupy-interrupt-kernel emjupy-restart-kernel emjupy-reconnect-kernel
    emjupy-connect-kernel-interactive emjupy-save-notebook emjupy-login
    emjupy-switch-notebook emjupy-export-py emjupy-status
    emjupy-show-traceback emjupy-lsp-diagnose emjupy-version
    ;; open a different buffer, or only move point
    emjupy-edit-cell-externally emjupy-menu
    emjupy-next-cell emjupy-previous-cell
    emjupy-beginning-of-cell emjupy-end-of-cell
    emjupy-beginning-of-next-cell emjupy-end-of-next-cell
    emjupy-beginning-of-previous-cell emjupy-end-of-previous-cell
    ;; edit at point by design, so "did the typing survive" is not the
    ;; question to ask of them
    emjupy-indent-or-cycle emjupy-latex-unrender-or-delete
    emjupy-copy-cell)
  "Commands the general undo guard does not run.

Every exemption is a claim that the command is covered elsewhere or
cannot be asked this question -- not that it may break undo.  Adding a
command here should feel like work.")

(defun emjupy-test--notebook-commands ()
  "Return the commands bound in `emjupy-mode-map', prefixes included."
  (let (found)
    (letrec ((collect (lambda (km)
                        (map-keymap
                         (lambda (_event def)
                           (cond
                            ((keymapp def) (funcall collect def))
                            ((and (symbolp def) (commandp def))
                             (cl-pushnew def found))))
                         km))))
      (funcall collect emjupy-mode-map))
    found))

(defconst emjupy-test--undo-guard-exempt
  '((emjupy-menu . "opens a menu; changes nothing")
    (emjupy-login . "asks for a server and talks to it")
    (emjupy-switch-notebook . "leaves this notebook")
    (emjupy-save-notebook . "talks to the server")
    (emjupy-export-py . "writes a file, asks where")
    (emjupy-status . "reports; changes nothing")
    (emjupy-execute-cell-at-point . "KNOWN BUG: needs a kernel, and loses the history")
    (emjupy-execute-cell-and-goto-next . "KNOWN BUG: needs a kernel, and loses the history")
    (emjupy-interrupt-kernel . "needs a kernel")
    (emjupy-restart-kernel . "needs a kernel")
    (emjupy-reconnect-kernel . "needs a kernel")
    (emjupy-connect-kernel-interactive . "needs a server")
    (emjupy-show-traceback . "opens another buffer")
    (emjupy-edit-cell-externally . "opens another buffer")
    (emjupy-latex-unrender-or-delete . "deletes a character when there is no formula"))
  "Commands the undo guard does not run, each with the reason.

Being on this list is a decision; being absent from it is not, which is
the point.  A command added to the keymap is tested from the day it is
bound unless someone writes down why it should not be.")

(defun emjupy-test--commands-on-keys ()
  "Return every command reachable by a key in `emjupy-mode-map\\='."
  (let (cmds)
    (letrec ((walk (lambda (km)
                     (map-keymap
                      (lambda (_event def)
                        (cond ((keymapp def) (funcall walk def))
                              ((and (symbolp def) (commandp def))
                               (cl-pushnew def cmds))))
                      km))))
      (funcall walk emjupy-mode-map))
    cmds))

(defun emjupy-test--undo-past-steps (regexp)
  "Undo, and go on undoing while REGEXP still matches the buffer.
Every cell command is an undo step of its own, so an edit made before
one is reached after that step -- and one more for each such command.
Bounded, so a history that never gets there fails rather than loops."
  (let ((last-command nil)) (undo))
  (let ((steps 0))
    (while (and (string-match-p regexp (buffer-string)) (< steps 5))
      (undo-more 1)
      (setq steps (1+ steps)))))

(defun emjupy-test--undo-survives-command (command)
  "Type into a cell, run COMMAND, and return whether the typing can be undone.

Returns t, or a string saying what went wrong."
  (let ((cells (vector (emjupy-test--cell-like 'code "first = 1")
                       (emjupy-test--cell-like 'code "second = 2\nthird = 3")
                       (emjupy-test--cell-like
                        'code "fourth = 4"
                        (vector (emjupy-test--stream-output "out\n")))))
        (result nil))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        ;; type into the first cell
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        ;; act somewhere else, with point in the middle cell
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 1))))
        ;; Prompts answered, so a command that asks does not hang the suite;
        ;; anything that still refuses is the command declining to act, which
        ;; is not a failure of undo.
        (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "x"))
                  ((symbol-function 'read-from-minibuffer) (lambda (&rest _)
                                                             "x"))
                  ((symbol-function 'completing-read) (lambda (&rest _) "x"))
                  ;; Answered yes, not no: a command that asks and is then
                  ;; declined does nothing, and a guard it cannot reach is
                  ;; no guard at all.
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'read-file-name) (lambda (&rest _)
                                                       "/tmp/emjupy-guard"))
                  ((symbol-function 'message) #'ignore))
          (ignore-errors (call-interactively command)))
        (setq result
              (cond
               ((not (string-match-p "\\+ 7" (buffer-string)))
                ;; the command removed the typing itself; nothing to undo
                t)
               (t
                (goto-char (point-min))
                (let ((undone (condition-case nil
                                  (progn (emjupy-test--undo-past-steps "\\+ 7") t)
                                (error nil))))
                  (cond
                   ((not undone) "undo refused")
                   ((string-match-p "\\+ 7"
                                    (buffer-string)) "typing not taken back")
                   ;; Sync first: undo changes the buffer, and the cells catch
                   ;; up at the next sync, which every path reading them
                   ;; performs.
                   ((progn (emjupy--sync-all-cells) (emjupy--check-invariants))
                    "buffer and cells disagree afterwards")
                   (t t))))))))
    result))

(defun emjupy-test--presses-to-recover (command)
  "Run COMMAND over the cell holding a fresh edit; return presses to get it back.

Returns nil if the edit never comes back.  The edit is made in the cell
ABOVE the one acted on, because that is the cell a move-up or a merge
rewrites -- the arrangement where an entry describing the edit sits
inside the region being replaced."
  (let ((cells (vector (emjupy-test--cell-like 'code "first = 1")
                       (emjupy-test--cell-like 'code "second = 2")
                       (emjupy-test--cell-like 'code "third = 3")))
        (recovered nil))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 1))))
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                  ((symbol-function 'message) #'ignore))
          (ignore-errors (call-interactively command)))
        (goto-char (point-min))
        (setq this-command nil last-command nil)
        (let ((presses 0))
          (while (and (< presses 6) (not recovered))
            (setq presses (1+ presses))
            (ignore-errors (undo))
            (setq this-command 'undo last-command 'undo)
            (unless (string-match-p "\\+ 7" (buffer-string))
              (setq recovered presses))))
        (setq this-command nil last-command nil)))
    recovered))

(defun emjupy-test--stream-on (name text)
  "Return a stream output on NAME carrying TEXT."
  (let ((o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o)
    (puthash "name" name o)
    (puthash "text" text o)
    o))

(defun emjupy-test--kernel-msg (msg-type parent-id content)
  "Return a kernel message of MSG-TYPE replying to PARENT-ID with CONTENT."
  (let ((hdr (make-hash-table :test 'equal))
        (ph (make-hash-table :test 'equal))
        (c (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (puthash "msg_type" msg-type hdr)
    (puthash "msg_id" parent-id ph)
    (pcase-dolist (`(,k . ,v) content) (puthash k v c))
    (puthash "header" hdr msg)
    (puthash "parent_header" ph msg)
    (puthash "content" c msg)
    (json-serialize msg)))

(defun emjupy-test--feed-in-order (order)
  "Run one request whose messages arrive in ORDER; return the cell and kernel.
ORDER is a list of `result', `reply' and `idle'."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "40 + 2" :outputs []
                                 :metadata (make-hash-table)))
         (id "req-1")
         (nb (make-emjupy-notebook :cells (vector cell)))
         (kernel (make-emjupy-kernel :id "k"
                                     :pending (make-hash-table :test 'equal)
                                     :notebook nb))
         (data (make-hash-table :test 'equal)))
    (clrhash emjupy--request-halves)
    (puthash "text/plain" "42" data)
    (puthash id cell (emjupy-kernel-pending kernel))
    (dolist (step order)
      (emjupy--handle-ws-message
       kernel
       (pcase step
         ('result (emjupy-test--kernel-msg
                   "execute_result" id
                   `(("data" . ,data) ("execution_count" . 1))))
         ('reply (emjupy-test--kernel-msg "execute_reply" id
                                          '(("execution_count" . 1))))
         ('idle (emjupy-test--kernel-msg "status" id '(("execution_state" .
                                                        "idle")))))))
    (list cell kernel id)))

(defun emjupy-test--drawn-widths (cell)
  "Return the width in columns of each line of CELL's drawn output.
The padding is left out: it is a stretch to the border, not text."
  (let ((ov (emjupy-cell-output-ov cell)) widths)
    (save-excursion
      (goto-char (overlay-start ov))
      (while (< (point) (overlay-end ov))
        (let ((w 0) (p (line-beginning-position)))
          (while (< p (line-end-position))
            (unless (get-text-property p 'emjupy-pad)
              (setq w (+ w (char-width (char-after p)))))
            (setq p (1+ p)))
          (push w widths))
        (forward-line 1)))
    (nreverse widths)))

(defun emjupy-test--png-output ()
  "Return a display_data output holding a 1x1 PNG."
  (let ((o (make-hash-table :test 'equal)) (d (make-hash-table :test 'equal)))
    (puthash "output_type" "display_data" o)
    (puthash "image/png" "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==" d)
    (puthash "text/plain" "<Figure size 1x1>" d)
    (puthash "data" d o)
    o))

(defun emjupy-test--unhex (hex)
  "Return the unibyte string HEX spells."
  (apply #'unibyte-string
         (cl-loop for i from 0 below (length hex) by 2
                  collect (string-to-number (substring hex i (+ i 2)) 16))))

(defun emjupy-test--models (models)
  "Return a table of widget models from MODELS, (ID . STATE-ALIST) pairs."
  (let ((table (make-hash-table :test 'equal)))
    (dolist (m models table)
      (let ((h (make-hash-table :test 'equal)))
        (dolist (kv (cdr m)) (puthash (car kv) (cdr kv) h))
        (puthash (car m) (cons 'kernel h) table)))))

(defmacro emjupy-test--with-saving (spec &rest body)
  "Run BODY with a notebook, a fake server and a temporary recovery directory.
SPEC is (BUF NB REQUESTS SERVER-MODIFIED): BUF and NB are bound to the
notebook, REQUESTS to the list of (METHOD PATH) made, newest first, and
SERVER-MODIFIED to the cons whose car is the server copy's last_modified.
The server answers a GET with it, and a PUT by setting a new one."
  (declare (indent 1))
  (let ((buf (nth 0 spec)) (nb (nth 1 spec)) (requests (nth 2 spec))
        (lm (nth 3 spec)))
    `(let* ((emjupy-recovery-directory (make-temp-file "emjupy-recovery" t))
            (,requests nil)
            (,lm (list "2026-01-01T00:00:00Z"))
            (cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                             :type 'code :source "x = 1"
                                             :outputs []
                                             :metadata (make-hash-table)))))
       (unwind-protect
           (cl-letf (((symbol-function 'emjupy--http-request)
                      (lambda (method _server path &rest _)
                        (push (list method path) ,requests)
                        (let ((model (make-hash-table :test 'equal)))
                          (when (equal method "PUT")
                            (setcar ,lm (format "2026-01-01T00:00:%02dZ"
                                                (length ,requests))))
                          (puthash "last_modified" (car ,lm) model)
                          model)))
                     ;; requests not waited for: answered at once, by the
                     ;; same fake server, through whatever stands for it
                     ((symbol-function 'emjupy--http-request-async)
                      (lambda (method server path body on-done on-fail)
                        (let ((answer nil) (failure nil))
                          (condition-case err
                              (setq answer (emjupy--http-request method server
                                                                 path body))
                            (file-error (setq failure err)))
                          (if failure (funcall on-fail failure)
                            (funcall on-done answer))))))
             (emjupy-test--with-notebook cells ,buf ,nb
               (with-current-buffer ,buf
                 (setf (emjupy-notebook-server ,nb)
                       (make-emjupy-server :base-url "localhost:9" :token "t")
                       (emjupy-notebook-path ,nb) "work.ipynb"
                       (emjupy-notebook-buffer ,nb) ,buf
                       (emjupy-notebook-last-modified ,nb) (car ,lm))
                 ,@body)))
         (delete-directory emjupy-recovery-directory t)))))

(defun emjupy-test--type-in-first-cell (text)
  "Type TEXT at the end of the first cell, as a user would."
  (goto-char (1- (overlay-end
                  (emjupy-cell-overlay
                   (aref (emjupy-notebook-cells emjupy--buffer-notebook) 0)))))
  (let ((inhibit-read-only t)) (insert text)))

(defun emjupy-test--fail-once (fn &optional insert)
  "Make FN fail the next time it is called, and only then.
With INSERT, it first inserts that text, so the failure leaves a redraw
half done, as a real one would.  Returns a function that removes this."
  (let* ((armed t)
         (advice (lambda (orig &rest args)
                   (if (not armed)
                       (apply orig args)
                     (setq armed nil)
                     (when insert (insert insert))
                     (error "Injected failure in %s" fn)))))
    (advice-add fn :around advice)
    (lambda () (advice-remove fn advice))))

(defun emjupy-test--two-cells ()
  "Return two code cells, the first with a stream output."
  (let ((out (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" out)
    (puthash "name" "stdout" out)
    (puthash "text" "hello\n" out)
    (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "print('hello')"
                              :outputs (vector out) :metadata (make-hash-table))
            (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "y = 2"
                              :outputs [] :metadata (make-hash-table)))))

(defun emjupy-test--stream-named (name text)
  "Return a stream output NAME holding TEXT."
  (let ((o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o) (puthash "name" name o)
    (puthash "text" text o) o))

(defun emjupy-test--drawn ()
  "Return this buffer's text with the properties that make up its drawing."
  (let ((text (buffer-string)))
    (set-text-properties 0 (length text) nil text)
    (list text
          (cl-loop for pos from (point-min) below (point-max)
                   collect (list (get-text-property pos 'face)
                                 (get-text-property pos 'read-only))))))

(defun emjupy-test--plotly-fetch (js limit)
  "Fetch JS as plotly.js from a fake kernel whose output is cut past LIMIT.
Return (ADDRESS-OR-NIL . PIECES-ASKED)."
  (let* ((dir (make-temp-file "emjupy-plotly" t))
         (file (expand-file-name "plotly-test.min.js" dir))
         (asked nil) (address nil))
    (cl-letf (((symbol-function 'emjupy--kernel-eval)
               (lambda (_kernel code callback)
                 (if (string-match "_emjupy_js\\[\\([0-9]+\\):\\([0-9]+\\)\\]" code)
                     (let ((from (string-to-number (match-string 1 code)))
                           (to (string-to-number (match-string 2 code))))
                       (push (- to from) asked)
                       (funcall callback
                                (if (> (- to from) limit)
                                    "IOPub data rate exceeded."
                                  (substring js from (min to (length js))))))
                   (funcall callback ""))))
              ((symbol-function 'run-at-time)
               (lambda (_time _repeat fn &rest args) (apply fn args)))
              ((symbol-function 'message) #'ignore))
      (emjupy--plotly-js-fetch
       (emjupy--plotly-fetch-make :kernel 'kernel :file file :size (length js)
                                  :callback (lambda (url) (setq address url))
                                  :chunk 100000))
      (prog1 (cons (and address
                        (with-temp-buffer (insert-file-contents file)
                                          (buffer-string)))
                   (nreverse asked))
        (delete-directory dir t)))))

(defconst emjupy-test--terminal-cases
  '(
  ;; two bars, positions
  ("two bars, positions"
   ("\rA:   0%|                              | 0/4 [00:00<?, ?it/s]" "\n"
    "\rB:   0%|                              | 0/4 [00:00<?, ?it/s]" "\e[A"
    "\rA:  25%|####7              | 1/4 [00:00<00:00, 17924.38it/s]" "\n"
    "\rB:  25%|####7              | 1/4 [00:00<00:00, 23431.87it/s]" "\e[A"
    "\rA:  50%|#########5         | 2/4 [00:00<00:00, 14181.22it/s]" "\n"
    "\rB:  50%|#########5         | 2/4 [00:00<00:00, 15834.59it/s]" "\e[A"
    "\rA:  75%|##############2    | 3/4 [00:00<00:00, 13489.08it/s]" "\n"
    "\rB:  75%|##############2    | 3/4 [00:00<00:00, 14347.68it/s]" "\e[A"
    "\rA: 100%|###################| 4/4 [00:00<00:00, 13522.40it/s]" "\n"
    "\rB: 100%|###################| 4/4 [00:00<00:00, 14087.67it/s]" "\e[A" ""
    "\rA: 100%|###################| 4/4 [00:00<00:00, 10686.12it/s]" "\n" ""
    "\rB: 100%|###################| 4/4 [00:00<00:00, 10965.50it/s]" "\n")
   ("A: 100%|###################| 4/4 [00:00<00:00, 10686.12it/s]"
    "B: 100%|###################| 4/4 [00:00<00:00, 10965.50it/s]"))
  ;; colours and a clear
  ("colours and a clear"
   ("\e[31mred\e[0m line\n" "progress 1" "\rprogress 2" "\e[K\rdone\n"
    "\e[1mbold\e[0m end\n")
   ("red line" "doneress 2" "bold end")))
  "Stream writes recorded from tqdm, and what a terminal shows of them.
Each is (NAME CHUNKS LINES): the writes, as tqdm made them, and the
screen pyte -- a terminal emulator -- showed, trailing blanks dropped.")

(defconst emjupy-test--docs-directory
  (expand-file-name "../docs/" (file-name-directory
                                (or load-file-name buffer-file-name)))
  "The docs beside the tests, whose names the tests check.")


(defun emjupy-test--timed-cell (start end)
  "A code cell run once, its metadata with START and END as JupyterLab's."
  (let ((times (make-hash-table :test 'equal))
        (meta (make-hash-table :test 'equal)))
    (when start (puthash "iopub.execute_input" start times))
    (when end (puthash "shell.execute_reply" end times))
    (puthash "execution" times meta)
    (make-emjupy-cell :id 1 :type 'code :exec-count 7 :metadata meta)))

(provide 'emjupy-test-helpers)

;;; emjupy-test-helpers.el ends here
