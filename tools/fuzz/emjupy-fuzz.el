;;; emjupy-fuzz.el --- A user's random session, checked at every step  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Opens notebooks as a user does -- through TRAMP, each in its own
;; environment -- and works in them at random: moving, typing, adding,
;; moving and deleting cells, running them, undoing, saving.  After every
;; step each notebook's buffer is checked; it ends by restarting each
;; kernel, running every cell, saving, and comparing the saved file with
;; the buffer.  A seed fixes the session: a failure replays.
;;
;;   EMJUPY_FUZZ_NOTEBOOKS  "FILE=ENV FILE=ENV ..."; FILE a notebook, TRAMP
;;                          names welcome, ENV its environment there
;;   EMJUPY_FUZZ_SEED       the session's seed (1)
;;   EMJUPY_FUZZ_STEPS      how many steps (200)
;;   EMJUPY_FUZZ_EGLOT      0 for no language support (on)
;;   EMJUPY_FUZZ_LOG        where the log goes (/tmp/emjupy-fuzz.log)
;;   EMJUPY_FUZZ_KEEP_GOING non-empty: go on after a failure
;;   EMJUPY_FUZZ_LIBRARY    non-empty: load the functions only -- see
;;                          emjupy-fuzz-daemon.el
;;
;;   emacs -batch -Q -L src -L WEBSOCKET -l tools/fuzz/emjupy-fuzz.el
;;
;; Run it as a user who cannot read the remote files: run as root, a
;; remote path taken for a local one goes unnoticed.  The exit status is
;; 1 if anything failed.  tools/fuzz/README.org sets up the testbed.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'emjupy)

;;;; Settings and the log

(defun fz-env (name default)
  "Return the environment variable NAME, or DEFAULT if unset or empty."
  (let ((v (getenv name))) (if (and v (not (string-empty-p v))) v default)))

(defvar fz-seed (string-to-number (fz-env "EMJUPY_FUZZ_SEED" "1")))
(defvar fz-steps (string-to-number (fz-env "EMJUPY_FUZZ_STEPS" "200")))
(defvar fz-log-file (fz-env "EMJUPY_FUZZ_LOG" "/tmp/emjupy-fuzz.log"))
(defvar fz-keep-going (fz-env "EMJUPY_FUZZ_KEEP_GOING" nil))
(defvar fz-start (float-time))
(defvar fz-failures nil "What went wrong: the session stops at the first.")
(defvar fz-problems nil "What a user would see go wrong, but not emjupy's.")
(defvar fz-history nil "The steps so far, latest first.")

(defun fz-log (format &rest args)
  "Append FORMAT with ARGS to the log, timed from the start."
  (let ((line (format "%6.1f %s\n" (- (float-time) fz-start)
                      (apply #'format format args))))
    (write-region line nil fz-log-file t 'silent)
    (princ line #'external-debugging-output)))

(defun fz-fail (what &rest args)
  "Record a failure: WHAT, formatted with ARGS, and the steps before it."
  (let ((text (apply #'format what args)))
    (push text fz-failures)
    (fz-log "FAILURE %s" text)
    (fz-log "  the last steps: %s"
            (mapconcat #'identity (reverse (seq-take fz-history 8)) " | "))))

(defun fz-problem (what &rest args)
  "Record a problem: WHAT, formatted with ARGS."
  (let ((text (apply #'format what args)))
    (push text fz-problems)
    (fz-log "PROBLEM %s" text)))

;;;; Randomness of its own: emjupy's use of `random' cannot shift it

(defvar fz-state (logand (* (1+ fz-seed) 2654435761) #x7fffffff))

(defun fz-random (n)
  "Return a number below N, the next of the session's sequence."
  (setq fz-state (logand (+ (* fz-state 1103515245) 12345) #x7fffffff))
  (mod (ash fz-state -8) n))

(defun fz-pick (list)
  "Return an element of LIST."
  (nth (fz-random (length list)) list))

(defun fz-weighted (choices)
  "Return the action of CHOICES, (WEIGHT . ACTION) pairs, by weight."
  (let ((n (fz-random (apply #'+ (mapcar #'car choices)))))
    (cl-loop for (w . action) in choices
             if (< n w) return action
             do (setq n (- n w)))))

;;;; Waiting, and what Emacs did meanwhile

(defun fz-pump (seconds &optional until)
  "Run the event loop SECONDS, or until UNTIL returns non-nil."
  (let ((deadline (+ (float-time) seconds)))
    (while (and (< (float-time) deadline) (not (and until (funcall until))))
      (accept-process-output nil 0.05))
    (and until (funcall until))))

(defvar fz-tick nil)
(defvar fz-blocked 0.0 "The longest Emacs went without running a timer.")
(defvar fz-blocked-at nil "The step it was in.")
(defvar fz-step-name "opening")

(defun fz-watch-blocking ()
  "Measure how long Emacs is kept from running timers."
  (let ((last (float-time)))
    (setq fz-tick
          (run-with-timer 0.1 0.1
                          (lambda ()
                            (let ((late (- (float-time) last 0.1)))
                              (when (> late fz-blocked)
                                (setq fz-blocked late
                                      fz-blocked-at fz-step-name)))
                            (setq last (float-time)))))))

(defvar fz-messages-seen 0)

(defun fz-new-async-errors ()
  "Return errors from timers and processes logged since the last call.
Shown to a user only in *Messages*, if at all."
  (with-current-buffer (messages-buffer)
    (let ((found nil))
      (save-excursion
        (goto-char (min fz-messages-seen (point-max)))
        (while (re-search-forward
                "^\\(Error running timer.*\\|error in process \\(filter\\|sentinel\\).*\\)$"
                nil t)
          (push (match-string 1) found)))
      (setq fz-messages-seen (point-max))
      (nreverse found))))

;;;; The notebooks

(defvar fz-notebooks nil "Each notebook's (FILE ENV BUFFER).")

(defun fz-open (file env)
  "Open FILE, a notebook, in ENV, as C-c C-o does; return its buffer."
  (cl-letf (((symbol-function 'completing-read) (lambda (&rest _) env)))
    (with-current-buffer (find-file-noselect file)
      (emjupy-open-this-notebook))))

(defun fz-show (buffer)
  "Show BUFFER in the window, as a user would, working in it."
  (set-window-buffer (selected-window) buffer)
  (set-buffer buffer))

(defun fz-cells (&optional buffer)
  "Return BUFFER's cells, a list."
  (append (emjupy-notebook-cells
           (buffer-local-value 'emjupy--buffer-notebook
                               (or buffer (current-buffer))))
          nil))

(defun fz-live-p (buffer)
  "Return non-nil if BUFFER's kernel connection is open."
  (with-current-buffer buffer (ignore-errors (emjupy--ws-live-p))))

;;;; The actions: what a user does, by the commands a user runs

(defun fz-run (command)
  "Run COMMAND as a key would."
  (setq last-command this-command
        this-command command)
  (call-interactively command))

(defun fz-command-loop (action)
  "Do ACTION as the command loop would: its hooks around it, then what
runs before the screen is redrawn -- `cursor-intangible-mode' moving
point off a separator, say.  A batch Emacs never redraws: without this,
point left on a separator by `undo' was typed at, refused."
  (run-hooks 'pre-command-hook)
  (unwind-protect
      (if (commandp action) (fz-run action) (funcall action))
    (run-hooks 'post-command-hook)
    ;; as redisplay does: for each window, its own buffer current -- a
    ;; window's point with another buffer's text was out of its range
    (dolist (w (window-list))
      (with-current-buffer (window-buffer w)
        (run-hook-with-args 'pre-redisplay-functions w)))))

(defun fz-into-a-cell ()
  "Put point in a cell, a random one, at a random place in it."
  (let* ((cells (fz-cells)) (cell (fz-pick cells)))
    (when cell
      (emjupy--goto-cell cell 'beginning)
      (forward-char (fz-random (1+ (length (emjupy-cell-source cell)))))
      (unless (fz-in-source-p)
        (emjupy--goto-cell cell 'beginning)))))

(defun fz-in-source-p ()
  "Return non-nil if point is in a cell's source, where text can be typed.
Not `emjupy--cell-at-point' alone: it answers which cell a command acts
on, the nearest one from an output or a separator."
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell))))
    (and ov (>= (point) (overlay-start ov)) (< (point) (overlay-end ov)))))

(defun fz-in-a-cell ()
  "Leave point in a cell's source: a user types where text can go."
  (unless (fz-in-source-p) (fz-into-a-cell)))

(defun fz-type ()
  "Type a line: valid code in a code cell, words in a markdown one."
  (fz-in-a-cell)
  (let ((cell (emjupy--cell-at-point)) (n (fz-random 1000)))
    (when cell
      (end-of-line)
      (insert (if (eq (emjupy-cell-type cell) 'code)
                  (fz-pick (list (format "\nv%d = %d" n n)
                                 (format "\n# typed %d" n)
                                 (format "\nprint(%d)" n)))
                (format "\nWords %d." n))))))

(defun fz-erase ()
  "Delete a few characters back, where text was typed."
  (fz-in-a-cell)
  (when (looking-back "# typed [0-9]+" (line-beginning-position))
    (delete-char -1)))

(defun fz-split ()
  "Split the cell at the start of one of its lines."
  (fz-in-a-cell)
  (beginning-of-line)
  (fz-run 'emjupy-split-cell))

(defconst fz-actions
  `((12 . emjupy-next-line) (10 . emjupy-previous-line)
    (5 . emjupy-next-cell) (4 . emjupy-previous-cell)
    (3 . emjupy-beginning-of-next-cell) (3 . emjupy-end-of-previous-cell)
    (6 . fz-into-a-cell) (14 . fz-type) (3 . fz-erase)
    (4 . emjupy-insert-cell-below) (3 . emjupy-insert-cell-above)
    (2 . emjupy-delete-cell) (3 . emjupy-move-cell-up)
    (3 . emjupy-move-cell-down) (2 . fz-split) (2 . emjupy-join-cell-above)
    (2 . emjupy-copy-cell) (2 . emjupy-yank-cell)
    (2 . emjupy-cycle-cell-type) (3 . emjupy-toggle-cell-output)
    (2 . emjupy-toggle-cell-code) (2 . emjupy-clear-cell-output)
    (1 . emjupy-clear-all-outputs) (6 . undo)
    (10 . emjupy-execute-cell-at-point)
    (6 . emjupy-execute-cell-and-goto-next)
    (1 . emjupy-interrupt-kernel) (1 . emjupy-restart-kernel)
    (2 . emjupy-save-notebook) (4 . fz-other-notebook)
    (2 . fz-complete))
  "What a user does, by weight.")

(defun fz-other-notebook ()
  "Work in another notebook."
  (fz-show (nth 2 (fz-pick fz-notebooks))))

(defun fz-complete ()
  "Ask for completion, as typing a name does."
  (fz-in-a-cell)
  (with-timeout (5 (fz-problem "completion took over 5 s"))
    (completion-at-point)))

(defun fz-act (action)
  "Do ACTION, a command or one of the driver's; return how it went."
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
            ((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
            ((symbol-function 'read-string)
             (lambda (_p &optional initial _h default &rest _)
               (or initial default ""))))
    (condition-case err
        (progn
          (fz-command-loop action)
          "done")
      (user-error (format "refused: %s" (error-message-string err)))
      ((beginning-of-buffer end-of-buffer) "at the end")
      (error (fz-fail "%s raised %S" action err)
             (fz-log "  where: %s" (fz-where))
             "failed"))))

(defun fz-where ()
  "Describe point: its cell, the text around it, and its properties."
  (let ((cell (emjupy--cell-at-point)))
    (format "point %d in %s, %S|%S, properties %S, line %S"
            (point)
            (if cell (format "cell %S (%s)" (emjupy-cell-id cell)
                             (emjupy-cell-type cell))
              "no cell")
            (buffer-substring-no-properties (max (point-min) (- (point) 12))
                                            (point))
            (buffer-substring-no-properties (point)
                                            (min (point-max) (+ (point) 12)))
            (let ((props (text-properties-at (point))))
              (cl-loop for (k v) on props by #'cddr
                       when (memq k '(read-only invisible display emjupy-cell
                                      cursor-intangible front-sticky
                                      rear-nonsticky))
                       append (list k (if (stringp v) "<string>" v))))
            (buffer-substring-no-properties (line-beginning-position)
                                            (line-end-position)))))

;;;; The checks, after every step

(defun fz-check (buffer)
  "Check BUFFER: its cells, protection and runs; return non-nil if sound."
  (with-current-buffer buffer
    (let ((ok t) (label (buffer-name)))
      (emjupy--sync-all-cells)
      (unless (emjupy--overlays-sane-p)
        (fz-fail "%s: the cells' overlays no longer describe the cells" label)
        (setq ok nil))
      (when (and ok (bound-and-true-p emjupy-protect-non-cell-regions))
        ;; every character outside the cells is protected
        (let ((pos (point-min)) (cells (fz-cells)))
          (dolist (cell cells)
            (let ((start (overlay-start (emjupy-cell-overlay cell))))
              (when (and ok (< pos start)
                         (text-property-not-all pos start 'read-only 'emjupy))
                (fz-fail "%s: text before cell %S is not protected" label
                         (emjupy-cell-id cell))
                (setq ok nil))
              (setq pos (overlay-end (emjupy-cell-overlay cell)))))))
      (let ((ids (mapcar #'emjupy-cell-id (fz-cells))))
        (dolist (id (bound-and-true-p emjupy--running-cells))
          (unless (memq id ids)
            (fz-fail "%s: cell %S is marked running, and is gone" label id)
            (setq ok nil))))
      ok)))

;;;; The end: every cell run from a fresh kernel, then saved and compared

(defun fz-run-everything (buffer)
  "Restart BUFFER's kernel, run every cell, report what did not run."
  (fz-show buffer)
  (with-current-buffer buffer
    (let ((label (buffer-name)))
      (fz-pump 60 (lambda () (null emjupy--running-cells)))
      (fz-act 'emjupy-restart-kernel)
      (unless (fz-pump 60 (lambda () (fz-live-p buffer)))
        (fz-fail "%s: the kernel did not come back after a restart" label))
      (fz-act 'emjupy-run-all-cells)
      (fz-pump 5)
      (unless (fz-pump 180 (lambda () (null emjupy--running-cells)))
        (fz-fail "%s: cells still running 180 s after running them all" label))
      (let ((code (seq-filter (lambda (c) (eq (emjupy-cell-type c) 'code))
                              (fz-cells))))
        (dolist (cell code)
          (let ((error (seq-find
                        (lambda (o) (equal (gethash "output_type" o) "error"))
                        (append (or (emjupy-cell-outputs cell) []) nil))))
            (when error
              (fz-problem "%s: a cell raised %s: %S" label
                          (gethash "ename" error)
                          (truncate-string-to-width
                           (emjupy-cell-source cell) 40)))))
        (fz-log "%s: final run: %d of %d code cells run"
                label
                (seq-count #'emjupy-cell-exec-count code) (length code))))))

(defun fz-saved-cells (file)
  "Return FILE's cells as saved: (TYPE . SOURCE) pairs."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((nb (json-parse-buffer)))
      (mapcar (lambda (c)
                (let ((s (gethash "source" c)))
                  (cons (intern (gethash "cell_type" c))
                        (if (stringp s) s (mapconcat #'identity s "")))))
              (append (gethash "cells" nb) nil)))))

(defun fz-save-and-compare (file buffer)
  "Save BUFFER, then compare FILE as saved with the buffer's cells."
  (with-current-buffer buffer
    (emjupy--sync-all-cells)
    (fz-act 'emjupy-save-notebook)
    (fz-pump 30 (lambda () (not (buffer-modified-p))))
    (let ((here (mapcar (lambda (c) (cons (emjupy-cell-type c)
                                          (emjupy-cell-source c)))
                        (fz-cells)))
          (saved (condition-case err (fz-saved-cells file)
                   (error (fz-fail "%s: the saved file unreadable: %S"
                                   (buffer-name) err)
                          nil))))
      (cond ((null saved))
            ((equal here saved)
             (fz-log "%s: saved, %d cells, as in the buffer"
                     (buffer-name) (length saved)))
            (t (fz-fail "%s: saved %d cells, the buffer has %d; first differing: %S"
                        (buffer-name) (length saved) (length here)
                        (cl-loop for a in here for b in saved
                                 unless (equal a b) return (list a b))))))))

;;;; The session

(defun fz-one-step (step)
  "Do STEP, a random action as a user would, then check every notebook."
  (let* ((action (fz-weighted fz-actions))
         (where (buffer-name))
         (t0 (float-time)))
    (setq fz-step-name (format "step %d %s" step action))
    (let ((how (fz-act action)))
      (fz-pump (/ (+ 5 (fz-random 40)) 100.0))
      (push (format "%d %s %s" step action how) fz-history)
      (fz-log "step %d %s: %s, %s (%.2f s)" step where action how
              (- (float-time) t0)))
    (dolist (err (fz-new-async-errors))
      (fz-fail "step %d: %s" step err))
    (dolist (nb fz-notebooks) (fz-check (nth 2 nb)))))

(defun fz-open-all ()
  "Open the notebooks EMJUPY_FUZZ_NOTEBOOKS names, each timed."
  (dolist (spec (split-string (fz-env "EMJUPY_FUZZ_NOTEBOOKS" "")))
    (pcase-let ((`(,file ,env) (split-string spec "=")))
      (let* ((t0 (float-time))
             (buffer (condition-case err (fz-open file env)
                       (error (fz-fail "opening %s raised %S" file err) nil))))
        (when buffer
          (if (fz-pump 90 (lambda () (fz-live-p buffer)))
              (fz-log "opened %s in %.1f s" (buffer-name buffer)
                      (- (float-time) t0))
            (fz-fail "%s: no kernel connection 90 s after opening" file))
          (push (list file env buffer) fz-notebooks)))))
  (setq fz-notebooks (nreverse fz-notebooks))
  (unless fz-notebooks
    (fz-fail "no notebook opened: set EMJUPY_FUZZ_NOTEBOOKS")))

(defun fz-main ()
  "Run the session; exit 1 if anything failed."
  (write-region "" nil fz-log-file nil 'silent)
  (setq emjupy-language-support
        (not (equal (fz-env "EMJUPY_FUZZ_EGLOT" "1") "0")))
  (fz-log "seed %d, %d steps, Emacs %s, language support %s"
          fz-seed fz-steps emacs-version emjupy-language-support)
  (fz-watch-blocking)
  (fz-open-all)
  (fz-new-async-errors)
  (when fz-notebooks
    (fz-show (nth 2 (car fz-notebooks)))
    (cl-loop for step from 1 to fz-steps
             while (or fz-keep-going (null fz-failures))
             do (fz-one-step step))
    (setq fz-step-name "the end")
    (when (or fz-keep-going (null fz-failures))
      (dolist (nb fz-notebooks)
        (fz-run-everything (nth 2 nb))
        (fz-save-and-compare (nth 0 nb) (nth 2 nb)))
      (dolist (err (fz-new-async-errors)) (fz-fail "at the end: %s" err))))
  (fz-log "the longest Emacs was blocked: %.2f s, in %s" fz-blocked fz-blocked-at)
  (fz-log "language server routes: %S"
          (mapcar (lambda (nb) (emjupy-notebook-lsp-route
                                (buffer-local-value 'emjupy--buffer-notebook
                                                    (nth 2 nb))))
                  fz-notebooks))
  (fz-log "== %d failure(s), %d problem(s)" (length fz-failures)
          (length fz-problems))
  (kill-emacs (if fz-failures 1 0)))

(provide 'emjupy-fuzz)

;; As a library -- emjupy-fuzz-daemon.el, in a daemon -- nothing is run.
(unless (fz-env "EMJUPY_FUZZ_LIBRARY" nil) (fz-main))

;;; emjupy-fuzz.el ends here
