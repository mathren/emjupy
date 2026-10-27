;;; emjupy-cells.el --- The buffer synced into the cells, and checked  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Syncing buffer text back into the cell structs -- the cells are the
;; notebook, the buffer a drawing of them -- and checking the two agree.
;; The commands on cells are in emjupy-commands.el; drawing is in
;; emjupy-render.el, redrawing in emjupy-redraw.el.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)

;;;; Options

(defcustom emjupy-confirm-clear-output t
  "When non-nil, ask before discarding a cell's output.

Output can represent a long run, and the kernel cannot be asked to
produce it again for nothing.  The question costs a keystroke; the
mistake costs the run."
  :type 'boolean
  :group 'emjupy-editing
  :safe #'booleanp)

;;;; Syncing and checking

(defun emjupy--sync-cell-source-from-buffer (cell)
  "Copy the buffer text inside CELL's overlay into its `emjupy-cell-source'."
  (when-let* ((ov (emjupy-cell-overlay cell)))
    (when (overlay-buffer ov)
      (let* ((text (buffer-substring-no-properties (overlay-start ov)
                                                   (overlay-end ov)))
             (trimmed (string-trim-right text "\n")))
        ;; An empty cell is drawn with a newline, to have a line to type on;
        ;; that newline is not source.
        (setf (emjupy-cell-source cell)
              (if (string-empty-p (string-trim-right trimmed "[\n]+"))
                  "" trimmed))))))

(defun emjupy--cell-problems (cell index prev-end structure-only)
  "Return the ways CELL, the INDEXth, disagrees with this buffer, in order.
PREV-END is where the cell before it ends, or nil.  With STRUCTURE-ONLY,
its text is not compared with its source."
  (let ((ov (emjupy-cell-overlay cell))
        (out (emjupy-cell-output-ov cell))
        (problems nil))
    (cond
     ((not (overlayp ov))
      (push (format "cell %d has no overlay" index) problems))
     ((not (eq (overlay-buffer ov) (current-buffer)))
      (push (format "cell %d's overlay is in another buffer" index)
            problems))
     (t
      (when (> (overlay-start ov) (overlay-end ov))
        (push (format "cell %d's overlay is inside out" index) problems))
      (when (> (overlay-end ov) (point-max))
        (push (format "cell %d's overlay runs past the buffer" index)
              problems))
      (when (and prev-end (< (overlay-start ov) prev-end))
        (push (format "cell %d overlaps the cell before it" index)
              problems))
      ;; The text there, against the text that would be saved.  Trailing
      ;; blank lines are not compared: an empty cell's line is display.
      (unless structure-only
        (let ((shown (string-trim-right
                      (buffer-substring-no-properties
                       (overlay-start ov) (overlay-end ov))
                      "[\n]+"))
              (stored (string-trim-right (or (emjupy-cell-source cell) "")
                                         "[\n]+")))
          (unless (equal shown stored)
            (push (format "cell %d shows %S but holds %S" index
                          shown stored)
                  problems))))
      (when (overlayp out)
        (if (not (eq (overlay-buffer out) (current-buffer)))
            (push (format "cell %d's output box is in another buffer" index)
                  problems)
          (unless (= (overlay-start out) (overlay-end ov))
            (push (format "cell %d's output box does not follow its source"
                          index)
                  problems))))))
    (nreverse problems)))

(defun emjupy--cell-end (cell)
  "Return where CELL ends in the buffer, its output box included."
  (let ((ov (emjupy-cell-overlay cell))
        (out (emjupy-cell-output-ov cell)))
    (if (overlayp out)
        (max (overlay-end ov) (overlay-end out))
      (overlay-end ov))))

(cl-defun emjupy--check-invariants (&key structure-only)
  "Return a list of ways this buffer and its cells disagree, or nil.
With STRUCTURE-ONLY, whether each cell's text matches its source is not
checked: it is false while typing has not been synced.  The invariants:

- every cell has a live overlay in this buffer;
- the overlays run in cell order and do not overlap;
- an output box, where there is one, begins where its source ends;
- nothing but the separating newline sits between one cell and the next;
- the buffer text of each cell matches its source, so a sync would
  change nothing."
  (let ((problems nil)
        (index -1)
        (prev-end nil))
    (dolist (cell (append (or (and emjupy--buffer-notebook
                                   (emjupy-notebook-cells
                                    emjupy--buffer-notebook))
                              [])
                          nil))
      (setq index (1+ index))
      (setq problems (append problems (emjupy--cell-problems
                                       cell index prev-end structure-only)))
      (let ((ov (emjupy-cell-overlay cell)))
        (when (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer)))
          (setq prev-end (emjupy--cell-end cell)))))
    problems))

(defun emjupy-debug-check-invariants ()
  "Report whether this buffer and its cells still agree.
Tests call it after every operation."
  (interactive)
  (let ((problems (emjupy--check-invariants)))
    (if problems
        (message "[emjupy] %d problem%s: %s"
                 (length problems)
                 (if (= (length problems) 1) "" "s")
                 (string-join problems "; "))
      (message "[emjupy] Buffer and cells agree."))
    problems))

(defun emjupy--overlays-sane-p ()
  "Return non-nil if this buffer's cell overlays still describe the cells.
Every cell has a live overlay, in cell order, not overlapping."
  (let ((cells (append (or (emjupy-notebook-cells emjupy--buffer-notebook)
                           []) nil))
        (prev-end 0)
        (ok t))
    (dolist (cell cells)
      (let ((ov (emjupy-cell-overlay cell)))
        (cond
         ((not (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer))))
          (setq ok nil))
         ((or (< (overlay-start ov) prev-end)
              (> (overlay-end ov) (point-max))
              (> (overlay-start ov) (overlay-end ov)))
          (setq ok nil))
         (t (setq prev-end (overlay-end ov))))))
    ok))

(defvar-local emjupy--sync-refused nil
  "Non-nil once a sync has been refused, so the warning is said once.")

(defun emjupy--sync-all-cells ()
  "Sync buffer text for all cells in current buffer.
Refused when the overlays no longer describe the cells: one stale
overlay would read the notebook into a cell, and that would be saved.
\[emjupy-re-render] redraws the buffer from the cells."
  (when emjupy--buffer-notebook
    (if (not (emjupy--overlays-sane-p))
        (progn
          (unless emjupy--sync-refused
            (setq emjupy--sync-refused t)
            (message "[emjupy] Buffer and cells are out of step; not syncing.  %s"
                     (substitute-command-keys "Press \\[emjupy-re-render] to redraw.")))
          nil)
      (setq emjupy--sync-refused nil)
      (cl-loop for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
               do (emjupy--sync-cell-source-from-buffer cell))
      t)))

;;;; Checking the structure

(defvar-local emjupy--integrity-timer nil
  "Timer that checks this buffer when Emacs is next idle, or nil.")

(defun emjupy-cells-enable ()
  "Ready this notebook buffer's check of its structure.
Run when a notebook buffer starts."
  ;; A notebook closed before Emacs is idle again takes its check with it.
  (add-hook 'kill-buffer-hook #'emjupy--cancel-integrity-check nil t))

(defun emjupy--cancel-integrity-check ()
  "Cancel this buffer's pending structure check, if there is one."
  (when (timerp emjupy--integrity-timer)
    (cancel-timer emjupy--integrity-timer)
    (setq emjupy--integrity-timer nil)))

(provide 'emjupy-cells)
;;; emjupy-cells.el ends here
