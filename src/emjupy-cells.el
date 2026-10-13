;;; emjupy-cells.el --- Cell editing operations for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Syncing buffer text back into cell structs, the cell-level editing
;; commands -- insert, delete, move, split, merge, change type, navigation
;; -- and the commands that act on a cell's display: hiding and showing its
;; output, rendering markdown and LaTeX, showing a traceback.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)

;;;; Options

(defcustom emjupy-confirm-clear-output t
  "When non-nil, ask before discarding a cell's output.

Output can represent a long run, and the kernel cannot be asked to
produce it again for nothing.  The question costs a keystroke; the
mistake costs the run."
  :type 'boolean
  :group 'emjupy)

;;;; Redrawing, whole or put right

;; A redraw that stops partway is put right by drawing the notebook again from
;; its cells, which only this layer can do; so the redraws that need it are here
;; too.  The drawing itself is in emjupy-render.el.

(defvar-local emjupy--recovering nil
  "Non-nil while the buffer is being redrawn after a redraw failed.")

(defmacro emjupy--atomic-redraw (keep-typing &rest body)
  "Run BODY, a redraw, so that it happens whole or is put right.
An error or a quit partway leaves the buffer and the cells apart, so
the buffer is redrawn from the cells at once, and the error goes on up.
KEEP-TYPING non-nil means typed text may not be in the cells yet, and is
synced first; a structural command syncs before it starts, and passes
nil."
  (declare (indent 1) (debug t))
  (let ((done (make-symbol "done")))
    `(let ((,done nil))
       (unwind-protect
           (prog1 (progn ,@body) (setq ,done t))
         (cond
          ((and (not ,done) (not emjupy--recovering))
           (let ((emjupy--recovering t))
             (emjupy--recover-from-broken-redraw ,keep-typing)))
          ((and ,done (not emjupy--recovering))
           (emjupy--check-after-redraw)))))))

(defun emjupy--append-stream-output (cell)
  "Draw only what CELL's last stream output gained, if that is all.
Return non-nil if that was done; nothing new to draw counts, when only
the label changes.  Only when nothing drawn could look different: the
same pieces, the stream grown at its end, the same fold width, a whole
line last drawn, and no carriage return or colour escape in it."
  (let* ((out (emjupy-cell-output-ov cell))
         (drawn (and (overlayp out) (eq (overlay-buffer out) (current-buffer))
                     (overlay-get out 'emjupy-drawn)))
         (pieces (and drawn
                      (emjupy--outputs-for-render (emjupy-cell-outputs cell))))
         (last-out (car (last pieces)))
         (old (nth 2 drawn))
         (new (and last-out (emjupy--mime-text (gethash "text" last-out)))))
    (when (and drawn
               (= (length pieces) (nth 0 drawn))
               (eq last-out (nth 1 drawn))
               (eql (nth 3 drawn) (emjupy--box-width))
               (stringp old) (stringp new)
               (string-suffix-p "\n" old)
               (string-prefix-p old new)
               (not (string-match-p "[\r\e]" new)))
      (if (= (length new) (length old))
          ;; Nothing new: the cell finished, say, and only its label
          ;; changes.  That is redrawn; the output, the same, is not.
          ;; Set as drawing the box sets it, then placed as a redraw places it.
          (let ((inhibit-read-only t))
            (overlay-put out 'emjupy-header nil)
            (overlay-put out 'before-string
                         (emjupy--rule (emjupy--cell-out-label cell) "├"))
            (emjupy--refresh-cell-header cell)
            (emjupy--refresh-cell-footer cell)
            (emjupy--reconcile-rules)
            t)
        (let* ((start (overlay-end out))
               (inhibit-read-only t)
               (buffer-undo-list t)
               (emjupy--refontifying t)
               end)
          (emjupy--atomic-redraw t
            (save-excursion
              (goto-char start)
              (insert (substring new (length old)))
              (let ((widest (emjupy--finish-output-piece last-out start)))
                (unless (eq (char-before) ?\n) (insert "\n"))
                (setq end (point))
                (move-overlay out (overlay-start out) end)
                (overlay-put
                 out 'emjupy-widest (max widest
                                         (or (overlay-get out
                                                          'emjupy-widest) 0)))
                (overlay-put out 'emjupy-drawn (emjupy--drawn-state pieces))))
            (when emjupy-protect-non-cell-regions
              (emjupy--make-read-only start end))
            (emjupy--undo-adjust start start (- end start)))
          t)))))

(defun emjupy--refresh-cell-output (cell)
  "Redraw CELL's output box in place, leaving the rest of the buffer alone.
Return non-nil when that could be done; nil means the caller should
redraw the notebook."
  ;; Output that only grew at its end is added to; done is done, and says
  ;; so -- nil here means "redraw the whole notebook instead".
  (or (emjupy--append-stream-output cell)
      (let ((src (emjupy-cell-overlay cell))
            (out (emjupy-cell-output-ov cell)))
        (when (and (overlayp src) (eq (overlay-buffer src) (current-buffer)))
          (let* ((live-out (and (overlayp out)
                                (eq (overlay-buffer out) (current-buffer))))
                 (start (if live-out (overlay-start out) (overlay-end src)))
                 (end (if live-out (overlay-end out) start)))
            (emjupy--atomic-redraw t
              (let ((new-end start))
                (let ((inhibit-read-only t)
                      (buffer-undo-list t)
                      ;; emjupy's own drawing: not an edit to re-highlight
                      (emjupy--refontifying t))
                  (when live-out (delete-overlay out))
                  (setf (emjupy-cell-output-ov cell) nil)
                  (delete-region start end)
                  (save-excursion
                    (goto-char start)
                    (emjupy--render-cell-output cell)
                    (setq new-end (point)))
                  (emjupy--refresh-cell-header cell)
                  ;; The bottom edge belongs to the last box: the output's own
                  ;; footer, or, with no output, the cell's.
                  (emjupy--refresh-cell-footer cell)
                  ;; Only the box just drawn is marked: marking the whole
                  ;; notebook costs as much as all its output, on every update.
                  (when emjupy-protect-non-cell-regions
                    (emjupy--make-read-only start new-end))
                  (emjupy--reconcile-rules))
                ;; Done after the edit, since the shift is the size it turned
                ;; out to be rather than the size expected.
                (emjupy--undo-adjust start end (- new-end end))))
            t)))))

(defun emjupy--repad-output (cell)
  "Re-align CELL's output padding after the window width changed.
Output is folded again only if a line is wider than the narrower width;
anything else looks the same at either."
  (let ((ov (emjupy-cell-output-ov cell))
        (width (emjupy--box-width)))
    (when (and (overlayp ov)
               (let ((folded-at (overlay-get ov 'emjupy-fold-width))
                     (widest (or (overlay-get ov 'emjupy-widest) 0)))
                 (and folded-at (/= folded-at width)
                      (> widest (min folded-at width)))))
      (emjupy--refresh-cell-output cell)
      (setq ov nil))
    (when (overlayp ov)
      ;; Walk the pad markers, one per output line, not the characters: an
      ;; inline figure is one output of some 18,000.
      (let ((pos (overlay-start ov))
            (end (overlay-end ov)))
        (while (and pos (< pos end))
          (if (get-text-property pos 'emjupy-pad)
              (progn
                (put-text-property pos (1+ pos) 'display
                                   `(space :align-to ,width))
                (setq pos (1+ pos)))
            (setq pos (next-single-property-change pos 'emjupy-pad
                                                   nil end))))))))

(defun emjupy--refresh-box-rules (&optional force)
  "Redraw cell outlines at the current window width, if it changed.
With FORCE non-nil, redraw even when the width is unchanged.  Only the
overlay strings are rebuilt, so point, markers and undo are untouched."
  (let ((width (emjupy--box-width)))
    (when (and emjupy--buffer-notebook
               (or force (not (eql width emjupy--last-box-width))))
      (let ((changed (not (eql width emjupy--last-box-width))))
        (setq emjupy--last-box-width width)
        ;; Layers above this one -- the kernel, told how wide to draw --
        ;; hear about it here, since this one cannot name them.
        (when changed
          (run-hook-with-args 'emjupy-box-width-changed-functions
                              emjupy--buffer-notebook width)))
      (cl-loop for cell across
               (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell))
                        (out (emjupy-cell-output-ov cell)))
                    (when (overlayp ov)
                      (overlay-put ov 'emjupy-header nil)
                      (overlay-put ov 'before-string
                                   (emjupy--rule (emjupy--cell-label cell)))
                      ;; A cell with output has no footer of its own: the
                      ;; output box's header doubles as its bottom edge.
                      (overlay-put ov 'after-string
                                   (if (overlayp out) "" (emjupy--rule nil))))
                    (when (overlayp out)
                      (overlay-put out 'emjupy-header nil)
                      (overlay-put out 'before-string
                                   (emjupy--rule (emjupy--cell-out-label cell)
                                                 "├"))
                      (overlay-put out 'after-string (emjupy--rule nil))
                      ;; The output band is padded to a column, so it has to
                      ;; be re-aligned at the new width as well.
                      (let ((inhibit-read-only t)
                            (buffer-undo-list t)
                            ;; emjupy's own drawing: not an edit to re-highlight
                            (emjupy--refontifying t))
                        (emjupy--repad-output cell)))))
      (emjupy--reconcile-rules))))

(defun emjupy--line-numbers-toggled ()
  "Redraw the rules after line numbers are turned on or off.
The number column takes width from the page, so the rules were drawn
too wide for it, and wrapped."
  (when emjupy--buffer-notebook
    (emjupy--refresh-box-rules t)))

(defun emjupy--window-size-changed (&optional _frame)
  "Refresh box rules in every emjupy buffer after a window size change."
  (dolist (buf (emjupy--notebook-buffers))
    (with-current-buffer buf
      (emjupy--refresh-box-rules))))

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

(defun emjupy--check-invariants (&optional structure-only)
  "Return a list of ways this buffer and its cells disagree, or nil.
With STRUCTURE-ONLY, whether each cell's text matches its source is not
checked: it is false while typing has not been synced.  The invariants:

- every cell has a live overlay in this buffer;
- the overlays run in cell order and do not overlap;
- an output box, where there is one, begins where its source ends;
- nothing but the separating newline sits between one cell and the next;
- the buffer text of each cell matches its source, so a sync would
  change nothing."
  (let ((cells (append (or (and emjupy--buffer-notebook
                                (emjupy-notebook-cells emjupy--buffer-notebook))
                           [])
                       nil))
        (problems nil)
        (index -1)
        (prev-end nil))
    (dolist (cell cells)
      (setq index (1+ index))
      (let ((ov (emjupy-cell-overlay cell))
            (out (emjupy-cell-output-ov cell)))
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
                      problems))))
          (setq prev-end (if (overlayp out)
                             (max (overlay-end ov) (overlay-end out))
                           (overlay-end ov)))))))
    (nreverse problems)))

(defun emjupy-debug-check-invariants ()
  "Report whether this buffer and its cells still agree.

Useful when something looks wrong and it is not clear whether the
display or the notebook is at fault.  Tests call it after every
operation."
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

;;;; Redrawing cells in place

(defvar emjupy--rendering-buffers nil
  "Buffers currently being re-rendered.
Global: a re-entrant call comes while another buffer is current.")

(defun emjupy--sweep-stray-overlays ()
  "Delete emjupy overlays that no live cell owns.
A collapsed overlay still shows its rule, so one left by a deleted cell
is a stray border at the top of the buffer."
  (let ((claimed (make-hash-table :test 'eq)))
    (cl-loop for cell across (or (and emjupy--buffer-notebook
                                      (emjupy-notebook-cells
                                       emjupy--buffer-notebook))
                                 [])
             do (let ((ov (emjupy-cell-overlay cell))
                      (out (emjupy-cell-output-ov cell)))
                  (dolist (o (list ov out))
                    (when (overlayp o)
                      (puthash o t claimed)
                      ;; and the overlays that show their rules
                      (dolist (companion (overlay-get o 'emjupy-companions))
                        (when (overlayp companion) (puthash companion
                                                            t claimed)))))))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'emjupy-overlay)
                 (or (not (gethash ov claimed))
                     (= (overlay-start ov) (overlay-end ov))))
        (delete-overlay ov)))))

(defun emjupy--cell-region (cell)
  "Return (START . END) of everything CELL occupies, or nil.
Its source and output box, and the newline after them."
  (let ((src (emjupy-cell-overlay cell))
        (out (emjupy-cell-output-ov cell)))
    (when (and (overlayp src) (eq (overlay-buffer src) (current-buffer)))
      (let ((start (overlay-start src))
            (end (if (and (overlayp out)
                          (eq (overlay-buffer out) (current-buffer)))
                     (overlay-end out)
                   (overlay-end src))))
        (cons start (min (point-max) (1+ end)))))))

(defun emjupy--render-cell-incrementally (cell at)
  "Draw CELL at position AT, leaving the rest of the buffer alone.

Returns the number of characters inserted."
  (let ((inhibit-read-only t)
        (buffer-undo-list t)
        ;; emjupy's own drawing: not an edit to re-highlight
        (emjupy--refontifying t)
        (inserted 0))
    (save-excursion
      (goto-char at)
      (let ((before (point-max)))
        (emjupy--render-cell cell)
        (setq inserted (- (point-max) before))))
    (emjupy--protect-non-cell-regions)
    inserted))

(defun emjupy--redraw-cells-in-place (old-cells new-cells &optional known-sane)
  "Replace what OLD-CELLS occupy with NEW-CELLS, drawn in their place.
What inserting and deleting do, for a split or a merge: everything
outside moves by a known amount, so the undo history is kept.
OLD-CELLS must be adjacent and in buffer order.  KNOWN-SANE skips the
overlay check, for callers that have added a cell with no overlay yet.
Return non-nil when done, nil when the caller should redraw the
notebook."
  (emjupy--atomic-redraw nil
    (let ((regions (delq nil (mapcar #'emjupy--cell-region old-cells))))
      (when (and regions
                 (= (length regions) (length old-cells))
                 ;; KNOWN-SANE is for callers that have put a new cell in the
                 ;; notebook already: it has no overlay yet, so they check
                 ;; before changing anything.
                 (or known-sane (emjupy--overlays-sane-p)))
        (let ((start (apply #'min (mapcar #'car regions)))
              (end (apply #'max (mapcar #'cdr regions)))
              (new-end nil)
              (following-after nil))
          (let ((inhibit-read-only t)
                (buffer-undo-list t)
                ;; emjupy's own drawing: not an edit to re-highlight
                (emjupy--refontifying t))
            (dolist (cell old-cells)
              (when (overlayp (emjupy-cell-overlay cell))
                (delete-overlay (emjupy-cell-overlay cell)))
              (when (overlayp (emjupy-cell-output-ov cell))
                (delete-overlay (emjupy-cell-output-ov cell)))
              (setf (emjupy-cell-overlay cell) nil)
              (setf (emjupy-cell-output-ov cell) nil))
            ;; The next cell may start where this region ends, and take the
            ;; inserted text into its overlay; its bounds are noted and
            ;; restored.
            (setq following-after
                  (let ((cells (and emjupy--buffer-notebook
                                    (append (emjupy-notebook-cells
                                             emjupy--buffer-notebook)
                                            nil))))
                    (cl-find-if (lambda (cell)
                                  (let ((ov (emjupy-cell-overlay cell)))
                                    (and (overlayp ov)
                                         (eq (overlay-buffer ov)
                                             (current-buffer))
                                         (>= (overlay-start ov) end))))
                                cells)))
            (delete-region start end)
            (save-excursion
              (goto-char start)
              (dolist (cell new-cells)
                (emjupy--render-cell cell))
              (setq new-end (point)))
            ;; Put it back where it belongs if it swallowed the new text.
            (let ((ov (and following-after
                           (emjupy-cell-overlay following-after))))
              (when (and (overlayp ov)
                         (eq (overlay-buffer ov) (current-buffer))
                         (< (overlay-start ov) new-end))
                (move-overlay ov new-end (max new-end (overlay-end ov)))))
            (emjupy--protect-non-cell-regions))
          (emjupy--undo-adjust start end (- new-end end))
          t)))))

;;;; Inserting and deleting

(defun emjupy-insert-cell-at (nb index new-cell)
  "Put NEW-CELL into NB at INDEX and draw it without rebuilding the buffer.

A full rebuild costs the undo history, because it moves every position
in the buffer while undo recording is off and undo entries hold plain
positions.  Inserting one cell moves only what follows it, and by a
known amount, so the history can be kept: entries before the insertion
are untouched, and those after it shift.  See notes_undo.org.

Returns non-nil when it was done incrementally, nil when the caller
should fall back to a full redraw."
  (emjupy--atomic-redraw nil
    (let* ((cells (append (emjupy-notebook-cells nb) nil))
           (following (nth index cells))
           (at (cond
                ;; before an existing cell: at its start
                (following (car (emjupy--cell-region following)))
                ;; at the very end: after the last cell there is
                (cells (cdr (emjupy--cell-region (car (last cells)))))
                ;; an empty notebook has nowhere to be incremental about
                (t nil))))
      (when (and at (emjupy--overlays-sane-p))
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 index)
                               (list new-cell)
                               (cl-subseq cells index))))
        (let ((following-ov (and following (emjupy-cell-overlay following)))
              (inserted 0))
          (setq inserted (emjupy--render-cell-incrementally new-cell at))
          ;; Text inserted at an overlay's start may go into that overlay,
          ;; depending on how it was made and the Emacs version; then the next
          ;; cell swallows the new one, and syncing stops.  So the two overlays
          ;; are tested for overlap, and repaired.
          (let ((new-ov (emjupy-cell-overlay new-cell))
                (resume (+ at inserted)))
            (when (and (overlayp following-ov)
                       (overlayp new-ov)
                       (eq (overlay-buffer following-ov) (current-buffer))
                       (< (overlay-start following-ov) (overlay-end new-ov)))
              ;; Past everything inserted, the separating blank line included:
              ;; from the new cell's end, that newline would land in the next
              ;; cell's source.
              (move-overlay following-ov resume
                            (max resume (overlay-end following-ov))))
            ;; Again, now the next cell starts where it should: protected while
            ;; its overlay still covered the new cell, the gap was left
            ;; writable, and a backspace there joined the two cells.  Not
            ;; recorded for undo: it edits nothing.
            (let ((buffer-undo-list t))
              (emjupy--protect-non-cell-regions)))
          (emjupy--undo-adjust at at inserted))
        t))))

(defun emjupy-delete-cell-at (nb cell)
  "Remove CELL from NB and erase it without rebuilding the buffer.

Returns non-nil when it was done incrementally."
  (emjupy--atomic-redraw nil
    (let ((region (emjupy--cell-region cell)))
      (when (and region (emjupy--overlays-sane-p))
        (let ((start (car region))
              (end (cdr region)))
          (setf (emjupy-notebook-cells nb)
                (vconcat (delq cell (append (emjupy-notebook-cells nb) nil))))
          (let ((inhibit-read-only t)
                (buffer-undo-list t)
                ;; emjupy's own drawing: not an edit to re-highlight
                (emjupy--refontifying t))
            (when (overlayp (emjupy-cell-overlay cell))
              (delete-overlay (emjupy-cell-overlay cell)))
            (when (overlayp (emjupy-cell-output-ov cell))
              (delete-overlay (emjupy-cell-output-ov cell)))
            (setf (emjupy-cell-overlay cell) nil)
            (setf (emjupy-cell-output-ov cell) nil)
            (delete-region start end)
            (emjupy--protect-non-cell-regions))
          ;; The deleted text is gone, so entries describing it go; what
          ;; followed it moves up by its length.
          (emjupy--undo-adjust start end (- (- end start)))
          t)))))

;;;; Redrawing the notebook

(defun emjupy--rerender-notebook (&optional target-cell)
  "Re-render this notebook, leaving point at TARGET-CELL if given.
A call made while a render is running is dropped: re-entering would
rebuild a half-built buffer, and the running render reads the same,
updated cells."
  (unless (memq (current-buffer) emjupy--rendering-buffers)
    (let ((emjupy--rendering-buffers (cons (current-buffer)
                                           emjupy--rendering-buffers)))
      (if target-cell
          ;; A target says where point ends up, not that the window should jump
          ;; to it.
          (emjupy--keeping-the-window
           (lambda () (emjupy--rerender-notebook-1 target-cell)))
        (emjupy--keeping-the-view
         (lambda () (emjupy--rerender-notebook-1 nil)))))))

(defun emjupy--visible-from (pos)
  "Return POS, or the first visible position after it if it is hidden.
The empty line between two cells is invisible, and a window can start on
it; remembering that line put the view back on it, not on the cell."
  (if (invisible-p pos)
      (next-single-char-property-change pos 'invisible)
    pos))

(defun emjupy--keeping-the-window (thunk)
  "Call THUNK, leaving each window scrolled where it was.

Point is left wherever THUNK put it: this is for redraws that do mean to
move point, and only the scroll position has to survive."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (starts (mapcar (lambda (w)
                           (cons w
                                 (line-number-at-pos
                                  (emjupy--visible-from (window-start w)))))
                         windows)))
    (unwind-protect
        (funcall thunk)
      (dolist (pair starts)
        (let ((w (car pair)))
          (when (window-live-p w)
            (set-window-start
             w (save-excursion (emjupy--goto-line-column (cdr pair) 0) (point))
             t)
            (set-window-point w (point))))))))

(defun emjupy--keeping-the-view (thunk)
  "Call THUNK, leaving point and the window scrolled where they were.
As line and column: output boxes grow and shrink, so offsets change,
but the line being read is the same line."
  (let ((line (line-number-at-pos (point))) (column (current-column)))
    (emjupy--keeping-the-window
     (lambda ()
       (unwind-protect (funcall thunk)
         (emjupy--goto-line-column line column))))))

(defun emjupy--goto-line-column (line column)
  "Put point at COLUMN of LINE, or as near to it as the buffer permits."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))

(defun emjupy--rerender-notebook-1 (&optional target-cell)
  "Re-render every cell overlay in the current buffer.
If TARGET-CELL is given, leave point at that cell afterwards.  The
rebuild is not itself undone entry by entry: it records one step that
puts the cells back, so undo carries on through it."
  (when emjupy--buffer-notebook
    (let ((inhibit-read-only t)
          ;; emjupy's own drawing: not an edit to re-highlight
          (emjupy--refontifying t)
          ;; the window measured once for the whole redraw, not per rule
          (emjupy--box-width-memo (list nil))
          (cells (emjupy-notebook-cells emjupy--buffer-notebook))
          (target-start nil)
          (snapshot (emjupy--snapshot-cells))
          (before (buffer-substring-no-properties (point-min) (point-max))))
      (save-restriction
        (widen)
        (let ((buffer-undo-list t))
          ;; Every emjupy overlay goes, not only the current cells': one whose
          ;; cell is gone is unreachable from `cells', and `erase-buffer' would
          ;; leave it at position 1, showing its rules.
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (overlay-get ov 'emjupy-overlay)
              (delete-overlay ov)))
          (cl-loop for cell across cells
                   do (setf (emjupy-cell-overlay cell) nil)
                   (setf (emjupy-cell-output-ov cell) nil))
          (erase-buffer)
          ;; Every cell first, point after: moving point to the target mid-loop
          ;; would make later inserts land inside its overlay.
          (cl-loop for cell across cells
                   ;; Each at the end, wherever point has got to: drawn at
                   ;; point, one call that moved it -- measuring a window,
                   ;; which selects it -- drew the rest on top of each other.
                   do (goto-char (point-max))
                   (let ((start (point)))
                     (emjupy--render-cell cell)
                     (when (eq cell target-cell)
                       (setq target-start start))))
          (when target-start
            (goto-char target-start))
          ;; Nothing may be left drawing a rule that belongs to no cell.
          (emjupy--sweep-stray-overlays)
          ;; All that is not a cell's source is read-only: rules, gutters and
          ;; output boxes belong to no cell, and anything typed there would
          ;; vanish at the next redraw.
          (emjupy--protect-non-cell-regions)))
      (unless (equal before (buffer-substring-no-properties (point-min)
                                                            (point-max)))
        ;; The rebuild moved every position the history holds.  It records
        ;; itself as one step that puts the cells back as they were, so undoing
        ;; it restores the text the older entries describe, and undo carries on
        ;; through them.
        (emjupy--record-rebuild-step snapshot)))))

;;;; Undo

(defvar emjupy--restoring-snapshot nil
  "Non-nil while a recorded rebuild is being undone.
Stops the redraw that restoring performs from recording a step of its
own, which would undo the undo.")

(defun emjupy--copy-cell-fields (cell)
  "Return a copy of CELL's fields, with its metadata table copied too.
The metadata table is changed in place -- hiding output writes to it --
so a shallow copy would change along with the cell."
  (let ((copy (copy-emjupy-cell cell)))
    (when (hash-table-p (emjupy-cell-metadata cell))
      (setf (emjupy-cell-metadata copy)
            (copy-hash-table (emjupy-cell-metadata cell))))
    copy))

(defun emjupy--snapshot-cells ()
  "Return the notebook's cells as they are now, enough to put them back.
A vector of (CELL . FIELDS).  Restored into the same cell objects, not
copies, since other undo entries name the cells."
  (when emjupy--buffer-notebook
    (vconcat (mapcar (lambda (cell) (cons cell (emjupy--copy-cell-fields cell)))
                     (append
                      (emjupy-notebook-cells emjupy--buffer-notebook) nil)))))

(defun emjupy--snapshot-restore-into (snapshot)
  "Write SNAPSHOT's fields back into its cells; return the cells as a vector."
  (vconcat
   (mapcar (lambda (pair)
             (let ((cell (car pair)) (fields (cdr pair)))
               ;; every slot of the record, then the overlays, which belong
               ;; to the drawing and are made again by it
               (cl-loop for i from 1 below (length fields)
                        do (aset cell i (aref fields i)))
               (when (hash-table-p (emjupy-cell-metadata fields))
                 (setf (emjupy-cell-metadata cell)
                       (copy-hash-table (emjupy-cell-metadata fields))))
               (setf (emjupy-cell-overlay cell) nil)
               (setf (emjupy-cell-output-ov cell) nil)
               cell))
           (append snapshot nil))))

(defun emjupy--restore-cells-snapshot (snapshot)
  "Put SNAPSHOT back and redraw, as the undo of a rebuild."
  (when (and emjupy--buffer-notebook snapshot)
    (let ((now (emjupy--snapshot-cells))
          (emjupy--restoring-snapshot t))
      ;; the old drawing goes: its overlays belong to the cells as they are
      (cl-loop for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
               do (dolist (ov (list (emjupy-cell-overlay cell)
                                    (emjupy-cell-output-ov cell)))
                    (when (overlayp ov) (delete-overlay ov))))
      (setf (emjupy-notebook-cells emjupy--buffer-notebook)
            (emjupy--snapshot-restore-into snapshot))
      ;; Held still: this runs during an undo, and a rebuild that leaves
      ;; point at the end of the buffer throws the reader to the bottom of
      ;; the notebook just as they are trying to take something back.
      (emjupy--keeping-the-view (lambda () (emjupy--rerender-notebook-1)))
      ;; and the redo
      (unless (eq buffer-undo-list t)
        (push (list 'apply #'emjupy--restore-cells-snapshot now)
              buffer-undo-list)))))

(defmacro emjupy--as-one-undo-step (&rest body)
  "Run BODY, a change to the notebook's cells, as one step undo takes back.
The cells are snapshotted before BODY and restored by the undo, which
records the way forward again, for redo.  A BODY that signals records
nothing."
  (declare (indent 0) (debug t))
  `(let* ((snapshot (progn (emjupy--sync-all-cells) (emjupy--snapshot-cells)))
          ;; No earlier entry is shifted or dropped while BODY runs: undoing the
          ;; step restores the text they describe first.
          (result (let ((emjupy--in-cell-step t)) ,@body)))
     (unless (or (eq buffer-undo-list t) undo-in-progress (null snapshot))
       (push (list 'apply #'emjupy--restore-cells-snapshot snapshot)
             buffer-undo-list)
       (undo-boundary))
     result))

(defun emjupy--record-rebuild-step (snapshot)
  "Record SNAPSHOT as the state a rebuild can be undone to."
  (unless (or (eq buffer-undo-list t)
              emjupy--restoring-snapshot
              emjupy--in-cell-step
              undo-in-progress
              (null snapshot))
    (push (list 'apply #'emjupy--restore-cells-snapshot snapshot)
          buffer-undo-list)
    (undo-boundary)))

;;;; Cell commands

(defun emjupy--insert-empty-cell (below)
  "Insert an empty code cell next to the cell at point: BELOW it, or above."
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((nb emjupy--buffer-notebook)
           (cells (append (emjupy-notebook-cells nb) nil))
           (idx (cl-position (emjupy--cell-at-point) cells))
           (index (cond ((null idx) (if below (length cells) 0))
                        (below (1+ idx))
                        (t idx)))
           (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                       :source "" :outputs []
                                       :metadata (make-hash-table))))
      (unless (emjupy-insert-cell-at nb index new-cell)
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 index)
                               (list new-cell)
                               (cl-subseq cells index))))
        (emjupy--rerender-notebook new-cell))
      (emjupy--goto-cell new-cell 'start))))

(defun emjupy-insert-cell-below ()
  "Insert a new empty code cell below the cell at point."
  (interactive)
  (emjupy--insert-empty-cell t))

(defun emjupy-insert-cell-above ()
  "Insert a new empty code cell above the cell at point."
  (interactive)
  (emjupy--insert-empty-cell nil))

(defun emjupy-move-cell-up ()
  "Move the cell at point up, swapping it with the cell above.
Since a cell's output lives inside its own struct rather than as a
separate entity, the output always travels with its cell automatically."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
           (cell (emjupy--cell-at-point))
           (idx (cl-position cell cells)))
      (if (or (not idx) (= idx 0))
          (message "[emjupy] Cell is already at the top.")
        (let* ((above (aref cells (1- idx)))
               (sane (emjupy--overlays-sane-p)))
          (aset cells (1- idx) cell)
          (aset cells idx above)
          ;; Only the two cells swapped change, so only they are redrawn --
          ;; a rebuild moves every buffer position that an undo entry holds,
          ;; and takes the history with it.
          (unless (and sane
                       (emjupy--redraw-cells-in-place (list above cell)
                                                      (list cell above) t))
            (emjupy--rerender-notebook cell))
          ;; The move is its own undo step, so an edit in the cell it
          ;; rewrites is recovered by undoing the move rather than lost.
          (emjupy--goto-cell cell 'start))))))

(defun emjupy-move-cell-down ()
  "Move the cell at point down, swapping it with the cell below."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
           (cell (emjupy--cell-at-point))
           (idx (cl-position cell cells)))
      (if (or (not idx) (= idx (1- (length cells))))
          (message "[emjupy] Cell is already at the bottom.")
        (let* ((below (aref cells (1+ idx)))
               (sane (emjupy--overlays-sane-p)))
          (aset cells (1+ idx) cell)
          (aset cells idx below)
          ;; As for moving up: two cells change, so two cells are redrawn.
          (unless (and sane
                       (emjupy--redraw-cells-in-place (list cell below)
                                                      (list below cell) t))
            (emjupy--rerender-notebook cell))
          (emjupy--goto-cell cell 'start))))))

;;;; Putting a redraw right

(defun emjupy--rerender-preserving-point ()
  "Re-render the notebook without disturbing where the user is typing.
Not with TARGET-CELL, which moves point to the start of that cell."
  ;; What is on screen goes back into the cells FIRST: the redraw rebuilds the
  ;; buffer from them, and anything typed since the last sync would be lost.
  (emjupy--sync-all-cells)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell)))
         (offset (and ov (- (point) (overlay-start ov)))))
    (emjupy--rerender-notebook)
    (when-let* ((cell cell)
                (ov (emjupy-cell-overlay cell)))
      (when (overlayp ov)
        (goto-char (min (overlay-end ov)
                        (+ (overlay-start ov) (or offset 0))))))))

(defun emjupy--recover-from-broken-redraw (keep-typing)
  "Redraw the notebook from its cells, after a redraw that stopped partway.
With KEEP-TYPING, what is typed is synced first: for output redrawn, the
cells's text was not being touched, so it is what the user typed.  A
structural command synced before it started, and its cells are whole."
  (let ((inhibit-quit t))
    (condition-case err
        (progn
          (when keep-typing (emjupy--sync-sound-cells))
          (setq emjupy--sync-refused nil)
          (emjupy--rerender-preserving-point)
          (message "[emjupy] A redraw stopped partway; the notebook was redrawn from its cells."))
      ;; Everything: this runs while an error is already unwinding, and a
      ;; second one raised here would hide the first, which is the one to see.
      (error (message "[emjupy] A redraw stopped partway, and redrawing failed too (%s); %s"
                      (error-message-string err)
                      (substitute-command-keys "\\[emjupy-re-render] redraws it."))))))

(defun emjupy--sync-sound-cells ()
  "Sync every cell whose own overlay still spans exactly its text.
Where the overlays as a whole are wrong, syncing all is refused, but a
cell whose overlay is live, in order and clear of its neighbours holds
what was typed in it."
  (let ((prev-end (point-min)))
    (cl-loop for cell across
             (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
             do (let ((ov (emjupy-cell-overlay cell)))
                  (when (and (overlayp ov) (eq (overlay-buffer ov)
                                               (current-buffer))
                             (<= prev-end (overlay-start ov) (overlay-end ov)
                                 (point-max)))
                    (emjupy--sync-cell-source-from-buffer cell)
                    (setq prev-end (overlay-end ov)))))))

;;;; Checking the structure

(defvar-local emjupy--integrity-timer nil
  "Timer that checks this buffer when Emacs is next idle, or nil.")

(defun emjupy--check-after-redraw ()
  "Check this buffer's structure when Emacs is next idle, once.
Many redraws in a row -- output streaming in -- cost one check."
  (unless (timerp emjupy--integrity-timer)
    (setq emjupy--integrity-timer
          (run-with-idle-timer 1 nil #'emjupy--check-integrity
                               (current-buffer)))))

(defun emjupy--check-integrity (buffer)
  "Put BUFFER right if its structure and its cells have come apart.
Only the structure is checked, not the text, which differs while
typing is unsynced.  The problems found are reported."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq emjupy--integrity-timer nil)
      (when-let* ((problems (and emjupy--buffer-notebook
                                 (not emjupy--recovering)
                                 (emjupy--check-invariants t))))
        (let ((emjupy--recovering t))
          (emjupy--sync-sound-cells)
          (setq emjupy--sync-refused nil)
          (emjupy--rerender-preserving-point))
        (message "[emjupy] The notebook was redrawn: its display had come apart from its cells (%s)."
                 (string-join problems "; "))))))

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

;;;; Redrawing by hand

(defun emjupy-re-render ()
  "Rebuild the notebook display from the cells.

Everything on screen is drawn from the cell structs, so if the buffer
ever looks wrong -- overlays out of place after an unusual edit, say --
this puts it back without touching the notebook itself.  Edits in the
buffer are folded in first, so nothing you have typed is lost."
  (interactive)
  (emjupy--sync-all-cells)
  (emjupy--rerender-preserving-point)
  (message "[emjupy] Redrew the notebook."))

;;;; Splitting and joining

(defvar-local emjupy--split-was-sane nil
  "Whether the overlays lined up before a split rearranged the cells.")

(defun emjupy-split-cell ()
  "Split the cell at point in two, at point.

Text before point stays in this cell; text from point on moves to a new
cell of the same type just below, and point follows it there.

The output of BOTH halves is discarded, along with the execution count.
Neither half has been run as it now stands, so keeping the results would
attribute them to source that never produced them."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((nb (emjupy--notebook))
           (cell (emjupy--cell-at-point)))
      ;; Output text carries no cell property and lies outside the source
      ;; overlay, so this also catches point sitting in an output box.
      (unless cell
        (user-error "Point is not in a cell"))
      (let* ((ov (emjupy-cell-overlay cell))
             (source (or (emjupy-cell-source cell) ""))
             (offset (max 0 (min (- (point) (overlay-start ov))
                                 (length source))))
             (new-cell (make-emjupy-cell
                        :id (emjupy--new-cell-id)
                        :type (emjupy-cell-type cell)
                        :source (substring source offset)
                        :outputs []
                        :metadata (make-hash-table :test 'equal)))
             (cells (append (emjupy-notebook-cells nb) nil))
             (idx (cl-position cell cells)))
        (setq emjupy--split-was-sane (emjupy--overlays-sane-p))
        (setf (emjupy-cell-source cell) (substring source 0 offset))
        ;; Neither half produced what is on screen any more.
        (setf (emjupy-cell-outputs cell) [])
        (setf (emjupy-cell-exec-count cell) nil)
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 (1+ idx))
                               (list new-cell)
                               (cl-subseq cells (1+ idx)))))
        (unless (emjupy--redraw-cells-in-place (list cell)
                                               (list cell new-cell)
                                               emjupy--split-was-sane)
          (emjupy--rerender-notebook new-cell))
        (let ((ov (emjupy-cell-overlay new-cell)))
          (when (overlayp ov) (goto-char (overlay-start ov))))
        new-cell))))

(defun emjupy-join-cell-above ()
  "Merge the cell at point into the one above it.

The two sources are joined with a newline between them, the upper cell
absorbs the lower, and point lands at the seam -- where the second cell
used to begin.

The output of BOTH cells is discarded, along with their execution
counts.

The cells must be of the same type; merging code into prose, or the
reverse, would silently reinterpret one of them."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--as-one-undo-step
      (emjupy--sync-all-cells)
      (let* ((nb (emjupy--notebook))
             (cell (emjupy--cell-at-point)))
        (unless cell
          (user-error "Point is not in a cell"))
        (let* ((cells (append (emjupy-notebook-cells nb) nil))
               (idx (cl-position cell cells)))
          (when (zerop idx)
            (user-error "No cell above this one"))
          (let* ((above (nth (1- idx) cells)))
            (unless (eq (emjupy-cell-type above) (emjupy-cell-type cell))
              (user-error "Cannot merge a %s cell into a %s cell"
                          (emjupy-cell-type cell) (emjupy-cell-type above)))
            (let* ((sane (emjupy--overlays-sane-p))
                   (upper (or (emjupy-cell-source above) ""))
                   (lower (or (emjupy-cell-source cell) ""))
                   (seam (length (if (string-suffix-p "\n" upper)
                                     upper
                                   (concat upper "\n")))))
              (setf (emjupy-cell-source above)
                    (concat (if (string-suffix-p "\n" upper) upper
                              (concat upper "\n"))
                            lower))
              ;; Neither cell's results describe the merged source any more.
              (setf (emjupy-cell-outputs above) [])
              (setf (emjupy-cell-exec-count above) nil)
              (setf (emjupy-notebook-cells nb)
                    (vconcat (append (cl-subseq cells 0 idx)
                                     (cl-subseq cells (1+ idx)))))
              ;; Checked before the cell list changes: afterwards the merged
              ;; cell is gone, the overlays disagree, and a full rebuild loses
              ;; the undo history.
              (unless (emjupy--redraw-cells-in-place (list above cell)
                                                     (list above) sane)
                (emjupy--rerender-notebook above))
              ;; Leave point at the seam, where the merged-in cell begins.
              (let ((ov (emjupy-cell-overlay above)))
                (when (overlayp ov)
                  (goto-char (min (overlay-end ov)
                                  (+ (overlay-start ov) seam)))))
              above)))))))

;;;###autoload

;;;; Output

(defun emjupy-toggle-cell-output ()
  "Collapse or restore the output of the cell at point.

Collapsing hides the output and marks the cell's bottom rule with a
glyph.  Nothing is discarded: the outputs stay on the cell, so restoring
them is a redraw rather than a re-run, and the kernel is not involved
either way.

The state is kept in the cell's metadata where Jupyter keeps it, so it
survives saving and means the same thing elsewhere."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--as-one-undo-step
      (emjupy--sync-all-cells)
      (let ((cell (emjupy--cell-at-point-including-output)))
        (unless cell (user-error "Point is not in a cell"))
        (emjupy-toggle-output-of-cell cell)))))

(defun emjupy--cell-by-id (id)
  "Return the cell of this notebook whose id is ID, or nil."
  (when (and id emjupy--buffer-notebook)
    (cl-find-if (lambda (cell) (equal (emjupy-cell-id cell) id))
                (append (emjupy-notebook-cells emjupy--buffer-notebook) nil))))

(defun emjupy-toggle-output-of-cell (cell)
  "Collapse or restore the output of CELL.

Takes the cell rather than finding it at point, so that a click on a
collapsed marker can act on the cell the marker belongs to without
moving point or guessing from a buffer position."
  (unless cell (user-error "No cell there"))
  (when (zerop (length (or (emjupy-cell-outputs cell) [])))
    (user-error "This cell has no output to hide"))
  (let ((hidden (not (emjupy--cell-outputs-hidden-p cell))))
    (emjupy--set-cell-outputs-hidden cell hidden)
    ;; Only this cell changes, so only this cell is redrawn -- which also
    ;; keeps the undo history.
    (unless (emjupy--redraw-cells-in-place (list cell) (list cell))
      (emjupy--rerender-notebook cell))
    (message "[emjupy] Output %s." (if hidden "hidden" "shown"))
    hidden))

(defun emjupy--confirm-clearing (cell)
  "Return non-nil if CELL's output may be discarded.
The prompt names the outputs and the execution count that made them."
  (or (not emjupy-confirm-clear-output)
      (let* ((n (length (or (emjupy-cell-outputs cell) [])))
             (count (emjupy-cell-exec-count cell)))
        (yes-or-no-p (format "Clear %d output%s%s? "
                             n (if (= n 1) "" "s")
                             (if count (format " from [%s]" count) ""))))))

(defun emjupy-clear-cell-output ()
  "Discard the output of the cell at point.

The output box goes with it, so the cell shrinks back to just its
source, and the execution count is cleared too: nothing has been run
since, so leaving [In: 4] beside no output would claim otherwise.

Only the buffer is touched.  The notebook on the server keeps its
outputs until you save.

Asks first, since the output may have taken a long time to produce and
clearing it is not something the kernel can be asked to repeat cheaply.
`emjupy-confirm-clear-output' turns the question off."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--as-one-undo-step
      (emjupy--sync-all-cells)
      (let ((cell (emjupy--cell-at-point)))
        (unless cell (user-error "Point is not in a cell"))
        (if (zerop (length (or (emjupy-cell-outputs cell) [])))
            (message "[emjupy] This cell has no output.")
          (if (not (emjupy--confirm-clearing cell))
              (message "[emjupy] Left it alone.")
            (setf (emjupy-cell-outputs cell) [])
            (setf (emjupy-cell-exec-count cell) nil)
            ;; Only this cell's box goes, so only this cell is redrawn, which
            ;; keeps the undo history.
            (unless (emjupy--refresh-cell-output cell)
              (emjupy--rerender-notebook cell))
            (message "[emjupy] Cleared this cell's output.")))))))

(defun emjupy-clear-all-outputs ()
  "Discard the output of every cell in the notebook.

Asks first: this throws away results that may have taken a while to
produce, and re-running them is not always cheap.

Only the buffer is touched.  The notebook on the server keeps its
outputs until you save."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--as-one-undo-step
      (emjupy--sync-all-cells)
      (let* ((cells (emjupy-notebook-cells (emjupy--notebook)))
             (with-output (cl-count-if
                           (lambda (c)
                             (> (length (or (emjupy-cell-outputs c) [])) 0))
                           cells)))
        (cond
         ((zerop with-output)
          (message "[emjupy] No cell has any output."))
         ((not (yes-or-no-p (format "Clear the output of %d cell%s? "
                                    with-output (if (= with-output 1) "" "s"))))
          (message "[emjupy] Left them alone."))
         (t
          (let ((cleared (cl-loop for cell across cells
                                  do (setf (emjupy-cell-outputs cell) []
                                           (emjupy-cell-exec-count cell) nil)
                                  collect cell)))
            ;; Each cell is redrawn on its own, keeping the undo history; if any
            ;; cannot be, the whole notebook is, rather than half each way.
            (unless (cl-every (lambda (cell)
                                (and (emjupy--refresh-cell-header cell)
                                     (emjupy--refresh-cell-output cell)))
                              cleared)
              (emjupy--rerender-notebook)))
          (message "[emjupy] Cleared the output of %d cell%s."
                   with-output (if (= with-output 1) "" "s"))))))))

;;;; Deleting, copying and yanking

(defun emjupy--cell-index (cell)
  "Return the position of CELL in the notebook, or nil."
  (and emjupy--buffer-notebook
       (cl-position cell
                    (append
                     (emjupy-notebook-cells emjupy--buffer-notebook) nil))))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((nb emjupy--buffer-notebook)
           (curr-cell (emjupy--cell-at-point))
           (cells (append (emjupy-notebook-cells nb) nil)))
      (when curr-cell
        (let* ((idx (cl-position curr-cell cells))
               (remaining (remq curr-cell cells))
               ;; The cell that slides up into the deleted one's place, or the
               ;; last one if it was at the end.  Leaving point at the end of
               ;; the notebook, as it used to, loses your place entirely.
               (next (and remaining
                          (nth (min idx (1- (length remaining))) remaining))))
          (unless ;; undoing the delete puts this exact text back, so
              ;; edits recorded inside it are kept for then
              (emjupy-delete-cell-at nb curr-cell)
            (setf (emjupy-notebook-cells nb) (vconcat remaining))
            (emjupy--rerender-notebook next))
          ;; An entry undo can act on.  The text deletion itself is made with
          ;; recording off -- a redraw moves positions that undo entries hold
          ;; literally -- so the way back is a function, not a region.
          (when (and next (overlayp (emjupy-cell-overlay next)))
            (emjupy--goto-cell next 'start)))))))

(defun emjupy-cycle-cell-type ()
  "Cycle the cell at point between `code' and `markdown'."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--as-one-undo-step
      (emjupy--sync-all-cells)
      (let ((cell (emjupy--cell-at-point)))
        (unless cell
          (user-error "No cell found at point"))
        (setf (emjupy-cell-type cell)
              (if (eq (emjupy-cell-type cell) 'code) 'markdown 'code))
        (emjupy--rerender-notebook cell)))))

(defvar emjupy--cell-clipboard nil
  "The cell most recently copied, as a (TYPE . SOURCE) pair.
The output is deliberately not copied: it belongs to the run that
produced it, and carrying it to a copy would attribute results to code
that never generated them.")

(defun emjupy-copy-cell ()
  "Copy the cell at point.  Its output is not copied.

The source also goes to the kill ring, so it can be yanked as ordinary
text anywhere else."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (emjupy--cell-at-point)))
    (unless cell (user-error "Point is not in a cell"))
    (let ((source (or (emjupy-cell-source cell) "")))
      (setq emjupy--cell-clipboard (cons (emjupy-cell-type cell) source))
      (kill-new source)
      (message "[emjupy] Copied %s cell (%d chars); output not copied."
               (emjupy-cell-type cell) (length source))
      emjupy--cell-clipboard)))

(defun emjupy-yank-cell ()
  "Insert the most recently copied cell below the cell at point.

The new cell has no output and no execution count."
  (interactive)
  (emjupy--as-one-undo-step
    (unless emjupy--cell-clipboard
      (user-error "No cell has been copied yet"))
    (emjupy--sync-all-cells)
    (let* ((nb (emjupy--notebook))
           (cell (emjupy--cell-at-point))
           (cells (append (emjupy-notebook-cells nb) nil))
           (idx (and cell (cl-position cell cells)))
           (new-cell (make-emjupy-cell
                      :id (emjupy--new-cell-id)
                      :type (car emjupy--cell-clipboard)
                      :source (cdr emjupy--cell-clipboard)
                      :outputs []
                      :metadata (make-hash-table :test 'equal))))
      ;; Inserting one cell moves only what follows it, by a known amount,
      ;; so the history survives -- the same path `emjupy-insert-cell-below'
      ;; takes.  Rebuilding for it lost the history.
      (let ((at (if idx (1+ idx) (length cells))))
        (unless (emjupy-insert-cell-at nb at new-cell)
          (setf (emjupy-notebook-cells nb)
                (vconcat (append (cl-subseq cells 0 at)
                                 (list new-cell)
                                 (cl-subseq cells at))))
          (emjupy--rerender-notebook new-cell)))
      new-cell)))

;;;; Indentation

(defvar-local emjupy--indent-cycling nil
  "Non-nil when the last command was also an indent, so TAB cycles.")

(defvar python-indent-guess-indent-offset-verbose)

(defun emjupy-indent-or-cycle ()
  "Indent the current line the way `python-mode' would, cycling on repeat.

Python indentation is a guess -- after `if x:' the next line could be a
body, a continuation, or back at the outer level -- so `python-mode'
offers the alternatives in turn when TAB is pressed again.  Reproducing
that here means asking `python-mode' itself: the cell's source is put in a
real Python buffer, indented there, and the answer copied back.  A cell
is not a file, so the line's context is the cell, which is what a
notebook user means by it.

In a markdown cell, and outside any cell, TAB simply inserts."
  (interactive)
  (let ((cell (emjupy--cell-at-point)))
    (if (not (and cell (eq (emjupy-cell-type cell) 'code)))
        (insert-tab)
      (emjupy--sync-all-cells)
      (let* ((ov (emjupy-cell-overlay cell))
             (start (overlay-start ov))
             (offset (- (point) start))
             (source (or (emjupy-cell-source cell) ""))
             ;; Read in the notebook buffer: it is buffer-local there, and
             ;; the temp buffer below would only ever see the default.
             (cycling emjupy--indent-cycling)
             (result
              (with-temp-buffer
                (let ((python-indent-guess-indent-offset-verbose nil))
                  (delay-mode-hooks (python-mode)))
                (insert source)
                (goto-char (min (point-max) (+ (point-min) offset)))
                ;; python-mode cycles only when it believes TAB was pressed
                ;; again, which it reads from `last-command'.
                (let ((this-command 'indent-for-tab-command)
                      (last-command (if cycling
                                        'indent-for-tab-command
                                      last-command)))
                  (let ((tab-always-indent t))
                    (indent-for-tab-command)))
                (cons (buffer-substring-no-properties (point-min) (point-max))
                      (- (point) (point-min))))))
        (unless (equal (car result) source)
          (let ((inhibit-read-only t))
            (setf (emjupy-cell-source cell) (car result))
            (emjupy--rerender-notebook cell)))
        (let ((ov (emjupy-cell-overlay cell)))
          (when (overlayp ov)
            (goto-char (min (overlay-end ov) (+ (overlay-start ov)
                                                (cdr result))))))
        (setq emjupy--indent-cycling t)))))

(defun emjupy--clear-indent-cycling ()
  "Forget that the previous command indented, unless it did."
  (unless (eq this-command 'emjupy-indent-or-cycle)
    (setq emjupy--indent-cycling nil)))

;;;; Moving

(defun emjupy-beginning-of-cell ()
  "Move point to the start of the cell at point."
  (interactive)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell))))
    (unless (overlayp ov)
      (user-error "Point is not in a cell"))
    (goto-char (overlay-start ov))))

(defun emjupy-end-of-cell ()
  "Move point to the end of the cell's source at point.

The end of the source, not the end of the overlay: the overlay takes in
the newline that closes the cell, and landing after it puts point on the
next line, outside the code."
  (interactive)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell))))
    (unless (overlayp ov)
      (user-error "Point is not in a cell"))
    (goto-char (max (overlay-start ov)
                    (+ (overlay-start ov)
                       (length (or (emjupy-cell-source cell) "")))))))

(defun emjupy--sibling-cell (direction)
  "Return the cell DIRECTION (-1 or 1) away from the one at point, or nil."
  (let* ((nb (emjupy--notebook))
         (cells (append (emjupy-notebook-cells nb) nil))
         (cell (emjupy--cell-at-point))
         (idx (and cell (cl-position cell cells))))
    (when idx
      (let ((n (+ idx direction)))
        (when (and (>= n 0) (< n (length cells)))
          (nth n cells))))))

(defun emjupy--goto-cell (cell where)
  "Put point at the start or end of CELL's source, per WHERE."
  (let ((ov (and cell (emjupy-cell-overlay cell))))
    (unless (overlayp ov)
      (user-error "No cell there"))
    (goto-char (if (eq where 'beginning)
                   (overlay-start ov)
                 ;; The end of the SOURCE, not of the overlay: the overlay
                 ;; takes in the newline that closes the cell, so landing
                 ;; after it would put point on the next line, outside.
                 (min (overlay-end ov)
                      (+ (overlay-start ov)
                         (length (or (emjupy-cell-source cell) ""))))))))

(defun emjupy-beginning-of-previous-cell ()
  "Move point to the start of the previous cell."
  (interactive)
  (emjupy--goto-cell (emjupy--sibling-cell -1) 'beginning))

(defun emjupy-end-of-previous-cell ()
  "Move point to the end of the previous cell's source."
  (interactive)
  (emjupy--goto-cell (emjupy--sibling-cell -1) 'end))

(defun emjupy-beginning-of-next-cell ()
  "Move point to the start of the next cell."
  (interactive)
  (emjupy--goto-cell (emjupy--sibling-cell 1) 'beginning))

(defun emjupy-end-of-next-cell ()
  "Move point to the end of the next cell's source."
  (interactive)
  (emjupy--goto-cell (emjupy--sibling-cell 1) 'end))

(defun emjupy-next-cell ()
  "Move point to the next cell."
  (interactive)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell))))
    (if ov
        (let ((pos (overlay-end ov)))
          (when (< pos (point-max))
            (goto-char (1+ pos))))
      (goto-char (point-min)))))

(defun emjupy-previous-cell ()
  "Move point to the previous cell."
  (interactive)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell))))
    (if ov
        (let ((pos (overlay-start ov)))
          (when (> pos (point-min))
            (let ((prev-cell (get-text-property (1- pos) 'emjupy-cell)))
              (when (and prev-cell (emjupy-cell-overlay prev-cell))
                (goto-char (overlay-start (emjupy-cell-overlay prev-cell)))))))
      (goto-char (point-min)))))

;;;; Appearance

(defun emjupy--render-markdown-cell (cell)
  "Re-render CELL, which is markdown, and its math.

What running a markdown cell means in Jupyter: there is no code in it,
so the result is the rendered form rather than anything from a kernel."
  (emjupy--sync-all-cells)
  ;; Running a markdown cell means "show it formatted": its preview is turned on
  ;; if it was off.
  (unless emjupy-render-latex
    (setq-local emjupy-render-latex t))
  (emjupy--rerender-notebook cell)
  (let ((ov (emjupy-cell-overlay cell)))
    (when (overlayp ov)
      (emjupy--preview-latex-in (overlay-start ov) (overlay-end ov))))
  (message "[emjupy] Rendered markdown cell.")
  cell)

(defun emjupy-refresh-appearance ()
  "Re-derive page colours from the theme and redraw open notebooks.
Useful after changing `emjupy-output-color' or loading a theme in an
Emacs too old for `enable-theme-functions'."
  (interactive)
  (emjupy--sync-theme-colors)
  (dolist (buf (emjupy--notebook-buffers))
    (with-current-buffer buf
      (emjupy--rerender-notebook))))

(defun emjupy-toggle-latex-preview ()
  "Turn LaTeX previews in markdown cells on or off, and redraw."
  (interactive)
  (setq emjupy-render-latex (not emjupy-render-latex))
  (when (and emjupy-render-latex (not (emjupy--latex-available-p)))
    (message "[emjupy] No LaTeX renderer found: install latex and dvipng, or math-preview."))
  (when emjupy--buffer-notebook (emjupy--rerender-notebook))
  (message "[emjupy] LaTeX preview %s." (if emjupy-render-latex "on" "off")))

(defun emjupy-report-box-geometry ()
  "Report the widths the cell outlines are drawn at.

The rule above an output box is an overlay string, not buffer text, so
point cannot be placed in it and its columns cannot be inspected with
\\[describe-char].  This reports them instead: the width the rules are
drawn at, the width of the rule actually in place, and the column the
output background is aligned to.  If the last exceeds the others, the
background is wider than the box."
  (interactive)
  (let* ((cell (emjupy--cell-at-point))
         (width (emjupy--box-width))
         (ov (and cell (emjupy-cell-output-ov cell)))
         (rule (and (overlayp ov) (emjupy--overlay-header ov)))
         (rule-width (and rule (length (string-trim-right rule "\n"))))
         (pad (and (overlayp ov)
                   (save-excursion
                     (goto-char (overlay-start ov))
                     (let ((found nil))
                       (while (and (not found) (< (point) (overlay-end ov)))
                         (let ((d (get-text-property (point) 'display)))
                           (when (and (consp d) (eq (car d) 'space))
                             (setq found (plist-get (cdr d) :align-to))))
                         (forward-char 1))
                       found)))))
    (message "[emjupy] box width %s | rule %s | output aligned to %s%s"
             width (or rule-width "none") (or pad "none")
             (if (and rule-width pad (> pad rule-width))
                 (format "  -- background is %d column(s) wider" (- pad
                                                                    rule-width))
               ""))))

;;;###autoload

;;;; Tracebacks and hidden output

(defun emjupy-show-traceback ()
  "Show the full traceback of the cell at point in a buffer of its own.

The output box shows the traceback squeezed into the notebook, where a
deep stack is unreadable.  This puts it somewhere with room, with the
ANSI colouring the kernel sent and Python syntax highlighting, so the
frames and the offending lines are legible."
  (interactive)
  (let ((cell (emjupy--cell-at-point)))
    (unless cell (user-error "Point is not in a cell"))
    (let ((tb (emjupy--cell-traceback cell)))
      (unless tb (user-error "This cell has no traceback"))
      (let ((buf (get-buffer-create "*emjupy traceback*")))
        (with-current-buffer buf
          (let* ((inhibit-read-only t)
                 (coloured (emjupy--ansi-render tb)))
            (erase-buffer)
            (insert coloured)
            (let ((python-indent-guess-indent-offset-verbose nil))
              (delay-mode-hooks (python-mode)))
            ;; Highlighting first, then the kernel's colours on top: font-lock
            ;; rewrites `face' wholesale, and the colours mark the failing
            ;; frame.
            (font-lock-ensure)
            (let ((pos 0) (len (length coloured)))
              (while (< pos len)
                (let ((next (or (next-single-property-change
                                 pos 'face coloured) len))
                      (val (get-text-property pos 'face coloured)))
                  (when val
                    (font-lock-prepend-text-property
                     (+ (point-min) pos) (+ (point-min) next) 'face val))
                  (setq pos next))))
            (goto-char (point-min)))
          (setq buffer-read-only t)
          (local-set-key (kbd "q") #'quit-window))
        (pop-to-buffer buf)
        buf))))

(defun emjupy--toggle-output-from-string (pair buffer)
  "Toggle the cell named by PAIR, a (STRING . INDEX) pair, within BUFFER.

Separated from the mouse event so it can be tested: the accessors that
take a click apart are inlined by the compiler and cannot be stubbed."
  (let ((id (and (consp pair)
                 (get-text-property (cdr pair) 'emjupy-toggle-cell
                                    (car pair)))))
    (when (and id (buffer-live-p buffer))
      (with-current-buffer buffer
        (emjupy-toggle-output-of-cell (emjupy--cell-by-id id))))))

(defun emjupy-toggle-output-at-click (event)
  "Show the output whose collapsed marker was clicked.
EVENT is the mouse event."
  (interactive "e")
  ;; Located with `posn-string', the string and the index in it: a buffer
  ;; position at a cell's end is ambiguous between two cells.
  (let ((posn (event-start event)))
    (emjupy--toggle-output-from-string
     (posn-string posn)
     (window-buffer (posn-window posn)))))

;; Bound here, beside the command, rather than in the drawing layer that
;; makes the map: the click toggles a cell's output, which is this layer's.
(define-key emjupy-hidden-output-map [mouse-1] #'emjupy-toggle-output-at-click)

(provide 'emjupy-cells)
;;; emjupy-cells.el ends here
