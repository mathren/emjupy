;;; emjupy-commands.el --- Commands on cells: editing, moving, output  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The commands a notebook offers on its cells: inserting, deleting,
;; splitting and joining, copying and yanking, moving, indenting, changing
;; a cell's type, and showing, hiding and clearing output.  Each is one undo
;; step, redrawing what it changed.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-undo)
(require 'emjupy-faces)
(require 'emjupy-latex)
(require 'emjupy-cells)
(require 'emjupy-redraw)

(defvar python-indent-guess-indent-offset-verbose)


;;;; Inserting and deleting

(defun emjupy-insert-cell-at (nb index new-cell)
  "Put NEW-CELL into NB at INDEX and draw it without rebuilding the buffer.
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
          (setq inserted (emjupy--draw-cell-at new-cell at))
          ;; Text inserted at an overlay's start may go into it, depending on
          ;; the Emacs version, and the next cell swallow the new one: so the
          ;; overlays are checked for overlap, and repaired.
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
            ;; Again, now the next cell starts where it should: else the gap
            ;; stayed writable, and a backspace there joined the cells.  Not
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

;;;; Cell commands

(defun emjupy--insert-empty-cell (where)
  "Insert an empty code cell next to the cell at point.
WHERE is `above' or `below'."
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((nb emjupy--buffer-notebook)
           (cells (append (emjupy-notebook-cells nb) nil))
           (idx (cl-position (emjupy--cell-at-point) cells))
           (index (cond ((null idx) (if (eq where 'below) (length cells) 0))
                        ((eq where 'below) (1+ idx))
                        (t idx)))
           (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                       :source "" :outputs []
                                       :metadata (make-hash-table))))
      (unless (emjupy-insert-cell-at nb index new-cell)
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 index)
                               (list new-cell)
                               (cl-subseq cells index))))
        (emjupy--redraw-notebook new-cell))
      (emjupy--goto-cell new-cell 'beginning))))

(defun emjupy-insert-cell-below ()
  "Insert a new empty code cell below the cell at point."
  (interactive)
  (emjupy--insert-empty-cell 'below))

(defun emjupy-insert-cell-above ()
  "Insert a new empty code cell above the cell at point."
  (interactive)
  (emjupy--insert-empty-cell 'above))

(defun emjupy-move-cell-up ()
  "Move the cell at point up, swapping it with the cell above.
Since a cell's output lives inside its own struct rather than as a
separate entity, the output always travels with its cell automatically."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
           (cell (emjupy--cell-at-point))
           ;; where point is in it, kept: moving it is not moving in it
           (offset (if cell (emjupy--cell-offset cell) 0))
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
            (emjupy--redraw-notebook cell))
          ;; The move is its own undo step, so an edit in the cell it
          ;; rewrites is recovered by undoing the move rather than lost.
          (emjupy--goto-cell-offset cell offset))))))

(defun emjupy-move-cell-down ()
  "Move the cell at point down, swapping it with the cell below."
  (interactive)
  (emjupy--as-one-undo-step
    (emjupy--sync-all-cells)
    (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
           (cell (emjupy--cell-at-point))
           ;; where point is in it, kept: moving it is not moving in it
           (offset (if cell (emjupy--cell-offset cell) 0))
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
            (emjupy--redraw-notebook cell))
          (emjupy--goto-cell-offset cell offset))))))

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
          (emjupy--redraw-notebook new-cell))
        (let ((ov (emjupy-cell-overlay new-cell)))
          (when (overlayp ov) (goto-char (overlay-start ov))))
        new-cell))))

(defun emjupy-join-cell-above ()
  "Merge the cell at point into the one above it.

The two sources are joined with a newline between them, the upper cell
absorbs the lower, and point lands at the seam, where the second cell
began.

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
                (emjupy--redraw-notebook above))
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
    ;; Hidden for its size, it is shown: the notebook's mark is untouched.
    (if (emjupy-cell-size-hidden cell)
        (setf (emjupy-cell-size-hidden cell) nil)
      (emjupy--set-cell-outputs-hidden cell hidden))
    ;; Only this cell changes, so only this cell is redrawn -- which also
    ;; keeps the undo history.
    (unless (emjupy--redraw-cells-in-place (list cell) (list cell))
      (emjupy--redraw-notebook cell))
    (message "[emjupy] Output %s." (if hidden "hidden" "shown"))
    hidden))

(defun emjupy--confirm-clearing (cell)
  "Return non-nil if CELL's output may be discarded.
The prompt names the outputs and the execution count that made them."
  (or (not emjupy-confirm-clear-output)
      (let* ((n (length (or (emjupy-cell-outputs cell) [])))
             (count (emjupy-cell-exec-count cell)))
        (y-or-n-p (format "Clear %d output%s%s? "
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
            (unless (emjupy--redraw-cell-output cell)
              (emjupy--redraw-notebook cell))
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
         ((not (y-or-n-p (format "Clear the output of %d cell%s? "
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
                                (and (emjupy--redraw-cell-header cell)
                                     (emjupy--redraw-cell-output cell)))
                              cleared)
              (emjupy--redraw-notebook)))
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
               ;; last one if it was at the end: point at the notebook's end
               ;; would lose your place.
               (next (and remaining
                          (nth (min idx (1- (length remaining))) remaining))))
          ;; Undoing the delete puts this exact text back, so edits
          ;; recorded inside it are kept for then.
          (unless (emjupy-delete-cell-at nb curr-cell)
            (setf (emjupy-notebook-cells nb) (vconcat remaining))
            (emjupy--redraw-notebook next))
          ;; An entry undo can act on.  The text deletion itself is made with
          ;; recording off -- a redraw moves positions that undo entries hold
          ;; literally -- so the way back is a function, not a region.
          (when (and next (overlayp (emjupy-cell-overlay next)))
            (emjupy--goto-cell next 'beginning)))))))

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
        (emjupy--redraw-notebook cell)))))

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
          (emjupy--redraw-notebook new-cell)))
      new-cell)))

;;;; Indentation

(defvar-local emjupy--indent-cycling nil
  "Non-nil when the last command was also an indent, so TAB cycles.")

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
            (emjupy--redraw-notebook cell)))
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

(defun emjupy--cell-offset (cell)
  "Point's offset into CELL's source, or 0 if point is not in it."
  (let ((ov (emjupy-cell-overlay cell)))
    (if (and (overlayp ov) (<= (overlay-start ov) (point) (overlay-end ov)))
        (- (point) (overlay-start ov))
      0)))

(defun emjupy--goto-cell-offset (cell offset)
  "Put point OFFSET characters into CELL's source, at most at its end."
  (emjupy--goto-cell cell 'beginning)
  (goto-char (min (+ (point) offset)
                  (save-excursion (emjupy--goto-cell cell 'end) (point)))))

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

(defun emjupy--redraw-markdown-cell (cell)
  "Redraw markdown CELL, its LaTeX included.

What running a markdown cell means in Jupyter: there is no code in it,
so the result is the rendered form rather than anything from a kernel."
  (emjupy--sync-all-cells)
  ;; Running a markdown cell means "show it formatted": its preview is turned on
  ;; if it was off.
  (unless emjupy-render-latex
    (setq-local emjupy-render-latex t))
  (emjupy--redraw-notebook cell)
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
      (emjupy--redraw-notebook))))

(defun emjupy-toggle-latex-preview ()
  "Turn LaTeX previews in this notebook's markdown cells on or off.
Only this notebook: `emjupy-render-latex' sets it for all of them."
  (interactive)
  (setq-local emjupy-render-latex (not emjupy-render-latex))
  (when (and emjupy-render-latex (not (emjupy--latex-available-p)))
    (message "[emjupy] No LaTeX renderer found: install latex and dvipng, or math-preview."))
  (when emjupy--buffer-notebook (emjupy--redraw-notebook))
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
                 (coloured (emjupy--render-ansi tb)))
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

;;;; In a notebook buffer

(defun emjupy-commands-enable ()
  "Reset indentation cycling before each command in the current notebook."
  (add-hook 'pre-command-hook #'emjupy--clear-indent-cycling nil t))

(provide 'emjupy-commands)

;;; emjupy-commands.el ends here
