;;; emjupy-cells.el --- Cell editing operations for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Syncing buffer text back into cell structs, and the cell-level editing
;; commands: insert, delete, move, change type, and navigation.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)

(defun emjupy--sync-cell-source-from-buffer (cell)
  "Read the live buffer text bounded by CELL's overlay into `emjupy-cell-source'."
  (when-let ((ov (emjupy-cell-overlay cell)))
    (when (overlay-buffer ov)
      (let ((text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
        (setf (emjupy-cell-source cell) (string-trim-right text "\n"))))))

(defun emjupy--sync-all-cells ()
  "Sync buffer text for all cells in current buffer."
  (when emjupy--buffer-notebook
    (cl-loop for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
             do (emjupy--sync-cell-source-from-buffer cell))))

(defun emjupy--rerender-notebook (&optional target-cell)
  "Re-render every cell overlay in the current buffer.
If TARGET-CELL is given, leave point at that cell afterwards.

Rendering is kept OUT of the undo history.  It erases the whole buffer
and rebuilds it from the cell structs, which as a recorded change is
both enormous and meaningless to undo -- a single re-render used to push
~20 entries onto the list, and undoing one of them tore the notebook
apart, deleting every cell after the one being edited.

Because the rebuild is invisible to undo, any entries recorded BEFORE it
now refer to positions in a buffer that no longer exists, so they are
discarded when the rebuild actually changed the text.  Undo therefore
stops at the last render rather than corrupting the buffer.  When the
text comes out identical -- a re-render triggered by something that
changed nothing -- the history is left intact."
  (when emjupy--buffer-notebook
    (let ((inhibit-read-only t)
          (cells (emjupy-notebook-cells emjupy--buffer-notebook))
          (target-start nil)
          (before (buffer-substring-no-properties (point-min) (point-max))))
      (save-restriction
        (widen)
        (let ((buffer-undo-list t))
          ;; Delete old overlays
          (cl-loop for cell across cells
                   do (when (emjupy-cell-overlay cell)
                        (delete-overlay (emjupy-cell-overlay cell))
                        (setf (emjupy-cell-overlay cell) nil))
                   (when (emjupy-cell-output-ov cell)
                     (delete-overlay (emjupy-cell-output-ov cell))
                     (setf (emjupy-cell-output-ov cell) nil)))
          (erase-buffer)
          ;; Render every cell first; only move point afterward. Jumping point
          ;; back to target-cell mid-loop would make later `insert' calls land
          ;; inside target-cell's own overlay (which then grows to swallow
          ;; them), shoving it -- and its output -- to the end of the buffer.
          (cl-loop for cell across cells
                   do (let ((start (point)))
                        (emjupy--render-cell cell)
                        (when (eq cell target-cell)
                          (setq target-start start))))
          (when target-start
            (goto-char target-start))))
      (unless (equal before (buffer-substring-no-properties (point-min) (point-max)))
        (setq buffer-undo-list nil)))))

(defun emjupy-insert-cell-below ()
  "Insert a new empty code cell below the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (emjupy--cell-at-point))
         (cells (append (emjupy-notebook-cells nb) nil))
         (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (if idx
        (setq cells (append (cl-subseq cells 0 (1+ idx))
                            (list new-cell)
                            (cl-subseq cells (1+ idx))))
      (setq cells (append cells (list new-cell))))
    (setf (emjupy-notebook-cells nb) (vconcat cells))
    (emjupy--rerender-notebook new-cell)))

(defun emjupy-insert-cell-above ()
  "Insert a new empty code cell above the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (emjupy--cell-at-point))
         (cells (append (emjupy-notebook-cells nb) nil))
         (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (if idx
        (setq cells (append (cl-subseq cells 0 idx)
                            (list new-cell)
                            (cl-subseq cells idx)))
      (setq cells (cons new-cell cells)))
    (setf (emjupy-notebook-cells nb) (vconcat cells))
    (emjupy--rerender-notebook new-cell)))

(defun emjupy-move-cell-up ()
  "Move the cell at point up, swapping it with the cell above.
Since a cell's output lives inside its own struct rather than as a
separate entity, the output always travels with its cell automatically."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
         (cell (emjupy--cell-at-point))
         (idx (cl-position cell cells)))
    (if (or (not idx) (= idx 0))
        (message "[emjupy] Cell is already at the top.")
      (let ((above (aref cells (1- idx))))
        (aset cells (1- idx) cell)
        (aset cells idx above))
      (emjupy--rerender-notebook cell))))

(defun emjupy-move-cell-down ()
  "Move the cell at point down, swapping it with the cell below."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
         (cell (emjupy--cell-at-point))
         (idx (cl-position cell cells)))
    (if (or (not idx) (= idx (1- (length cells))))
        (message "[emjupy] Cell is already at the bottom.")
      (let ((below (aref cells (1+ idx))))
        (aset cells (1+ idx) cell)
        (aset cells idx below))
      (emjupy--rerender-notebook cell))))

(defun emjupy--rerender-preserving-point ()
  "Re-render the notebook without disturbing where the user is typing.

`emjupy--rerender-notebook\' erases the buffer and rebuilds it, so point
has to be put back deliberately.  Passing the cell as its TARGET-CELL
argument is not that: it moves point to the START of that cell.  Doing
so from the WebSocket handler meant every arriving line of output yanked
the cursor to the top of the executing cell -- so the cell you had just
run appeared to steal point back, and output landing while you edited
elsewhere threw you across the buffer mid-keystroke."
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell)))
         (offset (and ov (- (point) (overlay-start ov)))))
    (emjupy--rerender-notebook)
    (when-let* ((cell cell)
                (ov (emjupy-cell-overlay cell)))
      (when (overlayp ov)
        (goto-char (min (overlay-end ov)
                        (+ (overlay-start ov) (or offset 0))))))))

(defun emjupy--cell-at-point (&optional pos)
  "Return the cell containing POS (default point), or nil.

Looks at the cell overlays rather than the `emjupy-cell\' text property.
The property is stamped once at render time and plain `insert\' does not
carry it onto new text, so anything typed at a cell boundary -- the end
of its last line, the obvious place to type -- was invisible to a lookup
by property.  Overlays move with insertions, so they always know."
  (let ((pos (or pos (point)))
        (found nil))
    (when emjupy--buffer-notebook
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               until found
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (and (overlayp ov)
                               (>= pos (overlay-start ov))
                               (<= pos (overlay-end ov)))
                      (setq found cell)))))
    (or found (get-text-property pos 'emjupy-cell))))

(defun emjupy-split-cell ()
  "Split the cell at point in two, at point.

Text before point stays in this cell; text from point on moves to a new
cell of the same type just below, and point follows it there.

The output of BOTH halves is discarded, along with the execution count.
Neither half has been run as it now stands, so keeping the results would
attribute them to source that never produced them."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb (emjupy--notebook))
         (cell (emjupy--cell-at-point)))
    ;; Output text carries no cell property and lies outside the source
    ;; overlay, so this also catches point sitting in an output box.
    (unless cell
      (user-error "Point is not in a cell"))
    (let* ((ov (emjupy-cell-overlay cell))
           (source (or (emjupy-cell-source cell) ""))
           (offset (max 0 (min (- (point) (overlay-start ov)) (length source))))
           (new-cell (make-emjupy-cell
                      :id (emjupy--new-cell-id)
                      :type (emjupy-cell-type cell)
                      :source (substring source offset)
                      :outputs []
                      :metadata (make-hash-table :test 'equal)))
           (cells (append (emjupy-notebook-cells nb) nil))
           (idx (cl-position cell cells)))
      (setf (emjupy-cell-source cell) (substring source 0 offset))
      ;; Neither half produced what is on screen any more.
      (setf (emjupy-cell-outputs cell) [])
      (setf (emjupy-cell-exec-count cell) nil)
      (setf (emjupy-notebook-cells nb)
            (vconcat (append (cl-subseq cells 0 (1+ idx))
                             (list new-cell)
                             (cl-subseq cells (1+ idx)))))
      (emjupy--rerender-notebook new-cell)
      new-cell)))

(defun emjupy-merge-cell-above ()
  "Merge the cell at point into the one above it.

The two sources are joined with a newline between them, the upper cell
absorbs the lower, and point lands at the seam -- where the second cell
used to begin.

The output of BOTH cells is discarded, along with their execution
counts.  Neither set was produced by the merged code, and keeping one
would attribute results to source that never generated them.

The cells must be of the same type; merging code into prose, or the
reverse, would silently reinterpret one of them."
  (interactive)
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
        (let* ((upper (or (emjupy-cell-source above) ""))
               (lower (or (emjupy-cell-source cell) ""))
               (seam (length (if (string-suffix-p "\n" upper)
                                 upper
                               (concat upper "\n")))))
          (setf (emjupy-cell-source above)
                (concat (if (string-suffix-p "\n" upper) upper (concat upper "\n"))
                        lower))
          ;; Neither cell's results describe the merged source any more.
          (setf (emjupy-cell-outputs above) [])
          (setf (emjupy-cell-exec-count above) nil)
          (setf (emjupy-notebook-cells nb)
                (vconcat (append (cl-subseq cells 0 idx)
                                 (cl-subseq cells (1+ idx)))))
          (emjupy--rerender-notebook above)
          ;; Leave point at the seam, where the merged-in cell begins.
          (let ((ov (emjupy-cell-overlay above)))
            (when (overlayp ov)
              (goto-char (min (overlay-end ov) (+ (overlay-start ov) seam)))))
          above)))))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (emjupy--cell-at-point))
         (cells (append (emjupy-notebook-cells nb) nil)))
    (when curr-cell
      (setq cells (delete curr-cell cells))
      (setf (emjupy-notebook-cells nb) (vconcat cells))
      (emjupy--rerender-notebook))))

(defun emjupy-cycle-cell-type ()
  "Cycle the cell at point between `code' and `markdown'."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (emjupy--cell-at-point)))
    (unless cell
      (user-error "No cell found at point"))
    (setf (emjupy-cell-type cell)
          (if (eq (emjupy-cell-type cell) 'code) 'markdown 'code))
    (emjupy--rerender-notebook cell)))

(defvar emjupy--cell-clipboard nil
  "The cell most recently copied, as a (TYPE . SOURCE) pair.

Type and source only.  A cell's output belongs to the run that produced
it, so carrying it to a copy would attribute results to code that never
generated them.")

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

The new cell has no output and no execution count: it has not been run."
  (interactive)
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
    (setf (emjupy-notebook-cells nb)
          (vconcat (if idx
                       (append (cl-subseq cells 0 (1+ idx))
                               (list new-cell)
                               (cl-subseq cells (1+ idx)))
                     (append cells (list new-cell)))))
    (emjupy--rerender-notebook new-cell)
    new-cell))

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

(provide 'emjupy-cells)
;;; emjupy-cells.el ends here
