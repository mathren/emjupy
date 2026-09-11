;;; emjupy-cells.el --- Cell editing operations for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Assisted-by: Claude:claude-opus-5 and other free-tier LLMs
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

(defun emjupy--overlays-sane-p ()
  "Return non-nil if this buffer's cell overlays still describe the cells.

Every cell must have a live overlay inside the buffer, and the overlays
must run in cell order without overlapping.  Anything else means the
buffer and the structs have come apart -- a render interrupted partway,
or something outside emjupy having rewritten the text."
  (let ((cells (append (or (emjupy-notebook-cells emjupy--buffer-notebook) []) nil))
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

Refuses when the overlays no longer describe the cells.  Syncing reads
whatever an overlay spans straight into a cell\='s source, so one stale or
overlapping overlay is enough for a cell to swallow the whole notebook
-- and because the structs are what gets saved and re-rendered, that
corruption then survives every redraw.  Better to leave the structs
alone and say so: \[emjupy-re-render] rebuilds the display from them."
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

(defvar emjupy--rendering-buffers nil
  "Buffers currently being re-rendered.

Global rather than buffer-local on purpose: a re-entrant call arrives
while some OTHER buffer is current -- a WebSocket callback, a
fontification temp buffer -- and a buffer-local flag is invisible from
there.")

(defun emjupy--sweep-stray-overlays ()
  "Delete emjupy overlays that no live cell owns.

A cell overlay draws its rule through a `before-string\', which is shown
even when the overlay has collapsed to zero width -- so one left behind
by a cell that has gone appears as a stray box border, several of them
stacking up at the top of the buffer where `erase-buffer\' collapsed
them.

The render deletes the overlays it knows about, which is every overlay
reachable from the current cells.  This is the belt: anything tagged as
emjupy\='s that no current cell claims, or that has collapsed to nothing,
goes.  Cheap -- the list is a few dozen entries -- and it makes the
stray borders unrepresentable rather than merely unlikely, which
matters for a fault I have not been able to reproduce."
  (let ((claimed (make-hash-table :test 'eq)))
    (cl-loop for cell across (or (and emjupy--buffer-notebook
                                      (emjupy-notebook-cells emjupy--buffer-notebook))
                                 [])
             do (let ((ov (emjupy-cell-overlay cell))
                      (out (emjupy-cell-output-ov cell)))
                  (when (overlayp ov) (puthash ov t claimed))
                  (when (overlayp out) (puthash out t claimed))))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (and (overlay-get ov 'emjupy-overlay)
                 (or (not (gethash ov claimed))
                     (= (overlay-start ov) (overlay-end ov))))
        (delete-overlay ov)))))

(defun emjupy--rerender-notebook (&optional target-cell)
  "Re-render this notebook, leaving point at TARGET-CELL if given.

Does nothing if a render is already in progress.

Rendering erases the buffer and rebuilds it from the cell structs.
Anything that re-enters midway -- output arriving over the WebSocket
during a redraw -- restarts the rebuild on a half-built buffer and
leaves the notebook duplicated on top of itself.  The next sync then
copies that whole mess into the first cell\='s source, which is why
redrawing does not clear it: by then the junk IS the notebook.

The re-entrant call is dropped rather than queued.  It has nothing to
add: the render already running rebuilds from the same structs, which
the caller has by then already updated."
  (unless (memq (current-buffer) emjupy--rendering-buffers)
    (let ((emjupy--rendering-buffers (cons (current-buffer) emjupy--rendering-buffers)))
      (emjupy--rerender-notebook-1 target-cell))))

(defcustom emjupy-protect-non-cell-regions t
  "When non-nil, make everything outside a cell\='s source read-only.

The rules, the gutters between cells and the output boxes belong to no
cell.  Text typed there is stored nowhere and disappears at the next
redraw, so it is better refused than silently lost."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--protect-non-cell-regions ()
  "Mark every region that is not a cell\='s source read-only."
  (when emjupy-protect-non-cell-regions
    (let ((inhibit-read-only t)
          (spans nil))
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer)))
                      (push (cons (overlay-start ov) (overlay-end ov)) spans))))
      (setq spans (sort spans #'car-less-than-car))
      (let ((pos (point-min)))
        (dolist (span spans)
          (when (< pos (car span))
            (emjupy--make-read-only pos (car span)))
          (setq pos (max pos (cdr span))))
        (when (< pos (point-max))
          (emjupy--make-read-only pos (point-max)))))))

(defun emjupy--make-read-only (start end)
  "Refuse edits between START and END, without walling off the cells.

`rear-nonsticky\' matters: without it, text inserted immediately AFTER a
protected region inherits the property, so typing at the very start of a
cell would be refused along with the gutter above it."
  (add-text-properties start end
                       '(read-only emjupy
                         front-sticky (read-only)
                         rear-nonsticky (read-only))))

(defun emjupy--rerender-notebook-1 (&optional target-cell)
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
          ;; Delete EVERY emjupy overlay in the buffer, not only those the
          ;; current cells point at.  An overlay whose cell has gone -- the
          ;; cell was deleted or merged, or the notebook was re-parsed and
          ;; got a fresh set of structs -- is unreachable from `cells', so it
          ;; was never deleted.  `erase-buffer' then collapses it to position
          ;; 1, where it goes on displaying its before-string and
          ;; after-string: a stack of stray "[In: 8]" and "[Out: 15]" rules
          ;; piling up at the top of the notebook, one per abandoned cell.
          (dolist (ov (overlays-in (point-min) (point-max)))
            (when (overlay-get ov 'emjupy-overlay)
              (delete-overlay ov)))
          (cl-loop for cell across cells
                   do (setf (emjupy-cell-overlay cell) nil)
                      (setf (emjupy-cell-output-ov cell) nil))
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
            (goto-char target-start))
          ;; Nothing may be left drawing a rule that belongs to no cell.
          (emjupy--sweep-stray-overlays)
          ;; Everything that is not a cell's source is now made read-only.
          ;; Those regions -- the rules, the gutters between cells, the
          ;; output boxes -- belong to no cell, so anything typed into them
          ;; is written nowhere and vanishes at the next redraw.  Worse, it
          ;; sits in the buffer looking like content until then.
          (emjupy--protect-non-cell-regions)))
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
  ;; Fold what is on screen back into the structs FIRST.  The re-render
  ;; rebuilds the buffer from the structs, so anything typed since the last
  ;; sync -- which, while a slow cell runs, is everything the user has done
  ;; -- would simply be overwritten and lost.  That is the "scrambling":
  ;; output arrives, and your edits silently revert.
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
    (or found
        (get-text-property pos 'emjupy-cell)
        ;; Past the end of the last cell -- point-max, or a gutter line --
        ;; no overlay covers the position and no property was stamped there.
        ;; Fall back to the nearest cell rather than nothing: returning nil
        ;; made `emjupy-insert-cell-below' think there was no current cell,
        ;; so it appended to the END of the notebook instead of inserting
        ;; below the one you were looking at.
        (emjupy--nearest-cell pos))))

(defun emjupy--nearest-cell (pos)
  "Return the cell whose overlay is closest to POS, or nil."
  (when emjupy--buffer-notebook
    (let ((best nil) (best-d most-positive-fixnum))
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (overlayp ov)
                      (let ((d (cond ((< pos (overlay-start ov)) (- (overlay-start ov) pos))
                                     ((> pos (overlay-end ov)) (- pos (overlay-end ov)))
                                     (t 0))))
                        (when (< d best-d) (setq best-d d best cell))))))
      best)))

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

(defun emjupy-clear-cell-output ()
  "Discard the output of the cell at point.

The output box goes with it, so the cell shrinks back to just its
source, and the execution count is cleared too: nothing has been run
since, so leaving [In: 4] beside no output would claim otherwise.

Only the buffer is touched.  The notebook on the server keeps its
outputs until you save."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (emjupy--cell-at-point)))
    (unless cell (user-error "Point is not in a cell"))
    (if (zerop (length (or (emjupy-cell-outputs cell) [])))
        (message "[emjupy] This cell has no output.")
      (setf (emjupy-cell-outputs cell) [])
      (setf (emjupy-cell-exec-count cell) nil)
      (emjupy--rerender-notebook cell)
      (message "[emjupy] Cleared this cell's output."))))

(defun emjupy-clear-all-outputs ()
  "Discard the output of every cell in the notebook.

Asks first: this throws away results that may have taken a while to
produce, and re-running them is not always cheap.

Only the buffer is touched.  The notebook on the server keeps its
outputs until you save."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cells (emjupy-notebook-cells (emjupy--notebook)))
         (with-output (cl-count-if (lambda (c)
                                     (> (length (or (emjupy-cell-outputs c) [])) 0))
                                   cells)))
    (cond
     ((zerop with-output)
      (message "[emjupy] No cell has any output."))
     ((not (yes-or-no-p (format "Clear the output of %d cell%s? "
                                with-output (if (= with-output 1) "" "s"))))
      (message "[emjupy] Left them alone."))
     (t
      (cl-loop for cell across cells
               do (setf (emjupy-cell-outputs cell) [])
                  (setf (emjupy-cell-exec-count cell) nil))
      (emjupy--rerender-notebook)
      (message "[emjupy] Cleared the output of %d cell%s."
               with-output (if (= with-output 1) "" "s"))))))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (emjupy--cell-at-point))
         (cells (append (emjupy-notebook-cells nb) nil)))
    (when curr-cell
      (let* ((idx (cl-position curr-cell cells))
             (rest (progn (setq cells (delete curr-cell cells)) cells))
             ;; The cell that slides up into the deleted one's place, or the
             ;; last one if it was at the end.  Leaving point at the end of
             ;; the notebook, as it used to, loses your place entirely.
             (next (and rest (nth (min idx (1- (length rest))) rest))))
        (setf (emjupy-notebook-cells nb) (vconcat cells))
        (emjupy--rerender-notebook next)))))

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

(defvar-local emjupy--indent-cycling nil
  "Non-nil when the last command was also an indent, so TAB cycles.")

(defvar python-indent-guess-indent-offset-verbose)

(defun emjupy-indent-or-cycle ()
  "Indent the current line the way `python-mode\=' would, cycling on repeat.

Python indentation is a guess -- after `if x:\=' the next line could be a
body, a continuation, or back at the outer level -- so `python-mode\='
offers the alternatives in turn when TAB is pressed again.  Reproducing
that here means asking `python-mode\=' itself: the cell\='s source is put in a
real Python buffer, indented there, and the answer copied back.  A cell
is not a file, so the line\='s context is the cell, which is what a
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
                  (ignore-errors (indent-for-tab-command)))
                (cons (buffer-substring-no-properties (point-min) (point-max))
                      (- (point) (point-min))))))
        (unless (equal (car result) source)
          (let ((inhibit-read-only t))
            (setf (emjupy-cell-source cell) (car result))
            (emjupy--rerender-notebook cell)))
        (let ((ov (emjupy-cell-overlay cell)))
          (when (overlayp ov)
            (goto-char (min (overlay-end ov) (+ (overlay-start ov) (cdr result))))))
        (setq emjupy--indent-cycling t)))))

(defun emjupy--clear-indent-cycling ()
  "Forget that the previous command indented, unless it did."
  (unless (eq this-command 'emjupy-indent-or-cycle)
    (setq emjupy--indent-cycling nil)))

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

(provide 'emjupy-cells)
;;; emjupy-cells.el ends here
