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
      (let* ((text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov)))
             (trimmed (string-trim-right text "\n")))
        ;; An empty cell is drawn with a newline so that it occupies a line
        ;; and can be typed into.  Trimming one newline leaves that line in
        ;; the source, so reading the buffer back turned an empty cell into
        ;; one holding a newline -- a change to the notebook that nobody
        ;; made, on the first sync after opening it.
        (setf (emjupy-cell-source cell)
              (if (string-empty-p (string-trim-right trimmed "[\n]+")) "" trimmed))))))

(defun emjupy--check-invariants ()
  "Return a list of ways this buffer and its cells disagree, or nil.

`emjupy--overlays-sane-p' answers whether anything is wrong;  this says
what, which is the difference between a test that fails and a test that
tells you why.  The invariants:

- every cell has a live overlay in this buffer;
- the overlays run in cell order and do not overlap;
- an output box, where there is one, begins where its source ends;
- nothing but the separating newline sits between one cell and the next;
- the buffer text of each cell matches that cell's source, so a sync
  would be a no-op.

The last is the one that matters most: when it fails, what the user sees
and what would be saved have come apart."
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
          (push (format "cell %d's overlay is in another buffer" index) problems))
         (t
          (when (> (overlay-start ov) (overlay-end ov))
            (push (format "cell %d's overlay is inside out" index) problems))
          (when (> (overlay-end ov) (point-max))
            (push (format "cell %d's overlay runs past the buffer" index) problems))
          (when (and prev-end (< (overlay-start ov) prev-end))
            (push (format "cell %d overlaps the cell before it" index) problems))
          ;; The text really there, against the text we would save.  Trailing
          ;; blank lines are not compared: an empty cell is drawn with a
          ;; newline so that it occupies a line and can be typed into, and
          ;; that line is display, not content.
          (let ((shown (string-trim-right
                        (buffer-substring-no-properties
                         (overlay-start ov) (overlay-end ov))
                        "[\n]+"))
                (stored (string-trim-right (or (emjupy-cell-source cell) "")
                                           "[\n]+")))
            (unless (equal shown stored)
              (push (format "cell %d shows %S but holds %S" index shown stored)
                    problems)))
          (when (overlayp out)
            (if (not (eq (overlay-buffer out) (current-buffer)))
                (push (format "cell %d's output box is in another buffer" index)
                      problems)
              (unless (= (overlay-start out) (overlay-end ov))
                (push (format "cell %d's output box does not follow its source" index)
                      problems))))
          (setq prev-end (if (overlayp out)
                             (max (overlay-end ov) (overlay-end out))
                           (overlay-end ov)))))))
    (nreverse problems)))

;;;###autoload
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

(defun emjupy--cell-region (cell)
  "Return (START . END) of everything CELL occupies, or nil.

A cell owns its source and, when it has one, its output box; the two are
contiguous.  END is the position just past the gutter newline that
follows, since that newline belongs to this cell rather than the next."
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

The general form of what inserting and deleting do one cell at a time.
A split is one region becoming two cells, a merge is two regions
becoming one; in both the change is bounded by the cells involved, so
everything outside moves by a known amount and the undo history can be
kept.

OLD-CELLS must be adjacent and in buffer order.  KNOWN-SANE skips the
overlay check, for callers that have already added a cell which has no
overlay yet.  Returns non-nil when it was done, nil when the caller
should fall back to a full redraw."
  (let ((regions (delq nil (mapcar #'emjupy--cell-region old-cells))))
    (when (and regions
               (= (length regions) (length old-cells))
               ;; KNOWN-SANE is for callers that have already put a new cell
               ;; into the notebook: it has no overlay yet, so the check
               ;; would fail on the very cell about to be drawn.  They test
               ;; before mutating instead.
               (or known-sane (emjupy--overlays-sane-p)))
      (let ((start (apply #'min (mapcar #'car regions)))
            (end (apply #'max (mapcar #'cdr regions)))
            (new-end nil)
            (following-after nil))
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
          (dolist (cell old-cells)
            (when (overlayp (emjupy-cell-overlay cell))
              (delete-overlay (emjupy-cell-overlay cell)))
            (when (overlayp (emjupy-cell-output-ov cell))
              (delete-overlay (emjupy-cell-output-ov cell)))
            (setf (emjupy-cell-overlay cell) nil)
            (setf (emjupy-cell-output-ov cell) nil))
          ;; The cell that follows may start exactly where this region ends,
          ;; in which case the text about to be inserted is taken into its
          ;; overlay and the two come to share a region -- the same fault
          ;; that inserting a cell had.  Its bounds are noted here and
          ;; restored afterwards.
          (setq following-after
                (let ((cells (and emjupy--buffer-notebook
                                  (append (emjupy-notebook-cells emjupy--buffer-notebook)
                                          nil))))
                  (cl-find-if (lambda (cell)
                                (let ((ov (emjupy-cell-overlay cell)))
                                  (and (overlayp ov)
                                       (eq (overlay-buffer ov) (current-buffer))
                                       (>= (overlay-start ov) end))))
                              cells)))
          (delete-region start end)
          (save-excursion
            (goto-char start)
            (dolist (cell new-cells)
              (emjupy--render-cell cell))
            (setq new-end (point)))
          ;; Put it back where it belongs if it swallowed the new text.
          (let ((ov (and following-after (emjupy-cell-overlay following-after))))
            (when (and (overlayp ov)
                       (eq (overlay-buffer ov) (current-buffer))
                       (< (overlay-start ov) new-end))
              (move-overlay ov new-end (max new-end (overlay-end ov)))))
          (emjupy--protect-non-cell-regions))
        (emjupy--undo-adjust start end (- new-end end))
        t))))

(defun emjupy-insert-cell-at (nb index new-cell)
  "Put NEW-CELL into NB at INDEX and draw it without rebuilding the buffer.

A full rebuild costs the undo history, because it moves every position
in the buffer while undo recording is off and undo entries hold plain
positions.  Inserting one cell moves only what follows it, and by a
known amount, so the history can be kept: entries before the insertion
are untouched, and those after it shift.  See notes_undo.org.

Returns non-nil when it was done incrementally, nil when the caller
should fall back to a full redraw."
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
        ;; Text inserted at an overlay's start may be taken INTO that
        ;; overlay, depending on how the overlay was made and on the Emacs
        ;; version.  Where it is, the following cell swallows the new one:
        ;; two cells over one region, which shows as a doubled boundary,
        ;; fails the consistency check and stops syncing -- so the next edit
        ;; there is not saved and the cell appears to vanish.
        ;;
        ;; Tested for rather than assumed: the repair asks whether the two
        ;; overlays actually overlap, which is the condition that matters
        ;; and is true or false the same way everywhere.
        (let ((new-ov (emjupy-cell-overlay new-cell))
              (resume (+ at inserted)))
          (when (and (overlayp following-ov)
                     (overlayp new-ov)
                     (eq (overlay-buffer following-ov) (current-buffer))
                     (< (overlay-start following-ov) (overlay-end new-ov)))
            ;; Past everything just inserted, which includes the blank line
            ;; separating the new cell from this one -- starting at the new
            ;; cell's own end would leave that newline inside the following
            ;; cell, where it shows up as a stray line at the top of its
            ;; source.
            (move-overlay following-ov resume
                          (max resume (overlay-end following-ov)))))
        (emjupy--undo-adjust at at inserted))
      t)))

(defun emjupy-delete-cell-at (nb cell)
  "Remove CELL from NB and erase it without rebuilding the buffer.

Returns non-nil when it was done incrementally."
  (let ((region (emjupy--cell-region cell)))
    (when (and region (emjupy--overlays-sane-p))
      (let ((start (car region))
            (end (cdr region)))
        (setf (emjupy-notebook-cells nb)
              (vconcat (delq cell (append (emjupy-notebook-cells nb) nil))))
        (let ((inhibit-read-only t)
              (buffer-undo-list t))
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
        t))))

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
      (if target-cell
          ;; An explicit target is a request to move: honour it.
          (emjupy--rerender-notebook-1 target-cell)
        (emjupy--keeping-the-view
         (lambda () (emjupy--rerender-notebook-1 nil)))))))

(defun emjupy--keeping-the-view (thunk)
  "Call THUNK, leaving point and the window scrolled where they were.

Redrawing erases the buffer and rebuilds it, which moves point to the
end and scrolls the window to follow.  Output arriving in a cell far
from the one being read would therefore drag the view away from it --
the reason a running notebook could not be read while it ran.

Positions are remembered as line and column rather than as offsets:
the rebuild changes how many characters precede a line, since output
boxes grow and shrink, but the line a reader is looking at is still the
line they were looking at."
  (let* ((windows (get-buffer-window-list (current-buffer) nil t))
         (point-line (line-number-at-pos (point)))
         (point-col (current-column))
         (starts (mapcar (lambda (w)
                           (cons w (line-number-at-pos (window-start w))))
                         windows)))
    (unwind-protect
        (funcall thunk)
      (emjupy--goto-line-column point-line point-col)
      (dolist (pair starts)
        (let ((w (car pair)))
          (when (window-live-p w)
            (set-window-start
             w (save-excursion (emjupy--goto-line-column (cdr pair) 0) (point))
             t)
            (set-window-point w (point))))))))

(defun emjupy--goto-line-column (line column)
  "Put point at COLUMN of LINE, or as near as the buffer allows."
  (goto-char (point-min))
  (forward-line (1- line))
  (move-to-column column))



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
    (let ((index (if idx (1+ idx) (length cells))))
      (unless (emjupy-insert-cell-at nb index new-cell)
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 index)
                               (list new-cell)
                               (cl-subseq cells index))))
        (emjupy--rerender-notebook new-cell)))
    (emjupy--goto-cell new-cell 'start)))

(defun emjupy-insert-cell-above ()
  "Insert a new empty code cell above the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (emjupy--cell-at-point))
         (cells (append (emjupy-notebook-cells nb) nil))
         (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (let ((index (or idx 0)))
      (unless (emjupy-insert-cell-at nb index new-cell)
        (setf (emjupy-notebook-cells nb)
              (vconcat (append (cl-subseq cells 0 index)
                               (list new-cell)
                               (cl-subseq cells index))))
        (emjupy--rerender-notebook new-cell)))
    (emjupy--goto-cell new-cell 'start)))

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
      (setq emjupy--split-was-sane (emjupy--overlays-sane-p))
      (setf (emjupy-cell-source cell) (substring source 0 offset))
      ;; Neither half produced what is on screen any more.
      (setf (emjupy-cell-outputs cell) [])
      (setf (emjupy-cell-exec-count cell) nil)
      (setf (emjupy-notebook-cells nb)
            (vconcat (append (cl-subseq cells 0 (1+ idx))
                             (list new-cell)
                             (cl-subseq cells (1+ idx)))))
      (unless (emjupy--redraw-cells-in-place (list cell) (list cell new-cell)
                                             emjupy--split-was-sane)
        (emjupy--rerender-notebook new-cell))
      (let ((ov (emjupy-cell-overlay new-cell)))
        (when (overlayp ov) (goto-char (overlay-start ov))))
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
          (unless (emjupy--redraw-cells-in-place (list above cell) (list above))
            (emjupy--rerender-notebook above))
          ;; Leave point at the seam, where the merged-in cell begins.
          (let ((ov (emjupy-cell-overlay above)))
            (when (overlayp ov)
              (goto-char (min (overlay-end ov) (+ (overlay-start ov) seam)))))
          above)))))

(defun emjupy--cell-at-point-including-output (&optional pos)
  "Return the cell owning POS, counting its output box as part of it.

`emjupy--cell-at-point\' answers for the source only, which is right for
editing: output is not text the user is writing.  Commands about the
output itself need the other answer -- with point in an output box, the
cell meant is the one above, whose output that is."
  (let ((pos (or pos (point))))
    ;; The output box is asked about FIRST.  A position inside one lies
    ;; between two cells as far as the source lookup is concerned, and that
    ;; lookup answers with the cell below -- which has its own, usually
    ;; empty, output, so the command reported nothing to hide while point
    ;; sat in the very output meant.
    (or (when emjupy--buffer-notebook
          (cl-find-if
           (lambda (cell)
             (let ((out (emjupy-cell-output-ov cell)))
               (and (overlayp out)
                    (eq (overlay-buffer out) (current-buffer))
                    (>= pos (overlay-start out))
                    (<= pos (overlay-end out)))))
           (append (emjupy-notebook-cells emjupy--buffer-notebook) nil)))
        (emjupy--cell-at-point pos))))

;;;###autoload
(defun emjupy-toggle-cell-output ()
  "Collapse or restore the output of the cell at point.

Collapsing hides the output and marks the cell's bottom rule with a
glyph.  Nothing is discarded: the outputs stay on the cell, so restoring
them is a redraw rather than a re-run, and the kernel is not involved
either way.

The state is kept in the cell's metadata where Jupyter keeps it, so it
survives saving and means the same thing elsewhere."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (emjupy--cell-at-point-including-output)))
    (unless cell (user-error "Point is not in a cell"))
    (emjupy-toggle-output-of-cell cell)))

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
      ;; Only this cell's box goes, so only this cell need be redrawn -- and
      ;; redrawing just it keeps the undo history, where rebuilding the
      ;; notebook threw it away.  Nothing was lost even then, but an edit
      ;; made before clearing could not be undone afterwards.
      (unless (emjupy--refresh-cell-output cell)
        (emjupy--rerender-notebook cell))
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

(defun emjupy--reinsert-deleted-cell (index cell)
  "Put CELL back at INDEX, for undo.

Recorded in the undo list as an `apply' entry when a cell is deleted, so
that plain \\[undo] brings it back.  The deletion itself cannot be undone
the ordinary way: it is made with recording off, because a redraw moves
buffer positions that undo entries hold literally."
  (when emjupy--buffer-notebook
    (let* ((cells (append (emjupy-notebook-cells emjupy--buffer-notebook) nil))
           (at (min (max index 0) (length cells))))
      (setf (emjupy-cell-overlay cell) nil)
      (setf (emjupy-cell-output-ov cell) nil)
      (unless (emjupy-insert-cell-at emjupy--buffer-notebook at cell)
        (setf (emjupy-notebook-cells emjupy--buffer-notebook)
              (vconcat (append (cl-subseq cells 0 at) (list cell) (cl-subseq cells at))))
        (emjupy--rerender-notebook cell))
      ;; and undoing the undo removes it again
      (unless (eq buffer-undo-list t)
        (push (list 'apply #'emjupy--redelete-cell cell) buffer-undo-list))
      (emjupy--goto-cell cell 'start))))

(defun emjupy--redelete-cell (cell)
  "Remove CELL again, for redo after `emjupy--reinsert-deleted-cell'."
  (when emjupy--buffer-notebook
    (let ((index (cl-position cell (append (emjupy-notebook-cells emjupy--buffer-notebook)
                                           nil))))
      (unless (emjupy-delete-cell-at emjupy--buffer-notebook cell)
        (setf (emjupy-notebook-cells emjupy--buffer-notebook)
              (vconcat (remq cell (append (emjupy-notebook-cells emjupy--buffer-notebook)
                                          nil))))
        (emjupy--rerender-notebook))
      (unless (eq buffer-undo-list t)
        (push (list 'apply #'emjupy--reinsert-deleted-cell (or index 0) cell)
              buffer-undo-list)))))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
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
        (unless (emjupy-delete-cell-at nb curr-cell)
          (setf (emjupy-notebook-cells nb) (vconcat remaining))
          (emjupy--rerender-notebook next))
        ;; An entry undo can act on.  The text deletion itself is made with
        ;; recording off -- a redraw moves positions that undo entries hold
        ;; literally -- so the way back is a function, not a region.
        (unless (eq buffer-undo-list t)
          (push (list 'apply #'emjupy--reinsert-deleted-cell idx curr-cell)
                buffer-undo-list)
          (undo-boundary))
        (when next (ignore-errors (emjupy--goto-cell next 'start)))))))

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
