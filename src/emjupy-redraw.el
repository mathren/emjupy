;;; emjupy-redraw.el --- Redrawing cells, in place or whole, and putting a redraw right  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Redrawing the notebook from its cells: one cell's output in place, or
;; everything, inside `emjupy--atomic-redraw', which puts a redraw stopped
;; partway right by redrawing from the cells.  The structure check that
;; runs after redraws, and its repair, are here too, as repairing means
;; redrawing.
;;
;; A cell command is one undo step (`emjupy--as-one-undo-step'): a snapshot
;; of the cells, restored by redrawing.  It lives here because undoing a
;; step redraws the notebook, and a full redraw records a step.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-undo)
(require 'emjupy-cells)


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

(defun emjupy--relabel-output (cell out)
  "Redraw only the label of CELL's output box OUT, its output unchanged.
As when the cell finished and nothing new came.  Set as drawing the box
sets it, then placed as a redraw places it."
  (let ((inhibit-read-only t))
    (overlay-put out 'emjupy-header nil)
    (overlay-put out 'before-string
                 (emjupy--rule (emjupy--cell-out-label cell) "├"))
    (emjupy--redraw-cell-header cell)
    (emjupy--redraw-cell-footer cell)
    (emjupy--reconcile-rules)
    t))

(defun emjupy--draw-stream-gain (out pieces last-out added)
  "Draw ADDED, the text the last stream output gained, at the end of OUT.
PIECES are the outputs drawn, LAST-OUT the stream output; OUT is
extended to cover the text, which is kept out of the undo history."
  (let* ((start (overlay-end out))
         (inhibit-read-only t)
         (buffer-undo-list t)
         (emjupy--refontifying t)
         end)
    (emjupy--atomic-redraw t
      (save-excursion
        (goto-char start)
        (insert added)
        (let ((widest (emjupy--finish-output-piece last-out start)))
          (unless (eq (char-before) ?\n) (insert "\n"))
          (setq end (point))
          (move-overlay out (overlay-start out) end)
          (overlay-put out 'emjupy-widest
                       (max widest (or (overlay-get out 'emjupy-widest) 0)))
          (overlay-put out 'emjupy-drawn (emjupy--drawn-state pieces))))
      (when emjupy-protect-non-cell-regions
        (emjupy--make-read-only start end))
      (emjupy--undo-adjust start start (- end start)))
    t))

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
                      (emjupy--outputs-to-draw (emjupy-cell-outputs cell))))
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
          (emjupy--relabel-output cell out)
        (emjupy--draw-stream-gain out pieces last-out
                                  (substring new (length old)))))))

(defun emjupy--redraw-cell-output (cell)
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
                    (emjupy--draw-cell-output cell)
                    (setq new-end (point)))
                  (emjupy--redraw-cell-header cell)
                  ;; The bottom edge belongs to the last box: the output's own
                  ;; footer, or, with no output, the cell's.
                  (emjupy--redraw-cell-footer cell)
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
      (emjupy--redraw-cell-output cell)
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

(cl-defun emjupy--redraw-rules (&key force)
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
                                   (if (overlayp out)
                                       ""
                                     (emjupy--footer-rule))))
                    (when (overlayp out)
                      (overlay-put out 'emjupy-header nil)
                      (overlay-put out 'before-string
                                   (emjupy--rule (emjupy--cell-out-label cell)
                                                 "├"))
                      (overlay-put out 'after-string (emjupy--footer-rule))
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
The number column takes width from the page: rules drawn for the old
width would wrap."
  (when emjupy--buffer-notebook
    (emjupy--redraw-rules :force t)))

(defun emjupy--window-size-changed (&optional _frame)
  "Refresh box rules in every emjupy buffer after a window size change."
  (dolist (buf (emjupy--notebook-buffers))
    (with-current-buffer buf
      (emjupy--redraw-rules))))

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

(defun emjupy--draw-cell-at (cell at)
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
        (emjupy--draw-cell cell)
        (setq inserted (- (point-max) before))))
    (emjupy--protect-non-cell-regions)
    inserted))

(defun emjupy--forget-cell-overlays (cells)
  "Delete the overlays of CELLS, source and output, and forget them."
  (dolist (cell cells)
    (when (overlayp (emjupy-cell-overlay cell))
      (delete-overlay (emjupy-cell-overlay cell)))
    (when (overlayp (emjupy-cell-output-ov cell))
      (delete-overlay (emjupy-cell-output-ov cell)))
    (setf (emjupy-cell-overlay cell) nil)
    (setf (emjupy-cell-output-ov cell) nil)))

(defun emjupy--cell-from (pos)
  "Return the first cell drawn in this buffer at POS or after, or nil."
  (cl-find-if (lambda (cell)
                (let ((ov (emjupy-cell-overlay cell)))
                  (and (overlayp ov)
                       (eq (overlay-buffer ov) (current-buffer))
                       (>= (overlay-start ov) pos))))
              (and emjupy--buffer-notebook
                   (append (emjupy-notebook-cells emjupy--buffer-notebook)
                           nil))))

(defun emjupy--redraw-cells-in-place (old-cells new-cells &optional known-sane)
  "Replace what OLD-CELLS occupy with NEW-CELLS, drawn in their place.
Inserting, deleting, merging, splitting cause everything
outside to move by a known amount, so the undo history is kept.
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
            (emjupy--forget-cell-overlays old-cells)
            ;; The next cell may start where this region ends, and take the
            ;; inserted text into its overlay; its bounds are noted and
            ;; restored.
            (setq following-after (emjupy--cell-from end))
            (delete-region start end)
            (save-excursion
              (goto-char start)
              (dolist (cell new-cells)
                (emjupy--draw-cell cell))
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

;;;; Redrawing the notebook

(defun emjupy--redraw-notebook (&optional target-cell)
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
           (lambda () (emjupy--redraw-notebook-1 target-cell)))
        (emjupy--keeping-the-view
         (lambda () (emjupy--redraw-notebook-1)))))))

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

(defun emjupy--redraw-notebook-1 (&optional target-cell)
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
                     (emjupy--draw-cell cell)
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
        ;; The rebuild moved every position the history holds, so it records one
        ;; step that puts the cells back, and undo carries on through the older
        ;; entries.
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
      (emjupy--keeping-the-view (lambda () (emjupy--redraw-notebook-1)))
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

;;;; Putting a redraw right

(defun emjupy--redraw-notebook-keeping-point ()
  "Re-render the notebook without disturbing where the user is typing.
Not with TARGET-CELL, which moves point to the start of that cell."
  ;; What is on screen goes back into the cells FIRST: the redraw rebuilds the
  ;; buffer from them, and anything typed since the last sync would be lost.
  (emjupy--sync-all-cells)
  (let* ((cell (emjupy--cell-at-point))
         (ov (and cell (emjupy-cell-overlay cell)))
         (offset (and ov (- (point) (overlay-start ov)))))
    (emjupy--redraw-notebook)
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
          (emjupy--redraw-notebook-keeping-point)
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

(defun emjupy--check-integrity (buffer)
  "Put BUFFER right if its structure and its cells have come apart.
Only the structure is checked, not the text, which differs while
typing is unsynced.  The problems found are reported."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (setq emjupy--integrity-timer nil)
      (when-let* ((problems (and emjupy--buffer-notebook
                                 (not emjupy--recovering)
                                 (emjupy--check-invariants :structure-only t))))
        (let ((emjupy--recovering t))
          (emjupy--sync-sound-cells)
          (setq emjupy--sync-refused nil)
          (emjupy--redraw-notebook-keeping-point))
        (message "[emjupy] The notebook was redrawn: its display had come apart from its cells (%s)."
                 (string-join problems "; "))))))

(defun emjupy--check-after-redraw ()
  "Check this buffer's structure when Emacs is next idle, once.
Many redraws in a row -- output streaming in -- cost one check."
  (unless (timerp emjupy--integrity-timer)
    (setq emjupy--integrity-timer
          (run-with-idle-timer 1 nil #'emjupy--check-integrity
                               (current-buffer)))))

;;;; Redrawing by hand

(defun emjupy-re-render ()
  "Rebuild the notebook display from the cells.

Everything on screen is drawn from the cell structs, so if the buffer
ever looks wrong -- overlays out of place after an unusual edit, say --
this puts it back without touching the notebook itself.  Edits in the
buffer are folded in first, so nothing you have typed is lost."
  (interactive)
  (emjupy--sync-all-cells)
  (emjupy--redraw-notebook-keeping-point)
  (message "[emjupy] Redrew the notebook."))

(defun emjupy--redraw-rules-timed (&rest _)
  "Redraw the cell rules, reporting if it is slow.
A hook calls it; what the hook passes is not needed."
  (emjupy--timed "rule refresh" #'emjupy--redraw-rules))

;;;; In a notebook buffer

(defun emjupy-redraw-enable ()
  "Keep the current notebook's rules as wide as its page.
The number column narrows the page, and so does a resize.
`window-size-change-functions' is set globally, once for every notebook:
`emjupy--window-size-changed' refreshes all notebook buffers, so a
buffer-local copy would run once per buffer, each repeating the work."
  (add-hook 'display-line-numbers-mode-hook #'emjupy--line-numbers-toggled
            nil t)
  (add-hook 'window-size-change-functions #'emjupy--window-size-changed)
  ;; Outlines follow the window through splits and drags.
  (add-hook 'window-configuration-change-hook #'emjupy--redraw-rules-timed
            nil t))

(provide 'emjupy-redraw)

;;; emjupy-redraw.el ends here
