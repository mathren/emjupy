;;; emjupy-undo.el --- Keeping undo history right across redraws  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Undo entries hold buffer positions, not markers.  When a redraw replaces
;; a stretch of the buffer, the entries after it must shift and those
;; inside it must go, or the next undo rewrites the wrong text.  This is
;; that bookkeeping.  The undo step a cell command records, and its
;; snapshot of the cells, are with the redraws, in emjupy-redraw.el: undoing
;; a step redraws the notebook, and redrawing it records a step.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)


;;;; Undo

(defun emjupy--undo-entry-position (entry)
  "Return the largest buffer position ENTRY refers to, or nil.

Undo entries come in several shapes and only some carry positions;
one without any cannot be invalidated by an edit elsewhere."
  (cond
   ((integerp entry) (abs entry))
   ((not (consp entry)) nil)
   ;; (TEXT . POSITION) -- a deletion; POSITION may be negative
   ((and (stringp (car entry)) (integerp (cdr entry))) (abs (cdr entry)))
   ;; (BEG . END) -- an insertion
   ((and (integerp (car entry)) (integerp (cdr entry)))
    (max (car entry) (cdr entry)))
   ;; (nil PROP VAL BEG . END) -- a property change
   ((and (null (car entry)) (consp (last entry))
         (integerp (car (last entry))) (integerp (cdr (last entry))))
    (max (car (last entry)) (cdr (last entry))))
   ;; (apply DELTA BEG END FUN . ARGS)
   ((eq (car entry) 'apply)
    (let ((beg (nth 2 entry)) (end (nth 3 entry)))
      (and (integerp beg) (integerp end) (max beg end))))
   ((markerp (car entry)) (marker-position (car entry)))
   (t nil)))

(defun emjupy--undo-map-positions (entry fn)
  "Return ENTRY with every buffer position in it passed through FN.
Each shape `buffer-undo-list' documents keeps its positions somewhere
else; missing one would make undo rewrite the wrong text."
  (cond
   ((integerp entry) (funcall fn entry))
   ((not (consp entry)) entry)
   ;; (TEXT . POSITION) -- a deletion.  A negative POSITION means point
   ;; was at the end of the deleted text, so the sign has to be kept.
   ((and (stringp (car entry)) (integerp (cdr entry)))
    (let ((shifted (funcall fn (abs (cdr entry)))))
      (cons (car entry) (if (< (cdr entry) 0) (- shifted) shifted))))
   ;; (BEG . END) -- an insertion
   ((and (integerp (car entry)) (integerp (cdr entry)))
    (cons (funcall fn (car entry)) (funcall fn (cdr entry))))
   ;; (nil PROP VAL BEG .  END), a property change: a dotted list, so taken
   ;; apart by its fixed shape, not with `butlast'.
   ((and (null (car entry)) (consp (cdr entry)) (consp (cddr entry))
         (consp (cdddr entry))
         (integerp (car (cdddr entry))) (integerp (cdr (cdddr entry))))
    (pcase-let ((`(nil ,prop ,val ,beg . ,end) entry))
      `(nil ,prop ,val ,(funcall fn beg) . ,(funcall fn end))))
   ;; (apply DELTA BEG END FUN . ARGS)
   ((and (eq (car entry) 'apply) (integerp (nth 2 entry))
         (integerp (nth 3 entry)))
    (append (list (nth 0 entry) (nth 1 entry)
                  (funcall fn (nth 2 entry))
                  (funcall fn (nth 3 entry)))
            (nthcdr 4 entry)))
   (t entry)))

(defun emjupy--undo-adjust (start old-end delta)
  "Keep the undo history usable across a replacement of START..OLD-END.
Entries before START stay; entries inside the region describe text
that is gone, and go; entries after it have moved, and shift by DELTA.
Each entry shape is handled explicitly: a wrong position does not
fail, it rewrites the wrong text."
  (unless emjupy--in-cell-step
    (let ((transform
           (lambda (entry)
             (let ((p (emjupy--undo-entry-position entry)))
               (cond
                ((null p) entry)
                ((< p start) entry)
                ((< p old-end)
                 ;; Text inside the region is gone, and an entry for it goes
                 ;; too.  Structural commands never get here: they are undone
                 ;; whole.  This is for redraws, such as output arriving.
                 :emjupy-drop)
                (t (emjupy--undo-map-positions
                    entry (lambda (x) (if (>= x old-end) (+ x delta) x))))))))
          (seen (make-hash-table :test 'eq)))
      ;; In place: undo replays from a list `primitive-undo' holds, and the
      ;; three lists share their cons cells, so changing those reaches each,
      ;; once.  An entry to drop becomes one that does nothing.  All or nothing:
      ;; every new entry is worked out first, or a failure would leave the
      ;; history half shifted.
      (let ((changes nil))
        (condition-case err
            (progn
              (dolist (entries (list buffer-undo-list pending-undo-list))
                (when (consp entries)
                  (let ((c entries))
                    (while (consp c)
                      (unless (gethash c seen)
                        (puthash c t seen)
                        (push (cons c (funcall transform (car c))) changes))
                      (setq c (cdr c))))))
              (pcase-dolist (`(,c . ,new) changes)
                (setcar c (if (eq new :emjupy-drop)
                              (list 'apply #'ignore :emjupy-dropped)
                            new))))
          ;; Anything else is a mode's own entry: the ones from START on are
          ;; stale and go -- lost history is safe, wrong history is not.
          (error
           (emjupy--undo-drop-from start)
           (when (consp pending-undo-list)
             (setq pending-undo-list
                   (cl-remove-if (lambda (e)
                                   (let ((p (emjupy--undo-entry-position e)))
                                     (and p (>= p start))))
                                 pending-undo-list)))
           (message "[emjupy] Undo history after the redrawn output was dropped: %s"
                    (error-message-string err)))))
      ;; Outside an undo the placeholders can go for good.
      (unless (or undo-in-progress (consp pending-undo-list))
        (when (consp buffer-undo-list)
          (setq buffer-undo-list
                (delete (list 'apply #'ignore :emjupy-dropped)
                        buffer-undo-list)))))))

(defun emjupy--undo-drop-from (pos)
  "Drop undo entries referring to POS or later in this buffer.
Undo entries hold positions, not markers, so an edit at POS leaves
them stale.  Entries before POS stay."
  (when (listp buffer-undo-list)
    (setq buffer-undo-list
          (cl-remove-if (lambda (entry)
                          (let ((p (emjupy--undo-entry-position entry)))
                            (and p (>= p pos))))
                        buffer-undo-list))))

(provide 'emjupy-undo)

;;; emjupy-undo.el ends here
