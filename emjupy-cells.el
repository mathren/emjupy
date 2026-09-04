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
  "Re-render all cell overlays in current buffer. If TARGET-CELL is given, move point to it."
  (when emjupy--buffer-notebook
    (let ((inhibit-read-only t)
          (cells (emjupy-notebook-cells emjupy--buffer-notebook))
          (target-start nil))
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
        (goto-char target-start)))))

(defun emjupy-insert-cell-below ()
  "Insert a new empty code cell below the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (get-text-property (point) 'emjupy-cell))
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
         (curr-cell (get-text-property (point) 'emjupy-cell))
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
         (cell (get-text-property (point) 'emjupy-cell))
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
         (cell (get-text-property (point) 'emjupy-cell))
         (idx (cl-position cell cells)))
    (if (or (not idx) (= idx (1- (length cells))))
        (message "[emjupy] Cell is already at the bottom.")
      (let ((below (aref cells (1+ idx))))
        (aset cells (1+ idx) cell)
        (aset cells idx below))
      (emjupy--rerender-notebook cell))))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (get-text-property (point) 'emjupy-cell))
         (cells (append (emjupy-notebook-cells nb) nil)))
    (when curr-cell
      (setq cells (delete curr-cell cells))
      (setf (emjupy-notebook-cells nb) (vconcat cells))
      (emjupy--rerender-notebook))))

(defun emjupy-cycle-cell-type ()
  "Cycle the cell at point between `code' and `markdown'."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (get-text-property (point) 'emjupy-cell)))
    (unless cell
      (user-error "No cell found at point"))
    (setf (emjupy-cell-type cell)
          (if (eq (emjupy-cell-type cell) 'code) 'markdown 'code))
    (emjupy--rerender-notebook cell)))

(defun emjupy-next-cell ()
  "Move point to the next cell."
  (interactive)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
         (ov (and cell (emjupy-cell-overlay cell))))
    (if ov
        (let ((pos (overlay-end ov)))
          (when (< pos (point-max))
            (goto-char (1+ pos))))
      (goto-char (point-min)))))

(defun emjupy-previous-cell ()
  "Move point to the previous cell."
  (interactive)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
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
