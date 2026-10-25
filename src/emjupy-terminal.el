;;; emjupy-terminal.el --- Stream text as a terminal draws it  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Stream output, applied as a terminal would show it: carriage returns,
;; backspaces, cursor movement and line clearing, the escape codes progress
;; bars use.  Strings in, strings out: nothing here touches a buffer.

;;; Code:

(require 'cl-lib)
(require 'subr-x)


;;;; Stream text as a terminal shows it

;; Progress bars that stack -- tqdm nested, or with `position' -- move the
;; cursor up a line to redraw the bar above, and clear what is left of a
;; line.  Only carriage returns were applied, and stale bars piled up.

(defconst emjupy--terminal-codes-re "\e\\[[0-9;]*[ABCDGK]"
  "Escape codes, in stream output, that move the cursor or clear a line.")

(defun emjupy--terminal-codes-p (text)
  "Return non-nil if there are cursor or line-clearing codes in TEXT."
  (and (stringp text) (string-match-p emjupy--terminal-codes-re text)))

(defun emjupy--terminal-column (line col)
  "Return the index in LINE of its column COL, and the columns it has there.
A cons of the index and the column reached: colour codes take no column,
and one starting at an index is passed over to the character after it.
Past the end of LINE, the index is its length and the column its width."
  (let ((i 0) (c 0) (n (length line)) (done nil))
    (while (and (< i n) (not done))
      (cond ((and (eq (aref line i) ?\e)
                  (string-match "\e\\[[0-9;]*m" line i)
                  (= (match-beginning 0) i))
             (setq i (match-end 0)))
            ((= c col) (setq done t))
            (t (setq i (1+ i) c (1+ c)))))
    (cons i c)))

(defun emjupy--terminal-put (line col text)
  "Return LINE with TEXT written over it from column COL."
  (let* ((at (emjupy--terminal-column line col))
         (line (if (< (cdr at) col)
                   (concat line (make-string (- col (cdr at)) ?\s))
                 line))
         (i (car (emjupy--terminal-column line col)))
         (end (car (emjupy--terminal-column line (+ col (length text))))))
    (concat (substring line 0 i) text (substring line (min end
                                                           (length line))))))

(defun emjupy--terminal-erase (line col mode)
  "Return LINE erased as ESC [ MODE K erases it, the cursor at column COL.
MODE 0 erases from the cursor to the end, 1 from the start to the
cursor, anything else the whole line."
  (pcase mode
    (0 (substring line 0 (min (car (emjupy--terminal-column line col))
                              (length line))))
    (1 (emjupy--terminal-put line 0 (make-string col ?\s)))
    (_ "")))

(defun emjupy--terminal-colour (line col code)
  "Return LINE with CODE, a colour escape, kept at column COL.
It takes no column, so is put where the cursor is, not over text."
  (let ((at (car (emjupy--terminal-column line col))))
    (concat (substring line 0 at) code (substring line at))))

(defun emjupy--terminal-result (lines row col)
  "Return LINES, a vector, as text, with the cursor at ROW and COL.
A cons of the text and the cursor, nil for the end of the text.
Trailing spaces go: what a cleared bar leaves."
  (dotimes (i (length lines))
    (aset lines i (replace-regexp-in-string " +\\'" "" (aref lines i))))
  (let* ((last (1- (length lines)))
         (at-end (and (= row last)
                      (>= col (cdr (emjupy--terminal-column
                                    (aref lines last) most-positive-fixnum))))))
    (cons (mapconcat #'identity lines "\n")
          (unless at-end (cons row col)))))

(defun emjupy--terminal-apply (text chunk cursor)
  "Return TEXT with CHUNK written to it, as a terminal would show it.
CURSOR is where the last write left the cursor, a cons of line and
column, or nil for the end of TEXT.  Applied: newline, carriage return,
backspace, and the escape codes that move the cursor or clear a line;
colour codes are kept, taking no column, others dropped.  Return a cons
of the new text and the cursor, nil for the end of the text."
  (let* ((lines (vconcat (split-string text "\n")))
         (row (if cursor (car cursor) (1- (length lines))))
         (col (if cursor (cdr cursor)
                (cdr (emjupy--terminal-column (aref lines row)
                                              most-positive-fixnum))))
         (p 0) (n (length chunk)))
    (cl-flet ((line () (aref lines row))
              (set-line (s) (aset lines row s))
              (to-row (r)
                (setq row (max 0 r))
                (when (>= row (length lines))
                  (setq lines (vconcat lines (make-vector (- (1+ row)
                                                             (length lines))
                                                          ""))))))
      (while (< p n)
        (let ((ch (aref chunk p)))
          (cond
           ((eq ch ?\n) (to-row (1+ row)) (setq col 0 p (1+ p)))
           ((eq ch ?\r) (setq col 0 p (1+ p)))
           ((eq ch ?\b) (setq col (max 0 (1- col)) p (1+ p)))
           ((and (eq ch ?\e) (string-match "\e\\[\\([0-9;]*\\)\\([A-Za-z]\\)"
                                           chunk p)
                 (= (match-beginning 0) p))
            ;; All read from the match now: what follows matches again,
            ;; and a colour code read later came back as some other text.
            (let* ((code (match-string 0 chunk))
                   (args (match-string 1 chunk))
                   (num (if (string-empty-p args) 1
                          (max 1 (string-to-number args))))
                   (final (aref (match-string 2 chunk) 0)))
              (setq p (match-end 0))
              (pcase final
                (?A (to-row (- row num)))
                (?B (to-row (+ row num)))
                (?C (setq col (+ col num)))
                (?D (setq col (max 0 (- col num))))
                (?G (setq col (1- num)))
                (?K (set-line (emjupy--terminal-erase
                               (line) col
                               (if (string-empty-p args) 0
                                 (string-to-number args)))))
                (?m (set-line (emjupy--terminal-colour (line) col code)))
                (_ nil))))
           ((eq ch ?\e) (setq p (min n (+ p 2))))
           (t
            ;; a run of ordinary characters, written at once
            (let ((end (or (string-match "[\n\r\b\e]" chunk p) n)))
              (set-line (emjupy--terminal-put (line) col (substring chunk
                                                                    p end)))
              (setq col (+ col (- end p)) p end)))))))
    (emjupy--terminal-result lines row col)))

(defun emjupy--collapse-carriage-returns (text)
  "Return TEXT with everything before a carriage return on a line dropped.
As a terminal does, which progress bars rely on."
  (mapconcat (lambda (line)
               (let ((parts (split-string line "\r")))
                 (car (last parts))))
             (split-string text "\n" )
             "\n"))

(provide 'emjupy-terminal)

;;; emjupy-terminal.el ends here
