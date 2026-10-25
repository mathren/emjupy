;;; moves.el --- Did a change only move code?  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;; BEFORE=dir AFTER=dir emacs -batch -Q -l moves.el
;; Are the top-level forms of all emjupy*.el in the two directories the same
;; multiset, require and provide forms set aside?  Proves a move moved only:
;; run it on src/ (or test/) copied before the move and after.  It cannot see
;; a meaning change from a moved declaration -- the compiler does.
(defun mv-forms (dir)
  (let (forms)
    (dolist (f (directory-files dir t "\\`emjupy.*\\.el\\'"))
      (unless (string-match-p "pkg" f)
        (with-temp-buffer
          (insert-file-contents f) (goto-char (point-min))
          (condition-case nil
              (while t
                (let ((form (read (current-buffer))))
                  (unless (memq (car-safe form) '(require provide))
                    (push (prin1-to-string form) forms))))
            (end-of-file nil)))))
    (sort forms #'string<)))
(let* ((a (mv-forms (getenv "BEFORE"))) (b (mv-forms (getenv "AFTER")))
       (only-a (seq-difference a b)) (only-b (seq-difference b a)))
  (message "MOVES %d forms before, %d after: %s" (length a) (length b)
           (if (and (null only-a) (null only-b) (= (length a) (length b))) "only moved" "CHANGED"))
  (dolist (x (seq-take only-a 5)) (message "MOVES   only before: %s" (substring x 0 (min 100 (length x)))))
  (dolist (x (seq-take only-b 5)) (message "MOVES   only after:  %s" (substring x 0 (min 100 (length x))))))
