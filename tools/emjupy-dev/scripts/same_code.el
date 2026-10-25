;;; same_code.el --- Do two trees hold the same code?  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;; BEFORE=dir/ AFTER=dir/ FILES="a.el b.el" emacs -batch -Q -l same_code.el
;; Compares each file's top-level forms, in order, with every docstring
;; set aside -- of defun, defmacro, cl-defun, defvar, defconst, defcustom,
;; defface, define-minor-mode and ert-deftest -- so a prose pass or a
;; comment rewrite reads back as "same code".  SORTED=1 compares the forms
;; as a multiset, for a change that only reorders them.
;; Exits 1 if any file differs.

(defun same-code--strip (form)
  (when (consp form)
    (pcase (car form)
      ((or 'defun 'defmacro 'defsubst 'cl-defun 'cl-defmacro 'define-minor-mode 'ert-deftest
           'defvar 'defconst 'defvar-local 'defcustom 'defface)
       (when (and (> (length form) 3) (stringp (nth 3 form)))
         (setf (nth 3 form) "DOC")))))
  form)

(defun same-code--forms (file)
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (forms)
      (condition-case nil
          (while t (push (prin1-to-string (same-code--strip (read (current-buffer)))) forms))
        (end-of-file nil))
      (setq forms (nreverse forms))
      (if (getenv "SORTED") (sort forms #'string<) forms))))

(let ((before (getenv "BEFORE")) (after (getenv "AFTER")) (bad 0))
  (dolist (f (split-string (getenv "FILES")))
    (let ((same (equal (same-code--forms (expand-file-name f before))
                       (same-code--forms (expand-file-name f after)))))
      (unless same (setq bad (1+ bad)))
      (message "%-34s %s" f (if same "same code" "CODE DIFFERS"))))
  (kill-emacs (if (> bad 0) 1 0)))
