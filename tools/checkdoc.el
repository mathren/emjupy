;;; checkdoc.el --- checkdoc on the files emjupy ships  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; cd src && emacs -batch -Q -L . -l ../tools/checkdoc.el FILE...
;;
;; Every complaint is printed; any fails.

;;; Code:

(require 'checkdoc)

(let ((buffer "*checkdoc-status*")
      (n 0))
  (ignore-errors (kill-buffer buffer))
  (dolist (f command-line-args-left)
    (with-temp-buffer
      (insert-file-contents f)
      (emacs-lisp-mode)
      (setq buffer-file-name (expand-file-name f))
      (let ((checkdoc-diagnostic-buffer buffer))
        (checkdoc-current-buffer t))
      (setq buffer-file-name nil)))
  (when (get-buffer buffer)
    (with-current-buffer buffer
      (goto-char (point-min))
      (while (re-search-forward "^\\(.*\\.el\\):[0-9]+:" nil t)
        (setq n (1+ n))
        (message "  %s" (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position))))))
  (message "checkdoc: %d" n)
  (setq command-line-args-left nil)
  (kill-emacs (if (> n 0) 1 0)))

;;; checkdoc.el ends here
