;;; lint.el --- checkdoc and package-lint over the sources  -*- lexical-binding: t; -*-

;; This file is not part of the package: it is a development tool, and the
;; MELPA recipe lists the files that ship.

;;; Commentary:

;; What a MELPA review checks, run the same way locally and in CI, so that
;; the conventions the code already follows by hand cannot quietly lapse.
;;
;; package-lint is not part of Emacs.  Its absence is reported rather than
;; treated as a failure, so this is useful on a machine that only has Emacs.

;;; Code:

(require 'checkdoc)

(defconst lint-sources
  '("emjupy-core.el" "emjupy-http.el" "emjupy-render.el" "emjupy-cells.el"
    "emjupy-kernel.el" "emjupy-lsp.el" "emjupy-eglot.el" "emjupy-notebook.el"
    "emjupy.el")
  "Files that ship, and so the files that must be clean.")

(defun lint--checkdoc ()
  "Return the number of checkdoc complaints, printing each."
  (let ((buffer "*checkdoc-status*"))
    (ignore-errors (kill-buffer buffer))
    (dolist (f lint-sources)
      (with-temp-buffer
        (insert-file-contents f)
        (emacs-lisp-mode)
        (setq buffer-file-name (expand-file-name f))
        (let ((checkdoc-diagnostic-buffer buffer))
          (ignore-errors (checkdoc-current-buffer t)))
        (setq buffer-file-name nil)))
    (let ((n 0))
      (when (get-buffer buffer)
        (with-current-buffer buffer
          (goto-char (point-min))
          (while (re-search-forward "^\\(.*\\.el\\):[0-9]+:" nil t)
            (setq n (1+ n))
            (message "  %s" (buffer-substring-no-properties
                             (line-beginning-position) (line-end-position))))))
      n)))

(defun lint--package-lint ()
  "Return the number of package-lint complaints, or nil if it is absent."
  (when (require 'package-lint nil t)
    (require 'package)
    (unless package--initialized (package-initialize t))
    (let ((n 0))
      (dolist (f lint-sources)
        (with-temp-buffer
          (insert-file-contents f)
          (emacs-lisp-mode)
          (setq buffer-file-name (expand-file-name f))
          (unless (equal f "emjupy.el")
            (setq-local package-lint-main-file "emjupy.el"))
          (dolist (issue (ignore-errors (package-lint-buffer)))
            ;; The one dependency is vendored in CI rather than installed, so
            ;; "not installable" says nothing about the package.
            (unless (string-match-p "websocket" (format "%S" issue))
              (setq n (1+ n))
              (message "  %s: %S" f issue)))
          (setq buffer-file-name nil)))
      n)))

(let* ((doc (lint--checkdoc))
       (pkg (lint--package-lint))
       (bad (+ doc (or pkg 0))))
  (message "checkdoc: %d" doc)
  (message "package-lint: %s" (if pkg (number-to-string pkg) "not installed"))
  (kill-emacs (if (> bad 0) 1 0)))

;;; lint.el ends here
