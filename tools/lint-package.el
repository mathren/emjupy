;;; lint-package.el --- package-lint, as MELPA runs it  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; cd src && emacs -batch -Q -l ../tools/lint-package.el FILE...
;;
;; package-lint's own batch entry point, warnings failing too, from the
;; source directory so the main file .dir-locals.el names resolves.  It
;; fails -- it does not skip -- when package-lint is not installed, or when
;; no package archive lists the dependencies, which it checks are
;; installable.  The archive is MELPA, as the dependencies come from it, or
;; the one EMJUPY_PACKAGE_ARCHIVE names, such as a local MELPA build.

;;; Code:

(require 'package)

(setq package-archives
      (if (getenv "EMJUPY_PACKAGE_ARCHIVE")
          `(("local" . ,(getenv "EMJUPY_PACKAGE_ARCHIVE")))
        '(("gnu" . "https://elpa.gnu.org/packages/")
          ("melpa" . "https://melpa.org/packages/"))))
(package-initialize)

(unless (require 'package-lint nil t)
  (message "package-lint is not installed: install it from MELPA, or set PACKAGE_LINT_DIR to a checkout of it")
  (kill-emacs 2))

(unless package-archive-contents
  (package-refresh-contents))
(unless package-archive-contents
  (message "No package archive could be read, so package-lint cannot check the dependencies are installable")
  (kill-emacs 2))

(setq package-lint-batch-fail-on-warnings t)
(package-lint-batch-and-exit)

;;; lint-package.el ends here
