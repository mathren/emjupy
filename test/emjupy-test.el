;;; emjupy-test.el --- The unit tests, all of them  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests: fast, needing nothing external.  The few that need a real
;; Python language server on PATH -- pylsp, pyright, jedi-language-server --
;; skip without one.  They are in emjupy-*-test.el, one file for each file of
;; src/, what they share in emjupy-test-helpers.el; this file loads them all.

;;; Code:

(require 'emjupy-test-helpers)
(require 'emjupy-package-test)
(require 'emjupy-cells-test)
(require 'emjupy-commands-test)
(require 'emjupy-core-test)
(require 'emjupy-dashboard-test)
(require 'emjupy-eglot-test)
(require 'emjupy-faces-test)
(require 'emjupy-figures-test)
(require 'emjupy-http-test)
(require 'emjupy-ipynb-test)
(require 'emjupy-kernel-test)
(require 'emjupy-latex-test)
(require 'emjupy-lsp-test)
(require 'emjupy-mode-test)
(require 'emjupy-notebook-test)
(require 'emjupy-redraw-test)
(require 'emjupy-remote-test)
(require 'emjupy-render-test)
(require 'emjupy-save-test)
(require 'emjupy-shadow-test)
(require 'emjupy-tramp-open-test)
(require 'emjupy-undo-test)
(require 'emjupy-widget-page-test)
(require 'emjupy-widgets-test)


(provide 'emjupy-test)

;;; emjupy-test.el ends here
