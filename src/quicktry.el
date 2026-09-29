;;; quicktry.el --- Isolated test harness for emjupy -*- lexical-binding: t; -*-

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs

;; This file is not part of the package: it is a development tool, and the
;; MELPA recipe lists the files that ship.

;; Run from the repository root with: emacs -Q -l src/quicktry.el

(require 'package)
(setq package-user-dir (expand-file-name "/tmp/emjupy-test-packages"))

;; Add MELPA to resolve the 's' dependency required by websocket-test.el
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install websocket dependency into the temporary user dir
(unless (package-installed-p 'websocket)
  (package-refresh-contents)
  (package-install 'websocket))

;; Load emjupy from the directory this file is in, not from wherever Emacs
;; was started: `emacs -Q -L src -l src/quicktry.el' from the repository
;; root works as well as starting in src/.
(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(require 'emjupy)


;;; quicktry.el ends here
