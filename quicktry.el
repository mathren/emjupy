;;; quicktry.el --- Isolated test harness for emjupy -*- lexical-binding: t; -*-

;; Run with: emacs -Q --batch -l quicktry.el

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

;; Load local emjupy.el
(add-to-list 'load-path default-directory)
(require 'emjupy)


;;; quicktry.el ends here
