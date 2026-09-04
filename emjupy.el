;;; emjupy.el --- Interactive Jupyter notebooks in Emacs  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Maintainer: Mathieu Renzo <mathren90@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (websocket "1.15"))
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; emjupy edits Jupyter notebooks in Emacs by talking to a running Jupyter
;; server over the same HTTP + WebSocket API the browser uses.  Because that
;; API is plain HTTP on one port, everything works unchanged through an ssh
;; tunnel -- no ZMQ ports to forward.
;;
;; Quick start:
;;
;;   M-x emjupy-login RET 8888 RET
;;
;; The port is the whole prompt; a token is only asked for when the server
;; needs one.  emjupy adopts the kernel already running behind that port, so
;; picking a notebook drops you straight into the live REPL.
;;
;; One port = one kernel: log in once per ssh tunnel and each port keeps its
;; own kernel, so several remote sessions stay live in one Emacs.
;;
;; This file defines the major mode and keymap; the implementation lives in
;; emjupy-core, emjupy-http, emjupy-render, emjupy-cells, emjupy-kernel,
;; emjupy-notebook and emjupy-eglot.

;;; Code:

(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-notebook)
(require 'emjupy-eglot)

(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Execution
    (define-key map (kbd "C-c C-c") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "M-RET")   #'emjupy-execute-cell-and-goto-next)
    (define-key map (kbd "C-c C-r") #'emjupy-execute-cell-and-goto-next)

    ;; Cell Operations (EIN style)
    (define-key map (kbd "C-c C-a") #'emjupy-insert-cell-above)
    (define-key map (kbd "C-c C-b") #'emjupy-insert-cell-below)
    (define-key map (kbd "C-c C-k") #'emjupy-delete-cell)
    (define-key map (kbd "C-c C-t") #'emjupy-cycle-cell-type)
    (define-key map (kbd "M-<up>")   #'emjupy-move-cell-up)
    (define-key map (kbd "M-<down>") #'emjupy-move-cell-down)
    (define-key map (kbd "C-c '")    #'emjupy-edit-cell-externally)

    ;; Kernel
    (define-key map (kbd "C-c C-x C-r") #'emjupy-restart-kernel)
    (define-key map (kbd "C-c C-x C-c") #'emjupy-reconnect-kernel)

    ;; Multiple notebooks / servers
    (define-key map (kbd "C-c C-x b") #'emjupy-switch-notebook)
    (define-key map (kbd "C-c C-x s") #'emjupy-status)
    (define-key map (kbd "C-c C-x l") #'emjupy-login)

    ;; Navigation
    (define-key map (kbd "C-c C-n") #'emjupy-next-cell)
    (define-key map (kbd "M-n")     #'emjupy-next-cell)
    (define-key map (kbd "C-c C-p") #'emjupy-previous-cell)
    (define-key map (kbd "M-p")     #'emjupy-previous-cell)

    ;; Persistence & Server/Kernel Connection
    (define-key map (kbd "C-x C-s") #'emjupy-save-notebook)
    (define-key map (kbd "C-c C-s") #'emjupy-save-notebook)
    (define-key map (kbd "C-c C-z") #'emjupy-connect-kernel-interactive)
    map)
  "Keymap for `emjupy-mode'.")

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map)
  ;; Paint the page: the buffer's own background becomes the canvas, and the
  ;; cell overlays paint their interiors back to the theme's normal
  ;; background -- so the gaps between cells read as the page behind them.
  ;; Remapping (rather than setting a colour here) means the faces can be
  ;; re-derived on a theme change without redrawing anything.
  (emjupy--sync-theme-colors)
  (setq-local face-remapping-alist
              (cons '(default emjupy-canvas) face-remapping-alist))
  ;; Keep the outlines matched to the window. `window-configuration-change-hook'
  ;; catches splits and manual drags; `window-size-change-functions' catches
  ;; whole-frame resizes (full-screen toggles), which do not always change the
  ;; window configuration.
  (add-hook 'window-configuration-change-hook #'emjupy--refresh-box-rules nil t)
  (add-hook 'window-size-change-functions #'emjupy--window-size-changed)
  ;; Completion/eldoc for code cells are delegated to the shared code
  ;; shadow buffer (see section 8) automatically -- no action needed
  ;; from the user beyond normal editing and the usual M-TAB/eldoc UI.
  (add-hook 'completion-at-point-functions #'emjupy--cell-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'emjupy--cell-eldoc-function nil t)
  (eldoc-mode 1))

(provide 'emjupy)
;;; emjupy.el ends here
