;;; emjupy-mode.el --- The major mode for notebook buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; `emjupy-mode', and the glue between the buffer and the layers under it:
;; completion, documentation, and the language settings a cell borrows.

;;; Code:

(require 'loadhist)                     ; `feature-file'
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-latex)
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-commands)
(require 'emjupy-kernel)
(require 'emjupy-debug)
(require 'emjupy-toc)
(require 'emjupy-widgets)
(require 'emjupy-widget-page)
(require 'emjupy-lsp)
(require 'emjupy-shadow)
(require 'emjupy-eglot)

;;;; Options

(defcustom emjupy-electric-pairs t
  "Non-nil means pair brackets and quotes as you type in cells.

`electric-pair-mode' is global and off by default, and `emjupy-mode'
derives from `fundamental-mode'; this option gives cells the pairing a
Python buffer would have.  Set it to nil to rely on your own
configuration."
  :type 'boolean
  :group 'emjupy-editing
  :safe #'booleanp)

(defcustom emjupy-language-mode 'python-mode
  "Major mode whose settings notebook cells use.

Cells are edited as code in this language.  Set it to nil to keep the
settings of `fundamental-mode'."
  :type '(choice (const :tag "None" nil) function)
  :group 'emjupy-editing)

;;;; Timing hooks

;;;; Copying text out

;;;; Keys

;; `C-c' followed by a plain letter is reserved for users (Key Binding
;; Conventions, in the Emacs Lisp manual), and package-lint reports it; the
;; README shows how to put cell commands there.
(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Commands defined in emjupy-notebook.el, which loads after this file, are
    ;; quoted, not sharp-quoted: the key looks them up when pressed.

    ;; Running cells
    (define-key map (kbd "C-c C-c") #'emjupy-execute-cell-and-goto-next)
    ;; Shift-RET runs the cell and stays put, as in other notebook front-ends.
    ;; Both spellings are bound: a terminal Emacs reports S-<return> only if
    ;; the terminal distinguishes it.
    (define-key map (kbd "S-<return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "<S-return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "C-c C-e")    #'emjupy-execute-cell-at-point)

    ;; The kernel
    ;; C-c C-z interrupts the kernel, like C-c in a terminal; choosing a
    ;; kernel, which is rarer, takes another modifier.
    (define-key map (kbd "C-c C-z") #'emjupy-interrupt-kernel)
    (define-key map (kbd "C-c M-z") #'emjupy-connect-kernel-interactive)
    (define-key map (kbd "C-c C-x C-r") #'emjupy-restart-kernel)
    (define-key map (kbd "C-c C-x C-c") #'emjupy-reconnect-kernel)

    ;; Moving among cells
    (define-key map (kbd "C-c C-n") #'emjupy-next-cell)
    (define-key map (kbd "C-c C-p") #'emjupy-previous-cell)
    (define-key map (kbd "C-c <prior>") #'emjupy-beginning-of-cell)
    (define-key map (kbd "C-c <next>")  #'emjupy-end-of-cell)
    ;; Move by lines, keeping the column and skipping separators, on
    ;; whatever keys move by lines.
    (define-key map [remap next-line] 'emjupy-next-line)
    (define-key map [remap previous-line] 'emjupy-previous-line)
    ;; Neighbouring cells, the way each key points: Up and Page Up go back,
    ;; Down and Page Down forward; arrows land at the cell's start, Page keys
    ;; at its end.
    (define-key map (kbd "M-<up>")    #'emjupy-beginning-of-previous-cell)
    (define-key map (kbd "M-<down>")  #'emjupy-beginning-of-next-cell)
    (define-key map (kbd "M-<prior>") #'emjupy-end-of-previous-cell)
    (define-key map (kbd "M-<next>")  #'emjupy-end-of-next-cell)

    ;; Changing cells
    (define-key map (kbd "C-c C-a") #'emjupy-insert-cell-above)
    (define-key map (kbd "C-c C-b") #'emjupy-insert-cell-below)
    (define-key map (kbd "C-c C-k") #'emjupy-delete-cell)
    (define-key map (kbd "C-c C-s") #'emjupy-split-cell)
    (define-key map (kbd "C-c C-j") #'emjupy-join-cell-above)
    (define-key map (kbd "C-c C-t") #'emjupy-cycle-cell-type)
    (define-key map (kbd "C-c C-w") #'emjupy-copy-cell)
    (define-key map (kbd "C-c C-y") #'emjupy-yank-cell)
    (define-key map (kbd "C-c <up>")   #'emjupy-move-cell-up)
    (define-key map (kbd "C-c <down>") #'emjupy-move-cell-down)
    (define-key map (kbd "C-c '")    #'emjupy-edit-cell-externally)
    (define-key map (kbd "TAB")     #'emjupy-indent-or-cycle)

    ;; Output, and what a cell shows
    (define-key map (kbd "C-c C-l")     #'emjupy-clear-cell-output)
    ;; A prefix of control characters, `C-c C-u C-l', not a plain letter.
    (define-key map (kbd "C-c C-u C-l") #'emjupy-clear-all-outputs)
    ;; Not `C-c h': keys of the form C-c <letter> are reserved for users.
    (define-key map (kbd "C-c C-o")     #'emjupy-toggle-cell-output)
    (define-key map (kbd "C-c C-i")     #'emjupy-toggle-cell-code)
    (define-key map (kbd "C-c C-f")     #'emjupy-open-output)
    (define-key map (kbd "C-$")     #'emjupy-show-traceback)
    ;; Deleting into a rendered formula reveals its source.  Both keys are
    ;; bound: a configuration that binds <backspace> itself never reaches DEL.
    (define-key map (kbd "DEL")         #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "<backspace>") #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "C-c C-x C-l") #'emjupy-re-render)

    ;; The notebook, its outline and its servers
    ;; Remap the save command, so any key that saves also saves the notebook.
    (define-key map [remap save-buffer] 'emjupy-save-notebook)
    (define-key map (kbd "C-c C-q")     #'emjupy-toc)
    (define-key map (kbd "C-c C-x b") 'emjupy-switch-notebook)
    (define-key map (kbd "C-c C-x s") 'emjupy-status)
    (define-key map (kbd "C-c C-x l") 'emjupy-login)
    (define-key map (kbd "C-c C-x C-e") 'emjupy-export-py)

    ;; The debugger, and the menu of everything
    (define-key map (kbd "C-c C-d")     emjupy-debug-map)
    ;; The menu.  Not C-c C-h: C-h after a prefix lists that prefix's
    ;; bindings, which the menu improves on; taking the key would remove that
    ;; fallback.
    (define-key map (kbd "C-c C-v") 'emjupy-menu)
    map)
  "Keymap for `emjupy-mode'.")

;;;; Language settings

(defun emjupy--adopt-language-settings ()
  "Copy the syntax and comment settings of `emjupy-language-mode'.
Cells are then edited as code in that language: the syntax table
decides where \\[forward-word] stops and what \\[dabbrev-expand] finds,
and the comment variables what \\[comment-dwim] does.  The settings are
read from the mode itself, run in a temporary buffer."
  (when emjupy-language-mode
    (let (table vars)
      (with-temp-buffer
        (delay-mode-hooks (funcall emjupy-language-mode))
        (setq table (syntax-table))
        (setq vars
              (mapcar (lambda (v) (cons v (and (boundp v) (symbol-value v))))
                      '(comment-start comment-end comment-start-skip
                        comment-end-skip comment-use-syntax comment-column
                        parse-sexp-ignore-comments
                        forward-sexp-function
                        electric-indent-chars
                        beginning-of-defun-function
                        end-of-defun-function))))
      (set-syntax-table table)
      (pcase-dolist (`(,var . ,value) vars)
        (set (make-local-variable var) value)))))

;;;; Two installations at once

;;;; The mode

(defun emjupy--mode-line-status ()
  "Return the kernel's status and the server's port, for the mode line.
The status is the kernel's own (idle, busy, starting) or emjupy's (no
kernel, disconnected, reconnecting)."
  (let* ((nb emjupy--buffer-notebook)
         (kernel (and nb (emjupy-notebook-kernel nb)))
         (server (and nb (emjupy-notebook-server nb)))
         (parts (and server (emjupy--server-parts server)))
         (state (cond ((not kernel) "no kernel")
                      (emjupy--reconnect-timer "reconnecting")
                      ((not (emjupy--ws-live-p kernel)) "disconnected")
                      (t (or (emjupy-kernel-state kernel) "connecting")))))
    (format "[%s%s]%s"
            (propertize state 'face
                        (pcase state
                          ("idle" nil)
                          ((or "busy" "starting" "connecting" "reconnecting")
                           'warning)
                          ("no kernel" 'shadow)
                          (_ 'error)))
            (cond ((plist-get parts :port)
                   (format " :%s" (plist-get parts :port)))
                  ((plist-get parts :host)
                   (format " %s" (plist-get parts :host)))
                  (t ""))
            ;; Mark the fallback language server: it answers from this
            ;; machine.
            (if (and (emjupy--notebook)
                     (eq (emjupy-notebook-lsp-route (emjupy--notebook)) 'local))
                (propertize " LSP: this machine" 'face 'error)
              ""))))

(defcustom emjupy-scroll-margin 2
  "Number of lines of context to keep above and below the cursor.
In notebook buffers, the window then scrolls just enough to keep the
cursor in view, instead of recentring.  This sets `scroll-margin' and
`scroll-conservatively' locally, unless you have customized them.  nil
leaves scrolling to Emacs."
  :type '(choice (integer :tag "Lines") (const :tag "Emacs's own" nil))
  :group 'emjupy)

(defun emjupy--set-scrolling ()
  "Set scrolling in this buffer according to `emjupy-scroll-margin'.
Leave `scroll-margin' and `scroll-conservatively' alone if the user has
changed them."
  (when emjupy-scroll-margin
    (when (and (eql (default-value 'scroll-conservatively) 0)
               (not (local-variable-p 'scroll-conservatively)))
      (setq-local scroll-conservatively 101))
    (when (and (eql (default-value 'scroll-margin) 0)
               (not (local-variable-p 'scroll-margin)))
      (setq-local scroll-margin emjupy-scroll-margin))))

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  ;; When shown, report the messages logged quietly while it was hidden.
  (add-hook 'window-buffer-change-functions #'emjupy--tell-unseen nil t)
  (emjupy--set-scrolling)
  ;; Keep point off separators and padding, which would show it at the
  ;; end of a border's line.
  (cursor-intangible-mode 1)
  ;; The kernel's state and the server's port beside the mode's name.
  (setq mode-name '("emjupy" (:eval (emjupy--mode-line-status))))
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map)
  (emjupy--adopt-language-settings)
  ;; Each layer adds its hooks here, not when it loads.  The order matters:
  ;; language support asks the kernel where it runs before the kernel layer
  ;; sends the width, and a notebook closes its connections after the cells
  ;; layer stops its checks.
  (emjupy-eglot-enable)
  (emjupy-kernel-enable)
  (emjupy-debug-enable)
  (emjupy-toc-enable)
  (emjupy-shadow-enable)
  (emjupy-cells-enable)
  (emjupy-widgets-enable)
  (emjupy-widget-page-enable)
  (emjupy-redraw-enable)
  ;; Cells hold code: enable what a programming mode provides and
  ;; `fundamental-mode' lacks.
  (when emjupy-electric-pairs
    (electric-pair-local-mode 1))
  (emjupy-faces-enable)
  (emjupy-render-enable)
  (emjupy-commands-enable)
  (emjupy-eglot-completion-enable))

(provide 'emjupy-mode)

;;; emjupy-mode.el ends here
