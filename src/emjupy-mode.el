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
  "When non-nil, pair brackets and quotes as you type in cells.

`electric-pair-mode' is global and off by default, and `emjupy-mode'
derives from `fundamental-mode', so a notebook would otherwise behave
less like a Python buffer than the code in it deserves.  Set to nil to
leave the matter to your own configuration."
  :type 'boolean
  :group 'emjupy-editing
  :safe #'booleanp)

(defcustom emjupy-language-mode 'python-mode
  "The major mode whose settings a notebook borrows for its cells.

Cells hold code in that language, so the buffer should behave as a
buffer of it does.  Set to nil to leave the buffer with
`fundamental-mode\\='s settings."
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
    ;; Commands of the files above this one, in emjupy-notebook.el, are named
    ;; with a plain quote: a binding is resolved when the key is pressed.

    ;; Running cells
    (define-key map (kbd "C-c C-c") #'emjupy-execute-cell-and-goto-next)
    ;; Shift-RET runs the cell and stays put, the way a notebook front-end
    ;; does. Bound in both spellings: a terminal Emacs reports it as
    ;; "S-<return>" only if the terminal distinguishes it at all.
    (define-key map (kbd "S-<return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "<S-return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "C-c C-e")    #'emjupy-execute-cell-at-point)

    ;; The kernel
    ;; C-c C-z stops what is running, like C-c in a terminal; picking a
    ;; kernel is the rarer act and moves one modifier away.
    (define-key map (kbd "C-c C-z") #'emjupy-interrupt-kernel)
    (define-key map (kbd "C-c M-z") #'emjupy-connect-kernel-interactive)
    (define-key map (kbd "C-c C-x C-r") #'emjupy-restart-kernel)
    (define-key map (kbd "C-c C-x C-c") #'emjupy-reconnect-kernel)

    ;; Moving among cells
    (define-key map (kbd "C-c C-n") #'emjupy-next-cell)
    (define-key map (kbd "C-c C-p") #'emjupy-previous-cell)
    (define-key map (kbd "C-c <prior>") #'emjupy-beginning-of-cell)
    (define-key map (kbd "C-c <next>")  #'emjupy-end-of-cell)
    ;; Same axis one level out: C-c moves within this cell, M- moves to a
    ;; neighbouring one.
    (define-key map (kbd "M-<prior>") #'emjupy-beginning-of-previous-cell)
    (define-key map (kbd "M-<next>")  #'emjupy-end-of-previous-cell)
    (define-key map (kbd "M-<up>")    #'emjupy-beginning-of-next-cell)
    (define-key map (kbd "M-<down>")  #'emjupy-end-of-next-cell)

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
    ;; Deleting against a rendered formula reveals it.  Both spellings: a
    ;; configuration binding <backspace> itself would never reach DEL.
    (define-key map (kbd "DEL")         #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "<backspace>") #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "C-c C-x C-l") #'emjupy-re-render)

    ;; The notebook, its outline and its servers
    ;; The command, not its key: whichever key saves, saves the notebook.
    (define-key map [remap save-buffer] 'emjupy-save-notebook)
    (define-key map (kbd "C-c C-q")     #'emjupy-toc)
    (define-key map (kbd "C-c C-x b") 'emjupy-switch-notebook)
    (define-key map (kbd "C-c C-x s") 'emjupy-status)
    (define-key map (kbd "C-c C-x l") 'emjupy-login)
    (define-key map (kbd "C-c C-x C-e") 'emjupy-export-py)

    ;; The debugger, and the menu of everything
    (define-key map (kbd "C-c C-d")     emjupy-debug-map)
    ;; The menu.  Not `C-c C-h': `C-h' after a prefix is reserved for the
    ;; list of that prefix's bindings, which is the very thing this improves
    ;; on, so taking the key would remove the fallback.
    (define-key map (kbd "C-c C-v") 'emjupy-menu)
    map)
  "Keymap for `emjupy-mode'.")

;;;; Language settings

(defun emjupy--adopt-language-settings ()
  "Take the syntax and comment settings of `emjupy-language-mode'.
So a cell is edited as that language: the syntax table decides where
\\[forward-word] stops and what \\[dabbrev-expand] finds, the comment
variables what \\[comment-dwim] does.  Copied from the mode run in a
scratch buffer, not listed here, so they follow the mode."
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

(defconst emjupy--home
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "The directory this file was loaded from, where every emjupy file belongs.")

(defconst emjupy--modules
  '(emjupy-core emjupy-http emjupy-render emjupy-cells emjupy-kernel
    emjupy-figures emjupy-widgets emjupy-widget-page emjupy-remote emjupy-lsp
    emjupy-eglot emjupy-mode emjupy-notebook emjupy)
  "The files emjupy is made of, as features.")

(defun emjupy--stray-modules ()
  "Return the emjupy files loaded from a directory other than this one's.
Each is a cons of the feature and the file.  A package and a checkout
both on the `load-path' give a mix, which fails in ways that look like
bugs."
  (cl-loop for feature in emjupy--modules
           for file = (and (featurep feature) (feature-file feature))
           when (and file (not (file-equal-p (file-name-directory file)
                                             emjupy--home)))
           collect (cons feature file)))

(defvar emjupy--strays-checked nil
  "Non-nil once this session has looked for a mixed installation.")

(defun emjupy--warn-about-strays ()
  "Warn, once a session, if emjupy's files come from two installations.
Run when a notebook first opens rather than when emjupy is loaded, so
that loading it does nothing but define, and every file is loaded by
then however emjupy was -- by a `require', or an autoloaded command."
  (unless emjupy--strays-checked
    (setq emjupy--strays-checked t)
    (when-let* ((strays (emjupy--stray-modules)))
      (display-warning
       'emjupy
       (format "emjupy is loaded from two places: %s, but %s.  Two installations
are on the `load-path' -- remove one, or put the one you want first."
               emjupy--home
               (mapconcat (lambda (s) (format "%s from %s" (car s) (cdr s)))
                          strays ", "))
       :warning))))

;;;; The mode

(defun emjupy--mode-line-status ()
  "What the kernel is doing, and the port of its server, for the mode line.
The kernel's own word -- idle, busy, starting -- or what emjupy knows:
no kernel, disconnected, reconnecting."
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
            ;; The fallback, plain to see: its answers are this machine's.
            (if (eq emjupy--lsp-route 'local)
                (propertize " LSP: this machine" 'face 'error)
              ""))))

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  (emjupy--warn-about-strays)
  ;; The kernel's state and the server's port beside the mode's name.
  (setq mode-name '("emjupy" (:eval (emjupy--mode-line-status))))
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map)
  (emjupy--adopt-language-settings)
  ;; Each layer wires its own hooks, here rather than when it loads.  The
  ;; order keeps each hook's: language support asks a kernel where it runs
  ;; before the kernel layer tells it the width, and a notebook releases its
  ;; connections after the cells layer stops its checks.
  (emjupy-eglot-enable)
  (emjupy-kernel-enable)
  (emjupy-debug-enable)
  (emjupy-toc-enable)
  (emjupy-shadow-enable)
  (emjupy-cells-enable)
  (emjupy-widgets-enable)
  (emjupy-widget-page-enable)
  (emjupy-redraw-enable)
  ;; Cells hold code: the conveniences a programming mode gives, which
  ;; `fundamental-mode' does not.
  (when emjupy-electric-pairs
    (electric-pair-local-mode 1))
  (emjupy-faces-enable)
  (emjupy-render-enable)
  (emjupy-commands-enable)
  (emjupy-eglot-completion-enable))

(provide 'emjupy-mode)

;;; emjupy-mode.el ends here
