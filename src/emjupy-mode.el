;;; emjupy-mode.el --- The major mode for notebook buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;;; Commentary:

;; `emjupy-mode', its keymap and menu, and the glue between the buffer and
;; the layers under it: completion, documentation, and the language
;; settings a cell borrows.  Above everything that draws or runs cells,
;; and below the notebook browser, which opens buffers in this mode.

;;; Code:

(require 'transient)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-figures)
(require 'emjupy-widgets)
(require 'emjupy-widget-page)
(require 'emjupy-lsp)
(require 'emjupy-eglot)

(defcustom emjupy-report-slow-hooks nil
  "Report anything emjupy runs after a command that takes too long.

When a number, any per-command work taking more than that many seconds
is logged with the name of the function responsible.  0.05 is a good
starting point.  This exists because a hang is hard to attribute from
the outside: it says which of emjupy\='s hooks is the slow one, or --
just as usefully -- that none of them is, and the time is going
somewhere else entirely."
  :type '(choice (const :tag "Off" nil) (number :tag "Seconds"))
  :group 'emjupy)

(defun emjupy--timed (name fn &rest args)
  "Call FN with ARGS, reporting if it takes longer than allowed.
NAME is what gets reported."
  (if (not (numberp emjupy-report-slow-hooks))
      (apply fn args)
    (let* ((start (float-time))
           (value (apply fn args))
           (elapsed (- (float-time) start)))
      (when (> elapsed emjupy-report-slow-hooks)
        (message "[emjupy] %s took %.0f ms" name (* 1000 elapsed)))
      value)))

(defun emjupy--capf ()
  "Completion for the cell at point, on whichever transport is available."
  (when emjupy-language-support
    (emjupy--timed "completion" #'emjupy--cell-completion-at-point)))

(defun emjupy--eldoc (callback &rest args)
  "Report documentation for the cell at point to CALLBACK, passing ARGS on."
  (when emjupy-language-support
    (apply #'emjupy--timed "eldoc" #'emjupy--cell-eldoc-function callback args)))

(defun emjupy--refresh-rules-timed (&rest args)
  "Refresh the cell rules, reporting if it is slow.  ARGS are passed on."
  (apply #'emjupy--timed "rule refresh" #'emjupy--refresh-box-rules args))

(defconst emjupy--internal-text-properties
  '(emjupy-cell emjupy-pad emjupy-overlay)
  "Text properties that belong to this buffer and must not travel.

`emjupy-cell' is the worst of them: it holds the cell STRUCT, so text
copied out of a cell carries a reference to that whole cell -- its
outputs, its overlay, its hash tables -- and the reference is a snapshot
that stops being true the moment the cell changes.  Yank it somewhere
else and the pasted text claims to belong to a cell it has nothing to do
with, which `emjupy--cell-at-point' reads as a fallback when no overlay
covers the position.")

(defun emjupy--strip-internal-properties (string)
  "Return STRING without emjupy's internal text properties.

Faces and the like are kept: they are what makes yanked code still look
like code.  Only the properties that mean something to emjupy alone, and
only inside the buffer they came from, are removed."
  (let ((copy (copy-sequence string)))
    (remove-list-of-text-properties 0 (length copy)
                                    emjupy--internal-text-properties copy)
    copy))

(defun emjupy--filter-buffer-substring (beg end &optional delete)
  "Return the text between BEG and END, fit to leave this buffer.
DELETE is passed through to `buffer-substring--filter'."
  (emjupy--strip-internal-properties
   (buffer-substring--filter beg end delete)))

;; `C-c\' followed by a plain letter is reserved for USERS by the Emacs Lisp
;; manual (Key Binding Conventions), so a package may not take `C-c s\' or
;; `C-c w\'; package-lint reports it as an error.  Those keys are left free
;; for you -- see the README for a snippet that puts the cell commands there.
(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Execution
    (define-key map (kbd "C-c C-c") #'emjupy-execute-cell-and-goto-next)
    ;; Shift-RET runs the cell and stays put, the way a notebook front-end
    ;; does. Bound in both spellings: a terminal Emacs reports it as
    ;; "S-<return>" only if the terminal distinguishes it at all.
    (define-key map (kbd "S-<return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "<S-return>") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "C-c C-e")    #'emjupy-execute-cell-at-point)

    ;; Cell Operations (EIN style)
    (define-key map (kbd "C-c C-a") #'emjupy-insert-cell-above)
    (define-key map (kbd "C-c C-b") #'emjupy-insert-cell-below)
    (define-key map (kbd "C-c C-k") #'emjupy-delete-cell)
    (define-key map (kbd "C-c C-l")     #'emjupy-clear-cell-output)
    ;; Not `C-c h': keys of the form C-c <letter> are reserved for users.
    (define-key map (kbd "C-c C-o")     #'emjupy-toggle-cell-output)
    (define-key map (kbd "C-c C-f")     #'emjupy-open-output)
    ;; A prefix, not a plain letter: `C-c' followed by a letter is reserved
    ;; for users, and package-lint -- which MELPA requires to be clean --
    ;; reports any such binding as an error.  `C-c C-u C-l' is all control
    ;; characters and so belongs to the mode.
    (define-key map (kbd "C-c C-u C-l") #'emjupy-clear-all-outputs)
    (define-key map (kbd "C-c C-s") #'emjupy-split-cell)
    (define-key map (kbd "C-c C-j") #'emjupy-join-cell-above)
    (define-key map (kbd "C-c C-t") #'emjupy-cycle-cell-type)
    (define-key map (kbd "TAB")     #'emjupy-indent-or-cycle)
    ;; Deleting against a rendered formula reveals it rather than removing
    ;; a character that cannot be seen.
    ;; Both spellings: a graphical Emacs sends <backspace>, and although it
    ;; normally translates to DEL, a configuration that binds <backspace>
    ;; itself takes precedence and the DEL binding is never reached.
    (define-key map (kbd "DEL")         #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "<backspace>") #'emjupy-latex-unrender-or-delete)
    (define-key map (kbd "C-$")     #'emjupy-show-traceback)
    (define-key map (kbd "C-c C-w") #'emjupy-copy-cell)
    (define-key map (kbd "C-c C-y") #'emjupy-yank-cell)
    (define-key map (kbd "C-c <up>")   #'emjupy-move-cell-up)
    (define-key map (kbd "C-c <down>") #'emjupy-move-cell-down)
    (define-key map (kbd "C-c '")    #'emjupy-edit-cell-externally)

    ;; Kernel
    (define-key map (kbd "C-c C-x C-r") #'emjupy-restart-kernel)
    (define-key map (kbd "C-c C-x C-c") #'emjupy-reconnect-kernel)

    ;; Multiple notebooks / servers
    (define-key map (kbd "C-c C-x b") 'emjupy-switch-notebook)
    (define-key map (kbd "C-c C-x s") 'emjupy-status)
    (define-key map (kbd "C-c C-x l") 'emjupy-login)
    (define-key map (kbd "C-c C-x C-e") 'emjupy-export-py)

    ;; Navigation
    (define-key map (kbd "C-c C-n") #'emjupy-next-cell)
    (define-key map (kbd "C-c C-p") #'emjupy-previous-cell)
    (define-key map (kbd "C-c <prior>") #'emjupy-beginning-of-cell)
    ;; Same axis one level out: C-c moves within this cell, M- moves to a
    ;; neighbouring one.
    (define-key map (kbd "M-<prior>") #'emjupy-beginning-of-previous-cell)
    (define-key map (kbd "M-<next>")  #'emjupy-end-of-previous-cell)
    (define-key map (kbd "M-<up>")    #'emjupy-beginning-of-next-cell)
    (define-key map (kbd "M-<down>")  #'emjupy-end-of-next-cell)
    (define-key map (kbd "C-c C-x C-l") #'emjupy-re-render)
    (define-key map (kbd "C-c <next>")  #'emjupy-end-of-cell)

    ;; Persistence & Server/Kernel Connection
    (define-key map (kbd "C-x C-s") 'emjupy-save-notebook)
    ;; C-c C-z stops what is running, like C-c in a terminal; picking a
    ;; kernel is the rarer act and moves one modifier away.
    (define-key map (kbd "C-c C-z") #'emjupy-interrupt-kernel)
    (define-key map (kbd "C-c M-z") #'emjupy-connect-kernel-interactive)
    ;; The menu.  Not `C-c C-h': `C-h' after a prefix is reserved for the
    ;; list of that prefix's bindings, which is the very thing this improves
    ;; on, so taking the key would remove the fallback.
    (define-key map (kbd "C-c C-v") #'emjupy-menu)
    map)
  "Keymap for `emjupy-mode'.")

(defcustom emjupy-electric-pairs t
  "When non-nil, pair brackets and quotes as you type in cells.

`electric-pair-mode\=' is global and off by default, and `emjupy-mode\='
derives from `fundamental-mode\=', so a notebook would otherwise behave
less like a Python buffer than the code in it deserves.  Set to nil to
leave the matter to your own configuration."
  :type 'boolean
  :group 'emjupy)

;;;###autoload (autoload 'emjupy-menu "emjupy" nil t)
(transient-define-prefix emjupy-menu ()
  "Show what emjupy can do, grouped by what you are trying to do.

There are some forty bindings, and `C-h m' lists them in the order they
were defined rather than the order anyone thinks in.  This groups them,
and can be used as a menu in its own right: everything here runs from
this buffer."
  [:description
   (lambda ()
     (let ((nb (and (bound-and-true-p emjupy--buffer-notebook)
                    emjupy--buffer-notebook)))
       (if nb
           (format "emjupy  %s"
                   (or (emjupy-notebook-path nb) "unsaved"))
         "emjupy")))
   ["Run"
    ("c" "this cell, then move on" emjupy-execute-cell-and-goto-next)
    ("e" "this cell, stay here" emjupy-execute-cell-at-point)
    ("z" "interrupt the kernel" emjupy-interrupt-kernel)
    ("$" "show the last traceback" emjupy-show-traceback)]
   ["Cells"
    ("a" "new cell above" emjupy-insert-cell-above)
    ("b" "new cell below" emjupy-insert-cell-below)
    ("k" "delete this cell" emjupy-delete-cell)
    ("s" "split here" emjupy-split-cell)
    ("j" "join to the one above" emjupy-join-cell-above)
    ("t" "code or markdown" emjupy-cycle-cell-type)
    ("w" "copy this cell" emjupy-copy-cell)
    ("y" "paste a cell" emjupy-yank-cell)]
   ["Output"
    ("o" "hide or show this output" emjupy-toggle-cell-output)
    ("f" "open this output on its own" emjupy-open-output)
    ("l" "clear this output" emjupy-clear-cell-output)
    ("L" "clear every output" emjupy-clear-all-outputs)
    ("r" "redraw the notebook" emjupy-re-render)]]
  [["Moving"
    ("n" "next cell" emjupy-next-cell)
    ("p" "previous cell" emjupy-previous-cell)
    ("<" "start of this cell" emjupy-beginning-of-cell)
    (">" "end of this cell" emjupy-end-of-cell)
    ("M-p" "start of the previous cell" emjupy-beginning-of-previous-cell)
    ("M-n" "start of the next cell" emjupy-beginning-of-next-cell)
    ("M-P" "end of the previous cell" emjupy-end-of-previous-cell)
    ("M-N" "end of the next cell" emjupy-end-of-next-cell)
    ("<up>" "move this cell up" emjupy-move-cell-up)
    ("<down>" "move this cell down" emjupy-move-cell-down)]
   ["Notebook"
    ("S" "save to the server" emjupy-save-notebook)
    ("B" "switch notebook" emjupy-switch-notebook)
    ("X" "export as .py" emjupy-export-py)
    ("'" "edit this cell in a buffer" emjupy-edit-cell-externally)]]
  ;; A row of its own: four groups side by side came to some 120 columns,
  ;; so in an ordinary frame the last column wrapped mid-word.
  [["Server"
    ("g" "log in to a server" emjupy-login)
    ("R" "restart the kernel" emjupy-restart-kernel)
    ("C" "reconnect the kernel" emjupy-reconnect-kernel)
    ("K" "choose a kernel" emjupy-connect-kernel-interactive)
    ("?" "what is going on" emjupy-status)]
   ["Language server"
    ("d" "diagnose it" emjupy-lsp-diagnose)
    ("v" "which emjupy is this" emjupy-version)]])

(defcustom emjupy-language-mode 'python-mode
  "The major mode whose settings a notebook borrows for its cells.

Cells hold code in that language, so the buffer should behave as a
buffer of it does.  Set to nil to leave the buffer with
`fundamental-mode\\='s settings."
  :type '(choice (const :tag "None" nil) function)
  :group 'emjupy)

(defun emjupy--adopt-language-settings ()
  "Take the syntax and comment settings of `emjupy-language-mode\\='.

The aim is that editing a cell is editing that language, without
emulating one command at a time.  Most of what a programming mode gives
is not commands at all: it is the syntax table, which decides what
counts as a word or a symbol and so what \\[dabbrev-expand] will find
and where \\[forward-word] stops; and the comment variables, without
which \\[comment-dwim] has no comment syntax to use and says so.

Taken by running the mode in a scratch buffer and copying what it set,
rather than by listing the variables here.  A list would be a second
place to keep up to date, and would be wrong the moment the mode changed
-- which is the same trap as emulating the commands."
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

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map)
  (emjupy--adopt-language-settings)
  ;; When a kernel attaches, the language-server layer asks it where it
  ;; runs.  Wired here, where the layers meet, rather than by either file on
  ;; load: the kernel layer cannot name the one above it, and a package
  ;; should not add hooks just by being loaded.  `add-hook\=' does nothing
  ;; if it is already there.
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--refresh-kernel-cwd)
  ;; and how wide to draw, now and whenever the window changes
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--tell-kernel-width)
  (add-hook 'emjupy-box-width-changed-functions #'emjupy--tell-kernel-width)
  ;; Closing the notebook closes the connections it opened.
  (add-hook 'kill-buffer-hook #'emjupy--release-notebook nil t)
  (emjupy-widgets-enable)
  (emjupy-widget-page-enable)
  ;; The number column narrows the page; the rules are redrawn to fit.
  (add-hook 'display-line-numbers-mode-hook #'emjupy--line-numbers-toggled nil t)
  ;; Cells hold code, so the editing conveniences a programming mode would
  ;; give apply here too.  emjupy-mode derives from `fundamental-mode', which
  ;; brings none of them, and nothing about the buffer suggests to the user
  ;; that they have to be asked for.
  (when emjupy-electric-pairs
    (electric-pair-local-mode 1))
  ;; Paint the page: the buffer's own background becomes the canvas, and the
  ;; cell overlays paint their interiors back to the theme's normal
  ;; background -- so the gaps between cells read as the page behind them.
  ;; Remapping (rather than setting a colour here) means the faces can be
  ;; re-derived on a theme change without redrawing anything.
  (emjupy--sync-theme-colors)
  ;; Keep the outlines matched to the window. `window-configuration-change-hook'
  ;; catches splits and manual drags; `window-size-change-functions' catches
  ;; whole-frame resizes (full-screen toggles), which do not always change the
  ;; window configuration.
  (add-hook 'after-change-functions #'emjupy--refontify-after-change nil t)
  (add-hook 'window-configuration-change-hook #'emjupy--refresh-rules-timed nil t)
  (add-hook 'window-size-change-functions #'emjupy--window-size-changed)
  ;; Completion/eldoc for code cells are delegated to the shared code
  ;; shadow buffer (see section 8) automatically -- no action needed
  ;; from the user beyond normal editing and the usual M-TAB/eldoc UI.
  ;; The Jupyter-server transport first: no file, no TRAMP, and the server
  ;; sits next to the kernel.  The shadow-file path stays as the fallback for
  ;; a server without `jupyter-lsp'.
  (add-hook 'completion-at-point-functions #'emjupy--capf nil t)
  (add-hook 'eldoc-documentation-functions #'emjupy--eldoc nil t)
  ;; M-. and friends: Eglot's xref backend lives in the shadow buffer, so the
  ;; notebook needs a backend of its own that forwards there.
  (add-hook 'xref-backend-functions #'emjupy--xref-backend nil t)
  (add-hook 'pre-command-hook #'emjupy--clear-indent-cycling nil t)
  ;; Everything leaving this buffer -- kill, copy, `M-w' -- goes through
  ;; here, so the cell struct never reaches the kill ring.
  (setq-local filter-buffer-substring-function #'emjupy--filter-buffer-substring)
  ;; And nothing arriving carries one either, for text killed before this
  ;; was in place or copied from a notebook in an older session.
  (setq-local yank-excluded-properties
              (append emjupy--internal-text-properties
                      (if (listp yank-excluded-properties)
                          yank-excluded-properties
                        nil)))
  (eldoc-mode 1))

(provide 'emjupy-mode)

;;; emjupy-mode.el ends here
