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
(require 'transient)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-widgets)
(require 'emjupy-widget-page)
(require 'emjupy-lsp)
(require 'emjupy-eglot)

;;;; Options

(defcustom emjupy-report-slow-hooks nil
  "Report anything emjupy runs after a command that takes too long.

When a number, any per-command work taking more than that many seconds
is logged with the name of the function responsible.  0.05 is a good
starting point.  This exists because a hang is hard to attribute from
the outside: it says which of emjupy's hooks is the slow one, or --
just as usefully -- that none of them is, and the time is going
somewhere else entirely."
  :type '(choice (const :tag "Off" nil) (number :tag "Seconds"))
  :group 'emjupy)

(defcustom emjupy-electric-pairs t
  "When non-nil, pair brackets and quotes as you type in cells.

`electric-pair-mode' is global and off by default, and `emjupy-mode'
derives from `fundamental-mode', so a notebook would otherwise behave
less like a Python buffer than the code in it deserves.  Set to nil to
leave the matter to your own configuration."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-language-mode 'python-mode
  "The major mode whose settings a notebook borrows for its cells.

Cells hold code in that language, so the buffer should behave as a
buffer of it does.  Set to nil to leave the buffer with
`fundamental-mode\\='s settings."
  :type '(choice (const :tag "None" nil) function)
  :group 'emjupy)

;;;; Timing hooks

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
    (apply #'emjupy--timed "eldoc" #'emjupy--cell-eldoc-function
           callback args)))

(defun emjupy--refresh-rules-timed (&rest args)
  "Refresh the cell rules, reporting if it is slow.  ARGS are passed on."
  (apply #'emjupy--timed "rule refresh" #'emjupy--refresh-box-rules args))

;;;; Copying text out

(defconst emjupy--internal-text-properties
  '(emjupy-cell emjupy-pad emjupy-overlay)
  "Text properties that belong to this buffer and must not travel.
`emjupy-cell' holds the cell itself: yanked elsewhere, the text would
claim a cell it has nothing to do with.")

(defun emjupy--strip-internal-properties (string)
  "Return STRING without emjupy's internal text properties.
Faces stay, so yanked code still looks like code."
  (let ((copy (copy-sequence string)))
    (remove-list-of-text-properties 0 (length copy)
                                    emjupy--internal-text-properties copy)
    copy))

(defun emjupy--filter-buffer-substring (beg end &optional delete)
  "Return the text between BEG and END, fit to leave this buffer.
DELETE is passed through to `buffer-substring--filter'."
  (emjupy--strip-internal-properties
   (buffer-substring--filter beg end delete)))

;;;; Keys

(defvar emjupy-mode-map (make-sparse-keymap)
  "Keymap for `emjupy-mode'.
Its keys are bound, with the menu, in emjupy-notebook.el: they name
commands of every file, so they are bound above them all.")

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

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  (emjupy--warn-about-strays)
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map)
  (emjupy--adopt-language-settings)
  ;; When a kernel attaches, the language-server layer asks it where it runs.
  ;; Wired here, where the layers meet, not on load: a package should not add
  ;; hooks by being loaded.
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--refresh-kernel-cwd)
  ;; and how wide to draw, now and whenever the window changes
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--tell-kernel-width)
  (add-hook 'emjupy-box-width-changed-functions #'emjupy--tell-kernel-width)
  ;; Closing the notebook closes the connections it opened.
  (add-hook 'kill-buffer-hook #'emjupy--release-notebook nil t)
  (emjupy-cells-enable)
  (emjupy-widgets-enable)
  (emjupy-widget-page-enable)
  ;; The number column narrows the page; the rules are redrawn to fit.
  (add-hook 'display-line-numbers-mode-hook #'emjupy--line-numbers-toggled
            nil t)
  ;; Cells hold code: the conveniences a programming mode gives, which
  ;; `fundamental-mode' does not.
  (when emjupy-electric-pairs
    (electric-pair-local-mode 1))
  ;; Output backgrounds, from the theme.
  (emjupy--sync-theme-colors)
  ;; Outlines follow the window: `window-configuration-change-hook' for splits
  ;; and drags, `window-size-change-functions' for frame resizes.
  (add-hook 'after-change-functions #'emjupy--refontify-after-change nil t)
  (add-hook 'window-configuration-change-hook #'emjupy--refresh-rules-timed
            nil t)
  (add-hook 'window-size-change-functions #'emjupy--window-size-changed)
  ;; Completion and eldoc in code cells, through the shadow buffer: over the
  ;; Jupyter server's `jupyter-lsp' first, a local server otherwise.
  (add-hook 'completion-at-point-functions #'emjupy--capf nil t)
  (add-hook 'eldoc-documentation-functions #'emjupy--eldoc nil t)
  ;; M-. and friends: Eglot's xref backend lives in the shadow buffer, so the
  ;; notebook needs a backend of its own that forwards there.
  (add-hook 'xref-backend-functions #'emjupy--xref-backend nil t)
  (add-hook 'pre-command-hook #'emjupy--clear-indent-cycling nil t)
  ;; Everything leaving this buffer -- kill, copy, `M-w' -- goes through
  ;; here, so the cell struct never reaches the kill ring.
  (setq-local filter-buffer-substring-function
              #'emjupy--filter-buffer-substring)
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
