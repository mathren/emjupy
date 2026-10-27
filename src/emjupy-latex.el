;;; emjupy-latex.el --- LaTeX in markdown cells, as images  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; LaTeX in markdown cells, shown as images over the source, which stays
;; in the buffer and is what is saved.  org renders the images and is
;; loaded only when a preview is asked for.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emjupy-core)

;; org is loaded only when a LaTeX preview is asked for: declared, not
;; required, for the compiler.
(declare-function org-create-formula-image "org")
(defvar org-format-latex-options)
(defvar org-format-latex-header)
(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)
(defvar org-preview-latex-process-alist)

;;;; Options

(defcustom emjupy-render-latex nil
  "When non-nil, show math in markdown cells as rendered images.

Needs a working LaTeX installation (`latex' plus `dvipng'), or
`math-preview' on PATH.  Nothing else: the rendering itself is done by
org, which comes with Emacs."
  :type 'boolean
  :group 'emjupy-appearance
  :safe #'booleanp)

(defcustom emjupy-latex-backend 'auto
  "How a LaTeX fragment is turned into an image.

`auto' prefers org's built-in preview, since it needs no Emacs package
beyond what ships with Emacs, and falls back to `math-preview'."
  :type '(choice (const :tag "Whatever is available" auto)
                 (const :tag "org's built-in preview" org)
                 (const :tag "math-preview" math-preview))
  :group 'emjupy-appearance)

(defcustom emjupy-latex-foreground 'auto
  "Colour to draw rendered math in.

`auto' (the default) follows the theme, taking the foreground of the
`default' face -- so formulae are dark on a light theme and light on a
dark one, instead of always black on a transparent background.

A colour string is used verbatim."
  :type '(choice (const :tag "Follow the theme" auto)
                 (color :tag "Explicit colour"))
  :group 'emjupy-appearance
  :safe #'emjupy--colour-option-safe-p)

(defcustom emjupy-latex-scale 1.0
  "Scale factor for rendered math images."
  :type 'float
  :group 'emjupy-appearance
  :safe #'numberp)

;;;; LaTeX

(defconst emjupy--latex-delimiters
  '(("\\\\\\[" . "\\\\\\]")
    ("\\$\\$"  . "\\$\\$")
    ("\\\\("   . "\\\\)")
    ("\\$"     . "\\$"))
  "Opening/closing math delimiter regexps, longest first.
`$$' has to be tried before `$', or a display block reads as two empty
inline ones.")

(defconst emjupy--latex-process
  '(dvipng :programs ("latex" "dvipng")
           :description "dvi > png"
           :message "you need latex and dvipng"
           :image-input-type "dvi"
           :image-output-type "png"
           :image-size-adjust (1.0 . 1.0)
           :latex-compiler
           ("latex -no-shell-escape -interaction nonstopmode -halt-on-error -output-directory %o %f")
           :image-converter ("dvipng -D %D -T tight -o %O %f")
           :transparent-image-converter
           ("dvipng -D %D -T tight -bg Transparent -o %O %f"))
  "How a notebook's LaTeX is made an image: emjupy's, whatever Org's says.
LaTeX runs with no shell escape: a notebook's markdown, someone else's
perhaps, could otherwise run a program merely by being shown.")

(defun emjupy--latex-fragments (start end)
  "Return math fragments between START and END as (BEG END BODY) triples.
BEG and END span the delimiters too, so an overlay across them replaces
the whole fragment with its image."
  (let ((found nil))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((hit nil)
              (here (point)))
          (cl-loop for (open . close) in emjupy--latex-delimiters
                   until hit
                   do (when (looking-at open)
                        (let ((body-start (match-end 0)))
                          (save-excursion
                            (goto-char body-start)
                            (when (re-search-forward close end t)
                              (setq hit (list here (point)
                                              (buffer-substring-no-properties
                                               body-start
                                               (match-beginning 0)))))))))
          (if hit
              (progn (push hit found) (goto-char (nth 1 hit)))
            (forward-char 1)))))
    (nreverse found)))

(defun emjupy--latex-available-p ()
  "Return the backend that can actually render math here, or nil."
  (pcase emjupy-latex-backend
    ('math-preview (and emjupy-probe-environment
                        (executable-find "math-preview") 'math-preview))
    ('org (and emjupy-probe-environment (executable-find "latex") 'org))
    (_ (cond ((not emjupy-probe-environment) nil)
             ((executable-find "latex") 'org)
             ((executable-find "math-preview") 'math-preview)
             (t nil)))))

(defun emjupy--latex-math (fragment)
  "Return FRAGMENT as LaTeX in math mode.
A fragment with its delimiters -- $...$, $$...$$, \\(...\\), \\[...\\]
-- is used as written; a bare body is taken as inline math."
  (if (string-match-p "\\`[ \t\n]*\\(?:\\$\\|\\\\[[(]\\)" fragment)
      fragment
    (concat "$" fragment "$")))

(defun emjupy--latex-image (body)
  "Render BODY, a LaTeX math fragment, to an image spec, or nil.
BODY may bring its delimiters or not; see `emjupy--latex-math'."
  (when (and (eq (emjupy--latex-available-p) 'org)
             (require 'org nil 'noerror))
    (let* ((dir (expand-file-name (format "emjupy-latex-%s/" (user-login-name))
                                  temporary-file-directory))
           (_private (emjupy--private-directory dir "LaTeX images"))
           (fg (if (stringp emjupy-latex-foreground)
                   emjupy-latex-foreground
                 (face-attribute 'default :foreground nil t)))
           ;; The colour is part of the cache key.  Without it a formula
           ;; rendered under one theme would be served back unchanged after
           ;; switching to another -- black glyphs on a dark background.
           (file (expand-file-name
                  (concat (md5 (format "%s-%s-%s" (emjupy--latex-math body)
                                       emjupy-latex-scale fg))
                          ".png")
                  dir))
           ;; Deliberately NOT org's full preamble.  That pulls in packages a
           ;; minimal TeX install does not have -- ulem, for one -- and then
           ;; every formula fails for want of something no formula needs.
           (org-latex-default-packages-alist '(("" "amsmath" t) (""
                                                                 "amssymb" t)))
           (org-latex-packages-alist nil)
           (org-format-latex-header
            (concat "\\documentclass{article}\n"
                    "\\usepackage[usenames]{color}\n"
                    "[PACKAGES]\n[DEFAULT-PACKAGES]\n"
                    "\\pagestyle{empty}"))
           (opts (copy-sequence org-format-latex-options)))
      (setq opts (plist-put opts :scale emjupy-latex-scale))
      (when (and fg (stringp fg))
        (setq opts (plist-put opts :foreground fg)))
      ;; Transparent: given a buffer, org would bake the theme's background into
      ;; the image, wrong on anything else.  The foreground carries the
      ;; contrast.
      (setq opts (plist-put opts :background "Transparent"))
      (make-directory dir t)
      (condition-case err
          (progn
            ;; Cached by content hash: dragging a slider over a notebook full
            ;; of formulae should not re-run LaTeX for each redraw.
            (unless (file-exists-p file)
              ;; The buffer is needed: without it org reads the :html- options
              ;; instead, black on transparent, and ignores
              ;; `emjupy-latex-scale'.
              (let ((org-preview-latex-process-alist
                     (list emjupy--latex-process)))
                (org-create-formula-image (emjupy--latex-math body) file opts
                                          (current-buffer) 'dvipng)))
            (when (and (file-exists-p file) (emjupy--image-displayable-p 'png))
              (create-image file 'png nil :ascent 'center)))
        ;; Everything: Org reports a failed LaTeX run as a plain error.
        (error
         (message "[emjupy] LaTeX preview failed: %s"
                  (error-message-string err))
         nil)))))

(defun emjupy--preview-latex-in (start end)
  "Lay rendered images over the math between START and END."
  (when (and emjupy-render-latex (emjupy--latex-available-p))
    (dolist (frag (emjupy--latex-fragments start end))
      (pcase-let ((`(,beg ,fin ,body) frag))
        (unless (string-empty-p (string-trim (or body "")))
          (when-let* ((image (emjupy--latex-image
                              ;; with its delimiters: they say whether the
                              ;; math is inline or display, and put LaTeX
                              ;; in math mode at all
                              (buffer-substring-no-properties beg fin))))
            (let ((ov (make-overlay beg fin)))
              (overlay-put ov 'display image)
              (overlay-put ov 'emjupy-latex t)
              (overlay-put ov 'evaporate t)
              ;; The source is still there underneath; show it on hover.
              (overlay-put ov 'help-echo body))))))))

(defun emjupy--latex-overlay-near (pos)
  "Return the rendered-LaTeX overlay covering or ending at POS, or nil."
  (or (seq-find (lambda (ov) (overlay-get ov 'emjupy-latex))
                (overlays-at pos))
      (and (> pos (point-min))
           (seq-find (lambda (ov) (overlay-get ov 'emjupy-latex))
                     (overlays-in (1- pos) pos)))))

(defun emjupy-latex-unrender-or-delete (&optional n)
  "Reveal the LaTeX source behind a rendered image, else delete backwards.

A rendered fragment is an image laid OVER its source, so the source is
still there and still editable -- but with the image covering it there
is no way to see what is being typed.  Pressing \\[backward-delete-char]
against one therefore takes the image away and leaves point after the
last character of the formula, ready to edit.  Press it again and it
deletes as usual.

N is passed on when this is an ordinary deletion."
  (interactive "p")
  (let ((ov (emjupy--latex-overlay-near (point))))
    (if (not ov)
        ;; `delete-char' with a negative count, not `delete-backward-char':
        ;; the latter is documented as interactive-only.
        (delete-char (- (or n 1)))
      (let ((end (overlay-end ov)))
        (delete-overlay ov)
        (goto-char end)
        (message "%s" (substitute-command-keys
                       "[emjupy] LaTeX source revealed; \\[emjupy-toggle-latex-preview] re-renders"))))))

(provide 'emjupy-latex)

;;; emjupy-latex.el ends here
