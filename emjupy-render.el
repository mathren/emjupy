;;; emjupy-render.el --- Buffer rendering, faces and cell outlines for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Everything that puts pixels on the screen: the theme-derived page
;; colours, the box-drawing outlines (which track the window width),
;; syntax highlighting for code and markdown cells, and the overlay
;; rendering of cells and their outputs.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'emjupy-core)

;; Defined in emjupy-cells.el, which requires this file. Only called at
;; runtime (from the interactive `emjupy-refresh-appearance'), so the
;; cycle never bites at load time.
(declare-function emjupy--rerender-notebook "emjupy-cells" (&optional cell))

;; --- Page colours ----------------------------------------------------------
;; Cells are marked out by their horizontal rules alone -- the buffer keeps
;; its normal background throughout.  The one exception is a cell's OUTPUT,
;; which gets a faint background so results are distinguishable from the code
;; that produced them.  That band lives on the output overlay, so when the
;; outputs are cleared and the box shrinks the background goes with them.

(defcustom emjupy-output-color 'auto
  "Background behind ordinary cell output.

`auto' (the default) derives a faint accent from the current theme by
blending the default background `emjupy-output-blend' of the way toward
the default foreground -- which tints a light theme slightly darker and
a dark theme slightly lighter, so it works both ways round and picks up
the theme's hue instead of forcing a neutral grey.

A colour string (e.g. \"#f0f0f0\") is used verbatim.  nil disables the
band, leaving output on the normal background.

Nothing outside a cell's output is ever recoloured."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Disabled" nil))
  :group 'emjupy)

(defcustom emjupy-output-error-color 'auto
  "Background behind an error output -- a traceback, or anything on stderr
that reads as a failure.  `auto' tints the buffer background toward red.
See `emjupy-output-color' for the accepted values."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy)

(defcustom emjupy-output-warning-color 'auto
  "Background behind a warning -- anything the kernel sent to stderr that
is not a traceback.  `auto' tints the buffer background toward yellow."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy)

(defcustom emjupy-output-image-color "white"
  "Background behind an image output.

Defaults to white because that is what a matplotlib figure is: a tinted
band around a white PNG shows as a frame the plot does not have.  Set it
to nil to use the ordinary output colour instead, or to any colour if
your figures have a different background."
  :type '(choice (color :tag "Explicit colour")
                 (const :tag "Derive from theme" auto)
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy)

(defcustom emjupy-output-blend 0.08
  "How far to blend toward the foreground for an `auto' output colour.
0 is invisible, 1 is the foreground colour itself."
  :type 'float
  :group 'emjupy)

(defcustom emjupy-output-tint-blend 0.16
  "How far to blend toward red or yellow for `auto' error and warning
colours.  Larger is louder."
  :type 'float
  :group 'emjupy)

;; Each of these specifies NOTHING by default beyond `:extend', and only
;; ever gains a `:background'.  They are applied as text properties over
;; output text, so any colour attribute they carried would fight the faces
;; on tracebacks and rich output.  `:extend' makes the band fill the whole
;; line rather than stopping at the last character.

(defface emjupy-output '((t))
  "Background behind ordinary cell output."
  :group 'emjupy)

(defface emjupy-output-error '((t))
  "Background behind an error output."
  :group 'emjupy)

(defface emjupy-output-warning '((t))
  "Background behind a warning output."
  :group 'emjupy)

(defface emjupy-output-image '((t))
  "Background behind an image output."
  :group 'emjupy)

(defface emjupy-box-line
  '((t :inherit shadow))
  "Face for the box-drawing rules that outline a cell."
  :group 'emjupy)

(defun emjupy--color-rgb (color)
  "Return COLOR as a list of three floats in 0..1, or nil if unknown.

Hex forms are parsed directly rather than handed to
`color-name-to-rgb', which resolves through the *display's* palette:
on a tty that quantises to the terminal's colour cube and collapses
e.g. \"#1c1c1c\" to pure black, which would silently flatten the
derived canvas colour on any dark theme."
  (when (stringp color)
    (if (string-match "\\`#\\([0-9a-fA-F]+\\)\\'" color)
        (let* ((hex (match-string 1 color))
               (n (/ (length hex) 3)))
          (when (and (> n 0) (= (* n 3) (length hex)))
            (let ((scale (float (1- (expt 16 n)))))
              (cl-loop for i from 0 below 3
                       collect (/ (string-to-number
                                   (substring hex (* i n) (* (1+ i) n)) 16)
                                  scale)))))
      (color-name-to-rgb color))))

(defun emjupy--blend-colors (a b alpha)
  "Blend ALPHA of colour B into colour A, returning a hex string.
Returns nil if either colour is unknown to Emacs -- which happens on a
terminal reporting `unspecified-bg', where there is nothing sensible to
blend and the effect should simply be skipped."
  (let ((ca (emjupy--color-rgb a))
        (cb (emjupy--color-rgb b)))
    (when (and ca cb)
      (apply #'color-rgb-to-hex
             (append (cl-loop for x in ca for y in cb
                              collect (+ (* (- 1.0 alpha) x) (* alpha y)))
                     (list 2))))))

(defconst emjupy--output-error-hue "#ff0000"
  "Colour an `auto' error background is tinted toward.")
(defconst emjupy--output-warning-hue "#ffd000"
  "Colour an `auto' warning background is tinted toward.")

(defun emjupy--resolve-output-color (spec fallback hue)
  "Return a colour string for SPEC, or nil.
SPEC is a colour, `auto', or nil meaning \"use FALLBACK\".  An `auto'
SPEC blends the buffer background toward HUE, or toward the foreground
when HUE is nil."
  (let ((bg (face-attribute 'default :background nil t))
        (fg (face-attribute 'default :foreground nil t)))
    (cond
     ((stringp spec) spec)
     ((null spec) fallback)
     (hue (emjupy--blend-colors bg hue emjupy-output-tint-blend))
     (t (emjupy--blend-colors bg fg emjupy-output-blend)))))

(defun emjupy--sync-theme-colors ()
  "Recompute the output background faces from the active theme.
Returns non-nil when the ordinary output colour could be derived.

Note what this does NOT do: it never touches the buffer's own
background.  An earlier design remapped `default' to a canvas colour and
painted cells back on top, which inverted the moment anything was wrong
with the cell colour."
  (let* ((base (emjupy--resolve-output-color emjupy-output-color nil nil))
         (specs (list (list 'emjupy-output base)
                      (list 'emjupy-output-error
                            (emjupy--resolve-output-color
                             emjupy-output-error-color base emjupy--output-error-hue))
                      (list 'emjupy-output-warning
                            (emjupy--resolve-output-color
                             emjupy-output-warning-color base emjupy--output-warning-hue))
                      (list 'emjupy-output-image
                            (emjupy--resolve-output-color
                             emjupy-output-image-color base nil)))))
    (dolist (spec specs)
      (let ((face (nth 0 spec))
            (colour (nth 1 spec)))
        (if (and colour (emjupy--color-rgb colour))
            (set-face-attribute face nil :background colour)
          (set-face-attribute face nil :background 'unspecified))))
    (and base (emjupy--color-rgb base) t)))

(defun emjupy--on-theme-change (&rest _)
  "Re-derive emjupy's page colours after a theme is enabled or disabled."
  (emjupy--sync-theme-colors))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'emjupy--on-theme-change))
(when (boundp 'disable-theme-functions)
  (add-hook 'disable-theme-functions #'emjupy--on-theme-change))

(defun emjupy-refresh-appearance ()
  "Re-derive page colours from the theme and redraw open notebooks.
Useful after changing `emjupy-output-color' or loading a theme in an
Emacs too old for `enable-theme-functions'."
  (interactive)
  (emjupy--sync-theme-colors)
  (dolist (buf (emjupy--notebook-buffers))
    (with-current-buffer buf
      (emjupy--rerender-notebook))))

(defcustom emjupy-box-width 'window
  "Width of the box-drawing rules that outline each cell.

An integer is used verbatim. The symbol `window' -- the default --
sizes the rule to the window the notebook is displayed in, so the
outline spans the buffer instead of stopping short at a fixed 80
columns on a wide frame."
  :type '(choice (const :tag "Fit the window" window)
                 (integer :tag "Fixed number of columns"))
  :group 'emjupy)

(defcustom emjupy-box-min-width 60
  "Lower bound for a window-fitted `emjupy-box-width'."
  :type 'integer
  :group 'emjupy)

(defun emjupy--window-text-width (win)
  "Return how many columns of text WIN can show without wrapping.

`window-body-width' is not that number.  It counts the line-number
column, which is drawn inside the text area, so with
`display-line-numbers-mode' on a rule sized from it overshoots the right
edge by the width of the numbers -- most visible on a wide window, where
the numbers are widest and the overshoot wraps a whole line."
  (let ((cols (or (ignore-errors (window-max-chars-per-line win))
                  (window-body-width win)))
        (numbers (or (ignore-errors
                       (with-selected-window win
                         (if (bound-and-true-p display-line-numbers)
                             (line-number-display-width)
                           0)))
                     0)))
    (max 1 (- cols numbers))))

(defcustom emjupy-box-right-margin 2
  "Columns left free at the right edge when fitting rules to the window.

Emacs cannot always be asked exactly how many columns are usable: a
right margin, a fill-column indicator, a scroll bar the toolkit reports
oddly, or a line-number width that is off by the separator can each eat
one or two.  Rather than guess, leave a couple spare.  Raise it if the
rules still run past the edge in your setup, lower it to 0 if they stop
short."
  :type 'integer
  :group 'emjupy)

(defun emjupy--box-width ()
  "Return the column width to draw cell outlines at.

When fitting the window, the NARROWEST window showing this buffer wins:
a rule sized to a wide window wraps onto a second line in a narrow one,
and a wrapped rule is far uglier than a short one."
  (if (integerp emjupy-box-width)
      emjupy-box-width
    (let* ((windows (get-buffer-window-list (current-buffer) nil t))
           ;; A notebook is rendered before it is displayed (see
           ;; `emjupy-open-notebook'), so there may be no window to measure
           ;; yet.  Falling back to a fixed 100 columns baked an over-wide
           ;; rule into every fresh notebook; the selected window is at least
           ;; the right order of magnitude.
           (widths (or (mapcar #'emjupy--window-text-width windows)
                       (list (emjupy--window-text-width (selected-window))))))
      (max emjupy-box-min-width
           ;; Leave a margin so the rule cannot run past the right edge.
           (- (apply #'min widths) (max 0 emjupy-box-right-margin))))))

(defun emjupy--cell-label (cell)
  "Return the header label for CELL's input box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[In: %s] %s"
            (if (numberp exec) (number-to-string exec) " ")
            (if (eq (emjupy-cell-type cell) 'code) "python" "markdown"))))

(defun emjupy--cell-out-label (cell)
  "Return the header label for CELL's output box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[Out: %s]" (if (numberp exec) (number-to-string exec) " "))))

(defun emjupy--rule (label &optional corner)
  "Return a propertized box rule line, or a footer when LABEL is nil."
  (propertize (if label (emjupy--box-header label corner) (emjupy--box-footer))
              'face 'emjupy-box-line))

;; --- Keeping the rules the right width ------------------------------------

(defvar-local emjupy--last-box-width nil
  "Width the visible box rules were last drawn at.")

(defun emjupy--refresh-box-rules (&optional force)
  "Redraw cell outlines at the current window width, if it changed.

The rules live in overlay before/after-strings, which are built once at
render time -- so without this a rule sized for a full-screen frame
stays that long when the window shrinks and wraps onto a second line.
Only the overlay strings are rebuilt, not the buffer text, so point,
markers and the undo history are all untouched."
  (let ((width (emjupy--box-width)))
    (when (and emjupy--buffer-notebook
               (or force (not (eql width emjupy--last-box-width))))
      (setq emjupy--last-box-width width)
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell))
                        (out (emjupy-cell-output-ov cell)))
                    (when (overlayp ov)
                      (overlay-put ov 'before-string
                                   (emjupy--rule (emjupy--cell-label cell)))
                      ;; A cell with output has no footer of its own: the
                      ;; output box's header doubles as its bottom edge.
                      (overlay-put ov 'after-string
                                   (if (overlayp out) "" (emjupy--rule nil))))
                    (when (overlayp out)
                      (overlay-put out 'before-string
                                   (emjupy--rule (emjupy--cell-out-label cell) "├"))
                      (overlay-put out 'after-string (emjupy--rule nil))
                      ;; The output band is padded to a column, so it has to
                      ;; be re-aligned at the new width as well.
                      (let ((inhibit-read-only t)
                            (buffer-undo-list t))
                        (emjupy--repad-output cell))))))))

(defun emjupy--window-size-changed (&optional _frame)
  "Refresh box rules in every emjupy buffer after a window size change."
  (dolist (buf (emjupy--notebook-buffers))
    (with-current-buffer buf
      (emjupy--refresh-box-rules))))

(defconst emjupy--box-corner-pairs
  '(("┌" . "┐")   ; top of a box
    ("├" . "┤"))  ; a shared edge: input box's bottom, output box's top
  "Left corner glyph -> matching right corner glyph.")

(defun emjupy--box-header (label &optional corner)
  "Return a box-drawing header line of `emjupy--box-width' columns with LABEL.
CORNER is the left corner glyph, default \"┌\"; pass \"├\" when this
header is meant to double as the closing edge of the box above it.  The
matching right corner is chosen to suit, so the rule closes the box
instead of trailing off into a bare horizontal line."
  (let* ((corner (or corner "┌"))
         (right (or (cdr (assoc corner emjupy--box-corner-pairs)) "┐"))
         (prefix (format "%s─ %s " corner label))
         ;; Reserve the last column for the right corner.
         (fill (max 0 (- (emjupy--box-width) (length prefix) (length right)))))
    (concat prefix (make-string fill ?─) right "\n")))

(defun emjupy--box-footer ()
  "Return a box-drawing footer line of `emjupy--box-width' columns."
  (concat "└" (make-string (max 0 (- (emjupy--box-width) 2)) ?─) "┘\n"))

;; --- Markdown highlighting -------------------------------------------------

(defconst emjupy--markdown-fallback-rules
  ;; (REGEXP GROUP FACE). Applied in order with `add-face-text-property', so
  ;; emphasis nested inside a heading composes rather than overwriting.
  '(;; fenced code blocks
    ("^[ \t]*\\(```\\|~~~\\)\\(?:.\\|\n\\)*?^[ \t]*\\1[ \t]*$" 0 font-lock-string-face)
    ;; setext + atx headings
    ("^[ \t]*#+[ \t].*$" 0 font-lock-function-name-face)
    ("^[ \t]*#+[ \t].*$" 0 bold)
    ;; blockquotes
    ("^[ \t]*>.*$" 0 font-lock-comment-face)
    ;; horizontal rules
    ("^[ \t]*\\(?:---+\\|\\*\\*\\*+\\|___+\\)[ \t]*$" 0 shadow)
    ;; list markers (the bullet only, not the item text)
    ("^[ \t]*\\([-*+]\\|[0-9]+[.)]\\)[ \t]+" 1 font-lock-keyword-face)
    ;; links and images: [label](target)
    ("!?\\[\\([^]]*\\)\\](\\([^)]*\\))" 1 link)
    ("!?\\[\\([^]]*\\)\\](\\([^)]*\\))" 2 font-lock-comment-face)
    ;; bold before italic, so ** isn't mistaken for a single *
    ("\\(\\*\\*\\|__\\)\\(?:[^*_\n]\\|\\*[^*]\\)+?\\1" 0 bold)
    ("\\(?:^\\|[^*_\\]\\)\\([*_]\\)\\([^*_\n]+?\\)\\1" 0 italic)
    ;; inline code last: it wins visually over emphasis inside it
    ("`[^`\n]+`" 0 font-lock-constant-face))
  "Regexp rules for `emjupy--markdown-fontify-fallback'.")


;; --- LaTeX preview in markdown cells ---------------------------------------
;; Off by default.  When on, math between the usual delimiters is replaced ON
;; SCREEN by a rendered image: the overlay carries a `display' property, so the
;; buffer text stays the LaTeX the user wrote and the cell saves unchanged.
;;
;; org ships with Emacs and already knows how to turn a formula into an image,
;; so this needs a LaTeX install but NO extra Emacs package -- which is the
;; whole reason not to reach for math-preview, which additionally wants nodejs
;; and npm.  math-preview is still used if it is what you have.

;; Compile-time only: nothing loads org until a preview is actually asked for.
(eval-when-compile (require 'org))
(declare-function org-create-formula-image "org")
(defvar org-format-latex-options)
(defvar org-format-latex-header)
(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)

(defcustom emjupy-render-latex nil
  "When non-nil, show math in markdown cells as rendered images.

Needs a working LaTeX installation (`latex' plus `dvipng'), or
`math-preview' on PATH.  Nothing else: the rendering itself is done by
org, which comes with Emacs."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-latex-backend 'auto
  "How a LaTeX fragment is turned into an image.

`auto' prefers org's built-in preview, since it needs no Emacs package
beyond what ships with Emacs, and falls back to `math-preview'."
  :type '(choice (const :tag "Whatever is available" auto)
                 (const :tag "org's built-in preview" org)
                 (const :tag "math-preview" math-preview))
  :group 'emjupy)

(defcustom emjupy-latex-foreground 'auto
  "Colour to draw rendered math in.

`auto\' (the default) follows the theme, taking the foreground of the
`default\' face -- so formulae are dark on a light theme and light on a
dark one, instead of always black on a transparent background.

A colour string is used verbatim."
  :type '(choice (const :tag "Follow the theme" auto)
                 (color :tag "Explicit colour"))
  :group 'emjupy)

(defcustom emjupy-latex-scale 1.0
  "Scale factor for rendered math images."
  :type 'float
  :group 'emjupy)

(defconst emjupy--latex-delimiters
  '(("\\\\\\[" . "\\\\\\]")
    ("\\$\\$"  . "\\$\\$")
    ("\\\\("   . "\\\\)")
    ("\\$"     . "\\$"))
  "Opening/closing math delimiter regexps, longest first.
`$$' has to be tried before `$', or a display block reads as two empty
inline ones.")

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
                                               body-start (match-beginning 0)))))))))
          (if hit
              (progn (push hit found) (goto-char (nth 1 hit)))
            (forward-char 1)))))
    (nreverse found)))

(defun emjupy--latex-available-p ()
  "Return the backend that can actually render math here, or nil."
  (pcase emjupy-latex-backend
    ('math-preview (and (executable-find "math-preview") 'math-preview))
    ('org (and (executable-find "latex") 'org))
    (_ (cond ((executable-find "latex") 'org)
             ((executable-find "math-preview") 'math-preview)
             (t nil)))))

(defun emjupy--latex-image (body)
  "Render BODY, a LaTeX math string, to an image spec, or nil."
  (when (and (eq (emjupy--latex-available-p) 'org)
             (require 'org nil 'noerror))
    (let* ((dir (expand-file-name "emjupy-latex/" temporary-file-directory))
           (fg (if (stringp emjupy-latex-foreground)
                   emjupy-latex-foreground
                 (face-attribute 'default :foreground nil t)))
           ;; The colour is part of the cache key.  Without it a formula
           ;; rendered under one theme would be served back unchanged after
           ;; switching to another -- black glyphs on a dark background.
           (file (expand-file-name
                  (concat (md5 (format "%s-%s-%s" body emjupy-latex-scale fg)) ".png")
                  dir))
           ;; Deliberately NOT org's full preamble.  That pulls in packages a
           ;; minimal TeX install does not have -- ulem, for one -- and then
           ;; every formula fails for want of something no formula needs.
           (org-latex-default-packages-alist '(("" "amsmath" t) ("" "amssymb" t)))
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
      ;; Keep the image transparent.  Passing a buffer makes org honour
      ;; :background too, and its default -- `default\' -- bakes the theme's
      ;; background into the PNG as a \\pagecolor.  That looks right until the
      ;; formula sits on anything else, so ask for transparency explicitly and
      ;; let the foreground alone carry the contrast.
      (setq opts (plist-put opts :background "Transparent"))
      (make-directory dir t)
      (condition-case err
          (progn
            ;; Cached by content hash: dragging a slider over a notebook full
            ;; of formulae should not re-run LaTeX for each redraw.
            (unless (file-exists-p file)
              ;; The BUFFER argument is not optional in effect: with nil, org
              ;; reads :html-foreground and :html-background instead of
              ;; :foreground and :background -- defaulting to "Black" on
              ;; "Transparent" whatever the theme says -- and takes :html-scale
              ;; rather than :scale, so emjupy-latex-scale was ignored too.
              (org-create-formula-image body file opts (current-buffer) 'dvipng))
            (when (and (file-exists-p file) (emjupy--image-displayable-p 'png))
              (create-image file 'png nil :ascent 'center)))
        (error
         (message "[emjupy] LaTeX preview failed: %s" (error-message-string err))
         nil)))))

(defun emjupy--preview-latex-in (start end)
  "Lay rendered images over the math between START and END."
  (when (and emjupy-render-latex (emjupy--latex-available-p))
    (dolist (frag (emjupy--latex-fragments start end))
      (pcase-let ((`(,beg ,fin ,body) frag))
        (unless (string-empty-p (string-trim (or body "")))
          (when-let ((image (emjupy--latex-image body)))
            (let ((ov (make-overlay beg fin)))
              (overlay-put ov 'display image)
              (overlay-put ov 'emjupy-latex t)
              (overlay-put ov 'evaporate t)
              ;; The source is still there underneath; show it on hover.
              (overlay-put ov 'help-echo body))))))))

(defun emjupy-toggle-latex-preview ()
  "Turn LaTeX previews in markdown cells on or off, and redraw."
  (interactive)
  (setq emjupy-render-latex (not emjupy-render-latex))
  (when (and emjupy-render-latex (not (emjupy--latex-available-p)))
    (message "[emjupy] No LaTeX renderer found: install latex and dvipng, or math-preview."))
  (when emjupy--buffer-notebook (emjupy--rerender-notebook))
  (message "[emjupy] LaTeX preview %s." (if emjupy-render-latex "on" "off")))

(defun emjupy--markdown-fontify-fallback ()
  "Apply markdown faces to the current buffer without any external package.

`markdown-mode' is a MELPA package and emjupy does not depend on it, so
without this fallback markdown cells render as undifferentiated plain
text for anyone who hasn't installed it."
  (dolist (rule emjupy--markdown-fallback-rules)
    (pcase-let ((`(,regexp ,group ,face) rule))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (when (match-beginning group)
            (add-face-text-property (match-beginning group) (match-end group)
                                    face nil)))))))

(defun emjupy--markdown-mode-fn ()
  "Return the best available markdown major-mode function, or nil.
Prefers the tree-sitter `markdown-ts-mode' (MELPA) when both the
package and its compiled grammar are actually available, then falls
back to the classic `markdown-mode' (MELPA), then nil -- in which case
`emjupy--markdown-fontify-fallback' handles highlighting."
  (cond
   ((and (fboundp 'markdown-ts-mode)
         (fboundp 'treesit-ready-p)
         (treesit-ready-p 'markdown t))
    'markdown-ts-mode)
   ((fboundp 'markdown-mode) 'markdown-mode)
   (t nil)))

(defun emjupy--fontify-as (text cell-type)
  "Return TEXT with font-lock faces applied appropriate for CELL-TYPE.
Code cells use `python-mode', which ships with Emacs core. Markdown
cells use whatever `emjupy--markdown-mode-fn' resolves to, and fall
back to emjupy's own highlighting when no markdown package is
installed."
  (with-temp-buffer
    (insert text)
    ;; delay-mode-hooks avoids running the user's own mode hooks (linters,
    ;; minor modes, etc.) in this throwaway buffer -- same technique
    ;; org-mode uses to fontify source blocks.
    (cond
     ((eq cell-type 'code)
      (delay-mode-hooks (python-mode))
      (font-lock-ensure))
     ((eq cell-type 'markdown)
      (let ((mode-fn (emjupy--markdown-mode-fn)))
        (if mode-fn
            (progn (delay-mode-hooks (funcall mode-fn))
                   (font-lock-ensure))
          (emjupy--markdown-fontify-fallback))))
     (t (font-lock-ensure)))
    (buffer-string)))

(defvar-local emjupy--refontifying nil
  "Non-nil while emjupy is re-applying faces, to stop the hook recursing.")

(defun emjupy--apply-faces-from (string start)
  "Copy the `face' properties of STRING onto the buffer text at START.
Only properties are touched; the buffer text itself is left alone."
  (let ((i 0) (len (length string)))
    (remove-text-properties start (+ start len) '(face nil))
    (while (< i len)
      (let* ((next (or (next-single-property-change i 'face string) len))
             (f (get-text-property i 'face string)))
        (when f
          (put-text-property (+ start i) (+ start next) 'face f))
        (setq i next)))))

(defun emjupy--refontify-cell (cell)
  "Re-highlight CELL's source in place, from its current buffer text."
  (let ((ov (emjupy-cell-overlay cell)))
    (when (overlayp ov)
      (let* ((start (overlay-start ov))
             (end (overlay-end ov))
             (text (buffer-substring-no-properties start end))
             (inhibit-read-only t)
             (inhibit-modification-hooks t)
             (buffer-undo-list t)
             (emjupy--refontifying t)
             (modified (buffer-modified-p)))
        (emjupy--apply-faces-from (emjupy--fontify-as text (emjupy-cell-type cell))
                                  start)
        (set-buffer-modified-p modified)))))

(defun emjupy--refontify-after-change (beg end _len)
  "Re-highlight the cell touched by an edit between BEG and END.

Highlighting is otherwise applied only when a cell is rendered, so
freshly typed text stays unhighlighted until something triggers a
re-render -- and `self-insert-command' inherits the sticky face of the
character before it, so typing after a keyword picks up that keyword's
face."
  (unless emjupy--refontifying
    (when emjupy--buffer-notebook
      (condition-case nil
          (let* ((lo (max (point-min) (min beg (point-max))))
                 (cell (or (and (< lo (point-max)) (get-text-property lo 'emjupy-cell))
                           (and (> lo (point-min))
                                (get-text-property (1- lo) 'emjupy-cell))
                           (and (< end (point-max))
                                (get-text-property end 'emjupy-cell)))))
            (when cell (emjupy--refontify-cell cell)))
        (error nil)))))

(defun emjupy--render-cell (cell)
  "Render CELL at point using overlays for boundary boxes and live outputs."
  (let* ((type (emjupy-cell-type cell))
         (source (emjupy-cell-source cell))
         (outputs (emjupy-cell-outputs cell))
         ;; Only show the output box when the cell actually has output --
         ;; e.g. a bare `import numpy as np' shouldn't grow an empty box.
         (has-outputs (and (eq type 'code) outputs (> (length outputs) 0)))
         (src-start (point)))

    ;; 1. Insert source code (syntax-highlighted per cell type) and tag text
    (let ((to-insert (emjupy--fontify-as (if (string-empty-p source) "\n" source) type)))
      (insert to-insert)
      ;; Faces are applied as plain (non-sticky) `face' properties so that
      ;; text typed at a cell edge does not inherit the neighbouring face.
      (remove-text-properties src-start (point) '(rear-nonsticky nil)))
    (unless (string-suffix-p "\n" source) (insert "\n"))

    (put-text-property src-start (point) 'emjupy-cell cell)

    ;; Markdown only: code cells have no math, and a stray `$' in a string
    ;; should not turn into a formula.
    (when (eq type 'markdown)
      (emjupy--preview-latex-in src-start (point)))

    ;; 2. Source Box Overlay
    (let* ((ov (make-overlay src-start (point)))
           ;; The rules carry the cell background too, so the outline reads
           ;; as the edge of the paper rather than floating on the canvas.
           (header (emjupy--rule (emjupy--cell-label cell)))
           ;; When output follows, its header line doubles as this box's
           ;; closing edge -- no separate footer, no gap between the two.
           (footer (if has-outputs "" (emjupy--rule nil))))
      (overlay-put ov 'before-string header)
      (overlay-put ov 'after-string footer)
      ;; No face: source cells keep the buffer's normal background, and are
      ;; marked out by their rules alone.
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Output Box Overlay
    (when has-outputs
      (let ((out-start (point)))
        (cl-loop for out in (emjupy--outputs-for-render outputs)
                 do (let ((out-type (gethash "output_type" out))
                          (piece-start (point)))
                      (cond
                       ((string= out-type "stream")
                        (insert (emjupy--mime-text (gethash "text" out))))
                       ((or (string= out-type "execute_result")
                            (string= out-type "display_data"))
                        (emjupy--insert-rich-output (gethash "data" out)))
                       ((string= out-type "error")
                        (let ((ename (gethash "ename" out))
                              (evalue (gethash "evalue" out))
                              (traceback (gethash "traceback" out)))
                          (insert (format "Error (%s): %s\n" ename evalue))
                          (when (or (vectorp traceback) (listp traceback))
                            (cl-loop for line in (append traceback nil)
                                     do (insert (replace-regexp-in-string
                                                 "\033\\[[0-9;]*m" ""
                                                 (emjupy--mime-text line))
                                                "\n"))))))
                      ;; Paint this piece, not the whole box: one cell can
                      ;; hold a figure, a warning and a traceback at once, and
                      ;; each should read as what it is.  A text property
                      ;; rather than an overlay face, so it does not override
                      ;; the font-lock colours on the text underneath.
                      (let ((face (emjupy--output-face out)))
                        (when face
                          (font-lock-prepend-text-property
                           piece-start (point) 'face face)
                          ;; Fill out to the border, not to the window edge.
                          (goto-char (emjupy--pad-output-lines
                                      piece-start (point) face))
                          ;; A newline still carrying the face paints one more
                          ;; column after the padding ends, so the band poked
                          ;; out past the right-hand rule by a character.
                          (emjupy--unface-newlines piece-start (point))))))

        (unless (string-suffix-p "\n" (buffer-substring-no-properties (max (point-min) (- (point) 1)) (point)))
          (insert "\n"))

        (let* ((ov (make-overlay out-start (point)))
               ;; "├" instead of "┌": this line IS the input box's bottom
               ;; edge, continuing straight into the output box's top edge.
               (header (emjupy--rule (emjupy--cell-out-label cell) "├"))
               (footer (emjupy--rule nil)))
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          ;; No face on the overlay: each output piece paints itself, so a
          ;; figure, a warning and a traceback in one cell each read as what
          ;; they are.  An overlay face here would override all of them.
          (setf (emjupy-cell-output-ov cell) ov))))

    (insert "\n")))

(defconst emjupy--image-mime-types
  '(("image/png"     . png)
    ("image/jpeg"    . jpeg)
    ("image/gif"     . gif)
    ("image/svg+xml" . svg))
  "MIME types emjupy can render inline, in preference order.
Maps each to the Emacs image type used to display it.")

(defun emjupy--mime-text (value)
  "Return VALUE as a string.
nbformat stores multi-line MIME payloads (`text/plain', stream `text',
tracebacks) as either a single string or an array of line strings
depending on where the JSON came from -- the Contents API joins them,
a notebook read straight off disk does not."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((vectorp value) (mapconcat #'identity (append value nil) ""))
   ((listp value) (mapconcat #'identity value ""))
   (t (format "%s" value))))

(defun emjupy--image-displayable-p (type)
  "Return non-nil if this Emacs can actually display an image of TYPE.
Both halves matter: a tty frame can't show images at all, and a build
without the relevant library (very common for `emacs-nox') will make
`create-image' signal rather than degrade."
  (and (display-graphic-p)
       (image-type-available-p type)))

(defun emjupy--pad-output-lines (start end face)
  "Fill every line between START and END out to the cell's right border.

Not `:extend\', which runs the background to the WINDOW edge and so
spilled the tint past the outline and across the rest of the frame.  A
stretch space aligned to `emjupy--box-width\' stops it exactly at the
border instead -- one character per line rather than a run of spaces,
and it costs nothing to re-align when the window changes width."
  (let ((width (emjupy--box-width)))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (end-of-line)
        (let ((eol (min (point) end)))
          (goto-char eol)
          (insert (propertize " "
                              'emjupy-pad t
                              'face face
                              ;; Ends exactly where the rule does: the rule is
                              ;; WIDTH characters, occupying columns 0..WIDTH-1,
                              ;; and a stretch to :align-to WIDTH paints the
                              ;; same span.
                              'display `(space :align-to ,width)))
          (setq end (+ end 1)))
        (forward-line 1)))
    end))

(defun emjupy--unface-newlines (start end)
  "Remove the output background from the newlines between START and END.

A newline carrying a background face paints a column of its own at the
end of the line, past where the padding stops -- so the band stuck out
one character beyond the right-hand rule."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (when (and (< (point) end) (eq (char-after) ?\n))
        (remove-text-properties (point) (1+ (point)) '(face nil)))
      (forward-line 1))))

(defun emjupy--repad-output (cell)
  "Re-align CELL's output padding after the window width changed."
  (let ((ov (emjupy-cell-output-ov cell))
        (width (emjupy--box-width)))
    (when (overlayp ov)
      (save-excursion
        (goto-char (overlay-start ov))
        (while (< (point) (overlay-end ov))
          (if (get-text-property (point) 'emjupy-pad)
              (progn
                (put-text-property (point) (1+ (point))
                                   'display `(space :align-to ,width))
                (forward-char 1))
            (forward-char 1)))))))

(defun emjupy--output-face (out)
  "Return the background face for output OUT, or nil to leave it bare.

An image gets its own face because a matplotlib figure is white and a
tinted band around it reads as a frame the plot does not have.  stderr
that is not a traceback is treated as a warning: that is where Python
puts `warnings.warn\', logging, and progress bars."
  (let ((type (gethash "output_type" out)))
    (cond
     ((equal type "error") 'emjupy-output-error)
     ((and (equal type "stream")
           (equal (gethash "name" out) "stderr"))
      'emjupy-output-warning)
     ((and (member type '("display_data" "execute_result"))
           (emjupy--output-image-key out))
      'emjupy-output-image)
     (t 'emjupy-output))))

(defun emjupy--insert-rich-output (data)
  "Insert the best available representation of the DATA MIME bundle at point.

Prefers an inline image when this Emacs can genuinely display one, and
otherwise falls back to the bundle's own `text/plain' representation
plus a short note. Without that fallback a figure renders as a
silently empty output box on a terminal or no-image Emacs: rendering
happens inside the WebSocket callback, where websocket.el swallows the
`Invalid image type' error `create-image' raises."
  (let* ((image (cl-loop for (mime . type) in emjupy--image-mime-types
                         for payload = (and data (gethash mime data))
                         when payload return (list mime type payload)))
         (text (and data (gethash "text/plain" data))))
    (cond
     ((and image (emjupy--image-displayable-p (nth 1 image)))
      (condition-case err
          (progn (insert-image (emjupy--render-image-output (nth 2 image) (nth 1 image)))
                 (insert "\n"))
        (error
         (insert (format "[emjupy: could not render %s: %s]\n"
                         (nth 0 image) (error-message-string err)))
         (when text (insert (emjupy--mime-text text) "\n")))))
     (image
      (when text (insert (emjupy--mime-text text) "\n"))
      (insert (propertize
               (format "[emjupy: %s output (%d bytes) not shown -- this Emacs has no %s image support]\n"
                       (nth 0 image) (length (nth 2 image)) (nth 1 image))
               'face 'shadow)))
     (text (insert (emjupy--mime-text text) "\n"))
     ;; Some bundle we don't know how to show at all: say so rather than
     ;; rendering an empty box.
     (data
      (let ((mimes (cl-loop for k being the hash-keys of data collect k)))
        (when mimes
          (insert (propertize (format "[emjupy: unsupported output types: %s]\n"
                                      (string-join mimes ", "))
                              'face 'shadow))))))))

(defcustom emjupy-deduplicate-image-outputs t
  "When non-nil, render a repeated identical image only once per cell.

A cell whose last expression is a figure gets the SAME picture twice
from the kernel: once as the `execute_result' repr and again as the
inline backend's `display_data'. That is kernel-side behaviour, so the
duplicate is dropped only at render time -- the cell's `outputs' vector
still holds exactly what the kernel sent, and is saved back to the
.ipynb unchanged, so the file stays byte-faithful for other clients."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--output-image-key (out)
  "Return a key identifying OUT's image payload, or nil if it carries none.
The payload is hashed rather than compared directly: a figure is
hundreds of kilobytes of base64, and this runs on every re-render."
  (when (hash-table-p out)
    (let ((data (gethash "data" out)))
      (when (hash-table-p data)
        (cl-loop for (mime . _type) in emjupy--image-mime-types
                 for payload = (gethash mime data)
                 when payload
                 return (cons mime (md5 (emjupy--mime-text payload))))))))

(defun emjupy--outputs-for-render (outputs)
  "Return OUTPUTS as a list, with repeated identical images dropped.

Only image-bearing outputs are collapsed, and only against images
already seen in the same cell: two `print' calls emitting the same text
are genuinely two outputs and both must show."
  (let ((all (append (or outputs []) nil)))
    (if (not emjupy-deduplicate-image-outputs)
        all
      (let ((seen (make-hash-table :test 'equal))
            (acc nil))
        (dolist (out all (nreverse acc))
          (let ((key (emjupy--output-image-key out)))
            (cond
             ((null key) (push out acc))
             ((gethash key seen) nil)   ; same picture again -- skip
             (t (puthash key t seen)
                (push out acc)))))))))

(defun emjupy--render-image-output (payload &optional type)
  "Convert PAYLOAD output into an Emacs image object of TYPE (default png).
Bitmap MIME payloads arrive base64-encoded; `image/svg+xml' arrives as
literal markup and must not be decoded."
  (let* ((type (or type 'png))
         (image-data (if (eq type 'svg)
                         (emjupy--mime-text payload)
                       (base64-decode-string (emjupy--mime-text payload)))))
    (create-image image-data type t)))

(provide 'emjupy-render)
;;; emjupy-render.el ends here
