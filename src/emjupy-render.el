;;; emjupy-render.el --- Buffer rendering, faces and cell outlines for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Everything that puts pixels on the screen: the theme-derived page
;; colours, the box-drawing outlines (which track the window width),
;; syntax highlighting for code and markdown cells, and the overlay
;; rendering of cells and their outputs.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'ansi-color)
(require 'emjupy-core)

;;;; Options

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
  "Background behind an error output.
This covers a traceback, or anything on stderr that reads as a failure.
`auto' tints the buffer background toward red.
See `emjupy-output-color' for the accepted values."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy)

(defcustom emjupy-output-warning-color 'auto
  "Background behind a warning output.
This covers anything the kernel sent to stderr that is not a traceback.
`auto' tints the buffer background toward yellow."
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
  "How far to blend toward red or yellow for `auto' tints.
This applies to the error and warning  Larger is louder."
  :type 'float
  :group 'emjupy)

(defcustom emjupy-box-width 'window
  "Width of the box-drawing rules that outline each cell.

An integer is used verbatim.  The symbol `window' -- the default --
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

(defcustom emjupy-box-right-margin 2
  "Columns left free at the right edge when fitting rules to the window.

Emacs cannot always be asked exactly how many columns are usable: a
right margin, a `fill-column' indicator, a scroll bar the toolkit reports
oddly, or a line-number width that is off by the separator can each eat
one or two.  Rather than guess, leave a couple spare.  Raise it if the
rules still run past the edge in your setup, lower it to 0 if they stop
short."
  :type 'integer
  :group 'emjupy)

(defcustom emjupy-running-indicator ["|" "/" "-" "\\"]
  "Frames cycled in a cell's header while it is executing.

A single-element vector such as [\"*\"] gives Jupyter's static marker
instead of an animation."
  :type '(vector string)
  :group 'emjupy)

(defcustom emjupy-running-indicator-interval 0.2
  "Seconds between frames of the running indicator."
  :type 'number
  :group 'emjupy)

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

`auto' (the default) follows the theme, taking the foreground of the
`default' face -- so formulae are dark on a light theme and light on a
dark one, instead of always black on a transparent background.

A colour string is used verbatim."
  :type '(choice (const :tag "Follow the theme" auto)
                 (color :tag "Explicit colour"))
  :group 'emjupy)

(defcustom emjupy-latex-scale 1.0
  "Scale factor for rendered math images."
  :type 'float
  :group 'emjupy)

(defcustom emjupy-hidden-output-glyph "▼"
  "Glyph marking an output box that is collapsed."
  :type 'string
  :group 'emjupy)

(defcustom emjupy-protect-non-cell-regions t
  "When non-nil, make everything outside a cell's source read-only.

The rules, the gutters between cells and the output boxes belong to no
cell.  Text typed there is stored nowhere and disappears at the next
redraw, so it is better refused than silently lost."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-render-ansi-colors t
  "When non-nil, turn ANSI colour escapes in output into real colours.

Libraries like termcolor, rich and colorama emit them, and a kernel
happily passes them through -- so without this the output reads
\"\\033[33mwarning\\033[0m\" instead of a yellow word."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-inline-figures t
  "Whether figures -- image outputs -- are drawn in the notebook.
nil shows each as a line instead, and opens it in a window of its own on
\\<emjupy-mode-map>\\[emjupy-open-output] or a click, the way a plotting
window would show it."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-deduplicate-image-outputs t
  "When non-nil, render a repeated identical image only once per cell.

A cell whose last expression is a figure gets the SAME picture twice
from the kernel: once as the `execute_result' repr and again as the
inline backend's `display_data'.  That is kernel-side behaviour, so the
duplicate is dropped only at render time -- the cell's `outputs' vector
still holds exactly what the kernel sent, and is saved back to the
.ipynb unchanged, so the file stays byte-faithful for other clients."
  :type 'boolean
  :group 'emjupy)

;; Cells are marked out by their rules alone, on the buffer's own background.
;; Output is the exception: a faint band on the output overlay, which goes when
;; the output does.

;; These faces only ever gain a `:background', with `:extend' so the band fills
;; the line: as text properties over output, any colour would fight the faces of
;; tracebacks and rich output.

;;;; Faces and colours

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
Hex forms are parsed here: `color-name-to-rgb' goes through the
display's palette, which on a tty turns a dark grey to black."
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
Return non-nil when the ordinary output colour could be derived.  The
buffer's own background is never touched."
  (let* ((base (emjupy--resolve-output-color emjupy-output-color nil nil))
         (specs (list (list 'emjupy-output base)
                      (list 'emjupy-output-error
                            (emjupy--resolve-output-color
                             emjupy-output-error-color base
                             emjupy--output-error-hue))
                      (list 'emjupy-output-warning
                            (emjupy--resolve-output-color
                             emjupy-output-warning-color base
                             emjupy--output-warning-hue))
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

;;;; Width

(defun emjupy--window-text-width (win)
  "Return how many columns of text WIN can show without wrapping.
Not `window-body-width', which includes the line-number column."
  (if (not (window-live-p win))
      ;; A window can die between a resize being scheduled and this
      ;; running; a dead one has no width to give, so the default stands.
      emjupy-box-min-width
    ;; Measured keeping point: `window-max-chars-per-line' selects WIN, which
    ;; moves point to the window's.  Midway through a redraw that drew every
    ;; later cell on top of the one before, in a window other than the selected
    ;; one.
    (save-excursion
      (let ((cols (window-max-chars-per-line win))
            ;; Read from the window's buffer rather than by selecting it.
            ;; Emacs reports the column's width for the selected window only,
            ;; so for any other it is worked out from the last line's number.
            (numbers (with-current-buffer (window-buffer win)
                       (if (bound-and-true-p display-line-numbers)
                           ;; Emacs's figure, or the line-number column's --
                           ;; digits and a space either side -- whichever is
                           ;; more: Emacs reports 0 until the window has been
                           ;; shown, and leaves out the gap.
                           (max (if (eq win (selected-window))
                                    (line-number-display-width) 0)
                                (+ 2 (max (length (number-to-string
                                                   (line-number-at-pos
                                                    (point-max) t)))
                                          (or (bound-and-true-p
                                               display-line-numbers-width)
                                              0))))
                         0))))
        (max 1 (- cols numbers))))))

(defvar emjupy--box-width-memo nil
  "A cons holding the width worked out for the redraw in progress, or nil.
Bound by a redraw: the width cannot change during one, and working it out
measures each window, which selects it -- six times a cell.")

(defun emjupy--box-width ()
  "Return the column width to draw cell outlines at.
Fitting the window, the narrowest window showing the buffer wins: a
rule too wide for a window wraps."
  (cond
   ((integerp emjupy-box-width) emjupy-box-width)
   ((and (consp emjupy--box-width-memo) (car emjupy--box-width-memo)))
   ((consp emjupy--box-width-memo)
    (setcar emjupy--box-width-memo (emjupy--box-width-measured)))
   (t (emjupy--box-width-measured))))

(defun emjupy--box-width-measured ()
  "Work out the width to draw cell outlines at, measuring the windows."
  (if (integerp emjupy-box-width)
      emjupy-box-width
    (let* ((windows (get-buffer-window-list (current-buffer) nil t))
           ;; A notebook is drawn before it is shown, so there may be no window
           ;; yet; the selected one is the right order of magnitude.
           (widths (or (mapcar #'emjupy--window-text-width windows)
                       (list (emjupy--window-text-width (selected-window))))))
      (max emjupy-box-min-width
           ;; Leave a margin so the rule cannot run past the right edge.
           (- (apply #'min widths) (max 0 emjupy-box-right-margin))))))

;;;; The running indicator

(defvar-local emjupy--running-cells nil
  "Ids of cells currently executing in this buffer.")

(defvar-local emjupy--spinner-timer nil
  "Timer animating the running indicator, or nil.")

(defvar-local emjupy--spinner-frame 0
  "Index into `emjupy-running-indicator'.")

(defun emjupy--cell-running-p (cell)
  "Return non-nil if CELL is currently executing."
  (and cell (memq (emjupy-cell-id cell) emjupy--running-cells)))

(defun emjupy--spinner-tick ()
  "Advance the indicator and redraw the headers of running cells.
Only overlay strings change, never buffer text, so typing and undo are
not disturbed."
  (let ((buf (current-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (not emjupy--running-cells)
            (emjupy--stop-spinner)
          (setq emjupy--spinner-frame
                (mod (1+ emjupy--spinner-frame)
                     (length emjupy-running-indicator)))
          (when emjupy--buffer-notebook
            (cl-loop for cell across
                     (emjupy-notebook-cells emjupy--buffer-notebook)
                     when (emjupy--cell-running-p cell)
                     do (emjupy--refresh-cell-header cell))))))))

(defun emjupy--start-spinner ()
  "Start animating the running indicator in this buffer."
  (unless (or emjupy--spinner-timer (< (length emjupy-running-indicator) 2))
    (setq emjupy--spinner-timer
          (run-with-timer emjupy-running-indicator-interval
                          emjupy-running-indicator-interval
                          #'emjupy--spinner-tick-in (current-buffer)))))

(defun emjupy--spinner-tick-in (buffer)
  "Run `emjupy--spinner-tick' inside BUFFER if it is still alive."
  (if (buffer-live-p buffer)
      (with-current-buffer buffer (emjupy--spinner-tick))
    (emjupy--stop-spinner)))

(defun emjupy--stop-spinner ()
  "Stop the running indicator in this buffer."
  (when emjupy--spinner-timer
    (cancel-timer emjupy--spinner-timer)
    (setq emjupy--spinner-timer nil)))

(defun emjupy--refresh-cell-header (cell)
  "Redraw CELL's header rule in place.
The header is an overlay `before-string', so this changes no buffer text
and does not enter the undo history."
  (let ((ov (emjupy-cell-overlay cell)))
    (when (overlayp ov)
      (overlay-put ov 'emjupy-header nil)
      (overlay-put ov 'before-string
                   (emjupy--rule (emjupy--cell-label cell) "┌"))
      (emjupy--reconcile-rules)
      (emjupy--overlay-header ov))))

(defun emjupy--mark-running (cell &optional buffer)
  "Mark CELL as executing in BUFFER, and start the indicator."
  (with-current-buffer (or buffer (current-buffer))
    (cl-pushnew (emjupy-cell-id cell) emjupy--running-cells)
    ;; Draw it at once rather than on the next tick, so pressing the key
    ;; visibly does something even for a cell that finishes quickly.
    (emjupy--refresh-cell-header cell)
    (emjupy--start-spinner)))

(defun emjupy--mark-done (cell &optional buffer)
  "Mark CELL as no longer executing in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq emjupy--running-cells
          (delq (emjupy-cell-id cell) emjupy--running-cells))
    (emjupy--refresh-cell-header cell)
    (unless emjupy--running-cells (emjupy--stop-spinner))))

(defvar python-indent-guess-indent-offset-verbose)

;;;; Labels and rules

(defun emjupy--cell-label (cell)
  "Return the header label for CELL's input box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[In: %s] %s"
            (cond
             ;; Jupyter's convention: a running cell shows a marker where its
             ;; execution count will go.
             ((emjupy--cell-running-p cell)
              (aref emjupy-running-indicator
                    (mod emjupy--spinner-frame
                         (length emjupy-running-indicator))))
             ((numberp exec) (number-to-string exec))
             (t " "))
            (if (eq (emjupy-cell-type cell) 'code) "python" "markdown"))))

(defun emjupy--cell-out-label (cell)
  "Return the header label for CELL's output box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[Out: %s]" (if (numberp exec) (number-to-string exec) " "))))

(defun emjupy--overlay-header (ov)
  "Return the header rule of cell or output overlay OV, wherever it is shown."
  (or (overlay-get ov 'before-string) (overlay-get ov 'emjupy-header)))

(defun emjupy--overlay-footer (ov)
  "Return the footer rule of cell or output overlay OV, wherever it is shown."
  (or (overlay-get ov 'after-string) (overlay-get ov 'emjupy-footer)))

(defun emjupy--rule-boxes ()
  "Return every box in this buffer, in buffer order, its rules taken in.
Each keeps its rules in `emjupy-header' and `emjupy-footer', and loses
its overlay strings, for the rules to be hung again."
  (let ((boxes nil))
    (cl-loop for cell across
             (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
             do (dolist (ov (list (emjupy-cell-overlay cell)
                                  (emjupy-cell-output-ov cell)))
                  (when (and (overlayp ov) (eq (overlay-buffer ov)
                                               (current-buffer)))
                    (overlay-put ov 'emjupy-header (emjupy--overlay-header ov))
                    (overlay-put ov 'emjupy-footer (emjupy--overlay-footer ov))
                    (overlay-put ov 'before-string nil)
                    (overlay-put ov 'after-string nil)
                    (push ov boxes))))
    (sort boxes (lambda (a b) (< (overlay-start a) (overlay-start b))))))

(defun emjupy--hang-rules (box anchor rules property)
  "Hang RULES on the newline at ANCHOR, BOX's last, as continuation lines.
PROPERTY is `display', drawn in place of the newline, or `before-string',
before it.  The overlay is BOX's companion, deleted with its rules."
  (let ((o (make-overlay anchor (1+ anchor) nil t nil))
        (text (concat "\n" rules)))
    ;; point there shows at the end of the text, before the rule
    (put-text-property 0 1 'cursor t text)
    (overlay-put o property text)
    (overlay-put o 'emjupy-overlay 'rules)
    (overlay-put o 'emjupy-rules-of box)
    (push o (overlay-get box 'emjupy-companions))))

(defun emjupy--reconcile-rules ()
  "Draw every rule as the continuation of the content line above it.
`display-line-numbers-mode' numbers the first screen line of each line,
so a header drawn on a cell's first line took the code's number.  Each
box's footer, and the header of the box after it, are drawn instead as
continuation lines of the box's last line, never numbered, and the empty
separator line is hidden.  The first cell's header has no line above it
and stays.

The rule strings are kept on each box in `emjupy-header' and
`emjupy-footer'.  Run after anything that redraws a box or changes the
buffer's structure; it rebuilds every rule."
  (when (bound-and-true-p emjupy--buffer-notebook)

    (let ((boxes (emjupy--rule-boxes))
          (prev nil))
      ;; the previous companions, all of them
      (dolist (o (overlays-in (point-min) (point-max)))
        (when (overlay-get o 'emjupy-rules-of) (delete-overlay o)))
      (dolist (ov boxes) (overlay-put ov 'emjupy-companions nil))
      (dolist (ov (append boxes (list nil)))
        (let* ((header (and ov (overlay-get ov 'emjupy-header)))
               (footer (and prev (overlay-get prev 'emjupy-footer)))
               (rules (mapconcat (lambda (r) (string-remove-suffix "\n" r))
                                 (seq-filter (lambda (r)
                                               (and r (not (string-empty-p r))))
                                             (list footer header))
                                 "\n"))
               (anchor (and prev (1- (overlay-end prev)))))
          (cond
           ;; after a box: on its last newline, as continuation lines
           ((and anchor (eq (char-after anchor) ?\n))
            (let ((gap-end (and ov (overlay-start ov))))
              (cond
               ;; Between two boxes.  A screen line is numbered by the line it
               ;; starts on, so a hidden separator gave the next line its
               ;; number.  Instead the box's last newline is hidden, the rules
               ;; hang on the separator as continuation lines, never numbered,
               ;; and the separator's newline ends them.  Drawn as a `display',
               ;; not by hiding the newline: Emacs draws no overlay string at a
               ;; hidden position.
               ((and gap-end (< (overlay-end prev) gap-end))
                (emjupy--hang-rules prev anchor rules 'display))
               ;; Followed at once by another box -- a cell by its output --
               ;; or the last box: the rules hang on its own last line.
               ((not (string-empty-p rules))
                (emjupy--hang-rules prev anchor rules 'before-string)))))
           ;; the first box, or one whose line above cannot carry it
           (ov (overlay-put ov 'before-string header)
               (when prev (overlay-put prev 'after-string footer)))
           (prev (overlay-put prev 'after-string footer))))
        (setq prev ov)))))

(defun emjupy--rule (label &optional corner)
  "Return a propertized box rule line showing LABEL.
CORNER is the left corner glyph; with LABEL nil a footer is returned."
  (let* ((text (if (and label (not (eq corner 'footer)))
                   (emjupy--box-header label corner)
                 (emjupy--box-footer label)))
         (end (if (string-suffix-p "\n" text) (1- (length text))
                (length text))))
    ;; Not the closing newline: one with a background paints a column of its own
    ;; past the rule.
    (put-text-property 0 end 'face 'emjupy-box-line text)
    text))

;; --- Keeping the rules the right width ------------------------------------

(defvar-local emjupy--last-box-width nil
  "Width the visible box rules were last drawn at.")

(defvar emjupy-box-width-changed-functions nil
  "Functions called with a notebook and its new box width, when it changes.")

(defvar-local emjupy--refontifying nil
  "Non-nil while emjupy is re-applying faces, to stop the hook recursing.")

(defconst emjupy--box-corner-pairs
  '(("┌" . "┐")   ; top of a box
    ("├" . "┤"))  ; a shared edge: input box's bottom, output box's top
  "Left corner glyph -> matching right corner glyph.")

(defun emjupy--box-header (label &optional corner)
  "Return a box-drawing header line of `emjupy--box-width' columns with LABEL.
CORNER is the left corner glyph, default \"┌\"; \"├\" makes the header
double as the bottom edge of the box above.  The right corner matches."
  (let* ((corner (or corner "┌"))
         (right (or (cdr (assoc corner emjupy--box-corner-pairs)) "┐"))
         (prefix (format "%s─ %s " corner label))
         ;; Reserve the last column for the right corner.
         (fill (max 0 (- (emjupy--box-width) (length prefix) (length right)))))
    (concat prefix (make-string fill ?─) right "\n")))

(defun emjupy--box-footer (&optional label)
  "Return a box-drawing footer line of `emjupy--box-width' columns.
With LABEL, it is written into the rule, which is how a collapsed output
box says that it is collapsed rather than absent."
  (if (null label)
      (concat "└" (make-string (max 0 (- (emjupy--box-width) 2)) ?─) "┘\n")
    (let* ((prefix (format "└─ %s " label))
           (fill (max 0 (- (emjupy--box-width) (string-width prefix) 1))))
      (concat prefix (make-string fill ?─) "┘\n"))))

;; --- Markdown highlighting -------------------------------------------------

(defconst emjupy--markdown-fallback-rules
  ;; (REGEXP GROUP FACE). Applied in order with `add-face-text-property', so
  ;; emphasis nested inside a heading composes rather than overwriting.
  '(;; fenced code blocks
    ("^[ \t]*\\(```\\|~~~\\)\\(?:.\\|\n\\)*?^[ \t]*\\1[ \t]*$" 0
     font-lock-string-face)
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

;; LaTeX preview in markdown cells, off by default.  Math is replaced on screen
;; by an image, a `display' on an overlay, so the text and the saved cell stay
;; the LaTeX written. org renders it, needing LaTeX and no other package;
;; math-preview is used if it is what there is.

;; Compile-time only: nothing loads org until a preview is actually asked for.
(eval-when-compile (require 'org) (require 'ansi-color))
(declare-function org-create-formula-image "org")
(declare-function ansi-color-apply "ansi-color" (string))
(defvar org-format-latex-options)
(defvar org-format-latex-header)
(defvar org-latex-default-packages-alist)
(defvar org-latex-packages-alist)

;;;; LaTeX

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
    (let* ((dir (expand-file-name "emjupy-latex/" temporary-file-directory))
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
              (org-create-formula-image (emjupy--latex-math body) file opts
                                        (current-buffer) 'dvipng))
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

;;;; Fontifying source

(defun emjupy--markdown-fontify-fallback ()
  "Apply markdown faces to the current buffer without any external package.
For when `markdown-mode', from MELPA, is not installed."
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
`markdown-ts-mode' when it and its grammar are available, then
`markdown-mode'; with neither, `emjupy--markdown-fontify-fallback'
highlights."
  (cond
   ((and (fboundp 'markdown-ts-mode)
         (fboundp 'treesit-ready-p)
         (treesit-ready-p 'markdown t))
    'markdown-ts-mode)
   ((fboundp 'markdown-mode) 'markdown-mode)
   (t nil)))

(defun emjupy--fontify-as (text cell-type)
  "Return TEXT with font-lock faces applied appropriate for CELL-TYPE.
`python-mode' for code, `emjupy--markdown-mode-fn' for markdown."
  (with-temp-buffer
    (insert text)
    ;; delay-mode-hooks avoids running the user's own mode hooks (linters,
    ;; minor modes, etc.) in this throwaway buffer -- same technique
    ;; org-mode uses to fontify source blocks.
    (cond
     ((eq cell-type 'code)
      ;; Quiet: python-mode cannot guess the indentation of a fragment, and
      ;; would say so for every code cell, on every redraw.
      (let ((python-indent-guess-indent-offset-verbose nil))
        (delay-mode-hooks (python-mode)))
      (font-lock-ensure))
     ((eq cell-type 'markdown)
      (let ((mode-fn (emjupy--markdown-mode-fn)))
        (if mode-fn
            (progn (delay-mode-hooks (funcall mode-fn))
                   (font-lock-ensure))
          (emjupy--markdown-fontify-fallback))))
     (t (font-lock-ensure)))
    (buffer-string)))

(defun emjupy--apply-faces-from (string start)
  "Copy the `face' properties of STRING onto the buffer text at START.
As `font-lock-face' too, which font-lock does not remove when it
refontifies."
  (let ((i 0) (len (length string)))
    ;; Padding is never source: the pads carry a mark of their own, kept even if
    ;; the region reaches into an output box.
    (emjupy--without-pads start (+ start len)
                          (lambda (from to)
                            (remove-text-properties
                             from to
                             '(face nil font-lock-face nil))))
    (while (< i len)
      (let* ((next (or (next-single-property-change i 'face string) len))
             (f (get-text-property i 'face string)))
        (when f
          (emjupy--without-pads
           (+ start i) (+ start next)
           (lambda (from to)
             (put-text-property from to 'face f)
             (put-text-property from to 'font-lock-face f))))
        (setq i next)))))

(defun emjupy--output-region-p (pos)
  "Return non-nil if POS lies inside an output box."
  (seq-find (lambda (ov) (eq (overlay-get ov 'emjupy-overlay) 'output))
            (overlays-at pos)))

(defun emjupy--without-pads (start end fn)
  "Call FN on each run between START and END that is not output.
Output, text and padding, is painted from the outputs when the box is
drawn; re-highlighting source would clear it and put nothing back."
  (let ((pos start))
    (while (< pos end)
      (let ((next (min end
                       (or (next-property-change pos nil end) end)
                       (or (next-overlay-change pos) end))))
        (when (= next pos) (setq next (min end (1+ pos))))
        (unless (or (get-text-property pos 'emjupy-pad)
                    (emjupy--output-region-p pos))
          (funcall fn pos next))
        (setq pos next)))))

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
        (emjupy--apply-faces-from (emjupy--fontify-as text
                                                      (emjupy-cell-type cell))
                                  start)
        (set-buffer-modified-p modified)))))

(defun emjupy--refontify-after-change (beg end _len)
  "Re-highlight the cell touched by an edit between BEG and END.
Otherwise typed text stays plain until a redraw, or takes the face of
the character before it."
  (unless emjupy--refontifying
    (when emjupy--buffer-notebook
      (condition-case err
          (let* ((lo (max (point-min) (min beg (point-max))))
                 (cell (or (and (< lo (point-max))
                                (get-text-property lo 'emjupy-cell))
                           (and (> lo (point-min))
                                (get-text-property (1- lo) 'emjupy-cell))
                           (and (< end (point-max))
                                (get-text-property end 'emjupy-cell)))))
            (when cell (emjupy--refontify-cell cell)))
        ;; Caught: an error in a change hook makes Emacs remove the hook,
        ;; and highlighting would stop for the rest of the session.  But
        ;; said, not swallowed.
        (error (message "[emjupy] Could not highlight the edit: %s"
                        (error-message-string err)))))))

;;;; Hidden output

(defun emjupy--cell-outputs-hidden-p (cell)
  "Return non-nil when CELL's output is collapsed.

Kept in the cell's own metadata under the key Jupyter uses, so the state
survives saving and means the same thing to other front ends."
  (let* ((meta (emjupy-cell-metadata cell))
         (jupyter (and (hash-table-p meta) (gethash "jupyter" meta))))
    (and (hash-table-p jupyter)
         (eq (gethash "outputs_hidden" jupyter) t))))

(defun emjupy--set-cell-outputs-hidden (cell hidden)
  "Record in CELL's metadata whether its output is HIDDEN."
  (let* ((meta (or (emjupy-cell-metadata cell)
                   (setf (emjupy-cell-metadata cell)
                         (make-hash-table :test 'equal))))
         (jupyter (or (gethash "jupyter" meta)
                      (puthash "jupyter" (make-hash-table :test 'equal) meta))))
    (if hidden
        (puthash "outputs_hidden" t jupyter)
      (remhash "outputs_hidden" jupyter)
      ;; leave nothing behind: an empty table would be written out as {}
      (when (zerop (hash-table-count jupyter))
        (remhash "jupyter" meta)))
    hidden))

(defvar emjupy-hidden-output-map
  (let ((map (make-sparse-keymap)))
    ;; Without this, the press begins a drag-selection and the release is
    ;; never delivered as a click.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap active on the marker of a collapsed output box.")

(defun emjupy--hidden-output-label (cell)
  "Return the label shown on CELL's footer while its output is hidden.
Only the marker is live: it carries the cell's id and a keymap, so a
click on it knows the cell."
  (let* ((outputs (or (emjupy-cell-outputs cell) []))
         (n (length outputs))
         (glyph (copy-sequence emjupy-hidden-output-glyph))
         (rest (format " %d output%s hidden" n (if (= n 1) "" "s"))))
    (add-text-properties
     0 (length glyph)
     (list 'emjupy-toggle-cell (emjupy-cell-id cell)
           'keymap emjupy-hidden-output-map
           'mouse-face 'highlight
           'help-echo "mouse-1: show this output")
     glyph)
    (concat glyph rest)))

;;;; Drawing output

(defun emjupy--fold-output-lines (start end width)
  "Break every line between START and END wider than WIDTH columns.
Return (NEW-END . WIDEST), WIDEST the widest line before breaking, so
every kind of output stays inside its box.  Only the drawing changes:
the cell keeps the text the kernel sent.  Each break is marked
`emjupy-fold'."
  (let ((end (copy-marker end))
        (widest 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((bol (point)))
          (end-of-line)
          (let ((eol (min (point) end)))
            (setq widest
                  (max widest
                       (string-width (buffer-substring-no-properties bol eol))))
            (goto-char bol)
            ;; step through the line WIDTH columns at a time
            (while (progn (move-to-column width)
                          (and (< (point) eol) (< (point) end)))
              ;; never split a character that straddles the edge
              (when (> (current-column) width) (backward-char))
              (if (bolp)
                  (goto-char eol)       ; one character wider than the box
                (insert (propertize "\n" 'emjupy-fold t))
                (setq eol (save-excursion (end-of-line) (min (point) end)))))
            (goto-char eol)
            (forward-line 1)))))
    (cons (prog1 (marker-position end) (set-marker end nil)) widest)))

(defun emjupy--finish-output-piece (out piece-start)
  "Fold, paint and pad the output OUT just drawn from PIECE-START to point.
Leave point after it, and return the width of its widest line.  Shared
by drawing a whole output box and by adding to one, so that both draw
the same text the same way."
  (let ((widest 0))
    (let ((folded (emjupy--fold-output-lines piece-start (point)
                                             (emjupy--box-width))))
      (goto-char (car folded))
      (setq widest (cdr folded)))
    ;; Paint this piece, not the box, so a figure, a warning and a traceback in
    ;; one cell each read as what they are; a text property, so font-lock
    ;; colours show through.
    (let ((face (emjupy--output-face out)))
      (when face
        (font-lock-prepend-text-property piece-start (point) 'face face)
        (font-lock-prepend-text-property piece-start (point)
                                         'font-lock-face face)
        ;; Fill out to the border, not to the window edge.
        (goto-char (emjupy--pad-output-lines piece-start (point) face))
        ;; A newline still carrying the face paints one more column after
        ;; the padding ends, so the band poked out past the right-hand rule.
        (emjupy--unface-newlines piece-start (point))))
    widest))

(defun emjupy--drawn-state (pieces)
  "Return what drawing PIECES, a cell's outputs in order, drew, or nil.
Recorded on the output box so output that only grew at its end can be
added to it rather than drawn again: how many pieces, the last if it is
a stream, its text as drawn, and the width it was folded at."
  (let ((last-out (car (last pieces))))
    (when (and last-out (equal (gethash "output_type" last-out) "stream"))
      (list (length pieces) last-out
            (emjupy--mime-text (gethash "text" last-out))
            (emjupy--box-width)))))

(defun emjupy--insert-output-piece (out)
  "Insert OUT, one output, at point, drawn as fits its type."
  (let ((out-type (gethash "output_type" out)))
    (cond
     ((string= out-type "stream")
      ;; Carriage returns applied here too: a notebook saved by Jupyter
      ;; stores a progress bar as all its updates.
      (insert (emjupy--ansi-render
               (emjupy--collapse-carriage-returns
                (emjupy--mime-text (gethash "text" out))))))
     ((or (string= out-type "execute_result")
          (string= out-type "display_data"))
      (emjupy--insert-rich-output (gethash "data" out)))
     ((string= out-type "error")
      (let ((traceback (gethash "traceback" out)))
        (insert (format "Error (%s): %s\n"
                        (gethash "ename" out) (gethash "evalue" out)))
        (when (or (vectorp traceback) (listp traceback))
          (cl-loop for line in (append traceback nil)
                   do (insert (emjupy--ansi-render (emjupy--mime-text line))
                              "\n"))))))))

(defun emjupy--make-output-overlay (cell start widest pieces)
  "Give CELL's output box, from START to point, its overlay.
WIDEST is its widest line before folding, PIECES the outputs drawn."
  (let* ((ov (make-overlay start (point)))
         ;; "├" instead of "┌": this line IS the input box's bottom
         ;; edge, continuing straight into the output box's top edge.
         (header (emjupy--rule (emjupy--cell-out-label cell) "├"))
         (footer (emjupy--rule nil)))
    (overlay-put ov 'emjupy-overlay 'output)
    ;; What a resize needs: the width the lines were folded at, and the
    ;; widest line.  Output that fits both widths is not redrawn.
    (overlay-put ov 'emjupy-fold-width (emjupy--box-width))
    (overlay-put ov 'emjupy-widest widest)
    (overlay-put ov 'emjupy-drawn (emjupy--drawn-state pieces))
    (overlay-put ov 'emjupy-header nil)
    (overlay-put ov 'before-string header)
    (overlay-put ov 'after-string footer)
    ;; No face on the overlay: each output piece paints itself, so a
    ;; figure, a warning and a traceback in one cell each read as what
    ;; they are.  An overlay face here would override all of them.
    (setf (emjupy-cell-output-ov cell) ov)))

(defun emjupy--render-cell-output (cell)
  "Insert CELL's output box at point and give it its overlay.
Apart from `emjupy--render-cell' so output arriving redraws one box,
not the notebook."
  (let ((outputs (emjupy-cell-outputs cell)))
    (when (and outputs (> (length outputs) 0))
      (let ((out-start (point))
            ;; widest line as the kernel sent it, before folding
            (widest 0)
            (pieces (emjupy--outputs-for-render outputs)))
        (dolist (out pieces)
          (let ((piece-start (point)))
            (emjupy--insert-output-piece out)
            ;; Kept inside the box, whatever kind of output it is.
            (setq widest (max widest (emjupy--finish-output-piece
                                      out piece-start)))))
        (unless (string-suffix-p "\n" (buffer-substring-no-properties
                                       (max (point-min) (- (point) 1))
                                       (point)))
          (insert "\n"))
        (emjupy--make-output-overlay cell out-start widest pieces)))))

;;;; Undo

(defun emjupy--undo-entry-position (entry)
  "Return the largest buffer position ENTRY refers to, or nil.

Undo entries come in several shapes and only some carry positions;
one without any cannot be invalidated by an edit elsewhere."
  (cond
   ((integerp entry) (abs entry))
   ((not (consp entry)) nil)
   ;; (TEXT . POSITION) -- a deletion; POSITION may be negative
   ((and (stringp (car entry)) (integerp (cdr entry))) (abs (cdr entry)))
   ;; (BEG . END) -- an insertion
   ((and (integerp (car entry)) (integerp (cdr entry)))
    (max (car entry) (cdr entry)))
   ;; (nil PROP VAL BEG . END) -- a property change
   ((and (null (car entry)) (consp (last entry))
         (integerp (car (last entry))) (integerp (cdr (last entry))))
    (max (car (last entry)) (cdr (last entry))))
   ;; (apply DELTA BEG END FUN . ARGS)
   ((eq (car entry) 'apply)
    (let ((beg (nth 2 entry)) (end (nth 3 entry)))
      (and (integerp beg) (integerp end) (max beg end))))
   ((markerp (car entry)) (marker-position (car entry)))
   (t nil)))

(defun emjupy--undo-map-positions (entry fn)
  "Return ENTRY with every buffer position in it passed through FN.
Each shape `buffer-undo-list' documents keeps its positions somewhere
else; missing one would make undo rewrite the wrong text."
  (cond
   ((integerp entry) (funcall fn entry))
   ((not (consp entry)) entry)
   ;; (TEXT . POSITION) -- a deletion.  A negative POSITION means point
   ;; was at the end of the deleted text, so the sign has to be kept.
   ((and (stringp (car entry)) (integerp (cdr entry)))
    (let ((shifted (funcall fn (abs (cdr entry)))))
      (cons (car entry) (if (< (cdr entry) 0) (- shifted) shifted))))
   ;; (BEG . END) -- an insertion
   ((and (integerp (car entry)) (integerp (cdr entry)))
    (cons (funcall fn (car entry)) (funcall fn (cdr entry))))
   ;; (nil PROP VAL BEG .  END), a property change: a dotted list, so taken
   ;; apart by its fixed shape, not with `butlast'.
   ((and (null (car entry)) (consp (cdr entry)) (consp (cddr entry))
         (consp (cdddr entry))
         (integerp (car (cdddr entry))) (integerp (cdr (cdddr entry))))
    (pcase-let ((`(nil ,prop ,val ,beg . ,end) entry))
      `(nil ,prop ,val ,(funcall fn beg) . ,(funcall fn end))))
   ;; (apply DELTA BEG END FUN . ARGS)
   ((and (eq (car entry) 'apply) (integerp (nth 2 entry))
         (integerp (nth 3 entry)))
    (append (list (nth 0 entry) (nth 1 entry)
                  (funcall fn (nth 2 entry))
                  (funcall fn (nth 3 entry)))
            (nthcdr 4 entry)))
   (t entry)))

(defun emjupy--undo-adjust (start old-end delta)
  "Keep the undo history usable across a replacement of START..OLD-END.
Entries before START stay; entries inside the region describe text
that is gone, and go; entries after it have moved, and shift by DELTA.
Each entry shape is handled explicitly: a wrong position does not
fail, it rewrites the wrong text."
  (unless emjupy--in-cell-step
    (let ((transform
           (lambda (entry)
             (let ((p (emjupy--undo-entry-position entry)))
               (cond
                ((null p) entry)
                ((< p start) entry)
                ((< p old-end)
                 ;; Text inside the region is gone, and an entry for it goes
                 ;; too.  Structural commands never get here: they are undone
                 ;; whole.  This is for redraws, such as output arriving.
                 :emjupy-drop)
                (t (emjupy--undo-map-positions
                    entry (lambda (x) (if (>= x old-end) (+ x delta) x))))))))
          (seen (make-hash-table :test 'eq)))
      ;; In place: an undo replays from a list `primitive-undo' keeps in a local
      ;; variable, so a new list would not be seen.  The cons cells are shared
      ;; by all three lists, so changing them reaches each; each once, since the
      ;; lists share tails.  An entry to drop becomes one that does nothing.
      ;;
      ;; All or nothing: every new entry is worked out before any is changed, or
      ;; a failure partway would leave the history half shifted, and the next
      ;; undo would write in the wrong places.
      (let ((changes nil))
        (condition-case err
            (progn
              (dolist (entries (list buffer-undo-list pending-undo-list))
                (when (consp entries)
                  (let ((c entries))
                    (while (consp c)
                      (unless (gethash c seen)
                        (puthash c t seen)
                        (push (cons c (funcall transform (car c))) changes))
                      (setq c (cdr c))))))
              (pcase-dolist (`(,c . ,new) changes)
                (setcar c (if (eq new :emjupy-drop)
                              (list 'apply #'ignore :emjupy-dropped)
                            new))))
          ;; Anything else is a mode's own entry: the ones from START on are
          ;; stale and go -- lost history is safe, wrong history is not.
          (error
           (emjupy--undo-drop-from start)
           (when (consp pending-undo-list)
             (setq pending-undo-list
                   (cl-remove-if (lambda (e)
                                   (let ((p (emjupy--undo-entry-position e)))
                                     (and p (>= p start))))
                                 pending-undo-list)))
           (message "[emjupy] Undo history after the redrawn output was dropped: %s"
                    (error-message-string err)))))
      ;; Outside an undo the placeholders can go for good.
      (unless (or undo-in-progress (consp pending-undo-list))
        (when (consp buffer-undo-list)
          (setq buffer-undo-list
                (delete (list 'apply #'ignore :emjupy-dropped)
                        buffer-undo-list)))))))

(defun emjupy--undo-drop-from (pos)
  "Drop undo entries referring to POS or later in this buffer.
Undo entries hold positions, not markers, so an edit at POS leaves
them stale.  Entries before POS stay."
  (when (listp buffer-undo-list)
    (setq buffer-undo-list
          (cl-remove-if (lambda (entry)
                          (let ((p (emjupy--undo-entry-position entry)))
                            (and p (>= p pos))))
                        buffer-undo-list))))

;;;; Drawing a cell

(defun emjupy--refresh-cell-footer (cell)
  "Give CELL the bottom rule it should have, or none if its output has one."
  (let ((src (emjupy-cell-overlay cell))
        (out (emjupy-cell-output-ov cell))
        (outputs (or (emjupy-cell-outputs cell) [])))
    (when (overlayp src)
      (overlay-put
       src 'after-string
       (cond
        ((overlayp out) "")
        ((and (> (length outputs) 0) (emjupy--cell-outputs-hidden-p cell))
         (emjupy--rule (emjupy--hidden-output-label cell) 'footer))
        (t (emjupy--rule nil)))))))

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
    (let ((to-insert (emjupy--fontify-as (if (string-empty-p source)
                                             "\n" source) type)))
      (insert to-insert)
      ;; Faces are applied as plain (non-sticky) `face' properties so that
      ;; text typed at a cell edge does not inherit the neighbouring face.
      (remove-text-properties src-start (point) '(rear-nonsticky nil)))
    ;; Always one newline after the source, which sync removes; added only when
    ;; missing, a source ending in a newline lost it on the first save.
    (insert "\n")

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
           ;; When output follows, its header doubles as this box's bottom edge.
           ;; Hidden output is not drawn -- it stays in the cell -- and the
           ;; footer says so.
           (hidden (and has-outputs (emjupy--cell-outputs-hidden-p cell)))
           (footer (cond
                    (hidden (emjupy--rule
                             (emjupy--hidden-output-label cell) 'footer))
                    (has-outputs "")
                    (t (emjupy--rule nil)))))
      (overlay-put ov 'emjupy-overlay 'cell)
      (overlay-put ov 'emjupy-header nil)
      (overlay-put ov 'before-string header)
      (overlay-put ov 'after-string footer)
      ;; No face: source cells keep the buffer's normal background, and are
      ;; marked out by their rules alone.
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Output Box Overlay
    (when (and has-outputs (not (emjupy--cell-outputs-hidden-p cell)))
      (emjupy--render-cell-output cell))

    (insert "\n")))

;;;; MIME types

(defconst emjupy--image-mime-types
  '(("image/png"     . png)
    ("image/jpeg"    . jpeg)
    ("image/gif"     . gif)
    ("image/svg+xml" . svg))
  "MIME types emjupy can render inline, in preference order.
Maps each to the Emacs image type used to display it.")

(defun emjupy--mime-text (value)
  "Return VALUE as a string.
nbformat stores multi-line payloads as one string or a list of lines,
depending on where the JSON came from."
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
FACE is the background the filler carries.  A stretch space aligned to
`emjupy--box-width' stops at the border, where `:extend' would run to
the window's edge."
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
                              ;; Also as `font-lock-face': font-lock removes
                              ;; `face' from what it refontifies, and leaves
                              ;; `font-lock-face' alone.
                              'font-lock-face face
                              ;; Ends where the rule does: WIDTH characters, and
                              ;; a stretch to :align-to WIDTH covers the same.
                              'display `(space :align-to ,width)))
          (setq end (+ end 1)))
        (forward-line 1)))
    end))

(defun emjupy--unface-newlines (start end)
  "Remove the output background from the newlines between START and END.
A newline with a background paints a column past the rule."
  (save-excursion
    (goto-char start)
    (while (< (point) end)
      (end-of-line)
      (when (and (< (point) end) (eq (char-after) ?\n))
        (remove-text-properties (point) (1+ (point))
                                '(face nil font-lock-face nil)))
      (forward-line 1))))

;;;; Colour and tracebacks

(defun emjupy--ansi-render (text)
  "Return TEXT with ANSI escapes applied as faces, or stripped.
With `ansi-color-apply', which comes with Emacs."
  (let ((s (or text "")))
    (if (not emjupy-render-ansi-colors)
        (replace-regexp-in-string "\033\\[[0-9;]*m" "" s)
      (let ((coloured (ansi-color-apply s)))
        ;; `ansi-color-apply' sets `font-lock-face', honoured only with
        ;; font-lock on; emjupy uses plain `face', so the colours are moved
        ;; there.
        (let ((pos 0) (len (length coloured)))
          (while (< pos len)
            (let ((next (or (next-single-property-change
                             pos 'font-lock-face coloured) len))
                  (val (get-text-property pos 'font-lock-face coloured)))
              (when val (put-text-property pos next 'face val coloured))
              (setq pos next))))
        coloured))))

(defun emjupy--cell-traceback (cell)
  "Return CELL's traceback as a string, or nil if it has no error output."
  (let (found)
    (cl-loop for out across (or (emjupy-cell-outputs cell) [])
             until found
             do (when (and (hash-table-p out)
                           (equal (gethash "output_type" out) "error"))
                  (let ((tb (gethash "traceback" out)))
                    (setq found
                          (cond
                           ((stringp tb) tb)
                           ((or (vectorp tb) (listp tb))
                            (mapconcat #'emjupy--mime-text (append tb nil)
                                       "\n"))
                           (t (format "%s: %s"
                                      (gethash "ename" out)
                                      (gethash "evalue" out))))))))
    found))

(defun emjupy--output-face (out)
  "Return the background face for output OUT, or nil to leave it bare.
Images have their own: a tinted band round a white figure reads as a
frame.  stderr that is not a traceback is a warning -- warnings,
logging and progress bars go there."
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

;;;; Rich output

(defconst emjupy--widget-view-mime "application/vnd.jupyter.widget-view+json"
  "MIME type of an output that shows a widget.")

(defvar emjupy-widget-view-functions nil
  "Function returning the text that shows a widget, or nil.
Called with the widget's model id.  Set by the layer that knows about
widgets; nil, or a nil return, shows the output's `text/plain' instead.")

(defconst emjupy--plotly-mime "application/vnd.plotly.v1+json"
  "MIME type of a plotly figure, sent as its JSON specification.")

(defun emjupy--scripted-html (data)
  "Return the `text/html' of DATA if it has a script in it, or nil.
Only that needs opening elsewhere: HTML without a script -- a pandas
table -- says the same in its `text/plain', which is shown instead."
  (let ((html (and data (gethash "text/html" data))))
    (when html
      (let ((text (emjupy--mime-text html)))
        (and (string-match-p "<script" text) text)))))

(defvar emjupy-output-button-map (make-sparse-keymap)
  "Keymap on the line that stands for an interactive output.
Its keys are bound by emjupy-figures.el, where opening an output is.")

(defun emjupy--insert-open-line (what size data)
  "Insert a line standing for an output that is opened elsewhere.
WHAT names it, SIZE is its size in bytes, and DATA is its MIME bundle,
kept on the line so that opening it opens this one."
  (insert (propertize
           (format "▶ %s (%s) -- %s, or click, to open it\n"
                   what (file-size-human-readable size)
                   (substitute-command-keys "\\<emjupy-mode-map>\\[emjupy-open-output]"))
           'face 'link 'mouse-face 'highlight
           'help-echo "Open this output in a window of its own"
           'keymap emjupy-output-button-map
           'emjupy-output-data data)))

(defun emjupy--insert-rich-output (data)
  "Insert the best available representation of the DATA MIME bundle at point.
An inline image where this Emacs can show one, otherwise the bundle's
`text/plain' and a note -- `create-image' failing inside a WebSocket
callback would leave the box silently empty."
  (let* ((image (cl-loop for (mime . type) in emjupy--image-mime-types
                         for payload = (and data (gethash mime data))
                         when payload return (list mime type payload)))
         (text (and data (gethash "text/plain" data)))
         (plotly (and data (gethash emjupy--plotly-mime data)))
         (widget (let ((view (and data
                                  (gethash emjupy--widget-view-mime data))))
                   (and view (run-hook-with-args-until-success
                              'emjupy-widget-view-functions
                              (gethash "model_id" view)))))
         (scripted (and (not plotly) (emjupy--scripted-html data))))
    (cond
     ;; A widget the layer above knows how to show: its controls.
     (widget (insert widget))
     ;; A figure that draws itself in JavaScript: there is nothing to draw
     ;; here, so a line says what it is and opens it.  Nothing runs until
     ;; asked -- opening it runs the notebook's JavaScript.
     (plotly
      (emjupy--insert-open-line "Interactive plotly figure"
                                (length (json-serialize plotly)) data))
     (scripted
      (when text (insert (emjupy--mime-text text) "\n"))
      (emjupy--insert-open-line "Interactive HTML output"
                                (length scripted) data))
     ;; Figures shown on their own, not in the notebook, when so asked.
     ((and image (not emjupy-inline-figures)
           (emjupy--image-displayable-p (nth 1 image)))
      (emjupy--insert-open-line (format "Figure (%s)"
                                        (upcase (symbol-name (nth 1 image))))
                                (length (nth 2 image)) data))
     ((and image (emjupy--image-displayable-p (nth 1 image)))
      (condition-case err
          (progn (insert-image (emjupy--render-image-output (nth 2 image)
                                                            (nth 1 image)))
                 (insert "\n"))
        ;; Everything: the image is the notebook's data, which may be
        ;; anything, and what Emacs makes of bad data is a plain error.
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

;;;; Images, once each

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

(defun emjupy--widgets-first (outputs)
  "Return OUTPUTS with the ones showing widgets first, otherwise in order.
An `interact' sends its figure before its controls; controls go above,
where a notebook puts them.  Only the drawing changes, not the order
saved."
  (let ((widget-p (lambda (o)
                    (let ((d (and (hash-table-p o) (gethash "data" o))))
                      (and (hash-table-p d)
                           (gethash emjupy--widget-view-mime d))))))
    (append (seq-filter widget-p outputs) (seq-remove widget-p outputs))))

(defun emjupy--outputs-for-render (outputs)
  "Return OUTPUTS as a list, with repeated identical images dropped.
Only images, and only against those already in the cell: two identical
prints are two outputs."
  (let ((all (emjupy--widgets-first (append (or outputs []) nil))))
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

;;;; Stream text as a terminal shows it

;; Progress bars that stack -- tqdm nested, or with `position' -- move the
;; cursor up a line to redraw the bar above, and clear what is left of a
;; line.  Only carriage returns were applied, and stale bars piled up.

(defconst emjupy--terminal-codes-re "\e\\[[0-9;]*[ABCDGK]"
  "Escape codes, in stream output, that move the cursor or clear a line.")

(defun emjupy--terminal-codes-p (text)
  "Return non-nil if there are cursor or line-clearing codes in TEXT."
  (and (stringp text) (string-match-p emjupy--terminal-codes-re text)))

(defun emjupy--terminal-column (line col)
  "Return the index in LINE of its column COL, and the columns it has there.
A cons of the index and the column reached: colour codes take no column,
and one starting at an index is passed over to the character after it.
Past the end of LINE, the index is its length and the column its width."
  (let ((i 0) (c 0) (n (length line)) (done nil))
    (while (and (< i n) (not done))
      (cond ((and (eq (aref line i) ?\e)
                  (string-match "\e\\[[0-9;]*m" line i)
                  (= (match-beginning 0) i))
             (setq i (match-end 0)))
            ((= c col) (setq done t))
            (t (setq i (1+ i) c (1+ c)))))
    (cons i c)))

(defun emjupy--terminal-put (line col text)
  "Return LINE with TEXT written over it from column COL."
  (let* ((at (emjupy--terminal-column line col))
         (line (if (< (cdr at) col)
                   (concat line (make-string (- col (cdr at)) ?\s))
                 line))
         (i (car (emjupy--terminal-column line col)))
         (end (car (emjupy--terminal-column line (+ col (length text))))))
    (concat (substring line 0 i) text (substring line (min end
                                                           (length line))))))

(defun emjupy--terminal-apply (text chunk cursor)
  "Return TEXT with CHUNK written to it, as a terminal would show it.
CURSOR is where the last write left the cursor, a cons of line and
column, or nil for the end of TEXT.  Applied: newline, carriage return,
backspace, and the escape codes that move the cursor or clear a line;
colour codes are kept, taking no column, others dropped.  Return a cons
of the new text and the cursor, nil for the end of the text."
  (let* ((lines (vconcat (split-string text "\n")))
         (row (if cursor (car cursor) (1- (length lines))))
         (col (if cursor (cdr cursor)
                (cdr (emjupy--terminal-column (aref lines row)
                                              most-positive-fixnum))))
         (p 0) (n (length chunk)))
    (cl-flet ((line () (aref lines row))
              (set-line (s) (aset lines row s))
              (to-row (r)
                (setq row (max 0 r))
                (when (>= row (length lines))
                  (setq lines (vconcat lines (make-vector (- (1+ row)
                                                             (length lines))
                                                          ""))))))
      (while (< p n)
        (let ((ch (aref chunk p)))
          (cond
           ((eq ch ?\n) (to-row (1+ row)) (setq col 0 p (1+ p)))
           ((eq ch ?\r) (setq col 0 p (1+ p)))
           ((eq ch ?\b) (setq col (max 0 (1- col)) p (1+ p)))
           ((and (eq ch ?\e) (string-match "\e\\[\\([0-9;]*\\)\\([A-Za-z]\\)"
                                           chunk p)
                 (= (match-beginning 0) p))
            ;; All read from the match now: what follows matches again,
            ;; and a colour code read later came back as some other text.
            (let* ((code (match-string 0 chunk))
                   (args (match-string 1 chunk))
                   (num (if (string-empty-p args) 1
                          (max 1 (string-to-number args))))
                   (final (aref (match-string 2 chunk) 0)))
              (setq p (match-end 0))
              (pcase final
                (?A (to-row (- row num)))
                (?B (to-row (+ row num)))
                (?C (setq col (+ col num)))
                (?D (setq col (max 0 (- col num))))
                (?G (setq col (1- num)))
                (?K (let ((at (car (emjupy--terminal-column (line) col))))
                      (pcase (if (string-empty-p args) 0
                               (string-to-number args))
                        (0 (set-line (substring (line) 0
                                                (min at (length (line))))))
                        (1 (set-line (emjupy--terminal-put
                                      (line)
                                      0 (make-string col ?\s))))
                        (_ (set-line "")))))
                ;; a colour: kept where the cursor is, taking no column
                (?m (let ((at (car (emjupy--terminal-column (line) col))))
                      (set-line (concat (substring (line) 0 at) code
                                        (substring (line) at)))))
                (_ nil))))
           ((eq ch ?\e) (setq p (min n (+ p 2))))
           (t
            ;; a run of ordinary characters, written at once
            (let ((end (or (string-match "[\n\r\b\e]" chunk p) n)))
              (set-line (emjupy--terminal-put (line) col (substring chunk
                                                                    p end)))
              (setq col (+ col (- end p)) p end)))))))
    ;; What a cleared bar leaves: spaces at the end of lines.
    (dotimes (i (length lines))
      (aset lines i (replace-regexp-in-string " +\\'" "" (aref lines i))))
    (let* ((last (1- (length lines)))
           (at-end (and (= row last)
                        (>= col
                            (cdr (emjupy--terminal-column
                                  (aref lines last) most-positive-fixnum))))))
      (cons (mapconcat #'identity lines "\n")
            (unless at-end (cons row col))))))

(defun emjupy--collapse-carriage-returns (text)
  "Return TEXT with everything before a carriage return on a line dropped.
As a terminal does, which progress bars rely on."
  (mapconcat (lambda (line)
               (let ((parts (split-string line "\r")))
                 (car (last parts))))
             (split-string text "\n" )
             "\n"))

;;;; Read-only regions

(defun emjupy--make-read-only (start end)
  "Refuse edits between START and END, without walling off the cells.
`rear-nonsticky', so text typed just after a protected region, at the
start of a cell, is not refused with it."
  (add-text-properties start end
                       '(read-only emjupy
                         front-sticky (read-only)
                         rear-nonsticky (read-only))))

(defun emjupy--protect-non-cell-regions ()
  "Mark every region that is not a cell's source read-only.
Then put every header and footer where its cell now is."
  (when emjupy-protect-non-cell-regions
    (let ((inhibit-read-only t)
          (spans nil))
      (cl-loop for cell across
               (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (and (overlayp ov) (eq (overlay-buffer ov)
                                                 (current-buffer)))
                      (push (cons (overlay-start ov) (overlay-end ov)) spans))))
      (setq spans (sort spans #'car-less-than-car))
      (let ((pos (point-min)))
        (dolist (span spans)
          (when (< pos (car span))
            (emjupy--make-read-only pos (car span)))
          (setq pos (max pos (cdr span))))
        (when (< pos (point-max))
          (emjupy--make-read-only pos (point-max))))))
  ;; Run after every change to the buffer's structure, as this is; so the
  ;; rules are put back where their cells are here too, whether or not the
  ;; gaps are protected.
  (emjupy--reconcile-rules))

(provide 'emjupy-render)
;;; emjupy-render.el ends here
