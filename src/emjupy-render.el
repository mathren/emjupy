;;; emjupy-render.el --- Buffer rendering, faces and cell outlines for emjupy  -*- lexical-binding: t; -*-

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

;; Everything that puts pixels on the screen: the theme-derived page
;; colours, the box-drawing outlines (which track the window width),
;; syntax highlighting for code and markdown cells, and the overlay
;; rendering of cells and their outputs.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'emjupy-core)

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

(defun emjupy--window-text-width (win)
  "Return how many columns of text WIN can show without wrapping.

`window-body-width' is not that number.  It counts the line-number
column, which is drawn inside the text area, so with
`display-line-numbers-mode' on a rule sized from it overshoots the right
edge by the width of the numbers -- most visible on a wide window, where
the numbers are widest and the overshoot wraps a whole line."
  (if (not (window-live-p win))
      ;; A window can die between a resize being scheduled and this
      ;; running; a dead one has no width to give, so the default stands.
      emjupy-box-min-width
    ;; Measured keeping point: `window-max-chars-per-line', Emacs's own,
    ;; selects WIN to measure it, and selecting a window moves its buffer's
    ;; point to the window's.  Midway through a redraw that is a stale
    ;; position near the top, and every cell after it was drawn there, on
    ;; top of the one before: the notebook came out backwards, cells
    ;; overlapping and text lost -- when it was shown in a window other
    ;; than the selected one, as opening and closing buffers leaves it.
    (save-excursion
      (let ((cols (window-max-chars-per-line win))
            ;; Read from the window's buffer rather than by selecting it.
            ;; Emacs reports the column's width for the selected window only,
            ;; so for any other it is worked out from the last line's number.
            (numbers (with-current-buffer (window-buffer win)
                       (if (bound-and-true-p display-line-numbers)
                           ;; What Emacs reports, or what the column will need --
                           ;; the digits of the last line number and the space
                           ;; either side -- whichever is more: the report is 0
                           ;; until the window has been displayed once, which is
                           ;; when a notebook is first drawn, and leaves out the
                           ;; gap before the text.  Either way the rule fits.
                           (max (if (eq win (selected-window)) (line-number-display-width) 0)
				(+ 2 (max (length (number-to-string
                                                   (line-number-at-pos (point-max) t)))
                                          (or (bound-and-true-p display-line-numbers-width)
                                              0))))
			 0))))
	(max 1 (- cols numbers))))))

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

Only the header strings are touched -- they are overlay `before-string'
properties, not buffer text -- so this cannot disturb what is being
typed, and does not enter the undo history."
  (let ((buf (current-buffer)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (if (not emjupy--running-cells)
            (emjupy--stop-spinner)
          (setq emjupy--spinner-frame
                (mod (1+ emjupy--spinner-frame) (length emjupy-running-indicator)))
          (when emjupy--buffer-notebook
            (cl-loop for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
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

(defun emjupy--cell-label (cell)
  "Return the header label for CELL's input box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[In: %s] %s"
            (cond
             ;; Jupyter's convention: a running cell shows a marker where its
             ;; execution count will go.
             ((emjupy--cell-running-p cell)
              (aref emjupy-running-indicator
                    (mod emjupy--spinner-frame (length emjupy-running-indicator))))
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

(defun emjupy--reconcile-rules ()
  "Draw every rule as the continuation of the content line above it.

`display-line-numbers-mode' numbers the first screen line of each buffer
line.  A header drawn as a `before-string' on a cell's first line is
that screen line, so the number went on the rule and the first line of
code got none; the footer, drawn at the start of the separator line
after a cell, took that line's number.  So each box's footer, and the
header of the box after it, are shown instead as further screen lines of
the last line of the box -- continuation lines, which are never numbered
and keep the number column blank, so every rule lines up -- and the
empty separator line between cells is made invisible.  What is on screen
is unchanged; the numbers move to where the text is.

The first cell\\='s header has no line above it and stays where it was.

The rule strings are kept on each box in `emjupy-header' and
`emjupy-footer'; a `before-string' or `after-string' set on a box is
taken as its new rule.  Run after anything that redraws a box or
changes the buffer\\'s structure; it rebuilds every companion, so it
cannot leave one behind."
  (when (bound-and-true-p emjupy--buffer-notebook)
    ;; every box, in buffer order, with its rules taken into keeping
    (let ((boxes nil))
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (dolist (ov (list (emjupy-cell-overlay cell) (emjupy-cell-output-ov cell)))
                    (when (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer)))
                      (overlay-put ov 'emjupy-header (emjupy--overlay-header ov))
                      (overlay-put ov 'emjupy-footer (emjupy--overlay-footer ov))
                      (overlay-put ov 'before-string nil)
                      (overlay-put ov 'after-string nil)
                      (push ov boxes))))
      (setq boxes (sort boxes (lambda (a b) (< (overlay-start a) (overlay-start b)))))
      ;; the previous companions, all of them
      (dolist (o (overlays-in (point-min) (point-max)))
        (when (overlay-get o 'emjupy-rules-of) (delete-overlay o)))
      (dolist (ov boxes) (overlay-put ov 'emjupy-companions nil))
      (let ((prev nil))
        (dolist (ov (append boxes (list nil)))
          (let* ((header (and ov (overlay-get ov 'emjupy-header)))
                 (footer (and prev (overlay-get prev 'emjupy-footer)))
                 (rules (mapconcat (lambda (r) (string-remove-suffix "\n" r))
                                   (seq-filter (lambda (r) (and r (not (string-empty-p r))))
                                               (list footer header))
                                   "\n"))
                 (anchor (and prev (1- (overlay-end prev)))))
            (cond
             ;; after a box: on its last newline, as continuation lines
             ((and anchor (eq (char-after anchor) ?\n))
              (let* ((gap-end (and ov (overlay-start ov)))
                     (gap (and gap-end (< (overlay-end prev) gap-end))))
                (cond
                 ;; Between two boxes, with the separator line between
                 ;; them.  It must not begin a screen line: one is numbered
                 ;; by the line it starts on, so the line after a hidden
                 ;; separator showed the separator's number, and its own was
                 ;; never seen -- each cell's numbers went 29, 31.  So the
                 ;; box's last newline is hidden instead, the rules hang on
                 ;; the separator as continuation lines, which are never
                 ;; numbered, and the separator's newline ends them: the
                 ;; cell after starts a screen line of its own, numbered.
                 (gap
                  ;; The box's last newline is drawn as itself followed by
                  ;; the rules: a `display' replacing it, not a hidden
                  ;; newline -- Emacs draws no overlay string at a hidden
                  ;; position, so the rules vanished.  The screen lines the
                  ;; rules make begin mid-line, and are never numbered; the
                  ;; separator's newline, still there, ends the last of them.
                  (let ((o (make-overlay anchor (1+ anchor) nil t nil))
                        (text (concat "\n" rules)))
                    ;; point there shows at the end of the text, before the rule
                    (put-text-property 0 1 'cursor t text)
                    (overlay-put o 'display text)
                    (overlay-put o 'emjupy-overlay 'rules)
                    (overlay-put o 'emjupy-rules-of prev)
                    (push o (overlay-get prev 'emjupy-companions))))
                 ;; Followed at once by another box -- a cell by its output
                 ;; -- or the last box: the rules hang on its own last line.
                 ((not (string-empty-p rules))
                  (let ((o (make-overlay anchor (1+ anchor) nil t nil))
                        (text (concat "\n" rules)))
                    (put-text-property 0 1 'cursor t text)
                    (overlay-put o 'before-string text)
                    (overlay-put o 'emjupy-overlay 'rules)
                    (overlay-put o 'emjupy-rules-of prev)
                    (push o (overlay-get prev 'emjupy-companions)))))))
             ;; the first box, or one whose line above cannot carry it
             (ov (overlay-put ov 'before-string header)
                 (when prev (overlay-put prev 'after-string footer)))
             (prev (overlay-put prev 'after-string footer))))
          (setq prev ov))))))

(defun emjupy--line-numbers-toggled ()
  "Redraw the rules after line numbers are turned on or off.
The number column takes width from the page, so the rules were drawn
too wide for it, and wrapped."
  (when (derived-mode-p 'emjupy-mode)
    (emjupy--refresh-box-rules t)))

(defun emjupy--rule (label &optional corner)
  "Return a propertized box rule line showing LABEL.
CORNER is the left corner glyph; with LABEL nil a footer is returned."
  (let* ((text (if (and label (not (eq corner 'footer)))
                   (emjupy--box-header label corner)
                 (emjupy--box-footer label)))
         (end (if (string-suffix-p "\n" text) (1- (length text)) (length text))))
    ;; Face the glyphs but NOT the closing newline.  A newline carrying a
    ;; background paints a column of its own past the last box character, so
    ;; on any theme where the inherited face has a background the rule
    ;; separating input from output stuck out one column to the right.
    (put-text-property 0 end 'face 'emjupy-box-line text)
    text))

;; --- Keeping the rules the right width ------------------------------------

(defvar-local emjupy--last-box-width nil
  "Width the visible box rules were last drawn at.")

(defvar emjupy-box-width-changed-functions nil
  "Functions called with a notebook and its new box width, when it changes.")

(defvar-local emjupy--refontifying nil
  "Non-nil while emjupy is re-applying faces, to stop the hook recursing.")

(defun emjupy--refresh-box-rules (&optional force)
  "Redraw cell outlines at the current window width, if it changed.
With FORCE non-nil, redraw even when the width is unchanged.

The rules live in overlay before/after-strings, which are built once at
render time -- so without this a rule sized for a full-screen frame
stays that long when the window shrinks and wraps onto a second line.
Only the overlay strings are rebuilt, not the buffer text, so point,
markers and the undo history are all untouched."
  (let ((width (emjupy--box-width)))
    (when (and emjupy--buffer-notebook
               (or force (not (eql width emjupy--last-box-width))))
      (let ((changed (not (eql width emjupy--last-box-width))))
        (setq emjupy--last-box-width width)
        ;; Layers above this one -- the kernel, told how wide to draw --
        ;; hear about it here, since this one cannot name them.
        (when changed
          (run-hook-with-args 'emjupy-box-width-changed-functions
                              emjupy--buffer-notebook width)))
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell))
                        (out (emjupy-cell-output-ov cell)))
                    (when (overlayp ov)
                      (overlay-put ov 'emjupy-header nil)
                      (overlay-put ov 'before-string
                                   (emjupy--rule (emjupy--cell-label cell)))
                                  ;; A cell with output has no footer of its own: the
                      ;; output box's header doubles as its bottom edge.
                      (overlay-put ov 'after-string
                                   (if (overlayp out) "" (emjupy--rule nil))))
                    (when (overlayp out)
                      (overlay-put out 'emjupy-header nil)
                      (overlay-put out 'before-string
                                   (emjupy--rule (emjupy--cell-out-label cell) "├"))
                      (overlay-put out 'after-string (emjupy--rule nil))
                      ;; The output band is padded to a column, so it has to
                      ;; be re-aligned at the new width as well.
                      (let ((inhibit-read-only t)
                            (buffer-undo-list t)
                            ;; emjupy's own drawing: not an edit to re-highlight
                            (emjupy--refontifying t))
                        (emjupy--repad-output cell)))))
      (emjupy--reconcile-rules))))

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
(eval-when-compile (require 'org) (require 'ansi-color))
(declare-function org-create-formula-image "org")
(declare-function ansi-color-apply "ansi-color" (string))
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
    ('math-preview (and emjupy-probe-environment
                        (executable-find "math-preview") 'math-preview))
    ('org (and emjupy-probe-environment (executable-find "latex") 'org))
    (_ (cond ((not emjupy-probe-environment) nil)
             ((executable-find "latex") 'org)
             ((executable-find "math-preview") 'math-preview)
             (t nil)))))

(defun emjupy--latex-math (fragment)
  "Return FRAGMENT as LaTeX in math mode.

A fragment that brings its delimiters -- $...$, $$...$$, \\(...\\),
\\[...\\] -- is used as written, so display math stays display math.  A
bare body is taken as inline math.  The body alone is what used to be
handed to LaTeX, which reads it in TEXT mode: $\\frac{x}{y}$ came out as
an upright x above a stray bar over y."
  (if (string-match-p "\\`[ \t\n]*\\(?:\\$\\|\\\\[[(]\\)" fragment)
      fragment
    (concat "$" fragment "$")))

(defun emjupy--latex-image (body)
  "Render BODY, a LaTeX math fragment, to an image spec, or nil.
BODY may bring its delimiters or not; see `emjupy--latex-math\='."
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
              (org-create-formula-image (emjupy--latex-math body) file opts
                                        (current-buffer) 'dvipng))
            (when (and (file-exists-p file) (emjupy--image-displayable-p 'png))
              (create-image file 'png nil :ascent 'center)))
        ;; Everything: Org reports a failed LaTeX run as a plain error.
        (error
         (message "[emjupy] LaTeX preview failed: %s" (error-message-string err))
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
                       "LaTeX source revealed; \\[emjupy-toggle-latex-preview] re-renders"))))))

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
Code cells use `python-mode', which ships with Emacs core.  Markdown
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
      ;; python-mode guesses `python-indent-offset' on entry and says so
      ;; when it cannot.  A cell is a fragment, so it usually cannot -- and
      ;; this runs once per code cell, filling the echo area with
      ;; "Can't guess python-indent-offset" on every redraw.
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
Only properties are touched; the buffer text itself is left alone.

Copied to `font-lock-face' as well, for the same reason as everything
else painted here: font-lock removes `face' from the regions it
refontifies, so the ANSI colours of a traceback and the highlighting
inside it lasted only until the next pass."
  (let ((i 0) (len (length string)))
    ;; Padding is never source.  This re-highlights a cell, and the faces it
    ;; clears first are the ones it is about to replace -- but if it is ever
    ;; asked about a region that reaches into an output box, the padding
    ;; there loses its background and nothing puts it back, because the
    ;; string being copied from has no padding in it.  The pads carry a mark
    ;; of their own; it is cheaper to respect it than to rely on the region
    ;; always being right.
    (emjupy--without-pads start (+ start len)
      (lambda (from to)
        (remove-text-properties from to '(face nil font-lock-face nil))))
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

Output is skipped whole -- its text as well as its padding.  Both are
painted when the box is drawn, from the outputs themselves, and neither
has anything to do with the source being re-highlighted; a region that
reaches in here clears them and puts nothing back, because the source it
copies from contains no output.  Skipping only the padding left the text
beside it exposed to the same thing."
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
      (condition-case err
          (let* ((lo (max (point-min) (min beg (point-max))))
                 (cell (or (and (< lo (point-max)) (get-text-property lo 'emjupy-cell))
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

(defcustom emjupy-hidden-output-glyph "▼"
  "Glyph marking an output box that is collapsed."
  :type 'string
  :group 'emjupy)

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
    (define-key map [mouse-1] 'emjupy-toggle-output-at-click)
    ;; Without this, the press begins a drag-selection and the release is
    ;; never delivered as a click.
    (define-key map [down-mouse-1] #'ignore)
    map)
  "Keymap active on the marker of a collapsed output box.")

(defun emjupy--hidden-output-label (cell)
  "Return the label shown on CELL's footer while its output is hidden.

The marker carries the cell's id and a keymap of its own, so a click on
it can say which cell was meant.  Only the marker is live: the rest of
the rule is an ordinary line, and the pointer highlights the part that
does something."
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

(defun emjupy--fold-output-lines (start end width)
  "Break every line between START and END wider than WIDTH columns.

Returns (NEW-END . WIDEST), WIDEST being the widest line as it was
before breaking.  Nothing in the output decides how wide it is drawn: a
progress bar sized to a wide terminal, a long log line or a wide table
ran past the cell's right-hand edge.  Breaking the lines where the box
ends keeps every kind of output inside it.

Only the drawing changes.  The cell's outputs keep the text as the
kernel sent it, so saving writes it back unchanged.  Each break is
marked with `emjupy-fold', so it can be told from a line the output
really had."
  (let ((end (copy-marker end))
        (widest 0))
    (save-excursion
      (goto-char start)
      (while (< (point) end)
        (let ((bol (point)))
          (end-of-line)
          (let ((eol (min (point) end)))
            (setq widest (max widest (string-width
                                      (buffer-substring-no-properties bol eol))))
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

(defun emjupy--render-cell-output (cell)
  "Insert CELL\='s output box at point and give it its overlay.

Separate from `emjupy--render-cell\=' so that output arriving can be drawn
on its own.  Rebuilding the whole notebook for one cell\='s output is what
made a `tqdm\=' loop crawl, threw away the undo history on every message,
and restarted fontification from scratch each time."
  (let* ((outputs (emjupy-cell-outputs cell))
         (has-outputs (and outputs (> (length outputs) 0))))
    (when has-outputs
      (let ((out-start (point))
            ;; widest line as the kernel sent it, before folding
            (widest 0))
        (cl-loop for out in (emjupy--outputs-for-render outputs)
                 do (let ((out-type (gethash "output_type" out))
                          (piece-start (point)))
                      (cond
                       ((string= out-type "stream")
                        ;; Carriage returns applied here as well as on
                        ;; arrival: a notebook saved by Jupyter stores the
                        ;; stream exactly as written, so a progress bar comes
                        ;; back as every one of its updates joined by ^M.
                        (insert (emjupy--ansi-render
                                 (emjupy--collapse-carriage-returns
                                  (emjupy--mime-text (gethash "text" out))))))
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
                                     do (insert (emjupy--ansi-render
                                                 (emjupy--mime-text line))
                                                "\n"))))))
                      ;; Keep the piece inside the box, whatever kind of output it is.
                      (let ((folded (emjupy--fold-output-lines
                                     piece-start (point) (emjupy--box-width))))
                        (goto-char (car folded))
                        (setq widest (max widest (cdr folded))))
                      ;; Paint this piece, not the whole box: one cell can
                      ;; hold a figure, a warning and a traceback at once, and
                      ;; each should read as what it is.  A text property
                      ;; rather than an overlay face, so it does not override
                      ;; the font-lock colours on the text underneath.
                      (let ((face (emjupy--output-face out)))
                        (when face
                          (font-lock-prepend-text-property
                           piece-start (point) 'face face)
                          (font-lock-prepend-text-property
                           piece-start (point) 'font-lock-face face)
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
          (overlay-put ov 'emjupy-overlay 'output)
          ;; What a resize needs to know: at what width the lines were
          ;; folded, and how wide the widest was before.  Output whose widest
          ;; line fits both the old and the new width looks the same at
          ;; either, and is not redrawn.
          (overlay-put ov 'emjupy-fold-width (emjupy--box-width))
          (overlay-put ov 'emjupy-widest widest)
          (overlay-put ov 'emjupy-header nil)
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          ;; No face on the overlay: each output piece paints itself, so a
          ;; figure, a warning and a traceback in one cell each read as what
          ;; they are.  An overlay face here would override all of them.
          (setf (emjupy-cell-output-ov cell) ov))))))

(defvar emjupy-redraw-recover-function nil
  "Function putting the buffer back in step with its cells, or nil.
Called with one argument: non-nil if what is typed may not be in the
cells yet, and is to be kept.  Set by `emjupy-cells', which can sync and
redraw, when a notebook buffer starts; this file is below it.")

(defvar emjupy-after-redraw-function nil
  "Function called after a redraw finishes, or nil.
Set by `emjupy-cells', which uses it to check the buffer when Emacs is
next idle; this file is below it.")

(defvar-local emjupy--recovering nil
  "Non-nil while the buffer is being redrawn after a redraw failed.")

(defmacro emjupy--atomic-redraw (keep-typing &rest body)
  "Run BODY, a redraw, so that it happens whole or is put right.
An error or a quit partway leaves the buffer half drawn -- text replaced
and its overlays not yet, or the other way round -- which is scrambling:
what is shown and what the cells hold come apart.  The cells are the
notebook and the buffer a drawing of them, so when BODY does not finish
the buffer is redrawn from them at once, and the error goes on up.
KEEP-TYPING non-nil means text typed may not be in the cells yet and is
synced first; a structural command syncs before it starts, and passes
nil."
  (declare (indent 1) (debug t))
  (let ((done (make-symbol "done")))
    `(let ((,done nil))
       (unwind-protect
           (prog1 (progn ,@body) (setq ,done t))
         (cond
          ((and (not ,done) (not emjupy--recovering) emjupy-redraw-recover-function)
           (let ((emjupy--recovering t))
             (funcall emjupy-redraw-recover-function ,keep-typing)))
          ((and ,done (not emjupy--recovering) emjupy-after-redraw-function)
           (funcall emjupy-after-redraw-function)))))))

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

Undo entries come in several shapes and each hides its positions
somewhere different.  Missing one does not fail loudly -- it leaves a
position pointing at the wrong text, and undo then rewrites the wrong
text.  The shapes are those Emacs documents for `buffer-undo-list'."
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
   ;; (nil PROP VAL BEG . END) -- a property change, recorded when a mode
   ;; puts a text property on the buffer.  A dotted list: `butlast' and
   ;; `length' fail on it -- "listp, END" -- so it is taken apart by its
   ;; shape, which is fixed: three elements, then BEG and END.
   ((and (null (car entry)) (consp (cdr entry)) (consp (cddr entry))
         (consp (cdddr entry))
         (integerp (car (cdddr entry))) (integerp (cdr (cdddr entry))))
    (pcase-let ((`(nil ,prop ,val ,beg . ,end) entry))
      `(nil ,prop ,val ,(funcall fn beg) . ,(funcall fn end))))
   ;; (apply DELTA BEG END FUN . ARGS)
   ((and (eq (car entry) 'apply) (integerp (nth 2 entry)) (integerp (nth 3 entry)))
    (append (list (nth 0 entry) (nth 1 entry)
                  (funcall fn (nth 2 entry))
                  (funcall fn (nth 3 entry)))
            (nthcdr 4 entry)))
   (t entry)))

(defun emjupy--undo-adjust (start old-end delta)
  "Keep the undo history usable across a replacement of START..OLD-END.

Three cases, and they are all the story:

- entries wholly before START are untouched by the edit and stay as they
  are;
- entries describing text inside the replaced region describe text that
  no longer exists, and go;
- entries after the region are still valid but have moved, so their
  positions shift by DELTA.

Without the third case this is nearly pointless in practice: output
usually arrives from a cell ABOVE the one being edited, so the edits
worth keeping are precisely the ones that moved.  Getting the shift
wrong is worse than dropping everything -- a wrong position does not
raise an error, it quietly rewrites the wrong text -- which is why each
entry shape is handled explicitly rather than by pattern-guessing."
  (unless (bound-and-true-p emjupy--in-cell-step)
  (let ((transform
         (lambda (entry)
           (let ((p (emjupy--undo-entry-position entry)))
             (cond
              ((null p) entry)
              ((< p start) entry)
              ((< p old-end)
               ;; Text inside the region is gone, so an entry describing it
               ;; goes too.  Structural commands never get here -- they
               ;; suspend this and are undone as a whole -- so this is for
               ;; redraws that are not undo steps, such as output arriving.
               :emjupy-drop)
              (t (emjupy--undo-map-positions
                  entry (lambda (x) (if (>= x old-end) (+ x delta) x))))))))
        (seen (make-hash-table :test 'eq)))
    ;; In place, cell by cell.  An undo in progress replays from
    ;; `pending-undo-list', and `primitive-undo' walks that through a local
    ;; variable it writes back when it is done -- so a structural change
    ;; made BY an undo, deleting the cell an insertion made, could not be
    ;; seen by rebinding the list: the next step replayed at the old
    ;; position and rewrote the wrong text, a typed character surviving
    ;; while its neighbour was deleted.  The list's cons cells are shared,
    ;; though -- by it, by `buffer-undo-list' and by that local variable --
    ;; so changing the entries in them reaches all three.  Each cell once:
    ;; the two lists share their tails.  An entry to drop cannot be
    ;; unlinked in place, so it becomes one that does nothing.
    ;; All or nothing.  Every new entry is worked out before any is
    ;; changed: changed in place one by one, a failure partway left some
    ;; entries shifted and the rest not, and the next undo wrote text in
    ;; the wrong places -- scrambling the notebook.
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
        ;; Everything: an entry is whatever a mode recorded, and none of it
        ;; has been changed yet.  The entries from START on are stale now,
        ;; though, and replaying one writes in the wrong place, so they go:
        ;; some undo history lost is safe, wrong history is not.
        (error
         (emjupy--undo-drop-from start)
         (when (consp pending-undo-list)
           (setq pending-undo-list
                 (cl-remove-if (lambda (e) (let ((p (emjupy--undo-entry-position e)))
                                             (and p (>= p start))))
                               pending-undo-list)))
         (message "[emjupy] Undo history after the redrawn output was dropped: %s"
                  (error-message-string err)))))
    ;; Outside an undo the placeholders can go for good.
    (unless (or undo-in-progress (consp pending-undo-list))
      (when (consp buffer-undo-list)
        (setq buffer-undo-list
              (delete (list 'apply #'ignore :emjupy-dropped) buffer-undo-list)))))))

(defun emjupy--undo-drop-from (pos)
  "Drop undo entries referring to POS or later in this buffer.

An edit at POS shifts every position at or after it, and undo entries
hold plain buffer positions rather than markers, so nothing adjusts them.
Replaying a stale one does not fail loudly; it rewrites the wrong text.

Entries wholly BEFORE the edit are untouched by it and are kept, which is
the point: redrawing one cell's output no longer costs the history of
every edit made anywhere else in the notebook.  See notes_undo.org."
  (when (listp buffer-undo-list)
    (setq buffer-undo-list
          (cl-remove-if (lambda (entry)
                          (let ((p (emjupy--undo-entry-position entry)))
                            (and p (>= p pos))))
                        buffer-undo-list))))

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

(defun emjupy--refresh-cell-output (cell)
  "Redraw CELL's output box in place, leaving the rest of the buffer alone.

Only the text between the end of the cell's source and the end of its
output box is replaced.  Returns non-nil when that could be done; nil
means the caller should fall back to a full redraw."
  (let ((src (emjupy-cell-overlay cell))
        (out (emjupy-cell-output-ov cell)))
    (when (and (overlayp src) (eq (overlay-buffer src) (current-buffer)))
      (let* ((live-out (and (overlayp out)
                            (eq (overlay-buffer out) (current-buffer))))
             (start (if live-out (overlay-start out) (overlay-end src)))
             (end (if live-out (overlay-end out) start)))
        (emjupy--atomic-redraw t
        (let ((new-end start))
          (let ((inhibit-read-only t)
                (buffer-undo-list t)
                ;; emjupy's own drawing: not an edit to re-highlight
                (emjupy--refontifying t))
            (when live-out (delete-overlay out))
            (setf (emjupy-cell-output-ov cell) nil)
            (delete-region start end)
            (save-excursion
              (goto-char start)
              (emjupy--render-cell-output cell)
              (setq new-end (point)))
            (emjupy--refresh-cell-header cell)
            ;; The bottom edge belongs to whichever box is last.  With
            ;; output, that is the output box's own footer and the cell has
            ;; none; without, the cell must carry it again -- otherwise
            ;; clearing output left the cell with no bottom at all.
            (emjupy--refresh-cell-footer cell)
            (emjupy--protect-non-cell-regions))
          ;; Done after the edit, since the shift is the size it turned out
          ;; to be rather than the size expected.
          (emjupy--undo-adjust start end (- new-end end))))
        t))))

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
    ;; Always one newline after the source, which sync always removes.  It
    ;; was added only when the source did not already end in one, so a
    ;; source ending in a newline lost it on the first sync -- and every
    ;; save of such a notebook changed its cells.  Found by the
    ;; round-trip corpus.
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
           ;; When output follows, its header line doubles as this box's
           ;; closing edge -- no separate footer, no gap between the two.
           ;; Hidden output is not drawn at all -- the outputs stay in the
           ;; cell, so showing them again is a redraw and never a re-run --
           ;; and this cell's own footer says so.
           (hidden (and has-outputs (emjupy--cell-outputs-hidden-p cell)))
           (footer (cond
                    (hidden (emjupy--rule (emjupy--hidden-output-label cell) 'footer))
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
FACE is the background the filler carries.

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
                              ;; Also as `font-lock-face': font-lock owns the
                              ;; `face' property and removes it from any
                              ;; region it refontifies, which is how the
                              ;; output background disappeared a moment after
                              ;; a cell was redrawn.  It does not touch
                              ;; `font-lock-face', and displays it as a face
                              ;; whenever font-lock is on.  Both are set so
                              ;; the tint survives either way.
                              'font-lock-face face
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
        (remove-text-properties (point) (1+ (point))
                                '(face nil font-lock-face nil)))
      (forward-line 1))))

(defun emjupy--repad-output (cell)
  "Re-align CELL's output padding after the window width changed.

Output folded to the old width is drawn again for the new one -- but
only if it has a line wider than the narrower of the two, since
anything else looks the same at either and redrawing it would cost a
cell's worth of work for nothing."
  (let ((ov (emjupy-cell-output-ov cell))
        (width (emjupy--box-width)))
    (when (and (overlayp ov)
               (let ((folded-at (overlay-get ov 'emjupy-fold-width))
                     (widest (or (overlay-get ov 'emjupy-widest) 0)))
                 (and folded-at (/= folded-at width)
                      (> widest (min folded-at width)))))
      (emjupy--refresh-cell-output cell)
      (setq ov nil))
    (when (overlayp ov)
      ;; Walk the pad markers, not the characters.  There is one pad per
      ;; output LINE and there can be tens of thousands of characters -- an
      ;; inline figure is a single output of some 18,000 -- so stepping
      ;; through them one at a time made re-aligning proportional to the size
      ;; of the output rather than to the number of lines in it.
      (let ((pos (overlay-start ov))
            (end (overlay-end ov)))
        (while (and pos (< pos end))
          (if (get-text-property pos 'emjupy-pad)
              (progn
                (put-text-property pos (1+ pos) 'display
                                   `(space :align-to ,width))
                (setq pos (1+ pos)))
            (setq pos (next-single-property-change pos 'emjupy-pad nil end))))))))

(defcustom emjupy-render-ansi-colors t
  "When non-nil, turn ANSI colour escapes in output into real colours.

Libraries like termcolor, rich and colorama emit them, and a kernel
happily passes them through -- so without this the output reads
\"\\033[33mwarning\\033[0m\" instead of a yellow word."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--ansi-render (text)
  "Return TEXT with ANSI escapes applied as faces, or stripped.

`ansi-color-apply' ships with Emacs and converts the escapes into text
properties.  Tracebacks used to have them stripped and streams showed
them raw; both now go through here."
  (let ((s (or text "")))
    (if (not emjupy-render-ansi-colors)
        (replace-regexp-in-string "\033\\[[0-9;]*m" "" s)
      (require 'ansi-color)
      (let ((coloured (ansi-color-apply s)))
        ;; `ansi-color-apply' marks its output with `font-lock-face', which
        ;; is only honoured where font-lock is running.  emjupy paints cells
        ;; with plain `face' properties and leaves font-lock off, so without
        ;; this the escapes vanish and the colour goes with them.
        (let ((pos 0) (len (length coloured)))
          (while (< pos len)
            (let ((next (or (next-single-property-change pos 'font-lock-face coloured) len))
                  (val (get-text-property pos 'font-lock-face coloured)))
              (when val (put-text-property pos next 'face val coloured))
              (setq pos next))))
        coloured))))

(defun emjupy--cell-traceback (cell)
  "Return CELL\='s traceback as a string, or nil if it has no error output."
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
                            (mapconcat #'emjupy--mime-text (append tb nil) "\n"))
                           (t (format "%s: %s"
                                      (gethash "ename" out)
                                      (gethash "evalue" out))))))))
    found))

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

(defcustom emjupy-inline-figures t
  "Whether figures -- image outputs -- are drawn in the notebook.
nil shows each as a line instead, and opens it in a window of its own on
\\<emjupy-mode-map>\\[emjupy-open-output] or a click, the way a plotting
window would show it."
  :type 'boolean
  :group 'emjupy)

(defconst emjupy--widget-view-mime "application/vnd.jupyter.widget-view+json"
  "MIME type of an output that shows a widget.")

(defvar emjupy-widget-view-function nil
  "Function returning the text that shows a widget, or nil.
Called with the widget\'s model id.  Set by the layer that knows about
widgets; nil, or a nil return, shows the output\'s `text/plain\=' instead.")

(defconst emjupy--plotly-mime "application/vnd.plotly.v1+json"
  "MIME type of a plotly figure, sent as its JSON specification.")

(defvar emjupy-open-output-function nil
  "Function opening an output on its own, called with its MIME bundle.
Set by `emjupy-figures', the layer that opens outputs, when a notebook
buffer starts; this file is below it, so it does not call it by name.")

(defun emjupy--scripted-html (data)
  "Return the `text/html' of DATA if it has a script in it, or nil.
Only that needs opening elsewhere: HTML without a script -- a pandas
table -- says the same in its `text/plain', which is shown instead."
  (let ((html (and data (gethash "text/html" data))))
    (when html
      (let ((text (emjupy--mime-text html)))
        (and (string-match-p "<script" text) text)))))

(defvar emjupy-output-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emjupy-open-output-at-point)
    (define-key map [mouse-1] #'emjupy-open-output-at-point)
    map)
  "Keymap on the line that stands for an interactive output.")

(defun emjupy-open-output-at-point (&optional event)
  "Open the interactive output whose line is at point, or was clicked.
EVENT is the mouse event, when there was one."
  (interactive (list last-nonmenu-event))
  (let ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point))))
    (if emjupy-open-output-function
        (funcall emjupy-open-output-function (get-text-property pos 'emjupy-output-data))
      (user-error "Opening an output needs emjupy-figures"))))

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

Prefers an inline image when this Emacs can genuinely display one, and
otherwise falls back to the bundle's own `text/plain' representation
plus a short note.  Without that fallback a figure renders as a
silently empty output box on a terminal or no-image Emacs: rendering
happens inside the WebSocket callback, where websocket.el swallows the
`Invalid image type' error `create-image' raises."
  (let* ((image (cl-loop for (mime . type) in emjupy--image-mime-types
                         for payload = (and data (gethash mime data))
                         when payload return (list mime type payload)))
         (text (and data (gethash "text/plain" data)))
         (plotly (and data (gethash emjupy--plotly-mime data)))
         (widget (let ((view (and data (gethash emjupy--widget-view-mime data))))
                   (and view emjupy-widget-view-function
                        (funcall emjupy-widget-view-function
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
      (emjupy--insert-open-line "Interactive HTML output" (length scripted) data))
     ;; Figures shown on their own, not in the notebook, when so asked.
     ((and image (not emjupy-inline-figures)
           (emjupy--image-displayable-p (nth 1 image)))
      (emjupy--insert-open-line (format "Figure (%s)" (upcase (symbol-name (nth 1 image))))
                                (length (nth 2 image)) data))
     ((and image (emjupy--image-displayable-p (nth 1 image)))
      (condition-case err
          (progn (insert-image (emjupy--render-image-output (nth 2 image) (nth 1 image)))
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
An `interact\=' sends its figure before its controls, and after each move
clears the figure but not the controls, so the controls came below the
figure at first and above it from then on.  Above is where a notebook
puts them.  Only the drawing changes: the cell keeps its outputs in the
order the kernel sent them, which is what is saved."
  (let ((widget-p (lambda (o)
                    (let ((d (and (hash-table-p o) (gethash "data" o))))
                      (and (hash-table-p d) (gethash emjupy--widget-view-mime d))))))
    (append (seq-filter widget-p outputs) (seq-remove widget-p outputs))))

(defun emjupy--outputs-for-render (outputs)
  "Return OUTPUTS as a list, with repeated identical images dropped.

Only image-bearing outputs are collapsed, and only against images
already seen in the same cell: two `print' calls emitting the same text
are genuinely two outputs and both must show."
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

(defun emjupy--collapse-carriage-returns (text)
  "Return TEXT with everything before a carriage return on a line dropped.

What a terminal does, and what a progress bar relies on: a carriage
return takes the cursor to
the start of the line and the next write covers what was there."
  (mapconcat (lambda (line)
               (let ((parts (split-string line "\r")))
                 (car (last parts))))
             (split-string text "\n" )
             "\n"))

(defcustom emjupy-protect-non-cell-regions t
  "When non-nil, make everything outside a cell\='s source read-only.

The rules, the gutters between cells and the output boxes belong to no
cell.  Text typed there is stored nowhere and disappears at the next
redraw, so it is better refused than silently lost."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--make-read-only (start end)
  "Refuse edits between START and END, without walling off the cells.

`rear-nonsticky\' matters: without it, text inserted immediately AFTER a
protected region inherits the property, so typing at the very start of a
cell would be refused along with the gutter above it."
  (add-text-properties start end
                       '(read-only emjupy
                         front-sticky (read-only)
                         rear-nonsticky (read-only))))

(defun emjupy--protect-non-cell-regions ()
  "Mark every region that is not a cell\='s source read-only.
Then put every header and footer where its cell now is."
  (when emjupy-protect-non-cell-regions
    (let ((inhibit-read-only t)
          (spans nil))
      (cl-loop for cell across (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (and (overlayp ov) (eq (overlay-buffer ov) (current-buffer)))
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
