;;; emjupy-faces.el --- How notebooks look: options, faces and colours  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The options and faces that decide how a notebook looks -- the outlines'
;; width, the running indicator, the output band and its colours -- and the
;; colours derived from the theme.  The drawing itself is in
;; emjupy-render.el.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'emjupy-core)


;;;; Options

(defcustom emjupy-large-output-count 50
  "Outputs a cell may have before, opening a notebook, they are hidden.
JupyterLab draws at most 50 of a cell's outputs.  Hidden for drawing
only, not marked in the notebook; \\[emjupy-toggle-cell-output] shows
them.  nil never hides for this."
  :type '(choice (const :tag "Never" nil) natnum)
  :safe (lambda (v) (or (null v) (natnump v)))
  :group 'emjupy-appearance)

(defcustom emjupy-large-output-lines 100
  "Lines of text a cell's output may have before it is hidden on opening.
The Jupyter Notebook scrolls an output past 100 lines; a large output,
drawn, makes a notebook slow to open and to redraw.  Hidden for drawing
only, not marked in the notebook; \\[emjupy-toggle-cell-output] shows
it.  nil never hides for this."
  :type '(choice (const :tag "Never" nil) natnum)
  :safe (lambda (v) (or (null v) (natnump v)))
  :group 'emjupy-appearance)

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
  :group 'emjupy-appearance
  :safe #'emjupy--colour-option-safe-p)

(defcustom emjupy-output-error-color 'auto
  "Background behind an error output.
This covers a traceback, or anything on stderr that reads as a failure.
`auto' tints the buffer background toward red.
See `emjupy-output-color' for the accepted values."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy-appearance
  :safe #'emjupy--colour-option-safe-p)

(defcustom emjupy-output-warning-color 'auto
  "Background behind a warning output.
This covers anything the kernel sent to stderr that is not a traceback.
`auto' tints the buffer background toward yellow."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy-appearance
  :safe #'emjupy--colour-option-safe-p)

(defcustom emjupy-output-image-color "white"
  "Background behind an image output.

Defaults to white because that is what a matplotlib figure is: a tinted
band around a white PNG shows as a frame the plot does not have.  Set it
to nil to use the ordinary output colour instead, or to any colour if
your figures have a different background."
  :type '(choice (color :tag "Explicit colour")
                 (const :tag "Derive from theme" auto)
                 (const :tag "Use the ordinary output colour" nil))
  :group 'emjupy-appearance
  :safe #'emjupy--colour-option-safe-p)

(defcustom emjupy-output-blend 0.08
  "How far to blend toward the foreground for an `auto' output colour.
0 is invisible, 1 is the foreground colour itself."
  :type 'float
  :group 'emjupy-appearance
  :safe #'numberp)

(defcustom emjupy-output-tint-blend 0.16
  "How far to blend toward red or yellow for `auto' tints.
This applies to the error and warning  Larger is louder."
  :type 'float
  :group 'emjupy-appearance
  :safe #'numberp)

(defcustom emjupy-box-width 'window
  "Width of the box-drawing rules that outline each cell.

An integer is used verbatim.  The symbol `window' -- the default --
sizes the rule to the window the notebook is displayed in, so the
outline spans the buffer instead of stopping short at a fixed 80
columns on a wide frame."
  :type '(choice (const :tag "Fit the window" window)
                 (integer :tag "Fixed number of columns"))
  :group 'emjupy-appearance
  :safe #'emjupy--box-width-safe-p)

(defcustom emjupy-box-min-width 60
  "Lower bound for a window-fitted `emjupy-box-width'."
  :type 'integer
  :group 'emjupy-appearance
  :safe #'natnump)

(defcustom emjupy-box-right-margin 2
  "Columns left free at the right edge when fitting rules to the window.

Emacs cannot always be asked exactly how many columns are usable: a
right margin, a `fill-column' indicator, a scroll bar the toolkit reports
oddly, or a line-number width that is off by the separator can each eat
one or two.  Rather than guess, leave a couple spare.  Raise it if the
rules still run past the edge in your setup, lower it to 0 if they stop
short."
  :type 'integer
  :group 'emjupy-appearance
  :safe #'natnump)

(defcustom emjupy-running-indicator ["|" "/" "-" "\\"]
  "Frames cycled in a cell's header while it is executing.

A single-element vector such as [\"*\"] gives Jupyter's static marker
instead of an animation."
  :type '(vector string)
  :group 'emjupy-appearance)

(defcustom emjupy-running-indicator-interval 0.2
  "Seconds between frames of the running indicator."
  :type 'number
  :group 'emjupy-appearance)

(defcustom emjupy-hidden-output-glyph "▼"
  "Glyph marking an output box that is collapsed."
  :type 'string
  :group 'emjupy-appearance
  :safe #'stringp)

(defcustom emjupy-render-ansi-colors t
  "When non-nil, turn ANSI colour escapes in output into real colours.

Libraries like termcolor, rich and colorama emit them, and a kernel
happily passes them through -- so without this the output reads
\"\\033[33mwarning\\033[0m\" instead of a yellow word."
  :type 'boolean
  :group 'emjupy-appearance
  :safe #'booleanp)

(defcustom emjupy-inline-figures t
  "Non-nil means figures -- image outputs -- are drawn in the notebook.
nil shows each as a line instead, and opens it in a window of its own on
\\<emjupy-mode-map>\\[emjupy-open-output] or a click, the way a plotting
window would show it."
  :type 'boolean
  :group 'emjupy-appearance
  :safe #'booleanp)

(defcustom emjupy-deduplicate-image-outputs t
  "When non-nil, render a repeated identical image only once per cell.

A cell whose last expression is a figure gets the SAME picture twice
from the kernel: once as the `execute_result' repr and again as the
inline backend's `display_data'.  That is kernel-side behaviour, so the
duplicate is dropped only at render time -- the cell's `outputs' vector
still holds exactly what the kernel sent, and is saved back to the
.ipynb unchanged, so the file stays byte-faithful for other clients."
  :type 'boolean
  :group 'emjupy-appearance
  :safe #'booleanp)

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

;;;; In a notebook buffer

(defun emjupy-faces-enable ()
  "Follow the theme: re-derive emjupy's colours on enabling or disabling one.
The theme hooks are global, and adding to them again does nothing."
  (when (boundp 'enable-theme-functions)
    (add-hook 'enable-theme-functions #'emjupy--on-theme-change))
  (when (boundp 'disable-theme-functions)
    (add-hook 'disable-theme-functions #'emjupy--on-theme-change)))

(provide 'emjupy-faces)

;;; emjupy-faces.el ends here
