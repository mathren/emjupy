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
;; The notebook reads as a page: cells sit on a lighter surface, the gaps
;; between them show a slightly contrasting canvas. Both colours are derived
;; from the active theme rather than hard-coded, so this follows whatever
;; theme is loaded instead of fighting it.

(defcustom emjupy-canvas-color 'auto
  "Background shown OUTSIDE cell outlines.

`auto' (the default) derives a colour from the current theme by
blending the default background `emjupy-canvas-blend' of the way
toward the default foreground -- which darkens a light theme and
lightens a dark one, so it works both ways round.

A colour string (e.g. \"#e8e8e8\") is used verbatim. nil disables the
effect, leaving the whole buffer the theme's normal background."
  :type '(choice (const :tag "Derive from theme" auto)
                 (color :tag "Explicit colour")
                 (const :tag "Disabled" nil))
  :group 'emjupy)

(defcustom emjupy-canvas-blend 0.12
  "How far to blend toward the foreground for an `auto' canvas colour.
0 is invisible, 1 is the foreground colour itself."
  :type 'float
  :group 'emjupy)

(defface emjupy-cell
  ;; Deliberately specifies NOTHING by default, and only ever gains a
  ;; `:background'. Two reasons, both of which bit:
  ;;
  ;; 1. This face is applied as an OVERLAY face over cell text, and overlay
  ;;    faces merge on top of text properties. Any attribute it specifies
  ;;    wins -- so inheriting `default' (which carries a `:foreground')
  ;;    masked every font-lock colour and killed syntax highlighting.
  ;;
  ;; 2. `default' is remapped to `emjupy-canvas' inside notebook buffers, so
  ;;    `:inherit default' resolves THROUGH that remap and paints cells with
  ;;    the canvas colour -- exactly inverting the intended look.
  '((t))
  "Background INSIDE a cell outline -- the `paper' the cell sits on.
Only `:background' is ever set on this face; see the comment above."
  :group 'emjupy)

(defface emjupy-canvas
  '((t))
  "Background OUTSIDE cell outlines -- the page behind the cells.
Only `:background' is ever set on this face."
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

(defun emjupy--sync-theme-colors ()
  "Recompute `emjupy-cell' and `emjupy-canvas' from the active theme.

Returns non-nil when both colours could be derived AND differ, i.e. when
painting a canvas is actually meaningful. Callers use that to decide
whether to install the remap at all: remapping without a usable cell
colour would leave cells inheriting the canvas and invert the look."
  (let* ((bg (face-attribute 'default :background nil t))
         (fg (face-attribute 'default :foreground nil t))
         (canvas (cond
                  ((stringp emjupy-canvas-color) emjupy-canvas-color)
                  ((null emjupy-canvas-color) bg)
                  (t (or (emjupy--blend-colors bg fg emjupy-canvas-blend) bg)))))
    (when (emjupy--color-rgb bg)
      (set-face-attribute 'emjupy-cell nil :background bg))
    (when (emjupy--color-rgb canvas)
      (set-face-attribute 'emjupy-canvas nil :background canvas))
    (and (emjupy--color-rgb bg)
         (emjupy--color-rgb canvas)
         (not (equal bg canvas)))))

(defun emjupy--apply-page-colors ()
  "Paint this buffer's background with the canvas colour, if there is one.

Installs the remap only when `emjupy--sync-theme-colors' reports two
usable, distinct colours. Otherwise the buffer is left entirely alone --
better a flat notebook than one where every cell is painted the canvas
colour because no cell colour could be derived."
  (when (emjupy--sync-theme-colors)
    (unless (assq 'default face-remapping-alist)
      (setq-local face-remapping-alist
                  (cons '(default emjupy-canvas) face-remapping-alist)))))

(defun emjupy--on-theme-change (&rest _)
  "Re-derive emjupy's page colours after a theme is enabled or disabled."
  (emjupy--sync-theme-colors))

(when (boundp 'enable-theme-functions)
  (add-hook 'enable-theme-functions #'emjupy--on-theme-change))
(when (boundp 'disable-theme-functions)
  (add-hook 'disable-theme-functions #'emjupy--on-theme-change))

(defun emjupy-refresh-appearance ()
  "Re-derive page colours from the theme and redraw open notebooks.
Useful after changing `emjupy-canvas-color' or loading a theme in an
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

(defun emjupy--box-width ()
  "Return the column width to draw cell outlines at.

When fitting the window, the NARROWEST window showing this buffer wins:
a rule sized to a wide window wraps onto a second line in a narrow one,
and a wrapped rule is far uglier than a short one."
  (if (integerp emjupy-box-width)
      emjupy-box-width
    (let ((widths (mapcar #'window-body-width
                          (get-buffer-window-list (current-buffer) nil t))))
      (max emjupy-box-min-width
           ;; Leave a column so the rule can't wrap onto a second line.
           (1- (if widths (apply #'min widths) 100))))))

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
              'face '(emjupy-box-line emjupy-cell)))

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
                      (overlay-put out 'after-string (emjupy--rule nil))))))))

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
      (insert to-insert))
    (unless (string-suffix-p "\n" source) (insert "\n"))

    (put-text-property src-start (point) 'emjupy-cell cell)

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
      ;; Only `:background' is set, so font-lock's foreground colours on the
      ;; text underneath still show through.
      (overlay-put ov 'face 'emjupy-cell)
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Output Box Overlay
    (when has-outputs
      (let ((out-start (point)))
        (cl-loop for out in (emjupy--outputs-for-render outputs)
                 do (let ((out-type (gethash "output_type" out)))
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
                                                "\n"))))))))

        (unless (string-suffix-p "\n" (buffer-substring-no-properties (max (point-min) (- (point) 1)) (point)))
          (insert "\n"))

        (let* ((ov (make-overlay out-start (point)))
               ;; "├" instead of "┌": this line IS the input box's bottom
               ;; edge, continuing straight into the output box's top edge.
               (header (emjupy--rule (emjupy--cell-out-label cell) "├"))
               (footer (emjupy--rule nil)))
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          (overlay-put ov 'face 'emjupy-cell)
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
