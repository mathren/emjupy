;;; emjupy-render.el --- Drawing a notebook: cells, outputs, outlines  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Drawing a notebook: the box-drawing outlines (which track the window
;; width), syntax highlighting for code and markdown cells, and the
;; overlays of cells and their outputs.  The faces and the theme-derived
;; colours are in emjupy-faces.el; redrawing is in emjupy-redraw.el.

;;; Code:

(require 'cl-lib)
(require 'color)
(require 'subr-x)
(require 'ansi-color)
(require 'emjupy-core)
(require 'emjupy-faces)
(require 'emjupy-terminal)
(require 'emjupy-latex)

(defvar python-indent-guess-indent-offset-verbose)

;;;; Options

(defcustom emjupy-protect-non-cell-regions t
  "When non-nil, make everything outside a cell's source read-only.

The rules, the gutters between cells and the output boxes belong to no
cell.  Text typed there is stored nowhere and disappears at the next
redraw, so it is better refused than silently lost."
  :type 'boolean
  :group 'emjupy-editing
  :safe #'booleanp)

;;;; Width

(defun emjupy--line-number-columns (win)
  "Return how many columns WIN's line numbers take, or 0 without them.
Read from the window's buffer rather than by selecting it: Emacs reports
the column's width for the selected window only, so for any other it is
worked out from the last line's number."
  (with-current-buffer (window-buffer win)
    (if (bound-and-true-p display-line-numbers)
        ;; Emacs's figure or the line-number column's, whichever is more:
        ;; Emacs reports 0 until the window has been shown, and leaves out
        ;; the gap.
        (max (if (eq win (selected-window))
                 (line-number-display-width)
               0)
             (+ 2 (max (length (number-to-string
                                (line-number-at-pos (point-max) t)))
                       (or (bound-and-true-p display-line-numbers-width)
                           0))))
      0)))

(defun emjupy--window-text-width (win)
  "Return how many columns of text WIN can show without wrapping.
Not `window-body-width', which includes the line-number column."
  (if (not (window-live-p win))
      ;; A window can die between a resize being scheduled and this
      ;; running; a dead one has no width to give, so the default stands.
      emjupy-box-min-width
    ;; Measured keeping point: `window-max-chars-per-line' selects WIN, which
    ;; moves point to the window's.
    (save-excursion
      (max 1 (- (window-max-chars-per-line win)
                (emjupy--line-number-columns win))))))

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

;;;; Labels and rules

;; The frame the running indicator, below, has reached: the label draws it.
(defvar-local emjupy--spinner-frame 0
  "Index into `emjupy-running-indicator'.")

(defun emjupy--cell-label (cell)
  "Return the header label for CELL's input box."
  (let ((exec (emjupy-cell-exec-count cell)))
    (format "[In: %s] %s"
            (cond
             ;; Jupyter's convention: a running cell shows a marker where its
             ;; execution count will go.
             ((emjupy--cell-executing-p cell)
              (aref emjupy-running-indicator
                    (mod emjupy--spinner-frame
                         (length emjupy-running-indicator))))
             ;; queued behind it, as Jupyter shows them
             ((emjupy--cell-running-p cell) "*")
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
    (push o (overlay-get box 'emjupy-companions))
    o))

(defvar emjupy--rules-batch nil
  "Non-nil inside `emjupy--with-rules-once': rebuilding the rules waits.")

(defvar emjupy--rules-stale nil
  "Non-nil when a rebuild of the rules was asked for inside a batch.")

(defmacro emjupy--with-rules-once (&rest body)
  "Run BODY, rebuilding the rules once at its end, however often asked.
Each rebuild walks every box of the notebook: a redraw of several cells,
or of a cell's header, footer and output, would otherwise walk it each
time.  Nested, only the outermost rebuilds."
  (declare (indent 0) (debug t))
  `(if emjupy--rules-batch
       (progn ,@body)
     (let ((emjupy--rules-batch t) (emjupy--rules-stale nil))
       (unwind-protect (progn ,@body)
         (when emjupy--rules-stale
           (emjupy--reconcile-rules-now))))))

(defun emjupy--reconcile-rules ()
  "Rebuild every rule now -- or, inside `emjupy--with-rules-once', at its end."
  (if emjupy--rules-batch
      (setq emjupy--rules-stale t)
    (emjupy--reconcile-rules-now)))

(defun emjupy--reconcile-rules-now ()
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
      (dolist (ov boxes)
        (overlay-put ov 'emjupy-companions nil)
        (overlay-put ov 'emjupy-header-carrier nil))
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
               ;; Between two boxes: the rules hang on the separator as
               ;; continuation lines, never numbered, and the box's last newline
               ;; is hidden -- by a `display', as Emacs draws no overlay string
               ;; at a hidden position.
               ((and gap-end (< (overlay-end prev) gap-end))
                (let ((o (emjupy--hang-rules prev anchor rules 'display)))
                  (when ov (overlay-put ov 'emjupy-header-carrier o))))
               ;; Followed at once by another box -- a cell by its output --
               ;; or the last box: the rules hang on its own last line.
               ((not (string-empty-p rules))
                (let ((o (emjupy--hang-rules prev anchor rules
                                             'before-string)))
                  (when ov (overlay-put ov 'emjupy-header-carrier o)))))))
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

(defun emjupy--footer-rule ()
  "Return the plain rule closing a cell's box."
  (emjupy--rule nil))

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

;;;; The running indicator

(defvar-local emjupy--running-cells nil
  "Ids of cells currently executing in this buffer.")

(defvar-local emjupy--spinner-timer nil
  "Timer animating the running indicator, or nil.")

(defun emjupy--cell-executing-p (cell)
  "Return non-nil if CELL is the one its kernel is executing now.
The kernel runs cells in the order sent: of those running, the first
sent -- the last of `emjupy--running-cells', newest first."
  (and cell (eql (emjupy-cell-id cell) (car (last emjupy--running-cells)))))

(defun emjupy--cell-running-p (cell)
  "Return non-nil if CELL is currently executing."
  (and cell (memq (emjupy-cell-id cell) emjupy--running-cells)))

(defun emjupy--spinner-tick ()
  "Advance the indicator and redraw the header of the executing cell.
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
          ;; only the executing cell moves; those queued show a still *
          (when emjupy--buffer-notebook
            (let ((cell (cl-find (car (last emjupy--running-cells))
                                 (emjupy-notebook-cells emjupy--buffer-notebook)
                                 :key #'emjupy-cell-id)))
              (when cell
                (emjupy--with-rules-once
                  (emjupy--redraw-cell-header cell))))))))))

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

(defun emjupy--rehang-header (ov rule)
  "Show RULE as OV's header in the rules it hangs with, if they are known.
Only that one carrier's text changes: no rebuild of every rule.  Return
non-nil if done; nil when OV has no carrier -- the first box, or rules
not built yet."
  (let* ((carrier (overlay-get ov 'emjupy-header-carrier))
         (prev (and (overlayp carrier) (overlay-buffer carrier)
                    (overlay-get carrier 'emjupy-rules-of))))
    (when prev
      (let* ((footer (overlay-get prev 'emjupy-footer))
             (rules (mapconcat (lambda (r) (string-remove-suffix "\n" r))
                               (seq-filter (lambda (r)
                                             (and r (not (string-empty-p r))))
                                           (list footer rule))
                               "\n"))
             (text (concat "\n" rules))
             (property (if (overlay-get carrier 'display)
                           'display
                         'before-string)))
        (put-text-property 0 1 'cursor t text)
        (overlay-put ov 'emjupy-header rule)
        (overlay-put carrier property text)
        t))))

(defun emjupy--redraw-cell-header (cell)
  "Redraw CELL's header rule in place.
The header is an overlay string, so this changes no buffer text and does
not enter the undo history.  Where the rules carrying it are known, only
they change; else every rule is rebuilt."
  (let ((ov (emjupy-cell-overlay cell)))
    (when (overlayp ov)
      (let ((rule (emjupy--rule (emjupy--cell-label cell) "┌")))
        (unless (emjupy--rehang-header ov rule)
          (overlay-put ov 'emjupy-header nil)
          (overlay-put ov 'before-string rule)
          (emjupy--reconcile-rules)))
      (emjupy--overlay-header ov))))

(defun emjupy--mark-running (cell &optional buffer)
  "Mark CELL as executing in BUFFER, and start the indicator."
  (with-current-buffer (or buffer (current-buffer))
    (cl-pushnew (emjupy-cell-id cell) emjupy--running-cells)
    ;; Draw it at once rather than on the next tick, so pressing the key
    ;; visibly does something even for a cell that finishes quickly.
    (emjupy--redraw-cell-header cell)
    (emjupy--start-spinner)))

(defun emjupy--mark-done (cell &optional buffer)
  "Mark CELL as no longer executing in BUFFER."
  (with-current-buffer (or buffer (current-buffer))
    (setq emjupy--running-cells
          (delq (emjupy-cell-id cell) emjupy--running-cells))
    (emjupy--redraw-cell-header cell)
    (unless emjupy--running-cells (emjupy--stop-spinner))))

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
    (define-key map [mouse-1] 'emjupy-toggle-output-at-click)
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

;;;; MIME types

(defconst emjupy--widget-view-mime "application/vnd.jupyter.widget-view+json"
  "MIME type of an output that shows a widget.")

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

;;;; Images, once each

(defun emjupy--output-image-key (out)
  "Return a key identifying OUT's image payload, or nil if it carries none.
The payload is hashed rather than compared directly: a figure is
hundreds of kilobytes of base64, and this runs on every redraw."
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

(defun emjupy--outputs-to-draw (outputs)
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

;;;; Rich output

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

(defvar emjupy-output-button-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'emjupy-open-output-at-point)
    (define-key map [mouse-1] 'emjupy-open-output-at-point)
    map)
  "Keymap on the line that stands for an interactive output.")

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

;;;; Colour and tracebacks

(defun emjupy--render-ansi (text)
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
      (insert (emjupy--render-ansi
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
                   do (insert (emjupy--render-ansi (emjupy--mime-text line))
                              "\n"))))))))

(defun emjupy--make-output-overlay (cell start widest pieces)
  "Give CELL's output box, from START to point, its overlay.
WIDEST is its widest line before folding, PIECES the outputs drawn."
  (let* ((ov (make-overlay start (point)))
         ;; "├" instead of "┌": this line IS the input box's bottom
         ;; edge, continuing straight into the output box's top edge.
         (header (emjupy--rule (emjupy--cell-out-label cell) "├"))
         (footer (emjupy--footer-rule)))
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

(defun emjupy--draw-cell-output (cell)
  "Insert CELL's output box at point and give it its overlay.
Apart from `emjupy--draw-cell' so output arriving redraws one box,
not the notebook."
  (let ((outputs (emjupy-cell-outputs cell)))
    (when (and outputs (> (length outputs) 0))
      (let ((out-start (point))
            ;; widest line as the kernel sent it, before folding
            (widest 0)
            (pieces (emjupy--outputs-to-draw outputs)))
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

;;;; Drawing a cell

(defun emjupy--redraw-cell-footer (cell)
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
        (t (emjupy--footer-rule)))))))

(defun emjupy--draw-cell (cell)
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
                    (t (emjupy--footer-rule)))))
      (overlay-put ov 'emjupy-overlay 'cell)
      (overlay-put ov 'emjupy-header nil)
      (overlay-put ov 'before-string header)
      (overlay-put ov 'after-string footer)
      ;; No face: source cells keep the buffer's normal background, and are
      ;; marked out by their rules alone.
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Output Box Overlay
    (when (and has-outputs (not (emjupy--cell-outputs-hidden-p cell)))
      (emjupy--draw-cell-output cell))

    (insert "\n")))

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

;;;; Text leaving and entering the buffer

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

;;;; In a notebook buffer

(defun emjupy-render-enable ()
  "Draw the current notebook in the theme's colours, and refontify edits.
Keep emjupy's own text properties out of the kill ring, both ways."
  (emjupy--sync-theme-colors)
  (add-hook 'after-change-functions #'emjupy--refontify-after-change nil t)
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
                        nil))))

(provide 'emjupy-render)
;;; emjupy-render.el ends here
