;;; emjupy-render-test.el --- Unit tests: emjupy-render.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-render.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-markdown-highlighted-without-markdown-mode ()
  "Markdown cells must be highlighted even when no markdown package is
installed. `markdown-mode' is a MELPA package emjupy does not depend
on, so relying on it alone left markdown cells as flat, undifferentiated
plain text for most users."
  (cl-letf (((symbol-function 'emjupy--markdown-mode-fn) (lambda () nil)))
    (let* ((src "# A heading\n\nSome **bold** and *italic* and `code`.\n\n- a bullet\n> a quote\n\n[label](http://example.org)")
           (out (emjupy--fontify-as src 'markdown))
           (face-at (lambda (needle &optional off)
                      (let ((i (+ (string-match (regexp-quote needle) out)
                                  (or off 0))))
                        (get-text-property i 'face out)))))
      (cl-flet ((has (faces f) (if (listp faces) (memq f faces) (eq faces f))))
        ;; heading
        (should (has (funcall face-at "# A heading")
                     'font-lock-function-name-face))
        ;; emphasis
        (should (has (funcall face-at "**bold**") 'bold))
        (should (has (funcall face-at "*italic*" 1) 'italic))
        ;; inline code
        (should (has (funcall face-at "`code`") 'font-lock-constant-face))
        ;; list bullet
        (should (has (funcall face-at "- a bullet") 'font-lock-keyword-face))
        ;; blockquote
        (should (has (funcall face-at "> a quote") 'font-lock-comment-face))
        ;; link label
        (should (has (funcall face-at "label") 'link))))))

(ert-deftest emjupy-test-markdown-fallback-not-used-when-mode-available ()
  "When a real markdown mode IS installed, defer to it rather than
layering emjupy's approximation on top."
  (let ((called nil))
    (cl-letf (((symbol-function 'emjupy--markdown-mode-fn) (lambda ()
                                                             'text-mode))
              ((symbol-function 'emjupy--markdown-fontify-fallback)
               (lambda () (setq called t))))
      (emjupy--fontify-as "# hi" 'markdown)
      (should-not called))))

(ert-deftest emjupy-test-code-cells-still-use-python ()
  "The markdown work must not disturb python highlighting."
  (let ((out (emjupy--fontify-as "def f():\n    return 1" 'code)))
    (should (text-property-not-all 0 (length out) 'face nil out))))

;;;; Output background band

(ert-deftest emjupy-test-refontifying-leaves-text-and-undo-alone ()
  "Re-highlighting rewrites face properties only: the buffer text is
untouched and nothing lands in the undo history."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((before (buffer-substring-no-properties (point-min) (point-max))))
          (setq buffer-undo-list nil)
          (emjupy--refontify-cell cell)
          (should (equal before (buffer-substring-no-properties (point-min)
                                                                (point-max))))
          (should (null (delq nil buffer-undo-list))))))))

;;;; Output-box rendering: visibility, merged divider, 80-col borders

(ert-deftest emjupy-test-no-output-box-when-no-output ()
  "A cell with no output (e.g. a bare import) gets no output box at all,
and its own bottom border is never hidden."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import numpy as np"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should-not (emjupy-cell-output-ov cell))
      (let ((footer (emjupy--overlay-footer (emjupy-cell-overlay cell))))
        (should (and footer (string-match-p "└" footer)))))))

(ert-deftest emjupy-test-output-box-merges-with-input-border ()
  "A cell WITH output gets ONE merged divider line (T-junction), not a
closing border immediately followed by a separate opening one."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "print(1)"
                                 :outputs [] :metadata (make-hash-table)))
         (oh (make-hash-table :test 'equal))
         (dat (make-hash-table :test 'equal)))
    (puthash "text/plain" "1" dat)
    (puthash "output_type" "execute_result" oh) (puthash "data" dat oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should (emjupy-cell-output-ov cell))
      (should (string= (emjupy--overlay-footer (emjupy-cell-overlay cell)) ""))
      (should (string-prefix-p
               "├" (emjupy--overlay-header (emjupy-cell-output-ov cell)))))))

(ert-deftest emjupy-test-box-borders-match-configured-width ()
  "Box-drawing borders (excluding their trailing newline) are pinned to
`emjupy-box-width', regardless of label length, and never crash on an
overlong label."
  (let ((emjupy-box-width 80))
    (should (= (1- (length (emjupy--box-header "[In: 1] python"))) 80))
    (should (= (1- (length (emjupy--box-footer))) 80))
    (should (string-suffix-p "\n" (emjupy--box-header (make-string 500 ?x)))))
  ;; header and footer always agree, at any configured width
  (dolist (w '(60 100 140))
    (let ((emjupy-box-width w))
      (should (= (length (emjupy--box-header "[In: 12] python"))
                 (length (emjupy--box-footer))))
      (should (= (1- (length (emjupy--box-footer))) w)))))

;;;; Rich output: display_data / image routing

(ert-deftest emjupy-test-svg-is-not-base64-decoded ()
  "image/svg+xml arrives as literal markup, not base64 -- decoding it
would corrupt it (or signal)."
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type
                                                         :data data))))
    (let ((img (emjupy--render-image-output "<svg width='1'></svg>" 'svg)))
      (should (equal (plist-get (cdr img) :data) "<svg width='1'></svg>")))))

(ert-deftest emjupy-test-mime-text-accepts-string-or-array ()
  "nbformat stores multi-line MIME payloads as a string OR an array of
lines depending on whether the JSON came from the Contents API or
straight off disk; both must render."
  (should (equal (emjupy--mime-text "a\nb") "a\nb"))
  (should (equal (emjupy--mime-text ["a\n" "b"]) "a\nb"))
  (should (equal (emjupy--mime-text nil) "")))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-window-width-excludes-line-numbers ()
  "The line-number column is drawn inside the text area, so
`window-body-width' counts it -- and a rule sized from that overshoots
the right edge by the width of the numbers."
  (let ((win (selected-window)))
    (with-current-buffer (window-buffer win)
      (cl-letf (((symbol-function 'window-max-chars-per-line)
                 (lambda (&rest _) 200)))
        (setq-local display-line-numbers nil)
        (should (= (emjupy--window-text-width win) 200))
        (setq-local display-line-numbers t)
        (cl-letf (((symbol-function 'line-number-display-width)
                   (lambda (&rest _) 6)))
          (should (= (emjupy--window-text-width win) 194)))
        (setq-local display-line-numbers nil)))))

(ert-deftest emjupy-test-running-indicator ()
  "A running cell shows a marker where its execution count will go, and
only that cell does."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "slow()"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (should (string-match-p "In:  " (emjupy--cell-label c1)))
        (emjupy--mark-running c1)
        (should (emjupy--cell-running-p c1))
        (should-not (emjupy--cell-running-p c2))
        ;; the marker is in the label and in the drawn header
        (should (string-match-p "In: |" (emjupy--cell-label c1)))
        (should (string-match-p "In: |"
                                (emjupy--overlay-header (emjupy-cell-overlay c1))))
        (should (string-match-p "In:  " (emjupy--cell-label c2)))
        ;; the animation advances
        (setq emjupy--spinner-frame 1)
        (should (string-match-p "In: /" (emjupy--cell-label c1)))
        ;; and finishing replaces it with the execution count
        (setf (emjupy-cell-exec-count c1) 7)
        (emjupy--mark-done c1)
        (should-not (emjupy--cell-running-p c1))
        (should (string-match-p "In: 7" (emjupy--cell-label c1)))
        (should-not emjupy--spinner-timer)))))

(ert-deftest emjupy-test-running-indicator-touches-no-buffer-text ()
  "The indicator animates through overlay strings only.  Were it buffer
text, every frame would land in the undo history and fight whatever is
being typed."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "slow()"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (emjupy--mark-running cell)
        (let ((before (buffer-string)))
          (dotimes (_ 4) (emjupy--spinner-tick))
          (should (equal (buffer-string) before)))))))

(ert-deftest emjupy-test-python-indent-warning-silenced ()
  "python-mode says \"Can't guess python-indent-offset\" when it cannot work
one out.  A cell is a fragment, so it usually cannot -- and this runs
once per code cell, filling the echo area on every redraw."
  (let ((seen nil))
    (cl-letf (((symbol-function 'message)
               (lambda (f &rest args)
                 (when f (push (apply #'format f args) seen))
                 nil)))
      (emjupy--fontify-as "if True:\n  x = 1" 'code))
    (should-not (cl-some (lambda (m)
                           (string-match-p "python-indent-offset" m)) seen))))

(ert-deftest emjupy-test-render-is-not-re-entrant ()
  "A render that re-enters must be dropped, not run.

Rendering erases the buffer and rebuilds it.  Output arriving over the
WebSocket midway restarted the rebuild on a half-built buffer and left
the notebook duplicated on top of itself -- and the next sync copied
that into the first cell's source, so redrawing kept it."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((fired nil)
              (target buf))
          (advice-add 'emjupy--fontify-as :before
                      (lambda (&rest _)
                        (unless fired
                          (setq fired t)
                          (with-current-buffer target
                            (emjupy--redraw-notebook)))))
          (unwind-protect
              (emjupy--redraw-notebook)
            (advice-mapc (lambda (f _p) (advice-remove 'emjupy--fontify-as f))
                         'emjupy--fontify-as)))
        ;; one copy of each cell, not two
        (should (= (cl-count "a = 1" (split-string (buffer-string) "\n")
                             :test #'equal) 1))
        (emjupy--sync-all-cells)
        (should (equal (emjupy-cell-source c1) "a = 1"))
        (should (equal (emjupy-cell-source c2) "b = 2"))))))

;;;; Performance

(ert-deftest emjupy-test-toggle-cell-output ()
  "Collapsing output hides it without discarding it.

The outputs stay on the cell, so showing them again is a redraw and
never a re-run.  The bottom rule says the output is there but folded
away, which is the difference between hidden and absent."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)"
               (vector (emjupy-test--stream-output "line one\nline two\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should (string-match-p "line two" (buffer-string)))
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-toggle-cell-output)
        ;; gone from view, still on the cell
        (should-not (string-match-p "line two" (buffer-string)))
        (should (= (length (emjupy-cell-outputs cell)) 1))
        (should (emjupy--cell-outputs-hidden-p cell))
        ;; the rule says so, and the cell still has a bottom
        (let ((footer (emjupy--overlay-footer (emjupy-cell-overlay cell))))
          (should (string-match-p emjupy-hidden-output-glyph footer))
          (should (string-match-p "hidden" footer))
          (should (string-prefix-p "└" footer)))
        (should-not (emjupy--check-invariants))
        ;; and back again, from the outputs that were never thrown away
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-toggle-cell-output)
        (should (string-match-p "line two" (buffer-string)))
        (should-not (emjupy--cell-outputs-hidden-p cell))
        (should-not (emjupy--check-invariants))
        ;; nothing left behind in the metadata to be written out
        (should-not (gethash "jupyter" (emjupy-cell-metadata cell)))))))

(ert-deftest emjupy-test-hidden-state-is-where-jupyter-keeps-it ()
  "The collapsed flag lives at metadata.jupyter.outputs_hidden.

That is where Jupyter keeps it, so the state survives saving and means
the same thing to other front ends rather than being private to emjupy."
  (let ((cell (emjupy-test--cell-like
               'code "x" (vector (emjupy-test--stream-output "out\n")))))
    (emjupy--set-cell-outputs-hidden cell t)
    (should (eq (gethash "outputs_hidden"
                         (gethash "jupyter" (emjupy-cell-metadata cell)))
                t))
    (should (emjupy--cell-outputs-hidden-p cell))
    (emjupy--set-cell-outputs-hidden cell nil)
    (should-not (emjupy--cell-outputs-hidden-p cell))))

(ert-deftest emjupy-test-clearing-output-keeps-the-bottom-rule ()
  "A cell keeps a bottom edge after its output is cleared.

The bottom rule belongs to whichever box is last.  With output that is
the output box's own footer and the cell has none -- but when the output
went, so did the only bottom edge, leaving the cell open."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)" (vector (emjupy-test--stream-output "1\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; with output, the output box carries the bottom
        (should (equal (emjupy--overlay-footer (emjupy-cell-overlay cell)) ""))
        (should (string-prefix-p
                 "└" (emjupy--overlay-footer (emjupy-cell-output-ov cell))))
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        ;; with it gone, the cell carries it again
        (should-not (emjupy-cell-output-ov cell))
        (should (string-prefix-p
                 "└" (emjupy--overlay-footer (emjupy-cell-overlay cell))))
        (should-not (emjupy--check-invariants))))))

(ert-deftest emjupy-test-collapsed-marker-is-clickable ()
  "Clicking the marker on a collapsed box restores that cell's output.

The marker carries the cell's id, so the click acts on the cell the
marker belongs to rather than on whatever is near point.  A buffer
position would not serve: the footer sits at the very end of a cell,
where it is ambiguous which of two neighbours is meant."
  (let ((c1 (emjupy-test--cell-like
             'code "print(1)" (vector (emjupy-test--stream-output "first\n"))))
        (c2 (emjupy-test--cell-like
             'code "print(2)"
             (vector (emjupy-test--stream-output "second\n")))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (emjupy-toggle-cell-output)
        (should-not (string-match-p "first" (buffer-string)))
        (let* ((footer (emjupy--overlay-footer (emjupy-cell-overlay c1)))
               (idx (text-property-not-all 0 (length footer)
                                           'emjupy-toggle-cell nil footer)))
          ;; only the marker is live; the rest of the rule is an ordinary line
          (should idx)
          (should (equal (get-text-property idx 'emjupy-toggle-cell footer)
                         (emjupy-cell-id c1)))
          (should (get-text-property idx 'keymap footer))
          (should (eq (get-text-property idx 'mouse-face footer) 'highlight))
          (should-not (get-text-property 0 'emjupy-toggle-cell footer))
          ;; the click itself
          (emjupy--toggle-output-from-string (cons footer idx) buf)
          (should (string-match-p "first" (buffer-string)))
          (should-not (emjupy--cell-outputs-hidden-p c1))
          ;; the neighbour is untouched
          (should-not (emjupy--cell-outputs-hidden-p c2))
          (should (string-match-p "second" (buffer-string)))
          (should-not (emjupy--check-invariants)))))))

(ert-deftest emjupy-test-toggle-works-from-the-output-area ()
  "The toggle acts on the cell whose output point is in.

A position inside an output box lies between two cells as far as the
source lookup is concerned.  Commands about the output itself want the
cell above -- the one whose output that is -- not the one below, whose
own output is usually empty and which therefore reported nothing to
hide."
  (let ((c1 (emjupy-test--cell-like
             'code "print(1)"
             (vector (emjupy-test--stream-output "alpha\nbeta\n"))))
        (c2 (emjupy-test--cell-like 'code "y = 2")))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((out (emjupy-cell-output-ov c1)))
          (goto-char (/ (+ (overlay-start out) (overlay-end out)) 2))
          ;; the cell owning this position, output included, is the one above
          (should (eq (emjupy--cell-at-point-including-output) c1))
          (emjupy-toggle-cell-output)
          (should (emjupy--cell-outputs-hidden-p c1))
          (should-not (emjupy--cell-outputs-hidden-p c2))
          (should-not (emjupy--check-invariants)))))))

(ert-deftest emjupy-test-refontifying-leaves-the-output-box-alone ()
  "Re-highlighting a cell cannot disturb anything in an output box.

Highlighting is re-applied to a cell's SOURCE after an edit: the old
faces are cleared over its region and new ones copied in from a
fontified copy of the source.  Where that region reaches into an output
box it clears what is there and puts nothing back, because the source it
copies from contains no output.

Both the padding and the text beside it are checked.  Protecting only
the padding brought the band back in the margins and left it missing
behind the output itself, which looked like a different bug and was not."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)"
               (vector (emjupy-test--stream-output "alpha\nbeta\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-flet ((kept ()
                    ;; characters in the box still showing an output face,
                    ;; counted separately for text and padding
                    (let ((ov (emjupy-cell-output-ov cell)) (text 0) (pad 0))
                      (save-excursion
                        (goto-char (overlay-start ov))
                        (while (< (point) (overlay-end ov))
                          (unless (eq (char-after) ?\n)
                            (when (string-match-p
                                   "emjupy-output"
                                   (format "%S" (get-text-property
                                                 (point) 'font-lock-face)))
                              (if (get-text-property (point) 'emjupy-pad)
                                  (setq pad (1+ pad))
                                (setq text (1+ text)))))
                          (forward-char 1)))
                      (cons text pad))))
          (let ((before (kept)))
            (should (> (car before) 0))
            (should (> (cdr before) 0))
            ;; a re-highlight whose region covers the whole buffer, output
            ;; and all -- the worst case of a region being wrong
            (let ((inhibit-read-only t))
              (emjupy--apply-faces-from
               (propertize (make-string (- (point-max) (point-min)) ?x)
                           'face 'font-lock-keyword-face)
               (point-min)))
            (should (equal (kept) before))))))))

(ert-deftest emjupy-test-finishing-a-cell-keeps-undo ()
  "Updating a cell's execution count does not throw away the history.

The count lives in the cell's header, which can be redrawn on its own.
Rebuilding the notebook for it meant undo died the moment a cell
finished -- after the user had already gone back to typing."
  (let ((cell (emjupy-test--cell-like 'code "print(1)")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (end-of-line)
        (insert "  # note")
        (undo-boundary)
        (let ((before (length buffer-undo-list)))
          (setf (emjupy-cell-exec-count cell) 7)
          (should (emjupy--redraw-cell-header cell))
          ;; the header now says so, and nothing was lost
          (should (>= (length buffer-undo-list) before))
          (should (string-match-p "# note" (buffer-string))))))))

;;;; The undo guard

(ert-deftest emjupy-test-different-pictures-are-both-drawn ()
  "Two figures whose bytes differ are two pictures, and both are drawn.

A kernel can send the same figure twice with different bytes -- a
different DPI or format setting for one of the two renders -- and
nothing in the payload says they are the same.  Deduplication is by
content, so those are kept, as two distinct figures must be."
  (cl-flet ((png (b64) (let ((o (make-hash-table :test 'equal))
                             (d (make-hash-table :test 'equal)))
                         (puthash "output_type" "display_data" o)
                         (puthash "image/png" b64 d)
                         (puthash "data" d o)
                         o)))
    (let ((emjupy-deduplicate-image-outputs t))
      ;; different bytes: both drawn
      (should (= 2 (length (emjupy--outputs-to-draw
                            (vector (png "iVBORw0KGgoAAA=")
                                    (png "iVBORw0KGgoBBB="))))))
      ;; the same bytes: one
      (should (= 1 (length (emjupy--outputs-to-draw
                            (vector (png "iVBORw0KGgoAAA=")
                                    (png "iVBORw0KGgoAAA=")))))))))

(ert-deftest emjupy-test-wide-output-stays-inside-the-box ()
  "No line of any output is drawn wider than the cell.

Nothing in the output decides how wide it is drawn: a progress bar sized
to a wide terminal ran far past the right-hand edge, and so would a long
log line or a wide table.  Breaking lines where the box ends is done for
every output type, not for progress bars."
  (dolist (type '("stream" "execute_result" "error"))
    (let* ((long (concat (make-string 190 ?\u2588) " and some text after"))
           (out (let ((o (make-hash-table :test 'equal)))
                  (puthash "output_type" type o)
                  (pcase type
                    ("stream" (puthash "name" "stderr" o)
                     (puthash "text" (concat long "\n") o))
                    ("execute_result"
                     (let ((d (make-hash-table :test 'equal)))
                       (puthash "text/plain" long d) (puthash "data" d o)))
                    ("error" (puthash "ename" "E" o) (puthash "evalue" "v" o)
                     (puthash "traceback" (vector long) o)))
                  o))
           (cell (emjupy-test--cell-like 'code "x" (vector out))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((width (emjupy--box-width))
                (widths (emjupy-test--drawn-widths cell)))
            (should (<= (apply #'max widths) width))
            ;; broken, not cut: every character is still drawn
            (should (> (length widths) 2))))))))

(ert-deftest emjupy-test-folding-never-splits-a-wide-character ()
  "A double-width character at the edge moves to the next line whole."
  (with-temp-buffer
    (insert (concat (make-string 9 ?a) "\u4e2d\u4e2d\u4e2d"))  ; 9 + 3x2 columns
    (emjupy--fold-output-lines (point-min) (point-max) 10)
    (dolist (line (split-string (buffer-string) "\n"))
      (should (<= (string-width line) 10)))
    ;; nothing lost
    (should (equal (replace-regexp-in-string "\n" "" (buffer-string))
                   (concat (make-string 9 ?a) "\u4e2d\u4e2d\u4e2d")))))

(ert-deftest emjupy-test-widgets-are-drawn-before-their-output ()
  "A widget is drawn above the cell's other outputs; the saved order stays."
  (let* ((img (let ((o (make-hash-table :test 'equal))
                    (d (make-hash-table :test 'equal)))
                (puthash "output_type" "display_data" o) (puthash "text/plain"
                                                                  "<Figure>" d)
                (puthash "data" d o) o))
         (view (let ((o (make-hash-table :test 'equal))
                     (d (make-hash-table :test 'equal))
                     (v (make-hash-table :test 'equal)))
                 (puthash "model_id" "m" v)
                 (puthash "output_type" "display_data" o)
                 (puthash emjupy--widget-view-mime v d) (puthash "data" d o) o))
         (outputs (vector img view)))
    (should (equal (emjupy--outputs-to-draw outputs) (list view img)))
    ;; the cell's own order is untouched
    (should (eq (aref outputs 0) img))))

(ert-deftest emjupy-test-key-hint-does-not-depend-on-the-current-buffer ()
  "The line opening an output names its key, whatever buffer is current.
Output can be drawn while another buffer is current, where the key is
not bound, and the line said \"M-x emjupy-open-output\"."
  (with-temp-buffer
    (emjupy--insert-open-line "Interactive plotly figure" 1000
                              (make-hash-table))
    (should (string-match-p "C-c C-f" (buffer-string)))))


;;;; Large notebooks: rules rebuilt once, headers in place

(defun emjupy-test--rules-picture ()
  "Every rule string shown in this buffer, with where and how."
  (sort (delq nil
              (mapcar (lambda (o)
                        (let ((text (or (overlay-get o 'display)
                                        (overlay-get o 'before-string)
                                        (overlay-get o 'after-string))))
                          (when (stringp text)
                            (list (overlay-start o)
                                  (substring-no-properties text)))))
                      (overlays-in (point-min) (point-max))))
        (lambda (a b) (< (car a) (car b)))))

(defun emjupy-test--cells (n)
  "N code cells, each with an output."
  (vconcat (cl-loop for i below n collect
                    (make-emjupy-cell
                     :id (1+ i) :type 'code :source (format "a = %d" i)
                     :outputs (vector (let ((o (make-hash-table :test 'equal)))
                                        (puthash "output_type" "stream" o)
                                        (puthash "name" "stdout" o)
                                        (puthash "text" (format "%d\n" i) o)
                                        o))
                     :metadata (make-hash-table :test 'equal)))))

(ert-deftest emjupy-test-a-header-changed-in-place-is-a-rebuilds-header ()
  "A header changed in place shows exactly what a rebuild of every rule would.
Changing one carrier's text, not walking every box, is what keeps a
running indicator cheap in a large notebook."
  (emjupy-test--with-notebook (emjupy-test--cells 12) buf nb
    (let ((cell (aref (emjupy-notebook-cells nb) 6)))
      (emjupy--mark-running cell)
      (unwind-protect
          (let ((in-place (emjupy-test--rules-picture)))
            (emjupy--reconcile-rules-now)
            (should (equal in-place (emjupy-test--rules-picture)))
            (should (seq-some (lambda (r) (string-match-p "\\[In: " (cadr r)))
                              in-place)))
        (emjupy--stop-spinner)))))

(ert-deftest emjupy-test-a-tick-with-many-cells-queued-is-cheap ()
  "One running indicator tick, a hundred cells queued, costs one header.
It redrew every running cell's header, each rebuilding every rule: with
a long notebook all run, Emacs froze.  Only the executing cell moves;
those queued show a still *, as in Jupyter."
  (emjupy-test--with-notebook (emjupy-test--cells 120) buf nb
    (let ((ids (mapcar #'emjupy-cell-id
                       (append (emjupy-notebook-cells nb) nil))))
      (setq emjupy--running-cells (reverse (seq-take ids 100)))
      (unwind-protect
          ;; Counted, not timed: a tick's cost must not grow with the queue,
          ;; on any machine -- instrumented for coverage too.
          (let ((rebuilds 0) (headers 0))
            (cl-letf* ((real (symbol-function 'emjupy--reconcile-rules-now))
                       ((symbol-function 'emjupy--reconcile-rules-now)
                        (lambda () (cl-incf rebuilds) (funcall real)))
                       (header (symbol-function 'emjupy--redraw-cell-header))
                       ((symbol-function 'emjupy--redraw-cell-header)
                        (lambda (c) (cl-incf headers) (funcall header c))))
              (emjupy--spinner-tick))
            (should (= headers 1))
            (should (<= rebuilds 1))
            (should (emjupy--cell-executing-p
                     (aref (emjupy-notebook-cells nb) 0)))
            (should (string-match-p "\\[In: \\*\\]"
                                    (emjupy--cell-label
                                     (aref (emjupy-notebook-cells nb) 50)))))
        (setq emjupy--running-cells nil)
        (emjupy--stop-spinner)))))

(ert-deftest emjupy-test-an-output-redraw-rebuilds-the-rules-once ()
  "Redrawing a cell's output rebuilds the rules once, not once per part."
  (emjupy-test--with-notebook (emjupy-test--cells 12) buf nb
    (let ((cell (aref (emjupy-notebook-cells nb) 5)) (rebuilds 0))
      (setf (emjupy-cell-outputs cell)
            (vector (let ((o (make-hash-table :test 'equal)))
                      (puthash "output_type" "stream" o)
                      (puthash "name" "stdout" o)
                      (puthash "text" "changed\nand longer\n" o)
                      o)))
      (cl-letf* ((real (symbol-function 'emjupy--reconcile-rules-now))
                 ((symbol-function 'emjupy--reconcile-rules-now)
                  (lambda () (cl-incf rebuilds) (funcall real))))
        (should (emjupy--redraw-cell-output cell)))
      (should (<= rebuilds 1)))))


(ert-deftest emjupy-test-a-reply-redraws-its-cell-not-the-notebook ()
  "A cell's reply redraws that cell, not the notebook.
Each reply redrew the whole notebook: a notebook of N cells, run whole,
was redrawn N times, slower with each cell added."
  (emjupy-test--with-notebook (emjupy-test--cells 120) buf nb
    (let* ((cell (aref (emjupy-notebook-cells nb) 60))
           (pending (make-hash-table :test 'equal))
           (whole 0)
           (content (let ((h (make-hash-table :test 'equal)))
                      (puthash "execution_count" 7 h) h)))
      (puthash "m1" cell pending)
      (emjupy--mark-running cell)
      (cl-letf* ((real (symbol-function 'emjupy--redraw-notebook))
                 ((symbol-function 'emjupy--redraw-notebook)
                  (lambda (&rest args) (cl-incf whole) (apply real args))))
        (emjupy--cell-replied cell content "m1" pending nb))
      (should (= whole 0))
      (should (string-match-p "\\[In: 7\\]"
                              (emjupy--overlay-header
                               (emjupy-cell-overlay cell)))))))

(provide 'emjupy-render-test)

;;; emjupy-render-test.el ends here
