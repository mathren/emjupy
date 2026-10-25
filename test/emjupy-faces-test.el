;;; emjupy-faces-test.el --- Unit tests: emjupy-faces.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-faces.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Output background band

(ert-deftest emjupy-test-hex-colors-parsed-exactly ()
  "Hex colours must be parsed arithmetically, NOT through
`color-name-to-rgb', which resolves via the display palette and on a
tty quantises \"#1c1c1c\" all the way to black."
  (should (equal (emjupy--color-rgb "#000000") '(0.0 0.0 0.0)))
  (should (equal (emjupy--color-rgb "#ffffff") '(1.0 1.0 1.0)))
  (let ((rgb (emjupy--color-rgb "#1c1c1c")))
    (should rgb)
    (should (> (car rgb) 0.0))
    (should (< (car rgb) 0.2)))
  (should (equal (emjupy--color-rgb "#fff") '(1.0 1.0 1.0)))
  (should-not (emjupy--color-rgb "not-a-color"))
  (should-not (emjupy--color-rgb nil)))

(ert-deftest emjupy-test-output-color-contrasts-in-both-directions ()
  "The output accent has to differ from the buffer background in BOTH
directions: darker on a light theme, lighter on a dark one."
  (cl-flet ((accent (bg fg) (emjupy--blend-colors bg fg emjupy-output-blend))
            (lum (c) (apply #'+ (emjupy--color-rgb c))))
    (let ((c (accent "#ffffff" "#000000")))
      (should c)
      (should (< (lum c) (lum "#ffffff"))))
    (let ((c (accent "#1c1c1c" "#e5e5e5")))
      (should c)
      (should (> (lum c) (lum "#1c1c1c"))))))

(ert-deftest emjupy-test-output-color-explicit-and-disabled ()
  "An explicit colour is used verbatim; nil disables the band entirely,
leaving output on the buffer's normal background."
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face attr &rest _)
               (if (eq face 'default)
                   (cond ((eq attr :background) "#ffffff")
                         ((eq attr :foreground) "#000000"))
                 'unspecified))))
    (let ((got (make-hash-table :test 'eq)))
      (cl-letf (((symbol-function 'set-face-attribute)
                 (lambda (face _f _a value) (puthash face value got))))
        (let ((emjupy-output-color "#f0f0f0"))
          (should (emjupy--sync-theme-colors))
          (should (equal (gethash 'emjupy-output got) "#f0f0f0")))
        (let ((emjupy-output-color nil)
              (emjupy-output-error-color nil)
              (emjupy-output-warning-color nil)
              (emjupy-output-image-color nil))
          (should-not (emjupy--sync-theme-colors))
          (should (eq (gethash 'emjupy-output got) 'unspecified))
          (should (eq (gethash 'emjupy-output-error got) 'unspecified)))))))

(ert-deftest emjupy-test-output-face-sets-only-background ()
  "`emjupy-output' must specify a background and NOTHING else.

It is applied as an overlay face over output text, and overlay faces
merge on top of text properties -- so every attribute it specifies
overrides the faces on tracebacks and rich output."
  (should (eq (face-attribute 'emjupy-output :inherit nil nil) 'unspecified))
  ;; INHERIT must be t here -- with nil, `face-attribute' does not follow
  ;; `:inherit', so an inheriting face still reports `unspecified' and the
  ;; check passes while the bug is present.
  (dolist (attr '(:foreground :weight :slant :underline :overline
                  :strike-through :box :inverse-video))
    (should (eq (face-attribute 'emjupy-output attr nil t) 'unspecified))))

;;;; Output-box rendering: visibility, merged divider, 80-col borders

(ert-deftest emjupy-test-box-width-fits-the-window ()
  "The default `window' setting sizes the outline to the window, so the
rule spans a wide frame instead of stopping short at 80 columns."
  (let ((emjupy-box-width 'window)
        (emjupy-box-min-width 60))
    ;; With no window (batch), fall back to a sane fixed width rather than
    ;; erroring or collapsing to zero.
    (should (>= (emjupy--box-width) emjupy-box-min-width))
    ;; Never narrower than the configured floor.
    (let ((emjupy-box-min-width 200))
      (should (>= (emjupy--box-width) 200)))))

;;;; Rich output: display_data / image routing

(ert-deftest emjupy-test-dedup-can-be-disabled ()
  "`emjupy-deduplicate-image-outputs' nil restores the raw behaviour."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda ()
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" "display_data" oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell) (vector (funcall mk) (funcall mk)))
    (let ((emjupy-deduplicate-image-outputs nil))
      (should (= (length (emjupy--outputs-to-draw
                          (emjupy-cell-outputs cell)))
                 2)))
    (let ((emjupy-deduplicate-image-outputs t))
      (should (= (length (emjupy--outputs-to-draw
                          (emjupy-cell-outputs cell)))
                 1)))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-output-faces-are-background-only ()
  "Every output face may set a background and `:extend' and nothing else.
They are applied over output text, so any colour attribute they carried
would fight the faces on tracebacks and rich output."
  (dolist (face '(emjupy-output emjupy-output-error
                 emjupy-output-warning emjupy-output-image))
    (should (eq (face-attribute face :inherit nil nil) 'unspecified))
    ;; NOT :extend -- that runs the background to the window edge, past the
    ;; cell outline. The band is padded to the border instead.
    (should (eq (face-attribute face :extend nil nil) 'unspecified))
    (dolist (attr '(:foreground :weight :slant :underline :box :inverse-video))
      (should (eq (face-attribute face attr nil t) 'unspecified)))))

(ert-deftest emjupy-test-output-classified-by-kind ()
  "Errors, warnings, images and ordinary output each get their own face."
  (cl-flet ((mk (type &optional name img)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (when name (puthash "name" name o))
                (when img
                  (let ((d (make-hash-table :test 'equal)))
                    (puthash "image/png" "iVBORw0KGgoAAAAA" d)
                    (puthash "data" d o)))
                o)))
    (should (eq (emjupy--output-face (mk "error")) 'emjupy-output-error))
    ;; stderr that is not a traceback is where warnings.warn and logging go
    (should (eq (emjupy--output-face (mk "stream" "stderr"))
                'emjupy-output-warning))
    (should (eq (emjupy--output-face (mk "stream" "stdout")) 'emjupy-output))
    (should (eq (emjupy--output-face (mk "display_data" nil t))
                'emjupy-output-image))
    (should (eq (emjupy--output-face (mk "execute_result")) 'emjupy-output))))

(ert-deftest emjupy-test-each-output-piece-painted-separately ()
  "One cell can hold a figure, a warning and a traceback at once, and each
must read as what it is -- so the faces go on the pieces, not on one
overlay across the whole box."
  (cl-flet ((mk (type &rest kv)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (while kv (puthash (pop kv) (pop kv) o))
                o)))
    (let* ((img (let ((d (make-hash-table :test 'equal)))
                  (puthash "image/png" "iVBORw0KGgoAAAAA" d)
                  (puthash "text/plain" "<Figure>" d) d))
           (cell (make-emjupy-cell
                  :id (emjupy--new-cell-id) :type 'code :source "run()"
                  :outputs (vector (mk "stream" "name" "stdout" "text"
                                       "normal out\n")
                                   (mk "stream" "name" "stderr" "text"
                                       "a warning\n")
                                   (mk "display_data" "data" img)
                                   (mk "error" "ename" "ValueError" "evalue"
                                       "boom"
                                       "traceback" ["Traceback line"]))
                  :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (cl-flet ((face-at (probe)
                      (goto-char (point-min))
                      (and (search-forward probe nil t)
                           (get-text-property (match-beginning 0) 'face))))
            (should (memq 'emjupy-output (face-at "normal out")))
            (should (memq 'emjupy-output-warning (face-at "a warning")))
            (should (memq 'emjupy-output-image (face-at "<Figure>")))
            (should (memq 'emjupy-output-error (face-at "ValueError")))
            (should (memq 'emjupy-output-error (face-at "Traceback line"))))
          ;; and the box overlay paints nothing, or it would override them all
          (should-not (overlay-get (emjupy-cell-output-ov cell) 'face)))))))

(ert-deftest emjupy-test-error-and-warning-tints-work-on-both-themes ()
  "`auto' error and warning colours tint toward red and yellow, and stay
distinguishable from the ordinary output colour on light and dark alike."
  (dolist (theme '(("#ffffff" . "#000000") ("#1c1c1c" . "#e5e5e5")))
    (cl-letf (((symbol-function 'face-attribute)
               (lambda (face attr &rest _)
                 (if (eq face 'default)
                     (cond ((eq attr :background) (car theme))
                           ((eq attr :foreground) (cdr theme)))
                   'unspecified))))
      (let ((got (make-hash-table :test 'eq)))
        (cl-letf (((symbol-function 'set-face-attribute)
                   (lambda (face _f _a value) (puthash face value got))))
          (emjupy--sync-theme-colors))
        (let ((err (gethash 'emjupy-output-error got))
              (warn (gethash 'emjupy-output-warning got))
              (out (gethash 'emjupy-output got)))
          (should (emjupy--color-rgb err))
          (should (emjupy--color-rgb warn))
          ;; each is its own colour, not all the same band
          (should-not (equal err out))
          (should-not (equal warn out))
          (should-not (equal err warn))
          ;; red tint: red channel dominates; yellow tint: blue is lowest
          (let ((e (emjupy--color-rgb err)) (w (emjupy--color-rgb warn)))
            (should (> (nth 0 e) (nth 2 e)))
            (should (> (nth 0 w) (nth 2 w)))
            (should (> (nth 1 w) (nth 2 w)))))))))

(ert-deftest emjupy-test-output-colors-are-customizable ()
  "Explicit colours are used verbatim; nil falls back to the ordinary
output colour."
  (cl-letf (((symbol-function 'face-attribute)
             (lambda (face attr &rest _)
               (if (eq face 'default)
                   (cond ((eq attr :background) "#ffffff")
                         ((eq attr :foreground) "#000000"))
                 'unspecified))))
    (let ((got (make-hash-table :test 'eq)))
      (cl-letf (((symbol-function 'set-face-attribute)
                 (lambda (face _f _a value) (puthash face value got))))
        (let ((emjupy-output-error-color "#ff0000")
              (emjupy-output-warning-color "#00ff00")
              (emjupy-output-image-color "#123456")
              (emjupy-output-color "#abcdef"))
          (emjupy--sync-theme-colors)
          (should (equal (gethash 'emjupy-output got) "#abcdef"))
          (should (equal (gethash 'emjupy-output-error got) "#ff0000"))
          (should (equal (gethash 'emjupy-output-warning got) "#00ff00"))
          (should (equal (gethash 'emjupy-output-image got) "#123456")))
        ;; nil means "same as ordinary output"
        (let ((emjupy-output-color "#abcdef")
              (emjupy-output-error-color nil)
              (emjupy-output-image-color nil))
          (emjupy--sync-theme-colors)
          (should (equal (gethash 'emjupy-output-error got) "#abcdef"))
          (should (equal (gethash 'emjupy-output-image got) "#abcdef")))))))

(ert-deftest emjupy-test-box-width-leaves-a-right-margin ()
  "Rules stop short of the right edge by `emjupy-box-right-margin'.
Emacs cannot always report exactly how many columns are usable, so the
margin is the knob for setups where the rule still runs past the edge."
  (let ((emjupy-box-width 'window)
        (emjupy-box-min-width 10))
    (cl-letf (((symbol-function 'emjupy--window-text-width)
               (lambda (&rest _) 100)))
      (dolist (pair '((0 . 100) (2 . 98) (6 . 94)))
        (let ((emjupy-box-right-margin (car pair)))
          (should (= (emjupy--box-width) (cdr pair))))))))

(ert-deftest emjupy-test-output-band-stops-at-the-cell-border ()
  "The tint fills each output line to the cell's right border and no
further.  `:extend' would run it to the WINDOW edge, spilling the colour
past the outline and across the rest of the frame."
  (dolist (face '(emjupy-output emjupy-output-error
                  emjupy-output-warning emjupy-output-image))
    (should (eq (face-attribute face :extend nil nil) 'unspecified)))
  (cl-flet ((mk (type &rest kv)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (while kv (puthash (pop kv) (pop kv) o))
                o)))
    (let ((cell (make-emjupy-cell
                 :id (emjupy--new-cell-id) :type 'code :source "run()"
                 :outputs (vector (mk "stream" "name" "stdout"
                                      "text" "short\nmuch longer line here\n"))
                 :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((emjupy-box-width 40))
            (emjupy--redraw-rules t)
            (let ((ov (emjupy-cell-output-ov cell))
                  (pads 0))
              (save-excursion
                (goto-char (overlay-start ov))
                (while (< (point) (overlay-end ov))
                  (when (get-text-property (point) 'emjupy-pad)
                    (setq pads (1+ pads))
                    ;; every pad reaches the rule exactly: one short, with a
                    ;; faced newline, would leave the band past the border
                    (should (= (plist-get
                                (cdr (get-text-property (point) 'display))
                                :align-to)
                               40)))
                  (forward-char 1)))
              ;; one per output line, not one per character
              (should (> pads 0)))))))))

(ert-deftest emjupy-test-output-padding-follows-a-resize ()
  "The band is padded to a column, so it has to be re-aligned when the
window width changes -- otherwise it stops short of, or runs past, the
newly-drawn rules."
  (cl-flet ((mk (type &rest kv)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (while kv (puthash (pop kv) (pop kv) o))
                o)))
    (let ((cell (make-emjupy-cell
                 :id (emjupy--new-cell-id) :type 'code :source "run()"
                 :outputs (vector (mk "stream" "name" "stdout" "text" "out\n"))
                 :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (cl-flet ((pad-col ()
                      (let ((ov (emjupy-cell-output-ov cell)) (col nil))
                        (save-excursion
                          (goto-char (overlay-start ov))
                          (while (and (not col) (< (point) (overlay-end ov)))
                            (when (get-text-property (point) 'emjupy-pad)
                              (setq col (plist-get
                                         (cdr (get-text-property (point) 'display))
                                         :align-to)))
                            (forward-char 1)))
                        col)))
            (dolist (w '(40 90 60))
              (let ((emjupy-box-width w))
                (emjupy--redraw-rules t)
                ;; the padding tracks the width, and lands on the rule
                (should (= (pad-col) w))))))))))

(ert-deftest emjupy-test-output-band-is-flush-with-the-rule ()
  "The output background must end exactly where the rule does.

Two things pushed it past: the padding stopped one column short of the
rule, and the newline closing each line still carried the face, painting
a column of its own after the padding ended.  Between them the band
poked out beyond the right-hand border."
  (cl-flet ((mk (type &rest kv)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" type o)
                (while kv (puthash (pop kv) (pop kv) o))
                o)))
    (let ((cell (make-emjupy-cell
                 :id (emjupy--new-cell-id) :type 'code :source "run()"
                 :outputs (vector (mk "stream" "name" "stdout" "text" "a\nbb\n")
                                  (mk "stream" "name" "stderr" "text" "warn\n")
                                  (mk "error" "ename" "E" "evalue" "v"
                                      "traceback" ["tb"]))
                 :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (dolist (w '(40 90 60))
            (let ((emjupy-box-width w))
              (emjupy--redraw-rules t)
              (let* ((ov (emjupy-cell-output-ov cell))
                     (rule (1- (length (emjupy--overlay-header ov))))
                     (pads 0))
                (should (= rule w))
                (save-excursion
                  (goto-char (overlay-start ov))
                  (while (< (point) (overlay-end ov))
                    (when (get-text-property (point) 'emjupy-pad)
                      (setq pads (1+ pads))
                      ;; the padding reaches the rule exactly
                      (should (= (plist-get
                                  (cdr (get-text-property (point) 'display))
                                  :align-to)
                                 rule)))
                    ;; and nothing paints past it
                    (when (eq (char-after) ?\n)
                      (should-not (get-text-property (point) 'face)))
                    (forward-char 1)))
                (should (> pads 0))))))))))

(ert-deftest emjupy-test-rule-does-not-face-its-newline ()
  "A newline carrying a background paints a column of its own past the
last box character, so the rule stuck out one column to the right."
  (let ((s (emjupy--rule "[Out: 1]" "├")))
    (should (eq (get-text-property 0 'face s) 'emjupy-box-line))
    (should (string-suffix-p "\n" s))
    (should-not (get-text-property (1- (length s)) 'face s))))

;;;; Performance

(ert-deftest emjupy-test-refontifying-a-cell-leaves-output-padding-alone ()
  "Re-highlighting a cell cannot disturb the padding of an output box.

Highlighting is re-applied to a cell after an edit: the old faces are
cleared and the new ones copied in from a fontified copy of the source.
If that region ever reaches into an output box -- and it need only be
off by a line -- the padding there is cleared and nothing puts it back,
because the source being copied from has no padding in it.  The result
is an output band whose background stops partway across the line."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)"
               (vector (emjupy-test--stream-output "alpha\nbeta\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-flet ((pad-faces ()
                    (let (faces)
                      (save-excursion
                        (goto-char (point-min))
                        (while (< (point) (point-max))
                          (when (get-text-property (point) 'emjupy-pad)
                            (push (get-text-property (point)
                                                     'font-lock-face) faces))
                          (forward-char 1)))
                      (nreverse faces))))
          (should (equal (pad-faces) '(emjupy-output emjupy-output)))
          ;; a re-highlight whose region covers the whole buffer, output and
          ;; all -- the worst case of a region being wrong
          (let ((inhibit-read-only t))
            (emjupy--apply-faces-from
             (propertize (make-string (- (point-max) (point-min)) ?x)
                         'face 'font-lock-keyword-face)
             (point-min)))
          ;; the padding is untouched: it is not source and never was
          (should (equal (pad-faces) '(emjupy-output emjupy-output))))))))

;;;; The undo guard

(ert-deftest emjupy-test-figures-can-be-kept-out-of-the-notebook ()
  "With `emjupy-inline-figures' nil, a figure is a line that opens it."
  (let ((emjupy-inline-figures nil))
    (cl-letf (((symbol-function 'emjupy--image-displayable-p)
               (lambda (&rest _) t)))
      (with-temp-buffer
        (emjupy--insert-rich-output (gethash "data" (emjupy-test--png-output)))
        (should (string-match-p "▶ Figure (PNG)" (buffer-string)))
        ;; no image drawn anywhere
        (should-not (get-text-property (point-min) 'display))
        (should-not (next-single-property-change (point-min) 'display))
        (goto-char (point-min))
        (should (get-text-property (point) 'emjupy-output-data))))))

(provide 'emjupy-faces-test)

;;; emjupy-faces-test.el ends here
