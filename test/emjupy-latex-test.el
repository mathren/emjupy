;;; emjupy-latex-test.el --- Unit tests: emjupy-latex.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-latex.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-latex-fragments-found ()
  "All four delimiter styles are recognised, and `$$' is matched before
`$' or a display block reads as two empty inline ones."
  (with-temp-buffer
    (insert "Inline $x^2$ then $$e^{i\\pi}+1=0$$ then \\(a+b\\) then \\[c=d\\] end")
    (let ((frags (emjupy--latex-fragments (point-min) (point-max))))
      (should (= (length frags) 4))
      (should (equal (mapcar (lambda (f) (nth 2 f)) frags)
                     '("x^2" "e^{i\\pi}+1=0" "a+b" "c=d")))
      ;; the span covers the delimiters, so an overlay replaces the whole thing
      (should (equal (buffer-substring-no-properties (nth 0 (car frags))
                                                     (nth 1 (car frags)))
                     "$x^2$")))))

(ert-deftest emjupy-test-latex-preview-is-opt-in-and-markdown-only ()
  "Previews are off unless asked for, and never touch code cells: a `$'
in a string is not a formula."
  (should-not emjupy-render-latex)
  (cl-letf (((symbol-function 'emjupy--latex-available-p) (lambda () 'org))
            ((symbol-function 'emjupy--latex-image)
             (lambda (_b) (list 'image :type 'png))))
    (let* ((md (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                 :source "see $x^2$ here" :outputs []
                                 :metadata (make-hash-table)))
           (code (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "cost = 5 # $x$" :outputs []
                                   :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector md code) buf nb
        (with-current-buffer buf
          (cl-flet ((latex-overlays ()
                      (seq-filter (lambda (o) (overlay-get o 'emjupy-latex))
                                  (overlays-in (point-min) (point-max)))))
            ;; off by default
            (let ((emjupy-render-latex nil))
              (emjupy--rerender-notebook)
              (should (= (length (latex-overlays)) 0)))
            ;; on: exactly one, over the markdown cell's formula
            (let ((emjupy-render-latex t))
              (emjupy--rerender-notebook)
              (let ((ovs (latex-overlays)))
                (should (= (length ovs) 1))
                (should (equal (buffer-substring-no-properties
                                (overlay-start (car ovs))
                                (overlay-end (car ovs)))
                               "$x^2$"))
                ;; the LaTeX is still the buffer text underneath
                (should (string-match-p (regexp-quote "$x^2$") (buffer-string)))
                (should (equal (overlay-get (car ovs) 'help-echo)
                               "x^2"))))))))))

(ert-deftest emjupy-test-latex-backend-detection ()
  "`auto' prefers org, which needs no Emacs package beyond what ships
with Emacs; math-preview is the fallback.

Probing is enabled deliberately: this test is about the detection
itself, and `executable-find' is stubbed so the answer still does not
depend on what this machine has installed."
  (let ((emjupy-probe-environment t))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (member p '("latex" "math-preview")))))
      (let ((emjupy-latex-backend 'auto))
        (should (eq (emjupy--latex-available-p) 'org))))
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (equal p "math-preview"))))
      (let ((emjupy-latex-backend 'auto))
        (should (eq (emjupy--latex-available-p) 'math-preview))))
    (cl-letf (((symbol-function 'executable-find) (lambda (_p) nil)))
      (let ((emjupy-latex-backend 'auto))
        (should-not (emjupy--latex-available-p))))
    ;; and with probing off nothing is detected, whatever is installed
    (let ((emjupy-probe-environment nil)
          (emjupy-latex-backend 'auto))
      (should-not (emjupy--latex-available-p)))))

(ert-deftest emjupy-test-latex-follows-the-theme-foreground ()
  "Rendered math takes its colour from the theme, and the image stays
transparent.

org picks which options it reads from the BUFFER argument:

  (fg (or (plist-get options (if buffer :foreground :html-foreground)) \"Black\"))

Passing nil there -- as emjupy did -- makes it read :html-foreground and
:html-background, whose defaults are literally \"Black\" and
\"Transparent\".  So every formula came out black on transparent whatever
the theme said, and :scale was ignored for the same reason."
  ;; Load org BEFORE stubbing: `emjupy--latex-image' requires it, and that
  ;; require redefines `org-create-formula-image' -- clobbering the stub
  ;; below whenever org was not already loaded.
  (let ((captured nil))
    (cl-letf (((symbol-function 'emjupy--latex-available-p) (lambda () 'org))
              ((symbol-function 'emjupy--image-displayable-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-create-formula-image)
               (lambda (_body _file opts buffer &optional _type)
                 (setq captured (list opts buffer))))
              ((symbol-function 'face-attribute)
               (lambda (face attr &rest _)
                 (if (and (eq face 'default) (eq attr :foreground))
                     "#e5e5e5"
                   'unspecified))))
      (let ((emjupy-latex-foreground 'auto)
            (emjupy-latex-scale 1.5))
        (emjupy--latex-image "x^2")
        (pcase-let ((`(,opts ,buffer) captured))
          ;; a real buffer, or org reads the html-* options instead
          (should (bufferp buffer))
          ;; colour follows the theme
          (should (equal (plist-get opts :foreground) "#e5e5e5"))
          ;; and stays transparent rather than baking in a page colour
          (should (equal (plist-get opts :background) "Transparent"))
          ;; :scale is only honoured when a buffer is passed
          (should (= (plist-get opts :scale) 1.5))))
      ;; an explicit colour wins over the theme
      (setq captured nil)
      (let ((emjupy-latex-foreground "#ff0000"))
        (emjupy--latex-image "x^2")
        (should (equal (plist-get (car captured) :foreground) "#ff0000"))))))

(ert-deftest emjupy-test-latex-cache-is-keyed-on-colour ()
  "The cache key includes the colour.  Without it, a formula rendered
under one theme is served back unchanged after switching to another --
black glyphs on a dark background."
  ;; Load org BEFORE stubbing: `emjupy--latex-image' requires it, and that
  ;; require redefines `org-create-formula-image' -- clobbering the stub
  ;; below whenever org was not already loaded.
  (let ((files nil)
        ;; A fresh cache dir: a previous render of the same formula would
        ;; short-circuit the call this test is counting.
        (temporary-file-directory (make-temp-file "emjupy-latex-test" t)))
    (cl-letf (((symbol-function 'emjupy--latex-available-p) (lambda () 'org))
              ((symbol-function 'emjupy--image-displayable-p)
               (lambda (&rest _) nil))
              ((symbol-function 'org-create-formula-image)
               (lambda (_b file &rest _) (push file files))))
      (let ((emjupy-latex-foreground "#000000")) (emjupy--latex-image "x^2"))
      (let ((emjupy-latex-foreground "#ffffff")) (emjupy--latex-image "x^2"))
      (should (= (length files) 2))
      (should-not (equal (nth 0 files) (nth 1 files))))))

(ert-deftest emjupy-test-del-reveals-rendered-latex ()
  "Deleting against a rendered formula reveals its source.

The image is laid OVER the text, so the source was always still there
and still editable -- but with the image covering it there was no way to
see what was being typed.  The first press takes the image away and puts
point after the last character of the formula; the next deletes as
usual."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "see $x^2 + 1$ here" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should (search-forward "$x^2" nil t))
        ;; stand in for a rendered fragment: an overlay displaying an image
        (let* ((beg (match-beginning 0))
               (fin (save-excursion (search-forward "$" nil t)))
               (ov (make-overlay beg fin)))
          (overlay-put ov 'display "IMAGE")
          (overlay-put ov 'emjupy-latex t)
          (goto-char fin)
          (should (emjupy--latex-overlay-near (point)))
          (let ((before (buffer-string)))
            (emjupy-latex-unrender-or-delete 1)
            ;; the image is gone, no text was removed, and point sits at the
            ;; end of the formula ready to edit
            (should-not (emjupy--latex-overlay-near (point)))
            (should (equal (buffer-string) before))
            (should (= (point) fin)))
          ;; pressing again deletes, as it normally would
          (emjupy-latex-unrender-or-delete 1)
          (should-not (equal (char-before (1+ (point))) ?$)))))))

(ert-deftest emjupy-test-del-is-bound-both-ways ()
  "Both spellings of the key reach the command.

A graphical Emacs sends =<backspace>=, which normally translates to DEL --
but a configuration binding =<backspace>= itself takes precedence, and the
DEL binding is then never reached.  Testing the function rather than the
key hid that."
  (should (eq (lookup-key emjupy-mode-map (kbd "DEL"))
              'emjupy-latex-unrender-or-delete))
  (should (eq (lookup-key emjupy-mode-map (kbd "<backspace>"))
              'emjupy-latex-unrender-or-delete)))

;;;; The undo guard

(ert-deftest emjupy-test-latex-is-rendered-in-math-mode ()
  "Math is handed to LaTeX with its delimiters, so in math mode.

The body alone was passed, which LaTeX reads as text: $\\frac{x}{y}$
rendered as an upright x above a stray bar over y.  The delimiters also
say whether the math is inline or display, and are kept for that."
  (should (equal (emjupy--latex-math "$\\frac{x}{y}$") "$\\frac{x}{y}$"))
  (should (equal (emjupy--latex-math "$$\\frac{x}{y}$$") "$$\\frac{x}{y}$$"))
  (should (equal (emjupy--latex-math "\\(a\\)") "\\(a\\)"))
  (should (equal (emjupy--latex-math "\\[a\\]") "\\[a\\]"))
  ;; a bare body is taken as inline math
  (should (equal (emjupy--latex-math "x^2") "$x^2$"))
  ;; and what the preview sends is the whole fragment
  (let ((sent nil) (emjupy-render-latex t))
    (with-temp-buffer
      (insert "a $\\frac{x}{y}$ and $$y^2$$")
      (cl-letf (((symbol-function 'emjupy--latex-available-p) (lambda () 'org))
                ((symbol-function 'emjupy--latex-image)
                 (lambda (frag) (push frag sent) nil)))
        (emjupy--preview-latex-in (point-min) (point-max))))
    (should (member "$\\frac{x}{y}$" sent))
    (should (member "$$y^2$$" sent))))

(ert-deftest emjupy-test-latex-toggle-is-this-notebooks-own ()
  "Toggling LaTeX preview changes this notebook, not the option itself.
Set globally, it turned previews on or off in every notebook, and showed
in Customize as changed outside it."
  (let ((emjupy-render-latex nil)
        (a (generate-new-buffer " *latex-a*"))
        (b (generate-new-buffer " *latex-b*")))
    (unwind-protect
        (cl-letf (((symbol-function 'emjupy--latex-available-p) (lambda () t)))
          (with-current-buffer a (emjupy-toggle-latex-preview))
          (should (buffer-local-value 'emjupy-render-latex a))
          (should-not (buffer-local-value 'emjupy-render-latex b))
          (should-not (default-value 'emjupy-render-latex))
          (with-current-buffer a (emjupy-toggle-latex-preview))
          (should-not (buffer-local-value 'emjupy-render-latex a)))
      (kill-buffer a) (kill-buffer b))))

(provide 'emjupy-latex-test)

;;; emjupy-latex-test.el ends here
