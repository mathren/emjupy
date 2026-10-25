;;; emjupy-undo-test.el --- Unit tests: emjupy-undo.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-undo.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-undo-entries-before-an-edit-are-untouched ()
  "An edit leaves earlier undo entries exactly as they were."
  (with-temp-buffer
    (let ((buffer-undo-list (list (cons 5 10) nil (cons "gone" 50)
                                  (cons 60 70))))
      ;; replace 40..80 with something 5 characters longer
      (emjupy--undo-adjust 40 80 5)
      ;; what is before the edit survives unchanged, boundary included
      (should (equal buffer-undo-list (list (cons 5 10) nil)))
      ;; both entries describing text inside the replaced region are gone,
      ;; because that text no longer exists
      (should-not (cl-find-if (lambda (e) (equal e (cons "gone" 50)))
                              buffer-undo-list))
      (should-not (cl-find-if (lambda (e) (equal e (cons 60 70)))
                              buffer-undo-list)))))

(ert-deftest emjupy-test-undo-entries-after-an-edit-shift ()
  "Entries after the edited region move by the size change, keeping shape."
  (with-temp-buffer
    (let ((buffer-undo-list (list (cons 100 110) (cons "text" 120)
                                  (cons "text" -130))))
      (emjupy--undo-adjust 10 20 7)
      (should (equal (nth 0 buffer-undo-list) (cons 107 117)))
      (should (equal (nth 1 buffer-undo-list) (cons "text" 127)))
      ;; a negative position means point sat at the end of the deleted text;
      ;; the sign carries information and has to survive the shift
      (should (equal (nth 2 buffer-undo-list) (cons "text" -137))))))

;;;; The undo guard

(ert-deftest emjupy-test-undo-map-positions-takes-every-entry-shape ()
  "Every shape of `buffer-undo-list' entry has its positions moved, or is kept.
A property change, (nil PROP VALUE BEG . END), is a dotted list, and was
taken apart with `butlast', which fails on one: \"Wrong type argument:
listp, END\", raised whenever a mode had put a text property on the
notebook and output then arrived above it -- so running a cell failed."
  (let ((plus5 (lambda (x) (+ x 5)))
        (marker (with-temp-buffer (insert "x") (copy-marker 1))))
    (dolist (case `((12 17)                                       ; point
                    ((10 . 20) (15 . 25))                         ; insertion
                    (("abc" . 10) ("abc" . 15))                   ; deletion
                    ;; ... point at its end
                    (("abc" . -10) ("abc" . -15))
                    ;; property
                    ((nil face bold 100 . 1291) (nil face bold 105 . 1296))
                    ;; apply
                    ((apply 3 10 20 ignore x) (apply 3 15 25 ignore x))
                    ((t . 0) (t . 0))                             ; first change
                    ;; marker adjustment
                    ((,marker . 2) (,marker . 2))
                    (nil nil)))                                   ; boundary
      (ert-info ((format "%S" (car case)))
        (should (equal (emjupy--undo-map-positions (car case) plus5)
                       (cadr case)))))))

(ert-deftest emjupy-test-a-property-change-survives-a-redraw ()
  "A recorded property change after a redrawn region is shifted, not fatal."
  (with-temp-buffer
    (insert (make-string 100 ?x))
    (setq buffer-undo-list (list '(nil face bold 50 . 60) '(nil face italic
                                                                2 . 4)))
    ;; text 10..20 redrawn, 7 characters longer
    (emjupy--undo-adjust 10 20 7)
    ;; after: shifted
    (should (member '(nil face bold 57 . 67) buffer-undo-list))
    (should (member '(nil face italic 2 . 4) buffer-undo-list)))) ; before: kept

(ert-deftest emjupy-test-undo-adjustment-is-all-or-nothing ()
  "An undo adjustment that fails partway changes no entry, and keeps no stale one.
Changed in place one by one, a failure left some entries shifted and the
rest not, and the next undo wrote text in the wrong places."
  (with-temp-buffer
    (insert (make-string 200 ?x))
    (setq buffer-undo-list (list '(150 . 160) '(120 . 130) '(5 . 8)))
    (let ((undo (emjupy-test--fail-once 'emjupy--undo-map-positions)))
      (unwind-protect
          ;; text 10..20 redrawn, 7 longer: the first entry after it fails
          (emjupy--undo-adjust 10 20 7)
        (funcall undo)))
    ;; before the region: untouched
    (should (member '(5 . 8) buffer-undo-list))
    ;; at or after it: gone, none half shifted
    (should-not (cl-some (lambda (e) (and (consp e) (integerp (car e))
                                          (>= (car e) 10)))
                         buffer-undo-list))))

(provide 'emjupy-undo-test)

;;; emjupy-undo-test.el ends here
