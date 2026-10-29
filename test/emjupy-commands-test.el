;;; emjupy-commands-test.el --- Unit tests: emjupy-commands.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-commands.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Cell operations: insert / delete / move / cycle-type

(ert-deftest emjupy-test-cell-insertion-and-deletion ()
  "Creating and deleting notebook cells dynamically."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 10"
                                :outputs [] :metadata (make-hash-table)))
      buf nb
    (with-current-buffer buf
      (goto-char (point-min))
      (emjupy-insert-cell-below)
      (should (= (length (emjupy-notebook-cells nb)) 2))
      (emjupy-delete-cell)
      (should (= (length (emjupy-notebook-cells nb)) 1)))))

(ert-deftest emjupy-test-move-cell-up-down ()
  "Moving a cell swaps its position; its output moves WITH it (output is
a field on the cell struct, not a separate entity); the top/bottom
boundary is a no-op rather than an error."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "A" :outputs []
                               :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "B" :outputs []
                               :metadata (make-hash-table)))
         (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "C" :outputs []
                               :metadata (make-hash-table)))
         (out (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" out) (puthash "text" "B-output\n" out)
    (setf (emjupy-cell-outputs c2) (vector out))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source
                               (emjupy-notebook-cells nb)) '("B" "A" "C")))
        (should (> (length (emjupy-cell-outputs
                            (aref (emjupy-notebook-cells nb) 0))) 0))
        ;; the (now) top cell moving up again must be a no-op
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source
                               (emjupy-notebook-cells nb)) '("B" "A" "C")))))))

(ert-deftest emjupy-test-cycle-cell-type ()
  "C-c C-t toggles a cell between code and markdown."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "1+1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'markdown))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'code))))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-split-cell-at-point ()
  "Splitting divides the source at point: text before stays, text from
point on moves to a new cell just below, and the rest keeps its order."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "a = 1\nb = 2\nc = 3"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "tail = 0"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (should (search-forward "b = 2" nil t))
        (beginning-of-line)
        (let ((new (emjupy-split-cell))
              (cells (emjupy-notebook-cells nb)))
          (should (= (length cells) 3))
          (should (equal (emjupy-cell-source (aref cells 0)) "a = 1\n"))
          (should (equal (emjupy-cell-source (aref cells 1)) "b = 2\nc = 3"))
          (should (equal (emjupy-cell-source (aref cells 2)) "tail = 0"))
          (should (= (point) (overlay-start (emjupy-cell-overlay new)))))))))

(ert-deftest emjupy-test-split-cell-keeps-type ()
  "The new half is the same kind of cell as the one it came from."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "# one\n# two"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should (search-forward "# two" nil t))
        (beginning-of-line)
        (emjupy-split-cell)
        (let ((cells (emjupy-notebook-cells nb)))
          (should (eq (emjupy-cell-type (aref cells 0)) 'markdown))
          (should (eq (emjupy-cell-type (aref cells 1)) 'markdown))
          (should (equal (emjupy-cell-source (aref cells 0)) "# one\n"))
          (should (equal (emjupy-cell-source (aref cells 1)) "# two")))))))

(ert-deftest emjupy-test-merge-cell-above-joins-and-clears-output ()
  "Merging joins the two sources with a newline and discards the output of
BOTH cells: neither set was produced by the merged code, and keeping one
would attribute results to source that never generated them."
  (cl-flet ((with-out (cell text)
              (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" "stream" o)
                (puthash "name" "stdout" o)
                (puthash "text" text o)
                (setf (emjupy-cell-outputs cell) (vector o))
                (setf (emjupy-cell-exec-count cell) 5)
                cell)))
    (let* ((c1 (with-out (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                           :source "a = 1" :outputs []
                                           :metadata (make-hash-table))
                         "out-a\n"))
           (c2 (with-out (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                           :source "b = 2" :outputs []
                                           :metadata (make-hash-table))
                         "out-b\n"))
           (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "c = 3"
                                 :outputs [] :metadata (make-hash-table))))
      (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
        (with-current-buffer buf
          (goto-char (overlay-start (emjupy-cell-overlay c2)))
          (let ((merged (emjupy-join-cell-above))
                (cells (emjupy-notebook-cells nb)))
            (should (eq merged c1))
            (should (= (length cells) 2))
            (should (equal (emjupy-cell-source (aref cells 0)) "a = 1\nb = 2"))
            (should (equal (emjupy-cell-source (aref cells 1)) "c = 3"))
            ;; both outputs gone, and the box with them
            (should (= (length (emjupy-cell-outputs merged)) 0))
            (should-not (emjupy-cell-exec-count merged))
            (should-not (emjupy-cell-output-ov merged))
            (should-not (string-match-p "out-a" (buffer-string)))
            (should-not (string-match-p "out-b" (buffer-string)))
            ;; point lands at the seam, where the merged-in cell began
            (should (string-prefix-p "b = 2"
                                     (buffer-substring-no-properties
                                      (point) (min (point-max)
                                                   (+ (point) 5)))))))))))

(ert-deftest emjupy-test-merge-cell-above-guards ()
  "Merging refuses where it would be meaningless or destructive: there is
nothing above the first cell, and merging code into prose would silently
reinterpret one of them."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                               :source "# x"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "y = 1"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (should-error (emjupy-join-cell-above) :type 'user-error)
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (should-error (emjupy-join-cell-above) :type 'user-error)
        ;; nothing was changed by either refusal
        (should (= (length (emjupy-notebook-cells nb)) 2))))))

(ert-deftest emjupy-test-split-and-merge-round-trip ()
  "Splitting and merging back gives the original source."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1\nb = 2"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should (search-forward "b = 2" nil t))
        (beginning-of-line)
        (emjupy-split-cell)
        (should (= (length (emjupy-notebook-cells nb)) 2))
        (emjupy-join-cell-above)
        (let ((cells (emjupy-notebook-cells nb)))
          (should (= (length cells) 1))
          (should (equal (emjupy-cell-source (aref cells 0))
                         "a = 1\nb = 2")))))))

(ert-deftest emjupy-test-cell-lookup-survives-typing-at-a-boundary ()
  "Text typed at the end of a cell's last line still belongs to that cell.

The `emjupy-cell' property is stamped at render time and plain `insert'
does not carry it onto new text, so a lookup by property missed anything
typed at a boundary -- failing loudly in some commands and silently in
others: `emjupy-insert-cell-below' found no current cell and appended to
the END of the notebook instead of inserting below."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "x = 1"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "last = 1"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-end (emjupy-cell-overlay c1)))
        (forward-line -1)
        (end-of-line)
        (insert "\ny = 2")
        (goto-char (point-min))
        (should (search-forward "y = 2" nil t))
        (beginning-of-line)
        (should (eq (emjupy--cell-at-point) c1))
        (emjupy-insert-cell-below)
        (let ((cells (emjupy-notebook-cells nb)))
          (should (= (length cells) 3))
          (should (eq (aref cells 0) c1))
          (should (eq (aref cells 2) c2)))))))

(ert-deftest emjupy-test-split-and-merge-are-bound ()
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-s")) 'emjupy-split-cell))
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-j"))
              'emjupy-join-cell-above)))

(ert-deftest emjupy-test-beginning-and-end-of-cell ()
  "`C-c <up>' and `C-c <down>' go to the start and the end of the cell's
SOURCE -- the end of the source, not of the overlay, which takes in the
closing newline and would land point on the next line."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1\nb = 2"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (+ 3 (overlay-start (emjupy-cell-overlay cell))))
        (emjupy-beginning-of-cell)
        (should (= (point) (overlay-start (emjupy-cell-overlay cell))))
        (emjupy-end-of-cell)
        (should (equal (buffer-substring-no-properties (- (point) 5) (point))
                       "b = 2"))
        ;; still inside the cell, not on the line after it
        (should (eq (emjupy--cell-at-point) cell))))))

(ert-deftest emjupy-test-split-clears-output-of-both-halves ()
  "Splitting discards the output of BOTH halves.  Neither has been run as
it now stands, so keeping the results would attribute them to source
that never produced them."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1\nb = 2" :outputs []
                                :metadata (make-hash-table)))
        (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "stale\n" oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (setf (emjupy-cell-exec-count cell) 4)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should (search-forward "b = 2" nil t))
        (beginning-of-line)
        (emjupy-split-cell)
        (let ((cells (emjupy-notebook-cells nb)))
          (should (= (length cells) 2))
          (dotimes (i 2)
            (should (= (length (emjupy-cell-outputs (aref cells i))) 0))
            (should-not (emjupy-cell-exec-count (aref cells i)))
            (should-not (emjupy-cell-output-ov (aref cells i))))
          (should-not (string-match-p "stale" (buffer-string))))))))

(ert-deftest emjupy-test-copy-cell-strips-output ()
  "Copying takes the type and the source only.  A cell's output belongs to
the run that produced it."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "# heading" :outputs []
                                :metadata (make-hash-table)))
        (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "not copied\n" oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((emjupy--cell-clipboard nil))
          (emjupy-copy-cell)
          (should (equal (car emjupy--cell-clipboard) 'markdown))
          (should (equal (cdr emjupy--cell-clipboard) "# heading"))
          ;; and the source is on the kill ring for use elsewhere
          (should (equal (current-kill 0) "# heading")))))))

(ert-deftest emjupy-test-yank-cell-inserts-below-without-output ()
  "Yanking puts the copy below the cell at point, unrun."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "first"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "second"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((emjupy--cell-clipboard '(markdown . "# pasted")))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (let ((new (emjupy-yank-cell))
                (cells (emjupy-notebook-cells nb)))
            (should (= (length cells) 3))
            ;; inserted directly below the cell point was in
            (should (eq (aref cells 0) c1))
            (should (eq (aref cells 1) new))
            (should (eq (aref cells 2) c2))
            (should (eq (emjupy-cell-type new) 'markdown))
            (should (equal (emjupy-cell-source new) "# pasted"))
            (should (= (length (emjupy-cell-outputs new)) 0))
            (should-not (emjupy-cell-exec-count new))))))))

(ert-deftest emjupy-test-yank-without-a-copy-says-so ()
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((emjupy--cell-clipboard nil))
          (should-error (emjupy-yank-cell) :type 'user-error))))))

(ert-deftest emjupy-test-cell-bindings ()
  "The bindings this release settled on."
  (dolist (pair '(("C-c <up>"    . emjupy-move-cell-up)
                 ("C-c <down>"  . emjupy-move-cell-down)
                 ("C-c <prior>" . emjupy-beginning-of-cell)
                 ("C-c <next>"  . emjupy-end-of-cell)
                 ("C-c C-w"     . emjupy-copy-cell)
                 ("C-c C-y"     . emjupy-yank-cell)
                 ("C-c C-s"     . emjupy-split-cell)
                 ("C-c C-j"     . emjupy-join-cell-above)))
    (should (eq (lookup-key emjupy-mode-map (kbd (car pair))) (cdr pair))))
  ;; and the retired duplicates are gone -- except C-c C-s, now split-cell, and
  ;; M-<up>/M-<down>, which now move to a neighbouring cell.
  (dolist (key '("M-RET" "C-c C-r" "M-n" "M-p"))
    (should-not (lookup-key emjupy-mode-map (kbd key)))))

(ert-deftest emjupy-test-clear-all-outputs-asks-first ()
  "Clearing everything throws away results that may have been slow to
produce, so it asks -- and declining changes nothing."
  (let ((cells (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "a"
                                         :outputs []
                                         :metadata (make-hash-table))
                       (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "b"
                                         :outputs []
                                         :metadata (make-hash-table)))))
    (cl-loop for c across cells
             do (let ((o (make-hash-table :test 'equal)))
                  (puthash "output_type" "stream" o)
                  (puthash "name" "stdout" o)
                  (puthash "text" "out\n" o)
                  (setf (emjupy-cell-outputs c) (vector o))
                  (setf (emjupy-cell-exec-count c) 2)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        ;; declined
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
          (emjupy-clear-all-outputs))
        (should (= (cl-loop for c across cells sum
                            (length (emjupy-cell-outputs c))) 2))
        (should (string-match-p "out" (buffer-string)))
        ;; accepted
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (emjupy-clear-all-outputs))
        (should (= (cl-loop for c across cells sum
                            (length (emjupy-cell-outputs c))) 0))
        (cl-loop for c across cells
                 do (should-not (emjupy-cell-exec-count c))
                 (should-not (emjupy-cell-output-ov c)))
        (should-not (string-match-p "out\n" (buffer-string)))))))

(ert-deftest emjupy-test-neighbouring-cell-navigation ()
  "M-<prior>/M-<next> reach the previous cell, M-<up>/M-<down> the next."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "one"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "two"
                              :outputs [] :metadata (make-hash-table)))
        (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "three"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (emjupy-beginning-of-previous-cell)
        (should (eq (emjupy--cell-at-point) c1))
        (should (= (point) (overlay-start (emjupy-cell-overlay c1))))
        (emjupy-end-of-next-cell)
        (should (eq (emjupy--cell-at-point) c2))
        ;; end of the SOURCE, so still inside the cell
        (should (equal (buffer-substring-no-properties (- (point) 3) (point))
                       "two"))
        (emjupy-beginning-of-next-cell)
        (should (eq (emjupy--cell-at-point) c3))
        ;; nothing beyond the last cell, or before the first
        (should-error (emjupy-beginning-of-next-cell) :type 'user-error)
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (should-error (emjupy-beginning-of-previous-cell) :type 'user-error)))))

(ert-deftest emjupy-test-navigation-bindings ()
  (dolist (pair '(("M-<prior>" . emjupy-beginning-of-previous-cell)
                 ("M-<next>"  . emjupy-end-of-previous-cell)
                 ("M-<up>"    . emjupy-beginning-of-next-cell)
                 ("M-<down>"  . emjupy-end-of-next-cell)
                 ("C-c C-x C-l" . emjupy-re-render)))
    (should (eq (lookup-key emjupy-mode-map (kbd (car pair))) (cdr pair)))))

(ert-deftest emjupy-test-cell-at-point-past-the-last-cell ()
  "Point past the last cell still resolves to a cell.

Returning nil made `emjupy-insert-cell-below' think there was no
current cell, so it appended to the END of the notebook instead of
inserting below the one being looked at."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "a"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "b"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (point-max))
        (should (eq (emjupy--cell-at-point) c2))
        ;; and a new cell lands after it rather than being appended blindly
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (emjupy-insert-cell-below)
        (let ((cells (emjupy-notebook-cells nb)))
          (should (= (length cells) 3))
          (should (eq (aref cells 0) c1))
          (should (eq (aref cells 2) c2)))))))

(ert-deftest emjupy-test-delete-cell-lands-on-the-next-cell ()
  "Deleting leaves point on the cell that slid up into its place, not at
the end of the notebook."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "a"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "b"
                              :outputs [] :metadata (make-hash-table)))
        (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "c"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (emjupy-delete-cell)
        (should (= (length (emjupy-notebook-cells nb)) 2))
        (should (eq (emjupy--cell-at-point) c3))
        ;; deleting the last cell leaves point on the new last one
        (emjupy-delete-cell)
        (should (= (length (emjupy-notebook-cells nb)) 1))
        (should (eq (emjupy--cell-at-point) c1))))))

(ert-deftest emjupy-test-tab-indents-and-cycles ()
  "TAB indents the line as python-mode would, and offers the
alternatives on repeat.  After `def f():' the next line could be the
body or back at the outer level, and python-mode cycles between them --
so emjupy asks python-mode rather than guessing."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "def f():\nreturn 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (forward-line 1)
        (cl-flet ((second-line ()
                    (nth 1 (split-string (emjupy-cell-source cell) "\n"))))
          (emjupy-indent-or-cycle)
          (should (equal (second-line) "    return 1"))
          ;; pressed again, the other candidate
          (emjupy-indent-or-cycle)
          (should (equal (second-line) "return 1"))
          (emjupy-indent-or-cycle)
          (should (equal (second-line) "    return 1")))))))

(ert-deftest emjupy-test-traceback-buffer ()
  "C-$ opens the full traceback with room to read it, keeping the
kernel's colours on top of Python highlighting."
  (let ((o (make-hash-table :test 'equal))
        (cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "boom()"
                                :outputs [] :metadata (make-hash-table))))
    (puthash "output_type" "error" o)
    (puthash "ename" "ValueError" o)
    (puthash "evalue" "bad" o)
    (puthash "traceback"
             (vector "\033[31mTraceback (most recent call last)\033[0m"
                     "  File \"<ipython>\", line 1, in <module>"
                     "    boom()"
                     "ValueError: bad")
             o)
    (setf (emjupy-cell-outputs cell) (vector o))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((tbuf (emjupy-show-traceback)))
          (unwind-protect
              (with-current-buffer tbuf
                (should (eq major-mode 'python-mode))
                (should buffer-read-only)
                ;; every frame is there, not just the summary line
                (should (= (count-lines (point-min) (point-max)) 4))
                (should (string-match-p "in <module>" (buffer-string)))
                ;; escapes gone, colour kept
                (goto-char (point-min))
                (should-not (save-excursion (search-forward "31m" nil t)))
                (should (search-forward "Traceback" nil t))
                (let ((face (get-text-property (- (point) 3) 'face)))
                  (should (cl-some (lambda (f) (and (consp f)
                                                    (plist-get f :foreground)))
                                   (if (listp face) face (list face))))))
            (let ((kill-buffer-query-functions nil)) (kill-buffer tbuf))))))))

(ert-deftest emjupy-test-traceback-without-an-error ()
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should-error (emjupy-show-traceback) :type 'user-error)))))

(ert-deftest emjupy-test-new-bindings ()
  (should (eq (lookup-key emjupy-mode-map (kbd "TAB")) 'emjupy-indent-or-cycle))
  (should (eq (lookup-key emjupy-mode-map (kbd "C-$")) 'emjupy-show-traceback)))

(ert-deftest emjupy-test-undo-survives-cell-insertion-and-deletion ()
  "Adding or removing a cell no longer costs the undo history.

A full rebuild moves every position in the buffer while undo recording
is off, and undo entries hold plain positions, so the history had to be
dropped.  Inserting or deleting one cell moves only what follows it, by
a known amount, so entries before it are untouched and those after it
shift.  This was the structural gap notes_undo.org described."
  (cl-flet ((mk (s) (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                      :source s
                                      :outputs [] :metadata (make-hash-table))))
    (dolist (action '(insert delete))
      (let* ((c1 (mk "a = 1")) (c2 (mk "b = 2")) (c3 (mk "c = 3"))
             (nb (make-emjupy-notebook :cells (vector c1 c2 c3)
                                       :path "n.ipynb"))
             (buf (generate-new-buffer "*structural*")))
        (unwind-protect
            (with-current-buffer buf
              (emjupy-mode)
              (setq emjupy--buffer-notebook nb)
              (setf (emjupy-notebook-buffer nb) buf)
              (emjupy--redraw-notebook)
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              ;; type into the FIRST cell, then act on the LAST one
              (goto-char (overlay-start (emjupy-cell-overlay c1)))
              (end-of-line)
              (insert " + 7")
              (undo-boundary)
              (goto-char (overlay-start (emjupy-cell-overlay c3)))
              (if (eq action 'insert)
                  (emjupy-insert-cell-below)
                (emjupy-delete-cell))
              (should (= (length (emjupy-notebook-cells nb))
                         (if (eq action 'insert) 4 2)))
              ;; the typing is still there and still undoable
              (should (string-match-p "a = 1 \\+ 7" (buffer-string)))
              (undo)
              ;; deleting a cell is undoable in its own right, so that step
              ;; comes back first and the typing needs one more
              (when (string-match-p "\\+ 7" (buffer-string))
                (undo-more 1))
              (should-not (string-match-p "\\+ 7" (buffer-string)))
              (should (string-match-p "a = 1" (buffer-string))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))))

(ert-deftest emjupy-test-undo-survives-split-and-merge ()
  "Splitting and merging no longer cost the undo history.

Both change only the cells involved, so everything outside moves by a
known amount and `emjupy--undo-adjust' can keep the entries: before is
untouched, inside is dropped, after is shifted.  This was the last of
the structural gaps notes_undo.org described."
  (cl-flet ((mk (s) (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                      :source s
                                      :outputs [] :metadata (make-hash-table))))
    (dolist (action '(split merge))
      (let* ((c1 (mk "a = 1")) (c2 (mk "b = 2\nc = 3")) (c3 (mk "d = 4"))
             (nb (make-emjupy-notebook :cells (vector c1 c2 c3)
                                       :path "n.ipynb"))
             (buf (generate-new-buffer "*structural2*")))
        (unwind-protect
            (with-current-buffer buf
              (emjupy-mode)
              (setq emjupy--buffer-notebook nb)
              (setf (emjupy-notebook-buffer nb) buf)
              (emjupy--redraw-notebook)
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              ;; type into the FIRST cell, then act further down
              (goto-char (overlay-start (emjupy-cell-overlay c1)))
              (end-of-line)
              (insert " + 7")
              (undo-boundary)
              (if (eq action 'split)
                  (progn (goto-char (overlay-start (emjupy-cell-overlay c2)))
                         (forward-line 1)
                         (emjupy-split-cell))
                (goto-char (overlay-start (emjupy-cell-overlay c3)))
                (emjupy-join-cell-above))
              (should (= (length (emjupy-notebook-cells nb))
                         (if (eq action 'split) 4 2)))
              (should (string-match-p "a = 1 \\+ 7" (buffer-string)))
              (emjupy-test--undo-past-steps "\\+ 7")
              (should-not (string-match-p "\\+ 7" (buffer-string)))
              (should (string-match-p "a = 1" (buffer-string))))
          (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))))

;;;; Invariants

(ert-deftest emjupy-test-invariants-hold-across-every-operation ()
  "Buffer and cells agree after each structural operation in turn.

Run as a sequence rather than separately: the failures this is meant to
catch -- an overlay left overlapping, a sync quietly refusing -- appear
when operations follow one another, not in isolation."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'code "b = 2\nc = 3")
                       (emjupy-test--cell-like 'code "d = 4"))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (emjupy-test--assert-invariants "start")
        ;; insert
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (emjupy-insert-cell-below)
        (emjupy-test--assert-invariants "after insert")
        ;; type into a cell
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert " + 9")
        (should (emjupy--sync-all-cells))
        (emjupy-test--assert-invariants "after typing")
        ;; split
        (let ((multi (cl-find-if (lambda (c)
                                   (string-match-p "\n" (emjupy-cell-source c)))
                                 (append (emjupy-notebook-cells nb) nil))))
          (when multi
            (goto-char (overlay-start (emjupy-cell-overlay multi)))
            (forward-line 1)
            (emjupy-split-cell)
            (emjupy-test--assert-invariants "after split")))
        ;; merge
        (goto-char (overlay-start
                    (emjupy-cell-overlay
                     (aref (emjupy-notebook-cells nb)
                           (1- (length (emjupy-notebook-cells nb)))))))
        (emjupy-join-cell-above)
        (emjupy-test--assert-invariants "after merge")
        ;; output arriving
        (emjupy--append-output-to-cell (aref (emjupy-notebook-cells nb) 0)
                                       (emjupy-test--stream-output "out\n") nb)
        (emjupy-flush-output buf)
        (emjupy-test--assert-invariants "after output")
        ;; clearing it again
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        (emjupy-test--assert-invariants "after clearing output")
        ;; delete
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 1))))
        (emjupy-delete-cell)
        (emjupy-test--assert-invariants "after delete")
        ;; and a full redraw on top of all that
        (emjupy--redraw-notebook)
        (emjupy-test--assert-invariants "after redraw")))))

;;;; Undo matrix

(ert-deftest emjupy-test-undo-matrix ()
  "Undo survives every operation that redraws part of the buffer.

Each case types into the first cell, does something elsewhere, then
undoes.  A full rebuild cannot keep the history -- undo entries hold
plain buffer positions and a rebuild moves all of them -- so an
operation appearing here is a claim that it does not rebuild."
  (dolist (case
           (list
            (cons "output arrives"
                  (lambda (nb buf)
                    (emjupy--append-output-to-cell
                     (aref (emjupy-notebook-cells nb) 2)
                     (emjupy-test--stream-output "out\n") nb)
                    (emjupy-flush-output buf)))
            (cons "insert a cell"
                  (lambda (nb _buf)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 2))))
                    (emjupy-insert-cell-below)))
            (cons "delete a cell"
                  (lambda (nb _buf)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 2))))
                    (emjupy-delete-cell)))
            (cons "split a cell"
                  (lambda (nb _buf)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 1))))
                    (forward-line 1)
                    (emjupy-split-cell)))
            (cons "merge cells"
                  (lambda (nb _buf)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 2))))
                    (emjupy-join-cell-above)))
            (cons "clear output"
                  (lambda (nb buf)
                    (let ((cell (aref (emjupy-notebook-cells nb) 2)))
                      (emjupy--append-output-to-cell
                       cell (emjupy-test--stream-output "out\n") nb)
                      (emjupy-flush-output buf)
                      (goto-char (overlay-start (emjupy-cell-overlay cell)))
                      (let ((emjupy-confirm-clear-output nil))
                        (emjupy-clear-cell-output)))))
            (cons "two operations in a row"
                  (lambda (nb _buf)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 2))))
                    (emjupy-insert-cell-below)
                    (goto-char (overlay-start
                                (emjupy-cell-overlay
                                 (aref (emjupy-notebook-cells nb) 1))))
                    (emjupy-delete-cell)))))
    (let* ((label (car case))
           (r (emjupy-test--undo-case (cdr case))))
      (should (equal (cons label (plist-get r :worked)) (cons label t)))
      (should (equal (cons label (plist-get r :undone)) (cons label t)))
      (should (equal (cons label (plist-get r :intact)) (cons label t)))
      ;; and what would be saved matches what is shown
      (should (equal (cons label (plist-get r :saved-undone)) (cons label t)))
      (should (equal (cons label (plist-get r :problems)) (cons label nil))))))

;;;; Performance

(ert-deftest emjupy-test-click-elsewhere-does-nothing ()
  "A click that names no cell is ignored rather than acting on a guess."
  (let ((cell (emjupy-test--cell-like
               'code "x" (vector (emjupy-test--stream-output "out\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; a plain string, as a click on ordinary text gives
        (should-not (emjupy--toggle-output-from-string
                     (cons "no marker here" 2) buf))
        (should-not (emjupy--cell-outputs-hidden-p cell))
        ;; and nothing at all, as a click outside any string gives
        (should-not (emjupy--toggle-output-from-string nil buf))
        (should-not (emjupy--cell-outputs-hidden-p cell))))))

;;;; The undo guard

(ert-deftest emjupy-test-clearing-one-cell-asks-first ()
  "Clearing a cell's output asks, and says what would go.

The output may be the result of a long run, and the kernel cannot be
asked to produce it again for nothing."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)"
               (vector (emjupy-test--stream-output "out\n")))))
    (setf (emjupy-cell-exec-count cell) 4)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        ;; answered no: the output stays
        (let ((asked nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (prompt) (setq asked prompt) nil)))
            (emjupy-clear-cell-output))
          (should asked)
          ;; the prompt is about this cell: how much, and which run
          (should (string-match-p "1 output" asked))
          (should (string-match-p "\\[4\\]" asked))
          (should (= (length (emjupy-cell-outputs cell)) 1)))
        ;; answered yes: it goes
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (emjupy-clear-cell-output))
        (should (= (length (emjupy-cell-outputs cell)) 0))))))

(ert-deftest emjupy-test-edit-survives-the-command-that-rewrites-its-cell ()
  "An edit comes back even when the command rewrote the cell holding it.

Moving a cell up and merging into the one above both replace the text of
the cell ABOVE, so an entry describing an edit there is inside the
region being replaced.  Dropping it -- right in general, since that text
usually ceases to exist -- made the edit unrecoverable.  These commands
record a step that restores the region exactly, and nothing can reach
those entries until that step has been undone, so they are kept.

Asserted as `recoverable within a few presses' rather than `one press':
a structural command is its own undo step now, and the edit sits behind
it."
  (dolist (command '(emjupy-move-cell-up emjupy-join-cell-above
                    emjupy-split-cell emjupy-delete-cell
                    emjupy-cycle-cell-type emjupy-clear-all-outputs))
    (let ((presses (emjupy-test--presses-to-recover command)))
      (should presses)
      (should (<= presses 3)))))

(ert-deftest emjupy-test-reshown-output-is-not-highlighted-as-code ()
  "Output shown again after being hidden keeps its own face, not Python's.

Redrawing a cell in place inserts its text at the start of the next
cell's overlay, which takes the insertion in until the redraw moves it
back.  The change hook, there to re-highlight what the user types, took
the next cell to be the one edited and re-highlighted its region --
now the reshown output -- as code: keywords coloured, and the output's
background gone where they were.  emjupy's own drawing is not an edit
to re-highlight, and does not run the hook."
  (dolist (how '(command click))
    (let* ((out (let ((o (make-hash-table :test 'equal)))
                  (puthash "output_type" "stream" o) (puthash "name" "stdout" o)
                  (puthash "text"
                           "for x in y: print(True, None)\n# a comment\n" o)
                  o))
           (cells (vector (emjupy-test--cell-like 'code "x = 1" (vector out))
                          ;; the next cell is what the redraw inserted into
                          (emjupy-test--cell-like 'code "y = 2")))
           (cell (aref cells 0)))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (goto-char (overlay-start (emjupy-cell-overlay cell)))
          (dotimes (_ 2)
            (if (eq how 'command)
                (emjupy-toggle-cell-output)
              (emjupy-toggle-output-of-cell cell)))
          (let ((ov (emjupy-cell-output-ov cell)))
            (should (overlayp ov))
            (save-excursion
              (dolist (word '("for" "print" "True" "None" "comment"))
                (goto-char (overlay-start ov))
                (should (search-forward word (overlay-end ov) t))
                (ert-info ((format "%s, %s" how word))
                  (should (equal (get-text-property (match-beginning 0) 'face)
                                 '(emjupy-output))))))))))))


(ert-deftest emjupy-test-moving-a-cell-keeps-point-where-it-was-in-it ()
  "Moving a cell up or down leaves point at the same place in its code.
It went to the end of the code: \\='start, which `emjupy--goto-cell'
takes for the end."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id 1 :type 'code :source "first = 1")
              (make-emjupy-cell :id 2 :type 'code
                                :source "second = 2\nmore = 3"))
      buf nb
    (let ((cell (aref (emjupy-notebook-cells nb) 1)))
      (emjupy--goto-cell cell 'beginning)
      (forward-char 3)
      (emjupy-move-cell-up)
      (should (eq (emjupy--cell-at-point) cell))
      (should (looking-at "ond = 2"))
      (emjupy-move-cell-down)
      (should (eq (emjupy--cell-at-point) cell))
      (should (looking-at "ond = 2")))))

(ert-deftest emjupy-test-deleting-a-cell-goes-to-the-next-ones-start ()
  "Deleting a cell puts point at the start of the next, as Jupyter selects it."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id 1 :type 'code :source "gone = 1")
              (make-emjupy-cell :id 2 :type 'code :source "next = 2\nmore = 3"))
      buf nb
    (emjupy--goto-cell (aref (emjupy-notebook-cells nb) 0) 'beginning)
    (emjupy-delete-cell)
    (should (looking-at "next = 2"))))


;;;; Hiding a cell's code

(defmacro emjupy-test--hideable (&rest body)
  "Run BODY in three code cells, the second with output; CELLS bound."
  (declare (indent 0))
  `(emjupy-test--with-notebook
       (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1\nb = 2"
                                 :exec-count 1
                                 :metadata (make-hash-table :test 'equal))
               (make-emjupy-cell :id 2 :type 'code :source "c = 3"
                                 :exec-count 2
                                 :outputs (vector (let ((o (make-hash-table
                                                            :test 'equal)))
                                                    (puthash "output_type"
                                                             "stream" o)
                                                    (puthash "name" "stdout" o)
                                                    (puthash "text" "3\n" o)
                                                    o))
                                 :metadata (make-hash-table :test 'equal))
               (make-emjupy-cell :id 3 :type 'code :source "d = 4"
                                 :metadata (make-hash-table :test 'equal)))
       buf nb
     (let ((cells (emjupy-notebook-cells nb)))
       ,@body)))

(defun emjupy-test--hidden-line (cell)
  "The overlay showing CELL's hidden code, or nil."
  (let ((ov (emjupy-cell-overlay cell)))
    (cl-find-if (lambda (o) (overlay-get o 'emjupy-hidden-code))
                (overlays-in (overlay-start ov) (overlay-end ov)))))

(ert-deftest emjupy-test-hiding-a-cells-code-and-its-output ()
  "Hidden, a cell is one line in its code's place, marked as Jupyter does;
its output goes with it; point leaves it."
  (emjupy-test--hideable
    (let ((cell (aref cells 1)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-toggle-cell-code)
      (should (emjupy--cell-source-hidden-p cell))
      (should (eq (gethash "source_hidden"
                           (gethash "jupyter" (emjupy-cell-metadata cell)))
                  t))
      (let ((line (emjupy-test--hidden-line cell)))
        (should line)
        (should (string-match-p "▸ \\[In: 2\\] python, 1 line of code hidden, and its output"
                                (overlay-get line 'display))))
      (should-not (overlayp (emjupy-cell-output-ov cell)))
      ;; on its line still: pressed again, it shows
      (should (eq (emjupy--cell-at-point) cell)))))

(ert-deftest emjupy-test-the-code-key-toggles ()
  "C-c C-i on a hidden cell's line shows it, as C-c C-o does output."
  (emjupy-test--hideable
    (let ((cell (aref cells 1)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-toggle-cell-code)
      (should (emjupy--cell-source-hidden-p cell))
      (emjupy-toggle-cell-code)
      (should-not (emjupy--cell-source-hidden-p cell))
      (should-not (emjupy-test--hidden-line cell))
      (should (overlayp (emjupy-cell-output-ov cell))))))

(ert-deftest emjupy-test-the-code-key-on-a-region ()
  "Over a region, every code cell it touches is hidden; all hidden, shown.
Some hidden and some not, all are hidden: not each one turned over."
  (emjupy-test--hideable
    (let ((transient-mark-mode t))
      (cl-flet ((region-over (from to)
                  (emjupy--goto-cell (aref cells from) 'beginning)
                  (push-mark (point) t t)
                  (emjupy--goto-cell (aref cells to) 'end)))
        (region-over 0 1)
        (emjupy-toggle-cell-code)
        (should (emjupy--cell-source-hidden-p (aref cells 0)))
        (should (emjupy--cell-source-hidden-p (aref cells 1)))
        (should-not (emjupy--cell-source-hidden-p (aref cells 2)))
        (should-not (use-region-p))
        (region-over 0 1)
        (emjupy-toggle-cell-code)
        (should-not (emjupy--cell-source-hidden-p (aref cells 0)))
        (should-not (emjupy--cell-source-hidden-p (aref cells 1)))
        (emjupy--set-cell-source-hidden (aref cells 1) t)
        (emjupy--redraw-notebook)
        (region-over 0 1)
        (emjupy-toggle-cell-code)
        (should (emjupy--cell-source-hidden-p (aref cells 0)))
        (should (emjupy--cell-source-hidden-p (aref cells 1)))))))

(ert-deftest emjupy-test-a-click-shows-hidden-code ()
  "A click on the marker shows the code, and its output, again."
  (emjupy-test--hideable
    (let ((cell (aref cells 1)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-toggle-cell-code)
      (let ((shown (overlay-get (emjupy-test--hidden-line cell) 'display)))
        (emjupy--show-code-from-string (cons shown (string-match "▸" shown))
                                       (current-buffer)))
      (should-not (emjupy--cell-source-hidden-p cell))
      (should-not (emjupy-test--hidden-line cell))
      (should (overlayp (emjupy-cell-output-ov cell))))))

(ert-deftest emjupy-test-hidden-code-is-not-typed-into ()
  "Typing into hidden code is refused; emjupy's own redrawing is not."
  (emjupy-test--hideable
    (let ((cell (aref cells 0)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-toggle-cell-code)
      (goto-char (overlay-start (emjupy-test--hidden-line cell)))
      (should-error (insert "x") :type 'user-error)
      (emjupy--redraw-notebook)
      (should (emjupy-test--hidden-line cell)))))

(ert-deftest emjupy-test-hidden-cells-in-a-row-leave-no-empty-line ()
  "Between two hidden cells, nothing to draw: the first's newline is none."
  (emjupy-test--hideable
    (dolist (i '(0 1))
      (emjupy--set-cell-source-hidden (aref cells i) t))
    (emjupy--redraw-notebook)
    (let* ((ov (emjupy-cell-overlay (aref cells 0)))
           (newline (1- (overlay-end ov))))
      (should (cl-some (lambda (o) (equal (overlay-get o 'display) ""))
                       (overlays-in newline (1+ newline)))))))

(ert-deftest emjupy-test-a-hidden-cells-line-follows-its-count ()
  "Run again, a hidden cell's line shows its new count."
  (emjupy-test--hideable
    (let ((cell (aref cells 0)))
      (emjupy--goto-cell cell 'beginning)
      (emjupy-toggle-cell-code)
      (setf (emjupy-cell-exec-count cell) 9)
      (emjupy--redraw-cell-header cell)
      (should (string-match-p "\\[In: 9\\]"
                              (overlay-get (emjupy-test--hidden-line cell)
                                           'display))))))


(ert-deftest emjupy-test-the-output-key-on-a-region ()
  "Over a region, the output of every cell it touches is hidden, or, all
hidden, shown; some hidden, all are.  A cell without output, or whose
code is hidden, is left alone; a region over output alone counts."
  (cl-flet ((stream (text) (let ((o (make-hash-table :test 'equal)))
                             (puthash "output_type" "stream" o)
                             (puthash "name" "stdout" o)
                             (puthash "text" text o)
                             o)))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1"
                                  :outputs (vector (stream "1\n"))
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 2 :type 'code :source "b = 2"
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 3 :type 'code :source "c = 3"
                                  :outputs (vector (stream "3\n"))
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 4 :type 'code :source "d = 4"
                                  :outputs (vector (stream "4\n"))
                                  :metadata (make-hash-table :test 'equal)))
        buf nb
      (let ((cells (emjupy-notebook-cells nb))
            (transient-mark-mode t))
        (emjupy--set-cell-source-hidden (aref cells 3) t)
        (emjupy--redraw-notebook)
        (cl-flet ((region (from to)
                    (goto-char from) (push-mark (point) t t) (goto-char to))
                  (hidden (i) (emjupy--cell-outputs-hidden-p (aref cells i))))
          (region (point-min) (point-max))
          (emjupy-toggle-cell-output)
          (should (and (hidden 0) (hidden 2)))
          (should (emjupy--cell-source-hidden-p (aref cells 3)))
          (should-not (use-region-p))
          (region (point-min) (point-max))
          (emjupy-toggle-cell-output)
          (should-not (or (hidden 0) (hidden 2)))
          (emjupy--set-cell-outputs-hidden (aref cells 0) t)
          (emjupy--redraw-notebook)
          (region (point-min) (point-max))
          (emjupy-toggle-cell-output)
          (should (and (hidden 0) (hidden 2)))
          ;; a region over cell 3's output text alone
          (emjupy--set-cell-outputs-hidden (aref cells 2) nil)
          (emjupy--redraw-notebook)
          (let ((out (emjupy-cell-output-ov (aref cells 2))))
            (region (overlay-start out) (1- (overlay-end out))))
          (emjupy-toggle-cell-output)
          (should (hidden 2)))))))


(ert-deftest emjupy-test-a-region-over-cells-left-alone-keeps-them ()
  "A region over code, markdown, code: the markdown cell, not acted on,
is kept whole -- for C-c C-i, and for C-c C-o over a cell without output.
Redrawn together, the cells acted on drew over the one between."
  (cl-flet ((stream (text) (let ((o (make-hash-table :test 'equal)))
                             (puthash "output_type" "stream" o)
                             (puthash "name" "stdout" o)
                             (puthash "text" text o)
                             o)))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1"
                                  :outputs (vector (stream "1\n"))
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 2 :type 'markdown :source "# Between"
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 3 :type 'code :source "b = 2"
                                  :metadata (make-hash-table :test 'equal))
                (make-emjupy-cell :id 4 :type 'code :source "c = 3"
                                  :outputs (vector (stream "3\n"))
                                  :metadata (make-hash-table :test 'equal)))
        buf nb
      (let ((cells (emjupy-notebook-cells nb)) (transient-mark-mode t))
        (cl-flet ((whole ()
                    (emjupy--sync-all-cells)
                    (should-not (emjupy--check-invariants))
                    (should (equal (emjupy-cell-source (aref cells 1))
                                   "# Between"))))
          (dolist (command '(emjupy-toggle-cell-code
                             emjupy-toggle-cell-code
                             emjupy-toggle-cell-output
                             emjupy-toggle-cell-output))
            (goto-char (point-min))
            (push-mark (point) t t)
            (goto-char (point-max))
            (funcall command)
            (whole)))))))

(provide 'emjupy-commands-test)

;;; emjupy-commands-test.el ends here
