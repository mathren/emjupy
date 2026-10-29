;;; emjupy-cells-test.el --- Unit tests: emjupy-cells.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-cells.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Undo safety

(ert-deftest emjupy-test-undo-from-another-cell-does-not-scramble ()
  "Undo invoked with point in a different cell must never damage the
notebook -- such as deleting every cell after the one being edited."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let ((c1 (aref cells 0)) (c3 (aref cells 2)))
          (setq buffer-undo-list nil)
          ;; type in cell 1
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (insert "11")
          (undo-boundary)
          (emjupy--sync-all-cells)
          ;; something re-renders (output arriving on another cell)
          (let ((oh (make-hash-table :test 'equal)))
            (puthash "output_type" "stream" oh)
            (puthash "name" "stdout" oh)
            (puthash "text" "hi\n" oh)
            (setf (emjupy-cell-outputs c3) (vector oh)))
          (emjupy--redraw-notebook c3)
          ;; undo from cell 3
          (goto-char (overlay-start (emjupy-cell-overlay c3)))
          (ignore-errors (undo))
          (emjupy--sync-all-cells)
          ;; every cell survives, in order, with its own source
          (should (= (length (emjupy-notebook-cells nb)) 3))
          (should (equal (emjupy-cell-source (aref cells 1)) "bbb = 2"))
          (should (equal (emjupy-cell-source (aref cells 2)) "ccc = 3"))
          (should (string-match-p "bbb = 2" (buffer-string)))
          (should (string-match-p "ccc = 3" (buffer-string))))))))

(ert-deftest emjupy-test-undo-still-works-when-render-changed-nothing ()
  "A re-render that produces identical text must leave the undo history
intact -- otherwise every stray refresh would silently drop it."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let ((c1 (aref cells 0)) (c3 (aref cells 2)))
          (setq buffer-undo-list nil)
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (insert "11")
          (undo-boundary)
          (emjupy--sync-all-cells)
          (emjupy--redraw-notebook c3)   ; no textual change
          (goto-char (overlay-start (emjupy-cell-overlay c3)))
          (undo)
          (emjupy--sync-all-cells)
          (should (equal (emjupy-cell-source c1) "aaa = 1"))
          (should (= (length (emjupy-notebook-cells nb)) 3)))))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-clear-cell-output ()
  "`C-c C-l' discards this cell's output and its execution count, and
takes the output box with it -- leaving [In: 4] beside no output would
claim something had been run since."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "a"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "b"
                              :outputs [] :metadata (make-hash-table))))
    (dolist (pair (list (cons c1 "out-a\n") (cons c2 "out-b\n")))
      (let ((o (make-hash-table :test 'equal)))
        (puthash "output_type" "stream" o)
        (puthash "name" "stdout" o)
        (puthash "text" (cdr pair) o)
        (setf (emjupy-cell-outputs (car pair)) (vector o))
        (setf (emjupy-cell-exec-count (car pair)) 4)))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        (should (= (length (emjupy-cell-outputs c1)) 0))
        (should-not (emjupy-cell-exec-count c1))
        (should-not (emjupy-cell-output-ov c1))
        (should-not (string-match-p "out-a" (buffer-string)))
        ;; the other cell is untouched
        (should (= (length (emjupy-cell-outputs c2)) 1))
        (should (string-match-p "out-b" (buffer-string)))))))

(ert-deftest emjupy-test-clear-cell-output-with-nothing-to-clear ()
  "A cell with no output says so rather than silently re-rendering."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((msg nil))
          (cl-letf (((symbol-function 'message)
                     (lambda (f &rest args) (setq msg (apply #'format
                                                             f args)))))
            (let ((emjupy-confirm-clear-output nil))
              (emjupy-clear-cell-output)))
          (should (string-match-p "no output" msg)))))))

(ert-deftest emjupy-test-clear-cell-output-from-the-output-area ()
  "Clearing works with point in the OUTPUT box, not just in the source.

That is where you are when you look at a result and decide to be rid of
it, so requiring point to be up in the code would be the wrong way
round."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1"
                                :outputs [] :metadata (make-hash-table)))
        (o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o)
    (puthash "name" "stdout" o)
    (puthash "text" "RESULT\n" o)
    (setf (emjupy-cell-outputs cell) (vector o))
    (setf (emjupy-cell-exec-count cell) 4)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "RESULT" nil t))
        (goto-char (match-beginning 0))
        ;; point is in the output box, and the cell is still found
        (should (eq (emjupy--cell-at-point) cell))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        (should (= (length (emjupy-cell-outputs cell)) 0))
        (should-not (string-match-p "RESULT" (buffer-string)))))))

(ert-deftest emjupy-test-sync-refuses-when-overlays-are-out-of-step ()
  "Syncing must never write garbage into the cells.

It reads whatever an overlay spans straight into a cell's source, so one
stale or overlapping overlay is enough for a cell to swallow the whole
notebook -- and since the structs are what gets saved and re-rendered,
that corruption survives every redraw.  Refusing keeps the notebook."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (should (emjupy--overlays-sane-p))
        (should (emjupy--sync-all-cells))
        ;; let the first cell's overlay swallow everything
        (move-overlay (emjupy-cell-overlay c1) (point-min) (point-max))
        (should-not (emjupy--overlays-sane-p))
        (should-not (emjupy--sync-all-cells))
        ;; the cells are untouched, so a redraw restores the notebook
        (should (equal (emjupy-cell-source c1) "a = 1"))
        (should (equal (emjupy-cell-source c2) "b = 2"))))))

(ert-deftest emjupy-test-non-cell-regions-are-read-only ()
  "Text can only be typed into a cell's source.

The gutters, rules and output boxes belong to no cell, so anything typed
there is stored nowhere and vanishes at the next redraw -- better
refused than silently lost.  The boundaries matter as much as the
middle: typing at the end of a cell's last line must still work."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (cl-flet ((editable-p (pos)
                    (goto-char pos)
                    (condition-case nil
                        (progn (insert "X") (delete-char -1) t)
                      (error nil))))
          ;; inside a cell, and at the end of its last line
          (should (editable-p (+ 2 (overlay-start (emjupy-cell-overlay c1)))))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (should (editable-p (point)))
          ;; the start of the next cell is still reachable
          (should (editable-p (overlay-start (emjupy-cell-overlay c2))))
          ;; but the gutter between them is not
          (should-not (editable-p (overlay-end (emjupy-cell-overlay c1)))))
        ;; and ordinary editing still reaches the struct
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (end-of-line)
        (insert " + 9")
        (should (emjupy--sync-all-cells))
        (should (equal (emjupy-cell-source c1) "a = 1 + 9"))))))

(ert-deftest emjupy-test-tab-outside-a-code-cell ()
  "In a markdown cell TAB just inserts, rather than running Python
indentation over prose."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "# heading" :outputs []
                                :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-indent-or-cycle)
        (emjupy--sync-all-cells)
        (should (string-match-p "heading" (emjupy-cell-source cell)))))))

(ert-deftest emjupy-test-shadow-failure-does-not-break-the-notebook ()
  "Editing and running keep working when the language server cannot be set
up: the shadow buffer is a convenience, not the notebook."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; a shadow beside the notebook: the local route's
        (setq emjupy--lsp-route 'local)
        (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
        (let ((emjupy-shadow-directory "/ssh:nosuchhost.invalid:/tmp/x/"))
          (cl-letf (((symbol-function 'make-directory)
                     ;; a remote root misconfigured: TRAMP's user error
                     (emjupy-test--failing-make-directory
                      "nosuchhost" 'user-error "Method `sshh' is not known")))
            (should-not (emjupy--ensure-shadow-buffer nb))))
        ;; the notebook itself is untouched
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (end-of-line)
        (insert " + 1")
        (should (emjupy--sync-all-cells))
        (should (equal (emjupy-cell-source cell) "x = 1 + 1"))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        (should (= (length (emjupy-cell-outputs cell)) 0))))))

(ert-deftest emjupy-test-clearing-output-keeps-undo ()
  "Clearing a cell's output redraws that cell, not the notebook.

Nothing was ever lost to the full rebuild -- the text and the cells came
through it intact -- but the undo history did not, so an edit made
before clearing could not be undone afterwards.  The last operation that
behaved that way."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "a = 1"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "b = 2"
                               :outputs [] :metadata (make-hash-table)))
         (out (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" out)
    (puthash "name" "stdout" out)
    (puthash "text" "result\n" out)
    (setf (emjupy-cell-outputs c2) (vector out))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        ;; type into the first cell, then clear the second cell's output
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (let ((emjupy-confirm-clear-output nil))
          (emjupy-clear-cell-output))
        (should (= (length (emjupy-cell-outputs c2)) 0))
        (should-not (string-match-p "result" (buffer-string)))
        ;; the typing is there and can still be taken back
        (should (string-match-p "a = 1 \\+ 7" (buffer-string)))
        (emjupy-test--undo-past-steps "\\+ 7")
        (should-not (string-match-p "\\+ 7" (buffer-string)))
        (should (string-match-p "a = 1" (buffer-string)))))))

(ert-deftest emjupy-test-insert-keeps-overlays-consistent ()
  "Inserting a cell before another leaves both with their own region.

Text inserted at an overlay's start is taken into that overlay, since
overlays do not advance at the front.  The following cell therefore
swallowed the new one: two cells sharing a region, which shows as a
doubled boundary, fails the consistency check, and stops syncing -- so
the next edit there is not saved and a cell appears to vanish."
  (cl-flet ((mk (s) (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                      :source s
                                      :outputs [] :metadata (make-hash-table))))
    (let* ((c1 (mk "a = 1")) (c2 (mk "b = 2"))
           (nb (make-emjupy-notebook :cells (vector c1 c2) :path "n.ipynb"))
           (buf (generate-new-buffer "*insert*")))
      (unwind-protect
          (with-current-buffer buf
            (emjupy-mode)
            (setq emjupy--buffer-notebook nb)
            (setf (emjupy-notebook-buffer nb) buf)
            (emjupy--redraw-notebook)
            (should (emjupy--overlays-sane-p))
            ;; insert between the two
            (goto-char (overlay-start (emjupy-cell-overlay c1)))
            (emjupy-insert-cell-below)
            (should (= (length (emjupy-notebook-cells nb)) 3))
            (should (emjupy--overlays-sane-p))
            ;; and syncing still works, so edits are not silently dropped
            (should (emjupy--sync-all-cells))
            ;; exactly their own text: an overlay resumed one character early
            ;; leaves the separating newline inside the following cell, where
            ;; it shows as a stray blank line at the top of its source
            (should (equal (emjupy-cell-source c1) "a = 1"))
            (should (equal (emjupy-cell-source c2) "b = 2"))
            ;; the new cell carries no content of its own; not compared to "",
            ;; as an empty cell holds a newline in incremental and full renders
            ;; alike.
            (should (string-empty-p
                     (string-trim (emjupy-cell-source
                                   (aref (emjupy-notebook-cells nb) 1)))))
            ;; at the end of the notebook too
            (goto-char (point-max))
            (emjupy-insert-cell-below)
            (should (emjupy--overlays-sane-p)))
        (let ((kill-buffer-query-functions nil)) (kill-buffer buf))))))

;;;; Invariants

(ert-deftest emjupy-test-render-round-trip-preserves-everything ()
  "Rendering and reading back must change nothing about the cells.

The property the whole package rests on: what is drawn is what is held,
so what is saved is what was seen.  Checked over the kinds of cell that
differ in how they render -- empty, multiline, unicode, markdown, one
with output and an execution count, one carrying metadata nothing else
looks at."
  (let* ((meta (make-hash-table :test 'equal))
         (cells (progn
                  (puthash "scrolled" t meta)
                  (vector
                   (emjupy-test--cell-like 'code "a = 1")
                   (emjupy-test--cell-like 'code "")
                   (emjupy-test--cell-like 'code
                                           "def f(x):\n    return x + 1\n")
                   (emjupy-test--cell-like 'markdown "# Title\n\nsome *prose*")
                   (emjupy-test--cell-like 'code "s = \"héllo — ünicode\"")
                   (emjupy-test--cell-like
                    'code "print('hi')"
                    (vector (emjupy-test--stream-output "hi\n")) 7)
                   (emjupy-test--cell-like 'code "m = 1" nil nil meta))))
         (snapshot (lambda (list-of-cells)
                     (mapcar (lambda (c)
                               (list (emjupy-cell-type c)
                                     ;; trailing newlines are normalised on
                                     ;; the way in, so they are not part of
                                     ;; what must be preserved
                                     (string-trim-right
                                      (or (emjupy-cell-source c) "")
                                      "[\n]+")
                                     (length (emjupy-cell-outputs c))
                                     (emjupy-cell-exec-count c)
                                     (emjupy-cell-metadata c)))
                             list-of-cells)))
         (before (funcall snapshot (append cells nil))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (emjupy-test--assert-invariants "after render")
        ;; reading the buffer back must not alter anything
        (should (emjupy--sync-all-cells))
        (let ((after (funcall snapshot
                              (append (emjupy-notebook-cells nb) nil))))
          (should (= (length after) (length before)))
          (cl-loop for b in before
                   for a in after
                   for i from 0
                   do (should (equal (cons i a) (cons i b)))))
        ;; and again after a redraw, which is where drift would show: a
        ;; second cycle must change nothing at all, or repeated opening and
        ;; saving would keep rewriting the notebook
        (let ((once (funcall snapshot (append (emjupy-notebook-cells nb) nil))))
          (emjupy--redraw-notebook)
          (emjupy-test--assert-invariants "after redraw")
          (should (emjupy--sync-all-cells))
          (should (equal (funcall snapshot
                                  (append (emjupy-notebook-cells nb) nil))
                         once)))))))

;;;; Performance

(ert-deftest emjupy-test-reconnect-repairs-a-scrambled-buffer ()
  "Reconnecting redraws when the buffer and the cells disagree.

Suspending a machine kills a tunnelled socket, and one that dies during
a render leaves the buffer showing something the cells do not hold.  The
cells are the notebook and the buffer is a drawing of them, so redrawing
is free and correct -- which is why doing it by hand worked, and why the
reconnection should do it."
  (let ((cell (emjupy-test--cell-like 'code "a = 1")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; nothing wrong: nothing to report and nothing to redo
        (should-not (emjupy--repair-if-scrambled nb))
        ;; scramble it the way a half-finished render does
        (let ((inhibit-read-only t))
          (goto-char (overlay-start (emjupy-cell-overlay cell)))
          (insert "garbage "))
        (should (emjupy--check-invariants))
        (let ((problems (emjupy--repair-if-scrambled nb)))
          (should problems))
        ;; and afterwards buffer and cells agree again
        (should-not (emjupy--check-invariants))
        (should (string-match-p "a = 1" (buffer-string)))
        (should-not (string-match-p "garbage" (buffer-string)))))))

(ert-deftest emjupy-test-deleting-a-cell-can-be-undone ()
  "Undo brings a deleted cell back, in its place.

The deletion cannot be undone the ordinary way -- it is made with
recording off, because a redraw moves buffer positions that undo entries
hold literally -- so the way back is recorded as a function to call."
  (cl-flet ((mk (s) (emjupy-test--cell-like 'code s)))
    (let* ((c1 (mk "a = 1")) (c2 (mk "b = 2")) (c3 (mk "c = 3"))
           (cells (vector c1 c2 c3)))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (goto-char (overlay-start (emjupy-cell-overlay c2)))
          (emjupy-delete-cell)
          (should (= (length (emjupy-notebook-cells nb)) 2))
          (should-not (string-match-p "b = 2" (buffer-string)))
          (undo)
          ;; back, and in the middle where it was
          (should (= (length (emjupy-notebook-cells nb)) 3))
          (should (string-match-p "b = 2" (buffer-string)))
          (should (equal (mapcar #'emjupy-cell-source
                                 (append (emjupy-notebook-cells nb) nil))
                         '("a = 1" "b = 2" "c = 3")))
          (should-not (emjupy--check-invariants)))))))

;;;; The undo guard

(ert-deftest emjupy-test-clear-confirmation-can-be-turned-off ()
  "`emjupy-confirm-clear-output' nil clears without asking."
  (let ((cell (emjupy-test--cell-like
               'code "print(1)"
               (vector (emjupy-test--stream-output "out\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (let ((emjupy-confirm-clear-output nil)
              (asked nil))
          (cl-letf (((symbol-function 'y-or-n-p)
                     (lambda (&rest _) (setq asked t) t)))
            (emjupy-clear-cell-output))
          (should-not asked)
          (should (= (length (emjupy-cell-outputs cell)) 0)))))))

(ert-deftest emjupy-test-inserting-above-keeps-the-edit ()
  "Adding a cell above keeps an edit made elsewhere undoable.

Inserting moves everything below it by a known amount, so the entries
survive; this pins that down for the cell-above direction, which the
matrix covers only downward."
  (let ((cells (vector (emjupy-test--cell-like 'code "first = 1")
                       (emjupy-test--cell-like 'code "second = 2"))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 1))))
        (emjupy-insert-cell-above)
        (should (= (length (emjupy-notebook-cells nb)) 3))
        (goto-char (point-min))
        (setq this-command nil last-command nil)
        (let ((presses 0) (recovered nil))
          (while (and (< presses 4) (not recovered))
            (setq presses (1+ presses))
            (ignore-errors (undo))
            (setq this-command 'undo last-command 'undo)
            (unless (string-match-p "\\+ 7" (buffer-string))
              (setq recovered presses)))
          (setq this-command nil last-command nil)
          (should recovered))
        (emjupy--sync-all-cells)
        (should-not (emjupy--check-invariants))))))

(ert-deftest emjupy-test-shadow-leaves-nothing-beside-the-notebook ()
  "The shadow file is the only thing written beside the notebook.

It is a generated copy of the cells: no lock file, no auto-save file.
Setting its coding system marks the buffer modified, and Emacs locks on
the first modification: done before locking is off, it would leave a
`.#' file, and in a graphical session an auto-save `#...#' one, in the
user's project directory."
  (let* ((dir (make-temp-file "emjupy-shadow-dir" t))
         (emjupy-lsp-enabled nil)
         (c (emjupy-test--cell-like 'code "x = 1")))
    (unwind-protect
        (emjupy-test--with-notebook (vector c) buf nb
          (with-current-buffer buf
            (setf (emjupy-notebook-kernel-cwd nb) dir
                  (emjupy-notebook-path nb) "nb.ipynb")
            (cl-letf (((symbol-function 'eglot-ensure) #'ignore))
              (let ((sb (emjupy--ensure-shadow-buffer nb)))
                (goto-char (overlay-start (emjupy-cell-overlay c)))
                (end-of-line)
                (insert " + 2")
                (emjupy--sync-all-cells)
                (with-current-buffer sb
                  (should-not create-lockfiles)
                  (should-not buffer-auto-save-file-name)
                  (insert "# edited\n"))
                ;; nothing but the shadow file itself
                (should (equal (directory-files dir nil "\\`[^.]\\|\\`\\.[^.]")
                               (list (file-name-nondirectory (buffer-file-name sb)))))
                (with-current-buffer sb (set-buffer-modified-p nil))
                (kill-buffer sb)))))
      (delete-directory dir t))))

(ert-deftest emjupy-test-inserting-a-cell-is-undone-and-redone ()
  "Undo takes back an inserted cell, and redo puts it back.

Drawn with undo recording off, the insertion would record nothing, and
undo would take back the edit before, leaving the new cell in place."
  (dolist (cmd (list #'emjupy-insert-cell-below #'emjupy-insert-cell-above))
    (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                         (emjupy-test--cell-like 'code "b = 2"))))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (goto-char (overlay-start (emjupy-cell-overlay (aref cells 0))))
          (funcall cmd)
          (undo-boundary)
          (should (= (length (emjupy-notebook-cells nb)) 3))
          (let ((last-command nil)) (undo))
          (should (= (length (emjupy-notebook-cells nb)) 2))
          (should-not (emjupy--check-invariants))
          ;; redo: undo the undo
          (undo-boundary)
          (let ((last-command nil)) (undo))
          (should (= (length (emjupy-notebook-cells nb)) 3))
          (should-not (emjupy--check-invariants)))))))

(ert-deftest emjupy-test-redraw-in-a-window-not-selected-keeps-its-place ()
  "A notebook redrawn while shown in a window other than the selected one stays whole.
Measuring that window for the line numbers selects it, moving its
buffer's point to the window's: midway through a redraw a stale position
near the top, where every later cell would be drawn on top of the one
before, the notebook backwards, cells overlapping and text lost.
Opening and closing other buffers leaves a notebook in a window not
selected."
  (let ((cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                         :type 'markdown
                                         :source "# Title\n\nSome prose."
                                         :outputs []
                                         :metadata (make-hash-table))
                       (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "a = 1"
                                         :outputs []
                                         :metadata (make-hash-table))
                       (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "b = 2"
                                         :outputs []
                                         :metadata (make-hash-table)))))
    (emjupy-test--with-notebook cells buf nb
      (save-window-excursion
        (delete-other-windows)
        (let ((shown (split-window))
              (other (get-buffer-create "*emjupy-test-other*")))
          (unwind-protect
              (progn
                (set-window-buffer shown buf)
                (set-window-buffer (selected-window) other)
                (with-current-buffer buf
                  (display-line-numbers-mode 1)
                  (set-window-point shown 2)
                  (emjupy--redraw-notebook)
                  (should-not (emjupy--check-invariants))
                  (should (string-prefix-p "# Title" (buffer-string)))
                  (should (< (string-match "a = 1" (buffer-string))
                             (string-match "b = 2" (buffer-string))))))
            (kill-buffer other)))))))

(ert-deftest emjupy-test-an-output-redraw-that-fails-is-put-right ()
  "Output drawn partway when an error strikes leaves no scrambled buffer.
The error still reaches the user; the buffer is redrawn from the cells
at once; and what was typed elsewhere, not yet synced, is kept."
  (let ((cells (emjupy-test--two-cells)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (emjupy-cells-enable)
        ;; typed in the second cell, not synced
        (goto-char (overlay-start (emjupy-cell-overlay (aref cells 1))))
        (end-of-line)
        (let ((inhibit-read-only t)) (insert " + 1"))
        ;; output changed so that it must be drawn whole: a different one
        (puthash "text" "goodbye\n"
                 (aref (emjupy-cell-outputs (aref cells 0)) 0))
        (let ((undo (emjupy-test--fail-once 'emjupy--draw-cell-output
                                            "half an outp")))
          (unwind-protect
              (should-error (emjupy--redraw-cell-output (aref cells 0)))
            (funcall undo)))
        (should-not (emjupy--check-invariants))
        (should (equal (emjupy-cell-source (aref cells 1)) "y = 2 + 1"))
        (should (string-match-p "goodbye" (buffer-string)))
        (should-not (string-match-p "half an outp" (buffer-string)))))))

(ert-deftest emjupy-test-a-structural-redraw-that-fails-is-put-right ()
  "A cell inserted partway when an error strikes leaves no scrambled buffer."
  (let ((cells (emjupy-test--two-cells)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (emjupy-cells-enable)
        (let ((new (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                     :source "z = 3"
                                     :outputs [] :metadata (make-hash-table)))
              (undo (emjupy-test--fail-once 'emjupy--draw-cell "z =")))
          (unwind-protect
              (should-error (emjupy-insert-cell-at nb 1 new))
            (funcall undo)))
        (should-not (emjupy--check-invariants))))))

(ert-deftest emjupy-test-a-broken-structure-is-put-right-but-typing-is-not-undone ()
  "The check after redraws repairs a broken structure, never plain typing.
Text typed and not yet synced differs from the cells; redrawing on that
would undo the typing, so only the structure is checked."
  (let ((cells (emjupy-test--two-cells)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        ;; typing alone: nothing redrawn, the typing still there
        (goto-char (overlay-start (emjupy-cell-overlay (aref cells 1))))
        (end-of-line)
        (let ((inhibit-read-only t)) (insert " + 1"))
        (let ((text (buffer-string)))
          (emjupy--check-integrity buf)
          (should (equal (buffer-string) text)))
        ;; a broken structure: the second cell's overlay over the first
        (let ((ov (emjupy-cell-overlay (aref cells 1))))
          (move-overlay ov (point-min) (overlay-end ov)))
        (should (emjupy--check-invariants :structure-only t))
        (emjupy--check-integrity buf)
        (should-not (emjupy--check-invariants :structure-only t))))))


(ert-deftest emjupy-test-a-new-cell-typed-in-has-no-trailing-newline ()
  "An empty cell is drawn with a line to type on; typed on, its code does
not keep that line's newline.  It did: every new cell, typed in, ended in
a newline its author never typed."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                        :source "a = 1"))
      buf nb
    (cl-flet ((new-cell-with (text)
                (emjupy--goto-cell (aref (emjupy-notebook-cells nb) 0)
                                   'beginning)
                (emjupy-insert-cell-below)
                (insert text)
                (emjupy--sync-all-cells)
                (emjupy-cell-source (aref (emjupy-notebook-cells nb) 1))))
      (should (equal (new-cell-with "q = 1") "q = 1"))
      (should (equal (new-cell-with "x = 1\ny = 2") "x = 1\ny = 2"))
      (should (equal (new-cell-with "") "")))))

(provide 'emjupy-cells-test)

;;; emjupy-cells-test.el ends here
