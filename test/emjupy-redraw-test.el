;;; emjupy-redraw-test.el --- Unit tests: emjupy-redraw.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-redraw.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Output background band

(ert-deftest emjupy-test-output-band-appears-and-disappears-with-output ()
  "Output gets a background band; clearing the outputs shrinks the box
and takes the band with it."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table)))
        (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "hello\n" oh)
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; no output yet: no band
        (should-not (emjupy-cell-output-ov cell))
        ;; output arrives
        (setf (emjupy-cell-outputs cell) (vector oh))
        (emjupy--rerender-notebook)
        (should (emjupy-cell-output-ov cell))
        ;; the band is painted on the output text itself now, not on the
        ;; overlay, so several kinds of output can be coloured differently
        ;; within one box
        (goto-char (point-min))
        (should (search-forward "hello" nil t))
        (should (memq 'emjupy-output
                      (get-text-property (match-beginning 0) 'face)))
        ;; and the band covers the output text, not the source
        (let ((ov (emjupy-cell-output-ov cell)))
          (should (string-match-p "hello"
                                  (buffer-substring-no-properties
                                   (overlay-start ov) (overlay-end ov))))
          (should-not (string-match-p "x = 1"
                                      (buffer-substring-no-properties
                                       (overlay-start ov) (overlay-end ov)))))
        ;; outputs cleared: box shrinks, band goes with it
        (setf (emjupy-cell-outputs cell) [])
        (emjupy--rerender-notebook)
        (should-not (emjupy-cell-output-ov cell))
        (should-not (string-match-p "hello" (buffer-string)))))))

;;;; Undo safety

(ert-deftest emjupy-test-rerender-is-not-recorded-in-undo ()
  "Re-rendering erases and rebuilds the whole buffer.  Recorded as an
undoable change, it would push ~20 entries, undoing one of which tears
the notebook apart."
  (let ((cells (emjupy-test--three-cell-notebook)))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (setq buffer-undo-list nil)
        (emjupy--rerender-notebook)
        (should (null (delq nil buffer-undo-list)))))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-re-render-command ()
  "`emjupy-re-render' rebuilds the display and keeps pending edits."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (end-of-line)
        (insert " + 2")
        (emjupy-re-render)
        (should (equal (emjupy-cell-source cell) "a = 1 + 2"))
        (should (string-match-p (regexp-quote "a = 1 + 2") (buffer-string)))))))

(ert-deftest emjupy-test-protection-can-be-turned-off ()
  "`emjupy-protect-non-cell-regions' nil leaves the buffer fully editable."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "a = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((emjupy-protect-non-cell-regions nil))
          (emjupy--rerender-notebook)
          (goto-char (overlay-end (emjupy-cell-overlay cell)))
          (should (condition-case nil (progn (insert "X") t) (error nil))))))))

(ert-deftest emjupy-test-no-stray-overlays-after-reparse ()
  "Overlays whose cell has gone must be deleted, not left at position 1.

Re-rendering deleted the overlays reachable from the current cells, so
an overlay belonging to a cell that had been removed -- deleted, merged,
or dropped when the notebook was re-parsed into fresh structs -- was
never deleted.  `erase-buffer' collapses it to position 1, where it goes
on displaying its before-string and after-string: a stack of stray
\"[In: 8]\" and \"[Out: 15]\" rules piling up above the first cell."
  (cl-flet ((mk (s) (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                      :source s
                                      :outputs [] :metadata (make-hash-table))))
    (let ((nb (make-emjupy-notebook :cells (vector (mk "a = 1") (mk "b = 2"))))
          (buf (generate-new-buffer "*stray*")))
      (unwind-protect
          (with-current-buffer buf
            (emjupy-mode)
            (setq emjupy--buffer-notebook nb)
            (setf (emjupy-notebook-buffer nb) buf)
            (emjupy--rerender-notebook)
            (cl-flet ((emjupy-overlays ()
                        (seq-filter (lambda (o)
                                      (memq (overlay-get o 'emjupy-overlay)
                                            '(cell output)))
                                    (overlays-in (point-min) (point-max)))))
              (should (= (length (emjupy-overlays)) 2))
              ;; re-parse three times: fresh structs, old ones abandoned
              (dotimes (_ 3)
                (setf (emjupy-notebook-cells nb) (vector (mk "a = 1")
                                                         (mk "b = 2")))
                (emjupy--rerender-notebook))
              (should (= (length (emjupy-overlays)) 2))
              ;; nothing collapsed at the top
              (should-not (cl-some (lambda (o) (= (overlay-start o)
                                                  (overlay-end o)))
                                   (emjupy-overlays)))))
        (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
  (emjupy-test--companions-all-owned))

(ert-deftest emjupy-test-repad-is-per-line-not-per-character ()
  "Re-aligning the output band walks pad markers, not characters.

There is one pad per output LINE and there can be tens of thousands of
characters -- an inline figure is a single output of some 18,000 -- so
stepping character by character made a window resize proportional to the
SIZE of the output instead of the number of lines in it."
  (cl-flet ((mk (n) (let ((o (make-hash-table :test 'equal)))
                      (puthash "output_type" "stream" o)
                      (puthash "name" "stdout" o)
                      (puthash "text" (make-string n ?x) o)
                      o)))
    (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                  :source "run()"
                                  :outputs (vector (mk 20000))
                                  :metadata (make-hash-table)))
          (looks 0))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (cl-letf* ((real (symbol-function 'get-text-property))
                     ((symbol-function 'get-text-property)
                      (lambda (&rest args)
                        (when (eq (nth 1 args) 'emjupy-pad) (setq looks
                                                                  (1+ looks)))
                        (apply real args))))
            (let ((inhibit-read-only t))
              (emjupy--repad-output cell)))
          ;; One look per drawn LINE -- some hundreds for twenty thousand
          ;; characters, as wide lines are broken at the box -- not one per
          ;; character.
          (let* ((ov (emjupy-cell-output-ov cell))
                 (lines (count-lines (overlay-start ov) (overlay-end ov))))
            ;; two looks a line -- at the pad, and at the text before it --
            ;; and one for the end
            (should (<= looks (+ 1 (* 2 lines))))
            (should (< looks 2000))))))))

(ert-deftest emjupy-test-stray-rules-are-swept ()
  "Any emjupy overlay no live cell owns is deleted on a redraw.

A cell overlay draws its rule through a `before-string', which shows
even when the overlay has collapsed to zero width -- so one left behind
appears as a stray box border, and several stack up at the top where
`erase-buffer' collapsed them.  The render already deletes the overlays
it knows about; this catches anything it does not, which is what makes
the symptom unrepresentable rather than merely unlikely."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (cl-flet ((strays ()
                    (seq-filter (lambda (o)
                                  (and (memq (overlay-get o 'emjupy-overlay)
                                             '(cell output))
                                       (= (overlay-start o) (overlay-end o))))
                                (overlays-in (point-min) (point-max))))
                  (tagged ()
                    (seq-filter (lambda (o)
                                  (memq (overlay-get o 'emjupy-overlay)
                                        '(cell output)))
                                (overlays-in (point-min) (point-max)))))
          ;; forge the symptom: collapsed overlays at the top, each drawing
          ;; a rule, exactly as an abandoned cell would leave behind
          (dotimes (_ 4)
            (let ((ov (make-overlay 1 1)))
              (overlay-put ov 'emjupy-overlay 'cell)
              (overlay-put ov 'before-string (emjupy--rule "[In: 8] python"
                                                           "┌"))))
          (should (= (length (strays)) 4))
          ;; anything that redraws clears them
          (emjupy--rerender-notebook)
          (should (= (length (strays)) 0))
          ;; and the real cells are untouched
          (should (= (length (tagged)) 2))
          (should (overlayp (emjupy-cell-overlay c1)))
          (should (overlayp (emjupy-cell-overlay c2)))
          (should (string-match-p "a = 1" (buffer-string)))
          (should (string-match-p "b = 2" (buffer-string)))))))
  (emjupy-test--companions-all-owned))

(ert-deftest emjupy-test-sweep-keeps-output-overlays ()
  "The sweep must not take the output boxes with it: they are claimed by
their cells just as the source overlays are."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "run()"
                                :outputs [] :metadata (make-hash-table)))
        (o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o)
    (puthash "name" "stdout" o)
    (puthash "text" "result\n" o)
    (setf (emjupy-cell-outputs cell) (vector o))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should (emjupy-cell-output-ov cell))
        (emjupy--sweep-stray-overlays)
        (should (overlayp (emjupy-cell-output-ov cell)))
        (should (overlay-buffer (emjupy-cell-output-ov cell)))
        (should (string-match-p "result" (buffer-string)))))))

;;;; Undo matrix

(ert-deftest emjupy-test-rendering-is-not-an-undo-step ()
  "Redrawing never appears in the undo history as something to take back.

A redraw erases the buffer and rebuilds it.  Recorded, one such entry
deleted every cell after the one being edited, which is why rendering is
kept out of the history entirely."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'code "b = 2"))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        ;; a redraw on its own records nothing
        (emjupy--rerender-notebook)
        (should (null buffer-undo-list))
        ;; nor does re-aligning the outlines
        (let ((emjupy-box-width 60))
          (emjupy--refresh-box-rules t))
        (should (null buffer-undo-list))
        ;; a real edit does
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 0))))
        (end-of-line)
        (insert "!")
        (should buffer-undo-list)))))

;;;; Performance

(ert-deftest emjupy-test-output-colour-survives-fontification ()
  "Nothing in an output box loses its colour to a fontification pass.

font-lock owns the `face' property and removes it from every region it
refontifies.  Redrawing a cell marks its region for refontification, so
the next pass stripped the lot -- which is why the output background
vanished a moment after a cell was redrawn and never came back.

Checked over a box holding three kinds of output at once, plain output,
a warning, and an error with ANSI escapes, because each is coloured by
different code and fixing one of them is not fixing the others."
  (let* ((err (make-hash-table :test 'equal))
         (cell nil))
    (puthash "output_type" "error" err)
    (puthash "ename" "NameError" err)
    (puthash "evalue" "name 'error' is not defined" err)
    (puthash "traceback"
             (vector "\e[0;31mNameError\e[0m  Traceback (most recent call last)"
                     "Cell In[58], line 3"
                     "----> 3 print(error)"
                     "\e[0;31mNameError\e[0m: name 'error' is not defined")
             err)
    (setq cell (emjupy-test--cell-like
                'code "print(\"output line\")"
                (vector (emjupy-test--stream-output "output line\n")
                        (emjupy-test--stream-output "RuntimeWarning: divide by zero\n")
                        err)))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-flet ((uncoloured ()
                    ;; characters showing no face at all, newlines aside --
                    ;; those are left bare deliberately, so the background
                    ;; stops at the rule instead of painting past it
                    (let ((ov (emjupy-cell-output-ov cell)) (bare 0))
                      (save-excursion
                        (goto-char (overlay-start ov))
                        (while (< (point) (overlay-end ov))
                          (unless (eq (char-after) ?\n)
                            (unless (or (get-text-property (point) 'face)
                                        (get-text-property (point)
                                                           'font-lock-face))
                              (setq bare (1+ bare))))
                          (forward-char 1)))
                      bare)))
          (should (= (uncoloured) 0))
          ;; after a redraw, which is what marks the region for font-lock
          (emjupy--rerender-notebook)
          (should (= (uncoloured) 0))
          ;; and after font-lock has actually been over it
          (font-lock-fontify-region (point-min) (point-max))
          (should (= (uncoloured) 0)))))))

(ert-deftest emjupy-test-redraw-leaves-the-view-where-it-was ()
  "A redraw does not drag the view away from what is being read.

Redrawing erases the buffer and rebuilds it, which sent point to the end
and scrolled the window after it -- so a notebook could not be read
while it ran.  The line under point and the line at the top of the
window are what must not change; their buffer positions may, since
output boxes above grow and shrink."
  (let ((cells (vconcat (cl-loop for i from 0 below 40
                                 collect (emjupy-test--cell-like
                                          'code (format "line_%d = %d" i i))))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        ;; the buffer has to be on screen for this to mean anything
        (set-window-buffer (selected-window) buf)
        (let ((w (selected-window)))
          (cl-flet ((line-at (pos)
                      (save-excursion
                        (goto-char pos)
                        (buffer-substring-no-properties
                         (line-beginning-position) (line-end-position)))))
            (goto-char (overlay-start
                        (emjupy-cell-overlay
                         (aref (emjupy-notebook-cells nb) 25))))
            (recenter 0)
            (redisplay t)
            (let ((top (line-at (emjupy--visible-from (window-start w))))
                  (here (line-at (point))))
              (emjupy--rerender-notebook)
              (redisplay t)
              (should (equal (line-at (emjupy--visible-from (window-start w)))
                             top))
              (should (equal (line-at (point)) here)))))))))

(ert-deftest emjupy-test-redraw-with-a-target-keeps-the-scroll ()
  "A redraw aimed at a cell moves point, not the window.

Cycling a cell's type and re-indenting both redraw with the cell they
are working on as the target, and both threw the window to the bottom of
the buffer -- which is what made them unusable rather than merely
untidy."
  (let ((cells (vconcat (cl-loop for i from 0 below 40
                                 collect (emjupy-test--cell-like
                                          'code (format "line_%d = %d" i i))))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (set-window-buffer (selected-window) buf)
        (let* ((w (selected-window))
               (target (aref (emjupy-notebook-cells nb) 25)))
          (goto-char (overlay-start (emjupy-cell-overlay target)))
          (recenter 0)
          (redisplay t)
          (let ((top (line-number-at-pos
                      (emjupy--visible-from (window-start w)))))
            (emjupy-cycle-cell-type)
            (redisplay t)
            ;; compared by line number: the line's text may change, its
            ;; place in the buffer should not
            (should (= (line-number-at-pos
                        (emjupy--visible-from (window-start w))) top))))))))

;;;; The undo guard

(ert-deftest emjupy-test-undoing-a-rebuild-does-not-move-the-view ()
  "Undoing a redraw leaves the reader where they were.

The restore redraws the notebook, and a redraw leaves point at the end
of the buffer unless held -- so taking something back threw the view to
the bottom, at the moment someone is least expecting to be moved."
  (let ((cells (vconcat (cl-loop for i from 0 below 30
                                 collect (emjupy-test--cell-like
                                          'code (format "line_%d = %d" i i))))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (set-window-buffer (selected-window) buf)
        (let* ((w (selected-window))
               (target (aref (emjupy-notebook-cells nb) 20)))
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (goto-char (overlay-start (emjupy-cell-overlay target)))
          (end-of-line)
          (insert " + 7")
          (undo-boundary)
          (recenter 0)
          (redisplay t)
          (let ((top (line-number-at-pos (window-start w))))
            ;; a rebuild, then undo it
            (emjupy--rerender-notebook)
            (setq this-command nil last-command nil)
            (ignore-errors (undo))
            (redisplay t)
            (should (= (line-number-at-pos (window-start w)) top))
            (should-not (eobp))))))))

(ert-deftest emjupy-test-appended-stream-output-is-drawn-as-a-redraw-would ()
  "Output added to a stream that only grew is drawn exactly as drawing it all would.
Drawing the whole box for each update cost as much as the output was
long, so a long run grew slower with every line.  Now only what is new is
drawn -- which must be the same text, folded, painted and protected the
same way, or the notebook would drift from its cells."
  (dolist (name '("stdout" "stderr"))
    (let* ((out (emjupy-test--stream-named name "first line\n"))
           (cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                            :type 'code :source "run()"
                                            :outputs (vector out)
                                            :metadata (make-hash-table))
                          (make-emjupy-cell :id (emjupy--new-cell-id)
                                            :type 'code :source "x = 1"
                                            :outputs []
                                            :metadata (make-hash-table))))
           (appended 0))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (cl-letf* ((real (symbol-function 'emjupy--append-stream-output))
                     ((symbol-function 'emjupy--append-stream-output)
                      (lambda (c) (let ((r (funcall real c)))
                                    (when r (cl-incf appended)) r))))
            (dolist (more (list "second line\n" (concat (make-string 300 ?w)
                                                        "\n")
                                "third\nfourth\n"))
              (puthash "text" (concat (gethash "text" out) more) out)
              (emjupy--refresh-cell-output (aref cells 0)))
            ;; finished: nothing new, only the label
            (setf (emjupy-cell-exec-count (aref cells 0)) 7)
            (emjupy--refresh-cell-output (aref cells 0)))
          (ert-info ((format "%s: added to %d times" name appended))
            (should (= appended 4))
            (should (string-match-p "Out: 7"
                                    (emjupy--overlay-header
                                     (emjupy-cell-output-ov (aref cells 0)))))
            (let ((shown (emjupy-test--drawn)))
              (emjupy--rerender-notebook)
              (should (equal shown (emjupy-test--drawn))))))))))

(ert-deftest emjupy-test-appending-declines-what-could-change-earlier-lines ()
  "A carriage return, a colour escape, or an unfinished line is drawn whole.
Each can change how what came before looks; the result is still right."
  (dolist (case '(("done\n" . "\rdone\n") ("done\n" . "\e[31mred\e[0m\n")
                  ("half" . " done\n")))
    (let* ((out (emjupy-test--stream-named "stdout" (car case)))
           (cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                            :type 'code :source "run()"
                                            :outputs (vector out)
                                            :metadata (make-hash-table))))
           (appended nil))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (cl-letf* ((real (symbol-function 'emjupy--append-stream-output))
                     ((symbol-function 'emjupy--append-stream-output)
                      (lambda (c) (let ((r (funcall real c)))
                                    (when r (setq appended t)) r))))
            (puthash "text" (concat (car case) (cdr case)) out)
            (emjupy--refresh-cell-output (aref cells 0)))
          (ert-info ((format "%S" case))
            (should-not appended)
            (let ((shown (emjupy-test--drawn)))
              (emjupy--rerender-notebook)
              (should (equal shown (emjupy-test--drawn))))))))))

(ert-deftest emjupy-test-a-redraw-measures-the-window-once ()
  "A full redraw measures the window once, not for every rule it draws.
Measuring a window selects it; a redraw did it six times a cell."
  (let ((cells (emjupy-test--big-notebook 20 3)) (measures 0))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (cl-letf* ((real (symbol-function 'window-max-chars-per-line))
                   ((symbol-function 'window-max-chars-per-line)
                    (lambda (&rest args) (cl-incf measures) (apply real args))))
          (emjupy--rerender-notebook))
        ;; once for the redraw: a window showing it, or the selected one
        (should (<= measures 1))))))

(provide 'emjupy-redraw-test)

;;; emjupy-redraw-test.el ends here
