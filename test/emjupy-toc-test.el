;;; emjupy-toc-test.el --- Tests of emjupy-toc.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; The outline: the headings found, the imenu index, and the panel --
;; opened and closed, its buttons, following edits and point.

;;; Code:

(require 'ert)
(require 'emjupy-test-helpers)
(require 'emjupy-toc)

(defmacro emjupy-test--outlined (&rest body)
  "Run BODY in a notebook with headings, a code cell's comment, a fence."
  (declare (indent 0))
  `(emjupy-test--with-notebook
       (vector (make-emjupy-cell :id 1 :type 'markdown
                                 :source "# Title\nintro\n## Sub\n```\n# not one\n```")
               (make-emjupy-cell :id 2 :type 'code :source "# a comment\nx = 1")
               (make-emjupy-cell :id 3 :type 'markdown :source "### Deep"))
       buf nb
     (unwind-protect (progn ,@body)
       (let ((panel (get-buffer (emjupy--toc-buffer-name buf))))
         (when panel
           (when (get-buffer-window panel) (delete-window (get-buffer-window panel)))
           (kill-buffer panel))))))

(ert-deftest emjupy-test-the-headings-of-a-notebook ()
  "Headings come from markdown cells, at their levels and lines.
A code cell's comment is none, and nor is a # in a fenced block."
  (emjupy-test--outlined
    (let ((headings (emjupy--toc-headings)))
      (should (equal (mapcar (lambda (h) (list (nth 0 h) (nth 1 h))) headings)
                     '((1 "Title") (2 "Sub") (3 "Deep"))))
      (dolist (h headings)
        (save-excursion
          (goto-char (nth 2 h))
          (should (looking-at (concat "#+ " (regexp-quote (nth 1 h))))))))))

(ert-deftest emjupy-test-imenu-indexes-the-headings ()
  "imenu lists the headings, indented by level."
  (emjupy-test--outlined
    (should (eq imenu-create-index-function #'emjupy--imenu-index))
    (should (equal (mapcar #'car (funcall imenu-create-index-function))
                   '("Title" "  Sub" "    Deep")))))

(ert-deftest emjupy-test-the-outline-panel-opens-and-closes ()
  "The panel is off until asked for; asked again, it goes, and stops following."
  (emjupy-test--outlined
    (should-not (get-buffer (emjupy--toc-buffer-name buf)))
    (emjupy-toc)
    (should (get-buffer-window (emjupy--toc-buffer-name buf)))
    (should (memq #'emjupy--toc-edited after-change-functions))
    (emjupy-toc)
    (should-not (get-buffer-window (emjupy--toc-buffer-name buf)))
    (should-not (memq #'emjupy--toc-edited after-change-functions))))

(ert-deftest emjupy-test-the-outline-panel-jumps-follows-and-marks ()
  "Its buttons go to their headings; a new heading shows; the one point
is under is marked."
  (emjupy-test--outlined
    (emjupy-toc)
    (let ((panel (get-buffer (emjupy--toc-buffer-name buf))))
      (with-current-buffer panel
        (goto-char (point-min))
        (forward-line 1)
        (push-button (next-button (line-beginning-position) t)))
      (should (eq (current-buffer) buf))
      (should (looking-at "## Sub"))
      (emjupy--toc-mark-current)
      (with-current-buffer panel
        (should (string-match-p "Sub" (buffer-substring
                                       (overlay-start emjupy--toc-mark)
                                       (overlay-end emjupy--toc-mark)))))
      (let ((cell (aref (emjupy-notebook-cells emjupy--buffer-notebook) 2)))
        (emjupy--goto-cell cell 'end)
        (insert "\n# Later"))
      ;; The edit schedules the refresh; fired -- batch Emacs runs no idle
      ;; timers by itself -- it draws the new heading, unasked.
      (should (timerp emjupy--toc-timer))
      (timer-event-handler emjupy--toc-timer)
      (should-not emjupy--toc-timer)
      (with-current-buffer panel
        (should (string-match-p "Later" (buffer-string)))))))

(provide 'emjupy-toc-test)
;;; emjupy-toc-test.el ends here
