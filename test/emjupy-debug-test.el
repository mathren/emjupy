;;; emjupy-debug-test.el --- Tests of emjupy-debug.el  -*- lexical-binding: t; -*-

;;; Commentary:

;; The debugger with its replies stubbed: what it asks the kernel, and
;; what it shows.  The live test, against a kernel, is in the integration
;; tests.

;;; Code:

(require 'ert)
(require 'emjupy-test-helpers)
(require 'emjupy-debug)

(defmacro emjupy-test--debugging (replies &rest body)
  "Run BODY in a notebook of one code cell, the debugger's REPLIES stubbed.
REPLIES maps a command to the body it answers; ASKED, in BODY, lists
\(COMMAND . ARGUMENTS) sent, oldest first; CELL is the cell."
  (declare (indent 1))
  `(emjupy-test--with-notebook
       (vector (make-emjupy-cell :id 1 :type 'code
                                 :source "a = 1\nb = a + 1\nc = b * 3\nprint(c)"))
       buf nb
     (let ((asked nil) (cell (aref (emjupy-notebook-cells nb) 0)))
       (cl-letf (((symbol-function 'emjupy--debug-request)
                  (lambda (command &optional arguments)
                    (push (cons command arguments) asked)
                    (cdr (assoc command ,replies))))
                 ((symbol-function 'message) #'ignore))
         (cl-flet ((asked () (reverse asked)))
           ,@body)))))

(defun emjupy-test--line (cell n)
  "Put point at the start of line N of CELL."
  (emjupy--goto-cell cell 'beginning)
  (forward-line (1- n)))

(ert-deftest emjupy-test-a-cell-run-while-debugging-gets-its-breakpoints ()
  "Before a cell runs, the debugger has its code and its breakpoints' lines.
configurationDone once in a session; with the mode off, nothing."
  (emjupy-test--debugging
      `(("dumpCell" . ,(emjupy--debug-object "sourcePath" "/tmp/k/1.py")))
    (emjupy-test--line cell 3) (emjupy-debug-toggle-breakpoint)
    (emjupy--debug-before-execute cell)
    (should-not asked)
    (setq emjupy-debug-mode t
          emjupy--debug-paths (make-hash-table :test 'equal))
    (emjupy--debug-before-execute cell)
    (emjupy--debug-before-execute cell)
    (should (equal (mapcar #'car (asked))
                   '("dumpCell" "setBreakpoints" "configurationDone"
                     "dumpCell" "setBreakpoints")))
    (let ((set (cdr (nth 1 (asked)))))
      (should (equal (gethash "path" (gethash "source" set)) "/tmp/k/1.py"))
      (should (equal (mapcar (lambda (b) (gethash "line" b))
                             (gethash "breakpoints" set))
                     '(3))))
    (should (eq (gethash "/tmp/k/1.py" emjupy--debug-paths) cell))))

(ert-deftest emjupy-test-a-stop-marks-its-line-in-its-cell ()
  "Stopped, the line of the top frame is marked in the cell it ran from."
  (emjupy-test--debugging
      `(("stackTrace" . ,(emjupy--debug-object
                          "stackFrames"
                          (vector (emjupy--debug-object
                                   "id" 7 "line" 3
                                   "source" (emjupy--debug-object
                                             "path" "/tmp/k/1.py"))))))
    (setq emjupy--debug-paths (make-hash-table :test 'equal))
    (puthash "/tmp/k/1.py" cell emjupy--debug-paths)
    (emjupy--debug-event buf (emjupy--debug-object
                              "event" "stopped"
                              "body" (emjupy--debug-object "threadId" 1)))
    (should (equal emjupy--debug-stopped '(1 . 7)))
    (should (equal (string-trim (buffer-substring
                                 (overlay-start emjupy--debug-arrow)
                                 (overlay-end emjupy--debug-arrow)))
                   "c = b * 3"))))

(ert-deftest emjupy-test-stepping-sends-its-command ()
  "Next, in, out, continue: each its command, for the thread stopped."
  (emjupy-test--debugging nil
    (should-error (emjupy-debug-next) :type 'user-error)
    (dolist (step '((emjupy-debug-next . "next")
                    (emjupy-debug-step-in . "stepIn")
                    (emjupy-debug-step-out . "stepOut")
                    (emjupy-debug-continue . "continue")))
      (setq emjupy--debug-stopped '(4 . 9) asked nil)
      (funcall (car step))
      (should (equal (car (car asked)) (cdr step)))
      (should (= (gethash "threadId" (cdr (car asked))) 4))
      (should-not emjupy--debug-stopped))))

(ert-deftest emjupy-test-a-kernel-that-cannot-debug-says-so ()
  "Turning the debugger on with a kernel that cannot debug says what can."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code :source "x"))
      buf nb
    (cl-letf (((symbol-function 'emjupy--debug-request)
               (lambda (&rest _) (user-error "The debugger did not answer"))))
      (let ((err (should-error (emjupy-debug-mode 1) :type 'user-error)))
        (should (string-match-p "ipykernel 6" (cadr err))))
      (should-not emjupy-debug-mode))))

(provide 'emjupy-debug-test)
;;; emjupy-debug-test.el ends here
