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
      `(("dumpCell" . ,(emjupy--json-object "sourcePath" "/tmp/k/1.py")))
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
      `(("stackTrace" . ,(emjupy--json-object
                          "stackFrames"
                          (vector (emjupy--json-object
                                   "id" 7 "line" 3
                                   "source" (emjupy--json-object
                                             "path" "/tmp/k/1.py"))))))
    (setq emjupy--debug-paths (make-hash-table :test 'equal))
    (puthash "/tmp/k/1.py" cell emjupy--debug-paths)
    (emjupy--debug-event buf (emjupy--json-object
                              "event" "stopped"
                              "body" (emjupy--json-object "threadId" 1)))
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


(ert-deftest emjupy-test-a-breakpoint-with-a-condition ()
  "A prefix asks for a condition, sent with the breakpoint's line; asked on
a breakpoint there, it changes its condition, not removes it."
  (emjupy-test--debugging
      `(("dumpCell" . ,(emjupy--json-object "sourcePath" "/tmp/k/1.py")))
    (emjupy-test--line cell 3)
    (emjupy-debug-toggle-breakpoint "b > 1")
    (emjupy-debug-toggle-breakpoint "b > 5")
    (should (= (length emjupy--debug-breakpoints) 1))
    (setq emjupy-debug-mode t
          emjupy--debug-paths (make-hash-table :test 'equal))
    (emjupy--debug-before-execute cell)
    (let ((b (aref (gethash "breakpoints" (cdr (nth 1 (asked)))) 0)))
      (should (= (gethash "line" b) 3))
      (should (equal (gethash "condition" b) "b > 5")))))

(ert-deftest emjupy-test-stopping-on-exceptions ()
  "When to stop on an exception: sent at once if debugging, else kept and
sent when debugging starts."
  (emjupy-test--debugging nil
    (emjupy-debug-break-on-exceptions '("raised"))
    (should-not asked)
    (emjupy-debug-mode 1)
    (should (member "setExceptionBreakpoints" (mapcar #'car (asked))))
    (setq asked nil)
    (emjupy-debug-break-on-exceptions '("uncaught"))
    (should (equal (append (gethash "filters" (cdr (car asked))) nil)
                   '("uncaught")))
    (setq emjupy-debug-mode nil)))

(ert-deftest emjupy-test-choosing-a-frame ()
  "Choosing a frame makes it the one evaluated in, its line marked."
  (emjupy-test--debugging nil
    (let ((inner (emjupy--json-object "id" 1 "name" "inner" "line" 4
                                       "source" (emjupy--json-object
                                                 "path" "/tmp/k/1.py")))
          (outer (emjupy--json-object "id" 2 "name" "outer" "line" 2
                                       "source" (emjupy--json-object
                                                 "path" "/tmp/k/1.py"))))
      (setq emjupy--debug-paths (make-hash-table :test 'equal)
            emjupy--debug-stopped (cons 9 1)
            emjupy--debug-frames (list inner outer))
      (puthash "/tmp/k/1.py" cell emjupy--debug-paths)
      (emjupy-debug-frame outer)
      (should (equal emjupy--debug-stopped '(9 . 2)))
      (should (equal (string-trim (buffer-substring
                                   (overlay-start emjupy--debug-arrow)
                                   (overlay-end emjupy--debug-arrow)))
                     "b = a + 1")))))

(ert-deftest emjupy-test-a-variable-shows-what-it-holds ()
  "A variable holding others has a button: pressed, they show; again, hide."
  (let ((replies (list (cons 1 (vector (emjupy--json-object
                                        "name" "d" "value" "{'a': 1}"
                                        "variablesReference" 2)))
                       (cons 2 (vector (emjupy--json-object
                                        "name" "'a'" "value" "1"
                                        "variablesReference" 0))))))
    (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                          :source "x"))
        buf nb
      (cl-letf (((symbol-function 'emjupy--debug-request)
                 (lambda (command &optional arguments)
                   (if (equal command "scopes")
                       (emjupy--json-object
                        "scopes" (vector (emjupy--json-object
                                          "name" "Locals"
                                          "variablesReference" 1)))
                     (emjupy--json-object
                      "variables"
                      (cdr (assoc (gethash "variablesReference" arguments)
                                  replies))))))
                ((symbol-function 'display-buffer) #'ignore))
        (setq emjupy--debug-stopped '(1 . 1))
        (emjupy-debug-variables)
        (with-current-buffer "*emjupy variables*"
          (goto-char (point-min))
          (search-forward "+ d = {'a': 1}")
          (let ((button (button-at (match-beginning 0))))
            (button-activate button)
            (should (string-match-p "- d = .*\n +'a' = 1" (buffer-string)))
            (button-activate (save-excursion (goto-char (point-min))
                                             (search-forward "- d")
                                             (button-at (match-beginning 0))))
            (should-not (string-match-p "'a' = 1" (buffer-string)))))))))

(provide 'emjupy-debug-test)
;;; emjupy-debug-test.el ends here
