;;; emjupy-debug.el --- A debugger for notebook cells, through the kernel  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; JupyterLab's debugger, at its smallest: breakpoints in code cells,
;; stepping, the line stopped at, an expression evaluated, the variables
;; of the frame.  As JupyterLab does, it speaks the Debug Adapter Protocol
;; to the kernel, in debug_request messages on the control channel of the
;; kernel's own WebSocket; the kernel answers in debug_reply, and tells of
;; a stop in debug_event.  A cell's code is known to the debugger by the
;; file the kernel names after it (dumpCell): breakpoints are set there.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-commands)
(require 'emjupy-kernel)

;;;; Options

(defface emjupy-debug-line
  '((t :inherit highlight))
  "Face of the line the debugger stopped at."
  :group 'emjupy-appearance)

(defface emjupy-debug-breakpoint
  '((t :inherit error))
  "Face of a breakpoint's mark in the fringe."
  :group 'emjupy-appearance)

;;;; State

(defvar emjupy--debug-replies (make-hash-table :test 'equal)
  "Debug requests sent, by message id: `waiting', or the reply's content.")

(defvar-local emjupy--debug-breakpoints nil
  "Breakpoints of this notebook: overlays on their lines' starts.")

(defvar-local emjupy--debug-paths nil
  "The kernel's file name for each cell's code run while debugging.")

(defvar-local emjupy--debug-configured nil
  "Non-nil once configurationDone has been sent in this session.")

(defvar-local emjupy--debug-stopped nil
  "Where the debugger stopped: (THREAD-ID . FRAME-ID), or nil.")

(defvar-local emjupy--debug-arrow nil
  "The overlay marking the line stopped at.")

(defvar-local emjupy--debug-frames nil
  "The frames of the stack stopped in, innermost first.")

(defvar-local emjupy--debug-exceptions nil
  "The exception filters to stop on, as debugpy names them.
None, or one of \"raised\", \"uncaught\" and \"userUnhandled\".")

;;;; The mode

(defvar-keymap emjupy-debug-map
  :doc "Keys of the debugger, under \\`C-c C-d'."
  :repeat (emjupy-debug-next emjupy-debug-step-in emjupy-debug-step-out
           emjupy-debug-continue)
  "d" #'emjupy-debug-mode
  "b" #'emjupy-debug-toggle-breakpoint
  "x" #'emjupy-debug-break-on-exceptions
  "f" #'emjupy-debug-frame
  "n" #'emjupy-debug-next
  "s" #'emjupy-debug-step-in
  "o" #'emjupy-debug-step-out
  "c" #'emjupy-debug-continue
  "e" #'emjupy-debug-evaluate
  "v" #'emjupy-debug-variables)

(define-minor-mode emjupy-debug-mode
  "Debug the cells of this notebook in its kernel, as JupyterLab does.
Set breakpoints with \\[emjupy-debug-toggle-breakpoint], then run a cell:
it stops there.  The kernel must be able to: ipykernel 6 or later, with
debugpy."
  :lighter " Debug"
  (if emjupy-debug-mode
      (condition-case err
          (progn
            (emjupy--debug-request
             "initialize"
             (emjupy--json-object
              "clientID" "emacs" "adapterID" "python" "pathFormat" "path"
              "linesStartAt1" t "columnsStartAt1" t))
            (emjupy--debug-request "attach")
            (emjupy--debug-send-exceptions)
            (setq emjupy--debug-paths (make-hash-table :test 'equal)
                  emjupy--debug-configured nil
                  emjupy--debug-stopped nil)
            (message "[emjupy] Debugging: %s sets a breakpoint, then run a cell"
                     (substitute-command-keys
                      "\\[emjupy-debug-toggle-breakpoint]")))
        (user-error
         (setq emjupy-debug-mode nil)
         (user-error "This kernel cannot debug (%s): ipykernel 6 or later, with debugpy, can"
                     (cadr err))))
    (emjupy--debug-clear-arrow)
    (setq emjupy--debug-stopped nil)
    (ignore-errors
      (emjupy--debug-request "disconnect"
                             (emjupy--json-object "restart" :false
                                                   "terminateDebuggee" :false)))))

;;;; Requests and replies

(defun emjupy--debug-request (command &optional arguments)
  "Send the debugger COMMAND with ARGUMENTS, a hash table; return its body.
Waits for the reply, a few milliseconds on the control channel: never
called from a process filter, which could not read it."
  (let* ((kernel (or (emjupy--kernel) (user-error "No kernel")))
         (msg (emjupy--make-message
               "debug_request"
               (emjupy--json-object "seq" 1 "type" "request"
                                     "command" command
                                     "arguments" (or arguments
                                                     (make-hash-table)))
               "control"))
         (id (car msg))
         (deadline (+ (float-time) 10)))
    (puthash id 'waiting emjupy--debug-replies)
    (emjupy--ws-send (cdr msg) kernel)
    (while (and (eq (gethash id emjupy--debug-replies) 'waiting)
                (< (float-time) deadline))
      (accept-process-output nil 0.05))
    (let ((reply (prog1 (gethash id emjupy--debug-replies)
                   (remhash id emjupy--debug-replies))))
      (cond ((eq reply 'waiting)
             (user-error "The debugger did not answer %s" command))
            ((not (eq (gethash "success" reply) t))
             (user-error "The debugger could not %s: %s" command
                         (or (gethash "message" reply) "no reason given")))
            (t (gethash "body" reply))))))

(defun emjupy--debug-message (kernel msg-type data)
  "Take DATA, a message of MSG-TYPE from KERNEL for the debugger.
A reply goes to its request; an event is handled from a timer, out of
the process filter, so that it can ask the debugger in turn."
  (pcase msg-type
    ("debug_reply"
     (let ((id (emjupy--ws-parent-id data)))
       (when (eq (gethash id emjupy--debug-replies) 'waiting)
         (puthash id (gethash "content" data) emjupy--debug-replies))))
    ("debug_event"
     (let ((buf (emjupy-notebook-buffer (emjupy-kernel-notebook kernel))))
       (run-at-time 0 nil #'emjupy--debug-event buf (gethash "content" data))))))

;;;; Breakpoints

(defun emjupy--debug-cell-breakpoints (cell)
  "Return CELL's breakpoints, as the debugger takes them.
Each is its line, from 1, and its condition if it has one."
  (let ((start (overlay-start (emjupy-cell-overlay cell))))
    (cl-loop for o in emjupy--debug-breakpoints
             when (eq (emjupy--debug-cell-at (overlay-start o)) cell)
             collect (let ((b (emjupy--json-object
                               "line" (1+ (count-lines start
                                                       (overlay-start o)))))
                           (condition (overlay-get o 'emjupy-condition)))
                       (when condition (puthash "condition" condition b))
                       b))))

(defun emjupy--debug-cell-at (pos)
  "The cell at POS, or nil."
  (save-excursion (goto-char pos) (emjupy--cell-at-point)))

(defun emjupy-debug-toggle-breakpoint (&optional condition)
  "Set a breakpoint on this line of a code cell, or remove the one there.
With a prefix argument, ask for a CONDITION, a Python expression: the
breakpoint, new or the one there, stops only when it is true."
  (interactive
   (list (and current-prefix-arg
              (read-string "Stop when (Python): "))))
  (let ((cell (emjupy--cell-at-point))
        (here (line-beginning-position)))
    (unless (and cell (eq (emjupy-cell-type cell) 'code))
      (user-error "Breakpoints go in code cells"))
    (let ((there (cl-find here emjupy--debug-breakpoints
                          :key #'overlay-start)))
      (cond
       ((and there condition)
        (emjupy--debug-set-condition there condition))
       (there
        (delete-overlay there)
        (setq emjupy--debug-breakpoints
              (delq there emjupy--debug-breakpoints)))
       (t
        (let ((o (make-overlay here here nil t nil)))
          (overlay-put o 'before-string
                       (propertize "*" 'display
                                   '(left-fringe filled-square
                                                 emjupy-debug-breakpoint)))
          (overlay-put o 'emjupy-breakpoint t)
          (emjupy--debug-set-condition o condition)
          (push o emjupy--debug-breakpoints)))))))

(defun emjupy--debug-set-condition (breakpoint condition)
  "Give BREAKPOINT, an overlay, CONDITION; an empty one or nil, none."
  (let ((condition (and condition (not (string-empty-p condition))
                        condition)))
    (overlay-put breakpoint 'emjupy-condition condition)
    (overlay-put breakpoint 'help-echo
                 (if condition (format "Breakpoint, when %s" condition)
                   "Breakpoint"))))

(defun emjupy--debug-before-execute (cell)
  "Make the debugger know CELL, about to run, and its breakpoints.
Its code is dumped to the file the kernel runs it as, by its content, so
anew each time it may have changed."
  (when (and emjupy-debug-mode (eq (emjupy-cell-type cell) 'code))
    (let ((path (gethash "sourcePath"
                         (emjupy--debug-request
                          "dumpCell" (emjupy--json-object
                                      "code" (emjupy-cell-source cell))))))
      (puthash path cell emjupy--debug-paths)
      (emjupy--debug-request
       "setBreakpoints"
       (emjupy--json-object
        "source" (emjupy--json-object "path" path)
        "breakpoints" (vconcat (emjupy--debug-cell-breakpoints cell))
        "sourceModified" :false))
      (unless emjupy--debug-configured
        (emjupy--debug-request "configurationDone")
        (setq emjupy--debug-configured t)))))

(defconst emjupy--debug-exception-choices
  '(("never" . nil)
    ("when not caught" . ("uncaught"))
    ("when not caught in your code" . ("userUnhandled"))
    ("when raised, even if caught" . ("raised")))
  "Choices of when to stop on an exception, and debugpy's filters for each.")

(defun emjupy-debug-break-on-exceptions (filters)
  "Choose when the debugger stops on an exception: FILTERS, debugpy's.
Never, when not caught, when not caught in your own code, or whenever
raised -- as JupyterLab offers."
  (interactive
   (list (cdr (assoc (completing-read "Stop on an exception: "
                                      emjupy--debug-exception-choices nil t)
                     emjupy--debug-exception-choices))))
  (setq emjupy--debug-exceptions filters)
  (when emjupy-debug-mode (emjupy--debug-send-exceptions)))

(defun emjupy--debug-send-exceptions ()
  "Tell the debugger when to stop on an exception."
  (emjupy--debug-request "setExceptionBreakpoints"
                         (emjupy--json-object
                          "filters" (vconcat emjupy--debug-exceptions))))

;;;; Stopping and going on

(defun emjupy--debug-event (buf content)
  "Handle CONTENT, a debugger event for the notebook in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (pcase (gethash "event" content)
        ("stopped"
         (let* ((thread (gethash "threadId" (gethash "body" content)))
                (frames (gethash "stackFrames"
                                 (emjupy--debug-request
                                  "stackTrace"
                                  (emjupy--json-object "threadId" thread))))
                (top (and (> (length frames) 0) (aref frames 0))))
           (when top
             (setq emjupy--debug-stopped (cons thread (gethash "id" top))
                   emjupy--debug-frames (append frames nil))
             (emjupy--debug-show top)
             (when (equal (gethash "reason" (gethash "body" content))
                          "exception")
               (message "[emjupy] Stopped on an exception: %s"
                        (gethash "description"
                                 (emjupy--debug-request
                                  "exceptionInfo"
                                  (emjupy--json-object
                                   "threadId" thread))))))))
        ("continued" (emjupy--debug-clear-arrow))
        ((or "terminated" "exited") (emjupy-debug-mode -1))))))

(defun emjupy--debug-show (frame)
  "Mark the line of FRAME, if in a cell; else say where it is."
  (emjupy--debug-clear-arrow)
  (let* ((path (gethash "path" (gethash "source" frame)))
         (line (gethash "line" frame))
         (cell (and emjupy--debug-paths (gethash path emjupy--debug-paths))))
    (if (not (and cell (overlayp (emjupy-cell-overlay cell))))
        (message "[emjupy] Stopped in %s, line %d" path line)
      (save-excursion
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (forward-line (1- line))
        (setq emjupy--debug-arrow
              (make-overlay (line-beginning-position)
                            (1+ (line-end-position))))
        (overlay-put emjupy--debug-arrow 'face 'emjupy-debug-line)
        (overlay-put emjupy--debug-arrow 'before-string
                     (propertize ">" 'display
                                 '(left-fringe right-triangle))))
      (goto-char (overlay-start emjupy--debug-arrow))
      (message "[emjupy] Stopped at line %d of this cell: %s" line
               (substitute-command-keys
                "\\[emjupy-debug-next] next, \\[emjupy-debug-continue] continue")))))

(defun emjupy--debug-clear-arrow ()
  "Remove the mark of the line stopped at."
  (when (overlayp emjupy--debug-arrow) (delete-overlay emjupy--debug-arrow))
  (setq emjupy--debug-arrow nil))

(defun emjupy--debug-go (command)
  "Send COMMAND -- continue, next, stepIn, stepOut -- to the stopped thread."
  (unless emjupy--debug-stopped (user-error "The debugger is not stopped"))
  (let ((thread (car emjupy--debug-stopped)))
    (setq emjupy--debug-stopped nil emjupy--debug-frames nil)
    (emjupy--debug-clear-arrow)
    (emjupy--debug-request command (emjupy--json-object "threadId" thread))))

(defun emjupy-debug-continue () "Go on to the next breakpoint." (interactive)
       (emjupy--debug-go "continue"))
(defun emjupy-debug-next () "Run the line stopped at." (interactive)
       (emjupy--debug-go "next"))
(defun emjupy-debug-step-in () "Step into the call on this line." (interactive)
       (emjupy--debug-go "stepIn"))
(defun emjupy-debug-step-out () "Run on out of this function." (interactive)
       (emjupy--debug-go "stepOut"))

;;;; Looking around

(defun emjupy-debug-frame (frame)
  "Choose FRAME of the stack stopped in, and mark its line.
Evaluating and the variables are then of it."
  (interactive
   (progn
     (unless emjupy--debug-frames (user-error "The debugger is not stopped"))
     (let* ((names (mapcar (lambda (f)
                             (cons (format "%s, line %d" (gethash "name" f)
                                           (gethash "line" f))
                                   f))
                           emjupy--debug-frames)))
       (list (cdr (assoc (completing-read "Frame: " names nil t) names))))))
  (setcdr emjupy--debug-stopped (gethash "id" frame))
  (emjupy--debug-show frame))

(defun emjupy-debug-evaluate (expression)
  "Evaluate EXPRESSION in the frame stopped at, and show its value."
  ;; Not stopped, nothing to ask for: the check comes before the prompt.
  (interactive
   (progn (unless emjupy--debug-stopped
            (user-error "The debugger is not stopped"))
          (list (read-string "Evaluate: "))))
  (unless emjupy--debug-stopped (user-error "The debugger is not stopped"))
  (message "%s" (gethash "result"
                         (emjupy--debug-request
                          "evaluate"
                          (emjupy--json-object
                           "expression" expression
                           "frameId" (cdr emjupy--debug-stopped)
                           "context" "repl")))))

(defvar-local emjupy--debug-notebook nil
  "In the variables buffer: the notebook buffer whose debugger it shows.")

(defun emjupy--debug-children (notebook reference)
  "Return the variables under REFERENCE, from NOTEBOOK's debugger."
  (with-current-buffer notebook
    (append (gethash "variables"
                     (emjupy--debug-request
                      "variables" (emjupy--json-object
                                   "variablesReference" reference)))
            nil)))

(defun emjupy--debug-insert-variable (v depth)
  "Insert variable V at DEPTH; one with children gets a button to show them."
  (insert (make-string (* 2 depth) ?\s))
  (let ((reference (gethash "variablesReference" v)))
    (if (and (integerp reference) (> reference 0))
        (insert-text-button "+" 'action #'emjupy--debug-expand
                            'emjupy-reference reference 'emjupy-depth depth
                            'follow-link t 'help-echo "Show what it holds")
      (insert " ")))
  (insert (format " %s = %s\n" (gethash "name" v) (gethash "value" v))))

(defun emjupy--debug-expand (button)
  "Toggle the display of the children of BUTTON's variable."
  (let ((inhibit-read-only t)
        (depth (button-get button 'emjupy-depth))
        (shown (button-get button 'emjupy-shown)))
    (save-excursion
      ;; from the button's own line, wherever point is: a click is anywhere
      (goto-char (button-start button))
      (if shown
          (progn (delete-region (car shown) (cdr shown))
                 (button-put button 'emjupy-shown nil)
                 (set-marker (car shown) nil) (set-marker (cdr shown) nil)
                 (emjupy--debug-relabel button "+"))
        (forward-line 1)
        (let ((start (point-marker)))
          (dolist (v (emjupy--debug-children
                      emjupy--debug-notebook
                      (button-get button 'emjupy-reference)))
            (emjupy--debug-insert-variable v (1+ depth)))
          (button-put button 'emjupy-shown (cons start (point-marker)))
          (emjupy--debug-relabel button "-"))))))

(defun emjupy--debug-relabel (button label)
  "Make BUTTON read LABEL."
  (save-excursion
    (goto-char (button-start button))
    (let ((props (text-properties-at (point))))
      (delete-char 1)
      (insert (apply #'propertize label props)))))

(defun emjupy-debug-variables ()
  "Show the variables of the frame stopped at, local ones first.
One that holds others -- a dict, a list, an object -- has a button
showing them."
  (interactive)
  (unless emjupy--debug-stopped (user-error "The debugger is not stopped"))
  ;; Asked here, in the notebook: its kernel is this buffer's.
  (let* ((notebook (current-buffer))
         (scopes (append (gethash "scopes"
                                  (emjupy--debug-request
                                   "scopes" (emjupy--json-object
                                             "frameId"
                                             (cdr emjupy--debug-stopped))))
                         nil))
         (listed (mapcar (lambda (scope)
                           (cons (gethash "name" scope)
                                 (emjupy--debug-children
                                  notebook
                                  (gethash "variablesReference" scope))))
                         scopes))
         (buf (get-buffer-create "*emjupy variables*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (special-mode)
        (setq emjupy--debug-notebook notebook)
        (dolist (scope listed)
          (insert (propertize (car scope) 'face 'bold) "\n")
          (dolist (v (cdr scope))
            (emjupy--debug-insert-variable v 1)))
        (goto-char (point-min))))
    (display-buffer buf)))

(defun emjupy-debug-enable ()
  "Let the debugger hear the kernel, and know each cell sent to it."
  (add-hook 'emjupy-debug-functions #'emjupy--debug-message)
  (add-hook 'emjupy-before-execute-functions #'emjupy--debug-before-execute))

(provide 'emjupy-debug)
;;; emjupy-debug.el ends here
