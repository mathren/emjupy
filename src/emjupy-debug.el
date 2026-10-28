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

;;;; The mode

(defvar-keymap emjupy-debug-map
  :doc "Keys of the debugger, under \\`C-c C-d'."
  :repeat (emjupy-debug-next emjupy-debug-step-in emjupy-debug-step-out
           emjupy-debug-continue)
  "d" #'emjupy-debug-mode
  "b" #'emjupy-debug-toggle-breakpoint
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
             (emjupy--debug-object
              "clientID" "emacs" "adapterID" "python" "pathFormat" "path"
              "linesStartAt1" t "columnsStartAt1" t))
            (emjupy--debug-request "attach")
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
                             (emjupy--debug-object "restart" :false
                                                   "terminateDebuggee" :false)))))

;;;; Requests and replies

(defun emjupy--debug-object (&rest pairs)
  "Return PAIRS, a plist of keys and values, as a hash table for JSON."
  (let ((h (make-hash-table :test 'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) h))
    h))

(defun emjupy--debug-request (command &optional arguments)
  "Send the debugger COMMAND with ARGUMENTS, a hash table; return its body.
Waits for the reply, a few milliseconds on the control channel: never
called from a process filter, which could not read it."
  (let* ((kernel (or (emjupy--kernel) (user-error "No kernel")))
         (msg (emjupy--make-message
               "debug_request"
               (emjupy--debug-object "seq" 1 "type" "request"
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

(defun emjupy--debug-cell-lines (cell)
  "The lines of CELL, from 1, that have a breakpoint."
  (let ((start (overlay-start (emjupy-cell-overlay cell))))
    (cl-loop for o in emjupy--debug-breakpoints
             when (eq (emjupy--debug-cell-at (overlay-start o)) cell)
             collect (1+ (count-lines start (overlay-start o))))))

(defun emjupy--debug-cell-at (pos)
  "The cell at POS, or nil."
  (save-excursion (goto-char pos) (emjupy--cell-at-point)))

(defun emjupy-debug-toggle-breakpoint ()
  "Set a breakpoint on this line of a code cell, or remove the one there."
  (interactive)
  (let ((cell (emjupy--cell-at-point))
        (here (line-beginning-position)))
    (unless (and cell (eq (emjupy-cell-type cell) 'code))
      (user-error "Breakpoints go in code cells"))
    (let ((there (cl-find here emjupy--debug-breakpoints
                          :key #'overlay-start)))
      (if there
          (progn (delete-overlay there)
                 (setq emjupy--debug-breakpoints
                       (delq there emjupy--debug-breakpoints)))
        (let ((o (make-overlay here here nil t nil)))
          (overlay-put o 'before-string
                       (propertize "*" 'display
                                   '(left-fringe filled-square
                                                 emjupy-debug-breakpoint)))
          (overlay-put o 'emjupy-breakpoint t)
          (push o emjupy--debug-breakpoints))))))

(defun emjupy--debug-before-execute (cell)
  "Make the debugger know CELL, about to run, and its breakpoints.
Its code is dumped to the file the kernel runs it as, by its content, so
anew each time it may have changed."
  (when (and emjupy-debug-mode (eq (emjupy-cell-type cell) 'code))
    (let ((path (gethash "sourcePath"
                         (emjupy--debug-request
                          "dumpCell" (emjupy--debug-object
                                      "code" (emjupy-cell-source cell))))))
      (puthash path cell emjupy--debug-paths)
      (emjupy--debug-request
       "setBreakpoints"
       (emjupy--debug-object
        "source" (emjupy--debug-object "path" path)
        "breakpoints" (vconcat (mapcar (lambda (line)
                                         (emjupy--debug-object "line" line))
                                       (emjupy--debug-cell-lines cell)))
        "sourceModified" :false))
      (unless emjupy--debug-configured
        (emjupy--debug-request "configurationDone")
        (setq emjupy--debug-configured t)))))

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
                                  (emjupy--debug-object "threadId" thread))))
                (top (and (> (length frames) 0) (aref frames 0))))
           (when top
             (setq emjupy--debug-stopped (cons thread (gethash "id" top)))
             (emjupy--debug-show top))))
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
    (setq emjupy--debug-stopped nil)
    (emjupy--debug-clear-arrow)
    (emjupy--debug-request command (emjupy--debug-object "threadId" thread))))

(defun emjupy-debug-continue () "Go on to the next breakpoint." (interactive)
       (emjupy--debug-go "continue"))
(defun emjupy-debug-next () "Run the line stopped at." (interactive)
       (emjupy--debug-go "next"))
(defun emjupy-debug-step-in () "Step into the call on this line." (interactive)
       (emjupy--debug-go "stepIn"))
(defun emjupy-debug-step-out () "Run on out of this function." (interactive)
       (emjupy--debug-go "stepOut"))

;;;; Looking around

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
                          (emjupy--debug-object
                           "expression" expression
                           "frameId" (cdr emjupy--debug-stopped)
                           "context" "repl")))))

(defun emjupy-debug-variables ()
  "Show the variables of the frame stopped at, local ones first."
  (interactive)
  (unless emjupy--debug-stopped (user-error "The debugger is not stopped"))
  ;; Asked here, in the notebook: its kernel is this buffer's.
  (let* ((scopes (append (gethash "scopes"
                                  (emjupy--debug-request
                                   "scopes" (emjupy--debug-object
                                             "frameId"
                                             (cdr emjupy--debug-stopped))))
                         nil))
         (listed (mapcar (lambda (scope)
                           (cons (gethash "name" scope)
                                 (append (gethash
                                          "variables"
                                          (emjupy--debug-request
                                           "variables"
                                           (emjupy--debug-object
                                            "variablesReference"
                                            (gethash "variablesReference"
                                                     scope))))
                                         nil)))
                         scopes))
         (buf (get-buffer-create "*emjupy variables*")))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dolist (scope listed)
          (insert (propertize (car scope) 'face 'bold) "\n")
          (dolist (v (cdr scope))
            (insert (format "  %s = %s\n" (gethash "name" v)
                            (gethash "value" v)))))
        (goto-char (point-min))
        (special-mode)))
    (display-buffer buf)))

(defun emjupy-debug-enable ()
  "Let the debugger hear the kernel, and know each cell sent to it."
  (add-hook 'emjupy-debug-functions #'emjupy--debug-message)
  (add-hook 'emjupy-before-execute-functions #'emjupy--debug-before-execute))

(provide 'emjupy-debug)
;;; emjupy-debug.el ends here
