;;; emjupy-kernel.el --- Kernel WebSocket transport and execution for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The WebSocket half of the Jupyter transport: connecting a notebook to a
;; kernel, sending execute_requests, and routing incoming frames back to the
;; notebook that asked.  Each kernel has its own socket and pending table, so
;; notebooks and servers stay live together.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'websocket)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-terminal)
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-commands)

;;;; Options

(defcustom emjupy-output-render-interval 0.15
  "Seconds to let cell output accumulate before redrawing.

A redraw rebuilds the whole notebook, so doing one per arriving message
is the difference between a progress bar and a frozen Emacs: a
`tqdm' loop sends a message per iteration, and each redraw of a
notebook with figures costs tens of milliseconds.  Coalescing them
bounds the cost to one redraw per interval however fast the output
comes."
  :type 'number
  :group 'emjupy-kernels)

(defcustom emjupy-reconnect-window 30
  "Seconds to keep trying to reconnect after a kernel's connection drops.
Cells running meanwhile are kept running: the server keeps their output
while the connection is down and hands it over when it is back, so a
tunnel that drops for a moment loses nothing.  nil does not try: the
cells are ended at once, and \\[emjupy-reconnect-kernel] reconnects."
  :type '(choice (const :tag "Do not try" nil) number)
  :group 'emjupy-kernels)

(defcustom emjupy-record-execution-time t
  "Non-nil to record when each cell runs in its metadata, as JupyterLab can.
The times go under \"execution\", JupyterLab's keys, and a cell's border
shows how long it last ran.  Saved with the notebook: nil leaves files as
they were, and shows only times recorded elsewhere."
  :type 'boolean
  :group 'emjupy-kernels)

(defcustom emjupy-kernel-check-interval 30
  "Seconds a kernel running a cell may be quiet before emjupy asks after it.
After that long with nothing from the kernel while a cell runs, emjupy
asks the server, without waiting, whether the kernel is still there.  A
kernel shut down or culled while its connection stays open would leave
the cell showing as running; gone, its cells end and the notebook is left
without a kernel.  nil never asks."
  :type '(choice (const :tag "Never" nil) number)
  :safe (lambda (v) (or (null v) (and (numberp v) (> v 0))))
  :group 'emjupy-kernels)

;;;; Connection state

(defvar-local emjupy--kernel-width-told nil
  "The kernel and width last sent to it, as (KERNEL-ID . WIDTH).")

(defvar emjupy-debug-functions nil
  "Functions called with KERNEL, MSG-TYPE and DATA for the debugger's messages.
debug_reply and debug_event: the debugger, a layer above, hears them so.")

(defvar emjupy-before-execute-functions nil
  "Functions called with the cell about to run, synced, before it is sent.")

(defvar emjupy-reconnect-functions nil
  "Functions called with a server before each attempt to reconnect to it.
A layer keeping the way to a server open -- an SSH tunnel -- restores it
here.  Each is called with the `emjupy-server', and should not wait: the
attempt after finds what it restored.")

(defvar-local emjupy--kept-runs nil
  "Requests running when this notebook's connection dropped, kept.
A cons of the kernel id and a table of request id to cell, handed to the
connection that replaces it.")

(defvar-local emjupy--runs-waiting nil
  "Cells run while the connection was being restored, in the order run.
They run when it is back, as each reads then; if it is not, they do not.")

(defvar-local emjupy--server-silent nil
  "Non-nil when the server did not answer when last asked, reconnecting.
Said if the reconnect gives up: the server may have stopped.")

(defvar-local emjupy--reconnect-timer nil
  "Timer trying to reconnect after a dropped connection, or nil.")

;;;; Messages

(defun emjupy--uuid ()
  "Generate a pseudo-random UUID v4 string."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536) (random 4096)
          (logior (random 4096) #x8000)
          (random 65536) (random 65536) (random 65536)))

(defvar emjupy--session-id (emjupy--uuid)
  "Unique identifier for the current Emacs session.")

(cl-defun emjupy--make-execute-request (code &key allow-stdin)
  "Return a Jupyter `execute_request' payload for CODE.
The value is a cons of the message id and the serialized message.  With
ALLOW-STDIN, the kernel may ask for input -- `input', `getpass', pdb --
as a cell run by the user does; emjupy's own requests never do."
  (let* ((msg-id (emjupy--uuid))
         (header (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal))
         (msg (make-hash-table :test 'equal)))

    (puthash "msg_id" msg-id header)
    (puthash "username" "emacs" header)
    (puthash "session" emjupy--session-id header)
    (puthash "msg_type" "execute_request" header)
    (puthash "version" "5.3" header)

    (puthash "code" code content)
    (puthash "silent" :false content)
    (puthash "store_history" t content)
    (puthash "user_expressions" (make-hash-table) content)
    (puthash "allow_stdin" (if allow-stdin t :false) content)
    (puthash "stop_on_error" t content)

    (puthash "header" header msg)
    (puthash "parent_header" (make-hash-table) msg)
    (puthash "channel" "shell" msg)
    (puthash "metadata" (make-hash-table) msg)
    (puthash "content" content msg)
    (puthash "buffers" [] msg)

    (cons msg-id (json-serialize msg))))

;;;; Drawing output as it arrives

(defvar-local emjupy--render-timer nil
  "Timer coalescing pending output redraws in this buffer.")

(defun emjupy--schedule-redraw (buffer)
  "Arrange for BUFFER to be redrawn soon, at most once per interval."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (unless emjupy--render-timer
        (setq emjupy--render-timer
              (run-with-timer
               emjupy-output-render-interval nil
               (lambda ()
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq emjupy--render-timer nil)
                     ;; An output that cannot be drawn is logged, not left as a
                     ;; blank box.
                     (condition-case err
                         (emjupy--redraw-pending-output)
                       (error
                        (message "[emjupy] Failed to render cell output: %s"
                                 (error-message-string err)))))))))))))

;;;; Streams

(defun emjupy--stream-p (output &optional name)
  "Non-nil if OUTPUT is stream output, on stream NAME when that is given."
  (and output
       (equal (gethash "output_type" output) "stream")
       (or (null name) (equal (gethash "name" output) name))))

(defun emjupy--open-stream-output (existing name)
  "Return the output in EXISTING that new text on stream NAME continues, or nil.
The last output, if it is on NAME: consecutive output on one stream is
one stream, as in Jupyter.  Otherwise the latest output on NAME whose
last line is still open, not ended by a newline -- a progress bar, whose
updates on stderr alternate with prints on stdout.  A terminal keeps each
stream's line open until a newline closes it, and so does this."
  (let ((last (car (last existing))))
    (if (emjupy--stream-p last name)
        last
      (let ((recent (seq-find (lambda (o) (emjupy--stream-p o name))
                              (reverse existing))))
        (and recent
             (not (string-suffix-p "\n"
                                   (emjupy--mime-text (gethash "text" recent))))
             recent)))))

(defvar emjupy--stream-cursors (make-hash-table :test 'eq :weakness 'key)
  "Where the cursor is in each stream output that cursor codes left mid-text.
Kept apart from the output, which is saved as it is: the position is the
terminal's, not the notebook's.")

(defun emjupy--stream-write (output old chunk)
  "Set OUTPUT's text to OLD with CHUNK written to it, as on a terminal.
Cursor codes -- stacked progress bars -- are applied from where the last
write left the cursor; text without them, written at the end, has only
its carriage returns applied, as it always had."
  (let ((cursor (gethash output emjupy--stream-cursors)))
    (if (or cursor (emjupy--terminal-codes-p chunk))
        (let ((written (emjupy--terminal-apply old chunk cursor)))
          (puthash "text" (car written) output)
          (if (cdr written)
              (puthash output (cdr written) emjupy--stream-cursors)
            (remhash output emjupy--stream-cursors)))
      (puthash "text"
               (emjupy--collapse-carriage-returns (concat old chunk)) output))))

(defun emjupy--merge-stream-output (existing output-hash)
  "Return EXISTING with OUTPUT-HASH merged in, or nil if it cannot be.
Stream text continues the output it belongs to, chosen by
`emjupy--open-stream-output', rather than adding one per message."
  (when (emjupy--stream-p output-hash)
    (let ((target (emjupy--open-stream-output existing (gethash "name"
                                                                output-hash))))
      (when target
        (emjupy--stream-write target (emjupy--mime-text (gethash "text" target))
                              (emjupy--mime-text (gethash "text" output-hash)))
        existing))))

(defun emjupy-flush-output (&optional buffer)
  "Redraw BUFFER now instead of waiting for the coalescing timer.

Output is normally redrawn a fraction of a second after it arrives, so
that a burst of messages costs one redraw rather than hundreds.  This
forces the pending one, which matters when something needs the buffer to
match the cells right away -- saving, or a test."
  (interactive)
  (let ((buffer (or buffer (current-buffer))))
    (when (buffer-live-p buffer)
      (with-current-buffer buffer
        (when emjupy--render-timer
          (cancel-timer emjupy--render-timer)
          (setq emjupy--render-timer nil))
        (emjupy--redraw-pending-output)))))

(defvar-local emjupy--cells-awaiting-output nil
  "Cells whose output box is waiting to be redrawn in this buffer.")

(defun emjupy--redraw-pending-output ()
  "Redraw the output of cells that have some waiting, in place if possible.
The notebook is rebuilt instead when the buffer and the cells disagree."
  (let ((cells emjupy--cells-awaiting-output))
    (setq emjupy--cells-awaiting-output nil)
    ;; Keep the cells matching the buffer: patching one region does not read the
    ;; others, and an edit made while output was in flight would never reach its
    ;; cell.
    (when cells (emjupy--sync-all-cells))
    ;; every cell redrawn, then the rules rebuilt once
    (if (and cells (emjupy--overlays-sane-p)
             (emjupy--with-rules-once
               (cl-every (lambda (cell)
                           (emjupy--redraw-cell-output cell))
                         cells)))
        t
      (emjupy--redraw-notebook-keeping-point))))

;;;; Who an output belongs to

(defvar emjupy-comm-functions nil
  "Functions called with each comm message from a kernel.
Each is called with the KERNEL, the message type -- \"comm_open\",
\"comm_msg\" or \"comm_close\" -- the message content, and the binary
buffers the message carried, a list of `emjupy-bytes'.  This is how
the layer that knows about widgets hears of them.")

(defvar emjupy-output-owner-functions nil
  "Functions asked which cell output answering a request belongs to.
Each is called with the request's message id and returns a cell or nil.
Asked only for a request no cell is waiting on: one emjupy sent itself,
such as moving a widget's slider, whose redrawn figure belongs to the
cell showing the widget.")

;;;; Clearing output

(defvar emjupy--clear-before-next-output (make-hash-table :test 'eq
                                                          :weakness 'key)
  "Cells whose outputs are cleared when their next output arrives.")

(defun emjupy--widget-view-output-p (output)
  "Return non-nil if OUTPUT is the view of a widget."
  (let ((data (and (hash-table-p output) (gethash "data" output))))
    (and (hash-table-p data) (gethash emjupy--widget-view-mime data))))

(defun emjupy--clear-cell-outputs-now (cell notebook)
  "Clear CELL's outputs in NOTEBOOK, as a kernel's `clear_output' asks.
Widgets stay: in a notebook a widget's own output area is what an
`interact' clears before drawing again, and the controls are outside
it.  Here the outputs are one list, so the widgets are what is kept."
  (setf (emjupy-cell-outputs cell)
        (vconcat (seq-filter #'emjupy--widget-view-output-p
                             (append (or (emjupy-cell-outputs cell) []) nil))))
  (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cl-pushnew cell emjupy--cells-awaiting-output))
      (emjupy--schedule-redraw buf))))

;;;; Replies and input

(defun emjupy--make-message (msg-type content &optional channel parent-header)
  "Return a message of MSG-TYPE carrying CONTENT, a hash table.
It goes on CHANNEL, the shell channel by default, in answer to the
message whose PARENT-HEADER is given, if any.  The value is a cons of the
message id and the serialized message."
  (let ((msg-id (emjupy--uuid))
        (header (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (puthash "msg_id" msg-id header)
    (puthash "username" "emacs" header)
    (puthash "session" emjupy--session-id header)
    (puthash "msg_type" msg-type header)
    (puthash "version" "5.3" header)
    (puthash "header" header msg)
    (puthash "parent_header" (or parent-header (make-hash-table)) msg)
    (puthash "channel" (or channel "shell") msg)
    (puthash "metadata" (make-hash-table) msg)
    (puthash "content" content msg)
    (puthash "buffers" [] msg)
    (cons msg-id (json-serialize msg))))

(defvar emjupy--input-history nil
  "Answers given to a kernel's requests for input -- pdb's commands, say.")

(defun emjupy--input-requested (kernel data)
  "Ask the user for what KERNEL's request for input, DATA, asks.
A cell called `input', `getpass', or pdb, and the kernel waits for
the answer.  Asked from a timer, not here in the process filter, where
waiting on the minibuffer would hold up every other message."
  (let ((content (gethash "content" data)))
    (run-at-time 0 nil #'emjupy--answer-input-request kernel
                 (or (gethash "prompt" content) "")
                 (eq (gethash "password" content) t)
                 (gethash "header" data))))

(defun emjupy--answer-input-request (kernel prompt password parent-header)
  "Ask PROMPT in the minibuffer and send the answer to KERNEL.
PARENT-HEADER is the request's.  With PASSWORD the answer is not shown,
and not kept in the history.  \\[keyboard-quit] interrupts the kernel,
as Jupyter's own frontends do: an empty answer would count, and pdb
would repeat its last command."
  (let* ((nb (emjupy-kernel-notebook kernel))
         (label (format "[%s] %s"
                        (file-name-nondirectory
                         (or (and nb (emjupy-notebook-path nb)) "kernel"))
                        (if (string-empty-p prompt) "input: " prompt)))
         (value (condition-case nil
                    (if password
                        (read-passwd label)
                      (read-string label nil 'emjupy--input-history))
                  (quit nil))))
    (if value
        (let ((content (make-hash-table :test 'equal)))
          (puthash "value" value content)
          (emjupy--ws-send (cdr (emjupy--make-message "input_reply" content
                                                      "stdin" parent-header))
                           kernel))
      (when (and nb (buffer-live-p (emjupy-notebook-buffer nb)))
        (with-current-buffer (emjupy-notebook-buffer nb)
          (emjupy-interrupt-kernel))))))

;;;; Adding output to a cell

(defun emjupy--append-output-to-cell (cell output-hash &optional notebook)
  "Append OUTPUT-HASH to CELL outputs and refresh NOTEBOOK's buffer."
  ;; `clear_output' with wait: the clear happens now, as the new output
  ;; arrives, so the old one stays up until there is something to replace it
  (when (gethash cell emjupy--clear-before-next-output)
    (remhash cell emjupy--clear-before-next-output)
    (setf (emjupy-cell-outputs cell)
          (vconcat (seq-filter #'emjupy--widget-view-output-p
                               (append (or (emjupy-cell-outputs cell)
                                           []) nil)))))
  (let* ((existing (append (or (emjupy-cell-outputs cell) []) nil))
         (merged (emjupy--merge-stream-output existing output-hash)))
    ;; Stream text starting an output of its own still has carriage returns
    ;; applied: the first update of a progress bar comes that way.
    (when (and (not merged) (emjupy--stream-p output-hash))
      (emjupy--stream-write output-hash ""
                            (emjupy--mime-text (gethash "text" output-hash))))
    (setf (emjupy-cell-outputs cell)
          (vconcat (or merged (append existing (list output-hash)))))
    (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (cl-pushnew cell emjupy--cells-awaiting-output)))
      (emjupy--schedule-redraw buf))))

;;;; Binary frames

(cl-defstruct (emjupy-bytes (:constructor emjupy-bytes-make (data)))
  "Bytes a kernel message carried in binary: an image widget's, say.
Wrapped, so that no one takes them for text."
  data)

(defun emjupy--u32 (bytes pos)
  "Return the big-endian 32-bit number at POS in the unibyte string BYTES."
  (logior (ash (aref bytes pos) 24) (ash (aref bytes (+ pos 1)) 16)
          (ash (aref bytes (+ pos 2)) 8) (aref bytes (+ pos 3))))

(defun emjupy--u32-bytes (n)
  "Return the number N as four big-endian bytes."
  (unibyte-string (logand (ash n -24) 255) (logand (ash n -16) 255)
                  (logand (ash n -8) 255) (logand n 255)))

(defun emjupy--decode-binary-message (bytes)
  "Split a binary WebSocket frame BYTES into its message and its buffers.
Return a cons of the message's JSON text and a list of the buffers, as
unibyte strings.  The format is the Jupyter server's: a count of parts,
the offset of each, then the parts -- the JSON first, then the buffers."
  (let* ((n (emjupy--u32 bytes 0))
         (offsets (cl-loop for i from 1 to n collect (emjupy--u32 bytes
                                                                  (* 4 i))))
         (ends (append (cdr offsets) (list (length bytes))))
         (parts (cl-mapcar (lambda (from to) (substring bytes from to))
                           offsets ends)))
    (cons (decode-coding-string (car parts) 'utf-8) (cdr parts))))

(defun emjupy--encode-binary-message (json buffers)
  "Return a binary WebSocket frame of the message JSON and its BUFFERS.
JSON is the serialized message, as `json-serialize' gives it; BUFFERS
unibyte strings.  The format `emjupy--decode-binary-message' reads."
  (let* ((parts (cons (encode-coding-string json 'utf-8) buffers))
         (n (length parts))
         (offset (* 4 (1+ n)))
         (offsets nil))
    (dolist (part parts)
      (push offset offsets)
      (setq offset (+ offset (length part))))
    (apply #'concat (emjupy--u32-bytes n)
           (append (mapcar #'emjupy--u32-bytes (nreverse offsets)) parts))))

(defun emjupy--ws-message-parts (frame)
  "Return FRAME's message as a cons of its JSON text and its buffers.
A message carrying binary data comes as a binary frame, read here; any
other has no buffers."
  (if (and (websocket-frame-p frame)
           (eq (websocket-frame-opcode frame) 'binary))
      (emjupy--decode-binary-message (websocket-frame-payload frame))
    (cons (emjupy--ws-payload frame) nil)))

(defun emjupy--ws-send-binary (bytes kernel)
  "Send the binary frame BYTES on KERNEL's WebSocket."
  (websocket-send (emjupy-kernel-ws kernel)
                  (make-websocket-frame :opcode 'binary :payload bytes
                                        :completep t)))

(defun emjupy--ws-payload (frame)
  "Return the text payload of FRAME.
Accepts either a `websocket-frame' (what websocket.el hands the
callback) or a plain JSON string, so recorded kernel traffic can be
replayed straight through the handler."
  (if (websocket-frame-p frame) (websocket-frame-payload frame) frame))

;;;; emjupy's own requests

(defvar emjupy--internal-requests (make-hash-table :test 'equal)
  "Message ids of emjupy's own requests, mapped to their callbacks.
Apart from a kernel's pending table: they belong to no cell.")

(defun emjupy--kernel-eval (kernel code callback)
  "Run CODE on KERNEL and pass its printed output, as printed, to CALLBACK.

For emjupy's own questions -- \"where are you?\" -- not for user code."
  (when (emjupy--ws-live-p kernel)
    (let* ((req (emjupy--make-execute-request code))
           (msg-id (car req)))
      ;; The callback, and the output gathered so far, newest first.
      (puthash msg-id (cons callback nil) emjupy--internal-requests)
      (emjupy--ws-send (cdr req) kernel)
      msg-id)))

(defun emjupy--idle-p (msg-type data)
  "Return non-nil if MSG-TYPE and DATA make up an idle status message."
  (and (string= msg-type "status")
       (equal (gethash "execution_state" (gethash "content" data)) "idle")))

(defvar emjupy--request-halves (make-hash-table :test 'equal)
  "Requests for which one of execute_reply and the closing idle has come.")

(defun emjupy--request-half-seen (id part pending)
  "Note that PART of request ID has arrived; forget the request once both have.
PART is `reply' for execute_reply and `idle' for the status that
follows the last output.  The protocol orders neither against the
other, and an output can come after the reply, so the request is
forgotten only when both have come.  PENDING is the kernel's table of
requests awaiting output."
  (let ((seen (gethash id emjupy--request-halves)))
    (if (and seen (not (eq seen part)))
        (progn (remhash id emjupy--request-halves)
               (remhash id pending))
      (puthash id part emjupy--request-halves))))

;;;; Incoming messages

(defun emjupy--internal-reply (id request msg-type data)
  "Handle DATA, a message of MSG-TYPE answering emjupy's own request ID.
REQUEST is the request's entry, (CALLBACK . TEXTS).  The answer is kept
out of the notebook, and handed to the callback whole once the request
is done: a large answer comes in several messages."
  (when (string= msg-type "stream")
    (push (or (gethash "text" (gethash "content" data)) "") (cdr request)))
  (when (emjupy--idle-p msg-type data)
    ;; Its answer has come by now: idle follows every output of the
    ;; request on the same channel, where execute_reply may not.
    (remhash id emjupy--internal-requests)
    (when (cdr request)
      ;; As printed: a caller asking for a line trims it; one asking for
      ;; data, a piece of a file, needs every character.
      (funcall (car request) (apply #'concat (reverse (cdr request)))))))

(defun emjupy--stream-output (content)
  "Return a stream output, from CONTENT, a stream message's content."
  (let ((output (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" output)
    (puthash "name" (or (gethash "name" content) "stdout") output)
    (puthash "text" (or (gethash "text" content) "") output)
    output))

(defun emjupy--error-output (content)
  "Return an error output, from CONTENT, an error message's content."
  (let ((output (make-hash-table :test 'equal)))
    (puthash "output_type" "error" output)
    (puthash "ename" (gethash "ename" content) output)
    (puthash "evalue" (gethash "evalue" content) output)
    (puthash "traceback" (gethash "traceback" content) output)
    output))

(defun emjupy--cell-replied (cell content id pending notebook)
  "Finish CELL's run, its request ID answered by an execute_reply.
CONTENT is the reply's content; PENDING the kernel's table of requests;
NOTEBOOK the cell's.  The count is set, the cell marked done, and its
output redrawn -- the notebook only if that cannot be done in place:
redrawing it on every reply, a notebook run whole was redrawn once per
cell."
  (setf (emjupy-cell-exec-count cell) (gethash "execution_count" content))
  (emjupy--request-half-seen id 'reply pending)
  (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook)))
              ((buffer-live-p buf)))
    (emjupy--mark-done cell buf)
    (with-current-buffer buf
      (unless (emjupy--redraw-cell-output cell)
        (emjupy--redraw-notebook-keeping-point)))))

(defun emjupy--rich-output (type content)
  "Return an output of TYPE, from CONTENT, a kernel message's content.
TYPE is \"execute_result\" or \"display_data\".  The fields nbformat v4
requires are always there -- metadata, and an execute_result's count --
or the saved notebook fails validation."
  (let ((output (make-hash-table :test 'equal)))
    (puthash "output_type" type output)
    (puthash "data" (or (gethash "data" content) (make-hash-table :test 'equal))
             output)
    (puthash "metadata" (or (gethash "metadata" content)
                            (make-hash-table :test 'equal))
             output)
    (when (equal type "execute_result")
      (puthash "execution_count" (or (gethash "execution_count" content) :null)
               output))
    output))

(defun emjupy--ws-message-owner (pending parent-id)
  "Return the cell a message answering PARENT-ID is for, or nil.
A request a cell made is in PENDING; one emjupy sent itself, such as
moving a slider, is claimed through `emjupy-output-owner-functions'."
  (when parent-id
    (or (and pending (gethash parent-id pending))
        (run-hook-with-args-until-success
         'emjupy-output-owner-functions parent-id))))

(defun emjupy--ws-parent-id (data)
  "Return the msg_id of the request DATA, a kernel message, answers, or nil."
  (let ((parent-header (gethash "parent_header" data)))
    (when parent-header (gethash "msg_id" parent-header))))

(defun emjupy--record-execution-time (cell msg-type data)
  "Record in CELL's metadata the time of DATA, a message of MSG-TYPE.
JupyterLab's keys, the kernel's own time from the message header; a run
starting anew, its times from before go."
  (let ((key (pcase msg-type
               ("execute_input" "iopub.execute_input")
               ("execute_reply" "shell.execute_reply")
               ("status" (pcase (gethash "execution_state"
                                         (gethash "content" data))
                           ("busy" "iopub.status.busy")
                           ("idle" "iopub.status.idle")))))
        (date (gethash "date" (gethash "header" data))))
    (when (and key (stringp date))
      (unless (hash-table-p (emjupy-cell-metadata cell))
        (setf (emjupy-cell-metadata cell) (make-hash-table :test 'equal)))
      (let ((times (gethash "execution" (emjupy-cell-metadata cell))))
        (when (or (not (hash-table-p times)) (equal key "iopub.status.busy"))
          (setq times (make-hash-table :test 'equal))
          (puthash "execution" times (emjupy-cell-metadata cell)))
        (puthash key date times)))))

(defun emjupy--apply-to-cell (cell kernel data)
  "Apply DATA, a message from KERNEL, to CELL, the cell it is for."
  (let ((msg-type (gethash "msg_type" (gethash "header" data)))
        (content (gethash "content" data))
        (parent-id (emjupy--ws-parent-id data))
        (pending (and kernel (emjupy-kernel-pending kernel)))
        (notebook (and kernel (emjupy-kernel-notebook kernel))))
    (when emjupy-record-execution-time
      (emjupy--record-execution-time cell msg-type data))
    (pcase msg-type
      ("clear_output"
       (if (eq (gethash "wait" content) t)
           (puthash cell t emjupy--clear-before-next-output)
         (emjupy--clear-cell-outputs-now cell notebook)))
      ("stream"
       (emjupy--append-output-to-cell
        cell (emjupy--stream-output content) notebook))
      ;; A value returned, or rich output: a figure comes as display_data.
      ((or "execute_result" "display_data")
       (emjupy--append-output-to-cell
        cell (emjupy--rich-output msg-type content) notebook))
      ("error"
       (emjupy--append-output-to-cell
        cell (emjupy--error-output content) notebook))
      (_
       (cond
        ;; The kernel has published everything this request will produce.
        ((emjupy--idle-p msg-type data)
         (emjupy--request-half-seen parent-id 'idle pending))
        ((string= msg-type "execute_reply")
         (emjupy--cell-replied cell content parent-id pending notebook)))))))

(defun emjupy--handle-ws-message (kernel frame)
  "Handle an incoming WebSocket FRAME belonging to KERNEL.
KERNEL carries both the pending-request table and the backlink to the
notebook whose buffer should be refreshed, so frames from several
kernels never cross-talk."
  (let* ((parts (emjupy--ws-message-parts frame))
         (data (json-parse-string (car parts) :object-type 'hash-table
                                  :array-type 'array))
         (buffers (mapcar #'emjupy-bytes-make (cdr parts)))
         (content (gethash "content" data))
         (msg-type (gethash "msg_type" (gethash "header" data)))
         (parent-id (emjupy--ws-parent-id data))
         (pending (and kernel (emjupy-kernel-pending kernel)))
         (cell (progn (when kernel
                        (setf (emjupy-kernel-heard kernel) (float-time)))
                      (emjupy--ws-message-owner pending parent-id)))
         (internal (when parent-id (gethash parent-id
                                            emjupy--internal-requests))))
    ;; What it is doing, for the mode line: any status, whoever's request.
    (when (and kernel (equal msg-type "status"))
      (let ((state (gethash "execution_state" content)))
        (unless (equal state (emjupy-kernel-state kernel))
          (setf (emjupy-kernel-state kernel) state)
          (emjupy--kernel-told kernel))))
    ;; The kernel waiting on the user: a cell asked for input.
    (when (equal msg-type "input_request")
      (emjupy--input-requested kernel data))
    (when internal
      (emjupy--internal-reply parent-id internal msg-type data))
    (when (member msg-type '("debug_reply" "debug_event"))
      (run-hook-with-args 'emjupy-debug-functions kernel msg-type data))
    (when (member msg-type '("comm_open" "comm_msg" "comm_close"))
      (run-hook-with-args 'emjupy-comm-functions kernel msg-type content
                          buffers))
    (when cell
      (emjupy--apply-to-cell cell kernel data))))

;;;; Connecting

(defun emjupy--guard-refused-socket (ws)
  "Keep a refused WS's leftovers from raising errors.
Its handshake refused, websocket.el goes on to read the server's error
page as frames, and answers a ping found there on the closed connection:
an error in the process's filter.  Errors of a socket marked refused are
dropped; any other's are raised as they were."
  (when-let* (((websocket-p ws))
              (conn (websocket-conn ws))
              ((processp conn))
              (filter (process-filter conn)))
    (set-process-filter
     conn (lambda (process output)
            (condition-case err
                (funcall filter process output)
              (error
               (unless (eq (websocket-client-data ws) 'refused)
                 (signal (car err) (cdr err)))))))))

(defun emjupy-connect-kernel (notebook kernel-id &optional kernel-name)
  "Attach NOTEBOOK to KERNEL-ID on its own server, over its own WebSocket.
KERNEL-NAME, when given, is the kernelspec name to record.
Returns the `emjupy-kernel'.  Each notebook keeps its own kernel, so
several notebooks -- from several servers -- stay live at once."
  ;; A new connection, or a restarted kernel under the same id: told the
  ;; width afresh.
  (when (buffer-live-p (emjupy-notebook-buffer notebook))
    (with-current-buffer (emjupy-notebook-buffer notebook)
      (setq emjupy--kernel-width-told nil)))
  (let* ((server (emjupy-notebook-server notebook))
         (parts (emjupy--server-parts server))
         ;; https:// servers require wss://, and any base path (proxy or
         ;; JupyterHub prefix) has to be kept -- rebuilding the URL from
         ;; host+port alone silently drops both.
         (ws-url (format "%s://%s:%s%s/api/kernels/%s/channels%s"
                         (plist-get parts :ws-scheme)
                         (plist-get parts :host)
                         (plist-get parts :port)
                         (plist-get parts :path)
                         kernel-id
                         (concat "?session_id="
                                 (emjupy--ws-session-id notebook kernel-id)
                                 (emjupy--ws-token-param server "&"))))
         ;; The token in a header; in the address only for a server refusing
         ;; it so.  Shared with the language server's socket.
         (headers (emjupy--websocket-auth-headers server))
         (kernel (make-emjupy-kernel
                  :id kernel-id :name kernel-name :server server
                  :pending (make-hash-table :test 'equal)
                  :notebook notebook))
         (label (emjupy-notebook-path notebook)))
    ;; The callbacks close over KERNEL, so a frame is always delivered to the
    ;; notebook that asked for it -- never to whichever one happens to be
    ;; current when it arrives.
    (setf (emjupy-kernel-ws kernel)
          (websocket-open
           ws-url
           :custom-header-alist headers
           ;; A socket the server refused is never open: websocket.el, its
           ;; handshake refused, still calls `on-open', then reads the error
           ;; page as frames.
           :on-message (lambda (ws frame)
                         (unless (eq (websocket-client-data ws) 'refused)
                           (emjupy--handle-ws-message kernel frame)))
           :on-open (lambda (ws)
                      (emjupy--kernel-told kernel)
                      (unless (eq (websocket-client-data ws) 'refused)
                        (message "[emjupy] %s connected to kernel %s" label
                                 kernel-id)))
           :on-close (lambda (_ws)
                       (emjupy--kernel-connection-closed kernel label))
           :on-error (lambda (ws type err)
                       ;; Its handshake refused -- the kernel gone, a server
                       ;; restarted -- the socket is closed, and the
                       ;; reconnecting asks the server what became of it.
                       (when (and (websocket-p ws)
                                  (eq (car-safe err)
                                      'websocket-received-error-http-response))
                         (setf (websocket-client-data ws) 'refused)
                         ;; Dropped, not closed: closing writes a frame to a
                         ;; connection the server has shut.
                         (when (process-live-p (websocket-conn ws))
                           (delete-process (websocket-conn ws))))
                       ;; Refused with the token in a header alone: the next
                       ;; attempt, reconnecting, has it in the address too.
                       (if (emjupy--note-ws-refusal server err)
                           (message "[emjupy] %s: the server wants the token in the WebSocket address; trying so"
                                    label)
                         (message "[emjupy] %s WebSocket error (%s): %s" label
                                  type err)))))
    (emjupy--guard-refused-socket (emjupy-kernel-ws kernel))
    (setf (emjupy-notebook-kernel notebook) kernel)
    ;; cells kept running when the last connection dropped
    (emjupy--adopt-kept-runs kernel)
    ;; A hook, run once the socket is up: the language-server layer, above this
    ;; one, asks the kernel where it runs.
    (run-with-timer 0.5 nil #'run-hook-with-args
                    'emjupy-kernel-connected-functions notebook)
    kernel))

(defun emjupy--start-kernel-session (notebook &optional name)
  "Create a session for NOTEBOOK and return its kernel description, or nil.
NAME is the kernel to start, by default the one NOTEBOOK records.

Returns nil rather than signalling if the server will not make one, so
the caller can fall back to a bare kernel."
  (let ((server (emjupy-notebook-server notebook))
        (path (emjupy-notebook-path notebook))
        (body (make-hash-table :test 'equal))
        (kspec (make-hash-table :test 'equal)))
    (when path
      (puthash "name" (or name (emjupy--notebook-kernel-name notebook)) kspec)
      (puthash "path" path body)
      (puthash "type" "notebook" body)
      (puthash "name" (file-name-nondirectory path) body)
      (puthash "kernel" kspec body)
      (condition-case err
          (let ((res (emjupy--http-request "POST" server "/api/sessions"
                                           (json-serialize body))))
            (and (hash-table-p res) (gethash "kernel" res)))
        ;; Its callers carry on without a session; the server's reason --
        ;; a token refused, a kernel it does not have -- is said, not lost.
        (emjupy-http-error
         (message "[emjupy] Could not start a session for %s: %s"
                  path (error-message-string err))
         nil)))))

(defun emjupy--notebook-kernel-name (notebook)
  "Return the name of the kernel NOTEBOOK records: its kernelspec's.
\"python3\" for a notebook that records none."
  (let* ((meta (emjupy-notebook-metadata notebook))
         (spec (and (hash-table-p meta) (gethash "kernelspec" meta)))
         (name (and (hash-table-p spec) (gethash "name" spec))))
    (if (and (stringp name) (not (string-empty-p name))) name "python3")))

(defun emjupy--spawn-and-connect-kernel (notebook)
  "Start a fresh kernel on NOTEBOOK's server, the one it records, and attach it.
Through a session bound to the notebook's path, so the kernel runs in
the notebook's directory, as in Jupyter, and other front-ends see it as
the notebook's kernel.  A kernel the server does not have, say from a
notebook made elsewhere, gives way to python3, saying so."
  (let* ((server (emjupy-notebook-server notebook))
         (wanted (emjupy--notebook-kernel-name notebook))
         (name wanted)
         (res (or (emjupy--start-kernel-session notebook name)
                  (unless (equal name "python3")
                    (message "[emjupy] %s asks for the kernel %s, which the server could not start; starting python3"
                             (emjupy-notebook-path notebook) wanted)
                    (setq name "python3")
                    (emjupy--start-kernel-session notebook name))
                  (emjupy--http-request
                   "POST" server "/api/kernels"
                   (json-serialize (emjupy--json-object "name" name)))))
         (new-id (gethash "id" res)))
    (message "[emjupy] Started %s kernel (%s) for %s. Connecting..."
             name new-id (emjupy-notebook-path notebook))
    (emjupy-connect-kernel notebook new-id name)))

(defun emjupy-connect-kernel-interactive ()
  "Select an existing kernel, or spawn a new one, for THIS notebook.
Kernels already driving another open notebook are marked, since
attaching two notebooks to one kernel makes them share state."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (kernels-data (emjupy--http-request "GET" server "/api/kernels"))
         (in-use (emjupy--kernel-ids-in-use))
         (kernel-options '("[Start New Python 3 Kernel]"))
         (kernel-map (make-hash-table :test 'equal)))

    (cl-loop for k across kernels-data
             for id = (gethash "id" k)
             for name = (gethash "name" k)
             for label = (format "%s (%s)%s" name id
                                 (if (member id in-use) " [in use]" ""))
             do (push label kernel-options)
             (puthash label id kernel-map))

    (let ((choice (completing-read (format "Kernel for %s on %s: "
                                           (emjupy-notebook-path nb)
                                           (emjupy--server-label server))
                                   (nreverse kernel-options))))
      ;; Drop any socket this notebook already had, or it keeps receiving
      ;; frames from the kernel we are replacing.
      (emjupy--disconnect-kernel nb)
      (if (string= choice "[Start New Python 3 Kernel]")
          (emjupy--spawn-and-connect-kernel nb)
        (let ((selected-id (gethash choice kernel-map)))
          (message "[emjupy] Connecting %s to existing kernel %s..."
                   (emjupy-notebook-path nb) selected-id)
          (emjupy-connect-kernel nb selected-id))))))

(defun emjupy--kernel-ids-in-use ()
  "Return kernel ids currently attached to some open emjupy notebook."
  (delq nil
        (mapcar (lambda (b)
                  (let* ((nb (buffer-local-value 'emjupy--buffer-notebook b))
                         (k (and nb (emjupy-notebook-kernel nb))))
                    (and k (emjupy-kernel-id k))))
                (emjupy--notebook-buffers))))

(defun emjupy--disconnect-kernel (notebook)
  "Close NOTEBOOK's WebSocket, if any, and drop its in-flight requests."
  (when-let* ((kernel (emjupy-notebook-kernel notebook)))
    ;; closed on purpose: not a drop to reconnect after
    (setf (emjupy-kernel-closing kernel) t)
    (when (emjupy--ws-live-p kernel)
      (websocket-close (emjupy-kernel-ws kernel)))
    (when (emjupy-kernel-pending kernel)
      (clrhash (emjupy-kernel-pending kernel)))
    (setf (emjupy-kernel-ws kernel) nil))
  ;; Nor is a reconnecting begun by an earlier drop to go on: it would
  ;; replace the connection made next, requests in flight on this one.
  ;; Cells held for it run no more, as Jupyter's queue on a restart.
  (let ((buf (emjupy-notebook-buffer notebook)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (timerp emjupy--reconnect-timer)
          (cancel-timer emjupy--reconnect-timer))
        (setq emjupy--reconnect-timer nil)
        (dolist (cell emjupy--runs-waiting) (emjupy--mark-done cell buf))
        (setq emjupy--runs-waiting nil)))))

;;;; The connection

(defun emjupy--kernel-told (kernel)
  "Have the mode line of KERNEL's notebook show what it is doing now."
  (let ((buf (and kernel (emjupy-kernel-notebook kernel)
                  (emjupy-notebook-buffer (emjupy-kernel-notebook kernel)))))
    (when (buffer-live-p buf)
      (with-current-buffer buf (force-mode-line-update)))))

(defun emjupy--ws-live-p (&optional kernel)
  "Return non-nil when KERNEL (default this buffer's) has a usable socket."
  (let* ((kernel (or kernel (emjupy--kernel)))
         (ws (and kernel (emjupy-kernel-ws kernel))))
    (and ws (websocket-p ws) (websocket-openp ws))))

(defun emjupy--ws-send (payload &optional kernel)
  "Send PAYLOAD, a string, over KERNEL's WebSocket."
  (let ((kernel (or kernel (emjupy--kernel))))
    (websocket-send-text (emjupy-kernel-ws kernel) payload)))

;;;; Once connected

(defvar emjupy-kernel-connected-functions nil
  "Functions called with a notebook once a kernel is attached to it.

Run a moment after the connection, when the socket is up.  Layers above
this one hang their start-up work here -- asking the kernel where it
runs, for instance -- rather than this file calling into them.")

(defun emjupy--tell-kernel-width (nb &optional width)
  "Tell NB's kernel how wide its output is drawn: WIDTH, or the box's now.
Sets COLUMNS in the kernel's environment, which programs read when their
output is not a terminal, so progress bars and tables fit.  Wider output
is still folded where the box ends."
  (let ((kernel (and nb (emjupy-notebook-kernel nb)))
        (buf (and nb (emjupy-notebook-buffer nb))))
    (when (and kernel (emjupy--ws-live-p kernel) (buffer-live-p buf))
      (with-current-buffer buf
        (let ((columns (or width (emjupy--box-width))))
          ;; Told already, on this connection: not again.
          (unless (equal emjupy--kernel-width-told
                         (cons (emjupy-kernel-id kernel) columns))
            (emjupy--kernel-eval
             kernel
             (format "import os as _emjupy_os; _emjupy_os.environ['COLUMNS'] = '%d'; del _emjupy_os"
                     columns)
             #'ignore)
            (setq emjupy--kernel-width-told
                  (cons (emjupy-kernel-id kernel) columns))))))))

(defun emjupy--ensure-kernel-width (nb)
  "Tell NB's kernel the box width, unless it already has this one."
  (let ((kernel (and nb (emjupy-notebook-kernel nb))))
    (when kernel
      (unless (equal emjupy--kernel-width-told
                     (cons (emjupy-kernel-id kernel) (emjupy--box-width)))
        (emjupy--tell-kernel-width nb)))))

;;;; Reconnecting after a drop

(defun emjupy--restore-way-to (server)
  "Run `emjupy-reconnect-functions' for SERVER, reporting what fails."
  (when server
    (with-demoted-errors "[emjupy] Restoring the way to the server: %S"
      (run-hook-with-args 'emjupy-reconnect-functions server))))

(defun emjupy--end-runs (cells buf label why)
  "End CELLS in BUF, for the notebook called LABEL, and say WHY."
  (when (buffer-live-p buf)
    (dolist (cell cells) (emjupy--mark-done cell buf)))
  (message "[emjupy] %s: %s%s." label why
           (if cells (format "; %d running cell%s stopped" (length cells)
                             (if (cdr cells) "s" ""))
             "")))

(defun emjupy--kernel-connection-closed (kernel label)
  "Note that KERNEL's connection closed, LABEL naming its notebook.
Recovers as below, then says so in the mode line: reconnecting, or not."
  (emjupy--kernel-connection-closed-1 kernel label)
  (emjupy--kernel-told kernel))

(defun emjupy--kernel-connection-closed-1 (kernel label)
  "The WebSocket to KERNEL, for the notebook called LABEL, has closed.
Closed by emjupy, or with reconnecting off, cells still running on it
are ended.  Dropped, they keep running and emjupy reconnects for
`emjupy-reconnect-window' seconds; the server replays what came
meanwhile to the same session."
  (let* ((pending (emjupy-kernel-pending kernel))
         (nb (emjupy-kernel-notebook kernel))
         (buf (and nb (emjupy-notebook-buffer nb)))
         (cells nil))
    (when (hash-table-p pending)
      (maphash (lambda (_id cell) (push cell cells)) pending))
    (cond
     ;; A connection that never came up, while reconnecting: the timer
     ;; tries again, and the kept cells stay kept.
     ((and (buffer-live-p buf)
           (buffer-local-value 'emjupy--reconnect-timer buf)))
     ((or (emjupy-kernel-closing kernel) (not emjupy-reconnect-window)
          (not (buffer-live-p buf)) (not (emjupy-kernel-id kernel)))
      (when (hash-table-p pending)
        (maphash (lambda (id _) (remhash id emjupy--request-halves)) pending)
        (clrhash pending))
      (emjupy--end-runs cells buf label "kernel connection closed"))
     (t
      (with-current-buffer buf
        (let ((kept (make-hash-table :test 'equal)))
          (when (hash-table-p pending)
            (maphash (lambda (id cell) (puthash id cell kept)) pending)
            (clrhash pending))
          (setq emjupy--kept-runs (cons (emjupy-kernel-id kernel) kept)))
        (setq emjupy--reconnect-timer
              (run-with-timer 3 3 #'emjupy--try-reconnect buf
                              (emjupy-kernel-id kernel)
                              (emjupy-kernel-name kernel)
                              (+ (float-time) emjupy-reconnect-window) label)))
      (message "[emjupy] %s: kernel connection dropped; reconnecting%s..." label
               (if cells (format ", %d running cell%s kept" (length cells)
                                 (if (cdr cells) "s" ""))
                 ""))))))

(defun emjupy--run-waiting-cells (nb)
  "Run the cells of NB run while its connection was being restored.
In the order they were run, each as it reads now; one deleted meanwhile
is not run."
  (let ((cells emjupy--runs-waiting))
    (setq emjupy--runs-waiting nil)
    (dolist (cell cells)
      (when (memq cell (append (emjupy-notebook-cells nb) nil))
        (save-excursion
          (emjupy--goto-cell cell 'beginning)
          (emjupy-execute-cell-at-point))))))

;; The reconnect loop -- connecting sets a close handler, which starts the
;; timer below, which connects again -- is a cycle on purpose: retrying is
;; one.  It runs through a callback and a timer, never as recursion.

(defun emjupy--try-reconnect (buf kernel-id kernel-name deadline label)
  "Try to reconnect BUF's notebook to KERNEL-ID, named KERNEL-NAME, once.
Run every few seconds after its connection dropped, until it is back or
DEADLINE passes; then the kept cells, for the notebook called LABEL, are
ended as a closed connection ends them."
  (if (not (buffer-live-p buf))
      (cancel-function-timers #'emjupy--try-reconnect)
    (with-current-buffer buf
      (let* ((nb emjupy--buffer-notebook)
             (kernel (and nb (emjupy-notebook-kernel nb))))
        (cond
         ;; back: by this, or by hand
         ((and kernel (emjupy--ws-live-p kernel)
               (equal (emjupy-kernel-id kernel) kernel-id))
          (cancel-timer emjupy--reconnect-timer)
          (setq emjupy--reconnect-timer nil)
          (message "[emjupy] %s: reconnected to kernel %s." label kernel-id)
          (emjupy--run-waiting-cells nb))
         ((> (float-time) deadline)
          (cancel-timer emjupy--reconnect-timer)
          (setq emjupy--reconnect-timer nil)
          (let ((cells nil))
            (when emjupy--kept-runs
              (maphash (lambda (id cell) (push cell cells)
                         (remhash id emjupy--request-halves))
                       (cdr emjupy--kept-runs)))
            (setq emjupy--kept-runs nil)
            (when emjupy--runs-waiting
              (let ((n (length emjupy--runs-waiting)))
                (message "[emjupy] %s: %d cell%s run while reconnecting did not run."
                         label n (if (= n 1) "" "s")))
              (setq emjupy--runs-waiting nil))
            (emjupy--end-runs
             cells buf label
             (substitute-command-keys
              (if emjupy--server-silent
                  "could not reconnect: the Jupyter server does not answer, and may have stopped (for a notebook opened through TRAMP, \\[emjupy-open-this-notebook] in its file starts it again)"
                "could not reconnect (\\[emjupy-reconnect-kernel] tries again)")))))
         (t
          ;; Not waited for: a socket that comes up is found live next time; one
          ;; that does not closes, and this tries again.  Any error counts as
          ;; one more try: `websocket-open' reports a refused connection as a
          ;; plain error.
          (emjupy--restore-way-to (emjupy-notebook-server nb))
          (emjupy--check-kernel-still-there buf (emjupy-notebook-server nb)
                                            kernel-id label)
          (condition-case nil
              (emjupy-connect-kernel nb kernel-id kernel-name)
            (error nil))))))))

(defun emjupy--adopt-kept-runs (kernel)
  "Hand KERNEL the requests kept when its notebook's connection dropped.
A little later, a cell still running is checked: if the kernel is idle,
the run ended while disconnected, and the cell is ended too."
  (let* ((nb (emjupy-kernel-notebook kernel))
         (buf (and nb (emjupy-notebook-buffer nb))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (when (and emjupy--kept-runs (equal (car emjupy--kept-runs)
                                            (emjupy-kernel-id kernel)))
          (let ((ids nil))
            (maphash (lambda (id cell)
                       (puthash id cell (emjupy-kernel-pending kernel))
                       (push id ids))
                     (cdr emjupy--kept-runs))
            (setq emjupy--kept-runs nil)
            (run-with-timer 4 nil #'emjupy--settle-adopted-runs kernel ids)))))))

(defun emjupy--settle-adopted-runs (kernel ids)
  "Settle IDS, requests KERNEL adopted after a drop: end them if it is idle.
They ended unseen, while the connection was down.  Only these: a run
sent since may have finished on the kernel with its output still on the
way, and ending it would lose that output.  A kernel heard from lately
is working through them -- idle a moment between two queued cells --
and is asked again later."
  (let* ((nb (emjupy-kernel-notebook kernel))
         (buf (and nb (emjupy-notebook-buffer nb)))
         (pending (emjupy-kernel-pending kernel))
         (left (and (hash-table-p pending)
                    (seq-filter (lambda (id) (gethash id pending)) ids))))
    (when (and (buffer-live-p buf) left
               (< (- (float-time) (or (emjupy-kernel-heard kernel) 0)) 3))
      (run-with-timer 4 nil #'emjupy--settle-adopted-runs kernel ids)
      (setq left nil))
    (when (and (buffer-live-p buf) left)
      (let ((state (condition-case nil
                       (gethash "execution_state"
                                (emjupy--http-request
                                 "GET" (emjupy-notebook-server nb)
                                 (format "/api/kernels/%s"
                                         (emjupy-kernel-id kernel))))
                     ;; unknown: the cells stay running, as they may be
                     (emjupy-http-error nil))))
        (when (equal state "idle")
          (let ((cells nil))
            (dolist (id left)
              (push (gethash id pending) cells)
              (remhash id pending)
              (remhash id emjupy--request-halves))
            (emjupy--end-runs (delete-dups cells) buf (emjupy-notebook-path nb)
                              "a run ended while the connection was down, its output perhaps incomplete")))))))

(defun emjupy--ws-session-id (notebook kernel-id)
  "Return the session id NOTEBOOK's connections to KERNEL-ID go under.
The same each time, in this Emacs: the server keeps a session's messages
while its connection is down, and replays them to the next connection
of that session -- one with a new id, every connection did, got none."
  (let ((h (md5 (format "%s|%s|%s" emjupy--session-id kernel-id
                        (or (emjupy-notebook-path notebook) "")))))
    (format "%s-%s-%s-%s-%s" (substring h 0 8) (substring h 8 12)
            (substring h 12 16)
            (substring h 16 20) (substring h 20 32))))

;;;; Is the kernel still there?

(defun emjupy--check-kernel-still-there (buf server kernel-id label)
  "Ask SERVER, without waiting, whether KERNEL-ID, BUF's kernel, is there.
Gone -- died, or shut down -- BUF stops reconnecting at once, saying so
for the notebook called LABEL.  No answer is noted, for the message if
the reconnecting gives up."
  (when server
    (emjupy--http-request-async
     "GET" server (concat "/api/kernels/" kernel-id) nil
     (lambda (_)
       (when (buffer-live-p buf)
         (with-current-buffer buf (setq emjupy--server-silent nil))))
     (lambda (err)
       (when (buffer-live-p buf)
         (with-current-buffer buf
           (if (and (eq (car err) 'emjupy-http-status)
                    (string-match-p "HTTP 404" (format "%s" (cadr err))))
               (emjupy--kernel-gone buf kernel-id label)
             (setq emjupy--server-silent t))))))))

(defun emjupy--kernel-gone (buf kernel-id label)
  "Let BUF go of KERNEL-ID, which the server no longer has.
Reconnecting to it stops; the cells running on it, or kept for it, end;
those waiting to run do not run; its connection, if open, closes; and
the notebook called LABEL is left without a kernel, for a new one."
  (with-current-buffer buf
    (when (timerp emjupy--reconnect-timer)
      (cancel-timer emjupy--reconnect-timer))
    (setq emjupy--reconnect-timer nil)
    (let ((cells nil))
      (when emjupy--kept-runs
        (maphash (lambda (id cell) (push cell cells)
                   (remhash id emjupy--request-halves))
                 (cdr emjupy--kept-runs)))
      (setq emjupy--kept-runs nil)
      (when emjupy--runs-waiting
        (let ((n (length emjupy--runs-waiting)))
          (message "[emjupy] %s: %d cell%s run while reconnecting did not run."
                   label n (if (= n 1) "" "s")))
        (setq emjupy--runs-waiting nil))
      (let* ((nb emjupy--buffer-notebook)
             (kernel (and nb (emjupy-notebook-kernel nb))))
        (when (and kernel (equal (emjupy-kernel-id kernel) kernel-id))
          (let ((pending (emjupy-kernel-pending kernel)))
            (when (hash-table-p pending)
              (maphash (lambda (id cell) (push cell cells)
                         (remhash id emjupy--request-halves))
                       pending)
              (clrhash pending)))
          (emjupy--disconnect-kernel nb)
          (setf (emjupy-notebook-kernel nb) nil)))
      (emjupy--end-runs
       cells buf label
       (substitute-command-keys
        (format "kernel %s is gone from the server -- it died, or was shut down; \\[emjupy-connect-kernel-interactive] starts a new one"
                kernel-id)))))
  (when (buffer-live-p buf)
    (with-current-buffer buf (force-mode-line-update))))

(defvar emjupy--quiet-check-timer nil
  "The timer asking after quiet kernels, while a cell runs anywhere.")

(defun emjupy--watch-quiet-kernels ()
  "Have kernels running cells asked after, should they go quiet.
The timer ticks every 5 seconds, each tick reading
`emjupy-kernel-check-interval' as it is then: set while a cell runs, it
counts at once."
  (when (and emjupy-kernel-check-interval
             (not (timerp emjupy--quiet-check-timer)))
    (setq emjupy--quiet-check-timer
          (run-with-timer 5 5 #'emjupy--check-quiet-kernels))))

(defun emjupy--check-quiet-kernels ()
  "Ask the server after each kernel running a cell, quiet for too long.
Quiet means nothing from it for `emjupy-kernel-check-interval' seconds.
The timer calling this stops once no cell runs anywhere."
  (let ((running nil))
    (dolist (buf (emjupy--notebook-buffers))
      (let* ((nb (buffer-local-value 'emjupy--buffer-notebook buf))
             (kernel (and nb (emjupy-notebook-kernel nb)))
             (pending (and kernel (emjupy-kernel-pending kernel))))
        (when (and (hash-table-p pending) (> (hash-table-count pending) 0)
                   (emjupy--ws-live-p kernel))
          (setq running t)
          (when (and emjupy-kernel-check-interval
                     (>= (- (float-time) (or (emjupy-kernel-heard kernel) 0))
                         emjupy-kernel-check-interval))
            (emjupy--check-kernel-still-there buf (emjupy-notebook-server nb)
                                              (emjupy-kernel-id kernel)
                                              (emjupy-notebook-path nb))))))
    (unless (and running emjupy-kernel-check-interval)
      (when (timerp emjupy--quiet-check-timer)
        (cancel-timer emjupy--quiet-check-timer))
      (setq emjupy--quiet-check-timer nil))))

;;;; Running cells

(cl-defun emjupy-execute-cell-at-point ()
  "Sync cell code and send to this notebook's kernel for execution."
  (interactive)
  ;; A markdown cell has nothing to run.  Jupyter treats running one as
  ;; rendering it, and sending its prose to Python produces a syntax error
  ;; from text that was never code.
  (when (use-region-p)
    (emjupy--run-region)
    (cl-return-from emjupy-execute-cell-at-point nil))
  (let ((here (emjupy--cell-at-point)))
    (when (and here (eq (emjupy-cell-type here) 'markdown))
      (emjupy--redraw-markdown-cell here)
      (cl-return-from emjupy-execute-cell-at-point here))
    ;; A raw cell is not run, as in Jupyter: it is for other tools.
    (when (and here (eq (emjupy-cell-type here) 'raw))
      (cl-return-from emjupy-execute-cell-at-point here)))
  (let ((kernel (emjupy--kernel)))
    ;; Reconnecting after a dropped connection: the kernel is there, and the
    ;; cell runs on it when the connection is back.
    (when (and (not (emjupy--ws-live-p kernel)) emjupy--reconnect-timer)
      (let ((cell (emjupy--cell-at-point)))
        (unless cell
          (user-error "No cell found at point"))
        (unless (memq cell emjupy--runs-waiting)
          (setq emjupy--runs-waiting (append emjupy--runs-waiting (list cell))))
        (message "[emjupy] Reconnecting to the kernel; the cell runs when it is back.")
        (cl-return-from emjupy-execute-cell-at-point cell)))
    (when (and kernel (emjupy-kernel-id kernel)
               (not (emjupy--ws-live-p kernel)))
      (user-error "The connection to this notebook's kernel is down: %s reconnects"
                  (substitute-command-keys "\\[emjupy-reconnect-kernel]")))
    (unless (emjupy--ws-live-p kernel)
      (user-error "This notebook has no kernel! Use %s to select or start one"
                  (substitute-command-keys "\\[emjupy-connect-kernel-interactive]")))

    (let* ((cell (emjupy--cell-at-point)))
      (unless cell
        (user-error "No cell found at point"))

      ;; 1.  Sync ALL cells: the code sent is read from the cell, and a fallback
      ;; redraw rebuilds the notebook from the cells.
      (unless emjupy--cells-synced (emjupy--sync-all-cells))
      (run-hook-with-args 'emjupy-before-execute-functions cell)

      ;; 2.  Clear the previous output, in place as C-c C-l does: a full redraw
      ;; would move every position held in the buffer -- markers, other windows'
      ;; points -- and record an undo step for a command that edits nothing.
      (setf (emjupy-cell-outputs cell) [])
      (unless (emjupy--redraw-cell-output cell)
        (emjupy--redraw-notebook cell))

      ;; 3. Build execution payload
      (let* ((code (emjupy-cell-source cell))
             (req (emjupy--make-execute-request code :allow-stdin t))
             (msg-id (car req))
             (json-payload (cdr req)))

        ;; The kernel handles requests in order, so the width is told first, if
        ;; it is new, and the cell runs with it.
        (emjupy--ensure-kernel-width emjupy--buffer-notebook)
        ;; The cell's output is this run's from now on: what an earlier run
        ;; of it still sends -- one kept through a dropped connection, its
        ;; rest replayed -- is not added to it, as in Jupyter's frontends.
        (let ((pending (emjupy-kernel-pending kernel)) (earlier nil))
          (maphash (lambda (id c) (when (eq c cell) (push id earlier))) pending)
          (dolist (id earlier)
            (remhash id pending)
            (remhash id emjupy--request-halves))
          (puthash msg-id cell pending))
        (emjupy--ws-send json-payload kernel)
        ;; Its outputs from now on are the user's own, and drawn.
        (emjupy--mark-cell-trusted cell)
        (setf (emjupy-cell-size-hidden cell) nil)
        ;; Quiet from now on, it is asked after on the server.
        (setf (emjupy-kernel-heard kernel) (float-time))
        (emjupy--watch-quiet-kernels)
        ;; Show that it is running.  Long cells otherwise look identical to
        ;; cells that were never run: an empty box and no execution count.
        (emjupy--mark-running cell)))))

(cl-defun emjupy-execute-cell-and-goto-next (&optional n)
  "Execute the cell at point, then move to the start of the next cell.
With a numeric prefix N, do so N times: \\[universal-argument] 5
\\[emjupy-execute-cell-and-goto-next] runs five cells.

If this is the last cell, a new empty one is made below and point goes
there -- so running the last cell leaves you ready to write the next
thing rather than stranded at the bottom.  With N, only after the last
of them: running past the end stops there.

Point is taken from the cell list rather than by walking the buffer: the
executed cell may still be redrawing as its output arrives, and
stepping over overlays mid-flight lands in the output box."
  (interactive "p")
  (when (use-region-p)
    (emjupy--run-region)
    (cl-return-from emjupy-execute-cell-and-goto-next nil))
  (catch 'end
    (dotimes (i (max 1 (or n 1)))
      (let* ((cell (emjupy--cell-at-point)))
        (unless cell (user-error "Point is not in a cell"))
        (emjupy-execute-cell-at-point)
        (let* ((nb (emjupy--notebook))
               (cells (append (emjupy-notebook-cells nb) nil))
               (idx (cl-position cell cells))
               (next (and idx (nth (1+ idx) cells))))
          (cond ((and next (overlayp (emjupy-cell-overlay next)))
                 (goto-char (overlay-start (emjupy-cell-overlay next))))
                ((= i (1- (max 1 (or n 1))))
                 (emjupy-insert-cell-below))
                (t (throw 'end nil))))))))

(defun emjupy--run-cells (cells)
  "Run CELLS in order, as each would be run from its own place.
Raw cells are left alone, as Jupyter does; markdown ones are rendered.
Point stays where it was."
  (emjupy--sync-all-cells)
  (save-excursion
    ;; Synced once, the rules rebuilt once, for all of them.
    (let ((emjupy--cells-synced t))
      (emjupy--with-rules-once
        (dolist (cell cells)
          (unless (eq (emjupy-cell-type cell) 'raw)
            (emjupy--goto-cell cell 'beginning)
            (emjupy-execute-cell-at-point)))))))

(defun emjupy--run-region ()
  "Run the cells the region touches, in order: Jupyter's run selected cells.
The region goes first: the cells' runs, each from its own place, are of
one cell each."
  (let ((cells (emjupy--cells-between (region-beginning) (region-end))))
    (deactivate-mark)
    (emjupy--run-cells cells)))

(defun emjupy-run-all-cells ()
  "Run every cell of the notebook, top to bottom, as JupyterLab's Run All."
  (interactive)
  (emjupy--run-cells (append (emjupy-notebook-cells (emjupy--notebook)) nil)))

(defun emjupy-run-cells-above ()
  "Run every cell above the one at point, not it, as JupyterLab does."
  (interactive)
  (let* ((cell (or (emjupy--cell-at-point)
                   (user-error "Point is not in a cell")))
         (cells (append (emjupy-notebook-cells (emjupy--notebook)) nil)))
    (emjupy--run-cells (seq-take cells (cl-position cell cells)))))

(defun emjupy-run-cells-below ()
  "Run the cell at point and every one below it, as JupyterLab does."
  (interactive)
  (let* ((cell (or (emjupy--cell-at-point)
                   (user-error "Point is not in a cell")))
         (cells (append (emjupy-notebook-cells (emjupy--notebook)) nil)))
    (emjupy--run-cells (nthcdr (cl-position cell cells) cells))))

;; Everything touching a live WebSocket goes through these two functions, each
;; for one kernel: a stale value gets a plain "no kernel" rather than an error
;; inside websocket.el, and tests have one place to substitute a fake transport.

;;;; Interrupting and restarting

(defun emjupy-interrupt-kernel ()
  "Interrupt whatever this notebook's kernel is running.

The equivalent of an interrupt signal in a terminal: the running cell
stops with a
KeyboardInterrupt and the session -- variables, imports, everything --
is left alone.  Use \\[emjupy-restart-kernel] when you want the session
itself thrown away."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (kernel (emjupy-notebook-kernel nb)))
    (unless (and kernel (emjupy-kernel-id kernel))
      (user-error "This notebook has no kernel"))
    (emjupy--http-request "POST" (emjupy-notebook-server nb)
                          (format "/api/kernels/%s/interrupt"
                                  (emjupy-kernel-id kernel)))
    (message "[emjupy] Interrupt sent to kernel %s."
             (emjupy-kernel-id kernel))))

(defun emjupy-restart-kernel ()
  "Restart THIS notebook's kernel and reconnect its websocket.
Other open notebooks, and their kernels, are untouched."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (kernel (emjupy-notebook-kernel nb)))
    (unless (and kernel (emjupy-kernel-id kernel))
      (user-error "This notebook has no kernel! Use %s to select or start one"
                  (substitute-command-keys "\\[emjupy-connect-kernel-interactive]")))
    ;; Ask first: a restart throws away every variable in the session, and
    ;; there is no undo for that.  C-c C-x C-r is one slip from C-c C-x C-c,
    ;; which merely reconnects.
    (unless (y-or-n-p
             (format "Restart the kernel for %s -- all variables will be lost?"
                     (emjupy-notebook-path nb)))
      (user-error "Kernel left running"))
    (emjupy--restart-kernel-1 nb)))

(defun emjupy--restart-kernel-1 (nb)
  "Restart NB's kernel and reconnect its websocket; nothing asked."
  (let ((kernel (emjupy-notebook-kernel nb)))
    (let ((kernel-id (emjupy-kernel-id kernel))
          (server (emjupy-notebook-server nb))
          (name (emjupy-kernel-name kernel)))
      ;; Old in-flight requests will never get a reply from the restarted
      ;; kernel process, so drop them rather than leave them pending forever.
      (emjupy--disconnect-kernel nb)
      (emjupy--http-request "POST" server (format "/api/kernels/%s/restart"
                                                  kernel-id))
      (message "[emjupy] Kernel %s restarting..." kernel-id)
      (emjupy-connect-kernel nb kernel-id name))))

(defun emjupy--kernel-or-say (nb)
  "Return NB's kernel, or say how to have one."
  (let ((kernel (emjupy-notebook-kernel nb)))
    (unless (and kernel (emjupy-kernel-id kernel))
      (user-error "This notebook has no kernel! Use %s to select or start one"
                  (substitute-command-keys
                   "\\[emjupy-connect-kernel-interactive]")))
    kernel))

(defun emjupy--await-connection (nb)
  "Wait, at most 30 s, for NB's kernel to be connected; say if not."
  (let ((deadline (+ (float-time) 30)))
    (while (and (not (emjupy--ws-live-p (emjupy-notebook-kernel nb)))
                (< (float-time) deadline))
      (accept-process-output nil 0.1))
    (unless (emjupy--ws-live-p (emjupy-notebook-kernel nb))
      (user-error "The kernel restarted, but is not connected yet"))))

(defun emjupy-restart-kernel-and-run-all ()
  "Restart this notebook's kernel, then run every cell, as JupyterLab does."
  (interactive)
  (let ((nb (emjupy--notebook)))
    (emjupy--kernel-or-say nb)
    (unless (y-or-n-p (format "Restart the kernel for %s and run all cells -- all variables will be lost?"
                              (emjupy-notebook-path nb)))
      (user-error "Kernel left running"))
    (emjupy--restart-kernel-1 nb)
    (emjupy--await-connection nb)
    (emjupy-run-all-cells)))

(defun emjupy-restart-kernel-and-clear-outputs ()
  "Restart this notebook's kernel, and clear every cell's output."
  (interactive)
  (let ((nb (emjupy--notebook)))
    (emjupy--kernel-or-say nb)
    (unless (y-or-n-p (format "Restart the kernel for %s and clear all outputs -- all variables will be lost?"
                              (emjupy-notebook-path nb)))
      (user-error "Kernel left running"))
    (emjupy--restart-kernel-1 nb)
    (emjupy-clear-all-outputs)))

(defun emjupy-shutdown-kernel ()
  "Shut this notebook's kernel down, on its server.
The notebook stays as it is; only the kernel's state is lost."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (kernel (emjupy--kernel-or-say nb))
         (kernel-id (emjupy-kernel-id kernel)))
    (unless (y-or-n-p (format "Shut down the kernel of %s -- all variables will be lost?"
                              (emjupy-notebook-path nb)))
      (user-error "Kernel left running"))
    (emjupy--disconnect-kernel nb)
    (emjupy--http-request "DELETE" (emjupy-notebook-server nb)
                          (format "/api/kernels/%s" kernel-id))
    (setf (emjupy-notebook-kernel nb) nil)
    (force-mode-line-update)
    (message "[emjupy] Kernel %s shut down; %s starts another" kernel-id
             (substitute-command-keys "\\[emjupy-connect-kernel-interactive]"))))

(defun emjupy--repair-if-scrambled (nb)
  "Redraw NB if its buffer and cells have come out of step.
A socket dying mid-render, as suspending the machine does to a tunnel,
can leave the buffer unlike the cells.  Return the problems found, or
nil."
  (when-let* ((buf (emjupy-notebook-buffer nb)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((problems (emjupy--check-invariants)))
          (when problems
            (emjupy--redraw-notebook)
            (message "[emjupy] Buffer and cells were out of step after reconnecting; redrawn.")
            problems))))))

(defun emjupy-reconnect-kernel ()
  "Reopen this notebook's WebSocket to the SAME kernel.
For when the tunnel dropped but the remote kernel kept running: the
kernel's state is intact, only Emacs's socket needs re-establishing."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (kernel (emjupy-notebook-kernel nb)))
    (unless (and kernel (emjupy-kernel-id kernel))
      (user-error "This notebook has no kernel to reconnect to"))
    (let ((id (emjupy-kernel-id kernel))
          (name (emjupy-kernel-name kernel))
          (label (emjupy-notebook-path nb)))
      (emjupy--disconnect-kernel nb)
      (emjupy--restore-way-to (emjupy-notebook-server nb))
      (condition-case err
          (emjupy-connect-kernel nb id name)
        (error
         ;; The way to the server may be coming back -- a tunnel reopening:
         ;; keep trying, as after a dropped connection.
         (unless emjupy-reconnect-window
           (user-error "Could not reach the kernel's server: %s"
                       (error-message-string err)))
         (when (timerp emjupy--reconnect-timer)
           (cancel-timer emjupy--reconnect-timer))
         (setq emjupy--reconnect-timer
               (run-with-timer 3 3 #'emjupy--try-reconnect (current-buffer)
                               id name
                               (+ (float-time) emjupy-reconnect-window)
                               label))))
      (emjupy--repair-if-scrambled nb)
      (message "[emjupy] Reconnecting %s to kernel %s..." label id))))

;;;; In a notebook buffer

(defun emjupy-kernel-enable ()
  "Wire the kernel layer into the current notebook buffer.
The kernel is told how wide to draw when it attaches, and whenever the
width changes."
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--tell-kernel-width)
  (add-hook 'emjupy-box-width-changed-functions #'emjupy--tell-kernel-width))

(provide 'emjupy-kernel)
;;; emjupy-kernel.el ends here
