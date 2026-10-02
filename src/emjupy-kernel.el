;;; emjupy-kernel.el --- Kernel WebSocket transport and execution for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The WebSocket half of the Jupyter transport: connecting a notebook to a
;; kernel, sending execute_requests, and routing incoming frames back to
;; the notebook that asked for them.
;;
;; Each kernel owns its own socket and its own pending-request table, so
;; several notebooks -- and several servers -- stay live at once without
;; cross-talk.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'websocket)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-cells)

(defun emjupy--uuid ()
  "Generate a pseudo-random UUID v4 string."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536) (random 4096)
          (logior (random 4096) #x8000)
          (random 65536) (random 65536) (random 65536)))

(defvar emjupy--session-id (emjupy--uuid)
  "Unique identifier for the current Emacs session.")

(defun emjupy--make-execute-request (code)
  "Return a Jupyter `execute_request' payload for CODE.
The value is a cons of the message id and the serialized message."
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
    (puthash "allow_stdin" :false content)
    (puthash "stop_on_error" t content)

    (puthash "header" header msg)
    (puthash "parent_header" (make-hash-table) msg)
    (puthash "channel" "shell" msg)
    (puthash "metadata" (make-hash-table) msg)
    (puthash "content" content msg)
    (puthash "buffers" [] msg)

    (cons msg-id (json-serialize msg))))

(defcustom emjupy-output-render-interval 0.15
  "Seconds to let cell output accumulate before redrawing.

A redraw rebuilds the whole notebook, so doing one per arriving message
is the difference between a progress bar and a frozen Emacs: a
`tqdm\' loop sends a message per iteration, and each redraw of a
notebook with figures costs tens of milliseconds.  Coalescing them
bounds the cost to one redraw per interval however fast the output
comes."
  :type 'number
  :group 'emjupy)

(defvar-local emjupy--render-timer nil
  "Timer coalescing pending output redraws in this buffer.")

(defun emjupy--schedule-render (buffer)
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
                     ;; Rendering runs from a timer now rather than inside
                     ;; the WebSocket callback, but the same reasoning
                     ;; applies: an un-renderable output must not leave the
                     ;; box silently blank with nothing logged.
                     (condition-case err
                         (emjupy--redraw-pending-output)
                       (error
                        (message "[emjupy] Failed to render cell output: %s"
                                 (error-message-string err)))))))))))))

(defun emjupy--stream-p (output &optional name)
  "Non-nil if OUTPUT is stream output, on stream NAME when that is given."
  (and output
       (equal (gethash "output_type" output) "stream")
       (or (null name) (equal (gethash "name" output) name))))

(defun emjupy--open-stream-output (existing name)
  "Return the output in EXISTING that new text on stream NAME continues, or nil.

The last output, if it is on NAME: consecutive output on one stream is
one stream, as in Jupyter.  Otherwise the most recent output on NAME,
provided its last line is still open -- it did not end with a newline.

That second case is a progress bar.  `tqdm\=' writes to stderr, and each
update begins with a carriage return: go back to the start of the line I
am on and overwrite it.  A `print\=' in the loop writes to stdout between
those updates, so they were never consecutive, never merged, and every
update became an output of its own -- a column of bars, each showing its
carriage return as a literal ^M, instead of one bar being redrawn.  A
terminal keeps each stream\='s line open until a newline closes it, and
this does the same.  A line that has been closed is left alone, so text
that genuinely came later is not moved above what came before it."
  (let ((last (car (last existing))))
    (if (emjupy--stream-p last name)
        last
      (let ((recent (seq-find (lambda (o) (emjupy--stream-p o name))
                              (reverse existing))))
        (and recent
             (not (string-suffix-p "\n" (emjupy--mime-text (gethash "text" recent))))
             recent)))))

(defun emjupy--merge-stream-output (existing output-hash)
  "Return EXISTING with OUTPUT-HASH merged in, or nil if it cannot be.

Stream text continues the output it belongs to rather than starting a
new one -- see `emjupy--open-stream-output\=' for which that is.  Keeping
each message separately is what let a `tqdm\=' bar leave thousands of
entries behind, every one walked on every redraw, so the notebook got
slower for the rest of the session."
  (when (emjupy--stream-p output-hash)
    (let ((target (emjupy--open-stream-output existing (gethash "name" output-hash))))
      (when target
        (puthash "text"
                 (emjupy--collapse-carriage-returns
                  (concat (emjupy--mime-text (gethash "text" target))
                          (emjupy--mime-text (gethash "text" output-hash))))
                 target)
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

Falls back to rebuilding the notebook when the buffer and the cells have
come out of step -- a cell added or removed, or a render interrupted --
since patching one region only makes sense while the rest still matches."
  (let ((cells emjupy--cells-awaiting-output))
    (setq emjupy--cells-awaiting-output nil)
    ;; Keep the structs matching the buffer.  The full redraw used to do
    ;; this on the way past; patching one region does not touch the other
    ;; cells, so an edit made while output was in flight would otherwise sit
    ;; in the buffer and never reach the cell it belongs to.
    (when cells (emjupy--sync-all-cells))
    (if (and cells (emjupy--overlays-sane-p)
             (cl-every (lambda (cell) (emjupy--refresh-cell-output cell)) cells))
        t
      (emjupy--rerender-preserving-point))))

(defvar emjupy-comm-functions nil
  "Functions called with each comm message from a kernel.
Each is called with the KERNEL, the message type -- \"comm_open\",
\"comm_msg\" or \"comm_close\" -- the message content, and the binary
buffers the message carried, a list of `emjupy-bytes\='.  This is how
the layer that knows about widgets hears of them.")

(defvar emjupy-output-owner-functions nil
  "Functions asked which cell output answering a request belongs to.
Each is called with the request\'s message id and returns a cell or nil.
Asked only for a request no cell is waiting on: one emjupy sent itself,
such as moving a widget\'s slider, whose redrawn figure belongs to the
cell showing the widget.")

(defvar emjupy--clear-before-next-output (make-hash-table :test 'eq :weakness 'key)
  "Cells whose outputs are cleared when their next output arrives.")

(defun emjupy--widget-view-output-p (output)
  "Return non-nil if OUTPUT is the view of a widget."
  (let ((data (and (hash-table-p output) (gethash "data" output))))
    (and (hash-table-p data) (gethash emjupy--widget-view-mime data))))

(defun emjupy--clear-cell-outputs-now (cell notebook)
  "Clear CELL's outputs in NOTEBOOK, as a kernel's `clear_output' asks.
Widgets stay: in a notebook a widget\'s own output area is what an
`interact\=' clears before drawing again, and the controls are outside
it.  Here the outputs are one list, so the widgets are what is kept."
  (setf (emjupy-cell-outputs cell)
        (vconcat (seq-filter #'emjupy--widget-view-output-p
                             (append (or (emjupy-cell-outputs cell) []) nil))))
  (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook))))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (cl-pushnew cell emjupy--cells-awaiting-output))
      (emjupy--schedule-render buf))))

(defun emjupy--make-message (msg-type content)
  "Return a shell-channel message of MSG-TYPE carrying CONTENT, a hash table.
The value is a cons of the message id and the serialized message."
  (let ((msg-id (emjupy--uuid))
        (header (make-hash-table :test 'equal))
        (msg (make-hash-table :test 'equal)))
    (puthash "msg_id" msg-id header)
    (puthash "username" "emacs" header)
    (puthash "session" emjupy--session-id header)
    (puthash "msg_type" msg-type header)
    (puthash "version" "5.3" header)
    (puthash "header" header msg)
    (puthash "parent_header" (make-hash-table) msg)
    (puthash "channel" "shell" msg)
    (puthash "metadata" (make-hash-table) msg)
    (puthash "content" content msg)
    (puthash "buffers" [] msg)
    (cons msg-id (json-serialize msg))))

(defun emjupy--append-output-to-cell (cell output-hash &optional notebook)
  "Append OUTPUT-HASH to CELL outputs and refresh NOTEBOOK\='s buffer."
  ;; `clear_output\=' with wait: the clear happens now, as the new output
  ;; arrives, so the old one stays up until there is something to replace it
  (when (gethash cell emjupy--clear-before-next-output)
    (remhash cell emjupy--clear-before-next-output)
    (setf (emjupy-cell-outputs cell)
          (vconcat (seq-filter #'emjupy--widget-view-output-p
                               (append (or (emjupy-cell-outputs cell) []) nil)))))
  (let* ((existing (append (or (emjupy-cell-outputs cell) []) nil))
         (merged (emjupy--merge-stream-output existing output-hash)))
    ;; Stream text that starts an output of its own still has its carriage
    ;; returns applied.  The first update of a progress bar arrives like
    ;; that -- nothing before it to merge into -- and rendering it as it came
    ;; put a literal ^M at the start of the line.
    (when (and (not merged) (emjupy--stream-p output-hash))
      (puthash "text"
               (emjupy--collapse-carriage-returns
                (emjupy--mime-text (gethash "text" output-hash)))
               output-hash))
    (setf (emjupy-cell-outputs cell)
          (vconcat (or merged (append existing (list output-hash)))))
    (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (cl-pushnew cell emjupy--cells-awaiting-output)))
      (emjupy--schedule-render buf))))

(cl-defstruct (emjupy-bytes (:constructor emjupy-bytes-make (data)))
  "Bytes a kernel message carried in binary: an image widget\'s, say.
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
Return a cons of the message\'s JSON text and a list of the buffers, as
unibyte strings.  The format is the Jupyter server\'s: a count of parts,
the offset of each, then the parts -- the JSON first, then the buffers."
  (let* ((n (emjupy--u32 bytes 0))
         (offsets (cl-loop for i from 1 to n collect (emjupy--u32 bytes (* 4 i))))
         (ends (append (cdr offsets) (list (length bytes))))
         (parts (cl-mapcar (lambda (from to) (substring bytes from to)) offsets ends)))
    (cons (decode-coding-string (car parts) 'utf-8) (cdr parts))))

(defun emjupy--encode-binary-message (json buffers)
  "Return a binary WebSocket frame of the message JSON and its BUFFERS.
JSON is the serialized message, as `json-serialize\=' gives it; BUFFERS
unibyte strings.  The format `emjupy--decode-binary-message\=' reads."
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
  "Return FRAME\'s message as a cons of its JSON text and its buffers.
A message carrying binary data comes as a binary frame, read here; any
other has no buffers."
  (if (and (websocket-frame-p frame) (eq (websocket-frame-opcode frame) 'binary))
      (emjupy--decode-binary-message (websocket-frame-payload frame))
    (cons (emjupy--ws-payload frame) nil)))

(defun emjupy--ws-send-binary (bytes kernel)
  "Send the binary frame BYTES on KERNEL\'s WebSocket."
  (websocket-send (emjupy-kernel-ws kernel)
                  (make-websocket-frame :opcode 'binary :payload bytes :completep t)))

(defun emjupy--ws-payload (frame)
  "Return the text payload of FRAME.
Accepts either a `websocket-frame' (what websocket.el hands the
callback) or a plain JSON string, so recorded kernel traffic can be
replayed straight through the handler."
  (if (websocket-frame-p frame) (websocket-frame-payload frame) frame))

(defvar emjupy--internal-requests (make-hash-table :test 'equal)
  "Message ids of emjupy's own requests, mapped to their callbacks.

Kept apart from a kernel's pending table, which maps ids to cells: these
requests belong to no cell and their output must not be rendered as if
it did.")

(defun emjupy--kernel-eval (kernel code callback)
  "Run CODE on KERNEL and pass its printed output, as printed, to CALLBACK.

For emjupy\='s own questions -- \"where are you?\" -- not for user code."
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

PART is `reply\=' for execute_reply and `idle\=' for the status message
that closes the request.  Outputs travel on the iopub channel and the
reply on the shell channel, and the protocol does not order one against
the other: an output can come after the reply.  The request used to be
forgotten on the reply, so an output arriving later found nothing waiting
for it and was dropped -- on a slow machine, routinely.  Idle is what
follows the last output, and it can itself come before the reply, so the
request is forgotten only when both have been seen, in either order.
PENDING is the kernel\'s table of requests awaiting output."
  (let ((seen (gethash id emjupy--request-halves)))
    (if (and seen (not (eq seen part)))
        (progn (remhash id emjupy--request-halves)
               (remhash id pending))
      (puthash id part emjupy--request-halves))))

(defun emjupy--handle-ws-message (kernel frame)
  "Handle an incoming WebSocket FRAME belonging to KERNEL.
KERNEL carries both the pending-request table and the backlink to the
notebook whose buffer should be refreshed, so frames from several
kernels never cross-talk."
  (let* ((parts (emjupy--ws-message-parts frame))
         (data (json-parse-string (car parts) :object-type 'hash-table :array-type 'array))
         (buffers (mapcar #'emjupy-bytes-make (cdr parts)))
         (header (gethash "header" data))
         (parent-header (gethash "parent_header" data))
         (msg-type (gethash "msg_type" header))
         (parent-id (when parent-header (gethash "msg_id" parent-header)))
         (pending (and kernel (emjupy-kernel-pending kernel)))
         (notebook (and kernel (emjupy-kernel-notebook kernel)))
         (cell (when parent-id
                 (or (and pending (gethash parent-id pending))
                     ;; output answering a request no cell waits on: one
                     ;; emjupy sent itself, such as moving a slider
                     (run-hook-with-args-until-success
                      'emjupy-output-owner-functions parent-id))))
         (internal (when parent-id (gethash parent-id emjupy--internal-requests))))

    ;; One of emjupy's own questions: hand the answer to its callback and
    ;; keep it out of the notebook entirely.
    (when internal
      ;; Gathered until the request is done, then handed over whole.  The
      ;; callback used to get the first stream message alone, which for a
      ;; one-line answer is all of it -- but a large answer comes in
      ;; several, and the rest was dropped.
      (when (string= msg-type "stream")
        (push (or (gethash "text" (gethash "content" data)) "") (cdr internal)))
      (when (emjupy--idle-p msg-type data)
        ;; Its answer, if any, has come by now: idle follows every output
        ;; of the request on the same channel.  Forgetting it on
        ;; execute_reply instead dropped an answer that arrived late.
        (remhash parent-id emjupy--internal-requests)
        (when (cdr internal)
          ;; As printed: a caller asking for a line trims it; one asking
          ;; for data, a piece of a file, needs every character.
          (funcall (car internal) (apply #'concat (reverse (cdr internal)))))))

    (when (member msg-type '("comm_open" "comm_msg" "comm_close"))
      (run-hook-with-args 'emjupy-comm-functions kernel msg-type
                          (gethash "content" data) buffers))
    (when cell
      (cond
       ((string= msg-type "clear_output")
        (if (eq (gethash "wait" (gethash "content" data)) t)
            (puthash cell t emjupy--clear-before-next-output)
          (emjupy--clear-cell-outputs-now cell notebook)))
       ;; Stdout/Stderr live streaming
       ((string= msg-type "stream")
        (let* ((content (gethash "content" data))
               (text (gethash "text" content))
               (name (gethash "name" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" out-hash)
          (puthash "name" (or name "stdout") out-hash)
          (puthash "text" (or text "") out-hash)
          (emjupy--append-output-to-cell cell out-hash notebook)))

       ;; Returned execution evaluation results
       ((string= msg-type "execute_result")
        (let* ((content (gethash "content" data))
               (data-obj (gethash "data" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "execute_result" out-hash)
          (puthash "data" (or data-obj (make-hash-table :test 'equal)) out-hash)
          ;; `metadata' and `execution_count' are REQUIRED on an
          ;; execute_result by the nbformat v4 schema; omitting them makes
          ;; the notebook we save fail `nbformat.validate' and so unusable
          ;; by nbconvert/papermill/other tools.
          (puthash "metadata" (or (gethash "metadata" content)
                                  (make-hash-table :test 'equal))
                   out-hash)
          (puthash "execution_count" (or (gethash "execution_count" content) :null)
                   out-hash)
          (emjupy--append-output-to-cell cell out-hash notebook)))

       ;; Rich display data -- this is how matplotlib's inline figure
       ;; actually arrives (separately from the execute_result text repr)
       ((string= msg-type "display_data")
        (let* ((content (gethash "content" data))
               (data-obj (gethash "data" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "display_data" out-hash)
          (puthash "data" (or data-obj (make-hash-table :test 'equal)) out-hash)
          ;; Also schema-required (see execute_result above).
          (puthash "metadata" (or (gethash "metadata" content)
                                  (make-hash-table :test 'equal))
                   out-hash)
          (emjupy--append-output-to-cell cell out-hash notebook)))

       ;; Python Error Tracebacks
       ((string= msg-type "error")
        (let* ((content (gethash "content" data))
               (ename (gethash "ename" content))
               (evalue (gethash "evalue" content))
               (traceback (gethash "traceback" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "error" out-hash)
          (puthash "ename" ename out-hash)
          (puthash "evalue" evalue out-hash)
          (puthash "traceback" traceback out-hash)
          (emjupy--append-output-to-cell cell out-hash notebook)))

       ;; The kernel has published everything this request will produce.
       ((emjupy--idle-p msg-type data)
        (emjupy--request-half-seen parent-id 'idle pending))

       ;; Finish execution frame
       ((string= msg-type "execute_reply")
        (let* ((content (gethash "content" data))
               (count (gethash "execution_count" content)))
          (setf (emjupy-cell-exec-count cell) count)
          (emjupy--request-half-seen parent-id 'reply pending)
          (when-let* ((b (and notebook (emjupy-notebook-buffer notebook))))
            (when (buffer-live-p b) (emjupy--mark-done cell b)))
          (when-let* ((buf (and notebook (emjupy-notebook-buffer notebook))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (emjupy--rerender-preserving-point))))
          ;; (message "[emjupy] %s: cell execution complete. [In: %s]"
          ;;          (if notebook (emjupy-notebook-path notebook) "notebook")
          ;;          count)
	  ))))))

(cl-defun emjupy-execute-cell-at-point ()
  "Sync cell code and send to this notebook's kernel for execution."
  (interactive)
  ;; A markdown cell has nothing to run.  Jupyter treats running one as
  ;; rendering it, and sending its prose to Python produces a syntax error
  ;; from text that was never code.
  (let ((here (emjupy--cell-at-point)))
    (when (and here (eq (emjupy-cell-type here) 'markdown))
      (emjupy--render-markdown-cell here)
      (cl-return-from emjupy-execute-cell-at-point here)))
  (let ((kernel (emjupy--kernel)))
    (unless (emjupy--ws-live-p kernel)
      (user-error "This notebook has no kernel! Use %s to select or start one"
                  (substitute-command-keys "\\[emjupy-connect-kernel-interactive]")))

    (let* ((cell (emjupy--cell-at-point)))
      (unless cell
        (user-error "No cell found at point"))

      ;; 1. Sync buffer edits back into ALL cells, not just this one: the
      ;; source sent to the kernel is read from the struct, and if the old
      ;; output cannot be cleared in place the fallback redraws the whole
      ;; notebook from the structs.
      (emjupy--sync-all-cells)

      ;; 2. Clear the previous output, so a stale box does not linger while
      ;;    the new run is in flight.  In place, as C-c C-l does: only this
      ;;    cell's output changes, and redrawing the whole notebook for it
      ;;    moved every position held in the buffer -- the fake cursors of
      ;;    `multiple-cursors', markers, other windows' points -- to its
      ;;    start, and recorded an undo step for a command that edits
      ;;    nothing.
      (setf (emjupy-cell-outputs cell) [])
      (unless (emjupy--refresh-cell-output cell)
        (emjupy--rerender-notebook cell))

      ;; 3. Build execution payload
      (let* ((code (emjupy-cell-source cell))
             (req (emjupy--make-execute-request code))
             (msg-id (car req))
             (json-payload (cdr req)))

        ;; The kernel handles requests in order, so telling it the width
        ;; first -- if it has not been told this one -- means the cell
        ;; runs with it set.  Sent from a timer on connect alone, it lost
        ;; the race to a cell run straight after opening a notebook.
        (emjupy--ensure-kernel-width emjupy--buffer-notebook)
        (puthash msg-id cell (emjupy-kernel-pending kernel))
        (emjupy--ws-send json-payload kernel)
        ;; Show that it is running.  Long cells otherwise look identical to
        ;; cells that were never run: an empty box and no execution count.
        (emjupy--mark-running cell)
        ;; (message "[emjupy] Executing cell (%s)..." msg-id)
	))))

(defun emjupy-execute-cell-and-goto-next ()
  "Execute the cell at point, then move to the start of the next cell.

If this is the last cell, a new empty one is made below and point goes
there -- so running the last cell leaves you ready to write the next
thing rather than stranded at the bottom.

Point is taken from the cell list rather than by walking the buffer: the
executed cell may still be re-rendering as its output arrives, and
stepping over overlays mid-flight lands in the output box."
  (interactive)
  (let* ((cell (emjupy--cell-at-point)))
    (unless cell (user-error "Point is not in a cell"))
    (emjupy-execute-cell-at-point)
    (let* ((nb (emjupy--notebook))
           (cells (append (emjupy-notebook-cells nb) nil))
           (idx (cl-position cell cells))
           (next (and idx (nth (1+ idx) cells))))
      (if (and next (overlayp (emjupy-cell-overlay next)))
          (goto-char (overlay-start (emjupy-cell-overlay next)))
        (emjupy-insert-cell-below)))))


;; --- Kernel transport seam -------------------------------------------------
;; Everything that touches a live WebSocket goes through these two functions,
;; each scoped to ONE kernel. That keeps a stale value from blowing up with an
;; opaque `wrong-type-argument' deep inside websocket.el -- the user just gets
;; told this notebook has no kernel -- and gives the test-suite a single,
;; honest place to substitute a fake transport.

(defun emjupy--ws-live-p (&optional kernel)
  "Return non-nil when KERNEL (default this buffer's) has a usable socket."
  (let* ((kernel (or kernel (emjupy--kernel)))
         (ws (and kernel (emjupy-kernel-ws kernel))))
    (and ws (websocket-p ws) (websocket-openp ws))))

(defun emjupy--ws-send (payload &optional kernel)
  "Send PAYLOAD, a string, over KERNEL's WebSocket."
  (let ((kernel (or kernel (emjupy--kernel))))
    (websocket-send-text (emjupy-kernel-ws kernel) payload)))

(defvar emjupy-kernel-connected-functions nil
  "Functions called with a notebook once a kernel is attached to it.

Run a moment after the connection, when the socket is up.  Layers above
this one hang their start-up work here -- asking the kernel where it
runs, for instance -- rather than this file calling into them.")

(defvar-local emjupy--kernel-width-told nil
  "The kernel and width last sent to it, as (KERNEL-ID . WIDTH).")

(defun emjupy--tell-kernel-width (nb &optional width)
  "Tell NB\='s kernel how wide its output is drawn: WIDTH, or the box\='s now.

Sets COLUMNS in the kernel\='s environment, the variable programs consult
to learn how wide the terminal is -- directly, or through
`shutil.get_terminal_size\=' -- when their output is not a terminal.
Without it they guess, and a guess sized to the machine the server runs
on drew progress bars and tables far wider than the cell.  Output that
still comes out too wide is folded where the box ends when it is drawn;
this is what lets well-behaved output not need that."
  (let ((kernel (and nb (emjupy-notebook-kernel nb)))
        (buf (and nb (emjupy-notebook-buffer nb))))
    (when (and kernel (emjupy--ws-live-p kernel) (buffer-live-p buf))
      (with-current-buffer buf
        (let ((columns (or width (emjupy--box-width))))
          (emjupy--kernel-eval
           kernel
           (format "import os as _emjupy_os; _emjupy_os.environ['COLUMNS'] = '%d'; del _emjupy_os"
                   columns)
           #'ignore)
          (setq emjupy--kernel-width-told (cons (emjupy-kernel-id kernel) columns)))))))

(defun emjupy--ensure-kernel-width (nb)
  "Tell NB\='s kernel the box width, unless it already has this one."
  (let ((kernel (and nb (emjupy-notebook-kernel nb))))
    (when kernel
      (unless (equal emjupy--kernel-width-told
                     (cons (emjupy-kernel-id kernel) (emjupy--box-width)))
        (emjupy--tell-kernel-width nb)))))

(defun emjupy--kernel-connection-closed (kernel label)
  "The WebSocket to KERNEL, for the notebook called LABEL, has closed.

Any cell still running on it is ended: its reply and closing idle will
not come over a closed connection.  A kernel that dies mid-cell is one
way that happens -- the server restarts it and closes the socket -- and
the cell was left showing as running for ever, waiting for them."
  (let* ((pending (emjupy-kernel-pending kernel))
         (nb (emjupy-kernel-notebook kernel))
         (buf (and nb (emjupy-notebook-buffer nb)))
         (cells nil))
    (when (hash-table-p pending)
      (maphash (lambda (id cell)
                 (push cell cells)
                 (remhash id emjupy--request-halves))
               pending)
      (clrhash pending))
    (when (buffer-live-p buf)
      (dolist (cell cells) (emjupy--mark-done cell buf)))
    (message "[emjupy] %s: kernel connection closed%s." label
             (if cells
                 (format "; %d running cell%s stopped" (length cells)
                         (if (cdr cells) "s" ""))
               ""))))

(defun emjupy-connect-kernel (notebook kernel-id &optional kernel-name)
  "Attach NOTEBOOK to KERNEL-ID on its own server, over its own WebSocket.
KERNEL-NAME, when given, is the kernelspec name to record.
Returns the `emjupy-kernel'.  Each notebook keeps its own kernel, so
several notebooks -- from several servers -- stay live at once."
  (let* ((server (emjupy-notebook-server notebook))
         (parts (emjupy--server-parts server))
         (token (or (emjupy-server-token server) ""))
         ;; https:// servers require wss://, and any base path (proxy or
         ;; JupyterHub prefix) has to be kept -- rebuilding the URL from
         ;; host+port alone silently drops both.
         (ws-url (format "%s://%s:%s%s/api/kernels/%s/channels%s"
                         (plist-get parts :ws-scheme)
                         (plist-get parts :host)
                         (plist-get parts :port)
                         (plist-get parts :path)
                         kernel-id
                         (if (string-empty-p token)
                             ""
                           (concat "?token=" (url-hexify-string token)))))
         ;; Belt and braces: some deployments (and some reverse proxies in
         ;; front of a tunnel) strip or ignore the query-string token but
         ;; honour the Authorization header.  Shared with the language
         ;; server's socket, which lacked them and could not connect where
         ;; this one could.
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
           :on-message (lambda (_ws frame) (emjupy--handle-ws-message kernel frame))
           :on-open (lambda (_ws) (message "[emjupy] %s connected to kernel %s" label kernel-id))
           :on-close (lambda (_ws) (emjupy--kernel-connection-closed kernel label))
           :on-error (lambda (_ws type err)
                       (message "[emjupy] %s WebSocket error (%s): %s" label type err))))
    (setf (emjupy-notebook-kernel notebook) kernel)
    ;; Tell whoever wants to know that a kernel is now attached -- the
    ;; language-server layer asks it where it runs.  A hook rather than a
    ;; call: that layer sits above this one, and naming it here would make
    ;; each depend on the other.  Deferred so the socket is up first.
    (run-with-timer 0.5 nil #'run-hook-with-args
                    'emjupy-kernel-connected-functions notebook)
    kernel))

(defun emjupy--start-kernel-session (notebook)
  "Create a session for NOTEBOOK and return its kernel description, or nil.

Returns nil rather than signalling if the server will not make one, so
the caller can fall back to a bare kernel."
  (let ((server (emjupy-notebook-server notebook))
        (path (emjupy-notebook-path notebook))
        (body (make-hash-table :test 'equal))
        (kspec (make-hash-table :test 'equal)))
    (when path
      (puthash "name" "python3" kspec)
      (puthash "path" path body)
      (puthash "type" "notebook" body)
      (puthash "name" (file-name-nondirectory path) body)
      (puthash "kernel" kspec body)
      (condition-case nil
          (let ((res (emjupy--http-request "POST" server "/api/sessions"
                                           (json-serialize body))))
            (and (hash-table-p res) (gethash "kernel" res)))
        (error nil)))))

(defun emjupy--spawn-and-connect-kernel (notebook)
  "Start a fresh Python 3 kernel on NOTEBOOK's server and attach it.

Started through a SESSION bound to the notebook\='s path, not by posting
to /api/kernels directly.  Jupyter starts a session\='s kernel in the
notebook\='s own directory, so `open(\"data.csv\")\=' in a cell means what it
means in Jupyter -- beside the notebook -- rather than resolving against
the server root.  It also makes the kernel discoverable as that
notebook\='s kernel by other front-ends."
  (let ((server (emjupy-notebook-server notebook)))
    (let* ((res (or (emjupy--start-kernel-session notebook)
                    (let ((payload (make-hash-table :test 'equal)))
                      (puthash "name" "python3" payload)
                      (emjupy--http-request "POST" server "/api/kernels"
                                            (json-serialize payload)))))
           (new-id (gethash "id" res)))
      (message "Started Python 3 kernel (%s) for %s. Connecting..."
               new-id (emjupy-notebook-path notebook))
      (emjupy-connect-kernel notebook new-id "python3"))))

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
          (message "Connecting %s to existing kernel %s..."
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
    (when (emjupy--ws-live-p kernel)
      (websocket-close (emjupy-kernel-ws kernel)))
    (when (emjupy-kernel-pending kernel)
      (clrhash (emjupy-kernel-pending kernel)))
    (setf (emjupy-kernel-ws kernel) nil)))

(defun emjupy-interrupt-kernel ()
  "Interrupt whatever this notebook\='s kernel is running.

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
                          (format "/api/kernels/%s/interrupt" (emjupy-kernel-id kernel)))
    (message "[emjupy] Interrupt sent to kernel %s." (emjupy-kernel-id kernel))))

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
    (unless (yes-or-no-p
             (format "Restart the kernel for %s -- all variables will be lost?"
                     (emjupy-notebook-path nb)))
      (user-error "Kernel left running"))
    (let ((kernel-id (emjupy-kernel-id kernel))
          (server (emjupy-notebook-server nb))
          (name (emjupy-kernel-name kernel)))
      ;; Old in-flight requests will never get a reply from the restarted
      ;; kernel process, so drop them rather than leave them pending forever.
      (emjupy--disconnect-kernel nb)
      (emjupy--http-request "POST" server (format "/api/kernels/%s/restart" kernel-id))
      (message "Kernel %s restarting..." kernel-id)
      (emjupy-connect-kernel nb kernel-id name))))

(defun emjupy--repair-if-scrambled (nb)
  "Redraw NB if its buffer and cells have come out of step.

A socket that dies mid-render -- which is what suspending the machine
does to a tunnelled connection -- can leave the buffer showing something
the cells do not hold.  The cells are the notebook; the buffer is a
drawing of them, so redrawing costs nothing and fixes it.

Returns the problems that were found, or nil if there were none.  This
is why `emjupy-re-render' worked: it was doing by hand what the
reconnection should have done itself."
  (when-let* ((buf (emjupy-notebook-buffer nb)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (let ((problems (emjupy--check-invariants)))
          (when problems
            (emjupy--rerender-notebook)
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
          (name (emjupy-kernel-name kernel)))
      (emjupy--disconnect-kernel nb)
      (emjupy-connect-kernel nb id name)
      (emjupy--repair-if-scrambled nb)
      (message "[emjupy] Reconnecting %s to kernel %s..." (emjupy-notebook-path nb) id))))

(provide 'emjupy-kernel)
;;; emjupy-kernel.el ends here
