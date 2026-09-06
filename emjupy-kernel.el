;;; emjupy-kernel.el --- Kernel WebSocket transport and execution for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
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
  "Construct a Jupyter protocol `execute_request` message payload."
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

(defun emjupy--append-output-to-cell (cell output-hash &optional notebook)
  "Append OUTPUT-HASH to CELL outputs and refresh NOTEBOOK's buffer."
  (let ((existing (append (or (emjupy-cell-outputs cell) []) nil)))
    (setf (emjupy-cell-outputs cell) (vconcat (append existing (list output-hash))))
    (when-let ((buf (and notebook (emjupy-notebook-buffer notebook))))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          ;; Rendering runs inside the WebSocket callback, and websocket.el
          ;; catches errors raised there -- so an un-renderable output (a PNG
          ;; on a build without image support, say) would otherwise leave the
          ;; box silently blank with nothing logged anywhere.
          (condition-case err
              (emjupy--rerender-notebook cell)
            (error
             (message "[emjupy] Failed to render cell output: %s"
                      (error-message-string err)))))))))

(defun emjupy--ws-payload (frame)
  "Return the text payload of FRAME.
Accepts either a `websocket-frame' (what websocket.el hands the
callback) or a plain JSON string, so recorded kernel traffic can be
replayed straight through the handler."
  (if (websocket-frame-p frame) (websocket-frame-payload frame) frame))

(defun emjupy--handle-ws-message (kernel frame)
  "Handle an incoming WebSocket FRAME belonging to KERNEL.
KERNEL carries both the pending-request table and the backlink to the
notebook whose buffer should be refreshed, so frames from several
kernels never cross-talk."
  (let* ((payload (emjupy--ws-payload frame))
         (data (json-parse-string payload :object-type 'hash-table :array-type 'array))
         (header (gethash "header" data))
         (parent-header (gethash "parent_header" data))
         (msg-type (gethash "msg_type" header))
         (parent-id (when parent-header (gethash "msg_id" parent-header)))
         (pending (and kernel (emjupy-kernel-pending kernel)))
         (notebook (and kernel (emjupy-kernel-notebook kernel)))
         (cell (when (and parent-id pending) (gethash parent-id pending))))

    (when cell
      (cond
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

       ;; Finish execution frame
       ((string= msg-type "execute_reply")
        (let* ((content (gethash "content" data))
               (count (gethash "execution_count" content)))
          (setf (emjupy-cell-exec-count cell) count)
          (remhash parent-id pending)
          (when-let ((buf (and notebook (emjupy-notebook-buffer notebook))))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (emjupy--rerender-notebook cell))))
          (message "[emjupy] %s: cell execution complete. [In: %s]"
                   (if notebook (emjupy-notebook-path notebook) "notebook")
                   count)))))))

(defun emjupy-execute-cell-at-point ()
  "Sync cell code and send to this notebook's kernel for execution."
  (interactive)
  (let ((kernel (emjupy--kernel)))
    (unless (emjupy--ws-live-p kernel)
      (user-error "This notebook has no kernel! Use C-c C-z to select/start one"))

    (let* ((cell (emjupy--cell-at-point)))
      (unless cell
        (user-error "No cell found at point"))

      ;; 1. Sync buffer edits back into ALL cells (not just this one) --
      ;; the upcoming rerender rebuilds the whole buffer from cell structs,
      ;; so unsynced edits sitting in other cells would otherwise be lost.
      (emjupy--sync-all-cells)

      ;; 2. Clear previous outputs for rerun so a stale output box doesn't
      ;;    linger while the new run is in flight.
      (setf (emjupy-cell-outputs cell) [])
      (emjupy--rerender-notebook cell)

      ;; 3. Build execution payload
      (let* ((code (emjupy-cell-source cell))
             (req (emjupy--make-execute-request code))
             (msg-id (car req))
             (json-payload (cdr req)))

        (puthash msg-id cell (emjupy-kernel-pending kernel))
        (emjupy--ws-send json-payload kernel)
        ;; (message "[emjupy] Executing cell (%s)..." msg-id)
	))))

(defun emjupy-execute-cell-and-goto-next ()
  "Execute cell at point and advance to the next cell."
  (interactive)
  (emjupy-execute-cell-at-point)
  (emjupy-next-cell))


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

(defun emjupy-connect-kernel (notebook kernel-id &optional kernel-name)
  "Attach NOTEBOOK to KERNEL-ID on its own server, over its own WebSocket.
Returns the `emjupy-kernel'. Each notebook keeps its own kernel, so
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
         ;; honour the Authorization header.
         (headers (append
                   (unless (string-empty-p token)
                     `(("Authorization" . ,(format "token %s" token))))
                   (when (emjupy-server-xsrf server)
                     `(("Cookie" . ,(format "_xsrf=%s" (emjupy-server-xsrf server)))))))
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
           :on-close (lambda (_ws) (message "[emjupy] %s: kernel WebSocket closed." label))
           :on-error (lambda (_ws type err)
                       (message "[emjupy] %s WebSocket error (%s): %s" label type err))))
    (setf (emjupy-notebook-kernel notebook) kernel)
    kernel))

(defun emjupy--spawn-and-connect-kernel (notebook)
  "Start a fresh Python 3 kernel on NOTEBOOK's server and attach it."
  (let ((payload (make-hash-table :test 'equal))
        (server (emjupy-notebook-server notebook)))
    (puthash "name" "python3" payload)
    (let* ((res (emjupy--http-request "POST" server "/api/kernels" (json-serialize payload)))
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
  (when-let ((kernel (emjupy-notebook-kernel notebook)))
    (when (emjupy--ws-live-p kernel)
      (websocket-close (emjupy-kernel-ws kernel)))
    (when (emjupy-kernel-pending kernel)
      (clrhash (emjupy-kernel-pending kernel)))
    (setf (emjupy-kernel-ws kernel) nil)))

(defun emjupy-restart-kernel ()
  "Restart THIS notebook's kernel and reconnect its websocket.
Other open notebooks, and their kernels, are untouched."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (kernel (emjupy-notebook-kernel nb)))
    (unless (and kernel (emjupy-kernel-id kernel))
      (user-error "This notebook has no kernel! Use C-c C-z to select/start one"))
    (let ((kernel-id (emjupy-kernel-id kernel))
          (server (emjupy-notebook-server nb))
          (name (emjupy-kernel-name kernel)))
      ;; Old in-flight requests will never get a reply from the restarted
      ;; kernel process, so drop them rather than leave them pending forever.
      (emjupy--disconnect-kernel nb)
      (emjupy--http-request "POST" server (format "/api/kernels/%s/restart" kernel-id))
      (message "Kernel %s restarting..." kernel-id)
      (emjupy-connect-kernel nb kernel-id name))))

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
      (message "[emjupy] Reconnecting %s to kernel %s..." (emjupy-notebook-path nb) id))))

(provide 'emjupy-kernel)
;;; emjupy-kernel.el ends here
