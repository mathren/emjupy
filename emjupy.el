;;; emjupy.el --- Minimal Jupyter Notebook editor -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (websocket "1.14"))
;;; Commentary:
;; Native Jupyter notebook editing using the Jupyter HTTP+WebSocket REST API.
;; Treats the .ipynb JSON as the absolute source of truth.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'url)
(require 'websocket)

;; =============================================================================
;; 1. Customization & Data Structures
;; =============================================================================

(defgroup emjupy nil
  "Minimal Jupyter Notebook editor."
  :group 'tools)

(defvar emjupy--current-server nil
  "The currently active `emjupy-server' object.")

(defvar-local emjupy--buffer-notebook nil
  "The `emjupy-notebook' struct associated with the current buffer.")

(defvar emjupy--xsrf-token nil
  "The XSRF token automatically extracted from the Jupyter server.")

(defvar emjupy--current-notebook-buffer nil
  "Buffer of the currently active emjupy notebook.")

(cl-defstruct emjupy-server
  host port token base-url)

(cl-defstruct emjupy-kernel
  id name server ws status pending)

(cl-defstruct emjupy-cell
  type exec-count source outputs metadata overlay output-ov)

(cl-defstruct emjupy-notebook
  path server kernel cells metadata buffer)

;; =============================================================================
;; 2. Major Mode & EIN-Style Keymaps
;; =============================================================================

(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Execution
    (define-key map (kbd "C-c C-c") #'emjupy-execute-cell-at-point)
    (define-key map (kbd "M-RET")   #'emjupy-execute-cell-and-goto-next)
    (define-key map (kbd "C-c C-r") #'emjupy-execute-cell-and-goto-next)

    ;; Cell Operations (EIN style)
    (define-key map (kbd "C-c C-a") #'emjupy-insert-cell-above)
    (define-key map (kbd "C-c C-b") #'emjupy-insert-cell-below)
    (define-key map (kbd "C-c C-k") #'emjupy-delete-cell)
    (define-key map (kbd "C-c C-t") #'emjupy-cycle-cell-type)

    ;; Navigation
    (define-key map (kbd "C-c C-n") #'emjupy-next-cell)
    (define-key map (kbd "M-n")     #'emjupy-next-cell)
    (define-key map (kbd "C-c C-p") #'emjupy-previous-cell)
    (define-key map (kbd "M-p")     #'emjupy-previous-cell)

    ;; Persistence & Server/Kernel Connection
    (define-key map (kbd "C-x C-s") #'emjupy-save-notebook)
    (define-key map (kbd "C-c C-s") #'emjupy-save-notebook)
    (define-key map (kbd "C-c C-z") #'emjupy-connect-kernel-interactive)
    map)
  "Keymap for `emjupy-mode'.")

(define-derived-mode emjupy-mode fundamental-mode "emjupy"
  "Major mode for interactive Jupyter Notebook editing in Emacs."
  (setq-local line-move-ignore-invisible t)
  (use-local-map emjupy-mode-map))

;; =============================================================================
;; 3. HTTP & WebSocket Layer
;; =============================================================================

(defun emjupy-login (url token)
  "Connect to a Jupyter server, open a notebook, and attach a fresh kernel."
  (interactive "sJupyter Server URL (e.g., localhost:8888) or port (e.g., 8888): \nsToken: ")
  (let ((clean-url (if (string-match-p "^[0-9]+$" url)
                       (concat "localhost:" url)
                     url)))
    (setq emjupy--current-server (make-emjupy-server :base-url clean-url :token token))

    ;; Ping the API root to harvest the _xsrf cookie before proceeding
    (condition-case nil
        (emjupy--http-request "GET" emjupy--current-server "/api")
      (error nil))

    (message "Logging into %s..." clean-url)
    (emjupy-list-notebooks)

    ;; Streamline setup: attach a fresh kernel right away so C-c C-z is only
    ;; needed if you want to pick an already-running kernel instead.
    (let* ((parts (split-string clean-url ":"))
           (host (car parts))
           (port (or (cadr parts) "8888")))
      (emjupy--spawn-and-connect-kernel host port token))))


(defun emjupy-list-notebooks ()
  "Fetch root directory contents and prompt user to open or create a notebook."
  (interactive)
  (if (not emjupy--current-server)
      (error "Not logged in! Call emjupy-login first.")
    (let* ((data (emjupy--http-request "GET" emjupy--current-server "/api/contents"))
           (content (if (hash-table-p data) (gethash "content" data) []))
           (notebooks (list "[Create New Notebook]")))

      (cl-loop for item across content
               when (and (hash-table-p item)
                         (string= (gethash "type" item) "notebook"))
               do (push (gethash "path" item) notebooks))

      (let ((choice (completing-read "Select Notebook: " (nreverse notebooks))))
        (if (string= choice "[Create New Notebook]")
            (emjupy-create-notebook)
          (emjupy-open-notebook choice))))))

(defun emjupy-open-notebook (path)
  "Fetch notebook JSON from server, parse it, and render it in `emjupy-mode'."
  (if (not emjupy--current-server)
      (error "Not logged in!")
    (message "Fetching notebook: %s..." path)
    (let* ((response-data (emjupy--http-request "GET" emjupy--current-server (concat "/api/contents/" path)))
           (content-hash (and response-data (gethash "content" response-data))))
      (if (not content-hash)
          (error "Failed to fetch notebook content from server")
        (let* ((ipynb-json (json-serialize content-hash))
               (nb-struct (emjupy--parse-ipynb ipynb-json))
               (buf-name (format "*emjupy: %s*" path))
               (buf (get-buffer-create buf-name)))

          (setf (emjupy-notebook-server nb-struct) emjupy--current-server)
          (setf (emjupy-notebook-path nb-struct) path)
          (setf (emjupy-notebook-buffer nb-struct) buf)

          (with-current-buffer buf
            (emjupy-mode)
            (let ((inhibit-read-only t))
              (erase-buffer)
              (cl-loop for cell across (emjupy-notebook-cells nb-struct)
                       do (emjupy--render-cell cell)))
            (setq emjupy--buffer-notebook nb-struct))

          (switch-to-buffer buf)
          (setq emjupy--current-notebook-buffer buf)
          (message "Opened notebook: %s. Press C-c C-z to select or spawn a kernel." path))))))

(defun emjupy-create-notebook ()
  "Create a brand new blank notebook on the Jupyter server and open it."
  (interactive)
  (if (not emjupy--current-server)
      (error "Not logged in!")
    (let* ((raw-name (read-string "New notebook name (default Untitled.ipynb): "))
           (name (if (string-empty-p raw-name) "Untitled.ipynb" raw-name))
           (filename (if (string-match-p "\\.ipynb$" name) name (concat name ".ipynb")))
           (nb-payload (make-hash-table :test 'equal))
           (req-body (make-hash-table :test 'equal)))

      (puthash "cells" [] nb-payload)
      (puthash "metadata" (make-hash-table :test 'equal) nb-payload)
      (puthash "nbformat" 4 nb-payload)
      (puthash "nbformat_minor" 5 nb-payload)

      (puthash "type" "notebook" req-body)
      (puthash "format" "json" req-body)
      (puthash "content" nb-payload req-body)

      (message "Creating %s on Jupyter server..." filename)
      (let ((response (emjupy--http-request "PUT" emjupy--current-server
                                            (concat "/api/contents/" filename)
                                            (json-serialize req-body))))
        (if response
            (emjupy-open-notebook filename)
          (error "Failed to write %s to server" filename))))))

(defun emjupy--http-request (method server path &optional body callback)
  "Internal wrapper for url-retrieve that reports exact HTTP errors and handles XSRF."
  (let* ((url-request-method method)
         (url-request-data (when body (encode-coding-string body 'utf-8)))
         (url-automatic-caching nil)
         (token (emjupy-server-token server))
         (url-request-extra-headers
          (append `(("Content-Type" . "application/json"))
                  (when (and token (not (string-empty-p token)))
                    `(("Authorization" . ,(format "token %s" token))))
                  ;; Automatically inject XSRF tokens to bypass Jupyter 403 CSRF blocks
                  (when emjupy--xsrf-token
                    `(("X-XSRFToken" . ,emjupy--xsrf-token)
                      ("Cookie" . ,(format "_xsrf=%s" emjupy--xsrf-token))))))
         (base-url (emjupy-server-base-url server))
         (cache-buster (if (string= method "GET")
                           (format (if (string-match-p "\\?" path) "&_t=%s" "?_t=%s")
                                   (float-time))
                         ""))
         (full-url (concat (if (string-prefix-p "http" base-url) "" "http://")
                           base-url path cache-buster)))

    (if callback
        (url-retrieve full-url callback)
      (let ((buffer (url-retrieve-synchronously full-url t nil 5)))
        (if (not buffer)
            (error "Network error: Could not reach %s" full-url)
          (with-current-buffer buffer
            (goto-char (point-min))
            (let ((status 200))
              (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                (setq status (string-to-number (match-string 1))))

              ;; Harvest XSRF cookie from server response
              (goto-char (point-min))
              (when (re-search-forward "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
                (setq emjupy--xsrf-token (match-string 1)))

              (goto-char (point-min))
              (re-search-forward "\r?\n\r?\n" nil t)
              (let ((json-str (buffer-substring-no-properties (point) (point-max))))
                (kill-buffer buffer)
                (if (>= status 400)
                    (error "[Jupyter HTTP %d] %s: %s" status method json-str)
                  (condition-case err
                      (json-parse-string json-str :object-type 'hash-table :array-type 'array)
                    (error (error "JSON Parse Error on %s: %s" path err))))))))))))

;; =============================================================================
;; 4. .ipynb Serialization & Parsing
;; =============================================================================

(defun emjupy--parse-ipynb (json-string)
  "Parse strict nbformat v4 JSON-STRING into an `emjupy-notebook' struct."
  (let* ((data (json-parse-string json-string :object-type 'hash-table :array-type 'array))
         (cells-data (gethash "cells" data))
         (metadata (gethash "metadata" data))
         (nb (make-emjupy-notebook :metadata metadata :cells (make-vector (length cells-data) nil))))
    (cl-loop for i from 0 below (length cells-data)
             for c-data = (aref cells-data i)
             do (aset (emjupy-notebook-cells nb) i
                      (make-emjupy-cell
                       :type (intern (gethash "cell_type" c-data))
                       :exec-count (gethash "execution_count" c-data)
                       :source (let ((src (gethash "source" c-data)))
                                 (if (vectorp src) (mapconcat #'identity src "") src))
                       :outputs (gethash "outputs" c-data)
                       :metadata (gethash "metadata" c-data))))
    nb))

(defun emjupy--serialize-notebook (nb)
  "Serialize NB `emjupy-notebook' struct back to strict nbformat v4 JSON."
  (let ((data (make-hash-table :test 'equal)))
    (puthash "nbformat" 4 data)
    (puthash "nbformat_minor" 5 data)
    (puthash "metadata" (or (emjupy-notebook-metadata nb) (make-hash-table)) data)

    (let ((cells-vec (make-vector (length (emjupy-notebook-cells nb)) nil)))
      (cl-loop for i from 0 below (length (emjupy-notebook-cells nb))
               for cell = (aref (emjupy-notebook-cells nb) i)
               for c-hash = (make-hash-table :test 'equal)
               do
               (puthash "cell_type" (symbol-name (emjupy-cell-type cell)) c-hash)
               (puthash "metadata" (or (emjupy-cell-metadata cell) (make-hash-table)) c-hash)
               (puthash "source"
                        (vconcat (mapcar (lambda (s) (concat s "\n"))
                                         (split-string (emjupy-cell-source cell) "\n")))
                        c-hash)
               (when (eq (emjupy-cell-type cell) 'code)
                 (puthash "outputs" (or (emjupy-cell-outputs cell) []) c-hash)
                 ;; Safely write :null back to the JSON payload for unexecuted cells
                 (puthash "execution_count" (if (numberp (emjupy-cell-exec-count cell))
                                                (emjupy-cell-exec-count cell)
                                              :null)
                          c-hash))
               (aset cells-vec i c-hash))
      (puthash "cells" cells-vec data))
    (json-serialize data)))

(defun emjupy-save-notebook ()
  "Sync cell buffer contents and save notebook back to the Jupyter server."
  (interactive)
  (unless emjupy--buffer-notebook
    (user-error "No emjupy notebook associated with this buffer"))
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (path (emjupy-notebook-path nb))
         (server (emjupy-notebook-server nb))
         (req-body (make-hash-table :test 'equal))
         (serialized-str (emjupy--serialize-notebook nb))
         (parsed-json (json-parse-string serialized-str :object-type 'hash-table :array-type 'array)))

    (puthash "type" "notebook" req-body)
    (puthash "format" "json" req-body)
    (puthash "content" parsed-json req-body)

    (message "Saving notebook %s..." path)
    (emjupy--http-request "PUT" server (concat "/api/contents/" path) (json-serialize req-body))
    (set-buffer-modified-p nil)
    (message "Successfully saved %s!" path)))

;; =============================================================================
;; 5. Buffer-to-Struct Syncing & Cell Operations
;; =============================================================================

(defun emjupy--sync-cell-source-from-buffer (cell)
  "Read the live buffer text bounded by CELL's overlay into `emjupy-cell-source'."
  (when-let ((ov (emjupy-cell-overlay cell)))
    (when (overlay-buffer ov)
      (let ((text (buffer-substring-no-properties (overlay-start ov) (overlay-end ov))))
        (setf (emjupy-cell-source cell) (string-trim-right text "\n"))))))

(defun emjupy--sync-all-cells ()
  "Sync buffer text for all cells in current buffer."
  (when emjupy--buffer-notebook
    (cl-loop for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
             do (emjupy--sync-cell-source-from-buffer cell))))

(defun emjupy--rerender-notebook (&optional target-cell)
  "Re-render all cell overlays in current buffer. If TARGET-CELL is given, move point to it."
  (when emjupy--buffer-notebook
    (let ((inhibit-read-only t)
          (cells (emjupy-notebook-cells emjupy--buffer-notebook))
          (target-start nil))
      ;; Delete old overlays
      (cl-loop for cell across cells
               do (when (emjupy-cell-overlay cell)
                    (delete-overlay (emjupy-cell-overlay cell))
                    (setf (emjupy-cell-overlay cell) nil))
               (when (emjupy-cell-output-ov cell)
                 (delete-overlay (emjupy-cell-output-ov cell))
                 (setf (emjupy-cell-output-ov cell) nil)))
      (erase-buffer)
      ;; Render every cell first; only move point afterward. Jumping point
      ;; back to target-cell mid-loop would make later `insert' calls land
      ;; inside target-cell's own overlay (which then grows to swallow
      ;; them), shoving it -- and its output -- to the end of the buffer.
      (cl-loop for cell across cells
               do (let ((start (point)))
                    (emjupy--render-cell cell)
                    (when (eq cell target-cell)
                      (setq target-start start))))
      (when target-start
        (goto-char target-start)))))

(defun emjupy-insert-cell-below ()
  "Insert a new empty code cell below the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (get-text-property (point) 'emjupy-cell))
         (cells (append (emjupy-notebook-cells nb) nil))
         (new-cell (make-emjupy-cell :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (if idx
        (setq cells (append (cl-subseq cells 0 (1+ idx))
                            (list new-cell)
                            (cl-subseq cells (1+ idx))))
      (setq cells (append cells (list new-cell))))
    (setf (emjupy-notebook-cells nb) (vconcat cells))
    (emjupy--rerender-notebook new-cell)))

(defun emjupy-insert-cell-above ()
  "Insert a new empty code cell above the cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (get-text-property (point) 'emjupy-cell))
         (cells (append (emjupy-notebook-cells nb) nil))
         (new-cell (make-emjupy-cell :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (if idx
        (setq cells (append (cl-subseq cells 0 idx)
                            (list new-cell)
                            (cl-subseq cells idx)))
      (setq cells (cons new-cell cells)))
    (setf (emjupy-notebook-cells nb) (vconcat cells))
    (emjupy--rerender-notebook new-cell)))

(defun emjupy-delete-cell ()
  "Delete current cell at point."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((nb emjupy--buffer-notebook)
         (curr-cell (get-text-property (point) 'emjupy-cell))
         (cells (append (emjupy-notebook-cells nb) nil)))
    (when curr-cell
      (setq cells (delete curr-cell cells))
      (setf (emjupy-notebook-cells nb) (vconcat cells))
      (emjupy--rerender-notebook))))

(defun emjupy-cycle-cell-type ()
  "Cycle the cell at point between `code' and `markdown'."
  (interactive)
  (emjupy--sync-all-cells)
  (let ((cell (get-text-property (point) 'emjupy-cell)))
    (unless cell
      (user-error "No cell found at point"))
    (setf (emjupy-cell-type cell)
          (if (eq (emjupy-cell-type cell) 'code) 'markdown 'code))
    (emjupy--rerender-notebook cell)))

(defun emjupy-next-cell ()
  "Move point to the next cell."
  (interactive)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
         (ov (and cell (emjupy-cell-overlay cell))))
    (if ov
        (let ((pos (overlay-end ov)))
          (when (< pos (point-max))
            (goto-char (1+ pos))))
      (goto-char (point-min)))))

(defun emjupy-previous-cell ()
  "Move point to the previous cell."
  (interactive)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
         (ov (and cell (emjupy-cell-overlay cell))))
    (if ov
        (let ((pos (overlay-start ov)))
          (when (> pos (point-min))
            (let ((prev-cell (get-text-property (1- pos) 'emjupy-cell)))
              (when (and prev-cell (emjupy-cell-overlay prev-cell))
                (goto-char (overlay-start (emjupy-cell-overlay prev-cell)))))))
      (goto-char (point-min)))))

;; =============================================================================
;; 6. Kernel WebSocket Engine & Live Messaging
;; =============================================================================

(defun emjupy--uuid ()
  "Generate a pseudo-random UUID v4 string."
  (format "%04x%04x-%04x-4%03x-%04x-%04x%04x%04x"
          (random 65536) (random 65536)
          (random 65536) (random 4096)
          (logior (random 4096) #x8000)
          (random 65536) (random 65536) (random 65536)))

(defvar emjupy--session-id (emjupy--uuid)
  "Unique identifier for the current Emacs session.")

(defvar emjupy--ws-connection nil
  "Active WebSocket process for the Jupyter Kernel.")

(defvar emjupy--pending-requests (make-hash-table :test 'equal)
  "Map of msg_id -> target `emjupy-cell` struct awaiting execution output.")

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

(defun emjupy--append-output-to-cell (cell output-hash)
  "Append OUTPUT-HASH frame to CELL outputs and visually refresh the notebook overlay."
  (let ((existing (append (or (emjupy-cell-outputs cell) []) nil)))
    (setf (emjupy-cell-outputs cell) (vconcat (append existing (list output-hash))))
    (when-let ((buf emjupy--current-notebook-buffer))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (emjupy--rerender-notebook cell))))))

(defun emjupy--handle-ws-message (_ws frame)
  "Callback for incoming WebSocket frames from Jupyter kernel."
  (let* ((payload (websocket-frame-payload frame))
         (data (json-parse-string payload :object-type 'hash-table :array-type 'array))
         (header (gethash "header" data))
         (parent-header (gethash "parent_header" data))
         (msg-type (gethash "msg_type" header))
         (parent-id (when parent-header (gethash "msg_id" parent-header)))
         (cell (when parent-id (gethash parent-id emjupy--pending-requests))))

    (when cell
      (cond
       ;; Stdout/Stderr live streaming
       ((string= msg-type "stream")
        (let* ((content (gethash "content" data))
               (text (gethash "text" content))
               (name (gethash "name" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" out-hash)
          (puthash "name" name out-hash)
          (puthash "text" text out-hash)
          (emjupy--append-output-to-cell cell out-hash)))

       ;; Returned execution evaluation results
       ((string= msg-type "execute_result")
        (let* ((content (gethash "content" data))
               (data-obj (gethash "data" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "execute_result" out-hash)
          (puthash "data" data-obj out-hash)
          (emjupy--append-output-to-cell cell out-hash)))

       ;; Rich display data -- this is how matplotlib's inline figure
       ;; actually arrives (separately from the execute_result text repr)
       ((string= msg-type "display_data")
        (let* ((content (gethash "content" data))
               (data-obj (gethash "data" content))
               (out-hash (make-hash-table :test 'equal)))
          (puthash "output_type" "display_data" out-hash)
          (puthash "data" data-obj out-hash)
          (emjupy--append-output-to-cell cell out-hash)))

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
          (emjupy--append-output-to-cell cell out-hash)))

       ;; Finish execution frame
       ((string= msg-type "execute_reply")
        (let* ((content (gethash "content" data))
               (count (gethash "execution_count" content)))
          (setf (emjupy-cell-exec-count cell) count)
          (remhash parent-id emjupy--pending-requests)
          (when-let ((buf emjupy--current-notebook-buffer))
            (when (buffer-live-p buf)
              (with-current-buffer buf
                (emjupy--rerender-notebook cell))))
          (message "[emjupy] Cell execution complete. [In: %s]" count)))))))

(defun emjupy-execute-cell-at-point ()
  "Sync cell code and send to kernel WebSocket for execution."
  (interactive)
  (unless (and emjupy--ws-connection (websocket-openp emjupy--ws-connection))
    (user-error "Not connected to a Jupyter kernel! Use C-c C-z to select/start a kernel"))

  (let* ((cell (get-text-property (point) 'emjupy-cell)))
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

      (puthash msg-id cell emjupy--pending-requests)
      (websocket-send-text emjupy--ws-connection json-payload)
      (message "[emjupy] Executing cell (%s)..." msg-id))))

(defun emjupy-execute-cell-and-goto-next ()
  "Execute cell at point and advance to the next cell."
  (interactive)
  (emjupy-execute-cell-at-point)
  (emjupy-next-cell))

(defun emjupy-connect-kernel (host port kernel-id token)
  "Establish WebSocket channels connection to KERNEL-ID."
  (let ((ws-url (format "ws://%s:%s/api/kernels/%s/channels?token=%s"
                        host port kernel-id token)))
    (setq emjupy--ws-connection
          (websocket-open
           ws-url
           :on-message #'emjupy--handle-ws-message
           :on-open (lambda (_ws) (message "[emjupy] Connected to Kernel %s!" kernel-id))
           :on-close (lambda (_ws) (message "[emjupy] Kernel WebSocket closed."))
           :on-error (lambda (_ws type err) (message "[emjupy] WebSocket Error (%s): %s" type err))))))

(defun emjupy--spawn-and-connect-kernel (host port token)
  "Start a fresh Python 3 kernel on the server and connect to it via websocket."
  (let ((payload (make-hash-table :test 'equal)))
    (puthash "name" "python3" payload)
    (let* ((res (emjupy--http-request "POST" emjupy--current-server "/api/kernels" (json-serialize payload)))
           (new-id (gethash "id" res)))
      (message "Started Python 3 kernel (%s). Connecting..." new-id)
      (emjupy-connect-kernel host port new-id token))))

(defun emjupy-connect-kernel-interactive ()
  "Select an existing kernel or spawn a new Python 3 kernel interactively."
  (interactive)
  (unless emjupy--current-server
    (user-error "Not logged in! Call emjupy-login first"))
  (let* ((kernels-data (emjupy--http-request "GET" emjupy--current-server "/api/kernels"))
         (kernel-options '("[Start New Python 3 Kernel]"))
         (kernel-map (make-hash-table :test 'equal)))

    (cl-loop for k across kernels-data
             for id = (gethash "id" k)
             for name = (gethash "name" k)
             for label = (format "%s (%s)" name id)
             do (push label kernel-options)
                (puthash label id kernel-map))

    (let ((choice (completing-read "Select Kernel: " (nreverse kernel-options))))
      (let* ((clean-url (replace-regexp-in-string "^https?://" "" (emjupy-server-base-url emjupy--current-server)))
             (parts (split-string clean-url ":"))
             (host (car parts))
             (port (or (cadr parts) "8888"))
             (token (or (emjupy-server-token emjupy--current-server) "")))

        (if (string= choice "[Start New Python 3 Kernel]")
            (emjupy--spawn-and-connect-kernel host port token)
          (let ((selected-id (gethash choice kernel-map)))
            (message "Connecting to existing kernel %s..." selected-id)
            (emjupy-connect-kernel host port selected-id token)))))))

;; =============================================================================
;; 7. Visual Rendering Overlays
;; =============================================================================

(defconst emjupy--box-width 80
  "Target character width for cell boundary box-drawing lines.")

(defun emjupy--box-header (label)
  "Return an `emjupy--box-width'-column box-drawing header line with LABEL."
  (let* ((prefix (format "┌─ %s " label))
         (fill (max 0 (- emjupy--box-width (length prefix)))))
    (concat prefix (make-string fill ?─) "\n")))

(defun emjupy--box-footer ()
  "Return an `emjupy--box-width'-column box-drawing footer line."
  (concat "└" (make-string (max 0 (- emjupy--box-width 2)) ?─) "┘\n"))

(defun emjupy--fontify-code (beg end)
  "Apply python-mode syntax highlighting to the region."
  (let ((font-lock-mode t)
        (major-mode 'python-mode))
    (unless (featurep 'python) (require 'python))
    (font-lock-fontify-region beg end)))

(defun emjupy--render-cell (cell)
  "Render CELL at point using overlays for boundary boxes and live outputs."
  (let* ((type (emjupy-cell-type cell))
         (source (emjupy-cell-source cell))
         (outputs (emjupy-cell-outputs cell))
         (exec-val (emjupy-cell-exec-count cell))
         ;; Safely handle :null values from unexecuted cells
         (exec-str (if (numberp exec-val) (number-to-string exec-val) " "))
         ;; Only show the output box when the cell actually has output --
         ;; e.g. a bare `import numpy as np' shouldn't grow an empty box.
         (has-outputs (and (eq type 'code) outputs (> (length outputs) 0)))
         (src-start (point)))

    ;; 1. Insert source code and tag text
    (insert (if (string-empty-p source) "\n" source))
    (unless (string-suffix-p "\n" source) (insert "\n"))

    (put-text-property src-start (point) 'emjupy-cell cell)
    (when (eq type 'code)
      (emjupy--fontify-code src-start (point)))

    ;; 2. Source Box Overlay
    (let* ((ov (make-overlay src-start (point)))
           (header (propertize (emjupy--box-header
                                 (format "[In: %s] %s" exec-str
                                         (if (eq type 'code) "python" "markdown")))
                               'face 'shadow))
           (footer (propertize (emjupy--box-footer) 'face 'shadow)))
      (overlay-put ov 'before-string header)
      (overlay-put ov 'after-string footer)
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Output Box Overlay
    (when has-outputs
      (let ((out-start (point)))
        (cl-loop for out across outputs
                 do (let ((out-type (gethash "output_type" out)))
                      (cond
                       ((string= out-type "stream")
                        (insert (gethash "text" out)))
                       ((or (string= out-type "execute_result")
                            (string= out-type "display_data"))
                        (let* ((data (gethash "data" out))
                               (png (gethash "image/png" data))
                               (jpeg (gethash "image/jpeg" data)))
                          (cond
                           (png (insert-image (emjupy--render-image-output png 'png))
                                (insert "\n"))
                           (jpeg (insert-image (emjupy--render-image-output jpeg 'jpeg))
                                 (insert "\n"))
                           ((gethash "text/plain" data)
                            (insert (gethash "text/plain" data) "\n")))))
                       ((string= out-type "error")
                        (let ((ename (gethash "ename" out))
                              (evalue (gethash "evalue" out))
                              (traceback (gethash "traceback" out)))
                          (insert (format "Error (%s): %s\n" ename evalue))
                          (when (vectorp traceback)
                            (cl-loop for line across traceback
                                     do (insert (replace-regexp-in-string "\033\\[[0-9;]*m" "" line) "\n"))))))))

        (unless (string-suffix-p "\n" (buffer-substring-no-properties (max (point-min) (- (point) 1)) (point)))
          (insert "\n"))

        (let* ((ov (make-overlay out-start (point)))
               (header (propertize (emjupy--box-header (format "[Out: %s]" exec-str)) 'face 'shadow))
               (footer (propertize (emjupy--box-footer) 'face 'shadow)))
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          (setf (emjupy-cell-output-ov cell) ov))))

    (insert "\n")))

(defun emjupy--render-image-output (base64-string &optional type)
  "Convert BASE64-STRING output into an Emacs image object of TYPE (default png)."
  (let ((image-data (base64-decode-string base64-string)))
    (create-image image-data (or type 'png) t)))

(defun emjupy-restart-kernel (kernel)
  "Issue restart request to KERNEL via HTTP API."
  (let ((server (emjupy-kernel-server kernel))
        (kernel-id (emjupy-kernel-id kernel)))
    (emjupy--http-request "POST" server (format "/api/kernels/%s/restart" kernel-id))
    (setf (emjupy-kernel-status kernel) 'restarting)
    (message "Kernel %s restarting..." kernel-id)))

;; =============================================================================
;; 8. Unit Tests (ERT)
;; =============================================================================

(ert-deftest emjupy-test-ipynb-roundtrip ()
  "Test parsing and serializing a notebook preserves structure."
  (let* ((raw-json "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"import numpy as np\\n\",\"print(1)\"]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")
         (parsed-nb (emjupy--parse-ipynb raw-json))
         (first-cell (aref (emjupy-notebook-cells parsed-nb) 0))
         (reserialized (emjupy--serialize-notebook parsed-nb)))

    (should (eq (emjupy-cell-type first-cell) 'code))
    (should (string= (emjupy-cell-source first-cell) "import numpy as np\nprint(1)"))
    (should (string-match-p "\"nbformat\":4" reserialized))
    (should (string-match-p "\"import numpy as np\\\\n\"" reserialized))))

(ert-deftest emjupy-test-cell-insertion-and-deletion ()
  "Test creating and deleting notebook cells dynamically."
  (let* ((cell1 (make-emjupy-cell :type 'code :source "x = 10"))
         (nb (make-emjupy-notebook :cells (vector cell1)))
         (buf (get-buffer-create "*test-emjupy-cells*")))
    (with-current-buffer buf
      (emjupy-mode)
      (setq emjupy--buffer-notebook nb)
      (emjupy--rerender-notebook)

      ;; Insert below
      (goto-char (point-min))
      (emjupy-insert-cell-below)
      (should (= (length (emjupy-notebook-cells emjupy--buffer-notebook)) 2))

      ;; Delete current cell
      (emjupy-delete-cell)
      (should (= (length (emjupy-notebook-cells emjupy--buffer-notebook)) 1)))
    (kill-buffer buf)))

(ert-deftest emjupy-test-render-image ()
  "Test decoding base64 PNG into an Emacs image."
  (let* ((b64-png "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
         (img (emjupy--render-image-output b64-png)))
    (should (eq (car img) 'image))
    (should (eq (plist-get (cdr img) :type) 'png))
    (should (plist-get (cdr img) :data))))

(provide 'emjupy)
;;; emjupy.el ends here
