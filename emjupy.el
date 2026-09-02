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

(defvar emjupy--next-cell-id 0
  "Monotonically increasing counter for assigning stable `emjupy-cell' ids.")

(defun emjupy--new-cell-id ()
  "Return a fresh, never-reused cell id."
  (setq emjupy--next-cell-id (1+ emjupy--next-cell-id)))

(cl-defstruct emjupy-cell
  id type exec-count source outputs metadata overlay output-ov)

(cl-defstruct emjupy-notebook
  path server kernel cells metadata buffer shadow-buffer)

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
    (define-key map (kbd "M-<up>")   #'emjupy-move-cell-up)
    (define-key map (kbd "M-<down>") #'emjupy-move-cell-down)
    (define-key map (kbd "C-c '")    #'emjupy-edit-cell-externally)

    ;; Kernel
    (define-key map (kbd "C-c C-x C-r") #'emjupy-restart-kernel)

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
  (use-local-map emjupy-mode-map)
  ;; Completion/eldoc for code cells are delegated to the shared code
  ;; shadow buffer (see section 8) automatically -- no action needed
  ;; from the user beyond normal editing and the usual M-TAB/eldoc UI.
  (add-hook 'completion-at-point-functions #'emjupy--cell-completion-at-point nil t)
  (add-hook 'eldoc-documentation-functions #'emjupy--cell-eldoc-function nil t)
  (eldoc-mode 1))

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
    (let* ((hp (emjupy--server-host-port)))
      (emjupy--spawn-and-connect-kernel (car hp) (cdr hp) token))))


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
          ;; Warm up the code shadow-buffer + Eglot now, in the background,
          ;; so completions are ready once the user starts typing instead of
          ;; paying the LSP server startup cost on the first keystroke.
          (ignore-errors (emjupy--ensure-shadow-buffer nb-struct))
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
                       :id (emjupy--new-cell-id)
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
         (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table)))
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
         (new-cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table)))
         (idx (cl-position curr-cell cells)))
    (if idx
        (setq cells (append (cl-subseq cells 0 idx)
                            (list new-cell)
                            (cl-subseq cells idx)))
      (setq cells (cons new-cell cells)))
    (setf (emjupy-notebook-cells nb) (vconcat cells))
    (emjupy--rerender-notebook new-cell)))

(defun emjupy-move-cell-up ()
  "Move the cell at point up, swapping it with the cell above.
Since a cell's output lives inside its own struct rather than as a
separate entity, the output always travels with its cell automatically."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
         (cell (get-text-property (point) 'emjupy-cell))
         (idx (cl-position cell cells)))
    (if (or (not idx) (= idx 0))
        (message "[emjupy] Cell is already at the top.")
      (let ((above (aref cells (1- idx))))
        (aset cells (1- idx) cell)
        (aset cells idx above))
      (emjupy--rerender-notebook cell))))

(defun emjupy-move-cell-down ()
  "Move the cell at point down, swapping it with the cell below."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cells (emjupy-notebook-cells emjupy--buffer-notebook))
         (cell (get-text-property (point) 'emjupy-cell))
         (idx (cl-position cell cells)))
    (if (or (not idx) (= idx (1- (length cells))))
        (message "[emjupy] Cell is already at the bottom.")
      (let ((below (aref cells (1+ idx))))
        (aset cells (1+ idx) cell)
        (aset cells idx below))
      (emjupy--rerender-notebook cell))))

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

(defvar emjupy--current-kernel-id nil
  "Kernel id of the currently connected Jupyter kernel, if any.")

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

(defun emjupy--server-host-port ()
  "Return (HOST . PORT) parsed from the current server's base-url."
  (let* ((clean-url (replace-regexp-in-string "^https?://" "" (emjupy-server-base-url emjupy--current-server)))
         (parts (split-string clean-url ":")))
    (cons (car parts) (or (cadr parts) "8888"))))

(defun emjupy-connect-kernel (host port kernel-id token)
  "Establish WebSocket channels connection to KERNEL-ID."
  (setq emjupy--current-kernel-id kernel-id)
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
      (let* ((hp (emjupy--server-host-port))
             (host (car hp))
             (port (cdr hp))
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

(defun emjupy--box-header (label &optional corner)
  "Return an `emjupy--box-width'-column box-drawing header line with LABEL.
CORNER is the left corner glyph, default \"┌\"; pass \"├\" when this
header is meant to double as the closing edge of the box above it."
  (let* ((prefix (format "%s─ %s " (or corner "┌") label))
         (fill (max 0 (- emjupy--box-width (length prefix)))))
    (concat prefix (make-string fill ?─) "\n")))

(defun emjupy--box-footer ()
  "Return an `emjupy--box-width'-column box-drawing footer line."
  (concat "└" (make-string (max 0 (- emjupy--box-width 2)) ?─) "┘\n"))

(defun emjupy--markdown-mode-fn ()
  "Return the best available markdown major-mode function, or nil.
Prefers the tree-sitter `markdown-ts-mode' (MELPA) when both the
package and its compiled grammar are actually available, then falls
back to the classic `markdown-mode' (MELPA), then nil (plain text)
if neither is installed."
  (cond
   ((and (fboundp 'markdown-ts-mode)
         (fboundp 'treesit-ready-p)
         (treesit-ready-p 'markdown t))
    'markdown-ts-mode)
   ((fboundp 'markdown-mode) 'markdown-mode)
   (t nil)))

(defun emjupy--fontify-as (text cell-type)
  "Return TEXT with font-lock faces applied appropriate for CELL-TYPE.
Code cells use `python-mode', which ships with Emacs core. Markdown
cells use whatever `emjupy--markdown-mode-fn' resolves to, falling
back to plain text if no markdown package is installed."
  (with-temp-buffer
    (insert text)
    ;; delay-mode-hooks avoids running the user's own mode hooks (linters,
    ;; minor modes, etc.) in this throwaway buffer -- same technique
    ;; org-mode uses to fontify source blocks.
    (cond
     ((eq cell-type 'code)
      (delay-mode-hooks (python-mode)))
     ((eq cell-type 'markdown)
      (let ((mode-fn (emjupy--markdown-mode-fn)))
        (when mode-fn (delay-mode-hooks (funcall mode-fn))))))
    (font-lock-ensure)
    (buffer-string)))

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

    ;; 1. Insert source code (syntax-highlighted per cell type) and tag text
    (let ((to-insert (emjupy--fontify-as (if (string-empty-p source) "\n" source) type)))
      (insert to-insert))
    (unless (string-suffix-p "\n" source) (insert "\n"))

    (put-text-property src-start (point) 'emjupy-cell cell)

    ;; 2. Source Box Overlay
    (let* ((ov (make-overlay src-start (point)))
           (header (propertize (emjupy--box-header
                                 (format "[In: %s] %s" exec-str
                                         (if (eq type 'code) "python" "markdown")))
                               'face 'shadow))
           ;; When output follows, its header line doubles as this box's
           ;; closing edge -- no separate footer, no gap between the two.
           (footer (if has-outputs
                       ""
                     (propertize (emjupy--box-footer) 'face 'shadow))))
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
               ;; "├" instead of "┌": this line IS the input box's bottom
               ;; edge, continuing straight into the output box's top edge.
               (header (propertize (emjupy--box-header (format "[Out: %s]" exec-str) "├") 'face 'shadow))
               (footer (propertize (emjupy--box-footer) 'face 'shadow)))
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          (setf (emjupy-cell-output-ov cell) ov))))

    (insert "\n")))

(defun emjupy--render-image-output (base64-string &optional type)
  "Convert BASE64-STRING output into an Emacs image object of TYPE (default png)."
  (let ((image-data (base64-decode-string base64-string)))
    (create-image image-data (or type 'png) t)))

(defun emjupy-restart-kernel ()
  "Restart the currently connected kernel and reconnect its websocket."
  (interactive)
  (unless (and emjupy--current-server emjupy--current-kernel-id)
    (user-error "No kernel connected! Use C-c C-z to select/start a kernel"))
  (let* ((hp (emjupy--server-host-port))
         (host (car hp))
         (port (cdr hp))
         (token (or (emjupy-server-token emjupy--current-server) ""))
         (kernel-id emjupy--current-kernel-id))
    (when (and emjupy--ws-connection (websocket-openp emjupy--ws-connection))
      (websocket-close emjupy--ws-connection))
    ;; Old in-flight requests will never get a reply from the restarted
    ;; kernel process, so drop them rather than leave them pending forever.
    (clrhash emjupy--pending-requests)
    (emjupy--http-request "POST" emjupy--current-server (format "/api/kernels/%s/restart" kernel-id))
    (message "Kernel %s restarting..." kernel-id)
    (emjupy-connect-kernel host port kernel-id token)))

;; =============================================================================
;; 8. External Editing (real major-mode buffer, for Eglot/LSP etc.)
;; =============================================================================
;; emjupy-mode is a single fundamental-mode-derived buffer mixing code cells,
;; markdown cells, box-drawing decoration, and output text all interleaved --
;; nothing like the single-language file Eglot (or any tool that expects
;; `buffer-file-name'/major-mode to mean one coherent source file) needs to
;; attach to, and Eglot has no client-side support for LSP's notebookDocument
;; sync extension (checked directly: no `eglot-*notebook*' symbols exist even
;; though some servers advertise it), so there's no protocol-level shortcut.
;;
;; For CODE cells, all of the notebook's code cells are shown together in one
;; persistent, real python-mode buffer -- a "shadow" buffer, marked with
;; `# %% [emjupy:ID]' section headers (in the spirit of the jupytext percent
;; format) -- so Eglot sees one coherent multi-cell Python document and can
;; resolve names defined in any cell. Eglot is started on it automatically;
;; the buffer and its LSP connection persist across edits, so only the FIRST
;; access in a session pays the server-startup cost.
;;
;; That shadow buffer is reachable directly via `C-c '' for heavier editing,
;; but for everyday use you never need to: completion-at-point-functions and
;; eldoc-documentation-functions are wired into emjupy-mode itself, silently
;; delegating to the shadow buffer's Eglot session and mapping positions back
;; and forth -- so completion/eldoc for code cells just work while typing
;; directly in the notebook, cross-cell-aware, no action required.
;;
;; MARKDOWN cells don't benefit from cross-cell LSP awareness, so they keep
;; the simpler single-cell external-edit buffer.

(defvar-local emjupy--edit-source-cell nil
  "The `emjupy-cell' struct backing this transient markdown-cell-edit buffer.")
(defvar-local emjupy--edit-source-notebook-buffer nil
  "The notebook buffer this markdown-cell-edit buffer commits changes into.")

(defvar emjupy-cell-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emjupy-commit-cell-edit)
    (define-key map (kbd "C-c '")   #'emjupy-commit-cell-edit)
    (define-key map (kbd "C-c C-k") #'emjupy-abort-cell-edit)
    map)
  "Keymap active in the transient markdown-cell edit buffer.")

(define-minor-mode emjupy-cell-edit-mode
  "Minor mode for the transient markdown-cell buffer opened by
`emjupy-edit-cell-externally'."
  :lighter " emjupy-edit"
  :keymap emjupy-cell-edit-mode-map)

(defun emjupy--edit-markdown-cell-externally (cell nb-buf)
  "Open markdown CELL from notebook buffer NB-BUF in its own edit buffer."
  (let* ((mode-fn (or (emjupy--markdown-mode-fn) 'text-mode))
         (buf (generate-new-buffer
               (format "*emjupy-cell-edit: %s[%s]*" (buffer-name nb-buf) (emjupy-cell-id cell)))))
    (with-current-buffer buf
      (insert (emjupy-cell-source cell))
      (funcall mode-fn)
      (emjupy-cell-edit-mode 1)
      (setq emjupy--edit-source-cell cell)
      (setq emjupy--edit-source-notebook-buffer nb-buf)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Editing cell externally in %s -- C-c C-c to commit, C-c C-k to discard." mode-fn)))

(defun emjupy-commit-cell-edit ()
  "Commit this markdown-cell-edit buffer's text back into its cell, then close it."
  (interactive)
  (unless (and emjupy--edit-source-cell (buffer-live-p emjupy--edit-source-notebook-buffer))
    (user-error "This buffer isn't an emjupy cell-edit buffer"))
  (let ((new-source (string-trim-right (buffer-string) "\n"))
        (cell emjupy--edit-source-cell)
        (nb-buf emjupy--edit-source-notebook-buffer)
        (edit-buf (current-buffer)))
    (setf (emjupy-cell-source cell) new-source)
    (with-current-buffer nb-buf
      (emjupy--rerender-notebook cell))
    (kill-buffer edit-buf)
    (message "[emjupy] Cell updated.")))

(defun emjupy-abort-cell-edit ()
  "Discard this markdown-cell-edit buffer's changes without committing them."
  (interactive)
  (when (y-or-n-p "Discard changes to this cell? ")
    (kill-buffer)))

;; --- Code cells: persistent multi-cell shadow buffer with Eglot ------------

(defvar-local emjupy--edit-shadow-notebook nil
  "The `emjupy-notebook' struct this shared code shadow-buffer belongs to.")

(defvar emjupy-shadow-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emjupy-commit-shadow-edit)
    (define-key map (kbd "C-c '")   #'emjupy-commit-shadow-edit)
    (define-key map (kbd "C-c C-k") #'emjupy-abort-shadow-edit)
    map)
  "Keymap active in the shared multi-cell code shadow-buffer.")

(define-minor-mode emjupy-shadow-edit-mode
  "Minor mode for the persistent shared-code buffer opened by
`emjupy-edit-cell-externally' for code cells. Eglot manages this
buffer like any ordinary Python file."
  :lighter " emjupy-shadow")

(defun emjupy--shadow-cell-marker (id)
  "Return the `# %% [emjupy:ID]' section-header line text for cell ID."
  (format "# %%%% [emjupy:%d]" id))

(defun emjupy--build-shadow-content (nb)
  "Concatenate every code cell in NB into one Python source, section-marked."
  (mapconcat
   (lambda (cell) (concat (emjupy--shadow-cell-marker (emjupy-cell-id cell))
                          "\n" (emjupy-cell-source cell) "\n"))
   (cl-remove-if-not (lambda (c) (eq (emjupy-cell-type c) 'code))
                      (append (emjupy-notebook-cells nb) nil))
   "\n"))

(defun emjupy--parse-shadow-sections (text)
  "Return an alist of (ID . SOURCE) parsed from TEXT's `# %% [emjupy:ID]' markers."
  (let (sections current-id current-lines)
    (dolist (line (split-string text "\n"))
      (if (string-match "\\`# %% \\[emjupy:\\([0-9]+\\)\\]\\'" line)
          (progn
            (when current-id
              (push (cons current-id (string-trim (mapconcat #'identity (nreverse current-lines) "\n")))
                    sections))
            (setq current-id (string-to-number (match-string 1 line)))
            (setq current-lines nil))
        (when current-id (push line current-lines))))
    (when current-id
      (push (cons current-id (string-trim (mapconcat #'identity (nreverse current-lines) "\n")))
            sections))
    (nreverse sections)))

(defun emjupy--shadow-file-path (nb)
  "Return a stable on-disk path for NB's shadow Python file."
  (let* ((dir (expand-file-name "emjupy-shadow" temporary-file-directory))
         (safe-name (replace-regexp-in-string
                     "[^A-Za-z0-9._-]" "_" (or (emjupy-notebook-path nb) "untitled"))))
    (make-directory dir t)
    (expand-file-name (concat safe-name ".py") dir)))

(defun emjupy--ensure-shadow-buffer (nb)
  "Get-or-create NB's persistent code shadow-buffer, refresh its content to
match the current cells, and make sure Eglot is (or becomes) attached --
automatically, with nothing for the user to run."
  (let ((buf (emjupy-notebook-shadow-buffer nb)))
    (unless (buffer-live-p buf)
      (setq buf (find-file-noselect (emjupy--shadow-file-path nb)))
      (setf (emjupy-notebook-shadow-buffer nb) buf)
      (with-current-buffer buf
        (python-mode)
        (emjupy-shadow-edit-mode 1)
        (setq emjupy--edit-shadow-notebook nb)))
    (with-current-buffer buf
      (let ((new-content (emjupy--build-shadow-content nb)))
        (unless (string= new-content (buffer-string))
          (erase-buffer)
          (insert new-content)
          (write-region (point-min) (point-max) buffer-file-name nil 'quiet)
          (set-buffer-modified-p nil)))
      (if (not (require 'eglot nil t))
          (message "[emjupy] Eglot isn't available in this Emacs (needs Emacs 29+).")
        (condition-case err
            ;; NOT eglot-ensure: it defers connecting to `post-command-hook',
            ;; added *buffer-locally* to whatever buffer was current at call
            ;; time. This shadow buffer is deliberately never the user's
            ;; focused buffer -- that's the whole point of automatic,
            ;; no-switching completion -- so that hook would never fire.
            ;; This replicates eglot-ensure's own deferred callback body
            ;; (see `eglot-ensure' in eglot.el) but runs it immediately;
            ;; that's safe here because, unlike a mode-hook, this buffer is
            ;; already fully set up (real python-mode, real file) by the
            ;; time we reach this call.
            ;;
            ;; `require' (not just `fboundp' on `eglot-ensure') matters here:
            ;; on a fresh Emacs, `eglot-ensure' exists only as an autoload
            ;; stub until something actually calls it, which loads the real
            ;; file and only then defines `eglot--guess-contact' et al. Since
            ;; we deliberately never call `eglot-ensure' itself, relying on
            ;; `fboundp' would leave those internals void the first time a
            ;; notebook is opened in a session that never ran Eglot before.
            (unless (and (boundp 'eglot--managed-mode) eglot--managed-mode)
              (apply #'eglot--connect (eglot--guess-contact)))
          (error (message "[emjupy] Eglot couldn't start automatically: %s" err)))))
    buf))

(defun emjupy--goto-shadow-section (buf cell-id)
  "Move point in BUF to the start of CELL-ID's marked section."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (search-forward (emjupy--shadow-cell-marker cell-id) nil t)
        (forward-line 1)
      (goto-char (point-min)))))

(defun emjupy-commit-shadow-edit ()
  "Write each `# %% [emjupy:ID]' section in this buffer back into its cell
and refresh the notebook. Sections for cells you didn't touch are written
back unchanged; the shadow buffer and its Eglot connection stay alive for
next time."
  (interactive)
  (unless emjupy--edit-shadow-notebook
    (user-error "This buffer isn't an emjupy shadow-edit buffer"))
  (let* ((nb emjupy--edit-shadow-notebook)
         (sections (emjupy--parse-shadow-sections (buffer-string)))
         (nb-buf (emjupy-notebook-buffer nb))
         (updated 0))
    (cl-loop for cell across (emjupy-notebook-cells nb)
             do (let ((match (assq (emjupy-cell-id cell) sections)))
                  (when (and match (not (string= (cdr match) (emjupy-cell-source cell))))
                    (setf (emjupy-cell-source cell) (cdr match))
                    (setq updated (1+ updated)))))
    (set-buffer-modified-p nil)
    (when (and nb-buf (buffer-live-p nb-buf))
      (with-current-buffer nb-buf (emjupy--rerender-notebook))
      (switch-to-buffer nb-buf))
    (message "[emjupy] %d cell%s updated." updated (if (= updated 1) "" "s"))))

(defun emjupy-abort-shadow-edit ()
  "Discard uncommitted edits in the shared code buffer (reverting it to
match the cells' last-committed state) and switch back to the notebook.
The buffer and its Eglot connection are kept alive, not killed."
  (interactive)
  (when (y-or-n-p "Discard uncommitted edits in the shared code view? ")
    (let ((nb emjupy--edit-shadow-notebook))
      (when nb
        (erase-buffer)
        (insert (emjupy--build-shadow-content nb))
        (write-region (point-min) (point-max) buffer-file-name nil 'quiet)
        (set-buffer-modified-p nil)
        (when (buffer-live-p (emjupy-notebook-buffer nb))
          (switch-to-buffer (emjupy-notebook-buffer nb)))))))

(defun emjupy-edit-cell-externally ()
  "Edit the cell at point with real language tooling.

Code cells open a persistent, shared Python buffer containing ALL of
the notebook's code cells (marked `# %% [emjupy:ID]'), with Eglot
started automatically -- so completions, diagnostics, and go-to-def
are aware of definitions from every cell, not just this one, and
there's nothing extra for you to run. `C-c C-c' commits every section
you touched back into its cell; `C-c C-k' discards uncommitted edits.
The buffer and its Eglot connection persist, so only the first use
per session pays the language-server startup cost.

Markdown cells open their own simple edit buffer instead -- cross-cell
LSP awareness doesn't apply to prose."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
         (nb emjupy--buffer-notebook)
         (nb-buf (current-buffer)))
    (unless cell (user-error "No cell found at point"))
    (if (eq (emjupy-cell-type cell) 'code)
        (let ((buf (emjupy--ensure-shadow-buffer nb)))
          (emjupy--goto-shadow-section buf (emjupy-cell-id cell))
          (pop-to-buffer buf)
          (message "Editing notebook code (all cells, Eglot active) -- C-c C-c to commit, C-c C-k to discard."))
      (emjupy--edit-markdown-cell-externally cell nb-buf))))

;; --- Automatic in-place completion/eldoc: no buffer-switching needed -------
;; The pieces above (shadow buffer + Eglot) already give a *complete* editing
;; experience via `C-c ''; the functions below make its intelligence show up
;; directly while typing in a cell in the ORDINARY notebook buffer, with no
;; action from the user at all -- completion-at-point-functions and
;; eldoc-documentation-functions are standard Emacs extension points, so
;; whatever completion UI the user already has (plain M-TAB, Corfu, Company,
;; Emacs 30's completion-preview-mode, ...) picks this up automatically, the
;; same way it would for a normal, single-file Eglot-managed buffer.

(defun emjupy--shadow-section-start (buf cell-id)
  "Return the position where CELL-ID's source begins in shadow buffer BUF."
  (emjupy--goto-shadow-section buf cell-id)
  (with-current-buffer buf (point)))

(defun emjupy--cell-shadow-delegate (fn)
  "If point is in a code cell, sync + warm the shadow buffer, move an
indirect cursor there to the equivalent position, and call FN with
CELL-START, SHADOW-START, and the shadow BUFFER itself -- FN reads
`(point)' there (already positioned) to do its work. Returns FN's
value, or nil if point isn't in a code cell."
  (let ((cell (get-text-property (point) 'emjupy-cell))
        (nb emjupy--buffer-notebook))
    (when (and cell nb (eq (emjupy-cell-type cell) 'code) (emjupy-cell-overlay cell))
      (emjupy--sync-all-cells)
      (let* ((main-point (point))
             (cell-start (overlay-start (emjupy-cell-overlay cell)))
             (buf (emjupy--ensure-shadow-buffer nb))
             (shadow-start (emjupy--shadow-section-start buf (emjupy-cell-id cell))))
        (with-current-buffer buf
          (goto-char (+ shadow-start (- main-point cell-start)))
          (funcall fn cell-start shadow-start buf))))))

(defun emjupy--cell-completion-at-point ()
  "`completion-at-point-functions' entry: delegate to Eglot via the
shared code shadow buffer, so completions see definitions from every
cell in the notebook, automatically."
  (emjupy--cell-shadow-delegate
   (lambda (cell-start shadow-start buf)
     (let ((result (run-hook-with-args-until-success 'completion-at-point-functions)))
       (when (consp result)
         (let* ((orig-collection (nth 2 result))
                ;; Eglot's collection is a closure the completion UI calls
                ;; again later, when `current-buffer' is back to the
                ;; notebook buffer -- but it needs the shadow buffer's own
                ;; buffer-file-name/server context (e.g. for building LSP
                ;; requests), so force that context on every invocation.
                (wrapped (if (functionp orig-collection)
                             (lambda (string pred action)
                               (with-current-buffer buf
                                 (funcall orig-collection string pred action)))
                           orig-collection)))
           (append (list (+ cell-start (- (nth 0 result) shadow-start))
                         (+ cell-start (- (nth 1 result) shadow-start))
                         wrapped)
                   (nthcdr 3 result))))))))

(defun emjupy--cell-eldoc-function (callback)
  "`eldoc-documentation-functions' entry: request hover info from Eglot
directly via the shared code shadow buffer.

Deliberately does NOT delegate to `eldoc-documentation-functions' the
way completion does: Eglot's own `eglot-hover-eldoc-function' only
calls back when its buffer is visibly displayed in a window, which is
never true for this shadow buffer -- staying hidden in the background
is the whole point. `jsonrpc-request' (blocking) bypasses that gate."
  (emjupy--cell-shadow-delegate
   (lambda (_cell-start _shadow-start _buf)
     (when (and (fboundp 'eglot-current-server) (eglot-current-server)
                (fboundp 'jsonrpc-request)
                (fboundp 'eglot--server-capable)
                (ignore-errors (eglot--server-capable :hoverProvider)))
       (ignore-errors
         (let* ((server (eglot--current-server-or-lose))
                (resp (jsonrpc-request server :textDocument/hover
                                        (eglot--TextDocumentPositionParams)))
                (contents (plist-get resp :contents)))
           (unless (seq-empty-p contents)
             (funcall callback (eglot--hover-info contents (plist-get resp :range))))))
       t))))

(provide 'emjupy)
;;; emjupy.el ends here
