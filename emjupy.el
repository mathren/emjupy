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

;; --- State model -----------------------------------------------------------
;; emjupy supports several notebooks open at once, from several servers at
;; once (e.g. two ssh tunnels on different local ports). Nothing about a
;; connection is global: a server owns its own token and XSRF cookie, a
;; notebook owns its own kernel, and a kernel owns its own WebSocket and its
;; own table of in-flight requests. The only globals left are the registry of
;; known servers and a "most recently used" default for commands invoked from
;; outside a notebook buffer.

(defvar emjupy--servers (make-hash-table :test 'equal)
  "Registry of known servers, keyed by base-url string.
Lets several Jupyter servers -- typically several ssh tunnels on
different local ports -- be live in one Emacs session.")

(defvar emjupy--current-server nil
  "Default `emjupy-server' for commands run outside a notebook buffer.
Inside a notebook buffer the notebook's OWN server is always used
instead; this is only the fallback for things like `emjupy-login'.")

(defvar-local emjupy--buffer-notebook nil
  "The `emjupy-notebook' struct associated with the current buffer.")

(cl-defstruct emjupy-server
  host port token base-url
  ;; Per-server, not global: two servers issue different XSRF cookies, and
  ;; replaying one server's cookie at another gets a 403.
  xsrf)

(cl-defstruct emjupy-kernel
  id name server ws pending
  ;; Backlink to the notebook this kernel drives, so an incoming WebSocket
  ;; frame can be routed to the right buffer without consulting any global.
  notebook)

(defvar emjupy--next-cell-id 0
  "Monotonically increasing counter for assigning stable `emjupy-cell' ids.")

(defun emjupy--new-cell-id ()
  "Return a fresh, never-reused cell id."
  (setq emjupy--next-cell-id (1+ emjupy--next-cell-id)))

(cl-defstruct emjupy-cell
  id type exec-count source outputs metadata overlay output-ov nb-id)

(cl-defstruct emjupy-notebook
  path server kernel cells metadata buffer shadow-buffer)

;; --- Lookups ---------------------------------------------------------------

(defun emjupy--intern-server (base-url token)
  "Return the registered `emjupy-server' for BASE-URL, creating it if new.
Re-logging into a server already open updates its token and keeps the
same object, so notebooks already pointing at it stay valid."
  (let ((server (gethash base-url emjupy--servers)))
    (if server
        (progn (setf (emjupy-server-token server) token) server)
      (setq server (make-emjupy-server :base-url base-url :token token))
      (puthash base-url server emjupy--servers)
      server)))

(defun emjupy--server ()
  "Return the server this buffer's notebook belongs to, else the default."
  (or (and emjupy--buffer-notebook (emjupy-notebook-server emjupy--buffer-notebook))
      emjupy--current-server
      (user-error "Not logged in! Call `emjupy-login' first")))

(defun emjupy--notebook ()
  "Return this buffer's notebook, or signal a clear error."
  (or emjupy--buffer-notebook
      (user-error "No emjupy notebook associated with this buffer")))

(defun emjupy--kernel ()
  "Return the `emjupy-kernel' driving this buffer's notebook, or nil."
  (and emjupy--buffer-notebook (emjupy-notebook-kernel emjupy--buffer-notebook)))

(defun emjupy--notebook-buffers ()
  "Return the list of live emjupy notebook buffers."
  (cl-remove-if-not
   (lambda (b) (buffer-local-value 'emjupy--buffer-notebook b))
   (buffer-list)))

(defun emjupy--server-label (server)
  "Return a short human label for SERVER."
  (emjupy-server-base-url server))

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
    (define-key map (kbd "C-c C-x C-c") #'emjupy-reconnect-kernel)

    ;; Multiple notebooks / servers
    (define-key map (kbd "C-c C-x b") #'emjupy-switch-notebook)
    (define-key map (kbd "C-c C-x s") #'emjupy-status)
    (define-key map (kbd "C-c C-x l") #'emjupy-login)

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
  "Connect to a Jupyter server and open a notebook on it.

No kernel is started: use \\[emjupy-connect-kernel-interactive] to pick
an already-running kernel or spawn a new one. That matters once several
servers are in play, since auto-spawning would leave an idle kernel
process behind on every server you log into.

Several servers can be logged into at once -- each ssh tunnel is just a
different local port, and each gets its own `emjupy-server' object."
  (interactive "sJupyter Server URL (e.g., localhost:8888) or port (e.g., 8888): \nsToken: ")
  (let* ((clean-url (if (string-match-p "^[0-9]+$" url)
                        (concat "localhost:" url)
                      url))
         (server (emjupy--intern-server clean-url token)))
    (setq emjupy--current-server server)

    ;; Ping the API root to harvest the _xsrf cookie before proceeding
    (condition-case nil
        (emjupy--http-request "GET" server "/api")
      (error nil))

    (message "Logging into %s..." clean-url)
    (emjupy-list-notebooks server)))

(defun emjupy-list-notebooks (&optional server)
  "Fetch SERVER's root contents and prompt to open or create a notebook.
Returns the notebook buffer. SERVER defaults to this buffer's server,
or the last one logged into."
  (interactive)
  (let* ((server (or server (emjupy--server)))
         (data (emjupy--http-request "GET" server "/api/contents"))
         (content (if (hash-table-p data) (gethash "content" data) []))
         (notebooks (list "[Create New Notebook]")))

    (cl-loop for item across content
             when (and (hash-table-p item)
                       (string= (gethash "type" item) "notebook"))
             do (push (gethash "path" item) notebooks))

    (let ((choice (completing-read (format "Select Notebook (%s): "
                                           (emjupy--server-label server))
                                   (nreverse notebooks))))
      (if (string= choice "[Create New Notebook]")
          (emjupy-create-notebook server)
        (emjupy-open-notebook choice server)))))

(defun emjupy--notebook-buffer-name (path server)
  "Return the buffer name for PATH on SERVER.
The server is part of the name: the same notebook path can exist on two
different servers, and one buffer cannot represent both."
  (format "*emjupy: %s [%s]*" path (emjupy--server-label server)))

(defun emjupy-open-notebook (path &optional server)
  "Fetch notebook JSON from SERVER, parse it, and render it in `emjupy-mode'.
Returns the notebook buffer."
  (let* ((server (or server (emjupy--server))))
    (message "Fetching notebook: %s..." path)
    (let* ((response-data (emjupy--http-request "GET" server (concat "/api/contents/" path)))
           (content-hash (and response-data (gethash "content" response-data))))
      (if (not content-hash)
          (error "Failed to fetch notebook content from server")
        (let* ((ipynb-json (json-serialize content-hash))
               (nb-struct (emjupy--parse-ipynb ipynb-json))
               (buf-name (emjupy--notebook-buffer-name path server))
               (buf (get-buffer-create buf-name)))

          (setf (emjupy-notebook-server nb-struct) server)
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
          ;; Warm up the code shadow-buffer + Eglot now, in the background,
          ;; so completions are ready once the user starts typing instead of
          ;; paying the LSP server startup cost on the first keystroke.
          (ignore-errors
            (with-current-buffer buf (emjupy--ensure-shadow-buffer nb-struct)))
          (message "Opened notebook: %s. Press C-c C-z to select or spawn a kernel." path)
          buf)))))

(defun emjupy-create-notebook (&optional server)
  "Create a brand new blank notebook on SERVER and open it."
  (interactive)
  (let* ((server (or server (emjupy--server)))
         (raw-name (read-string "New notebook name (default Untitled.ipynb): "))
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
    (let ((response (emjupy--http-request "PUT" server
                                          (concat "/api/contents/" filename)
                                          (json-serialize req-body))))
      (if response
          (emjupy-open-notebook filename server)
        (error "Failed to write %s to server" filename)))))

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
                  ;; Automatically inject XSRF tokens to bypass Jupyter 403 CSRF blocks.
                  ;; Read from THIS server: a cookie issued by another server
                  ;; (a second tunnel, say) would just earn a 403.
                  (when (emjupy-server-xsrf server)
                    `(("X-XSRFToken" . ,(emjupy-server-xsrf server))
                      ("Cookie" . ,(format "_xsrf=%s" (emjupy-server-xsrf server)))))))
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

              ;; Harvest XSRF cookie from response, onto THIS server
              (goto-char (point-min))
              (when (re-search-forward "^Set-Cookie:.*_xsrf=\\([^; \r\n]+\\)" nil t)
                (setf (emjupy-server-xsrf server) (match-string 1)))

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
                       ;; The notebook's OWN nbformat >=4.5 cell id, kept
                       ;; distinct from our internal buffer-local `id' so a
                       ;; round trip doesn't renumber cells for collaborators.
                       :nb-id (let ((v (gethash "id" c-data)))
                                (and (stringp v) v))
                       :type (intern (gethash "cell_type" c-data))
                       :exec-count (gethash "execution_count" c-data)
                       :source (let ((src (gethash "source" c-data)))
                                 (if (vectorp src) (mapconcat #'identity src "") src))
                       :outputs (gethash "outputs" c-data)
                       :metadata (gethash "metadata" c-data))))
    nb))

(defun emjupy--source-lines (source)
  "Split SOURCE into an nbformat `source' array.
Every line keeps its newline EXCEPT the last, matching the nbformat
convention -- appending one unconditionally makes each save/reload
cycle grow the cell by a trailing blank line."
  (let* ((lines (split-string (or source "") "\n"))
         (n (length lines)))
    (vconcat
     (cl-loop for line in lines
              for i from 1
              collect (if (= i n) line (concat line "\n"))))))

(defun emjupy--nb-cell-id (cell)
  "Return a stable nbformat >=4.5 id for CELL, creating one if needed.
Cells created inside emjupy have no notebook id yet; nbformat_minor 5
requires one on every cell, so a notebook missing them fails
`nbformat.validate' and is rejected by nbconvert and friends."
  (or (emjupy-cell-nb-id cell)
      (setf (emjupy-cell-nb-id cell)
            (substring (md5 (format "%s-%s" (emjupy-cell-id cell) (random 1000000))) 0 8))))

(defun emjupy--normalize-output (out)
  "Return OUT with the fields the nbformat v4 schema requires.
Kernels don't always send `metadata', and an execute_result without
`execution_count' is invalid -- both make a saved notebook fail
validation even though it looks fine in emjupy."
  (if (not (hash-table-p out))
      out
    (let ((o (copy-hash-table out))
          (type (gethash "output_type" out)))
      (when (member type '("execute_result" "display_data"))
        (unless (gethash "data" o) (puthash "data" (make-hash-table :test 'equal) o))
        (unless (gethash "metadata" o) (puthash "metadata" (make-hash-table :test 'equal) o)))
      (when (string= type "execute_result")
        (unless (gethash "execution_count" o) (puthash "execution_count" :null o)))
      (when (string= type "stream")
        (unless (gethash "name" o) (puthash "name" "stdout" o))
        (unless (gethash "text" o) (puthash "text" "" o)))
      o)))

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
               (puthash "id" (emjupy--nb-cell-id cell) c-hash)
               (puthash "metadata" (or (emjupy-cell-metadata cell) (make-hash-table)) c-hash)
               (puthash "source" (emjupy--source-lines (emjupy-cell-source cell)) c-hash)
               (when (eq (emjupy-cell-type cell) 'code)
                 (puthash "outputs"
                          (vconcat (mapcar #'emjupy--normalize-output
                                           (append (or (emjupy-cell-outputs cell) []) nil)))
                          c-hash)
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

        (puthash msg-id cell (emjupy-kernel-pending kernel))
        (emjupy--ws-send json-payload kernel)
        (message "[emjupy] Executing cell (%s)..." msg-id)))))

(defun emjupy-execute-cell-and-goto-next ()
  "Execute cell at point and advance to the next cell."
  (interactive)
  (emjupy-execute-cell-at-point)
  (emjupy-next-cell))

(defun emjupy--server-parts (&optional server)
  "Return a plist describing SERVER's base-url.

Keys are :scheme, :ws-scheme, :host, :port and :path. Accepts a bare
port (\"8888\"), a host:port pair, a full http(s) URL, and an optional
trailing base path -- the last of which matters for servers reached
through an SSH tunnel into a proxied setup (e.g. a JupyterHub
single-user server at localhost:8888/user/alice), where the prefix has
to survive into the WebSocket URL as well as the REST calls."
  (let* ((server (or server emjupy--current-server))
         (raw (emjupy-server-base-url server))
         (secure (string-prefix-p "https" raw))
         (stripped (replace-regexp-in-string "\\`https?://" "" raw))
         (slash (string-match-p "/" stripped))
         (hostport (if slash (substring stripped 0 slash) stripped))
         (path (if slash
                   (string-trim-right (substring stripped slash) "/+")
                 ""))
         (parts (split-string hostport ":"))
         (host (if (string-empty-p (or (car parts) "")) "localhost" (car parts)))
         (port (or (cadr parts) (if secure "443" "80"))))
    (list :scheme (if secure "https" "http")
          :ws-scheme (if secure "wss" "ws")
          :host host :port port :path path)))

(defun emjupy--server-host-port ()
  "Return (HOST . PORT) parsed from the current server's base-url."
  (let ((p (emjupy--server-parts)))
    (cons (plist-get p :host) (plist-get p :port))))

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
        (cl-loop for out in (emjupy--outputs-for-render outputs)
                 do (let ((out-type (gethash "output_type" out)))
                      (cond
                       ((string= out-type "stream")
                        (insert (emjupy--mime-text (gethash "text" out))))
                       ((or (string= out-type "execute_result")
                            (string= out-type "display_data"))
                        (emjupy--insert-rich-output (gethash "data" out)))
                       ((string= out-type "error")
                        (let ((ename (gethash "ename" out))
                              (evalue (gethash "evalue" out))
                              (traceback (gethash "traceback" out)))
                          (insert (format "Error (%s): %s\n" ename evalue))
                          (when (or (vectorp traceback) (listp traceback))
                            (cl-loop for line in (append traceback nil)
                                     do (insert (replace-regexp-in-string
                                                 "\033\\[[0-9;]*m" ""
                                                 (emjupy--mime-text line))
                                                "\n"))))))))

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

(defconst emjupy--image-mime-types
  '(("image/png"     . png)
    ("image/jpeg"    . jpeg)
    ("image/gif"     . gif)
    ("image/svg+xml" . svg))
  "MIME types emjupy can render inline, in preference order.
Maps each to the Emacs image type used to display it.")

(defun emjupy--mime-text (value)
  "Return VALUE as a string.
nbformat stores multi-line MIME payloads (`text/plain', stream `text',
tracebacks) as either a single string or an array of line strings
depending on where the JSON came from -- the Contents API joins them,
a notebook read straight off disk does not."
  (cond
   ((null value) "")
   ((stringp value) value)
   ((vectorp value) (mapconcat #'identity (append value nil) ""))
   ((listp value) (mapconcat #'identity value ""))
   (t (format "%s" value))))

(defun emjupy--image-displayable-p (type)
  "Return non-nil if this Emacs can actually display an image of TYPE.
Both halves matter: a tty frame can't show images at all, and a build
without the relevant library (very common for `emacs-nox') will make
`create-image' signal rather than degrade."
  (and (display-graphic-p)
       (image-type-available-p type)))

(defun emjupy--insert-rich-output (data)
  "Insert the best available representation of the DATA MIME bundle at point.

Prefers an inline image when this Emacs can genuinely display one, and
otherwise falls back to the bundle's own `text/plain' representation
plus a short note. Without that fallback a figure renders as a
silently empty output box on a terminal or no-image Emacs: rendering
happens inside the WebSocket callback, where websocket.el swallows the
`Invalid image type' error `create-image' raises."
  (let* ((image (cl-loop for (mime . type) in emjupy--image-mime-types
                         for payload = (and data (gethash mime data))
                         when payload return (list mime type payload)))
         (text (and data (gethash "text/plain" data))))
    (cond
     ((and image (emjupy--image-displayable-p (nth 1 image)))
      (condition-case err
          (progn (insert-image (emjupy--render-image-output (nth 2 image) (nth 1 image)))
                 (insert "\n"))
        (error
         (insert (format "[emjupy: could not render %s: %s]\n"
                         (nth 0 image) (error-message-string err)))
         (when text (insert (emjupy--mime-text text) "\n")))))
     (image
      (when text (insert (emjupy--mime-text text) "\n"))
      (insert (propertize
               (format "[emjupy: %s output (%d bytes) not shown -- this Emacs has no %s image support]\n"
                       (nth 0 image) (length (nth 2 image)) (nth 1 image))
               'face 'shadow)))
     (text (insert (emjupy--mime-text text) "\n"))
     ;; Some bundle we don't know how to show at all: say so rather than
     ;; rendering an empty box.
     (data
      (let ((mimes (cl-loop for k being the hash-keys of data collect k)))
        (when mimes
          (insert (propertize (format "[emjupy: unsupported output types: %s]\n"
                                      (string-join mimes ", "))
                              'face 'shadow))))))))

(defcustom emjupy-deduplicate-image-outputs t
  "When non-nil, render a repeated identical image only once per cell.

A cell whose last expression is a figure gets the SAME picture twice
from the kernel: once as the `execute_result' repr and again as the
inline backend's `display_data'. That is kernel-side behaviour, so the
duplicate is dropped only at render time -- the cell's `outputs' vector
still holds exactly what the kernel sent, and is saved back to the
.ipynb unchanged, so the file stays byte-faithful for other clients."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--output-image-key (out)
  "Return a key identifying OUT's image payload, or nil if it carries none.
The payload is hashed rather than compared directly: a figure is
hundreds of kilobytes of base64, and this runs on every re-render."
  (when (hash-table-p out)
    (let ((data (gethash "data" out)))
      (when (hash-table-p data)
        (cl-loop for (mime . _type) in emjupy--image-mime-types
                 for payload = (gethash mime data)
                 when payload
                 return (cons mime (md5 (emjupy--mime-text payload))))))))

(defun emjupy--outputs-for-render (outputs)
  "Return OUTPUTS as a list, with repeated identical images dropped.

Only image-bearing outputs are collapsed, and only against images
already seen in the same cell: two `print' calls emitting the same text
are genuinely two outputs and both must show."
  (let ((all (append (or outputs []) nil)))
    (if (not emjupy-deduplicate-image-outputs)
        all
      (let ((seen (make-hash-table :test 'equal))
            (acc nil))
        (dolist (out all (nreverse acc))
          (let ((key (emjupy--output-image-key out)))
            (cond
             ((null key) (push out acc))
             ((gethash key seen) nil)   ; same picture again -- skip
             (t (puthash key t seen)
                (push out acc)))))))))

(defun emjupy--render-image-output (payload &optional type)
  "Convert PAYLOAD output into an Emacs image object of TYPE (default png).
Bitmap MIME payloads arrive base64-encoded; `image/svg+xml' arrives as
literal markup and must not be decoded."
  (let* ((type (or type 'png))
         (image-data (if (eq type 'svg)
                         (emjupy--mime-text payload)
                       (base64-decode-string (emjupy--mime-text payload)))))
    (create-image image-data type t)))

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

(defun emjupy-switch-notebook ()
  "Switch to another open emjupy notebook, labelled by server."
  (interactive)
  (let* ((buffers (emjupy--notebook-buffers))
         (names (mapcar #'buffer-name buffers)))
    (unless names (user-error "No emjupy notebooks are open"))
    (switch-to-buffer (completing-read "Notebook: " names nil t))))

(defun emjupy-status ()
  "Report every open notebook, its server, and its kernel."
  (interactive)
  (let ((buffers (emjupy--notebook-buffers)))
    (if (not buffers)
        (message "[emjupy] No notebooks open.")
      (message
       "%s"
       (mapconcat
        (lambda (b)
          (let* ((nb (buffer-local-value 'emjupy--buffer-notebook b))
                 (k (emjupy-notebook-kernel nb)))
            (format "%-28s %-22s %s"
                    (emjupy-notebook-path nb)
                    (emjupy--server-label (emjupy-notebook-server nb))
                    (cond ((null k) "no kernel")
                          ((emjupy--ws-live-p k)
                           (format "kernel %s (connected)" (emjupy-kernel-id k)))
                          (t (format "kernel %s (disconnected)" (emjupy-kernel-id k)))))))
        buffers "\n")))))

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
  "Return a stable on-disk path for NB's shadow Python file.
The server is folded into the name: two servers can both host
`analysis.ipynb', and one shadow file cannot stand for both."
  (let* ((dir (expand-file-name "emjupy-shadow" temporary-file-directory))
         (server (emjupy-notebook-server nb))
         (tag (if server (emjupy--server-label server) "local"))
         (safe-name (replace-regexp-in-string
                     "[^A-Za-z0-9._-]" "_"
                     (format "%s__%s" tag (or (emjupy-notebook-path nb) "untitled")))))
    (make-directory dir t)
    (expand-file-name (concat safe-name ".py") dir)))

(defun emjupy--ensure-shadow-buffer (nb)
  "Get-or-create NB's persistent code shadow-buffer, refresh its content to
match the current cells, and make sure Eglot is (or becomes) attached --
automatically, with nothing for the user to run."
  (let ((buf (emjupy-notebook-shadow-buffer nb))
        (content (emjupy--build-shadow-content nb))
        (path (emjupy--shadow-file-path nb)))
    (unless (buffer-live-p buf)
      ;; A buffer may already be visiting this path from an earlier open of
      ;; the same notebook. Reuse it rather than writing the file behind its
      ;; back, which would leave a stale modtime and make the next visit
      ;; interrupt with "changed on disk. Reread from disk?".
      (setq buf (find-buffer-visiting path))
      (unless (buffer-live-p buf)
        ;; Write the real content to disk BEFORE visiting the file.
        ;;
        ;; Once any server is running for this project, Eglot auto-manages a
        ;; newly visited file from `find-file-hook' and sends didOpen with
        ;; whatever the buffer holds at that instant. Visiting first and
        ;; filling the buffer afterwards therefore hands the server a stale
        ;; (or empty) document -- which is why the SECOND notebook opened in
        ;; a session silently got no hover and almost no completions, even
        ;; though `eglot--managed-mode' reported t.
        (write-region content nil path nil 'quiet)
        (setq buf (find-file-noselect path)))
      (setf (emjupy-notebook-shadow-buffer nb) buf)
      (with-current-buffer buf
        ;; `find-file-noselect' already picked python-mode from the .py
        ;; suffix. Re-running it would `kill-all-local-variables', tearing
        ;; down both Eglot's buffer-local state and the two variables set
        ;; just below.
        (unless (derived-mode-p 'python-mode 'python-ts-mode)
          (python-mode))
        (emjupy-shadow-edit-mode 1)
        (setq emjupy--edit-shadow-notebook nb)))
    (with-current-buffer buf
      (unless (string= content (buffer-string))
        (erase-buffer)
        (insert content)
        (write-region (point-min) (point-max) buffer-file-name nil 'quiet)
        ;; Record the modtime we just created, otherwise this buffer looks
        ;; stale to Emacs forever after and every later visit prompts.
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
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
          ;; Clamp: if the cell's shadow section is shorter than the offset
          ;; (mid-edit, before a resync), an unclamped goto-char signals
          ;; `args-out-of-range' and kills completion for the whole buffer.
          (goto-char (max (point-min)
                          (min (point-max)
                               (+ shadow-start (- main-point cell-start)))))
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

(defun emjupy--eglot-capable-p (&rest capabilities)
  "Return non-nil if the current Eglot server advertises CAPABILITIES.
`eglot--server-capable' was renamed `eglot-server-capable' in Emacs
30, so guarding on the old private name alone silently disables eldoc
on newer Emacs."
  (cond
   ((fboundp 'eglot-server-capable) (apply #'eglot-server-capable capabilities))
   ((fboundp 'eglot--server-capable) (apply #'eglot--server-capable capabilities))
   (t nil)))

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
                (ignore-errors (emjupy--eglot-capable-p :hoverProvider)))
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
