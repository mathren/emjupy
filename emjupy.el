;;; emjupy.el --- Minimal Jupyter Notebook editor -*- lexical-binding: t; -*-

;; Package-Requires: ((emacs "27.1") (websocket "1.14"))
;;; Commentary:
;; Native Jupyter notebook editing using the Jupyter HTTP+WebSocket REST API.
;; Treats the .ipynb JSON as the absolute source of truth.

;;; Code:

(require 'cl-lib)
(require 'json)
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

(cl-defstruct emjupy-server
  host port token base-url)

(cl-defstruct emjupy-kernel
  id name server ws status pending)

(cl-defstruct emjupy-cell
  type exec-count source outputs metadata overlay output-ov)

(cl-defstruct emjupy-notebook
  path server kernel cells metadata buffer)

;; =============================================================================
;; 2. HTTP & WebSocket Layer (Active Core)
;; =============================================================================

(defun emjupy-login (url token)
  "Connect to a Jupyter server and fetch available notebooks."
  (interactive "sJupyter Server URL (e.g., localhost:8888) or port (e.g., 8888): \nsToken: ")

  ;; If the user only types a 4-5 digit port number, prepend 'localhost:'
  (let ((clean-url (if (string-match-p "^[0-9]+$" url)
                       (concat "localhost:" url)
                     url)))

    (setq emjupy--current-server (make-emjupy-server :base-url clean-url :token token))
    (message "Logging into %s..." clean-url)
    (emjupy-list-notebooks)))

(defun emjupy-list-notebooks ()
  "Fetch the root directory contents and prompt the user to open or create a notebook."
  (interactive)
  (if (not emjupy--current-server)
      (error "Not logged in! Call emjupy-login first.")
    (let* ((data (emjupy--http-request "GET" emjupy--current-server "/api/contents"))
           (content (if (hash-table-p data) (gethash "content" data) []))
           (notebooks (list "[Create New Notebook]")))

      ;; Extract notebook paths from the JSON array
      (cl-loop for item across content
               when (and (hash-table-p item)
                         (string= (gethash "type" item) "notebook"))
               do (push (gethash "path" item) notebooks))

      ;; Prompt the user in the minibuffer safely in the main thread
      (let ((choice (completing-read "Select Notebook: " (nreverse notebooks))))
        (if (string= choice "[Create New Notebook]")
            (emjupy-create-notebook)
          (emjupy-open-notebook choice))))))

(defun emjupy-open-notebook (path)
  "Fetch the notebook JSON from the server, parse it, and render it in a buffer."
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

          ;; Tie metadata states back to the notebook structural object
          (setf (emjupy-notebook-server nb-struct) emjupy--current-server)
          (setf (emjupy-notebook-path nb-struct) path)
          (setf (emjupy-notebook-buffer nb-struct) buf)

	  (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              ;; Loop through and draw all cells
              (cl-loop for cell across (emjupy-notebook-cells nb-struct)
                       do (emjupy--render-cell cell)))
            (setq emjupy--buffer-notebook nb-struct))

          (switch-to-buffer buf)
          (message "Opened notebook: %s" path))))))

(defun emjupy-create-notebook ()
  "Create a brand new blank notebook on the Jupyter server and open it."
  (interactive)
  (if (not emjupy--current-server)
      (error "Not logged in!")
    (let* ((raw-name (read-string "New notebook name (default Untitled.ipynb): "))
           ;; Fallback to Untitled.ipynb if the user just presses Enter
           (name (if (string-empty-p raw-name) "Untitled.ipynb" raw-name))
           ;; Ensure it appends extension if omitted
           (filename (if (string-match-p "\\.ipynb$" name) name (concat name ".ipynb")))
           ;; Structure a completely blank nbformat v4 notebook structure
           (nb-payload (make-hash-table :test 'equal))
           (req-body (make-hash-table :test 'equal)))

      (puthash "cells" [] nb-payload)
      (puthash "metadata" (make-hash-table :test 'equal) nb-payload)
      (puthash "nbformat" 4 nb-payload)
      (puthash "nbformat_minor" 5 nb-payload)

      (puthash "type" "notebook" req-body)
      (puthash "format" "json" req-body) ;; Explicitly tell Jupyter this is JSON
      (puthash "content" nb-payload req-body)

      (message "Creating %s on Jupyter server..." filename)
      ;; PUT request saves the file directly onto the specified path
      (let ((response (emjupy--http-request "PUT" emjupy--current-server
                                            (concat "/api/contents/" filename)
                                            (json-serialize req-body))))
        (if response
            (emjupy-open-notebook filename)
          (error "Failed to write %s to server. Check Jupyter terminal logs for details" filename))))))

(defun emjupy-fetch-kernels ()
  "Fetch the list of running kernels from the server."
  (interactive)
  (if emjupy--current-server
      (let ((data (emjupy--http-request "GET" emjupy--current-server "/api/kernels")))
        (message "Kernels fetched: %s running." (length data)))
    (error "Not logged in! Call emjupy-login first.")))

(defun emjupy--http-request (method server path &optional body callback)
  "Internal wrapper for url-retrieve that reports exact HTTP errors."
  (let* ((url-request-method method)
         (url-request-data (when body (encode-coding-string body 'utf-8)))
         (url-automatic-caching nil)
         (token (emjupy-server-token server))
         (url-request-extra-headers
          (append `(("Content-Type" . "application/json"))
                  (when (and token (not (string-empty-p token)))
                    `(("Authorization" . ,(format "token %s" token))))))
         (base-url (emjupy-server-base-url server))
         ;; Append a timestamp to completely bust all caches on GET requests
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
              ;; 1. Extract the HTTP status code
              (when (re-search-forward "^HTTP/[0-9.]+ \\([0-9]+\\)" nil t)
                (setq status (string-to-number (match-string 1))))

              ;; 2. Move past the HTTP headers
              (re-search-forward "\r?\n\r?\n" nil t)

              (let ((json-str (buffer-substring-no-properties (point) (point-max))))
                (kill-buffer buffer)

                ;; 3. STOP silently swallowing errors! Report exact Jupyter rejections.
                (if (>= status 400)
                    (error "[Jupyter HTTP %d] %s: %s" status method json-str)

                  ;; 4. Parse JSON safely
                  (condition-case err
                      (json-parse-string json-str :object-type 'hash-table :array-type 'array)
                    (error (error "JSON Parse Error on %s: %s\nRaw output: %s" path err json-str))))))))))))

;; =============================================================================
;; 3. .ipynb I/O (The Source of Truth)
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
    (puthash "metadata" (emjupy-notebook-metadata nb) data)

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
                 (puthash "execution_count" nil c-hash))
               (aset cells-vec i c-hash))
      (puthash "cells" cells-vec data))
    (json-serialize data)))

;; =============================================================================
;; 4. Output Rendering
;; =============================================================================

(defun emjupy--fontify-code (beg end)
  "Apply python-mode syntax highlighting to the region."
  (let ((font-lock-mode t)
        (major-mode 'python-mode))
    (unless (featurep 'python) (require 'python))
    (font-lock-fontify-region beg end)))

(defun emjupy--render-cell (cell)
  "Render a CELL at point using overlays for the visual boundary boxes."
  (let* ((type (emjupy-cell-type cell))
         (source (emjupy-cell-source cell))
         (outputs (emjupy-cell-outputs cell))
         (exec-val (emjupy-cell-exec-count cell))
         (exec-str (if exec-val (number-to-string exec-val) " "))
         (src-start (point)))

    ;; 1. Insert and fontify the source code
    (insert (if (string-empty-p source) "\n" source))
    (unless (string-suffix-p "\n" source) (insert "\n"))

    ;; Tag the text with the struct so we can easily look it up later
    (put-text-property src-start (point) 'emjupy-cell cell)
    (when (eq type 'code)
      (emjupy--fontify-code src-start (point)))

    ;; 2. Create the Source Overlay (Draws the top box and bottom line)
    (let* ((ov (make-overlay src-start (point)))
           (header (propertize (format "┌─ [In: %s] %s ──────────────────────────────────────────────────────────────────────────────────────────\n"
                                       exec-str (if (eq type 'code) "python" "markdown"))
                               'face 'shadow))
           ;; If there are outputs, we skip the bottom line here and let the output box draw it
           (footer (if (or (not outputs) (= (length outputs) 0))
                       (propertize "└─────────────────────────────────────────────────────────────────────────────────────────────────────────┘\n" 'face 'shadow)
                     "")))
      (overlay-put ov 'before-string header)
      (overlay-put ov 'after-string footer)
      (setf (emjupy-cell-overlay cell) ov))

    ;; 3. Insert and box the Outputs (if any)
    (when (and (eq type 'code) outputs (> (length outputs) 0))
      (let ((out-start (point)))
        (cl-loop for out across outputs
                 do (let ((out-type (gethash "output_type" out)))
                      (cond
                       ((string= out-type "stream")
                        (insert (gethash "text" out)))
                       ((string= out-type "execute_result")
                        (let ((data (gethash "data" out)))
                          (when (gethash "text/plain" data)
                            (insert (gethash "text/plain" data) "\n")))))))

        (unless (string-suffix-p "\n" (buffer-substring (max (point-min) (- (point) 1)) (point)))
          (insert "\n"))

        ;; Create Output Overlay
        (let* ((ov (make-overlay out-start (point)))
               (header (propertize (format "┌─ [Out: %s] ─────────────────────────────────\n" exec-str) 'face 'shadow))
               (footer (propertize "└─────────────────────────────────────────────┘\n" 'face 'shadow)))
          (overlay-put ov 'before-string header)
          (overlay-put ov 'after-string footer)
          (setf (emjupy-cell-output-ov cell) ov))))

    ;; Gap between cells
    (insert "\n")))

(defun emjupy--render-image-output (base64-string)
  "Convert BASE64-STRING from cell output into an Emacs image descriptor."
  (let ((image-data (base64-decode-string base64-string)))
    (create-image image-data 'png t)))

;; =============================================================================
;; 5. Kernel Actions
;; =============================================================================

(defun emjupy-restart-kernel (kernel)
  "Issue a restart request to KERNEL via the HTTP API."
  (let ((server (emjupy-kernel-server kernel))
        (kernel-id (emjupy-kernel-id kernel)))
    (emjupy--http-request "POST" server (format "/api/kernels/%s/restart" kernel-id))
    (setf (emjupy-kernel-status kernel) 'restarting)
    (message "Kernel %s restarting..." kernel-id)))

;; =============================================================================
;; 6. Unit Tests (ERT)
;; =============================================================================

(ert-deftest emjupy-test-ipynb-roundtrip ()
  "Test parsing and serializing a notebook preserves the structure and acts as the source of truth."
  (let* ((raw-json "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"import numpy as np\\n\",\"print(1)\"]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")
         (parsed-nb (emjupy--parse-ipynb raw-json))
         (first-cell (aref (emjupy-notebook-cells parsed-nb) 0))
         (reserialized (emjupy--serialize-notebook parsed-nb)))

    (should (eq (emjupy-cell-type first-cell) 'code))
    (should (string= (emjupy-cell-source first-cell) "import numpy as np\nprint(1)"))
    (should (string-match-p "\"nbformat\":4" reserialized))
    (should (string-match-p "\"import numpy as np\\\\n\"" reserialized))))

(ert-deftest emjupy-test-render-image ()
  "Test that base64 PNG data is correctly parsed into an Emacs image object."
  (let* ((b64-png "iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAQAAAC1HAwCAAAAC0lEQVR42mNkYAAAAAYAAjCB0C8AAAAASUVORK5CYII=")
         (img (emjupy--render-image-output b64-png)))
    (should (eq (car img) 'image))
    (should (eq (plist-get (cdr img) :type) 'png))
    (should (plist-get (cdr img) :data))))

(ert-deftest emjupy-test-kernel-state-restart ()
  "Test that restarting a kernel correctly updates the internal struct state."
  (cl-letf (((symbol-function 'emjupy--http-request) (lambda (&rest _) t)))
    (let* ((server (make-emjupy-server :host "localhost" :port 8888 :base-url "http://localhost:8888"))
           (kernel (make-emjupy-kernel :id "test-id" :server server :status 'idle)))
      (should (eq (emjupy-kernel-status kernel) 'idle))
      (emjupy-restart-kernel kernel)
      (should (eq (emjupy-kernel-status kernel) 'restarting)))))

(provide 'emjupy)
;;; emjupy.el ends here
