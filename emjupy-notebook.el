;;; emjupy-notebook.el --- Notebook session management and .ipynb I/O for emjupy  -*- lexical-binding: t; -*-

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

;; Logging in to a server, listing/opening/creating notebooks, and reading
;; and writing strict nbformat v4 JSON.
;;
;; One port = one kernel: `emjupy-login' adopts the kernel already running
;; behind a tunnel and binds it to that server, so opening a notebook lands
;; in the live REPL.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-eglot)

;; Defined in emjupy.el, which requires this file: the mode function is
;; only ever called at runtime, so the cycle is harmless.
(declare-function emjupy-mode "emjupy")

(defvar emjupy--port-history nil
  "Minibuffer history for server ports and URLs.")
(defvar emjupy--notebook-history nil
  "Minibuffer history for notebook names and paths.")
(defvar emjupy--kernel-history nil
  "Minibuffer history for kernel choices.")

;; Tokens deliberately have NO history: they are secrets, and a history
;; would put them back on screen -- and into savehist, for anyone who
;; persists it -- which is the whole thing `read-passwd' avoids.

(defun emjupy--read-token (prompt)
  "Read a token or password for PROMPT without echoing it.
`read-string' prints what is typed straight into the minibuffer, so the
credential ends up on screen and in the minibuffer history."
  (read-passwd prompt))

(defun emjupy--normalize-url (url)
  "Return URL as a host:port base-url. A bare port means localhost."
  (if (string-match-p "\\`[0-9]+\\'" url)
      (concat "localhost:" url)
    url))

(defun emjupy--server-reachable-p (server)
  "Return non-nil if SERVER answers /api/status with SERVER's credentials."
  (condition-case nil
      (and (emjupy--http-request "GET" server "/api/status") t)
    (error nil)))

(defun emjupy--resolve-token (base-url explicit)
  "Work out the token for BASE-URL, prompting only when unavoidable.

Order: EXPLICIT, then the token already registered for this server,
then no token at all (many tunnelled servers are started with
`--IdentityProvider.token='), and only then ask. Asking every time is
what made logging into a second tunnel tedious."
  (or explicit
      (let ((known (gethash base-url emjupy--servers)))
        (and known
             (let ((tok (emjupy-server-token known)))
               (and tok (not (string-empty-p tok))
                    (emjupy--server-reachable-p known)
                    tok))))
      (and (emjupy--server-reachable-p
            (make-emjupy-server :base-url base-url :token ""))
           "")
      (emjupy--read-token (format "Token for %s: " base-url))))

(defun emjupy--server-kernels (server)
  "Return SERVER's running kernels as a list of (LABEL . ID)."
  (cl-loop for k across (emjupy--http-request "GET" server "/api/kernels")
           collect (cons (format "%s (%s)" (gethash "name" k) (gethash "id" k))
                         (gethash "id" k))))

(defun emjupy--bind-server-kernel (server)
  "Pick the kernel this SERVER's port should be bound to, and remember it.

One port = one kernel. The usual case is a single kernel already
running behind the tunnel, which is adopted silently -- no prompt, and
no second kernel process spawned next to the one you started. Only a
server with nothing running gets a fresh kernel."
  (let* ((kernels (emjupy--server-kernels server))
         (id (cond
              ((= (length kernels) 1) (cdar kernels))
              ((null kernels)
               (let ((payload (make-hash-table :test 'equal)))
                 (puthash "name" "python3" payload)
                 (gethash "id" (emjupy--http-request
                                "POST" server "/api/kernels"
                                (json-serialize payload)))))
              (t (cdr (assoc (completing-read
                              (format "Kernel on %s: " (emjupy--server-label server))
                              (mapcar #'car kernels) nil t nil
                              'emjupy--kernel-history)
                             kernels))))))
    (setf (emjupy-server-kernel-id server) id)
    id))

;;;###autoload
(defun emjupy-login (url &optional token)
  "Connect to the Jupyter server at URL and open one of its notebooks.

URL may be a bare port (\"8888\"), a host:port pair, or a full http(s)
URL. A bare port means localhost -- the usual case when the server is
reached through `ssh -L'.

Typical workflow: start a notebook kernel on the remote host, forward
its port with `ssh -L', then run this on that local port. emjupy adopts
the kernel already running behind the tunnel, so opening a notebook
puts you straight into that live REPL. A token is only asked for if the
server actually needs one.

One port = one kernel: log in once per tunnel and each port keeps its
own kernel, so several remote sessions can be live in one Emacs. Use
\\[emjupy-connect-kernel-interactive] to give an individual notebook a
different kernel. With a prefix argument, always prompt for the token."
  (interactive
   (list (read-string "Jupyter port or URL (e.g. 8888): "
                      nil 'emjupy--port-history)
         (when current-prefix-arg (emjupy--read-token "Token: "))))
  (let* ((base-url (emjupy--normalize-url url))
         (token (emjupy--resolve-token base-url token))
         (server (emjupy--intern-server base-url token)))
    (setq emjupy--current-server server)

    ;; Prime the XSRF cookie before anything tries to write. /api does not
    ;; hand one out; only the HTML pages do.
    (ignore-errors (emjupy--harvest-xsrf server))

    (let ((kernel-id (emjupy--bind-server-kernel server)))
      (message "Connected to %s, kernel %s." base-url kernel-id))
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
                                   (nreverse notebooks)
                                   nil nil nil 'emjupy--notebook-history)))
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
          ;; Attach this notebook to the kernel bound to its port, so opening
          ;; it drops you into the REPL already running behind that tunnel
          ;; rather than into a dead buffer needing a separate connect step.
          (when-let ((kernel-id (emjupy-server-kernel-id server)))
            (with-current-buffer buf
              (condition-case err
                  (emjupy-connect-kernel nb-struct kernel-id)
                (error (message "[emjupy] Could not attach kernel %s: %s"
                                kernel-id (error-message-string err))))))
          ;; Warm up the code shadow-buffer + Eglot now, in the background,
          ;; so completions are ready once the user starts typing instead of
          ;; paying the LSP server startup cost on the first keystroke.
          (ignore-errors
            (with-current-buffer buf (emjupy--ensure-shadow-buffer nb-struct)))
          (message "Opened notebook: %s%s" path
                   (if (emjupy-server-kernel-id server)
                       ""
                     ". Press C-c C-z to select or spawn a kernel."))
          buf)))))

(defun emjupy-create-notebook (&optional server)
  "Create a brand new blank notebook on SERVER and open it."
  (interactive)
  (let* ((server (or server (emjupy--server)))
         (raw-name (read-string "New notebook name (default Untitled.ipynb): "
                                nil 'emjupy--notebook-history))
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


;;; ---------------------------------------------------------------------
;;; Server dashboard
;;; ---------------------------------------------------------------------
;; One buffer per server showing what it has: the kernels running on it and
;; the notebooks stored on it, browsable into subdirectories. Built on
;; `tabulated-list-mode' rather than hand-drawn, so sorting, navigation and
;; column handling come from Emacs. It is emphatically NOT a file manager --
;; press `d\' to hand the directory to Dired, over TRAMP if the server is
;; remote.

(defcustom emjupy-remote-root nil
  "Where the notebook directory lives as a *file name*, for Dired.

The Contents API tells emjupy what notebooks a server has, but not how
to reach them as files -- and a tunnelled server looks like localhost
from here, so there is nothing to infer.  Set this to hand `d\' in the
dashboard somewhere useful:

  (setq emjupy-remote-root \"/ssh:user@host:/home/user/notebooks\")

An alist maps it per server:

  (setq emjupy-remote-root
        \='((\"localhost:8888\" . \"~/notebooks\")
          (\"localhost:9999\" . \"/ssh:box:/srv/nb\")))

nil disables `d\'."
  :type '(choice (const :tag "Disabled" nil)
                 (directory :tag "One directory for every server")
                 (alist :key-type string :value-type directory))
  :group 'emjupy)

(defvar-local emjupy-list--server nil
  "The `emjupy-server\' this dashboard describes.")
(defvar-local emjupy-list--path ""
  "Contents-API subdirectory this dashboard is showing.")

(defun emjupy--remote-root-for (server)
  "Return the Dired root configured for SERVER, or nil."
  (cond
   ((null emjupy-remote-root) nil)
   ((stringp emjupy-remote-root) emjupy-remote-root)
   ((consp emjupy-remote-root)
    (cdr (assoc (emjupy--server-label server) emjupy-remote-root)))))

(defun emjupy--list-entries (server path)
  "Return `tabulated-list-entries\' for PATH on SERVER."
  (let* ((contents (emjupy--http-request
                    "GET" server (concat "/api/contents/" path)))
         (items (and contents (gethash "content" contents)))
         (kernels (ignore-errors
                    (emjupy--http-request "GET" server "/api/kernels")))
         (rows nil))
    (cl-loop for k across (or kernels [])
             do (push (list (list :kind 'kernel :id (gethash "id" k))
                            (vector "kernel"
                                    (or (gethash "name" k) "?")
                                    (format "%s  (%s connection%s)"
                                            (substring (or (gethash "id" k) "") 0 8)
                                            (or (gethash "connections" k) 0)
                                            (if (eql (gethash "connections" k) 1) "" "s"))))
                      rows))
    (unless (string-empty-p path)
      (push (list (list :kind 'up)
                  (vector "dir" ".." ""))
            rows))
    (cl-loop for item across (or items [])
             for type = (gethash "type" item)
             for name = (gethash "name" item)
             for ipath = (gethash "path" item)
             do (push (list (list :kind (intern type) :path ipath)
                            (vector type name
                                    (or (gethash "last_modified" item) "")))
                      rows))
    (nreverse rows)))

(defun emjupy-list-refresh ()
  "Re-fetch this dashboard from the server."
  (interactive)
  (let ((server emjupy-list--server)
        (path emjupy-list--path))
    (setq tabulated-list-entries (emjupy--list-entries server path))
    (setq header-line-format
          (format "  %s   %s   %d kernel(s)   [RET] open  [^] up  [g] refresh  [k] kill kernel  [d] dired"
                  (emjupy--server-label server)
                  (if (string-empty-p path) "/" (concat "/" path))
                  (length (cl-remove-if-not
                           (lambda (e) (eq (plist-get (car e) :kind) 'kernel))
                           tabulated-list-entries))))
    (tabulated-list-print t)))

(defun emjupy-list-open ()
  "Open the notebook, or descend into the directory, at point."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (kind (plist-get row :kind)))
    (pcase kind
      ('up (setq emjupy-list--path
                 (let ((parent (file-name-directory
                                (directory-file-name emjupy-list--path))))
                   (if parent (directory-file-name parent) "")))
           (emjupy-list-refresh))
      ('directory (setq emjupy-list--path (plist-get row :path))
                  (emjupy-list-refresh))
      ('notebook (emjupy-open-notebook (plist-get row :path) emjupy-list--server))
      ('kernel (message "[emjupy] Kernel %s -- press k to shut it down."
                        (plist-get row :id)))
      (_ (message "[emjupy] Not a notebook; press d for Dired.")))))

(defun emjupy-list-kill-kernel ()
  "Shut down the kernel on this line."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (id (plist-get row :id)))
    (unless (eq (plist-get row :kind) 'kernel)
      (user-error "Not a kernel"))
    (when (yes-or-no-p (format "Shut down kernel %s? " id))
      (emjupy--http-request "DELETE" emjupy-list--server
                            (format "/api/kernels/%s" id))
      (emjupy-list-refresh))))

(defun emjupy-list-dired ()
  "Open this directory in Dired, over TRAMP when the server is remote.

emjupy deliberately does not implement a file manager: Dired already is
one, and TRAMP already knows how to reach another machine."
  (interactive)
  (let ((root (emjupy--remote-root-for emjupy-list--server)))
    (unless root
      (user-error "Set `emjupy-remote-root\' to browse this server\='s files in Dired"))
    (dired (expand-file-name emjupy-list--path (file-name-as-directory root)))))

(defun emjupy-list-new-notebook ()
  "Create a new notebook in the directory being shown, and open it."
  (interactive)
  (let* ((server emjupy-list--server)
         (dir emjupy-list--path)
         (raw (read-string "New notebook name: " nil 'emjupy--notebook-history))
         (name (if (string-match-p "\\.ipynb\\'" raw) raw (concat raw ".ipynb")))
         (path (if (string-empty-p dir)
                   name
                 (concat (directory-file-name dir) "/" name)))
         (nb (make-hash-table :test 'equal))
         (req (make-hash-table :test 'equal)))
    (puthash "cells" [] nb)
    (puthash "metadata" (make-hash-table :test 'equal) nb)
    (puthash "nbformat" 4 nb)
    (puthash "nbformat_minor" 5 nb)
    (puthash "type" "notebook" req)
    (puthash "format" "json" req)
    (puthash "content" nb req)
    (when (and (ignore-errors
                 (emjupy--http-request "GET" server (concat "/api/contents/" path)))
               (not (yes-or-no-p (format "%s already exists.  Overwrite it? " path))))
      (user-error "Not overwriting %s" path))
    (emjupy--http-request "PUT" server (concat "/api/contents/" path)
                          (json-serialize req))
    (emjupy-open-notebook path server)))

(defun emjupy-list-kill-all-kernels ()
  "Shut down every kernel on this server."
  (interactive)
  (let* ((server emjupy-list--server)
         (kernels (emjupy--http-request "GET" server "/api/kernels"))
         (ids (cl-loop for k across kernels collect (gethash "id" k))))
    (cond
     ((null ids) (message "[emjupy] No kernels running."))
     ((yes-or-no-p (format "Shut down all %d kernel(s)? " (length ids)))
      (dolist (id ids)
        (ignore-errors
          (emjupy--http-request "DELETE" server (format "/api/kernels/%s" id))))
      (emjupy-list-refresh)))))

(defvar emjupy-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emjupy-list-open)
    (define-key map (kbd "^")   #'emjupy-list-open)
    (define-key map (kbd "g")   #'emjupy-list-refresh)
    (define-key map (kbd "k")   #'emjupy-list-kill-kernel)
    (define-key map (kbd "K")   #'emjupy-list-kill-all-kernels)
    (define-key map (kbd "d")   #'emjupy-list-dired)
    (define-key map (kbd "n")   #'emjupy-list-new-notebook)
    map)
  "Keymap for `emjupy-list-mode\'.")

(define-derived-mode emjupy-list-mode tabulated-list-mode "emjupy-server"
  "Dashboard for one Jupyter server: its kernels and its notebooks."
  (setq tabulated-list-format [("Kind" 10 t) ("Name" 44 t) ("Info" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun emjupy-server-dashboard (&optional server)
  "Show what SERVER has: the kernels running on it and its notebooks.

Defaults to this buffer\='s server, or the last one logged into.  One
buffer per server, so several tunnels can be inspected side by side."
  (interactive)
  (let* ((server (or server (emjupy--server)))
         (buf (get-buffer-create
               (format "*emjupy server: %s*" (emjupy--server-label server)))))
    (with-current-buffer buf
      (emjupy-list-mode)
      (setq emjupy-list--server server)
      (unless emjupy-list--path (setq emjupy-list--path ""))
      (emjupy-list-refresh))
    (switch-to-buffer buf)
    buf))

(defalias 'emjupy-notebook-list #'emjupy-server-dashboard
  "Alias for `emjupy-server-dashboard\'.")

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

(defun emjupy-switch-notebook ()
  "Switch to another open emjupy notebook, labelled by server."
  (interactive)
  (let* ((buffers (emjupy--notebook-buffers))
         (names (mapcar #'buffer-name buffers)))
    (unless names (user-error "No emjupy notebooks are open"))
    (switch-to-buffer (completing-read "Notebook: " names nil t nil
                                       'emjupy--notebook-history))))

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

(provide 'emjupy-notebook)
;;; emjupy-notebook.el ends here
