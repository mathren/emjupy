;;; emjupy-dashboard.el --- The server dashboard: notebooks and kernels  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The listing of a server's notebooks, folders and kernels -- browsing,
;; opening, creating, shutting kernels down -- and opening its files.  One
;; buffer per server, browsable into subdirectories, built on
;; `tabulated-list-mode'.  Not a file manager: `d' hands the directory to
;; Dired, over TRAMP when it is remote.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tramp)
(require 'transient)
(require 'emjupy-core)
(require 'emjupy-ipynb)
(require 'emjupy-http)
(require 'emjupy-remote)
(require 'emjupy-mode)
(require 'emjupy-notebook)

(declare-function dired-goto-file "dired" (file))

;;;; The notebook listing

(defvar-local emjupy-list--server nil
  "The `emjupy-server' this dashboard describes.")
(defvar-local emjupy-list--path ""
  "Contents-API subdirectory this dashboard is showing.")

(defface emjupy-list-notebook
  ;; Inherit only: MELPA asks packages not to inherit a face and override its
  ;; attributes.  Customise `emjupy-list-notebook' for bolder notebooks.
  '((t :inherit font-lock-function-name-face))
  "Face for notebook rows in the server dashboard."
  :group 'emjupy)

(defface emjupy-list-directory
  '((t :inherit font-lock-keyword-face))
  "Face for directory rows in the server dashboard."
  :group 'emjupy)

(defface emjupy-list-kernel
  '((t :inherit font-lock-warning-face))
  "Face for kernel rows in the server dashboard.
A kernel is a running process to keep an eye on, not something to open."
  :group 'emjupy)

(defface emjupy-list-file
  '((t :inherit shadow))
  "Face for non-notebook file rows in the server dashboard."
  :group 'emjupy)

(defun emjupy--list-face (kind)
  "Return the face for a dashboard row of KIND."
  (pcase kind
    ('notebook 'emjupy-list-notebook)
    ((or 'directory 'up) 'emjupy-list-directory)
    ('kernel 'emjupy-list-kernel)
    (_ 'emjupy-list-file)))

(defun emjupy--list-row (kind label name info)
  "Build a dashboard row of KIND showing LABEL, NAME and INFO.
Propertized by kind so the row is identifiable at a glance."
  (let ((face (emjupy--list-face kind)))
    (vector (propertize label 'face face)
            (propertize name 'face face)
            (propertize info 'face (if (eq kind 'kernel)
                                       'emjupy-list-kernel
                                     'shadow)))))

(defun emjupy--list-file-row (item)
  "Return the listing's row for ITEM, a file in a Contents API listing."
  (let ((type (gethash "type" item))
        (modified (or (gethash "last_modified" item) "")))
    (list (list :kind (intern type) :path (gethash "path" item)
                :modified modified)
          (emjupy--list-row (intern type) type (gethash "name" item)
                            modified))))

(defun emjupy--list-kernel-row (kernel)
  "Return the listing's row for KERNEL, from the server's list of kernels."
  (let ((connections (gethash "connections" kernel)))
    (list (list :kind 'kernel :id (gethash "id" kernel))
          (emjupy--list-row
           'kernel "kernel" (or (gethash "name" kernel) "?")
           (format "%s  (%s connection%s)"
                   (substring (or (gethash "id" kernel) "") 0 8)
                   (or connections 0)
                   (if (eql connections 1) "" "s"))))))

(defun emjupy--list-by-name (rows)
  "Return ROWS sorted by name."
  (sort rows (lambda (a b) (string-lessp (aref (cadr a) 1) (aref (cadr b) 1)))))

(defun emjupy--list-by-recent (rows)
  "Return ROWS, the most recently modified first, ties by name.
The server's ISO-8601 UTC times sort as text; a row without one goes
last."
  (sort rows
        (lambda (a b)
          (let ((ta (or (plist-get (car a) :modified) ""))
                (tb (or (plist-get (car b) :modified) "")))
            (if (equal ta tb)
                (string-lessp (aref (cadr a) 1) (aref (cadr b) 1))
              (string-greaterp ta tb))))))

(defun emjupy--list-entries (server path)
  "Return `tabulated-list-entries' for PATH on SERVER.
Directories, notebooks, other files, then the server's kernels; within
each, most recently modified first.  Kernels, without a time, by name."
  (let* ((contents (emjupy--http-request
                    "GET" server (emjupy--contents-path path)))
         (items (and contents (gethash "content" contents)))
         (kernels (condition-case nil
                      (emjupy--http-request "GET" server "/api/kernels")
                    (emjupy-http-error nil)))
         (dirs nil) (notebooks nil) (files nil))
    (cl-loop for item across (or items [])
             for row = (emjupy--list-file-row item)
             do (pcase (gethash "type" item)
                  ("directory" (push row dirs))
                  ("notebook" (push row notebooks))
                  (_ (push row files))))
    (append
     (unless (string-empty-p path)
       (list (list (list :kind 'up) (emjupy--list-row 'up "dir" ".." ""))))
     (emjupy--list-by-recent dirs)
     (emjupy--list-by-recent notebooks)
     (emjupy--list-by-recent files)
     ;; Reversed, as they were gathered: kernels often share a name, and
     ;; the sort keeps ties in the order given.
     (emjupy--list-by-name
      (nreverse (mapcar #'emjupy--list-kernel-row (or kernels [])))))))

;;;; Moving and acting in the listing

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

(defun emjupy-list-open-at-click (event)
  "Open the row that was clicked.
EVENT is the mouse event."
  (interactive "e")
  (let ((posn (event-end event)))
    (with-current-buffer (window-buffer (posn-window posn))
      (goto-char (posn-point posn))
      (emjupy-list-open))))

(defun emjupy-list-up ()
  "Show the parent of the directory being listed, wherever point is.

Bound to \\`^', as in Dired.  It was bound to `emjupy-list-open', which
only goes up from the \"..\" row: anywhere else it opened the notebook at
point, or descended into the directory there."
  (interactive)
  (unless (string-empty-p emjupy-list--path)
    (setq emjupy-list--path
          (let ((parent (file-name-directory
                         (directory-file-name emjupy-list--path))))
            (if parent (directory-file-name parent) "")))
    (emjupy-list-refresh)))

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
      ('notebook (emjupy-open-notebook (plist-get row :path)
                                       emjupy-list--server))
      ('file (emjupy-list-open-file (plist-get row :path)))
      ('kernel (message "[emjupy] Kernel %s -- press k to shut it down."
                        (plist-get row :id)))
      (_ (message "[emjupy] Nothing to open here.")))))

(defun emjupy-list-kill-kernel ()
  "Shut down the kernel on this line."
  (interactive)
  (let* ((row (tabulated-list-get-id))
         (id (plist-get row :id)))
    (unless (eq (plist-get row :kind) 'kernel)
      (user-error "Not a kernel"))
    (when (y-or-n-p (format "Shut down kernel %s? " id))
      (emjupy--http-request "DELETE" emjupy-list--server
                            (format "/api/kernels/%s" id))
      (emjupy-list-refresh))))

;;;; Opening files from the listing

(defun emjupy-list-open-file (path)
  "Open PATH, a file on this server, the best way available.
PATH is relative to what the server serves.  Tried in order:
- through `emjupy-remote-root', when set: editable;
- over TRAMP, when the server's address names a real host: editable;
- as a local file, when the server is on this machine: editable;
- otherwise through the Contents API, a read-only copy over HTTP."
  (let* ((server emjupy-list--server)
         (configured (emjupy--configured-root-for server))
         (tramp-root (and (not configured) (emjupy--tramp-root-for server)))
         (server-side (emjupy--server-side-root-for server)))
    (cond
     ;; A root set by hand is addressable by definition: that is why it was
     ;; set.  It may be TRAMP or it may be local; `find-file' knows which.
     (configured
      (find-file (expand-file-name path (file-name-as-directory configured))))
     ;; The address names a real machine, so a TRAMP path can be built from
     ;; it and the path the kernel reported.  Not so for a tunnel, which
     ;; answers at localhost and never mentions the host it leads to.
     (tramp-root
      (find-file (expand-file-name path (file-name-as-directory tramp-root))))
     ;; The server is on this machine, so its own path is ours too.
     ((and server-side
           (file-exists-p (expand-file-name
                           path
                           (file-name-as-directory server-side))))
      (find-file (expand-file-name path (file-name-as-directory server-side))))
     ;; Neither -- fetch it.  Read-only, and said so in the header line.
     (server-side
      (emjupy-open-server-file
       (expand-file-name path (file-name-as-directory server-side)) server))
     (t
      (user-error "%s %s"
                  "Cannot tell where this server's files are."
                  "Open a notebook so a kernel can say, or set `emjupy-remote-root'")))))

(defun emjupy-list-dired ()
  "Open this directory in Dired, over TRAMP when the server is remote.

emjupy deliberately does not implement a file manager: Dired already is
one, and TRAMP already knows how to reach another machine."
  (interactive)
  (let ((root (emjupy--remote-root-for emjupy-list--server)))
    (unless root
      (user-error "%s %s"
                  "Cannot tell where this server's files are."
                  "Open a notebook so a kernel can say, or set `emjupy-remote-root'"))
    ;; The row under the cursor, not the directory being listed: pressing
    ;; this on a line means that line.  With no row -- an empty listing, or
    ;; point above the first entry -- fall back to what is being shown.
    (let* ((row (tabulated-list-get-id))
           (kind (plist-get row :kind))
           (path (or (and (memq kind '(notebook file directory))
                          (plist-get row :path))
                     emjupy-list--path))
           (target (expand-file-name path (file-name-as-directory root))))
      (if (memq kind '(notebook file))
          ;; A file: open the directory holding it and put point on it, as
          ;; `dired-jump' would locally.
          (progn (dired (file-name-directory target))
                 (dired-goto-file target))
        (dired target)))))

;;;; Creating a notebook

(defun emjupy--blank-code-cell-json ()
  "Return the nbformat representation of one empty code cell."
  (let ((cell (make-hash-table :test 'equal)))
    (puthash "cell_type" "code" cell)
    (puthash "source" "" cell)
    (puthash "outputs" [] cell)
    (puthash "execution_count" :null cell)
    (puthash "metadata" (make-hash-table :test 'equal) cell)
    (puthash "id" (format "%08x" (random (expt 16 8))) cell)
    cell))

(defun emjupy-create-new-notebook ()
  "Create a new notebook in the directory being shown, and open it.

The notebook starts with one empty code cell.  A notebook with no cells
at all is not a thing anyone wants: it offers nowhere to type, and the
first act would always be to add one."
  (interactive)
  (let* ((server emjupy-list--server)
         (dir emjupy-list--path)
         (raw (read-string "New notebook name: " nil 'emjupy--notebook-history))
         (name (if (string-match-p "\\.ipynb\\'" raw) raw (concat raw
                                                                  ".ipynb")))
         (path (if (string-empty-p dir)
                   name
                 (concat (directory-file-name dir) "/" name)))
         (req (emjupy--notebook-request
               (emjupy--new-notebook-json
                (vector (emjupy--blank-code-cell-json))))))
    (when (and (emjupy--http-exists-p server path)
               (not (y-or-n-p
                     (format "%s already exists.  Overwrite it? " path))))
      (user-error "Not overwriting %s" path))
    (emjupy--http-request "PUT" server (emjupy--contents-path path)
                          (json-serialize req))
    (emjupy-open-notebook path server)
    ;; Into the cell, not merely into the buffer.  A new notebook is made to
    ;; be typed in, and landing at `point-min' leaves the caret against the
    ;; outline with nothing to type into.
    (let ((buf (emjupy--notebook-buffer-for path server)))
      (when (buffer-live-p buf)
        (with-current-buffer buf
          (let ((first (car (append
                             (emjupy-notebook-cells emjupy--buffer-notebook)
                             nil))))
            (when (and first (overlayp (emjupy-cell-overlay first)))
              (goto-char (overlay-start (emjupy-cell-overlay first)))
              (when (get-buffer-window buf)
                (set-window-point (get-buffer-window buf) (point))))))))))

;;;; Shutting every kernel down

(defun emjupy--notebook-buffer-for (path server)
  "Return the buffer showing PATH on SERVER, or nil."
  (get-buffer (emjupy--notebook-buffer-name path server)))

(defun emjupy-list-kill-all-kernels ()
  "Shut down every kernel on this server."
  (interactive)
  (let* ((server emjupy-list--server)
         (kernels (emjupy--http-request "GET" server "/api/kernels"))
         (ids (cl-loop for k across kernels collect (gethash "id" k))))
    (cond
     ((null ids) (message "[emjupy] No kernels running."))
     ((y-or-n-p (format "Shut down all %d kernel(s)? " (length ids)))
      (dolist (id ids)
        (condition-case err
            (emjupy--http-request "DELETE" server (format "/api/kernels/%s" id))
          (emjupy-http-error
           (message "[emjupy] Kernel %s: %s" id (error-message-string err)))))
      (emjupy-list-refresh)))))

;;;; The dashboard, its keys and mode

(defvar emjupy-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emjupy-list-open)
    ;; Clicking a row opens it, as it would in any listing.  The click moves
    ;; point to the row first: `emjupy-list-open' reads the row at point,
    ;; and a click elsewhere should act on what was clicked.
    (define-key map [mouse-1] #'emjupy-list-open-at-click)
    (define-key map [double-mouse-1] #'emjupy-list-open-at-click)
    (define-key map (kbd "^")   #'emjupy-list-up)
    (define-key map (kbd "g")   #'emjupy-list-refresh)
    (define-key map (kbd "k")   #'emjupy-list-kill-kernel)
    (define-key map (kbd "K")   #'emjupy-list-kill-all-kernels)
    (define-key map (kbd "d")   #'emjupy-list-dired)
    (define-key map (kbd "n")   #'emjupy-create-new-notebook)
    map)
  "Keymap for `emjupy-list-mode'.")

(define-derived-mode emjupy-list-mode tabulated-list-mode "emjupy-server"
  "Dashboard for one Jupyter server: its kernels and its notebooks."
  (setq tabulated-list-format [("Kind" 10 t) ("Name" 44 t) ("Info" 40 t)])
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

;;;###autoload
(defun emjupy-server-dashboard (&optional server)
  "Show what SERVER has: the kernels running on it and its notebooks.

Defaults to this buffer's server, or the last one logged into.  One
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

;;;###autoload
(defalias 'emjupy-notebook-list #'emjupy-server-dashboard
  "Alias for `emjupy-server-dashboard'.")

(provide 'emjupy-dashboard)

;;; emjupy-dashboard.el ends here
