;;; emjupy-notebook.el --- Notebook session management and .ipynb I/O for emjupy  -*- lexical-binding: t; -*-

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
(declare-function dired-goto-file "dired" (file))
(defvar python-indent-guess-indent-offset-verbose)
(require 'emjupy-http)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-eglot)
(require 'emjupy-remote)
(require 'emjupy-mode)

;; Defined in emjupy.el, which requires this file: the mode function is
;; only ever called at runtime, so the cycle is harmless.

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
  "Return URL as a host:port base-url.  A bare port means localhost."
  (if (string-match-p "\\`[0-9]+\\'" url)
      (concat "localhost:" url)
    url))

(defun emjupy--server-reachable-p (server)
  "Return non-nil if SERVER accepts SERVER's credentials.

Probes /api/contents, not /api/status.  Status is a health endpoint and
several jupyter_server versions answer it without authentication at all,
so a tokenless probe came back 200 and emjupy concluded no token was
needed -- then every write was refused with 403 and the token was never
asked for.  Contents is both genuinely protected and the thing emjupy
actually needs."
  (let ((inhibit-message t)
        (message-log-max nil))
    ;; A failed request -- refused, or no server -- is the answer "no";
    ;; anything else is a bug, and is raised rather than taken for it.
    (condition-case nil
        (and (emjupy--http-request "GET" server "/api/contents") t)
      (emjupy-http-error nil))))

(defun emjupy--resolve-token (base-url explicit)
  "Work out the token for BASE-URL, prompting only when unavoidable.

Order: EXPLICIT, then the token already registered for this server,
then no token at all (many tunnelled servers are started with
`--IdentityProvider.token='), and only then ask.  Asking every time is
what made logging into a second tunnel tedious."
  (or explicit
      ;; A token already registered for this server is used as it stands.
      ;; It was accepted once; re-probing only to confirm costs a request
      ;; whose failure is then reported as an error the user did not cause.
      (let ((known (gethash base-url emjupy--servers)))
        (and known
             (let ((tok (emjupy-server-token known)))
               (and tok (not (string-empty-p tok)) tok))))
      ;; Only now is a probe worth making: is this one of the tunnelled
      ;; servers started with `--IdentityProvider.token='?  Quietly -- a 403
      ;; here is the expected answer for a server that does want a token,
      ;; not a failure, and reporting it made every login look like it had
      ;; gone wrong once before succeeding.
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

One port = one kernel.  The usual case is a single kernel already
running behind the tunnel, which is adopted silently -- no prompt, and
no second kernel process spawned next to the one you started.  Only a
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
(defcustom emjupy-default-port "8888"
  "Port assumed when none is given.

Jupyter's own default, and the one a tunnel is usually pointed at."
  :type 'string
  :group 'emjupy)

(defun emjupy--read-server-url ()
  "Ask for a port or URL, returning `emjupy-default-port' if none is given.

The prompt shows the default, and answering nothing accepts it -- there
is little point making someone type the number Jupyter would have used
anyway."
  (let ((answer (read-string
                 (format "Jupyter port or URL (default %s): " emjupy-default-port)
                 nil 'emjupy--port-history)))
    (if (string-empty-p (string-trim answer))
        emjupy-default-port
      answer)))

;;;###autoload
(defun emjupy-login (url &optional token)
  "Connect to the Jupyter server at URL and open one of its notebooks.
TOKEN, when given, is used instead of prompting.

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
   (list (emjupy--read-server-url)
         (when current-prefix-arg (emjupy--read-token "Token: "))))
  (let* ((base-url (emjupy--normalize-url url))
         (token (emjupy--resolve-token base-url token))
         (server (emjupy--intern-server base-url token)))
    (setq emjupy--current-server server)

    ;; Prime the XSRF cookie before anything tries to write. /api does not
    ;; hand one out; only the HTML pages do.
    (emjupy--harvest-xsrf server)

    ;; If the credentials turn out to be wrong, ask once and carry on rather
    ;; than failing the command.  A 403 here means the token guessed above
    ;; was not accepted -- which is what made logging in take two attempts:
    ;; the first reported the 403 and stopped, and only the second, having
    ;; asked for a token, worked.
    (let ((kernel-id
           (condition-case err
               (emjupy--bind-server-kernel server)
             ;; Only a refusal is asked about; anything else goes on up.
             (emjupy-http-status
              (if (not (eql (emjupy--http-status-of err) 403))
                  (signal (car err) (cdr err))
                (setf (emjupy-server-token server)
                      (emjupy--read-token
                       (format "Token for %s (the server refused the last one): "
                               base-url)))
                (emjupy--harvest-xsrf server)
                (emjupy--bind-server-kernel server))))))
      (message "Connected to %s, kernel %s." base-url kernel-id))
    (emjupy-list-notebooks server)))

(defun emjupy-list-notebooks (&optional server)
  "Fetch SERVER's root contents and prompt to open or create a notebook.
Returns the notebook buffer.  SERVER defaults to this buffer's server,
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

;;;###autoload
(defun emjupy-open-notebook (path &optional server)
  "Fetch PATH from SERVER, parse it, and render it in `emjupy-mode'.
Returns the notebook buffer."
  (let* ((server (or server (emjupy--server)))
         (open (get-buffer (emjupy--notebook-buffer-name path server))))
    ;; Already open with unsaved changes: show it as it is.  Fetching it
    ;; again replaced its cells with the server's, and the changes were lost.
    (if (and open (buffer-live-p open)
             (with-current-buffer open (buffer-modified-p)))
        (progn (switch-to-buffer open)
               (message "[emjupy] %s is open with unsaved changes; shown as it is." path)
               open)
      (message "Fetching notebook: %s..." path)
      (let* ((response-data (emjupy--http-request "GET" server (emjupy--contents-path path)))
             (content-hash (and response-data (gethash "content" response-data))))
	(if (not content-hash)
            (error "Failed to fetch notebook content from server")
          (let* ((ipynb-json (json-serialize content-hash))
		 (nb-struct (emjupy--parse-ipynb ipynb-json))
		 (buf-name (emjupy--notebook-buffer-name path server))
		 ;; Whether this notebook is already on screen decides where
		 ;; point ends up below.
		 (already-open (get-buffer buf-name))
		 (buf (get-buffer-create buf-name)))

            (setf (emjupy-notebook-server nb-struct) server)
            (setf (emjupy-notebook-path nb-struct) path)
            (setf (emjupy-notebook-buffer nb-struct) buf)
            (setf (emjupy-notebook-last-modified nb-struct)
                  (gethash "last_modified" response-data))

            (with-current-buffer buf
              ;; `emjupy-mode' lives in emjupy.el, which this file cannot
              ;; require at load time without a cycle -- emjupy.el requires
              ;; this one.  Autoloading `emjupy-open-notebook' therefore
              ;; pulled in every file EXCEPT the one defining the mode, and
              ;; opening a notebook failed with the mode undefined.  Required
              ;; here, where there is no cycle to create.
              (require 'emjupy)
              (emjupy-mode)
              ;; Set the notebook BEFORE drawing, and draw through
              ;; `emjupy--rerender-notebook' rather than looping over the cells
              ;; here.  Re-opening a notebook reuses its buffer but parses
              ;; fresh cell structs, so the previous structs' overlays become
              ;; unreachable -- and this path used to erase the text without
              ;; deleting them, leaving each one collapsed at position 1 still
              ;; drawing its rule.  That is the stack of stray box borders at
              ;; the top of a re-opened notebook.
              (setq emjupy--buffer-notebook nb-struct)
              (emjupy--rerender-notebook)
              ;; A freshly opened notebook starts at the top, as a file does.
              ;; One already open keeps where you were: re-opening it to get
              ;; back to what you were reading should not lose your place.
              (unless already-open
		(goto-char (point-min)))
              ;; As drawn, it is the server copy: nothing unsaved yet.
              (set-buffer-modified-p nil)
              (emjupy--save-machinery-start)
              (add-hook 'kill-buffer-query-functions #'emjupy--kill-buffer-query nil t)
              (emjupy--offer-recovery nb-struct))

            (switch-to-buffer buf)
            (unless already-open
              (goto-char (point-min)))
            ;; Attach this notebook to the kernel bound to its port, so opening
            ;; it drops you into the REPL already running behind that tunnel
            ;; rather than into a dead buffer needing a separate connect step.
            (when-let* ((kernel-id (emjupy-server-kernel-id server)))
              (with-current-buffer buf
		(condition-case err
                    (emjupy-connect-kernel nb-struct kernel-id)
                  ;; Everything: connecting opens a WebSocket, and
                  ;; `websocket-open' reports a failure as a plain error.  The
                  ;; notebook opens either way; the kernel is connected later.
                  (error (message "[emjupy] Could not attach kernel %s: %s"
                                  kernel-id (error-message-string err))))))
            ;; Warm up the code shadow-buffer + Eglot now, in the background,
            ;; so completions are ready once the user starts typing instead of
            ;; paying the LSP server startup cost on the first keystroke.
            (with-demoted-errors "[emjupy] Language support did not start: %S"
              (with-current-buffer buf (emjupy--ensure-shadow-buffer nb-struct)))
            (message "Opened notebook: %s%s" path
                     (if (emjupy-server-kernel-id server)
			 ""
                       (format ". Press %s to select or spawn a kernel."
                               (substitute-command-keys "\\[emjupy-connect-kernel-interactive]"))))
            buf))))))

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
                                          (emjupy--contents-path filename)
                                          (json-serialize req-body))))
      (if response
          (emjupy-open-notebook filename server)
        (error "Failed to write %s to server" filename)))))


;; ---------------------------------------------------------------------
;;; Server dashboard:

;;; ---------------------------------------------------------------------
;; One buffer per server showing what it has: the kernels running on it and
;; the notebooks stored on it, browsable into subdirectories. Built on
;; `tabulated-list-mode' rather than hand-drawn, so sorting, navigation and
;; column handling come from Emacs. It is emphatically NOT a file manager --
;; press `d\' to hand the directory to Dired, over TRAMP if the server is
;; remote.

(defvar-local emjupy-list--server nil
  "The `emjupy-server\' this dashboard describes.")
(defvar-local emjupy-list--path ""
  "Contents-API subdirectory this dashboard is showing.")

(defface emjupy-list-notebook
  ;; Inherit only.  MELPA's guidelines ask packages not to inherit a face
  ;; AND override its attributes -- adding :weight bold here can look wrong
  ;; against a user's customisation of the inherited face.  Customise
  ;; `emjupy-list-notebook' if you want notebooks bolder.
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

(defun emjupy--list-entries (server path)
  "Return `tabulated-list-entries\' for PATH on SERVER.

Grouped rather than interleaved: directories, then notebooks, then
other files, then the kernels running on the server.  Notebooks sit
above kernels because opening one is what the dashboard is for; the
kernels are context.

Within each group, most recently modified first -- the notebook worked
on this morning is the one wanted, and alphabetical order buries it
among however many others share its prefix.  Kernels have no
modification time and stay sorted by name."
  (let* ((contents (emjupy--http-request
                    "GET" server (emjupy--contents-path path)))
         (items (and contents (gethash "content" contents)))
         (kernels (condition-case nil
                      (emjupy--http-request "GET" server "/api/kernels")
                    (emjupy-http-error nil)))
         (dirs nil) (notebooks nil) (files nil) (kernel-rows nil))
    (cl-loop for item across (or items [])
             for type = (gethash "type" item)
             for name = (gethash "name" item)
             for ipath = (gethash "path" item)
             for modified = (or (gethash "last_modified" item) "")
             for row = (list (list :kind (intern type) :path ipath
                                   :modified modified)
                             (emjupy--list-row (intern type) type name modified))
             do (pcase type
                  ("directory" (push row dirs))
                  ("notebook" (push row notebooks))
                  (_ (push row files))))
    (cl-loop for k across (or kernels [])
             do (push (list (list :kind 'kernel :id (gethash "id" k))
                            (emjupy--list-row
                             'kernel "kernel" (or (gethash "name" k) "?")
                             (format "%s  (%s connection%s)"
                                     (substring (or (gethash "id" k) "") 0 8)
                                     (or (gethash "connections" k) 0)
                                     (if (eql (gethash "connections" k) 1) "" "s"))))
                      kernel-rows))
    (cl-flet ((by-name (rows)
                (sort rows (lambda (a b)
                             (string-lessp (aref (cadr a) 1) (aref (cadr b) 1)))))
              (by-recent (rows)
                ;; Most recently touched first.  The server reports the time
                ;; as ISO-8601 in UTC, which sorts correctly as text, so
                ;; nothing needs parsing -- and an entry with no time falls
                ;; to the bottom rather than to an arbitrary place.  Ties
                ;; break by name, so a directory of files written in the
                ;; same second still reads in a settled order.
                (sort rows
                      (lambda (a b)
                        (let ((ta (or (plist-get (car a) :modified) ""))
                              (tb (or (plist-get (car b) :modified) "")))
                          (if (equal ta tb)
                              (string-lessp (aref (cadr a) 1) (aref (cadr b) 1))
                            (string-greaterp ta tb)))))))
      (append
       (unless (string-empty-p path)
         (list (list (list :kind 'up) (emjupy--list-row 'up "dir" ".." ""))))
       (by-recent dirs)
       (by-recent notebooks)
       (by-recent files)
       (by-name kernel-rows)))))

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

Bound to \\`^\', as in Dired.  It was bound to `emjupy-list-open\=', which
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
      ('notebook (emjupy-open-notebook (plist-get row :path) emjupy-list--server))
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
    (when (yes-or-no-p (format "Shut down kernel %s? " id))
      (emjupy--http-request "DELETE" emjupy-list--server
                            (format "/api/kernels/%s" id))
      (emjupy-list-refresh))))

(defun emjupy-list-open-file (path)
  "Open PATH, a file on this server, in the best way available.

PATH is relative to what the server serves.  There are four ways to
reach it, tried in this order, because they differ in whether the result
can be edited:
- through `emjupy-remote-root\', when it is set.  That is a path this
  Emacs can address -- a TRAMP location, or a local directory -- so the
  file opens normally and can be written back.
- over TRAMP, when the server\'s address names a real host.  Editable.
- as a local file, when the server turns out to be on this machine and
  the path exists.  Also editable.
- otherwise through the Contents API, read-only.  Nothing needs
  configuring for this and it always works, but it is a copy fetched
  over HTTP and writing it back is a different job from reading it."
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
           (file-exists-p (expand-file-name path
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
         (name (if (string-match-p "\\.ipynb\\'" raw) raw (concat raw ".ipynb")))
         (path (if (string-empty-p dir)
                   name
                 (concat (directory-file-name dir) "/" name)))
         (nb (make-hash-table :test 'equal))
         (req (make-hash-table :test 'equal)))
    (puthash "cells" (vector (emjupy--blank-code-cell-json)) nb)
    (puthash "metadata" (make-hash-table :test 'equal) nb)
    (puthash "nbformat" 4 nb)
    (puthash "nbformat_minor" 5 nb)
    (puthash "type" "notebook" req)
    (puthash "format" "json" req)
    (puthash "content" nb req)
    (when (and (emjupy--http-exists-p server path)
               (not (yes-or-no-p (format "%s already exists.  Overwrite it? " path))))
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
          (let ((first (car (append (emjupy-notebook-cells emjupy--buffer-notebook) nil))))
            (when (and first (overlayp (emjupy-cell-overlay first)))
              (goto-char (overlay-start (emjupy-cell-overlay first)))
              (when (get-buffer-window buf)
                (set-window-point (get-buffer-window buf) (point))))))))))

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
     ((yes-or-no-p (format "Shut down all %d kernel(s)? " (length ids)))
      (dolist (id ids)
        (condition-case err
            (emjupy--http-request "DELETE" server (format "/api/kernels/%s" id))
          (emjupy-http-error
           (message "[emjupy] Kernel %s: %s" id (error-message-string err)))))
      (emjupy-list-refresh)))))

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

;;;###autoload
(defalias 'emjupy-notebook-list #'emjupy-server-dashboard
  "Alias for `emjupy-server-dashboard\'.")

(defvar emjupy--export-history nil
  "Minibuffer history for export destinations.")

(defcustom emjupy-export-markdown-cells t
  "When non-nil, carry markdown cells into an exported .py as comments.

They are written as a `# %% [markdown]' block with each line commented,
which is the same percent format Jupytext, VS Code and Spyder read -- so
the prose survives the round trip instead of being dropped."
  :type 'boolean
  :group 'emjupy)

(defun emjupy--percent-export (nb)
  "Return NB's cells as a plain Python file in percent format.

Not the shadow buffer verbatim, though that is where the idea comes
from.  The shadow buffer's `# %% [emjupy:12]' markers carry emjupy's
internal cell ids, which mean nothing outside emjupy, and it holds code
cells only -- exporting it would silently drop every markdown cell.
Plain `# %%' is the convention Jupytext, VS Code and Spyder already
read."
  (let ((chunks nil))
    (cl-loop for cell across (or (emjupy-notebook-cells nb) [])
             do (let ((source (or (emjupy-cell-source cell) "")))
                  (pcase (emjupy-cell-type cell)
                    ('code (push (concat "# %%\n" source) chunks))
                    ('markdown
                     (when emjupy-export-markdown-cells
                       (push (concat "# %% [markdown]\n"
                                     (mapconcat (lambda (line)
                                                  (if (string-empty-p line)
                                                      "#"
                                                    (concat "# " line)))
                                                (split-string source "\n")
                                                "\n"))
                             chunks)))
                    (_ nil))))
    (let ((body (mapconcat #'identity (nreverse chunks) "\n\n")))
      (if (string-suffix-p "\n" body) body (concat body "\n")))))

(defun emjupy--export-default-name (nb)
  "Return the default .py path for NB, beside the notebook itself."
  (let ((path (or (emjupy-notebook-path nb) "notebook.ipynb")))
    (concat (file-name-sans-extension path) ".py")))

;;;###autoload
(defun emjupy-export-py (&optional local)
  "Export this notebook as a plain Python file in percent format.

The default destination is beside the notebook itself, on the server it
came from -- which is what makes this work for a remote kernel with no
TRAMP and no extra configuration: the file is written through the same
Contents API the notebook is read through, so it lands next to the
.ipynb on that machine.

With a prefix argument, LOCAL, write to a local file instead.  A TRAMP
file name works there, if you would rather push it somewhere else."
  (interactive "P")
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (content (emjupy--percent-export (progn (emjupy--sync-all-cells) nb))))
    (if local
        (let ((file (read-file-name "Export to file: " nil nil nil
                                    (file-name-nondirectory
                                     (emjupy--export-default-name nb)))))
          (when (or (not (file-exists-p file))
                    (yes-or-no-p (format "%s exists.  Overwrite it? " file)))
            (let ((coding-system-for-write 'utf-8-unix))
              (write-region content nil file))
            (message "[emjupy] Exported to %s" file)
            file))
      (let* ((default (emjupy--export-default-name nb))
             (path (read-string (format "Export to (on %s): "
                                        (emjupy--server-label server))
                                default 'emjupy--export-history))
             (body (make-hash-table :test 'equal)))
        (when (string-empty-p (string-trim path))
          (user-error "No destination given"))
        ;; A 404 is "not there"; any other failure is not an answer, and
        ;; is raised -- taking it for "not there" overwrote without asking.
        (when (and (emjupy--http-exists-p server path)
                   (not (yes-or-no-p (format "%s exists on %s.  Overwrite it? "
                                             path (emjupy--server-label server)))))
          (user-error "Not overwriting %s" path))
        (puthash "type" "file" body)
        (puthash "format" "text" body)
        (puthash "content" content body)
        (unless (emjupy--http-request "PUT" server (emjupy--contents-path path)
                                      (json-serialize body))
          (error "Server refused to write %s" path))
        (message "[emjupy] Exported to %s on %s"
                 path (emjupy--server-label server))
        path))))

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
                       :metadata (gethash "metadata" c-data)
                       :attachments (gethash "attachments" c-data))))
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
               ;; nbformat allows attachments on markdown and raw cells only
               (when (and (emjupy-cell-attachments cell)
                          (memq (emjupy-cell-type cell) '(markdown raw)))
                 (puthash "attachments" (emjupy-cell-attachments cell) c-hash))
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

;;;; Saving, and never losing what is not saved

(defvar-local emjupy--autosave-paused nil
  "Non-nil when the server copy changed elsewhere, and is not to be saved over.")

(defvar-local emjupy--autosave-last nil
  "When this notebook was last saved, or a save last tried, as a float time.")

(defvar-local emjupy--autosave-in-flight nil
  "Non-nil while an automatic save of this notebook is on its way.")

(define-error 'emjupy-save-conflict
              "The notebook changed on the server since it was opened")

(defun emjupy--server-last-modified (nb)
  "Return when NB's copy on its server last changed, or nil if there is none."
  (condition-case err
      (let ((model (emjupy--http-request
                    "GET" (emjupy-notebook-server nb)
                    (concat (emjupy--contents-path (emjupy-notebook-path nb)) "?content=0"))))
        (and (hash-table-p model) (gethash "last_modified" model)))
    ;; Gone from the server: there is nothing a save would overwrite.
    (emjupy-http-status
     (if (eql (emjupy--http-status-of err) 404) nil (signal (car err) (cdr err))))))

(defun emjupy--autosave-async (nb)
  "Save NB to its server without waiting, as auto-save does.
The same as saving by hand -- what is on the server checked first, and
not saved over if it changed elsewhere -- but nothing waits for it:
over a remote link both requests cost a round trip, and the upload as
long as the notebook, and auto-save made Emacs freeze for it every two
minutes.  The notebook is taken as it is now; edits made while it is on
its way leave it unsaved, for the next save to take."
  (let* ((buf (emjupy-notebook-buffer nb))
         (server (emjupy-notebook-server nb))
         (path (emjupy-notebook-path nb))
         (contents-path (emjupy--contents-path path))
         (known (emjupy-notebook-last-modified nb))
         tick body)
    (with-current-buffer buf
      (emjupy--sync-all-cells)
      (setq tick (buffer-modified-tick))
      (let ((request (make-hash-table :test 'equal)))
        (puthash "type" "notebook" request)
        (puthash "format" "json" request)
        (puthash "content" (json-parse-string (emjupy--serialize-notebook nb)
                                              :object-type 'hash-table :array-type 'array)
                 request)
        (setq body (json-serialize request)))
      (setq emjupy--autosave-in-flight t
            emjupy--autosave-last (float-time)))
    (cl-labels
        ((finish ()
           (when (buffer-live-p buf)
             (with-current-buffer buf (setq emjupy--autosave-in-flight nil))))
         (fail (err)
           (finish)
           (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
                    path (error-message-string err)))
         (upload ()
           (emjupy--http-request-async
            "PUT" server contents-path body
            (lambda (model)
              (finish)
              (setf (emjupy-notebook-last-modified nb)
                    (and (hash-table-p model) (gethash "last_modified" model)))
              (when (buffer-live-p buf)
                (with-current-buffer buf
                  ;; saved as it was when sent; edited since, it is not
                  (when (eql tick (buffer-modified-tick))
                    (set-buffer-modified-p nil)
                    (emjupy--recovery-delete nb)))))
            #'fail)))
      (if (not known)
          (upload)
        (emjupy--http-request-async
         "GET" server (concat contents-path "?content=0") nil
         (lambda (model)
           (let ((current (and (hash-table-p model) (gethash "last_modified" model))))
             (if (or (not current) (equal current known))
                 (upload)
               (finish)
               (when (buffer-live-p buf)
                 (with-current-buffer buf (setq emjupy--autosave-paused t)))
               (message "[emjupy] %s changed on the server since it was opened; not saved over.  %s"
                        path (substitute-command-keys "\\[emjupy-save-notebook] asks what to do.")))))
         (lambda (err)
           ;; gone from the server: nothing there to overwrite
           (if (eql (emjupy--http-status-of err) 404) (upload) (fail err))))))))

(defun emjupy--save-to-server (nb &optional overwrite)
  "Save NB to its server, and return non-nil.
Unless OVERWRITE, first make sure the server copy is the one opened or
last saved here, and signal `emjupy-save-conflict' if it changed since --
saved from JupyterLab, say -- rather than overwrite that."
  (with-current-buffer (emjupy-notebook-buffer nb)
    ;; An automatic save on its way finishes first: sent before, it could
    ;; still reach the server after, and put back what this one replaces.
    (let ((deadline (+ (float-time) 15)))
      (while (and emjupy--autosave-in-flight (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (emjupy--sync-all-cells)
    (let ((known (emjupy-notebook-last-modified nb)))
      (unless overwrite
        (let ((current (and known (emjupy--server-last-modified nb))))
          (when (and current (not (equal current known)))
            (signal 'emjupy-save-conflict (list (emjupy-notebook-path nb) current)))))
      (let ((body (make-hash-table :test 'equal)))
        (puthash "type" "notebook" body)
        (puthash "format" "json" body)
        (puthash "content" (json-parse-string (emjupy--serialize-notebook nb)
                                              :object-type 'hash-table :array-type 'array)
                 body)
        (let ((model (emjupy--http-request "PUT" (emjupy-notebook-server nb)
                                           (emjupy--contents-path (emjupy-notebook-path nb))
                                           (json-serialize body))))
          (setf (emjupy-notebook-last-modified nb)
                (and (hash-table-p model) (gethash "last_modified" model)))))
      (set-buffer-modified-p nil)
      (setq emjupy--autosave-paused nil
            emjupy--autosave-last (float-time))
      (emjupy--recovery-delete nb)
      t)))

(defun emjupy-save-notebook ()
  "Save the notebook to its Jupyter server.
If the server copy changed since it was opened here -- saved from
JupyterLab, or by someone else -- ask before overwriting it."
  (interactive)
  (unless emjupy--buffer-notebook
    (user-error "No emjupy notebook associated with this buffer"))
  (let* ((nb emjupy--buffer-notebook)
         (path (emjupy-notebook-path nb)))
    (message "Saving notebook %s..." path)
    (condition-case err
        (emjupy--save-to-server nb)
      (emjupy-save-conflict
       (if (yes-or-no-p (format "%s changed on the server since it was opened here (%s).  Overwrite it? "
                                path (nth 2 err)))
           (emjupy--save-to-server nb t)
         (user-error "Not saved: your changes are kept here, and in a recovery copy"))))
    (message "Successfully saved %s!" path)))

(defcustom emjupy-recovery-directory
  (expand-file-name (format "emjupy-recovery-%s/" (user-login-name))
                    temporary-file-directory)
  "Where notebooks with unsaved changes are copied, until they are saved.
A copy is written a few seconds after a change, when Emacs is idle, and
when Emacs quits; it is local, so it is written with the server gone.
Opening the notebook again offers to restore it.

By default a directory of your own under the variable
`temporary-file-directory' -- /tmp/emjupy-recovery-USER/ on most systems.
That is emptied when the machine restarts on many systems, so a copy
outlives Emacs crashing but not the machine: for one that outlives both,
choose a directory under `user-emacs-directory', which
\(locate-user-emacs-file \"emjupy-recovery/\") gives.  Copies
are only written into a directory you own that is not a symbolic link,
so another user of a shared /tmp cannot have them written elsewhere."
  :type 'directory
  :group 'emjupy)

(defcustom emjupy-recovery-delay 3
  "Seconds of idle time before unsaved changes are copied for recovery."
  :type 'number
  :group 'emjupy)

(defcustom emjupy-autosave-interval 120
  "Seconds between saves to the server of a notebook with unsaved changes.
As Jupyter does.  A save that would overwrite changes made on the server
since the notebook was opened is not made, and auto-saving that notebook
stops until it is saved by hand.  nil never saves on its own."
  :type '(choice (const :tag "Never" nil) number)
  :group 'emjupy)

(defvar-local emjupy--recovery-tick nil
  "`buffer-modified-tick' when the recovery copy was last written.")

(defvar emjupy--idle-save-timer nil
  "Timer writing recovery copies and saving, when Emacs is idle.")

(defun emjupy--recovery-file (nb)
  "Return the file NB's recovery copy is written to."
  (expand-file-name
   (concat (md5 (format "%s|%s" (emjupy-server-base-url (emjupy-notebook-server nb))
                        (emjupy-notebook-path nb)))
           ".json")
   emjupy-recovery-directory))

(defun emjupy--recovery-write (nb)
  "Copy NB, as it is in its buffer, to its recovery file.
Written whole to a temporary file and renamed, so a copy is never half
written; readable by its owner only, as the notebook may hold anything."
  (with-current-buffer (emjupy-notebook-buffer nb)
    (emjupy--sync-all-cells)
    (let ((file (emjupy--recovery-file nb))
          (record (concat "{\"server\":" (json-serialize (emjupy-server-base-url
                                                          (emjupy-notebook-server nb)))
                          ",\"path\":" (json-serialize (emjupy-notebook-path nb))
                          ",\"written\":" (json-serialize (format-time-string "%F %T"))
                          ",\"notebook\":" (emjupy--serialize-notebook nb) "}")))
      (with-file-modes #o700 (make-directory (file-name-directory file) t))
      (emjupy--recovery-directory-check (file-name-directory file))
      (let ((partial (with-file-modes #o600
                       (make-temp-file (expand-file-name "partial-" (file-name-directory file))))))
        (let ((coding-system-for-write 'no-conversion))
          (write-region record nil partial nil 'quiet))
        (rename-file partial file t))
      (setq emjupy--recovery-tick (buffer-modified-tick))
      file)))

(defun emjupy--recovery-directory-check (dir)
  "Signal a `file-error' unless DIR is a directory of the user\'s own.
In a shared /tmp another user could have made it first, or left a
symbolic link there pointing elsewhere, and the copies -- which may hold
anything the notebook does -- would be written where they choose."
  (let ((attributes (file-attributes (directory-file-name dir))))
    (when (or (file-symlink-p (directory-file-name dir))
              (not (eq (file-attribute-type attributes) t))
              (not (eql (file-attribute-user-id attributes) (user-uid))))
      (signal 'file-error
              (list "Not writing recovery copies" "not a directory of your own" dir)))))

(defun emjupy--recovery-delete (nb)
  "Delete NB's recovery copy, if there is one."
  (let ((file (emjupy--recovery-file nb)))
    (when (file-exists-p file) (delete-file file))))

(defun emjupy--modified-notebook-buffers ()
  "Return the notebook buffers with unsaved edits."
  (seq-filter (lambda (b) (with-current-buffer b
                            (and emjupy--buffer-notebook (buffer-modified-p))))
              (emjupy--notebook-buffers)))

(defun emjupy--idle-save ()
  "Copy unsaved edits for recovery, and save to the server when due.
Run when Emacs is idle, so that neither interrupts typing.  A failure is
said and left: the recovery copy, written first, keeps the changes."
  (dolist (buf (emjupy--modified-notebook-buffers))
    (with-current-buffer buf
      (let ((nb emjupy--buffer-notebook))
        (unless (eql emjupy--recovery-tick (buffer-modified-tick))
          (condition-case err
              (emjupy--recovery-write nb)
            (file-error (message "[emjupy] Could not write a recovery copy of %s: %s"
                                 (emjupy-notebook-path nb) (error-message-string err)))))
        (when (and emjupy-autosave-interval (not emjupy--autosave-paused)
                   (not emjupy--autosave-in-flight)
                   (>= (- (float-time) (or emjupy--autosave-last 0)) emjupy-autosave-interval))
          ;; Not waited for: it says itself how it went.  Starting it can
          ;; still fail -- serializing, or a cookie to fetch first.
          (condition-case err
              (emjupy--autosave-async nb)
            (file-error
             (setq emjupy--autosave-in-flight nil)
             (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
                      (emjupy-notebook-path nb) (error-message-string err)))))))))

(defun emjupy--recovery-write-all ()
  "Copy every notebook with unsaved edits for recovery, as Emacs quits."
  (dolist (buf (emjupy--modified-notebook-buffers))
    (with-current-buffer buf
      (condition-case err
          (emjupy--recovery-write emjupy--buffer-notebook)
        (file-error (message "[emjupy] Could not write a recovery copy: %s"
                             (error-message-string err)))))))

(defun emjupy--kill-buffer-query ()
  "Ask before closing a notebook with unsaved edits.
Return non-nil when it may be closed."
  (or (not emjupy--buffer-notebook) (not (buffer-modified-p)) noninteractive
      (let ((nb emjupy--buffer-notebook))
        (pcase (car (read-multiple-choice
                     (format "%s has unsaved changes" (emjupy-notebook-path nb))
                     '((?s "save" "Save it to the server, then close it")
                       (?k "keep" "Close it; the changes stay in a recovery copy")
                       (?d "discard" "Close it and discard the changes")
                       (?c "cancel" "Do not close it"))))
          (?s (emjupy-save-notebook) t)
          (?k (emjupy--recovery-write nb) t)
          (?d (emjupy--recovery-delete nb) t)
          (_ nil)))))

(defun emjupy--kill-emacs-query ()
  "Offer to save notebooks with unsaved edits before Emacs quits.
Non-nil lets it quit.  Quitting without saving keeps them in recovery
copies, written as Emacs quits."
  (let ((modified (emjupy--modified-notebook-buffers)))
    (or (null modified) noninteractive
        (pcase (car (read-multiple-choice
                     (format "%d notebook%s with unsaved changes" (length modified)
                             (if (cdr modified) "s" ""))
                     '((?s "save" "Save them to their servers, then quit")
                       (?q "quit" "Quit; the changes stay in recovery copies")
                       (?c "cancel" "Do not quit"))))
          (?s (dolist (buf modified)
                (with-current-buffer buf
                  ;; A notebook that cannot be saved is said, and kept in a
                  ;; recovery copy; the others are still saved.
                  (condition-case err
                      (emjupy-save-notebook)
                    ((file-error user-error)
                     (message "[emjupy] %s not saved: %s" (buffer-name)
                              (error-message-string err))))))
              t)
          (?q t)
          (_ nil)))))

(defun emjupy--offer-recovery (nb)
  "Offer to restore unsaved edits to NB from its recovery copy.
A copy no different from what was opened is deleted.  One declined is
kept aside, never deleted, in the recovery directory."
  (let ((file (emjupy--recovery-file nb)))
    (when (file-exists-p file)
      (let* ((record (json-parse-string
                      (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8))
                          (insert-file-contents file))
                        (buffer-string))
                      :object-type 'hash-table :array-type 'array))
             (saved (json-serialize (gethash "notebook" record))))
        (cond
         ((equal saved (emjupy--serialize-notebook nb))
          (delete-file file))
         ((or noninteractive
              (not (y-or-n-p (format "Unsaved changes to %s from %s were kept.  Restore them? "
                                     (emjupy-notebook-path nb) (gethash "written" record)))))
          (let ((aside (concat (file-name-sans-extension file)
                               (format-time-string "-declined-%Y%m%d-%H%M%S.json"))))
            (rename-file file aside t)
            (message "[emjupy] The unsaved changes are kept in %s." aside)))
         (t
          (let ((restored (emjupy--parse-ipynb saved)))
            (with-current-buffer (emjupy-notebook-buffer nb)
              (setf (emjupy-notebook-cells nb) (emjupy-notebook-cells restored)
                    (emjupy-notebook-metadata nb) (emjupy-notebook-metadata restored))
              (emjupy--rerender-notebook)
              (set-buffer-modified-p t)
              (message "[emjupy] Unsaved changes restored; %s"
                       (substitute-command-keys "\\[emjupy-save-notebook] saves them."))))))))))

(defun emjupy-recover-notebook ()
  "Restore unsaved edits to this notebook from its recovery copy."
  (interactive)
  (let ((nb (emjupy--notebook)))
    (unless (file-exists-p (emjupy--recovery-file nb))
      (user-error "No recovery copy of %s" (emjupy-notebook-path nb)))
    (emjupy--offer-recovery nb)))

(defun emjupy--save-machinery-start ()
  "Start copying and saving unsaved edits, and asking before losing them.
Started when the first notebook is opened, so that loading emjupy changes
nothing."
  (unless (timerp emjupy--idle-save-timer)
    (setq emjupy--idle-save-timer
          (run-with-idle-timer emjupy-recovery-delay t #'emjupy--idle-save)))
  (add-hook 'kill-emacs-query-functions #'emjupy--kill-emacs-query)
  (add-hook 'kill-emacs-hook #'emjupy--recovery-write-all))

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

;;;; An .ipynb file opened as text, opened as a notebook

(defcustom emjupy-local-server-timeout 60
  "Seconds to wait for a Jupyter server emjupy starts to answer."
  :type 'number
  :group 'emjupy)

(defvar emjupy--environment-history nil
  "Environments given to `emjupy-open-this-notebook'.")

(defvar emjupy--local-servers nil
  "Jupyter servers emjupy started: plists of :dir, :jupyter, :process, :server.")

(defun emjupy--environment-yml-name (dir)
  "Return the `name:' of the conda environment file in DIR, or nil.
That is environment.yml, or environment.yaml."
  (cl-loop for file in '("environment.yml" "environment.yaml")
           for path = (expand-file-name file dir)
           when (file-readable-p path)
           return (with-temp-buffer
                    (insert-file-contents path)
                    (goto-char (point-min))
                    (when (re-search-forward
                           "^name:[ \t]*[\"']?\\([^\"'#\n]*[^\"'#\n \t]\\)[\"']?[ \t]*\\(?:#.*\\)?$"
                           nil t)
                      (match-string 1)))))

(defun emjupy--conda-environments ()
  "Return conda's environments as an alist of name and directory, or nil.
nil when there is no conda, mamba or micromamba to ask."
  (let ((conda (or (executable-find "conda") (executable-find "mamba")
                   (executable-find "micromamba")))
        (default-directory temporary-file-directory))
    (when conda
      (with-temp-buffer
        (when (eq 0 (call-process conda nil '(t nil) nil "env" "list" "--json"))
          (goto-char (point-min))
          ;; Output that is not JSON -- a warning first, an old conda -- is
          ;; no list of environments: none are offered, and a name or a
          ;; path can still be typed.
          (let ((envs (condition-case nil
                          (gethash "envs" (json-parse-buffer :object-type 'hash-table))
                        (json-error nil))))
            (cl-loop for dir across (or envs [])
                     ;; a named environment lives in .../envs/NAME; the one
                     ;; elsewhere is the installation's own, base
                     collect (cons (if (string-match-p "/envs/[^/]+/?\\'" dir)
                                       (file-name-nondirectory (directory-file-name dir))
                                     "base")
                                   dir))))))))

(defun emjupy--read-environment (dir)
  "Ask for the Python environment to run a notebook in DIR with.
Offered first: the environment named in an environment.yml beside it."
  (let* ((named (emjupy--environment-yml-name dir))
         (envs (emjupy--conda-environments)))
    (completing-read
     (format-prompt "Environment (a conda name, or a venv's directory; empty for the PATH's)" named)
     (mapcar #'car envs) nil nil nil 'emjupy--environment-history named)))

(defun emjupy--environment-jupyter (env)
  "Return the `jupyter' program of the environment ENV.
ENV is a conda environment's name, an environment's directory -- a venv
or a conda prefix -- or empty for the one first on the PATH."
  (let ((in-dir (lambda (dir)
                  (cl-loop for name in '("bin/jupyter" "Scripts/jupyter.exe")
                           for path = (expand-file-name name dir)
                           when (file-executable-p path) return path))))
    (cond
     ((or (null env) (string-empty-p (string-trim env)))
      (or (executable-find "jupyter")
          (user-error "No `jupyter' on the PATH: name an environment that has it")))
     ((file-directory-p (expand-file-name env))
      (or (funcall in-dir (expand-file-name env))
          (user-error "No jupyter in %s: install jupyter-server there" env)))
     (t
      (let ((dir (cdr (assoc env (emjupy--conda-environments)))))
        (cond ((null dir) (user-error "No environment named %s" env))
              ((funcall in-dir dir))
              (t (user-error "No jupyter in the environment %s: install jupyter-server there"
                             env))))))))

(defun emjupy--free-port ()
  "Return a TCP port on this machine that nothing listens on."
  (let ((probe (make-network-process :name "emjupy-port" :server t :host 'local
                                     :service t :family 'ipv4 :noquery t)))
    (prog1 (process-contact probe :service)
      (delete-process probe))))

(defun emjupy--start-local-server (dir jupyter)
  "Return a Jupyter server serving DIR, run by JUPYTER, started if need be.
One already started for DIR with the same JUPYTER is used again.  It
listens on this machine only, on a free port, with a token of its own,
and is stopped when the last notebook it serves is closed."
  (or (cl-loop for s in emjupy--local-servers
               when (and (equal (plist-get s :dir) dir) (equal (plist-get s :jupyter) jupyter)
                         (process-live-p (plist-get s :process)))
               return (plist-get s :server))
      (let* ((port (emjupy--free-port))
             (token (secure-hash 'sha256 (format "%s%s%s" (random) (float-time) (emacs-pid))))
             (output (generate-new-buffer (format " *emjupy server %s*" dir)))
             (default-directory dir)
             (process (make-process
                       :name "emjupy-server" :buffer output :noquery t
                       :command (append
                                 (list jupyter "server" "--no-browser"
                                       "--ServerApp.ip=127.0.0.1"
                                       (format "--ServerApp.port=%d" port)
                                       "--ServerApp.port_retries=0"
                                       (format "--IdentityProvider.token=%s" token)
                                       (format "--ServerApp.root_dir=%s" (expand-file-name dir)))
                                 ;; Jupyter refuses to run as root without it
                                 (and (eql (user-uid) 0) (list "--allow-root")))))
             (server (emjupy--intern-server (format "127.0.0.1:%d" port) token))
             (deadline (+ (float-time) emjupy-local-server-timeout)))
        (message "[emjupy] Starting a Jupyter server for %s..." (abbreviate-file-name dir))
        (while (and (process-live-p process) (< (float-time) deadline)
                    (not (emjupy--server-reachable-p server)))
          (accept-process-output process 0.25))
        (unless (and (process-live-p process) (emjupy--server-reachable-p server))
          (let ((said (with-current-buffer output
                        (buffer-substring-no-properties (max (point-min) (- (point-max) 600))
                                                        (point-max)))))
            (when (process-live-p process) (delete-process process))
            (user-error "The Jupyter server did not start:\n%s" said)))
        (push (list :dir dir :jupyter jupyter :process process :server server)
              emjupy--local-servers)
        server)))

(defun emjupy--stop-local-server-if-unused (server)
  "Stop SERVER, if emjupy started it and no notebook it serves is open."
  (let ((entry (cl-find server emjupy--local-servers
                        :key (lambda (s) (plist-get s :server)))))
    (when (and entry
               (not (cl-some (lambda (b)
                               (and (not (eq b (current-buffer)))
                                    (eq (emjupy-notebook-server
                                         (buffer-local-value 'emjupy--buffer-notebook b))
                                        server)))
                             (emjupy--notebook-buffers))))
      (when (process-live-p (plist-get entry :process))
        (delete-process (plist-get entry :process)))
      (setq emjupy--local-servers (delq entry emjupy--local-servers)))))

;;;###autoload
(defun emjupy-open-this-notebook ()
  "Open the .ipynb file this buffer visits as a notebook, on a local kernel.
For a notebook opened as a file -- its JSON shown as text.  Asks for the
Python environment to run it in, offered first the one an
environment.yml beside it names; starts a Jupyter server there, for the
notebook\\='s directory, on this machine only; and replaces this buffer
with the notebook.  The server stops when the last notebook it serves is
closed, and with Emacs."
  (interactive)
  (let ((file buffer-file-name))
    (unless (and file (string-suffix-p ".ipynb" file t))
      (user-error "This buffer is not visiting a .ipynb file"))
    (when (file-remote-p file)
      (user-error "Only a notebook on this machine; for another, %s"
                  (substitute-command-keys "\\[emjupy-login] to its server")))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "The notebook opens from the file: save %s first? "
                            (file-name-nondirectory file)))
          (save-buffer)
        (user-error "Not opened: save the file, or revert it, first")))
    (let* ((dir (file-name-directory file))
           (jupyter (emjupy--environment-jupyter (emjupy--read-environment dir)))
           (server (emjupy--start-local-server dir jupyter))
           ;; A kernel for the notebook to open on, as logging in binds one:
           ;; a server just started has none of its own.
           (_kernel (emjupy--bind-server-kernel server))
           (text (current-buffer))
           (window (selected-window))
           (notebook (emjupy-open-notebook (file-name-nondirectory file) server)))
      (setq emjupy--current-server server)
      (when (buffer-live-p notebook)
        (with-current-buffer notebook
          (add-hook 'kill-buffer-hook
                    (lambda () (emjupy--stop-local-server-if-unused server)) nil t))
        (when (window-live-p window) (set-window-buffer window notebook))
        ;; In place: the text is the same file, and would go stale as the
        ;; notebook is saved.
        (kill-buffer text))
      notebook)))

(provide 'emjupy-notebook)
;;; emjupy-notebook.el ends here
