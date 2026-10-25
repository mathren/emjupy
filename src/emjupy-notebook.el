;;; emjupy-notebook.el --- Notebook session management and .ipynb I/O for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Logging in to a server, listing, opening and creating notebooks, and reading
;; and writing nbformat v4.
;;
;; One port, one kernel: `emjupy-login' adopts the kernel already running behind
;; a tunnel, so opening a notebook lands in the live session.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tramp)
(require 'transient)
(require 'emjupy-core)
(require 'emjupy-ipynb)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-figures)
(require 'emjupy-eglot)
(require 'emjupy-remote)
(require 'emjupy-mode)

(declare-function dired-goto-file "dired" (file))
(defvar python-indent-guess-indent-offset-verbose)

;;;; Options

(defcustom emjupy-default-port "8888"
  "Port assumed when none is given.

Jupyter's own default, and the one a tunnel is usually pointed at."
  :type 'string
  :group 'emjupy-kernels)

(defcustom emjupy-recovery-directory
  (expand-file-name (format "emjupy-recovery-%s/" (user-login-name))
                    temporary-file-directory)
  "Where notebooks with unsaved changes are copied, until saved.
A copy is written a few seconds after a change, when idle, and as Emacs
quits -- locally, so with the server gone too; reopening the notebook
offers it.  By default a directory of your own under the variable
`temporary-file-directory', /tmp/emjupy-recovery-USER/ on most systems,
often emptied on restart: for copies outliving the machine, use
\(locate-user-emacs-file \"emjupy-recovery/\").  Copies go only into a
directory you own that is not a symbolic link."
  :type 'directory
  :group 'emjupy-saving)

(defcustom emjupy-recovery-delay 3
  "Seconds of idle time before unsaved changes are copied for recovery."
  :type 'number
  :group 'emjupy-saving)

(defcustom emjupy-autosave-interval 120
  "Seconds between saves to the server of a notebook with unsaved changes.
As Jupyter does.  A save that would overwrite changes made on the server
since the notebook was opened is not made, and auto-saving that notebook
stops until it is saved by hand.  nil never saves on its own."
  :type '(choice (const :tag "Never" nil) number)
  :group 'emjupy-saving)

(defcustom emjupy-local-server-timeout 60
  "Seconds to wait for a Jupyter server emjupy starts to answer."
  :type 'number
  :group 'emjupy-kernels)

;;;; Logging in

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
Probes /api/contents: some jupyter_server versions answer /api/status
without authentication."
  (let ((inhibit-message t)
        (message-log-max nil))
    ;; A failed request -- refused, or no server -- is the answer "no";
    ;; anything else is a bug, and is raised rather than taken for it.
    (condition-case nil
        (and (emjupy--http-request "GET" server "/api/contents") t)
      (emjupy-http-error nil))))

(defun emjupy--resolve-token (base-url explicit)
  "Work out the token for BASE-URL, prompting only when unavoidable.
EXPLICIT, then the token registered for this server, then none -- many
tunnelled servers run without -- and only then ask."
  (or explicit
      ;; A token already registered for this server is used as it stands.
      ;; It was accepted once; re-probing only to confirm costs a request
      ;; whose failure is then reported as an error the user did not cause.
      (let ((known (gethash base-url emjupy--servers)))
        (and known
             (let ((tok (emjupy-server-token known)))
               (and tok (not (string-empty-p tok)) tok))))
      ;; Now a probe is worth making, for a server started with
      ;; `--IdentityProvider.token='.  Quietly: a 403 is the expected answer
      ;; from one that wants a token.
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
One port, one kernel: a single kernel running there is adopted
silently; only a server with none gets a fresh one."
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
                              (format "Kernel on %s: "
                                      (emjupy--server-label server))
                              (mapcar #'car kernels) nil t nil
                              'emjupy--kernel-history)
                             kernels))))))
    (setf (emjupy-server-kernel-id server) id)
    id))

(defun emjupy--read-server-url ()
  "Ask for a port or URL, returning `emjupy-default-port' if none is given."
  (let ((answer (read-string
                 (format "Jupyter port or URL (default %s): "
                         emjupy-default-port)
                 nil 'emjupy--port-history)))
    (if (string-empty-p (string-trim answer))
        emjupy-default-port
      answer)))

;;;###autoload
(defun emjupy-login (url &optional token)
  "Connect to the Jupyter server at URL and open one of its notebooks.
TOKEN, when given, is used instead of prompting; with a prefix argument
the token is always asked for, otherwise only if the server needs one.

URL may be a bare port (\"8888\"), host:port, or an http(s) URL; a bare
port means localhost, as behind `ssh -L'.  emjupy adopts the kernel
already running behind it, so a notebook opens into that live REPL.

One port, one kernel: log in once per tunnel, and several remote
sessions stay live.  \\[emjupy-connect-kernel-interactive] gives one
notebook a different kernel."
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

    ;; Wrong credentials: ask once and carry on, rather than fail the command.
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
      (message "[emjupy] Connected to %s, kernel %s." base-url kernel-id))
    (emjupy-list-notebooks server)))

;;;###autoload
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

;;;; Opening and creating notebooks

(defun emjupy--notebook-buffer-name (path server)
  "Return the buffer name for PATH on SERVER.
The server is part of the name: the same notebook path can exist on two
different servers, and one buffer cannot represent both."
  (format "*emjupy: %s [%s]*" path (emjupy--server-label server)))

(defun emjupy--fetch-notebook (path server)
  "Return the notebook at PATH on SERVER, fetched and parsed.
Signal an error if the server returns no notebook."
  (let* ((response (emjupy--http-request "GET" server
                                         (emjupy--contents-path path)))
         (content (and response (gethash "content" response))))
    (unless content
      (error "Failed to fetch notebook content from server"))
    (let ((nb (emjupy--parse-ipynb (json-serialize content))))
      (setf (emjupy-notebook-server nb) server
            (emjupy-notebook-path nb) path
            (emjupy-notebook-last-modified nb)
            (gethash "last_modified" response))
      nb)))

(defun emjupy--draw-opened-notebook (nb buf already-open)
  "Draw NB, just fetched, in BUF, in `emjupy-mode'.
ALREADY-OPEN non-nil means BUF showed it before: point stays."
  (with-current-buffer buf
    (emjupy-mode)
    ;; The notebook is set before drawing, through
    ;; `emjupy--rerender-notebook': reopening reuses the buffer with
    ;; fresh cells, and the old overlays must go with the text.
    (setq emjupy--buffer-notebook nb)
    (emjupy--rerender-notebook)
    ;; A freshly opened notebook starts at the top, as a file does.
    ;; One already open keeps where you were: re-opening it to get
    ;; back to what you were reading should not lose your place.
    (unless already-open
      (goto-char (point-min)))
    ;; As drawn, it is the server copy: nothing unsaved yet.
    (set-buffer-modified-p nil)
    (emjupy--save-machinery-start)
    (add-hook 'kill-buffer-query-functions
              #'emjupy--kill-buffer-query nil t)
    (emjupy--offer-recovery nb)))

(defun emjupy--attach-bound-kernel (nb buf server)
  "Attach NB, shown in BUF, to the kernel bound to SERVER's port, if any.
So opening a notebook drops you into the kernel already running behind
that tunnel, rather than into a dead buffer needing a connect step."
  (when-let* ((kernel-id (emjupy-server-kernel-id server)))
    (with-current-buffer buf
      (condition-case err
          (emjupy-connect-kernel nb kernel-id)
        ;; Everything: connecting opens a WebSocket, and
        ;; `websocket-open' reports a failure as a plain error.  The
        ;; notebook opens either way; the kernel is connected later.
        (error (message "[emjupy] Could not attach kernel %s: %s"
                        kernel-id (error-message-string err)))))))

;;;###autoload
(defun emjupy-open-notebook (path &optional server)
  "Fetch PATH from SERVER, parse it, and render it in `emjupy-mode'.
Returns the notebook buffer."
  (let* ((server (or server (emjupy--server)))
         (open (get-buffer (emjupy--notebook-buffer-name path server))))
    ;; Already open with unsaved changes: show it as it is.  Fetching it
    ;; again would replace its cells with the server's, losing the changes.
    (if (and open (buffer-live-p open)
             (with-current-buffer open (buffer-modified-p)))
        (progn (switch-to-buffer open)
               (message "[emjupy] %s is open with unsaved changes; shown as it is." path)
               open)
      (message "[emjupy] Fetching notebook: %s..." path)
      (let* ((nb (emjupy--fetch-notebook path server))
             (buf-name (emjupy--notebook-buffer-name path server))
             ;; Whether this notebook is already on screen decides where
             ;; point ends up.
             (already-open (get-buffer buf-name))
             (buf (get-buffer-create buf-name)))
        (setf (emjupy-notebook-buffer nb) buf)
        (emjupy--draw-opened-notebook nb buf already-open)
        (switch-to-buffer buf)
        (unless already-open
          (goto-char (point-min)))
        (emjupy--attach-bound-kernel nb buf server)
        ;; Language support warmed up now, so completion is ready when
        ;; typing starts rather than paying the server's start then.
        (when emjupy-language-support
          (with-demoted-errors "[emjupy] Language support did not start: %S"
            (with-current-buffer buf
              (emjupy--ensure-shadow-buffer nb))))
        (message "[emjupy] Opened notebook: %s%s" path
                 (if (emjupy-server-kernel-id server)
                     ""
                   (format ". Press %s to select or spawn a kernel."
                           (substitute-command-keys "\\[emjupy-connect-kernel-interactive]"))))
        buf))))

;;;###autoload
(defun emjupy-create-notebook (&optional server)
  "Create a brand new blank notebook on SERVER and open it."
  (interactive)
  (let* ((server (or server (emjupy--server)))
         (raw-name (read-string "New notebook name (default Untitled.ipynb): "
                                nil 'emjupy--notebook-history))
         (name (if (string-empty-p raw-name) "Untitled.ipynb" raw-name))
         (filename (if (string-match-p "\\.ipynb$" name) name
                     (concat name ".ipynb")))
         (req-body (emjupy--notebook-request (emjupy--new-notebook-json []))))
    (message "[emjupy] Creating %s on Jupyter server..." filename)
    (let ((response (emjupy--http-request "PUT" server
                                          (emjupy--contents-path filename)
                                          (json-serialize req-body))))
      (if response
          (emjupy-open-notebook filename server)
        (error "Failed to write %s to server" filename)))))

;; ---------------------------------------------------------------------
;;; Server dashboard:

;;; ---------------------------------------------------------------------
;; One buffer per server: the kernels running on it and the notebooks stored on
;; it, browsable into subdirectories.  Built on `tabulated-list-mode'.  Not a
;; file manager: `d' hands the directory to Dired, over TRAMP when it is remote.

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

;;;; Exporting to Python

(defvar emjupy--export-history nil
  "Minibuffer history for export destinations.")

(defun emjupy--export-default-name (nb)
  "Return the default .py path for NB, beside the notebook itself."
  (let ((path (or (emjupy-notebook-path nb) "notebook.ipynb")))
    (concat (file-name-sans-extension path) ".py")))

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
                    (y-or-n-p (format "%s exists.  Overwrite it? " file)))
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
                   (not (y-or-n-p (format "%s exists on %s.  Overwrite it? "
                                             path
                                             (emjupy--server-label server)))))
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

;;;; Saving, and never losing what is not saved

(defvar-local emjupy--autosave-paused nil
  "Non-nil when the server copy changed elsewhere and must not be saved over.")

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
                    (concat (emjupy--contents-path (emjupy-notebook-path nb))
                            "?content=0"))))
        (and (hash-table-p model) (gethash "last_modified" model)))
    ;; Gone from the server: there is nothing a save would overwrite.
    (emjupy-http-status
     (if (eql (emjupy--http-status-of err) 404) nil (signal (car err)
                                                            (cdr err))))))

(defun emjupy--autosave-done (nb)
  "Mark NB's auto-save as no longer in flight."
  (let ((buf (emjupy-notebook-buffer nb)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (setq emjupy--autosave-in-flight nil)))))

(defun emjupy--autosave-failed (nb err)
  "End NB's auto-save, which failed with ERR, saying so."
  (emjupy--autosave-done nb)
  (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
           (emjupy-notebook-path nb) (error-message-string err)))

(defun emjupy--autosave-upload (nb body tick)
  "Send BODY, NB as it was at its buffer's modification TICK, not waiting.
Once saved, the buffer is unmodified and the recovery copy goes -- unless
it was edited since."
  (let ((buf (emjupy-notebook-buffer nb)))
    (emjupy--http-request-async
     "PUT" (emjupy-notebook-server nb)
     (emjupy--contents-path (emjupy-notebook-path nb)) body
     (lambda (model)
       (emjupy--autosave-done nb)
       (setf (emjupy-notebook-last-modified nb)
             (and (hash-table-p model) (gethash "last_modified" model)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           ;; saved as it was when sent; edited since, it is not
           (when (eql tick (buffer-modified-tick))
             (set-buffer-modified-p nil)
             (emjupy--recovery-delete nb)))))
     (lambda (err) (emjupy--autosave-failed nb err)))))

(defun emjupy--autosave-async (nb)
  "Save NB to its server without waiting, as auto-save does.
As saving by hand -- not over a notebook changed elsewhere -- but
asynchronous: over a remote link the requests take round trips.  Edits
made meanwhile leave the notebook unsaved, for the next save."
  (let* ((buf (emjupy-notebook-buffer nb))
         (path (emjupy-notebook-path nb))
         (known (emjupy-notebook-last-modified nb))
         tick body)
    (with-current-buffer buf
      (emjupy--sync-all-cells)
      (setq tick (buffer-modified-tick))
      (setq body (json-serialize
                  (emjupy--notebook-request (emjupy--notebook-json nb))))
      (setq emjupy--autosave-in-flight t
            emjupy--autosave-last (float-time)))
    (if (not known)
        (emjupy--autosave-upload nb body tick)
      (emjupy--http-request-async
       "GET" (emjupy-notebook-server nb)
       (concat (emjupy--contents-path path) "?content=0") nil
       (lambda (model)
         (let ((current (and (hash-table-p model)
                             (gethash "last_modified" model))))
           (if (or (not current) (equal current known))
               (emjupy--autosave-upload nb body tick)
             (emjupy--autosave-done nb)
             (when (buffer-live-p buf)
               (with-current-buffer buf (setq emjupy--autosave-paused t)))
             (message "[emjupy] %s changed on the server since it was opened; not saved over.  %s"
                      path (substitute-command-keys "\\[emjupy-save-notebook] asks what to do.")))))
       (lambda (err)
         ;; gone from the server: nothing there to overwrite
         (if (eql (emjupy--http-status-of err) 404)
             (emjupy--autosave-upload nb body tick)
           (emjupy--autosave-failed nb err)))))))

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
            (signal 'emjupy-save-conflict
                    (list (emjupy-notebook-path nb) current)))))
      (let ((body (emjupy--notebook-request (emjupy--notebook-json nb))))
        (let ((model (emjupy--http-request
                      "PUT" (emjupy-notebook-server nb)
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
    (message "[emjupy] Saving notebook %s..." path)
    (condition-case err
        (emjupy--save-to-server nb)
      (emjupy-save-conflict
       (if (y-or-n-p (format "%s changed on the server since it was opened here (%s).  Overwrite it? "
                                path (nth 2 err)))
           (emjupy--save-to-server nb t)
         (user-error "Not saved: your changes are kept here, and in a recovery copy"))))
    (message "[emjupy] Saved %s" path)))

(defvar-local emjupy--recovery-tick nil
  "`buffer-modified-tick' when the recovery copy was last written.")

(defvar emjupy--idle-save-timer nil
  "Timer writing recovery copies and saving, when Emacs is idle.")

(defun emjupy--recovery-file (nb)
  "Return the file NB's recovery copy is written to."
  (expand-file-name
   (concat (md5 (format "%s|%s"
                        (emjupy-server-base-url (emjupy-notebook-server nb))
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
          (record (concat "{\"server\":" (json-serialize
                                          (emjupy-server-base-url
                                           (emjupy-notebook-server nb)))
                          ",\"path\":"
                          (json-serialize (emjupy-notebook-path nb))
                          ",\"written\":"
                          (json-serialize (format-time-string "%F %T"))
                          ",\"notebook\":" (emjupy--serialize-notebook nb)
                          "}")))
      (emjupy--private-directory (file-name-directory file) "recovery copies")
      (let ((partial (with-file-modes #o600
                       (make-temp-file (expand-file-name
                                        "partial-"
                                        (file-name-directory file))))))
        (let ((coding-system-for-write 'no-conversion))
          (write-region record nil partial nil 'quiet))
        (rename-file partial file t))
      (setq emjupy--recovery-tick (buffer-modified-tick))
      file)))

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
                                 (emjupy-notebook-path nb)
                                 (error-message-string err)))))
        (when (and emjupy-autosave-interval (not emjupy--autosave-paused)
                   (not emjupy--autosave-in-flight)
                   (>= (- (float-time) (or emjupy--autosave-last 0))
                       emjupy-autosave-interval))
          ;; Not waited for: it says itself how it went.  Starting it can
          ;; still fail -- serializing, or a cookie to fetch first.
          (condition-case err
              (emjupy--autosave-async nb)
            (file-error
             (setq emjupy--autosave-in-flight nil)
             (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
                      (emjupy-notebook-path nb)
                      (error-message-string err)))))))))

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
                       (?k "keep"
                           "Close it; the changes stay in a recovery copy")
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
                     (format "%d notebook%s with unsaved changes"
                             (length modified)
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
                                     (emjupy-notebook-path nb)
                                     (gethash "written" record)))))
          (let ((aside (concat (file-name-sans-extension file)
                               (format-time-string "-declined-%Y%m%d-%H%M%S.json"))))
            (rename-file file aside t)
            (message "[emjupy] The unsaved changes are kept in %s." aside)))
         (t
          (let ((restored (emjupy--parse-ipynb saved)))
            (with-current-buffer (emjupy-notebook-buffer nb)
              (setf (emjupy-notebook-cells nb) (emjupy-notebook-cells restored)
                    (emjupy-notebook-metadata nb)
                    (emjupy-notebook-metadata restored))
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

;;;; Switching, and status

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
                           (format "kernel %s (connected)"
                                   (emjupy-kernel-id k)))
                          (t (format "kernel %s (disconnected)"
                                     (emjupy-kernel-id k)))))))
        buffers "\n")))))

;;;; An .ipynb file opened as text, opened as a notebook

(defvar emjupy--environment-history nil
  "Environments given to `emjupy-open-this-notebook'.")

(defvar emjupy--local-servers nil
  "Servers emjupy started: plists of :dir, :jupyter, :process and :server.")

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

(defun emjupy--conda-environments (&optional where)
  "Return conda's environments as an alist of name and directory, or nil.
nil when there is no conda, mamba or micromamba to ask.  Asked on the
machine of WHERE, a directory -- one through TRAMP asks the remote."
  (let* ((default-directory (or where temporary-file-directory))
         (remote (and (file-remote-p default-directory) t))
         (conda (or (executable-find "conda" remote)
                    (executable-find "mamba" remote)
                    (executable-find "micromamba" remote))))
    (when conda
      (with-temp-buffer
        (when (eq 0 (process-file (or (file-remote-p conda 'localname) conda)
                                  nil '(t nil) nil "env" "list" "--json"))
          (goto-char (point-min))
          ;; Output that is not JSON -- a warning first, an old conda -- is
          ;; no list of environments: none are offered, and a name or a
          ;; path can still be typed.
          (let ((envs (condition-case nil
                          (gethash "envs"
                                   (json-parse-buffer :object-type 'hash-table))
                        (json-error nil))))
            (cl-loop for dir across (or envs [])
                     ;; a named environment lives in .../envs/NAME; the one
                     ;; elsewhere is the installation's own, base
                     collect (cons (if (string-match-p "/envs/[^/]+/?\\'" dir)
                                       (file-name-nondirectory
                                        (directory-file-name dir))
                                     "base")
                                   dir))))))))

(defun emjupy--read-environment (dir)
  "Ask for the Python environment to run a notebook in DIR with.
Offered first: the environment named in an environment.yml beside it."
  (let* ((named (emjupy--environment-yml-name dir))
         (envs (emjupy--conda-environments dir)))
    (completing-read
     (format-prompt "Environment (a conda name, or a venv's directory; empty for the PATH's)" named)
     (mapcar #'car envs) nil nil nil 'emjupy--environment-history named)))

(defun emjupy--environment-jupyter (env &optional where)
  "Return the `jupyter' program of the environment ENV.
ENV is a conda environment's name, an environment's directory -- a venv
or a conda prefix -- or empty for the one first on the PATH.  Looked
for on the machine of WHERE, a directory, and named as it names it."
  (let* ((default-directory (or where default-directory))
         (remote (and (file-remote-p default-directory) t))
         (here (lambda (path) (or (file-remote-p path 'localname) path)))
         (in-dir (lambda (dir)
                   (cl-loop for name in '("bin/jupyter" "Scripts/jupyter.exe")
                            for path = (expand-file-name name dir)
                            when (file-executable-p path)
                            return (funcall here path)))))
    (cond
     ((or (null env) (string-empty-p (string-trim env)))
      (let ((found (executable-find "jupyter" remote)))
        (cond (found (funcall here found))
              (remote (user-error "No `jupyter' on the remote PATH -- conda or modules often set it up only in a login shell: name the environment"))
              (t (user-error "No `jupyter' on the PATH: name an environment that has it")))))
     ((file-directory-p (expand-file-name env))
      (or (funcall in-dir (expand-file-name env))
          (user-error "No jupyter in %s: install jupyter-server there" env)))
     (t
      (let ((dir (cdr (assoc env (emjupy--conda-environments where)))))
        (cond ((null dir) (user-error "No environment named %s" env))
              ((funcall in-dir (if remote
                                   (concat (file-remote-p default-directory)
                                           dir) dir)))
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
               when (and (equal (plist-get s :dir) dir)
                         (equal (plist-get s :jupyter) jupyter)
                         (process-live-p (plist-get s :process)))
               return (plist-get s :server))
      (let* ((port (emjupy--free-port))
             (token (secure-hash 'sha256 (format "%s%s%s" (random)
                                                 (float-time) (emacs-pid))))
             (output (generate-new-buffer (format " *emjupy server %s*" dir)))
             (default-directory dir)
             (process (make-process
                       :name "emjupy-server" :buffer output :noquery t
                       :command (append
                                 (list
                                  jupyter "server" "--no-browser"
                                  "--ServerApp.ip=127.0.0.1"
                                  (format "--ServerApp.port=%d" port)
                                  "--ServerApp.port_retries=0"
                                  (concat "--IdentityProvider.token=" token)
                                  (format "--ServerApp.root_dir=%s"
                                          (expand-file-name dir)))
                                 ;; Jupyter refuses to run as root without it
                                 (and (eql (user-uid) 0)
                                      (list "--allow-root")))))
             (server (emjupy--intern-server (format "127.0.0.1:%d" port) token))
             (deadline (+ (float-time) emjupy-local-server-timeout)))
        (message "[emjupy] Starting a Jupyter server for %s..."
                 (abbreviate-file-name dir))
        (while (and (process-live-p process) (< (float-time) deadline)
                    (not (emjupy--server-reachable-p server)))
          (accept-process-output process 0.25))
        (unless (and (process-live-p process)
                     (emjupy--server-reachable-p server))
          (let ((said (with-current-buffer output
                        (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 600))
                         (point-max)))))
            (when (process-live-p process) (delete-process process))
            (user-error "The Jupyter server did not start:\n%s" said)))
        (push (list :dir dir :jupyter jupyter :process process :server server)
              emjupy--local-servers)
        server)))

(defun emjupy--server-in-use-p (server)
  "Return non-nil if a notebook buffer other than this one is on SERVER."
  (cl-some (lambda (b)
             (and (not (eq b (current-buffer)))
                  (eq (emjupy-notebook-server
                       (buffer-local-value 'emjupy--buffer-notebook b))
                      server)))
           (emjupy--notebook-buffers)))

(defun emjupy--stop-local-server-if-unused (server)
  "Stop SERVER, if emjupy started it and no notebook it serves is open."
  (let ((entry (cl-find server emjupy--local-servers
                        :key (lambda (s) (plist-get s :server)))))
    (when (and entry
               (not (emjupy--server-in-use-p server)))
      (when (process-live-p (plist-get entry :process))
        (delete-process (plist-get entry :process)))
      (setq emjupy--local-servers (delq entry emjupy--local-servers)))))

;;;; An .ipynb file through TRAMP, opened on its own machine

(defconst emjupy--ssh-methods '("ssh" "sshx" "scp" "scpx" "rsync")
  "TRAMP methods that reach a host over SSH, through which a tunnel can go.")

(defvar emjupy--remote-tunnels nil
  "Tunnels emjupy opened to servers it found or started through TRAMP.
Each a plist of :key, naming the route and the remote port; :process;
:port, the local one; and :server.")

(defun emjupy--tramp-chain (file)
  "Return the machines on TRAMP's way to FILE, its own machine last.
Asked of TRAMP, not read from FILE's name: once TRAMP has gone through a
hop it names the file without it, and remembers the way: read from the
name, the hop -- the bastion -- would be lost."
  (tramp-compute-multi-hops (tramp-dissect-file-name file)))

(defun emjupy--tramp-address (v &optional for-ssh)
  "Return the machine of V, a TRAMP file name, as TRAMP writes it.
With FOR-SSH, as ssh -J writes it: a port after a colon, not a hash."
  (let ((user (tramp-file-name-user v)) (host (tramp-file-name-host v))
        (port (tramp-file-name-port v)))
    (substring-no-properties
     (concat (and user (concat user "@")) host
             (and port (concat (if for-ssh ":" "#") port))))))

(defun emjupy--remote-prefix (file)
  "Return the TRAMP prefix reaching FILE's machine, every hop written out."
  (concat "/" (mapconcat
               (lambda (v)
                 (concat (substring-no-properties (tramp-file-name-method v))
                         ":" (emjupy--tramp-address v)))
               (emjupy--tramp-chain file) "|")
          ":"))

(defun emjupy--ssh-route (file)
  "Return how to reach FILE's machine with ssh, for a tunnel.
A plist of :host, :user and :port, and :jumps, the hops before it as ssh
-J takes them.  A file reached otherwise than over SSH -- through sudo, a
container, a method of its own -- has no tunnel to it: a `user-error'."
  (let* ((chain (emjupy--tramp-chain file))
         (target (car (last chain)))
         (hops (butlast chain)))
    (dolist (v chain)
      (unless (member (tramp-file-name-method v) emjupy--ssh-methods)
        (user-error "Only a notebook reached over SSH can be opened on its machine: %s is reached with %s"
                    (tramp-file-name-host v) (tramp-file-name-method v))))
    (list :host (substring-no-properties (tramp-file-name-host target))
          :user (tramp-file-name-user target)
          :port (tramp-file-name-port target)
          :jumps (and hops (mapconcat (lambda (v) (emjupy--tramp-address v t))
                                      hops ",")))))

(defun emjupy--tunnel-command (route local-port remote-port)
  "Return the ssh command forwarding LOCAL-PORT to REMOTE-PORT along ROUTE.
BatchMode: with no terminal to type a password into, a login that wants
one fails at once rather than waiting for ever."
  (append (list "ssh" "-N" "-o" "BatchMode=yes" "-o" "ExitOnForwardFailure=yes"
                "-L" (format "%d:127.0.0.1:%d" local-port remote-port))
          (and (plist-get route :jumps) (list "-J" (plist-get route :jumps)))
          (and (plist-get route :port) (list "-p" (plist-get route :port)))
          (list (if (plist-get route :user)
                    (concat (plist-get route :user) "@" (plist-get route :host))
                  (plist-get route :host)))))

(defun emjupy--remote-shell (dir command)
  "Run COMMAND with sh in DIR, on DIR's machine; return what it printed.
Signal a `file-error' with what it printed if it fails."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((status (process-file "sh" nil t nil "-c" command)))
        (unless (eq status 0)
          (signal 'file-error (list "Running on the remote machine" command
                                    (string-trim (buffer-string)))))
        (string-trim (buffer-string))))))

(defun emjupy--remote-server-for (servers dir)
  "Return the server of SERVERS serving DIR, or a folder above it, or nil.
SERVERS are what `jupyter server list --json' prints, one object a line;
of several, the one serving the folder nearest DIR."
  (let ((dir (file-name-as-directory dir)) (best nil))
    (dolist (line (split-string servers "\n" t))
      (let* ((s (condition-case nil (json-parse-string line :object-type 'plist)
                  (json-error nil)))
             (root (and s (plist-get s :root_dir))))
        (when (and (stringp root)
                   (string-prefix-p (file-name-as-directory root) dir)
                   (or (null best) (> (length root)
                                      (length (plist-get best :root_dir)))))
          (setq best s))))
    best))

(defun emjupy--remote-free-port (dir python)
  "Return a TCP port nothing listens on, on DIR's machine, asked of PYTHON.
python3 is asked if PYTHON cannot be; 0 if neither can say."
  (let ((ask (shell-quote-argument
              (concat "import socket; s = socket.socket(); "
                      "s.bind(('127.0.0.1', 0)); print(s.getsockname()[1])"))))
    (string-to-number
     (emjupy--remote-shell
      dir (format "%s -c %s 2>/dev/null || python3 -c %s"
                  (shell-quote-argument python) ask ask)))))

(defun emjupy--start-remote-server (dir jupyter)
  "Start JUPYTER's server in DIR, through TRAMP, and return what it serves.
A plist like those `emjupy--remote-server-for' returns.  It is left
running when its notebooks are closed, and when Emacs quits: opening one
of them again finds it, and its kernels, as they were."
  (let* ((here (file-remote-p dir 'localname))
         (python (concat (file-name-directory jupyter) "python"))
         (port (emjupy--remote-free-port dir python))
         (token (secure-hash 'sha256 (format "%s%s%s" (random) (float-time)
                                             (emacs-pid))))
         (log (format "${TMPDIR:-/tmp}/emjupy-jupyter-$(id -un)-%d.log" port)))
    (when (zerop port)
      (user-error "Could not find a free port on the remote machine"))
    (emjupy--remote-shell
     dir (format "nohup %s server --no-browser --ServerApp.ip=127.0.0.1 --ServerApp.port=%d --ServerApp.port_retries=0 --IdentityProvider.token=%s --ServerApp.root_dir=%s > %s 2>&1 < /dev/null &"
                 (shell-quote-argument jupyter) port token
                 (shell-quote-argument here) log))
    ;; As `jupyter server list' reports a root: without a final slash, so a
    ;; server started here and one found running give the same root.
    (list :port port :token token :root_dir (directory-file-name here)
          :log log)))

(defun emjupy--open-tunnel (route remote-port)
  "Return a live tunnel's local port to REMOTE-PORT along ROUTE.
One already open is used again.  A login that wanted a password gets a
message saying so: they are not handled yet."
  (let ((key (format "%S:%d" route remote-port)))
    (or (cl-loop for tunnel in emjupy--remote-tunnels
                 when (and (equal (plist-get tunnel :key) key)
                           (process-live-p (plist-get tunnel :process)))
                 return (plist-get tunnel :port))
        (let* ((port (emjupy--free-port))
               (output (generate-new-buffer (format " *emjupy tunnel %s*"
                                                    (plist-get route :host))))
               (process (let ((default-directory temporary-file-directory))
                          (make-process :name "emjupy-tunnel" :buffer output
                                        :noquery t
                                        :command (emjupy--tunnel-command
                                                  route
                                                  port remote-port))))
               (deadline (+ (float-time) 30)))
          ;; Up when something answers on the local port, or failed when ssh
          ;; exits.
          (while (and (process-live-p process) (< (float-time) deadline)
                      (not (ignore-error file-error
                             (let ((probe (open-network-stream
                                           "emjupy-probe"
                                           nil "127.0.0.1" port)))
                               (delete-process probe)
                               t))))
            (accept-process-output process 0.25))
          (unless (process-live-p process)
            (let ((said (with-current-buffer output
                          (string-trim (buffer-string)))))
              (if (string-match-p "Permission denied\\|password\\|passphrase\\|keyboard-interactive" said)
                  (user-error "The tunnel to %s needs an SSH login without a password: emjupy runs ssh itself, with nowhere to type one.  Use an SSH key, loaded in an agent (ssh-add) if it has a passphrase -- password logins are not handled yet"
                              (plist-get route :host))
                (user-error "The tunnel to %s did not open: %s"
                            (plist-get route :host) said))))
          (push (list :key key :process process :port port)
                emjupy--remote-tunnels)
          port))))

(defun emjupy--close-tunnel-if-unused (server)
  "Close the tunnel to SERVER if no notebook it serves is open.
The server itself, on the other machine, keeps running."
  (let ((tunnel (cl-find server emjupy--remote-tunnels
                         :key (lambda (s) (plist-get s :server)))))
    (when (and tunnel
               (not (emjupy--server-in-use-p server)))
      (when (process-live-p (plist-get tunnel :process))
        (delete-process (plist-get tunnel :process)))
      (setq emjupy--remote-tunnels (delq tunnel emjupy--remote-tunnels))
      (message "[emjupy] Tunnel closed; the server on %s keeps running -- open the notebook again to get back to it."
               (emjupy-server-files-root server)))))

(defun emjupy--remote-server (dir jupyter route)
  "Return the Jupyter server serving DIR on its machine, started if need be.
One already serving DIR, or a folder above it, is used; otherwise JUPYTER
starts one.  ROUTE names the machine, for the messages."
  (let* ((host (plist-get route :host))
         (_ (message "[emjupy] Looking for a Jupyter server on %s..." host))
         (listed (condition-case nil
                     (emjupy--remote-shell
                      dir (format "%s server list --json"
                                  (shell-quote-argument jupyter)))
                   (file-error "")))
         (found (emjupy--remote-server-for listed (file-remote-p dir
                                                                 'localname))))
    (if found
        (progn (message "[emjupy] Using the Jupyter server already serving %s on %s"
                        (plist-get found :root_dir) host)
               found)
      (message "[emjupy] Starting a Jupyter server on %s..." host)
      (emjupy--start-remote-server dir jupyter))))

(defun emjupy--open-remote-notebook (file)
  "Open FILE, an .ipynb through TRAMP, as a notebook on its own machine.
Finds a Jupyter server already serving its folder there, or starts one
in the environment asked for; opens a tunnel to it, through every hop;
and opens the notebook on it.  Return the notebook\\='s buffer."
  (let* ((route (emjupy--ssh-route file))
         (prefix (emjupy--remote-prefix file))
         (dir (file-name-directory file))
         (jupyter (emjupy--environment-jupyter (emjupy--read-environment dir)
                                               dir))
         (remote (emjupy--remote-server dir jupyter route))
         (local-port (emjupy--open-tunnel route (plist-get remote :port)))
         (server (emjupy--intern-server (format "127.0.0.1:%d" local-port)
                                        (plist-get remote :token)))
         (root (plist-get remote :root_dir))
         (deadline (+ (float-time) emjupy-local-server-timeout)))
    (let ((tunnel (cl-find local-port emjupy--remote-tunnels
                           :key (lambda (s) (plist-get s :port)))))
      (setq emjupy--remote-tunnels
            (cons (plist-put tunnel :server server)
                  (delq tunnel emjupy--remote-tunnels))))
    (setf (emjupy-server-files-root server) (concat prefix root))
    (while (and (< (float-time) deadline)
                (not (emjupy--server-reachable-p server)))
      (sleep-for 0.5))
    (unless (emjupy--server-reachable-p server)
      (user-error "The Jupyter server on %s did not answer%s"
                  (plist-get route :host)
                  (if (plist-get remote :log)
                      (format ":\n%s" (condition-case nil
                                          (emjupy--remote-shell
                                           dir (format "tail -n 15 %s"
                                                       (plist-get remote :log)))
                                        (file-error "")))
                    "")))
    (emjupy--bind-server-kernel server)
    (list server (file-relative-name (file-remote-p file 'localname) root))))

;;;###autoload
(defun emjupy-open-this-notebook ()
  "Open the .ipynb file this buffer visits as a notebook, on its machine.
For a notebook opened as a file -- its JSON shown as text.  Asks for the
Python environment to run it in, offered first the one an
environment.yml beside it names; starts a Jupyter server there, for the
notebook\\='s directory; and replaces this buffer with the notebook.

On this machine, the server listens here only, and stops when the last
notebook it serves is closed, and with Emacs.  Through TRAMP -- over
SSH, through any hops -- a server already serving the folder there is
used, or one is started, and reached through a tunnel; closing the last
notebook closes the tunnel, and leaves the server, and its kernels,
running, to come back to."
  (interactive)
  (let ((file buffer-file-name))
    (unless (and file (string-suffix-p ".ipynb" file t))
      (user-error "This buffer is not visiting a .ipynb file"))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "The notebook opens from the file: save %s first? "
                            (file-name-nondirectory file)))
          (save-buffer)
        (user-error "Not opened: save the file, or revert it, first")))
    (let* ((remote (file-remote-p file))
           (opened (if remote
                       (emjupy--open-remote-notebook file)
                     (let* ((dir (file-name-directory file))
                            (jupyter (emjupy--environment-jupyter
                                      (emjupy--read-environment dir)))
                            (server (emjupy--start-local-server dir jupyter)))
                       ;; A kernel for the notebook to open on, as logging in
                       ;; binds one: a server just started has none of its own.
                       (emjupy--bind-server-kernel server)
                       (list server (file-name-nondirectory file)))))
           (server (car opened))
           (text (current-buffer))
           (window (selected-window))
           (notebook (emjupy-open-notebook (cadr opened) server)))
      (setq emjupy--current-server server)
      (when (buffer-live-p notebook)
        (with-current-buffer notebook
          (add-hook 'kill-buffer-hook
                    (if remote
                        (lambda () (emjupy--close-tunnel-if-unused server))
                      (lambda () (emjupy--stop-local-server-if-unused server)))
                    nil t))
        (when (window-live-p window) (set-window-buffer window notebook))
        ;; In place: the text is the same file, and would go stale as the
        ;; notebook is saved.
        (kill-buffer text))
      notebook)))

;;;; The menu

(defun emjupy--menu-title ()
  "Return the title of `emjupy-menu': emjupy, and the notebook's path.
Transient computes it in a temporary buffer; the notebook is the one the
menu was opened from, `transient--original-buffer'."
  (with-current-buffer (if (buffer-live-p transient--original-buffer)
                           transient--original-buffer
                         (current-buffer))
    (if emjupy--buffer-notebook
        (format "emjupy  %s"
                (or (emjupy-notebook-path emjupy--buffer-notebook) "unsaved"))
      "emjupy")))

;;;###autoload (autoload 'emjupy-menu "emjupy" nil t)
(transient-define-prefix emjupy-menu ()
  "Show what emjupy can do, grouped by what you are trying to do.

There are some forty bindings, and `C-h m' lists them in the order they
were defined rather than the order anyone thinks in.  This groups them,
and can be used as a menu in its own right: everything here runs from
this buffer."
  [:description emjupy--menu-title
   ["Run"
    ("c" "this cell, then move on" emjupy-execute-cell-and-goto-next)
    ("e" "this cell, stay here" emjupy-execute-cell-at-point)
    ("z" "interrupt the kernel" emjupy-interrupt-kernel)
    ("$" "show the last traceback" emjupy-show-traceback)]
   ["Cells"
    ("a" "new cell above" emjupy-insert-cell-above)
    ("b" "new cell below" emjupy-insert-cell-below)
    ("k" "delete this cell" emjupy-delete-cell)
    ("s" "split here" emjupy-split-cell)
    ("j" "join to the one above" emjupy-join-cell-above)
    ("t" "code or markdown" emjupy-cycle-cell-type)
    ("w" "copy this cell" emjupy-copy-cell)
    ("y" "paste a cell" emjupy-yank-cell)]
   ["Output"
    ("o" "hide or show this output" emjupy-toggle-cell-output)
    ("f" "open this output on its own" emjupy-open-output)
    ("l" "clear this output" emjupy-clear-cell-output)
    ("L" "clear every output" emjupy-clear-all-outputs)
    ("r" "redraw the notebook" emjupy-re-render)]]
  [["Moving"
    ("n" "next cell" emjupy-next-cell)
    ("p" "previous cell" emjupy-previous-cell)
    ("<" "start of this cell" emjupy-beginning-of-cell)
    (">" "end of this cell" emjupy-end-of-cell)
    ("M-p" "start of the previous cell" emjupy-beginning-of-previous-cell)
    ("M-n" "start of the next cell" emjupy-beginning-of-next-cell)
    ("M-P" "end of the previous cell" emjupy-end-of-previous-cell)
    ("M-N" "end of the next cell" emjupy-end-of-next-cell)
    ("<up>" "move this cell up" emjupy-move-cell-up)
    ("<down>" "move this cell down" emjupy-move-cell-down)]
   ["Notebook"
    ("S" "save to the server" emjupy-save-notebook)
    ("B" "switch notebook" emjupy-switch-notebook)
    ("X" "export as .py" emjupy-export-py)
    ("'" "edit this cell in a buffer" emjupy-edit-cell-externally)]]
  ;; A row of its own: four groups side by side came to some 120 columns,
  ;; so in an ordinary frame the last column wrapped mid-word.
  [["Server"
    ("g" "log in to a server" emjupy-login)
    ("R" "restart the kernel" emjupy-restart-kernel)
    ("C" "reconnect the kernel" emjupy-reconnect-kernel)
    ("K" "choose a kernel" emjupy-connect-kernel-interactive)
    ("?" "what is going on" emjupy-status)]
   ["Language server"
    ("d" "diagnose it" emjupy-lsp-diagnose)
    ("v" "which emjupy is this" emjupy-version)]])

(provide 'emjupy-notebook)
;;; emjupy-notebook.el ends here
