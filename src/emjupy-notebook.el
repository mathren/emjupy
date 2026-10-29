;;; emjupy-notebook.el --- Logging in; opening, creating and switching notebooks  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Logging in to a server; listing, opening, creating and switching
;; notebooks; exporting one to Python; trusting one; and the menu.  Reading
;; and writing nbformat is in emjupy-ipynb.el, saving in emjupy-save.el.
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
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-commands)
(require 'emjupy-kernel)
(require 'emjupy-figures)
(require 'emjupy-eglot)
(require 'emjupy-mode)
(require 'emjupy-save)

;;;; Options

(defcustom emjupy-default-port "8888"
  "Port assumed when none is given.

Jupyter's own default, and the one a tunnel is usually pointed at."
  :type 'string
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
    ;; `emjupy--redraw-notebook': reopening reuses the buffer with
    ;; fresh cells, and the old overlays must go with the text.
    (setq emjupy--buffer-notebook nb)
    ;; Large outputs are hidden, for drawing only: Jupyter scrolls or cuts
    ;; them, and drawn, they make the notebook slow.
    (cl-loop for cell across (emjupy-notebook-cells nb)
             do (setf (emjupy-cell-size-hidden cell)
                      (emjupy--cell-outputs-large-p cell)))
    (emjupy--redraw-notebook)
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

;;;; Exporting to Python

(defvar emjupy--export-history nil
  "Minibuffer history for export destinations.")

(defun emjupy--export-default-name (nb)
  "Return the default .py path for NB, beside the notebook itself."
  (let ((path (or (emjupy-notebook-path nb) "notebook.ipynb")))
    (concat (file-name-sans-extension path) ".py")))

(defun emjupy-trust-notebook ()
  "Trust this notebook, as Jupyter's Trust Notebook does.
Its outputs' HTML and JavaScript then run when opened.  The server signs
it, so it is trusted wherever Jupyter opens it from that server; asks
first, as a notebook is a file people share."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (path (emjupy-notebook-path nb)))
    (when (y-or-n-p (format "Trust %s, and run the scripts its outputs carry when they are opened? "
                            path))
      (emjupy--http-request "POST" (emjupy-notebook-server nb)
                            (concat (emjupy--contents-path path) "/trust"))
      (mapc #'emjupy--mark-cell-trusted (emjupy-notebook-cells nb))
      (message "[emjupy] %s is trusted." path))))

(defun emjupy--json-body (&rest pairs)
  "Return PAIRS, a plist of keys and values, as a JSON object string."
  (let ((h (make-hash-table :test 'equal)))
    (while pairs (puthash (pop pairs) (pop pairs) h))
    (json-serialize h)))

(defun emjupy--follow-notebook (nb path)
  "Have NB, and its buffer, be the notebook at PATH on its server now."
  (setf (emjupy-notebook-path nb) path)
  (with-current-buffer (emjupy-notebook-buffer nb)
    (rename-buffer (emjupy--notebook-buffer-name path
                                                 (emjupy-notebook-server nb))
                   t)))

(defun emjupy-rename-notebook (new-path)
  "Rename this notebook, on its server, to NEW-PATH.
Unsaved changes stay here, saved under the new name."
  (interactive
   (list (read-string "Rename notebook to: "
                      (emjupy-notebook-path (emjupy--notebook)))))
  (let* ((nb (emjupy--notebook)) (old (emjupy-notebook-path nb)))
    (emjupy--http-request "PATCH" (emjupy-notebook-server nb)
                          (emjupy--contents-path old)
                          (emjupy--json-body "path" new-path))
    (emjupy--follow-notebook nb new-path)
    (message "[emjupy] Renamed %s to %s" old new-path)))

(defun emjupy-save-notebook-as (new-path)
  "Save this notebook as NEW-PATH on its server, and go on with that one.
The file it was is left as it was last saved, as JupyterLab's Save As."
  (interactive
   (list (read-string "Save notebook as: "
                      (emjupy-notebook-path (emjupy--notebook)))))
  (let ((nb (emjupy--notebook)))
    (when (and (emjupy--http-exists-p (emjupy-notebook-server nb) new-path)
               (not (y-or-n-p (format "%s exists.  Overwrite it? "
                                      new-path))))
      (user-error "Not saved"))
    (emjupy--follow-notebook nb new-path)
    (emjupy--save-to-server nb t)
    (message "[emjupy] Saved as %s" new-path)))

(defun emjupy-duplicate-notebook ()
  "Make a copy of this notebook's file beside it, on its server, and open it.
The copy is of the file as last saved, as JupyterLab's Duplicate."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (path (emjupy-notebook-path nb))
         (model (emjupy--http-request
                 "POST" server
                 (emjupy--contents-path (or (file-name-directory path) ""))
                 (emjupy--json-body "copy_from" path))))
    (emjupy-open-notebook (gethash "path" model) server)))

(defun emjupy-revert-to-checkpoint ()
  "Revert this notebook to its checkpoint on the server, made when saved.
What has changed since, saved or not, is lost: asks first."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (path (emjupy-notebook-path nb))
         (address (concat (emjupy--contents-path path) "/checkpoints"))
         (latest (car (last (append (emjupy--http-request
                                     "GET" server address)
                                    nil)))))
    (unless latest
      (user-error "No checkpoint: one is made when the notebook is saved"))
    (when (y-or-n-p (format "Revert %s to its checkpoint of %s, losing changes since? "
                            path (gethash "last_modified" latest)))
      (emjupy--http-request "POST" server
                            (concat address "/" (gethash "id" latest)))
      (with-current-buffer (emjupy-notebook-buffer nb)
        (set-buffer-modified-p nil))
      (emjupy-open-notebook path server)
      (message "[emjupy] %s reverted to its checkpoint" path))))

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
    ("A" "all cells" emjupy-run-all-cells)
    ("{" "all above this one" emjupy-run-cells-above)
    ("}" "this one and all below" emjupy-run-cells-below)
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
    ("O" "outline, beside" emjupy-toc)
    ("I" "hide or show code" emjupy-toggle-cell-code)
    ("N" "rename" emjupy-rename-notebook)
    ("W" "save as" emjupy-save-notebook-as)
    ("D" "duplicate" emjupy-duplicate-notebook)
    ("V" "revert to the checkpoint" emjupy-revert-to-checkpoint)
    ("'" "edit this cell in a buffer" emjupy-edit-cell-externally)]]
  ;; A row of its own: four groups side by side came to some 120 columns,
  ;; so in an ordinary frame the last column wrapped mid-word.
  [["Server"
    ("g" "log in to a server" emjupy-login)
    ("R" "restart the kernel" emjupy-restart-kernel)
    ("Q" "restart, then run all" emjupy-restart-kernel-and-run-all)
    ("Z" "restart, clearing outputs" emjupy-restart-kernel-and-clear-outputs)
    ("Y" "shut the kernel down" emjupy-shutdown-kernel)
    ("C" "reconnect the kernel" emjupy-reconnect-kernel)
    ("K" "choose a kernel" emjupy-connect-kernel-interactive)
    ("?" "what is going on" emjupy-status)]
   ["Debugger"
    ("x d" "start or stop debugging" emjupy-debug-mode)
    ("x b" "breakpoint on this line" emjupy-debug-toggle-breakpoint)
    ("x x" "stop on exceptions" emjupy-debug-break-on-exceptions)
    ("x f" "choose a frame" emjupy-debug-frame)
    ("x n" "next line" emjupy-debug-next)
    ("x s" "step in" emjupy-debug-step-in)
    ("x o" "step out" emjupy-debug-step-out)
    ("x c" "continue" emjupy-debug-continue)
    ("x e" "evaluate here" emjupy-debug-evaluate)
    ("x v" "variables here" emjupy-debug-variables)]
   ["Language server"
    ("d" "diagnose it" emjupy-lsp-diagnose)
    ("v" "which emjupy is this" emjupy-version)]])

(provide 'emjupy-notebook)
;;; emjupy-notebook.el ends here
