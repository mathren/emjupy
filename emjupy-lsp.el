;;; emjupy-lsp.el --- LSP over the Jupyter server's own WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Assisted-by: Claude:claude-opus-5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Talks LSP to the language server that `jupyter-lsp' runs ON THE JUPYTER
;; SERVER, over that server's own WebSocket -- the same transport the
;; notebook already uses, and the same arrangement JupyterLab's
;; `jupyterlab-lsp' uses.
;;
;; This replaces the shadow FILE for remote notebooks, and the reason is
;; measured rather than aesthetic.  Handing Eglot a file means Eglot opens
;; it, and when the notebook is remote that file is remote: creating one
;; cost 36 SSH round trips, mostly directory probing while something looked
;; for a project root.  Worse, TRAMP waits by calling
;; `accept-process-output', which runs Emacs's timers -- 29 of them in a
;; 0.3s wait, measured -- so the eldoc timer fired INSIDE the TRAMP call and
;; asked for the shadow buffer again, recursively, until Emacs gave up with
;; "Lisp nesting exceeds `max-lisp-eval-depth'".
;;
;; Over this transport there is no file and no second connection.  The
;; document is synced in-band with `didChange', which is what LSP is for,
;; and the server sits next to the kernel, so `import mylib' resolves
;; against the environment the code will actually run in.
;;
;; The document URI is a path on the SERVER's filesystem, not this one.
;; That is the point: the server resolves it, we never open it.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'jsonrpc)
;; Eglot ships with Emacs 29, which this package requires, so it is always
;; there in practice.  Required softly all the same, as the rest of emjupy
;; does -- though note the WebSocket transport below inherits from Eglot's
;; own class, so its absence is a load error rather than a graceful
;; degradation.  If emjupy ever supports an Emacs without Eglot, that
;; section needs guarding, and a `featurep' check around it is not enough.
(require 'eglot nil t)

(declare-function websocket-open "websocket")
(declare-function websocket-send-text "websocket")
(declare-function websocket-openp "websocket")
(declare-function websocket-close "websocket")
(declare-function websocket-frame-text "websocket")
(declare-function websocket-on-message "websocket" (ws))
(declare-function emjupy--build-shadow-content "emjupy-eglot" (nb))
(declare-function emjupy--parse-shadow-sections "emjupy-eglot" (text))
(declare-function emjupy--rerender-notebook "emjupy-cells" (&optional cell))
(declare-function emjupy--shadow-section-start "emjupy-eglot" (buf id))
(declare-function emjupy--shadow-cell-marker "emjupy-eglot" (id))
(declare-function emjupy--cell-at-point "emjupy-cells" (&optional pos))
(declare-function emjupy--remote-root-for "emjupy-notebook" (server))
(declare-function emjupy--redact-url "emjupy-http" (url))
(declare-function emjupy--shadow-blocked-p "emjupy-eglot" ())

(defcustom emjupy-lsp-enabled t
  "When non-nil, use the language server run by `jupyter-lsp\='.

That server lives next to the kernel and is reached over the Jupyter
server\='s own WebSocket -- no file on disk, no second connection, and
the environment it describes is the one the code will run in.  Set this
to nil to fall back to the Eglot shadow-file arrangement."
  :type 'boolean
  :group 'emjupy)

(defcustom emjupy-lsp-server "pylsp"
  "Identifier of the language server to ask `jupyter-lsp' for.

Whatever `/lsp/status' lists.  \"pylsp\" is the usual Python one."
  :type 'string
  :group 'emjupy)

(defcustom emjupy-lsp-first-timeout 10.0
  "Seconds to allow the first request after the handshake.

`jupyter-lsp' starts the language server lazily, so the first question
waits for a process launch and an initial index.  Measured cold, a
completion that answers in milliseconds afterwards took seconds the
first time -- long enough that a one-second budget returned nothing and
looked like a server that did not work."
  :type 'number
  :group 'emjupy)

(defcustom emjupy-lsp-timeout 0.3
  "Seconds to wait for a language-server reply before giving up.

Deliberately short.  Completion and eldoc run after ordinary commands,
so this is time the user spends waiting; a late answer is worth less
than a responsive editor."
  :type 'number
  :group 'emjupy)

(cl-defstruct emjupy-lsp
  "A live LSP session against a server run by `jupyter-lsp'."
  ws          ; the websocket
  server      ; the `emjupy-server' it belongs to
  (next-id 0) ; JSON-RPC request counter
  pending     ; id -> result, filled by the on-message handler
  callbacks   ; id -> function, for replies nobody waits for
  uri         ; document URI, on the SERVER's filesystem
  (version 0) ; didChange version counter
  ready       ; non-nil once `initialize' has been answered
  warmed      ; non-nil once one request has been answered
  text)       ; the document as last sent, so didChange is skipped when equal

;; --- transport --------------------------------------------------------------

(defun emjupy--lsp-url (server)
  "Return the `jupyter-lsp' WebSocket URL on SERVER."
  (let* ((parts (emjupy--server-parts server))
         (scheme (if (equal (plist-get parts :scheme) "https") "wss" "ws"))
         (token (emjupy-server-token server)))
    (format "%s://%s:%s%s/lsp/ws/%s%s"
            scheme (plist-get parts :host) (plist-get parts :port)
            (or (plist-get parts :path) "")
            emjupy-lsp-server
            (if (and token (not (string-empty-p token)))
                (concat "?token=" (url-hexify-string token))
              ""))))

(defun emjupy--lsp-handle (session frame)
  "Record the message in FRAME against SESSION.

Frames are plain JSON, one message each -- `jupyter-lsp' has already
stripped the Content-Length framing LSP uses over a pipe."
  (let* ((text (websocket-frame-text frame))
         (msg (ignore-errors (json-parse-string text :object-type 'hash-table))))
    (when (hash-table-p msg)
      (let ((id (gethash "id" msg)))
        ;; Responses only.  Server-initiated requests and notifications --
        ;; diagnostics, progress -- are not answered: nothing here needs
        ;; them, and pretending otherwise would mean maintaining state the
        ;; notebook never reads.
        (when (and id (or (gethash "result" msg) (gethash "error" msg)))
          (let ((callback (and (emjupy-lsp-callbacks session)
                               (gethash id (emjupy-lsp-callbacks session)))))
            (if callback
                (progn
                  (remhash id (emjupy-lsp-callbacks session))
                  (setf (emjupy-lsp-warmed session) t)
                  (ignore-errors
                    (funcall callback (and (not (gethash "error" msg))
                                           (gethash "result" msg)))))
              (puthash id msg (emjupy-lsp-pending session)))))))))

(defun emjupy--lsp-async (session method params callback)
  "Send METHOD with PARAMS over SESSION; call CALLBACK with the result.

Nothing waits.  A blocking wait here is what made every cursor movement
cost the full request timeout when the server was slow to answer or
never answered at all -- and since eldoc runs after every command, that
was the whole editor."
  (when (emjupy--lsp-live-p session)
    (let ((id (cl-incf (emjupy-lsp-next-id session))))
      (unless (emjupy-lsp-callbacks session)
        (setf (emjupy-lsp-callbacks session) (make-hash-table :test 'equal)))
      (puthash id callback (emjupy-lsp-callbacks session))
      (emjupy--lsp-send session method params id)
      id)))

(defun emjupy--lsp-send (session method params &optional id)
  "Send METHOD with PARAMS over SESSION, as request ID or a notification."
  (let ((msg (make-hash-table :test 'equal)))
    (puthash "jsonrpc" "2.0" msg)
    (puthash "method" method msg)
    (puthash "params" (or params (make-hash-table :test 'equal)) msg)
    (when id (puthash "id" id msg))
    (websocket-send-text (emjupy-lsp-ws session) (json-serialize msg))))

(defvar emjupy--lsp-in-request nil
  "Non-nil while a request is waiting, to keep replies from nesting.")

(defun emjupy--lsp-request (session method params &optional timeout)
  "Send METHOD with PARAMS over SESSION and wait for the reply.

Returns the result, or nil on timeout or error.  Waits with
`accept-process-output', which runs timers -- so a request that arrives
while another is waiting is refused rather than allowed to nest.  That
nesting is exactly what made the file-based client recurse until Emacs
ran out of stack."
  (when (and session (emjupy--lsp-live-p session)
             (not emjupy--lsp-in-request))
    (let* ((emjupy--lsp-in-request t)
           (id (cl-incf (emjupy-lsp-next-id session)))
           (deadline (+ (float-time) (or timeout emjupy-lsp-timeout)))
           (pending (emjupy-lsp-pending session)))
      (emjupy--lsp-send session method params id)
      (while (and (not (gethash id pending))
                  (< (float-time) deadline))
        (accept-process-output nil 0.01))
      (let ((msg (gethash id pending)))
        (remhash id pending)
        (and msg (not (gethash "error" msg)) (gethash "result" msg))))))

(defun emjupy--lsp-live-p (session)
  "Return non-nil if SESSION's socket is open."
  (and session
       (emjupy-lsp-ws session)
       (ignore-errors (websocket-openp (emjupy-lsp-ws session)))))

;; --- session ----------------------------------------------------------------

(defun emjupy--lsp-document-uri (nb)
  "Return the document URI for NB, on the SERVER's filesystem.

Built from the kernel's working directory, which is the notebook's own
folder, so the server resolves `import mylib' against the code that
sits beside it.  Nothing here ever opens this path."
  (let ((dir (or (emjupy-notebook-kernel-cwd nb) "/"))
        (name (file-name-nondirectory
               (or (emjupy-notebook-path nb) "notebook.ipynb"))))
    (concat "file://" (file-name-as-directory dir)
            (concat (file-name-sans-extension name) ".emjupy.py"))))

(defun emjupy--lsp-connect (nb)
  "Open an LSP session for NB, or return nil.

Returns as soon as the socket is up; `initialize' is sent immediately
and the answer collected on the first request that needs it."
  (let* ((server (emjupy-notebook-server nb))
         (session (make-emjupy-lsp
                   :server server
                   :pending (make-hash-table :test 'equal)
                   :uri (emjupy--lsp-document-uri nb))))
    (condition-case err
        (progn
          (setf (emjupy-lsp-ws session)
                (websocket-open
                 (emjupy--lsp-url server)
                 :on-message (lambda (_ws frame) (emjupy--lsp-handle session frame))
                 :on-close (lambda (_ws) (setf (emjupy-lsp-ready session) nil))
                 :on-error (lambda (_ws _type _err) nil)))
          session)
      (error
       (ignore err)
       (emjupy--lsp-explain-failure server)
       nil))))

;;;###autoload
(defun emjupy-lsp-diagnose ()
  "Report which language server this notebook is using, and how it got there.

Reads the server Eglot has attached to the shadow buffer, which is where
the answer lives.  It used to read a slot filled in by an earlier,
hand-written client; once that client was replaced the slot stayed
empty, and this reported a socket that had never opened however well the
socket was working."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (shadow (emjupy-notebook-shadow-buffer nb))
         (attached (and (buffer-live-p shadow)
                        (with-current-buffer shadow
                          (and (fboundp 'eglot-current-server)
                               (ignore-errors (eglot-current-server))))))
         (kind (cond
                ((null attached) "none")
                ((ignore-errors (object-of-class-p attached 'emjupy-eglot-server))
                 "over the Jupyter WebSocket")
                (t "a local process")))
         (alive (and attached (ignore-errors (jsonrpc-running-p attached))))
         (lines
          (list
           (format "notebook        %s" (or (emjupy-notebook-path nb) "?"))
           (format "server          %s" (if server (emjupy--server-label server) "none"))
           (format "server root     %s" (or (and server (emjupy--remote-root-for server))
                                            "unknown -- no kernel has reported in"))
           (format "kernel cwd      %s" (or (emjupy-notebook-kernel-cwd nb)
                                            "unknown -- kernel has not answered"))
           (format "lsp enabled     %s" (if emjupy-lsp-enabled "yes" "no"))
           (format "lsp url         %s"
                   ;; Redacted: this report is written to be pasted into a
                   ;; bug report, and the URL carries the token.
                   (if server (emjupy--redact-url (emjupy--lsp-url server)) "n/a"))
           (format "shadow file     %s"
                   (if (buffer-live-p shadow)
                       (or (buffer-local-value 'buffer-file-name shadow) "unnamed")
                     "not created"))
           (format "language server %s" kind)
           (format "connection      %s"
                   (cond ((null attached) "not attached")
                         (alive "running")
                         (t "attached but not running")))
           (format "back-off        %s"
                   (if (ignore-errors (emjupy--shadow-blocked-p)) "in effect" "none")))))
    (with-current-buffer (get-buffer-create "*emjupy language server*")
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (string-join lines "\n") "\n\n")
        (insert "If no language server is attached, ask the Jupyter server directly:\n")
        (insert (format "  curl -s 'http://%s/lsp/status?token=TOKEN'\n"
                        (and server (emjupy-server-base-url server))))
        (insert "  404 no jupyter-lsp   403 wrong token   sessions {} no language server\n")
        (goto-char (point-min)))
      (setq buffer-read-only t)
      (display-buffer (current-buffer)))))

(defun emjupy--lsp-explain-failure (server)
  "Say why SERVER has no language server, distinguishing the three causes.

The WebSocket refusing tells us nothing useful on its own, so ask
/lsp/status, whose answer separates the cases:

  404  `jupyter-lsp\' is not installed, so there is no endpoint at all
  403  the token was refused
  200  the endpoint is there -- then the question is what it lists,
       and an empty list means no LANGUAGE server is installed, which
       is the usual surprise: `jupyterlab-lsp\' is only the plumbing
       and installs none."
  (let* ((status (condition-case err
                     (emjupy--http-request "GET" server "/lsp/status")
                   (error (error-message-string err))))
         (label (emjupy--server-label server)))
    (cond
     ((and (stringp status) (string-match-p "404" status))
      (message "[emjupy] No jupyter-lsp on %s: pip install jupyter-lsp" label))
     ((and (stringp status) (string-match-p "403" status))
      (message "[emjupy] %s refused the token for /lsp/status." label))
     ((hash-table-p status)
      (let ((servers (and (hash-table-p (gethash "sessions" status))
                          (hash-table-keys (gethash "sessions" status)))))
        (if servers
            (message "[emjupy] %s offers %s but %s did not start."
                     label (string-join servers ", ") emjupy-lsp-server)
          (message "%s %s"
                   (format "[emjupy] jupyter-lsp on %s lists no language server." label)
                   "Install one where the kernel runs, e.g. python-lsp-server."))))
     (t
      (message "[emjupy] No language server on %s (%s)." label status)))))

(defvar-local emjupy--lsp-blocked-until nil
  "Time before which no further attempt is made to set up a session.")

(defcustom emjupy-lsp-retry-interval 30
  "Seconds to wait before trying the language server again after a failure."
  :type 'number
  :group 'emjupy)

(defun emjupy--lsp-initialize (nb session)
  "Run the LSP handshake for NB on SESSION.  Return non-nil on success."
  (let* ((root (concat "file://" (or (emjupy-notebook-kernel-cwd nb) "/")))
         (caps (make-hash-table :test 'equal))
         (params (make-hash-table :test 'equal)))
    (puthash "processId" :null params)
    (puthash "rootUri" root params)
    (puthash "capabilities" caps params)
    (puthash "workspaceFolders" :null params)
    ;; Asynchronous.  The handshake can take seconds -- `jupyter-lsp' starts
    ;; the language server lazily -- and waiting for it on the path eldoc
    ;; uses means waiting for it after every command.
    (emjupy--lsp-async
     session "initialize" params
     (lambda (result)
       (when result
         (emjupy--lsp-send session "initialized" (make-hash-table :test 'equal))
         (setf (emjupy-lsp-ready session) t)
         (emjupy--lsp-sync nb session))))
    t))

(defun emjupy--lsp-sync (nb session)
  "Send NB's code to SESSION, opening the document or changing it."
  (let ((text (emjupy--build-shadow-content nb)))
    (unless (equal text (emjupy-lsp-text session))
      (let ((doc (make-hash-table :test 'equal))
            (params (make-hash-table :test 'equal)))
        (if (null (emjupy-lsp-text session))
            (progn
              (puthash "uri" (emjupy-lsp-uri session) doc)
              (puthash "languageId" "python" doc)
              (puthash "version" (cl-incf (emjupy-lsp-version session)) doc)
              (puthash "text" text doc)
              (puthash "textDocument" doc params)
              (emjupy--lsp-send session "textDocument/didOpen" params))
          (let ((change (make-hash-table :test 'equal)))
            (puthash "uri" (emjupy-lsp-uri session) doc)
            (puthash "version" (cl-incf (emjupy-lsp-version session)) doc)
            (puthash "text" text change)
            (puthash "textDocument" doc params)
            ;; Whole-document sync.  A notebook's code is small and the
            ;; alternative is tracking ranges across cells that are
            ;; constantly renumbered.
            (puthash "contentChanges" (vector change) params)
            (emjupy--lsp-send session "textDocument/didChange" params)))
        (setf (emjupy-lsp-text session) text)))))

(cl-defun emjupy--lsp-session (nb)
  "Return a ready LSP session for NB, making one if needed, or nil.

Needs a server and a kernel working directory: without the first there
is nowhere to connect, and without the second no URI the server could
resolve.  Both arrive in the ordinary course of opening a notebook, so
the nil here means \"not yet\", not \"never\"."
  (unless (and nb (emjupy-notebook-server nb) (emjupy-notebook-kernel-cwd nb))
    (cl-return-from emjupy--lsp-session nil))
  (let ((session (emjupy-notebook-lsp nb)))
    ;; Connecting and handshaking are started here but never waited for.
    ;; Until the session reports ready this returns nil, and the caller does
    ;; nothing -- which is the correct behaviour for eldoc and completion:
    ;; better no answer than a frozen editor.
    (cond
     ((and session (emjupy-lsp-ready session) (emjupy--lsp-live-p session))
      (emjupy--lsp-sync nb session)
      session)
     ((and emjupy--lsp-blocked-until
           (time-less-p (current-time) emjupy--lsp-blocked-until))
      nil)
     (t
      (unless (emjupy--lsp-live-p session)
        (setq session (emjupy--lsp-connect nb))
        (setf (emjupy-notebook-lsp nb) session)
        (if session
            (emjupy--lsp-initialize nb session)
          ;; Could not even open the socket: usually `jupyter-lsp' is not
          ;; installed on that server.  Stop asking for a while.
          (setq emjupy--lsp-blocked-until
                (time-add (current-time) emjupy-lsp-retry-interval))))
      nil))))

(defun emjupy-lsp-shutdown (&optional nb)
  "Close NB's language-server session."
  (interactive)
  (let* ((nb (or nb (emjupy--notebook)))
         (session (emjupy-notebook-lsp nb)))
    (when (emjupy--lsp-live-p session)
      (ignore-errors (websocket-close (emjupy-lsp-ws session))))
    (setf (emjupy-notebook-lsp nb) nil)
    (when (called-interactively-p 'interactive)
      (message "[emjupy] Language-server session closed."))))

;; --- positions --------------------------------------------------------------

(defun emjupy--lsp-position (nb)
  "Return the LSP position in NB's document matching point, or nil."
  (let* ((cell (emjupy--cell-at-point))
         (text (emjupy--build-shadow-content nb)))
    (when (and cell (eq (emjupy-cell-type cell) 'code) text)
      (let* ((marker (emjupy--shadow-cell-marker (emjupy-cell-id cell)))
             (idx (string-search marker text)))
        (when idx
          (let* ((body (+ idx (length marker) 1))
                 (ov (emjupy-cell-overlay cell))
                 (offset (if (overlayp ov) (- (point) (overlay-start ov)) 0))
                 (abs (min (length text) (+ body (max 0 offset))))
                 (before (substring text 0 abs))
                 (line (- (length (split-string before "\n" nil)) 1))
                 (col (- abs (or (cl-position ?\n before :from-end t) -1) 1))
                 (pos (make-hash-table :test 'equal)))
            (puthash "line" line pos)
            (puthash "character" col pos)
            pos))))))

(defun emjupy--lsp-text-document-params (session position)
  "Return textDocument/position params for SESSION at POSITION."
  (let ((doc (make-hash-table :test 'equal))
        (params (make-hash-table :test 'equal)))
    (puthash "uri" (emjupy-lsp-uri session) doc)
    (puthash "textDocument" doc params)
    (puthash "position" position params)
    params))


;; --- what the notebook asks for ---------------------------------------------


(defun emjupy--lsp-offset-of-position (text line character)
  "Return the character offset of LINE and CHARACTER within TEXT.

LSP counts lines and characters from zero; Emacs counts buffer positions
from one and does not count either.  This is the conversion, and it
clamps rather than signalling, because an edit landing one past the end
of a document is a server being generous about a final newline, not a
reason to abandon a rename."
  (let ((offset 0)
        (remaining line)
        (len (length text)))
    (while (and (> remaining 0) (< offset len))
      (let ((nl (string-search "\n" text offset)))
        (if nl
            (setq offset (1+ nl) remaining (1- remaining))
          (setq offset len remaining 0))))
    (min len (+ offset (or character 0)))))

(defun emjupy--lsp-apply-edits (text edits)
  "Return TEXT with EDITS applied, where EDITS are LSP TextEdits.

Applied back to front.  Every edit's range refers to the document as the
server last saw it, so applying one from the front moves the ground
under all the others -- the classic way to corrupt a rename into
nonsense that still looks plausible."
  (let* ((prepared
          (mapcar (lambda (edit)
                    (let* ((range (gethash "range" edit))
                           (start (gethash "start" range))
                           (end (gethash "end" range)))
                      (list (emjupy--lsp-offset-of-position
                             text (gethash "line" start) (gethash "character" start))
                            (emjupy--lsp-offset-of-position
                             text (gethash "line" end) (gethash "character" end))
                            (or (gethash "newText" edit) ""))))
                  (append edits nil)))
         (sorted (sort prepared (lambda (a b) (> (car a) (car b))))))
    (dolist (edit sorted text)
      (setq text (concat (substring text 0 (nth 0 edit))
                         (nth 2 edit)
                         (substring text (nth 1 edit)))))))

(defun emjupy--lsp-edits-for-this-document (result uri)
  "Return the TextEdits in a WorkspaceEdit RESULT that apply to URI."
  (when (hash-table-p result)
    (let ((changes (gethash "changes" result))
          (docs (gethash "documentChanges" result)))
      (cond
       ((hash-table-p changes) (gethash uri changes))
       ((and docs (> (length docs) 0))
        (let (found)
          (cl-loop for change across docs
                   until found
                   do (let* ((doc (and (hash-table-p change)
                                       (gethash "textDocument" change)))
                             (this (and (hash-table-p doc) (gethash "uri" doc))))
                        (when (equal this uri)
                          (setq found (gethash "edits" change)))))
          found))))))


(provide 'emjupy-lsp)
;;; Eglot over the Jupyter WebSocket ----------------------------------------

;; Everything below exists so that Eglot's own commands -- all of them, and
;; any added later -- reach the language server running beside the kernel.
;;
;; The alternative was wiring each command up by hand, which does not scale
;; and did not: rename worked and nothing else did.  Eglot talks to a server
;; through three jsonrpc generics, and `jsonrpc-connection-send' dispatches
;; on the connection class, so a subclass can carry the traffic anywhere it
;; likes.  Here it goes over the same WebSocket the notebook already uses.
;;
;; `jsonrpc-process-connection' insists on a process object, so one is
;; created and then ignored: it exists to satisfy a type check and carries
;; nothing.

(defcustom emjupy-eglot-idle-command "cat"
  "Program stood up to satisfy Eglot's requirement for a process.

Eglot builds its connection on `jsonrpc-process-connection', which type
checks for a process object.  Nothing is written to this one and nothing
is read from it -- the traffic goes over the WebSocket -- so the
requirement is met by the most inert program available."
  :type 'string
  :group 'emjupy)

(defvar emjupy--eglot-pending nil
  "Plist handed to the next `emjupy-eglot-server' as it is constructed.

`eglot--connect' decides the initargs, so there is no way to pass the
WebSocket in.  It is left here instead and collected on the way past.")

(defclass emjupy-eglot-server (eglot-lsp-server)
  ((ws :initform nil :accessor emjupy-eglot-server-ws)
   (local-uri :initform nil :accessor emjupy-eglot-server-local-uri)
   (server-uri :initform nil :accessor emjupy-eglot-server-server-uri)
   (local-doc :initform nil :accessor emjupy-eglot-server-local-doc)
   (server-doc :initform nil :accessor emjupy-eglot-server-server-doc))
  :documentation "An Eglot server reached over the Jupyter server's WebSocket.")

(cl-defmethod initialize-instance :after ((server emjupy-eglot-server) &rest _)
  "Collect the WebSocket and URIs left by `emjupy--eglot-connect'."
  (setf (emjupy-eglot-server-ws server) (plist-get emjupy--eglot-pending :ws)
        (emjupy-eglot-server-local-uri server) (plist-get emjupy--eglot-pending :local-uri)
        (emjupy-eglot-server-server-uri server) (plist-get emjupy--eglot-pending :server-uri)
        (emjupy-eglot-server-local-doc server) (plist-get emjupy--eglot-pending :local-doc)
        (emjupy-eglot-server-server-doc server) (plist-get emjupy--eglot-pending :server-doc))
  (let ((ws (emjupy-eglot-server-ws server)))
    (when ws
      ;; The socket was opened before the server existed, so its handler is
      ;; pointed at the server now that there is one.
      ;; The socket was opened before the server existed, so its handler is
      ;; pointed at the server now that there is one.  Set through the
      ;; struct rather than a setf accessor, which websocket.el does not
      ;; export.
      (aset ws (cl-struct-slot-offset 'websocket 'on-message)
            (lambda (_ws frame) (emjupy--eglot-receive server frame))))))

(defun emjupy--eglot-rewrite-uris (object from to)
  "Return OBJECT with every URI equal to FROM replaced by TO.

Eglot names the document by the file it manages, which is the shadow
file on this machine.  The language server knows it by the path the
kernel would see.  Both directions of every message pass through here,
which is the one place the two names have to agree."
  (cond
   ((and (stringp object) (equal object from)) to)
   ((and (stringp object) from to (string-suffix-p "/" from)
         (string-prefix-p from object))
    (concat to (substring object (length from))))
   ((hash-table-p object)
    (let ((copy (make-hash-table :test 'equal)))
      (maphash (lambda (k v)
                 (puthash k (emjupy--eglot-rewrite-uris v from to) copy))
               object)
      copy))
   ((vectorp object)
    (vconcat (mapcar (lambda (x) (emjupy--eglot-rewrite-uris x from to))
                     (append object nil))))
   ((consp object)
    ;; A plist from Eglot: values may be URIs, keys never are.
    (let (out)
      (while object
        (let ((k (car object)) (v (cadr object)))
          (push k out)
          (push (emjupy--eglot-rewrite-uris v from to) out)
          (setq object (cddr object))))
      (nreverse out)))
   (t object)))

(cl-defmethod jsonrpc-connection-send ((server emjupy-eglot-server)
                                       &rest args
                                       &key id method params
                                       result error)
  "Send a JSON-RPC message for SERVER over its WebSocket.
ARGS carry ID, METHOD, PARAMS, RESULT and ERROR as jsonrpc defines them."
  (ignore args)
  (let* ((msg (make-hash-table :test 'equal)))
    (puthash "jsonrpc" "2.0" msg)
    (when id (puthash "id" id msg))
    (when method
      (puthash "method" (cond ((keywordp method) (substring (symbol-name method) 1))
                              ((symbolp method) (symbol-name method))
                              (t method))
               msg))
    (when params
      (puthash "params"
               (emjupy--eglot-rewrite-uris
                (emjupy--eglot-plist-to-table params)
                (emjupy-eglot-server-local-uri server)
                (emjupy-eglot-server-server-uri server))
               msg))
    (when result (puthash "result" (emjupy--eglot-plist-to-table result) msg))
    (when error (puthash "error" (emjupy--eglot-plist-to-table error) msg))
    (let ((ws (emjupy-eglot-server-ws server)))
      (when (and ws (websocket-openp ws))
        (websocket-send-text ws (json-serialize msg))))))

(defun emjupy--eglot-plist-to-table (object)
  "Return OBJECT with plists turned into hash tables, for `json-serialize'."
  (cond
   ((and (consp object) (keywordp (car object)))
    (let ((table (make-hash-table :test 'equal)))
      (while object
        (puthash (substring (symbol-name (car object)) 1)
                 (emjupy--eglot-plist-to-table (cadr object))
                 table)
        (setq object (cddr object)))
      table))
   ((and (consp object) (listp (cdr object)))
    (vconcat (mapcar #'emjupy--eglot-plist-to-table object)))
   ((eq object :json-false) :false)
   ((eq object t) t)
   (t object)))

(defun emjupy--eglot-receive (server frame)
  "Hand the message in FRAME to SERVER, as if it had come from a process."
  (let* ((text (websocket-frame-text frame))
         (msg (ignore-errors (json-parse-string text :object-type 'plist
                                                :null-object nil
                                                :false-object :json-false))))
    (when msg
      (jsonrpc-connection-receive
       server
       ;; Only the document itself is renamed on the way back.  Rewriting by
       ;; directory, as the outbound direction does, would drag every other
       ;; path under the server's root along with it -- so a definition
       ;; correctly answered as .../latmod.py came back pointing at a file
       ;; of that name beside the shadow, which does not exist, and the
       ;; lookup produced nothing.  Paths elsewhere on the server are left
       ;; as they are; `emjupy-open-server-file' knows how to fetch them.
       (emjupy--eglot-rewrite-uris
        msg
        (emjupy-eglot-server-server-doc server)
        (emjupy-eglot-server-local-doc server))))))

(cl-defmethod jsonrpc-running-p ((server emjupy-eglot-server))
  "Return non-nil while SERVER's WebSocket is open."
  (let ((ws (emjupy-eglot-server-ws server)))
    (and ws (ignore-errors (websocket-openp ws)) t)))

(cl-defmethod jsonrpc-shutdown ((server emjupy-eglot-server) &optional _cleanup)
  "Close SERVER's WebSocket."
  (let ((ws (emjupy-eglot-server-ws server)))
    (when (and ws (ignore-errors (websocket-openp ws)))
      (ignore-errors (websocket-close ws))))
  (setf (emjupy-eglot-server-ws server) nil))

(defun emjupy--eglot-connect (nb buffer)
  "Attach Eglot to BUFFER, talking to NB's server over its WebSocket.

BUFFER is the shadow buffer, which stays on this machine: it is now only
something for Eglot to manage, not something a language server reads.
Returns the server, or nil."
  (let* ((server (emjupy-notebook-server nb))
         (local-file (buffer-local-value 'buffer-file-name buffer))
         (ws (condition-case err
                 (websocket-open (emjupy--lsp-url server)
                                 :on-message (lambda (_ws _frame) nil)
                                 :on-error (lambda (&rest _) nil))
               (error (emjupy--lsp-explain-failure server)
                      (ignore err)
                      nil))))
    (when (and ws local-file)
      (let* ((emjupy--eglot-pending
              (list :ws ws
                    ;; Directory prefixes, not the one file: the same
                    ;; substitution then fixes rootUri, the document URI and
                    ;; anything else naming a path under them.
                    :local-uri (concat "file://" (file-name-directory local-file))
                    :server-uri (concat "file://"
                                        (file-name-as-directory
                                         (or (emjupy-notebook-kernel-cwd nb) "/")))
                    :local-doc (concat "file://" local-file)
                    :server-doc (concat "file://"
                                        (file-name-as-directory
                                         (or (emjupy-notebook-kernel-cwd nb) "/"))
                                        (file-name-nondirectory local-file))))
             ;; The root has to be the directory the SERVER will resolve
             ;; against, not the one holding the shadow file here.  Eglot
             ;; turns this into rootUri, and pylsp resolves `import mylib'
             ;; against it -- so pointing it at the local shadow directory
             ;; meant every cross-file lookup came back empty while
             ;; same-document ones worked, which is a confusing way to fail.
             ;; The project must contain the shadow buffer or Eglot will not
             ;; manage it -- and an unmanaged buffer sends no didOpen, so the
             ;; server never learns the document exists.  The root the SERVER
             ;; is told about is fixed on the wire instead, below.
             (project (cons 'transient (file-name-directory local-file))))
        (with-current-buffer buffer
          (condition-case err
              (eglot--connect (list major-mode) project
                              'emjupy-eglot-server
                              ;; A process must exist for the base class;
                              ;; this one is never written to or read from.
                              (list emjupy-eglot-idle-command)
                              "python")
            (error
             (ignore-errors (websocket-close ws))
             (message "[emjupy] Could not attach Eglot over the WebSocket: %s"
                      (error-message-string err))
             nil)))))))

;;; emjupy-lsp.el ends here
