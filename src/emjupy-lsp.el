;;; emjupy-lsp.el --- LSP over the Jupyter server's own WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Talks LSP to the language server `jupyter-lsp' runs on the Jupyter server,
;; over its WebSocket, as JupyterLab does: the server sits beside the kernel, so
;; imports resolve where the code runs, and the document is synced in-band.
;; Also builds the document's text -- every code cell, under a marker naming it
;; -- and maps positions back, for this transport and Eglot's.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'jsonrpc)
;; Eglot and websocket.el are required: Eglot comes with Emacs 30, websocket is
;; a dependency, and the transport inherits from Eglot's server class.
(require 'eglot)
(require 'websocket)

;;;; Options

(defcustom emjupy-lsp-enabled t
  "When non-nil, use the language server run by `jupyter-lsp'.

That server lives next to the kernel and is reached over the Jupyter
server's own WebSocket -- no file on disk, no second connection, and
the environment it describes is the one the code will run in.  Set this
to nil to fall back to the Eglot shadow-file arrangement."
  :type 'boolean
  :group 'emjupy-language-server)

(defcustom emjupy-lsp-server "pylsp"
  "Identifier of the language server to ask `jupyter-lsp' for.

Whatever `/lsp/status' lists.  \"pylsp\" is the usual Python one."
  :type 'string
  :group 'emjupy-language-server)

(defcustom emjupy-lsp-retry-interval 30
  "Seconds to wait before trying the language server again after a failure."
  :type 'number
  :group 'emjupy-language-server)

(defcustom emjupy-eglot-idle-command "cat"
  "Program stood up to satisfy Eglot's requirement for a process.

Eglot builds its connection on the class named by the symbol
`jsonrpc-process-connection', which insists on a process object.
Nothing is written to this one and nothing is read from it -- the
traffic goes over the WebSocket -- so the most inert program available
meets the requirement."
  :type 'string
  :group 'emjupy-language-server)

(defcustom emjupy-tell-server-where-modules-are t
  "When non-nil, name the notebook's own directory to the language server.

A language server finds `import mylib' by looking beside the file that
imports it.  Beside a notebook there is no file: there is a shadow whose
name the server is told and whose contents it is sent, and whether it
can be read from that directory is a question of how the notebook is
reached.  Saying where the modules are does not depend on that.

Written for `python-lsp-server'.  Set to nil if your server takes its
settings from elsewhere and would rather not be told."
  :type 'boolean
  :group 'emjupy-language-server)

;;;; The connection to jupyter-lsp

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
  "Return the `jupyter-lsp' WebSocket URL on SERVER.
The token goes in a header, in the address only for a server refusing
it so (`emjupy--ws-token-param')."
  (let* ((parts (emjupy--server-parts server))
         (scheme (if (equal (plist-get parts :scheme) "https") "wss" "ws")))
    (format "%s://%s:%s%s/lsp/ws/%s%s"
            scheme (plist-get parts :host) (plist-get parts :port)
            (or (plist-get parts :path) "")
            emjupy-lsp-server
            (emjupy--ws-token-param server "?"))))

(defun emjupy--lsp-error-handler (server)
  "Return the error handler of a language server WebSocket to SERVER.
It notes a refusal of the token in a header alone, and reports."
  (lambda (ws type err)
    (emjupy--note-ws-refusal server err)
    (emjupy--lsp-socket-error ws type err)))

(defun emjupy--lsp-handle (session frame)
  "Record the message in FRAME against SESSION.

Frames are plain JSON, one message each -- `jupyter-lsp' has already
stripped the Content-Length framing LSP uses over a pipe."
  (let* ((text (websocket-frame-text frame))
         (msg (condition-case err
                  (json-parse-string text :object-type 'hash-table)
                ;; dropped -- it cannot be answered -- but said
                (json-error
                 (message "[emjupy] The language server sent what is not JSON: %s"
                          (error-message-string err))
                 nil))))
    (when (hash-table-p msg)
      (let ((id (gethash "id" msg)))
        ;; Responses only: requests and notifications from the server --
        ;; diagnostics, progress -- are not answered.
        (when (and id (or (gethash "result" msg) (gethash "error" msg)))
          (let ((callback (and (emjupy-lsp-callbacks session)
                               (gethash id (emjupy-lsp-callbacks session)))))
            (if callback
                (progn
                  (remhash id (emjupy-lsp-callbacks session))
                  (setf (emjupy-lsp-warmed session) t)
                  (funcall callback (and (not (gethash "error" msg))
                                         (gethash "result" msg))))
              (puthash id msg (emjupy-lsp-pending session)))))))))

(defun emjupy--lsp-async (session method params callback)
  "Send METHOD with PARAMS over SESSION; call CALLBACK with the result.
Nothing waits: eldoc runs after every command."
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

(defun emjupy--lsp-live-p (session)
  "Return non-nil if SESSION's socket is open."
  (and session
       (websocket-p (emjupy-lsp-ws session))
       (websocket-openp (emjupy-lsp-ws session))))

;; --- session ----------------------------------------------------------------

(defun emjupy--lsp-document-uri (nb)
  "Return the document URI for NB, on the SERVER's filesystem.
In the kernel's working directory, the notebook's own, so imports
resolve against the code beside it."
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
                 :custom-header-alist (emjupy--websocket-auth-headers server)
                 :on-message (lambda (_ws frame)
                               (emjupy--lsp-handle session frame))
                 :on-close (lambda (_ws) (setf (emjupy-lsp-ready session) nil))
                 :on-error (emjupy--lsp-error-handler server)))
          session)
      ;; Everything: `websocket-open' reports a failed connection as a
      ;; plain error.  What failed is then asked of the server and said.
      (error
       (ignore err)
       (emjupy--lsp-explain-failure server)
       nil))))

;;;; Failures

(defvar emjupy--lsp-last-failure nil
  "Why the WebSocket to the language server could not be opened.

Kept so the warning about falling back can say what went wrong, rather
than leaving the user to find out that anything did.")

(defun emjupy--lsp-socket-error (_ws type err)
  "Report ERR, raised in the language server WebSocket's TYPE callback.
Given as the socket's error handler, which otherwise -- an empty one was
given -- dropped every error unseen.  Recorded too, in
`emjupy--lsp-last-failure', for when the echo area has moved on."
  (let ((what (format "%s: %s" type (error-message-string err))))
    (setq emjupy--lsp-last-failure what)
    (message "[emjupy] Language server connection: %s" what)))

(defvar emjupy--lsp-offered (make-hash-table :test 'equal)
  "Whether each server, by address, has the language server: `yes' or `no'.
That is `emjupy-lsp-server', over `jupyter-lsp'.  Asked once.")

(defun emjupy--server-offers-lsp-p (server)
  "Return non-nil if SERVER has `emjupy-lsp-server', through jupyter-lsp.
It runs beside the kernel, in its environment: the one to use.  One
round trip, the first time; remembered for the server after."
  (let ((key (and server (emjupy-server-base-url server))))
    ;; no server, nothing offered
    (when (and key (eq (gethash key emjupy--lsp-offered 'unknown) 'unknown))
      (let* ((status (ignore-errors
                       (emjupy--http-request "GET" server "/lsp/status")))
             (sessions (and (hash-table-p status) (gethash "sessions" status))))
        (puthash key (if (and (hash-table-p sessions)
                              (gethash emjupy-lsp-server sessions))
                         'yes 'no)
                 emjupy--lsp-offered)))
    (and key (eq (gethash key emjupy--lsp-offered) 'yes))))

(defun emjupy--lsp-explain-failure (server)
  "Say why SERVER has no language server, distinguishing the three causes.
Asked of /lsp/status:

  404  `jupyter-lsp' is not installed
  403  the token was refused
  200  it is there; an empty list means no language server is
       installed -- `jupyterlab-lsp' installs none."
  (let* ((status (condition-case err
                     (emjupy--http-request "GET" server "/lsp/status")
                   ;; a refusal is the answer looked for; a bug is not
                   (emjupy-http-error (error-message-string err))))
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
          (message
           "%s %s"
           (format "[emjupy] jupyter-lsp on %s lists no language server." label)
           "Install one where the kernel runs, e.g. python-lsp-server."))))
     (t
      (message "[emjupy] No language server on %s (%s)." label status)))))

(defvar-local emjupy--lsp-blocked-until nil
  "Time before which no further attempt is made to set up a session.")

;;;; Sessions

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
It needs a server and the kernel's working directory, which both come
as a notebook opens: nil means not yet."
  (unless (and nb (emjupy-notebook-server nb) (emjupy-notebook-kernel-cwd nb))
    (cl-return-from emjupy--lsp-session nil))
  (let ((session (emjupy-notebook-lsp nb)))
    ;; Connecting and handshaking start here and are never waited for: until the
    ;; session is ready this returns nil, and eldoc and completion give no
    ;; answer rather than freeze.
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
      (websocket-close (emjupy-lsp-ws session)))
    (setf (emjupy-notebook-lsp nb) nil)
    (when (called-interactively-p 'interactive)
      (message "[emjupy] Language-server session closed."))))

;; --- positions --------------------------------------------------------------

;;;; Positions and edits

;; --- what the notebook asks for ---------------------------------------------

;;;; The shadow document

(defun emjupy--sanitize-for-lsp (string)
  "Return STRING with undecodable raw bytes replaced by U+FFFD.
A raw byte is not UTF-8, and `jsonrpc' would refuse the document.  One
character for one, so offsets still line up with the cells; only the
copy the server sees is changed."
  (if (not (stringp string))
      string
    (apply #'string
           (mapcar (lambda (ch)
                     (if (and (>= ch #x3FFF80) (<= ch #x3FFFFF))
                         ?\uFFFD
                       ch))
                   (append string nil)))))

(defun emjupy--shadow-cell-marker (id)
  "Return the `# %% [emjupy:ID]' section-header line text for cell ID."
  (format "# %%%% [emjupy:%d]" id))

(defvar emjupy--shadow-content-memo (make-hash-table :test 'eq :weakness 'key)
  "Each notebook's shadow content, with what it was built from.")

(defun emjupy--build-shadow-content (nb)
  "Concatenate every code cell in NB into one Python source, section-marked.
Asked after every command, for completion and eldoc; rebuilt only when
the cells changed.  A source is replaced, never changed in place, so the
same cells holding the same source objects build the same content."
  (let* ((key (mapcar (lambda (c) (list (emjupy-cell-id c) (emjupy-cell-type c)
                                        (emjupy-cell-source c)))
                      (append (emjupy-notebook-cells nb) nil)))
         (memo (gethash nb emjupy--shadow-content-memo)))
    (if (and memo
             (= (length key) (length (car memo)))
             (cl-every (lambda (a b) (and (eql (nth 0 a) (nth 0 b))
                                          (eq (nth 1 a) (nth 1 b))
                                          (eq (nth 2 a) (nth 2 b))))
                       key (car memo)))
        (cdr memo)
      (let ((content (emjupy--build-shadow-content-1 nb)))
        (puthash nb (cons key content) emjupy--shadow-content-memo)
        content))))

(defun emjupy--build-shadow-content-1 (nb)
  "Concatenate every code cell in NB into one Python source, section-marked."
  (mapconcat
   (lambda (cell) (concat (emjupy--shadow-cell-marker (emjupy-cell-id cell))
                          "\n"
                          (emjupy--sanitize-for-lsp (emjupy-cell-source cell))
                          "\n"))
   (cl-remove-if-not (lambda (c) (eq (emjupy-cell-type c) 'code))
                     (append (emjupy-notebook-cells nb) nil))
   "\n"))

(defun emjupy--parse-shadow-sections (text)
  "Return (ID . SOURCE) pairs parsed from TEXT's `# %% [emjupy:ID]' markers."
  (let (sections current-id current-lines)
    (dolist (line (split-string text "\n"))
      (if (string-match "\\`# %% \\[emjupy:\\([0-9]+\\)\\]\\'" line)
          (progn
            (when current-id
              (push (cons current-id
                          (string-trim (mapconcat #'identity
                                                  (nreverse current-lines)
                                                  "\n")))
                    sections))
            (setq current-id (string-to-number (match-string 1 line)))
            (setq current-lines nil))
        (when current-id (push line current-lines))))
    (when current-id
      (push (cons current-id (string-trim (mapconcat #'identity
                                                     (nreverse current-lines)
                                                     "\n")))
            sections))
    (nreverse sections)))

(defun emjupy--goto-shadow-section (buf cell-id)
  "Move point in BUF to the start of CELL-ID's marked section."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (search-forward (emjupy--shadow-cell-marker cell-id) nil t)
        (forward-line 1)
      (goto-char (point-min)))))

(defun emjupy--shadow-section-start (buf cell-id)
  "Return the position where CELL-ID's source begins in shadow buffer BUF."
  (emjupy--goto-shadow-section buf cell-id)
  (with-current-buffer buf (point)))

;;;; Eglot over the WebSocket

;; Eglot's commands reach the server beside the kernel through a jsonrpc
;; connection class of emjupy's, sending over the notebook's WebSocket.
;; `jsonrpc-process-connection' insists on a process, so an idle one is made and
;; ignored.

(defvar emjupy--eglot-pending nil
  "Plist for the next server to be built.
`eglot--connect' decides the initargs, so the WebSocket is left here
for the server class, `emjupy-eglot-server', to collect.")

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
        (emjupy-eglot-server-local-uri server)
        (plist-get emjupy--eglot-pending :local-uri)
        (emjupy-eglot-server-server-uri server)
        (plist-get emjupy--eglot-pending :server-uri)
        (emjupy-eglot-server-local-doc server)
        (plist-get emjupy--eglot-pending :local-doc)
        (emjupy-eglot-server-server-doc server)
        (plist-get emjupy--eglot-pending :server-doc))
  (let ((ws (emjupy-eglot-server-ws server)))
    (when ws
      ;; The socket was opened before the server existed, so its handler is
      ;; pointed at the server now, through the struct: websocket.el exports no
      ;; setf accessor.
      (aset ws (cl-struct-slot-offset 'websocket 'on-message)
            (lambda (_ws frame) (emjupy--eglot-receive server frame))))))

(defun emjupy--eglot-rewrite-uris (object from to)
  "Return OBJECT with every URI equal to FROM replaced by TO.
Eglot names the document by the shadow file here, the server by the
kernel's path; every message passes through here, both ways."
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
      (puthash "method"
               (cond ((keywordp method) (substring (symbol-name method) 1))
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
         (msg (condition-case err
                  (json-parse-string text :object-type 'plist
                                     :null-object nil
                                     :false-object :json-false)
                ;; dropped -- it cannot be answered -- but said
                (json-error
                 (message "[emjupy] The language server sent what is not JSON: %s"
                          (error-message-string err))
                 nil))))
    (when msg
      (jsonrpc-connection-receive
       server
       ;; Only the document itself is renamed on the way back: renaming by
       ;; directory, as outbound, would move every path under the server's root
       ;; beside the shadow.  `emjupy-open-server-file' fetches the others.
       (emjupy--eglot-rewrite-uris
        msg
        (emjupy-eglot-server-server-doc server)
        (emjupy-eglot-server-local-doc server))))))

(cl-defmethod jsonrpc-running-p ((server emjupy-eglot-server))
  "Return non-nil while SERVER's WebSocket is open."
  (let ((ws (emjupy-eglot-server-ws server)))
    (and (websocket-p ws) (websocket-openp ws) t)))

(cl-defmethod jsonrpc-shutdown ((server emjupy-eglot-server) &optional cleanup)
  "Close SERVER's WebSocket and end its placeholder process.

The WebSocket is the connection that matters, but Eglot also keeps a
placeholder process for it -- see `emjupy-eglot-idle-command' -- and the
standard shutdown is what takes the server out of Eglot's table.
Closing only the WebSocket left one such process behind for every
notebook ever opened.  CLEANUP is passed on."
  (let ((ws (emjupy-eglot-server-ws server)))
    (when (and (websocket-p ws) (websocket-openp ws))
      (websocket-close ws)))
  (setf (emjupy-eglot-server-ws server) nil)
  ;; The placeholder process, `cat', is ended here; its sentinel takes the
  ;; server out of Eglot's table.  Not waited for: the standard shutdown waits
  ;; seconds for the server to exit.
  (let ((proc (jsonrpc--process server)))
    (when (process-live-p proc)
      (kill-process proc))
    (when (and cleanup (buffer-live-p (process-buffer proc)))
      (kill-buffer (process-buffer proc)))))

;;;; Workspace configuration

(defun emjupy--set-workspace-configuration (nb server)
  "Tell SERVER where NB's own modules are.
Sent directly: Eglot reads `eglot-workspace-configuration' while
connecting, before the buffer is managed, and sends it empty."
  (when-let* ((cwd (and emjupy-tell-server-where-modules-are
                        server
                        (emjupy-notebook-kernel-cwd nb))))
    (when (jsonrpc-running-p server)
      (jsonrpc-notify
       server :workspace/didChangeConfiguration
       (list :settings
             (list :pylsp
                   (list :plugins
                         (list :jedi
                               (list :extra_paths (vector cwd))))))))))

(defun emjupy--ensure-managed-by (server)
  "Make the current buffer one that SERVER manages, if it is not already.
With `eglot--maybe-activate-editing-mode', what Eglot's own hooks
call."
  (when server
    (unless (eq (eglot-current-server) server)
      (setq-local eglot--cached-server server)
      ;; Activating announces the buffer with didOpen, which goes out over
      ;; this server's connection -- so only while that connection is up.
      (when (jsonrpc-running-p server)
        (eglot--maybe-activate-editing-mode)))
    (eq (eglot-current-server) server)))

(defun emjupy--lsp-open-socket (server)
  "Open a WebSocket to SERVER's language server; return it, or nil.
A failure is explained, and recorded for `emjupy-lsp-diagnose'."
  (condition-case err
      (websocket-open (emjupy--lsp-url server)
                      :custom-header-alist
                      (emjupy--websocket-auth-headers server)
                      ;; replaced by the Eglot connection made from it,
                      ;; which takes the socket over
                      :on-message (lambda (_ws _frame) nil)
                      :on-error (emjupy--lsp-error-handler server))
    ;; Everything: `websocket-open' reports a failed connection as a plain
    ;; error.
    (error (emjupy--lsp-explain-failure server)
           (setq emjupy--lsp-last-failure (error-message-string err))
           nil)))

(defun emjupy--eglot-pending-for (nb ws local-file)
  "Return what the server class collects, for NB's WS and LOCAL-FILE.
LOCAL-FILE is the shadow file.  Directory prefixes, not the one file:
the same substitution then fixes rootUri, the document URI and anything
else naming a path under them."
  (let ((server-dir (concat "file://"
                            (file-name-as-directory
                             (or (emjupy-notebook-kernel-cwd nb) "/")))))
    (list :ws ws
          :local-uri (concat "file://" (file-name-directory local-file))
          :server-uri server-dir
          :local-doc (concat "file://" local-file)
          :server-doc (concat server-dir (file-name-nondirectory local-file)))))

(defun emjupy--eglot-connect (nb buffer)
  "Attach Eglot to BUFFER, talking to NB's server over its WebSocket.
BUFFER is the shadow buffer, on this machine, only for Eglot to
manage.  Return the server, or nil."
  (let* ((local-file (buffer-local-value 'buffer-file-name buffer))
         (ws (emjupy--lsp-open-socket (emjupy-notebook-server nb))))
    (when (and ws local-file)
      (let* ((emjupy--eglot-pending
              (emjupy--eglot-pending-for nb ws local-file))
             ;; The project must contain the shadow buffer, or Eglot will not
             ;; manage it; the server's own root is fixed on the wire.
             (project (cons 'transient (file-name-directory local-file))))
        (with-current-buffer buffer
          (condition-case err
              (let ((connected
                     (eglot--connect (list major-mode) project
                                     'emjupy-eglot-server
                                     ;; A process must exist for the base
                                     ;; class; this one is never written to or
                                     ;; read from.
                                     (list emjupy-eglot-idle-command)
                                     "python")))
                (emjupy--set-workspace-configuration nb connected)
                ;; Make sure the server manages this buffer: Eglot does it
                ;; through project hooks, which on Emacs 30 may miss a shadow
                ;; buffer in a transient project.
                (emjupy--ensure-managed-by connected)
                connected)
            ;; Everything: the socket is opened and Eglot started here, and
            ;; either fails with plain errors; the socket is closed whatever.
            (error
             (when (websocket-openp ws) (websocket-close ws))
             ;; Recorded, not only said: an echo-area message at startup is gone
             ;; by the time anyone asks why a local server took over.
             (setq emjupy--lsp-last-failure
                   (format "Eglot would not attach: %s"
                           (error-message-string err)))
             (message "[emjupy] Could not attach Eglot over the WebSocket: %s"
                      (error-message-string err))
             nil)))))))

(provide 'emjupy-lsp)

;;; emjupy-lsp.el ends here
