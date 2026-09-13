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

(declare-function websocket-open "websocket")
(declare-function websocket-send-text "websocket")
(declare-function websocket-openp "websocket")
(declare-function websocket-close "websocket")
(declare-function websocket-frame-text "websocket")
(declare-function emjupy--build-shadow-content "emjupy-eglot" (nb))
(declare-function emjupy--shadow-section-start "emjupy-eglot" (buf id))
(declare-function emjupy--shadow-cell-marker "emjupy-eglot" (id))
(declare-function emjupy--cell-at-point "emjupy-cells" (&optional pos))

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

(defun emjupy--lsp-ask-async (method callback)
  "Ask the language server METHOD about point; call CALLBACK with the result."
  (when-let* ((nb (and (bound-and-true-p emjupy--buffer-notebook)
                       emjupy--buffer-notebook))
              (position (emjupy--lsp-position nb))
              (session (emjupy--lsp-session nb)))
    (emjupy--lsp-async session method
                       (emjupy--lsp-text-document-params session position)
                       callback)))

(defun emjupy--lsp-ask (method)
  "Ask the language server METHOD about point, and return the result."
  (when-let* ((nb (and (bound-and-true-p emjupy--buffer-notebook)
                       emjupy--buffer-notebook))
              (position (emjupy--lsp-position nb))
              (session (emjupy--lsp-session nb)))
    ;; Only a server that has already answered something is worth waiting
    ;; for, and then only briefly.  Before that, return nothing rather than
    ;; hold the editor while a language server starts up.
    (when (emjupy-lsp-warmed session)
      (emjupy--lsp-request session method
                           (emjupy--lsp-text-document-params session position)
                           emjupy-lsp-timeout))))

;; --- what the notebook asks for ---------------------------------------------

(defvar-local emjupy--lsp-completion-cache nil
  "Last completion answer, as (KEY . CANDIDATES).")

(defun emjupy-lsp-completion-at-point ()
  "`completion-at-point-functions\=' entry backed by the Jupyter server.

Answers from the last reply and asks for the next one in the
background.  Nothing waits: with an eager completion UI this runs on
every keystroke, so a wait here is a wait on every keystroke -- and the
answer for the position one character back is worth more than a frozen
editor."
  (when emjupy-lsp-enabled
    (let* ((key (list (point) (buffer-chars-modified-tick)))
           (cached (and emjupy--lsp-completion-cache
                        (equal (car emjupy--lsp-completion-cache) key)
                        (cdr emjupy--lsp-completion-cache)))
           (buffer (current-buffer)))
      (unless cached
        (emjupy--lsp-ask-async
         "textDocument/completion"
         (lambda (result)
           (when (buffer-live-p buffer)
             (with-current-buffer buffer
               (let* ((items (if (hash-table-p result)
                                 (gethash "items" result)
                               result))
                      (cands (delq nil (mapcar (lambda (it)
                                                 (and (hash-table-p it)
                                                      (gethash "label" it)))
                                               (append items nil)))))
                 (setq emjupy--lsp-completion-cache (cons key cands))))))))
      (when cached
        (let ((bounds (bounds-of-thing-at-point 'symbol)))
          (list (or (car bounds) (point)) (or (cdr bounds) (point))
                cached :exclusive 'no))))))

(defun emjupy-lsp-eldoc (callback &rest _)
  "Report hover documentation for point to CALLBACK.
An `eldoc-documentation-functions\=' entry, backed by the language server
the Jupyter host is running."
  (when emjupy-lsp-enabled
    (emjupy--lsp-ask-async
     "textDocument/hover"
     (lambda (result)
       (when result
        (let* ((contents (gethash "contents" result))
               (text (cond
                      ((stringp contents) contents)
                      ((hash-table-p contents) (gethash "value" contents))
                      ((vectorp contents)
                       (mapconcat (lambda (c) (if (hash-table-p c)
                                                  (or (gethash "value" c) "")
                                                (format "%s" c)))
                                  (append contents nil) "\n")))))
          (when (and text (not (string-empty-p (string-trim text))))
            (funcall callback (string-trim text)))))))
    ;; Answering later is allowed: this is what the callback is for.
    t))

(defun emjupy--lsp-definitions ()
  "Return definition locations for point as a list of (FILE LINE COL)."
  (let ((result (emjupy--lsp-ask "textDocument/definition")))
    (when result
      (let ((locs (if (vectorp result) (append result nil) (list result))))
        (delq nil
              (mapcar
               (lambda (loc)
                 (when (hash-table-p loc)
                   (let* ((uri (or (gethash "uri" loc) (gethash "targetUri" loc)))
                          (range (or (gethash "range" loc) (gethash "targetRange" loc)))
                          (start (and range (gethash "start" range))))
                     (when (and uri start)
                       (list uri (gethash "line" start) (gethash "character" start))))))
               locs))))))

(provide 'emjupy-lsp)
;;; emjupy-lsp.el ends here
