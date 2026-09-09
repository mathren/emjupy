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

(defcustom emjupy-lsp-timeout 1.0
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
          (puthash id msg (emjupy-lsp-pending session)))))))

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
       (message "[emjupy] Could not reach the language server on %s: %s"
                (emjupy--server-label server) (error-message-string err))
       nil))))

(defun emjupy--lsp-initialize (nb session)
  "Run the LSP handshake for NB on SESSION.  Return non-nil on success."
  (let* ((root (concat "file://" (or (emjupy-notebook-kernel-cwd nb) "/")))
         (caps (make-hash-table :test 'equal))
         (params (make-hash-table :test 'equal)))
    (puthash "processId" :null params)
    (puthash "rootUri" root params)
    (puthash "capabilities" caps params)
    (puthash "workspaceFolders" :null params)
    (when (emjupy--lsp-request session "initialize" params 10)
      (emjupy--lsp-send session "initialized" (make-hash-table :test 'equal))
      (setf (emjupy-lsp-ready session) t))))

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
    (unless (emjupy--lsp-live-p session)
      (setq session (emjupy--lsp-connect nb))
      (setf (emjupy-notebook-lsp nb) session))
    (when session
      (unless (emjupy-lsp-ready session)
        (emjupy--lsp-initialize nb session))
      (when (emjupy-lsp-ready session)
        (emjupy--lsp-sync nb session)
        session))))

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

(defun emjupy--lsp-ask (method)
  "Ask the language server METHOD about point, and return the result."
  (when-let* ((nb (and (bound-and-true-p emjupy--buffer-notebook)
                       emjupy--buffer-notebook))
              (position (emjupy--lsp-position nb))
              (session (emjupy--lsp-session nb)))
    (let ((result (emjupy--lsp-request
                   session method
                   (emjupy--lsp-text-document-params session position)
                   (if (emjupy-lsp-warmed session)
                       emjupy-lsp-timeout
                     emjupy-lsp-first-timeout))))
      (when result (setf (emjupy-lsp-warmed session) t))
      result)))

;; --- what the notebook asks for ---------------------------------------------

(defun emjupy-lsp-completion-at-point ()
  "`completion-at-point-functions\=' entry backed by the Jupyter server."
  (when emjupy-lsp-enabled
    (let ((result (emjupy--lsp-ask "textDocument/completion")))
      (when result
        (let* ((items (if (hash-table-p result) (gethash "items" result) result))
               (cands (mapcar (lambda (it) (gethash "label" it)) (append items nil)))
               (bounds (bounds-of-thing-at-point 'symbol)))
          (when cands
            (list (or (car bounds) (point)) (or (cdr bounds) (point))
                  cands :exclusive 'no)))))))

(defun emjupy-lsp-eldoc (callback &rest _)
  "Report hover documentation for point to CALLBACK.
An `eldoc-documentation-functions\=' entry, backed by the language server
the Jupyter host is running."
  (when emjupy-lsp-enabled
    (let ((result (emjupy--lsp-ask "textDocument/hover")))
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
            (funcall callback (string-trim text))
            t))))))

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
