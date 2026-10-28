;;; emjupy-core.el --- Shared state and data structures for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The data model every other emjupy file builds on: the customization group,
;; the server, kernel, notebook and cell structs, the registry of servers, and
;; the lookups that say which a buffer belongs to.
;;
;; Nothing about a connection is global: a server owns its token and XSRF
;; cookie, a notebook its kernel, a kernel its WebSocket.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'iso8601)

(defgroup emjupy nil
  "Minimal Jupyter Notebook editor."
  :group 'tools)

(defgroup emjupy-appearance nil
  "How notebooks look: output, outlines, LaTeX and figures in the buffer."
  :group 'emjupy)

(defgroup emjupy-editing nil
  "Editing cells, and exporting them."
  :group 'emjupy)

(defgroup emjupy-kernels nil
  "Servers, kernels, and output as it arrives."
  :group 'emjupy)

(defgroup emjupy-figures nil
  "Interactive figures and widgets, opened outside the buffer."
  :group 'emjupy)

(defgroup emjupy-language-server nil
  "Completion, documentation and definitions, from a language server."
  :group 'emjupy)

(defgroup emjupy-remote nil
  "Where a server on another machine keeps its files."
  :group 'emjupy)

(defgroup emjupy-saving nil
  "Saving to the server, and keeping copies of unsaved edits."
  :group 'emjupy)

(defun emjupy--colour-option-safe-p (value)
  "Return non-nil if VALUE is a harmless colour: auto, nil, or a name."
  (or (memq value '(auto nil)) (stringp value)))

(defun emjupy--box-width-safe-p (value)
  "Return non-nil if VALUE is a harmless `emjupy-box-width': window, a count."
  (or (eq value 'window) (natnump value)))

;; Several notebooks, from several servers, may be open at once.  The only
;; globals are the registry of servers and the most recently used one, a default
;; for commands run outside a notebook.

;;;; Options

(define-obsolete-variable-alias 'emjupy-report-slow-hooks
  'emjupy-slow-hook-threshold "0.2")

(defcustom emjupy-slow-hook-threshold nil
  "Seconds after which emjupy's work after a command is reported, or nil.
When a number, any per-command work taking longer is logged with the
name of the function responsible; 0.05 is a good starting point.  A hang
is hard to attribute from the outside: this says which of emjupy's hooks
is the slow one, or -- just as usefully -- that none of them is, and the
time is going somewhere else entirely."
  :type '(choice (const :tag "Off" nil) (number :tag "Seconds"))
  :group 'emjupy)

;;;; Shared state

(defvar emjupy--servers (make-hash-table :test 'equal)
  "Registry of known servers, keyed by base-url string.
Lets several Jupyter servers -- typically several ssh tunnels on
different local ports -- be live in one Emacs session.")

(defvar emjupy--current-server nil
  "Default `emjupy-server' for commands run outside a notebook buffer.
Inside a notebook buffer the notebook's OWN server is always used
instead; this is only the fallback for things like `emjupy-login'.")

(defvar-local emjupy--buffer-notebook nil
  "The `emjupy-notebook' struct associated with the current buffer.")

(defvar emjupy-probe-environment t
  "When non-nil, emjupy may look at the machine it is running on.

Two things depend on it: finding the host of an SSH tunnel by reading
the process list, and deciding whether LaTeX can be rendered by looking
for the programs that do it.  Both are right in use and wrong in a test,
where they make the result depend on what the machine happens to have --
a test asking what a tunnelled server resolves to found the tunnels of
whichever machine ran it, and passed or failed accordingly.

The unit tests bind this to nil, so a test that depends on the machine
gets a definite answer rather than a local one.")

;;;; Structures

(defun emjupy--parse-base-url (base-url)
  "Return a plist of the parts of BASE-URL, as a server is reached.
Keys are :scheme, :ws-scheme, :host, :port and :path.  Accepts a bare
port (\"8888\"), host:port, or an http(s) URL, with an optional base
path that the WebSocket URL keeps too, as for a JupyterHub server at
localhost:8888/user/alice."
  (let* ((raw base-url)
         (secure (string-match-p "\\`https://" raw))
         (stripped (replace-regexp-in-string "\\`https?://" "" raw))
         (slash (string-match-p "/" stripped))
         (hostport (if slash (substring stripped 0 slash) stripped))
         (path (if slash
                   (string-trim-right (substring stripped slash) "/+")
                 ""))
         (parts (split-string hostport ":"))
         (host (if (string-empty-p (or (car parts) "")) "localhost"
                 (car parts)))
         (port (or (cadr parts) (if secure "443" "80"))))
    (list :scheme (if secure "https" "http")
          :ws-scheme (if secure "wss" "ws")
          :host host :port port :path path)))
(cl-defstruct (emjupy-server
               (:constructor make-emjupy-server
                (&key base-url token host port root xsrf kernel-id
                      files-root
                      &aux (parts (and base-url
                                       (emjupy--parse-base-url base-url)))
                      (scheme (plist-get parts :scheme))
                      (host (or host (plist-get parts :host)))
                      (port (or port (plist-get parts :port)))
                      (path (plist-get parts :path)))))
  "A Jupyter server emjupy has logged in to, one per address.
BASE-URL is the address given to `emjupy-login', host:port or a URL;
TOKEN the token sent with every request.  The address is parsed once,
when the server is made, into SCHEME, HOST, PORT and PATH: \"http\" or
\"https\", the host, the port as a string, and a base path such as
/user/alice, or \"\"."
  host port token base-url scheme path
  ;; Absolute path of the directory this server serves, worked out from a
  ;; kernel rather than configured.  nil until a kernel has reported in.
  root
  ;; Per-server, not global: two servers issue different XSRF cookies, and
  ;; replaying one server's cookie at another gets a 403.
  xsrf
  ;; The kernel bound to this port.  One port, one kernel: notebooks opened from
  ;; this server attach to it.  C-c M-z overrides it for one notebook.
  kernel-id
  ;; Where the files it serves are, as a file name Emacs can open -- a
  ;; TRAMP name, hops and all -- when emjupy knows rather than infers it:
  ;; for a server it started, or found, through a notebook opened by TRAMP.
  files-root)

(cl-defstruct emjupy-kernel
  "A kernel on a server, and emjupy's connection to it.
ID is the kernel's id on SERVER; NAME its kernelspec, such as python3.
WS is the WebSocket, PENDING a table from each request's msg_id to the
cell it runs, until its reply and its closing idle have both come."
  id name server ws pending
  ;; Backlink to the notebook this kernel drives, so an incoming WebSocket
  ;; frame can be routed to the right buffer without consulting any global.
  notebook
  ;; Non-nil once emjupy closes the socket itself: its closing is then no
  ;; drop to recover from.
  closing
  ;; When a message last came from it, as `float-time': a kernel quiet too
  ;; long while running a cell is asked after on the server.
  heard
  ;; What it last said it is doing -- \"idle\", \"busy\", \"starting\":
  ;; the mode line shows it.
  state)

(defvar emjupy--next-cell-id 0
  "Monotonically increasing counter for assigning stable `emjupy-cell' ids.")

(defun emjupy--new-cell-id ()
  "Return a fresh, never-reused cell id."
  (setq emjupy--next-cell-id (1+ emjupy--next-cell-id)))

(cl-defstruct emjupy-cell
  "One cell of a notebook: what is saved, and how it is drawn.
ID is emjupy's own, a number unique in this Emacs session, never saved;
NB-ID the cell id nbformat saves, read from the file or made when a
notebook has none.  TYPE is \"code\", \"markdown\" or \"raw\";
EXEC-COUNT the number shown as In: [N], or nil; SOURCE the text;
OUTPUTS a vector of nbformat outputs; METADATA the cell's metadata.
OVERLAY spans the cell's source in the buffer, OUTPUT-OV its output box;
both nil until the cell is drawn."
  id type exec-count source outputs metadata overlay output-ov nb-id
  ;; Files pasted into a markdown or raw cell, by name, as nbformat keeps
  ;; them.  Carried untouched: emjupy does not show them, but a notebook
  ;; saved without them has lost the images its markdown refers to.
  attachments
  ;; Non-nil when its output is hidden for its size, on opening: for
  ;; drawing only, not in the metadata, so the file is unchanged.
  size-hidden)

(cl-defstruct emjupy-notebook
  "A notebook open in a buffer.
PATH is its path on SERVER, as the Contents API names it; KERNEL the
`emjupy-kernel' running its code; CELLS a list of `emjupy-cell's, in
order; METADATA the notebook's metadata.  BUFFER shows it, and
SHADOW-BUFFER holds its code for the language server."
  path server kernel cells metadata buffer shadow-buffer
  ;; Live LSP session against the server `jupyter-lsp' runs, if any.
  lsp
  ;; Where the kernel says it is running.  Absolute, on the kernel's own
  ;; filesystem, so only meaningful here when the kernel is local or
  ;; `emjupy-shadow-host' says how to reach it.
  kernel-cwd
  ;; The server copy's `last_modified' when it was opened or last saved:
  ;; what a save checks, so as not to overwrite changes made elsewhere.
  last-modified)

;;;; Trust, as Jupyter decides it

(defun emjupy--cell-trusted-p (cell)
  "Return non-nil if Jupyter trusts CELL's outputs to run their scripts.
The server marks each code cell of a notebook it hands over, checking the
notebook's signature: one saved by someone else, or edited outside
Jupyter, is not trusted.  A cell with no mark was made here, and one run
here holds the user's own output: both are trusted."
  (let ((metadata (emjupy-cell-metadata cell)))
    (not (and (hash-table-p metadata)
              (eq (gethash "trusted" metadata) :false)))))

(defun emjupy--mark-cell-trusted (cell)
  "Mark CELL trusted: run here, its outputs are the user's own.
Saved with every cell so marked, the server signs the notebook."
  (unless (hash-table-p (emjupy-cell-metadata cell))
    (setf (emjupy-cell-metadata cell) (make-hash-table :test 'equal)))
  (puthash "trusted" t (emjupy-cell-metadata cell)))

;;;; Execution time, as JupyterLab records it

(defun emjupy--cell-execution-seconds (cell)
  "Return how long CELL last ran, in seconds, from its metadata, or nil.
JupyterLab, recording timing, keeps the times under \"execution\":
from \"iopub.execute_input\" to \"shell.execute_reply\"."
  (let* ((meta (emjupy-cell-metadata cell))
         (times (and (hash-table-p meta) (gethash "execution" meta)))
         (start (and (hash-table-p times)
                     (gethash "iopub.execute_input" times)))
         (end (and (hash-table-p times) (gethash "shell.execute_reply" times))))
    (when (and (stringp start) (stringp end))
      (ignore-errors
        (let ((seconds (- (float-time (encode-time (iso8601-parse end t)))
                          (float-time (encode-time (iso8601-parse start t))))))
          (and (>= seconds 0) seconds))))))

(defun emjupy--format-duration (seconds)
  "Return SECONDS as a short duration: 85 ms, 1.2 s, 3m 05s, 1h 02m."
  (cond ((< seconds 1) (format "%d ms" (round (* 1000 seconds))))
        ((< seconds 60) (format "%.1f s" seconds))
        ((< seconds 3600) (format "%dm %02ds" (floor seconds 60)
                                  (mod (floor seconds) 60)))
        (t (format "%dh %02dm" (floor seconds 3600)
                   (mod (floor seconds 60) 60)))))

;;;; Lookups

(defun emjupy--intern-server (base-url token)
  "Return the registered `emjupy-server' for BASE-URL, creating it if new.
TOKEN is stored on the server and replaces any token already held.
Re-logging into a server already open updates its token and keeps the
same object, so notebooks already pointing at it stay valid."
  (let ((server (gethash base-url emjupy--servers)))
    (if server
        (progn (setf (emjupy-server-token server) token) server)
      (setq server (make-emjupy-server :base-url base-url :token token))
      (puthash base-url server emjupy--servers)
      server)))

(defun emjupy--server ()
  "Return the server this buffer's notebook belongs to, else the default."
  (or (and emjupy--buffer-notebook
           (emjupy-notebook-server emjupy--buffer-notebook))
      emjupy--current-server
      (user-error "Not logged in! Call `emjupy-login' first")))

(defun emjupy--notebook ()
  "Return this buffer's notebook, or signal a clear error."
  (or emjupy--buffer-notebook
      (user-error "No emjupy notebook associated with this buffer")))

(defun emjupy--kernel ()
  "Return the `emjupy-kernel' driving this buffer's notebook, or nil."
  (and emjupy--buffer-notebook
       (emjupy-notebook-kernel emjupy--buffer-notebook)))

(defun emjupy--notebook-buffers ()
  "Return the list of live emjupy notebook buffers."
  (cl-remove-if-not
   (lambda (b) (buffer-local-value 'emjupy--buffer-notebook b))
   (buffer-list)))

(defun emjupy--server-label (server)
  "Return a short human label for SERVER."
  (emjupy-server-base-url server))

(defvar emjupy--in-cell-step nil
  "Non-nil while a command recorded by `emjupy--as-one-undo-step' runs.
Its redraws record nothing of their own: the command records one step,
from a snapshot taken before it changed anything.")

;;;; A folder of one's own

(defun emjupy--private-directory (dir purpose)
  "Make DIR, readable by the user alone, unless it exists; then check it.
Signal a `file-error' unless DIR is a directory of the user's own,
not a symbolic link: in a shared /tmp another user could have made it,
or left a link there.  PURPOSE says what is written in it, for the
message."
  (let ((dir (directory-file-name dir)))
    (unless (file-exists-p dir)
      (with-file-modes #o700 (make-directory dir t)))
    (let ((attributes (file-attributes dir)))
      (when (or (file-symlink-p dir)
                (not (eq (file-attribute-type attributes) t))
                (not (eql (file-attribute-user-id attributes) (user-uid))))
        (signal 'file-error
                (list (format "Not writing %s" purpose)
                      "not a directory of your own" dir))))))

;;;; What this Emacs can display

(defun emjupy--image-displayable-p (type)
  "Return non-nil if this Emacs can actually display an image of TYPE.
Both halves matter: a tty frame can't show images at all, and a build
without the relevant library (very common for `emacs-nox') will make
`create-image' signal rather than degrade."
  (and (display-graphic-p)
       (image-type-available-p type)))

;;;; Timing a hook

(defun emjupy--timed (name fn &rest args)
  "Call FN with ARGS, reporting if it takes longer than allowed.
NAME is what gets reported."
  (if (not (numberp emjupy-slow-hook-threshold))
      (apply fn args)
    (let* ((start (float-time))
           (value (apply fn args))
           (elapsed (- (float-time) start)))
      (when (> elapsed emjupy-slow-hook-threshold)
        (message "[emjupy] %s took %.0f ms" name (* 1000 elapsed)))
      value)))

;;;; The cell at a position

;; Lookups over the cells and their overlays, nothing more: every layer
;; asks "which cell is this?", and each required the cells layer for it.

(defun emjupy--cell-at-point (&optional pos)
  "Return the cell containing POS (default point), or nil.
From the overlays, which move with insertions, not the `emjupy-cell'
property, which text typed at a cell's end does not carry."
  (let ((pos (or pos (point)))
        (found nil))
    (when emjupy--buffer-notebook
      (cl-loop for cell across
               (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               until found
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (and (overlayp ov)
                               (>= pos (overlay-start ov))
                               (<= pos (overlay-end ov)))
                      (setq found cell)))))
    (or found
        (get-text-property pos 'emjupy-cell)
        ;; Past the last cell -- point-max, or a gutter line -- no overlay
        ;; covers the position: the nearest cell, so a new cell goes below the
        ;; one in view, not at the end.
        (emjupy--nearest-cell pos))))

(defun emjupy--nearest-cell (pos)
  "Return the cell whose overlay is closest to POS, or nil."
  (when emjupy--buffer-notebook
    (let ((best nil) (best-d most-positive-fixnum))
      (cl-loop for cell across
               (or (emjupy-notebook-cells emjupy--buffer-notebook) [])
               do (let ((ov (emjupy-cell-overlay cell)))
                    (when (overlayp ov)
                      (let ((d (cond ((< pos (overlay-start ov))
                                      (- (overlay-start ov) pos))
                                     ((> pos (overlay-end ov))
                                      (- pos (overlay-end ov)))
                                     (t 0))))
                        (when (< d best-d) (setq best-d d best cell))))))
      best)))

(defun emjupy--cell-at-point-including-output (&optional pos)
  "Return the cell owning POS, counting its output box as part of it.
For commands about output: in an output box, the cell meant is the one
above.  `emjupy--cell-at-point' answers for the source only."
  (let ((pos (or pos (point))))
    ;; The output box is asked about first: as far as the source lookup goes, a
    ;; position in one lies before the cell below, and would be answered with
    ;; it.
    (or (when emjupy--buffer-notebook
          (cl-find-if
           (lambda (cell)
             (let ((out (emjupy-cell-output-ov cell)))
               (and (overlayp out)
                    (eq (overlay-buffer out) (current-buffer))
                    (>= pos (overlay-start out))
                    (<= pos (overlay-end out)))))
           (append (emjupy-notebook-cells emjupy--buffer-notebook) nil)))
        (emjupy--cell-at-point pos))))

(provide 'emjupy-core)
;;; emjupy-core.el ends here
