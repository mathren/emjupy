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

(cl-defstruct emjupy-server
  host port token base-url
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
  id name server ws pending
  ;; Backlink to the notebook this kernel drives, so an incoming WebSocket
  ;; frame can be routed to the right buffer without consulting any global.
  notebook
  ;; Non-nil once emjupy closes the socket itself: its closing is then no
  ;; drop to recover from.
  closing)

(defvar emjupy--next-cell-id 0
  "Monotonically increasing counter for assigning stable `emjupy-cell' ids.")

(defun emjupy--new-cell-id ()
  "Return a fresh, never-reused cell id."
  (setq emjupy--next-cell-id (1+ emjupy--next-cell-id)))

(cl-defstruct emjupy-cell
  id type exec-count source outputs metadata overlay output-ov nb-id
  ;; Files pasted into a markdown or raw cell, by name, as nbformat keeps
  ;; them.  Carried untouched: emjupy does not show them, but a notebook
  ;; saved without them has lost the images its markdown refers to.
  attachments)

(cl-defstruct emjupy-notebook
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
