;;; emjupy-core.el --- Shared state and data structures for emjupy  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; The data model every other emjupy file builds on: the customization
;; group, the server/kernel/notebook/cell structs, the registry of known
;; servers, and the small lookups that answer "which server/notebook/kernel
;; does this buffer belong to?".
;;
;; Nothing about a connection is global: a server owns its token and XSRF
;; cookie, a notebook owns its kernel, a kernel owns its WebSocket.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup emjupy nil
  "Minimal Jupyter Notebook editor."
  :group 'tools)

;; --- State model -----------------------------------------------------------
;; emjupy supports several notebooks open at once, from several servers at
;; once (e.g. two ssh tunnels on different local ports). Nothing about a
;; connection is global: a server owns its own token and XSRF cookie, a
;; notebook owns its own kernel, and a kernel owns its own WebSocket and its
;; own table of in-flight requests. The only globals left are the registry of
;; known servers and a "most recently used" default for commands invoked from
;; outside a notebook buffer.

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

(cl-defstruct emjupy-server
  host port token base-url
  ;; Per-server, not global: two servers issue different XSRF cookies, and
  ;; replaying one server's cookie at another gets a 403.
  xsrf
  ;; The kernel bound to this port. One port = one kernel: notebooks opened
  ;; from this server attach to it, so selecting a notebook drops you into
  ;; the REPL already running behind that tunnel. C-c C-z overrides it for
  ;; an individual notebook.
  kernel-id)

(cl-defstruct emjupy-kernel
  id name server ws pending
  ;; Backlink to the notebook this kernel drives, so an incoming WebSocket
  ;; frame can be routed to the right buffer without consulting any global.
  notebook)

(defvar emjupy--next-cell-id 0
  "Monotonically increasing counter for assigning stable `emjupy-cell' ids.")

(defun emjupy--new-cell-id ()
  "Return a fresh, never-reused cell id."
  (setq emjupy--next-cell-id (1+ emjupy--next-cell-id)))

(cl-defstruct emjupy-cell
  id type exec-count source outputs metadata overlay output-ov nb-id)

(cl-defstruct emjupy-notebook
  path server kernel cells metadata buffer shadow-buffer)

;; --- Lookups ---------------------------------------------------------------

(defun emjupy--intern-server (base-url token)
  "Return the registered `emjupy-server' for BASE-URL, creating it if new.
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
  (or (and emjupy--buffer-notebook (emjupy-notebook-server emjupy--buffer-notebook))
      emjupy--current-server
      (user-error "Not logged in! Call `emjupy-login' first")))

(defun emjupy--notebook ()
  "Return this buffer's notebook, or signal a clear error."
  (or emjupy--buffer-notebook
      (user-error "No emjupy notebook associated with this buffer")))

(defun emjupy--kernel ()
  "Return the `emjupy-kernel' driving this buffer's notebook, or nil."
  (and emjupy--buffer-notebook (emjupy-notebook-kernel emjupy--buffer-notebook)))

(defun emjupy--notebook-buffers ()
  "Return the list of live emjupy notebook buffers."
  (cl-remove-if-not
   (lambda (b) (buffer-local-value 'emjupy--buffer-notebook b))
   (buffer-list)))

(defun emjupy--server-label (server)
  "Return a short human label for SERVER."
  (emjupy-server-base-url server))

(provide 'emjupy-core)
;;; emjupy-core.el ends here
