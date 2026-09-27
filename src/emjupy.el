;;; emjupy.el --- Interactive Jupyter notebooks over HTTP and WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Maintainer: Mathieu Renzo <mrenzo@arizona.edu>
;; Version: 0.1.2
;; Package-Requires: ((emacs "30.1") (websocket "1.15"))
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; emjupy edits Jupyter notebooks in Emacs by talking to a running Jupyter
;; server over the same HTTP + WebSocket API the browser uses.  Because that
;; API is plain HTTP on one port, everything works unchanged through an ssh
;; tunnel -- no ZMQ ports to forward.
;;
;; Quick start:
;;
;;   M-x emjupy-login RET 8888 RET
;;
;; The port is the whole prompt; a token is only asked for when the server
;; needs one.  emjupy adopts the kernel already running behind that port, so
;; picking a notebook drops you straight into the live REPL.
;;
;; One port = one kernel: log in once per ssh tunnel and each port keeps its
;; own kernel, so several remote sessions stay live in one Emacs.
;;
;; This file carries the package header and version, and loads the rest.
;; The mode, its keymap and menu are in emjupy-mode; the implementation is
;; layered, each file requiring only the ones before it: emjupy-core,
;; emjupy-http, emjupy-render, emjupy-cells, emjupy-kernel, emjupy-remote,
;; emjupy-lsp, emjupy-eglot, emjupy-mode and emjupy-notebook.

;;; Code:

(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-notebook)
(require 'emjupy-lsp)
(require 'emjupy-eglot)
(require 'emjupy-mode)
(require 'emjupy-remote)

(defconst emjupy-version "0.1.2"
  "Version of emjupy, kept in step with the Version: header above.")

(defun emjupy--source-directory ()
  "Return the directory emjupy was loaded from, or nil."
  (let ((file (or (locate-library "emjupy-cells") (locate-library "emjupy"))))
    (and file (file-name-directory file))))

(defun emjupy--git-revision (dir)
  "Return a short description of the git revision in DIR, or nil."
  (when (and dir (file-directory-p (expand-file-name ".git" dir))
             (executable-find "git"))
    (let* ((default-directory dir)
           (rev (string-trim (shell-command-to-string
                              "git rev-parse --short HEAD 2>/dev/null")))
           (date (string-trim (shell-command-to-string
                               "git log -1 --format=%cs 2>/dev/null")))
           (dirty (not (string-empty-p
                        (string-trim (shell-command-to-string
                                      "git status --porcelain 2>/dev/null"))))))
      (unless (string-empty-p rev)
        (format "%s (%s)%s" rev date (if dirty ", with local changes" ""))))))

;;;###autoload
(defun emjupy-version (&optional insert)
  "Report which emjupy is running, and from where.

Says the version, the directory the code was loaded from and, when that
is a git checkout, the revision -- so \"am I running the fix?\" has an
answer that does not depend on remembering how it was installed.

In a notebook buffer it also says where the shadow file is, which is
what decides whether the language server can see your own modules.

With a prefix argument, INSERT, put the report in the buffer instead of
the echo area, for pasting into a bug report."
  (interactive "P")
  (let* ((dir (emjupy--source-directory))
         (rev (emjupy--git-revision dir))
         (nb (and (derived-mode-p 'emjupy-mode) emjupy--buffer-notebook))
         (shadow (and nb (emjupy-notebook-shadow-buffer nb)))
         (shadow-file (and (buffer-live-p shadow)
                           (buffer-local-value 'buffer-file-name shadow)))
         (report (concat
                  (format "emjupy %s" emjupy-version)
                  (if rev (format ", git %s" rev) "")
                  (if dir (format ", loaded from %s" dir) "")
                  (if nb
                      (format "; kernel cwd %s; shadow %s"
                              (or (emjupy-notebook-kernel-cwd nb) "unknown")
                              (or shadow-file "not created yet"))
                    ""))))
    (if insert (insert report) (message "%s" report))
    report))

(provide 'emjupy)
;;; emjupy.el ends here
