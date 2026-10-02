;;; emjupy.el --- Interactive Jupyter notebooks over HTTP and WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Maintainer: Mathieu Renzo <mrenzo@arizona.edu>
;; Version: 0.1.3
;; Package-Requires: ((emacs "30.1") (websocket "1.15"))
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; emjupy edits and runs Jupyter notebooks in Emacs.  It talks to a
;; running Jupyter server over the same HTTP and WebSocket API the
;; browser uses, so a kernel on another machine works through an ssh
;; tunnel, and the notebook stays a standard .ipynb that any other
;; Jupyter tool opens.  Completion, documentation and `M-.' come through
;; Eglot, from a language server running beside the kernel.
;;
;; Remote kernels: log in to a server by its port -- the local end of a
;; tunnel -- and emjupy adopts the kernel already running there, so
;; opening a notebook drops you into that live session.  Each port keeps
;; its own kernel, so several remote sessions can be open in one Emacs,
;; and after a dropped connection `C-c C-x C-c' reconnects to the same
;; kernel, with its state intact.
;;
;; Language server: through the jupyter-lsp extension on the Jupyter
;; server, over the same connection.  It sees the environment the code
;; runs in, so `M-.' reaches your own modules beside the notebook.  Without
;; jupyter-lsp, emjupy falls back to a language server on this machine
;; and says so.
;;
;; Requirements:
;;   - Emacs 30.1 or later, and the websocket package.
;;   - A Jupyter server with a kernel: jupyter-server and ipykernel.
;;   - For completion, documentation and `M-.': jupyter-lsp and
;;     python-lsp-server, installed where the server runs.
;;   - Optionally, LaTeX and dvipng, to render formulae in markdown cells.
;;
;; Quick start:
;;
;;   M-x emjupy-login RET 8888 RET
;;
;; A token is asked for only if the server needs one.  Pick a notebook;
;; `C-c C-c' runs the cell at point, and `C-c C-v' shows every command.
;;
;; Documentation: https://mathren.github.io/emjupy/

;;; Code:
;; This file carries the package header and version, and loads the rest.
;; The mode, its keymap and menu are in emjupy-mode; the implementation is
;; layered, each file requiring only the ones before it: emjupy-core,
;; emjupy-http, emjupy-render, emjupy-cells, emjupy-kernel, emjupy-figures, emjupy-widgets, emjupy-widget-page, emjupy-remote,
;; emjupy-lsp, emjupy-eglot, emjupy-mode and emjupy-notebook.


(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)
(require 'emjupy-figures)
(require 'emjupy-widgets)
(require 'emjupy-widget-page)
(require 'emjupy-notebook)

(require 'loadhist)                     ; `feature-file'

(defconst emjupy--home
  (file-name-directory (or load-file-name buffer-file-name default-directory))
  "The directory this file was loaded from, where every emjupy file belongs.")

(defconst emjupy--modules
  '(emjupy-core emjupy-http emjupy-render emjupy-cells emjupy-kernel
    emjupy-figures emjupy-widgets emjupy-widget-page emjupy-remote emjupy-lsp emjupy-eglot
    emjupy-mode emjupy-notebook)
  "The files emjupy is made of, as features.")

(defun emjupy--stray-modules ()
  "Return the emjupy files loaded from a directory other than this one\='s.
Each is a cons of the feature and the file.  Two installations on the
`load-path' at once -- a package and a checkout -- give a mix, and a
mix fails in ways that look like bugs: a command named where its key
should be, widgets drawn as text."
  (cl-loop for feature in emjupy--modules
           for file = (and (featurep feature) (feature-file feature))
           when (and file (not (file-equal-p (file-name-directory file) emjupy--home)))
           collect (cons feature file)))

(when-let* ((strays (emjupy--stray-modules)))
  (display-warning
   'emjupy
   (format "emjupy is loaded from two places: %s, but %s.  Two installations
are on the `load-path' -- remove one, or put the one you want first."
           emjupy--home
           (mapconcat (lambda (s) (format "%s from %s" (car s) (cdr s))) strays ", "))
   :warning))
(require 'emjupy-lsp)
(require 'emjupy-eglot)
(require 'emjupy-mode)
(require 'emjupy-remote)

(defconst emjupy-version "0.1.3"
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

(defun emjupy--language-server-description (nb shadow)
  "Say which language server answers for NB, whose shadow buffer is SHADOW."
  (let ((server (and (buffer-live-p shadow)
                     (with-current-buffer shadow (eglot-current-server)))))
    (cond ((null server) "not started")
          ((emjupy--answer-is-from-the-kernels-machine-p nb)
           "beside the kernel")
          (t "on this machine"))))

;;;###autoload
(defun emjupy-version (&optional insert)
  "Report which emjupy is running, and from where.

Says the version, the directory the code was loaded from and, when that
is a git checkout, the revision -- so \"am I running the fix?\" has an
answer that does not depend on remembering how it was installed.

In a notebook buffer it also says the kernel\='s working directory and
which language server is answering -- the one beside the kernel, or one
on this machine, which cannot see your own modules when the kernel runs
elsewhere.

With a prefix argument, INSERT, put the report in the buffer instead of
the echo area, for pasting into a bug report."
  (interactive "P")
  (let* ((dir (emjupy--source-directory))
         (rev (emjupy--git-revision dir))
         (nb (and (derived-mode-p 'emjupy-mode) emjupy--buffer-notebook))
         (shadow (and nb (emjupy-notebook-shadow-buffer nb)))

         (report (concat
                  (format "emjupy %s" emjupy-version)
                  (if rev (format ", git %s" rev) "")
                  (if dir (format ", loaded from %s" dir) "")
                  (let ((strays (emjupy--stray-modules)))
                    (if strays
                        (format " -- but %s"
                                (mapconcat (lambda (s) (format "%s from %s" (car s) (cdr s)))
                                           strays ", "))
                      ""))
                  (if nb
                      (format "; kernel cwd %s; language server %s"
                              (or (emjupy-notebook-kernel-cwd nb) "unknown")
                              (emjupy--language-server-description nb shadow))
                    ""))))
    (if insert (insert report) (message "%s" report))
    report))

(provide 'emjupy)
;;; emjupy.el ends here
