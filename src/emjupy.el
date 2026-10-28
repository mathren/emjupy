;;; emjupy.el --- Interactive Jupyter notebooks over HTTP and WebSocket  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Maintainer: Mathieu Renzo <mrenzo@arizona.edu>
;; Version: 0.2.1
;; Package-Requires: ((emacs "30.1") (websocket "1.15"))
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

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

;; Every file of the package, in the order its layers build on each other.
(require 'emjupy-core)
(require 'emjupy-terminal)
(require 'emjupy-ipynb)
(require 'emjupy-latex)
(require 'emjupy-undo)
(require 'emjupy-faces)
(require 'emjupy-http)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-commands)
(require 'emjupy-kernel)
(require 'emjupy-debug)
(require 'emjupy-figures)
(require 'emjupy-widgets)
(require 'emjupy-widget-page)
(require 'emjupy-remote)
(require 'emjupy-lsp)
(require 'emjupy-shadow)
(require 'emjupy-eglot)
(require 'emjupy-mode)
(require 'emjupy-save)
(require 'emjupy-notebook)
(require 'emjupy-dashboard)
(require 'emjupy-tramp-open)

;;;; Version

(defconst emjupy-version "0.2.1"
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
  "Report the version, and the directory the code was loaded from.
When that is a git checkout, the revision too.

In a notebook buffer it also says the kernel's working directory and
which language server is answering.

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
                                (mapconcat (lambda (s) (format "%s from %s"
                                                               (car s) (cdr s)))
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
