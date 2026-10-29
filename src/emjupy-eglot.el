;;; emjupy-eglot.el --- Eglot/LSP integration for emjupy code cells  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Completion and eldoc inside notebook cells, delegated to a hidden
;; "shadow" buffer holding the concatenated code of every cell.  A real
;; language server sees one ordinary Python file, so a name defined in one
;; cell completes in another with no manual step.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-http)
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-lsp)
(require 'xref)
(require 'emjupy-kernel)
(require 'emjupy-remote)
(require 'emjupy-shadow)


;;;; Options

(defcustom emjupy-language-support t
  "When non-nil, offer completion, eldoc and \\[xref-find-definitions].

A single switch over every path that talks to a language server, on
either transport: completion, eldoc, M-., starting one when a notebook
opens, and the commands that ask one for something.  Set it to nil to
find out whether a slowdown comes from language support or from
somewhere else: the notebook keeps working -- cells run, output arrives,
everything renders -- and nothing runs after each command except
emjupy's own bookkeeping.  Editing all the code in one buffer, with
\\[emjupy-edit-cell-externally], still works, without a server; set
back to t, a server is started when next one is asked for."
  :type 'boolean
  :group 'emjupy-language-server)

(defcustom emjupy-eglot-delegated-commands
  '(eglot-rename
    eglot-code-actions
    eglot-format
    eglot-format-buffer
    eglot-find-declaration
    eglot-find-implementation
    eglot-find-typeDefinition)
  "Eglot commands redirected to the shadow buffer when run in a notebook.

Each is advised so that inside `emjupy-mode' it runs against the buffer
Eglot actually manages, with point at the matching position, and any
edits it makes are pulled back into the cells.  Outside `emjupy-mode'
the advice does nothing at all."
  :type '(repeat symbol)
  :group 'emjupy-language-server)

;; Eglot is loaded with emjupy -- `emjupy-lsp' requires it for its server class
;; -- but not started: nothing connects until a shadow buffer asks.

;; Eglot's internal API, declared for the compiler; every use is guarded.  These
;; names have no stability promise: `eglot--server-capable' became
;; `eglot-server-capable' in Emacs 30, hence `emjupy--eglot-capable-p'.

;; Eglot cannot attach to a notebook buffer, and has no notebookDocument
;; support.  So all code cells are kept in one python-mode "shadow" buffer, a `#
;; %% [emjupy:ID]' section each, which Eglot sees as one Python file; completion
;; and eldoc map positions to and from it.

;;;; Starting language support

(defun emjupy--refresh-kernel-cwd (nb)
  "Ask NB's kernel which directory it is running in, and remember it."
  (let ((kernel (emjupy-notebook-kernel nb)))
    (when (and kernel (emjupy--ws-live-p kernel))
      (emjupy--kernel-eval
       kernel "import os as _o; print(_o.getcwd())"
       (lambda (out)
         (setq out (string-trim (or out "")))
         (unless (string-empty-p out)
           (setf (emjupy-notebook-kernel-cwd nb) out)
           ;; Now that a kernel has said where it runs, the server's root
           ;; can be worked out from it -- so browsing its files needs no
           ;; configuration either.
           (emjupy--remember-server-root nb)
           ;; The directory was the last thing missing: start now, so the
           ;; handshake -- or its failure -- comes here, not at the first
           ;; completion.
           (emjupy-start-language-support nb)))))))


(defun emjupy--lsp-fall-back (nb why)
  "Have NB use a language server on this machine, WHY the server's could not."
  (let ((buf (emjupy-notebook-buffer nb)))
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (setq emjupy--lsp-route 'local)
        (force-mode-line-update)
        (message "[emjupy] LANGUAGE SERVER FALLBACK for %s: %s.  Using one on THIS machine: it does not see the notebook's environment or files.  M-x emjupy-lsp-diagnose"
                 (emjupy-notebook-path nb) why)))))

(defvar emjupy--suppress-plain-eglot nil
  "Non-nil while a WebSocket-backed Eglot is about to be attached.
`emjupy--ensure-shadow-buffer' would otherwise start an ordinary Eglot
first, and the WebSocket one would find it there and never attach.")

(defun emjupy-start-language-support (&optional nb)
  "Begin language support for NB, without waiting for it.

The shadow buffer is built either way -- it is what Eglot manages -- and
what differs is the transport underneath.  When the server runs
`jupyter-lsp', Eglot is attached over its WebSocket and so reaches the
language server beside the kernel; otherwise Eglot starts one here in
the ordinary way.

Called when a notebook learns where its kernel is, and available by
hand.  Reports what it found rather than failing quietly."
  (interactive)
  (when (and (called-interactively-p 'any) (not emjupy-language-support))
    (user-error "Language support is off: set %s to t"
                'emjupy-language-support))
  (let ((nb (or nb (emjupy--notebook))))
    (when (and nb emjupy-language-support
               ;; It is called when the kernel says where it runs, which may
               ;; be after the notebook was closed: a server started then
               ;; would serve nothing, and nothing would ever end it.
               (buffer-live-p (emjupy-notebook-buffer nb))
               (not (emjupy--shadow-would-mislead-p nb)))
      ;; The route, decided once, before anything attaches: the server's
      ;; language server when it offers one.
      (with-current-buffer (emjupy-notebook-buffer nb)
        (unless emjupy--lsp-route
          (pcase (and (bound-and-true-p emjupy-lsp-enabled)
                      (emjupy--server-offers-lsp-p (emjupy-notebook-server nb)))
            ('yes (setq emjupy--lsp-route 'socket))
            ('no (emjupy--lsp-fall-back
                  nb (format "the server offers no %s through jupyter-lsp"
                             emjupy-lsp-server)))
            ('unanswered (emjupy--lsp-fall-back
                          nb "the server did not say whether it has one"))
            ((guard (not (bound-and-true-p emjupy-lsp-enabled)))
             (emjupy--lsp-fall-back nb "emjupy-lsp-enabled is off"))
            ((guard (null (emjupy-notebook-server nb)))
             (emjupy--lsp-fall-back nb "the notebook has no server"))
            ;; Not known: asked, without waiting; this is done again once
            ;; it has answered.  Nothing attaches meanwhile.
            (_ (emjupy--ask-server-lsp
                (emjupy-notebook-server nb)
                ;; Answered later: only for a notebook still open, its
                ;; kernel connected.  One closed meanwhile was attached to
                ;; all the same, and Eglot waited 30 s for no one.
                (lambda ()
                  (let ((buf (emjupy-notebook-buffer nb)))
                    (when (and (buffer-live-p buf)
                               (with-current-buffer buf (emjupy--ws-live-p)))
                      (emjupy-start-language-support nb)))))))))
      (when (buffer-local-value 'emjupy--lsp-route (emjupy-notebook-buffer nb))
        (let* ((want-socket (and (eq (buffer-local-value
                                      'emjupy--lsp-route
                                      (emjupy-notebook-buffer nb))
                                     'socket)
                                 (emjupy-notebook-kernel-cwd nb)))
               (emjupy--suppress-plain-eglot want-socket)
               ;; Keep it local when the socket is what will read it -- which
               ;; is to say, when nothing will read it.
               (emjupy--force-local-shadow want-socket)
               (buffer (condition-case err
                           (emjupy--ensure-shadow-buffer nb)
                         (file-error
                          (emjupy--shadow-block (error-message-string err))
                          nil))))
          (when (buffer-live-p buffer)
            (or (and want-socket
                     ;; Attached already -- started again as the kernel
                     ;; came back -- is no failure: taken for one, it fell
                     ;; back, and lost the environment's server.
                     (emjupy--socket-attached-p buffer)
                     buffer)
                (and want-socket
                     (emjupy--claim-shadow-for-websocket buffer)
                     (emjupy--eglot-connect nb buffer))
                (progn
                  ;; Offered, but not reached: the fallback, said.
                  (when want-socket
                    (emjupy--lsp-fall-back
                     nb (format "its %s did not connect%s" emjupy-lsp-server
                                (if (bound-and-true-p emjupy--lsp-last-failure)
                                    (format " (%s)" emjupy--lsp-last-failure)
                                  "")))
                    (with-current-buffer buffer
                      (emjupy--attach-plain-eglot
                       nb (emjupy-notebook-buffer nb))))
                  buffer))))))))

(defun emjupy--socket-attached-p (buffer)
  "Return non-nil if BUFFER has emjupy's WebSocket Eglot attached."
  (with-current-buffer buffer
    (let ((server (eglot-current-server)))
      (and server (object-of-class-p server 'emjupy-eglot-server)))))

(defun emjupy--claim-shadow-for-websocket (buffer)
  "Return non-nil if BUFFER is free for a WebSocket server to attach to.
An ordinary Eglot server, started when completion first built the
buffer, may be there; it is shut down.  Only one of ours counts as
attached."
  (with-current-buffer buffer
    (let ((server (eglot-current-server)))
      (cond
       ((null server) t)
       ((object-of-class-p server 'emjupy-eglot-server) nil)
       (t
        ;; It is being replaced either way; a shutdown it does not answer
        ;; leaves it to end with Emacs, and the user is told.
        (condition-case err
            (eglot-shutdown server)
          (jsonrpc-error
           (message "[emjupy] The local language server did not shut down: %s"
                    (error-message-string err))))
        t)))))

(defun emjupy--shadow-file-path (nb)
  "Return a stable on-disk path for NB's shadow Python file.
The server is folded into the name: two servers can both host
`analysis.ipynb', and one shadow file cannot stand for both."
  (let* ((dir (emjupy--shadow-directory-for nb))
         (server (emjupy-notebook-server nb))
         (tag (if server (emjupy--server-label server) "local"))
         ;; Leading dot: the file belongs to emjupy, not to the user, and it
         ;; may well sit in the directory they are working in.  Hidden, it
         ;; stays out of their listings and their completions.
         (safe-name (concat ".emjupy_"
                            (replace-regexp-in-string
                             "[^A-Za-z0-9._-]" "_"
                             (format "%s__%s" tag
                                     (or (emjupy-notebook-path nb)
                                         "untitled"))))))
    ;; No `make-directory' here: for a TRAMP directory it would open a
    ;; connection just to name a path.  `emjupy--ensure-shadow-buffer' makes it.
    (expand-file-name (concat safe-name ".py") dir)))

;;;; The shadow buffer, with Eglot attached

(defun emjupy--attach-plain-eglot (nb nb-buffer)
  "Attach an ordinary Eglot to the current buffer, NB's shadow buffer.
Unless one is attached, another is being, language support is off, or
NB-BUFFER, the notebook's, is backing off."
  (condition-case err
      ;; Connected here, not with `eglot-ensure', which waits for a command in
      ;; this buffer that never comes.  Backed off like the file; the flag is
      ;; set in the notebook buffer, where the next request comes from.  With
      ;; `emjupy-language-support' off the buffer is still built, for editing
      ;; the code, but nothing is attached.
      (unless (or emjupy--suppress-plain-eglot
                  (not emjupy-language-support)
                  ;; only as the fallback, decided: never first, racing
                  (not (eq (buffer-local-value 'emjupy--lsp-route nb-buffer)
                           'local))
                  (with-current-buffer nb-buffer (emjupy--shadow-blocked-p))
                  (and (boundp 'eglot--managed-mode) eglot--managed-mode))
        (with-timeout (emjupy-shadow-timeout
                       (emjupy--shadow-block "language server did not start"
                                             nb-buffer))
          (apply #'eglot--connect (eglot--guess-contact))
          ;; Started, and on this machine: say so, since this fallback
          ;; answers about the wrong files while seeming to work.  Wrapped:
          ;; a warning must not break what it warns about.
          (emjupy--warn-local-language-server nb)))
    (error
     (emjupy--shadow-block (error-message-string err) nb-buffer))))

(defun emjupy--ensure-shadow-buffer (nb)
  "Return NB's persistent code shadow buffer, creating it if needed.
Its content is refreshed to match the current cells, and Eglot attached
if it is not -- automatically, with nothing for the user to run.  Nil
while making it is backing off."
  (let* ((nb-buffer (or (emjupy-notebook-buffer nb) (current-buffer)))
         (content (emjupy--build-shadow-content nb))
         (path (emjupy--shadow-file-path nb))
         (buf (emjupy--retire-moved-shadow nb path)))
    (unless (buffer-live-p buf)
      ;; This runs from eldoc and completion, after every command, so remote
      ;; work is backed off: one unreachable host would hang every keystroke.
      ;; A local directory cannot hang, and is not.
      (setq buf (and (not (and (file-remote-p path)
                               (emjupy--shadow-blocked-p)))
                     (emjupy--make-shadow-directory path)
                     (emjupy--create-shadow-buffer nb path content))))
    (when buf
      (with-current-buffer buf
        (emjupy--sync-shadow-content content)
        (emjupy--attach-plain-eglot nb nb-buffer)))
    buf))

;;;; Editing all the code

(defun emjupy-edit-cell-externally ()
  "Edit the cell at point with real language tooling.

Code cells open a persistent, shared Python buffer containing ALL of the
notebook's code cells (marked `# %% [emjupy:ID]'), with Eglot started
automatically -- so completions, diagnostics, and go-to-def are aware of
definitions from every cell, not just this one, and there's nothing extra for
you to run.  \\[emjupy-commit-shadow-edit] commits every section you touched
back into its cell; \\[emjupy-abort-shadow-edit] discards uncommitted edits.
The buffer and its Eglot connection persist, so only the first use per session
pays the language-server startup cost.

Markdown cells open their own simple edit buffer instead -- cross-cell
LSP awareness doesn't apply to prose."
  (interactive)
  (emjupy--sync-all-cells)
  (let* ((cell (get-text-property (point) 'emjupy-cell))
         (nb emjupy--buffer-notebook)
         (nb-buf (current-buffer)))
    (unless cell (user-error "No cell found at point"))
    (if (eq (emjupy-cell-type cell) 'code)
        (let ((buf (emjupy--ensure-shadow-buffer nb)))
          (emjupy--goto-shadow-section buf (emjupy-cell-id cell))
          (pop-to-buffer buf)
          (message "[emjupy] Editing notebook code (all cells%s) -- C-c C-c to commit, C-c C-k to discard."
                   (if emjupy-language-support ", Eglot active" "")))
      (emjupy--edit-markdown-cell-externally cell nb-buf))))

;;;; Definitions and references

(defun emjupy--xref-backend ()
  "`xref-backend-functions' entry for notebook buffers.
None when `emjupy-language-support' is nil, which leaves
\\[xref-find-definitions] to the other backends rather than starting a
language server."
  (and emjupy-language-support emjupy--buffer-notebook 'emjupy))

(defun emjupy--xref-in-shadow (fn)
  "Call FN with Eglot's own xref backend, inside the shadow buffer at point."
  (emjupy--cell-shadow-delegate
   (lambda (_cell-start _shadow-start buf)
     (when (emjupy--eglot-live-server buf)
       (let ((backend (run-hook-with-args-until-success
                       'xref-backend-functions)))
         (when backend (funcall fn backend)))))))

(defun emjupy--names-a-file-elsewhere-p (nb file)
  "Non-nil when FILE belongs to NB's kernel and is not on this machine.
A path under the kernel's root that also exists here is the same file;
one with nothing here is a remote file with a local-looking name."
  (let* ((server (emjupy-notebook-server nb))
         (root (and server (emjupy--server-side-root-for server))))
    (and root
         (string-prefix-p (file-name-as-directory root) file)
         (not (file-exists-p file)))))

(defun emjupy--answer-is-from-the-kernels-machine-p (nb)
  "Non-nil if NB's language server is the one beside its kernel.
That is the WebSocket server; any other is on this machine, answering
about its files."
  (let ((sb (emjupy-notebook-shadow-buffer nb)))
    (and (buffer-live-p sb)
         (with-current-buffer sb
           (let ((server (eglot-current-server)))
             (and server (object-of-class-p server 'emjupy-eglot-server)))))))

(defun emjupy--xref-on-the-right-machine (nb item)
  "Return ITEM with its file named on the machine of NB's kernel.
The language server reports paths as the kernel's machine sees them.
Rewritten to a remote name when the kernel is elsewhere and one can be
built; otherwise left as it is -- a wrong remote name is worse."
  (let* ((loc (xref-item-location item))
         ;; Only the server beside the kernel names the kernel's files; the
         ;; local fallback names files on this machine.
         (file (and (emjupy--answer-is-from-the-kernels-machine-p nb)
                    (xref-file-location-p loc)
                    (xref-location-group loc)))
         (remote (and file
                      (not (file-remote-p file))
                      (emjupy--remote-name-for nb file))))
    (when (and file (not remote) (emjupy--names-a-file-elsewhere-p nb file))
      ;; No remote name to build, and the local file does not exist: opening it
      ;; would show an empty buffer at a plausible path, as if the definition
      ;; were blank.
      (user-error
       "%s is on the machine running the kernel; set `emjupy-remote-root' to reach it"
       (abbreviate-file-name file)))
    (if (not remote)
        item
      (xref-make (xref-item-summary item)
                 (xref-make-file-location
                  remote
                  (or (xref-location-line loc) 1)
                  (xref-file-location-column loc))))))

(defun emjupy--remote-name-for (nb file)
  "Return FILE as a name on NB's server, or nil to leave it alone.
Every absolute path the server reports is on the kernel's machine --
a standard library path too, which may exist here as a different file.
Rewritten only when that machine is known to be another one."
  (let* ((server (emjupy-notebook-server nb))
         ;; A root set by hand first: a tunnelled server answers at localhost,
         ;; and only the configured value says where its files are.
         (tramp (and server (or (emjupy--remote-root-for-files server)
                                (emjupy--tramp-root-for server))))
         (prefix (and tramp (file-remote-p tramp))))
    (when (and prefix
               (stringp file)
               (file-name-absolute-p file)
               (not (file-remote-p file)))
      (concat prefix file))))

(defun emjupy--local-truename (file)
  "Return FILE's true name if it is on this machine, else FILE as written.
`file-truename' on a TRAMP name asks the other machine for every link;
the shadow's remote name has none."
  (if (file-remote-p file) (expand-file-name file) (file-truename file)))

(defun emjupy--shadow-point (buf line col)
  "Return the position of LINE and COL in shadow buffer BUF.
Nil without a LINE; never past the end of the buffer."
  (when line
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (forward-line (1- line))
        (min (point-max) (+ (point) col))))))

(defun emjupy--xref-remap (nb items)
  "Rewrite ITEMS pointing into NB's shadow file so they point at its cells.
Items pointing anywhere else, a library say, are left as they are."
  (let* ((buf (emjupy-notebook-shadow-buffer nb))
         (shadow-file (and (buffer-live-p buf)
                           (buffer-local-value 'buffer-file-name buf)))
         ;; once for the search, not once per item it returned
         (shadow-name (and shadow-file (emjupy--local-truename shadow-file)))
         (nb-buf (emjupy-notebook-buffer nb)))
    (if (not (and items shadow-file (buffer-live-p nb-buf)))
        items
      (mapcar
       (lambda (item)
         (let* ((loc (xref-item-location item))
                (file (and (xref-file-location-p loc)
                           (xref-location-group loc)))
                (same (and file (equal (emjupy--local-truename file)
                                       shadow-name))))
           (if (not same)
               (emjupy--xref-on-the-right-machine nb item)
             (let* ((line (xref-location-line loc))
                    (col (xref-file-location-column loc))
                    (spos (emjupy--shadow-point buf line col))
                    (mapped (and spos
                                 (emjupy--shadow-position-to-cell nb spos))))
               (if (not mapped)
                   item
                 (xref-make (xref-item-summary item)
                            (xref-make-buffer-location nb-buf
                                                       (cdr mapped))))))))
       items))))

(cl-defmethod xref-backend-identifier-at-point ((_backend (eql emjupy)))
  "Return the identifier at point, as Eglot sees it in the shadow buffer."
  (or (emjupy--xref-in-shadow #'xref-backend-identifier-at-point)
      (thing-at-point 'symbol t)))

(cl-defmethod xref-backend-identifier-completion-table ((_backend (eql emjupy)))
  "Return the completion table Eglot provides for identifiers."
  (emjupy--xref-in-shadow #'xref-backend-identifier-completion-table))

(cl-defmethod xref-backend-definitions ((_backend (eql emjupy)) identifier)
  "Return definitions of IDENTIFIER, mapped back onto notebook cells."
  (let ((nb emjupy--buffer-notebook))
    (emjupy--xref-remap
     nb (emjupy--xref-in-shadow
         (lambda (b) (xref-backend-definitions b identifier))))))

(cl-defmethod xref-backend-references ((_backend (eql emjupy)) identifier)
  "Return references to IDENTIFIER, mapped back onto notebook cells."
  (let ((nb emjupy--buffer-notebook))
    (emjupy--xref-remap
     nb (emjupy--xref-in-shadow
         (lambda (b) (xref-backend-references b identifier))))))

(cl-defmethod xref-backend-apropos ((_backend (eql emjupy)) pattern)
  "Return matches for PATTERN, mapped back onto notebook cells."
  (let ((nb emjupy--buffer-notebook))
    (emjupy--xref-remap
     nb (emjupy--xref-in-shadow
         (lambda (b) (xref-backend-apropos b pattern))))))

;;;; Eglot commands, from the notebook

(defun emjupy--pull-shadow-into-cells (nb)
  "Write NB's shadow sections back into its cells.
Return the number of cells updated; the notebook is redrawn if any was."
  (let ((buf (emjupy-notebook-shadow-buffer nb))
        (updated 0))
    (when (buffer-live-p buf)
      (let ((sections (with-current-buffer buf
                        (emjupy--parse-shadow-sections (buffer-string)))))
        (cl-loop for cell across (emjupy-notebook-cells nb)
                 do (let ((match (assq (emjupy-cell-id cell) sections)))
                      (when (and match
                                 (not (string= (cdr match)
                                               (emjupy-cell-source cell))))
                        (setf (emjupy-cell-source cell) (cdr match))
                        (setq updated (1+ updated)))))
        (when (> updated 0)
          (let ((nb-buf (emjupy-notebook-buffer nb)))
            (when (buffer-live-p nb-buf)
              (with-current-buffer nb-buf (emjupy--redraw-notebook)))))))
    updated))

(defun emjupy-eglot-delegate (fn)
  "Run FN in this notebook's shadow buffer, at the matching position.
Anything FN changes is then pulled back into the cells.

This is what makes Eglot's own commands usable from a notebook.  They
call `eglot--current-server-or-lose', and the notebook buffer is not the
one Eglot manages -- the hidden shadow buffer is -- so invoked directly
they fail with a bare \"No current JSON-RPC connection\".  Completion and
eldoc already went through the shadow buffer; everything else did not."
  (let ((nb (emjupy--notebook)))
    (unless (get-text-property (point) 'emjupy-cell)
      (user-error "Point is not in a cell"))
    (unless (emjupy--cell-shadow-delegate
             (lambda (_cell-start _shadow-start buf)
               (unless (emjupy--eglot-live-server buf)
                 (user-error "No language server is running for this notebook"))
               (funcall fn)
               t))
      (user-error "Point is not in a code cell"))
    (let ((updated (emjupy--pull-shadow-into-cells nb)))
      (when (> updated 0)
        (message "[emjupy] %d cell%s updated." updated (if (= updated 1) ""
                                                         "s")))
      updated)))

;; Eglot's commands are advised, against the manual's advice for packages:
;; Eglot finds its server from the current buffer -- for a notebook, its
;; shadow buffer -- with no way to act through another, and remapping keys
;; would leave M-x failing.  Narrow: in notebook buffers only, on
;; `emjupy-eglot-delegated-commands' only; `emjupy-eglot-remove-advice'.

(defun emjupy--eglot-command-advice (orig &rest args)
  "Around-advice running ORIG with ARGS in the shadow buffer, in notebooks.
Interactive arguments are read in the notebook first, so prompts see the
symbol under the real cursor."
  (cond
   ((not emjupy--buffer-notebook)
    (apply orig args))
   ;; Every command goes to the shadow buffer, where Eglot is attached -- over
   ;; the Jupyter WebSocket with jupyter-lsp, a local process otherwise.
   (t (emjupy-eglot-delegate (lambda () (apply orig args))))))

(defvar emjupy--eglot-advice-installed nil
  "Non-nil once Eglot's commands have been advised in this session.
The first notebook advises them; one removed by hand stays removed.")

(defun emjupy-eglot-install-advice ()
  "Advise `emjupy-eglot-delegated-commands' to work inside notebooks."
  (setq emjupy--eglot-advice-installed t)
  (dolist (cmd emjupy-eglot-delegated-commands)
    (advice-add cmd :around #'emjupy--eglot-command-advice)))

(defun emjupy-eglot-remove-advice ()
  "Undo `emjupy-eglot-install-advice'."
  (dolist (cmd emjupy-eglot-delegated-commands)
    (advice-remove cmd #'emjupy--eglot-command-advice)))

;;;; Completion and eldoc

(defun emjupy--lsp-in-charge-p ()
  "Return non-nil when Eglot is attached over the Jupyter WebSocket.
For reporting, in `emjupy-lsp-diagnose' and one message; requests all
go to the shadow buffer either way."
  (and (bound-and-true-p emjupy--buffer-notebook)
       (let ((buf (emjupy-notebook-shadow-buffer emjupy--buffer-notebook)))
         (and (buffer-live-p buf)
              (with-current-buffer buf
                (let ((server (eglot-current-server)))
                  (and server
                       (object-of-class-p server 'emjupy-eglot-server))))))))

(defun emjupy--cell-shadow-delegate (fn)
  "Delegate to the shadow buffer when point is in a code cell.
The buffer is synced, point moved to the matching position there, and
FN called with CELL-START, SHADOW-START and the shadow BUFFER, with that
buffer current.  Return FN's value, or nil outside a code cell."
  ;; Only when the better transport is unavailable, not while it is still
  ;; connecting -- as it is whenever a notebook opens.
  (when (and emjupy--buffer-notebook
             (or (not (bound-and-true-p emjupy-lsp-enabled))
                 (emjupy--shadow-blocked-p))
             (not (emjupy--lsp-in-charge-p)))
    (emjupy--warn-shadow-is-local emjupy--buffer-notebook))
  (unless (and emjupy--buffer-notebook
               (emjupy--shadow-would-mislead-p emjupy--buffer-notebook))
    (let ((cell (get-text-property (point) 'emjupy-cell))
          (nb emjupy--buffer-notebook))
      (when (and cell nb (eq (emjupy-cell-type cell) 'code)
                 (emjupy-cell-overlay cell))
        (emjupy--sync-all-cells)
        (let* ((main-point (point))
               (cell-start (overlay-start (emjupy-cell-overlay cell)))
               (buf (emjupy--ensure-shadow-buffer nb))
               (shadow-start (emjupy--shadow-section-start
                              buf (emjupy-cell-id cell))))
          (with-current-buffer buf
            ;; Clamp: if the cell's shadow section is shorter than the offset
            ;; (mid-edit, before a resync), an unclamped goto-char signals
            ;; `args-out-of-range' and kills completion for the whole buffer.
            (goto-char (max (point-min)
                            (min (point-max)
                                 (+ shadow-start (- main-point cell-start)))))
            (funcall fn cell-start shadow-start buf)))))))

(defun emjupy--cell-completion-at-point ()
  "Return completions for the cell at point.
This is a `completion-at-point-functions' entry; it delegates to Eglot via the
shared code shadow buffer, so completions see definitions from every
cell in the notebook, automatically."
  (emjupy--cell-shadow-delegate
   (lambda (cell-start shadow-start buf)
     ;; Eglot's capf signals a jsonrpc-error when its server is gone: skip the
     ;; delegation rather than query a dead server.
     (let ((result (and (emjupy--eglot-live-server buf)
                        (condition-case err
                            (run-hook-with-args-until-success
                             'completion-at-point-functions)
                          ;; a server gone between the check and the call
                          (jsonrpc-error
                           (message "[emjupy] completion unavailable: %s"
                                    (error-message-string err))
                           nil)))))
       (when (consp result)
         (let* ((orig-collection (nth 2 result))
                ;; Eglot's collection is called again later, from the notebook
                ;; buffer, but needs the shadow buffer's context: force it on
                ;; every call.
                (wrapped (if (functionp orig-collection)
                             (lambda (string pred action)
                               (with-current-buffer buf
                                 (funcall orig-collection string pred action)))
                           orig-collection)))
           (append (list (+ cell-start (- (nth 0 result) shadow-start))
                         (+ cell-start (- (nth 1 result) shadow-start))
                         wrapped)
                   (nthcdr 3 result))))))))

(defun emjupy--eglot-capable-p (&rest capabilities)
  "Return non-nil if the current Eglot server advertises CAPABILITIES.
`eglot--server-capable' was renamed `eglot-server-capable' in Emacs
30, so guarding on the old private name alone silently disables eldoc
on newer Emacs."
  (and (eglot-current-server)
       (apply #'eglot-server-capable capabilities)))

(defun emjupy--cell-eldoc-function (callback)
  "Report hover documentation for the cell at point to CALLBACK.
An `eldoc-documentation-functions' entry, asking Eglot directly:
Eglot's own function answers only for a buffer shown in a window,
which the shadow buffer never is."
  (emjupy--cell-shadow-delegate
   (lambda (_cell-start _shadow-start _buf)
     (when (and (emjupy--eglot-live-server)
                (emjupy--eglot-capable-p :hoverProvider))
       (let ((server (emjupy--eglot-live-server)))
         ;; Asked for, not waited for: this runs after every command, and eldoc
         ;; is built for a late answer.
         (jsonrpc-async-request
          server :textDocument/hover (eglot--TextDocumentPositionParams)
          :success-fn
          (lambda (resp)
            (let ((contents (plist-get resp :contents)))
              (unless (seq-empty-p contents)
                (funcall callback
                         (eglot--hover-info contents
                                            (plist-get resp :range))))))
          :error-fn #'ignore
          :timeout-fn #'ignore))
       t))))

;;;; Diagnosis

(defun emjupy--lsp-diagnosis (nb)
  "Return the lines of the language-server report for NB.
Run in NB's buffer: the back-off it reports is that buffer's."
  (let* ((server (emjupy-notebook-server nb))
         (shadow (emjupy-notebook-shadow-buffer nb))
         (attached (and (buffer-live-p shadow)
                        (with-current-buffer shadow
                          (eglot-current-server))))
         (kind (cond
                ((null attached) "none")
                ((object-of-class-p attached 'emjupy-eglot-server)
                 "over the Jupyter WebSocket")
                (t "a local process")))
         (alive (and attached (jsonrpc-running-p attached))))
    (list
     (format "notebook        %s" (or (emjupy-notebook-path nb) "?"))
     (format "server          %s" (if server
                                      (emjupy--server-label server)
                                    "none"))
     (format "server root     %s"
             (or (and server (emjupy--remote-root-for server))
                 "unknown -- no kernel has reported in"))
     (format "kernel cwd      %s"
             (or (emjupy-notebook-kernel-cwd nb)
                 "unknown -- kernel has not answered"))
     (format "lsp enabled     %s" (if emjupy-lsp-enabled "yes" "no"))
     (format "lsp url         %s"
             ;; Redacted: this report is written to be pasted into a
             ;; bug report, and the URL carries the token.
             (if server (emjupy--redact-url (emjupy--lsp-url server))
               "n/a"))
     (format "shadow file     %s"
             (if (buffer-live-p shadow)
                 (or (buffer-local-value 'buffer-file-name shadow)
                     "unnamed")
               "not created"))
     (format "language server %s" kind)
     (format "connection      %s"
             (cond ((null attached) "not attached")
                   (alive "running")
                   (t "attached but not running")))
     (format "back-off        %s"
             (if (emjupy--shadow-blocked-p) "in effect" "none"))
     ;; The reason the WebSocket route was not taken, if it was tried
     ;; and failed.  Without this the report said everything was well
     ;; and a local server was running anyway.
     (format "socket failure  %s"
             (or (bound-and-true-p emjupy--lsp-last-failure)
                 "none recorded")))))

(defun emjupy-lsp-diagnose ()
  "Report which language server this notebook is using, and how it got there.
Reads the server Eglot has attached to the shadow buffer, which is where
the answer lives."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (lines (emjupy--lsp-diagnosis nb)))
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

;;;; Completion and eldoc in a notebook

(defun emjupy--capf ()
  "Completion for the cell at point, on whichever transport is available."
  (when emjupy-language-support
    (emjupy--timed "completion" #'emjupy--cell-completion-at-point)))

(defun emjupy--eldoc (callback &rest args)
  "Report documentation for the cell at point to CALLBACK, passing ARGS on."
  (when emjupy-language-support
    (apply #'emjupy--timed "eldoc" #'emjupy--cell-eldoc-function
           callback args)))

;;;; In a notebook buffer

(defun emjupy-eglot-enable ()
  "Wire language support into the current notebook buffer.
When a kernel attaches, it is asked where it runs, for the language
server; and \\[xref-find-definitions] gets a backend forwarding to the
shadow buffer, where Eglot's is.  Eglot's commands are advised to work
here, once a session: see `emjupy-eglot-install-advice'."
  (unless emjupy--eglot-advice-installed
    (emjupy-eglot-install-advice))
  (add-hook 'emjupy-kernel-connected-functions #'emjupy--refresh-kernel-cwd)
  (add-hook 'xref-backend-functions #'emjupy--xref-backend nil t))

(defun emjupy-eglot-completion-enable ()
  "Turn on completion and eldoc in the current notebook's code cells.
They go through the shadow buffer: over the Jupyter server's
`jupyter-lsp' first, a local server otherwise.  Turned on last, as
`eldoc-mode' adds its own command hooks after the other layers'."
  (add-hook 'completion-at-point-functions #'emjupy--capf nil t)
  (add-hook 'eldoc-documentation-functions #'emjupy--eldoc nil t)
  (eldoc-mode 1))

(provide 'emjupy-eglot)
;;; emjupy-eglot.el ends here
