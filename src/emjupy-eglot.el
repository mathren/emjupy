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
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-lsp)
(require 'xref)
(require 'emjupy-kernel)
(require 'emjupy-remote)

(defvar python-indent-guess-indent-offset-verbose)

;;;; Options

(defcustom emjupy-shadow-directory nil
  "Directory holding the Eglot shadow files, or nil for a local temp dir.

Set this to a TRAMP path to run the language server on the machine the
kernel runs on:

  (setq emjupy-shadow-directory \"/ssh:user@host:/tmp/emjupy-shadow\")

The shadow file is an ordinary Python file, so Eglot starts whatever
server that directory implies -- a remote one over TRAMP, a local one
otherwise.  That matters when the Jupyter server is remote: a local
language server sees the local environment, not the packages the kernel
actually has.  It is a setting rather than something inferred, because a
server reached through `ssh -L' looks like localhost from here."
  :type '(choice (const :tag "Local temporary directory" nil)
                 (directory :tag "Directory (may be a TRAMP path)"))
  :group 'emjupy)

(defcustom emjupy-shadow-sync-to-disk 'lazy
  "When to write the shadow file to disk.

`lazy' (the default) writes it once, when it is created.  Later edits
are sent to the language server by Eglot as `didChange' notifications,
which is how LSP is meant to work and needs no file at all.

`always' also rewrites the file on every edit.  That is only useful if
something outside Emacs reads it, and it is expensive: a `write-region'
over TRAMP measured 214 ms against ssh on localhost, and it lands on the
completion and eldoc paths, once per keystroke."
  :type '(choice (const :tag "Once, when created" lazy)
                 (const :tag "On every edit" always))
  :group 'emjupy)

(defcustom emjupy-shadow-host nil
  "TRAMP prefix for reaching the machine the kernel runs on, or nil.

Only needed when the kernel is NOT on this machine.  emjupy asks the
kernel where the notebook lives and gets back an absolute path on ITS
filesystem -- =/srv/nb/sub= say.  That path means nothing here: Emacs
would read it as a local one and write the shadow file into whatever
=/srv/nb/sub= happens to be on your own disk.  So a kernel-reported
directory is used only when emjupy can actually address it:

  (setq emjupy-shadow-host \"/ssh:box:\")

giving =/ssh:box:/srv/nb/sub=, where Eglot starts the language server on
=box= and sees the environment the kernel really runs in.

Unset, with a remote kernel, emjupy falls back to a local temp directory
rather than guess: `xref-find-definitions' will not find your own
modules, but nothing is written to a wrong path."
  :type '(choice (const :tag "Kernel runs on this machine" nil)
                 (string :tag "TRAMP prefix, e.g. /ssh:box:"))
  :group 'emjupy)

(defcustom emjupy-shadow-beside-notebook t
  "When non-nil, keep the shadow file in the notebook's own directory.

This is what makes \\[xref-find-definitions] work on your own modules.
A language server resolves `import mylib' relative to the directory of
the file it is reading, so a shadow file parked in a temp directory
cannot see the .py sitting next to the notebook, and every such lookup
answers \"No definition found\".

The directory is worked out from `emjupy-remote-root', which maps a
server to where its files actually live.  Without that mapping there is
nothing to derive -- a tunnelled server looks like localhost -- so the
temp directory is used and this quietly has no effect.

It also has no effect when the shadow has to stay on this machine and
the notebook is on another: \"beside\" would then name a LOCAL directory
after a REMOTE path, and emjupy would create it here.  The WebSocket
transport does not read the file anyway."
  :type 'boolean
  :group 'emjupy)

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
  :group 'emjupy)

(defcustom emjupy-shadow-retry-interval 30
  "Seconds to wait before retrying a shadow buffer that failed to set up.

Completion and eldoc ask for the shadow buffer after every command.  If
building it fails -- an unreachable host, a directory that cannot be
created -- retrying each time turns one slow failure into a hang, since
each attempt blocks Emacs while TRAMP waits."
  :type 'number
  :group 'emjupy)

(defcustom emjupy-shadow-timeout 2
  "Seconds to let shadow-buffer setup block Emacs before giving up.

Reached from eldoc and completion, so this is time the user spends
waiting after an ordinary command.  Language-server support is worth a
short pause and no more; editing and running cells do not depend on it."
  :type 'number
  :group 'emjupy)

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
  :group 'emjupy)

(defcustom emjupy-shadow-when-kernel-unreachable t
  "Whether to run a local language server when the kernel is elsewhere.

The shadow file then sits on THIS machine, so what the server describes
is this machine: its interpreter, its packages, and none of the modules
that live beside the notebook.  For ordinary Python that is mostly
right and worth having; for anything that depends on the environment the
code will actually run in, it is confidently wrong.

Set it to nil to have no language support at all in that case rather
than approximate support.  Either way, `emjupy-shadow-host' or
`jupyter-lsp' on the server gives answers from the right machine."
  :type 'boolean
  :group 'emjupy)

;; Eglot is loaded with emjupy -- `emjupy-lsp' requires it for its server class
;; -- but not started: nothing connects until a shadow buffer asks.

;; Eglot's internal API, declared for the compiler; every use is guarded.  These
;; names have no stability promise: `eglot--server-capable' became
;; `eglot-server-capable' in Emacs 30, hence `emjupy--eglot-capable-p'.

;; A notebook buffer mixes cells, decoration and output, which Eglot cannot
;; attach to, and Eglot has no notebookDocument support.  So all code cells are
;; kept together in one python-mode "shadow" buffer, a `# %% [emjupy:ID]'
;; section per cell, which Eglot sees as one Python file.  Completion and eldoc
;; in the notebook delegate to it, mapping positions both ways; `C-c '' opens it
;; for heavier editing.  Markdown cells are edited one at a time.

;;;; Editing a cell in a buffer of its own

(defvar-local emjupy--edit-source-cell nil
  "The `emjupy-cell' struct backing this transient markdown-cell-edit buffer.")
(defvar-local emjupy--edit-source-notebook-buffer nil
  "The notebook buffer this markdown-cell-edit buffer commits changes into.")

(defvar emjupy-cell-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emjupy-commit-cell-edit)
    (define-key map (kbd "C-c '")   #'emjupy-commit-cell-edit)
    (define-key map (kbd "C-c C-k") #'emjupy-abort-cell-edit)
    map)
  "Keymap active in the transient markdown-cell edit buffer.")

(define-minor-mode emjupy-cell-edit-mode
  "Minor mode for the transient markdown-cell edit buffer.
Opened by
`emjupy-edit-cell-externally'."
  :lighter " emjupy-edit"
  :keymap emjupy-cell-edit-mode-map)

(defun emjupy--edit-markdown-cell-externally (cell nb-buf)
  "Open markdown CELL from notebook buffer NB-BUF in its own edit buffer."
  (let* ((mode-fn (or (emjupy--markdown-mode-fn) 'text-mode))
         (buf (generate-new-buffer
               (format "*emjupy-cell-edit: %s[%s]*" (buffer-name nb-buf)
                       (emjupy-cell-id cell)))))
    (with-current-buffer buf
      (insert (emjupy-cell-source cell))
      (funcall mode-fn)
      (emjupy-cell-edit-mode 1)
      (setq emjupy--edit-source-cell cell)
      (setq emjupy--edit-source-notebook-buffer nb-buf)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "[emjupy] Editing cell externally in %s -- C-c C-c to commit, C-c C-k to discard." mode-fn)))

(defun emjupy-commit-cell-edit ()
  "Commit this cell-edit buffer's text back into its cell, then close it."
  (interactive)
  (unless (and emjupy--edit-source-cell
               (buffer-live-p emjupy--edit-source-notebook-buffer))
    (user-error "This buffer isn't an emjupy cell-edit buffer"))
  (let ((new-source (string-trim-right (buffer-string) "\n"))
        (cell emjupy--edit-source-cell)
        (nb-buf emjupy--edit-source-notebook-buffer)
        (edit-buf (current-buffer)))
    (setf (emjupy-cell-source cell) new-source)
    (with-current-buffer nb-buf
      (emjupy--rerender-notebook cell))
    (kill-buffer edit-buf)
    (message "[emjupy] Cell updated.")))

(defun emjupy-abort-cell-edit ()
  "Abandon this markdown-cell-edit buffer without committing."
  (interactive)
  (when (y-or-n-p "Discard changes to this cell? ")
    (kill-buffer)))

;; --- Code cells: persistent multi-cell shadow buffer with Eglot ------------

;;;; Editing all the code

(defvar-local emjupy--edit-shadow-notebook nil
  "The `emjupy-notebook' struct this shared code shadow-buffer belongs to.")

(defvar emjupy-shadow-edit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") #'emjupy-commit-shadow-edit)
    (define-key map (kbd "C-c '")   #'emjupy-commit-shadow-edit)
    (define-key map (kbd "C-c C-k") #'emjupy-abort-shadow-edit)
    map)
  "Keymap active in the shared multi-cell code shadow-buffer.")

(define-minor-mode emjupy-shadow-edit-mode
  "Minor mode for the persistent shared-code shadow buffer.
Opened by
`emjupy-edit-cell-externally' for code cells.  Eglot manages this
buffer like any ordinary Python file."
  :lighter " emjupy-shadow")

;;;; The shadow file

(defconst emjupy--shadow-coding 'utf-8-unix
  "Coding system used for the Eglot shadow file, in both directions.
Pinned: left to choose, `write-region' asks for a coding system on an
undecodable byte, and `raw-text' would turn the file to mojibake.  LSP
is UTF-8 anyway.")

(defun emjupy--eglot-live-server (&optional buffer)
  "Return the live Eglot server for BUFFER, or nil.
`eglot-current-server' can return a server whose process has died, and
using one raises a jsonrpc-error out of completion."
  (with-current-buffer (or buffer (current-buffer))
    (let ((server (eglot-current-server)))
      (and server (jsonrpc-running-p server) server))))

(defvar emjupy--force-local-shadow nil
  "Non-nil while the shadow file must be on this machine.
The WebSocket transport never reads the file, and rewrites the paths it
reports; building it over TRAMP would only be slow, or time out.")

(defun emjupy--shadow-must-stay-here-p (nb)
  "Non-nil when NB's shadow belongs on this machine but its files do not.

True exactly when the WebSocket transport is in use and the notebook is
somewhere this machine cannot reach as a plain path."
  (and emjupy--force-local-shadow
       (let ((dir (emjupy--notebook-directory nb)))
         (and dir (not (emjupy--local-directory-p dir))))))

(defun emjupy--shadow-directory-for (nb)
  "Return the directory NB's shadow file belongs in."
  ;; Forcing local rejects REMOTE candidates only.  A directory set by hand
  ;; that is already on this machine is still what the user asked for.
  (cl-flet ((usable (dir)
              (and dir
                   (not (and emjupy--force-local-shadow (file-remote-p dir)))
                   dir)))
    (let* ((explicit (usable (and emjupy-shadow-directory
                                  (file-name-as-directory
                                   emjupy-shadow-directory))))
           ;; Not beside a notebook that lives elsewhere when the shadow must
           ;; stay here: that would make a local directory named after a remote
           ;; path.  The socket transport never reads the file, so a temporary
           ;; directory serves.
           (beside (and (not explicit)
                        emjupy-shadow-beside-notebook
                        (not (emjupy--shadow-must-stay-here-p nb))
                        (usable (emjupy--notebook-directory nb)))))
      (or explicit beside (emjupy--shadow-temporary-directory)))))

(defun emjupy--shadow-temporary-directory ()
  "Return the temporary directory for this user's shadow files.
One per user, named after them: a single one in a shared /tmp was made
by whoever came first, and nobody else could write in it.  It is made
readable by its owner alone -- a shadow file holds a notebook's code."
  (file-name-as-directory
   (expand-file-name (format "emjupy-shadow-%s" (user-login-name))
                     temporary-file-directory)))

;;;; Where the notebook lives

(defun emjupy--notebook-directory (nb)
  "Return the directory NB lives in as a file name Emacs can use, or nil.
The kernel is asked first: it runs in the notebook's directory.  Then
`emjupy-remote-root'.  A remote kernel's answer is used only when
`emjupy-shadow-host' says how to reach its machine."
  (or (emjupy--notebook-directory-from-kernel nb)
      (let* ((server (emjupy-notebook-server nb))
             (root (and server (emjupy--remote-root-for server)))
             (path (emjupy-notebook-path nb)))
        (when (and root path)
          (let ((dir (file-name-directory
                      (expand-file-name path (file-name-as-directory root)))))
            (and dir (file-name-as-directory dir)))))))

(defun emjupy--local-directory-p (dir)
  "Return non-nil if DIR exists on this machine, without contacting another.
`file-directory-p' on a TRAMP name opens a connection, and this runs
after commands.  A remote name is not here by definition."
  (and dir
       (not (file-remote-p dir))
       (file-directory-p dir)))

(defun emjupy--notebook-directory-from-kernel (nb)
  "Return NB's directory as its kernel reports it, or nil."
  (let ((cwd (emjupy-notebook-kernel-cwd nb)))
    (when (and cwd (not (string-empty-p cwd)))
      (cond
       ((and emjupy-shadow-host (not (string-empty-p emjupy-shadow-host)))
        (file-name-as-directory (concat emjupy-shadow-host cwd)))
       ;; No prefix: trust the path only if it exists here, which it does
       ;; when the kernel is on this machine.  Treating a remote absolute
       ;; path as a local one is how an unrelated file gets clobbered.
       ((emjupy--local-directory-p cwd) (file-name-as-directory cwd))
       (t nil)))))

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

;;;; Warnings

(defvar-local emjupy--warned-local-server nil
  "Non-nil once this notebook has been told its language server is local.")

(defun emjupy--warn-local-language-server (nb)
  "Say that NB is about to get a language server on THIS machine.
The fallback when the server beside the kernel cannot be reached: it
reads this machine's files and Python, so imports of the user's own
modules quietly come back empty.  Said once per notebook, with the
reason the socket failed, if there is one."
  (let ((nb-buffer (or (and nb (emjupy-notebook-buffer nb)) (current-buffer))))
    (when (buffer-live-p nb-buffer)
      (with-current-buffer nb-buffer
        ;; Marked only when something is said: an early call, with nothing to
        ;; say, must not silence the one that matters.
        (unless emjupy--warned-local-server
          ;; Not conditional on the kernel's directory: a local server starts at
          ;; the first completion, before the kernel has answered.
          (progn
            (setq emjupy--warned-local-server t)
            (message
             "[emjupy] %s %s%s"
             "Using a language server on THIS machine: it cannot see the"
             "kernel's files or environment.  M-x emjupy-lsp-diagnose"
             (if (bound-and-true-p emjupy--lsp-last-failure)
                 (format " (%s)" emjupy--lsp-last-failure)
               ""))))))))

;;;; Starting language support

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
      (let* ((want-socket (and (bound-and-true-p emjupy-lsp-enabled)
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
                   (emjupy--claim-shadow-for-websocket buffer)
                   (emjupy--eglot-connect nb buffer))
              buffer))))))

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

;;;; The shadow buffer

(defvar-local emjupy--shadow-blocked-until nil
  "Time before which this buffer will not try to build a shadow buffer.")

(defun emjupy--shadow-blocked-p ()
  "Return non-nil while shadow setup is in its back-off window."
  (and emjupy--shadow-blocked-until
       (time-less-p (current-time) emjupy--shadow-blocked-until)))

(defun emjupy--shadow-block (reason &optional buffer)
  "Stop trying to build a shadow buffer for a while, reporting REASON.
BUFFER is where the pause is recorded, by default the current one.  It
must be the notebook buffer: the next request comes from there."
  (with-current-buffer (or buffer (current-buffer))
    (setq emjupy--shadow-blocked-until
          (time-add (current-time) emjupy-shadow-retry-interval)))
  (message "[emjupy] Language-server setup failed (%s); pausing %ds.  %s"
           reason emjupy-shadow-retry-interval
           "Notebook editing and execution are unaffected."))

;; The back-off covers starting a language server, the slow or impossible
;; part; building the buffer is local and always safe.

(defun emjupy--retire-moved-shadow (nb path)
  "Return NB's shadow buffer, unless it visits a file other than PATH.
Then it is killed, and nil returned.  The path can change: the kernel
reports its directory after the first buffer was made, in the temporary
fallback.  Rebuilt, so the server reads the notebook's own directory and
finds its modules."
  (let ((buf (emjupy-notebook-shadow-buffer nb)))
    (if (not (and (buffer-live-p buf)
                  (buffer-local-value 'buffer-file-name buf)
                  (not (equal (expand-file-name
                               (buffer-local-value 'buffer-file-name buf))
                              (expand-file-name path)))))
        buf
      ;; Never ask: the buffer is a regenerated copy of the cells, always
      ;; "modified".
      (with-current-buffer buf
        (set-buffer-modified-p nil))
      (let ((kill-buffer-query-functions nil)
            (kill-buffer-hook nil))
        (when (buffer-live-p buf) (kill-buffer buf)))
      (setf (emjupy-notebook-shadow-buffer nb) nil)
      nil)))

(defun emjupy--make-shadow-directory (path)
  "Make the directory PATH is in; return non-nil, or nil after backing off.
A timeout, a file error -- unreachable, refused -- or TRAMP's user error
backs off: the notebook stays open, without language support."
  (condition-case err
      (with-timeout (emjupy-shadow-timeout
                     (emjupy--shadow-block "timed out")
                     nil)
        (let ((dir (file-name-directory path)))
          (if (equal dir (emjupy--shadow-temporary-directory))
              (emjupy--private-directory dir "shadow files")
            (make-directory dir t)))
        t)
    ((file-error user-error)
     (emjupy--shadow-block (error-message-string err))
     nil)))

(defun emjupy--create-shadow-buffer (nb path content)
  "Return a buffer visiting PATH, holding CONTENT, as NB's shadow buffer."
  ;; Reuse a buffer already visiting the path: writing behind its back would
  ;; make the next visit ask to reread it.
  (let ((buf (find-buffer-visiting path)))
    (unless (buffer-live-p buf)
      ;; The content is written BEFORE the file is visited: Eglot sends
      ;; didOpen from `find-file-hook' with what the buffer holds then, and an
      ;; empty document left later notebooks without completion.
      (let ((coding-system-for-write emjupy--shadow-coding))
        (write-region content nil path nil 'quiet))
      (setq buf (let ((coding-system-for-read emjupy--shadow-coding))
                  (find-file-noselect path))))
    (setf (emjupy-notebook-shadow-buffer nb) buf)
    (with-current-buffer buf
      ;; `find-file-noselect' chose python-mode already; running it again
      ;; would kill Eglot's local state, and the two variables set below.
      (unless (derived-mode-p 'python-mode 'python-ts-mode)
        ;; Quietly: see the note in `emjupy--fontify-as'.
        (let ((python-indent-guess-indent-offset-verbose nil))
          (python-mode)))
      ;; No lock or auto-save files: the file is a generated copy nobody else
      ;; edits, and for a remote one each lock is remote I/O on the completion
      ;; path.  Set before anything modifies the buffer -- the first
      ;; modification takes the lock.
      (setq-local create-lockfiles nil)
      (setq buffer-auto-save-file-name nil)
      (auto-save-mode -1)
      ;; Saving must never stop to ask either.  NOMODIFY: this changes how
      ;; the file is written, not what it holds.
      (set-buffer-file-coding-system emjupy--shadow-coding t t)
      (emjupy-shadow-edit-mode 1)
      (setq emjupy--edit-shadow-notebook nb))
    buf))

(defun emjupy--sync-shadow-content (content)
  "Make the current buffer, a shadow buffer, hold CONTENT."
  (unless (string= content (buffer-string))
    (setq-local create-lockfiles nil)
    (erase-buffer)
    (insert content)
    ;; Not written to disk: Eglot sends changes in-band (didChange), and the
    ;; file exists only to attach to.  Writing it per edit is a round trip
    ;; over TRAMP, on the completion path, blocking cell output too.
    (when (eq emjupy-shadow-sync-to-disk 'always)
      (let ((coding-system-for-write emjupy--shadow-coding))
        (write-region (point-min) (point-max) buffer-file-name nil 'quiet))
      (set-visited-file-modtime))
    ;; The disk copy is a bootstrap, not the record: the cells are.  Say
    ;; unmodified either way, or Emacs offers to save a scratch file.
    (set-buffer-modified-p nil)))

(defun emjupy--attach-plain-eglot (nb nb-buffer)
  "Attach an ordinary Eglot to the current buffer, NB's shadow buffer.
Unless one is attached, another is being, language support is off, or
NB-BUFFER, the notebook's, is backing off."
  (condition-case err
      ;; Connected here, not with `eglot-ensure': that waits for
      ;; `post-command-hook' in the current buffer, which is never this one.
      ;; `require', not `fboundp': `eglot-ensure' may be an autoload stub,
      ;; its internals not yet defined.
      ;;
      ;; Connecting is the expensive half, retried while no server is
      ;; attached, so it is backed off like the file.  The flag is set in
      ;; the notebook buffer, where the next request comes from.
      ;; `emjupy-language-support' is checked here, where every path to a
      ;; server passes; the buffer is still built, for editing all the code,
      ;; but nothing is attached.
      (unless (or emjupy--suppress-plain-eglot
                  (not emjupy-language-support)
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

;;;; Positions

(defun emjupy--shadow-position-to-cell (nb shadow-pos)
  "Map SHADOW-POS in NB's shadow buffer to (CELL . NOTEBOOK-POS).
Return nil outside every cell section, as on a `# %%' line.  The
inverse of what `emjupy--cell-shadow-delegate' does on the way in."
  (let ((buf (emjupy-notebook-shadow-buffer nb))
        (found nil))
    (when (buffer-live-p buf)
      (cl-loop for cell across (emjupy-notebook-cells nb)
               until found
               when (eq (emjupy-cell-type cell) 'code)
               do
               (let* ((start (emjupy--shadow-section-start
                              buf (emjupy-cell-id cell)))
                      (len (length (emjupy-cell-source cell)))
                      (ov (emjupy-cell-overlay cell)))
                 (when (and start (overlayp ov)
                            (>= shadow-pos start)
                            (<= shadow-pos (+ start len)))
                   (setq found (cons cell (+ (overlay-start ov)
                                             (- shadow-pos start))))))))
    found))

;; M-. reaches the server through the shadow buffer like the rest: Eglot's xref
;; backend is installed in the shadow buffer, not in the notebook.

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
                    (spos (when line
                            (with-current-buffer buf
                              (save-excursion
                                (goto-char (point-min))
                                (forward-line (1- line))
                                (min (point-max) (+ (point) col))))))
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

(defun emjupy--shadow-offset-to-cell (nb offset)
  "Return (CELL . POSITION) for OFFSET in NB's synthetic document."
  (let ((text (emjupy--build-shadow-content nb))
        (found nil))
    (cl-loop for cell across (emjupy-notebook-cells nb)
             until found
             when (eq (emjupy-cell-type cell) 'code)
             do
             (let* ((marker (emjupy--shadow-cell-marker (emjupy-cell-id cell)))
                    (idx (string-search marker text)))
               (when idx
                 (let* ((body (+ idx (length marker) 1))
                        (len (length (or (emjupy-cell-source cell) "")))
                        (ov (emjupy-cell-overlay cell)))
                   (when (and (overlayp ov) (>= offset body)
                              (<= offset (+ body len)))
                     (setq found (list cell (+ (overlay-start ov)
                                               (- offset body)))))))))
    found))

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
The notebook is re-rendered if any  Returns the number of cells updated."
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
              (with-current-buffer nb-buf (emjupy--rerender-notebook)))))))
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

(defun emjupy-eglot-install-advice ()
  "Advise `emjupy-eglot-delegated-commands' to work inside notebooks."
  (dolist (cmd emjupy-eglot-delegated-commands)
    (advice-add cmd :around #'emjupy--eglot-command-advice)))

(defun emjupy-eglot-remove-advice ()
  "Undo `emjupy-eglot-install-advice'."
  (dolist (cmd emjupy-eglot-delegated-commands)
    (advice-remove cmd #'emjupy--eglot-command-advice)))

(emjupy-eglot-install-advice)

;;;; Committing and aborting an edit

(defun emjupy-commit-shadow-edit ()
  "Commit this shadow buffer back into the notebook.
Each `# %% [emjupy:ID]' section is written into its cell.
Sections for cells you didn't touch are written
back unchanged; the shadow buffer and its Eglot connection stay alive for
next time."
  (interactive)
  (unless emjupy--edit-shadow-notebook
    (user-error "This buffer isn't an emjupy shadow-edit buffer"))
  (let* ((nb emjupy--edit-shadow-notebook)
         (sections (emjupy--parse-shadow-sections (buffer-string)))
         (nb-buf (emjupy-notebook-buffer nb))
         (updated 0))
    (cl-loop for cell across (emjupy-notebook-cells nb)
             do (let ((match (assq (emjupy-cell-id cell) sections)))
                  (when (and match (not (string= (cdr match)
                                                 (emjupy-cell-source cell))))
                    (setf (emjupy-cell-source cell) (cdr match))
                    (setq updated (1+ updated)))))
    (set-buffer-modified-p nil)
    (when (and nb-buf (buffer-live-p nb-buf))
      (with-current-buffer nb-buf (emjupy--rerender-notebook))
      (switch-to-buffer nb-buf))
    (message "[emjupy] %d cell%s updated." updated (if (= updated 1) "" "s"))))

(defun emjupy-abort-shadow-edit ()
  "Discard uncommitted edits in the shared code buffer.
It is reverted to the cells' last-committed state, then the notebook is
switched back to.
The buffer and its Eglot connection are kept alive, not killed."
  (interactive)
  (when (y-or-n-p "Discard uncommitted edits in the shared code view? ")
    (let ((nb emjupy--edit-shadow-notebook))
      (when nb
        (erase-buffer)
        (insert (emjupy--build-shadow-content nb))
        (let ((coding-system-for-write emjupy--shadow-coding))
          (write-region (point-min) (point-max) buffer-file-name nil 'quiet))
        (set-buffer-modified-p nil)
        (when (buffer-live-p (emjupy-notebook-buffer nb))
          (switch-to-buffer (emjupy-notebook-buffer nb)))))))

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

;; Completion and eldoc while typing in the notebook itself, delegated to the
;; shadow buffer through the standard `completion-at-point-functions' and
;; `eldoc-documentation-functions', so any completion UI works.

;;;; When the shadow cannot see the kernel

(defun emjupy--shadow-would-mislead-p (nb)
  "Return non-nil when a shadow file for NB would describe the wrong machine.
The kernel's directory is not here and `emjupy-shadow-host' does not
say how to reach it, so a server would read this machine's Python and
files: plausible answers, and wrong."
  (let ((cwd (emjupy-notebook-kernel-cwd nb)))
    (and (not emjupy-shadow-when-kernel-unreachable)
         cwd
         (not (emjupy--local-directory-p cwd))
         (not (and (boundp 'emjupy-shadow-host)
                   emjupy-shadow-host
                   (not (string-empty-p emjupy-shadow-host)))))))

(defvar-local emjupy--shadow-locality-warned nil
  "Non-nil once this buffer has explained where its shadow file lives.")

(defun emjupy--warn-shadow-is-local (nb)
  "Say once that NB's shadow file, and so its language server, is local."
  (unless emjupy--shadow-locality-warned
    (setq emjupy--shadow-locality-warned t)
    (let ((cwd (emjupy-notebook-kernel-cwd nb)))
      (when (and cwd (not (emjupy--local-directory-p cwd)))
        (message "%s %s"
                 "[emjupy] The kernel runs elsewhere, so completions describe"
                 "THIS machine.  Set emjupy-shadow-host, or install jupyter-lsp on the server.")))))

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

;;;; Releasing a notebook

(defun emjupy--release-notebook ()
  "Close the connections opened by this buffer's notebook, on killing it.
Its kernel connection and language-server session are closed and its
shadow buffer killed; the kernel keeps running on the server, to be
adopted again.  A language server serving another buffer stays."
  (when-let* ((nb (bound-and-true-p emjupy--buffer-notebook)))
    (emjupy--disconnect-kernel nb)
    (let* ((sb (emjupy-notebook-shadow-buffer nb))
           (server (and (buffer-live-p sb)
                        (with-current-buffer sb (eglot-current-server)))))
      ;; The shadow buffer first: killing it is how Eglot is told the document
      ;; is closed.  Shutting the server down while it was still managed failed
      ;; flushing its changes.
      (when (buffer-live-p sb)
        (with-current-buffer sb (set-buffer-modified-p nil))
        (let ((kill-buffer-query-functions nil))
          (kill-buffer sb)))
      (when (and server
                 (jsonrpc-running-p server)
                 (null (eglot--managed-buffers server)))
        ;; Ended directly: `eglot-shutdown' first flushes changes from the
        ;; current buffer, which fails an assertion in Emacs 30.1 when that is
        ;; not a managed one, and `jsonrpc-shutdown' waits seconds for the
        ;; server to exit.  The process sentinel takes the server out of Eglot's
        ;; table.  Marked as wanted first, as `eglot-shutdown' does, or Eglot
        ;; would start a replacement.
        (setf (eglot--shutdown-requested server) t)
        ;; Answers still awaited are no longer wanted.  Left in place, each
        ;; is told "Server died" as the process goes, and Eglot's default
        ;; handler raises that as an error from the process sentinel.
        (jsonrpc-forget-pending-continuations server)
        (if (cl-typep server 'emjupy-eglot-server)
            (jsonrpc-shutdown server)
          ;; `kill-process', not `delete-process', which waits about two seconds
          ;; for the pipes to drain.
          (let ((proc (jsonrpc--process server)))
            (when (process-live-p proc)
              (kill-process proc))))
        ;; Out of Eglot's table now, not when the sentinel runs: a notebook
        ;; opened in between would be handed the dying server, which a project's
        ;; notebooks share.
        (eglot--on-shutdown server))
      (setf (emjupy-notebook-shadow-buffer nb) nil))))

;;;###autoload

;;;; Diagnosis

(defun emjupy-lsp-diagnose ()
  "Report which language server this notebook is using, and how it got there.

Reads the server Eglot has attached to the shadow buffer, which is where
the answer lives.  It used to read a slot filled in by an earlier,
hand-written client; once that client was replaced the slot stayed
empty, and this reported a socket that had never opened however well the
socket was working."
  (interactive)
  (let* ((nb (emjupy--notebook))
         (server (emjupy-notebook-server nb))
         (shadow (emjupy-notebook-shadow-buffer nb))
         (attached (and (buffer-live-p shadow)
                        (with-current-buffer shadow
                          (eglot-current-server))))
         (kind (cond
                ((null attached) "none")
                ((object-of-class-p attached 'emjupy-eglot-server)
                 "over the Jupyter WebSocket")
                (t "a local process")))
         (alive (and attached (jsonrpc-running-p attached)))
         (lines
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

(provide 'emjupy-eglot)
;;; emjupy-eglot.el ends here
