;;; emjupy-shadow.el --- The shadow buffer: all of a notebook's code as one Python file  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A language server reads files, and a notebook is not one: its code
;; cells are kept in a hidden "shadow" buffer, one Python file, each cell
;; after a marker naming it, with its file beside the notebook or in a
;; temporary directory.  Here: the shadow file and buffer, mapping
;; positions between cells and the shadow, and the buffers for editing one
;; cell or all the code.  Attaching Eglot to the shadow, and what the
;; language server answers, are in emjupy-eglot.el.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-redraw)
(require 'emjupy-lsp)
(require 'xref)
(require 'emjupy-kernel)
(require 'emjupy-remote)

(defvar python-indent-guess-indent-offset-verbose)


;;;; Options

(defcustom emjupy-shadow-directory nil
  "Directory holding the Eglot shadow files, or nil for a local temp dir.
A TRAMP path runs the language server on that machine, as Eglot starts
the server its file's directory implies:

  (setq emjupy-shadow-directory \"/ssh:user@host:/tmp/emjupy-shadow\")

For a remote Jupyter server, a local language server sees local packages,
not the kernel's.  A setting, not inferred: behind `ssh -L' the server
looks like localhost."
  :type '(choice (const :tag "Local temporary directory" nil)
                 (directory :tag "Directory (may be a TRAMP path)"))
  :group 'emjupy-advanced)

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
  :group 'emjupy-advanced)

(defcustom emjupy-shadow-host nil
  "TRAMP prefix reaching the machine the kernel runs on, or nil.
Needed only for a kernel elsewhere: the directory it reports, say
`/srv/nb/sub', names a path on ITS machine, which here would be a local
one.  So it is used only when addressable:

  (setq emjupy-shadow-host \"/ssh:box:\")

gives `/ssh:box:/srv/nb/sub', where Eglot starts the language server on
`box', in the kernel's environment.  Unset, a remote kernel's shadow
goes to a local temporary directory: \\[xref-find-definitions] misses
your own modules, but nothing is written to a wrong path."
  :type '(choice (const :tag "Kernel runs on this machine" nil)
                 (string :tag "TRAMP prefix, e.g. /ssh:box:"))
  :group 'emjupy-advanced)

(defcustom emjupy-shadow-beside-notebook t
  "When non-nil, keep the shadow file in the notebook's own directory.
A language server resolves `import mylib' against the file's directory,
so this is what lets \\[xref-find-definitions] reach the modules beside
the notebook.  The directory is the kernel's working directory, or
`emjupy-remote-root''s; a remote one is used only through
`emjupy-shadow-host'.  No effect when the shadow must stay on this
machine and the notebook is on another: \"beside\" would make a local
directory named after a remote path."
  :type 'boolean
  :group 'emjupy-advanced)

(define-obsolete-variable-alias 'emjupy-shadow-retry-interval
  'emjupy-lsp-retry-interval "0.3"
  "One wait before trying a language server again, whichever failed.")

(defcustom emjupy-shadow-timeout 2
  "Seconds to let shadow-buffer setup block Emacs before giving up.

Reached from eldoc and completion, so this is time the user spends
waiting after an ordinary command.  Language-server support is worth a
short pause and no more; editing and running cells do not depend on it."
  :type 'number
  :group 'emjupy-advanced)

(defcustom emjupy-shadow-when-kernel-unreachable t
  "Non-nil means a local language server runs when the kernel is elsewhere.

The shadow file then sits on THIS machine, so what the server describes
is this machine: its interpreter, its packages, and none of the modules
that live beside the notebook.  For ordinary Python that is mostly
right and worth having; for anything that depends on the environment the
code will actually run in, it is confidently wrong.

Set it to nil to have no language support at all in that case rather
than approximate support.  Either way, `emjupy-shadow-host' or
`jupyter-lsp' on the server gives answers from the right machine."
  :type 'boolean
  :group 'emjupy-advanced)

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
      (emjupy--redraw-notebook cell))
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

(defun emjupy--shadow-stays-here-p (nb)
  "Return non-nil if NB's shadow file belongs on this machine.
Unless NB's route is `local': the server's language server is sent the
text, and never reads the file -- built over TRAMP, it would only be
slow -- and while the route is undecided, nothing reads it yet.  Built
beside a remote notebook then, it was written over TRAMP, only to move
here once the socket route was found, and the server connected to it
went with it.  Only the fallback, an ordinary Eglot, reads it there."
  (not (eq (emjupy-notebook-lsp-route nb) 'local)))

(defun emjupy--shadow-must-stay-here-p (nb)
  "Non-nil when NB's shadow belongs on this machine but its files do not.

True exactly when the WebSocket transport is in use and the notebook is
somewhere this machine cannot reach as a plain path."
  (and (emjupy--shadow-stays-here-p nb)
       (let ((dir (emjupy--notebook-directory nb)))
         (and dir (not (emjupy--local-directory-p dir))))))

(defun emjupy--shadow-directory-for (nb)
  "Return the directory NB's shadow file belongs in."
  ;; Kept here rejects REMOTE candidates only.  A directory set by hand
  ;; that is already on this machine is still what the user asked for.
  (let ((here (emjupy--shadow-stays-here-p nb)))
    (cl-flet ((usable (dir)
                (and dir
                     (not (and here (file-remote-p dir)))
                     dir)))
      (let* ((explicit (usable (and emjupy-shadow-directory
                                    (file-name-as-directory
                                     emjupy-shadow-directory))))
             ;; Not beside a notebook that lives elsewhere when the shadow must
             ;; stay here: that would make a local directory named after a
             ;; remote
             ;; path.  The socket transport never reads the file, so a temporary
             ;; directory serves.
             (beside (and (not explicit)
                          emjupy-shadow-beside-notebook
                          (not (emjupy--shadow-must-stay-here-p nb))
                          (usable (emjupy--notebook-directory nb)))))
        (or explicit beside (emjupy--shadow-temporary-directory))))))

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
          (time-add (current-time) emjupy-lsp-retry-interval)))
  (message "[emjupy] Language-server setup failed (%s); pausing %ds.  %s"
           reason emjupy-lsp-retry-interval
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
      ;; No lock or auto-save files: the file is a generated copy, and for a
      ;; remote one each lock is remote I/O.  Set before the first modification,
      ;; which takes the lock.
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
      (with-current-buffer nb-buf (emjupy--redraw-notebook))
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
        ;; Ended directly: `eglot-shutdown' fails an assertion in Emacs 30.1
        ;; outside a managed buffer, and `jsonrpc-shutdown' waits seconds.
        ;; Marked as wanted first, or Eglot would start a replacement.
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

;;;; In a notebook buffer

(defun emjupy-shadow-enable ()
  "Close what the current notebook opened when its buffer is killed."
  (add-hook 'kill-buffer-hook #'emjupy--release-notebook nil t))

(provide 'emjupy-shadow)

;;; emjupy-shadow.el ends here
