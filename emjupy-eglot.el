;;; emjupy-eglot.el --- Eglot/LSP integration for emjupy code cells  -*- lexical-binding: t; -*-

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

;; Completion and eldoc inside notebook cells, delegated to a hidden
;; "shadow" buffer holding the concatenated code of every cell.  A real
;; language server sees one ordinary Python file, so a name defined in one
;; cell completes in another with no manual step.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-cells)

;; Eglot ships with Emacs (29.1+, which this package requires) but is pulled in
;; at COMPILE time only: emjupy is fully usable without a language server, so
;; nothing here loads Eglot until a shadow buffer actually asks for it -- see
;; the runtime `require' in `emjupy--ensure-shadow-buffer'.
(eval-when-compile (require 'eglot nil t))

;; Eglot's private API. Every call site below is already guarded by `fboundp'
;; or a runtime `require', but the compiler cannot see that and reports each
;; one as "not known to be defined" -- noisy under native compilation, where
;; the warnings surface in the user's *Warnings* buffer at install time.
;;
;; These are declared rather than assumed: they are internal names with no
;; stability promise, and one of them has already been renamed once
;; (`eglot--server-capable' -> `eglot-server-capable' in Emacs 30), which is
;; why `emjupy--eglot-capable-p' probes for both.
(declare-function eglot--connect "eglot")
(declare-function eglot--guess-contact "eglot")
(declare-function eglot--current-server-or-lose "eglot")
(declare-function eglot--TextDocumentPositionParams "eglot")
(declare-function eglot--hover-info "eglot")
(declare-function eglot--server-capable "eglot")
(declare-function eglot-server-capable "eglot")
(declare-function eglot-current-server "eglot")
(declare-function eglot-ensure "eglot")
(declare-function eglot-hover-eldoc-function "eglot")
(declare-function jsonrpc-request "jsonrpc")

;; emjupy-mode is a single fundamental-mode-derived buffer mixing code cells,
;; markdown cells, box-drawing decoration, and output text all interleaved --
;; nothing like the single-language file Eglot (or any tool that expects
;; `buffer-file-name'/major-mode to mean one coherent source file) needs to
;; attach to, and Eglot has no client-side support for LSP's notebookDocument
;; sync extension (checked directly: no `eglot-*notebook*' symbols exist even
;; though some servers advertise it), so there's no protocol-level shortcut.
;;
;; For CODE cells, all of the notebook's code cells are shown together in one
;; persistent, real python-mode buffer -- a "shadow" buffer, marked with
;; `# %% [emjupy:ID]' section headers (in the spirit of the jupytext percent
;; format) -- so Eglot sees one coherent multi-cell Python document and can
;; resolve names defined in any cell. Eglot is started on it automatically;
;; the buffer and its LSP connection persist across edits, so only the FIRST
;; access in a session pays the server-startup cost.
;;
;; That shadow buffer is reachable directly via `C-c '' for heavier editing,
;; but for everyday use you never need to: completion-at-point-functions and
;; eldoc-documentation-functions are wired into emjupy-mode itself, silently
;; delegating to the shadow buffer's Eglot session and mapping positions back
;; and forth -- so completion/eldoc for code cells just work while typing
;; directly in the notebook, cross-cell-aware, no action required.
;;
;; MARKDOWN cells don't benefit from cross-cell LSP awareness, so they keep
;; the simpler single-cell external-edit buffer.

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
  "Minor mode for the transient markdown-cell buffer opened by
`emjupy-edit-cell-externally'."
  :lighter " emjupy-edit"
  :keymap emjupy-cell-edit-mode-map)

(defun emjupy--edit-markdown-cell-externally (cell nb-buf)
  "Open markdown CELL from notebook buffer NB-BUF in its own edit buffer."
  (let* ((mode-fn (or (emjupy--markdown-mode-fn) 'text-mode))
         (buf (generate-new-buffer
               (format "*emjupy-cell-edit: %s[%s]*" (buffer-name nb-buf) (emjupy-cell-id cell)))))
    (with-current-buffer buf
      (insert (emjupy-cell-source cell))
      (funcall mode-fn)
      (emjupy-cell-edit-mode 1)
      (setq emjupy--edit-source-cell cell)
      (setq emjupy--edit-source-notebook-buffer nb-buf)
      (goto-char (point-min)))
    (pop-to-buffer buf)
    (message "Editing cell externally in %s -- C-c C-c to commit, C-c C-k to discard." mode-fn)))

(defun emjupy-commit-cell-edit ()
  "Commit this markdown-cell-edit buffer's text back into its cell, then close it."
  (interactive)
  (unless (and emjupy--edit-source-cell (buffer-live-p emjupy--edit-source-notebook-buffer))
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
  "Discard this markdown-cell-edit buffer's changes without committing them."
  (interactive)
  (when (y-or-n-p "Discard changes to this cell? ")
    (kill-buffer)))

;; --- Code cells: persistent multi-cell shadow buffer with Eglot ------------

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
  "Minor mode for the persistent shared-code buffer opened by
`emjupy-edit-cell-externally' for code cells. Eglot manages this
buffer like any ordinary Python file."
  :lighter " emjupy-shadow")

(defun emjupy--shadow-cell-marker (id)
  "Return the `# %% [emjupy:ID]' section-header line text for cell ID."
  (format "# %%%% [emjupy:%d]" id))

(defun emjupy--build-shadow-content (nb)
  "Concatenate every code cell in NB into one Python source, section-marked."
  (mapconcat
   (lambda (cell) (concat (emjupy--shadow-cell-marker (emjupy-cell-id cell))
                          "\n" (emjupy-cell-source cell) "\n"))
   (cl-remove-if-not (lambda (c) (eq (emjupy-cell-type c) 'code))
                      (append (emjupy-notebook-cells nb) nil))
   "\n"))

(defun emjupy--parse-shadow-sections (text)
  "Return an alist of (ID . SOURCE) parsed from TEXT's `# %% [emjupy:ID]' markers."
  (let (sections current-id current-lines)
    (dolist (line (split-string text "\n"))
      (if (string-match "\\`# %% \\[emjupy:\\([0-9]+\\)\\]\\'" line)
          (progn
            (when current-id
              (push (cons current-id (string-trim (mapconcat #'identity (nreverse current-lines) "\n")))
                    sections))
            (setq current-id (string-to-number (match-string 1 line)))
            (setq current-lines nil))
        (when current-id (push line current-lines))))
    (when current-id
      (push (cons current-id (string-trim (mapconcat #'identity (nreverse current-lines) "\n")))
            sections))
    (nreverse sections)))

(defun emjupy--shadow-file-path (nb)
  "Return a stable on-disk path for NB's shadow Python file.
The server is folded into the name: two servers can both host
`analysis.ipynb', and one shadow file cannot stand for both."
  (let* ((dir (expand-file-name "emjupy-shadow" temporary-file-directory))
         (server (emjupy-notebook-server nb))
         (tag (if server (emjupy--server-label server) "local"))
         (safe-name (replace-regexp-in-string
                     "[^A-Za-z0-9._-]" "_"
                     (format "%s__%s" tag (or (emjupy-notebook-path nb) "untitled")))))
    (make-directory dir t)
    (expand-file-name (concat safe-name ".py") dir)))

(defun emjupy--ensure-shadow-buffer (nb)
  "Get-or-create NB's persistent code shadow-buffer, refresh its content to
match the current cells, and make sure Eglot is (or becomes) attached --
automatically, with nothing for the user to run."
  (let ((buf (emjupy-notebook-shadow-buffer nb))
        (content (emjupy--build-shadow-content nb))
        (path (emjupy--shadow-file-path nb)))
    (unless (buffer-live-p buf)
      ;; A buffer may already be visiting this path from an earlier open of
      ;; the same notebook. Reuse it rather than writing the file behind its
      ;; back, which would leave a stale modtime and make the next visit
      ;; interrupt with "changed on disk. Reread from disk?".
      (setq buf (find-buffer-visiting path))
      (unless (buffer-live-p buf)
        ;; Write the real content to disk BEFORE visiting the file.
        ;;
        ;; Once any server is running for this project, Eglot auto-manages a
        ;; newly visited file from `find-file-hook' and sends didOpen with
        ;; whatever the buffer holds at that instant. Visiting first and
        ;; filling the buffer afterwards therefore hands the server a stale
        ;; (or empty) document -- which is why the SECOND notebook opened in
        ;; a session silently got no hover and almost no completions, even
        ;; though `eglot--managed-mode' reported t.
        (write-region content nil path nil 'quiet)
        (setq buf (find-file-noselect path)))
      (setf (emjupy-notebook-shadow-buffer nb) buf)
      (with-current-buffer buf
        ;; `find-file-noselect' already picked python-mode from the .py
        ;; suffix. Re-running it would `kill-all-local-variables', tearing
        ;; down both Eglot's buffer-local state and the two variables set
        ;; just below.
        (unless (derived-mode-p 'python-mode 'python-ts-mode)
          (python-mode))
        (emjupy-shadow-edit-mode 1)
        (setq emjupy--edit-shadow-notebook nb)))
    (with-current-buffer buf
      (unless (string= content (buffer-string))
        (erase-buffer)
        (insert content)
        (write-region (point-min) (point-max) buffer-file-name nil 'quiet)
        ;; Record the modtime we just created, otherwise this buffer looks
        ;; stale to Emacs forever after and every later visit prompts.
        (set-visited-file-modtime)
        (set-buffer-modified-p nil))
      (if (not (require 'eglot nil t))
          (message "[emjupy] Eglot isn't available in this Emacs (needs Emacs 29+).")
        (condition-case err
            ;; NOT eglot-ensure: it defers connecting to `post-command-hook',
            ;; added *buffer-locally* to whatever buffer was current at call
            ;; time. This shadow buffer is deliberately never the user's
            ;; focused buffer -- that's the whole point of automatic,
            ;; no-switching completion -- so that hook would never fire.
            ;; This replicates eglot-ensure's own deferred callback body
            ;; (see `eglot-ensure' in eglot.el) but runs it immediately;
            ;; that's safe here because, unlike a mode-hook, this buffer is
            ;; already fully set up (real python-mode, real file) by the
            ;; time we reach this call.
            ;;
            ;; `require' (not just `fboundp' on `eglot-ensure') matters here:
            ;; on a fresh Emacs, `eglot-ensure' exists only as an autoload
            ;; stub until something actually calls it, which loads the real
            ;; file and only then defines `eglot--guess-contact' et al. Since
            ;; we deliberately never call `eglot-ensure' itself, relying on
            ;; `fboundp' would leave those internals void the first time a
            ;; notebook is opened in a session that never ran Eglot before.
            (unless (and (boundp 'eglot--managed-mode) eglot--managed-mode)
              (apply #'eglot--connect (eglot--guess-contact)))
          (error (message "[emjupy] Eglot couldn't start automatically: %s" err)))))
    buf))

(defun emjupy--goto-shadow-section (buf cell-id)
  "Move point in BUF to the start of CELL-ID's marked section."
  (with-current-buffer buf
    (goto-char (point-min))
    (if (search-forward (emjupy--shadow-cell-marker cell-id) nil t)
        (forward-line 1)
      (goto-char (point-min)))))

(defun emjupy-commit-shadow-edit ()
  "Write each `# %% [emjupy:ID]' section in this buffer back into its cell
and refresh the notebook. Sections for cells you didn't touch are written
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
                  (when (and match (not (string= (cdr match) (emjupy-cell-source cell))))
                    (setf (emjupy-cell-source cell) (cdr match))
                    (setq updated (1+ updated)))))
    (set-buffer-modified-p nil)
    (when (and nb-buf (buffer-live-p nb-buf))
      (with-current-buffer nb-buf (emjupy--rerender-notebook))
      (switch-to-buffer nb-buf))
    (message "[emjupy] %d cell%s updated." updated (if (= updated 1) "" "s"))))

(defun emjupy-abort-shadow-edit ()
  "Discard uncommitted edits in the shared code buffer (reverting it to
match the cells' last-committed state) and switch back to the notebook.
The buffer and its Eglot connection are kept alive, not killed."
  (interactive)
  (when (y-or-n-p "Discard uncommitted edits in the shared code view? ")
    (let ((nb emjupy--edit-shadow-notebook))
      (when nb
        (erase-buffer)
        (insert (emjupy--build-shadow-content nb))
        (write-region (point-min) (point-max) buffer-file-name nil 'quiet)
        (set-buffer-modified-p nil)
        (when (buffer-live-p (emjupy-notebook-buffer nb))
          (switch-to-buffer (emjupy-notebook-buffer nb)))))))

(defun emjupy-edit-cell-externally ()
  "Edit the cell at point with real language tooling.

Code cells open a persistent, shared Python buffer containing ALL of
the notebook's code cells (marked `# %% [emjupy:ID]'), with Eglot
started automatically -- so completions, diagnostics, and go-to-def
are aware of definitions from every cell, not just this one, and
there's nothing extra for you to run. `C-c C-c' commits every section
you touched back into its cell; `C-c C-k' discards uncommitted edits.
The buffer and its Eglot connection persist, so only the first use
per session pays the language-server startup cost.

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
          (message "Editing notebook code (all cells, Eglot active) -- C-c C-c to commit, C-c C-k to discard."))
      (emjupy--edit-markdown-cell-externally cell nb-buf))))

;; --- Automatic in-place completion/eldoc: no buffer-switching needed -------
;; The pieces above (shadow buffer + Eglot) already give a *complete* editing
;; experience via `C-c ''; the functions below make its intelligence show up
;; directly while typing in a cell in the ORDINARY notebook buffer, with no
;; action from the user at all -- completion-at-point-functions and
;; eldoc-documentation-functions are standard Emacs extension points, so
;; whatever completion UI the user already has (plain M-TAB, Corfu, Company,
;; Emacs 30's completion-preview-mode, ...) picks this up automatically, the
;; same way it would for a normal, single-file Eglot-managed buffer.

(defun emjupy--shadow-section-start (buf cell-id)
  "Return the position where CELL-ID's source begins in shadow buffer BUF."
  (emjupy--goto-shadow-section buf cell-id)
  (with-current-buffer buf (point)))

(defun emjupy--cell-shadow-delegate (fn)
  "If point is in a code cell, sync + warm the shadow buffer, move an
indirect cursor there to the equivalent position, and call FN with
CELL-START, SHADOW-START, and the shadow BUFFER itself -- FN reads
`(point)' there (already positioned) to do its work. Returns FN's
value, or nil if point isn't in a code cell."
  (let ((cell (get-text-property (point) 'emjupy-cell))
        (nb emjupy--buffer-notebook))
    (when (and cell nb (eq (emjupy-cell-type cell) 'code) (emjupy-cell-overlay cell))
      (emjupy--sync-all-cells)
      (let* ((main-point (point))
             (cell-start (overlay-start (emjupy-cell-overlay cell)))
             (buf (emjupy--ensure-shadow-buffer nb))
             (shadow-start (emjupy--shadow-section-start buf (emjupy-cell-id cell))))
        (with-current-buffer buf
          ;; Clamp: if the cell's shadow section is shorter than the offset
          ;; (mid-edit, before a resync), an unclamped goto-char signals
          ;; `args-out-of-range' and kills completion for the whole buffer.
          (goto-char (max (point-min)
                          (min (point-max)
                               (+ shadow-start (- main-point cell-start)))))
          (funcall fn cell-start shadow-start buf))))))

(defun emjupy--cell-completion-at-point ()
  "`completion-at-point-functions' entry: delegate to Eglot via the
shared code shadow buffer, so completions see definitions from every
cell in the notebook, automatically."
  (emjupy--cell-shadow-delegate
   (lambda (cell-start shadow-start buf)
     (let ((result (run-hook-with-args-until-success 'completion-at-point-functions)))
       (when (consp result)
         (let* ((orig-collection (nth 2 result))
                ;; Eglot's collection is a closure the completion UI calls
                ;; again later, when `current-buffer' is back to the
                ;; notebook buffer -- but it needs the shadow buffer's own
                ;; buffer-file-name/server context (e.g. for building LSP
                ;; requests), so force that context on every invocation.
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
  (cond
   ((fboundp 'eglot-server-capable) (apply #'eglot-server-capable capabilities))
   ((fboundp 'eglot--server-capable) (apply #'eglot--server-capable capabilities))
   (t nil)))

(defun emjupy--cell-eldoc-function (callback)
  "`eldoc-documentation-functions' entry: request hover info from Eglot
directly via the shared code shadow buffer.

Deliberately does NOT delegate to `eldoc-documentation-functions' the
way completion does: Eglot's own `eglot-hover-eldoc-function' only
calls back when its buffer is visibly displayed in a window, which is
never true for this shadow buffer -- staying hidden in the background
is the whole point. `jsonrpc-request' (blocking) bypasses that gate."
  (emjupy--cell-shadow-delegate
   (lambda (_cell-start _shadow-start _buf)
     (when (and (fboundp 'eglot-current-server) (eglot-current-server)
                (fboundp 'jsonrpc-request)
                (ignore-errors (emjupy--eglot-capable-p :hoverProvider)))
       (ignore-errors
         (let* ((server (eglot--current-server-or-lose))
                (resp (jsonrpc-request server :textDocument/hover
                                        (eglot--TextDocumentPositionParams)))
                (contents (plist-get resp :contents)))
           (unless (seq-empty-p contents)
             (funcall callback (eglot--hover-info contents (plist-get resp :range))))))
       t))))

(provide 'emjupy-eglot)
;;; emjupy-eglot.el ends here
