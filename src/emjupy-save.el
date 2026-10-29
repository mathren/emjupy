;;; emjupy-save.el --- Saving notebooks, and never losing what is not saved  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Saving a notebook to its server, by hand and automatically, without
;; overwriting a copy changed elsewhere; and copies of unsaved edits kept
;; on this machine, offered back when the notebook is opened again.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tramp)
(require 'transient)
(require 'emjupy-core)
(require 'emjupy-ipynb)
(require 'emjupy-http)
(require 'emjupy-cells)
(require 'emjupy-redraw)
(require 'emjupy-mode)


;;;; Options

(defcustom emjupy-recovery-directory
  (expand-file-name (format "emjupy-recovery-%s/" (user-login-name))
                    temporary-file-directory)
  "Where notebooks with unsaved changes are copied, until saved.
A copy is written a few seconds after a change, when idle, and as Emacs
quits -- locally, so with the server gone too; reopening the notebook
offers it.  By default a directory of your own under the variable
`temporary-file-directory', /tmp/emjupy-recovery-USER/ on most systems,
often emptied on restart: for copies outliving the machine, use
\(locate-user-emacs-file \"emjupy-recovery/\").  Copies go only into a
directory you own that is not a symbolic link."
  :type 'directory
  :group 'emjupy-saving)

(defcustom emjupy-recovery-delay 3
  "Seconds of idle time before unsaved changes are copied for recovery."
  :type 'number
  :group 'emjupy-saving)

(defcustom emjupy-autosave-interval 120
  "Seconds between saves to the server of a notebook with unsaved changes.
As Jupyter does.  A save that would overwrite changes made on the server
since the notebook was opened is not made, and auto-saving that notebook
stops until it is saved by hand.  nil never saves on its own."
  :type '(choice (const :tag "Never" nil) number)
  :group 'emjupy-saving)

;;;; Saving, and never losing what is not saved

(defvar-local emjupy--autosave-paused nil
  "Non-nil when the server copy changed elsewhere and must not be saved over.")

(defvar-local emjupy--autosave-last nil
  "When this notebook was last saved, or a save last tried, as a float time.")

(defvar-local emjupy--autosave-in-flight nil
  "Non-nil while an automatic save of this notebook is on its way.")

(define-error 'emjupy-save-conflict
              "The notebook changed on the server since it was opened")

(defun emjupy--server-last-modified (nb)
  "Return when NB's copy on its server last changed, or nil if there is none."
  (condition-case err
      (let ((model (emjupy--http-request
                    "GET" (emjupy-notebook-server nb)
                    (concat (emjupy--contents-path (emjupy-notebook-path nb))
                            "?content=0"))))
        (and (hash-table-p model) (gethash "last_modified" model)))
    ;; Gone from the server: there is nothing a save would overwrite.
    (emjupy-http-status
     (if (eql (emjupy--http-status-of err) 404) nil (signal (car err)
                                                            (cdr err))))))

(defun emjupy--autosave-done (nb)
  "Mark NB's auto-save as no longer in flight."
  (let ((buf (emjupy-notebook-buffer nb)))
    (when (buffer-live-p buf)
      (with-current-buffer buf (setq emjupy--autosave-in-flight nil)))))

(defun emjupy--autosave-failed (nb err)
  "End NB's auto-save, which failed with ERR, saying so."
  (emjupy--autosave-done nb)
  (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
           (emjupy-notebook-path nb) (error-message-string err)))

(defun emjupy--autosave-upload (nb body tick)
  "Send BODY, NB as it was at its buffer's modification TICK, not waiting.
Once saved, the buffer is unmodified and the recovery copy goes -- unless
it was edited since."
  (let ((buf (emjupy-notebook-buffer nb)))
    (emjupy--http-request-async
     "PUT" (emjupy-notebook-server nb)
     (emjupy--contents-path (emjupy-notebook-path nb)) body
     (lambda (model)
       (emjupy--autosave-done nb)
       (setf (emjupy-notebook-last-modified nb)
             (and (hash-table-p model) (gethash "last_modified" model)))
       (when (buffer-live-p buf)
         (with-current-buffer buf
           ;; saved as it was when sent; edited since, it is not
           (when (eql tick (buffer-modified-tick))
             (set-buffer-modified-p nil)
             (emjupy--recovery-delete nb)))))
     (lambda (err) (emjupy--autosave-failed nb err)))))

(defun emjupy--autosave-async (nb)
  "Save NB to its server without waiting, as auto-save does.
As saving by hand -- not over a notebook changed elsewhere -- but
asynchronous: over a remote link the requests take round trips.  Edits
made meanwhile leave the notebook unsaved, for the next save."
  (let* ((buf (emjupy-notebook-buffer nb))
         (path (emjupy-notebook-path nb))
         (known (emjupy-notebook-last-modified nb))
         tick body)
    (with-current-buffer buf
      (emjupy--sync-all-cells)
      (setq tick (buffer-modified-tick))
      (setq body (json-serialize
                  (emjupy--notebook-request (emjupy--notebook-json nb))))
      (setq emjupy--autosave-in-flight t
            emjupy--autosave-last (float-time)))
    (if (not known)
        (emjupy--autosave-upload nb body tick)
      (emjupy--http-request-async
       "GET" (emjupy-notebook-server nb)
       (concat (emjupy--contents-path path) "?content=0") nil
       (lambda (model)
         (let ((current (and (hash-table-p model)
                             (gethash "last_modified" model))))
           (if (or (not current) (equal current known))
               (emjupy--autosave-upload nb body tick)
             (emjupy--autosave-done nb)
             (when (buffer-live-p buf)
               (with-current-buffer buf (setq emjupy--autosave-paused t)))
             (message "[emjupy] %s changed on the server since it was opened; not saved over.  %s"
                      path (substitute-command-keys "\\[emjupy-save-notebook] asks what to do.")))))
       (lambda (err)
         ;; gone from the server: nothing there to overwrite
         (if (eql (emjupy--http-status-of err) 404)
             (emjupy--autosave-upload nb body tick)
           (emjupy--autosave-failed nb err)))))))

(defun emjupy--save-to-server (nb &optional overwrite)
  "Save NB to its server, and return non-nil.
Unless OVERWRITE, first make sure the server copy is the one opened or
last saved here, and signal `emjupy-save-conflict' if it changed since --
saved from JupyterLab, say -- rather than overwrite that."
  (with-current-buffer (emjupy-notebook-buffer nb)
    ;; An automatic save on its way finishes first: sent before, it could
    ;; still reach the server after, and put back what this one replaces.
    (let ((deadline (+ (float-time) 15)))
      (while (and emjupy--autosave-in-flight (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (emjupy--sync-all-cells)
    (let ((known (emjupy-notebook-last-modified nb)))
      (unless overwrite
        (let ((current (and known (emjupy--server-last-modified nb))))
          (when (and current (not (equal current known)))
            (signal 'emjupy-save-conflict
                    (list (emjupy-notebook-path nb) current)))))
      (let ((body (emjupy--notebook-request (emjupy--notebook-json nb))))
        (let ((model (emjupy--http-request
                      "PUT" (emjupy-notebook-server nb)
                      (emjupy--contents-path (emjupy-notebook-path nb))
                      (json-serialize body))))
          (setf (emjupy-notebook-last-modified nb)
                (and (hash-table-p model) (gethash "last_modified" model)))))
      (set-buffer-modified-p nil)
      (setq emjupy--autosave-paused nil
            emjupy--autosave-last (float-time))
      (emjupy--recovery-delete nb)
      t)))

(defun emjupy-save-notebook ()
  "Save the notebook to its Jupyter server.
If the server copy changed since it was opened here -- saved from
JupyterLab, or by someone else -- ask before overwriting it."
  (interactive)
  (unless emjupy--buffer-notebook
    (user-error "No emjupy notebook associated with this buffer"))
  (let* ((nb emjupy--buffer-notebook)
         (path (emjupy-notebook-path nb)))
    (message "[emjupy] Saving notebook %s..." path)
    (condition-case err
        (emjupy--save-to-server nb)
      (emjupy-save-conflict
       (if (y-or-n-p (format "%s changed on the server since it was opened here (%s).  Overwrite it? "
                                path (nth 2 err)))
           (emjupy--save-to-server nb t)
         (user-error "Not saved: your changes are kept here, and in a recovery copy"))))
    ;; A checkpoint, as JupyterLab makes on saving: what
    ;; `emjupy-revert-to-checkpoint' goes back to.  Not for automatic saves;
    ;; not waited for, a round trip on a slow link, but said if it fails.
    (emjupy--http-request-async
     "POST" (emjupy-notebook-server nb)
     (concat (emjupy--contents-path path) "/checkpoints") nil
     #'ignore
     (lambda (err) (message "[emjupy] No checkpoint made: %S" err)))
    (message "[emjupy] Saved %s" path)))

;;;; Copies of unsaved edits

(defvar-local emjupy--recovery-tick nil
  "`buffer-modified-tick' when the recovery copy was last written.")

(defvar emjupy--idle-save-timer nil
  "Timer writing recovery copies and saving, when Emacs is idle.")

(defun emjupy--recovery-file (nb)
  "Return the file NB's recovery copy is written to."
  (expand-file-name
   (concat (md5 (format "%s|%s"
                        (emjupy-server-base-url (emjupy-notebook-server nb))
                        (emjupy-notebook-path nb)))
           ".json")
   emjupy-recovery-directory))

(defun emjupy--recovery-write (nb)
  "Copy NB, as it is in its buffer, to its recovery file.
Written whole to a temporary file and renamed, so a copy is never half
written; readable by its owner only, as the notebook may hold anything."
  (with-current-buffer (emjupy-notebook-buffer nb)
    (emjupy--sync-all-cells)
    (let ((file (emjupy--recovery-file nb))
          (record (concat "{\"server\":" (json-serialize
                                          (emjupy-server-base-url
                                           (emjupy-notebook-server nb)))
                          ",\"path\":"
                          (json-serialize (emjupy-notebook-path nb))
                          ",\"written\":"
                          (json-serialize (format-time-string "%F %T"))
                          ",\"notebook\":" (emjupy--serialize-notebook nb)
                          "}")))
      (emjupy--private-directory (file-name-directory file) "recovery copies")
      (let ((partial (with-file-modes #o600
                       (make-temp-file (expand-file-name
                                        "partial-"
                                        (file-name-directory file))))))
        (let ((coding-system-for-write 'no-conversion))
          (write-region record nil partial nil 'quiet))
        (rename-file partial file t))
      (setq emjupy--recovery-tick (buffer-modified-tick))
      file)))

(defun emjupy--recovery-delete (nb)
  "Delete NB's recovery copy, if there is one."
  (let ((file (emjupy--recovery-file nb)))
    (when (file-exists-p file) (delete-file file))))

(defun emjupy--modified-notebook-buffers ()
  "Return the notebook buffers with unsaved edits."
  (seq-filter (lambda (b) (with-current-buffer b
                            (and emjupy--buffer-notebook (buffer-modified-p))))
              (emjupy--notebook-buffers)))

(defun emjupy--idle-save ()
  "Copy unsaved edits for recovery, and save to the server when due.
Run when Emacs is idle, so that neither interrupts typing.  A failure is
said and left: the recovery copy, written first, keeps the changes."
  (dolist (buf (emjupy--modified-notebook-buffers))
    (with-current-buffer buf
      (let ((nb emjupy--buffer-notebook))
        (unless (eql emjupy--recovery-tick (buffer-modified-tick))
          (condition-case err
              (emjupy--recovery-write nb)
            (file-error (message "[emjupy] Could not write a recovery copy of %s: %s"
                                 (emjupy-notebook-path nb)
                                 (error-message-string err)))))
        (when (and emjupy-autosave-interval (not emjupy--autosave-paused)
                   (not emjupy--autosave-in-flight)
                   (>= (- (float-time) (or emjupy--autosave-last 0))
                       emjupy-autosave-interval))
          ;; Not waited for: it says itself how it went.  Starting it can
          ;; still fail -- serializing, or a cookie to fetch first.
          (condition-case err
              (emjupy--autosave-async nb)
            (file-error
             (setq emjupy--autosave-in-flight nil)
             (message "[emjupy] Could not save %s (%s); a recovery copy is kept."
                      (emjupy-notebook-path nb)
                      (error-message-string err)))))))))

(defun emjupy--recovery-write-all ()
  "Copy every notebook with unsaved edits for recovery, as Emacs quits."
  (dolist (buf (emjupy--modified-notebook-buffers))
    (with-current-buffer buf
      (condition-case err
          (emjupy--recovery-write emjupy--buffer-notebook)
        (file-error (message "[emjupy] Could not write a recovery copy: %s"
                             (error-message-string err)))))))

;;;; Asking before closing

(defun emjupy--kill-buffer-query ()
  "Ask before closing a notebook with unsaved edits.
Return non-nil when it may be closed."
  (or (not emjupy--buffer-notebook) (not (buffer-modified-p)) noninteractive
      (let ((nb emjupy--buffer-notebook))
        (pcase (car (read-multiple-choice
                     (format "%s has unsaved changes" (emjupy-notebook-path nb))
                     '((?s "save" "Save it to the server, then close it")
                       (?k "keep"
                           "Close it; the changes stay in a recovery copy")
                       (?d "discard" "Close it and discard the changes")
                       (?c "cancel" "Do not close it"))))
          (?s (emjupy-save-notebook) t)
          (?k (emjupy--recovery-write nb) t)
          (?d (emjupy--recovery-delete nb) t)
          (_ nil)))))

(defun emjupy--kill-emacs-query ()
  "Offer to save notebooks with unsaved edits before Emacs quits.
Non-nil lets it quit.  Quitting without saving keeps them in recovery
copies, written as Emacs quits."
  (let ((modified (emjupy--modified-notebook-buffers)))
    (or (null modified) noninteractive
        (pcase (car (read-multiple-choice
                     (format "%d notebook%s with unsaved changes"
                             (length modified)
                             (if (cdr modified) "s" ""))
                     '((?s "save" "Save them to their servers, then quit")
                       (?q "quit" "Quit; the changes stay in recovery copies")
                       (?c "cancel" "Do not quit"))))
          (?s (dolist (buf modified)
                (with-current-buffer buf
                  ;; A notebook that cannot be saved is said, and kept in a
                  ;; recovery copy; the others are still saved.
                  (condition-case err
                      (emjupy-save-notebook)
                    ((file-error user-error)
                     (message "[emjupy] %s not saved: %s" (buffer-name)
                              (error-message-string err))))))
              t)
          (?q t)
          (_ nil)))))

;;;; Restoring a copy

(defun emjupy--offer-recovery (nb)
  "Offer to restore unsaved edits to NB from its recovery copy.
A copy no different from what was opened is deleted.  One declined is
kept aside, never deleted, in the recovery directory."
  (let ((file (emjupy--recovery-file nb)))
    (when (file-exists-p file)
      (let* ((record (json-parse-string
                      (with-temp-buffer
                        (let ((coding-system-for-read 'utf-8))
                          (insert-file-contents file))
                        (buffer-string))
                      :object-type 'hash-table :array-type 'array))
             (saved (json-serialize (gethash "notebook" record))))
        (cond
         ((equal saved (emjupy--serialize-notebook nb))
          (delete-file file))
         ((or noninteractive
              (not (y-or-n-p (format "Unsaved changes to %s from %s were kept.  Restore them? "
                                     (emjupy-notebook-path nb)
                                     (gethash "written" record)))))
          (let ((aside (concat (file-name-sans-extension file)
                               (format-time-string "-declined-%Y%m%d-%H%M%S.json"))))
            (rename-file file aside t)
            (message "[emjupy] The unsaved changes are kept in %s." aside)))
         (t
          (let ((restored (emjupy--parse-ipynb saved)))
            (with-current-buffer (emjupy-notebook-buffer nb)
              (setf (emjupy-notebook-cells nb) (emjupy-notebook-cells restored)
                    (emjupy-notebook-metadata nb)
                    (emjupy-notebook-metadata restored))
              (emjupy--redraw-notebook)
              (set-buffer-modified-p t)
              (message "[emjupy] Unsaved changes restored; %s"
                       (substitute-command-keys "\\[emjupy-save-notebook] saves them."))))))))))

(defun emjupy-recover-notebook ()
  "Restore unsaved edits to this notebook from its recovery copy."
  (interactive)
  (let ((nb (emjupy--notebook)))
    (unless (file-exists-p (emjupy--recovery-file nb))
      (user-error "No recovery copy of %s" (emjupy-notebook-path nb)))
    (emjupy--offer-recovery nb)))

;;;; Turning it on

(defun emjupy--save-machinery-start ()
  "Start copying and saving unsaved edits, and asking before losing them.
Started when the first notebook is opened, so that loading emjupy changes
nothing."
  (unless (timerp emjupy--idle-save-timer)
    (setq emjupy--idle-save-timer
          (run-with-idle-timer emjupy-recovery-delay t #'emjupy--idle-save)))
  (add-hook 'kill-emacs-query-functions #'emjupy--kill-emacs-query)
  (add-hook 'kill-emacs-hook #'emjupy--recovery-write-all))

(provide 'emjupy-save)

;;; emjupy-save.el ends here
