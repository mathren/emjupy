;;; emjupy-save-test.el --- Unit tests: emjupy-save.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-save.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; The undo guard

(ert-deftest emjupy-test-unsaved-changes-are-copied-and-a-save-removes-the-copy ()
  "What is typed is copied for recovery; saving it removes the copy."
  (emjupy-test--with-saving (buf nb requests lm)
    (let ((emjupy-autosave-interval nil))
      (emjupy-test--type-in-first-cell " + 41")
      (emjupy--idle-save)
      (let ((file (emjupy--recovery-file nb)))
        (should (file-exists-p file))
        (should (string-match-p "x = 1 \\+ 41"
                                (with-temp-buffer (insert-file-contents file)
                                                  (buffer-string))))
        (emjupy-save-notebook)
        (should-not (file-exists-p file))
        (should-not (buffer-modified-p))
        (should (equal (emjupy-notebook-last-modified nb) (car lm)))))))

(ert-deftest emjupy-test-a-save-never-overwrites-changes-made-elsewhere ()
  "A server copy changed since it was opened is not saved over without asking."
  (emjupy-test--with-saving (buf nb requests lm)
    (emjupy-test--type-in-first-cell " + 1")
    ;; saved from JupyterLab meanwhile
    (setcar lm "2026-06-01T00:00:00Z")
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
      (should-error (emjupy-save-notebook) :type 'user-error))
    (should-not (assoc "PUT" requests))
    (should (buffer-modified-p))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      (emjupy-save-notebook))
    (should (assoc "PUT" requests))
    (should-not (buffer-modified-p))))

(ert-deftest emjupy-test-autosave-stops-on-a-conflict-and-survives-a-dead-server ()
  "Auto-saving stops rather than overwrite, and a failure raises nothing."
  (emjupy-test--with-saving (buf nb requests lm)
    (let ((emjupy-autosave-interval 0))
      (emjupy-test--type-in-first-cell " + 1")
      (setcar lm "2026-06-01T00:00:00Z")
      (emjupy--idle-save)
      (should emjupy--autosave-paused)
      (should-not (assoc "PUT" requests))
      (setq requests nil)
      (emjupy--idle-save)
      (should-not requests)                     ; not tried again
      ;; the server unreachable: said, not raised; the copy holds the changes
      (setq emjupy--autosave-paused nil)
      (cl-letf (((symbol-function 'emjupy--http-request)
                 (lambda (&rest _) (signal 'emjupy-http-error
                                           (list "no server")))))
        (emjupy--idle-save))
      (should (buffer-modified-p))
      (should (file-exists-p (emjupy--recovery-file nb))))))

(ert-deftest emjupy-test-reopening-offers-the-unsaved-changes ()
  "A recovery copy is offered back; declined, it is kept aside, never deleted."
  (emjupy-test--with-saving (buf nb requests lm)
    (let ((noninteractive nil))
      ;; a copy no different from what was opened: deleted, nothing asked
      (emjupy--recovery-write nb)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _)
                                               (error "Asked"))))
        (emjupy--offer-recovery nb))
      (should-not (file-exists-p (emjupy--recovery-file nb)))
      ;; changes, then the notebook as the server has it again
      (emjupy-test--type-in-first-cell " + 2")
      (emjupy--recovery-write nb)
      (setf (emjupy-cell-source (aref (emjupy-notebook-cells nb) 0)) "x = 1")
      (emjupy--redraw-notebook)
      (set-buffer-modified-p nil)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
        (emjupy--offer-recovery nb))
      (should (equal (emjupy-cell-source (aref (emjupy-notebook-cells nb) 0))
                     "x = 1 + 2"))
      (should (buffer-modified-p))
      ;; declined: kept aside
      (emjupy--recovery-write nb)
      (setf (emjupy-cell-source (aref (emjupy-notebook-cells nb) 0)) "x = 1")
      (emjupy--redraw-notebook)
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
        (emjupy--offer-recovery nb))
      (should-not (file-exists-p (emjupy--recovery-file nb)))
      (should (directory-files emjupy-recovery-directory nil "-declined-")))))

(ert-deftest emjupy-test-closing-a-modified-notebook-asks ()
  "Closing a notebook with unsaved changes asks; cancelling keeps it open."
  (emjupy-test--with-saving (buf nb requests lm)
    (let ((noninteractive nil))
      (add-hook 'kill-buffer-query-functions #'emjupy--kill-buffer-query nil t)
      (emjupy-test--type-in-first-cell " + 4")
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _)
                                                           '(?c "cancel"))))
        (should-not (kill-buffer buf)))
      (should (buffer-live-p buf))
      (cl-letf (((symbol-function 'read-multiple-choice) (lambda (&rest _)
                                                           '(?k "keep"))))
        (should (emjupy--kill-buffer-query)))
      (should (file-exists-p (emjupy--recovery-file nb)))
      (remove-hook 'kill-buffer-query-functions
                   #'emjupy--kill-buffer-query t))))

(ert-deftest emjupy-test-quitting-emacs-keeps-unsaved-notebooks ()
  "As Emacs quits, every notebook with unsaved changes is copied for recovery."
  (emjupy-test--with-saving (buf nb requests lm)
    (emjupy-test--type-in-first-cell " + 5")
    (emjupy--recovery-write-all)
    (should (file-exists-p (emjupy--recovery-file nb)))))

(ert-deftest emjupy-test-recovery-directory-is-the-users-own-in-tmp ()
  "Recovery copies go to a directory of the user's own under the temporary one.
In a shared /tmp it could be made first by someone else, or be a symbolic
link to elsewhere; then no copy is written there."
  (let ((default (eval (car (get 'emjupy-recovery-directory 'standard-value)))))
    (should (string-prefix-p
             (expand-file-name temporary-file-directory) default))
    (should (string-match-p (regexp-quote (user-login-name)) default)))
  (emjupy-test--with-saving (buf nb requests lm)
    (let* ((elsewhere (make-temp-file "emjupy-elsewhere" t))
           (emjupy-recovery-directory
            (file-name-as-directory (concat (make-temp-name
                                             (expand-file-name
                                              "emjupy-link"
                                              temporary-file-directory))))))
      (unwind-protect
          (progn
            (make-symbolic-link elsewhere
                                (directory-file-name emjupy-recovery-directory))
            (emjupy-test--type-in-first-cell " + 6")
            (should-error (emjupy--recovery-write nb) :type 'file-error)
            (should-not (directory-files elsewhere nil "\\.json\\'")))
        (delete-file (directory-file-name emjupy-recovery-directory))
        (delete-directory elsewhere t)))))

(ert-deftest emjupy-test-autosave-does-not-wait-and-keeps-later-edits ()
  "Auto-save is not waited for; edits made while it is on its way stay unsaved.
Over a remote link each request is a round trip and the upload as long as
the notebook, and auto-save froze Emacs for them every two minutes."
  (emjupy-test--with-saving (buf nb requests lm)
    (let ((emjupy-autosave-interval 0) (pending nil))
      (cl-letf (((symbol-function 'emjupy--http-request-async)
                 (lambda (method _server path _body on-done _on-fail)
                   (push (list method path on-done) pending))))
        (emjupy-test--type-in-first-cell " + 1")
        (emjupy--idle-save)
        (should emjupy--autosave-in-flight)
        (should (= (length pending) 1))          ; asked, not waited for
        (emjupy--idle-save)
        (should (= (length pending) 1))          ; one at a time
        ;; the check answers: unchanged; the upload goes
        (let ((check (pop pending)) (model (make-hash-table :test 'equal)))
          (puthash "last_modified" (car lm) model)
          (funcall (nth 2 check) model))
        (should (equal (car (car pending)) "PUT"))
        ;; typed while it is on its way
        (emjupy-test--type-in-first-cell " + 2")
        (let ((upload (pop pending)) (model (make-hash-table :test 'equal)))
          (puthash "last_modified" "2026-01-02T00:00:00Z" model)
          (funcall (nth 2 upload) model))
        (should-not emjupy--autosave-in-flight)
        (should (equal (emjupy-notebook-last-modified nb)
                       "2026-01-02T00:00:00Z"))
        ;; the later edit is not saved yet
        (should (buffer-modified-p))))))

(ert-deftest emjupy-test-a-save-by-hand-waits-for-an-autosave-on-its-way ()
  "Saving by hand waits for an automatic save already sent.
Sent before, it could otherwise reach the server after, and put back
what the save by hand replaces."
  (emjupy-test--with-saving (buf nb requests lm)
    (emjupy-test--type-in-first-cell " + 3")
    (setq emjupy--autosave-in-flight t)
    (let ((b buf))
      (run-at-time
       0.3 nil (lambda () (with-current-buffer b
                            (push (list "PUT-auto" "") requests)
                            (setq emjupy--autosave-in-flight nil)))))
    (emjupy-save-notebook)
    (let ((order (mapcar #'car (reverse requests))))
      (should (equal (car order) "PUT-auto"))
      (should (member "PUT" order)))))

(provide 'emjupy-save-test)

;;; emjupy-save-test.el ends here
