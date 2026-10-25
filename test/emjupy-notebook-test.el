;;; emjupy-notebook-test.el --- Unit tests: emjupy-notebook.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-notebook.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-notebook-buffer-name-includes-server ()
  "The same notebook path can exist on two servers; one buffer cannot
represent both."
  (let ((a (make-emjupy-server :base-url "localhost:18888"))
        (b (make-emjupy-server :base-url "localhost:19999")))
    (should-not (equal (emjupy--notebook-buffer-name "analysis.ipynb" a)
                       (emjupy--notebook-buffer-name "analysis.ipynb" b)))))

;;;; Login: one port, one kernel

(ert-deftest emjupy-test-login-normalizes-bare-port ()
  "A bare port means localhost, which is what an `ssh -L' forward gives you."
  (should (equal (emjupy--normalize-url "8888") "localhost:8888"))
  (should (equal (emjupy--normalize-url "localhost:8888") "localhost:8888"))
  (should (equal (emjupy--normalize-url "https://example.org/jup")
                 "https://example.org/jup")))

(ert-deftest emjupy-test-login-does-not-prompt-for-token-when-unneeded ()
  "Many tunnelled servers run token-less. Asking every time is what made
logging into a second tunnel tedious."
  (cl-letf (((symbol-function 'emjupy--server-reachable-p) (lambda (&rest _) t))
            ((symbol-function 'read-string)
             (lambda (&rest _)
               (error "must not prompt when no token is needed"))))
    (should (equal (emjupy--resolve-token "localhost:18888" nil) "")))
  ;; but it does ask when the server rejects an empty token
  (cl-letf (((symbol-function 'emjupy--server-reachable-p)
             (lambda (&rest _) nil))
            ((symbol-function 'read-string) (lambda (&rest _) "typed-token")))
    (should (equal (emjupy--resolve-token "localhost:18888" nil)
                   "typed-token")))
  ;; an explicit token always wins
  (should (equal (emjupy--resolve-token "localhost:18888" "explicit")
                 "explicit")))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-token-is-not-echoed ()
  "Tokens are read without echoing.  `read-string' prints what is typed
straight into the minibuffer, so the credential ends up on screen."
  (let ((used nil))
    (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _)
                                                (setq used 'passwd) "s3cret"))
              ((symbol-function 'read-string) (lambda (&rest _)
                                                (setq used 'plain) "leaked")))
      (should (equal (emjupy--read-token "Token: ") "s3cret"))
      (should (eq used 'passwd))))
  ;; and the prompt reached from resolve-token goes the same way
  (let ((used nil))
    (cl-letf (((symbol-function 'emjupy--server-reachable-p)
               (lambda (&rest _) nil))
              ((symbol-function 'read-passwd) (lambda (&rest _)
                                                (setq used 'passwd) "tok"))
              ((symbol-function 'read-string) (lambda (&rest _)
                                                (setq used 'plain) "leaked")))
      (should (equal (emjupy--resolve-token "localhost:8888" nil) "tok"))
      (should (eq used 'passwd)))))

(ert-deftest emjupy-test-token-has-no-history ()
  "A token must not be put into any minibuffer history: that would place
the secret back on screen, and into savehist for anyone who persists it."
  (let ((seen 'unset))
    (cl-letf (((symbol-function 'read-passwd)
               (lambda (&rest args) (setq seen args) "tok")))
      (emjupy--read-token "Token: ")
      ;; read-passwd takes no history argument at all
      (should (= (length seen) 1)))))

(ert-deftest emjupy-test-prompt-histories-are-separate ()
  "Ports, notebooks and kernels each keep their own history, so a port
does not turn up while completing a notebook name.

Asserted by driving the prompts rather than reading `interactive-form':
once the package is byte-compiled -- which is what any archive does --
that form is a byte-code object and nothing can be read out of it."
  (should (boundp 'emjupy--port-history))
  (should (boundp 'emjupy--notebook-history))
  (should (boundp 'emjupy--kernel-history))
  (let ((hist 'unset))
    (cl-letf (((symbol-function 'read-string)
               (lambda (_p &optional _i h &rest _) (setq hist h) "8888"))
              ((symbol-function 'emjupy--resolve-token) (lambda (&rest _) ""))
              ((symbol-function 'emjupy--intern-server)
               (lambda (&rest _)
                 (make-emjupy-server :base-url "localhost:8888")))
              ((symbol-function 'emjupy--http-request) (lambda (&rest _) nil))
              ((symbol-function 'emjupy--harvest-xsrf) (lambda (&rest _) nil))
              ((symbol-function 'emjupy--bind-server-kernel) (lambda (&rest _)
                                                               "k"))
              ((symbol-function 'emjupy-list-notebooks) (lambda (&rest _) nil)))
      (ignore-errors (call-interactively 'emjupy-login))
      (should (eq hist 'emjupy--port-history))))
  (let ((hist 'unset)
        (server (make-emjupy-server :base-url "localhost:8888" :token "t")))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (&rest _) (make-hash-table :test 'equal)))
              ((symbol-function 'completing-read)
               (lambda (_p _c &optional _pr _rm _ii h &rest _)
                 (setq hist h) "[Create New Notebook]"))
              ((symbol-function 'emjupy-create-notebook) (lambda (&rest _) nil))
              ((symbol-function 'emjupy-new-notebook) (lambda (&rest _) nil)))
      (ignore-errors (emjupy-list-notebooks server))
      (should (eq hist 'emjupy--notebook-history)))))

(ert-deftest emjupy-test-known-token-is-not-reprobed ()
  "A token already registered for a server is used as it stands.

Re-probing to confirm it costs a request whose failure is then reported
as an error the user did not cause -- which is why every login appeared
to fail once before succeeding."
  (let ((probes 0)
        (emjupy--servers (make-hash-table :test 'equal)))
    (puthash "localhost:9999"
             (make-emjupy-server :base-url "localhost:9999" :token "good")
             emjupy--servers)
    (cl-letf (((symbol-function 'emjupy--server-reachable-p)
               (lambda (&rest _) (setq probes (1+ probes)) t)))
      (should (equal (emjupy--resolve-token "localhost:9999" nil) "good"))
      (should (= probes 0)))))

(ert-deftest emjupy-test-login-recovers-from-a-refused-token ()
  "A refused token is asked for once and the login continues.

Before, the 403 ended the command, so logging in took two goes: the
first reported the refusal and stopped, the second asked and worked."
  (let ((asked 0) (attempts 0))
    (cl-letf (((symbol-function 'emjupy--normalize-url) (lambda (u) u))
              ((symbol-function 'emjupy--resolve-token) (lambda (&rest _)
                                                          "wrong"))
              ((symbol-function 'emjupy--harvest-xsrf) (lambda (&rest _) nil))
              ((symbol-function 'emjupy--read-token)
               (lambda (&rest _) (setq asked (1+ asked)) "right"))
              ((symbol-function 'emjupy-list-notebooks)
               (lambda (&rest _) 'done))
              ((symbol-function 'emjupy--bind-server-kernel)
               (lambda (server)
                 (setq attempts (1+ attempts))
                 (if (equal (emjupy-server-token server) "right")
                     "kernel-1"
                   (signal 'emjupy-http-status
                           (list "[Jupyter HTTP 403] GET: Forbidden"))))))
      (should (eq (emjupy-login "localhost:9999" nil) 'done))
      (should (= asked 1))
      (should (= attempts 2)))))

;;;; Performance

(ert-deftest emjupy-test-empty-answer-means-the-default-port ()
  "Answering nothing at the port prompt accepts the default.

There is little point making someone type the number Jupyter would have
used anyway, and the prompt says which it is."
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "")))
    (should (equal (emjupy--read-server-url) emjupy-default-port)))
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "   ")))
    (should (equal (emjupy--read-server-url) emjupy-default-port)))
  ;; and an answer is still an answer
  (cl-letf (((symbol-function 'read-string) (lambda (&rest _) "9999")))
    (should (equal (emjupy--read-server-url) "9999"))))

;;;; The undo guard

(ert-deftest emjupy-test-export-py-does-not-take-a-failure-for-absence ()
  "Export asks before overwriting; a failed check is not \"not there\".
The check took any failure -- a 403, a server error -- for a file that
does not exist, so the export overwrote it without asking.  Now a 404 is
\"not there\" and anything else is raised, and nothing is written."
  (let* ((server (make-emjupy-server :base-url "localhost:8888" :token "t"))
         (cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                          :type 'code :source "x = 1"
                                          :outputs []
                                          :metadata (make-hash-table)))))
    (dolist (case '((403 . refused) (404 . written)))
      (let ((wrote nil))
        (emjupy-test--with-notebook cells buf nb
          (with-current-buffer buf
            (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
            (setf (emjupy-notebook-server emjupy--buffer-notebook) server)
            (cl-letf (((symbol-function 'read-string) (lambda (_p d &rest _) d))
                      ((symbol-function 'y-or-n-p)
                       (lambda (&rest _)
                         (error "Should not ask: the file is not there")))
                      ((symbol-function 'emjupy--http-request)
                       (lambda (method _s path &rest _)
                         (if (equal method "GET")
                             (signal 'emjupy-http-status
                                     (list (format "[Jupyter HTTP %d] GET %s"
                                                   (car case) path)))
                           (setq wrote path)))))
              (ert-info ((format "%d" (car case)))
                (if (eq (cdr case) 'refused)
                    (progn (should-error (emjupy-export-py)
                                         :type 'emjupy-http-status)
                           (should-not wrote))
                  (emjupy-export-py)
                  (should (equal wrote "/api/contents/nb.py")))))))))))

(ert-deftest emjupy-test-opening-an-open-notebook-keeps-its-changes ()
  "Opening a notebook open with unsaved changes shows it, and fetches nothing.
It was fetched again, and its cells replaced by the server's."
  (emjupy-test--with-saving (buf nb requests lm)
    (rename-buffer (emjupy--notebook-buffer-name "work.ipynb"
                                                 (emjupy-notebook-server nb)) t)
    (emjupy-test--type-in-first-cell " + 3")
    (should (eq (emjupy-open-notebook "work.ipynb"
                                      (emjupy-notebook-server nb)) buf))
    (should-not requests)
    (should (string-match-p "x = 1 \\+ 3" (buffer-string)))))

(ert-deftest emjupy-test-the-menu-names-its-notebook ()
  "The menu's title names the notebook it was opened from.
Transient computes the title in a temporary buffer, so the notebook
buffer is found through `transient--original-buffer'; looked for in the
current buffer, the title never named a notebook."
  (let ((notebook (get-buffer-create " *emjupy-menu-title*")))
    (unwind-protect
        (progn
          (with-current-buffer notebook
            (setq-local emjupy--buffer-notebook
                        (make-emjupy-notebook :path "analysis/run.ipynb")))
          (with-temp-buffer
            (let ((transient--original-buffer notebook))
              (should (equal (emjupy--menu-title)
                             "emjupy  analysis/run.ipynb")))
            (let ((transient--original-buffer nil))
              (should (equal (emjupy--menu-title) "emjupy")))))
      (kill-buffer notebook))))

(provide 'emjupy-notebook-test)

;;; emjupy-notebook-test.el ends here
