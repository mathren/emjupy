;;; emjupy-tramp-open-test.el --- Unit tests: emjupy-tramp-open.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-tramp-open.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; The undo guard

(ert-deftest emjupy-test-environment-yml-names-the-environment ()
  "The environment offered is the one an environment.yml beside the notebook names."
  (let ((dir (make-temp-file "emjupy-env" t)))
    (unwind-protect
        (cl-flet ((yml (file text)
                    (dolist (f '("environment.yml" "environment.yaml"))
                      (let ((p (expand-file-name f dir)))
                        (when (file-exists-p p) (delete-file p))))
                    (with-temp-file (expand-file-name file dir) (insert text))))
          (should-not (emjupy--environment-yml-name dir))
          (yml "environment.yml" "name: analysis\ndependencies:\n  - numpy\n")
          (should (equal (emjupy--environment-yml-name dir) "analysis"))
          (yml "environment.yml"
               "# the project\nname: \"my-env\"   # used by CI\n")
          (should (equal (emjupy--environment-yml-name dir) "my-env"))
          (yml "environment.yaml" "channels:\n  - conda-forge\nname: 'other'\n")
          (should (equal (emjupy--environment-yml-name dir) "other")))
      (delete-directory dir t))))

(ert-deftest emjupy-test-an-environment-gives-its-jupyter ()
  "An environment is found by its directory, its conda name, or the PATH."
  (let* ((venv (make-temp-file "emjupy-venv" t))
         (jupyter (expand-file-name "bin/jupyter" venv)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory jupyter) t)
          (with-temp-file jupyter (insert "#!/bin/sh\n"))
          (set-file-modes jupyter #o755)
          (should (equal (emjupy--environment-jupyter venv) jupyter))
          (cl-letf (((symbol-function 'emjupy--conda-environments)
                     (lambda (&rest _) (list (cons "analysis" venv)
                                             (cons "bare" "/nonexistent")))))
            (should (equal (emjupy--environment-jupyter "analysis") jupyter))
            (should-error (emjupy--environment-jupyter "bare")
                          :type 'user-error)
            (should-error (emjupy--environment-jupyter "nowhere")
                          :type 'user-error))
          (cl-letf (((symbol-function 'executable-find) (lambda (&rest _)
                                                          "/usr/bin/jupyter")))
            (should (equal (emjupy--environment-jupyter "")
                           "/usr/bin/jupyter")))
          (delete-file jupyter)
          (should-error (emjupy--environment-jupyter venv) :type 'user-error))
      (delete-directory venv t))))

(ert-deftest emjupy-test-conda-environments-are-named-as-conda-names-them ()
  "A named environment is its directory under envs/; the other is base."
  (cl-letf (((symbol-function 'executable-find) (lambda (p &rest _)
                                                  (and (equal p "conda")
                                                       "/opt/conda/bin/conda")))
            ((symbol-function 'call-process)
             (lambda (&rest _)
               (insert "{\"envs\": [\"/opt/conda\", \"/opt/conda/envs/analysis\", \"/home/me/.conda/envs/ml\"]}")
               0)))
    (should (equal (emjupy--conda-environments)
                   '(("base" . "/opt/conda") ("analysis" .
                                              "/opt/conda/envs/analysis")
                     ("ml" . "/home/me/.conda/envs/ml"))))))

(ert-deftest emjupy-test-a-notebook-through-tramp-has-an-ssh-route ()
  "A TRAMP name gives the ssh route to its machine, every hop as ssh -J."
  (let ((tramp-default-proxies-alist nil))
    (should (equal (emjupy--ssh-route "/ssh:me@box#2200:/home/me/nb.ipynb")
                   '(:host "box" :user "me" :port "2200" :jumps nil)))
    (should (equal (emjupy--ssh-route "/ssh:bastion|ssh:me@inner#2200|ssh:node7:/data/nb.ipynb")
                   '(:host "node7" :user nil :port nil
                         :jumps "bastion,me@inner:2200")))
    (should (equal (emjupy--remote-prefix "/ssh:bastion|ssh:node7:/data/nb.ipynb")
                   "/ssh:bastion|ssh:node7:"))
    (should-error (emjupy--ssh-route "/ssh:box|sudo:root@box:/root/nb.ipynb")
                  :type 'user-error)
    (should (equal (emjupy--tunnel-command '(:host "node7" :user "me" :port "22"
                                                 :jumps "bastion") 9000 8888)
                   '("ssh" "-N" "-o" "BatchMode=yes" "-o"
                   "ExitOnForwardFailure=yes"
                   "-L" "9000:127.0.0.1:8888" "-J" "bastion" "-p" "22"
                   "me@node7"))))
  ;; once TRAMP has been through a hop, it names the file without it:
  ;; the route still has it
  (let ((tramp-default-proxies-alist '(("^node7$" nil "/ssh:bastion:"))))
    (should (equal (plist-get (emjupy--ssh-route "/ssh:node7:/data/nb.ipynb")
                              :jumps) "bastion"))
    (should (equal (emjupy--remote-prefix "/ssh:node7:/data/nb.ipynb")
                   "/ssh:bastion|ssh:node7:"))))

(ert-deftest emjupy-test-a-running-remote-server-is-found-again ()
  "A server already serving a notebook's folder, or one above it, is used again.
That is how a notebook opened on its machine gets back to the server, and
the kernels, it left running."
  (let ((listed (concat "{\"port\": 8888, \"token\": \"a\", \"root_dir\": \"/data\"}\n"
                        "not json at all\n"
                        "{\"port\": 8889, \"token\": \"b\", \"root_dir\": \"/data/project\"}\n"
                        "{\"port\": 8890, \"token\": \"c\", \"root_dir\": \"/elsewhere\"}")))
    (should (equal (plist-get (emjupy--remote-server-for listed "/data/project/analysis")
                              :port)
                   8889))
    (should (equal (plist-get (emjupy--remote-server-for listed "/data/other")
                              :port) 8888))
    (should-not (emjupy--remote-server-for listed "/home/me"))
    (should-not (emjupy--remote-server-for "" "/data"))))

(ert-deftest emjupy-test-a-tunnel-wanting-a-password-says-so ()
  "A tunnel whose login wants a password says how to log in without one."
  (let ((emjupy--remote-tunnels nil))
    (cl-letf (((symbol-function 'emjupy--tunnel-command)
               (lambda (&rest _)
                 (list "sh"
                       "-c" "echo 'me@box: Permission denied (publickey,password).' >&2; exit 255"))))
      (let ((err (should-error (emjupy--open-tunnel '(:host "box") 8888)
                               :type 'user-error)))
        (should (string-match-p "without a password" (cadr err)))
        (should (string-match-p "agent" (cadr err)))))
    (should-not emjupy--remote-tunnels)))

(ert-deftest emjupy-test-open-this-notebook-refuses-without-starting-anything ()
  "Not a notebook, not on this machine, or with unsaved text kept: nothing started."
  (let ((started nil) (file (make-temp-file "emjupy-nb" nil ".ipynb")))
    (unwind-protect
        (cl-letf (((symbol-function 'emjupy--start-local-server)
                   (lambda (&rest _) (setq started t)
                     (error "Should not start"))))
          (with-temp-buffer
            (should-error (emjupy-open-this-notebook) :type 'user-error))
          (with-temp-buffer
            ;; reached otherwise than over SSH: no tunnel to it
            (setq buffer-file-name "/sudo:root@localhost:/home/me/nb.ipynb")
            (should-error (emjupy-open-this-notebook) :type 'user-error)
            (setq buffer-file-name nil))
          (with-current-buffer (find-file-noselect file)
            (insert "{}")
            (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil)))
              (should-error (emjupy-open-this-notebook) :type 'user-error))
            (should (buffer-modified-p))          ; the text is kept as it was
            (set-buffer-modified-p nil)
            (kill-buffer))
          (should-not started))
      (delete-file file))))

(ert-deftest emjupy-test-a-local-server-stops-with-its-last-notebook ()
  "A server emjupy started stops with the last notebook it serves, not before."
  (let* ((server (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
         (process (make-process :name "emjupy-test-server"
                                :command '("sleep" "30") :noquery t))
         (emjupy--local-servers (list (list :dir "/tmp/" :jupyter "jupyter"
                                            :process process :server server)))
         (cells (vector (make-emjupy-cell :id (emjupy--new-cell-id)
                                          :type 'code :source "x"
                                          :outputs []
                                          :metadata (make-hash-table)))))
    (unwind-protect
        (emjupy-test--with-notebook cells a nb-a
          (setf (emjupy-notebook-server nb-a) server)
          (emjupy-test--with-notebook (vector
                                       (copy-emjupy-cell (aref cells 0))) b nb-b
            (setf (emjupy-notebook-server nb-b) server)
            ;; one of two closes: still needed
            (with-current-buffer b (emjupy--stop-local-server-if-unused server))
            (should (process-live-p process)))
          ;; the last closes: stopped
          (with-current-buffer a (emjupy--stop-local-server-if-unused server))
          (should-not (process-live-p process))
          (should-not emjupy--local-servers))
      (when (process-live-p process) (delete-process process)))))

(provide 'emjupy-tramp-open-test)

;;; emjupy-tramp-open-test.el ends here
