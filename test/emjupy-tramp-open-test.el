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
                   "-L" "127.0.0.1:9000:127.0.0.1:8888" "-J" "bastion" "-p" "22"
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


(ert-deftest emjupy-test-a-dead-tunnel-is-reopened-on-its-port ()
  "A tunnel that died is started again on the port its server is reached at.
The same port keeps the server's address good, so its kernel's
connection, and what the server kept for it, come back.  A live tunnel,
or another server's, is left alone."
  (let* ((dead (make-process :name "emjupy-test-dead" :command '("true")))
         (route '(:host "rbox" :jumps "bhost"))
         (emjupy--remote-tunnels
          (list (list :key "k" :process dead :port 41234
                      :route route :remote-port 8888)))
         (started nil))
    (while (process-live-p dead) (accept-process-output dead 0.05))
    (cl-letf (((symbol-function 'emjupy--start-tunnel)
               (lambda (&rest args) (push args started) 'new-process)))
      (emjupy--reopen-tunnel (make-emjupy-server :base-url "127.0.0.1:9999"))
      (should-not started)
      (emjupy--reopen-tunnel (make-emjupy-server :base-url "127.0.0.1:41234"))
      (should (equal started (list (list route 41234 8888))))
      (should (eq (plist-get (car emjupy--remote-tunnels) :process)
                  'new-process)))))


;;;; The same notebook, reached through another tunnel

(defmacro emjupy-test--remote-notebook-open (old-port new-port path &rest body)
  "Run BODY with a notebook buffer for PATH from a tunnel to rbox port OLD-PORT.
NEW, in BODY, is a server through a fresh tunnel to rbox port NEW-PORT;
BUF the open notebook's buffer, NB its notebook."
  (declare (indent 3))
  `(emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                         :source "x = 1"))
       buf nb
     (let* ((old (make-emjupy-server :base-url "127.0.0.1:40001" :token "t"))
            (new (make-emjupy-server :base-url "127.0.0.1:40002" :token "t"))
            (route '(:host "rbox" :jumps "bhost"))
            (emjupy--remote-tunnels
             (list (list :key "a" :process nil :port 40001 :route route
                         :remote-port ,old-port :server old)
                   (list :key "b" :process nil :port 40002 :route route
                         :remote-port ,new-port :server new)))
            (reconnected nil))
       (setf (emjupy-notebook-server nb) old
             (emjupy-notebook-path nb) "a.ipynb")
       (cl-letf (((symbol-function 'emjupy--notebook-buffers)
                  (lambda () (list buf)))
                 ((symbol-function 'emjupy-reconnect-kernel)
                  (lambda () (setq reconnected t)))
                 ((symbol-function 'switch-to-buffer) #'ignore))
         (ignore ,path)
         ,@body))))

(ert-deftest emjupy-test-the-same-remote-notebook-is-reached-again-not-twice ()
  "The same machine, port and path: the buffer open is moved, not doubled.
Opening it again, through a new tunnel, made a second buffer, the first
left stranded with its edits."
  (emjupy-test--remote-notebook-open 8888 8888 "a.ipynb"
    (setf (emjupy-notebook-kernel nb) (make-emjupy-kernel :id "k"))
    (should (eq (emjupy--adopt-remote-notebook "a.ipynb" new) buf))
    (should (eq (emjupy-notebook-server nb) new))
    (should reconnected)
    (should (= (length emjupy--remote-tunnels) 1))))

(ert-deftest emjupy-test-the-same-path-on-another-port-is-another-notebook ()
  "The same path from a server on another port opens apart, and says so.
Untitled.ipynb on two servers is two notebooks."
  (emjupy-test--remote-notebook-open 8888 8890 "a.ipynb"
    (let ((said nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format fmt args) said))))
        (should-not (emjupy--adopt-remote-notebook "a.ipynb" new)))
      (should (eq (emjupy-notebook-server nb) old))
      (should (seq-some (lambda (m) (string-match-p "also open" m)) said)))))

(ert-deftest emjupy-test-another-path-on-the-same-server-is-another-notebook ()
  "Another path on the same server is another notebook: nothing moves."
  (emjupy-test--remote-notebook-open 8888 8888 "b.ipynb"
    (should-not (emjupy--adopt-remote-notebook "b.ipynb" new))
    (should (eq (emjupy-notebook-server nb) old))))


;;;; What other users of the machine cannot see

(ert-deftest emjupy-test-a-local-server-has-its-token-out-of-sight ()
  "A local server gets its token in its environment, not its command line.
Every user of the machine can read a command line with ps, and the token
is all that keeps them from the server."
  (let ((command nil) (environment nil) (emjupy--local-servers nil))
    (cl-letf (((symbol-function 'make-process)
               (lambda (&rest args)
                 (setq command (plist-get args :command)
                       environment process-environment)
                 (error "Stop here")))
              ((symbol-function 'emjupy--free-port) (lambda () 9999)))
      (ignore-errors
        (emjupy--start-local-server temporary-file-directory "jupyter")))
    (should command)
    (should-not (seq-some (lambda (a) (string-match-p "token" a)) command))
    (should (seq-some (lambda (e) (string-prefix-p "JUPYTER_TOKEN=" e))
                      environment))))

(ert-deftest emjupy-test-a-remote-server-has-its-token-and-log-out-of-sight ()
  "A remote server gets its token in its environment, its log a folder of
the user's own -- not a guessable name in a shared /tmp."
  (let ((sent nil))
    (cl-letf (((symbol-function 'emjupy--remote-free-port)
               (lambda (&rest _) 8890))
              ((symbol-function 'emjupy--remote-shell)
               (lambda (_dir command) (setq sent command) "")))
      (emjupy--start-remote-server "/ssh:rbox:/home/u/proj/"
                                   "/env/bin/jupyter"))
    (should (string-match-p "JUPYTER_TOKEN=[0-9a-f]+ nohup " sent))
    (should-not (string-match-p "IdentityProvider.token" sent))
    (should (string-match-p "chmod 700 \"\\${XDG_CACHE_HOME:-\\$HOME/.cache}/emjupy\""
                            sent))
    (should-not (string-match-p "/tmp/" sent))))

(ert-deftest emjupy-test-two-tokens-are-not-the-same ()
  "Tokens are 64 hex digits, and two are never the same."
  (let ((a (emjupy--new-token)) (b (emjupy--new-token)))
    (should (string-match-p "\\`[0-9a-f]\\{64\\}\\'" a))
    (should-not (equal a b))))


(ert-deftest emjupy-test-an-environment-path-is-on-the-notebooks-machine ()
  "A venv named by an absolute path, for a notebook opened through TRAMP,
is looked for on that notebook's machine, not this one.  Looked for
here, a venv there was not found -- and one at the same path here was,
wrongly."
  (let ((default-directory "/ssh:somehost:/home/u/proj/"))
    (should (equal (emjupy--environment-directory "/home/u/envs/sci" t)
                   "/ssh:somehost:/home/u/envs/sci"))
    (should (equal (emjupy--environment-directory "~/envs/sci" t)
                   "/ssh:somehost:~/envs/sci")))
  (let ((default-directory "/tmp/proj/"))
    (should (equal (emjupy--environment-directory "envs/sci" nil)
                   "/tmp/proj/envs/sci"))))


(ert-deftest emjupy-test-a-server-already-reached-is-used-again ()
  "Opening a notebook, a server this Emacs reaches is used before asking
the machine -- on its machine, in its environment, serving the folder,
and answering.  Asking runs jupyter server list there: seconds of Python
starting, for a server opened a moment ago."
  (let* ((server (make-emjupy-server :base-url "127.0.0.1:5000" :token "tk"
                                     :files-root "/ssh:h:/home/u/proj"))
         (emjupy--remote-tunnels
          (list (list :route '(:host "h") :port 5000 :remote-port 8888
                      :server server :jupyter "/home/u/envs/a/bin/jupyter")))
         (answers t))
    (cl-letf (((symbol-function 'emjupy--server-reachable-p)
               (lambda (_s) answers)))
      (should (equal (emjupy--known-remote-server
                      "/ssh:h:/home/u/proj/sub/" "/home/u/envs/a/bin/jupyter"
                      '(:host "h"))
                     '(:port 8888 :token "tk" :root_dir "/home/u/proj")))
      ;; another environment, machine or folder; or not answering: asked
      (should-not (emjupy--known-remote-server
                   "/ssh:h:/home/u/proj/"
                   "/home/u/envs/b/bin/jupyter" '(:host "h")))
      (should-not (emjupy--known-remote-server
                   "/ssh:h:/home/u/proj/"
                   "/home/u/envs/a/bin/jupyter" '(:host "g")))
      (should-not (emjupy--known-remote-server
                   "/ssh:h:/home/u/other/"
                   "/home/u/envs/a/bin/jupyter" '(:host "h")))
      (setq answers nil)
      (should-not (emjupy--known-remote-server
                   "/ssh:h:/home/u/proj/"
                   "/home/u/envs/a/bin/jupyter" '(:host "h"))))))


(ert-deftest emjupy-test-a-remote-machines-environments-are-asked-once ()
  "A remote machine's environments and conda are found once a session:
each was a round trip or more at every opening."
  (let ((emjupy--environment-jupyters (make-hash-table :test 'equal))
        (emjupy--conda-environments-of (make-hash-table :test 'equal))
        (found 0) (asked 0))
    (cl-letf (((symbol-function 'emjupy--find-environment-jupyter)
               (lambda (&rest _) (cl-incf found) "/e/bin/jupyter"))
              ((symbol-function 'emjupy--ask-conda-environments)
               (lambda (&rest _) (cl-incf asked) '(("base" . "/c")))))
      (dotimes (_ 3)
        (should (equal (emjupy--environment-jupyter "/e" "/ssh:h:/p/")
                       "/e/bin/jupyter"))
        (should (equal (emjupy--conda-environments "/ssh:h:/p/")
                       '(("base" . "/c")))))
      (should (= found 1))
      (should (= asked 1))
      ;; another environment, or machine, is found again
      (emjupy--environment-jupyter "/other" "/ssh:h:/p/")
      (emjupy--conda-environments "/ssh:g:/p/")
      (should (= found 2))
      (should (= asked 2)))))

(ert-deftest emjupy-test-servers-are-listed-from-their-runtime-files ()
  "Running servers are read from their runtime files, those of live
processes only; with no runtime file, jupyter's own list."
  (let* ((dir (make-temp-file "emjupy-runtime" t))
         (jupyter (expand-file-name "fake-jupyter" dir))
         ;; HOME too: this machine's own runtime files are not the test's
         (process-environment (append (list (concat "JUPYTER_RUNTIME_DIR=" dir)
                                            (concat "HOME=" dir))
                                      process-environment))
         (run (lambda ()
                ;; From the test's own folder, an absolute name: with HOME
                ;; moved, a current directory written ~/... was not found.
                (let ((default-directory (file-name-as-directory dir)))
                  (with-temp-buffer
                    (call-process "sh" nil t nil "-c"
                                  (emjupy--server-list-command jupyter))
                    (buffer-string))))))
    (unwind-protect
        (progn
          (with-temp-file jupyter (insert "#!/bin/sh\necho FROM-JUPYTER\n"))
          (set-file-modes jupyter #o755)
          ;; none: jupyter's own list
          (should (string-match-p "FROM-JUPYTER" (funcall run)))
          ;; a live server's file read, a dead one's not
          (with-temp-file (expand-file-name
                           (format "jpserver-%d.json" (emacs-pid)) dir)
            (insert "{\"port\": 8888,\n \"root_dir\": \"/r\"}"))
          (with-temp-file (expand-file-name "jpserver-999999.json" dir)
            (insert "{\"port\": 9999}"))
          (let ((out (funcall run)))
            (should (string-match-p "\"port\": 8888" out))
            (should-not (string-match-p "9999" out))
            (should-not (string-match-p "FROM-JUPYTER" out))
            (should (= 1 (length (split-string out "\n" t))))))
      (delete-directory dir t))))


(ert-deftest emjupy-test-a-folders-environment-is-remembered ()
  "A folder's environment is asked the first time, then used as last
chosen, saying so; asked again with a prefix argument, the last offered;
one gone, forgotten and asked; and remembered across sessions, in
`emjupy-environments-file'.  Asked at every opening, it cost a
keystroke, and the remote its lookups."
  (let* ((file (make-temp-file "emjupy-environments" nil ".eld"))
         (emjupy-environments-file file)
         (emjupy--environments 'unread)
         (gone nil) (asked nil) (said nil) (answer "/e/sci"))
    (unwind-protect
        (cl-letf (((symbol-function 'emjupy--read-environment)
                   (lambda (dir)
                     (push (cons dir (cdr (assoc (emjupy--environment-key dir)
                                                 (emjupy--environments))))
                           asked)
                     answer))
                  ((symbol-function 'emjupy--environment-jupyter)
                   (lambda (env &rest _)
                     (when (member env gone)
                       (user-error "No jupyter in %s" env))
                     (concat env "/bin/jupyter")))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) said))))
          (should (equal (emjupy--chosen-environment-jupyter "/p/" nil)
                         "/e/sci/bin/jupyter"))
          (should (= (length asked) 1))
          ;; again: not asked, and said
          (should (equal (emjupy--chosen-environment-jupyter "/p/" nil)
                         "/e/sci/bin/jupyter"))
          (should (= (length asked) 1))
          (should (string-match-p "as last time here" (car said)))
          ;; C-u: asked, the last offered
          (setq answer "/e/plain")
          (should (equal (emjupy--chosen-environment-jupyter "/p/" t)
                         "/e/plain/bin/jupyter"))
          (should (equal (car asked) '("/p/" . "/e/sci")))
          ;; another session: read from the file
          (setq emjupy--environments 'unread)
          (should (equal (emjupy--chosen-environment-jupyter "/p/" nil)
                         "/e/plain/bin/jupyter"))
          (should (= (length asked) 2))
          ;; gone: forgotten, said, asked
          (setq gone '("/e/plain") answer "/e/sci")
          (should (equal (emjupy--chosen-environment-jupyter "/p/" nil)
                         "/e/sci/bin/jupyter"))
          (should (= (length asked) 3))
          (should (seq-some (lambda (m) (string-match-p "asking again" m))
                            said)))
      (delete-file file))))


(ert-deftest emjupy-test-a-folder-is-remembered-by-its-machine ()
  "A remote folder is remembered by its machine and path, whatever the
route: TRAMP names it with its hops, or without them once it knows them."
  (should (equal (emjupy--environment-key "/ssh:bastion|ssh:u@node:/home/u/p/")
                 (emjupy--environment-key "/ssh:u@node:/home/u/p/")))
  (should-not (equal (emjupy--environment-key "/ssh:u@node:/home/u/p/")
                     (emjupy--environment-key "/ssh:u@other:/home/u/p/"))))


(ert-deftest emjupy-test-what-a-session-learnt-is-forgotten-on-request ()
  "emjupy-forget-remote-state empties every table of what the session
learnt, and says how much; the tunnels stay, and the environments chosen
for folders, unless asked with a prefix.  An environment removed on a
machine was found again only after a restart of Emacs."
  (should (seq-every-p (lambda (v) (memq v emjupy--session-caches))
                       '(emjupy--reachable-at emjupy--lsp-offered
                         emjupy--lsp-asking emjupy--environment-jupyters
                         emjupy--conda-environments-of)))
  (let* ((file (make-temp-file "emjupy-environments" nil ".eld"))
         (emjupy-environments-file file)
         (emjupy--environments '(("@h:/p/" . "/e")))
         (emjupy--remote-tunnels '((:port 5000)))
         (emjupy--session-caches
          (mapcar (lambda (v) (let ((table (make-hash-table :test 'equal)))
                                (puthash "k" t table)
                                (set (make-local-variable v) table)
                                v))
                  emjupy--session-caches))
         (found 0) (said nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) said)))
                  ((symbol-function 'emjupy--find-environment-jupyter)
                   (lambda (&rest _) (cl-incf found) "/e/bin/jupyter")))
          (emjupy--environment-jupyter "/e" "/ssh:h:/p/")
          (let ((known (apply #'+ (mapcar (lambda (v)
                                            (hash-table-count (symbol-value v)))
                                          emjupy--session-caches))))
            (emjupy-forget-remote-state)
            (should (string-match-p (format "Forgot %d things" known)
                                    (car said))))
          (should (seq-every-p (lambda (v)
                                 (zerop (hash-table-count (symbol-value v))))
                               emjupy--session-caches))
          (should emjupy--environments)
          (should (equal emjupy--remote-tunnels '((:port 5000))))
          ;; found again: asked anew
          (emjupy--environment-jupyter "/e" "/ssh:h:/p/")
          (should (= found 2))
          (emjupy-forget-remote-state t)
          (should-not emjupy--environments)
          (should (equal (with-temp-buffer (insert-file-contents file)
                                           (buffer-string))
                         "nil")))
      (delete-file file))))

(provide 'emjupy-tramp-open-test)

;;; emjupy-tramp-open-test.el ends here
