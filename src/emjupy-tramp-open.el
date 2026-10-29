;;; emjupy-tramp-open.el --- Opening an .ipynb file as a notebook, here or through TRAMP  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; An .ipynb file visited as text, opened as a notebook instead: on a
;; Jupyter server started here, or, for a file reached through TRAMP, on
;; one found or started on its own machine and reached through an SSH
;; tunnel, across every hop.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'tramp)
(require 'transient)
(require 'emjupy-core)
(require 'emjupy-mode)
(require 'emjupy-notebook)


;;;; Options

(defcustom emjupy-local-server-timeout 60
  "Seconds to wait for a Jupyter server emjupy starts to answer."
  :type 'number
  :group 'emjupy-kernels)

;;;; An .ipynb file opened as text, opened as a notebook

(defvar emjupy--environment-history nil
  "Environments given to `emjupy-open-this-notebook'.")

(defvar emjupy--local-servers nil
  "Servers emjupy started: plists of :dir, :jupyter, :process and :server.")

(defun emjupy--environment-yml-name (dir)
  "Return the `name:' of the conda environment file in DIR, or nil.
That is environment.yml, or environment.yaml.  On another machine read
with one command: tested and read through TRAMP, three round trips."
  (if (file-remote-p dir)
      (with-temp-buffer
        (let ((default-directory dir))
          (ignore-errors
            (process-file "sh" nil t nil "-c"
                          (concat "cat environment.yml 2>/dev/null"
                                  " || cat environment.yaml 2>/dev/null"))))
        (goto-char (point-min))
        (emjupy--environment-yml-name-here))
    (cl-loop for file in '("environment.yml" "environment.yaml")
             for path = (expand-file-name file dir)
             when (file-readable-p path)
             return (with-temp-buffer
                      (insert-file-contents path)
                      (goto-char (point-min))
                      (emjupy--environment-yml-name-here)))))

(defun emjupy--environment-yml-name-here ()
  "Return the `name:' in the environment file in this buffer, from point."
  (when (re-search-forward
         "^name:[ \t]*[\"']?\\([^\"'#\n]*[^\"'#\n \t]\\)[\"']?[ \t]*\\(?:#.*\\)?$"
         nil t)
    (match-string 1)))

(defun emjupy--find-conda (remote)
  "Return conda, mamba or micromamba, the first there is, or nil.
On the machine of `default-directory'; REMOTE when that is another.
There, one command asks: `executable-find' over TRAMP tests each folder
of the PATH for each name, a round trip each -- 21 on a plain machine."
  (if (not remote)
      (or (executable-find "conda") (executable-find "mamba")
          (executable-find "micromamba"))
    (with-temp-buffer
      (when (eq 0 (ignore-errors
                    (process-file "sh" nil t nil "-c"
                                  "command -v conda || command -v mamba || command -v micromamba")))
        (let ((found (string-trim (buffer-string))))
          (and (not (string-empty-p found))
               (concat (file-remote-p default-directory) found)))))))

(defvar emjupy--conda-environments-of (make-hash-table :test 'equal)
  "Conda's environments, by remote machine: asked once a session.")

(defun emjupy--conda-environments (&optional where)
  "Return conda's environments on WHERE's machine, as name and directory.
See `emjupy--ask-conda-environments'.  A remote machine's, asked once:
a round trip or two, at every opening, for an answer that rarely
changes."
  (let ((machine (file-remote-p (or where temporary-file-directory))))
    (if (not machine)
        (emjupy--ask-conda-environments where)
      (pcase (gethash machine emjupy--conda-environments-of 'unasked)
        ('unasked (puthash machine (emjupy--ask-conda-environments where)
                           emjupy--conda-environments-of))
        (known known)))))

(defun emjupy--ask-conda-environments (&optional where)
  "Return conda's environments as an alist of name and directory, or nil.
nil when there is no conda, mamba or micromamba to ask.  Asked on the
machine of WHERE, a directory -- one through TRAMP asks the remote."
  (let* ((default-directory (or where temporary-file-directory))
         (remote (and (file-remote-p default-directory) t))
         (conda (emjupy--find-conda remote)))
    (when conda
      (with-temp-buffer
        (when (eq 0 (process-file (or (file-remote-p conda 'localname) conda)
                                  nil '(t nil) nil "env" "list" "--json"))
          (goto-char (point-min))
          ;; Output that is not JSON -- a warning first, an old conda -- is
          ;; no list of environments: none are offered, and a name or a
          ;; path can still be typed.
          (let ((envs (condition-case nil
                          (gethash "envs"
                                   (json-parse-buffer :object-type 'hash-table))
                        (json-error nil))))
            (cl-loop for dir across (or envs [])
                     ;; a named environment lives in .../envs/NAME; the one
                     ;; elsewhere is the installation's own, base
                     collect (cons (if (string-match-p "/envs/[^/]+/?\\'" dir)
                                       (file-name-nondirectory
                                        (directory-file-name dir))
                                     "base")
                                   dir))))))))

(defun emjupy--read-environment (dir)
  "Ask for the Python environment to run a notebook in DIR with.
Offered first: the environment named in an environment.yml beside it."
  (let* ((named (emjupy--environment-yml-name dir))
         (envs (emjupy--conda-environments dir)))
    (completing-read
     (format-prompt "Environment (a conda name, or a venv's directory; empty for the PATH's)" named)
     (mapcar #'car envs) nil nil nil 'emjupy--environment-history named)))

(defun emjupy--environment-directory (env remote)
  "The directory ENV names, on the machine of `default-directory'.
REMOTE when that machine is not this one: an absolute name, or one from
the home directory, is then that machine's, not this one's -- expanded
here, a venv there was looked for on this machine, and not found."
  (if (and remote (file-name-absolute-p env))
      (concat (file-remote-p default-directory) env)
    (expand-file-name env)))

(defvar emjupy--environment-jupyters (make-hash-table :test 'equal)
  "The jupyter of each environment found, by machine and name.
Asked over TRAMP, each a round trip or two at every opening.")

(defun emjupy--environment-jupyter (env &optional where)
  "Return the `jupyter' program of the environment ENV, on WHERE's machine.
See `emjupy--find-environment-jupyter'; on a remote machine, found once."
  (let* ((default-directory (or where default-directory))
         (machine (file-remote-p default-directory))
         (key (list machine env)))
    (if (not machine)
        ;; Here a look is cheap, and an environment may change under it.
        (emjupy--find-environment-jupyter env where)
      (or (gethash key emjupy--environment-jupyters)
          (puthash key (emjupy--find-environment-jupyter env where)
                   emjupy--environment-jupyters)))))

(defun emjupy--find-environment-jupyter (env &optional where)
  "Return the `jupyter' program of the environment ENV.
ENV is a conda environment's name, an environment's directory -- a venv
or a conda prefix -- or empty for the one first on the PATH.  Looked
for on the machine of WHERE, a directory, and named as it names it."
  (let* ((default-directory (or where default-directory))
         (remote (and (file-remote-p default-directory) t))
         (here (lambda (path) (or (file-remote-p path 'localname) path)))
         (in-dir (lambda (dir)
                   (cl-loop for name in '("bin/jupyter" "Scripts/jupyter.exe")
                            for path = (expand-file-name name dir)
                            when (file-executable-p path)
                            return (funcall here path)))))
    (cond
     ((or (null env) (string-empty-p (string-trim env)))
      (let ((found (executable-find "jupyter" remote)))
        (cond (found (funcall here found))
              (remote (user-error "No `jupyter' on the remote PATH -- conda or modules often set it up only in a login shell: name the environment"))
              (t (user-error "No `jupyter' on the PATH: name an environment that has it")))))
     ((file-directory-p (emjupy--environment-directory env remote))
      (or (funcall in-dir (emjupy--environment-directory env remote))
          (user-error "No jupyter in %s: install jupyter-server there" env)))
     (t
      (let ((dir (cdr (assoc env (emjupy--conda-environments where)))))
        (cond ((null dir) (user-error "No environment named %s" env))
              ((funcall in-dir (if remote
                                   (concat (file-remote-p default-directory)
                                           dir) dir)))
              (t (user-error "No jupyter in the environment %s: install jupyter-server there"
                             env))))))))

(defun emjupy--free-port ()
  "Return a TCP port on this machine that nothing listens on."
  (let ((probe (make-network-process :name "emjupy-port" :server t :host 'local
                                     :service t :family 'ipv4 :noquery t)))
    (prog1 (process-contact probe :service)
      (delete-process probe))))

(defun emjupy--start-local-server (dir jupyter)
  "Return a Jupyter server serving DIR, run by JUPYTER, started if need be.
One already started for DIR with the same JUPYTER is used again.  It
listens on this machine only, on a free port, with a token of its own,
and is stopped when the last notebook it serves is closed."
  (or (cl-loop for s in emjupy--local-servers
               when (and (equal (plist-get s :dir) dir)
                         (equal (plist-get s :jupyter) jupyter)
                         (process-live-p (plist-get s :process)))
               return (plist-get s :server))
      (let* ((port (emjupy--free-port))
             (token (emjupy--new-token))
             (output (generate-new-buffer (format " *emjupy server %s*" dir)))
             (default-directory dir)
             ;; The token in the environment, which only its user can read --
             ;; on the command line, every user of the machine could.
             (process-environment (cons (concat "JUPYTER_TOKEN=" token)
                                        process-environment))
             (process (make-process
                       :name "emjupy-server" :buffer output :noquery t
                       :command (append
                                 (list
                                  jupyter "server" "--no-browser"
                                  "--ServerApp.ip=127.0.0.1"
                                  (format "--ServerApp.port=%d" port)
                                  "--ServerApp.port_retries=0"
                                  (format "--ServerApp.root_dir=%s"
                                          (expand-file-name dir)))
                                 ;; Jupyter refuses to run as root without it
                                 (and (eql (user-uid) 0)
                                      (list "--allow-root")))))
             (server (emjupy--intern-server (format "127.0.0.1:%d" port) token))
             (deadline (+ (float-time) emjupy-local-server-timeout)))
        (message "[emjupy] Starting a Jupyter server for %s..."
                 (abbreviate-file-name dir))
        (while (and (process-live-p process) (< (float-time) deadline)
                    (not (emjupy--server-reachable-p server)))
          (accept-process-output process 0.25))
        (unless (and (process-live-p process)
                     (emjupy--server-reachable-p server))
          (let ((said (with-current-buffer output
                        (buffer-substring-no-properties
                         (max (point-min) (- (point-max) 600))
                         (point-max)))))
            (when (process-live-p process) (delete-process process))
            (user-error "The Jupyter server did not start:\n%s" said)))
        (push (list :dir dir :jupyter jupyter :process process :server server)
              emjupy--local-servers)
        server)))

(defun emjupy--server-in-use-p (server)
  "Return non-nil if a notebook buffer other than this one is on SERVER."
  (cl-some (lambda (b)
             (and (not (eq b (current-buffer)))
                  (eq (emjupy-notebook-server
                       (buffer-local-value 'emjupy--buffer-notebook b))
                      server)))
           (emjupy--notebook-buffers)))

(defun emjupy--stop-local-server-if-unused (server)
  "Stop SERVER, if emjupy started it and no notebook it serves is open."
  (let ((entry (cl-find server emjupy--local-servers
                        :key (lambda (s) (plist-get s :server)))))
    (when (and entry
               (not (emjupy--server-in-use-p server)))
      (when (process-live-p (plist-get entry :process))
        (delete-process (plist-get entry :process)))
      (setq emjupy--local-servers (delq entry emjupy--local-servers)))))

;;;; An .ipynb file through TRAMP, opened on its own machine

(defconst emjupy--ssh-methods '("ssh" "sshx" "scp" "scpx" "rsync")
  "TRAMP methods that reach a host over SSH, through which a tunnel can go.")

(defvar emjupy--remote-tunnels nil
  "Tunnels emjupy opened to servers it found or started through TRAMP.
Each a plist of :key, naming the route and the remote port; :process;
:port, the local one; and :server.")

(defun emjupy--tramp-chain (file)
  "Return the machines on TRAMP's way to FILE, its own machine last.
Asked of TRAMP, not read from FILE's name: once TRAMP has gone through a
hop it names the file without it, and remembers the way: read from the
name, the hop -- the bastion -- would be lost."
  (tramp-compute-multi-hops (tramp-dissect-file-name file)))

(defun emjupy--tramp-address (v &optional for-ssh)
  "Return the machine of V, a TRAMP file name, as TRAMP writes it.
With FOR-SSH, as ssh -J writes it: a port after a colon, not a hash."
  (let ((user (tramp-file-name-user v)) (host (tramp-file-name-host v))
        (port (tramp-file-name-port v)))
    (substring-no-properties
     (concat (and user (concat user "@")) host
             (and port (concat (if for-ssh ":" "#") port))))))

(defun emjupy--remote-prefix (file)
  "Return the TRAMP prefix reaching FILE's machine, every hop written out."
  (concat "/" (mapconcat
               (lambda (v)
                 (concat (substring-no-properties (tramp-file-name-method v))
                         ":" (emjupy--tramp-address v)))
               (emjupy--tramp-chain file) "|")
          ":"))

(defun emjupy--ssh-route (file)
  "Return how to reach FILE's machine with ssh, for a tunnel.
A plist of :host, :user and :port, and :jumps, the hops before it as ssh
-J takes them.  A file reached otherwise than over SSH -- through sudo, a
container, a method of its own -- has no tunnel to it: a `user-error'."
  (let* ((chain (emjupy--tramp-chain file))
         (target (car (last chain)))
         (hops (butlast chain)))
    (dolist (v chain)
      (unless (member (tramp-file-name-method v) emjupy--ssh-methods)
        (user-error "Only a notebook reached over SSH can be opened on its machine: %s is reached with %s"
                    (tramp-file-name-host v) (tramp-file-name-method v))))
    (list :host (substring-no-properties (tramp-file-name-host target))
          :user (tramp-file-name-user target)
          :port (tramp-file-name-port target)
          :jumps (and hops (mapconcat (lambda (v) (emjupy--tramp-address v t))
                                      hops ",")))))

(defun emjupy--tunnel-command (route local-port remote-port)
  "Return the ssh command forwarding LOCAL-PORT to REMOTE-PORT along ROUTE.
BatchMode: with no terminal to type a password into, a login that wants
one fails at once rather than waiting for ever."
  (append (list "ssh" "-N" "-o" "BatchMode=yes" "-o" "ExitOnForwardFailure=yes"
                "-L" (format "127.0.0.1:%d:127.0.0.1:%d"
                             local-port remote-port))
          (and (plist-get route :jumps) (list "-J" (plist-get route :jumps)))
          (and (plist-get route :port) (list "-p" (plist-get route :port)))
          (list (if (plist-get route :user)
                    (concat (plist-get route :user) "@" (plist-get route :host))
                  (plist-get route :host)))))

;;;; A Jupyter server on the remote machine

(defun emjupy--remote-shell (dir command)
  "Run COMMAND with sh in DIR, on DIR's machine; return what it printed.
Signal a `file-error' with what it printed if it fails."
  (let ((default-directory (file-name-as-directory dir)))
    (with-temp-buffer
      (let ((status (process-file "sh" nil t nil "-c" command)))
        (unless (eq status 0)
          (signal 'file-error (list "Running on the remote machine" command
                                    (string-trim (buffer-string)))))
        (string-trim (buffer-string))))))

(defun emjupy--remote-server-for (servers dir)
  "Return the server of SERVERS serving DIR, or a folder above it, or nil.
SERVERS are what `jupyter server list --json' prints, one object a line;
of several, the one serving the folder nearest DIR."
  (let ((dir (file-name-as-directory dir)) (best nil))
    (dolist (line (split-string servers "\n" t))
      (let* ((s (condition-case nil (json-parse-string line :object-type 'plist)
                  (json-error nil)))
             (root (and s (plist-get s :root_dir))))
        (when (and (stringp root)
                   (string-prefix-p (file-name-as-directory root) dir)
                   (or (null best) (> (length root)
                                      (length (plist-get best :root_dir)))))
          (setq best s))))
    best))

(defun emjupy--remote-free-port (dir python)
  "Return a TCP port nothing listens on, on DIR's machine, asked of PYTHON.
python3 is asked if PYTHON cannot be; 0 if neither can say."
  (let ((ask (shell-quote-argument
              (concat "import socket; s = socket.socket(); "
                      "s.bind(('127.0.0.1', 0)); print(s.getsockname()[1])"))))
    (string-to-number
     (emjupy--remote-shell
      dir (format "%s -c %s 2>/dev/null || python3 -c %s"
                  (shell-quote-argument python) ask ask)))))

(defun emjupy--new-token ()
  "Return a new token for a Jupyter server, from freshly seeded randomness."
  (random t)
  (secure-hash 'sha256 (format "%d %d %d %d %s %d" (random) (random) (random)
                               (random) (float-time) (emacs-pid))))

(defun emjupy--start-remote-server (dir jupyter)
  "Start JUPYTER's server in DIR, through TRAMP, and return what it serves.
A plist like those `emjupy--remote-server-for' returns.  It is left
running when its notebooks are closed, and when Emacs quits: opening one
of them again finds it, and its kernels, as they were."
  (let* ((here (file-remote-p dir 'localname))
         (python (concat (file-name-directory jupyter) "python"))
         (port (emjupy--remote-free-port dir python))
         (token (emjupy--new-token))
         ;; The log in a folder of the user's own: in a shared /tmp, under a
         ;; name another user can guess, a link put there first would have
         ;; the server's output overwrite a file of the user's.
         (logs "\"${XDG_CACHE_HOME:-$HOME/.cache}/emjupy\"")
         (log (format (concat "\"${XDG_CACHE_HOME:-$HOME/.cache}"
                              "/emjupy/jupyter-%d.log\"")
                      port)))
    (when (zerop port)
      (user-error "Could not find a free port on the remote machine"))
    (emjupy--remote-shell
     ;; The token in the environment, not the command line, which every user
     ;; there can read; the command itself goes over the shell's input.
     dir (format "mkdir -p %s && chmod 700 %s && JUPYTER_TOKEN=%s nohup %s server --no-browser --ServerApp.ip=127.0.0.1 --ServerApp.port=%d --ServerApp.port_retries=0 --ServerApp.root_dir=%s > %s 2>&1 < /dev/null &"
                 logs logs token (shell-quote-argument jupyter) port
                 (shell-quote-argument here) log))
    ;; As `jupyter server list' reports a root: without a final slash, so a
    ;; server started here and one found running give the same root.
    (list :port port :token token :root_dir (directory-file-name here)
          :log log)))

;;;; Tunnels

(defun emjupy--open-tunnel (route remote-port)
  "Return a live tunnel's local port to REMOTE-PORT along ROUTE.
One already open is used again.  A login that wanted a password gets a
message saying so: they are not handled yet."
  (let ((key (format "%S:%d" route remote-port)))
    (or (cl-loop for tunnel in emjupy--remote-tunnels
                 when (and (equal (plist-get tunnel :key) key)
                           (process-live-p (plist-get tunnel :process)))
                 return (plist-get tunnel :port))
        (let* ((port (emjupy--free-port))
               (process (emjupy--start-tunnel route port remote-port))
               (output (process-buffer process))
               (deadline (+ (float-time) 30)))
          ;; Up when something answers on the local port, or failed when ssh
          ;; exits.
          (while (and (process-live-p process) (< (float-time) deadline)
                      (not (ignore-error file-error
                             (let ((probe (open-network-stream
                                           "emjupy-probe"
                                           nil "127.0.0.1" port)))
                               (delete-process probe)
                               t))))
            (accept-process-output process 0.25))
          (unless (process-live-p process)
            (let ((said (with-current-buffer output
                          (string-trim (buffer-string)))))
              (if (string-match-p "Permission denied\\|password\\|passphrase\\|keyboard-interactive" said)
                  (user-error "The tunnel to %s needs an SSH login without a password: emjupy runs ssh itself, with nowhere to type one.  Use an SSH key, loaded in an agent (ssh-add) if it has a passphrase -- password logins are not handled yet"
                              (plist-get route :host))
                (user-error "The tunnel to %s did not open: %s"
                            (plist-get route :host) said))))
          (push (list :key key :process process :port port
                      :route route :remote-port remote-port)
                emjupy--remote-tunnels)
          ;; Should it die, reconnecting reopens it.
          (add-hook 'emjupy-reconnect-functions #'emjupy--reopen-tunnel)
          port))))

(defun emjupy--start-tunnel (route port remote-port)
  "Start ssh forwarding local PORT to REMOTE-PORT along ROUTE.
Return its process; what ssh says goes to its buffer."
  (let ((default-directory temporary-file-directory))
    (make-process :name "emjupy-tunnel"
                  :buffer (generate-new-buffer
                           (format " *emjupy tunnel %s*"
                                   (plist-get route :host)))
                  :noquery t
                  :command (emjupy--tunnel-command route port remote-port))))

(defun emjupy--reopen-tunnel (server)
  "Reopen the tunnel SERVER is reached by, on the same port, if it died.
Not waited for: the reconnect calling this finds it up on a later try.
A server not reached by a tunnel of emjupy's is left alone."
  (let ((tunnel (cl-find-if
                 (lambda (tn)
                   (and (equal (emjupy-server-host server) "127.0.0.1")
                        (equal (emjupy-server-port server)
                               (number-to-string (plist-get tn :port)))))
                 emjupy--remote-tunnels)))
    (when (and tunnel (not (process-live-p (plist-get tunnel :process))))
      (message "[emjupy] Reopening the tunnel to %s..."
               (plist-get (plist-get tunnel :route) :host))
      (setf (plist-get tunnel :process)
            (emjupy--start-tunnel (plist-get tunnel :route)
                                  (plist-get tunnel :port)
                                  (plist-get tunnel :remote-port))))))

(defun emjupy--close-tunnel-if-unused (server)
  "Close the tunnel to SERVER if no notebook it serves is open.
The server itself, on the other machine, keeps running."
  (let ((tunnel (cl-find server emjupy--remote-tunnels
                         :key (lambda (s) (plist-get s :server)))))
    (when (and tunnel
               (not (emjupy--server-in-use-p server)))
      (when (process-live-p (plist-get tunnel :process))
        (delete-process (plist-get tunnel :process)))
      (setq emjupy--remote-tunnels (delq tunnel emjupy--remote-tunnels))
      (message "[emjupy] Tunnel closed; the server on %s keeps running -- open the notebook again to get back to it."
               (emjupy-server-files-root server)))))

;;;; Opening the notebook

(defun emjupy--known-remote-server (dir jupyter route)
  "Return a server this Emacs reaches, on ROUTE's machine, serving DIR.
In JUPYTER's environment, its root DIR or a folder above it, and
answering, through its tunnel: as `emjupy--remote-server-for' returns
one.  Asking the machine, `jupyter server list' starts a Python and
imports Jupyter -- seconds, for a server opened a moment ago."
  (cl-loop for tn in emjupy--remote-tunnels
           for server = (plist-get tn :server)
           for root = (and server (emjupy-server-files-root server))
           when (and root
                     (equal (plist-get tn :jupyter) jupyter)
                     (equal (plist-get (plist-get tn :route) :host)
                            (plist-get route :host))
                     (string-prefix-p (file-name-as-directory root)
                                      (file-name-as-directory dir))
                     (emjupy--server-reachable-p server))
           return (list :port (plist-get tn :remote-port)
                        :token (emjupy-server-token server)
                        :root_dir (directory-file-name
                                   (or (file-remote-p root 'localname) root)))))

(defun emjupy--server-list-command (jupyter)
  "Return a shell command printing the running servers, as JUPYTER's list.
The runtime files the list is made of, one JSON line for each server
whose process is alive -- `jupyter server list' starts a Python and
imports Jupyter for it, a second or more.  No runtime file where Jupyter
puts them on Linux or macOS, or where JUPYTER_RUNTIME_DIR says: then
JUPYTER's own list."
  (concat
   "found=; for d in \"${JUPYTER_RUNTIME_DIR:-}\" "
   "\"$HOME/.local/share/jupyter/runtime\" "
   "\"$HOME/Library/Jupyter/runtime\"; do "
   "[ -n \"$d\" ] || continue; "
   "for f in \"$d\"/jpserver-*.json; do [ -f \"$f\" ] || continue; found=1; "
   "p=${f##*jpserver-}; p=${p%.json}; "
   "kill -0 \"$p\" 2>/dev/null && { tr -d '\\n' < \"$f\"; echo; }; done; done; "
   "[ -n \"$found\" ] || "
   (shell-quote-argument jupyter) " server list --json"))

(defun emjupy--remote-server (dir jupyter route)
  "Return the Jupyter server serving DIR on its machine, started if need be.
One already serving DIR, or a folder above it, is used; otherwise JUPYTER
starts one.  ROUTE names the machine, for the messages."
  (let* ((host (plist-get route :host))
         (known (emjupy--known-remote-server dir jupyter route))
         (_ (unless known
              (message "[emjupy] Looking for a Jupyter server on %s..." host)))
         (listed (and (not known)
                      (condition-case nil
                     (emjupy--remote-shell
                      dir (emjupy--server-list-command jupyter))
                   (file-error ""))))
         (found (or known
                    (emjupy--remote-server-for
                     listed (file-remote-p dir 'localname)))))
    (if found
        (progn (message "[emjupy] Using the Jupyter server already serving %s on %s"
                        (plist-get found :root_dir) host)
               found)
      (message "[emjupy] Starting a Jupyter server on %s..." host)
      (emjupy--start-remote-server dir jupyter))))

(defun emjupy--open-remote-notebook (file)
  "Open FILE, an .ipynb through TRAMP, as a notebook on its own machine.
Finds a Jupyter server already serving its folder there, or starts one
in the environment asked for; opens a tunnel to it, through every hop;
and opens the notebook on it.  Return the notebook\\='s buffer."
  (let* ((route (emjupy--ssh-route file))
         (prefix (emjupy--remote-prefix file))
         (dir (file-name-directory file))
         (jupyter (emjupy--environment-jupyter (emjupy--read-environment dir)
                                               dir))
         (remote (emjupy--remote-server dir jupyter route))
         (local-port (emjupy--open-tunnel route (plist-get remote :port)))
         (server (emjupy--intern-server (format "127.0.0.1:%d" local-port)
                                        (plist-get remote :token)))
         (root (plist-get remote :root_dir))
         (deadline (+ (float-time) emjupy-local-server-timeout)))
    (let ((tunnel (cl-find local-port emjupy--remote-tunnels
                           :key (lambda (s) (plist-get s :port)))))
      (setq emjupy--remote-tunnels
            (cons (plist-put (plist-put tunnel :server server)
                             :jupyter jupyter)
                  (delq tunnel emjupy--remote-tunnels))))
    (setf (emjupy-server-files-root server) (concat prefix root))
    (while (and (< (float-time) deadline)
                (not (emjupy--server-reachable-p server)))
      (sleep-for 0.5))
    (unless (emjupy--server-reachable-p server)
      (user-error "The Jupyter server on %s did not answer%s"
                  (plist-get route :host)
                  (if (plist-get remote :log)
                      (format ":\n%s" (condition-case nil
                                          (emjupy--remote-shell
                                           dir (format "tail -n 15 %s"
                                                       (plist-get remote :log)))
                                        (file-error "")))
                    "")))
    ;; Bound already, by an earlier opening: asking again lists its
    ;; kernels, a round trip, to come to the same one.
    (unless (emjupy-server-kernel-id server)
      (emjupy--bind-server-kernel server))
    (list server (file-relative-name (file-remote-p file 'localname) root))))

(defun emjupy--tunnel-of (server)
  "Return the record of the tunnel SERVER is reached through, or nil."
  (cl-find server emjupy--remote-tunnels
           :key (lambda (tunnel) (plist-get tunnel :server))))

(defun emjupy--remote-notebooks-of (path server)
  "Return the buffers showing PATH from another tunnel to SERVER's machine.
Each as (BUFFER . SAME), SAME non-nil when its tunnel reaches the same
port there as SERVER's: then it is the same notebook, on the same
server, reached another way.  The same path on another port is on
another server, and may be another notebook."
  (let ((tunnel (emjupy--tunnel-of server)) (found nil))
    (when tunnel
      (dolist (buf (emjupy--notebook-buffers))
        (let* ((nb (buffer-local-value 'emjupy--buffer-notebook buf))
               (old (and nb (emjupy-notebook-server nb)))
               (old-tunnel (and old (not (eq old server))
                                (emjupy--tunnel-of old))))
          (when (and old-tunnel (equal (emjupy-notebook-path nb) path)
                     (equal (plist-get (plist-get old-tunnel :route) :host)
                            (plist-get (plist-get tunnel :route) :host)))
            (push (cons buf (equal (plist-get old-tunnel :remote-port)
                                   (plist-get tunnel :remote-port)))
                  found)))))
    found))

(defun emjupy--adopt-remote-notebook (path server)
  "Move the buffer already showing PATH on SERVER's server onto SERVER.
The same machine, port and path: the same notebook, reached through
another tunnel.  Its cells and unsaved edits stay; its kernel is
reconnected if its connection is down.  Return the buffer, or nil --
saying so when the path is open from another server there."
  (let* ((found (emjupy--remote-notebooks-of path server))
         (same (car (cl-find-if #'cdr found))))
    (if (not same)
        (progn
          (dolist (other found)
            (message "[emjupy] %s is also open in %s, from another server on %s; to emjupy that is a different notebook, and its edits stay there"
                     path (buffer-name (car other))
                     (plist-get (plist-get (emjupy--tunnel-of server) :route)
                                :host)))
          nil)
      (with-current-buffer same
        (let* ((nb emjupy--buffer-notebook)
               (old-tunnel (emjupy--tunnel-of (emjupy-notebook-server nb)))
               (kernel (emjupy-notebook-kernel nb)))
          (when old-tunnel
            (when (process-live-p (plist-get old-tunnel :process))
              (delete-process (plist-get old-tunnel :process)))
            (setq emjupy--remote-tunnels
                  (delq old-tunnel emjupy--remote-tunnels)))
          (setf (emjupy-notebook-server nb) server)
          (rename-buffer (emjupy--notebook-buffer-name path server) t)
          (message "[emjupy] %s: already open, from the same server; reached again"
                   path)
          (when (and kernel (emjupy-kernel-id kernel)
                     (not (emjupy--ws-live-p kernel)))
            (emjupy-reconnect-kernel))))
      same)))

;;;###autoload
(defun emjupy-open-this-notebook ()
  "Open the .ipynb file this buffer visits as a notebook, on its machine.
For a notebook opened as a file -- its JSON shown as text.  Asks for the
Python environment to run it in, offered first the one an
environment.yml beside it names; starts a Jupyter server there, for the
notebook\\='s directory; and replaces this buffer with the notebook.

On this machine, the server listens here only, and stops when the last
notebook it serves is closed, and with Emacs.  Through TRAMP -- over
SSH, through any hops -- a server already serving the folder there is
used, or one is started, and reached through a tunnel; closing the last
notebook closes the tunnel, and leaves the server, and its kernels,
running, to come back to.  A notebook already open from the same machine,
from the server on the same port there, is the same notebook: its buffer
is reused, its edits kept."
  (interactive)
  (let ((file buffer-file-name))
    (unless (and file (string-suffix-p ".ipynb" file t))
      (user-error "This buffer is not visiting a .ipynb file"))
    (when (buffer-modified-p)
      (if (y-or-n-p (format "The notebook opens from the file: save %s first? "
                            (file-name-nondirectory file)))
          (save-buffer)
        (user-error "Not opened: save the file, or revert it, first")))
    (let* ((remote (file-remote-p file))
           (opened (if remote
                       (emjupy--open-remote-notebook file)
                     (let* ((dir (file-name-directory file))
                            (jupyter (emjupy--environment-jupyter
                                      (emjupy--read-environment dir)))
                            (server (emjupy--start-local-server dir jupyter)))
                       ;; A kernel for the notebook to open on, as logging in
                       ;; binds one: a server just started has none of its own.
                       (emjupy--bind-server-kernel server)
                       (list server (file-name-nondirectory file)))))
           (server (car opened))
           (text (current-buffer))
           (window (selected-window))
           (notebook (or (and remote
                              (emjupy--adopt-remote-notebook (cadr opened)
                                                             server))
                         (emjupy-open-notebook (cadr opened) server))))
      (setq emjupy--current-server server)
      (when (buffer-live-p notebook)
        (with-current-buffer notebook
          (add-hook 'kill-buffer-hook
                    (if remote
                        (lambda () (emjupy--close-tunnel-if-unused server))
                      (lambda () (emjupy--stop-local-server-if-unused server)))
                    nil t))
        (when (window-live-p window) (set-window-buffer window notebook))
        ;; In place: the text is the same file, and would go stale as the
        ;; notebook is saved.
        (kill-buffer text))
      notebook)))

(provide 'emjupy-tramp-open)

;;; emjupy-tramp-open.el ends here
