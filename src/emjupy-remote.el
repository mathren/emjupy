;;; emjupy-remote.el --- Where a Jupyter server's files live  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy

;; This file is part of emjupy.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.


;;; Commentary:

;; A server is reached over HTTP, but its files are on a machine that
;; HTTP does not name -- a tunnelled server answers at localhost.  This
;; works out where they are: the directory the server serves, the host
;; behind a tunnel, and the TRAMP name that reaches a file there.
;;
;; Below the notebook, language-server and Eglot layers, all of which
;; need it; it needs nothing above HTTP.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-http)

(defvar python-indent-guess-indent-offset-verbose)

(defcustom emjupy-remote-root nil
  "Where the notebook directory lives as a *file name*, for Dired.

The Contents API tells emjupy what notebooks a server has, but not how
to reach them as files -- and a tunnelled server looks like localhost
from here, so there is nothing to infer.  Set this to hand `d\' in the
dashboard somewhere useful:

  (setq emjupy-remote-root \"/ssh:user@host:/home/user/notebooks\")

An alist maps it per server:

  (setq emjupy-remote-root
        \='((\"localhost:8888\" . \"~/notebooks\")
          (\"localhost:9999\" . \"/ssh:box:/srv/nb\")))

nil disables `d\'."
  :type '(choice (const :tag "Disabled" nil)
                 (directory :tag "One directory for every server")
                 (alist :key-type string :value-type directory))
  :group 'emjupy)

(defcustom emjupy-ssh-host nil
  "How to reach the machine a tunnelled server runs on.

A host name -- what you would type after `ssh\' -- or an alist keyed by
server label, or nil to work it out from the running tunnel.

This is a HOST and not a path, deliberately.  The path comes from the
kernel and differs per notebook, so a setting that included one would
have to be changed for every project on the same machine; a host is the
same for all of them."
  :type '(choice (const :tag "Work it out" nil)
                 (string :tag "Host, e.g. box")
                 (alist :key-type string :value-type string))
  :group 'emjupy)

(defconst emjupy--local-hostnames '("localhost" "127.0.0.1" "::1" "0.0.0.0" "")
  "Host names that name this machine rather than another one.")

(defun emjupy--configured-root-for (server)
  "Return the root configured for SERVER by hand, or nil."
  (cond
   ((null emjupy-remote-root) nil)
   ((stringp emjupy-remote-root) emjupy-remote-root)
   ((consp emjupy-remote-root)
    (cdr (assoc (emjupy--server-label server) emjupy-remote-root)))))

(defun emjupy--derive-server-root (nb)
  "Work out the absolute directory NB's server serves, or nil.

Jupyter does not publish its root directory -- deliberately, it is not
something a client should be told.  But it can be worked out, because two
things that ARE known overlap: the kernel reports the directory it runs
in, which is the notebook's own folder, and the Contents API gives the
notebook's path relative to the root.  Take the second off the end of the
first and what remains is the root:

  kernel cwd     /home/you/project/analysis
  notebook path  analysis/run.ipynb
  root           /home/you/project

This is why `emjupy-remote-root' need not be set per project: the running
kernel already knows, and is asked."
  (let* ((cwd (emjupy-notebook-kernel-cwd nb))
         (path (emjupy-notebook-path nb))
         (rel (and path (file-name-directory path))))
    (when (and cwd (not (string-empty-p cwd)))
      (let* ((cwd (directory-file-name cwd))
             (rel (and rel (directory-file-name rel))))
        (cond
         ;; The notebook sits in the root itself, so the kernel's directory
         ;; IS the root.
         ((null rel) cwd)
         ((string-suffix-p (concat "/" rel) cwd)
          (let ((root (substring cwd 0 (- (length cwd) (length rel) 1))))
            (if (string-empty-p root) "/" root)))
         ;; The two do not line up -- a kernel started somewhere else, say.
         ;; Better to admit that than to invent a path.
         (t nil))))))

(defun emjupy--server-relative-path (server absolute)
  "Return ABSOLUTE as a path relative to SERVER's root, or nil.

The Contents API speaks in paths relative to the root it serves; a
language server running beside the kernel speaks in absolute ones.  This
is the conversion between them, and it fails rather than guesses when
the path lies outside what the server serves."
  (let ((root (and server (emjupy--remote-root-for server))))
    (when (and root absolute)
      (let ((root (file-name-as-directory (directory-file-name root))))
        (when (string-prefix-p root absolute)
          (substring absolute (length root)))))))

(defun emjupy--remember-server-root (nb)
  "Record on NB's server the root derived from NB, if one can be."
  (let ((server (emjupy-notebook-server nb)))
    (when (and server (null (emjupy-server-root server)))
      (when-let* ((root (emjupy--derive-server-root nb)))
        (setf (emjupy-server-root server) root)))))

(defun emjupy--remote-root-for (server)
  "Return the root to use for SERVER, or nil.

A value set by hand wins, since it may point at a mirror or through a
TRAMP hop that emjupy cannot infer.  Otherwise the root derived from a
running kernel is used, which needs no configuration at all."
  (or (emjupy--configured-root-for server)
      (and server (emjupy-server-root server))))

(defconst emjupy--ssh-value-options
  '("-L" "-R" "-D" "-p" "-o" "-i" "-b" "-c" "-e" "-F" "-l" "-m"
    "-O" "-Q" "-S" "-W" "-w" "-J")
  "SSH options that take a separate argument, which is therefore not a host.")

(defun emjupy--ssh-destination (argv)
  "Return the host ARGV connects to, or nil.

ARGV is an `ssh\' command line split into words.  The destination is the
last word that is neither an option nor an option\='s argument, which is
tedious to work out but not ambiguous."
  (let ((words (split-string argv " +" t))
        (destination nil)
        (skip nil))
    (dolist (word (cdr words) destination)
      (cond
       (skip (setq skip nil))
       ((member word emjupy--ssh-value-options) (setq skip t))
       ((string-prefix-p "-" word) nil)
       (t (setq destination word))))))

(defvar emjupy--tunnel-hosts (make-hash-table)
  "The tunnel found forwarding each local port: (TIME . HOST), HOST nil if none.")

(defconst emjupy--tunnel-hosts-seconds 60
  "How long the tunnel found for a port is taken as still the one.")

(defun emjupy--ssh-host-forwarding (port)
  "Return the host of a running SSH tunnel forwarding local PORT, or nil.

The tunnel is invisible in the HTTP conversation, but it is a process on
this machine and its command line says where it goes.  Reading it back
is how emjupy learns a name nobody told it.

It is asked for each item a search for definitions returns, so the
answer -- none, too -- is kept for `emjupy--tunnel-hosts-seconds\='
rather than running ps for every one.  Tunnels outlive that easily."
  (when (and port emjupy-probe-environment)
    (let ((known (gethash port emjupy--tunnel-hosts)))
      (if (and known (< (- (float-time) (car known)) emjupy--tunnel-hosts-seconds))
          (cdr known)
        (let ((host (emjupy--ssh-host-forwarding-1 port)))
          (puthash port (cons (float-time) host) emjupy--tunnel-hosts)
          host)))))

(defun emjupy--ssh-host-forwarding-1 (port)
  "Look through this machine\='s processes for an SSH tunnel forwarding PORT.
Run from a local directory whatever the current buffer\='s: from a TRAMP
one -- a shadow file beside a remote notebook -- `ps\=' ran on the remote
machine, through TRAMP, where the tunnel is not."
  (let ((default-directory temporary-file-directory))
    (when (executable-find "ps")
      (let ((lines (split-string
                    (shell-command-to-string "ps -eo args= 2>/dev/null") "\n" t))
            (pattern (format "-L *\\(?:[^ :]*:\\)?%s:" port))
            (found nil))
        (dolist (line lines found)
          ;; The command may be a full path, and may be a wrapper that keeps
          ;; a tunnel alive.  Matching only a line beginning "ssh" missed
          ;; /usr/bin/ssh and autossh, which is most tunnels that were not
          ;; typed by hand a moment ago.
          (when (and (not found)
                     (string-match-p "\\`\\(?:[^ ]*/\\)?\\(?:auto\\)?ssh\\(?: \\|\\'\\)" line)
                     (string-match-p pattern line))
            (setq found (emjupy--ssh-destination line))))))))

(defun emjupy--ssh-host-for (server)
  "Return the name of the host SERVER is on, or nil."
  (let ((configured (cond
                     ((stringp emjupy-ssh-host) emjupy-ssh-host)
                     ((consp emjupy-ssh-host)
                      (cdr (assoc (emjupy--server-label server) emjupy-ssh-host))))))
    (or configured
        (emjupy--ssh-host-forwarding
         (plist-get (emjupy--server-parts server) :port)))))

(defun emjupy--remote-root-for-files (server)
  "Return the configured file-name root for SERVER, if it names another machine.

`emjupy-remote-root\=' is what the user sets when the server is reached
through a tunnel, which is exactly when nothing else can say where the
files are."
  (let ((root (emjupy--configured-root-for server)))
    (and root (file-remote-p root) root)))

(defun emjupy--tramp-root-for (server)
  "Return a TRAMP path to SERVER\='s files, or nil if one cannot be built.

Two things are needed and only one is ever guaranteed.  The absolute
path comes from a kernel, which reports where it runs.  The machine has
to come from the address emjupy connects to -- and that only names the
machine when the connection goes to it directly.

A server reached through `ssh -L 9999:localhost:9999 host\' answers at
localhost, and nothing in the HTTP conversation mentions `host\' at all:
the tunnel is invisible from this end, which is the whole point of a
tunnel.  So exactly in the case where a TRAMP path is most wanted, the
address cannot supply it, and `emjupy-remote-root\' has to say."
  (let* ((parts (emjupy--server-parts server))
         (host (plist-get parts :host))
         (root (emjupy--server-side-root-for server)))
    (cond
     ;; The address names the machine directly.
     ((and root host (not (member (downcase host) emjupy--local-hostnames)))
      (format "/ssh:%s:%s" host root))
     ;; It does not -- a tunnel -- but the tunnel itself can be asked, or
     ;; `emjupy-ssh-host\' told.  Either way the PATH still comes from the
     ;; kernel, so projects on one machine keep their own roots.
     (root
      (when-let* ((via (emjupy--ssh-host-for server)))
        (format "/ssh:%s:%s" via root))))))

(defun emjupy--server-side-root-for (server)
  "Return the absolute path SERVER serves, on SERVER\='s own filesystem.

Deliberately not `emjupy--remote-root-for\', which answers \"how do I
reach these files\" and may be a TRAMP location meaning nothing to the
Contents API or to a language server.  This answers \"what does the
server call them\", and only a root derived from a running kernel can
say, since a kernel reports an absolute path on the machine it runs on."
  (and server (emjupy-server-root server)))

(defun emjupy-open-server-file (path &optional server line)
  "Open PATH from SERVER in a read-only buffer, and go to LINE.

PATH is absolute on the machine the server runs on.  The file is fetched
through the Contents API -- the same connection the notebook came down --
so jumping into a module beside a remote notebook needs no TRAMP, no
mirror, and nothing configured.

Read-only on purpose: this is a copy fetched over HTTP, and writing it
back is a different job from reading it.  The buffer is named after the
server so two servers holding the same path do not collide."
  (let* ((server (or server (emjupy--server)))
         (rel (emjupy--server-relative-path server path)))
    (unless rel
      (user-error "%s is outside what %s serves"
                  path (emjupy--server-label server)))
    (let* ((name (format "*emjupy: %s [%s]*"
                         (file-name-nondirectory path)
                         (emjupy--server-label server)))
           (existing (get-buffer name))
           (buf (or existing (generate-new-buffer name))))
      (unless existing
        (let* ((res (emjupy--http-request
                     "GET" server
                     (concat (emjupy--contents-path rel) "?type=file&format=text")))
               (content (and (hash-table-p res) (gethash "content" res))))
          (unless (stringp content)
            (kill-buffer buf)
            (user-error "Could not read %s from %s" rel (emjupy--server-label server)))
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert content))
            (goto-char (point-min))
            (let ((python-indent-guess-indent-offset-verbose nil))
              (delay-mode-hooks (python-mode)))
            (setq-local default-directory temporary-file-directory)
            ;; What it is, and where it came from, so nobody edits a copy
            ;; believing it is the original.
            (setq-local header-line-format
                        (format " %s on %s -- read-only copy"
                                path (emjupy--server-label server)))
            (setq buffer-read-only t))))
      (with-current-buffer buf
        (when line
          (goto-char (point-min))
          (forward-line (max 0 (1- line)))))
      buf)))

(provide 'emjupy-remote)

;;; emjupy-remote.el ends here
