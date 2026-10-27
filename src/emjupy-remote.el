;;; emjupy-remote.el --- Where a Jupyter server's files live  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; Keywords: languages, tools, python, jupyter
;; URL: https://github.com/mathren/emjupy
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A server is reached over HTTP, but its files are on a machine HTTP does not
;; name: a tunnelled server answers at localhost.  This works out the directory
;; the server serves, the host behind a tunnel, and the TRAMP name that reaches
;; a file there.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-http)

(defvar python-indent-guess-indent-offset-verbose)

;;;; Options

(defcustom emjupy-remote-root nil
  "Where the notebook directory lives as a *file name*, for Dired.

The Contents API tells emjupy what notebooks a server has, but not how
to reach them as files -- and a tunnelled server looks like localhost
from here, so there is nothing to infer.  Set this to hand `d' in the
dashboard somewhere useful:

  (setq emjupy-remote-root \"/ssh:user@host:/home/user/notebooks\")

An alist maps it per server:

  (setq emjupy-remote-root
        \\='((\"localhost:8888\" . \"~/notebooks\")
          (\"localhost:9999\" . \"/ssh:box:/srv/nb\")))

nil disables `d'."
  :type '(choice (const :tag "Disabled" nil)
                 (directory :tag "One directory for every server")
                 (alist :key-type string :value-type directory))
  :group 'emjupy-remote)

(defcustom emjupy-ssh-host nil
  "How to reach the machine a tunnelled server runs on.

A host name -- what you would type after `ssh' -- or an alist keyed by
server label, or nil to work it out from the running tunnel.

This is a HOST and not a path, deliberately.  The path comes from the
kernel and differs per notebook, so a setting that included one would
have to be changed for every project on the same machine; a host is the
same for all of them."
  :type '(choice (const :tag "Work it out" nil)
                 (string :tag "Host, e.g. box")
                 (alist :key-type string :value-type string))
  :group 'emjupy-remote)

;;;; The server's root

(defconst emjupy--local-hostnames '("localhost" "127.0.0.1" "::1" "0.0.0.0" "")
  "Host names that name this machine rather than another one.")

(defun emjupy--configured-root-for (server)
  "Return the root configured for SERVER by hand, or nil."
  (cond
   ((and server (emjupy-server-files-root server)))
   ((null emjupy-remote-root) nil)
   ((stringp emjupy-remote-root) emjupy-remote-root)
   ((consp emjupy-remote-root)
    (cdr (assoc (emjupy--server-label server) emjupy-remote-root)))))

(defun emjupy--derive-server-root (nb)
  "Work out the absolute directory NB's server serves, or nil.
Jupyter does not publish it, but the kernel reports the directory it
runs in, the notebook's, and the Contents API the notebook's path
under the root.  The second off the end of the first is the root:

  kernel cwd     /home/you/project/analysis
  notebook path  analysis/run.ipynb
  root           /home/you/project"
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
The Contents API's paths are relative, a language server's absolute.
Nil for a path outside the root."
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
One set by hand wins: it may name a mirror or a TRAMP hop.  Otherwise
the root derived from a running kernel."
  (or (emjupy--configured-root-for server)
      (and server (emjupy-server-root server))))

;;;; SSH tunnels

(defconst emjupy--ssh-value-options
  '("-L" "-R" "-D" "-p" "-o" "-i" "-b" "-c" "-e" "-F" "-l" "-m"
    "-O" "-Q" "-S" "-W" "-w" "-J")
  "SSH options that take a separate argument, which is therefore not a host.")

(defun emjupy--ssh-destination (argv)
  "Return the host ARGV connects to, or nil.
ARGV is an `ssh' command line split into words.  The destination is
the last word that is neither an option nor an option's argument."
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
  "Tunnels found per local port: (TIME . HOST), HOST nil if there is none.")

(defconst emjupy--tunnel-hosts-seconds 60
  "How long the tunnel found for a port is taken as still the one.")

(defun emjupy--ssh-host-forwarding (port)
  "Return the host of a running SSH tunnel forwarding local PORT, or nil.
Read from the tunnel's command line.  Kept for
`emjupy--tunnel-hosts-seconds', the answer none included: it is asked
for every definition a search returns."
  (when (and port emjupy-probe-environment)
    (let ((known (gethash port emjupy--tunnel-hosts)))
      (if (and known (< (- (float-time) (car known))
                        emjupy--tunnel-hosts-seconds))
          (cdr known)
        (let ((host (emjupy--ssh-host-forwarding-1 port)))
          (puthash port (cons (float-time) host) emjupy--tunnel-hosts)
          host)))))

(defun emjupy--ssh-host-forwarding-1 (port)
  "Look through this machine's processes for an SSH tunnel forwarding PORT.
Run from a local directory whatever the current buffer's: from a TRAMP
one -- a shadow file beside a remote notebook -- `ps' ran on the remote
machine, through TRAMP, where the tunnel is not."
  (let ((default-directory temporary-file-directory))
    (when (executable-find "ps")
      (let ((lines (split-string
                    (shell-command-to-string
                     (format "ps -U %d -o args= 2>/dev/null" (user-uid)))
                    "\n" t))
            (pattern (format "-L *\\(?:[^ :]*:\\)?%s:" port))
            (found nil))
        (dolist (line lines found)
          ;; The command may be a full path, or a wrapper such as autossh.
          (when (and (not found)
                     (string-match-p "\\`\\(?:[^ ]*/\\)?\\(?:auto\\)?ssh\\(?: \\|\\'\\)" line)
                     (string-match-p pattern line))
            (setq found (emjupy--ssh-destination line))))))))

(defun emjupy--ssh-host-for (server)
  "Return the name of the host SERVER is on, or nil."
  (let ((configured (cond
                     ((stringp emjupy-ssh-host) emjupy-ssh-host)
                     ((consp emjupy-ssh-host)
                      (cdr (assoc (emjupy--server-label server)
                                  emjupy-ssh-host))))))
    (or configured
        (emjupy--ssh-host-forwarding
         (plist-get (emjupy--server-parts server) :port)))))

;;;; File names through TRAMP

(defun emjupy--remote-root-for-files (server)
  "Return the configured file-name root for SERVER, if it names another machine.
From `emjupy-remote-root', set when nothing else can say where the
files are."
  (let ((root (emjupy--configured-root-for server)))
    (and root (file-remote-p root) root)))

(defun emjupy--tramp-root-for (server)
  "Return a TRAMP path to SERVER's files, or nil if one cannot be built.
The path comes from a kernel; the machine from the address connected
to, which names it only without a tunnel.  Through a tunnel,
`emjupy-remote-root' has to say."
  (let* ((parts (emjupy--server-parts server))
         (host (plist-get parts :host))
         (root (emjupy--server-side-root-for server)))
    (cond
     ;; The address names the machine directly.
     ((and root host (not (member (downcase host) emjupy--local-hostnames)))
      (format "/ssh:%s:%s" host root))
     ;; It does not -- a tunnel -- but the tunnel itself can be asked, or
     ;; `emjupy-ssh-host' told.  Either way the PATH still comes from the
     ;; kernel, so projects on one machine keep their own roots.
     (root
      (when-let* ((via (emjupy--ssh-host-for server)))
        (format "/ssh:%s:%s" via root))))))

(defun emjupy--server-side-root-for (server)
  "Return the absolute path SERVER serves, on SERVER's own filesystem.
What the server calls its files, not how to reach them, which is
`emjupy--remote-root-for'.  Only a running kernel can say."
  (and server (emjupy-server-root server)))

;;;; Commands

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
                     (concat (emjupy--contents-path rel)
                             "?type=file&format=text")))
               (content (and (hash-table-p res) (gethash "content" res))))
          (unless (stringp content)
            (kill-buffer buf)
            (user-error "Could not read %s from %s" rel
                        (emjupy--server-label server)))
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
