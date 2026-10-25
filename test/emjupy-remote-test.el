;;; emjupy-remote-test.el --- Unit tests: emjupy-remote.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-remote.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-dashboard-remote-root-lookup ()
  "`emjupy-remote-root' takes one directory for every server, or an
alist keyed by server."
  (let ((a (make-emjupy-server :base-url "localhost:8888"))
        (b (make-emjupy-server :base-url "localhost:9999")))
    (let ((emjupy-remote-root nil))
      (should-not (emjupy--remote-root-for a)))
    (let ((emjupy-remote-root "/ssh:box:/srv/nb"))
      (should (equal (emjupy--remote-root-for a) "/ssh:box:/srv/nb"))
      (should (equal (emjupy--remote-root-for b) "/ssh:box:/srv/nb")))
    (let ((emjupy-remote-root '(("localhost:8888" . "~/notebooks")
                               ("localhost:9999" . "/ssh:box:/srv/nb"))))
      (should (equal (emjupy--remote-root-for a) "~/notebooks"))
      (should (equal (emjupy--remote-root-for b) "/ssh:box:/srv/nb")))))

(ert-deftest emjupy-test-server-root-is-derived-not-configured ()
  "The directory a server serves is worked out, not asked for.

Jupyter does not publish its root -- deliberately.  But two things that
are known overlap: the kernel reports the directory it runs in, which is
the notebook's own folder, and the Contents API gives the notebook's
path relative to the root.  Take the second off the end of the first."
  (cl-flet ((derive (cwd path)
              (emjupy--derive-server-root
               (make-emjupy-notebook :cells [] :path path :kernel-cwd cwd))))
    (should (equal (derive "/home/you/project/analysis" "analysis/run.ipynb")
                   "/home/you/project"))
    ;; a notebook in the root: the kernel's directory IS the root
    (should (equal (derive "/home/you/project" "run.ipynb")
                   "/home/you/project"))
    (should (equal (derive "/srv/nb/a/b" "a/b/x.ipynb") "/srv/nb"))
    ;; when the two do not line up, say so rather than invent a path
    (should-not (derive "/tmp/somewhere" "a/b/x.ipynb"))
    (should-not (derive nil "a/b/x.ipynb"))))

(ert-deftest emjupy-test-server-root-remembered-and-overridable ()
  "A derived root is kept on the server; one set by hand still wins."
  (let* ((server (make-emjupy-server :base-url "box:9999" :token "t"))
         (nb (make-emjupy-notebook :cells [] :path "analysis/run.ipynb"
                                   :kernel-cwd "/home/you/project/analysis"
                                   :server server)))
    (let ((emjupy-remote-root nil))
      (should-not (emjupy--remote-root-for server))
      (emjupy--remember-server-root nb)
      (should (equal (emjupy--remote-root-for server) "/home/you/project")))
    ;; a value set by hand may point at a mirror, or through a TRAMP hop
    ;; emjupy cannot infer, so it takes precedence
    (let ((emjupy-remote-root "/ssh:box:/mirror"))
      (should (equal (emjupy--remote-root-for server) "/ssh:box:/mirror")))))

(ert-deftest emjupy-test-server-relative-path ()
  "Absolute server paths convert to Contents-API paths, or fail plainly."
  (let ((server (make-emjupy-server :base-url "box:9999" :token "t"
                                    :root "/srv/nb")))
    (let ((emjupy-remote-root nil))
      (should (equal (emjupy--server-relative-path server "/srv/nb/lib/mod.py")
                     "lib/mod.py"))
      (should (equal (emjupy--server-relative-path server "/srv/nb/top.py")
                     "top.py"))
      ;; outside what the server serves: say so rather than guess
      (should-not (emjupy--server-relative-path server "/etc/passwd"))
      (should-not (emjupy--server-relative-path server nil)))))

(ert-deftest emjupy-test-tramp-root-from-the-address ()
  "A TRAMP path is built when the address names a real machine.

The absolute path comes from a kernel.  The machine can only come from
the address emjupy connects to, and that names the machine only when the
connection goes straight to it.  A tunnelled server answers at localhost
and never mentions the host the tunnel leads to -- so exactly where a
TRAMP path is most wanted, the address cannot supply one."
  ;; a direct connection to a named host
  (should (equal (emjupy--tramp-root-for
                  (make-emjupy-server :base-url "box:8888" :token "t"
                                      :root "/home/you/nb"))
                 "/ssh:box:/home/you/nb"))
  (should (equal (emjupy--tramp-root-for
                  (make-emjupy-server :base-url "http://box.example:8888"
                                      :token "t"
                                      :root "/srv/nb"))
                 "/ssh:box.example:/srv/nb"))
  ;; a tunnel: the address is local and names no host.  The detector is stubbed,
  ;; or this would find the tunnels of the machine running it.
  (cl-letf (((symbol-function 'emjupy--ssh-host-forwarding)
             (lambda (&rest _) nil)))
    (let ((emjupy-ssh-host nil))
      (dolist (local '("localhost:9999" "127.0.0.1:9999"))
        (should-not (emjupy--tramp-root-for
                     (make-emjupy-server :base-url local :token "t"
                                         :root "/srv/nb"))))))
  ;; and with no kernel having reported, there is no path to append
  (should-not (emjupy--tramp-root-for
               (make-emjupy-server :base-url "box:8888" :token "t"))))

(ert-deftest emjupy-test-ssh-destination-parsing ()
  "The host is the last word that is neither an option nor an option's
argument.  Tedious, but not ambiguous."
  (should (equal (emjupy--ssh-destination "ssh -L 9999:localhost:9999 ua_w")
                 "ua_w"))
  ;; the destination may come before the options
  (should (equal (emjupy--ssh-destination "ssh ua_w -L 9999:localhost:9999 -N")
                 "ua_w"))
  ;; options that take a value must not be mistaken for one
  (should (equal (emjupy--ssh-destination
                  "ssh -o BatchMode=yes -p 2222 -L 19100:localhost:18888 -N -f me@box")
                 "me@box"))
  (should-not (emjupy--ssh-destination "ssh -N")))

(ert-deftest emjupy-test-ssh-host-is-a-host-not-a-path ()
  "`emjupy-ssh-host' names the machine only; the path comes from the kernel.

That is what lets several projects on one machine each keep their own
root while the machine is said once."
  (let ((s1 (make-emjupy-server :base-url "localhost:9999" :token "t"
                                :root "/home/you/project-one"))
        (s2 (make-emjupy-server :base-url "localhost:9999" :token "t"
                                :root "/home/you/project-two")))
    (let ((emjupy-ssh-host "box"))
      (should (equal (emjupy--tramp-root-for s1)
                     "/ssh:box:/home/you/project-one"))
      (should (equal (emjupy--tramp-root-for s2)
                     "/ssh:box:/home/you/project-two")))
    ;; per-server, for more than one machine
    (let ((emjupy-ssh-host '(("localhost:9999" . "box"))))
      (should (equal (emjupy--tramp-root-for s1)
                     "/ssh:box:/home/you/project-one")))
    ;; and with nothing to go on, nothing is invented
    (let ((emjupy-ssh-host nil))
      (cl-letf (((symbol-function 'emjupy--ssh-host-forwarding)
                 (lambda (&rest _) nil)))
        (should-not (emjupy--tramp-root-for s1))))))

;;;; The undo guard

(ert-deftest emjupy-test-tunnel-detected-however-ssh-was-invoked ()
  "A tunnel is recognised whether or not the command line begins \"ssh\".

Matching only a line starting with `ssh' missed /usr/bin/ssh and
autossh -- between them most tunnels that were not typed by hand a
moment ago.  A missed tunnel means no host, so no remote name, so
\\[xref-find-definitions] lands on a local path that is not there."
  (let ((emjupy-probe-environment t))
    (dolist (line '("ssh -N -L 9999:localhost:9999 me@box"
                    "/usr/bin/ssh -o ControlMaster=auto -f -N -L 9999:localhost:9999 me@box"
                    "autossh -M 0 -N -L 9999:localhost:9999 me@box"))
      (cl-letf (((symbol-function 'shell-command-to-string)
                 (lambda (&rest _) (concat line "\n")))
                ((symbol-function 'executable-find) (lambda (&rest _)
                                                      "/bin/ps")))
        (should (equal (let ((emjupy--tunnel-hosts (make-hash-table)))
                         (emjupy--ssh-host-forwarding 9999)) "me@box"))))
    ;; and something that merely mentions ssh is not a tunnel
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (&rest _) "emacs --eval (ssh -L 9999:localhost:9999)\n"))
              ((symbol-function 'executable-find) (lambda (&rest _) "/bin/ps")))
      (should-not (let ((emjupy--tunnel-hosts (make-hash-table)))
                    (emjupy--ssh-host-forwarding 9999))))))

(ert-deftest emjupy-test-tunnel-line-as-typed-is-enough ()
  "A tunnel written the ordinary way gives a host, a root and a name.

The line is the one a user reported: no user@, no options, the
destination an alias from ~/.ssh/config.  Every step from that line to a
file name is checked here, because the failure it caused was invisible
-- M-. opened a plausible local path rather than saying anything."
  (let ((emjupy-probe-environment t)
        (emjupy-remote-root nil)
        (emjupy-ssh-host nil)
        (line "ssh -L 9999:localhost:9999 ua_w"))
    (cl-letf (((symbol-function 'shell-command-to-string)
               (lambda (&rest _) (concat line "\n")))
              ((symbol-function 'executable-find) (lambda (&rest _) "/bin/ps")))
      (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"
                                         :root "/home/me/scripts"))
             (nb (make-emjupy-notebook :cells [] :path "t.ipynb" :server server
                                       :kernel-cwd "/home/me/scripts")))
        (should (equal (let ((emjupy--tunnel-hosts (make-hash-table)))
                         (emjupy--ssh-host-forwarding 9999)) "ua_w"))
        (should (equal (emjupy--tramp-root-for server)
                       "/ssh:ua_w:/home/me/scripts"))
        (should (equal (emjupy--remote-name-for nb
                                                "/home/me/scripts/plot_aux.py")
                       "/ssh:ua_w:/home/me/scripts/plot_aux.py"))))))

(ert-deftest emjupy-test-tunnel-lookup-runs-ps-once-and-here ()
  "Finding the SSH tunnel for a port runs ps once a minute, on this machine.
It is asked for every item a search for definitions returns; ps ran for
each.  And it ran in the current buffer's directory, which beside a
remote notebook is a TRAMP one: ps then ran on the remote machine,
through TRAMP, where the tunnel is not."
  (let ((emjupy--tunnel-hosts (make-hash-table))
        (emjupy-probe-environment t)
        (runs 0) (where nil))
    (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) "/bin/ps"))
              ((symbol-function 'shell-command-to-string)
               (lambda (&rest _)
                 (setq runs (1+ runs) where default-directory)
                 "/usr/bin/ssh -N -L 8888:localhost:8888 analysis-box\n")))
      (let ((default-directory "/ssh:analysis-box:/home/me/"))
        (dotimes (_ 30)
          (should (equal (emjupy--ssh-host-forwarding 8888) "analysis-box"))))
      (should (= runs 1))
      (should-not (file-remote-p where))
      ;; no tunnel is remembered as well
      (should-not (emjupy--ssh-host-forwarding 9999))
      (should-not (emjupy--ssh-host-forwarding 9999))
      (should (= runs 2)))))

(provide 'emjupy-remote-test)

;;; emjupy-remote-test.el ends here
