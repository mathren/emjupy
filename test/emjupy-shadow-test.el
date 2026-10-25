;;; emjupy-shadow-test.el --- Unit tests: emjupy-shadow.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-shadow.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-eglot-live-server-rejects-dead-ones ()
  "A server whose process has died must not be handed back.

Eglot's own capf calls `eglot--current-server-or-lose' on it, which
signals \"No current JSON-RPC connection\" -- and that jsonrpc-error
escaped the completion machinery and reached the user."
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) nil)))
    (should-not (emjupy--eglot-live-server)))
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) 'srv))
            ((symbol-function 'jsonrpc-running-p) (lambda (&rest _) nil)))
    (should-not (emjupy--eglot-live-server)))
  (cl-letf (((symbol-function 'eglot-current-server) (lambda (&rest _) 'srv))
            ((symbol-function 'jsonrpc-running-p) (lambda (&rest _) t)))
    (should (eq (emjupy--eglot-live-server) 'srv))))

(ert-deftest emjupy-test-shadow-directory-can-be-remote ()
  "`emjupy-shadow-directory' selects where the shadow file lives, so the
language server can be started on the machine the kernel runs on.

Computing the path must not touch the filesystem: with a TRAMP
directory that would open an ssh connection just to ask for a name."
  (let ((nb (make-emjupy-notebook
             :path "a.ipynb"
             :server (make-emjupy-server :base-url "localhost:18888"))))
    (let ((emjupy-shadow-directory nil))
      (should (string-prefix-p (emjupy--shadow-temporary-directory)
                               (emjupy--shadow-file-path nb))))
    (let ((emjupy-shadow-directory "/tmp/custom-shadow"))
      (should (string-prefix-p "/tmp/custom-shadow/"
                               (emjupy--shadow-file-path nb))))
    ;; a TRAMP path is carried through verbatim, with no connection attempt
    (let* ((emjupy-shadow-directory "/ssh:user@host:/tmp/emjupy-shadow")
           (path (emjupy--shadow-file-path nb)))
      (should (string-prefix-p "/ssh:user@host:/tmp/emjupy-shadow/" path))
      (should (string-suffix-p ".py" path)))))

(ert-deftest emjupy-test-shadow-coding-is-pinned ()
  "The shadow file's coding system is fixed, not negotiated.

Left to itself `write-region' calls `select-safe-coding-system', which
for content it cannot encode cleanly stops and ASKS, defaulting to
`raw-text' -- and a shadow file written as raw text reads back as
mojibake."
  (should (eq emjupy--shadow-coding 'utf-8-unix)))

(ert-deftest emjupy-test-shadow-sits-beside-the-notebook ()
  "The shadow file goes in the notebook's own directory when one can be
worked out.

A language server resolves `import mylib' relative to the directory of
the file it is reading, so a shadow file in a temp directory cannot see
the .py next to the notebook -- which is why looking up your own
functions answered \"No definition found\"."
  (let* ((server (make-emjupy-server :base-url "localhost:8888" :token "t"))
         (nb (make-emjupy-notebook :cells [] :path "sub/nb.ipynb"
                                   :server server)))
    ;; with a mapping, beside the notebook
    (let ((emjupy-remote-root "/tmp/proj")
          (emjupy-shadow-directory nil)
          (emjupy-shadow-beside-notebook t))
      (should (equal (emjupy--shadow-directory-for nb) "/tmp/proj/sub/")))
    ;; without one there is nothing to derive, so the temp dir
    (let ((emjupy-remote-root nil)
          (emjupy-shadow-directory nil)
          (emjupy-shadow-beside-notebook t))
      (should (string-match-p "emjupy-shadow"
                              (emjupy--shadow-directory-for nb))))
    ;; an explicit setting always wins
    (let ((emjupy-remote-root "/tmp/proj")
          (emjupy-shadow-directory "/tmp/explicit"))
      (should (equal (emjupy--shadow-directory-for nb) "/tmp/explicit/")))))

(ert-deftest emjupy-test-shadow-directory-from-the-kernel ()
  "The kernel is asked where it is, so no configuration is needed when it
runs on this machine -- and a remote path is refused rather than read as
a local one."
  (let ((nb (make-emjupy-notebook :path "sub/nb.ipynb" :cells []
                                  :server (make-emjupy-server :base-url "h"
                                                              :token "t"))))
    ;; a directory that exists here: the kernel is local, use it
    (setf (emjupy-notebook-kernel-cwd nb) temporary-file-directory)
    (let ((emjupy-shadow-host nil))
      (should (equal (emjupy--notebook-directory-from-kernel nb)
                     (file-name-as-directory temporary-file-directory))))
    ;; a path that does not exist here is a remote one.  Using it would
    ;; write into whatever that path happens to be on this disk.
    (setf (emjupy-notebook-kernel-cwd nb) "/srv/definitely/not/here")
    (let ((emjupy-shadow-host nil))
      (should-not (emjupy--notebook-directory-from-kernel nb)))
    ;; with a prefix it becomes addressable
    (let ((emjupy-shadow-host "/ssh:box:"))
      (should (equal (emjupy--notebook-directory-from-kernel nb)
                     "/ssh:box:/srv/definitely/not/here/")))))

(ert-deftest emjupy-test-remote-kernel-without-a-host-falls-back ()
  "With a remote kernel and no `emjupy-shadow-host', the shadow file goes
to a temp directory -- xref will not find your modules, but nothing is
written to a wrong path."
  (let ((nb (make-emjupy-notebook :path "sub/nb.ipynb" :cells []
                                  :server (make-emjupy-server :base-url "h"
                                                              :token "t"))))
    (setf (emjupy-notebook-kernel-cwd nb) "/srv/definitely/not/here")
    (let ((emjupy-shadow-host nil)
          (emjupy-shadow-directory nil)
          (emjupy-remote-root nil))
      (should (string-match-p "emjupy-shadow"
                              (emjupy--shadow-directory-for nb))))))

(ert-deftest emjupy-test-shadow-setup-backs-off-after-failure ()
  "A shadow buffer that cannot be built is not retried after every command.

Completion and eldoc ask for it once per command.  When the notebook is
remote and the host is unreachable, each attempt blocks Emacs while
TRAMP waits, so retrying every time turns one slow failure into what
looks exactly like a hang."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x = 1"
                                 :outputs [] :metadata (make-hash-table)))
         (attempts 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
        (let ((emjupy-shadow-directory "/ssh:nosuchhost.invalid:/tmp/x/")
              (emjupy-shadow-retry-interval 60))
          (cl-letf (((symbol-function 'make-directory)
                     ;; the host unreachable: TRAMP's file error
                     (emjupy-test--failing-make-directory
                      "nosuchhost" 'remote-file-error "host unreachable"
                      (lambda () (setq attempts (1+ attempts))))))
            (dotimes (_ 5)
              (ignore-errors (emjupy--ensure-shadow-buffer nb))))
          ;; tried once, then left alone
          (should (= attempts 1))
          (should (emjupy--shadow-blocked-p))
          ;; and once the window passes it is willing again
          (setq emjupy--shadow-blocked-until nil)
          (should-not (emjupy--shadow-blocked-p)))))))

(ert-deftest emjupy-test-failed-eglot-connect-backs-off ()
  "A language server that will not start is tried once, not per command.

Connecting is the expensive half and was retried whenever the server was
not attached -- which, if it cannot start at all, is after every
command.  Over TRAMP that is a remote process launch each time, landing
squarely on the user while they are running cells."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x = 1"
                                 :outputs [] :metadata (make-hash-table)))
         (connects 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-path emjupy--buffer-notebook) "nb.ipynb")
        (let ((emjupy-shadow-directory
               (expand-file-name "emjupy-connect-test"
                                 temporary-file-directory))
              (emjupy-shadow-retry-interval 60))
          (cl-letf (((symbol-function 'eglot--guess-contact)
                     (lambda (&rest _) (list nil nil nil nil)))
                    ((symbol-function 'eglot--connect)
                     (lambda (&rest _) (setq connects (1+ connects))
                       (error "no server there"))))
            (dotimes (_ 6)
              (ignore-errors (emjupy--ensure-shadow-buffer nb))))
          (should (= connects 1))
          ;; recorded in the NOTEBOOK buffer, where the next request comes
          ;; from -- the connect itself runs in the shadow buffer, and a flag
          ;; left there would be invisible and the attempt would repeat
          (should (emjupy--shadow-blocked-p)))))))

(ert-deftest emjupy-test-shadow-fallback-is-a-choice ()
  "When the kernel is elsewhere, a local shadow is used unless refused.

The server then describes THIS machine -- its interpreter, its packages,
none of the modules beside the notebook -- which for ordinary Python is
mostly right and worth having, and for anything environment-dependent is
confidently wrong.  So it is offered by default and
`emjupy-shadow-when-kernel-unreachable' nil declines it."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((emjupy-shadow-host nil))
          (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
                "/srv/nowhere")
          ;; default: build it
          (let ((emjupy-shadow-when-kernel-unreachable t))
            (should-not (emjupy--shadow-would-mislead-p nb)))
          ;; declined: do not
          (let ((emjupy-shadow-when-kernel-unreachable nil))
            (should (emjupy--shadow-would-mislead-p emjupy--buffer-notebook))
            (let ((built nil))
              (cl-letf (((symbol-function 'emjupy--ensure-shadow-buffer)
                         (lambda (&rest _) (setq built t) nil)))
                (emjupy--cell-shadow-delegate (lambda (&rest _) t))
                (should-not built)))))
        ;; a kernel we can reach is never in question
        (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
              temporary-file-directory)
        (let ((emjupy-shadow-when-kernel-unreachable nil))
          (should-not (emjupy--shadow-would-mislead-p nb)))))))

(ert-deftest emjupy-test-socket-shadow-is-local ()
  "When the WebSocket transport is used, the shadow file stays here.

That transport never reads the file -- it is only something for Eglot to
attach to -- so putting it on the remote host buys nothing and costs
everything: building it over TRAMP is slow, can time out, and when it
does the attach waiting on it never happens, which is reported as a
socket that never opened."
  (let ((nb (make-emjupy-notebook :cells [] :path "n.ipynb"
                                  :kernel-cwd "/srv/elsewhere"
                                  :server (make-emjupy-server :base-url "h"
                                                              :token "t"))))
    (let ((emjupy-shadow-host "/ssh:box:")
          (emjupy-remote-root "/ssh:box:~/"))
      ;; ordinarily the shadow follows the notebook, remote and all
      (let ((emjupy--force-local-shadow nil))
        (should (string-prefix-p "/ssh:" (emjupy--shadow-directory-for nb))))
      ;; but not when the socket is what will attach to it
      (let ((emjupy--force-local-shadow t))
        (should-not (string-prefix-p "/ssh:"
                                     (emjupy--shadow-directory-for nb)))))))

;;;; Performance

(ert-deftest emjupy-test-local-language-server-says-so ()
  "Falling back to a language server here is announced, once.

It reads this machine's files and this machine's Python, so it answers
about neither the code nor the environment the notebook runs in.  The
standard library still resolves, which is what makes it easy to miss --
the imports that quietly come back empty are the user's own modules.
Silence there cost a long diagnosis, and the transcript that settled it
began with the words `Running language server: /usr/bin/pylsp'."
  (let ((cell (emjupy-test--cell-like 'code "x = 1"))
        (said nil))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
              "/srv/elsewhere")
        (setq emjupy--lsp-last-failure "Connection refused")
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (when fmt (push (apply #'format fmt args) said)))))
          (emjupy--warn-local-language-server emjupy--buffer-notebook)
          ;; says what it means, and why it happened
          (let ((warning (seq-find (lambda (m)
                                     (string-match-p "THIS machine" m)) said)))
            (should warning)
            (should (string-match-p "Connection refused" warning))
            (should (string-match-p "emjupy-lsp-diagnose" warning)))
          ;; and does not repeat for this notebook
          (setq said nil)
          (emjupy--warn-local-language-server emjupy--buffer-notebook)
          (should-not (seq-find (lambda (m)
                                  (string-match-p "THIS machine" m)) said)))))))

;;;; The undo guard

(ert-deftest emjupy-test-shadow-files-go-in-a-folder-of-the-users-own ()
  "Shadow files left for /tmp go in a folder named after the user, private.
One /tmp/emjupy-shadow was made by whoever came first, and nobody else
could write in it -- and what it held, a notebook's code, others could
read."
  (let ((dir (emjupy--shadow-temporary-directory)))
    (should (string-prefix-p (expand-file-name temporary-file-directory) dir))
    (should (string-match-p (regexp-quote (user-login-name)) dir)))
  (let* ((temporary-file-directory (file-name-as-directory
                                    (make-temp-file "emjupy-tmp" t)))
         (dir (emjupy--shadow-temporary-directory)))
    (unwind-protect
        (progn
          (emjupy--private-directory dir "shadow files")
          (should (file-directory-p dir))
          (should (= (logand (file-modes dir) #o777) #o700))
          ;; made first by someone else, or a link to elsewhere: refused
          (delete-directory dir)
          (let ((elsewhere (make-temp-file "emjupy-elsewhere" t)))
            (make-symbolic-link elsewhere (directory-file-name dir))
            (should-error (emjupy--private-directory dir "shadow files")
                          :type 'file-error)
            (delete-file (directory-file-name dir))
            (delete-directory elsewhere)))
      (delete-directory temporary-file-directory t))))

(ert-deftest emjupy-test-shadow-does-not-invent-a-local-directory ()
  "The shadow goes to a temp directory rather than a made-up local path.

Keeping it beside the notebook is what lets a language server resolve
`import mylib' by looking next to the file.  But when the shadow must
stay on this machine and the notebook is on another, \"beside\" names a
LOCAL directory after a REMOTE path -- so emjupy created
/home/you/project/ here and put the shadow in it, which is how a
definition lookup came to open an empty buffer at a plausible path on
the wrong machine.

The socket transport never reads the file: it rewrites the paths it
reports, and the server is told separately where the modules are.  A
temp directory does the same job without inventing directories."
  (let ((nb (make-emjupy-notebook :cells [] :path "n.ipynb"
                                  :kernel-cwd "/srv/somewhere-else"
                                  :server (make-emjupy-server :base-url "h"
                                                              :token "t"))))
    (let ((emjupy-shadow-host "/ssh:box:")
          (emjupy-remote-root "/ssh:box:~/")
          (emjupy-shadow-beside-notebook t))
      ;; with the socket attaching, neither remote nor a local look-alike
      (let* ((emjupy--force-local-shadow t)
             (dir (emjupy--shadow-directory-for nb)))
        (should-not (file-remote-p dir))
        (should-not (string-match-p "somewhere-else" dir))
        (should (string-prefix-p (emjupy--shadow-temporary-directory) dir)))
      ;; and a directory that really is here is still used
      (let* ((emjupy--force-local-shadow t)
             (here (file-name-as-directory temporary-file-directory))
             (nb-here (make-emjupy-notebook
                       :cells [] :path "n.ipynb" :kernel-cwd here
                       :server (make-emjupy-server :base-url "h" :token "t")))
             (emjupy-remote-root here))
        (should (string-prefix-p here
                                 (emjupy--shadow-directory-for nb-here)))))))

(provide 'emjupy-shadow-test)

;;; emjupy-shadow-test.el ends here
