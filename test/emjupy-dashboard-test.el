;;; emjupy-dashboard-test.el --- Unit tests: emjupy-dashboard.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-dashboard.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-dashboard-dired-needs-a-root ()
  "Without `emjupy-remote-root' set, `d' says so rather than guessing."
  (with-temp-buffer
    (emjupy-list-mode)
    (setq emjupy-list--server (make-emjupy-server :base-url "localhost:8888"))
    (setq emjupy-list--path "")
    (let ((emjupy-remote-root nil))
      (should-error (emjupy-list-dired) :type 'user-error))))

(ert-deftest emjupy-test-dashboard-groups-notebooks-above-kernels ()
  "Rows are grouped, not interleaved: directories, notebooks, files, then
kernels -- notebooks above kernels because opening one is the point of
the dashboard."
  (cl-letf (((symbol-function 'emjupy--http-request)
             (lambda (_m _s path &rest _)
               (if (string-match-p "kernels" path)
                   (vector (let ((k (make-hash-table :test 'equal)))
                             (puthash "id" "abcdef123456" k)
                             (puthash "name" "python3" k)
                             (puthash "connections" 1 k) k))
                 (let ((c (make-hash-table :test 'equal)))
                   (puthash "content"
                            (vector (let ((i (make-hash-table :test 'equal)))
                                      (puthash "type" "notebook" i)
                                      (puthash "name" "zeta.ipynb" i)
                                      (puthash "path" "zeta.ipynb" i) i)
                                    (let ((i (make-hash-table :test 'equal)))
                                      (puthash "type" "directory" i)
                                      (puthash "name" "adir" i)
                                      (puthash "path" "adir" i) i)
                                    (let ((i (make-hash-table :test 'equal)))
                                      (puthash "type" "notebook" i)
                                      (puthash "name" "alpha.ipynb" i)
                                      (puthash "path" "alpha.ipynb" i) i)
                                    (let ((i (make-hash-table :test 'equal)))
                                      (puthash "type" "file" i)
                                      (puthash "name" "notes.txt" i)
                                      (puthash "path" "notes.txt" i) i))
                            c)
                   c)))))
    (let* ((server (make-emjupy-server :base-url "localhost:8888" :token "t"))
           (rows (emjupy--list-entries server ""))
           (kinds (mapcar (lambda (r) (plist-get (car r) :kind)) rows))
           (names (mapcar (lambda (r)
                            (substring-no-properties (aref (cadr r) 1))) rows)))
      (should (equal kinds '(directory notebook notebook file kernel)))
      ;; sorted within each group
      (should (equal names '("adir" "alpha.ipynb" "zeta.ipynb" "notes.txt"
                             "python3")))
      ;; and each kind is faced distinctly
      (cl-loop for r in rows
               for kind in kinds
               do (should (eq (get-text-property 0 'face (aref (cadr r) 1))
                              (emjupy--list-face kind))))
      (should-not (eq (emjupy--list-face 'notebook)
                      (emjupy--list-face 'kernel))))))

(ert-deftest emjupy-test-new-notebook-starts-with-a-cell ()
  "A new notebook has one empty code cell.

A notebook with no cells offers nowhere to type, so the first act would
always be to add one."
  (let ((cell (emjupy--blank-code-cell-json)))
    (should (equal (gethash "cell_type" cell) "code"))
    (should (equal (gethash "source" cell) ""))
    (should (equal (gethash "outputs" cell) []))
    (should (gethash "id" cell)))
  ;; and it survives the round trip through the parser, which takes the
  ;; notebook itself rather than the Contents API envelope around it
  (let ((nb (make-hash-table :test 'equal)))
    (puthash "cells" (vector (emjupy--blank-code-cell-json)) nb)
    (puthash "metadata" (make-hash-table :test 'equal) nb)
    (puthash "nbformat" 4 nb)
    (puthash "nbformat_minor" 5 nb)
    (let ((parsed (emjupy--parse-ipynb (json-serialize nb))))
      (should (= (length (emjupy-notebook-cells parsed)) 1))
      (should (eq (emjupy-cell-type
                   (aref (emjupy-notebook-cells parsed) 0)) 'code)))))

(ert-deftest emjupy-test-create-new-notebook-is-on-n ()
  "Creating a notebook is `emjupy-create-new-notebook', on n in the listing."
  (should (commandp 'emjupy-create-new-notebook))
  (should (eq (lookup-key emjupy-list-mode-map (kbd "n"))
              'emjupy-create-new-notebook)))

(ert-deftest emjupy-test-list-open-file-picks-the-best-route ()
  "Opening a file from the listing prefers a route that can be written.

Three ways to reach it, differing in whether the result is editable: a
root set by hand is addressable by definition, a server on this machine
can be opened directly, and otherwise the Contents API serves a
read-only copy -- which needs no configuration and always works."
  (let ((server (make-emjupy-server :base-url "box:9999" :token "t"
                                    :root "/srv/nb"))
        (opened nil)
        (fetched nil))
    (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f)))
              ((symbol-function 'emjupy-open-server-file)
               (lambda (path &rest _) (setq fetched path))))
      (let ((emjupy-list--server server))
        ;; a root set by hand wins, TRAMP or not
        (let ((emjupy-remote-root "/ssh:box:/mirror"))
          (setq opened nil fetched nil)
          (emjupy-list-open-file "lib/mod.py")
          (should (equal opened "/ssh:box:/mirror/lib/mod.py"))
          (should-not fetched))
        ;; no configured root, but the address names a real machine: build a
        ;; TRAMP path from it and the path the kernel reported
        (let ((emjupy-remote-root nil))
          (setq opened nil fetched nil)
          (emjupy-list-open-file "lib/mod.py")
          (should (equal opened "/ssh:box:/srv/nb/lib/mod.py"))
          (should-not fetched))
        ;; a tunnelled server answers at localhost, naming no host to build a
        ;; TRAMP path from, so the file is fetched.  The detector is stubbed:
        ;; live, it reads the tunnels of the machine running the test.
        (cl-letf (((symbol-function 'emjupy--ssh-host-forwarding)
                   (lambda (&rest _) nil)))
          (let ((emjupy-remote-root nil)
                (emjupy-ssh-host nil)
                (emjupy-list--server (make-emjupy-server :base-url "localhost:9999"
                                                         :token "t"
                                                         :root "/srv/nb")))
            (setq opened nil fetched nil)
            (emjupy-list-open-file "lib/mod.py")
            (should-not opened)
            (should (equal fetched "/srv/nb/lib/mod.py"))))
        ;; nothing known about where the files are: say so
        (let ((emjupy-remote-root nil)
              (emjupy-list--server (make-emjupy-server :base-url "h"
                                                       :token "t")))
          (should-error (emjupy-list-open-file "x.py") :type 'user-error))))))

;;;; Performance

(ert-deftest emjupy-test-notebook-list-is-clickable ()
  "A row in the listing opens when clicked, as it would in any listing."
  (should (eq (lookup-key emjupy-list-mode-map [mouse-1])
              'emjupy-list-open-at-click))
  (should (eq (lookup-key emjupy-list-mode-map [double-mouse-1])
              'emjupy-list-open-at-click))
  ;; RET still works, for those who never touch the mouse
  (should (eq (lookup-key emjupy-list-mode-map (kbd "RET")) 'emjupy-list-open)))

;;;; The undo guard

(ert-deftest emjupy-test-listing-row-carries-its-modification-time ()
  "A row remembers when its item was modified, so it can be sorted on.

Read back from the row rather than re-fetched: the dashboard already
asked the server once."
  (let* ((item (make-hash-table :test 'equal)))
    (puthash "type" "notebook" item)
    (puthash "name" "a.ipynb" item)
    (puthash "path" "a.ipynb" item)
    (puthash "last_modified" "2026-09-25T09:00:00Z" item)
    ;; the shape `emjupy--list-entries' builds
    (let ((row (list (list :kind 'notebook :path "a.ipynb"
                           :modified (gethash "last_modified" item))
                     (emjupy--list-row 'notebook "notebook" "a.ipynb"
                                       (gethash "last_modified" item)))))
      (should (equal (plist-get (car row) :modified) "2026-09-25T09:00:00Z"))
      (should (string-match-p "2026-09-25" (aref (cadr row) 2))))))

(ert-deftest emjupy-test-caret-goes-up-wherever-point-is ()
  "\\`^' in the server listing shows the parent directory, from any row.

Not RET's command, which goes up only from the \"..\" row, opening a
notebook or going down into a folder elsewhere."
  (should (eq (lookup-key emjupy-list-mode-map (kbd "^")) #'emjupy-list-up))
  (with-temp-buffer
    (emjupy-list-mode)
    (let ((refreshed 0))
      (cl-letf (((symbol-function 'emjupy-list-refresh) (lambda (&rest _)
                                                          (setq refreshed (1+ refreshed))))
                ;; point on a notebook row, not on ".."
                ((symbol-function 'tabulated-list-get-id)
                 (lambda (&rest _) '(:kind notebook :path "a/b/n.ipynb"))))
        (setq emjupy-list--path "a/b")
        (emjupy-list-up)
        (should (equal emjupy-list--path "a"))
        (emjupy-list-up)
        (should (equal emjupy-list--path ""))
        ;; at the top there is nowhere to go
        (emjupy-list-up)
        (should (equal emjupy-list--path ""))
        (should (= refreshed 2))))))

(provide 'emjupy-dashboard-test)

;;; emjupy-dashboard-test.el ends here
