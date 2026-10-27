;;; emjupy-figures-test.el --- Unit tests: emjupy-figures.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-figures.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; The undo guard

(ert-deftest emjupy-test-figure-window-command-is-used ()
  "A figure window is opened with `emjupy-figure-window-command', URL in."
  (let ((emjupy-figure-viewer 'window)
        (emjupy-figure-window-command '("myviewer" "--title" "x" "%s"))
        (started nil))
    (cl-letf (((symbol-function 'start-process)
               (lambda (_name _buffer program &rest args)
                 (setq started (cons program args)))))
      (emjupy--show-file "/tmp/page.html"))
    (should (equal started '("myviewer" "--title" "x"
                             "file:///tmp/page.html")))))

(ert-deftest emjupy-test-figure-window-browsers-in-order ()
  "Without the bundled window, the first browser of the list installed is used.
Firefox by default, then Chromium; a change to the list counts at once."
  (let ((emjupy-figure-window-command nil)
        (emjupy--figure-window-bundled-command nil))   ; it cannot run
    (cl-letf (((symbol-function 'executable-find)
               (lambda (p) (and (member p '("firefox" "chromium"))
                                (concat "/usr/bin/" p)))))
      (should (equal (emjupy--figure-window-command)
                     '("/usr/bin/firefox" "--new-window" "%s")))
      (let ((emjupy-figure-window-browsers '(("chromium" "--app=%s")
                                             ("firefox" "--new-window" "%s"))))
        (should (equal (emjupy--figure-window-command)
                       '("/usr/bin/chromium" "--app=%s"))))
      (let ((emjupy-figure-window-browsers '(("google-chrome" "--app=%s"))))
        ;; none installed: the caller falls back to the browser
        (should-not (emjupy--figure-window-command))))))

(ert-deftest emjupy-test-plotly-js-is-fetched-in-smaller-pieces-when-cut ()
  "A piece the server's output limit cut is asked again in halves.
A server set below Jupyter's default limit replaced the piece with a
notice, which was written into plotly.js as if it were code."
  (let* ((js (apply #'concat (make-list 20000 "var plotly = 1;\n")))
         (result (emjupy-test--plotly-fetch js 60000)))
    (should (equal (car result) js))
    ;; 100000 was cut, 50000 went through, and stayed the size
    (should (equal (seq-take (cdr result) 2) '(100000 50000)))
    (should (cl-every (lambda (n) (<= n 50000)) (cdr (cdr result)))))
  ;; a limit below the floor: given up, nothing written
  (let ((emjupy--plotly-js-min-chunk 25000))
    (should-not (car (emjupy-test--plotly-fetch
                      (make-string 200000 ?x) 10000)))))


(ert-deftest emjupy-test-figure-pages-go-to-a-folder-of-the-users-own ()
  "Figure pages are written to a folder of the user's own, made for them.
A fixed /tmp/emjupy-figures/, another user could make first: to read the
pages, or plant the plotly.js they load."
  (let* ((temporary-file-directory (make-temp-file "emjupy-tmp" t))
         (dir (emjupy--figures-dir)))
    (unwind-protect
        (progn
          (should (string-match-p (regexp-quote (user-login-name)) dir))
          (should (= (file-modes dir) #o700)))
      (delete-directory temporary-file-directory t))))


(defmacro emjupy-test--scripted-output (trusted &rest body)
  "Run BODY in a notebook whose one cell has scripted HTML, TRUSTED or not.
TRUSTED is the server's mark: t, :false, or `none' for no mark.  OPENED,
in BODY, is the file shown, if any; DATA the output."
  (declare (indent 1))
  `(let* ((data (let ((d (make-hash-table :test 'equal)))
                  (puthash "text/html" "<p>hi</p><script>alert(1)</script>" d)
                  (puthash "text/plain" "hi" d) d))
          (metadata (make-hash-table :test 'equal))
          (opened nil))
     (unless (eq ,trusted 'none) (puthash "trusted" ,trusted metadata))
     (emjupy-test--with-notebook
         (vector (make-emjupy-cell :id 1 :type 'code :source "x"
                                   :metadata metadata))
         buf nb
       (ignore nb)
       (cl-letf (((symbol-function 'emjupy--show-file)
                  (lambda (f) (setq opened f)))
                 ((symbol-function 'emjupy--write-page)
                  (lambda (_h) "page.html"))
                 ((symbol-function 'message) #'ignore))
         (emjupy--goto-cell (aref (emjupy-notebook-cells nb) 0) 'start)
         ,@body))))

(ert-deftest emjupy-test-an-untrusted-notebooks-scripts-are-not-run ()
  "Scripted HTML from a notebook Jupyter does not trust is not opened.
As Jupyter, which runs no script of a notebook saved by someone else."
  (emjupy-test--scripted-output :false
    (should-error (emjupy-open-output data) :type 'user-error)
    (should-not opened)))

(ert-deftest emjupy-test-a-trusted-notebooks-scripts-are-run ()
  "Scripted HTML from a trusted notebook, or a cell made here, opens."
  (emjupy-test--scripted-output t
    (emjupy-open-output data)
    (should opened))
  (emjupy-test--scripted-output 'none
    (emjupy-open-output data)
    (should opened)))

(ert-deftest emjupy-test-trusting-a-notebook-asks-the-server ()
  "Trusting asks the server to sign the notebook, and trusts its cells."
  (emjupy-test--scripted-output :false
    (let ((asked nil))
      (setf (emjupy-notebook-server nb)
            (make-emjupy-server :base-url "127.0.0.1:1")
            (emjupy-notebook-path nb) "dir/my nb.ipynb")
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'emjupy--http-request)
                 (lambda (method _server path &rest _)
                   (setq asked (list method path)))))
        (emjupy-trust-notebook))
      (should (equal asked '("POST" "/api/contents/dir/my%20nb.ipynb/trust")))
      (should (emjupy--cell-trusted-p (aref (emjupy-notebook-cells nb) 0))))))

(provide 'emjupy-figures-test)

;;; emjupy-figures-test.el ends here
