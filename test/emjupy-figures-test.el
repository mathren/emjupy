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

(provide 'emjupy-figures-test)

;;; emjupy-figures-test.el ends here
