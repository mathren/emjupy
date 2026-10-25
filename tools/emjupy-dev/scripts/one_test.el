;;; one_test.el --- Run one emjupy ERT test  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;; REPO=/path/to/clone T=test-name EMJUPY_WEBSOCKET_DIR=... emacs -batch -Q -l one_test.el
;; Prints "ONE NAME PASS|FAIL|SKIP seconds (condition)"; exits 1 unless it passed.
;; Add EMJUPY_TEST_URL, EMJUPY_TEST_TOKEN and EMJUPY_TEST_ROOT for a live test.
(let ((repo (or (getenv "REPO") default-directory)))
  (dolist (d (list (getenv "EMJUPY_WEBSOCKET_DIR")
                   (expand-file-name "src" repo) (expand-file-name "test" repo)))
    (when d (add-to-list 'load-path d))))
(require 'ert)
(require 'emjupy)
(require 'emjupy-test)
(require 'emjupy-integration-test nil t)
(require 'emjupy-robustness-test nil t)
(let* ((name (intern (getenv "T")))
       (test (ert-get-test name))
       (start (float-time))
       (result (ert-run-test test))
       (kind (cond ((ert-test-passed-p result) "PASS")
                   ((ert-test-skipped-p result) "SKIP")
                   (t "FAIL"))))
  (message "ONE %-62s %s %.1fs %s" name kind (- (float-time) start)
           (if (equal kind "PASS") ""
             (format "%S" (ert-test-result-with-condition-condition result))))
  (kill-emacs (if (equal kind "PASS") 0 1)))
