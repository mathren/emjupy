;;; emjupy-run-tests.el --- Batch test runner for emjupy -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Run with: emacs -batch -Q -l test/emjupy-run-tests.el, from anywhere; it puts
;; its own directory and ../src on `load-path'.
;;
;; Loads the dependency, the package and the tests, then runs ERT -- the package
;; alone defines no tests, and would pass with none run. websocket is found on
;; `load-path', in EMJUPY_WEBSOCKET_DIR, or else installed from GNU ELPA.

;;; Code:

(require 'package)

;; The tests beside this file, the package in ../src: found however this
;; is started, not only from the repository's root.
(let ((here (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path here)
  (add-to-list 'load-path (expand-file-name "../src" here)))

(let ((vendored (getenv "EMJUPY_WEBSOCKET_DIR")))
  (when (and vendored (file-directory-p vendored))
    (add-to-list 'load-path vendored)))

(unless (locate-library "websocket")
  (setq package-user-dir (expand-file-name "/tmp/emjupy-test-packages"))
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/") t)
  (package-initialize)
  (unless (package-installed-p 'websocket)
    (package-refresh-contents)
    (package-install 'websocket)))

;; Coverage, when asked for: undercover instruments a file as it loads, so it is
;; set up before the package is required, from .el files -- hence `make clean'
;; first.  EMJUPY_COVERAGE names the lcov file; EMJUPY_UNDERCOVER_PATH the
;; directories holding undercover.el and what it needs.
(when-let* ((report (getenv "EMJUPY_COVERAGE")))
  (dolist (dir (split-string (or (getenv "EMJUPY_UNDERCOVER_PATH") "") ":" t))
    (add-to-list 'load-path dir))
  (require 'undercover)
  ;; undercover only reports under a recognised CI unless forced; forced
  ;; always, so the report is the same locally and on GitHub
  (setq undercover-force-coverage t)
  ;; Every source file is measured, found rather than listed: a list went
  ;; stale when files were split, and unmeasured code says nothing.  Not the
  ;; package metadata, the tests, or these scripts.  Absolute names, as the
  ;; patterns are resolved against the directory Emacs was started in.
  (let ((src (expand-file-name "../src" (file-name-directory
                                          (or load-file-name buffer-file-name)))))
    (eval `(undercover
            ,@(seq-remove (lambda (f) (string-suffix-p "emjupy-pkg.el" f))
                          (directory-files src t "\\`emjupy.*\\.el\\'"))
            (:report-format 'lcov)
            (:report-file ,report)
            (:send-report nil)))))

(require 'emjupy)
(require 'emjupy-test)
(require 'emjupy-robustness-test)

;; Integration tests (real Jupyter server / ssh tunnel / language server) are
;; opt-in: they are skipped unless the relevant environment variables point at
;; a live server, so the default run stays fast and self-contained.
(when (locate-library "emjupy-integration-test")
  (require 'emjupy-integration-test))

(message "Dependencies loaded. Running emjupy ERT tests...")

;; Unit tests must not depend on the machine running them: its tunnels, or
;; whether LaTeX is installed.  The integration tests rebind this where they
;; need to.
(setq emjupy-probe-environment nil)

;; A test tagged :unstable is about a real fault not yet fixed, and fails some
;; of the time; EMJUPY_SKIP_UNSTABLE leaves it out, keeping CI's verdict
;; meaningful without hiding the fault.  Tests tagged :timing are left out of a
;; coverage run, where instrumentation slows everything several times.
(let ((excluded (append (and (getenv "EMJUPY_SKIP_UNSTABLE") '(:unstable))
                        (and (getenv "EMJUPY_COVERAGE") '(:timing)))))
  (ert-run-tests-batch-and-exit
   (if excluded
       `(not (or ,@(mapcar (lambda (tag) `(tag ,tag)) excluded)))
     t)))

;;; emjupy-run-tests.el ends here
