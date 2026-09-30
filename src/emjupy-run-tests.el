;;; emjupy-run-tests.el --- Batch test runner for emjupy -*- lexical-binding: t; no-byte-compile: t -*-

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs

;; Run with: emacs -batch -Q -L . -l emjupy-run-tests.el
;;
;; Loads the dependency, the implementation, AND the test file, then runs
;; ERT. (Loading emjupy.el alone defines no tests, so the suite would
;; report "Ran 0 tests" and exit 0 -- green, but vacuous.)
;;
;; websocket is resolved in this order:
;;   1. already on `load-path' (e.g. -L /path/to/websocket)
;;   2. EMJUPY_WEBSOCKET_DIR environment variable
;;   3. package.el, installing from GNU ELPA if necessary
;; so the suite runs offline / in CI without reaching out to ELPA.

(require 'package)

(add-to-list 'load-path default-directory)

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

;; Coverage, when asked for.  undercover instruments a file as it is
;; loaded, so it has to be set up here, before the package is required, and
;; the sources must load as .el rather than .elc -- hence `make clean' first
;; in the coverage target.  EMJUPY_COVERAGE names the lcov file to write;
;; undercover itself comes from EMJUPY_UNDERCOVER_PATH, a colon-separated
;; list of directories holding undercover.el and what it requires.
(when-let* ((report (getenv "EMJUPY_COVERAGE")))
  (dolist (dir (split-string (or (getenv "EMJUPY_UNDERCOVER_PATH") "") ":" t))
    (add-to-list 'load-path dir))
  (require 'undercover)
  ;; undercover only reports under a recognised CI unless forced; forced
  ;; always, so the report is the same locally and on GitHub
  (setq undercover-force-coverage t)
;; The patterns are resolved against the directory Emacs was started in,
  ;; which is the repository root under make; absolute names avoid the
  ;; question.  The tests and these scripts are not measured.
  (let ((src (file-name-directory (or load-file-name buffer-file-name))))
    (eval `(undercover
            ,@(mapcar (lambda (f) (expand-file-name f src))
                      '("emjupy-core.el" "emjupy-http.el" "emjupy-render.el"
                        "emjupy-cells.el" "emjupy-kernel.el" "emjupy-remote.el"
                        "emjupy-lsp.el" "emjupy-eglot.el" "emjupy-mode.el"
                        "emjupy-notebook.el" "emjupy.el"))
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

;; Unit tests must not depend on the machine running them.  Without this a
;; test asking what a tunnelled server resolves to finds whatever tunnels
;; are running here, and one rendering a markdown cell behaves differently
;; depending on whether LaTeX is installed.  The integration tests, which
;; are about a real server, rebind it themselves where they need to.
(setq emjupy-probe-environment nil)

;; A test tagged :unstable is one whose subject is a real fault that has
;; not been fixed -- it fails some of the time and says something true
;; when it does.  Setting EMJUPY_SKIP_UNSTABLE keeps CI's verdict
;; meaningful without deleting the test or hiding the fault behind a
;; retry; run the suite without it to see them.
;; Tests tagged :timing assert on how long something takes, which means
;; nothing under coverage instrumentation -- every function is several times
;; slower there -- so they are left out of a coverage run.  They measure the
;; code, not an instrumented copy of it.
(let ((excluded (append (and (getenv "EMJUPY_SKIP_UNSTABLE") '(:unstable))
                        (and (getenv "EMJUPY_COVERAGE") '(:timing)))))
  (ert-run-tests-batch-and-exit
   (if excluded
       `(not (or ,@(mapcar (lambda (tag) `(tag ,tag)) excluded)))
     t)))

;;; emjupy-run-tests.el ends here
