;;; emjupy-run-tests.el --- Batch test runner for emjupy -*- lexical-binding: t; -*-

;; Author: Mathieu Renzo <mathren90@gmail.com>
;; Assisted-by: Claude:claude-opus-5 and other free-tier LLMs

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

(require 'emjupy)
(require 'emjupy-test)

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
(ert-run-tests-batch-and-exit
 (if (getenv "EMJUPY_SKIP_UNSTABLE")
     '(not (tag :unstable))
   t))

;;; emjupy-run-tests.el ends here
