;;; emjupy-core.el --- Core kernel management and session fixes -*- lexical-binding: t; -*-
(require 'jupyter)
(require 'jupyter-base)
(require 'jupyter-server)
(require 'jupyter-repl)
(require 'jupyter-kernel)
(require 'jupyter-client)
(require 'eglot)

(defgroup emjupy nil
  "Jupyter integration for code-cells.el."
  :group 'programming)

(defcustom emjupy-save-outputs t
  "If non-nil, save graphical and text outputs into the companion .ipynb file."
  :type 'boolean)

;; BUG FIX: Patch the void 'state' variable in emacs-jupyter Issue #613
(with-eval-after-load 'jupyter-repl
  (defun jupyter-repl-sync-execution-state (client)
    "Correctly define 'state' to prevent void-variable errors."
    (jupyter-kernel-info client
      (lambda (info)
        (let ((state (plist-get (plist-get info :content) :execution_state)))
          (message "Kernel state: %s" (or state "unknown")))))))

;;;###autoload
(defun emjupy-connect (url)
  "Connect to a Jupyter server URL and list available kernels/notebooks.
Passes the URL as a keyword argument to satisfy the EIEIO constructor."
  (interactive (list (read-string "Jupyter Server URL: " "http://localhost:8888")))
  ;; The fix: use :url keyword to avoid the cl--assertion-failed error
  (let ((server (jupyter-server :url url)))
    (jupyter-server-list-kernels server)))

(defun emjupy-restart ()
  "Restart the current jupyter kernel."
  (interactive)
  ;; jupyter-it retrieves the client; jupyter-kernel gets the kernel object from it
  (if-let ((client (jupyter-it)))
      (jupyter-kernel-restart (jupyter-kernel client))
    (message "No active jupyter client found in this buffer.")))

(provide 'emjupy-core)
