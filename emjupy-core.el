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

;; Helper to find the current client across different emacs-jupyter versions
(defun emjupy--get-client ()
  "Get the current Jupyter client. Compatible with multiple emacs-jupyter versions."
  (or (and (boundp 'jupyter-current-client) jupyter-current-client)
      (and (fboundp 'jupyter-it) (ignore-errors (jupyter-it)))
      (and (fboundp 'jupyter-current-session)
           (let ((sess (jupyter-current-session)))
             (and sess (jupyter-session-client sess))))
      (and (boundp 'jupyter-repl-client) jupyter-repl-client)))

;; BUG FIX: Patch the void 'state' variable in emacs-jupyter Issue #613
;; Fixed: Handling the "Wrong number of arguments" for jupyter-kernel-info.
(with-eval-after-load 'jupyter-repl
  (defun jupyter-repl-sync-execution-state (&rest _args)
    "Correctly define 'state' and handle varying jupyter-kernel-info signatures."
    (when-let ((client (emjupy--get-client)))
      (condition-case nil
          ;; Try calling with callback first (older API)
          (jupyter-kernel-info client
            (lambda (info)
              (let ((state (plist-get (plist-get info :content) :execution_state)))
                (message "Kernel state: %s" (or state "unknown")))))
        (wrong-number-of-arguments
         ;; Fallback for newer API (2026+) which only takes the client
         (jupyter-kernel-info client))))))

;;;###autoload
(defun emjupy-connect (url)
  "Connect to a Jupyter server URL and list available kernels/notebooks."
  (interactive (list (read-string "Jupyter Server URL: " "http://localhost:8888")))
  (let ((server (jupyter-server :url url)))
    (condition-case err
        (jupyter-server-list-kernels server)
      (error
       (if (get-buffer "*Jupyter Server*")
           (display-buffer "*Jupyter Server*")
         (message "Connection update: %s" (error-message-string err)))))))

(defun emjupy-restart ()
  "Restart the current jupyter kernel."
  (interactive)
  (if-let ((client (emjupy--get-client)))
      (let ((kernel (jupyter-kernel client)))
        (if kernel
            (jupyter-kernel-restart kernel)
          (message "Kernel object not found for this client.")))
    (message "No active jupyter client found in this buffer.")))

(provide 'emjupy-core)
