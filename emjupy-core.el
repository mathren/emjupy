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

(defun emjupy--get-client ()
  "Get the current Jupyter client. Compatible with multiple emacs-jupyter versions."
  (or (and (boundp 'jupyter-current-client) jupyter-current-client)
      (and (fboundp 'jupyter-it) (ignore-errors (jupyter-it)))
      (and (boundp 'jupyter-repl-client) jupyter-repl-client)
      (and (fboundp 'jupyter-current-session)
           (let ((sess (jupyter-current-session)))
             (and sess (ignore-errors (jupyter-session-client sess)))))))

;; BUG FIX: Patch the void 'state' variable in emacs-jupyter Issue #613
(with-eval-after-load 'jupyter-repl
  (defun jupyter-repl-sync-execution-state (&rest _args)
    "Correctly define 'state' and handle varying jupyter-kernel-info signatures."
    (when-let ((client (emjupy--get-client)))
      (condition-case nil
          (jupyter-kernel-info client
            (lambda (info)
              (let ((state (plist-get (plist-get info :content) :execution_state)))
                (message "Kernel state: %s" (or state "unknown")))))
        (wrong-number-of-arguments
         (ignore-errors (jupyter-kernel-info client)))))))

;;;###autoload
(defun emjupy-connect (url)
  "Connect to a Jupyter server URL and list available kernels/notebooks."
  (interactive (list (read-string "Jupyter Server URL: " "http://localhost:8888")))
  (let ((server (jupyter-server :url url)))
    (condition-case nil
        (jupyter-server-list-kernels server)
      (error (when (get-buffer "*Jupyter Server*")
               (display-buffer "*Jupyter Server*"))))))

(defun emjupy-restart ()
  "Restart the current jupyter kernel."
  (interactive)
  (if-let ((client (emjupy--get-client)))
      (condition-case nil
          (jupyter-kernel-restart (jupyter-kernel client))
        (error (message "Restart failed: kernel busy or unavailable.")))
    (message "No active jupyter client found.")))

(provide 'emjupy-core)
