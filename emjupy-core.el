;;; emjupy-core.el --- Core kernel and connection management -*- lexical-binding: t; -*-
(require 'jupyter)
(require 'jupyter-base)
(require 'jupyter-server)
(require 'jupyter-repl)
(require 'jupyter-kernel)
(require 'jupyter-client)
(require 'eglot)

(defgroup emjupy nil "Jupyter integration for code-cells.el." :group 'programming)

(defcustom emjupy-save-outputs t
  "If non-nil, save graphical and text outputs into the companion .ipynb file."
  :type 'boolean)

;;;###autoload
(defun emjupy-connect (port)
  "Connect to a Jupyter kernel on PORT."
  (interactive "nConnect to port: ")
  ;; Safety check: ensure the server module is loaded
  (unless (fboundp 'jupyter-run-server-session)
    (require 'jupyter-server))
  (let* ((session (jupyter-run-server-session :name (format "emjupy-%d" port) :port port))
         (client (jupyter-session-client session)))
    (jupyter-repl-associate-buffer session)
    (jupyter-kernel-info client
      (lambda (info)
        (let ((py-path (plist-get (plist-get info :content) :executable)))
          (setq-local eglot-server-programs `((python-mode . (,py-path "-m" "pylsp"))))
          (eglot-ensure))))))

(defun emjupy-restart ()
  "Restart the current jupyter kernel."
  (interactive)
  (if-let ((session (jupyter-current-session)))
      (jupyter-repl-restart-kernel session)
    (message "No active session found.")))

(provide 'emjupy-core)
