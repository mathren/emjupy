;;; emjupy-ui.el --- Interaction, rendering, and polymode -*- lexical-binding: t; -*-
(require 'polymode)
(require 'emjupy-core)
(require 'emjupy-io)

(define-hostmode emjupy-poly-hostmode :mode 'python-mode)
(define-innermode emjupy-poly-markdown-innermode
  :mode 'markdown-mode :head-matcher "^# %% \\[markdown\\]\n" :tail-matcher "^# %%" :head-mode 'host :tail-mode 'host)
(define-innermode emjupy-poly-org-innermode
  :mode 'org-mode :head-matcher "^# %% \\[org\\]\n" :tail-matcher "^# %%" :head-mode 'host :tail-mode 'host)

(define-polymode poly-emjupy-mode
  :hostmode 'emjupy-poly-hostmode
  :innermodes '(emjupy-poly-markdown-innermode emjupy-poly-org-innermode))

(defun emjupy-save-and-export-all ()
  "Save and export to valid .ipynb format."
  (interactive)
  (let ((target-buffer (if (and (buffer-file-name) emjupy-mode)
                           (current-buffer)
                         (cl-find-if (lambda (b)
                                       (and (buffer-local-value 'emjupy-mode b)
                                            (buffer-file-name b)))
                                     (buffer-list)))))
    (if target-buffer
        (with-current-buffer target-buffer
          (save-buffer)
          (emjupy-sync-to-ipynb))
      (save-buffer))))

(defun emjupy-cycle-type ()
  "Cycle cell type between Python, Markdown, and Org."
  (interactive)
  (save-excursion
    (code-cells-backward-cell)
    (when (looking-at code-cells-boundary-regexp)
      (let* ((h (match-string 0))
             (n (cond ((string-match-p "\\[markdown\\]" h) "# %% [org]")
                      ((string-match-p "\\[org\\]" h) "# %%")
                      (t "# %% [markdown]"))))
        (replace-match n)))))

(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'emjupy-cycle-type)
    (define-key map (kbd "C-c C-e") 'emjupy-sync-to-ipynb)
    (define-key map (kbd "C-c C-r") 'emjupy-restart)
    (define-key map (kbd "C-c C-l") 'emjupy-connect)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    (define-key map (kbd "M-p") (lambda () (interactive) (code-cells-backward-cell 1)))
    (define-key map (kbd "M-n") (lambda () (interactive) (code-cells-forward-cell 1)))
    (define-key map (kbd "C-x C-s") 'emjupy-save-and-export-all)
    map))

;;;###autoload
(define-minor-mode emjupy-mode
  "Emjupy: Integrated Jupyter-Emacs workflow."
  :lighter " emjupy" :keymap emjupy-mode-map
  (if emjupy-mode
      (progn
        (unless (derived-mode-p 'poly-emjupy-mode) (poly-emjupy-mode))
        (code-cells-mode 1)
        (when-let ((client (emjupy--get-client)))
          (ignore-errors (jupyter-repl-associate-buffer client)))
        (add-hook 'after-save-hook #'emjupy-sync-to-ipynb nil t))
    (code-cells-mode -1)))

(add-hook 'jupyter-repl-mode-hook
          (lambda () (local-set-key (kbd "C-x C-s") 'emjupy-save-and-export-all)))

(provide 'emjupy-ui)
