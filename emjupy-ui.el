;;; emjupy-ui.el --- Interaction, rendering, and polymode -*- lexical-binding: t; -*-
(require 'polymode)
(require 'emjupy-core)
(require 'emjupy-io)

;; Host and Inner modes for multi-syntax cells
(define-hostmode emjupy-poly-hostmode :mode 'python-mode)

(define-innermode emjupy-poly-markdown-innermode
  :mode 'markdown-mode
  :head-matcher "^# %% \\[markdown\\]\n"
  :tail-matcher "^# %%"
  :head-mode 'host
  :tail-mode 'host)

(define-innermode emjupy-poly-org-innermode
  :mode 'org-mode
  :head-matcher "^# %% \\[org\\]\n"
  :tail-matcher "^# %%"
  :head-mode 'host
  :tail-mode 'host)

(define-polymode poly-emjupy-mode
  :hostmode 'emjupy-poly-hostmode
  :innermodes '(emjupy-poly-markdown-innermode emjupy-poly-org-innermode))

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

(defun emjupy-move-cell-up ()
  "Move current cell block up."
  (interactive)
  (code-cells-backward-cell)
  (transpose-paragraphs 1)
  (code-cells-backward-cell))

(defun emjupy-move-cell-down ()
  "Move current cell block down."
  (interactive)
  (code-cells-forward-cell)
  (transpose-paragraphs -1))

(defvar emjupy-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-t") 'emjupy-cycle-type)
    (define-key map (kbd "C-c C-e") 'emjupy-sync-to-ipynb)
    (define-key map (kbd "C-c C-r") 'emjupy-restart)
    (define-key map (kbd "C-c C-l") 'emjupy-connect)
    (define-key map (kbd "M-<up>") 'emjupy-move-cell-up)
    (define-key map (kbd "M-<down>") 'emjupy-move-cell-down)
    (define-key map (kbd "C-c C-c") 'code-cells-eval)
    (define-key map (kbd "M-p") 'code-cells-backward-cell)
    (define-key map (kbd "M-n") 'code-cells-forward-cell)
    map)
  "Keymap for emjupy-mode.")

;;;###autoload
(define-minor-mode emjupy-mode
  "Emjupy: Integrated Jupyter-Emacs workflow with .ipynb sync."
  :lighter " emjupy"
  :keymap emjupy-mode-map
  (if emjupy-mode
      (progn
        (unless (derived-mode-p 'poly-emjupy-mode) (poly-emjupy-mode))
        (code-cells-mode 1)
        ;; Associate with the existing client if one is active
        (when-let ((client (jupyter-it)))
          (jupyter-repl-associate-buffer client))
        (add-hook 'after-save-hook #'emjupy-sync-to-ipynb nil t))
    (code-cells-mode -1)))

(provide 'emjupy-ui)
