;;; emjupy-io.el --- JSON persistence and reconciliation -*- lexical-binding: t; -*-
(require 'json)
(require 'code-cells)
(require 'ox-md)

(defun emjupy--calculate-fingerprint (text)
  "Generate a hash for cell content."
  (secure-hash 'sha256 (substring-no-properties text)))

(defun emjupy--convert-org-to-md (start end)
  "Convert Org syntax to Markdown for JSON compatibility."
  (let ((org-text (buffer-substring-no-properties start end)))
    (delete-region start end)
    (insert (org-export-string-as org-text 'md t))))

;;;###autoload
(defun emjupy-sync-to-ipynb ()
  "Export current buffer to .ipynb. Org cells are converted to Markdown."
  (interactive)
  (let* ((py-file (buffer-file-name))
         (ipynb-file (concat (file-name-sans-extension py-file) ".ipynb"))
         (temp-buffer (generate-new-buffer " *emjupy-export*")))
    (copy-to-buffer temp-buffer (point-min) (point-max))
    (with-current-buffer temp-buffer
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^# %% \\[org\\]" nil t)
          (replace-match "# %% [markdown]")
          (forward-line 1)
          (let ((s (point))
                (e (save-excursion (if (re-search-forward code-cells-boundary-regexp nil t)
                                       (match-beginning 0) (point-max)))))
            (emjupy--convert-org-to-md s e))))
      (let ((json-data (code-cells--to-ipynb)))
        (with-temp-file ipynb-file
          (let ((json-encoding-pretty-print t))
            (insert (json-encode json-data))))))
    (kill-buffer temp-buffer)))

(provide 'emjupy-io)
