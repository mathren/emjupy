;;; emjupy-io.el --- JSON persistence and reconciliation -*- lexical-binding: t; -*-
(require 'json)
(require 'code-cells)

(defun emjupy--get-cell-content (start end)
  "Extract text and split it into a list of lines with newlines, as Jupyter expects."
  (let ((text (buffer-substring-no-properties start end)))
    (with-temp-buffer
      (insert text)
      (goto-char (point-min))
      (let (lines)
        (while (not (eobp))
          (push (concat (buffer-substring (line-beginning-position) (line-end-position)) "\n") lines)
          (forward-line 1))
        (vconcat (nreverse lines))))))

;;;###autoload
(defun emjupy-sync-to-ipynb ()
  "Export current buffer to a properly formatted .ipynb file."
  (interactive)
  (let ((py-file (buffer-file-name)))
    (if (not py-file)
        (message "Emjupy: No file associated with this buffer.")
      (let* ((ipynb-file (concat (file-name-sans-extension py-file) ".ipynb"))
             (cells []))
        (save-excursion
          (goto-char (point-min))
          ;; Compatible with 2024 code-cells: manual boundary collection
          (let (nodes)
            (save-excursion
              (goto-char (point-min))
              (while (not (eobp))
                (let ((start (point))
                      (end (save-excursion
                             (if (re-search-forward code-cells-boundary-regexp nil t)
                                 (match-beginning 0)
                               (point-max)))))
                  (push (cons start end) nodes)
                  (goto-char end)
                  (unless (eobp) (forward-line 1)))))
            (setq nodes (nreverse nodes))

            (dolist (node nodes)
              (let* ((start (car node))
                     (end (cdr node))
                     (header (buffer-substring-no-properties start (save-excursion (goto-char start) (line-end-position))))
                     (type (cond ((string-match-p "\\[markdown\\]" header) "markdown")
                                 ((string-match-p "\\[org\\]" header) "markdown")
                                 (t "code")))
                     ;; Strip the header line from the source
                     (content-start (save-excursion (goto-char start) (if (looking-at code-cells-boundary-regexp) (forward-line 1)) (point)))
                     (source (emjupy--get-cell-content content-start end))
                     (cell (list :cell_type type
                                 :metadata (make-hash-table)
                                 :source source)))
                (when (string= type "code")
                  (setq cell (append cell (list :execution_count nil :outputs []))))
                (setq cells (vconcat cells (list cell)))))))

        (let* ((nb-data (list :cells cells
                             :metadata (list :kernelspec (list :display_name "Python 3" :language "python" :name "python3")
                                            :language_info (list :name "python" :version "3.x"))
                             :nbformat 4 :nbformat_minor 5))
               (json-encoding-pretty-print t))
          (let ((json-encoding-pretty-print t)
		(json-encoding-default-indentation "  "))
	    (with-temp-file ipynb-file
	      (set-buffer-file-coding-system 'utf-8)
	      (insert (json-encode nb-data))))
          (message "Emjupy: Successfully exported %s" (file-name-nondirectory ipynb-file)))))))

(provide 'emjupy-io)
