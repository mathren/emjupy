;;; emjupy-test.el --- Tests for emjupy -*- lexical-binding: t; -*-
(require 'ert)
(require 'emjupy-ui)
(require 'emjupy-io)
(require 'emjupy-core)
(require 'json)

(defun emjupy-test--setup-py-buffer (name)
  (let ((buf (get-buffer-create name)))
    (with-current-buffer buf
      (python-mode)
      (erase-buffer)
      (insert "# %% [markdown]\n# Test\n# %%\nprint(1)\n")
      (set-visited-file-name (expand-file-name name))
      (emjupy-mode 1))
    buf))

(ert-deftest emjupy-test-json-structure ()
  "Test that the notebook export produces valid Jupyter v4 schema."
  (let* ((py-name "test_structure.py")
         (ipynb-name "test_structure.ipynb")
         (buf (emjupy-test--setup-py-buffer py-name)))
    (unwind-protect
        (with-current-buffer buf
          (emjupy-sync-to-ipynb)
          (should (file-exists-p ipynb-name))
          (let* ((json-object (with-temp-buffer
                                (insert-file-contents ipynb-name)
                                (goto-char (point-min))
                                (let ((json-object-type 'plist))
                                  (json-read)))))
            (should (equal (plist-get json-object :nbformat) 4))
            (should (sequencep (plist-get json-object :cells)))))
      (when (get-buffer buf) (kill-buffer buf))
      (dolist (f (list py-name ipynb-name))
        (when (file-exists-p f) (delete-file f))))))

(ert-deftest emjupy-test-cell-cycling ()
  "Test that emjupy-cycle-type correctly modifies the cell header."
  (with-temp-buffer
    (python-mode)
    (insert "# %%\nprint(1)")
    (goto-char (point-min))

    ;; Cycle 1: Code -> Markdown
    (emjupy-cycle-type)
    (goto-char (point-min))
    (should (string-match-p "markdown" (buffer-substring-no-properties (point) (line-end-position))))

    ;; Cycle 2: Markdown -> Org
    (emjupy-cycle-type)
    (goto-char (point-min))
    (should (string-match-p "org" (buffer-substring-no-properties (point) (line-end-position))))

    ;; Cycle 3: Org -> Code
    (emjupy-cycle-type)
    (goto-char (point-min))
    (should (equal "# %%" (buffer-substring-no-properties (point) (+ (point) 4))))))

(ert-deftest emjupy-test-live-connection ()
  "Integration test for a running server at localhost:8888."
  (condition-case err
      (let* ((url (read-string "If token required, paste full URL (else hit enter): " "http://localhost:8888"))
             (server (jupyter-server :url url))
             (req (jupyter-api-http-request server "GET" "api/kernels")))
        (should (sequencep req))
        (message "Live kernel test passed."))
    (error (ert-fail (format "Connection failed: %s" (error-message-string err))))))
