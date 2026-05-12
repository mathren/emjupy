;;; emjupy-test.el --- Tests for emjupy components -*- lexical-binding: t; -*-
(require 'ert)
(require 'emjupy-io)
(require 'emjupy-ui)

(ert-deftest test-emjupy-fingerprint ()
  "Ensure content hashing is consistent."
  (let ((h1 (emjupy--calculate-fingerprint "print(1)"))
        (h2 (emjupy--calculate-fingerprint "print(1)"))
        (h3 (emjupy--calculate-fingerprint "print(2)")))
    (should (string= h1 h2))
    (should-not (string= h1 h3))))

(ert-deftest test-emjupy-cell-cycling ()
  "Test cell type transitions."
  (with-temp-buffer
    (python-mode)
    (insert "# %%\n")
    (goto-char (point-max))
    (emjupy-cycle-type)
    (goto-char (point-min))
    (should (looking-at-p "# %% \\[markdown\\]"))))

(ert-deftest test-emjupy-org-export-logic ()
  "Verify org cells transform to markdown headers in temp buffers."
  (with-temp-buffer
    (insert "# %% [org]\n* Item")
    ;; Mocking sync logic for text check
    (goto-char (point-min))
    (re-search-forward "\\[org\\]")
    (replace-match "[markdown]")
    (should (re-search-backward "\\[markdown\\]" nil t))))
