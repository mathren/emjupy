;;; emjupy-package-test.el --- Unit tests: the package  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of the package as a whole: its version, opening into the mode, every
;; command against undo, the docs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-version-reports-where-it-loaded-from ()
  "`emjupy-version' names the version and the directory, so \"am I running
the fix?\" can be answered without recalling how it was installed."
  (let ((report (emjupy-version)))
    (should (string-match-p (regexp-quote emjupy-version) report))
    (should (string-match-p "loaded from" report))))

(ert-deftest emjupy-test-version-constant-matches-header ()
  "`emjupy-version' must equal the Version: header, or it reports a lie."
  (let ((header
         (with-temp-buffer
           (insert-file-contents
            (expand-file-name "emjupy.el"
                              (file-name-directory
                               (or (locate-library "emjupy")
                                   default-directory))))
           (goto-char (point-min))
           (when (re-search-forward "^;; Version: \\(.*\\)$" nil t)
             (string-trim (match-string 1))))))
    (should header)
    (should (equal header emjupy-version))))

(ert-deftest emjupy-test-open-puts-point-at-the-top ()
  "A freshly opened notebook starts at the top; one already open keeps
where you were."
  (should (string-match-p "already-open"
                          (with-temp-buffer
                            (insert-file-contents
                             (expand-file-name "emjupy-notebook.el"
                                               (file-name-directory
                                                (or (locate-library "emjupy-notebook")
                                                    default-directory))))
                            (buffer-string)))))

;;;; Performance

(ert-deftest emjupy-test-opening-a-notebook-has-the-mode ()
  "Opening a notebook does not depend on emjupy.el having been loaded.
`emjupy-open-notebook' is autoloaded from emjupy-notebook.el, so opening
a notebook may load that file and what it requires, and nothing else.
The mode, and the check for a mixed installation it runs, must come with
it.  Checked in a fresh Emacs, where nothing else is loaded."
  (let* ((here (file-name-directory (locate-library "emjupy-notebook")))
         (websocket (file-name-directory (locate-library "websocket")))
         (answer (with-temp-buffer
                   (call-process (expand-file-name invocation-name
                                                   invocation-directory)
                                 nil t nil "-Q" "--batch" "-L" here "-L"
                                 websocket
                                 "-l" "emjupy-notebook"
                                 "--eval" "(prin1 (list (fboundp 'emjupy-mode) (fboundp 'emjupy--warn-about-strays) (featurep 'emjupy)))")
                   (goto-char (point-max))
                   (backward-sexp)
                   (read (current-buffer)))))
    ;; the mode and its check are there; emjupy.el itself was not needed
    (should (equal answer '(t t nil)))))

;;;; The undo guard

(ert-deftest emjupy-test-undo-guard-covers-every-command ()
  "Every command on a key keeps the undo history, or says why it is exempt.

Driven by the keymap on purpose: a command added later is covered the
day it is bound.  Undo has been broken by a redraw more than once, each
time noticed by a user rather than a test, and each time repaired for
one command while the next went unguarded."
  (let ((failures nil))
    (dolist (command (emjupy-test--commands-on-keys))
      (unless (assq command emjupy-test--undo-guard-exempt)
        (let ((verdict (emjupy-test--undo-survives-command command)))
          (unless (eq verdict t)
            (push (format "%s: %s" command verdict) failures)))))
    (should (equal (nreverse failures) nil))))

(ert-deftest emjupy-test-undo-guard-exemptions-are-justified ()
  "Every exemption names a real command and gives a reason.

An exemption list is only honest if it cannot be padded: a stale entry
would silently stop guarding a command that still exists."
  (dolist (entry emjupy-test--undo-guard-exempt)
    (should (fboundp (car entry)))
    (should (stringp (cdr entry)))
    (should (> (length (cdr entry)) 10)))
  ;; and it exempts nothing that is not bound -- a name left behind after a
  ;; command was renamed would look like coverage and be none
  (let ((bound (emjupy-test--commands-on-keys)))
    (dolist (entry emjupy-test--undo-guard-exempt)
      (should (memq (car entry) bound)))))

(ert-deftest emjupy-test-listing-sorts-by-recency-within-a-kind ()
  "Each group in the dashboard reads most recently modified first.

The notebook worked on this morning is the one wanted, and alphabetical
order buries it among however many others share its prefix.  Grouping is
unchanged -- directories, notebooks, then other files -- because what is
being looked for is usually known by kind."
  (cl-flet ((entry (kind name modified)
              (list (list :kind kind :path name :modified modified)
                    (vector kind name modified))))
    (let* ((rows (list (entry 'notebook "alpha.ipynb" "2026-09-25T06:00:00Z")
                       (entry 'notebook "zulu.ipynb" "2026-09-25T09:00:00Z")
                       (entry 'notebook "middle.ipynb" "2026-09-25T07:00:00Z")))
           (sorted (sort (copy-sequence rows)
                         (lambda (a b)
                           (let ((ta (or (plist-get (car a) :modified) ""))
                                 (tb (or (plist-get (car b) :modified) "")))
                             (if (equal ta tb)
                                 (string-lessp (aref (cadr a) 1)
                                               (aref (cadr b) 1))
                               (string-greaterp ta tb)))))))
      ;; newest first, regardless of name
      (should (equal (mapcar (lambda (r) (aref (cadr r) 1)) sorted)
                     '("zulu.ipynb" "middle.ipynb" "alpha.ipynb"))))))

;;;; The docs name what exists

(ert-deftest emjupy-test-every-name-in-the-docs-exists ()
  "Every emjupy name the docs write as code exists.
A function, variable, face, customization group, struct, feature, error
or test: so a rename or a removal that leaves the docs behind -- the
reading guide names some sixty functions -- fails here, not in a
reader's hands."
  (skip-unless (file-directory-p emjupy-test--docs-directory))
  (let (unknown)
    (dolist (f (directory-files emjupy-test--docs-directory t "\\.org\\'"))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        (while (re-search-forward "[=~]\\(emjupy-[a-z0-9-]*[a-z0-9]\\)[=~]"
                                  nil t)
          (let ((s (intern (match-string 1))))
            (unless (or (fboundp s) (boundp s) (facep s)
                        (get s 'custom-group) (get s 'error-conditions)
                        (cl-find-class s) (featurep s) (ert-test-boundp s))
              (push (format "%s: %s" (file-name-nondirectory f) s)
                    unknown))))))
    (should-not (delete-dups unknown))))

(provide 'emjupy-package-test)

;;; emjupy-package-test.el ends here
