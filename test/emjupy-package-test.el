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

;;;; Performance

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

;;;; The docs name what exists

(ert-deftest emjupy-test-every-name-in-the-docs-exists ()
  "Every emjupy name the docs write as code exists.
A function, variable, face, customization group, struct, feature, error
or test: so a rename or a removal that leaves the docs behind -- the
reading guide names some sixty functions -- fails here, not in a
reader's hands.

The docs name tests too, which exist only once their files are loaded:
so they are loaded here -- defining tests, running none -- and this
passes run alone as in the whole suite."
  (skip-unless (file-directory-p emjupy-test--docs-directory))
  (dolist (tests '(emjupy-test emjupy-integration-test emjupy-robustness-test))
    (require tests))
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


(ert-deftest emjupy-test-the-docs-say-where-things-are ()
  "Where the docs say a name is defined -- =NAME= (FILE) -- it is.
The reading guide places functions in their files, and moving one
leaves the guide behind without failing anything else."
  (skip-unless (file-directory-p emjupy-test--docs-directory))
  (let ((src (expand-file-name "../src/" emjupy-test--docs-directory))
        (wrong nil))
    (dolist (f (directory-files emjupy-test--docs-directory t "\\.org\\'"))
      (with-temp-buffer
        (insert-file-contents f)
        (goto-char (point-min))
        ;; a few words may come between: "=NAME= (FILE)", "=NAME=, say (FILE)"
        (while (re-search-forward
                "=\\(emjupy-[a-z0-9-]*[a-z0-9]\\)=[^=(]\\{0,25\\}(\\([a-z-]+\\))"
                nil t)
          (let* ((name (intern (match-string 1)))
                 (claimed (concat "emjupy-" (match-string 2)))
                 (defined (or (symbol-file name 'defun)
                              (symbol-file name 'defvar))))
            (when (and defined
                       (file-exists-p (expand-file-name (concat claimed ".el")
                                                        src))
                       (not (equal (file-name-base defined) claimed)))
              (push (format "%s: %s is in %s, not %s"
                            (file-name-nondirectory f) name
                            (file-name-base defined) claimed)
                    wrong))))))
    (should-not wrong)))


(ert-deftest emjupy-test-the-reading-order-follows-the-layers ()
  "Reading the code lists every source file, each after what it requires.
Its table is the order to read the files in, bottom up; a file split or
added leaves it behind without failing anything else."
  (skip-unless (file-directory-p emjupy-test--docs-directory))
  (let* ((src (expand-file-name "../src/" emjupy-test--docs-directory))
         (guide (expand-file-name "reading-the-code.org"
                                  emjupy-test--docs-directory))
         (order
          (with-temp-buffer
            (insert-file-contents guide)
            (let (files)
              (while (re-search-forward
                      "^| +[0-9]+ | =\\(emjupy[a-z-]*\\.el\\)=" nil t)
                (push (match-string 1) files))
              (nreverse files))))
         (sources (seq-remove (lambda (f) (equal f "emjupy-pkg.el"))
                              (directory-files src nil "\\`emjupy.*\\.el\\'")))
         (problems nil))
    (dolist (f (seq-difference sources order))
      (push (format "%s is not in the table" f) problems))
    (let ((before nil))
      (dolist (f order)
        (with-temp-buffer
          (insert-file-contents (expand-file-name f src))
          (while (re-search-forward "^(require '\\(emjupy[a-z-]*\\))" nil t)
            (let ((required (concat (match-string 1) ".el")))
              (when (and (member required order) (not (member required before)))
                (push (format "%s requires %s, listed after it" f required)
                      problems)))))
        (push f before)))
    (should-not (nreverse problems))))


(ert-deftest emjupy-test-loading-changes-nothing ()
  "Loading emjupy defines things and changes nothing else.
Not Eglot's commands, nor the theme hooks: a notebook opening does that.
Checked in a fresh Emacs, as this one has opened notebooks."
  (let* ((emacs (expand-file-name invocation-name invocation-directory))
         (program "(progn
  (require 'emjupy)
  (princ (list (seq-some (lambda (c)
                           (advice-member-p #'emjupy--eglot-command-advice c))
                         emjupy-eglot-delegated-commands)
               (and (boundp 'enable-theme-functions)
                    (memq #'emjupy--on-theme-change
                          enable-theme-functions)))))")
         (args (append '("-batch" "-Q")
                       (mapcan (lambda (d) (list "-L" d))
                               (seq-filter #'file-directory-p load-path))
                       (list "--eval" program)))
         (out (with-temp-buffer
                (apply #'call-process emacs nil t nil args)
                (buffer-string))))
    (should (string-suffix-p "(nil nil)" (string-trim out)))))


(ert-deftest emjupy-test-every-file-binds-lexically ()
  "Every source file says lexical-binding: t on its first line.
Only there does Emacs read it: a long first line rewrapped once lost it,
and the file would have been compiled with dynamic binding."
  (dolist (file (directory-files
                 (file-name-directory (locate-library "emjupy-core.el"))
                 t "\\`emjupy.*\\.el\\'"))
    (with-temp-buffer
      (insert-file-contents file nil 0 300)
      (should (string-match-p "-\\*-.*lexical-binding: t.*-\\*-"
                              (buffer-substring (point-min)
                                                (line-end-position)))))))


;;;; Every link in the docs leads somewhere

(defun emjupy-test--org-links (file)
  "Return the internal links in the Org FILE, as (LINE . TARGET).
Not those in verbatim text or in a source or example block: those show
link syntax, they are not links."
  (let ((links nil) (line 0) (in-block nil))
    (dolist (text (split-string (with-temp-buffer
                                  (insert-file-contents file)
                                  (buffer-string))
                                "\n"))
      (setq line (1+ line))
      (cond ((string-match-p "\\`[ \t]*#\\+begin_\\(src\\|example\\)"
                             (downcase text))
             (setq in-block t))
            ((string-match-p "\\`[ \t]*#\\+end_\\(src\\|example\\)"
                             (downcase text))
             (setq in-block nil))
            ((not in-block)
             (let ((plain (replace-regexp-in-string "[=~][^=~\n]+[=~]" "" text))
                   (start 0))
               (while (string-match "\\[\\[\\([^]]+\\)\\]" plain start)
                 (let ((target (match-string 1 plain)))
                   (unless (string-match-p "\\`[a-z]+://\\|\\`mailto:" target)
                     (push (cons line target) links)))
                 (setq start (match-end 0)))))))
    (nreverse links)))

(defun emjupy-test--org-has-heading-p (file heading)
  "Return non-nil if the Org FILE has a heading titled HEADING."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward
     (concat "^\\*+[ \t]+" (regexp-quote heading)
             "[ \t]*\\(?::[[:alnum:]_@#%:]+:[ \t]*\\)?$")
     nil t)))

(ert-deftest emjupy-test-every-link-in-the-docs-leads-somewhere ()
  "Every internal link in the docs' Org sources resolves: its file exists,
and the heading it names is in it -- an image, in the site the docs are
exported to.  The site's checker, on the exported
HTML, runs only when the site is built; a heading renamed broke links
unseen until then."
  (skip-unless (file-directory-p emjupy-test--docs-directory))
  (let ((broken nil))
    (dolist (file (directory-files emjupy-test--docs-directory t "\\.org\\'"))
      (dolist (link (emjupy-test--org-links file))
        (let* ((target (cdr link))
               (parts (split-string target "::"))
               (path (car parts))
               (inside (cadr parts))
               (into (cond ((string-prefix-p "file:" path)
                            (expand-file-name (substring path 5)
                                              emjupy-test--docs-directory))
                           ((string-prefix-p "*" path) file)
                           (t nil)))
               (heading (cond ((string-prefix-p "*" path) (substring path 1))
                              ((and inside (string-prefix-p "*" inside))
                               (substring inside 1)))))
          ;; An image, a graph: in the site the docs are exported to,
          ;; beside the pages that show it.
          (when (and into (not (file-exists-p into))
                     (not (string-suffix-p ".org" into)))
            (setq into (expand-file-name (substring path 5)
                                         (expand-file-name
                                          "../html-content/"
                                          emjupy-test--docs-directory))))
          (unless (and into (file-exists-p into)
                       (or (null heading)
                           (emjupy-test--org-has-heading-p into heading)))
            (push (format "%s:%d: %s" (file-name-nondirectory file) (car link)
                          target)
                  broken)))))
    (should-not (nreverse broken))))

(provide 'emjupy-package-test)

;;; emjupy-package-test.el ends here
