;;; emjupy-mode-test.el --- Unit tests: emjupy-mode.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-mode.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-no-user-reserved-keys ()
  "`C-c' followed by a plain letter is reserved for USERS by the Emacs
Lisp manual, and package-lint reports any such binding as an error --
which MELPA requires to be clean.  Leave that space alone."
  (let ((taken nil))
    (dolist (letter (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
      (when (commandp (lookup-key emjupy-mode-map
                                  (kbd (format "C-c %c" letter))))
        (push (format "C-c %c" letter) taken)))
    (should-not taken)))

(ert-deftest emjupy-test-clear-output-bindings ()
  "The keys, and that they stay out of the space reserved for users.

`C-c' followed by a plain letter belongs to the user, and package-lint
-- which MELPA requires to be clean -- reports any such binding as an
error.  `C-c C-l' and `C-c C-u C-l' are all control characters."
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-l"))
              'emjupy-clear-cell-output))
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-u C-l"))
              'emjupy-clear-all-outputs))
  (dolist (letter (append (number-sequence ?a ?z) (number-sequence ?A ?Z)))
    (should-not (commandp (lookup-key emjupy-mode-map
                                      (kbd (format "C-c %c" letter)))))))

(ert-deftest emjupy-test-export-py-is-bound ()
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-x C-e"))
              'emjupy-export-py)))

(ert-deftest emjupy-test-slow-hook-reporting ()
  "`emjupy-report-slow-hooks' names the slow hook, or says nothing.

A hang is hard to attribute from the outside; this says which of
emjupy's per-command hooks is slow, or -- just as usefully -- that none
of them is."
  (let ((said nil))
    (cl-letf (((symbol-function 'message)
               (lambda (f &rest args)
                 (when f (push (apply #'format f args) said)) nil)))
      ;; off: nothing said, and the value still comes back
      (let ((emjupy-report-slow-hooks nil))
        (should (eq (emjupy--timed "quick" (lambda () 'value)) 'value))
        (should-not said))
      ;; on, but fast enough: still nothing
      (let ((emjupy-report-slow-hooks 5))
        (emjupy--timed "quick" (lambda () 'value))
        (should-not said))
      ;; on and slow: named
      (let ((emjupy-report-slow-hooks 0.01))
        (emjupy--timed "sluggish hook" (lambda () (sleep-for 0.05)))
        (should (cl-some (lambda (m)
                           (string-match-p "sluggish hook" m)) said))))))

(ert-deftest emjupy-test-killed-text-leaves-the-cell-behind ()
  "Text copied out of a cell must not carry the cell with it.

The `emjupy-cell' property holds the cell STRUCT, so killed text
referenced a whole cell -- outputs, overlay, hash tables -- as a snapshot
that stopped being true the moment the cell changed.  Yanked elsewhere,
that text claimed to belong to a cell it had nothing to do with, which
`emjupy--cell-at-point' reads as a fallback wherever no overlay covers
the position."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((kill-ring nil) (kill-ring-yank-pointer nil))
          ;; copy out of the second cell
          (let ((beg (overlay-start (emjupy-cell-overlay c2))))
            (copy-region-as-kill beg (+ beg 5)))
          (let ((killed (car kill-ring)))
            (should (equal (substring-no-properties killed) "b = 2"))
            ;; no internal property anywhere in it
            (dolist (prop emjupy--internal-text-properties)
              (should-not (text-property-not-all 0 (length killed) prop
                                                 nil killed)))
            ;; faces are kept -- yanked code should still look like code
            (should (text-properties-at 0 killed)))
          ;; and yanking it into another cell does not misattribute it
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (end-of-line)
          (yank)
          (backward-char 2)
          (should-not (get-text-property (point) 'emjupy-cell))
          (should (eq (emjupy--cell-at-point) c1)))))))

;;;; Performance

(ert-deftest emjupy-test-a-mixed-installation-is-warned-of-once ()
  "Files of emjupy from two places are warned of, once, when a notebook opens."
  (let ((emjupy--strays-checked nil) (warned 0))
    (cl-letf (((symbol-function 'emjupy--stray-modules)
               (lambda () '((emjupy-cells . "/elsewhere/emjupy-cells.el"))))
              ((symbol-function 'display-warning)
               (lambda (_type message &rest _)
                 (should (string-match-p "/elsewhere/emjupy-cells.el" message))
                 (cl-incf warned))))
      (emjupy--warn-about-strays)
      (emjupy--warn-about-strays)
      (should (= warned 1))))
  ;; nothing to say: nothing said
  (let ((emjupy--strays-checked nil))
    (cl-letf (((symbol-function 'emjupy--stray-modules) #'ignore)
              ((symbol-function 'display-warning) (lambda (&rest _)
                                                    (error "Warned"))))
      (emjupy--warn-about-strays))))

(ert-deftest emjupy-test-brackets-pair-in-cells ()
  "Brackets pair as you type, as they would in a Python buffer.

emjupy-mode derives from `fundamental-mode', which brings none of the
editing conveniences, and nothing about a notebook suggests they have to
be asked for separately."
  (let ((cell (emjupy-test--cell-like 'code "x = 1")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should electric-pair-mode)
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (end-of-line)
        (let ((last-command-event ?\())
          (call-interactively (key-binding "(")))
        (should (equal (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       "x = 1()")))))
  ;; and it can be declined
  (let ((emjupy-electric-pairs nil))
    (with-temp-buffer
      (emjupy-mode)
      (should-not electric-pair-mode))))

(ert-deftest emjupy-test-menu-covers-what-is-bound ()
  "Everything on a key is in the menu.

The menu is the discovery aid: forty-odd bindings are more than anyone
finds by reading `C-h m', which lists them in definition order rather
than by what they are for.  A command reachable by a key and absent from
the menu is one nobody will find, so this fails rather than letting the
two drift apart."
  (let ((in-menu (emjupy-test--menu-commands))
        (bound nil))
    ;; Recursively: most bindings are under the C-c prefix, and `map-keymap'
    ;; does not descend into a prefix.
    (letrec ((collect (lambda (km)
                        (map-keymap
                         (lambda (_event def)
                           (cond
                            ((keymapp def) (funcall collect def))
                            ((and (symbolp def) (commandp def))
                             (cl-pushnew def bound))))
                         km))))
      (funcall collect emjupy-mode-map))
    (should in-menu)
    (let ((missing (seq-remove (lambda (c) (or (memq c in-menu)
                                               (memq c
                                                     emjupy-test--menu-exempt)))
                               bound)))
      (should (equal missing nil)))
    ;; and nothing in the menu that does not exist
    (dolist (cmd in-menu)
      (should (fboundp cmd)))))

(ert-deftest emjupy-test-menu-is-reachable ()
  "The menu is on a key, and not on a reserved one.

`C-c C-h' would have been the guessable choice, but `C-h' after a prefix
is reserved for the list of that prefix's bindings -- the fallback this
menu improves on, and one worth keeping."
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-v")) 'emjupy-menu))
  (should-not (eq (lookup-key emjupy-mode-map (kbd "C-c C-h")) 'emjupy-menu))
  (should (commandp 'emjupy-menu)))

;;;; The undo guard

(ert-deftest emjupy-test-a-mixed-installation-is-noticed ()
  "Files loaded from two installations are named; one installation, none.
A package and a checkout both on the `load-path' gave a mix, and a mix
showed keys as \"M-x ...\" and widgets as text."
  (should-not (emjupy--stray-modules))
  (let ((real (symbol-function 'feature-file)))
    (cl-letf (((symbol-function 'feature-file)
               (lambda (f) (if (eq f 'emjupy-mode) "/elsewhere/emjupy-mode.elc"
                             (funcall real f)))))
      (should (equal (emjupy--stray-modules)
                     '((emjupy-mode . "/elsewhere/emjupy-mode.elc")))))))

(provide 'emjupy-mode-test)

;;; emjupy-mode-test.el ends here
