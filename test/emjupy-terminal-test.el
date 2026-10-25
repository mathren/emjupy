;;; emjupy-terminal-test.el --- Unit tests: emjupy-terminal.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-terminal.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)

;;;; Progress bars and escape codes

(ert-deftest emjupy-test-saved-progress-bar-renders-its-last-state ()
  "A notebook saved by Jupyter shows the bar as it ended, not every update.

Jupyter stores the stream exactly as written, so the bar comes back as
all of its updates joined by carriage returns, and none of that passes
through the path that handles output as it arrives."
  (let ((cell (emjupy-test--cell-like
               'code "x"
               (vector (emjupy-test--stream-on
                        "stderr"
                        "\r0it [00:00]\r1it [00:01]\r2it [00:02]\n")))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let* ((ov (emjupy-cell-output-ov cell))
               (text (buffer-substring-no-properties (overlay-start ov)
                                                     (overlay-end ov))))
          (should (string-match-p "2it \\[00:02\\]" text))
          (should-not (string-match-p "0it" text))
          (should-not (string-match-p "\r" text)))))))

(ert-deftest emjupy-test-terminal-codes-keep-colours-and-plain-text-as-it-was ()
  "Colour codes survive the cursor codes around them; text without any is unchanged."
  ;; x written over the red r is red too, as on a terminal; the line below stays
  (should (equal (car (emjupy--terminal-apply "" "\e[31mred\e[0m\n\e[1Ax" nil))
                 "\e[31mxed\e[0m\n"))
  ;; no cursor codes: the carriage-return rule, as before
  (let ((out (make-hash-table :test 'equal)))
    (emjupy--stream-write out "" "a\rb\nc")
    (should (equal (gethash "text" out) "b\nc"))))

(provide 'emjupy-terminal-test)

;;; emjupy-terminal-test.el ends here
