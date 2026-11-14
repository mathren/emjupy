;;; emjupy-ipynb-test.el --- Unit tests: emjupy-ipynb.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-ipynb.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; .ipynb parsing & serialization

(ert-deftest emjupy-test-ipynb-roundtrip ()
  "Parsing then re-serializing a notebook preserves structure."
  (let* ((raw-json "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"import numpy as np\\n\",\"print(1)\"]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")
         (parsed-nb (emjupy--parse-ipynb raw-json))
         (first-cell (aref (emjupy-notebook-cells parsed-nb) 0))
         (reserialized (emjupy--serialize-notebook parsed-nb)))
    (should (eq (emjupy-cell-type first-cell) 'code))
    (should (string= (emjupy-cell-source first-cell)
                     "import numpy as np\nprint(1)"))
    (should (string-match-p "\"nbformat\":4" reserialized))
    (should (string-match-p "\"import numpy as np\\\\n\"" reserialized))))

;;;; nbformat validity of what we WRITE BACK

(ert-deftest emjupy-test-serialize-emits-cell-ids ()
  "nbformat_minor 5 requires an `id' on EVERY cell."
  (let* ((c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "x=1"
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array))
         (cell (aref (gethash "cells" out) 0)))
    (should (stringp (gethash "id" cell)))
    (should (> (length (gethash "id" cell)) 0))))

(ert-deftest emjupy-test-serialize-preserves-original-cell-id ()
  "A cell that came from a real notebook keeps ITS id on the way back
out, so a round trip doesn't renumber cells for collaborators."
  (let* ((raw "{\"cells\":[{\"cell_type\":\"code\",\"id\":\"abc12345\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"x=1\"]}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}")
         (nb (emjupy--parse-ipynb raw))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array)))
    (should (equal (gethash "id" (aref (gethash "cells" out) 0)) "abc12345"))))

(ert-deftest emjupy-test-serialize-normalizes-required-output-fields ()
  "display_data/execute_result outputs REQUIRE `metadata' (and
execute_result also `execution_count') per the nbformat v4 schema.
Kernels don't always send them."
  (let* ((oh (make-hash-table :test 'equal))
         (dat (make-hash-table :test 'equal))
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "plot()"
                              :metadata (make-hash-table))))
    (puthash "text/plain" "<Figure>" dat)
    (puthash "output_type" "display_data" oh)
    (puthash "data" dat oh)                ; deliberately NO metadata
    (setf (emjupy-cell-outputs c) (vector oh))
    (let* ((nb (make-emjupy-notebook :cells (vector c)))
           (out (json-parse-string (emjupy--serialize-notebook nb)
                                   :object-type 'hash-table :array-type 'array))
           (o (aref (gethash "outputs" (aref (gethash "cells" out) 0)) 0)))
      (should (hash-table-p (gethash "metadata" o))))))

(ert-deftest emjupy-test-source-roundtrip-is-idempotent ()
  "Only interior lines carry a newline in the nbformat `source' array.
Appending one to the last line too makes every save/reload cycle grow
the cell by a trailing blank line."
  (should (equal (append (emjupy--source-lines "a\nb") nil) '("a\n" "b")))
  (should (equal (append (emjupy--source-lines "solo") nil) '("solo")))
  ;; parse(serialize(x)) == x
  (let* ((src "import numpy as np\nprint(1)")
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source src
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (again (emjupy--parse-ipynb (emjupy--serialize-notebook nb))))
    (should (equal (emjupy-cell-source
                    (aref (emjupy-notebook-cells again) 0)) src))))

;;;; Rich output: display_data / image routing

(ert-deftest emjupy-test-dedup-does-not-alter-saved-outputs ()
  "Deduplication is a RENDER-time concern only. The cell's outputs --
and therefore the .ipynb we write back -- must still contain exactly
what the kernel sent, so the file stays faithful for other clients."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda (kind)
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" kind oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell)
          (vector (funcall mk "execute_result") (funcall mk "display_data")))
    (emjupy-test--with-notebook (vector cell) buf nb
      ;; render collapses to one ...
      (should (= (length (emjupy--outputs-to-draw
                          (emjupy-cell-outputs cell)))
                 1))
      ;; ... but the struct, and the serialized notebook, keep both
      (should (= (length (emjupy-cell-outputs cell)) 2))
      (let* ((json (emjupy--serialize-notebook nb))
             (out (json-parse-string json :object-type 'hash-table
                                     :array-type 'array)))
        (should (= (length (gethash "outputs"
                                    (aref (gethash "cells" out) 0))) 2))))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-percent-export-format ()
  "The export is percent format: `# %%' per code cell, markdown carried
across as a commented `# %% [markdown]' block.

Deliberately not the shadow buffer verbatim, though that is where the
idea comes from: its markers carry emjupy's internal cell ids, which
mean nothing outside emjupy, and it holds code cells only -- so
exporting it would silently drop every markdown cell."
  (cl-flet ((mk (type src)
              (make-emjupy-cell :id (emjupy--new-cell-id) :type type :source src
                                :outputs [] :metadata (make-hash-table))))
    (let ((nb (make-emjupy-notebook
               :cells (vector (mk 'markdown "# Title\n\nSome prose.")
                              (mk 'code "import numpy as np")
                              (mk 'code "x = 1"))
               :path "subdir/analysis.ipynb")))
      (let ((out (emjupy--percent-export nb)))
        ;; no internal ids leak out
        (should-not (string-match-p "emjupy:" out))
        (should (string-match-p "^# %% \\[markdown\\]$" out))
        ;; every prose line is commented, blank lines included
        (should (string-match-p "^# # Title$" out))
        (should (string-match-p "^#$" out))
        (should (string-match-p "^# Some prose\\.$" out))
        ;; a code cell is a bare marker followed by its source
        (should (string-match-p (regexp-quote "# %%\nimport numpy as np") out))
        (should (string-match-p (regexp-quote "# %%\nx = 1") out))
        ;; one bare marker per code cell; "# %% [markdown]" is not counted
        (should (= (cl-count "# %%" (split-string out "\n") :test #'equal) 2))
        (should (string-suffix-p "\n" out)))
      ;; markdown can be left out
      (let* ((emjupy-export-markdown-cells nil)
             (out (emjupy--percent-export nb)))
        (should-not (string-match-p "markdown" out))
        (should-not (string-match-p "Title" out)))
      ;; the default destination sits beside the notebook
      (should (equal (emjupy--export-default-name nb) "subdir/analysis.py")))))


(ert-deftest emjupy-test-parsing-takes-null-for-none ()
  "Null where nbformat asks for an array or a source means none.
Some tools write it so; each raised `wrong-type-argument'."
  (let ((nb (emjupy--parse-ipynb
             (concat "{\"cells\":null,\"metadata\":{},"
                     "\"nbformat\":4,\"nbformat_minor\":5}"))))
    (should (= 0 (length (emjupy-notebook-cells nb)))))
  (let* ((nb (emjupy--parse-ipynb
              (concat "{\"cells\":[{\"cell_type\":\"code\",\"metadata\":{},"
                      "\"source\":null,\"outputs\":null,"
                      "\"execution_count\":null},"
                      "{\"cell_type\":\"markdown\",\"metadata\":{}}],"
                      "\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}")))
         (cells (emjupy-notebook-cells nb)))
    (should (equal "" (emjupy-cell-source (aref cells 0))))
    (should-not (emjupy-cell-outputs (aref cells 0)))
    (should (equal "" (emjupy-cell-source (aref cells 1))))))

(ert-deftest emjupy-test-parsing-refuses-what-is-not-a-notebook ()
  "Text that is not an nbformat 4 notebook is refused, saying why.
Each raised a raw error; a format 3 notebook opened as empty, to be saved
over the real one."
  (dolist (case '(("this is not json" . "Not a notebook")
                  ("" . "Not a notebook")
                  ("[1,2]" . "not an object")
                  ("{\"worksheets\":[],\"nbformat\":3}" . "too old")
                  ("{\"cells\":[{\"source\":[\"x\"]}],\"nbformat\":4}"
                   . "cell 1 has no cell_type")
                  ("{\"cells\":[{\"cell_type\":\"code\",\"source\":[1]}],\"nbformat\":4}"
                   . "source of cell 1 is not text")))
    (let ((err (should-error (emjupy--parse-ipynb (car case))
                             :type 'user-error)))
      (should (string-search (cdr case) (error-message-string err))))))

(provide 'emjupy-ipynb-test)

;;; emjupy-ipynb-test.el ends here
