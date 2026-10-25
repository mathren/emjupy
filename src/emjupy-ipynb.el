;;; emjupy-ipynb.el --- Notebooks as nbformat JSON, and as Python  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; A notebook as nbformat v4 JSON, read and written, and as a Python file
;; in the percent format Jupytext, VS Code and Spyder read.  No buffer, no
;; server: the commands that save and export are in emjupy-notebook.el.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'emjupy-core)


;;;; Options

(defcustom emjupy-export-markdown-cells t
  "When non-nil, carry markdown cells into an exported .py as comments.

They are written as a `# %% [markdown]' block with each line commented,
which is the same percent format Jupytext, VS Code and Spyder read -- so
the prose survives the round trip instead of being dropped."
  :type 'boolean
  :group 'emjupy-editing
  :safe #'booleanp)

;;;; Reading and writing .ipynb

(defun emjupy--parse-ipynb (json-string)
  "Parse strict nbformat v4 JSON-STRING into an `emjupy-notebook' struct."
  (let* ((data (json-parse-string json-string :object-type 'hash-table
                                  :array-type 'array))
         (cells-data (gethash "cells" data))
         (metadata (gethash "metadata" data))
         (nb (make-emjupy-notebook
              :metadata metadata :cells (make-vector (length cells-data) nil))))
    (cl-loop for i from 0 below (length cells-data)
             for c-data = (aref cells-data i)
             do (aset (emjupy-notebook-cells nb) i
                      (make-emjupy-cell
                       :id (emjupy--new-cell-id)
                       ;; The notebook's OWN nbformat >=4.5 cell id, kept
                       ;; distinct from our internal buffer-local `id' so a
                       ;; round trip doesn't renumber cells for collaborators.
                       :nb-id (let ((v (gethash "id" c-data)))
                                (and (stringp v) v))
                       :type (intern (gethash "cell_type" c-data))
                       :exec-count (gethash "execution_count" c-data)
                       :source (let ((src (gethash "source" c-data)))
                                 (if (vectorp src) (mapconcat #'identity src
                                                              "") src))
                       :outputs (gethash "outputs" c-data)
                       :metadata (gethash "metadata" c-data)
                       :attachments (gethash "attachments" c-data))))
    nb))

(defun emjupy--source-lines (source)
  "Split SOURCE into an nbformat `source' array.
Every line keeps its newline EXCEPT the last, matching the nbformat
convention -- appending one unconditionally makes each save/reload
cycle grow the cell by a trailing blank line."
  (let* ((lines (split-string (or source "") "\n"))
         (n (length lines)))
    (vconcat
     (cl-loop for line in lines
              for i from 1
              collect (if (= i n) line (concat line "\n"))))))

(defun emjupy--nb-cell-id (cell)
  "Return a stable nbformat >=4.5 id for CELL, creating one if needed.
Cells created inside emjupy have no notebook id yet; nbformat_minor 5
requires one on every cell, so a notebook missing them fails
`nbformat.validate' and is rejected by nbconvert and friends."
  (or (emjupy-cell-nb-id cell)
      (setf (emjupy-cell-nb-id cell)
            (substring (md5 (format "%s-%s" (emjupy-cell-id cell)
                                    (random 1000000))) 0 8))))

(defun emjupy--normalize-output (out)
  "Return OUT with the fields the nbformat v4 schema requires.
Kernels don't always send `metadata', and an execute_result without
`execution_count' is invalid -- both make a saved notebook fail
validation even though it looks fine in emjupy."
  (if (not (hash-table-p out))
      out
    (let ((o (copy-hash-table out))
          (type (gethash "output_type" out)))
      (when (member type '("execute_result" "display_data"))
        (unless (gethash "data" o) (puthash "data"
                                            (make-hash-table :test 'equal) o))
        (unless (gethash "metadata" o)
          (puthash "metadata" (make-hash-table :test 'equal) o)))
      (when (string= type "execute_result")
        (unless (gethash "execution_count" o) (puthash "execution_count"
                                                       :null o)))
      (when (string= type "stream")
        (unless (gethash "name" o) (puthash "name" "stdout" o))
        (unless (gethash "text" o) (puthash "text" "" o)))
      o)))

(defun emjupy--new-notebook-json (cells)
  "Return a new notebook holding CELLS, a vector, as a JSON table."
  (let ((notebook (make-hash-table :test 'equal)))
    (puthash "cells" cells notebook)
    (puthash "metadata" (make-hash-table :test 'equal) notebook)
    (puthash "nbformat" 4 notebook)
    (puthash "nbformat_minor" 5 notebook)
    notebook))

(defun emjupy--notebook-json (nb)
  "Return NB as the JSON table a save request carries."
  (json-parse-string (emjupy--serialize-notebook nb)
                     :object-type 'hash-table :array-type 'array))

(defun emjupy--notebook-request (content)
  "Return a Contents API request saving CONTENT, a notebook as a JSON table."
  (let ((request (make-hash-table :test 'equal)))
    (puthash "type" "notebook" request)
    (puthash "format" "json" request)
    (puthash "content" content request)
    request))

(defun emjupy--serialize-notebook (nb)
  "Serialize NB `emjupy-notebook' struct back to strict nbformat v4 JSON."
  (let ((data (make-hash-table :test 'equal)))
    (puthash "nbformat" 4 data)
    (puthash "nbformat_minor" 5 data)
    (puthash "metadata" (or (emjupy-notebook-metadata nb)
                            (make-hash-table)) data)

    (let ((cells-vec (make-vector (length (emjupy-notebook-cells nb)) nil)))
      (cl-loop for i from 0 below (length (emjupy-notebook-cells nb))
               for cell = (aref (emjupy-notebook-cells nb) i)
               for c-hash = (make-hash-table :test 'equal)
               do
               (puthash "cell_type"
                        (symbol-name (emjupy-cell-type cell)) c-hash)
               (puthash "id" (emjupy--nb-cell-id cell) c-hash)
               (puthash "metadata" (or (emjupy-cell-metadata cell)
                                       (make-hash-table)) c-hash)
               ;; nbformat allows attachments on markdown and raw cells only
               (when (and (emjupy-cell-attachments cell)
                          (memq (emjupy-cell-type cell) '(markdown raw)))
                 (puthash "attachments" (emjupy-cell-attachments cell) c-hash))
               (puthash "source"
                        (emjupy--source-lines (emjupy-cell-source cell)) c-hash)
               (when (eq (emjupy-cell-type cell) 'code)
                 (puthash "outputs"
                          (vconcat (mapcar #'emjupy--normalize-output
                                           (append
                                            (or (emjupy-cell-outputs cell)
                                                []) nil)))
                          c-hash)
                 ;; Safely write :null back to the JSON payload for unexecuted
                 ;; cells
                 (puthash "execution_count"
                          (if (numberp (emjupy-cell-exec-count cell))
                              (emjupy-cell-exec-count cell)
                            :null)
                          c-hash))
               (aset cells-vec i c-hash))
      (puthash "cells" cells-vec data))
    (json-serialize data)))

;;;; The percent format of a .py export

(defun emjupy--percent-export (nb)
  "Return NB's cells as a plain Python file in percent format.
Plain `# %%' markers, as Jupytext, VS Code and Spyder read, and the
markdown cells as comments -- not the shadow buffer, which has emjupy's
own ids and code cells only."
  (let ((chunks nil))
    (cl-loop for cell across (or (emjupy-notebook-cells nb) [])
             do (let ((source (or (emjupy-cell-source cell) "")))
                  (pcase (emjupy-cell-type cell)
                    ('code (push (concat "# %%\n" source) chunks))
                    ('markdown
                     (when emjupy-export-markdown-cells
                       (push (concat "# %% [markdown]\n"
                                     (mapconcat (lambda (line)
                                                  (if (string-empty-p line)
                                                      "#"
                                                    (concat "# " line)))
                                                (split-string source "\n")
                                                "\n"))
                             chunks)))
                    (_ nil))))
    (let ((body (mapconcat #'identity (nreverse chunks) "\n\n")))
      (if (string-suffix-p "\n" body) body (concat body "\n")))))

(provide 'emjupy-ipynb)

;;; emjupy-ipynb.el ends here
