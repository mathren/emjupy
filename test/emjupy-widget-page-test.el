;;; emjupy-widget-page-test.el --- Unit tests: emjupy-widget-page.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-widget-page.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; The undo guard

(ert-deftest emjupy-test-widget-tree-follows-references ()
  "A page gets a widget and every model it refers to, however deep, once."
  (let ((emjupy--widget-models (emjupy-test--models
                                '(("a" ("children" . ["IPY_MODEL_b"]))
        ("b" ("layout" . "IPY_MODEL_c") ("back" . "IPY_MODEL_a"))
        ("c" ("value" . 1))
        ("lone" ("value" . 2))))))
    (should (equal (sort (emjupy--widget-tree "a") #'string<) '("a" "b" "c")))))

(ert-deftest emjupy-test-widget-model-json-takes-the-bytes-out ()
  "A model goes to a page as JSON, its bytes as base64 with their paths."
  (let ((emjupy--widget-models (emjupy-test--models
                                `(("i" ("_model_name" . "ImageModel")
                                   ("_model_module" .
                                    "@jupyter-widgets/controls")
                                   ("_model_module_version" . "2.0.0")
                                   ("value" . ,(emjupy-bytes-make "AB")))))))
    (let ((m (emjupy--widget-model-json "i")))
      (should (equal (gethash "model_name" m) "ImageModel"))
      (should (eq (gethash "value" (gethash "state" m)) :null))
      (should (equal (gethash "buffer_paths" m) [["value"]]))
      (should (equal (gethash "buffers" m) ["QUI="]))
      ;; and it serializes
      (should (json-serialize m)))))

(ert-deftest emjupy-test-scripted-widgets-go-to-a-page ()
  "A widget whose view is not ipywidgets' own opens in a page."
  (emjupy-widget-page-enable)
  (cl-flet ((state (&rest kv)
              (let ((h (make-hash-table :test 'equal)))
                (while kv (puthash (pop kv) (pop kv) h))
                h)))
    (should (eq (car (emjupy--widget-rule
                      (state "_view_module" "anywidget" "_model_name" "AnyModel"
                             "_anywidget_id"
                             "plotly.graph_objs._figurewidget.FigureWidget")))
                'scripted))
    (should (equal (emjupy--widget-scripted-name
                    (state "_anywidget_id"
                           "plotly.graph_objs._figurewidget.FigureWidget"))
                   "FigureWidget"))
    (should-not (eq (car (emjupy--widget-rule
                          (state "_view_module" "@jupyter-widgets/controls"
                                 "value" 3 "min" 0 "max" 9 "step" 1)))
                    'scripted))))

(ert-deftest emjupy-test-bridge-passes-on-only-what-concerns-a-page ()
  "Kernel messages go to the pages showing their models; new models too."
  (let ((emjupy--bridge-pages (make-hash-table :test 'equal))
        (sent nil))
    (puthash "p1" (list :kernel 'k1 :ws 'ws1 :models (list "m1"))
             emjupy--bridge-pages)
    (puthash "p2" (list :kernel 'k2 :ws 'ws2 :models (list "m2"))
             emjupy--bridge-pages)
    (cl-letf (((symbol-function 'emjupy--bridge-send)
               (lambda (ws obj) (push (list ws (gethash "type" obj)
                                            (gethash "comm_id" obj)) sent))))
      (let ((content (make-hash-table :test 'equal)))
        (puthash "comm_id" "m1" content)
        (emjupy--bridge-on-comm 'k1 "comm_msg" content
                                (list (emjupy-bytes-make "x")))
        (puthash "comm_id" "other" content)
        ;; not shown there
        (emjupy--bridge-on-comm 'k1 "comm_msg" content nil)
        (puthash "comm_id" "new" content)
        (emjupy--bridge-on-comm 'k1 "comm_open" content nil)))  ; may come to be
    (should (equal (reverse sent) '((ws1 "comm_msg" "m1") (ws1 "comm_open"
                                                               "new"))))
    (should (member "new" (plist-get (gethash "p1"
                                              emjupy--bridge-pages) :models)))))

(ert-deftest emjupy-test-widget-page-has-what-it-needs ()
  "The page names the bridge's port, its token, its scripts and modules."
  (let* ((emjupy-widget-scripts '("/opt/require.js"
                                  "https://cdn.example/embed-amd.js"))
         (page (emjupy--widget-page "tok-1" 4567 "Counter"
                                    '(("anywidget" . "/tmp/m/anywidget.js")))))
    (should (string-match-p "ws://127.0.0.1:4567/" page))
    (should (string-match-p "\"tok-1\"" page))
    (should (string-match-p "<script src=\"file:///opt/require.js\">" page))
    (should (string-match-p "<script src=\"https://cdn.example/embed-amd.js\">"
                            page))
    (should (string-match-p "\"anywidget\":\"file:///tmp/m/anywidget.js\""
                            page))
    (should (string-match-p "<title>Counter</title>" page))))

(provide 'emjupy-widget-page-test)

;;; emjupy-widget-page-test.el ends here
