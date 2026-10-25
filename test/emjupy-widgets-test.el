;;; emjupy-widgets-test.el --- Unit tests: emjupy-widgets.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-widgets.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; The undo guard

(ert-deftest emjupy-test-widget-rules-by-shape ()
  "Each kind of widget state is drawn by the rule its shape calls for.
The order of `emjupy-widget-rules' matters, and this pins it down."
  (cl-flet ((state (&rest kv)
              (let ((h (make-hash-table :test 'equal)))
                (while kv (puthash (pop kv) (pop kv) h))
                h)))
    (dolist (case
             `((step   ,(state "_model_name" "IntSliderModel" "value" 3 "min"
                               0 "max" 9 "step" 1))
               (range  ,(state "_model_name" "IntRangeSliderModel" "value"
                               [2 8] "min" 0 "max" 9 "step" 1 "orientation"
                               "horizontal"))
               (bar    ,(state "_model_name" "IntProgressModel" "value" 3
                               "min" 0 "max" 9))
               (number ,(state "_model_name" "IntTextModel" "value" 3))
               (choose ,(state "_model_name" "DropdownModel" "_options_labels"
                               ["a"] "index" 0))
               (choose-several ,(state "_model_name" "SelectMultipleModel"
                                       "_options_labels" ["a"] "index" []))
               (toggle ,(state "_model_name" "CheckboxModel" "value" :false))
               (toggle ,(state "_model_name" "ValidModel" "value" t "readout"
                               "x"))
               (button ,(state "_model_name" "ButtonModel" "button_style" ""))
               (typed  ,(state "_model_name" "TextModel" "value" "x"
                               "continuous_update" t))
               (color  ,(state "_model_name" "ColorPickerModel" "value"
                               "black" "concise" :false))
               (text   ,(state "_model_name" "LabelModel" "value" "x"))
               (date   ,(state "_model_name" "DatePickerModel" "value" :null))
               (list   ,(state "_model_name" "TagsInputModel" "value" ["p"]
                               "allow_duplicates" t))
               (container ,(state "_model_name" "VBoxModel" "children" []))
               (nil    ,(state "_model_name" "FileUploadModel" "value" []
                               "accept" ""))))
      (ert-info ((format "%s" (gethash "_model_name" (cadr case))))
        (should (eq (car (emjupy--widget-rule (cadr case))) (car case)))))))

(ert-deftest emjupy-test-buffers-go-where-their-paths-say ()
  "Each binary buffer is put into the widget state at its path."
  (let* ((state (json-parse-string "{\"value\": null, \"files\": [{\"content\": null}]}"
                                   :object-type 'hash-table))
         (data (json-parse-string "{\"buffer_paths\": [[\"value\"], [\"files\", 0, \"content\"]]}"
                                  :object-type 'hash-table))
         (a (emjupy-bytes-make "aa")) (b (emjupy-bytes-make "bb")))
    (emjupy--widget-put-buffers state data (list a b))
    (should (eq (gethash "value" state) a))
    (should (eq (gethash "content" (aref (gethash "files" state) 0)) b))))

(ert-deftest emjupy-test-bytes-take-the-image-and-media-rules ()
  "A value of bytes with a format is an image; with autoplay, a sound or video."
  (cl-flet ((state (&rest kv)
              (let ((h (make-hash-table :test 'equal)))
                (while kv (puthash (pop kv) (pop kv) h))
                h)))
    (should (eq (car (emjupy--widget-rule
                      (state "value" (emjupy-bytes-make "x") "format" "png"
                             "width" "")))
                'image))
    (should (eq (car (emjupy--widget-rule
                      (state "value" (emjupy-bytes-make "x") "format" "wav"
                             "autoplay" t)))
                'media))
    (should (eq (car (emjupy--widget-rule
                      (state "value" [] "accept" "" "multiple" :false)))
                'upload))))

(provide 'emjupy-widgets-test)

;;; emjupy-widgets-test.el ends here
