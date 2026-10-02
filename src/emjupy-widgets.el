;;; emjupy-widgets.el --- Widget controls in the notebook buffer  -*- lexical-binding: t; -*-

;; Copyright (C) 2025-2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@flatironinstitute.org>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; ipywidgets controls -- sliders, checkboxes, dropdowns, and the boxes
;; that hold them -- drawn as text in the output, and driven from Emacs.
;;
;; A widget is a model in the kernel, announced and updated by comm
;; messages; an output showing one names its model.  The models are kept
;; here as the messages arrive, and a widget's output is drawn from them.
;; Using a control sends the kernel the new value, as a browser would.
;;
;; What that changes comes back as ordinary output -- an `interact' clears
;; its output and draws its figure again, as `image/png' -- answering the
;; message sent here rather than the cell's execution.  So the message is
;; remembered with the cell showing the widget, and the output goes there.
;;
;; Widgets that draw themselves in JavaScript -- plotly's FigureWidget,
;; maps -- have no text to show, and show as their `text/plain'.

;;; Code:

(require 'emjupy-core)
(require 'emjupy-render)
(require 'emjupy-cells)
(require 'emjupy-kernel)

(defvar emjupy--widget-models (make-hash-table :test 'equal)
  "Widget models, by comm id: a cons of the kernel and the state.")

(defvar emjupy--widget-requests (make-hash-table :test 'equal)
  "Cells whose widgets sent a message, by message id.
The output a message causes is drawn in the cell its widget is in.")

(defun emjupy--widget-on-comm (kernel msg-type content)
  "Keep the widget models up to date from KERNEL\\='s comm messages.
MSG-TYPE is the message type and CONTENT its content."
  (let ((id (gethash "comm_id" content))
        (data (gethash "data" content)))
    (pcase msg-type
      ("comm_open"
       (when (and (equal (gethash "target_name" content) "jupyter.widget")
                  (hash-table-p data))
         (puthash id (cons kernel (or (gethash "state" data)
                                      (make-hash-table :test 'equal)))
                  emjupy--widget-models)))
      ("comm_msg"
       (let ((model (gethash id emjupy--widget-models))
             (state (and (hash-table-p data) (gethash "state" data))))
         (when (and model (hash-table-p state))
           (maphash (lambda (k v) (puthash k v (cdr model))) state))))
      ("comm_close" (remhash id emjupy--widget-models)))))

(defun emjupy--widget-owner (msg-id)
  "Return the cell whose widget sent message MSG-ID, or nil."
  (gethash msg-id emjupy--widget-requests))

(defun emjupy--widget-state (id)
  "Return the state of the widget model with comm ID, or nil."
  (cdr (gethash id emjupy--widget-models)))

(defun emjupy--widget-ref (ref)
  "Return the comm id a child REF names, \"IPY_MODEL_<id>\"."
  (string-remove-prefix "IPY_MODEL_" ref))

(defvar emjupy-widget-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'emjupy-widget-activate)
    (define-key map [mouse-1] #'emjupy-widget-activate)
    (define-key map (kbd "<left>") #'emjupy-widget-decrease)
    (define-key map (kbd "<right>") #'emjupy-widget-increase)
    map)
  "Keymap on a widget\\='s controls.")

(defun emjupy--widget-button (label id action)
  "Return LABEL as a control acting on the widget with comm ID.
ACTION says what using it does: -1 or 1 to step a slider, `toggle', or
`choose'."
  (propertize label 'face 'link 'mouse-face 'highlight
              'keymap emjupy-widget-map
              'emjupy-widget id 'emjupy-widget-action action
              'help-echo "RET or click; on a slider, <left> and <right> too"))

(defun emjupy--widget-number (n)
  "Return the number N formatted for a widget."
  (if (floatp n) (format "%g" n) (format "%s" n)))

(defun emjupy--widget-lines (id)
  "Return the text lines for the widget with comm ID, or nil if unknown."
  (let* ((state (emjupy--widget-state id))
         (name (and state (gethash "_model_name" state)))
         (desc (and state (gethash "description" state)))
         (label (if (and desc (not (string-empty-p desc))) (concat desc " ") "")))
    (pcase name
      ('nil nil)
      ((or "IntSliderModel" "FloatSliderModel")
       ;; The whole line is the control: <left> and <right> step it from
       ;; anywhere on it, and RET on the value asks for one.
       (list (propertize
              (concat label
                     (emjupy--widget-button "◀" id -1) " "
                     (propertize (emjupy--widget-number (gethash "value" state))
                                 'face 'bold)
                     " " (emjupy--widget-button "▶" id 1)
                     (propertize (format "   %s … %s"
                                         (emjupy--widget-number (gethash "min" state))
                                         (emjupy--widget-number (gethash "max" state)))
                                 'face 'shadow))
              'emjupy-widget id 'keymap emjupy-widget-map)))
      ("CheckboxModel"
       (list (concat (emjupy--widget-button
                      (if (eq (gethash "value" state) t) "[x]" "[ ]") id 'toggle)
                     " " (or desc ""))))
      ((or "DropdownModel" "RadioButtonsModel" "SelectModel" "ToggleButtonsModel")
       (let* ((labels (append (gethash "_options_labels" state) nil))
              (index (gethash "index" state)))
         (list (concat label (emjupy--widget-button
                              (format "%s ▾" (if (integerp index) (nth index labels) "--"))
                              id 'choose)))))
      ((or "VBoxModel" "HBoxModel" "BoxModel" "GridBoxModel")
       (cl-loop for child across (or (gethash "children" state) [])
                append (emjupy--widget-lines (emjupy--widget-ref child))))
      ;; its outputs are drawn as the cell's own
      ("OutputModel" nil)
      ((or "LabelModel" "HTMLModel" "HTMLMathModel")
       (list (format "%s" (gethash "value" state))))
      (_ (list (propertize (format "[widget: %s]" (string-remove-suffix "Model" name))
                           'face 'shadow))))))

(defun emjupy--widget-view (id)
  "Return the text showing the widget with comm ID, or nil to fall back."
  (when (emjupy--widget-state id)
    (let ((lines (emjupy--widget-lines id)))
      (and lines (concat (string-join lines "\n") "\n")))))

(defun emjupy--widget-send (id state-changes cell)
  "Send the widget with comm ID its new STATE-CHANGES, an alist.
CELL is the cell showing it, where what the change causes is drawn."
  (let* ((model (gethash id emjupy--widget-models))
         (kernel (car model))
         (state (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal))
         (content (make-hash-table :test 'equal)))
    (unless (and kernel (emjupy--ws-live-p kernel))
      (user-error "This widget's kernel is not connected"))
    (pcase-dolist (`(,k . ,v) state-changes)
      (puthash k v state)
      (puthash k v (cdr model)))
    (puthash "method" "update" data)
    (puthash "state" state data)
    (puthash "buffer_paths" [] data)
    (puthash "comm_id" id content)
    (puthash "data" data content)
    (let ((msg (emjupy--make-message "comm_msg" content)))
      (puthash (car msg) cell emjupy--widget-requests)
      (emjupy--ws-send (cdr msg) kernel))
    ;; show the new value at once
    (when-let* ((buf (emjupy-notebook-buffer (emjupy-kernel-notebook kernel))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (cl-pushnew cell emjupy--cells-awaiting-output))
        (emjupy-flush-output buf)))))

(defun emjupy--widget-at (event)
  "Return the widget control at EVENT\\='s position, or at point.
The value is a list of the comm id, the action and the cell."
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (id (get-text-property pos 'emjupy-widget)))
    (unless id (user-error "No widget control here"))
    (list id (get-text-property pos 'emjupy-widget-action)
          (emjupy--cell-at-point-including-output pos))))

(defun emjupy--widget-step (id direction cell)
  "Move the slider with comm ID one step in DIRECTION, -1 or 1, in CELL."
  (let* ((state (emjupy--widget-state id))
         (step (or (gethash "step" state) 1))
         (value (+ (gethash "value" state) (* direction step)))
         (value (min (gethash "max" state) (max (gethash "min" state) value))))
    (emjupy--widget-send id `(("value" . ,value)) cell)))

(defun emjupy-widget-activate (&optional event)
  "Use the widget control at point, or the one clicked on by EVENT."
  (interactive (list last-nonmenu-event))
  (pcase-let ((`(,id ,action ,cell) (emjupy--widget-at event)))
    (let ((state (emjupy--widget-state id)))
      (pcase action
        ((or -1 1) (emjupy--widget-step id action cell))
        ('nil (let ((value (read-number
                            (format "Value (%s to %s): "
                                    (emjupy--widget-number (gethash "min" state))
                                    (emjupy--widget-number (gethash "max" state)))
                            (gethash "value" state))))
                (emjupy--widget-send
                 id `(("value" . ,(min (gethash "max" state)
                                       (max (gethash "min" state) value))))
                 cell)))
        ('toggle (emjupy--widget-send
                  id `(("value" . ,(if (eq (gethash "value" state) t) :false t))) cell))
        ('choose
         (let* ((labels (append (gethash "_options_labels" state) nil))
                (choice (completing-read "Choose: " labels nil t)))
           (emjupy--widget-send id `(("index" . ,(cl-position choice labels
                                                               :test #'equal)))
                                cell)))))))

(defun emjupy-widget-increase ()
  "Move the slider at point one step up."
  (interactive)
  (pcase-let ((`(,id ,_ ,cell) (emjupy--widget-at nil)))
    (emjupy--widget-step id 1 cell)))

(defun emjupy-widget-decrease ()
  "Move the slider at point one step down."
  (interactive)
  (pcase-let ((`(,id ,_ ,cell) (emjupy--widget-at nil)))
    (emjupy--widget-step id -1 cell)))

(defun emjupy-widgets-enable ()
  "Show widgets as controls, and route what they cause to their cells.
Run when a notebook buffer starts, like the other hooks between layers,
so that loading this file changes nothing."
  (add-hook 'emjupy-comm-functions #'emjupy--widget-on-comm)
  (add-hook 'emjupy-output-owner-functions #'emjupy--widget-owner)
  (setq emjupy-widget-view-function #'emjupy--widget-view))

(provide 'emjupy-widgets)

;;; emjupy-widgets.el ends here
