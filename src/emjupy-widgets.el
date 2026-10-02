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

;;;; Drawing a widget from the shape of its state

;; A widget is drawn from what its state holds, not from its type: a value
;; with bounds and a step is stepped, option labels are chosen from,
;; children are drawn in turn.  So a widget is drawn without being named
;; here -- one this file has never heard of, too, if its state has a
;; familiar shape.  The rules, in `emjupy-widget-rules', are tried in
;; order; a widget no rule fits shows as its name.

(defun emjupy--widget-button (label id action &optional arg)
  "Return LABEL as a control acting on the widget with comm ID.
ACTION says what using it does, with ARG: see `emjupy-widget-activate'."
  (propertize label 'face 'link 'mouse-face 'highlight
              'keymap emjupy-widget-map
              'emjupy-widget id 'emjupy-widget-action action
              'emjupy-widget-arg arg
              'help-echo "RET or click; on a number, <left> and <right> too"))

(defun emjupy--widget-number (n)
  "Return the number N formatted for a widget."
  (if (floatp n) (format "%g" n) (format "%s" n)))

(defun emjupy--widget-has (state &rest keys)
  "Return non-nil if STATE has every one of KEYS."
  (cl-every (lambda (k) (not (eq (gethash k state 'emjupy--absent) 'emjupy--absent)))
            keys))

(defun emjupy--widget-numbers-p (value)
  "Return non-nil if VALUE is a vector of numbers."
  (and (vectorp value) (cl-every #'numberp value)))

(defun emjupy--widget-date-p (value)
  "Return non-nil if VALUE is a date or time, as widgets send them."
  (and (hash-table-p value)
       (or (emjupy--widget-has value "year" "month" "date")
           (emjupy--widget-has value "hours" "minutes"))))

(defun emjupy--widget-date-text (value)
  "Return the date or time VALUE as text: ISO 8601, as it is edited.
A widget\'s month counts from 0, as in JavaScript."
  (let ((date (and (gethash "year" value)
                   (format "%04d-%02d-%02d" (gethash "year" value)
                           (1+ (gethash "month" value)) (gethash "date" value))))
        (time (and (gethash "hours" value)
                   (format "%02d:%02d:%02d" (gethash "hours" value)
                           (gethash "minutes" value) (or (gethash "seconds" value) 0)))))
    (string-join (delq nil (list date time)) " ")))

(defun emjupy--widget-bar (value low high)
  "Return a bar showing VALUE between LOW and HIGH."
  (let* ((width 20)
         (fill (if (> high low)
                   (round (* width (/ (float (- value low)) (- high low))))
                 0))
         (fill (max 0 (min width fill))))
    (concat (make-string fill ?█) (make-string (- width fill) ?░)
            (format " %s" (emjupy--widget-number value)))))

(defun emjupy--widget-strip-html (text)
  "Return TEXT with HTML tags removed, for a widget showing HTML."
  (string-trim (replace-regexp-in-string "<[^>]*>" "" text)))

;; Each rule's test and drawing.  A drawing is called with the comm id and
;; the state, and returns the widget's lines.

(defun emjupy--widget-container-p (s)
  "Return non-nil if S, a widget's state, has the shape of a container."
  (emjupy--widget-has s "children"))
(defun emjupy--widget-draw-container (_id s)
  "Draw the children of S, under their titles if they have some.
ID is the widget's comm id."
  (let ((titles (append (gethash "titles" s) nil)))
    (cl-loop for child across (gethash "children" s)
             for i from 0
             for title = (nth i titles)
             append (append
                     (and (stringp title) (not (string-empty-p title))
                          (list (propertize (concat "── " title) 'face 'bold)))
                     (let ((child-id (emjupy--widget-ref child)))
                       (if (emjupy--widget-state child-id)
                           (emjupy--widget-lines child-id)
                         ;; announced in a binary message, which is
                         ;; not read: it is there, if not shown
                         (list (propertize "[widget]" 'face 'shadow))))))))

(defun emjupy--widget-output-p (s)
  "Return non-nil if S, a widget's state, has the shape of a output."
  (equal (gethash "_model_name" s) "OutputModel"))
(defun emjupy--widget-draw-nothing (_id _s)
  "Draw nothing: an output area's outputs are drawn as the cell's own.
ID is the widget's comm id."
  nil)

(defun emjupy--widget-choose-several-p (s)
  "Return non-nil if S, a widget's state, has the shape of a choose several."
  (and (emjupy--widget-has s "_options_labels") (vectorp (gethash "index" s))))
(defun emjupy--widget-draw-choose-several (id s)
  "Draw the options chosen in S, a control choosing several.
ID is the widget's comm id."
  (let ((labels (append (gethash "_options_labels" s) nil)))
    (list (emjupy--widget-button
           (format "%s ▾" (if (seq-empty-p (gethash "index" s)) "(none)"
                             (mapconcat (lambda (i) (nth i labels)) (gethash "index" s) ", ")))
           id 'choose-several))))

(defun emjupy--widget-choose-p (s)
  "Return non-nil if S, a widget's state, has the shape of a choose."
  (emjupy--widget-has s "_options_labels"))
(defun emjupy--widget-draw-choose (id s)
  "Draw the option chosen in S, a control choosing one.
ID is the widget's comm id."
  (let ((labels (append (gethash "_options_labels" s) nil))
        (index (gethash "index" s)))
    (list (emjupy--widget-button
           (format "%s ▾" (if (integerp index) (nth index labels) "--"))
           id 'choose))))

(defun emjupy--widget-button-p (s)
  "Return non-nil if S, a widget's state, has the shape of a button."
  (and (emjupy--widget-has s "button_style") (not (emjupy--widget-has s "value"))))
(defun emjupy--widget-draw-button (id s)
  "Draw S as a button to click.
ID is the widget's comm id."
  (let ((d (gethash "description" s)))
    (list (emjupy--widget-button
           (format "[ %s ]" (if (and (stringp d) (not (string-empty-p d))) d "Button"))
           id 'click))))

(defun emjupy--widget-toggle-p (s)
  "Return non-nil if S, a widget's state, has the shape of a toggle."
  (memq (gethash "value" s) '(t :false)))
(defun emjupy--widget-draw-toggle (id s)
  "Draw S, true or false: a verdict only shown, else a box to toggle.
ID is the widget's comm id."
  (let ((on (eq (gethash "value" s) t)))
    (list (if (emjupy--widget-has s "readout")
              (if on "✓" "✗")
            (emjupy--widget-button (if on "[x]" "[ ]") id 'toggle)))))

(defun emjupy--widget-range-p (s)
  "Return non-nil if S, a widget's state, has the shape of a range."
  (let ((v (gethash "value" s)))
    (and (emjupy--widget-numbers-p v) (= (length v) 2)
         (emjupy--widget-has s "min" "max" "orientation"))))
(defun emjupy--widget-draw-range (id s)
  "Draw S, two numbers between bounds, each end editable.
ID is the widget's comm id."
  (let ((v (gethash "value" s)))
    (list (concat (emjupy--widget-button (emjupy--widget-number (aref v 0)) id 'edit 0)
                  " – "
                  (emjupy--widget-button (emjupy--widget-number (aref v 1)) id 'edit 1)
                  (emjupy--widget-bounds s)))))

(defun emjupy--widget-bar-p (s)
  "Return non-nil if S, a widget's state, has the shape of a bar."
  (and (numberp (gethash "value" s)) (emjupy--widget-has s "min" "max")
       (not (emjupy--widget-has s "step"))))
(defun emjupy--widget-draw-bar (_id s)
  "Draw S, a number between bounds without a step, as a bar to look at.
ID is the widget's comm id."
  (list (emjupy--widget-bar (gethash "value" s) (gethash "min" s) (gethash "max" s))))

(defun emjupy--widget-step-p (s)
  "Return non-nil if S, a widget's state, has the shape of a step."
  (and (numberp (gethash "value" s)) (emjupy--widget-has s "min" "max" "step")))
(defun emjupy--widget-draw-step (id s)
  "Draw S, a number between bounds with a step, to be stepped or edited.
ID is the widget's comm id."
  (list (concat (emjupy--widget-button "◀" id 'step -1) " "
                (emjupy--widget-button
                 (propertize (emjupy--widget-number (gethash "value" s)) 'face 'bold)
                 id 'edit)
                " " (emjupy--widget-button "▶" id 'step 1)
                (emjupy--widget-bounds s))))

(defun emjupy--widget-number-p (s)
  "Return non-nil if S, a widget's state, has the shape of a number."
  (numberp (gethash "value" s)))
(defun emjupy--widget-date-kind (s)
  "Return `date', `time' or `both' if S is for a date or a time, else nil.
From its value, or -- when it has none yet -- from its model\'s name, the
one thing that says what a value it would take."
  (let ((v (gethash "value" s))
        (name (or (gethash "_model_name" s) "")))
    (cond ((hash-table-p v)
           (cond ((and (gethash "year" v) (gethash "hours" v)) 'both)
                 ((gethash "year" v) 'date)
                 ((gethash "hours" v) 'time)))
          ((eq v :null)
           (let ((case-fold-search nil)
                 (date (string-match-p "Date" name))
                 (time (string-match-p "Time\\|time" name)))
             (cond ((and date time) 'both) (date 'date) (time 'time)))))))

(defun emjupy--widget-date-value-p (s)
  "Return non-nil if S, a widget's state, has the shape of a date value."
  (emjupy--widget-date-kind s))(defun emjupy--widget-list-p (s)
  "Return non-nil if S is a list of words or numbers, edited as text.
Those widgets say whether a value may repeat; a file upload\'s value is
a list too, of files, which no text stands for."
  (and (vectorp (gethash "value" s)) (emjupy--widget-has s "allow_duplicates")))
(defun emjupy--widget-typed-p (s)
  "Return non-nil if S, a widget's state, has the shape of a typed."
  (and (stringp (gethash "value" s)) (emjupy--widget-has s "continuous_update")))
(defun emjupy--widget-color-p (s)
  "Return non-nil if S, a widget's state, has the shape of a color."
  (and (stringp (gethash "value" s)) (emjupy--widget-has s "concise")))
(defun emjupy--widget-draw-edit (id s)
  "Draw the value of S in brackets, to be edited.
ID is the widget's comm id."
  (let ((v (gethash "value" s)))
    (list (emjupy--widget-button
           (format "[%s]"
                   (cond ((numberp v) (emjupy--widget-number v))
                         ((emjupy--widget-date-p v) (emjupy--widget-date-text v))
                         ((eq v :null) "--")
                         ((vectorp v) (mapconcat (lambda (x) (format "%s" x)) v ", "))
                         ((equal (gethash "_model_name" s) "PasswordModel")
                          (make-string (length v) ?•))
                         (t v)))
           id 'edit))))

(defun emjupy--widget-text-p (s)
  "Return non-nil if S, a widget's state, has the shape of a text."
  (stringp (gethash "value" s)))
(defun emjupy--widget-draw-text (_id s)
  "Draw S, text to read: a label, or HTML without its tags.
ID is the widget's comm id."
  (list (emjupy--widget-strip-html (gethash "value" s))))

(defun emjupy--widget-limits (s)
  "Return the lowest and highest value of S, as a cons.
A widget with a `base\=' is logarithmic: its bounds are exponents."
  (let ((low (gethash "min" s)) (high (gethash "max" s)) (base (gethash "base" s)))
    (if (and (numberp base) (numberp low) (numberp high))
        (cons (expt (float base) low) (expt (float base) high))
      (cons low high))))

(defun emjupy--widget-bounds (s)
  "Return the bounds of S, shown after its control."
  (let ((limits (emjupy--widget-limits s)))
    (propertize (format "   %s … %s" (emjupy--widget-number (car limits))
                        (emjupy--widget-number (cdr limits)))
                'face 'shadow)))

(defvar emjupy-widget-rules
  (list
   (list 'container      #'emjupy--widget-container-p      #'emjupy--widget-draw-container)
   (list 'output         #'emjupy--widget-output-p         #'emjupy--widget-draw-nothing)
   (list 'choose-several #'emjupy--widget-choose-several-p #'emjupy--widget-draw-choose-several)
   (list 'choose         #'emjupy--widget-choose-p         #'emjupy--widget-draw-choose)
   (list 'button         #'emjupy--widget-button-p         #'emjupy--widget-draw-button)
   (list 'toggle         #'emjupy--widget-toggle-p         #'emjupy--widget-draw-toggle)
   (list 'range          #'emjupy--widget-range-p          #'emjupy--widget-draw-range)
   (list 'bar            #'emjupy--widget-bar-p            #'emjupy--widget-draw-bar)
   (list 'step           #'emjupy--widget-step-p           #'emjupy--widget-draw-step)
   (list 'number         #'emjupy--widget-number-p         #'emjupy--widget-draw-edit)
   (list 'date           #'emjupy--widget-date-value-p     #'emjupy--widget-draw-edit)
   (list 'list           #'emjupy--widget-list-p           #'emjupy--widget-draw-edit)
   (list 'typed          #'emjupy--widget-typed-p          #'emjupy--widget-draw-edit)
   (list 'color          #'emjupy--widget-color-p          #'emjupy--widget-draw-edit)
   (list 'text           #'emjupy--widget-text-p           #'emjupy--widget-draw-text))
  "How a widget is drawn, by the shape of its state: (NAME TEST DRAW).
The first rule whose TEST is true of the state draws the widget: DRAW is
called with the comm id and the state, and returns the widget\'s lines.
The order matters -- a range\'s value is a list, and is tried before
lists; options before a value, since a dropdown has both.")

(defun emjupy--widget-rule (state)
  "Return the rule of `emjupy-widget-rules' that draws STATE, or nil."
  (cl-find-if (lambda (rule) (funcall (nth 1 rule) state)) emjupy-widget-rules))

(defun emjupy--widget-lines (id)
  "Return the text lines for the widget with comm ID, or nil if unknown."
  (let* ((state (emjupy--widget-state id))
         (rule (and state (emjupy--widget-rule state))))
    (cond
     ((null state) nil)
     ((null rule)
      (list (propertize (format "[widget: %s]"
                                (string-remove-suffix "Model" (or (gethash "_model_name" state) "?")))
                        'face 'shadow)))
     (t
      (let ((lines (funcall (nth 2 rule) id state))
            (desc (gethash "description" state)))
        ;; a description goes before the control, but a container\'s
        ;; children have their own, and a button wears it already
        (if (and lines (stringp desc) (not (string-empty-p desc))
                 (not (memq (car rule) '(container button))))
            (cons (concat desc " " (car lines)) (cdr lines))
          lines))))))

(defun emjupy--widget-view (id)
  "Return the text showing the widget with comm ID, or nil to fall back."
  (when (emjupy--widget-state id)
    (let ((lines (emjupy--widget-lines id)))
      (and lines (concat (string-join lines "\n") "\n")))))

;;;; Using a widget

(defun emjupy--widget-message (id data cell)
  "Send the widget with comm ID the comm message DATA, a hash table.
CELL is the cell showing it, where what the message causes is drawn."
  (let* ((kernel (car (gethash id emjupy--widget-models)))
         (content (make-hash-table :test 'equal)))
    (unless (and kernel (emjupy--ws-live-p kernel))
      (user-error "This widget's kernel is not connected"))
    (puthash "comm_id" id content)
    (puthash "data" data content)
    (let ((msg (emjupy--make-message "comm_msg" content)))
      (puthash (car msg) cell emjupy--widget-requests)
      (emjupy--ws-send (cdr msg) kernel))))

(defun emjupy--widget-send (id state-changes cell)
  "Send the widget with comm ID its new STATE-CHANGES, an alist.
CELL is the cell showing it, where what the change causes is drawn."
  (let* ((model (gethash id emjupy--widget-models))
         (state (make-hash-table :test 'equal))
         (data (make-hash-table :test 'equal)))
    (pcase-dolist (`(,k . ,v) state-changes)
      (puthash k v state)
      (puthash k v (cdr model)))
    (puthash "method" "update" data)
    (puthash "state" state data)
    (puthash "buffer_paths" [] data)
    (emjupy--widget-message id data cell)
    ;; show the new value at once
    (when-let* ((buf (emjupy-notebook-buffer (emjupy-kernel-notebook (car model)))))
      (when (buffer-live-p buf)
        (with-current-buffer buf (cl-pushnew cell emjupy--cells-awaiting-output))
        (emjupy-flush-output buf)))))

(defun emjupy--widget-at (event)
  "Return the widget control at EVENT\'s position, or at point.
The value is a list of the comm id, the action, its argument and the
cell."
  (let* ((pos (if (mouse-event-p event) (posn-point (event-start event)) (point)))
         (id (get-text-property pos 'emjupy-widget)))
    (unless id (user-error "No widget control here"))
    (list id (get-text-property pos 'emjupy-widget-action)
          (get-text-property pos 'emjupy-widget-arg)
          (emjupy--cell-at-point-including-output pos))))

(defun emjupy--widget-clamp (state value)
  "Return VALUE held within STATE\'s bounds, if it has any."
  (let* ((limits (emjupy--widget-limits state)) (low (car limits)) (high (cdr limits)))
    (if (and (numberp low) (numberp high)) (min high (max low value)) value)))

(defun emjupy--widget-step (id direction cell)
  "Move the number of the widget with comm ID a step in DIRECTION, in CELL.
A widget with a `base\\=' is logarithmic: a step multiplies by the base."
  (let* ((state (emjupy--widget-state id))
         (step (or (gethash "step" state) 1))
         (value (gethash "value" state))
         (base (gethash "base" state))
         (new (if (numberp base)
                  (* value (expt base (* direction step)))
                (+ value (* direction step)))))
    (emjupy--widget-send id `(("value" . ,(emjupy--widget-clamp state new))) cell)))

(defun emjupy--widget-read (state arg)
  "Ask for a new value for the widget whose state is STATE.
What is asked for follows the value\'s kind; ARG is the end of a range
being edited, 0 or 1.  Returns the value to send."
  (let* ((value (gethash "value" state))
         (limits (emjupy--widget-limits state))
         (bounds (and (numberp (car limits))
                      (format " (%s to %s)" (emjupy--widget-number (car limits))
                              (emjupy--widget-number (cdr limits))))))
    (cond
     ((integerp arg)                    ; one end of a range
      (let ((v (copy-sequence value)))
        (aset v arg (emjupy--widget-clamp
                     state (read-number (format "%s%s: " (if (= arg 0) "From" "To") (or bounds ""))
                                        (aref value arg))))
        v))
     ((numberp value)
      (let ((n (emjupy--widget-clamp state (read-number (format "Value%s: " (or bounds "")) value))))
        (if (integerp value) (round n) n)))
     ((emjupy--widget-date-kind state)
      (let* ((kind (emjupy--widget-date-kind state))
             (text (read-string (pcase kind
                                  ('date "Date (YYYY-MM-DD): ")
                                  ('time "Time (HH:MM:SS): ")
                                  (_ "Date and time (YYYY-MM-DD HH:MM:SS): "))
                                (and (hash-table-p value) (emjupy--widget-date-text value))))
             (parsed (parse-time-string text))
             (new (make-hash-table :test 'equal)))
        (when (memq kind '(date both))
          (unless (decoded-time-year parsed) (user-error "Not a date: %s" text))
          (puthash "year" (decoded-time-year parsed) new)
          (puthash "month" (1- (decoded-time-month parsed)) new) ; from 0, as in JavaScript
          (puthash "date" (decoded-time-day parsed) new))
        (when (memq kind '(time both))
          (puthash "hours" (or (decoded-time-hour parsed) 0) new)
          (puthash "minutes" (or (decoded-time-minute parsed) 0) new)
          (puthash "seconds" (or (decoded-time-second parsed) 0) new)
          (puthash "milliseconds" 0 new))
        new))
     ((vectorp value)
      (let ((items (split-string (read-string "Values, separated by commas: "
                                              (mapconcat (lambda (x) (format "%s" x)) value ", "))
                                 "[ \t]*,[ \t]*" t)))
        (vconcat (if (emjupy--widget-numbers-p value)
                     (mapcar #'string-to-number items)
                   items))))
     ((equal (gethash "_model_name" state) "PasswordModel") (read-passwd "Password: "))
     ((emjupy--widget-has state "concise")
      (let ((c (color-values (read-color "Colour: "))))
        (if c (apply #'format "#%02x%02x%02x" (mapcar (lambda (x) (/ x 256)) c)) value)))
     (t (read-string "Text: " value)))))

(defun emjupy-widget-activate (&optional event)
  "Use the widget control at point, or the one clicked on by EVENT.
What it does follows the control: a step, a toggle, a choice of one or
several options, a click, or a new value asked for in the minibuffer."
  (interactive (list last-nonmenu-event))
  (pcase-let ((`(,id ,action ,arg ,cell) (emjupy--widget-at event)))
    (let ((state (emjupy--widget-state id)))
      (pcase action
        ('step (emjupy--widget-step id arg cell))
        ('toggle (emjupy--widget-send
                  id `(("value" . ,(if (eq (gethash "value" state) t) :false t))) cell))
        ('choose
         (let* ((labels (append (gethash "_options_labels" state) nil))
                (choice (completing-read "Choose: " labels nil t)))
           (emjupy--widget-send id `(("index" . ,(cl-position choice labels :test #'equal)))
                                cell)))
        ('choose-several
         (let* ((labels (append (gethash "_options_labels" state) nil))
                (chosen (completing-read-multiple "Choose (separated by commas): " labels nil t)))
           (emjupy--widget-send
            id `(("index" . ,(vconcat (mapcar (lambda (c) (cl-position c labels :test #'equal))
                                              chosen))))
            cell)))
        ('click
         (let ((data (make-hash-table :test 'equal))
               (event (make-hash-table :test 'equal)))
           (puthash "event" "click" event)
           (puthash "method" "custom" data)
           (puthash "content" event data)
           (emjupy--widget-message id data cell)))
        ('edit (emjupy--widget-send id `(("value" . ,(emjupy--widget-read state arg))) cell))))))

(defun emjupy--widget-step-at-point (direction)
  "Step the number of the widget at point in DIRECTION, -1 or 1."
  (pcase-let ((`(,id ,_ ,_ ,cell) (emjupy--widget-at nil)))
    (unless (numberp (gethash "value" (emjupy--widget-state id)))
      (user-error "This widget has no number to step"))
    (emjupy--widget-step id direction cell)))

(defun emjupy-widget-increase ()
  "Step the number of the widget at point up."
  (interactive)
  (emjupy--widget-step-at-point 1))

(defun emjupy-widget-decrease ()
  "Step the number of the widget at point down."
  (interactive)
  (emjupy--widget-step-at-point -1))

(defun emjupy-widgets-enable ()
  "Show widgets as controls, and route what they cause to their cells.
Run when a notebook buffer starts, like the other hooks between layers,
so that loading this file changes nothing."
  (add-hook 'emjupy-comm-functions #'emjupy--widget-on-comm)
  (add-hook 'emjupy-output-owner-functions #'emjupy--widget-owner)
  (setq emjupy-widget-view-function #'emjupy--widget-view))

(provide 'emjupy-widgets)

;;; emjupy-widgets.el ends here
