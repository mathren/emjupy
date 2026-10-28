;;; emjupy-toc.el --- The notebook's outline: its headings, to see and jump to  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; The headings of a notebook's markdown cells, as JupyterLab's table of
;; contents has them, two ways.  An imenu index, always: M-g i, and any
;; tool built on imenu, jumps to a heading by name.  And a panel at the
;; side of the frame, off until asked for: the headings nested by level,
;; the one point is under marked, each a button to jump to it.  Open, it
;; follows the notebook's edits; closed, nothing follows anything.

;;; Code:

(require 'cl-lib)
(require 'emjupy-core)
(require 'emjupy-render)

;;;; Options

(defcustom emjupy-toc-width 32
  "Width of the outline panel, in columns."
  :type 'natnum
  :safe #'natnump
  :group 'emjupy-appearance)

;;;; State

(defvar-local emjupy--toc-notebook nil
  "In an outline panel: the notebook buffer it outlines.")

(defvar-local emjupy--toc-timer nil
  "In a notebook: the timer that will refresh its outline panel.")

(defvar-local emjupy--toc-mark nil
  "In an outline panel: the overlay on the heading point is under.")

;;;; The headings

(defun emjupy--toc-headings ()
  "Return this notebook's headings: (LEVEL TEXT POSITION), in order.
Read from the markdown cells' text as it is in the buffer, so edits not
yet synced are seen; a # in a fenced code block is no heading."
  (let (headings)
    (when emjupy--buffer-notebook
      (cl-loop
       for cell across (emjupy-notebook-cells emjupy--buffer-notebook)
       for ov = (emjupy-cell-overlay cell)
       when (and (eq (emjupy-cell-type cell) 'markdown) (overlayp ov))
       do (save-excursion
            (goto-char (overlay-start ov))
            (let ((fenced nil))
              (while (< (point) (overlay-end ov))
                (cond ((looking-at "[ \t]*\\(```\\|~~~\\)")
                       (setq fenced (not fenced)))
                      ((and (not fenced)
                            (looking-at "\\(#\\{1,6\\}\\)[ \t]+\\(.*[^ \t\n]\\)"))
                       (push (list (length (match-string 1))
                                   (match-string-no-properties 2)
                                   (point))
                             headings)))
                (forward-line 1))))))
    (nreverse headings)))

(defun emjupy--imenu-index ()
  "The notebook's headings for imenu, indented by level."
  (mapcar (lambda (h)
            (cons (concat (make-string (* 2 (1- (nth 0 h))) ?\s) (nth 1 h))
                  (nth 2 h)))
          (emjupy--toc-headings)))

;;;; The panel

(defun emjupy--toc-buffer-name (notebook)
  "The name of the outline panel of NOTEBOOK, a buffer."
  (format "*emjupy outline: %s*" (buffer-name notebook)))

(defvar-keymap emjupy-toc-mode-map
  :doc "Keys of the outline panel."
  "n" #'forward-button
  "p" #'backward-button
  "g" #'emjupy-toc-refresh
  "q" #'emjupy-toc)

(define-derived-mode emjupy-toc-mode special-mode "emjupy-outline"
  "The outline of a notebook: its headings, each a button to jump to it."
  (setq-local truncate-lines t))

(defun emjupy--toc-jump (button)
  "Go to the heading of BUTTON, in its notebook's window."
  (let* ((marker (button-get button 'emjupy-heading))
         (notebook (marker-buffer marker)))
    (when (buffer-live-p notebook)
      (let ((window (get-buffer-window notebook)))
        (if window (select-window window) (pop-to-buffer notebook)))
      (goto-char marker)
      (recenter 0))))

(defun emjupy-toc-refresh ()
  "Draw the outline panel anew, from its notebook as it is now."
  (interactive)
  (let* ((notebook (if (derived-mode-p 'emjupy-toc-mode)
                       emjupy--toc-notebook
                     (current-buffer)))
         (panel (and (buffer-live-p notebook)
                     (get-buffer (emjupy--toc-buffer-name notebook)))))
    (when panel
      (let ((headings (with-current-buffer notebook (emjupy--toc-headings))))
        (with-current-buffer panel
          (let ((inhibit-read-only t) (line (line-number-at-pos)))
            (erase-buffer)
            (if (null headings)
                (insert (propertize "No headings: a markdown cell's # lines\nmake them."
                                    'face 'shadow))
              (dolist (h headings)
                (insert (make-string (* 2 (1- (nth 0 h))) ?\s))
                (insert-text-button (nth 1 h)
                                    'action #'emjupy--toc-jump
                                    'follow-link t
                                    'emjupy-heading
                                    (with-current-buffer notebook
                                      (copy-marker (nth 2 h)))
                                    'help-echo "Go to this heading")
                (insert "\n")))
            (goto-char (point-min))
            (forward-line (1- line)))))
      (with-current-buffer notebook (emjupy--toc-mark-current)))))

(defun emjupy--toc-mark-current ()
  "In the panel, mark the heading point in this notebook is under."
  (let ((panel (get-buffer (emjupy--toc-buffer-name (current-buffer))))
        (here (point)))
    (when panel
      (with-current-buffer panel
        (let (found)
          (save-excursion
            (goto-char (point-min))
            (while (not (eobp))
              ;; a nested heading's button starts after its indentation
              (let ((b (save-excursion (back-to-indentation) (button-at (point)))))
                (when (and b (<= (button-get b 'emjupy-heading) here))
                  (setq found (line-beginning-position))))
              (forward-line 1)))
          (if (not found)
              (when (overlayp emjupy--toc-mark) (delete-overlay emjupy--toc-mark))
            (unless (overlayp emjupy--toc-mark)
              (setq emjupy--toc-mark (make-overlay 1 1))
              (overlay-put emjupy--toc-mark 'face 'highlight))
            (move-overlay emjupy--toc-mark found
                          (save-excursion (goto-char found)
                                          (1+ (line-end-position))))
            (let ((window (get-buffer-window panel)))
              (when window (set-window-point window found)))))))))

(defun emjupy--toc-edited (&rest _)
  "Refresh the outline panel soon after an edit, once."
  (when (timerp emjupy--toc-timer) (cancel-timer emjupy--toc-timer))
  (setq emjupy--toc-timer
        (run-with-idle-timer 0.5 nil
                             (lambda (buf)
                               (when (buffer-live-p buf)
                                 (with-current-buffer buf
                                   (setq emjupy--toc-timer nil)
                                   (emjupy-toc-refresh))))
                             (current-buffer))))

(defun emjupy--toc-follow (on)
  "Have the panel follow this notebook's edits and point if ON; else stop."
  (if on
      (progn (add-hook 'after-change-functions #'emjupy--toc-edited nil t)
             (add-hook 'post-command-hook #'emjupy--toc-mark-current nil t))
    (remove-hook 'after-change-functions #'emjupy--toc-edited t)
    (remove-hook 'post-command-hook #'emjupy--toc-mark-current t)
    (when (timerp emjupy--toc-timer) (cancel-timer emjupy--toc-timer))
    (setq emjupy--toc-timer nil)))

(defun emjupy-toc ()
  "Show the notebook's outline in a panel at the side, or hide it.
Off until asked for.  Its headings are buttons: RET or a click goes to
one; n and p move between them, g draws it anew, q hides it."
  (interactive)
  (let* ((notebook (if (derived-mode-p 'emjupy-toc-mode)
                       emjupy--toc-notebook
                     (current-buffer)))
         (name (emjupy--toc-buffer-name notebook))
         (window (and (get-buffer name) (get-buffer-window name))))
    (unless (buffer-local-value 'emjupy--buffer-notebook notebook)
      (user-error "Not in a notebook"))
    (if window
        (progn (delete-window window)
               (with-current-buffer notebook (emjupy--toc-follow nil)))
      (let ((panel (get-buffer-create name)))
        (with-current-buffer panel
          (emjupy-toc-mode)
          (setq emjupy--toc-notebook notebook))
        (with-current-buffer notebook
          (emjupy--toc-follow t)
          (emjupy-toc-refresh))
        (display-buffer-in-side-window
         panel `((side . left) (slot . 0)
                 (window-width . ,emjupy-toc-width)
                 (preserve-size . (t . nil))))))))

(defun emjupy-toc-enable ()
  "Give this notebook its outline, for imenu."
  (setq-local imenu-create-index-function #'emjupy--imenu-index))

(provide 'emjupy-toc)
;;; emjupy-toc.el ends here
