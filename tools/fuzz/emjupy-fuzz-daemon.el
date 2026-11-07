;;; emjupy-fuzz-daemon.el --- The fuzz session in a daemon, step by step  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; emjupy-fuzz.el's session, in an Emacs daemon with a frame, as one
;; works: daemon-fuzz.sh starts the daemon, loads this, and asks for one
;; step at a time with emacsclient.  Between steps the daemon is idle:
;; output, reconnections and timers run as they do between keys.  The
;; script disrupts the network, or the daemon itself, between steps.
;;
;; Besides EMJUPY_FUZZ_*, as emjupy-fuzz.el reads them:
;;   EMJUPY_FUZZ_MESSAGES  where each message goes, timed, with the
;;                         notebook in view and whether it was shown

;;; Code:

(setenv "EMJUPY_FUZZ_LIBRARY" "1")
(require 'emjupy-fuzz)

(defvar fzd-message-log
  (fz-env "EMJUPY_FUZZ_MESSAGES" "/tmp/emjupy-fuzz-messages.log"))

(defun fzd-in-view ()
  "Return the name of the buffer in the selected window."
  (buffer-name (window-buffer (selected-window))))

(defun fzd-note (kind text)
  "Log TEXT, of KIND, with the time and the buffer in view."
  (write-region (format "%.2f\t%s\t%s\t%s\n" (- (float-time) fz-start) kind
                        (fzd-in-view)
                        (replace-regexp-in-string "\n" " | " text))
                nil fzd-message-log t 'silent))

(defun fzd-record-messages ()
  "Log every message and warning: what reached the user, and what not."
  (advice-add 'message :around
              (lambda (orig format-string &rest args)
                (let ((text (apply orig format-string args)))
                  (when (stringp text)
                    (fzd-note (if inhibit-message "quiet" "shown") text))
                  text)))
  (advice-add 'display-warning :before
              (lambda (type message &rest _)
                (fzd-note "warning" (format "%s: %s" type message)))))

(defun fzd-setup ()
  "Prepare the daemon: a frame to work in, the logs, the notebooks open."
  (write-region "" nil fz-log-file nil 'silent)
  (write-region "" nil fzd-message-log nil 'silent)
  (select-frame (make-frame-on-display (getenv "DISPLAY")
                                       '((width . 100) (height . 45))))
  (setq emjupy-language-support
        (not (equal (fz-env "EMJUPY_FUZZ_EGLOT" "1") "0")))
  (fzd-record-messages)
  (fz-log "daemon: seed %d, Emacs %s, language support %s"
          fz-seed emacs-version emjupy-language-support)
  (fz-watch-blocking)
  (fz-open-all)
  (fz-new-async-errors)
  (when fz-notebooks (fz-show (nth 2 (car fz-notebooks))))
  (format "%d notebooks, %d failures" (length fz-notebooks)
          (length fz-failures)))

(defun fzd-step (step)
  "Do STEP; return the failures so far, a number."
  (when fz-notebooks
    (set-buffer (window-buffer (selected-window)))
    (fz-one-step step))
  (length fz-failures))

(defun fzd-cell-with (buffer text)
  "Return BUFFER's first cell whose source holds TEXT, or nil."
  (seq-find (lambda (c) (string-search text (emjupy-cell-source c)))
            (fz-cells buffer)))

(defun fzd-run-cell-with (text)
  "Run, in every notebook, the cell holding TEXT -- all at once."
  (dolist (nb fz-notebooks)
    (let* ((buffer (nth 2 nb)) (cell (fzd-cell-with buffer text)))
      (when cell
        (fz-show buffer)
        (emjupy--goto-cell cell 'beginning)
        (fz-act 'emjupy-execute-cell-at-point)))))

(defun fzd-run-slow-everywhere ()
  "Start the slow cell in every notebook, at once."
  (fz-log "concurrent: the slow cell run in every notebook")
  (fzd-run-cell-with "tick"))

(defun fzd-share-kernel ()
  "Attach the second notebook to the first one's kernel, if they share a server."
  (let* ((first (buffer-local-value 'emjupy--buffer-notebook
                                    (nth 2 (nth 0 fz-notebooks))))
         (second (buffer-local-value 'emjupy--buffer-notebook
                                     (nth 2 (nth 1 fz-notebooks))))
         (kernel (emjupy-notebook-kernel first)))
    (if (and kernel (eq (emjupy-notebook-server first)
                        (emjupy-notebook-server second)))
        (condition-case err
            (progn (emjupy-connect-kernel second (emjupy-kernel-id kernel))
                   (fz-log "shared: %s now on %s's kernel"
                           (buffer-name (emjupy-notebook-buffer second))
                           (buffer-name (emjupy-notebook-buffer first)))
                   "shared")
          (error (fz-fail "sharing a kernel raised %S" err) "failed"))
      "not shared: different servers")))

(defvar fzd-probe nil "Each notebook's probe cell and its count, before.")

(defun fzd-probe-start ()
  "Run a small cell in every notebook, to see when each answers again."
  (setq fzd-probe
        (delq nil
              (mapcar (lambda (nb)
                        (let ((cell (fzd-cell-with (nth 2 nb) "sys.version")))
                          (and cell (list (nth 2 nb) cell
                                          (emjupy-cell-exec-count cell)))))
                      fz-notebooks)))
  (fzd-run-cell-with "sys.version")
  (length fzd-probe))

(defun fzd-probe-pending ()
  "Return how many notebooks have not answered the probe yet."
  (seq-count (lambda (p)
               (pcase-let ((`(,buffer ,cell ,before) p))
                 (or (not (buffer-live-p buffer))
                     (equal (emjupy-cell-exec-count cell) before)
                     (memq (emjupy-cell-id cell)
                           (buffer-local-value 'emjupy--running-cells
                                               buffer)))))
             fzd-probe))

(defun fzd-end ()
  "End as emjupy-fuzz.el does: every cell run, saved, compared; a summary."
  (setq fz-step-name "the end")
  (dolist (nb fz-notebooks)
    (fz-run-everything (nth 2 nb))
    (fz-save-and-compare (nth 0 nb) (nth 2 nb)))
  (dolist (err (fz-new-async-errors)) (fz-fail "at the end: %s" err))
  (fz-log "the longest Emacs was blocked: %.2f s, in %s"
          fz-blocked fz-blocked-at)
  (fz-log "language server routes: %S"
          (mapcar (lambda (nb) (emjupy-notebook-lsp-route
                                (buffer-local-value 'emjupy--buffer-notebook
                                                    (nth 2 nb))))
                  fz-notebooks))
  (fz-log "== %d failure(s), %d problem(s)" (length fz-failures)
          (length fz-problems))
  (length fz-failures))

(provide 'emjupy-fuzz-daemon)

;;; emjupy-fuzz-daemon.el ends here
