;;; emjupy-core-test.el --- Unit tests: emjupy-core.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-core.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Data structures

(ert-deftest emjupy-test-struct-creation ()
  "Ensure structs initialize correctly."
  (let ((server (make-emjupy-server :host "localhost" :port 8888 :token "abc")))
    (should (equal (emjupy-server-host server) "localhost"))
    (should (equal (emjupy-server-token server) "abc"))))

;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-two-servers-keep-separate-xsrf ()
  "The XSRF cookie belongs to ONE server. Sharing it globally means a
second tunnel replays the first server's cookie and earns a 403."
  (let ((a (emjupy--intern-server "localhost:18888" "tok-a"))
        (b (emjupy--intern-server "localhost:19999" "tok-b")))
    (setf (emjupy-server-xsrf a) "xsrf-a")
    (setf (emjupy-server-xsrf b) "xsrf-b")
    (should (equal (emjupy-server-xsrf a) "xsrf-a"))
    (should (equal (emjupy-server-xsrf b) "xsrf-b"))
    (should-not (eq a b))
    ;; re-login to a known server updates its token but keeps the object,
    ;; so notebooks already pointing at it stay valid
    (let ((again (emjupy--intern-server "localhost:18888" "tok-a2")))
      (should (eq again a))
      (should (equal (emjupy-server-token a) "tok-a2"))
      (should (equal (emjupy-server-xsrf a) "xsrf-a")))))

;;;; Login: one port, one kernel

(ert-deftest emjupy-test-each-port-binds-its-own-kernel ()
  "One port = one kernel: two tunnels must not end up sharing one."
  (let ((a (emjupy--intern-server "localhost:18888" ""))
        (b (emjupy--intern-server "localhost:19999" "")))
    (setf (emjupy-server-kernel-id a) "kern-a")
    (setf (emjupy-server-kernel-id b) "kern-b")
    (should (equal (emjupy-server-kernel-id a) "kern-a"))
    (should (equal (emjupy-server-kernel-id b) "kern-b"))))

;;;; Output background band

(ert-deftest emjupy-test-nothing-but-output-is-recoloured ()
  "The buffer background is left completely alone, and a cell with no
output carries no background overlay at all.

Remapping `default' to a canvas colour, with cells painted back on top,
would invert the whole buffer the moment a cell colour was wrong."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (should-not (assq 'default face-remapping-alist))
        ;; the source overlay paints nothing
        (should-not (overlay-get (emjupy-cell-overlay cell) 'face))
        ;; and with no output there is no band overlay to paint
        (should-not (emjupy-cell-output-ov cell))))))

(ert-deftest emjupy-test-typing-is-highlighted-immediately ()
  "Highlighting must follow typing, not wait for the next render."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-end (emjupy-cell-overlay cell)))
        (forward-line -1)
        (end-of-line)
        (insert "\nimport os")
        (goto-char (point-min))
        (should (search-forward "import" nil t))
        (should (eq (get-text-property (match-beginning 0) 'face)
                    'font-lock-keyword-face))))))

(ert-deftest emjupy-test-typing-does-not-inherit-neighbouring-face ()
  "Text typed straight after a keyword must not pick up its face.
`self-insert-command' inherits sticky properties from the character
before point, so a plain word typed after `import' would come out
keyword coloured."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "import os"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-min))
        (should (search-forward "import" nil t))
        ;; type immediately after the keyword, the way `self-insert' would
        (insert-and-inherit "ed_thing")
        (goto-char (point-min))
        (should (search-forward "ed_thing" nil t))
        (should-not (eq (get-text-property (match-beginning 0) 'face)
                        'font-lock-keyword-face))))))

;;;; Rich output: display_data / image routing

(ert-deftest emjupy-test-image-preferred-over-text ()
  "When a result carries both image/png and text/plain, the image
branch is taken -- on an Emacs that can actually display images."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p)
             (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type
                                                         :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _)
               (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "plot()"
                                   :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "<<IMG:png>>" (buffer-string))))))))

(ert-deftest emjupy-test-image-falls-back-to-text-without-image-support ()
  "On an Emacs that CANNOT display images (a tty frame, or a build
without libpng -- `emacs-nox' is the common case), an image output must
degrade to the bundle's own text/plain repr plus a visible note.

Regression test: `create-image' SIGNALS `Invalid image type' rather
than returning nil, and rendering happens inside the websocket
callback where websocket.el swallows errors -- so the failure mode was
a silently blank output box with nothing logged."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p)
             (lambda (&rest _) nil)))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "plot()"
                                   :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure size 640x480>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((text (buffer-string)))
            ;; the text repr is shown ...
            (should (string-match-p "<Figure size 640x480>" text))
            ;; ... and the user is told why there's no picture
            (should (string-match-p "image/png" text))
            ;; and the output box is NOT empty
            (should (emjupy-cell-output-ov cell))))))))

(ert-deftest emjupy-test-image-render-error-is-reported-not-swallowed ()
  "If `create-image' signals even though the type looked available, the
cell must still render something explaining itself rather than a blank box."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p)
             (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (&rest _) (error "Invalid image type `png'"))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "plot()"
                                   :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "could not render" (buffer-string))))))))

(ert-deftest emjupy-test-duplicate-image-rendered-once ()
  "A cell whose last expression is a figure receives the SAME picture
twice from the kernel -- once as the execute_result repr, once as the
inline backend's display_data. It must be drawn once."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p)
             (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type
                                                         :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _)
               (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "fig"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (kind)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" "iVBORw0KGgoSAMEPAYLOAD==" dat)
                   (puthash "text/plain"
                            "<Figure size 640x480 with 1 Axes>" dat)
                   (puthash "output_type" kind oh)
                   (puthash "data" dat oh)
                   oh))))
      (setf (emjupy-cell-outputs cell)
            (vector (funcall mk "execute_result") (funcall mk "display_data")))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((n 0) (start 0))
            (while (string-match "<<IMG:png>>" (buffer-string) start)
              (setq n (1+ n) start (match-end 0)))
            (should (= n 1))))))))

(ert-deftest emjupy-test-distinct-images-both-rendered ()
  "Deduplication must only collapse IDENTICAL images -- two different
figures in one cell are two outputs and both have to show."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p)
             (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type
                                                         :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _)
               (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "two figs"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (payload)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" payload dat)
                   (puthash "output_type" "display_data" oh)
                   (puthash "data" dat oh)
                   oh))))
      (setf (emjupy-cell-outputs cell)
            (vector (funcall mk "iVBORw0KGgoAAAAA")
                    (funcall mk "iVBORw0KGgoBBBBB")))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((n 0) (start 0))
            (while (string-match "<<IMG:" (buffer-string) start)
              (setq n (1+ n) start (match-end 0)))
            (should (= n 2))))))))

(ert-deftest emjupy-test-repeated-text-output-is-not-deduplicated ()
  "Two identical `print' calls really are two outputs -- only images
are collapsed."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "print"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda ()
               (let ((oh (make-hash-table :test 'equal)))
                 (puthash "output_type" "stream" oh)
                 (puthash "name" "stdout" oh)
                 (puthash "text" "same line\n" oh)
                 oh))))
    (setf (emjupy-cell-outputs cell) (vector (funcall mk) (funcall mk)))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((n 0) (start 0))
          (while (string-match "same line" (buffer-string) start)
            (setq n (1+ n) start (match-end 0)))
          (should (= n 2)))))))

;;;; Several notebooks / several servers at once

(ert-deftest emjupy-test-each-notebook-has-its-own-kernel ()
  "Two open notebooks must hold two independent kernels. When kernel
state was global, opening the second silently stole the first's
connection."
  (let* ((nb1 (make-emjupy-notebook :path "one.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:18888")))
         (nb2 (make-emjupy-notebook :path "two.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:19999")))
         (k1 (make-emjupy-kernel :id "k-one"
                                 :pending (make-hash-table :test 'equal)
                                 :notebook nb1))
         (k2 (make-emjupy-kernel :id "k-two"
                                 :pending (make-hash-table :test 'equal)
                                 :notebook nb2)))
    (setf (emjupy-notebook-kernel nb1) k1)
    (setf (emjupy-notebook-kernel nb2) k2)
    (should-not (eq (emjupy-notebook-kernel nb1) (emjupy-notebook-kernel nb2)))
    (should-not (eq (emjupy-kernel-pending k1) (emjupy-kernel-pending k2)))
    ;; and each kernel points back at its OWN notebook
    (should (eq (emjupy-kernel-notebook k1) nb1))
    (should (eq (emjupy-kernel-notebook k2) nb2))))

;;;; Syntax highlighting

(ert-deftest emjupy-test-python-cell-gets-real-highlighting ()
  "Code cells are fontified via real `python-mode' (bundled with Emacs
core) in a throwaway temp buffer, not a fake major-mode-shadowing
hack -- this must produce actual `face' text properties."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "def foo(x):\n    return x + 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (let* ((ov (emjupy-cell-overlay cell))
             (faces (cl-loop for p from (overlay-start ov) below
                             (overlay-end ov)
                             for f = (get-text-property p 'face)
                             when f collect f)))
        (should faces)))))

(ert-deftest emjupy-test-markdown-cell-degrades-gracefully ()
  "Markdown cells render without error whether or not markdown-ts-mode
or markdown-mode happen to be installed (neither ships with Emacs)."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                                :source "# Title\nSome **bold** text"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should (buffer-live-p buf)))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-arriving-output-does-not-move-point ()
  "Output arriving must not drag the cursor.

Re-rendering with the executing cell as TARGET-CELL moves point to the
START of that cell, and the WebSocket handler did exactly that -- so
every line of output yanked the cursor to the top of the running cell,
and output landing while you edited elsewhere threw you across the
buffer mid-keystroke."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "one"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "two = 2"
                               :outputs [] :metadata (make-hash-table)))
         (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "streaming\n" oh)
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        ;; the user is typing three characters into cell 2
        (goto-char (+ 3 (overlay-start (emjupy-cell-overlay c2))))
        ;; output arrives for cell 1
        (emjupy-test--append-and-flush c1 oh nb)
        ;; still in cell 2, at the same place within it
        (should (eq (emjupy--cell-at-point) c2))
        (should (= (- (point) (overlay-start (emjupy-cell-overlay c2))) 3))
        ;; and the output did land
        (should (string-match-p "streaming" (buffer-string)))))))

(ert-deftest emjupy-test-editing-survives-arriving-output ()
  "Output arriving must not throw away what has been typed since.

The re-render rebuilds the buffer from the cell structs, so without
folding the buffer back in first, everything typed while a slow cell ran
was silently reverted -- which is what \"the notebook gets scrambled\"
looked like from the outside."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "slow()"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "x = 1"
                               :outputs [] :metadata (make-hash-table)))
         (oh (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" oh)
    (puthash "name" "stdout" oh)
    (puthash "text" "RESULT\n" oh)
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        ;; type into the second cell while the first is still running
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (end-of-line)
        (insert " + 99")
        ;; the first cell now produces output
        (emjupy-test--append-and-flush c1 oh nb)
        ;; the edit is still there, in the buffer and in the struct
        (should (string-match-p (regexp-quote "+ 99") (buffer-string)))
        (should (equal (emjupy-cell-source c2) "x = 1 + 99"))
        ;; and the output landed
        (should (string-match-p "RESULT" (buffer-string)))))))

(ert-deftest emjupy-test-ansi-colours-rendered ()
  "Escapes from termcolor and friends become colours, not literal text."
  (let ((o (make-hash-table :test 'equal))
        (cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "p()"
                                :outputs [] :metadata (make-hash-table))))
    (puthash "output_type" "stream" o)
    (puthash "name" "stdout" o)
    (puthash "text" "\033[33myellow\033[0m plain\n" o)
    (setf (emjupy-cell-outputs cell) (vector o))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (point-min))
        ;; no raw escapes on screen
        (should-not (save-excursion (search-forward "33m" nil t)))
        (should (search-forward "yellow" nil t))
        ;; and the word actually carries a colour, as a `face' property --
        ;; ansi-color marks its output with `font-lock-face', which is only
        ;; honoured where font-lock runs, and emjupy leaves it off
        (let ((face (get-text-property (- (point) 3) 'face)))
          (should (cl-some (lambda (f) (and (consp f) (plist-get f
                                                                 :foreground)))
                           (if (listp face) face (list face)))))))))

(ert-deftest emjupy-test-yank-strips-properties-from-older-text ()
  "Text killed before the filter existed is cleaned on the way in too."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x = 1"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let ((kill-ring (list (propertize "y = 2" 'emjupy-cell
                                           'a-stale-struct)))
              (kill-ring-yank-pointer nil))
          (setq kill-ring-yank-pointer kill-ring)
          (goto-char (overlay-start (emjupy-cell-overlay cell)))
          (end-of-line)
          (yank)
          (backward-char 2)
          (should-not (get-text-property (point) 'emjupy-cell)))))))

;;;; Performance

(ert-deftest emjupy-test-cells-behave-like-the-language ()
  "A cell edits like a buffer of the language it holds.

Not by emulating commands one at a time: most of what a programming mode
gives is the syntax table, which decides what counts as a symbol and so
what \\[dabbrev-expand] finds and where \\[forward-word] stops, and the
comment variables, without which \\[comment-dwim] has no comment syntax
and says so.  emjupy-mode derives from `fundamental-mode', which has
neither."
  (let ((cell (emjupy-test--cell-like
               'code "from library import my_function\nx = my_function()")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; the comment syntax is there, so M-; has something to insert
        (should (equal comment-start "# "))
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (forward-line 1)
        (end-of-line)
        (comment-dwim nil)
        (should (string-match-p "#" (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position))))
        ;; an underscore is part of a symbol, or every symbol is cut short
        (should (= (char-syntax ?_) ?_))
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should (search-forward "my_fun" nil t))
        (should (equal (thing-at-point 'symbol t) "my_function"))))))

(ert-deftest emjupy-test-dabbrev-expands-across-cells ()
  "\\[dabbrev-expand] finds a word defined in another cell.

It searches the buffer, and the buffer holds every cell -- so this works
as soon as the syntax table agrees about what a word is."
  (let ((c1 (emjupy-test--cell-like 'code
                                    "def compute_everything(x):\n    return x"))
        (c2 (emjupy-test--cell-like 'code "y = comp")))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (end-of-line)
        (dabbrev-expand nil)
        (should (equal (buffer-substring-no-properties
                        (line-beginning-position) (line-end-position))
                       "y = compute_everything"))))))

(ert-deftest emjupy-test-nothing-bound-breaks-undo ()
  "No command in the keymap may quietly cost the undo history.

The package redraws constantly, and a full redraw moves every buffer
position an undo entry holds -- so losing undo is the default outcome of
almost any change here, and has to be prevented deliberately each time.
This asks the question of every bound command at once, so that a command
added later is covered without anyone remembering to think about it.

Each command is run after typing into the first cell, with point in a
different cell; afterwards the typing must still be undoable."
  (let ((checked 0)
        (broke nil))
    (dolist (command (emjupy-test--notebook-commands))
      (unless (memq command emjupy-test--undo-exempt)
        (setq checked (1+ checked))
        (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                             (emjupy-test--cell-like 'code "b = 2\nc = 3")
                             (emjupy-test--cell-like
                              'code "d = 4"
                              (vector (emjupy-test--stream-output "out\n"))))))
          (emjupy-test--with-notebook cells buf nb
            (with-current-buffer buf
              (buffer-enable-undo)
              (setq buffer-undo-list nil)
              ;; type into the first cell
              (goto-char (overlay-start
                          (emjupy-cell-overlay
                           (aref (emjupy-notebook-cells nb) 0))))
              (end-of-line)
              (insert " + 7")
              (undo-boundary)
              ;; act somewhere else, so the typing is incidental to it
              (let ((elsewhere (aref (emjupy-notebook-cells nb)
                                     (1- (length (emjupy-notebook-cells nb))))))
                (when (overlayp (emjupy-cell-overlay elsewhere))
                  (goto-char (overlay-start (emjupy-cell-overlay elsewhere)))))
              (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                        ((symbol-function 'read-string) (lambda (&rest _) "x"))
                        ((symbol-function 'completing-read) (lambda (&rest _)
                                                              "x")))
                (ignore-errors (call-interactively command)))
              ;; and the typing must still be takeable back.  More than one step
              ;; is allowed -- an undoable operation, deleting a cell, comes
              ;; back first -- but not the typing gone from the history.
              (goto-char (point-min))
              (let ((steps 0))
                (while (and (< steps 4)
                            (string-match-p "\\+ 7" (buffer-string))
                            (ignore-errors
                              (undo)
                              ;; Continued, not restarted: a structural command
                              ;; is its own step, the typing behind it.  Not
                              ;; `undo-more': a step recording its own redo ends
                              ;; the sequence.
                              (setq this-command 'undo last-command 'undo)
                              t))
                  (setq steps (1+ steps)))
                ;; Left as it was found: `last-command' is global, and a
                ;; later test that presses undo once would otherwise be
                ;; continuing this sequence rather than starting its own.
                (setq this-command nil last-command nil))
              (when (string-match-p "\\+ 7" (buffer-string))
                (push command broke)))))))
    (should (> checked 3))
    (should (equal (nreverse broke) nil))))

;;;; The undo guard

(ert-deftest emjupy-test-output-after-the-reply-is-kept ()
  "An output that arrives after execute_reply still reaches its cell.

Outputs come on the iopub channel and the reply on the shell channel,
and the protocol does not order them against each other.  Forgotten on
the reply, the request would leave a result arriving afterwards nothing
to go to, and it would be dropped -- the order a slow machine produces."
  (pcase-let ((`(,cell ,kernel ,id)
               (emjupy-test--feed-in-order '(reply result idle))))
    (should (= (length (emjupy-cell-outputs cell)) 1))
    (should (equal (gethash "output_type" (aref (emjupy-cell-outputs cell) 0))
                   "execute_result"))
    (should (eql (emjupy-cell-exec-count cell) 1))
    ;; and once both halves have come, the request is forgotten
    (should-not (gethash id (emjupy-kernel-pending kernel)))))

(ert-deftest emjupy-test-idle-before-the-reply-still-counts ()
  "The closing idle may come first; the reply after it is still applied.

Forgetting the request on idle alone would drop the execution count."
  (pcase-let ((`(,cell ,kernel ,id)
               (emjupy-test--feed-in-order '(result idle reply))))
    (should (eql (emjupy-cell-exec-count cell) 1))
    (should (= (length (emjupy-cell-outputs cell)) 1))
    (should-not (gethash id (emjupy-kernel-pending kernel)))))

(ert-deftest emjupy-test-request-waits-for-both-halves ()
  "A request stays open until both its reply and its idle have arrived."
  (pcase-let ((`(,_cell ,kernel ,id) (emjupy-test--feed-in-order '(reply))))
    (should (gethash id (emjupy-kernel-pending kernel))))
  (pcase-let ((`(,_cell ,kernel ,id) (emjupy-test--feed-in-order '(idle))))
    (should (gethash id (emjupy-kernel-pending kernel)))))

(ert-deftest emjupy-test-folding-leaves-the-output-alone ()
  "Folding changes the drawing, never what is saved."
  (let* ((text (concat (make-string 300 ?x) "\n"))
         (out (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" "stream" o) (puthash "name" "stdout" o)
                (puthash "text" text o) o))
         (cell (emjupy-test--cell-like 'code "x" (vector out))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        ;; the stored output is exactly what arrived
        (should (equal (gethash "text"
                                (aref (emjupy-cell-outputs cell) 0)) text))
        ;; and each break the drawing added is marked as one
        (let ((ov (emjupy-cell-output-ov cell)) (marked 0))
          (save-excursion
            (goto-char (overlay-start ov))
            (while (search-forward "\n" (overlay-end ov) t)
              (when (get-text-property (1- (point)) 'emjupy-fold)
                (setq marked (1+ marked)))))
          (should (> marked 0)))))))

(ert-deftest emjupy-test-a-cell-first-line-keeps-its-line-number ()
  "Between two cells the separator starts no screen line, so it is not numbered.
A screen line is numbered by the line it begins on.  The separator was
hidden, so the screen line with the next cell's first line began on the
separator and showed its number; the first line's own was never seen,
and each cell's numbers ran 29, 31.  Now the box's last newline is drawn
as itself with the rules after it -- a `display', since Emacs draws no
overlay string at a hidden position -- and the separator is left as it
is, ending the rules' last screen line."
  (let ((cells (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "a = 1\nc = 3"
                                         :outputs []
                                         :metadata (make-hash-table))
                       (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                         :source "d = 4"
                                         :outputs []
                                         :metadata (make-hash-table)))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let* ((ov (emjupy-cell-overlay (aref cells 0)))
               (anchor (1- (overlay-end ov)))
               (separator (overlay-end ov))
               (next (overlay-start (emjupy-cell-overlay (aref cells 1)))))
          ;; the rules: a display on the box's last newline, the next header in
          ;; it
          (should (cl-some (lambda (o)
                             (let ((d (overlay-get o 'display)))
                               (and (stringp d) (string-prefix-p "\n" d)
                                    (string-match-p "In:" d))))
                           (overlays-at anchor)))
          ;; the separator: there, neither hidden nor drawn over
          (should (< separator next))
          (should-not (invisible-p separator))
          (should-not (get-char-property separator 'display))
          ;; and the next cell begins a line of its own, visible
          (should (save-excursion (goto-char next) (bolp)))
          (should-not (invisible-p next)))))))


(ert-deftest emjupy-test-execution-seconds-from-metadata ()
  "How long a cell ran, from JupyterLab's keys, fractions of seconds kept."
  (should (= (emjupy--cell-execution-seconds
              (emjupy-test--timed-cell "2026-10-28T05:16:41.123456Z"
                                       "2026-10-28T05:16:42.373456+00:00"))
             1.25))
  (should-not (emjupy--cell-execution-seconds
               (emjupy-test--timed-cell "2026-10-28T05:16:41Z" nil)))
  (should-not (emjupy--cell-execution-seconds
               (emjupy-test--timed-cell "2026-10-28T05:16:42Z"
                                        "2026-10-28T05:16:41Z"))))

(ert-deftest emjupy-test-durations-read-short ()
  "A duration in the unit that reads best."
  (should (equal (emjupy--format-duration 0.085) "85 ms"))
  (should (equal (emjupy--format-duration 1.4) "1.4 s"))
  (should (equal (emjupy--format-duration 185) "3m 05s"))
  (should (equal (emjupy--format-duration 3725) "1h 02m")))

(provide 'emjupy-core-test)

;;; emjupy-core-test.el ends here
