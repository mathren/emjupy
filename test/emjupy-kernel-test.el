;;; emjupy-kernel-test.el --- Unit tests: emjupy-kernel.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-kernel.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)

;;;; Server URL parsing (remote kernels reached through an SSH tunnel)

(ert-deftest emjupy-test-kernel-ws-url-is-scheme-and-path-aware ()
  "The kernel WebSocket URL carries the scheme and the base path.
Not the token, which goes in a header: an address is what proxies and
servers log.  For a server that refused the header alone, the token goes
in the address too, url-encoded -- never as a bare `?token='."
  (let ((captured nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest _) (setq captured url) nil)))
      (let* ((server (make-emjupy-server
                      :base-url "https://example.org:8443/user/alice"
                      :token "t o k"))
             (nb (make-emjupy-notebook :path "a.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "wss://" captured))
        (should (string-match-p "/user/alice/api/kernels/kid/channels"
                                captured))
        (should-not (string-match-p "token=" captured))
        (let ((emjupy--token-in-ws-address
               (list "https://example.org:8443/user/alice")))
          (emjupy-connect-kernel nb "kid")
          (should (string-match-p "token=t%20o%20k" captured))))
      (let* ((server (make-emjupy-server :base-url "localhost:18888" :token ""))
             (nb (make-emjupy-notebook :path "b.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "ws://" captured))
        (should-not (string-match-p "token=" captured))))))

;;;; Execution: cell order must never scramble; sync must cover ALL cells

(ert-deftest emjupy-test-execute-preserves-cell-order ()
  "Executing a non-last cell must not shove it (and its output) to the
end of the buffer -- output belongs directly after its own cell, and
every later cell must still render after it, in order."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "1+1" :outputs []
                              :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "2+2" :outputs []
                              :metadata (make-hash-table)))
        (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "3+3" :outputs []
                              :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point)
         ;; the code that got sent is the cell we asked for, not a neighbour
         (should (string-match-p (regexp-quote "1+1") (car emjupy-test--sent))))
        (let ((oh (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" oh) (puthash "name" "stdout" oh)
          (puthash "text" "2\n" oh)
          (emjupy-test--append-and-flush c1 oh nb))
        (should (< (overlay-start (emjupy-cell-output-ov c1))
                   (overlay-start (emjupy-cell-overlay c2))))
        (should (< (overlay-start (emjupy-cell-overlay c2))
                   (overlay-start (emjupy-cell-overlay c3))))))))

(ert-deftest emjupy-test-execute-syncs-all-cells-not-just-current ()
  "Executing one cell must not discard unsynced live edits sitting in a
DIFFERENT cell -- the rerender it triggers rebuilds the whole buffer
from cell structs, so every cell must be synced first, not just the
one being run."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "1+1" :outputs []
                              :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source ""
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         ;; type into cell 2 with no explicit sync, the way live typing works
         (goto-char (overlay-start (emjupy-cell-overlay c2)))
         (insert "999")
         ;; then execute cell 1
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point))
        (should (string-match-p "999" (buffer-string)))))))

;;;; Rich output: display_data / image routing

(ert-deftest emjupy-test-display-data-not-dropped ()
  "display_data messages -- how matplotlib's inline figure actually
arrives, separately from the execute_result text repr -- must be
captured, not silently ignored."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "plot()"
                                 :outputs [] :metadata (make-hash-table)))
         (msg-id "msg-display-data-test")
         (nb (make-emjupy-notebook :cells (vector cell)))
         (kernel (make-emjupy-kernel :id "k1"
                                     :pending (make-hash-table :test 'equal)
                                     :notebook nb)))
    (puthash msg-id cell (emjupy-kernel-pending kernel))
    (let* ((hdr (make-hash-table :test 'equal))
           (ph (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal))
           (content (make-hash-table :test 'equal))
           (msg (make-hash-table :test 'equal)))
      (puthash "msg_type" "display_data" hdr)
      (puthash "msg_id" msg-id ph)
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "data" dat content)
      (puthash "header" hdr msg) (puthash "parent_header" ph msg)
      (puthash "content" content msg)
      ;; The raw payload, which the handler accepts as a frame would be, so
      ;; recorded traffic can be replayed.  Not in `ignore-errors': an error
      ;; must fail the test, not pass as "0 outputs".
      (emjupy--handle-ws-message kernel (json-serialize msg))
      (should (> (length (emjupy-cell-outputs cell)) 0))
      (should (equal (gethash "output_type"
                              (aref (emjupy-cell-outputs cell) 0))
                     "display_data")))))

;;;; Kernel restart

(ert-deftest emjupy-test-restart-kernel-requires-connection ()
  "Restarting with no kernel signals a clear user-error instead of
failing deep inside some HTTP call."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "x"
                                :outputs [] :metadata (make-hash-table)))
      buf nb
    (should-error (emjupy-restart-kernel) :type 'user-error)))

(ert-deftest emjupy-test-restart-kernel-reuses-same-id ()
  "Restart must POST to THIS notebook's own kernel id and reconnect
with that SAME id (not spawn a fresh one), and drop now-orphaned
pending requests."
  (let* ((server (make-emjupy-server :base-url "localhost:8888" :token "tok"))
         (restart-path nil) (reconnected-with nil))
    (emjupy-test--with-notebook
        (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                  :source "x"
                                  :outputs [] :metadata (make-hash-table)))
        buf nb
      (setf (emjupy-notebook-server nb) server)
      (let ((kernel (make-emjupy-kernel :id "kernel-abc" :server server
                                        :pending (make-hash-table :test 'equal)
                                        :notebook nb)))
        (setf (emjupy-notebook-kernel nb) kernel)
        (puthash "stale-msg" 'some-cell (emjupy-kernel-pending kernel))
        ;; Sent without waiting; reconnected once the server answers.
        (cl-letf (((symbol-function 'emjupy--http-request-async)
                   (lambda (_method _server path _body on-done _on-fail)
                     (when (string-match-p "/restart" path)
                       (setq restart-path path))
                     (funcall on-done (make-hash-table :test 'equal))))
                  ((symbol-function 'emjupy-connect-kernel)
                   (lambda (notebook kernel-id &optional name)
                     (setq reconnected-with (list notebook kernel-id name))))
                  ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) nil)))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
            (emjupy-restart-kernel)))
        (should (string-match-p "kernel-abc" restart-path))
        (should (equal (nth 1 reconnected-with) "kernel-abc"))
        (should (eq (nth 0 reconnected-with) nb))
        (should (= (hash-table-count (emjupy-kernel-pending kernel)) 0))))))

;;;; Several notebooks / several servers at once

(ert-deftest emjupy-test-output-goes-to-the-requesting-notebook ()
  "A frame must be delivered to the notebook whose kernel produced it,
not to whichever buffer happens to be current. This is the
cross-talk that global `emjupy--current-notebook-buffer' caused: a
slow cell in notebook A finishing while you were reading notebook B
wrote A's output into B."
  (let* ((cell-a (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "a"
                                   :outputs [] :metadata (make-hash-table)))
         (cell-b (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                   :source "b"
                                   :outputs [] :metadata (make-hash-table)))
         (buf-a (generate-new-buffer "*emjupy-A*"))
         (buf-b (generate-new-buffer "*emjupy-B*"))
         (nb-a (make-emjupy-notebook :path "A.ipynb" :cells (vector cell-a)
                                     :buffer buf-a))
         (nb-b (make-emjupy-notebook :path "B.ipynb" :cells (vector cell-b)
                                     :buffer buf-b))
         (k-a (make-emjupy-kernel :id "ka"
                                  :pending (make-hash-table :test 'equal)
                                  :notebook nb-a)))
    (unwind-protect
        (progn
          (dolist (pair (list (cons buf-a nb-a) (cons buf-b nb-b)))
            (with-current-buffer (car pair)
              (emjupy-mode)
              (setq emjupy--buffer-notebook (cdr pair))
              (emjupy--redraw-notebook)))
          (puthash "msg-a" cell-a (emjupy-kernel-pending k-a))
          ;; deliver A's frame while B is the current buffer
          (with-current-buffer buf-b
            (let* ((hdr (make-hash-table :test 'equal))
                   (ph (make-hash-table :test 'equal))
                   (content (make-hash-table :test 'equal))
                   (msg (make-hash-table :test 'equal)))
              (puthash "msg_type" "stream" hdr)
              (puthash "msg_id" "msg-a" ph)
              (puthash "name" "stdout" content)
              (puthash "text" "belongs-to-A\n" content)
              (puthash "header" hdr msg) (puthash "parent_header" ph msg)
              (puthash "content" content msg)
              (emjupy--handle-ws-message k-a (json-serialize msg)))
            ;; output is coalesced behind a timer; force the redraw
            (emjupy-flush-output buf-a))
          (should (= (length (emjupy-cell-outputs cell-a)) 1))
          (should (= (length (emjupy-cell-outputs cell-b)) 0))
          (with-current-buffer buf-a
            (should (string-match-p "belongs-to-A" (buffer-string))))
          (with-current-buffer buf-b
            (should-not (string-match-p "belongs-to-A" (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (b (list buf-a buf-b)) (when (buffer-live-p b)
                                         (kill-buffer b)))))))

;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-execute-and-goto-next-lands-on-the-next-cell ()
  "Running a cell leaves point at the START of the FOLLOWING cell."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "one"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "two"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-and-goto-next)
         ;; the cell ran ...
         (should (string-match-p "one" (car emjupy-test--sent)))
         ;; ... and point is at the top of the next one, not back on c1
         (should (eq (emjupy--cell-at-point) c2))
         (should (= (point) (overlay-start (emjupy-cell-overlay c2))))
         ;; nothing was added
         (should (= (length (emjupy-notebook-cells nb)) 2)))))))

(ert-deftest emjupy-test-execute-and-goto-next-makes-a-cell-at-the-end ()
  "Running the LAST cell opens a fresh one below and goes there, rather
than leaving point stranded at the bottom."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "only"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay cell)))
         (emjupy-execute-cell-and-goto-next)
         (let ((cells (emjupy-notebook-cells emjupy--buffer-notebook)))
           (should (= (length cells) 2))
           (should (equal (emjupy-cell-source (aref cells 1)) ""))
           (should (eq (emjupy--cell-at-point) (aref cells 1)))
           (should (= (point)
                      (overlay-start
                       (emjupy-cell-overlay (aref cells 1)))))))))))

(ert-deftest emjupy-test-execute-bindings ()
  "C-c C-c runs and advances; Shift-RET runs and stays put."
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-c"))
              'emjupy-execute-cell-and-goto-next))
  (should (eq (lookup-key emjupy-mode-map (kbd "S-<return>"))
              'emjupy-execute-cell-at-point))
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-e"))
              'emjupy-execute-cell-at-point)))

(ert-deftest emjupy-test-execute-in-place-does-not-advance ()
  "Shift-RET leaves point in the cell it ran."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "one"
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "two"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point)
         (should (eq (emjupy--cell-at-point) c1))
         (should (= (length (emjupy-notebook-cells nb)) 2)))))))

(ert-deftest emjupy-test-keybindings-have-no-duplicates ()
  "Each command is reachable by one key, so `C-h m' is not a wall of
synonyms.  The prefix keys are the exception: they carry sub-bindings."
  (let ((by-command (make-hash-table :test 'eq))
        (dupes nil))
    (map-keymap
     (lambda (_event def)
       (when (and (symbolp def) (fboundp def))
         (puthash def (1+ (gethash def by-command 0)) by-command)))
     emjupy-mode-map)
    (maphash (lambda (cmd n)
               ;; Bound twice on purpose, as terminals and graphical frames send
               ;; different keys: Shift-RET to run, DEL and <backspace> to
               ;; reveal a formula.
               (when (and (> n 1)
                          (not (memq cmd '(emjupy-execute-cell-at-point
                                           emjupy-latex-unrender-or-delete))))
                 (push (cons cmd n) dupes)))
             by-command)
    (should-not dupes)))

(ert-deftest emjupy-test-kernel-bindings-separate-stop-from-select ()
  "C-c C-z stops what is running; picking a kernel moves to C-c M-z."
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c C-z"))
              'emjupy-interrupt-kernel))
  (should (eq (lookup-key emjupy-mode-map (kbd "C-c M-z"))
              'emjupy-connect-kernel-interactive)))

(ert-deftest emjupy-test-kernel-started-through-a-session ()
  "Kernels are started via a session bound to the notebook.

Jupyter starts a session's kernel in the notebook's own directory, so
`open(\"data.csv\")' in a cell means what it means in Jupyter.  Posting
to /api/kernels directly would leave the kernel in the server root."
  (let ((calls nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (method _s path &optional body &rest _)
                 (push (list method path body) calls)
                 (let ((h (make-hash-table :test 'equal))
                       (k (make-hash-table :test 'equal)))
                   (puthash "id" "k1" k)
                   (puthash "kernel" k h)
                   (puthash "id" "k1" h)
                   h)))
              ((symbol-function 'emjupy-connect-kernel) (lambda (&rest _) nil)))
      (emjupy--spawn-and-connect-kernel
       (make-emjupy-notebook :path "sub/nb.ipynb" :cells []
                             :server (make-emjupy-server :base-url "h"
                                                         :token "t")))
      (let ((session (cl-find "/api/sessions" calls :key #'cadr :test #'equal)))
        (should session)
        (let ((body (json-parse-string (nth 2 session)
                                       :object-type 'hash-table)))
          (should (equal (gethash "path" body) "sub/nb.ipynb"))
          (should (equal (gethash "type" body) "notebook")))))))

(ert-deftest emjupy-test-internal-requests-do-not-reach-cells ()
  "emjupy's own questions to the kernel must not print into the notebook."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "x"
                                 :outputs [] :metadata (make-hash-table)))
         (got nil))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (let* ((kernel (make-emjupy-kernel
                        :id "k" :name "python3"
                        :pending (make-hash-table :test 'equal)
                        :notebook emjupy--buffer-notebook)))
          (clrhash emjupy--internal-requests)
          (puthash "mid" (cons (lambda (out) (setq got out)) nil)
                   emjupy--internal-requests)
          (emjupy--handle-ws-message
           kernel
           (json-serialize
            (let ((m (make-hash-table :test 'equal))
                  (h (make-hash-table :test 'equal))
                  (p (make-hash-table :test 'equal))
                  (c (make-hash-table :test 'equal)))
              (puthash "msg_type" "stream" h)
              (puthash "msg_id" "mid" p)
              (puthash "text" "/home/me/nb\n" c)
              (puthash "name" "stdout" c)
              (puthash "header" h m)
              (puthash "parent_header" p m)
              (puthash "content" c m)
              m)))
          ;; the answer is handed over when the request is done
          (emjupy--handle-ws-message
           kernel (emjupy-test--kernel-msg "status" "mid"
                                           '(("execution_state" . "idle"))))
          ;; the callback saw it ...
          ;; as printed: a caller asking for a line trims it itself
          (should (equal got "/home/me/nb\n"))
          ;; ... and the cell did not
          (should (= (length (emjupy-cell-outputs cell)) 0))
          (should-not (string-match-p "/home/me/nb" (buffer-string))))))))

(ert-deftest emjupy-test-output-redraws-are-coalesced ()
  "A burst of output costs one redraw, not one per message.

A redraw rebuilds the whole notebook.  Doing that per arriving message
is the difference between a progress bar and a frozen Emacs: `tqdm'
sends one message per iteration, and each redraw of a notebook with
figures costs tens of milliseconds."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "loop()"
                                :outputs [] :metadata (make-hash-table)))
        (renders 0))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (cl-letf* ((real (symbol-function 'emjupy--redraw-pending-output))
                   ((symbol-function 'emjupy--redraw-pending-output)
                    (lambda (&rest args) (setq renders (1+ renders))
                      (apply real args))))
          (dotimes (i 50)
            (let ((o (make-hash-table :test 'equal)))
              (puthash "output_type" "stream" o)
              (puthash "name" "stdout" o)
              (puthash "text" (format "step %d\n" i) o)
              (emjupy--append-output-to-cell cell o nb)))
          ;; nothing has been drawn yet; one redraw is pending
          (should (= renders 0))
          (emjupy-flush-output buf)
          (should (= renders 1)))))))

(ert-deftest emjupy-test-progress-bar-collapses-to-one-output ()
  "Consecutive stream output on one channel is one stream, as in Jupyter.

Keeping each message is what let a `tqdm' bar leave thousands of entries
behind -- every one walked on every redraw, so the notebook stayed slow
for the rest of the session.  Carriage returns collapse too, which is
what makes a progress bar a bar rather than a thousand lines."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                :source "loop()"
                                :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (dotimes (i 50)
          (let ((o (make-hash-table :test 'equal)))
            (puthash "output_type" "stream" o)
            (puthash "name" "stderr" o)
            (puthash "text" (format "\r%d%%|#####| %d/50" i i) o)
            (emjupy--append-output-to-cell cell o nb)))
        (emjupy-flush-output buf)
        (should (= (length (emjupy-cell-outputs cell)) 1))
        ;; only the last state of the bar survives
        (let ((text (emjupy--mime-text
                     (gethash "text" (aref (emjupy-cell-outputs cell) 0)))))
          (should (string-match-p "49/50" text))
          (should-not (string-match-p "0/50" text)))
        ;; a different channel is still its own output
        (let ((o (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" o)
          (puthash "name" "stdout" o)
          (puthash "text" "done\n" o)
          (emjupy--append-output-to-cell cell o nb))
        (should (= (length (emjupy-cell-outputs cell)) 2))))))

(ert-deftest emjupy-test-output-redrawn-in-place ()
  "Output arriving redraws its own cell, not the whole notebook."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((full 0))
          (cl-letf* ((real (symbol-function
                            'emjupy--redraw-notebook-keeping-point))
                     ((symbol-function 'emjupy--redraw-notebook-keeping-point)
                      (lambda (&rest args) (setq full (1+ full))
                        (apply real args))))
            (emjupy--append-output-to-cell c1 (emjupy-test--stream "one\n") nb)
            (emjupy-flush-output buf))
          ;; no full rebuild was needed
          (should (= full 0)))
        ;; and the output is actually there, in the right cell
        (should (string-match-p "one" (buffer-string)))
        (should (emjupy-cell-output-ov c1))
        (should-not (emjupy-cell-output-ov c2))
        ;; the second cell is still intact and still its own overlay
        (should (string-match-p "b = 2" (buffer-string)))
        (should (overlayp (emjupy-cell-overlay c2)))))))

(ert-deftest emjupy-test-undo-survives-output-from-another-cell ()
  "Typing in one cell survives output arriving in another, undoably.

This is the case that matters: you run a long cell, move down to edit
another while it works, and its output lands.  The edits worth keeping
are precisely the ones the output has pushed further down the buffer,
so keeping them means shifting the positions inside the undo entries
rather than merely keeping the ones that did not move."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "a = 1"
                              :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                              :source "b = 2"
                              :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        ;; type into the SECOND cell
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (end-of-line)
        (insert " + 99")
        (undo-boundary)
        (should (string-match-p "b = 2 \\+ 99" (buffer-string)))
        ;; output lands in the FIRST cell, above it
        (emjupy--append-output-to-cell c1 (emjupy-test--stream "one\n") nb)
        (emjupy-flush-output buf)
        (should (string-match-p "one" (buffer-string)))
        (should (string-match-p "b = 2 \\+ 99" (buffer-string)))
        ;; and the typing can still be undone -- taking back exactly what
        ;; was typed, and nothing else
        (undo)
        (should-not (string-match-p "\\+ 99" (buffer-string)))
        (should (string-match-p "b = 2" (buffer-string)))
        (should (string-match-p "a = 1" (buffer-string)))
        (should (string-match-p "one" (buffer-string)))))))

(ert-deftest emjupy-test-executing-markdown-renders-it ()
  "Running a markdown cell renders it; no code is sent to the kernel.

Jupyter treats running a markdown cell as rendering it.  emjupy sent its
prose to Python, which answered with a syntax error about text that was
never code."
  (let ((md (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown
                              :source "# Title\n\n$x^2$" :outputs []
                              :metadata (make-hash-table)))
        (sent nil))
    (emjupy-test--with-notebook (vector md) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay md)))
        (cl-letf (((symbol-function 'emjupy--make-execute-request)
                   (lambda (&rest _) (setq sent t) (cons "id" "{}")))
                  ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t)))
          (emjupy-execute-cell-at-point))
        (should-not sent)
        ;; and the cell is untouched otherwise
        (should (eq (emjupy-cell-type md) 'markdown))
        (should (string-match-p "Title" (emjupy-cell-source md)))))))

;;;; Performance

(ert-deftest emjupy-test-output-redraw-does-not-scale-with-the-notebook ()
  "Output arriving redraws its own cell, so its cost does not grow with
the notebook.  Rebuilding everything for one cell's output is what made
a progress bar unusable."
  :tags '(:timing)
  (let* ((small (emjupy-test--big-notebook 5 5))
         (large (emjupy-test--big-notebook 80 20))
         (time-for
          (lambda (cells)
            (let ((elapsed nil))
              (emjupy-test--with-notebook cells buf nb
                (with-current-buffer buf
                  (let ((cell (aref (emjupy-notebook-cells nb) 0)))
                    (setq elapsed
                          (/ (emjupy-test--elapsed
                               (dotimes (i 20)
                                 (emjupy--append-output-to-cell
                                  cell (emjupy-test--stream-output (format "n %d\n" i)) nb)
                                 (emjupy-flush-output buf)))
                             20.0)))))
              elapsed))))
    (let ((small-time (funcall time-for small))
          (large-time (funcall time-for large)))
      ;; sixteen times the notebook must not cost anything like sixteen
      ;; times as much; allow a wide margin and still catch a rebuild
      (should (< large-time (max 0.05 (* 6 (max small-time 0.0005))))))))

(ert-deftest emjupy-test-nothing-remote-on-the-interactive-path ()
  "No path that runs after a command may touch a remote file.

Remote file operations cost a round trip each, and TRAMP blocks Emacs
while it waits -- which is how output once appeared to stall until the
user interrupted.  This asserts the absence rather than the speed,
because the speed depends on a network that a test cannot rely on."
  (let ((cells (emjupy-test--big-notebook 10 5))
        (remote-calls nil))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel-cwd emjupy--buffer-notebook)
              "/ssh:box:/srv/nb")
        (goto-char (overlay-start (emjupy-cell-overlay
                                   (aref (emjupy-notebook-cells nb) 3))))
        (cl-letf* ((real (symbol-function 'file-remote-p))
                   ;; Probing a remote path is itself a round trip: asking
                   ;; whether a TRAMP directory exists opens a connection to
                   ;; find out.  Counted alongside the writes.
                   (real-dirp (symbol-function 'file-directory-p))
                   (real-existsp (symbol-function 'file-exists-p))
                   ((symbol-function 'file-directory-p)
                    (lambda (dir &rest args)
                      (if (and (stringp dir) (funcall real dir))
                          (progn (push (list 'file-directory-p dir)
                                       remote-calls) nil)
                        (apply real-dirp dir args))))
                   ((symbol-function 'file-exists-p)
                    (lambda (file &rest args)
                      (if (and (stringp file) (funcall real file))
                          (progn (push (list 'file-exists-p file)
                                       remote-calls) nil)
                        (apply real-existsp file args))))
                   ((symbol-function 'make-directory)
                    (lambda (dir &rest _)
                      (when (funcall real dir)
                        (push (list 'make-directory dir) remote-calls))))
                   ((symbol-function 'write-region)
                    (lambda (_s _e file &rest _)
                      (when (and (stringp file) (funcall real file))
                        (push (list 'write-region file) remote-calls))))
                   ((symbol-function 'find-file-noselect)
                    (lambda (file &rest _)
                      (when (and (stringp file) (funcall real file))
                        (push (list 'find-file file) remote-calls))
                      (generate-new-buffer " *stand-in*"))))
          ;; what happens on every command; not in `ignore-errors': a TRAMP call
          ;; here is the fault tested for, and swallowing it would pass a path
          ;; opening connections
          (dotimes (_ 5)
            (run-hooks 'pre-command-hook)
            (emjupy--eldoc #'ignore)
            (emjupy--capf)
            (forward-line 1))
          ;; and output arriving
          (emjupy--append-output-to-cell
           (aref (emjupy-notebook-cells nb) 0)
           (emjupy-test--stream-output "out\n") nb)
          (emjupy-flush-output buf))
        (should (equal remote-calls nil))))))

;; deliberately no kernel directory: the warning must not wait for
;; one, since a local server starts before the kernel answers
(ert-deftest emjupy-test-running-a-cell-keeps-undo ()
  "Running a cell does not throw away the undo history.

Two full redraws happened on that path -- one clearing the cell's old
output before the run, one updating its execution count after -- and a
full redraw drops the history, because undo entries hold plain buffer
positions and a rebuild moves all of them.  Both changes are confined to
one cell, so both are done in place.

Running a cell is the commonest action in a notebook, so this was undo
being unusable in practice however well it survived everything else."
  (let ((c1 (emjupy-test--cell-like 'code "print(1)"))
        (c2 (emjupy-test--cell-like 'code "y = 2")))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        ;; type into the second cell
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (end-of-line)
        (insert " + 7")
        (undo-boundary)
        (should buffer-undo-list)
        ;; run the first, as far as a test without a kernel can
        (goto-char (overlay-start (emjupy-cell-overlay c1)))
        (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                  ((symbol-function 'emjupy--make-execute-request)
                   (lambda (&rest _) (cons "id" "{}")))
                  ((symbol-function 'emjupy--send-ws-text) #'ignore))
          (ignore-errors (emjupy-execute-cell-at-point)))
        ;; the history is still there, and still works
        (should buffer-undo-list)
        (should (string-match-p "y = 2 \\+ 7" (buffer-string)))
        (goto-char (point-min))
        (undo)
        (should-not (string-match-p "\\+ 7" (buffer-string)))
        (should (string-match-p "y = 2" (buffer-string)))))))

;;;; The undo guard

(ert-deftest emjupy-test-tqdm-bar-with-prints-is-one-bar ()
  "A progress bar with a print in the loop stays one bar, redrawn in place.

The messages are the ones ipykernel sends for tqdm over three items with
a print each, captured from a real kernel.  Each update of the bar is on
stderr and begins with a carriage return; the prints are on stdout and
fall between them, so the updates were never consecutive, never merged,
and each became an output of its own -- a column of bars, every one
showing its carriage return as a literal ^M."
  (let ((cell (make-emjupy-cell :id "t" :type 'code :source "" :outputs []
                                :metadata (make-hash-table :test 'equal))))
    (dolist (m '(("stderr" "\r0it [00:00, ?it/s]")
                ("stdout" "0 a/x\n")
                ("stderr" "\r1it [00:00,  3.33it/s]")
                ("stdout" "1 b/y\n")
                ("stderr" "\r2it [00:00,  3.33it/s]")
                ("stdout" "2 c/z\n")
                ("stderr" "\r3it [00:00,  3.32it/s]")
                ("stderr" "\r3it [00:00,  3.32it/s]")
                ("stderr" "\n")))
      (emjupy--append-output-to-cell cell (emjupy-test--stream-on (car m)
                                                                  (cadr m))))
    (let ((outs (append (emjupy-cell-outputs cell) nil)))
      ;; two outputs, not nine: the bar, and the prints
      (should (= (length outs) 2))
      ;; the bar shows only its final state, where it first appeared
      (should (equal (gethash "name" (nth 0 outs)) "stderr"))
      (should (equal (gethash "text" (nth 0 outs)) "3it [00:00,  3.32it/s]\n"))
      ;; the prints, in order and together
      (should (equal (gethash "text" (nth 1 outs)) "0 a/x\n1 b/y\n2 c/z\n"))
      ;; and no carriage return survives anywhere
      (dolist (o outs)
        (should-not (string-match-p "\r" (gethash "text" o)))))))

(ert-deftest emjupy-test-closed-stream-lines-keep-their-order ()
  "Output that genuinely came later is not moved above what came before.

Only a line still open -- no newline yet -- is continued across another
stream's output.  A warning that ends its line is finished, and the
next warning is a new line after whatever came in between."
  (let ((cell (make-emjupy-cell :id "o" :type 'code :source "" :outputs []
                                :metadata (make-hash-table :test 'equal))))
    (dolist (m '(("stderr" "warning one\n") ("stdout" "a\n")
                 ("stderr" "warning two\n")))
      (emjupy--append-output-to-cell cell (emjupy-test--stream-on (car m)
                                                                  (cadr m))))
    (should (equal (mapcar (lambda (o) (list (gethash "name" o)
                                             (gethash "text" o)))
                           (append (emjupy-cell-outputs cell) nil))
                   '(("stderr" "warning one\n")
                     ("stdout" "a\n")
                     ("stderr" "warning two\n"))))))

(ert-deftest emjupy-test-kernel-is-told-the-width ()
  "The kernel's COLUMNS is set to the width of the box."
  (let ((sent nil)
        (cell (emjupy-test--cell-like 'code "x")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel nb) (make-emjupy-kernel :id "k"))
        (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                  ((symbol-function 'emjupy--kernel-eval)
                   (lambda (_k code _cb) (setq sent code))))
          (emjupy--tell-kernel-width nb)
          (should (string-match-p (format "COLUMNS.*%d"
                                          (emjupy--box-width)) sent))
          (emjupy--tell-kernel-width nb 42)
          (should (string-match-p "COLUMNS.*42" sent)))))))

(ert-deftest emjupy-test-width-told-once-per-width ()
  "The kernel is told the width before a run, once for each width it has.

Sent ahead of the execute request, on the channel the kernel handles in
order, so the cell runs with it set -- not from a timer after connecting,
which lost the race to a cell run straight away."
  (let ((sent 0) (width 80)
        (cell (emjupy-test--cell-like 'code "x")))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel nb) (make-emjupy-kernel :id "k"))
        (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                  ((symbol-function 'emjupy--box-width) (lambda () width))
                  ((symbol-function 'emjupy--kernel-eval)
                   (lambda (&rest _) (setq sent (1+ sent)))))
          (emjupy--ensure-kernel-width nb)
          (emjupy--ensure-kernel-width nb)
          (should (= sent 1))               ; the same width is not sent twice
          (setq width 60)
          (emjupy--ensure-kernel-width nb)
          (should (= sent 2)))))))          ; a new one is

(ert-deftest emjupy-test-running-a-cell-does-not-redraw-the-notebook ()
  "Running a cell clears its old output in place, not by a whole redraw.

Only that cell's output changes.  A whole redraw moved every position
held in the buffer to its start -- the fake cursors of multiple-cursors,
markers, other windows' points -- and recorded an undo step for a
command that edits nothing."
  (let* ((out (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" "stream" o) (puthash "name" "stdout" o)
                (puthash "text" "old output\nover two lines\n" o) o))
         (c1 (emjupy-test--cell-like 'code "x1 = 1"))
         (c2 (emjupy-test--cell-like 'code "y = 2" (vector out)))
         (rebuilds 0))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-kernel nb)
              (make-emjupy-kernel :id "k"
                                  :pending (make-hash-table :test 'equal)))
        ;; something holding a position in the other cell
        (let ((mark (copy-marker (+ 2
                                    (overlay-start (emjupy-cell-overlay c1))))))
          (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                    ((symbol-function 'emjupy--ws-send) #'ignore)
                    ((symbol-function 'emjupy--kernel-eval) #'ignore)
                    ((symbol-function 'emjupy--redraw-notebook-1)
                     (let ((real (symbol-function
                                  'emjupy--redraw-notebook-1)))
                       (lambda (&rest args) (setq rebuilds (1+ rebuilds))
                         (apply real args)))))
            (goto-char (overlay-start (emjupy-cell-overlay c2)))
            (emjupy-execute-cell-at-point))
          (should (= rebuilds 0))
          ;; the old output is gone from the buffer
          (should-not (string-match-p "old output" (buffer-string)))
          ;; and the marker still sits where it was, in cell 1's text
          (should (equal (buffer-substring-no-properties
                          (overlay-start (emjupy-cell-overlay c1)) mark)
                         "x1")))))))

(ert-deftest emjupy-test-internal-answer-in-pieces-arrives-whole ()
  "An answer to one of emjupy's own questions arrives whole, however split.

The callback was given the first stream message and the request was then
forgotten, so a large answer -- plotly.js, asked of the kernel, is near
5 MB -- lost everything after its first piece."
  (let ((got nil)
        (kernel (make-emjupy-kernel :id "k" :name "python3"
                                    :pending (make-hash-table :test 'equal))))
    (clrhash emjupy--internal-requests)
    (puthash "big" (cons (lambda (out) (setq got out)) nil)
             emjupy--internal-requests)
    (dolist (piece '("first " "second " "third\n"))
      (emjupy--handle-ws-message
       kernel (emjupy-test--kernel-msg "stream" "big"
                                       `(("name" .
                                          "stdout") ("text" . ,piece)))))
    ;; nothing until the request is done
    (should-not got)
    (emjupy--handle-ws-message
     kernel (emjupy-test--kernel-msg "status" "big" '(("execution_state" .
                                                       "idle"))))
    (should (equal got "first second third\n"))
    (should-not (gethash "big" emjupy--internal-requests))))

(ert-deftest emjupy-test-binary-frames-as-the-server-writes-them ()
  "A binary frame is read as the Jupyter server writes it.
The frame below is what jupyter_server's serialize_binary_message made
of a message with two buffers; the buffers come back byte for byte."
  (let* ((frame (emjupy-test--unhex "0000000300000010000000f0000000f67b22686561646572223a207b226d73675f74797065223a2022636f6d6d5f6d7367222c20226d73675f6964223a20226d31227d2c2022706172656e745f686561646572223a207b7d2c20226d65746164617461223a207b7d2c20226368616e6e656c223a2022696f707562222c2022636f6e74656e74223a207b22636f6d6d5f6964223a20226331222c202264617461223a207b226d6574686f64223a2022757064617465222c20227374617465223a207b7d2c20226275666665725f7061746873223a205b5b2276616c7565225d2c205b2270616972222c20315d5d7d7d7d89504e4700ff4142"))
         (parts (emjupy--decode-binary-message frame))
         (msg (json-parse-string (car parts) :object-type 'hash-table)))
    (should (equal (gethash "msg_id" (gethash "header" msg)) "m1"))
    (should (equal (cdr parts) (list (unibyte-string #x89 ?P ?N ?G 0 #xff)
                                     "AB")))
    ;; and what is encoded here reads back the same
    (let ((again (emjupy--decode-binary-message
                  (emjupy--encode-binary-message (car parts) (cdr parts)))))
      (should (equal again parts)))
    ;; a message with no buffers at all
    (should (equal (emjupy--decode-binary-message
                    (emjupy--encode-binary-message "{}" nil))
                   (cons "{}" nil)))))

(ert-deftest emjupy-test-settling-adopted-runs-spares-newer-ones ()
  "After a reconnect, only the runs adopted are settled, not those sent since.
A run sent just after reconnecting can finish on the kernel, which then
reports idle, with its output still on the way; ending it lost that
output."
  (let* ((buf (generate-new-buffer " *emjupy-settle*"))
         (adopted (make-emjupy-cell :id "old"))
         (newer (make-emjupy-cell :id "new"))
         (nb (make-emjupy-notebook :buffer buf :path "n.ipynb"))
         (pending (make-hash-table :test 'equal))
         (kernel (make-emjupy-kernel :id "k" :notebook nb :pending pending))
         (ended nil))
    (puthash "req-old" adopted pending)
    (puthash "req-new" newer pending)
    (unwind-protect
        (cl-letf (((symbol-function 'emjupy--http-request)
                   (lambda (&rest _)
                     (let ((h (make-hash-table :test 'equal)))
                       (puthash "execution_state" "idle" h)
                       h)))
                  ((symbol-function 'emjupy--end-runs)
                   (lambda (cells &rest _) (setq ended cells))))
          (emjupy--settle-adopted-runs kernel '("req-old"))
          (should (equal ended (list adopted)))
          (should-not (gethash "req-old" pending))
          (should (eq (gethash "req-new" pending) newer)))
      (kill-buffer buf))))

(ert-deftest emjupy-test-stacked-progress-bars-are-shown-as-a-terminal-shows-them ()
  "Cursor codes in stream output are applied, chunk by chunk, as a terminal would.
Stacked progress bars -- tqdm nested, or with `position' -- move the
cursor up a line to redraw the bar above.  Only carriage returns were
applied, and stale bars piled up."
  (dolist (case emjupy-test--terminal-cases)
    (let ((out (make-hash-table :test 'equal)))
      (puthash "output_type" "stream" out) (puthash "name" "stderr" out)
      (emjupy--stream-write out "" (car (nth 1 case)))
      (dolist (chunk (cdr (nth 1 case)))
        (emjupy--stream-write out (gethash "text" out) chunk))
      (let* ((text (replace-regexp-in-string "\e\\[[0-9;]*m" ""
                                             (gethash "text" out)))
             (lines (split-string text "\n")))
        (while (and lines (string-empty-p (car (last lines))))
          (setq lines (butlast lines)))
        (ert-info ((car case))
          (should (equal lines (nth 2 case)))
          ;; where the cursor is, is the terminal's: never saved with the output
          (should (equal (sort (hash-table-keys out) #'string<)
                         '("name" "output_type" "text"))))))))

;;;; Running a cell while reconnecting

(defmacro emjupy-test--reconnecting (live-var sent-var &rest body)
  "Run BODY in a notebook whose connection is being restored.
LIVE-VAR says whether the connection is back; SENT-VAR collects what is
sent to the kernel."
  (declare (indent 2))
  `(emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                         :source "x = 1"))
       buf nb
     (let ((,live-var nil) (,sent-var nil))
       (setf (emjupy-notebook-server nb)
             (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
       (setf (emjupy-notebook-kernel nb)
             (make-emjupy-kernel :id "k" :notebook nb
                                 :pending (make-hash-table :test 'equal)))
       (setq emjupy--reconnect-timer (run-at-time 3600 nil #'ignore))
       (unwind-protect
           (cl-letf (((symbol-function 'emjupy--ws-live-p)
                      (lambda (&rest _) ,live-var))
                     ((symbol-function 'emjupy--ws-send)
                      (lambda (payload &rest _) (push payload ,sent-var)))
                     ((symbol-function 'emjupy--ensure-kernel-width) #'ignore)
                     ((symbol-function 'emjupy--mark-running) #'ignore))
             (emjupy--goto-cell (aref (emjupy-notebook-cells nb) 0) 'start)
             ,@body)
         (when (timerp emjupy--reconnect-timer)
           (cancel-timer emjupy--reconnect-timer))
         (setq emjupy--reconnect-timer nil)))))

(ert-deftest emjupy-test-a-cell-run-while-reconnecting-runs-once-back ()
  "A cell run while the connection is restored runs when it is back, once.
It said the notebook had no kernel, and to start one -- which would have
left the kernel being reconnected to, and everything defined in it."
  (emjupy-test--reconnecting live sent
    (emjupy-execute-cell-at-point)
    (emjupy-execute-cell-at-point)
    (should (= (length emjupy--runs-waiting) 1))
    (should-not sent)
    (setq live t)
    (emjupy--try-reconnect (current-buffer) "k" "python3"
                           (+ (float-time) 30) "nb")
    (should (= (length sent) 1))
    (should-not emjupy--runs-waiting)))

(ert-deftest emjupy-test-a-cell-run-while-reconnecting-is-dropped-if-not ()
  "A cell waiting for a connection that does not come back does not run."
  (emjupy-test--reconnecting live sent
    (emjupy-execute-cell-at-point)
    (should (= (length emjupy--runs-waiting) 1))
    (emjupy--try-reconnect (current-buffer) "k" "python3"
                           (- (float-time) 1) "nb")
    (should-not sent)
    (should-not emjupy--runs-waiting)))

(ert-deftest emjupy-test-reconnecting-restores-the-way-first ()
  "Each attempt to reconnect first asks for the way to the server back.
`emjupy-reconnect-functions' get the server, then the connection is tried:
a tunnel reopened there is what the attempt connects through."
  (emjupy-test--reconnecting live sent
    (ignore sent)
    (let ((order nil) (server (emjupy-notebook-server (emjupy--notebook))))
      (cl-letf (((symbol-function 'emjupy-connect-kernel)
                 (lambda (&rest _) (push 'connect order))))
        (let ((emjupy-reconnect-functions
               (list (lambda (s) (push (list 'restore (eq s server)) order)))))
          (emjupy--try-reconnect (current-buffer) "k" "python3"
                                 (+ (float-time) 30) "nb")))
      (should (equal (reverse order) '((restore t) connect))))))

(ert-deftest emjupy-test-reconnecting-by-hand-keeps-trying ()
  "C-c C-x C-c that cannot connect at once keeps trying, as after a drop.
It failed with a raw \"Connection refused\" -- the way to the server, a
tunnel, needing a moment to come back."
  (emjupy-test--reconnecting live sent
    (ignore sent)
    (cancel-timer emjupy--reconnect-timer)
    (setq emjupy--reconnect-timer nil)
    (let ((asked nil))
      (cl-letf (((symbol-function 'emjupy-connect-kernel)
                 (lambda (&rest _) (error "Connection refused")))
                ((symbol-function 'emjupy--disconnect-kernel) #'ignore)
                ((symbol-function 'emjupy--repair-if-scrambled) #'ignore))
        (let ((emjupy-reconnect-functions (list (lambda (_) (setq asked t)))))
          (emjupy-reconnect-kernel)))
      (should asked)
      (should (timerp emjupy--reconnect-timer)))))

(ert-deftest emjupy-test-a-cell-on-a-dropped-connection-says-so ()
  "Running a cell whose kernel's connection is down says so, not \"no kernel\".
The kernel is there; \\[emjupy-reconnect-kernel] reaches it again."
  (emjupy-test--reconnecting live sent
    (ignore sent)
    (cancel-timer emjupy--reconnect-timer)
    (setq emjupy--reconnect-timer nil)
    (let ((err (should-error (emjupy-execute-cell-at-point) :type 'user-error)))
      ;; ' or ', as `format-message' quotes it in the locale
      (should (string-match-p "connection to this notebook.s kernel is down"
                              (cadr err))))))

(ert-deftest emjupy-test-a-kernel-gone-from-the-server-stops-reconnecting ()
  "A kernel the server no longer has is not reconnected to for 30 seconds.
Asked during the reconnect, the server answers 404: the reconnecting
stops at once, the cells waiting do not run, and the notebook is left
without a kernel, for running a cell to say so."
  (emjupy-test--reconnecting live sent
    (emjupy-execute-cell-at-point)
    (should emjupy--runs-waiting)
    (cl-letf (((symbol-function 'emjupy-connect-kernel) #'ignore)
              ((symbol-function 'emjupy--http-request-async)
               (lambda (_m _s _p _b _done fail)
                 (funcall fail '(emjupy-http-status "[Jupyter HTTP 404] GET"
                                                    "{}")))))
      (emjupy--try-reconnect (current-buffer) "k" "python3"
                             (+ (float-time) 30) "nb"))
    (should-not emjupy--reconnect-timer)
    (should-not emjupy--runs-waiting)
    (should-not (emjupy-notebook-kernel (emjupy--notebook)))
    (should-not sent)))

(ert-deftest emjupy-test-a-silent-server-is-said-when-reconnecting-ends ()
  "Giving up on a server that never answered says it may have stopped.
\"C-c C-x C-c tries again\" was all it said, and tried again for nothing."
  (emjupy-test--reconnecting live sent
    (ignore sent)
    (let ((said nil))
      (cl-letf (((symbol-function 'emjupy-connect-kernel) #'ignore)
                ((symbol-function 'emjupy--http-request-async)
                 (lambda (_m _s _p _b _done fail)
                   (funcall fail '(emjupy-http-error "Network error: no answer from"
                                                     "127.0.0.1:1"))))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format-message fmt args) said))))
        (emjupy--try-reconnect (current-buffer) "k" "python3"
                               (+ (float-time) 30) "nb")
        (emjupy--try-reconnect (current-buffer) "k" "python3"
                               (- (float-time) 1) "nb"))
      (should (seq-some (lambda (m) (string-match-p "does not answer" m))
                        said)))))

;;;; A kernel quiet while running a cell

(defmacro emjupy-test--running-quietly (quiet answer &rest body)
  "Run BODY with a cell running on a kernel heard from QUIET seconds ago.
ANSWER is how the server answers being asked after the kernel: `gone'
or `there'.  ASKED, in BODY, says whether it was asked."
  (declare (indent 2))
  `(emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                         :source "x = 1"))
       buf nb
     (let* ((cell (aref (emjupy-notebook-cells nb) 0))
            (pending (make-hash-table :test 'equal))
            (kernel (make-emjupy-kernel :id "k" :notebook nb :pending pending
                                        :heard (- (float-time) ,quiet)))
            (asked nil) (emjupy--quiet-check-timer nil))
       (puthash "m1" cell pending)
       (setf (emjupy-notebook-server nb)
             (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
       (setf (emjupy-notebook-kernel nb) kernel)
       (cl-letf (((symbol-function 'emjupy--notebook-buffers)
                  (lambda () (list buf)))
                 ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                 ((symbol-function 'emjupy--disconnect-kernel) #'ignore)
                 ((symbol-function 'emjupy--http-request-async)
                  (lambda (_m _s _p _b done fail)
                    (setq asked t)
                    (if (eq ,answer 'gone)
                        (funcall fail '(emjupy-http-status
                                        "[Jupyter HTTP 404] GET" "{}"))
                      (funcall done nil)))))
         ,@body))))

(ert-deftest emjupy-test-a-quiet-kernel-gone-ends-its-cell ()
  "A kernel quiet past the interval, gone from the server, ends its cell.
Shut down or culled while its connection stayed open, it left the cell
showing as running for good."
  (let ((emjupy-kernel-check-interval 30))
    (emjupy-test--running-quietly 40 'gone
      (emjupy--check-quiet-kernels)
      (should asked)
      (should-not (emjupy-notebook-kernel nb))
      (should (= (hash-table-count pending) 0)))))

(ert-deftest emjupy-test-a-kernel-heard-lately-is-not-asked-after ()
  "A kernel heard from within the interval is not asked after."
  (let ((emjupy-kernel-check-interval 30))
    (emjupy-test--running-quietly 5 'gone
      (emjupy--check-quiet-kernels)
      (should-not asked)
      (should (emjupy-notebook-kernel nb)))))

(ert-deftest emjupy-test-no-kernel-is-asked-after-with-the-check-off ()
  "`emjupy-kernel-check-interval' nil asks after none, and stops its timer."
  (let ((emjupy-kernel-check-interval nil))
    (emjupy-test--running-quietly 600 'gone
      (setq emjupy--quiet-check-timer (run-at-time 3600 nil #'ignore))
      (emjupy--check-quiet-kernels)
      (should-not asked)
      (should-not emjupy--quiet-check-timer))))

(ert-deftest emjupy-test-a-refused-header-puts-the-token-in-the-address ()
  "A WebSocket refused, 403, with the token in a header alone is noted.
Its server then gets the token in the address too, from the next attempt
on; any other error is only reported."
  (let ((on-error nil) (emjupy--token-in-ws-address nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq on-error (plist-get args :on-error)) nil))
              ((symbol-function 'message) #'ignore))
      (let* ((server (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
             (nb (make-emjupy-notebook :path "a.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (funcall on-error nil 'on-open '(websocket-invalid-header "x"))
        (should-not emjupy--token-in-ws-address)
        (funcall on-error nil 'on-open
                 '(websocket-received-error-http-response 403))
        (should (equal emjupy--token-in-ws-address '("127.0.0.1:1")))))))

(ert-deftest emjupy-test-a-cell-run-here-is-trusted ()
  "A cell run is trusted from then on, as Jupyter marks a cell the user runs.
Saved with every cell trusted, the server signs the notebook."
  (emjupy-test--reconnecting live sent
    (setq live t)
    (let ((cell (aref (emjupy-notebook-cells (emjupy--notebook)) 0)))
      (setf (emjupy-cell-metadata cell)
            (let ((m (make-hash-table :test 'equal)))
              (puthash "trusted" :false m)
              m))
      (should-not (emjupy--cell-trusted-p cell))
      (emjupy-execute-cell-at-point)
      (should sent)
      (should (emjupy--cell-trusted-p cell)))))

(ert-deftest emjupy-test-a-refused-handshake-is-never-open ()
  "A WebSocket the server refuses -- a kernel gone, a server restarted -- is
never taken as open.  websocket.el, the handshake refused, still called
`on-open', and read the server's error page as messages: emjupy said
\"connected\", and errors followed."
  (let* ((handlers nil) (handled nil) (said nil)
         (conn (start-process "emjupy-test-conn" nil "sleep" "30"))
         (ws (websocket-inner-create :conn conn :url "ws://x"
                                     :ready-state 'connecting)))
    (unwind-protect
        (cl-letf (((symbol-function 'websocket-open)
                   (lambda (_url &rest args) (setq handlers args) ws))
                  ((symbol-function 'emjupy--handle-ws-message)
                   (lambda (&rest _) (setq handled t)))
                  ((symbol-function 'message)
                   (lambda (fmt &rest args)
                     (push (apply #'format fmt args) said))))
          (let* ((server (make-emjupy-server :base-url "127.0.0.1:1"
                                            :token "t"))
                 (nb (make-emjupy-notebook :path "a.ipynb" :server server)))
            (emjupy-connect-kernel nb "gone-kernel")
            (funcall (plist-get handlers :on-error) ws 'on-open
                     '(websocket-received-error-http-response 404))
            (funcall (plist-get handlers :on-open) ws)
            (funcall (plist-get handlers :on-message) ws "<html>404</html>")
            (should (eq (websocket-client-data ws) 'refused))
            (should-not (process-live-p conn))
            (should-not handled)
            (should-not (seq-some (lambda (m) (string-match-p "connected to kernel" m))
                                  said))))
      (when (process-live-p conn) (delete-process conn)))))

(ert-deftest emjupy-test-adopted-runs-are-not-ended-while-the-kernel-works ()
  "Runs adopted after a drop are not ended while their kernel is heard from.
A kernel working through queued cells is idle a moment between two: one
look at it then ended every run still queued, and their output, coming
after, was dropped -- a long notebook cut off mid-run lost its last
cells.  Quiet and idle, the runs end."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                        :source "x"))
      buf nb
    (let* ((pending (make-hash-table :test 'equal))
           (kernel (make-emjupy-kernel :id "k" :notebook nb :pending pending))
           (later nil) (ended nil))
      (puthash "m1" (aref (emjupy-notebook-cells nb) 0) pending)
      (setf (emjupy-notebook-kernel nb) kernel
            (emjupy-notebook-server nb) (make-emjupy-server :base-url "127.0.0.1:1"))
      (cl-letf (((symbol-function 'emjupy--http-request)
                 (lambda (&rest _) (let ((h (make-hash-table :test 'equal)))
                                     (puthash "execution_state" "idle" h) h)))
                ((symbol-function 'run-with-timer)
                 (lambda (&rest _) (setq later t)))
                ((symbol-function 'emjupy--end-runs)
                 (lambda (&rest _) (setq ended t))))
        (setf (emjupy-kernel-heard kernel) (float-time))
        (emjupy--settle-adopted-runs kernel '("m1"))
        (should later)
        (should-not ended)
        (should (gethash "m1" pending))
        (setf (emjupy-kernel-heard kernel) (- (float-time) 30))
        (emjupy--settle-adopted-runs kernel '("m1"))
        (should ended)
        (should-not (gethash "m1" pending))))))

;;;; Running several cells

(defmacro emjupy-test--five-cells (&rest body)
  "Run BODY in a notebook of five cells, the second raw.
RAN, in BODY, lists the ids of the cells run, in order."
  (declare (indent 0))
  `(emjupy-test--with-notebook
       (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1")
               (make-emjupy-cell :id 2 :type 'raw :source "raw")
               (make-emjupy-cell :id 3 :type 'markdown :source "# m")
               (make-emjupy-cell :id 4 :type 'code :source "b = 2")
               (make-emjupy-cell :id 5 :type 'code :source "c = 3"))
       buf nb
     (let ((ran nil))
       (cl-letf (((symbol-function 'emjupy-execute-cell-at-point)
                  (lambda ()
                    (push (emjupy-cell-id (emjupy--cell-at-point)) ran))))
         (cl-flet ((at (id) (emjupy--goto-cell
                             (cl-find id (emjupy-notebook-cells nb)
                                      :key #'emjupy-cell-id)
                             'start))
                   (ran () (reverse ran)))
           ,@body)))))

(ert-deftest emjupy-test-run-all-above-and-below ()
  "Run all, all above, all below: as JupyterLab, raw cells left alone.
Above leaves out the cell at point; below starts with it; point stays."
  (emjupy-test--five-cells
    (at 4)
    (let ((here (point)))
      (emjupy-run-all-cells)
      (should (equal (ran) '(1 3 4 5)))
      (should (= (point) here)))
    (setq ran nil) (at 4) (emjupy-run-cells-above)
    (should (equal (ran) '(1 3)))
    (setq ran nil) (at 4) (emjupy-run-cells-below)
    (should (equal (ran) '(4 5)))))

(ert-deftest emjupy-test-a-prefix-runs-that-many-cells ()
  "C-u N C-c C-c runs N cells, each moving on to the next.
Past the end it stops there; reaching it exactly, it makes the new cell
running the last one always makes."
  (emjupy-test--five-cells
    (at 1)
    (emjupy-execute-cell-and-goto-next 2)
    (should (equal (ran) '(1 2)))
    (should (= (emjupy-cell-id (emjupy--cell-at-point)) 3))
    (setq ran nil) (at 1)
    (emjupy-execute-cell-and-goto-next 10)
    (should (equal (ran) '(1 2 3 4 5)))
    (should (= (length (emjupy-notebook-cells nb)) 5))
    (setq ran nil) (at 1)
    (emjupy-execute-cell-and-goto-next 5)
    (should (= (length (emjupy-notebook-cells nb)) 6))))

(defun emjupy-test--message (type date &optional state)
  "A message of TYPE from the kernel at DATE; status messages with STATE."
  (let ((header (make-hash-table :test 'equal))
        (content (make-hash-table :test 'equal))
        (data (make-hash-table :test 'equal)))
    (puthash "msg_type" type header)
    (puthash "date" date header)
    (when state (puthash "execution_state" state content))
    (puthash "header" header data)
    (puthash "content" content data)
    data))

(ert-deftest emjupy-test-a-run-is-timed-as-jupyterlab-does ()
  "Running a cell records the kernel's times under JupyterLab's keys.
A new run starts afresh; with the option off, nothing is written."
  (let ((cell (make-emjupy-cell :id 1 :type 'code))
        (emjupy-record-execution-time t))
    (dolist (m (list (emjupy-test--message "status" "T0" "busy")
                     (emjupy-test--message "execute_input" "T1")
                     (emjupy-test--message "execute_reply" "T2")))
      (emjupy--record-execution-time
       cell (gethash "msg_type" (gethash "header" m)) m))
    (let ((times (gethash "execution" (emjupy-cell-metadata cell))))
      (should (equal (gethash "iopub.status.busy" times) "T0"))
      (should (equal (gethash "iopub.execute_input" times) "T1"))
      (should (equal (gethash "shell.execute_reply" times) "T2")))
    (let ((m (emjupy-test--message "status" "T3" "busy")))
      (emjupy--record-execution-time cell "status" m))
    (should-not (gethash "shell.execute_reply"
                         (gethash "execution" (emjupy-cell-metadata cell))))
    (let ((other (make-emjupy-cell :id 2 :type 'code))
          (emjupy-record-execution-time nil)
          (m (emjupy-test--message "execute_input" "T1")))
      (cl-letf (((symbol-function 'emjupy--ws-parent-id) #'ignore))
        (ignore-errors (emjupy--apply-to-cell other nil m)))
      (should-not (emjupy-cell-metadata other)))))


;;;; Raw cells, regions, restarting, shutting down

(ert-deftest emjupy-test-a-raw-cell-is-not-run ()
  "A raw cell is not sent to the kernel, as in Jupyter."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'raw
                                                        :source "not code"))
      buf nb
    (let ((sent nil))
      (cl-letf (((symbol-function 'emjupy--ws-send)
                 (lambda (&rest _) (setq sent t))))
        (emjupy--goto-cell (aref (emjupy-notebook-cells nb) 0) 'beginning)
        (emjupy-execute-cell-at-point))
      (should-not sent))))

(ert-deftest emjupy-test-running-a-region-runs-its-cells ()
  "C-c C-c with a region runs every cell it touches, in order."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1")
              (make-emjupy-cell :id 2 :type 'code :source "b = 2")
              (make-emjupy-cell :id 3 :type 'code :source "c = 3"))
      buf nb
    (let ((ran nil) (transient-mark-mode t)
          (cells (emjupy-notebook-cells nb)))
      (cl-letf (((symbol-function 'emjupy--run-cells)
                 (lambda (cs) (setq ran (mapcar #'emjupy-cell-id cs)))))
        (emjupy--goto-cell (aref cells 0) 'beginning)
        (push-mark (point) t t)
        (emjupy--goto-cell (aref cells 1) 'end)
        (emjupy-execute-cell-and-goto-next))
      (should (equal ran '(1 2)))
      (should-not (use-region-p)))))

(ert-deftest emjupy-test-restart-and-run-all-or-clear ()
  "Restarting, then running all, waits for the kernel first; restarting,
then clearing, clears; each asks first."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                        :source "x"))
      buf nb
    (setf (emjupy-notebook-kernel nb) (make-emjupy-kernel :id "k"))
    (let ((did nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'emjupy--restart-kernel-1)
                 (lambda (_) (push 'restart did)))
                ((symbol-function 'emjupy--await-connection)
                 (lambda (_) (push 'await did)))
                ((symbol-function 'emjupy-run-all-cells)
                 (lambda () (push 'run-all did)))
                ((symbol-function 'emjupy-clear-all-outputs)
                 (lambda () (push 'clear did))))
        (emjupy-restart-kernel-and-run-all)
        (should (equal (reverse did) '(restart await run-all)))
        (setq did nil)
        (emjupy-restart-kernel-and-clear-outputs)
        (should (equal (reverse did) '(restart clear)))))))

(ert-deftest emjupy-test-shutting-the-kernel-down ()
  "Shutting down deletes the kernel on its server; the notebook has none."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                        :source "x"))
      buf nb
    (setf (emjupy-notebook-kernel nb) (make-emjupy-kernel :id "k1")
          (emjupy-notebook-server nb)
          (make-emjupy-server :base-url "127.0.0.1:1" :token "t"))
    (let ((asked nil))
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t))
                ((symbol-function 'emjupy--disconnect-kernel) #'ignore)
                ((symbol-function 'message) #'ignore)
                ((symbol-function 'emjupy--http-request)
                 (lambda (method _server path &rest _)
                   (setq asked (list method path)))))
        (emjupy-shutdown-kernel))
      (should (equal asked '("DELETE" "/api/kernels/k1")))
      (should-not (emjupy-notebook-kernel nb)))))


(ert-deftest emjupy-test-a-disconnect-stops-reconnecting ()
  "Closing a kernel's connection on purpose -- a restart, a shutdown --
stops a reconnecting an earlier drop began, and releases the cells held
for it.  Going on, the reconnecting replaced the connection made next,
and the mode line said reconnecting."
  (emjupy-test--with-notebook (vector (make-emjupy-cell :id 1 :type 'code
                                                        :source "x"))
      buf nb
    (let ((cell (aref (emjupy-notebook-cells nb) 0)))
      (setf (emjupy-notebook-kernel nb)
            (make-emjupy-kernel :id "k" :notebook nb
                                :pending (make-hash-table :test 'equal)))
      (setq emjupy--reconnect-timer (run-at-time 3600 nil #'ignore))
      (emjupy--mark-running cell)
      (setq emjupy--runs-waiting (list cell))
      (unwind-protect
          (progn
            (emjupy--disconnect-kernel nb)
            (should-not emjupy--reconnect-timer)
            (should-not emjupy--runs-waiting)
            (should-not (emjupy--cell-running-p cell)))
        (when (timerp emjupy--reconnect-timer)
          (cancel-timer emjupy--reconnect-timer))
        (emjupy--stop-spinner)))))


;;;; The kernel a notebook records

(defun emjupy-test--notebook-on (kernel-name)
  "A notebook recording KERNEL-NAME as its kernelspec, nil for none."
  (let ((meta (make-hash-table :test 'equal)))
    (when kernel-name
      (puthash "kernelspec" (emjupy--json-object "name" kernel-name) meta))
    (make-emjupy-notebook :path "a.ipynb" :metadata meta
                          :server (make-emjupy-server :base-url "127.0.0.1:1"
                                                      :token "t"))))

(ert-deftest emjupy-test-a-notebook-gets-the-kernel-it-records ()
  "The kernel started is the notebook's kernelspec, as in Jupyter; python3
for a notebook recording none.  It was python3 for every notebook."
  (should (equal (emjupy--notebook-kernel-name
                  (emjupy-test--notebook-on "py-a"))
                 "py-a"))
  (should (equal (emjupy--notebook-kernel-name (emjupy-test--notebook-on nil))
                 "python3"))
  (let ((asked nil) (connected nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (_method _server path &optional body &rest _)
                 (let ((kernel (gethash "kernel" (json-parse-string body))))
                   (push (cons path (gethash "name" kernel)) asked))
                 (emjupy--json-object "kernel"
                                      (emjupy--json-object "id" "k1"))))
              ((symbol-function 'emjupy-connect-kernel)
               (lambda (_nb id name) (setq connected (list id name))))
              ((symbol-function 'message) #'ignore))
      (emjupy--spawn-and-connect-kernel (emjupy-test--notebook-on "py-a")))
    (should (equal asked '(("/api/sessions" . "py-a"))))
    (should (equal connected '("k1" "py-a")))))

(ert-deftest emjupy-test-a-kernel-the-server-lacks-gives-way-to-python3 ()
  "A notebook recording a kernel the server cannot start gets python3, and
says so."
  (let ((asked nil) (said nil))
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (_method _server path &optional body &rest _)
                 (let* ((kernel (gethash "kernel" (json-parse-string body)))
                        (name (gethash "name" kernel)))
                   (push name asked)
                   (if (equal name "python3")
                       (emjupy--json-object "kernel"
                                            (emjupy--json-object "id" "k2"))
                     (signal 'emjupy-http-error (list "no such kernel"))))))
              ((symbol-function 'emjupy-connect-kernel) #'ignore)
              ((symbol-function 'message)
               (lambda (fmt &rest args) (push (apply #'format fmt args) said))))
      (emjupy--spawn-and-connect-kernel (emjupy-test--notebook-on "gone")))
    (should (equal (reverse asked) '("gone" "python3")))
    (should (seq-some (lambda (m) (string-match-p "asks for the kernel gone" m))
                      said))))


;;;; Work done once, not once per cell

(defmacro emjupy-test--running (&rest body)
  "Run BODY in four cells, the last markdown, on a stubbed live kernel.
SENT, in BODY, collects what is sent; (calls FN) counts calls to FN."
  (declare (indent 0))
  `(emjupy-test--with-notebook
       (vector (make-emjupy-cell :id 1 :type 'code :source "a = 1")
               (make-emjupy-cell :id 2 :type 'code :source "b = 2")
               (make-emjupy-cell :id 3 :type 'code :source "c = 3")
               (make-emjupy-cell :id 4 :type 'markdown :source "# m"))
       buf nb
     (let ((sent nil) (counts (make-hash-table)))
       (setf (emjupy-notebook-kernel nb)
             (make-emjupy-kernel :id "k" :notebook nb
                                 :pending (make-hash-table :test 'equal)))
       (cl-letf* (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
                  ((symbol-function 'emjupy--ws-send)
                   (lambda (payload &rest _) (push payload sent)))
                  (sync (symbol-function 'emjupy--sync-all-cells))
                  ((symbol-function 'emjupy--sync-all-cells)
                   (lambda ()
                     (cl-incf (gethash 'sync counts 0))
                     (funcall sync)))
                  (whole (symbol-function 'emjupy--redraw-notebook))
                  ((symbol-function 'emjupy--redraw-notebook)
                   (lambda (&rest args)
                     (cl-incf (gethash 'whole counts 0)) (apply whole args))))
         (cl-flet ((calls (what) (gethash what counts 0)))
           (unwind-protect (progn ,@body) (emjupy--stop-spinner)))))))

(ert-deftest emjupy-test-run-all-reads-the-cells-once ()
  "Running all cells syncs them once, not once per cell, and renders a
markdown cell in place, not by redrawing the notebook.  Synced per cell,
a notebook run whole was read once per cell: 0.8 s at 500 cells."
  (emjupy-test--running
    (emjupy-run-all-cells)
    ;; the three code cells; the width, told once, besides
    (should (= (cl-count-if-not (lambda (p) (string-match-p "COLUMNS" p)) sent)
               3))
    (should (= (calls 'sync) 1))
    (should (= (calls 'whole) 0))))

(ert-deftest emjupy-test-the-kernel-is-told-its-width-every-time ()
  "The width is sent each time, the same width included.
Spared, a fresh kernel's first output could be a user's cell's, and was
lost: the first a new connection hears may come too late for it."
  (emjupy-test--running
    (let ((told 0))
      (cl-letf (((symbol-function 'emjupy--kernel-eval)
                 (lambda (&rest _) (cl-incf told))))
        (emjupy--tell-kernel-width nb 80)
        (emjupy--tell-kernel-width nb 80)
        (should (= told 2))))))

(ert-deftest emjupy-test-emjupys-own-code-is-no-run-of-the-users ()
  "emjupy's own code on the kernel counts nothing and stops nothing.
It took an execution count: a restarted kernel, told its width, numbered
the user's first cell 2."
  (cl-flet ((content (req)
              (gethash "content" (json-parse-string (cdr req)))))
    (let ((own (content (emjupy--make-execute-request "x" :internal t)))
          (users (content (emjupy--make-execute-request "x"))))
      (should (eq (gethash "store_history" own) :false))
      (should (eq (gethash "stop_on_error" own) :false))
      (should (eq (gethash "store_history" users) t))
      (should (eq (gethash "stop_on_error" users) t)))))


;;;; A new connection hears the kernel before anything is sent

(defmacro emjupy-test--handshaking (&rest body)
  "Run BODY with KERNEL connected, SENT what went over its socket, in order."
  (declare (indent 0))
  `(let* ((kernel (make-emjupy-kernel :id "k" :ws 'ws
                                      :pending (make-hash-table :test 'equal)))
          (sent nil) (said nil) (timer nil))
     (cl-letf (((symbol-function 'websocket-send-text)
                (lambda (_ws payload) (setq sent (append sent (list payload)))))
               ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
               ((symbol-function 'run-with-timer)
                (lambda (_secs _rep fn &rest args) (setq timer (cons fn args))))
               ((symbol-function 'message)
                (lambda (fmt &rest args)
                  (push (apply #'format fmt args) said))))
       ,@body)))

(defun emjupy-test--status-for (parent-id)
  "A status frame answering PARENT-ID."
  (json-serialize
   (emjupy--json-object
    "header" (emjupy--json-object "msg_type" "status" "msg_id" "s1")
    "parent_header" (emjupy--json-object "msg_id" parent-id)
    "channel" "iopub"
    "content" (emjupy--json-object "execution_state" "idle")
    "metadata" (make-hash-table))))

(ert-deftest emjupy-test-a-new-connection-holds-until-it-hears ()
  "What is sent before the kernel's output is heard is held, then sent in
order, once a status answers the kernel_info_request asked first.  A
first cell's output was lost otherwise."
  (emjupy-test--handshaking
    (emjupy--begin-handshake kernel)
    (should (= (length sent) 1))
    (should (string-match-p "kernel_info_request" (car sent)))
    (emjupy--ws-send "first" kernel)
    (emjupy--ws-send "second" kernel)
    (should (= (length sent) 1))
    ;; a status for another request does not end it
    (emjupy--handle-ws-message kernel (emjupy-test--status-for "other"))
    (should (= (length sent) 1))
    (emjupy--handle-ws-message
     kernel (emjupy-test--status-for (emjupy-kernel-handshake kernel)))
    (should (equal (cdr sent) '("first" "second")))
    (should-not (emjupy-kernel-handshake kernel))
    ;; and after, sent at once
    (emjupy--ws-send "third" kernel)
    (should (equal (car (last sent)) "third"))))

(ert-deftest emjupy-test-a-connection-not-heard-goes-on-saying-so ()
  "No status in time: what was held is sent all the same, and said."
  (emjupy-test--handshaking
    (emjupy--begin-handshake kernel)
    (emjupy--ws-send "first" kernel)
    (apply (car timer) (cdr timer))
    (should (equal (cdr sent) '("first")))
    (should-not (emjupy-kernel-handshake kernel))
    (should (seq-some (lambda (m) (string-match-p "not heard" m)) said))))

(provide 'emjupy-kernel-test)

;;; emjupy-kernel-test.el ends here
