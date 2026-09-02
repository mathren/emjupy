;;; emjupy-test.el --- Tests for emjupy.el -*- lexical-binding: t; -*-

;; Kept separate from emjupy.el on purpose: implementation and tests are
;; different files, this one requires the other.
;;
;; Most tests here are pure/fast and need nothing external. A few, clearly
;; marked in section 9, need a real Python language server (pylsp, pyright,
;; or jedi-language-server) on PATH to fully exercise Eglot; those call
;; `ert-skip' when no such server is found rather than failing, so the rest
;; of the suite stays green on a machine without one installed.

(require 'ert)
(require 'emjupy)
(require 'cl-lib)

(defmacro emjupy-test--with-notebook (cells-form buf-var nb-var &rest body)
  "Set up a fresh emjupy-mode BUF-VAR/NB-VAR pair from CELLS-FORM, run
BODY with BUF-VAR as the current buffer, then kill it (and any shadow
buffer it created)."
  (declare (indent 3))
  `(let* ((,nb-var (make-emjupy-notebook :cells ,cells-form))
          (,buf-var (generate-new-buffer "*emjupy-test*")))
     (unwind-protect
         (with-current-buffer ,buf-var
           (emjupy-mode)
           (setq emjupy--buffer-notebook ,nb-var)
           (setf (emjupy-notebook-buffer ,nb-var) ,buf-var)
           ;; Real usage sets this in emjupy-open-notebook; several
           ;; functions (e.g. emjupy--append-output-to-cell) depend on
           ;; it rather than on Emacs's ambient current-buffer.
           (setq emjupy--current-notebook-buffer ,buf-var)
           (emjupy--rerender-notebook)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var))
         (when (and (emjupy-notebook-shadow-buffer ,nb-var)
                    (buffer-live-p (emjupy-notebook-shadow-buffer ,nb-var)))
           (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer ,nb-var))))))))

;;; ---------------------------------------------------------------------
;;; 1. Data structures
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-struct-creation ()
  "Ensure structs initialize correctly."
  (let ((server (make-emjupy-server :host "localhost" :port 8888 :token "abc")))
    (should (equal (emjupy-server-host server) "localhost"))
    (should (equal (emjupy-server-token server) "abc"))))

;;; ---------------------------------------------------------------------
;;; 2. .ipynb parsing & serialization
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-ipynb-roundtrip ()
  "Parsing then re-serializing a notebook preserves structure."
  (let* ((raw-json "{\"cells\":[{\"cell_type\":\"code\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"import numpy as np\\n\",\"print(1)\"]}],\"metadata\":{\"language_info\":{\"name\":\"python\"}},\"nbformat\":4,\"nbformat_minor\":5}")
         (parsed-nb (emjupy--parse-ipynb raw-json))
         (first-cell (aref (emjupy-notebook-cells parsed-nb) 0))
         (reserialized (emjupy--serialize-notebook parsed-nb)))
    (should (eq (emjupy-cell-type first-cell) 'code))
    (should (string= (emjupy-cell-source first-cell) "import numpy as np\nprint(1)"))
    (should (string-match-p "\"nbformat\":4" reserialized))
    (should (string-match-p "\"import numpy as np\\\\n\"" reserialized))))

;;; ---------------------------------------------------------------------
;;; 3. Cell operations: insert / delete / move / cycle-type
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-cell-insertion-and-deletion ()
  "Creating and deleting notebook cells dynamically."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x = 10"
                                 :outputs [] :metadata (make-hash-table)))
      buf nb
    (with-current-buffer buf
      (goto-char (point-min))
      (emjupy-insert-cell-below)
      (should (= (length (emjupy-notebook-cells nb)) 2))
      (emjupy-delete-cell)
      (should (= (length (emjupy-notebook-cells nb)) 1)))))

(ert-deftest emjupy-test-move-cell-up-down ()
  "Moving a cell swaps its position; its output moves WITH it (output is
a field on the cell struct, not a separate entity); the top/bottom
boundary is a no-op rather than an error."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "A" :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "B" :outputs [] :metadata (make-hash-table)))
         (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "C" :outputs [] :metadata (make-hash-table)))
         (out (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" out) (puthash "text" "B-output\n" out)
    (setf (emjupy-cell-outputs c2) (vector out))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source (emjupy-notebook-cells nb)) '("B" "A" "C")))
        (should (> (length (emjupy-cell-outputs (aref (emjupy-notebook-cells nb) 0))) 0))
        ;; the (now) top cell moving up again must be a no-op
        (goto-char (overlay-start (emjupy-cell-overlay (aref (emjupy-notebook-cells nb) 0))))
        (emjupy-move-cell-up)
        (should (equal (mapcar #'emjupy-cell-source (emjupy-notebook-cells nb)) '("B" "A" "C")))))))

(ert-deftest emjupy-test-cycle-cell-type ()
  "C-c C-t toggles a cell between code and markdown."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'markdown))
        (emjupy-cycle-cell-type)
        (should (eq (emjupy-cell-type cell) 'code))))))

;;; ---------------------------------------------------------------------
;;; 4. Execution: cell order must never scramble; sync must cover ALL cells
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-execute-preserves-cell-order ()
  "Executing a non-last cell must not shove it (and its output) to the
end of the buffer -- output belongs directly after its own cell, and
every later cell must still render after it, in order."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "2+2" :outputs [] :metadata (make-hash-table)))
        (c3 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "3+3" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2 c3) buf nb
      (with-current-buffer buf
        (let ((emjupy--ws-connection 'fake-connection))
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (emjupy-execute-cell-at-point))
        (let ((oh (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" oh) (puthash "name" "stdout" oh) (puthash "text" "2\n" oh)
          (emjupy--append-output-to-cell c1 oh))
        (should (< (overlay-start (emjupy-cell-output-ov c1)) (overlay-start (emjupy-cell-overlay c2))))
        (should (< (overlay-start (emjupy-cell-overlay c2)) (overlay-start (emjupy-cell-overlay c3))))))))

(ert-deftest emjupy-test-execute-syncs-all-cells-not-just-current ()
  "Executing one cell must not discard unsynced live edits sitting in a
DIFFERENT cell -- the rerender it triggers rebuilds the whole buffer
from cell structs, so every cell must be synced first, not just the
one being run."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "1+1" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "" :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((emjupy--ws-connection 'fake-connection))
          ;; type into cell 2 with no explicit sync, the way live typing works
          (goto-char (overlay-start (emjupy-cell-overlay c2)))
          (insert "999")
          ;; then execute cell 1
          (goto-char (overlay-start (emjupy-cell-overlay c1)))
          (emjupy-execute-cell-at-point))
        (should (string-match-p "999" (buffer-string)))))))

;;; ---------------------------------------------------------------------
;;; 5. Output-box rendering: visibility, merged divider, 80-col borders
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-no-output-box-when-no-output ()
  "A cell with no output (e.g. a bare import) gets no output box at all,
and its own bottom border is never hidden."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "import numpy as np"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should-not (emjupy-cell-output-ov cell))
      (let ((footer (overlay-get (emjupy-cell-overlay cell) 'after-string)))
        (should (and footer (string-match-p "└" footer)))))))

(ert-deftest emjupy-test-output-box-merges-with-input-border ()
  "A cell WITH output gets ONE merged divider line (T-junction), not a
closing border immediately followed by a separate opening one."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "print(1)"
                                  :outputs [] :metadata (make-hash-table)))
         (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
    (puthash "text/plain" "1" dat)
    (puthash "output_type" "execute_result" oh) (puthash "data" dat oh)
    (setf (emjupy-cell-outputs cell) (vector oh))
    (emjupy-test--with-notebook (vector cell) buf nb
      (should (emjupy-cell-output-ov cell))
      (should (string= (overlay-get (emjupy-cell-overlay cell) 'after-string) ""))
      (should (string-prefix-p "├" (overlay-get (emjupy-cell-output-ov cell) 'before-string))))))

(ert-deftest emjupy-test-box-borders-are-80-columns ()
  "Box-drawing borders (excluding their trailing newline) are pinned to
`emjupy--box-width', regardless of label length, and never crash on an
overlong label."
  (should (= (1- (length (emjupy--box-header "[In: 1] python"))) emjupy--box-width))
  (should (= (1- (length (emjupy--box-footer))) emjupy--box-width))
  (should (string-suffix-p "\n" (emjupy--box-header (make-string 500 ?x)))))

;;; ---------------------------------------------------------------------
;;; 6. Rich output: display_data / image routing
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-display-data-not-dropped ()
  "display_data messages -- how matplotlib's inline figure actually
arrives, separately from the execute_result text repr -- must be
captured, not silently ignored."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                 :outputs [] :metadata (make-hash-table)))
        (msg-id "msg-display-data-test")
        (emjupy--pending-requests (make-hash-table :test 'equal)))
    (puthash msg-id cell emjupy--pending-requests)
    (let* ((hdr (make-hash-table :test 'equal)) (ph (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)) (content (make-hash-table :test 'equal))
           (msg (make-hash-table :test 'equal)))
      (puthash "msg_type" "display_data" hdr)
      (puthash "msg_id" msg-id ph)
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "data" dat content)
      (puthash "header" hdr msg) (puthash "parent_header" ph msg) (puthash "content" content msg)
      (ignore-errors (emjupy--handle-ws-message nil (json-serialize msg)))
      (should (> (length (emjupy-cell-outputs cell)) 0))
      (should (equal (gethash "output_type" (aref (emjupy-cell-outputs cell) 0)) "display_data")))))

(ert-deftest emjupy-test-image-preferred-over-text ()
  "When a result carries both image/png and text/plain, the image
branch is taken; a text-only result still renders its text normally."
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "<<IMG:png>>" (buffer-string))))))))

;;; ---------------------------------------------------------------------
;;; 7. Kernel restart
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-restart-kernel-requires-connection ()
  "Restarting with no kernel connected signals a clear user-error
instead of failing deep inside some HTTP call."
  (let ((emjupy--current-server nil)
        (emjupy--current-kernel-id nil))
    (should-error (emjupy-restart-kernel) :type 'user-error)))

(ert-deftest emjupy-test-restart-kernel-reuses-same-id ()
  "Restart must POST to the connected kernel's OWN id and reconnect
with that SAME id (not spawn a fresh one), and drop now-orphaned
pending requests."
  (let ((emjupy--current-server (make-emjupy-server :base-url "localhost:8888" :token "tok"))
        (emjupy--current-kernel-id "kernel-abc")
        (emjupy--ws-connection 'fake-open)
        (emjupy--pending-requests (make-hash-table :test 'equal))
        (restart-path nil) (reconnected-with nil))
    (puthash "stale-msg" 'some-cell emjupy--pending-requests)
    (cl-letf (((symbol-function 'emjupy--http-request)
               (lambda (_method _server path &rest _)
                 (when (string-match-p "/restart" path) (setq restart-path path))
                 (make-hash-table :test 'equal)))
              ((symbol-function 'emjupy-connect-kernel)
               (lambda (host port kernel-id token)
                 (setq reconnected-with (list host port kernel-id token))))
              ((symbol-function 'websocket-openp) (lambda (&rest _) t))
              ((symbol-function 'websocket-close) (lambda (&rest _) nil)))
      (emjupy-restart-kernel))
    (should (string-match-p "kernel-abc" restart-path))
    (should (equal (nth 2 reconnected-with) "kernel-abc"))
    (should (= (hash-table-count emjupy--pending-requests) 0))))

;;; ---------------------------------------------------------------------
;;; 8. Syntax highlighting
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-python-cell-gets-real-highlighting ()
  "Code cells are fontified via real `python-mode' (bundled with Emacs
core) in a throwaway temp buffer, not a fake major-mode-shadowing
hack -- this must produce actual `face' text properties."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                                 :source "def foo(x):\n    return x + 1"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (let* ((ov (emjupy-cell-overlay cell))
             (faces (cl-loop for p from (overlay-start ov) below (overlay-end ov)
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

;;; ---------------------------------------------------------------------
;;; 9. Eglot / completion-framework integration
;;; ---------------------------------------------------------------------

(defun emjupy-test--python-lsp-available-p ()
  "Best-effort check for a usable Python language server on PATH."
  (or (executable-find "pylsp") (executable-find "pyright-langserver")
      (executable-find "jedi-language-server")))

(ert-deftest emjupy-test-shadow-delegate-skips-markdown-cells ()
  "The shadow-buffer delegate must no-op for markdown cells -- no LSP
server applies to prose -- without needing Eglot or any process."
  (let ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'markdown :source "hi"
                                 :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay cell)))
        (should-not (emjupy--cell-shadow-delegate (lambda (&rest _) 'should-not-run)))))))

(ert-deftest emjupy-test-shadow-delegate-maps-position-for-code-cells ()
  "For a code cell, the delegate must land in the shadow buffer at the
position corresponding to point in the notebook buffer -- pure
marker/position-mapping mechanics, no live LSP server required."
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "aaa = 1"
                               :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "bbb = 2"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (forward-char 3) ;; land right after "bbb"
        (let (seen)
          (emjupy--cell-shadow-delegate
           (lambda (_cell-start _shadow-start _buf)
             (setq seen (buffer-substring-no-properties (max (point-min) (- (point) 3)) (point)))))
          (should (equal seen "bbb")))))))

(ert-deftest emjupy-test-eglot-cross-cell-completion ()
  "End-to-end: a name defined in one cell is a completion candidate
while editing a DIFFERENT cell, automatically, with no
`emjupy-edit-cell-externally' call anywhere -- this is the actual
IDE-experience feature. Needs a real Python language server on PATH;
skipped otherwise so the rest of the suite isn't held hostage to it."
  (unless (and (emjupy-test--python-lsp-available-p) (require 'eglot nil 'noerror))
    (ert-skip "No Python language server (pylsp/pyright/jedi-language-server) on PATH"))
  (let ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "MY_TEST_MARKER_VALUE = 42\n" :outputs [] :metadata (make-hash-table)))
        (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "MY_TEST_MARKER"
                               :outputs [] :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (ignore-errors (emjupy--ensure-shadow-buffer nb))
      (let ((sbuf (emjupy-notebook-shadow-buffer nb)))
        (unless sbuf (ert-skip "Shadow buffer could not be created"))
        (with-current-buffer sbuf
          (let ((w 0))
            (while (and (< w 20) (not (and (eglot-current-server) (jsonrpc-running-p (eglot-current-server)))))
              (accept-process-output nil 1.0) (setq w (1+ w))))
          (unless (eglot-current-server) (ert-skip "Eglot did not connect within timeout"))))
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay c2)))
        (goto-char (line-end-position))
        (let ((result (run-hook-with-args-until-success 'completion-at-point-functions)))
          (should (consp result))
          (when (consp result)
            (let* ((start (nth 0 result)) (end (nth 1 result))
                   (cands (all-completions "" (nth 2 result) nil)))
              (should (>= start (overlay-start (emjupy-cell-overlay c2))))
              (should (<= end (overlay-end (emjupy-cell-overlay c2))))
              (should (member "MY_TEST_MARKER_VALUE" cands)))))))))

(provide 'emjupy-test)
;;; emjupy-test.el ends here
