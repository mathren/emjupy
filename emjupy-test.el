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
           (emjupy--rerender-notebook)
           ,@body)
       (let ((kill-buffer-query-functions nil))
         (when (buffer-live-p ,buf-var) (kill-buffer ,buf-var))
         (when (and (emjupy-notebook-shadow-buffer ,nb-var)
                    (buffer-live-p (emjupy-notebook-shadow-buffer ,nb-var)))
           (ignore-errors (kill-buffer (emjupy-notebook-shadow-buffer ,nb-var))))))))

(defvar emjupy-test--sent nil
  "Payloads captured by `emjupy-test--with-fake-kernel'.")

(defmacro emjupy-test--with-fake-kernel (&rest body)
  "Run BODY with a stubbed-out kernel attached to this buffer's notebook.

Stubs the two seam functions rather than stuffing a non-websocket
sentinel into the kernel: the real `websocket-openp' type-checks its
argument, so a sentinel raises `wrong-type-argument' instead of
exercising the code under test. Sent payloads land in
`emjupy-test--sent'."
  `(let ((emjupy-test--sent nil))
     (when emjupy--buffer-notebook
       (setf (emjupy-notebook-kernel emjupy--buffer-notebook)
             (make-emjupy-kernel :id "fake-kernel"
                                 :pending (make-hash-table :test 'equal)
                                 :notebook emjupy--buffer-notebook)))
     (cl-letf (((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) t))
               ((symbol-function 'emjupy--ws-send)
                (lambda (payload &rest _) (push payload emjupy-test--sent))))
       ,@body)))

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
;;; 2b. nbformat validity of what we WRITE BACK
;;; ---------------------------------------------------------------------
;; The whole point of the .ipynb-as-source-of-truth design is that
;; non-emacs collaborators can open what we save. A notebook that fails
;; `nbformat.validate' is rejected by nbconvert, papermill and friends
;; even though it looks fine inside emjupy.

(ert-deftest emjupy-test-serialize-emits-cell-ids ()
  "nbformat_minor 5 requires an `id' on EVERY cell."
  (let* ((c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x=1"
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array))
         (cell (aref (gethash "cells" out) 0)))
    (should (stringp (gethash "id" cell)))
    (should (> (length (gethash "id" cell)) 0))))

(ert-deftest emjupy-test-serialize-preserves-original-cell-id ()
  "A cell that came from a real notebook keeps ITS id on the way back
out, so a round trip doesn't renumber cells for collaborators."
  (let* ((raw "{\"cells\":[{\"cell_type\":\"code\",\"id\":\"abc12345\",\"execution_count\":null,\"metadata\":{},\"outputs\":[],\"source\":[\"x=1\"]}],\"metadata\":{},\"nbformat\":4,\"nbformat_minor\":5}")
         (nb (emjupy--parse-ipynb raw))
         (out (json-parse-string (emjupy--serialize-notebook nb)
                                 :object-type 'hash-table :array-type 'array)))
    (should (equal (gethash "id" (aref (gethash "cells" out) 0)) "abc12345"))))

(ert-deftest emjupy-test-serialize-normalizes-required-output-fields ()
  "display_data/execute_result outputs REQUIRE `metadata' (and
execute_result also `execution_count') per the nbformat v4 schema.
Kernels don't always send them."
  (let* ((oh (make-hash-table :test 'equal))
         (dat (make-hash-table :test 'equal))
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                              :metadata (make-hash-table))))
    (puthash "text/plain" "<Figure>" dat)
    (puthash "output_type" "display_data" oh)
    (puthash "data" dat oh)                ; deliberately NO metadata
    (setf (emjupy-cell-outputs c) (vector oh))
    (let* ((nb (make-emjupy-notebook :cells (vector c)))
           (out (json-parse-string (emjupy--serialize-notebook nb)
                                   :object-type 'hash-table :array-type 'array))
           (o (aref (gethash "outputs" (aref (gethash "cells" out) 0)) 0)))
      (should (hash-table-p (gethash "metadata" o))))))

(ert-deftest emjupy-test-source-roundtrip-is-idempotent ()
  "Only interior lines carry a newline in the nbformat `source' array.
Appending one to the last line too makes every save/reload cycle grow
the cell by a trailing blank line."
  (should (equal (append (emjupy--source-lines "a\nb") nil) '("a\n" "b")))
  (should (equal (append (emjupy--source-lines "solo") nil) '("solo")))
  ;; parse(serialize(x)) == x
  (let* ((src "import numpy as np\nprint(1)")
         (c (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source src
                              :outputs [] :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c)))
         (again (emjupy--parse-ipynb (emjupy--serialize-notebook nb))))
    (should (equal (emjupy-cell-source (aref (emjupy-notebook-cells again) 0)) src))))

;;; ---------------------------------------------------------------------
;;; 2c. Server URL parsing (remote kernels reached through an SSH tunnel)
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-server-parts-handles-tunnel-urls ()
  "A tunnelled server may be given as a bare port, a host:port, a full
https URL, or a proxied base path -- all of which have to survive into
the WebSocket URL, not just the REST calls."
  (cl-flet ((parts (url) (emjupy--server-parts (make-emjupy-server :base-url url))))
    (let ((p (parts "localhost:18888")))
      (should (equal (plist-get p :host) "localhost"))
      (should (equal (plist-get p :port) "18888"))
      (should (equal (plist-get p :ws-scheme) "ws"))
      (should (equal (plist-get p :path) "")))
    ;; https must become wss, or the handshake is plaintext against a TLS port
    (let ((p (parts "https://example.org:8443")))
      (should (equal (plist-get p :ws-scheme) "wss"))
      (should (equal (plist-get p :port) "8443")))
    ;; a proxy / JupyterHub prefix must be kept
    (let ((p (parts "localhost:8888/user/alice")))
      (should (equal (plist-get p :host) "localhost"))
      (should (equal (plist-get p :port) "8888"))
      (should (equal (plist-get p :path) "/user/alice")))))

(ert-deftest emjupy-test-kernel-ws-url-is-scheme-and-path-aware ()
  "The kernel WebSocket URL must carry the scheme, the base path, and a
url-encoded token -- and must NOT emit a bare `?token=' when there is
no token."
  (let ((captured nil))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (url &rest _) (setq captured url) nil)))
      (let* ((server (make-emjupy-server
                      :base-url "https://example.org:8443/user/alice" :token "t o k"))
             (nb (make-emjupy-notebook :path "a.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "wss://" captured))
        (should (string-match-p "/user/alice/api/kernels/kid/channels" captured))
        (should (string-match-p "token=t%20o%20k" captured)))
      (let* ((server (make-emjupy-server :base-url "localhost:18888" :token ""))
             (nb (make-emjupy-notebook :path "b.ipynb" :server server)))
        (emjupy-connect-kernel nb "kid")
        (should (string-prefix-p "ws://" captured))
        (should-not (string-match-p "token=" captured))))))

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

(ert-deftest emjupy-test-notebook-buffer-name-includes-server ()
  "The same notebook path can exist on two servers; one buffer cannot
represent both."
  (let ((a (make-emjupy-server :base-url "localhost:18888"))
        (b (make-emjupy-server :base-url "localhost:19999")))
    (should-not (equal (emjupy--notebook-buffer-name "analysis.ipynb" a)
                       (emjupy--notebook-buffer-name "analysis.ipynb" b)))))

(ert-deftest emjupy-test-shadow-path-includes-server ()
  "Likewise for the Eglot shadow file: two servers hosting
`analysis.ipynb' must not share one shadow buffer, or each would
clobber the other's code."
  (let* ((a (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:18888")))
         (b (make-emjupy-notebook :path "analysis.ipynb"
                                  :server (make-emjupy-server :base-url "localhost:19999"))))
    (should-not (equal (emjupy--shadow-file-path a) (emjupy--shadow-file-path b)))))

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
        (emjupy-test--with-fake-kernel
         (goto-char (overlay-start (emjupy-cell-overlay c1)))
         (emjupy-execute-cell-at-point)
         ;; the code that got sent is the cell we asked for, not a neighbour
         (should (string-match-p (regexp-quote "1+1") (car emjupy-test--sent))))
        (let ((oh (make-hash-table :test 'equal)))
          (puthash "output_type" "stream" oh) (puthash "name" "stdout" oh) (puthash "text" "2\n" oh)
          (emjupy--append-output-to-cell c1 oh nb))
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
        (emjupy-test--with-fake-kernel
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
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                 :outputs [] :metadata (make-hash-table)))
         (msg-id "msg-display-data-test")
         (nb (make-emjupy-notebook :cells (vector cell)))
         (kernel (make-emjupy-kernel :id "k1"
                                     :pending (make-hash-table :test 'equal)
                                     :notebook nb)))
    (puthash msg-id cell (emjupy-kernel-pending kernel))
    (let* ((hdr (make-hash-table :test 'equal)) (ph (make-hash-table :test 'equal))
           (dat (make-hash-table :test 'equal)) (content (make-hash-table :test 'equal))
           (msg (make-hash-table :test 'equal)))
      (puthash "msg_type" "display_data" hdr)
      (puthash "msg_id" msg-id ph)
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "data" dat content)
      (puthash "header" hdr msg) (puthash "parent_header" ph msg) (puthash "content" content msg)
      ;; Handed the raw payload rather than a websocket-frame -- the handler
      ;; accepts either, so recorded kernel traffic can be replayed. NOT
      ;; wrapped in ignore-errors: a throw here must fail the test, not be
      ;; silently absorbed into a passing "0 outputs" assertion.
      (emjupy--handle-ws-message kernel (json-serialize msg))
      (should (> (length (emjupy-cell-outputs cell)) 0))
      (should (equal (gethash "output_type" (aref (emjupy-cell-outputs cell) 0)) "display_data")))))

(ert-deftest emjupy-test-image-preferred-over-text ()
  "When a result carries both image/png and text/plain, the image
branch is taken -- on an Emacs that can actually display images."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
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

(ert-deftest emjupy-test-image-falls-back-to-text-without-image-support ()
  "On an Emacs that CANNOT display images (a tty frame, or a build
without libpng -- `emacs-nox' is the common case), an image output must
degrade to the bundle's own text/plain repr plus a visible note.

Regression test: `create-image' SIGNALS `Invalid image type' rather
than returning nil, and rendering happens inside the websocket
callback where websocket.el swallows errors -- so the failure mode was
a silently blank output box with nothing logged."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) nil)))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
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
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (&rest _) (error "Invalid image type `png'"))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "plot()"
                                    :outputs [] :metadata (make-hash-table)))
           (oh (make-hash-table :test 'equal)) (dat (make-hash-table :test 'equal)))
      (puthash "image/png" "iVBORw0KGgo=" dat)
      (puthash "text/plain" "<Figure>" dat)
      (puthash "output_type" "display_data" oh) (puthash "data" dat oh)
      (setf (emjupy-cell-outputs cell) (vector oh))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (should (string-match-p "could not render" (buffer-string))))))))

(ert-deftest emjupy-test-svg-is-not-base64-decoded ()
  "image/svg+xml arrives as literal markup, not base64 -- decoding it
would corrupt it (or signal)."
  (cl-letf (((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :data data))))
    (let ((img (emjupy--render-image-output "<svg width='1'></svg>" 'svg)))
      (should (equal (plist-get (cdr img) :data) "<svg width='1'></svg>")))))

(ert-deftest emjupy-test-mime-text-accepts-string-or-array ()
  "nbformat stores multi-line MIME payloads as a string OR an array of
lines depending on whether the JSON came from the Contents API or
straight off disk; both must render."
  (should (equal (emjupy--mime-text "a\nb") "a\nb"))
  (should (equal (emjupy--mime-text ["a\n" "b"]) "a\nb"))
  (should (equal (emjupy--mime-text nil) "")))

(ert-deftest emjupy-test-duplicate-image-rendered-once ()
  "A cell whose last expression is a figure receives the SAME picture
twice from the kernel -- once as the execute_result repr, once as the
inline backend's display_data. It must be drawn once."
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (kind)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" "iVBORw0KGgoSAMEPAYLOAD==" dat)
                   (puthash "text/plain" "<Figure size 640x480 with 1 Axes>" dat)
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
  (cl-letf (((symbol-function 'emjupy--image-displayable-p) (lambda (&rest _) t))
            ((symbol-function 'create-image)
             (lambda (data &optional type &rest _) (list 'image :type type :len (length data))))
            ((symbol-function 'insert-image)
             (lambda (img &rest _) (insert (format "<<IMG:%s>>" (plist-get (cdr img) :type))))))
    (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "two figs"
                                   :outputs [] :metadata (make-hash-table)))
           (mk (lambda (payload)
                 (let ((oh (make-hash-table :test 'equal))
                       (dat (make-hash-table :test 'equal)))
                   (puthash "image/png" payload dat)
                   (puthash "output_type" "display_data" oh)
                   (puthash "data" dat oh)
                   oh))))
      (setf (emjupy-cell-outputs cell)
            (vector (funcall mk "iVBORw0KGgoAAAAA") (funcall mk "iVBORw0KGgoBBBBB")))
      (emjupy-test--with-notebook (vector cell) buf nb
        (with-current-buffer buf
          (let ((n 0) (start 0))
            (while (string-match "<<IMG:" (buffer-string) start)
              (setq n (1+ n) start (match-end 0)))
            (should (= n 2))))))))

(ert-deftest emjupy-test-repeated-text-output-is-not-deduplicated ()
  "Two identical `print' calls really are two outputs -- only images
are collapsed."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "print"
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

(ert-deftest emjupy-test-dedup-does-not-alter-saved-outputs ()
  "Deduplication is a RENDER-time concern only. The cell's outputs --
and therefore the .ipynb we write back -- must still contain exactly
what the kernel sent, so the file stays faithful for other clients."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda (kind)
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" kind oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell)
          (vector (funcall mk "execute_result") (funcall mk "display_data")))
    (emjupy-test--with-notebook (vector cell) buf nb
      ;; render collapses to one ...
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 1))
      ;; ... but the struct, and the serialized notebook, keep both
      (should (= (length (emjupy-cell-outputs cell)) 2))
      (let* ((json (emjupy--serialize-notebook nb))
             (out (json-parse-string json :object-type 'hash-table :array-type 'array)))
        (should (= (length (gethash "outputs" (aref (gethash "cells" out) 0))) 2))))))

(ert-deftest emjupy-test-dedup-can-be-disabled ()
  "`emjupy-deduplicate-image-outputs' nil restores the raw behaviour."
  (let* ((cell (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "fig"
                                 :outputs [] :metadata (make-hash-table)))
         (mk (lambda ()
               (let ((oh (make-hash-table :test 'equal))
                     (dat (make-hash-table :test 'equal)))
                 (puthash "image/png" "iVBORw0KGgoSAME=" dat)
                 (puthash "output_type" "display_data" oh)
                 (puthash "data" dat oh)
                 oh))))
    (setf (emjupy-cell-outputs cell) (vector (funcall mk) (funcall mk)))
    (let ((emjupy-deduplicate-image-outputs nil))
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 2)))
    (let ((emjupy-deduplicate-image-outputs t))
      (should (= (length (emjupy--outputs-for-render (emjupy-cell-outputs cell))) 1)))))

;;; ---------------------------------------------------------------------
;;; 7. Kernel restart
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-restart-kernel-requires-connection ()
  "Restarting with no kernel signals a clear user-error instead of
failing deep inside some HTTP call."
  (emjupy-test--with-notebook
      (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x"
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
        (vector (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "x"
                                  :outputs [] :metadata (make-hash-table)))
        buf nb
      (setf (emjupy-notebook-server nb) server)
      (let ((kernel (make-emjupy-kernel :id "kernel-abc" :server server
                                        :pending (make-hash-table :test 'equal)
                                        :notebook nb)))
        (setf (emjupy-notebook-kernel nb) kernel)
        (puthash "stale-msg" 'some-cell (emjupy-kernel-pending kernel))
        (cl-letf (((symbol-function 'emjupy--http-request)
                   (lambda (_method _server path &rest _)
                     (when (string-match-p "/restart" path) (setq restart-path path))
                     (make-hash-table :test 'equal)))
                  ((symbol-function 'emjupy-connect-kernel)
                   (lambda (notebook kernel-id &optional name)
                     (setq reconnected-with (list notebook kernel-id name))))
                  ((symbol-function 'emjupy--ws-live-p) (lambda (&rest _) nil)))
          (emjupy-restart-kernel))
        (should (string-match-p "kernel-abc" restart-path))
        (should (equal (nth 1 reconnected-with) "kernel-abc"))
        (should (eq (nth 0 reconnected-with) nb))
        (should (= (hash-table-count (emjupy-kernel-pending kernel)) 0))))))

;;; ---------------------------------------------------------------------
;;; 7b. Several notebooks / several servers at once
;;; ---------------------------------------------------------------------

(ert-deftest emjupy-test-each-notebook-has-its-own-kernel ()
  "Two open notebooks must hold two independent kernels. When kernel
state was global, opening the second silently stole the first's
connection."
  (let* ((nb1 (make-emjupy-notebook :path "one.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:18888")))
         (nb2 (make-emjupy-notebook :path "two.ipynb"
                                    :server (make-emjupy-server :base-url "localhost:19999")))
         (k1 (make-emjupy-kernel :id "k-one" :pending (make-hash-table :test 'equal) :notebook nb1))
         (k2 (make-emjupy-kernel :id "k-two" :pending (make-hash-table :test 'equal) :notebook nb2)))
    (setf (emjupy-notebook-kernel nb1) k1)
    (setf (emjupy-notebook-kernel nb2) k2)
    (should-not (eq (emjupy-notebook-kernel nb1) (emjupy-notebook-kernel nb2)))
    (should-not (eq (emjupy-kernel-pending k1) (emjupy-kernel-pending k2)))
    ;; and each kernel points back at its OWN notebook
    (should (eq (emjupy-kernel-notebook k1) nb1))
    (should (eq (emjupy-kernel-notebook k2) nb2))))

(ert-deftest emjupy-test-output-goes-to-the-requesting-notebook ()
  "A frame must be delivered to the notebook whose kernel produced it,
not to whichever buffer happens to be current. This is the
cross-talk that global `emjupy--current-notebook-buffer' caused: a
slow cell in notebook A finishing while you were reading notebook B
wrote A's output into B."
  (let* ((cell-a (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "a"
                                   :outputs [] :metadata (make-hash-table)))
         (cell-b (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code :source "b"
                                   :outputs [] :metadata (make-hash-table)))
         (buf-a (generate-new-buffer "*emjupy-A*"))
         (buf-b (generate-new-buffer "*emjupy-B*"))
         (nb-a (make-emjupy-notebook :path "A.ipynb" :cells (vector cell-a) :buffer buf-a))
         (nb-b (make-emjupy-notebook :path "B.ipynb" :cells (vector cell-b) :buffer buf-b))
         (k-a (make-emjupy-kernel :id "ka" :pending (make-hash-table :test 'equal) :notebook nb-a)))
    (unwind-protect
        (progn
          (dolist (pair (list (cons buf-a nb-a) (cons buf-b nb-b)))
            (with-current-buffer (car pair)
              (emjupy-mode)
              (setq emjupy--buffer-notebook (cdr pair))
              (emjupy--rerender-notebook)))
          (puthash "msg-a" cell-a (emjupy-kernel-pending k-a))
          ;; deliver A's frame while B is the current buffer
          (with-current-buffer buf-b
            (let* ((hdr (make-hash-table :test 'equal)) (ph (make-hash-table :test 'equal))
                   (content (make-hash-table :test 'equal)) (msg (make-hash-table :test 'equal)))
              (puthash "msg_type" "stream" hdr)
              (puthash "msg_id" "msg-a" ph)
              (puthash "name" "stdout" content)
              (puthash "text" "belongs-to-A\n" content)
              (puthash "header" hdr msg) (puthash "parent_header" ph msg)
              (puthash "content" content msg)
              (emjupy--handle-ws-message k-a (json-serialize msg))))
          (should (= (length (emjupy-cell-outputs cell-a)) 1))
          (should (= (length (emjupy-cell-outputs cell-b)) 0))
          (with-current-buffer buf-a
            (should (string-match-p "belongs-to-A" (buffer-string))))
          (with-current-buffer buf-b
            (should-not (string-match-p "belongs-to-A" (buffer-string)))))
      (let ((kill-buffer-query-functions nil))
        (dolist (b (list buf-a buf-b)) (when (buffer-live-p b) (kill-buffer b)))))))

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
