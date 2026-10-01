;;; emjupy-robustness-test.el --- Randomised and adversarial tests for emjupy  -*- lexical-binding: t; -*-

;;; Commentary:

;; Tests that generate their inputs: random message orders, random edit
;; sequences, notebooks in every shape nbformat allows, hostile content.
;; Each run draws a seed, and every failure names it; set EMJUPY_FUZZ_SEED
;; to replay one exactly.  EMJUPY_FUZZ_ITERATIONS sets how many cases each
;; test tries -- small by default so the suite stays quick, larger in the
;; nightly job.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test)

(defvar emjupy-fuzz--seed nil
  "The seed of this run, drawn once so every test in it can report it.")

(defun emjupy-fuzz-seed ()
  "Return this run's seed, from EMJUPY_FUZZ_SEED or drawn fresh."
  (or emjupy-fuzz--seed
      (setq emjupy-fuzz--seed
            (let ((env (getenv "EMJUPY_FUZZ_SEED")))
              (if (and env (not (string-empty-p env)))
                  (string-to-number env)
                (random 1000000))))))

(defun emjupy-fuzz-iterations (default)
  "How many cases to try: EMJUPY_FUZZ_ITERATIONS, or DEFAULT."
  (let ((env (getenv "EMJUPY_FUZZ_ITERATIONS")))
    (if (and env (not (string-empty-p env))) (string-to-number env) default)))

(defmacro emjupy-fuzz-cases (var n &rest body)
  "Run BODY N times with VAR bound to the case number, seeded reproducibly.
The seed and the case number are attached to any failure."
  (declare (indent 2))
  `(let ((seed (emjupy-fuzz-seed)))
     (dotimes (,var ,n)
       (random (format "%d-%d" seed ,var))
       (ert-info ((format "EMJUPY_FUZZ_SEED=%d, case %d" seed ,var))
         ,@body))))

(defun emjupy-fuzz-pick (list)
  "Return a random element of LIST."
  (nth (random (length list)) list))

;;;; 1. Message order

(defun emjupy-fuzz--output-message (parent)
  "Return a random output message, as (TYPE . CONTENT), for request PARENT."
  (ignore parent)
  (pcase (random 5)
    ((or 0 1) (list "stream" (cons "name" (emjupy-fuzz-pick '("stdout" "stderr")))
                    (cons "text" (format "line %d\n" (random 1000)))))
    (2 (let ((d (make-hash-table :test 'equal)))
         (puthash "text/plain" (format "%d" (random 1000)) d)
         (list "execute_result" (cons "data" d) (cons "execution_count" 1))))
    (3 (let ((d (make-hash-table :test 'equal)))
         (puthash "text/plain" (format "<figure %d>" (random 1000)) d)
         (list "display_data" (cons "data" d))))
    (4 (list "error" (cons "ename" "E") (cons "evalue" "v")
             (cons "traceback" (vector (format "trace %d" (random 1000))))))))

(defun emjupy-fuzz--requests (n)
  "Return N random requests as plists: :id, :outputs, :count."
  (cl-loop for i from 1 to n
           collect (list :id (format "req-%d" i)
                         :count i
                         :outputs (cl-loop repeat (random 5)
                                           collect (emjupy-fuzz--output-message i)))))

(defun emjupy-fuzz--channels (requests)
  "Return the iopub and shell message sequences for REQUESTS, in order.
The kernel handles requests one at a time, so each channel is ordered:
all of one request's outputs and its idle before the next request's,
and the replies in request order."
  (let (iopub shell)
    (dolist (r requests)
      (dolist (o (plist-get r :outputs))
        (push (list (car o) (plist-get r :id) (cdr o)) iopub))
      (push (list "status" (plist-get r :id) '(("execution_state" . "idle"))) iopub)
      (push (list "execute_reply" (plist-get r :id)
                  `(("execution_count" . ,(plist-get r :count))))
            shell))
    (list (nreverse iopub) (nreverse shell))))

(defun emjupy-fuzz--riffle (a b)
  "Return A and B interleaved at random, each keeping its own order."
  (let (out)
    (while (or a b)
      (if (and a (or (null b) (zerop (random 2))))
          (push (pop a) out)
        (push (pop b) out)))
    (nreverse out)))

(defun emjupy-fuzz--run-messages (requests messages)
  "Feed MESSAGES to a kernel with REQUESTS pending; return what the cells got.
The result is a list of (OUTPUT-SUMMARY EXEC-COUNT) per request, and
whether anything was left in the pending table."
  (clrhash emjupy--request-halves)
  (let* ((cells (mapcar (lambda (_) (make-emjupy-cell :id (emjupy--new-cell-id)
                                                      :type 'code :source "x"
                                                      :outputs []
                                                      :metadata (make-hash-table)))
                        requests))
         (nb (make-emjupy-notebook :cells (vconcat cells)))
         (kernel (make-emjupy-kernel :id "k" :pending (make-hash-table :test 'equal)
                                     :notebook nb)))
    (cl-mapc (lambda (r c) (puthash (plist-get r :id) c (emjupy-kernel-pending kernel)))
             requests cells)
    (dolist (m messages)
      (emjupy--handle-ws-message
       kernel (emjupy-test--kernel-msg (nth 0 m) (nth 1 m) (nth 2 m))))
    (list (mapcar (lambda (c)
                    (list (mapcar (lambda (o)
                                    (list (gethash "output_type" o)
                                          (gethash "name" o)
                                          (or (gethash "text" o)
                                              (let ((d (gethash "data" o)))
                                                (and (hash-table-p d)
                                                     (gethash "text/plain" d)))
                                              (gethash "traceback" o))))
                                  (append (emjupy-cell-outputs c) nil))
                          (emjupy-cell-exec-count c)))
                  cells)
          (hash-table-count (emjupy-kernel-pending kernel))
          (hash-table-count emjupy--request-halves))))

(ert-deftest emjupy-fuzz-message-order-does-not-matter ()
  "Wherever the replies fall among the outputs, every cell ends the same.

The protocol orders each channel but not one against the other.  The
dropped-output bug was one such order -- an output after its reply --
found by chance on a slow machine.  This tries many at random, for
several requests in flight, against the order with every reply last."
  (emjupy-fuzz-cases i (emjupy-fuzz-iterations 200)
    (let* ((requests (emjupy-fuzz--requests (1+ (random 3))))
           (channels (emjupy-fuzz--channels requests))
           (iopub (nth 0 channels)) (shell (nth 1 channels))
           (reference (emjupy-fuzz--run-messages requests (append iopub shell)))
           (shuffled (emjupy-fuzz--run-messages requests (emjupy-fuzz--riffle iopub shell))))
      ;; the same outputs, in the same cells, with the same counts
      (should (equal (nth 0 shuffled) (nth 0 reference)))
      ;; every count arrived
      (dolist (cell (nth 0 shuffled)) (should (numberp (nth 1 cell))))
      ;; nothing left waiting, and no bookkeeping left behind
      (should (= (nth 1 shuffled) 0))
      (should (= (nth 2 shuffled) 0)))))

;;;; 2. Random editing

(defun emjupy-fuzz--stream (text)
  "Return a stdout stream output carrying TEXT."
  (let ((o (make-hash-table :test 'equal)))
    (puthash "output_type" "stream" o) (puthash "name" "stdout" o)
    (puthash "text" text o) o))

(defun emjupy-fuzz--goto-random-cell (nb)
  "Put point somewhere inside the source of a random cell of NB; return it."
  (let* ((cells (emjupy-notebook-cells nb))
         (cell (aref cells (random (length cells))))
         (ov (emjupy-cell-overlay cell))
         ;; the cell's extent in the buffer, not its stored source, which
         ;; is stale until the next sync: a position past the source\'s end
         ;; is the separator, where typing is rightly refused
         (len (max 0 (- (overlay-end ov) (overlay-start ov) 1))))
    (goto-char (+ (overlay-start ov) (random (1+ len))))
    cell))

(defconst emjupy-fuzz--edits
  '(type delete-char insert-above insert-below delete-cell move-up move-down
    split join cycle-type clear-output toggle-output output-arrives undo)
  "The operations the edit fuzz chooses from.")

(defun emjupy-fuzz--do-edit (op nb buf)
  "Perform edit OP in notebook NB, shown in BUF, as a command would."
  (let ((cell (emjupy-fuzz--goto-random-cell nb))
        (n (length (emjupy-notebook-cells nb))))
    (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
      ;; A command that declines -- nothing to hide, no cell above to join
      ;; -- says so with `user-error\=', and that is a correct answer.
      (let ((emjupy-confirm-clear-output nil))
       (ignore-error user-error
        (pcase op
          ('type (insert (emjupy-fuzz-pick '("x" " = 1" "\n" "# note" "é"))))
          ('delete-char (unless (bobp)
                          (ignore-error (text-read-only buffer-read-only)
                            (delete-char -1))))
          ('insert-above (call-interactively #'emjupy-insert-cell-above))
          ('insert-below (call-interactively #'emjupy-insert-cell-below))
          ('delete-cell (when (> n 1) (call-interactively #'emjupy-delete-cell)))
          ('move-up (ignore-error user-error (call-interactively #'emjupy-move-cell-up)))
          ('move-down (ignore-error user-error (call-interactively #'emjupy-move-cell-down)))
          ('split (ignore-error user-error (call-interactively #'emjupy-split-cell)))
          ('join (ignore-error user-error (call-interactively #'emjupy-join-cell-above)))
          ('cycle-type (call-interactively #'emjupy-cycle-cell-type))
          ('clear-output (call-interactively #'emjupy-clear-cell-output))
          ('toggle-output (call-interactively #'emjupy-toggle-cell-output))
          ('output-arrives
           (when (eq (emjupy-cell-type cell) 'code)
             (emjupy--append-output-to-cell cell (emjupy-fuzz--stream "out\n") nb)
             (emjupy-flush-output buf)))
          ('undo (let ((last-command nil)) (undo)))))))))

(defun emjupy-fuzz--sources (nb)
  "Return NB's cells as (TYPE SOURCE) pairs, after syncing the buffer."
  (emjupy--sync-all-cells)
  (mapcar (lambda (c) (list (emjupy-cell-type c) (emjupy-cell-source c)))
          (append (emjupy-notebook-cells nb) nil)))

(ert-deftest emjupy-fuzz-random-edits-keep-the-buffer-sound ()
  "Any sequence of commands leaves the buffer and the cells agreeing.

After every step the invariants hold -- each cell drawn once, where its
struct says, with nothing stray -- and the cells read back from the
buffer are what is shown.  Most of this project's bugs have been one
particular sequence that broke that; this tries many."
  (emjupy-fuzz-cases i (emjupy-fuzz-iterations 30)
    (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                         (emjupy-test--cell-like 'markdown "# Title\ntext")
                         (emjupy-test--cell-like 'code "b = 2\nc = 3"
                                                 (vector (emjupy-fuzz--stream "hi\n")))
                         (emjupy-test--cell-like 'code "d = 4")))
          (trail nil))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (dotimes (_ 25)
            (let ((op (emjupy-fuzz-pick emjupy-fuzz--edits)))
              (push op trail)
              (ert-info ((format "after %S" (reverse trail)))
                (emjupy-fuzz--do-edit op nb buf)
                (undo-boundary)
                ;; The cells catch up with the buffer at the next sync, which
                ;; every command that reads them performs first; compare
                ;; after one, as the user's next command would.
                (emjupy--sync-all-cells)
                (should-not (emjupy--check-invariants))
                (should (> (length (emjupy-notebook-cells nb)) 0))))))))))

(ert-deftest emjupy-fuzz-undo-takes-every-edit-back ()
  "Undoing everything, after any sequence of edits, gives the notebook back.

Only commands that edit the notebook are used, and the comparison is of
the cells -- type and source -- as they read back from the buffer."
  (emjupy-fuzz-cases i (emjupy-fuzz-iterations 30)
    (let* ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                          (emjupy-test--cell-like 'markdown "# Title")
                          (emjupy-test--cell-like 'code "b = 2\nc = 3")))
           (edits (remq 'undo (remq 'output-arrives (remq 'toggle-output
                                                         (remq 'clear-output emjupy-fuzz--edits)))))
           (trail nil))
      (emjupy-test--with-notebook cells buf nb
        (with-current-buffer buf
          (buffer-enable-undo)
          (setq buffer-undo-list nil)
          (let ((before (emjupy-fuzz--sources nb)))
            (dotimes (_ 12)
              (let ((op (emjupy-fuzz-pick edits)))
                (push op trail)
                (ert-info ((format "while editing: %S" (reverse trail)))
                  (emjupy-fuzz--do-edit op nb buf))
                (undo-boundary)))
            (ert-info ((format "edits %S" (reverse trail)))
              (let ((last-command nil))
                (undo-start)
                (condition-case nil
                    (while t (undo-more 1))
                  (user-error nil) (error nil)))
              (emjupy--sync-all-cells)
              (should-not (emjupy--check-invariants))
              (should (equal (emjupy-fuzz--sources nb) before)))))))))

(ert-deftest emjupy-fuzz-undo-after-two-joins ()
  "Undo after joining twice, with output and typing between, does not err.

The edit fuzz\'s shortest failing sequence, fixed here so it can be
worked on: undo fails with \"Changes to be undone are outside visible
portion of buffer\", meaning some undo entries point past the end of the
buffer -- positions a join, or the redraw of output after one, did not
shift."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'markdown "# Title\ntext")
                       (emjupy-test--cell-like 'code "b = 2\nc = 3"
                                               (vector (emjupy-fuzz--stream "hi\n")))
                       (emjupy-test--cell-like 'code "d = 4")))
        (steps '((move-down 725 472) (split 175 736) (join 666 603)
                 (output-arrives 582 131) (type 27 195) (join 135 785) (undo 610 887))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (buffer-enable-undo)
        (setq buffer-undo-list nil)
        (dolist (st steps)
          (cl-letf (((symbol-function 'emjupy-fuzz--goto-random-cell)
                     (lambda (nb)
                       (let* ((cs (emjupy-notebook-cells nb))
                              (c (aref cs (mod (nth 1 st) (length cs))))
                              (ov (emjupy-cell-overlay c))
                              (len (length (emjupy-cell-source c))))
                         (goto-char (+ (overlay-start ov) (mod (nth 2 st) (1+ len))))
                         c))))
            (emjupy-fuzz--do-edit (nth 0 st) nb buf))
          (undo-boundary))
        (emjupy--sync-all-cells)
        (should-not (emjupy--check-invariants))))))

(ert-deftest emjupy-fuzz-inserted-cell-keeps-its-separator ()
  "A backspace at the start of a cell inserted before another is refused.

Inserting a cell protected the gaps while the following cell\'s overlay
still stretched back over the new one, so the gap between the two was
left writable; a backspace there deleted it and the cells ran together.
The edit fuzz found it, shrunk to: insert a cell, insert another just
before it, backspace."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'code "b = 2"))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (goto-char (overlay-start (emjupy-cell-overlay (aref cells 1))))
        (call-interactively #'emjupy-insert-cell-above)
        (let ((x (emjupy--cell-at-point)))
          (goto-char (overlay-start (emjupy-cell-overlay (aref cells 0))))
          (call-interactively #'emjupy-insert-cell-below)
          (goto-char (overlay-start (emjupy-cell-overlay x)))
          (should-error (delete-char -1) :type 'text-read-only)
          (emjupy--sync-all-cells)
          (should-not (emjupy--check-invariants)))))))

;;;; 3. Notebook round-trip

(defconst emjupy-fuzz--corpus
  (expand-file-name "test-corpus" (file-name-directory
                                   (or load-file-name buffer-file-name)))
  "Notebooks in every shape the round-trip tests read.
Written by tools/make-notebook-corpus.py, with nbformat itself.")

(defconst emjupy-fuzz--checker
  (expand-file-name "../tools/check-roundtrip.py" (file-name-directory
                                                  (or load-file-name buffer-file-name)))
  "The script that compares a notebook with emjupy's save of it.")

(defun emjupy-fuzz--read-bytes (file)
  "Return FILE's contents decoded as UTF-8."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8)) (insert-file-contents file))
    (buffer-string)))

(defun emjupy-fuzz--save-through-buffer (json)
  "Open JSON as a notebook buffer, read it back from the buffer, and save it.
This is the path a real save takes: cells drawn, the buffer synced back
into them, then serialised."
  (let* ((parsed (emjupy--parse-ipynb json))
         (out nil))
    (emjupy-test--with-notebook (emjupy-notebook-cells parsed) buf nb
      (with-current-buffer buf
        (setf (emjupy-notebook-metadata nb) (emjupy-notebook-metadata parsed))
        (emjupy--sync-all-cells)
        (setq out (emjupy--serialize-notebook nb))))
    out))

(defun emjupy-fuzz--nbformat-p ()
  "Non-nil if a Python with nbformat is available to validate against."
  (and (executable-find "python3")
       (zerop (call-process "python3" nil nil nil "-c" "import nbformat"))))

(ert-deftest emjupy-fuzz-notebooks-round-trip ()
  "Every notebook in the corpus saves back to what it was.

Through the structs and through a drawn buffer, saving twice gives the
same bytes as saving once, and -- where nbformat is available -- the
saved copy validates and holds every cell, source, output, metadata and
attachment the original held.  The corpus found that attachments, the
images pasted into markdown cells, were dropped on save."
  (let ((files (directory-files emjupy-fuzz--corpus t "\\.ipynb\\'"))
        (validate (emjupy-fuzz--nbformat-p)))
    (should (> (length files) 5))
    (dolist (f files)
      (ert-info ((file-name-nondirectory f))
        (let* ((json (emjupy-fuzz--read-bytes f))
               (once (emjupy-fuzz--save-through-buffer json))
               (twice (emjupy-fuzz--save-through-buffer
                       (decode-coding-string once 'utf-8))))
          ;; stable: a second save changes nothing
          (should (equal once twice))
          (when validate
            (let ((saved (make-temp-file "emjupy-rt" nil ".ipynb")))
              (unwind-protect
                  (progn
                    (let ((coding-system-for-write 'no-conversion))
                      (write-region once nil saved nil 'quiet))
                    (with-temp-buffer
                      (let ((status (call-process "python3" nil t nil
                                                  emjupy-fuzz--checker f saved)))
                        (ert-info ((string-trim (buffer-string)))
                          (should (zerop status))))))
                (delete-file saved)))))))))

(ert-deftest emjupy-fuzz-large-output-round-trips ()
  "A cell with a very large output saves back unchanged, and in fair time.

Built here rather than kept in the corpus: at 200,000 lines it is some
megabytes, too heavy for the repository."
  :tags '(:timing)
  (let* ((text (apply #'concat (make-list 200000 "line\n")))
         (out (let ((o (make-hash-table :test 'equal)))
                (puthash "output_type" "stream" o) (puthash "name" "stdout" o)
                (puthash "text" text o) o))
         (nb (make-emjupy-notebook
              :cells (vector (make-emjupy-cell :id "c" :type 'code :source "big"
                                               :outputs (vector out)
                                               :metadata (make-hash-table :test 'equal)))))
         (start (float-time))
         (saved (emjupy--serialize-notebook nb))
         (back (emjupy--parse-ipynb (decode-coding-string saved 'utf-8))))
    (should (equal (gethash "text" (aref (emjupy-cell-outputs (aref (emjupy-notebook-cells back) 0)) 0))
                   text))
    (should (< (- (float-time) start) 5.0))))

;;;; 7. Hostile content

(defun emjupy-fuzz--draw-output-seconds (output)
  "Draw a cell holding OUTPUT; return the seconds it took."
  (let ((cell (emjupy-test--cell-like 'code "x" (vector output)))
        (start (float-time)))
    (emjupy-test--with-notebook (vector cell) buf nb
      (with-current-buffer buf (should-not (emjupy--check-invariants))))
    (- (float-time) start)))

(ert-deftest emjupy-fuzz-ansi-flood-is-drawn-in-bounded-time ()
  "Output made of escape sequences is drawn, quickly, without error.

A program can write anything to a terminal: colour codes by the tens of
thousands, sequences cut off half-way, codes no terminal knows."
  :tags '(:timing)
  (let ((text (concat (apply #'concat (make-list 20000 "\e[31;1mx\e[0m"))
                      "\e[38;5;999m\e[?25l\e[" "\e]0;title\a" "unterminated \e[31")))
    (should (< (emjupy-fuzz--draw-output-seconds (emjupy-fuzz--stream text)) 10.0))))

(ert-deftest emjupy-fuzz-huge-svg-does-not-stall-drawing ()
  "A multi-megabyte SVG output is drawn, or declined, without stalling."
  :tags '(:timing)
  (let* ((svg (concat "<svg xmlns='http://www.w3.org/2000/svg' width='10' height='10'>"
                      (apply #'concat (make-list 60000 "<rect x='1' y='1' width='1' height='1'/>"))
                      "</svg>"))
         (d (make-hash-table :test 'equal))
         (o (make-hash-table :test 'equal)))
    (puthash "image/svg+xml" svg d) (puthash "text/plain" "<Figure>" d)
    (puthash "output_type" "display_data" o) (puthash "data" d o)
    (puthash "metadata" (make-hash-table :test 'equal) o)
    (should (< (emjupy-fuzz--draw-output-seconds o) 10.0))))

(ert-deftest emjupy-fuzz-deeply-nested-notebook-is-refused-cleanly ()
  "JSON nested beyond what the parser takes is a typed error, not a crash.

The parser refuses it with `json-object-too-deep\=', which is a
`json-error\=' but NOT a `json-parse-error\=' -- and the handlers caught
only the latter, so an over-deep reply from the server, or a frame of
invalid UTF-8 from the language server, escaped them as an untyped error.
They catch `json-error\=' now.  A reply that deep reaches callers as the
`emjupy-http-error\=' they expect."
  (let ((json (concat "{\"cells\": [], \"metadata\": "
                      (apply #'concat (make-list 20000 "{\"a\": "))
                      "1" (make-string 20000 ?}) ", \"nbformat\": 4, \"nbformat_minor\": 5}")))
    (should-error (emjupy--parse-ipynb json) :type 'json-error)
    (should-error (emjupy--http-interpret (make-emjupy-server :base-url "127.0.0.1:9" :token "")
                                          "GET" "/api/contents/deep.ipynb" 200 json)
                  :type 'emjupy-http-error)))

(ert-deftest emjupy-fuzz-notebook-paths-are-encoded ()
  "A notebook path with spaces, non-ASCII or dots reaches the server encoded.

The path is sent as part of the URL, where a space or an accented letter
must be percent-encoded, and \"..\" must be sent as written, not
resolved here.  \"#\" and \"?\" were not encoded, so a notebook named
a#b.ipynb was asked for as \"a\", and saved there."
  (let ((server (make-emjupy-server :base-url "127.0.0.1:9" :token ""))
        (seen nil))
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (url &rest _) (push url seen) nil)))
      (dolist (path '("my notebook.ipynb" "données/café é.ipynb" "a/../b.ipynb"
                      "a#b.ipynb" "what?.ipynb"))
        (ignore (condition-case nil
                    (emjupy--http-request "GET" server (emjupy--contents-path path))
                  (emjupy-http-error nil)))))
    (dolist (url seen)
      (ert-info (url)
        (should-not (string-match-p " " url))
        (should-not (multibyte-string-p url))
        (should-not (string-match-p "[^\x00-\x7f]" url))))
    (should (seq-find (lambda (u) (string-match-p "my%20notebook\\.ipynb" u)) seen))
    (should (seq-find (lambda (u) (string-match-p "a/\\.\\./b\\.ipynb" u)) seen))
    ;; "#" and "?" are part of the name, not the start of a fragment or query
    (should (seq-find (lambda (u) (string-match-p "a%23b\\.ipynb" u)) seen))
    (should (seq-find (lambda (u) (string-match-p "what%3F\\.ipynb" u)) seen))))

;;;; 5. Leaks

(defun emjupy-fuzz--leftovers ()
  "Return what a session leaves lying around, as a plist of counts."
  (list :timers (length timer-list)
        :idle-timers (length timer-idle-list)
        :processes (length (seq-filter #'process-live-p (process-list)))
        :request-halves (hash-table-count emjupy--request-halves)
        :internal-requests (hash-table-count emjupy--internal-requests)
        :post-command-hook (length (default-value 'post-command-hook))
        :window-size-change (length (default-value 'window-size-change-functions))
        :kernel-hook (length emjupy-kernel-connected-functions)
        :width-hook (length emjupy-box-width-changed-functions)
        :emjupy-buffers (length (seq-filter
                                 (lambda (b) (string-match-p "emjupy" (buffer-name b)))
                                 (buffer-list)))))

(defun emjupy-fuzz--one-session ()
  "Open a notebook, work in it the way a session does, and close it."
  (let ((cells (vector (emjupy-test--cell-like 'code "a = 1")
                       (emjupy-test--cell-like 'markdown "# Title")
                       (emjupy-test--cell-like 'code "for i in tqdm(x): pass"))))
    (emjupy-test--with-notebook cells buf nb
      (with-current-buffer buf
        (let ((bar (aref cells 2)))
          ;; a progress bar's worth of updates, redrawn in batches
          (dotimes (i 3000)
            (emjupy--append-output-to-cell
             bar (let ((o (make-hash-table :test 'equal)))
                   (puthash "output_type" "stream" o) (puthash "name" "stderr" o)
                   (puthash "text" (format "\r%d%%|%s|" (/ i 30) (make-string (/ i 100) ?#)) o)
                   o)
             nb)
            (when (zerop (mod i 500)) (emjupy-flush-output buf)))
          (emjupy-flush-output buf))
        (goto-char (overlay-start (emjupy-cell-overlay (aref cells 0))))
        (call-interactively #'emjupy-insert-cell-below)
        (goto-char (overlay-start (emjupy-cell-overlay (aref cells 2))))
        (call-interactively #'emjupy-toggle-cell-output)
        (emjupy-re-render)))))

(ert-deftest emjupy-fuzz-a-session-leaves-nothing-behind ()
  "Opening, working in and closing notebooks leaves nothing behind.

Many sessions in a row, compared before and after: no timer, process,
hook, table entry or buffer outlives the notebook that made it.  One
leaked timer per notebook is invisible in a test and, after a day of
opening notebooks, is Emacs running hundreds of them."
  (emjupy-fuzz--one-session)           ; anything created once, on first use
  (let ((before (emjupy-fuzz--leftovers)))
    (dotimes (_ (emjupy-fuzz-iterations 10))
      (emjupy-fuzz--one-session))
    ;; let any deferred work run and finish
    (dotimes (_ 20) (accept-process-output nil 0.05))
    (let ((after (emjupy-fuzz--leftovers)))
      (cl-loop for (key val) on before by #'cddr
               do (ert-info ((format "%s: %s before, %s after" key val (plist-get after key)))
                    (should (<= (plist-get after key) val)))))))

(provide 'emjupy-robustness-test)

;;; emjupy-robustness-test.el ends here
