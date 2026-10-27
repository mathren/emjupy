;;; emjupy-lsp-test.el --- Unit tests: emjupy-lsp.el  -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2026 Mathieu Renzo

;; Author: Mathieu Renzo <mrenzo@arizona.edu>
;; Assisted-by: Claude:claude-opus-5.5 and other free-tier LLMs
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Unit tests of emjupy-lsp.el.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'emjupy)
(require 'emjupy-test-helpers)


;;;; Eglot / completion-framework integration

(ert-deftest emjupy-test-shadow-sanitizes-raw-bytes-for-lsp ()
  "Undecodable raw bytes must not reach the language server.

The shadow document is handed over as a JSON string, and `jsonrpc'
rejects anything that is not valid UTF-8 with `wrong-type-argument
utf-8-string-p', so the connection never starts."
  (let* ((raw (concat "x = 1" (string #x3FFF97) "\ny = 2"))
         (clean (emjupy--sanitize-for-lsp raw)))
    ;; before: not serialisable; after: fine
    (should-error (json-serialize (vector raw)))
    (should (json-serialize (vector clean)))
    ;; one character for one character, so shadow offsets still map back
    ;; onto the cell source they came from
    (should (= (length raw) (length clean)))))

(ert-deftest emjupy-test-shadow-leaves-ordinary-unicode-alone ()
  "Sanitising must only touch raw bytes -- accented text, dashes, maths
and non-Latin scripts are perfectly good UTF-8 and pass through."
  (let ((s "caf\u00e9 \u2014 na\u00efve \u2713 \u03b1\u03b2\u03b3"))
    (should (equal s (emjupy--sanitize-for-lsp s))))
  (should (equal "" (emjupy--sanitize-for-lsp "")))
  (should (equal "plain ascii" (emjupy--sanitize-for-lsp "plain ascii"))))

(ert-deftest emjupy-test-shadow-content-is-serialisable ()
  "The assembled shadow document is valid UTF-8 even when a cell holds a
raw byte."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source (concat "a = 1" (string #x3FFF97))
                               :outputs [] :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "b = 2" :outputs []
                               :metadata (make-hash-table)))
         (nb (make-emjupy-notebook :cells (vector c1 c2) :path "raw.ipynb")))
    (should (json-serialize (vector (emjupy--build-shadow-content nb))))))

(ert-deftest emjupy-test-shadow-position-maps-back-to-a-cell ()
  "A position in the shadow buffer maps back onto the character it came
from, in the right cell.  This is what turns a location the server
describes in shadow-file coordinates into a usable jump."
  (let* ((c1 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "alpha = 1" :outputs []
                               :metadata (make-hash-table)))
         (c2 (make-emjupy-cell :id (emjupy--new-cell-id) :type 'code
                               :source "beta = 2" :outputs []
                               :metadata (make-hash-table))))
    (emjupy-test--with-notebook (vector c1 c2) buf nb
      (with-current-buffer buf
        (let ((shadow (generate-new-buffer " *fake-shadow*")))
          (unwind-protect
              (progn
                (with-current-buffer shadow
                  (insert (emjupy--shadow-cell-marker (emjupy-cell-id c1)) "\n"
                          "alpha = 1" "\n\n"
                          (emjupy--shadow-cell-marker (emjupy-cell-id c2)) "\n"
                          "beta = 2" "\n"))
                (setf (emjupy-notebook-shadow-buffer nb) shadow)
                ;; start of cell 2's section in the shadow
                (let* ((s2 (emjupy--shadow-section-start shadow
                                                         (emjupy-cell-id c2)))
                       (mapped (emjupy--shadow-position-to-cell nb s2)))
                  (should mapped)
                  (should (eq (car mapped) c2))
                  (should (eq (get-text-property (cdr mapped) 'emjupy-cell) c2))
                  (should (equal (buffer-substring-no-properties
                                  (cdr mapped) (+ (cdr mapped) 4))
                                 "beta")))
                ;; and a few characters in still lands in the same cell
                (let* ((s1 (emjupy--shadow-section-start shadow
                                                         (emjupy-cell-id c1)))
                       (mapped (emjupy--shadow-position-to-cell nb (+ s1 6))))
                  (should (eq (car mapped) c1))
                  (should (eq (get-text-property (cdr mapped)
                                                 'emjupy-cell) c1))))
            (let ((kill-buffer-query-functions nil))
              (when (buffer-live-p shadow) (kill-buffer shadow)))))))))

(ert-deftest emjupy-test-lsp-needs-a-server-and-a-cwd ()
  "No session without somewhere to connect and a URI the server can
resolve.  Both arrive while a notebook is opened, so nil here means
\"not yet\", and the shadow-file path is still available meanwhile."
  (should-not (emjupy--lsp-session nil))
  (should-not (emjupy--lsp-session (make-emjupy-notebook :cells []
                                                         :path "n.ipynb")))
  (should-not (emjupy--lsp-session
               (make-emjupy-notebook :cells [] :path "n.ipynb"
                                     :server (make-emjupy-server :base-url "h"
                                                                 :token "t")))))

(ert-deftest emjupy-test-lsp-document-uri-is-on-the-server ()
  "The document URI names a path on the SERVER, built from the kernel's
working directory, so `import mylib' resolves against the code beside
the notebook.  Nothing here ever opens it."
  (let ((nb (make-emjupy-notebook :cells [] :path "sub/analysis.ipynb"
                                  :kernel-cwd "/srv/project")))
    (should (equal (emjupy--lsp-document-uri nb)
                   "file:///srv/project/analysis.emjupy.py"))))

(ert-deftest emjupy-test-lsp-url-carries-the-token-only-if-refused ()
  "The WebSocket URL is the Jupyter server's own; the token goes in a header.
In the address only for a server refusing it so, as an address is what
proxies and servers log."
  (let ((server (make-emjupy-server :base-url "localhost:9999"
                                    :token "sekrit"))
        (emjupy--token-in-ws-address nil))
    (let ((url (emjupy--lsp-url server)))
      (should (string-prefix-p "ws://localhost:9999/lsp/ws/pylsp" url))
      (should-not (string-match-p "token=" url)))
    (let ((emjupy--token-in-ws-address '("localhost:9999")))
      (should (string-match-p "token=sekrit" (emjupy--lsp-url server))))))

(ert-deftest emjupy-test-lsp-socket-notes-a-refused-header ()
  "A language server WebSocket refused, 403, notes its server, and reports."
  (let* ((server (make-emjupy-server :base-url "localhost:9999" :token "t"))
         (emjupy--token-in-ws-address nil)
         (emjupy--lsp-last-failure nil))
    (cl-letf (((symbol-function 'message) #'ignore))
      (funcall (emjupy--lsp-error-handler server) nil 'on-open
               '(websocket-received-error-http-response 403)))
    (should (equal emjupy--token-in-ws-address '("localhost:9999")))
    (should emjupy--lsp-last-failure)))

(ert-deftest emjupy-test-lsp-edits-are-applied-back-to-front ()
  "Edits are applied from the end, or each one moves the next.

Every edit's range refers to the document as the server last saw it, so
applying from the front shifts the ground under the rest -- the classic
way to turn a rename into plausible-looking nonsense."
  (let ((text "aaa bbb aaa"))
    (cl-flet ((edit (l1 c1 l2 c2 new)
                (let ((e (make-hash-table :test 'equal))
                      (r (make-hash-table :test 'equal))
                      (s (make-hash-table :test 'equal))
                      (n (make-hash-table :test 'equal)))
                  (puthash "line" l1 s) (puthash "character" c1 s)
                  (puthash "line" l2 n) (puthash "character" c2 n)
                  (puthash "start" s r) (puthash "end" n r)
                  (puthash "range" r e) (puthash "newText" new e)
                  e)))
      ;; both occurrences of aaa -> zzzz, given in document order
      (should (equal (emjupy--lsp-apply-edits
                      text (vector (edit 0 0 0 3 "zzzz") (edit 0 8 0 11
                                                               "zzzz")))
                     "zzzz bbb zzzz")))))

(ert-deftest emjupy-test-lsp-position-offsets ()
  "LSP counts lines and characters from zero; buffer offsets differ."
  (let ((text "one\ntwo\nthree"))
    (should (= (emjupy--lsp-offset-of-position text 0 0) 0))
    (should (= (emjupy--lsp-offset-of-position text 1 0) 4))
    (should (= (emjupy--lsp-offset-of-position text 2 2) 10))
    ;; past the end clamps rather than signalling: a server being generous
    ;; about a final newline is not a reason to abandon a rename
    (should (= (emjupy--lsp-offset-of-position text 99 99) (length text)))))

(ert-deftest emjupy-test-eglot-websocket-class ()
  "The WebSocket-backed server overrides the transport, not the protocol.

Eglot reaches a server through jsonrpc generics, and
`jsonrpc-connection-send' dispatches on the connection class -- so a
subclass can carry the traffic anywhere.  That is what makes every Eglot
command work at once rather than one at a time."
  (should (memq 'eglot-lsp-server
                (mapcar #'eieio-class-name
                        (eieio-class-parents 'emjupy-eglot-server))))
  ;; the three generics that carry traffic are specialised here
  (dolist (generic '(jsonrpc-connection-send jsonrpc-running-p
                                             jsonrpc-shutdown))
    (should (cl-find-if
             (lambda (m)
               (equal (car (cl--generic-method-specializers m))
                      'emjupy-eglot-server))
             (cl--generic-method-table (cl--generic generic))))))

(ert-deftest emjupy-test-eglot-uri-rewriting ()
  "The document has two names and they have to agree.

Eglot names it by the shadow file on this machine; the language server
knows it by the path the kernel would see.  Every message passes through
the rewrite in one direction or the other."
  (let ((local "file:///tmp/shadow/nb.py")
        (remote "file:///srv/project/nb.emjupy.py"))
    ;; a bare URI
    (should (equal (emjupy--eglot-rewrite-uris local local remote) remote))
    ;; nested in a hash table
    (let* ((doc (make-hash-table :test 'equal))
           (params (make-hash-table :test 'equal)))
      (puthash "uri" local doc)
      (puthash "textDocument" doc params)
      (let ((out (emjupy--eglot-rewrite-uris params local remote)))
        (should (equal (gethash "uri" (gethash "textDocument" out)) remote))))
    ;; and in a plist, which is how Eglot hands them over
    (let ((out (emjupy--eglot-rewrite-uris (list :uri local :line 3)
                                           local remote)))
      (should (equal (plist-get out :uri) remote))
      (should (= (plist-get out :line) 3)))
    ;; anything else is left alone
    (should (equal (emjupy--eglot-rewrite-uris "unrelated" local remote)
                   "unrelated"))))

(ert-deftest emjupy-test-eglot-uris-rewritten-by-directory ()
  "Rewriting is by directory prefix, so the root travels with the document.

The project has to contain the shadow buffer or Eglot will not manage it,
and an unmanaged buffer sends no `didOpen' -- the server never learns the
document exists, and every lookup comes back empty.  So the project stays
local and the paths are corrected on the wire: one substitution fixes
`rootUri', the document URI, and anything else naming a path beneath
them."
  (let ((local "file:///tmp/shadow/")
        (remote "file:///srv/project/"))
    ;; the root itself
    (should (equal (emjupy--eglot-rewrite-uris local local remote) remote))
    ;; a file beneath it
    (should (equal (emjupy--eglot-rewrite-uris
                    (concat local "nb.py") local remote)
                   (concat remote "nb.py")))
    ;; nested, as Eglot sends it
    (let ((params (make-hash-table :test 'equal))
          (doc (make-hash-table :test 'equal)))
      (puthash "uri" (concat local "nb.py") doc)
      (puthash "textDocument" doc params)
      (should (equal (gethash "uri" (gethash "textDocument"
                                             (emjupy--eglot-rewrite-uris
                                              params local remote)))
                     (concat remote "nb.py"))))
    ;; paths elsewhere are left alone
    (should (equal (emjupy--eglot-rewrite-uris "file:///other/x.py"
                                               local remote)
                   "file:///other/x.py"))))

;;;; Performance

(ert-deftest emjupy-test-server-is-told-where-modules-are ()
  "The language server is told the notebook's own directory.

A server finds `import mylib' by looking beside the file that imports
it.  Beside a notebook there is no file -- only a shadow whose name the
server is told -- so whether that directory can be read depends on how
the notebook is reached.  Saying where the modules are does not depend
on that.

The message is built here rather than left to
`eglot-workspace-configuration', which Eglot reads while connecting,
before the buffer counts as one it manages: a value set there arrives as
an empty settings object."
  (let ((nb (make-emjupy-notebook :cells [] :path "n.ipynb"
                                  :kernel-cwd "/srv/project"))
        (sent nil))
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
              ((symbol-function 'jsonrpc-notify)
               (lambda (_server method params)
                 (push (cons method params) sent))))
      (emjupy--set-workspace-configuration nb 'a-server)
      (should sent)
      (let* ((msg (car sent))
             (settings (plist-get (cdr msg) :settings))
             (paths (thread-last settings
                                 (funcall (lambda (s) (plist-get s :pylsp)))
                                 (funcall (lambda (s) (plist-get s :plugins)))
                                 (funcall (lambda (s) (plist-get s :jedi)))
                                 (funcall (lambda (s)
                                            (plist-get s :extra_paths))))))
        (should (eq (car msg) :workspace/didChangeConfiguration))
        (should (equal (append paths nil) '("/srv/project")))))
    ;; nothing is sent when there is nowhere to point at
    (setq sent nil)
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) t))
              ((symbol-function 'jsonrpc-notify)
               (lambda (&rest _) (push t sent))))
      (emjupy--set-workspace-configuration
       (make-emjupy-notebook :cells [] :path "n.ipynb") 'a-server)
      (should-not sent)
      ;; nor when it has been declined
      (let ((emjupy-tell-server-where-modules-are nil))
        (emjupy--set-workspace-configuration nb 'a-server)
        (should-not sent)))))

(ert-deftest emjupy-test-configuration-not-sent-to-a-closed-connection ()
  "Nothing is written to a language server whose connection has gone.

Sending to it signals, and the old code swallowed that; asking first is
the actual answer."
  (let ((nb (make-emjupy-notebook :cells [] :path "n.ipynb"
                                  :kernel-cwd "/srv/p"))
        (sent nil))
    (cl-letf (((symbol-function 'jsonrpc-running-p) (lambda (_) nil))
              ((symbol-function 'jsonrpc-notify) (lambda (&rest _)
                                                   (setq sent t))))
      (emjupy--set-workspace-configuration nb 'a-server)
      (should-not sent))))

;;;; The undo guard

(ert-deftest emjupy-test-lsp-socket-sends-the-headers ()
  "The headers reach `websocket-open', not merely exist."
  (let ((sent 'unset)
        (server (make-emjupy-server :base-url "localhost:9999"
                                    :token "s3cret")))
    (cl-letf (((symbol-function 'websocket-open)
               (lambda (_url &rest args)
                 (setq sent (plist-get args :custom-header-alist))
                 nil)))
      (let ((nb (make-emjupy-notebook :cells [] :path "n.ipynb" :server server))
            (buf (generate-new-buffer " *shadow*")))
        (unwind-protect
            (progn
              (with-current-buffer buf (setq buffer-file-name "/tmp/shadow.py"))
              (ignore-errors (emjupy--eglot-connect nb buf)))
          (let ((kill-buffer-query-functions nil)) (kill-buffer buf)))))
    (should (equal sent '(("Authorization" . "token s3cret"))))))

(ert-deftest emjupy-test-shadow-content-is-built-again-only-when-cells-change ()
  "The shadow content, asked after every command, is rebuilt only when a cell changed."
  (let ((cells (emjupy-test--big-notebook 10 3)) (builds 0))
    (emjupy-test--with-notebook cells buf nb
      (cl-letf* ((real (symbol-function 'emjupy--build-shadow-content-1))
                 ((symbol-function 'emjupy--build-shadow-content-1)
                  (lambda (n) (cl-incf builds) (funcall real n))))
        (let ((first (emjupy--build-shadow-content nb)))
          (dotimes (_ 20) (should (eq (emjupy--build-shadow-content nb) first)))
          (should (= builds 1))
          (setf (emjupy-cell-source (aref (emjupy-notebook-cells nb) 2))
                "changed = True")
          (should (string-match-p "changed = True"
                                  (emjupy--build-shadow-content nb)))
          (should (= builds 2)))))))

(provide 'emjupy-lsp-test)

;;; emjupy-lsp-test.el ends here
