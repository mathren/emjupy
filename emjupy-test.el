;;; emjupy-test.el --- Tests for emjupy.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'emjupy)
(require 'cl-lib)

;;; 1. Data Structure Tests

(ert-deftest emjupy-test-struct-creation ()
  "Ensure structs initialize correctly."
  (let ((server (make-emjupy-server :host "localhost" :port 8888 :token "abc")))
    (should (equal (emjupy-server-host server) "localhost"))
    (should (equal (emjupy-server-token server) "abc"))))

;;; 2. HTTP Layer Tests

(ert-deftest emjupy-test-http-request-success ()
  "Test that the HTTP helper correctly parses a valid JSON response."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (with-current-buffer (generate-new-buffer " *temp-url*")
                 ;; Simulate standard HTTP response
                 (insert "HTTP/1.1 200 OK\nContent-Type: application/json\n\n{\"id\": \"kernel-123\"}")
                 (current-buffer)))))
    (let* ((server (make-emjupy-server :base-url "http://localhost:8888"))
           (response (emjupy--http-request server "POST" "/api/kernels")))
      (should (hash-table-p response))
      (should (equal (gethash "id" response) "kernel-123")))))

(ert-deftest emjupy-test-http-request-error ()
  "Test that the HTTP helper throws an error on 403 Forbidden."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (with-current-buffer (generate-new-buffer " *temp-url*")
                 (insert "HTTP/1.1 403 Forbidden\n\n{\"message\": \"Forbidden\"}")
                 (current-buffer)))))
    (let ((server (make-emjupy-server :base-url "http://localhost:8888")))
      (should-error (emjupy--http-request server "POST" "/api/kernels")
                    :type 'error))))

;;; 3. WebSocket Dispatch Tests

(ert-deftest emjupy-test-dispatch-message ()
  "Test that incoming Jupyter wire messages execute the correct callbacks."
  (let* ((kernel (make-emjupy-kernel :pending (make-hash-table :test 'equal)))
         (executed-type nil)
         (executed-text nil)
         (dummy-frame "dummy"))

    ;; Register a mock callback waiting for msg_id 'msg-abc'
    (puthash "msg-abc"
             (lambda (type content)
               (setq executed-type type)
               (setq executed-text (gethash "text" content)))
             (emjupy-kernel-pending kernel))

    ;; Mock the websocket-frame-text to return a valid Jupyter stream payload
    (cl-letf (((symbol-function 'websocket-frame-text)
               (lambda (_) "{\"channel\":\"shell\",\"header\":{\"msg_type\":\"stream\"},\"parent_header\":{\"msg_id\":\"msg-abc\"},\"content\":{\"text\":\"hello world\"}}")))

      (emjupy--dispatch-message kernel dummy-frame)

      (should (equal executed-type "stream"))
      (should (equal executed-text "hello world")))))

;;; 4. Buffer & Rendering Tests

(ert-deftest emjupy-test-render-initial-cells ()
  "Test that a raw ipynb JSON structure is correctly translated into buffer text and properties."
  (let ((nb-data (json-parse-string
                  "{\"cells\": [{\"cell_type\":\"code\",\"source\":\"print('MESA')\"}]}"
                  :object-type 'hash-table :array-type 'list)))
    (with-temp-buffer
      (setq-local emjupy--current-notebook (make-emjupy-notebook))
      (emjupy--render-initial-cells nb-data)

      ;; Buffer should not be empty
      (should (> (buffer-size) 0))

      ;; Source code should be inserted
      (should (string-match-p "print('MESA')" (buffer-string)))

      ;; The struct array should be populated
      (should (= (length (emjupy-notebook-cells emjupy--current-notebook)) 1)))))

(ert-deftest emjupy-test-update-cell-output ()
  "Test that updating a cell mutates the overlay display property correctly."
  (with-temp-buffer
    (let* ((ov (make-overlay (point) (point)))
           (cell (make-emjupy-cell :output-overlay ov))
           (dummy-outputs '(((output_type . "stream") (text . "stellar_mass = 15")))))

      (emjupy-update-cell-output cell dummy-outputs)

      ;; Verify the overlay has a display property containing our text
      (let ((display-prop (overlay-get ov 'display)))
        (should (stringp display-prop))
        (should (string-match-p "stellar_mass = 15" display-prop))))))

;;; 5. Navigation Tests

(ert-deftest emjupy-test-navigation ()
  "Test C-c C-n and C-c C-p navigation between cell boundaries."
  (with-temp-buffer
    ;; Setup dummy cells using text properties
    (insert "Cell 1 text\n")
    (put-text-property (point-min) (point) 'emjupy-cell 'cell-1-id)
    (let ((mid-point (point)))
      (insert "Gap text\n")
      (let ((start-cell-2 (point)))
        (insert "Cell 2 text\n")
        (put-text-property start-cell-2 (point) 'emjupy-cell 'cell-2-id)

        ;; Test jumping forward
        (goto-char (point-min))
        (emjupy-next-cell)
        (should (= (point) start-cell-2))

        ;; Test jumping backward
        (emjupy-prev-cell)
        (should (= (point) (point-min)))))))

(provide 'emjupy-test)
;;; emjupy-test.el ends here
