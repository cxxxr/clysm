;;;; bootstrap-full-test.lisp - Integration tests for full bootstrap (T016)
;;;; TDD: Tests written first per Constitution VII

(in-package :clysm/tests/integration/bootstrap-full)

;;; ============================================================
;;; T016: Full bootstrap produces valid wasm
;;; ============================================================

(deftest bootstrap-context-can-be-created
  "bootstrap-context should be creatable and usable"
  (testing "make-bootstrap-context creates valid context"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context)))
      (ok ctx "Context should be created")
      (ok (clysm/bootstrap:bootstrap-context-p ctx) "Should be a bootstrap-context")))

  (testing "context has expected default output path"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context)))
      (ok (string= "dist/clysm-stage0.wasm"
                   (clysm/bootstrap:bootstrap-context-output-path ctx))
          "Default output path should be dist/clysm-stage0.wasm"))))

(deftest collect-all-forms-reads-modules
  "collect-all-forms should read forms from source modules"
  (testing "collects forms into context"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context)))
      (let ((forms (clysm/bootstrap:collect-all-forms ctx)))
        (ok (listp forms) "Should return a list")
        (ok (> (length forms) 0) "Should collect some forms")
        (ok (clysm/bootstrap:bootstrap-context-all-forms ctx)
            "Context should have all-forms set")))))

(deftest compile-all-forms-produces-bytes
  "compile-all-forms should produce wasm bytes"
  (testing "compiles small set of forms"
    (let ((forms '((defun test-fn (x) (+ x 1))
                   (test-fn 41))))
      (let ((bytes (clysm/bootstrap:compile-all-forms forms)))
        (ok (arrayp bytes) "Should return an array")
        (ok (> (length bytes) 0) "Should produce non-empty output")
        (ok (typep bytes '(array (unsigned-byte 8)))
            "Should be byte array")))))

(deftest write-wasm-output-creates-file
  "write-wasm-output should write bytes to file"
  (testing "creates file at specified path"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2)))
          (test-path "/tmp/clysm-bootstrap-test.wasm"))
      (unwind-protect
           (progn
             (clysm/bootstrap:write-wasm-output bytes test-path)
             (ok (probe-file test-path) "File should exist")
             (let ((file-size (with-open-file (s test-path :element-type '(unsigned-byte 8))
                                (file-length s))))
               (ok (= (length bytes) file-size)
                   "File size should match bytes length")))
        (when (probe-file test-path)
          (delete-file test-path))))))

(deftest run-bootstrap-produces-output
  "run-bootstrap should produce valid wasm output"
  (testing "runs bootstrap to completion"
    ;; This test runs the full bootstrap but to a temp location
    (let ((test-path "/tmp/clysm-bootstrap-full-test.wasm"))
      (unwind-protect
           (let ((result (clysm/bootstrap:run-bootstrap :output-path test-path)))
             ;; run-bootstrap returns the output path on success, nil on failure
             (ok result "Bootstrap should complete successfully")
             (when result
               (ok (probe-file test-path) "Output file should exist")
               ;; Validate the output
               (multiple-value-bind (valid-p error-msg)
                   (clysm/bootstrap:validate-output test-path)
                 (declare (ignore error-msg))
                 (ok valid-p "Output should validate with wasm-tools"))))
        (when (probe-file test-path)
          (delete-file test-path))))))

(deftest bootstrap-handles-empty-forms
  "compile-all-forms should handle empty forms list"
  (testing "returns empty array for no forms"
    (let ((bytes (clysm/bootstrap:compile-all-forms nil)))
      (ok (arrayp bytes) "Should return an array")
      (ok (= 0 (length bytes)) "Empty forms should produce empty output"))))

(deftest bootstrap-error-info-on-failure
  "bootstrap-context should capture error info on failure"
  (testing "error-info is nil initially"
    (let ((ctx (clysm/bootstrap:make-bootstrap-context)))
      (ok (null (clysm/bootstrap:bootstrap-context-error-info ctx))
          "error-info should be nil initially"))))
