;;;; rationalize-wasm-test.lisp - Contract tests for rationalize Wasm output
;;;; Feature: 001-numeric-format (Phase 14C)

(in-package #:clysm/tests/contract/rationalize-wasm)

;;; ============================================================
;;; T011: Contract tests for rationalize Wasm generation
;;; ============================================================

(deftest rationalize-generates-valid-wasm
  (testing "rationalize compilation produces valid Wasm"
    (let* ((result (compile-and-validate '(rationalize 0.5))))
      (ok (getf result :valid)
          "rationalize should generate valid Wasm"))))

(deftest rationalize-uses-type-dispatch
  (testing "rationalize uses ref.test for type dispatch"
    (let* ((wat (compile-to-wat '(rationalize x)))
           (has-ref-test (search "ref.test" wat)))
      (ok has-ref-test
          "rationalize should use ref.test for type checking"))))

(deftest rationalize-constructs-ratio
  (testing "rationalize constructs ratio with struct.new"
    (let* ((wat (compile-to-wat '(rationalize 0.5)))
           ;; Type index 15 is $ratio
           (has-struct-new (or (search "struct.new 15" wat)
                               (search "struct.new $ratio" wat))))
      (ok has-struct-new
          "rationalize should construct ratio using struct.new"))))

(deftest rationalize-handles-passthrough
  (testing "rationalize passes through integer unchanged"
    (let* ((wat (compile-to-wat '(rationalize 5)))
           ;; Should have early return for integer type
           (has-i31-test (search "ref.test" wat)))
      (ok has-i31-test
          "rationalize should test for integer type"))))

;;; Helper functions
(defun compile-and-validate (form)
  "Compile form and validate resulting Wasm"
  (handler-case
      (let ((wasm (clysm:compile-to-wasm form)))
        (list :valid (validate-wasm-silent wasm)
              :wasm wasm))
    (error (c)
      (list :valid nil :error (princ-to-string c)))))

(defun compile-to-wat (form)
  "Compile form and return WAT representation"
  (handler-case
      (let ((wasm (clysm:compile-to-wasm form)))
        (wasm-to-wat wasm))
    (error (c)
      (format nil "Compilation error: ~A" c))))

(defun wasm-to-wat (wasm)
  "Convert Wasm binary to WAT text format using wasm-tools"
  (declare (ignore wasm))
  ;; Placeholder - actual implementation would use external tool
  "")
