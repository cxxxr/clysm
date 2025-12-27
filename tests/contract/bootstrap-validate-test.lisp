;;;; bootstrap-validate-test.lisp - Contract tests for wasm-tools validation (T015)
;;;; TDD: Tests written first per Constitution VII

(in-package :clysm/tests/contract/bootstrap-validate)

;;; ============================================================
;;; T015: Output binary validates with wasm-tools
;;; ============================================================

(deftest wasm-tools-validates-simple-module
  "wasm-tools validate should succeed on simple compiled modules"
  (testing "basic arithmetic module validates"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (with-temp-wasm-file (path bytes)
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate" "--features" "gc" path)
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code) "wasm-tools validate should exit with 0"))))))

(deftest wasm-tools-validates-complex-module
  "wasm-tools validate should succeed on modules with functions"
  (testing "defun module validates"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (defun square (x) (* x x))
                    (square 5)))))
      (with-temp-wasm-file (path bytes)
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate" "--features" "gc" path)
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code) "wasm-tools validate should exit with 0"))))))

(deftest wasm-tools-validates-control-flow
  "wasm-tools validate should succeed on modules with control flow"
  (testing "if/when module validates"
    (let ((bytes (clysm/compiler:compile-to-wasm
                  '(progn
                    (if (> 5 3) 1 2)))))
      (with-temp-wasm-file (path bytes)
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate" "--features" "gc" path)
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code) "wasm-tools validate should exit with 0"))))))

(deftest validate-output-function-works
  "The validate-output function from bootstrap.lisp should work"
  (testing "validate-output returns true for valid wasm"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (with-temp-wasm-file (path bytes)
        (multiple-value-bind (valid-p error-msg)
            (clysm/bootstrap:validate-output path)
          (declare (ignore error-msg))
          (ok valid-p "validate-output should return T for valid wasm"))))))

(deftest wasm-has-valid-header
  "Compiled wasm should have valid magic number and version"
  (testing "magic number is 0x00 0x61 0x73 0x6d"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (>= (length bytes) 8) "Should have at least 8 bytes")
      (ok (= #x00 (aref bytes 0)) "First byte should be 0x00")
      (ok (= #x61 (aref bytes 1)) "Second byte should be 0x61")
      (ok (= #x73 (aref bytes 2)) "Third byte should be 0x73")
      (ok (= #x6d (aref bytes 3)) "Fourth byte should be 0x6d")))
  (testing "version is 1"
    (let ((bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (= #x01 (aref bytes 4)) "Version byte 1 should be 0x01")
      (ok (= #x00 (aref bytes 5)) "Version byte 2 should be 0x00")
      (ok (= #x00 (aref bytes 6)) "Version byte 3 should be 0x00")
      (ok (= #x00 (aref bytes 7)) "Version byte 4 should be 0x00"))))
