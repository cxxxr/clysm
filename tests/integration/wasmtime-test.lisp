;;;; wasmtime-test.lisp - Integration tests for direct wasmtime execution
;;;;
;;;; Feature: 022-wasm-import-optimization
;;;; Tasks: T016-T017
;;;;
;;;; These tests verify that non-I/O code can execute directly in wasmtime
;;;; without requiring a host shim to provide FFI imports.

(in-package #:clysm/tests/integration/wasmtime)

;;; ==========================================================================
;;; Helper: Run compiled Wasm in wasmtime
;;; ==========================================================================

(defun run-in-wasmtime (wasm-bytes)
  "Execute WASM-BYTES in wasmtime and return the i32 result.
Returns (values result-integer success-p error-message)."
  (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
    (with-open-file (stream temp-path
                            :direction :output
                            :element-type '(unsigned-byte 8)
                            :if-exists :supersede)
      (write-sequence wasm-bytes stream))
    (multiple-value-bind (output error-output exit-code)
        (uiop:run-program (list "wasmtime"
                                "--wasm" "gc"
                                "--wasm" "exceptions"
                                "--invoke" "_start"
                                (namestring temp-path))
                          :output :string
                          :error-output :string
                          :ignore-error-status t)
      (if (zerop exit-code)
          ;; Parse the output - wasmtime prints the return value
          (let* ((trimmed (string-trim '(#\Space #\Newline #\Return) output))
                 (result (ignore-errors (parse-integer trimmed))))
            (if result
                (values result t nil)
                (values nil nil (format nil "Could not parse output: ~A" output))))
          (values nil nil (format nil "wasmtime failed: ~A~%~A" output error-output))))))

;;; ==========================================================================
;;; T016: (+ 1 2) executes in wasmtime returning 3
;;; ==========================================================================

(deftest wasmtime-addition
  (testing "(+ 1 2) executes in wasmtime returning 3"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (multiple-value-bind (result success-p error-msg)
          (run-in-wasmtime wasm-bytes)
        (ok success-p (or error-msg "wasmtime execution failed"))
        (ok (eql result 3)
            (format nil "Expected 3, got ~A" result))))))

;;; ==========================================================================
;;; T017: (* 7 6) executes in wasmtime returning 42
;;; ==========================================================================

(deftest wasmtime-multiplication
  (testing "(* 7 6) executes in wasmtime returning 42"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(* 7 6))))
      (multiple-value-bind (result success-p error-msg)
          (run-in-wasmtime wasm-bytes)
        (ok success-p (or error-msg "wasmtime execution failed"))
        (ok (eql result 42)
            (format nil "Expected 42, got ~A" result))))))

;;; ==========================================================================
;;; Additional arithmetic tests
;;; ==========================================================================

(deftest wasmtime-subtraction
  (testing "(- 10 3) executes in wasmtime returning 7"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(- 10 3))))
      (multiple-value-bind (result success-p error-msg)
          (run-in-wasmtime wasm-bytes)
        (ok success-p (or error-msg "wasmtime execution failed"))
        (ok (eql result 7)
            (format nil "Expected 7, got ~A" result))))))

(deftest wasmtime-nested-arithmetic
  (testing "(+ (* 2 3) (- 10 5)) executes in wasmtime returning 11"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ (* 2 3) (- 10 5)))))
      (multiple-value-bind (result success-p error-msg)
          (run-in-wasmtime wasm-bytes)
        (ok success-p (or error-msg "wasmtime execution failed"))
        (ok (eql result 11)
            (format nil "Expected 11, got ~A" result))))))

(deftest wasmtime-constant
  (testing "42 executes in wasmtime returning 42"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '42)))
      (multiple-value-bind (result success-p error-msg)
          (run-in-wasmtime wasm-bytes)
        (ok success-p (or error-msg "wasmtime execution failed"))
        (ok (eql result 42)
            (format nil "Expected 42, got ~A" result))))))
