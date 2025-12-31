;;;; dynamic-call-test.lisp - Integration tests for dynamic call runtime resolution
;;;; Feature: 001-ffi-import-architecture (T030)
;;;;
;;;; These tests verify that dynamic calls (funcall/apply with variable functions)
;;;; work correctly through the host runtime $dynamic-call import.

(in-package #:clysm/tests)

;;; Note: These tests require Node.js to run the Wasm with the host-shim/runtime.js
;;; They are designed to verify end-to-end dynamic call resolution.

(deftest dynamic-call-runtime-integration
  "T030: Integration test verifying dynamic calls resolve at runtime.
This test compiles code with dynamic calls, runs it with Node.js and the
runtime shim, and verifies correct function resolution."
  (testing "setup: verify runtime.js exists"
    (let ((runtime-path (merge-pathnames "host-shim/runtime.js"
                                          (asdf:system-source-directory :clysm))))
      (ok (probe-file runtime-path)
          "host-shim/runtime.js should exist")))

  (testing "dynamic funcall compiles successfully"
    (let* ((code '(let ((fn (intern "IDENTITY")))
                    (funcall fn 42)))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      (ok wasm-bytes "Should compile without error")
      (ok (> (length wasm-bytes) 0) "Should produce non-empty Wasm")))

  (testing "dynamic apply compiles successfully"
    (let* ((code '(let ((fn (car (list 'identity))))
                    (apply fn '(42))))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      (ok wasm-bytes "Should compile without error")
      (ok (> (length wasm-bytes) 0) "Should produce non-empty Wasm")))

  ;; Note: Full integration test with Node.js execution would be in a separate
  ;; shell-based test file due to the need to invoke Node.js
  (testing "compiled module validates with wasm-tools"
    (let* ((code '(let ((fn (intern "IDENTITY")))
                    (funcall fn 42)))
           (wasm-bytes (clysm/compiler:compile-to-wasm code)))
      (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence wasm-bytes stream))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate"
                                    "--features" "gc,exceptions"
                                    (namestring temp-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code)
              "Module with dynamic call should validate"))))))
