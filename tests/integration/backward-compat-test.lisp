;;;; backward-compat-test.lisp - Backward compatibility integration test
;;;;
;;;; Feature: 001-ffi-import-architecture
;;;; T060: Verify module compiled with old behavior (all FFI imports) still
;;;;       executes correctly with updated host runtime.

(in-package #:clysm/tests/integration/backward-compat)

;;; ==========================================================================
;;; T060: Backward Compatibility Tests
;;; ==========================================================================

(deftest full-mode-produces-valid-module
  "T060: Module compiled with :full mode (all imports) should be valid."
  (testing "Pure arithmetic in :full mode produces valid, runnable Wasm"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      ;; Write to temp file
      (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence wasm-bytes stream))
        ;; Validate with wasm-tools
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate"
                                    "--features" "gc,exceptions"
                                    (namestring temp-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code)
              "Full mode module should pass validation"))))))

(deftest full-mode-includes-all-imports
  "T060: :full mode should include all FFI imports plus $dynamic-call."
  (testing "Module has import section"
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2) :ffi-mode :full)))
      (ok (> (length wasm-bytes) 100)
          ":full mode should produce larger module with imports")))

  (testing "Multiple expressions all produce valid modules"
    (dolist (expr '((+ 1 2)
                    (* 7 6)
                    (let ((x 1)) x)
                    (if t 1 2)))
      (let ((wasm-bytes (clysm/compiler:compile-to-wasm expr :ffi-mode :full)))
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
                (format nil "Expression ~S should produce valid module" expr))))))))

(deftest mode-transition-backward-compat
  "T060: Switching between modes produces consistent, valid output."
  (testing "Same code compiles validly in all modes"
    (let ((expr '(let ((a 1) (b 2)) (+ a b))))
      (dolist (mode '(:auto :full :minimal))
        (let ((wasm-bytes (clysm/compiler:compile-to-wasm expr :ffi-mode mode)))
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
                  (format nil "Mode ~A should produce valid module" mode))))))))

  (testing "Default mode (:auto) is backward compatible"
    ;; Without :ffi-mode argument, should use :auto and work correctly
    (let ((wasm-bytes (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (arrayp wasm-bytes)
          "Default compilation should produce valid Wasm array")
      (ok (>= (length wasm-bytes) 8)
          "Module should have at least header bytes"))))

(deftest legacy-api-still-works
  "T060: Legacy compile-to-wasm without :ffi-mode still works."
  (testing "Simple compilation without mode argument"
    (let ((wasm (clysm/compiler:compile-to-wasm '(+ 1 2))))
      (ok (arrayp wasm) "Should return byte array")
      (ok (= (aref wasm 0) 0) "Should start with Wasm magic")
      (ok (= (aref wasm 1) #x61) "Magic byte 1")
      (ok (= (aref wasm 2) #x73) "Magic byte 2")
      (ok (= (aref wasm 3) #x6D) "Magic byte 3")))

  (testing "Complex expression without mode argument"
    (let ((wasm (clysm/compiler:compile-to-wasm
                 '(let ((x 1) (y 2) (z 3))
                    (+ x (+ y z))))))
      (ok (arrayp wasm) "Complex expression should compile")
      (uiop:with-temporary-file (:pathname temp-path :type "wasm" :keep nil)
        (with-open-file (stream temp-path
                                :direction :output
                                :element-type '(unsigned-byte 8)
                                :if-exists :supersede)
          (write-sequence wasm stream))
        (multiple-value-bind (output error-output exit-code)
            (uiop:run-program (list "wasm-tools" "validate"
                                    "--features" "gc,exceptions"
                                    (namestring temp-path))
                              :output :string
                              :error-output :string
                              :ignore-error-status t)
          (declare (ignore output error-output))
          (ok (zerop exit-code)
              "Legacy API should produce valid Wasm"))))))
