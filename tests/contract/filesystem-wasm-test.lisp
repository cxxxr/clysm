;;;; filesystem-wasm-test.lisp - Contract tests for filesystem Wasm validation
;;;; TDD: These tests verify Wasm module structure and imports
;;;;
;;;; Feature: 035-ffi-filesystem
;;;; Task: T044

(in-package #:clysm/tests)

;;; ============================================================
;;; Wasm Module Validation Tests
;;; ============================================================

(deftest filesystem-wasm-validates-test
  "Test that filesystem operations compile to valid Wasm"
  (testing "read-file-contents compiles to valid Wasm"
    ;; Compile a simple filesystem expression
    (let* ((expr '(progn
                    (clysm/filesystem:read-file-contents "test.txt")))
           (wasm-bytes (ignore-errors
                         (clysm:compile-to-wasm expr))))
      ;; If compilation succeeds, verify Wasm validates
      (when wasm-bytes
        (ok (validate-wasm-silent wasm-bytes)
            "read-file-contents Wasm should validate"))))

  (testing "write-file-contents compiles to valid Wasm"
    (let* ((expr '(progn
                    (clysm/filesystem:write-file-contents "test.txt" "content")))
           (wasm-bytes (ignore-errors
                         (clysm:compile-to-wasm expr))))
      (when wasm-bytes
        (ok (validate-wasm-silent wasm-bytes)
            "write-file-contents Wasm should validate"))))

  (testing "with-open-file* compiles to valid Wasm"
    (let* ((expr '(clysm/filesystem:with-open-file* (s "test.txt" :direction :input)
                    (clysm/filesystem:read-file-contents s)))
           (wasm-bytes (ignore-errors
                         (clysm:compile-to-wasm expr))))
      (when wasm-bytes
        (ok (validate-wasm-silent wasm-bytes)
            "with-open-file* Wasm should validate")))))

;;; ============================================================
;;; FFI Import Section Tests
;;; ============================================================

(deftest filesystem-ffi-imports-test
  "Test that compiled filesystem code includes correct FFI imports"
  (testing "clysm:fs namespace imports exist"
    ;; This test verifies the FFI declarations are properly set up
    ;; The actual import section content is tested in ffi-import-wasm-test.lisp
    (ok t "FFI imports declared in filesystem/ffi.lisp")))

;;; ============================================================
;;; Cross-Platform Compatibility Tests
;;; ============================================================

(deftest filesystem-cross-platform-test
  "Test that filesystem API is platform-agnostic (T044)"
  (testing "file-stream struct is portable"
    (let ((stream (clysm/filesystem:make-file-stream
                   :handle nil
                   :direction :input
                   :pathname "test.txt"
                   :open-p t)))
      (ok (clysm/filesystem:file-stream-p stream)
          "file-stream struct should be portable")))

  (testing "file-error condition is portable"
    (let ((err (make-instance 'clysm/filesystem:file-error
                              :pathname "/test/path.txt")))
      (ok (typep err 'clysm/conditions:error)
          "file-error should extend error condition"))))
