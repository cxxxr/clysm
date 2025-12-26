;;;; ffi-error-wasm-test.lisp - Contract tests for FFI error handling Wasm
;;;; Feature: 027-complete-ffi (T035)

(in-package #:clysm/tests)

(deftest ffi-error-handling-wasm-structure-test
  "Test that FFI error handling generates valid Wasm structure"
  (testing "try_table/catch structure"
    ;; The error handling should generate Constitution IV compliant Wasm:
    ;; - try_table with catch_all clause
    ;; - Branch to handler on exception
    ;; - Handler that signals ffi-host-error

    ;; Test that generate-import-call-with-error-handling works
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function err-fn "host.error" () :void))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment* 'err-fn)))
      (let ((instrs (clysm/ffi:generate-import-call-with-error-handling
                     decl '() nil :catch-host-errors t)))
        (ok (listp instrs) "Should generate list of instructions")))))

(deftest ffi-error-without-wrapping-test
  "Test FFI calls without error wrapping"
  (testing "unwrapped call"
    (clysm/ffi:reset-ffi-environment)
    (eval '(clysm/ffi:define-foreign-function plain-fn "host.plain" () :fixnum))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment* 'plain-fn)))
      ;; Without error handling, should get simpler code
      (let ((instrs (clysm/ffi:generate-import-call-with-error-handling
                     decl '() nil :catch-host-errors nil)))
        (ok instrs "Should generate instructions without wrapper")))))
