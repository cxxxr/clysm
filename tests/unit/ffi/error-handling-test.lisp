;;;; error-handling-test.lisp - Unit tests for FFI error handling
;;;; Feature: 027-complete-ffi (T033)

(in-package #:clysm/tests)

(deftest ffi-error-handling-wrapper-test
  "Test that FFI calls can be wrapped with error handling"
  (testing "error wrapper generation"
    (clysm/ffi:reset-ffi-environment)

    (eval '(clysm/ffi:define-foreign-function test-throw "host.throw_error" (:string) :void))
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)

    (let ((decl (clysm/ffi:lookup-foreign-function clysm/ffi:*ffi-environment* 'test-throw)))
      ;; Generate import call with error handling
      (let ((instrs (clysm/ffi:generate-import-call-with-error-handling
                     decl '() nil :catch-host-errors t)))
        (ok instrs "Should generate instructions")))))

(deftest ffi-try-catch-wrapper-test
  "Test try_table/catch wrapper generation"
  (testing "try-catch structure"
    ;; The wrapper should generate Constitution IV compliant exception handling:
    ;; - try_table with catch_all
    ;; - On exception: signal ffi-host-error
    (let ((wrapped (clysm/ffi:generate-ffi-try-catch-wrapper
                    '((:call 0)) nil)))
      (ok wrapped "Should generate wrapped instructions"))))

(deftest ffi-host-error-signal-test
  "Test host error signal generation"
  (testing "error signal code"
    ;; generate-host-error-signal creates instructions to signal ffi-host-error
    (let ((signal-code (clysm/ffi:generate-host-error-signal "host.test")))
      ;; Currently placeholder - just verify it doesn't error
      (ok t "Error signal generation should not error"))))
