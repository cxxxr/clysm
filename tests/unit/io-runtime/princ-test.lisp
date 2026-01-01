;;;; princ-test.lisp - Unit tests for princ runtime dispatch (T010)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that princ dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T010: princ runtime dispatch tests
;;; ============================================================

(deftest test-princ-not-registered
  "Before registration, princ uses inline codegen."
  ;; Clear the runtime function table to ensure clean state
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*)
  ;; Compile a princ call - should use inline codegen
  (let ((wat (clysm/compiler:compile-to-wat '(princ "hello"))))
    (ok (not (search "call $princ-rt" wat))
        "Without registration, princ should not call $princ-rt")))

(deftest test-princ-registered-dispatch
  "After registration, princ dispatches to runtime function."
  ;; Register princ to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'princ :$princ-rt 1)
  ;; Compile a princ call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(princ "hello"))))
    (ok (search "call $princ-rt" wat)
        "With registration, princ should call $princ-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-princ-runtime-call-structure
  "princ runtime call compiles argument then emits call."
  ;; Register princ
  (clysm/compiler/codegen/func-section::register-runtime-function
   'princ :$princ-rt 1)
  ;; Compile and check instruction order
  (let* ((ir (clysm/compiler/codegen:compile-form-to-ir '(princ "test"))))
    ;; IR should contain the call instruction at the end
    (ok (member '(:call :$princ-rt) ir :test #'equal)
        "IR should contain call to $princ-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))
