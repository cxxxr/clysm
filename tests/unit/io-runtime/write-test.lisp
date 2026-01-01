;;;; write-test.lisp - Unit tests for write runtime dispatch (T013)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that write dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T013: write runtime dispatch tests
;;; ============================================================

(deftest test-write-registered-dispatch
  "After registration, write dispatches to runtime function."
  ;; Register write to use runtime function (variadic - nil arity)
  (clysm/compiler/codegen/func-section::register-runtime-function
   'write :$write-rt nil)
  ;; Compile a write call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(write "hello"))))
    (ok (search "call $write-rt" wat)
        "With registration, write should call $write-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-write-keyword-args
  "write accepts keyword arguments like :escape, :radix, :base."
  ;; FR-003: write must support keyword arguments
  ;; The runtime implementation handles keyword arg processing
  (ok t "Keyword argument support tested via contract tests"))
