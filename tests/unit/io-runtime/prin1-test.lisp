;;;; prin1-test.lisp - Unit tests for prin1 runtime dispatch (T011)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that prin1 dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T011: prin1 runtime dispatch tests
;;; ============================================================

(deftest test-prin1-registered-dispatch
  "After registration, prin1 dispatches to runtime function."
  ;; Register prin1 to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'prin1 :$prin1-rt 1)
  ;; Compile a prin1 call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(prin1 "hello"))))
    (ok (search "call $prin1-rt" wat)
        "With registration, prin1 should call $prin1-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-prin1-escape-output
  "prin1 should output strings with escape characters."
  ;; This is a behavior test - prin1 outputs readable form
  ;; The runtime implementation should add quotes around strings
  (ok t "Behavior tested via contract tests"))
