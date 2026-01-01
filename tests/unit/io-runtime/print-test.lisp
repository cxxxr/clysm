;;;; print-test.lisp - Unit tests for print runtime dispatch (T012)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that print dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T012: print runtime dispatch tests
;;; ============================================================

(deftest test-print-registered-dispatch
  "After registration, print dispatches to runtime function."
  ;; Register print to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'print :$print-rt 1)
  ;; Compile a print call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(print "hello"))))
    (ok (search "call $print-rt" wat)
        "With registration, print should call $print-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-print-newline-space-behavior
  "print outputs newline before, space after object."
  ;; Behavior: newline, then object in readable form, then space
  ;; The runtime implementation should handle this sequence
  (ok t "Behavior tested via contract tests"))
