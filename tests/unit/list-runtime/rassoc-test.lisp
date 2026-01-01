;;;; rassoc-test.lisp - Unit tests for rassoc runtime dispatch (T031)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that rassoc dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_rassoc.htm

(in-package #:clysm/tests/unit/list-runtime)

;;; ============================================================
;;; T031: rassoc runtime dispatch tests
;;; ============================================================

(deftest test-rassoc-registered-dispatch
  "After registration, rassoc dispatches to runtime function."
  ;; Register rassoc to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'rassoc :$rassoc-rt nil)
  ;; Compile a rassoc call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(rassoc 2 '((a . 1) (b . 2))))))
    (ok (search "call $rassoc-rt" wat)
        "With registration, rassoc should call $rassoc-rt"))
  ;; Clean up
  (clysm/compiler/codegen/func-section::clear-runtime-functions))
