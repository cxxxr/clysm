;;;; assoc-test.lisp - Unit tests for assoc runtime dispatch (T030)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that assoc dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_assocc.htm

(in-package #:clysm/tests/unit/list-runtime)

;;; ============================================================
;;; T030: assoc runtime dispatch tests
;;; ============================================================

(deftest test-assoc-registered-dispatch
  "After registration, assoc dispatches to runtime function."
  ;; Register assoc to use runtime function
  (clysm/compiler/codegen/func-section::register-runtime-function
   'assoc :$assoc-rt nil)
  ;; Compile an assoc call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(assoc 'b '((a . 1) (b . 2))))))
    (ok (search "call $assoc-rt" wat)
        "With registration, assoc should call $assoc-rt"))
  ;; Clean up
  (clysm/compiler/codegen/func-section::clear-runtime-functions))

(deftest test-assoc-with-key-keyword
  "assoc with :key keyword dispatches to runtime."
  (clysm/compiler/codegen/func-section::register-runtime-function
   'assoc :$assoc-rt nil)
  ;; assoc with :key
  (let ((wat (clysm/compiler:compile-to-wat '(assoc 'b alist :key #'car))))
    (ok (search "call $assoc-rt" wat)
        "assoc with :key should call $assoc-rt"))
  (clysm/compiler/codegen/func-section::clear-runtime-functions))
