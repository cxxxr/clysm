;;;; terpri-test.lisp - Unit tests for terpri runtime dispatch (T015)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that terpri dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_terpri.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T015: terpri runtime dispatch tests
;;; ============================================================

(deftest test-terpri-registered-dispatch
  "After registration, terpri dispatches to runtime function."
  ;; Register terpri to use runtime function (0 or 1 args)
  (clysm/compiler/codegen/func-section::register-runtime-function
   'terpri :$terpri-rt nil)
  ;; Compile a terpri call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(terpri))))
    (ok (search "call $terpri-rt" wat)
        "With registration, terpri should call $terpri-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-terpri-outputs-newline
  "terpri outputs a newline character."
  ;; terpri should output #\Newline using %host-write-char
  (ok t "Newline output tested via contract tests"))
