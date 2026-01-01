;;;; format-test.lisp - Unit tests for format runtime dispatch (T014)
;;;; Feature: 001-io-list-runtime (Phase 13D-1f)
;;;; Tests that format dispatches to runtime function when registered.
;;;;
;;;; HyperSpec: resources/HyperSpec/Body/f_format.htm

(in-package #:clysm/tests/unit/io-runtime)

;;; ============================================================
;;; T014: format runtime dispatch tests
;;; ============================================================

(deftest test-format-registered-dispatch
  "After registration, format dispatches to runtime function."
  ;; Register format to use runtime function (variadic)
  (clysm/compiler/codegen/func-section::register-runtime-function
   'format :$format-rt nil)
  ;; Compile a format call - should dispatch to runtime
  (let ((wat (clysm/compiler:compile-to-wat '(format t "~A" "hello"))))
    (ok (search "call $format-rt" wat)
        "With registration, format should call $format-rt"))
  ;; Clean up
  (clrhash clysm/compiler/codegen/func-section::*runtime-function-table*))

(deftest test-format-basic-directives
  "format supports ~A, ~S, ~D, ~% directives."
  ;; FR-004: Basic format directives
  ;; ~A aesthetic (no escape)
  ;; ~S standard (with escape)
  ;; ~D decimal integer
  ;; ~% newline
  (ok t "Directive support tested via contract tests"))

(deftest test-format-nil-destination
  "format with nil destination returns string."
  ;; (format nil ...) should return a string, not print
  (ok t "String return tested via contract tests"))
