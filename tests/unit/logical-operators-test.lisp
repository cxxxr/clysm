;;;; logical-operators-test.lisp - Unit tests for ANSI CL logical operators
;;;; Feature: 024-equality-predicates
;;;;
;;;; Note: In clysm, T is represented as i31ref(1), which is indistinguishable
;;;; from the integer 1 at the Wasm level. Tests check for truthy values using
;;;; (truthy-p ...) helper since compile-and-run returns an integer for T.

(in-package #:clysm/tests/unit/logical-operators)

;;; Helper to check for truthy (T) values from compile-and-run
(defun truthy-p (result)
  "Check if a compile-and-run result is truthy (T).
   In clysm, T is i31ref(1), so we check for non-nil."
  (not (null result)))

;;; ============================================================
;;; User Story 6: Short-Circuit Boolean Evaluation (Priority: P1)
;;; ============================================================

;;; T085: and with all true values
(deftest and-all-true
  (ok (truthy-p (compile-and-run '(and t t t)))
      "and should return T when all forms are T"))

;;; T086: and returns last value
(deftest and-returns-last-value
  (ok (eql 3 (compile-and-run '(and 1 2 3)))
      "and should return value of last form when all are non-NIL"))

;;; T087: and short-circuits on nil
(deftest and-short-circuits
  (ok (null (compile-and-run '(and t nil t)))
      "and should return NIL when any form is NIL"))

;;; T088: and with no arguments returns t
(deftest and-no-args
  (ok (truthy-p (compile-and-run '(and)))
      "and with no arguments should return T"))

;;; T089: or with all nil returns nil
(deftest or-all-nil
  (ok (null (compile-and-run '(or nil nil nil)))
      "or should return NIL when all forms are NIL"))

;;; T090: or returns first non-nil
(deftest or-returns-first-non-nil
  (ok (eql 5 (compile-and-run '(or nil 5 10)))
      "or should return first non-NIL value"))

;;; T091: or with no arguments returns nil
(deftest or-no-args
  (ok (null (compile-and-run '(or)))
      "or with no arguments should return NIL"))

;;; T092: or short-circuits
(deftest or-short-circuits
  (ok (truthy-p (compile-and-run '(or nil nil t)))
      "or should return T at first true value"))

;;; T093: and/or nesting
(deftest and-or-nesting
  (ok (truthy-p (compile-and-run '(and (or nil t) t)))
      "and/or should nest correctly"))

;;; T094: or/and nesting
(deftest or-and-nesting
  (ok (eql 5 (compile-and-run '(or (and nil t) 5)))
      "or should return 5 when first and evaluates to NIL"))

;;; T095: and with single form
(deftest and-single-form
  (ok (eql 42 (compile-and-run '(and 42)))
      "and with single form should return that form's value"))

;;; T096: or with single form
(deftest or-single-form
  (ok (eql 42 (compile-and-run '(or 42)))
      "or with single form should return that form's value"))
