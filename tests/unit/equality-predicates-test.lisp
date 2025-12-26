;;;; equality-predicates-test.lisp - Unit tests for ANSI CL equality predicates
;;;; Feature: 024-equality-predicates
;;;;
;;;; Note: In clysm, T is represented as i31ref(1), which is indistinguishable
;;;; from the integer 1 at the Wasm level. Tests check for truthy values using
;;;; (not (null ...)) pattern since compile-and-run returns an integer for T.

(in-package #:clysm/tests/unit/equality-predicates)

;;; Helper to check for truthy (T) values from compile-and-run
;;; T is represented as i31ref(1), so we check for non-nil
(defun truthy-p (result)
  "Check if a compile-and-run result is truthy (T).
   In clysm, T is i31ref(1), so we check for non-nil."
  (not (null result)))

;;; ============================================================
;;; User Story 5: Logical Negation with not (Priority: P1)
;;; ============================================================

;;; T019: not with nil
(deftest not-nil-returns-t
  (ok (truthy-p (compile-and-run '(not nil)))
      "not should return T for NIL"))

;;; T020: not with t
(deftest not-t-returns-nil
  (ok (null (compile-and-run '(not t)))
      "not should return NIL for T"))

;;; T021: not with non-nil object
(deftest not-symbol-returns-nil
  (ok (null (compile-and-run '(not 'foo)))
      "not should return NIL for any non-NIL object"))

(deftest not-number-returns-nil
  (ok (null (compile-and-run '(not 42)))
      "not should return NIL for number"))

;;; ============================================================
;;; User Story 1: Pointer Identity with eq (Priority: P1)
;;; ============================================================

;;; T030: eq with identical symbols
(deftest eq-identical-symbols
  (ok (truthy-p (compile-and-run '(eq 'foo 'foo)))
      "eq should return T for identical symbols"))

;;; T031: eq with nil
(deftest eq-nil-nil
  (ok (truthy-p (compile-and-run '(eq nil nil)))
      "eq should return T for nil and nil"))

;;; T032: eq with different symbols
(deftest eq-different-symbols
  (ok (null (compile-and-run '(eq 'foo 'bar)))
      "eq should return NIL for different symbols"))

;;; T033: eq with fixnums (implementation-defined but consistent)
(deftest eq-same-fixnums
  (ok (truthy-p (compile-and-run '(eq 42 42)))
      "eq should return T for same fixnums (i31ref optimization)"))

;;; T034: eq with different cons cells
(deftest eq-different-cons
  (ok (null (compile-and-run '(eq (cons 1 2) (cons 1 2))))
      "eq should return NIL for different cons cells with same content"))

;;; T035: eq with same cons reference
(deftest eq-same-cons-reference
  (ok (truthy-p (compile-and-run '(let ((x (cons 1 2))) (eq x x))))
      "eq should return T for same cons reference"))

;;; ============================================================
;;; User Story 2: Type-Aware Value Comparison with eql (Priority: P1)
;;; ============================================================

;;; T043: eql with same fixnums
(deftest eql-same-fixnums
  (ok (truthy-p (compile-and-run '(eql 42 42)))
      "eql should return T for same fixnums"))

;;; T044: eql with fixnum and float
(deftest eql-fixnum-float-same-value
  (ok (null (compile-and-run '(eql 1 1.0)))
      "eql should return NIL for fixnum and float even with same numeric value"))

;;; T045: eql with same floats
(deftest eql-same-floats
  (ok (truthy-p (compile-and-run '(eql 3.14 3.14)))
      "eql should return T for same floats"))

;;; T046: eql with same characters
(deftest eql-same-characters
  (ok (truthy-p (compile-and-run '(eql #\a #\a)))
      "eql should return T for same characters"))

;;; T047: eql with different characters
(deftest eql-different-characters
  (ok (null (compile-and-run '(eql #\a #\b)))
      "eql should return NIL for different characters"))

;;; T048: eql with same ratios
(deftest eql-same-ratios
  (ok (truthy-p (compile-and-run '(eql 2/3 2/3)))
      "eql should return T for same ratios"))

;;; T049: eql delegates to eq for non-numbers
(deftest eql-symbols-delegates-to-eq
  (ok (truthy-p (compile-and-run '(eql 'foo 'foo)))
      "eql should return T for same symbols (delegates to eq)"))

;;; ============================================================
;;; User Story 3: Structural Equality with equal (Priority: P2)
;;; ============================================================

;;; T060: equal with cons cells
(deftest equal-cons-same-content
  (ok (truthy-p (compile-and-run '(equal (cons 1 2) (cons 1 2))))
      "equal should return T for cons cells with same content"))

;;; T061: equal with nested lists
(deftest equal-nested-lists
  (ok (truthy-p (compile-and-run '(equal '(1 (2 3)) '(1 (2 3)))))
      "equal should return T for nested lists with same structure"))

;;; T062: equal with strings
(deftest equal-same-strings
  (ok (truthy-p (compile-and-run '(equal "hello" "hello")))
      "equal should return T for strings with same content"))

;;; T063: equal with different strings
(deftest equal-different-strings
  (ok (null (compile-and-run '(equal "hello" "world")))
      "equal should return NIL for different strings"))

;;; T064: equal falls back to eql for non-cons non-string
(deftest equal-numbers-fallback
  (ok (truthy-p (compile-and-run '(equal 42 42)))
      "equal should return T for same numbers (falls back to eql)"))

(deftest equal-different-type-numbers
  (ok (null (compile-and-run '(equal 1 1.0)))
      "equal should return NIL for different type numbers (via eql)"))

;;; ============================================================
;;; User Story 4: Case-Insensitive Comparison with equalp (Priority: P3)
;;; ============================================================

;;; T075: equalp with strings differing in case
(deftest equalp-case-insensitive-strings
  (ok (truthy-p (compile-and-run '(equalp "Hello" "hello")))
      "equalp should return T for strings differing only in case"))

;;; T076: equalp with numbers of different types
(deftest equalp-fixnum-float-same-value
  (ok (truthy-p (compile-and-run '(equalp 1 1.0)))
      "equalp should return T for fixnum and float with same numeric value"))

;;; T077: equalp with characters differing in case
(deftest equalp-case-insensitive-characters
  (ok (truthy-p (compile-and-run '(equalp #\A #\a)))
      "equalp should return T for characters differing only in case"))

;;; T078: equalp with nested structures (using cons/list since quoted lists with strings aren't supported)
(deftest equalp-nested-structures
  (ok (truthy-p (compile-and-run '(equalp (cons "Hello" (cons 1 nil)) (cons "HELLO" (cons 1.0 nil)))))
      "equalp should return T for nested structures with equalp-equivalent elements"))
