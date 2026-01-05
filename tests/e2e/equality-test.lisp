;;;; tests/e2e/equality-test.lisp - E2E Tests for Equality Operations
;;;;
;;;; Tests that equality primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-equality-suite "E2E tests for equality operations")

;;; ============================================================
;;; EQ Tests (Identity)
;;; ============================================================

(deftest test-e2e-eq-same-symbol ()
  "EQ - same symbol"
  (is-e2e "(eq 'foo 'foo)" "T")
  (is-e2e "(eq 'bar 'bar)" "T"))

(deftest test-e2e-eq-different-symbol ()
  "EQ - different symbols"
  (is-e2e "(eq 'foo 'bar)" "NIL"))

(deftest test-e2e-eq-same-fixnum ()
  "EQ - same fixnum (implementation dependent but typically T for small integers)"
  (is-e2e "(eq 42 42)" "T")
  (is-e2e "(eq 0 0)" "T")
  (is-e2e "(eq -1 -1)" "T"))

(deftest test-e2e-eq-different-fixnum ()
  "EQ - different fixnums"
  (is-e2e "(eq 1 2)" "NIL")
  (is-e2e "(eq 0 1)" "NIL"))

(deftest test-e2e-eq-nil ()
  "EQ - NIL comparisons"
  (is-e2e "(eq nil nil)" "T")
  (is-e2e "(eq '() '())" "T")
  (is-e2e "(eq nil '())" "T"))

(deftest test-e2e-eq-t ()
  "EQ - T comparisons"
  (is-e2e "(eq t t)" "T"))

(deftest test-e2e-eq-nil-vs-t ()
  "EQ - NIL vs T"
  (is-e2e "(eq nil t)" "NIL")
  (is-e2e "(eq t nil)" "NIL"))

(deftest test-e2e-eq-variable ()
  "EQ - same variable"
  (is-e2e "(let ((x 'a)) (eq x x))" "T"))

;;; ============================================================
;;; EQL Tests (Same as EQ for most types)
;;; ============================================================

(deftest test-e2e-eql-same-symbol ()
  "EQL - same symbol"
  (is-e2e "(eql 'foo 'foo)" "T"))

(deftest test-e2e-eql-different-symbol ()
  "EQL - different symbols"
  (is-e2e "(eql 'foo 'bar)" "NIL"))

(deftest test-e2e-eql-same-fixnum ()
  "EQL - same fixnum"
  (is-e2e "(eql 42 42)" "T")
  (is-e2e "(eql -123 -123)" "T"))

(deftest test-e2e-eql-different-fixnum ()
  "EQL - different fixnums"
  (is-e2e "(eql 1 2)" "NIL"))

(deftest test-e2e-eql-nil ()
  "EQL - NIL"
  (is-e2e "(eql nil nil)" "T"))

(deftest test-e2e-eql-t ()
  "EQL - T"
  (is-e2e "(eql t t)" "T"))

;;; ============================================================
;;; Equality in Expressions
;;; ============================================================

(deftest test-e2e-eq-computed ()
  "EQ with computed values"
  (is-e2e "(let ((x 'foo)) (let ((y x)) (eq x y)))" "T"))

(deftest test-e2e-eql-computed ()
  "EQL with computed values"
  (is-e2e "(eql (+ 1 2) (+ 2 1))" "T")
  (is-e2e "(eql (* 2 3) (+ 3 3))" "T"))

;;; ============================================================
;;; NOT Tests
;;; ============================================================

(deftest test-e2e-not-nil ()
  "NOT of NIL is T"
  (is-e2e "(null nil)" "T"))

(deftest test-e2e-not-t ()
  "NOT of T is NIL"
  (is-e2e "(null t)" "NIL"))

(deftest test-e2e-not-truthy ()
  "NOT of truthy values is NIL"
  (is-e2e "(null 42)" "NIL")
  (is-e2e "(null 'foo)" "NIL")
  (is-e2e "(null (cons 1 2))" "NIL"))
