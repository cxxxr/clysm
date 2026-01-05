;;;; tests/e2e/cons-test.lisp - E2E Tests for Cons Cell Operations
;;;;
;;;; Tests that cons cell primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-cons-suite "E2E tests for cons cell operations")

;;; ============================================================
;;; Basic Cons Tests
;;; ============================================================

(deftest test-e2e-cons-simple ()
  "Simple cons cell creation"
  (is-e2e "(cons 1 2)" "(1 . 2)")
  (is-e2e "(cons 'a 'b)" "(A . B)"))

(deftest test-e2e-cons-with-nil ()
  "Cons with NIL creates proper list"
  (is-e2e "(cons 1 nil)" "(1)")
  (is-e2e "(cons 'a nil)" "(A)")
  (is-e2e "(cons nil nil)" "(NIL)"))

(deftest test-e2e-cons-build-list ()
  "Building lists with cons"
  (is-e2e "(cons 1 (cons 2 nil))" "(1 2)")
  (is-e2e "(cons 1 (cons 2 (cons 3 nil)))" "(1 2 3)")
  (is-e2e "(cons 'a (cons 'b (cons 'c nil)))" "(A B C)"))

(deftest test-e2e-cons-nested ()
  "Nested cons cells"
  (is-e2e "(cons (cons 1 2) (cons 3 4))" "((1 . 2) 3 . 4)")
  (is-e2e "(cons (cons 1 nil) nil)" "((1))"))

;;; ============================================================
;;; CAR Tests
;;; ============================================================

(deftest test-e2e-car-dotted ()
  "CAR of dotted pair"
  (is-e2e "(car (cons 1 2))" "1")
  (is-e2e "(car (cons 'a 'b))" "A"))

(deftest test-e2e-car-list ()
  "CAR of list"
  (is-e2e "(car '(a b c))" "A")
  (is-e2e "(car '(1 2 3))" "1"))

(deftest test-e2e-car-nested ()
  "CAR of nested list"
  (is-e2e "(car '((1 2) 3 4))" "(1 2)")
  (is-e2e "(car (cons (cons 1 2) nil))" "(1 . 2)"))

;;; ============================================================
;;; CDR Tests
;;; ============================================================

(deftest test-e2e-cdr-dotted ()
  "CDR of dotted pair"
  (is-e2e "(cdr (cons 1 2))" "2")
  (is-e2e "(cdr (cons 'a 'b))" "B"))

(deftest test-e2e-cdr-list ()
  "CDR of list"
  (is-e2e "(cdr '(a b c))" "(B C)")
  (is-e2e "(cdr '(1 2 3))" "(2 3)"))

(deftest test-e2e-cdr-single ()
  "CDR of single-element list"
  (is-e2e "(cdr '(a))" "NIL")
  (is-e2e "(cdr (cons 1 nil))" "NIL"))

;;; ============================================================
;;; CAR/CDR Combinations
;;; ============================================================

(deftest test-e2e-car-cdr-combo ()
  "Combined CAR and CDR"
  (is-e2e "(car (cdr '(1 2 3)))" "2")
  (is-e2e "(car (cdr (cdr '(1 2 3))))" "3")
  (is-e2e "(cdr (cdr '(1 2 3)))" "(3)"))

(deftest test-e2e-cadr ()
  "CADR pattern (second element)"
  (is-e2e "(car (cdr '(a b c)))" "B"))

(deftest test-e2e-caddr ()
  "CADDR pattern (third element)"
  (is-e2e "(car (cdr (cdr '(a b c))))" "C"))

;;; ============================================================
;;; RPLACA/RPLACD Tests
;;; ============================================================

(deftest test-e2e-rplaca ()
  "RPLACA (replace CAR)"
  (is-e2e "(let ((c (cons 1 2))) (rplaca c 10) (car c))" "10")
  (is-e2e "(let ((c (cons 'a 'b))) (rplaca c 'x) c)" "(X . B)"))

(deftest test-e2e-rplacd ()
  "RPLACD (replace CDR)"
  (is-e2e "(let ((c (cons 1 2))) (rplacd c 20) (cdr c))" "20")
  (is-e2e "(let ((c (cons 'a 'b))) (rplacd c 'y) c)" "(A . Y)"))

(deftest test-e2e-rplaca-list ()
  "RPLACA on list"
  (is-e2e "(let ((c (cons 1 (cons 2 nil)))) (rplaca c 10) c)" "(10 2)"))

;;; ============================================================
;;; List Predicate Tests
;;; ============================================================

(deftest test-e2e-null-true ()
  "NULL predicate - true cases"
  (is-e2e "(null nil)" "T")
  (is-e2e "(null '())" "T"))

(deftest test-e2e-null-false ()
  "NULL predicate - false cases"
  (is-e2e "(null '(1))" "NIL")
  (is-e2e "(null (cons 1 2))" "NIL")
  (is-e2e "(null t)" "NIL")
  (is-e2e "(null 0)" "NIL"))

(deftest test-e2e-consp-true ()
  "CONSP predicate - true cases"
  (is-e2e "(consp (cons 1 2))" "T")
  (is-e2e "(consp '(a))" "T")
  (is-e2e "(consp '(1 2 3))" "T"))

(deftest test-e2e-consp-false ()
  "CONSP predicate - false cases"
  (is-e2e "(consp nil)" "NIL")
  (is-e2e "(consp 42)" "NIL")
  (is-e2e "(consp 'foo)" "NIL"))

(deftest test-e2e-atom-true ()
  "ATOM predicate - true cases"
  (is-e2e "(atom 42)" "T")
  (is-e2e "(atom 'foo)" "T")
  (is-e2e "(atom nil)" "T")
  (is-e2e "(atom t)" "T"))

(deftest test-e2e-atom-false ()
  "ATOM predicate - false cases"
  (is-e2e "(atom (cons 1 2))" "NIL")
  (is-e2e "(atom '(a b))" "NIL"))

(deftest test-e2e-listp-true ()
  "LISTP predicate - true cases"
  (is-e2e "(listp nil)" "T")
  (is-e2e "(listp '())" "T")
  (is-e2e "(listp '(1 2))" "T")
  (is-e2e "(listp (cons 1 2))" "T"))

(deftest test-e2e-listp-false ()
  "LISTP predicate - false cases"
  (is-e2e "(listp 42)" "NIL")
  (is-e2e "(listp 'foo)" "NIL"))
