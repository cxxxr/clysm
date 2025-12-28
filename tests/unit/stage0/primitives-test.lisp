;;;; primitives-test.lisp - Unit tests for Stage 0 primitives
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Tests T009: Primitive operations skeleton
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(defpackage #:clysm/tests/unit/stage0/primitives-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:eval-form
                #:make-env))

(in-package #:clysm/tests/unit/stage0/primitives-test)

;;; ============================================================
;;; T014-T017 [US1]: Arithmetic Primitive Tests
;;; ============================================================

(deftest test-prim-add
  "Verify addition: (+ 1 2) -> 3"
  (let ((result (eval-form '(+ 1 2) (make-env))))
    (ok (eql result 3) "(+ 1 2) should evaluate to 3")))

(deftest test-prim-add-multiple
  "Verify multi-arg addition: (+ 1 2 3 4) -> 10"
  (let ((result (eval-form '(+ 1 2 3 4) (make-env))))
    (ok (eql result 10) "(+ 1 2 3 4) should evaluate to 10")))

(deftest test-prim-add-zero-args
  "Verify zero-arg addition: (+) -> 0"
  (let ((result (eval-form '(+) (make-env))))
    (ok (eql result 0) "(+) should evaluate to 0")))

(deftest test-prim-sub
  "Verify subtraction: (- 5 3) -> 2"
  (let ((result (eval-form '(- 5 3) (make-env))))
    (ok (eql result 2) "(- 5 3) should evaluate to 2")))

(deftest test-prim-sub-single
  "Verify single-arg subtraction (negation): (- 5) -> -5"
  (let ((result (eval-form '(- 5) (make-env))))
    (ok (eql result -5) "(- 5) should evaluate to -5")))

(deftest test-prim-mul
  "Verify multiplication: (* 3 4) -> 12"
  (let ((result (eval-form '(* 3 4) (make-env))))
    (ok (eql result 12) "(* 3 4) should evaluate to 12")))

(deftest test-prim-mul-zero-args
  "Verify zero-arg multiplication: (*) -> 1"
  (let ((result (eval-form '(*) (make-env))))
    (ok (eql result 1) "(*) should evaluate to 1")))

(deftest test-prim-div
  "Verify division: (/ 10 2) -> 5"
  (let ((result (eval-form '(/ 10 2) (make-env))))
    (ok (eql result 5) "(/ 10 2) should evaluate to 5")))

(deftest test-prim-div-multiple
  "Verify multi-arg division: (/ 100 2 5) -> 10"
  (let ((result (eval-form '(/ 100 2 5) (make-env))))
    (ok (eql result 10) "(/ 100 2 5) should evaluate to 10")))

;;; ============================================================
;;; T058-T060 [US4]: Comparison Primitive Tests
;;; ============================================================

(deftest test-prim-lt-true
  "Verify less-than true: (< 1 2) -> T"
  (let ((result (eval-form '(< 1 2) (make-env))))
    (ok result "(< 1 2) should be true")))

(deftest test-prim-lt-false
  "Verify less-than false: (< 2 1) -> NIL"
  (let ((result (eval-form '(< 2 1) (make-env))))
    (ok (null result) "(< 2 1) should be NIL")))

(deftest test-prim-gt-true
  "Verify greater-than true: (> 2 1) -> T"
  (let ((result (eval-form '(> 2 1) (make-env))))
    (ok result "(> 2 1) should be true")))

(deftest test-prim-gt-false
  "Verify greater-than false: (> 1 2) -> NIL"
  (let ((result (eval-form '(> 1 2) (make-env))))
    (ok (null result) "(> 1 2) should be NIL")))

(deftest test-prim-num-eq-true
  "Verify numeric equality true: (= 5 5) -> T"
  (let ((result (eval-form '(= 5 5) (make-env))))
    (ok result "(= 5 5) should be true")))

(deftest test-prim-num-eq-false
  "Verify numeric equality false: (= 5 6) -> NIL"
  (let ((result (eval-form '(= 5 6) (make-env))))
    (ok (null result) "(= 5 6) should be NIL")))

;;; ============================================================
;;; T076-T080 [US6]: List Primitive Tests
;;; ============================================================

(deftest test-prim-cons
  "Verify cons creates cell: (cons 1 2) creates cons"
  (let ((result (eval-form '(cons 1 2) (make-env))))
    (ok (consp result) "(cons 1 2) should create a cons cell")
    (ok (eql (car result) 1) "car should be 1")
    (ok (eql (cdr result) 2) "cdr should be 2")))

(deftest test-prim-car
  "Verify car extracts first: (car (cons 1 2)) -> 1"
  (let ((result (eval-form '(car (cons 1 2)) (make-env))))
    (ok (eql result 1) "(car (cons 1 2)) should return 1")))

(deftest test-prim-cdr
  "Verify cdr extracts rest: (cdr (cons 1 2)) -> 2"
  (let ((result (eval-form '(cdr (cons 1 2)) (make-env))))
    (ok (eql result 2) "(cdr (cons 1 2)) should return 2")))

(deftest test-prim-eq-same-symbol
  "Verify eq same symbol: (eq 'a 'a) -> T"
  (let ((result (eval-form '(eq 'a 'a) (make-env))))
    (ok result "(eq 'a 'a) should be true")))

(deftest test-prim-eq-different-symbols
  "Verify eq different symbols: (eq 'a 'b) -> NIL"
  (let ((result (eval-form '(eq 'a 'b) (make-env))))
    (ok (null result) "(eq 'a 'b) should be NIL")))

(deftest test-prim-eq-same-fixnum
  "Verify eq same fixnum: (eq 5 5) -> T"
  (let ((result (eval-form '(eq 5 5) (make-env))))
    (ok result "(eq 5 5) should be true")))

;;; ============================================================
;;; Edge Cases
;;; ============================================================

(deftest test-nested-list-operations
  "Verify nested list operations"
  (let ((result (eval-form '(car (cdr (cons 1 (cons 2 3)))) (make-env))))
    (ok (eql result 2) "(car (cdr (cons 1 (cons 2 3)))) should be 2")))

(deftest test-comparison-chain
  "Verify comparison with arithmetic: (< (+ 1 1) (* 2 2)) -> T"
  (let ((result (eval-form '(< (+ 1 1) (* 2 2)) (make-env))))
    (ok result "(< (+ 1 1) (* 2 2)) should be true")))
