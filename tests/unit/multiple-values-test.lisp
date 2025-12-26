;;;; multiple-values-test.lisp - Unit tests for ANSI CL multiple values
;;;; Feature: 025-multiple-values
;;;;
;;;; Note: Tests follow TDD - write failing tests first, then implement.
;;;; These tests will FAIL until the values form is implemented.

(in-package #:clysm/tests/unit/multiple-values)

;;; ============================================================
;;; User Story 1: Return Multiple Values with values (Priority: P1)
;;; ============================================================

;;; T010: (values) returning NIL with count=0
(deftest values-no-args-returns-nil
  (testing "values with no arguments should return NIL as primary value"
    (ok (null (compile-and-run '(values)))
        "values with no arguments should return NIL")))

;;; T011: (values 42) returning 42 with count=1
(deftest values-single-arg-returns-value
  (testing "values with single argument should return that value"
    (ok (= (compile-and-run '(values 42)) 42)
        "values with single argument should return 42")))

;;; T012: (values 1 2 3) returning 1 with count=3 (primary value only)
(deftest values-multiple-args-returns-first
  (testing "values with multiple arguments should return first as primary value"
    (ok (= (compile-and-run '(values 1 2 3)) 1)
        "values with multiple arguments should return 1 as primary value")))

;;; Test that values works in tail position
(deftest values-in-tail-position
  (testing "values should work correctly in tail position of a function"
    (let ((result (compile-and-run '(progn (defun foo () (values 10 20)) (foo)))))
      (ok (= result 10)
          "values in function should return primary value 10"))))

;;; ============================================================
;;; User Story 2: Bind Multiple Values (Priority: P1)
;;; Tests for multiple-value-bind (will fail until implemented)
;;; ============================================================

;;; T019: Basic mvb binding two values
(deftest mvb-basic-binding
  (testing "multiple-value-bind should bind variables to values"
    (ok (= (compile-and-run '(multiple-value-bind (a b) (values 1 2) (+ a b))) 3)
        "mvb should bind a=1 and b=2, returning 3")))

;;; T020: mvb with fewer values than variables (NIL fill)
(deftest mvb-fewer-values
  (testing "mvb with fewer values should bind remaining to NIL"
    ;; Use null check instead of symbol comparison across environments
    (ok (null (compile-and-run '(multiple-value-bind (a b c) (values 1 2) c)))
        "c should be NIL when only 2 values provided")))

;;; T021: mvb with more values than variables (extras ignored)
(deftest mvb-more-values
  (testing "mvb with more values should ignore extras"
    (ok (= (compile-and-run '(multiple-value-bind (a) (values 1 2 3) a)) 1)
        "a should be 1, extra values 2 and 3 ignored")))

;;; T022: mvb with empty variable list
(deftest mvb-empty-vars
  (testing "mvb with empty variable list should just evaluate body"
    ;; Use numeric value instead of symbol comparison
    (ok (= (compile-and-run '(multiple-value-bind () (values 1 2) 42)) 42)
        "body should return 42 with empty variable list")))

;;; ============================================================
;;; User Story 8: Arithmetic Functions (Priority: P1)
;;; Tests for floor/truncate/ceiling/round with multiple values
;;; ============================================================

;;; T029: floor returning quotient and remainder
(deftest floor-returns-quotient
  (testing "floor should return quotient as primary value"
    (ok (= (compile-and-run '(floor 7 3)) 2)
        "floor 7 3 should return 2 as quotient")))

;;; T030: truncate with negative numbers
(deftest truncate-negative
  (testing "truncate should return truncated quotient"
    (ok (= (compile-and-run '(truncate -7 3)) -2)
        "truncate -7 3 should return -2")))

;;; T031: ceiling returning quotient and remainder
(deftest ceiling-returns-quotient
  (testing "ceiling should return quotient as primary value"
    (ok (= (compile-and-run '(ceiling 7 3)) 3)
        "ceiling 7 3 should return 3 as quotient")))

;;; T032: round returning quotient and remainder
(deftest round-returns-quotient
  (testing "round should return quotient as primary value"
    (ok (= (compile-and-run '(round 7 3)) 2)
        "round 7 3 should return 2 as quotient")))

;;; ============================================================
;;; User Story 3: Collect as List (Priority: P2)
;;; Tests for multiple-value-list
;;; ============================================================

;;; T039: mvl with zero values
(deftest mvl-zero-values
  (testing "multiple-value-list with zero values should return NIL"
    (ok (null (compile-and-run '(multiple-value-list (values))))
        "mvl of (values) should return NIL")))

;;; T040: mvl with multiple values
(deftest mvl-multiple-values
  (testing "multiple-value-list should collect all values into a list"
    (let ((result (compile-and-run '(car (multiple-value-list (values 1 2 3))))))
      (ok (= result 1)
          "first element of mvl should be 1"))))

;;; T041: mvl with floor result
(deftest mvl-floor-result
  (testing "multiple-value-list should work with floor"
    (let ((result (compile-and-run '(car (multiple-value-list (floor 7 3))))))
      (ok (= result 2)
          "first element of mvl(floor 7 3) should be 2"))))

;;; Additional test: verify mvl returns a list of correct length
(deftest mvl-list-length
  (testing "multiple-value-list should return proper list structure"
    ;; Check that car cdr works correctly
    (let ((second (compile-and-run '(car (cdr (multiple-value-list (values 10 20 30)))))))
      (ok (= second 20)
          "second element should be 20"))))

;;; ============================================================
;;; User Story 4: Access by Index (Priority: P2)
;;; Tests for nth-value
;;; ============================================================

;;; T046: nth-value index 0 (primary)
(deftest nth-value-primary
  (testing "nth-value 0 should return primary value"
    (ok (= (compile-and-run '(nth-value 0 (values 10 20 30))) 10)
        "nth-value 0 should return 10")))

;;; T047: nth-value index 1 (secondary)
(deftest nth-value-secondary
  (testing "nth-value 1 should return second value"
    (ok (= (compile-and-run '(nth-value 1 (values 10 20 30))) 20)
        "nth-value 1 should return 20")))

;;; T048: nth-value index out of range (returns NIL)
(deftest nth-value-out-of-range
  (testing "nth-value with out of range index should return NIL"
    (ok (null (compile-and-run '(nth-value 5 (values 10 20))))
        "nth-value 5 should return NIL when only 2 values")))

;;; ============================================================
;;; User Story 5: Spread List as Values (Priority: P2)
;;; Tests for values-list
;;; ============================================================

;;; T053: values-list with nil
(deftest values-list-nil
  (testing "values-list with nil should return NIL"
    (ok (null (compile-and-run '(values-list nil)))
        "values-list nil should return NIL")))

;;; T054: values-list with proper list
(deftest values-list-proper-list
  (testing "values-list should spread list as values"
    (ok (= (compile-and-run '(values-list (list 42))) 42)
        "values-list (list 42) should return 42")))

;;; T055: values-list returning first element
(deftest values-list-first-element
  (testing "values-list should return first element as primary value"
    (ok (= (compile-and-run '(values-list (list 1 2 3))) 1)
        "values-list (list 1 2 3) should return 1 as primary")))

;;; ============================================================
;;; User Story 6: Preserve Values (Priority: P2)
;;; Tests for multiple-value-prog1
;;; ============================================================

;;; T061: mvp1 preserving primary value
(deftest mvp1-preserve-primary
  (testing "multiple-value-prog1 should return first form's primary value"
    (ok (= (compile-and-run '(multiple-value-prog1 (values 10 20) 999)) 10)
        "mvp1 should return 10 from first form, ignoring 999")))

;;; T062: mvp1 with single value form
(deftest mvp1-single-value
  (testing "multiple-value-prog1 should work with single value"
    (ok (= (compile-and-run '(multiple-value-prog1 42 (+ 1 2))) 42)
        "mvp1 should return 42")))

;;; ============================================================
;;; User Story 7: Pass Values to Functions (Priority: P3)
;;; Tests for multiple-value-call
;;; ============================================================

;;; T069: mvc with single form
(deftest mvc-single-form
  (testing "multiple-value-call with single form"
    (ok (= (compile-and-run '(multiple-value-call #'+ (values 1 2 3))) 6)
        "mvc + (values 1 2 3) should return 6")))

;;; T070: mvc with multiple forms
(deftest mvc-multiple-forms
  (testing "multiple-value-call with multiple forms"
    (ok (= (compile-and-run '(multiple-value-call #'+ (values 1 2) (values 3 4))) 10)
        "mvc + (values 1 2) (values 3 4) should return 10")))

;;; T071: mvc with floor result
(deftest mvc-floor-result
  (testing "multiple-value-call with floor"
    (ok (= (compile-and-run '(multiple-value-call #'+ (floor 7 3))) 3)
        "mvc + (floor 7 3) should return 2+1=3")))
