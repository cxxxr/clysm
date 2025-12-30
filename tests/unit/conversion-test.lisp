;;;; tests/unit/conversion-test.lisp
;;;; Unit tests for numeric type conversion (001-numeric-functions US5)
;;;;
;;;; Tests for float, rational conversion functions

(defpackage #:clysm/tests/unit/conversion
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/conversion)

;;; Use test helpers from main test package
(defun compile-and-run-numeric (form)
  "Compile and run a numeric expression, returning the result."
  (clysm/tests:compile-and-run-numeric form))

(defun approx= (expected actual &optional (epsilon 1d-10))
  "Test if two floating-point numbers are approximately equal."
  (clysm/tests:approx= expected actual epsilon))

;;; ============================================================
;;; Float Conversion Tests (FR-025)
;;; ============================================================

(deftest test-float-fixnum
  "float of fixnum: (float 5) => 5.0"
  (ok (= 5.0d0 (compile-and-run-numeric '(float 5)))
      "(float 5) should equal 5.0"))

(deftest test-float-negative-fixnum
  "float of negative fixnum: (float -42) => -42.0"
  (ok (= -42.0d0 (compile-and-run-numeric '(float -42)))
      "(float -42) should equal -42.0"))

(deftest test-float-ratio
  "float of ratio: (float 1/2) => 0.5"
  (ok (approx= 0.5d0 (compile-and-run-numeric '(float 1/2)))
      "(float 1/2) should equal 0.5"))

(deftest test-float-ratio-thirds
  "float of ratio: (float 1/3) => ~0.333..."
  (let ((result (compile-and-run-numeric '(float 1/3))))
    (ok (approx= (/ 1.0d0 3.0d0) result)
        "(float 1/3) should equal approximately 0.333")))

(deftest test-float-already-float
  "float of float: (float 3.14) => 3.14"
  (ok (= 3.14d0 (compile-and-run-numeric '(float 3.14)))
      "(float 3.14) should equal 3.14"))

(deftest test-float-zero
  "float of zero: (float 0) => 0.0"
  (ok (= 0.0d0 (compile-and-run-numeric '(float 0)))
      "(float 0) should equal 0.0"))

;;; ============================================================
;;; Rational Conversion Tests (FR-026)
;;; ============================================================

(deftest test-rational-half
  "rational of 0.5: (rational 0.5) => 1/2"
  (ok (= 1/2 (compile-and-run-numeric '(rational 0.5)))
      "(rational 0.5) should equal 1/2"))

(deftest test-rational-quarter
  "rational of 0.25: (rational 0.25) => 1/4"
  (ok (= 1/4 (compile-and-run-numeric '(rational 0.25)))
      "(rational 0.25) should equal 1/4"))

(deftest test-rational-fixnum
  "rational of fixnum: (rational 5) => 5"
  (ok (= 5 (compile-and-run-numeric '(rational 5)))
      "(rational 5) should equal 5 (already rational)"))

(deftest test-rational-already-ratio
  "rational of ratio: (rational 3/4) => 3/4"
  (ok (= 3/4 (compile-and-run-numeric '(rational 3/4)))
      "(rational 3/4) should equal 3/4"))

(deftest test-rational-whole-float
  "rational of whole float: (rational 4.0) => 4"
  (ok (= 4 (compile-and-run-numeric '(rational 4.0)))
      "(rational 4.0) should equal 4"))
