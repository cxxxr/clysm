;;;; rationalize-test.lisp - Unit tests for rationalize function
;;;; Feature: 001-numeric-format (Phase 14C)

(in-package #:clysm/tests/unit/numeric/rationalize)

;;; ============================================================
;;; User Story 1: Float to Rational Conversion
;;; ============================================================

;;; T010: rationalize tests - TDD (write first, must fail until implemented)

(deftest rationalize-simple-decimal
  (testing "rationalize 0.5 should return 1/2"
    (let ((result (compile-and-run '(rationalize 0.5))))
      (ok (eql result 1/2)
          "rationalize 0.5 should produce exactly 1/2"))))

(deftest rationalize-integer-float
  (testing "rationalize 3.0 should return integer 3"
    (let ((result (compile-and-run '(rationalize 3.0))))
      (ok (eql result 3)
          "rationalize 3.0 should produce integer 3, not ratio"))))

(deftest rationalize-repeating-decimal
  (testing "rationalize 0.333333 should return simple ratio approximation"
    (let ((result (compile-and-run '(rationalize 0.333333))))
      (ok (rationalp result)
          "rationalize should return a rational")
      ;; The result should be close to 1/3 with small denominator
      (ok (< (abs (- result 1/3)) 0.001)
          "rationalize 0.333333 should be close to 1/3"))))

(deftest rationalize-ratio-passthrough
  (testing "rationalize of ratio should return unchanged"
    (let ((result (compile-and-run '(rationalize 1/2))))
      (ok (eql result 1/2)
          "rationalize 1/2 should return 1/2 unchanged"))))

(deftest rationalize-integer-passthrough
  (testing "rationalize of integer should return unchanged"
    (let ((result (compile-and-run '(rationalize 5))))
      (ok (eql result 5)
          "rationalize 5 should return 5 unchanged"))))

(deftest rationalize-negative-float
  (testing "rationalize of negative float"
    (let ((result (compile-and-run '(rationalize -0.5))))
      (ok (eql result -1/2)
          "rationalize -0.5 should produce -1/2"))))

(deftest rationalize-small-float
  (testing "rationalize of small float near zero"
    (let ((result (compile-and-run '(rationalize 0.001))))
      (ok (rationalp result)
          "rationalize should return a rational for small values")
      (ok (< (abs (- result 0.001)) 0.0001)
          "rationalize 0.001 should be close to original value"))))

(deftest rationalize-quarter
  (testing "rationalize 0.25 should return 1/4"
    (let ((result (compile-and-run '(rationalize 0.25))))
      (ok (eql result 1/4)
          "rationalize 0.25 should produce 1/4"))))

(deftest rationalize-zero
  (testing "rationalize 0 should return 0"
    (let ((result (compile-and-run '(rationalize 0))))
      (ok (eql result 0)
          "rationalize 0 should return 0"))))

(deftest rationalize-one
  (testing "rationalize 1.0 should return 1"
    (let ((result (compile-and-run '(rationalize 1.0))))
      (ok (eql result 1)
          "rationalize 1.0 should return integer 1"))))
