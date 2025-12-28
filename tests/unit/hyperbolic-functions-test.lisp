;;;; tests/unit/hyperbolic-functions-test.lisp
;;;; Unit tests for hyperbolic functions (001-numeric-functions US5)
;;;;
;;;; Tests for sinh, cosh, tanh, asinh, acosh, atanh

(in-package #:clysm/tests/unit/hyperbolic-functions)

;;; ============================================================
;;; Sinh Tests (FR-013)
;;; ============================================================

(deftest test-sinh-zero
  "sinh of zero: (sinh 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(sinh 0)))
      "(sinh 0) should equal 0"))

(deftest test-sinh-one
  "sinh of one: (sinh 1) => ~1.1752"
  (let ((result (compile-and-run-numeric '(sinh 1))))
    (ok (approx= (sinh 1.0d0) result)
        "(sinh 1) should equal approximately 1.1752")))

(deftest test-sinh-negative
  "sinh of negative: (sinh -1) => ~-1.1752"
  (let ((result (compile-and-run-numeric '(sinh -1))))
    (ok (approx= (sinh -1.0d0) result)
        "(sinh -1) should equal approximately -1.1752")))

;;; ============================================================
;;; Cosh Tests (FR-014)
;;; ============================================================

(deftest test-cosh-zero
  "cosh of zero: (cosh 0) => 1"
  (ok (approx= 1.0d0 (compile-and-run-numeric '(cosh 0)))
      "(cosh 0) should equal 1"))

(deftest test-cosh-one
  "cosh of one: (cosh 1) => ~1.5431"
  (let ((result (compile-and-run-numeric '(cosh 1))))
    (ok (approx= (cosh 1.0d0) result)
        "(cosh 1) should equal approximately 1.5431")))

(deftest test-cosh-negative
  "cosh of negative: (cosh -1) => ~1.5431 (even function)"
  (let ((result (compile-and-run-numeric '(cosh -1))))
    (ok (approx= (cosh -1.0d0) result)
        "(cosh -1) should equal approximately 1.5431")))

;;; ============================================================
;;; Tanh Tests (FR-015)
;;; ============================================================

(deftest test-tanh-zero
  "tanh of zero: (tanh 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(tanh 0)))
      "(tanh 0) should equal 0"))

(deftest test-tanh-one
  "tanh of one: (tanh 1) => ~0.7616"
  (let ((result (compile-and-run-numeric '(tanh 1))))
    (ok (approx= (tanh 1.0d0) result)
        "(tanh 1) should equal approximately 0.7616")))

(deftest test-tanh-large
  "tanh of large: (tanh 10) => ~1.0"
  (let ((result (compile-and-run-numeric '(tanh 10))))
    (ok (approx= 1.0d0 result 1d-6)
        "(tanh 10) should be very close to 1.0")))

;;; ============================================================
;;; Asinh Tests (FR-016)
;;; ============================================================

(deftest test-asinh-zero
  "asinh of zero: (asinh 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(asinh 0)))
      "(asinh 0) should equal 0"))

(deftest test-asinh-one
  "asinh of one: (asinh 1) => ~0.8814"
  (let ((result (compile-and-run-numeric '(asinh 1))))
    (ok (approx= (asinh 1.0d0) result)
        "(asinh 1) should equal approximately 0.8814")))

(deftest test-asinh-negative
  "asinh of negative: (asinh -1) => ~-0.8814"
  (let ((result (compile-and-run-numeric '(asinh -1))))
    (ok (approx= (asinh -1.0d0) result)
        "(asinh -1) should equal approximately -0.8814")))

;;; ============================================================
;;; Acosh Tests (FR-017)
;;; ============================================================

(deftest test-acosh-one
  "acosh of one: (acosh 1) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(acosh 1)))
      "(acosh 1) should equal 0"))

(deftest test-acosh-two
  "acosh of two: (acosh 2) => ~1.3170"
  (let ((result (compile-and-run-numeric '(acosh 2))))
    (ok (approx= (acosh 2.0d0) result)
        "(acosh 2) should equal approximately 1.3170")))

;;; ============================================================
;;; Atanh Tests (FR-018)
;;; ============================================================

(deftest test-atanh-zero
  "atanh of zero: (atanh 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(atanh 0)))
      "(atanh 0) should equal 0"))

(deftest test-atanh-half
  "atanh of 0.5: (atanh 0.5) => ~0.5493"
  (let ((result (compile-and-run-numeric '(atanh 0.5))))
    (ok (approx= (atanh 0.5d0) result)
        "(atanh 0.5) should equal approximately 0.5493")))

(deftest test-atanh-negative-half
  "atanh of -0.5: (atanh -0.5) => ~-0.5493"
  (let ((result (compile-and-run-numeric '(atanh -0.5))))
    (ok (approx= (atanh -0.5d0) result)
        "(atanh -0.5) should equal approximately -0.5493")))
