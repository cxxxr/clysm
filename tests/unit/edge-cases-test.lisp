;;;; tests/unit/edge-cases-test.lisp
;;;; Edge case tests for NaN and Infinity inputs (001-numeric-functions T089)
;;;;
;;;; Tests for handling special floating-point values

(in-package #:clysm/tests/unit/edge-cases)

;;; ============================================================
;;; Helper for checking NaN
;;; ============================================================

(defun nan-p (x)
  "Check if X is NaN"
  (and (floatp x) (sb-ext:float-nan-p x)))

(defun infinity-p (x)
  "Check if X is positive or negative infinity"
  (and (floatp x)
       (or (sb-ext:float-infinity-p x))))

;;; ============================================================
;;; NaN Input Tests
;;; ============================================================

(deftest test-sin-nan
  "sin of NaN returns NaN"
  (let ((result (compile-and-run-numeric '(sin (/ 0.0d0 0.0d0)))))
    (ok (nan-p result)
        "(sin NaN) should return NaN")))

(deftest test-cos-nan
  "cos of NaN returns NaN"
  (let ((result (compile-and-run-numeric '(cos (/ 0.0d0 0.0d0)))))
    (ok (nan-p result)
        "(cos NaN) should return NaN")))

(deftest test-sqrt-nan
  "sqrt of NaN returns NaN"
  (let ((result (compile-and-run-numeric '(sqrt (/ 0.0d0 0.0d0)))))
    (ok (nan-p result)
        "(sqrt NaN) should return NaN")))

(deftest test-sqrt-negative
  "sqrt of negative returns NaN"
  (let ((result (compile-and-run-numeric '(sqrt -1.0d0))))
    (ok (nan-p result)
        "(sqrt -1.0) should return NaN")))

(deftest test-log-nan
  "log of NaN returns NaN"
  (let ((result (compile-and-run-numeric '(log (/ 0.0d0 0.0d0)))))
    (ok (nan-p result)
        "(log NaN) should return NaN")))

(deftest test-exp-nan
  "exp of NaN returns NaN"
  (let ((result (compile-and-run-numeric '(exp (/ 0.0d0 0.0d0)))))
    (ok (nan-p result)
        "(exp NaN) should return NaN")))

;;; ============================================================
;;; Infinity Input Tests
;;; ============================================================

(deftest test-exp-positive-infinity
  "exp of positive infinity returns infinity"
  (let ((result (compile-and-run-numeric '(exp (/ 1.0d0 0.0d0)))))
    (ok (infinity-p result)
        "(exp +inf) should return infinity")))

(deftest test-exp-negative-infinity
  "exp of negative infinity returns 0"
  (let ((result (compile-and-run-numeric '(exp (/ -1.0d0 0.0d0)))))
    (ok (approx= 0.0d0 result)
        "(exp -inf) should return 0")))

(deftest test-log-zero
  "log of zero returns negative infinity"
  (let ((result (compile-and-run-numeric '(log 0.0d0))))
    (ok (and (infinity-p result) (< result 0))
        "(log 0.0) should return negative infinity")))

(deftest test-log-infinity
  "log of positive infinity returns infinity"
  (let ((result (compile-and-run-numeric '(log (/ 1.0d0 0.0d0)))))
    (ok (infinity-p result)
        "(log +inf) should return infinity")))

(deftest test-sqrt-infinity
  "sqrt of positive infinity returns infinity"
  (let ((result (compile-and-run-numeric '(sqrt (/ 1.0d0 0.0d0)))))
    (ok (infinity-p result)
        "(sqrt +inf) should return infinity")))

;;; ============================================================
;;; Domain Boundary Tests
;;; ============================================================

(deftest test-asin-out-of-domain
  "asin of value > 1 returns NaN"
  (let ((result (compile-and-run-numeric '(asin 2.0d0))))
    (ok (nan-p result)
        "(asin 2.0) should return NaN")))

(deftest test-acos-out-of-domain
  "acos of value > 1 returns NaN"
  (let ((result (compile-and-run-numeric '(acos 2.0d0))))
    (ok (nan-p result)
        "(acos 2.0) should return NaN")))

(deftest test-acosh-out-of-domain
  "acosh of value < 1 returns NaN"
  (let ((result (compile-and-run-numeric '(acosh 0.5d0))))
    (ok (nan-p result)
        "(acosh 0.5) should return NaN")))

(deftest test-atanh-out-of-domain
  "atanh of value > 1 returns NaN"
  (let ((result (compile-and-run-numeric '(atanh 2.0d0))))
    (ok (nan-p result)
        "(atanh 2.0) should return NaN")))

(deftest test-log-negative
  "log of negative returns NaN"
  (let ((result (compile-and-run-numeric '(log -1.0d0))))
    (ok (nan-p result)
        "(log -1.0) should return NaN")))

;;; ============================================================
;;; Hyperbolic Edge Cases
;;; ============================================================

(deftest test-sinh-infinity
  "sinh of infinity returns infinity"
  (let ((result (compile-and-run-numeric '(sinh (/ 1.0d0 0.0d0)))))
    (ok (infinity-p result)
        "(sinh +inf) should return infinity")))

(deftest test-cosh-infinity
  "cosh of infinity returns infinity"
  (let ((result (compile-and-run-numeric '(cosh (/ 1.0d0 0.0d0)))))
    (ok (infinity-p result)
        "(cosh +inf) should return infinity")))

(deftest test-tanh-infinity
  "tanh of infinity returns 1"
  (let ((result (compile-and-run-numeric '(tanh (/ 1.0d0 0.0d0)))))
    (ok (approx= 1.0d0 result)
        "(tanh +inf) should return 1.0")))

(deftest test-tanh-negative-infinity
  "tanh of negative infinity returns -1"
  (let ((result (compile-and-run-numeric '(tanh (/ -1.0d0 0.0d0)))))
    (ok (approx= -1.0d0 result)
        "(tanh -inf) should return -1.0")))
