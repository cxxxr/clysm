;;;; tests/unit/trig-functions-test.lisp
;;;; Unit tests for trigonometric functions (001-numeric-functions US2)
;;;;
;;;; Tests for sin, cos, tan, asin, acos, atan

(in-package #:clysm/tests/unit/trig-functions)

;;; Use pi from host for test calculations
(defconstant +pi+ 3.141592653589793d0)

;;; ============================================================
;;; Sin Tests (FR-007)
;;; ============================================================

(deftest test-sin-zero
  "sin of zero: (sin 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(sin 0)))
      "(sin 0) should equal 0"))

(deftest test-sin-pi-half
  "sin of pi/2: (sin (/ pi 2)) => 1"
  (let ((result (compile-and-run-numeric `(sin ,(/ +pi+ 2)))))
    (ok (approx= 1.0d0 result)
        "(sin pi/2) should equal approximately 1")))

(deftest test-sin-pi
  "sin of pi: (sin pi) => 0"
  (let ((result (compile-and-run-numeric `(sin ,+pi+))))
    (ok (approx= 0.0d0 result)
        "(sin pi) should equal approximately 0")))

(deftest test-sin-negative
  "sin of negative: (sin -pi/2) => -1"
  (let ((result (compile-and-run-numeric `(sin ,(- (/ +pi+ 2))))))
    (ok (approx= -1.0d0 result)
        "(sin -pi/2) should equal approximately -1")))

;;; ============================================================
;;; Cos Tests (FR-008)
;;; ============================================================

(deftest test-cos-zero
  "cos of zero: (cos 0) => 1"
  (ok (approx= 1.0d0 (compile-and-run-numeric '(cos 0)))
      "(cos 0) should equal 1"))

(deftest test-cos-pi-half
  "cos of pi/2: (cos pi/2) => 0"
  (let ((result (compile-and-run-numeric `(cos ,(/ +pi+ 2)))))
    (ok (approx= 0.0d0 result)
        "(cos pi/2) should equal approximately 0")))

(deftest test-cos-pi
  "cos of pi: (cos pi) => -1"
  (let ((result (compile-and-run-numeric `(cos ,+pi+))))
    (ok (approx= -1.0d0 result)
        "(cos pi) should equal approximately -1")))

;;; ============================================================
;;; Tan Tests (FR-009)
;;; ============================================================

(deftest test-tan-zero
  "tan of zero: (tan 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(tan 0)))
      "(tan 0) should equal 0"))

(deftest test-tan-pi-quarter
  "tan of pi/4: (tan pi/4) => 1"
  (let ((result (compile-and-run-numeric `(tan ,(/ +pi+ 4)))))
    (ok (approx= 1.0d0 result)
        "(tan pi/4) should equal approximately 1")))

(deftest test-tan-negative-pi-quarter
  "tan of -pi/4: (tan -pi/4) => -1"
  (let ((result (compile-and-run-numeric `(tan ,(- (/ +pi+ 4))))))
    (ok (approx= -1.0d0 result)
        "(tan -pi/4) should equal approximately -1")))

;;; ============================================================
;;; Asin Tests (FR-010)
;;; ============================================================

(deftest test-asin-zero
  "asin of zero: (asin 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(asin 0)))
      "(asin 0) should equal 0"))

(deftest test-asin-one
  "asin of one: (asin 1) => pi/2"
  (let ((result (compile-and-run-numeric '(asin 1))))
    (ok (approx= (/ +pi+ 2) result)
        "(asin 1) should equal approximately pi/2")))

(deftest test-asin-negative-one
  "asin of -1: (asin -1) => -pi/2"
  (let ((result (compile-and-run-numeric '(asin -1))))
    (ok (approx= (- (/ +pi+ 2)) result)
        "(asin -1) should equal approximately -pi/2")))

;;; ============================================================
;;; Acos Tests (FR-011)
;;; ============================================================

(deftest test-acos-one
  "acos of one: (acos 1) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(acos 1)))
      "(acos 1) should equal 0"))

(deftest test-acos-zero
  "acos of zero: (acos 0) => pi/2"
  (let ((result (compile-and-run-numeric '(acos 0))))
    (ok (approx= (/ +pi+ 2) result)
        "(acos 0) should equal approximately pi/2")))

(deftest test-acos-negative-one
  "acos of -1: (acos -1) => pi"
  (let ((result (compile-and-run-numeric '(acos -1))))
    (ok (approx= +pi+ result)
        "(acos -1) should equal approximately pi")))

;;; ============================================================
;;; Atan Tests (FR-012)
;;; ============================================================

(deftest test-atan-zero
  "atan of zero: (atan 0) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(atan 0)))
      "(atan 0) should equal 0"))

(deftest test-atan-one
  "atan of one: (atan 1) => pi/4"
  (let ((result (compile-and-run-numeric '(atan 1))))
    (ok (approx= (/ +pi+ 4) result)
        "(atan 1) should equal approximately pi/4")))

(deftest test-atan-two-args
  "atan with two args: (atan 1 1) => pi/4"
  (let ((result (compile-and-run-numeric '(atan 1 1))))
    (ok (approx= (/ +pi+ 4) result)
        "(atan 1 1) should equal approximately pi/4")))

(deftest test-atan-quadrant2
  "atan in second quadrant: (atan 1 -1) => 3pi/4"
  (let ((result (compile-and-run-numeric '(atan 1 -1))))
    (ok (approx= (* 3 (/ +pi+ 4)) result)
        "(atan 1 -1) should equal approximately 3pi/4")))

(deftest test-atan-quadrant3
  "atan in third quadrant: (atan -1 -1) => -3pi/4"
  (let ((result (compile-and-run-numeric '(atan -1 -1))))
    (ok (approx= (* -3 (/ +pi+ 4)) result)
        "(atan -1 -1) should equal approximately -3pi/4")))

;;; ============================================================
;;; Edge Case Tests (001-numeric-functions T135)
;;; ============================================================

(deftest test-atan-zero-zero-edge-case
  "atan of (0, 0): (atan 0 0) => implementation-defined (typically 0)"
  ;; T135: atan(0, 0) is mathematically undefined but implementations
  ;; typically return 0.0 or signal an error
  (let ((result (compile-and-run-numeric '(atan 0 0))))
    (ok (or (approx= 0.0d0 result)
            ;; Some implementations may return NaN
            (and (floatp result) (sb-ext:float-nan-p result)))
        "(atan 0 0) should return 0.0 or NaN")))
