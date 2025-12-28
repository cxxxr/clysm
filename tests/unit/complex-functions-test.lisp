;;;; tests/unit/complex-functions-test.lisp
;;;; Unit tests for complex number operations (001-numeric-functions US6)
;;;;
;;;; Tests for complex, realpart, imagpart, conjugate, phase

(in-package #:clysm/tests/unit/complex-functions)

;;; Use pi from host for test calculations
(defconstant +pi+ 3.141592653589793d0)

;;; ============================================================
;;; Complex Constructor Tests (FR-030)
;;; ============================================================

(deftest test-complex-constructor
  "complex constructor: (complex 3 4) => #C(3 4)"
  (let ((result (compile-and-run-numeric '(complex 3 4))))
    (ok (complexp result) "(complex 3 4) should return a complex number")
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= 4 (imagpart result)) "Imaginary part should be 4")))

(deftest test-complex-zero-imaginary
  "complex with zero imaginary: (complex 5 0) => 5"
  ;; When imaginary part is 0, result should be a real number
  (let ((result (compile-and-run-numeric '(complex 5 0))))
    (ok (= 5 result) "(complex 5 0) should equal 5")))

(deftest test-complex-floats
  "complex with floats: (complex 1.5 2.5)"
  (let ((result (compile-and-run-numeric '(complex 1.5 2.5))))
    (ok (complexp result) "(complex 1.5 2.5) should return a complex number")
    (ok (= 1.5 (realpart result)) "Real part should be 1.5")
    (ok (= 2.5 (imagpart result)) "Imaginary part should be 2.5")))

;;; ============================================================
;;; Realpart Tests (FR-031)
;;; ============================================================

(deftest test-realpart-complex
  "realpart of complex: (realpart #C(3 4)) => 3"
  (ok (= 3 (compile-and-run-numeric '(realpart #C(3 4))))
      "(realpart #C(3 4)) should equal 3"))

(deftest test-realpart-real
  "realpart of real: (realpart 5) => 5"
  (ok (= 5 (compile-and-run-numeric '(realpart 5)))
      "(realpart 5) should equal 5"))

(deftest test-realpart-float
  "realpart of float: (realpart 3.14) => 3.14"
  (ok (= 3.14 (compile-and-run-numeric '(realpart 3.14)))
      "(realpart 3.14) should equal 3.14"))

;;; ============================================================
;;; Imagpart Tests (FR-032)
;;; ============================================================

(deftest test-imagpart-complex
  "imagpart of complex: (imagpart #C(3 4)) => 4"
  (ok (= 4 (compile-and-run-numeric '(imagpart #C(3 4))))
      "(imagpart #C(3 4)) should equal 4"))

(deftest test-imagpart-real
  "imagpart of real: (imagpart 5) => 0"
  (ok (= 0 (compile-and-run-numeric '(imagpart 5)))
      "(imagpart 5) should equal 0"))

(deftest test-imagpart-float
  "imagpart of float: (imagpart 3.14) => 0.0"
  (let ((result (compile-and-run-numeric '(imagpart 3.14))))
    (ok (= 0 result) "(imagpart 3.14) should equal 0")))

;;; ============================================================
;;; Conjugate Tests (FR-033)
;;; ============================================================

(deftest test-conjugate-complex
  "conjugate of complex: (conjugate #C(3 4)) => #C(3 -4)"
  (let ((result (compile-and-run-numeric '(conjugate #C(3 4)))))
    (ok (complexp result) "Conjugate should return complex")
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= -4 (imagpart result)) "Imaginary part should be -4")))

(deftest test-conjugate-real
  "conjugate of real: (conjugate 5) => 5"
  (ok (= 5 (compile-and-run-numeric '(conjugate 5)))
      "(conjugate 5) should equal 5"))

(deftest test-conjugate-negative-imaginary
  "conjugate of negative imaginary: (conjugate #C(3 -4)) => #C(3 4)"
  (let ((result (compile-and-run-numeric '(conjugate #C(3 -4)))))
    (ok (complexp result) "Conjugate should return complex")
    (ok (= 3 (realpart result)) "Real part should be 3")
    (ok (= 4 (imagpart result)) "Imaginary part should be 4")))

;;; ============================================================
;;; Phase Tests (FR-034)
;;; ============================================================

(deftest test-phase-positive-real
  "phase of positive real: (phase 1) => 0"
  (ok (approx= 0.0d0 (compile-and-run-numeric '(phase 1)))
      "(phase 1) should equal 0"))

(deftest test-phase-negative-real
  "phase of negative real: (phase -1) => pi"
  (let ((result (compile-and-run-numeric '(phase -1))))
    (ok (approx= +pi+ result)
        "(phase -1) should equal pi")))

(deftest test-phase-positive-imaginary
  "phase of i: (phase #C(0 1)) => pi/2"
  (let ((result (compile-and-run-numeric '(phase #C(0 1)))))
    (ok (approx= (/ +pi+ 2) result)
        "(phase #C(0 1)) should equal pi/2")))

(deftest test-phase-negative-imaginary
  "phase of -i: (phase #C(0 -1)) => -pi/2"
  (let ((result (compile-and-run-numeric '(phase #C(0 -1)))))
    (ok (approx= (- (/ +pi+ 2)) result)
        "(phase #C(0 -1)) should equal -pi/2")))

(deftest test-phase-first-quadrant
  "phase of 1+i: (phase #C(1 1)) => pi/4"
  (let ((result (compile-and-run-numeric '(phase #C(1 1)))))
    (ok (approx= (/ +pi+ 4) result)
        "(phase #C(1 1)) should equal pi/4")))
