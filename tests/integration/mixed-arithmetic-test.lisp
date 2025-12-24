;;;; tests/integration/mixed-arithmetic-test.lisp
;;;; Integration tests for mixed numeric type arithmetic (010-numeric-tower Phase 9)

(in-package #:clysm/tests/integration/mixed-arithmetic)

;;; ============================================================
;;; Mixed Fixnum/Bignum Arithmetic
;;; ============================================================

(deftest test-fixnum-plus-bignum
  "Fixnum + bignum: 5 + 10000000000000"
  (ok (= 10000000000005 (clysm/tests:compile-and-run-numeric '(+ 5 10000000000000)))
      "5 + 10000000000000 should equal 10000000000005"))

(deftest test-bignum-minus-fixnum
  "Bignum - fixnum: 10000000000000 - 5"
  (ok (= 9999999999995 (clysm/tests:compile-and-run-numeric '(- 10000000000000 5)))
      "10000000000000 - 5 should equal 9999999999995"))

(deftest test-fixnum-times-bignum
  "Fixnum * bignum: 2 * 10000000000000"
  (ok (= 20000000000000 (clysm/tests:compile-and-run-numeric '(* 2 10000000000000)))
      "2 * 10000000000000 should equal 20000000000000"))

;;; ============================================================
;;; Mixed Integer/Ratio Arithmetic
;;; ============================================================

(deftest test-fixnum-plus-ratio
  "Fixnum + ratio: 1 + 1/2"
  (ok (= 3/2 (clysm/tests:compile-and-run-numeric '(+ 1 1/2)))
      "1 + 1/2 should equal 3/2"))

(deftest test-ratio-times-fixnum
  "Ratio * fixnum: 1/3 * 6"
  (ok (= 2 (clysm/tests:compile-and-run-numeric '(* 1/3 6)))
      "1/3 * 6 should equal 2"))

(deftest test-bignum-div-ratio
  "Bignum / ratio: 10000000000000 / (1/2)"
  (ok (= 20000000000000 (clysm/tests:compile-and-run-numeric '(/ 10000000000000 1/2)))
      "10000000000000 / (1/2) should equal 20000000000000"))

;;; ============================================================
;;; Mixed Integer/Float Arithmetic
;;; ============================================================

(deftest test-fixnum-plus-float
  "Fixnum + float: 1 + 0.5"
  (ok (= 1.5 (clysm/tests:compile-and-run-numeric '(+ 1 0.5)))
      "1 + 0.5 should equal 1.5"))

(deftest test-float-minus-bignum
  "Float - bignum (returns float): 1.0e13 - 10000000000000"
  (ok (= 0.0 (clysm/tests:compile-and-run-numeric '(- 1.0e13 10000000000000)))
      "1.0e13 - 10000000000000 should equal 0.0"))

(deftest test-ratio-times-float
  "Ratio * float (contagion to float): 1/2 * 2.0"
  (ok (= 1.0 (clysm/tests:compile-and-run-numeric '(* 1/2 2.0)))
      "1/2 * 2.0 should equal 1.0"))

;;; ============================================================
;;; Mixed Real/Complex Arithmetic
;;; ============================================================

(deftest test-fixnum-plus-complex
  "Fixnum + complex: 1 + #C(0 1)"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1 #C(0 1)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 1 (realpart result)) "Real part should be 1")
    (ok (= 1 (imagpart result)) "Imaginary part should be 1")))

(deftest test-float-times-complex
  "Float * complex: 2.0 * #C(1 1)"
  (let ((result (clysm/tests:compile-and-run-numeric '(* 2.0 #C(1 1)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 2.0 (realpart result)) "Real part should be 2.0")
    (ok (= 2.0 (imagpart result)) "Imaginary part should be 2.0")))

(deftest test-complex-plus-ratio
  "Complex + ratio: #C(1 2) + 1/2"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ #C(1 2) 1/2))))
    (ok (complexp result) "Result should be complex")
    (ok (= 3/2 (realpart result)) "Real part should be 3/2")
    (ok (= 2 (imagpart result)) "Imaginary part should be 2")))

;;; ============================================================
;;; Type Coercion Chain Tests
;;; ============================================================

(deftest test-coercion-chain-to-float
  "Chain: fixnum + ratio + float => float"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ (+ 1 1/2) 0.5))))
    (ok (floatp result) "Result should be float")
    (ok (= 2.0 result) "Result should equal 2.0")))

(deftest test-coercion-chain-to-complex
  "Chain: fixnum + float + complex => complex"
  (let ((result (clysm/tests:compile-and-run-numeric '(+ (+ 1 0.5) #C(0 1)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 1.5 (realpart result)) "Real part should be 1.5")))

(deftest test-full-tower-expression
  "All types in one expression"
  ;; 1 + 1/2 + 0.25 + #C(0 1) = #C(1.75 1)
  (let ((result (clysm/tests:compile-and-run-numeric '(+ 1 (+ 1/2 (+ 0.25 #C(0 1)))))))
    (ok (complexp result) "Result should be complex")
    (ok (< (abs (- (realpart result) 1.75)) 0.0001) "Real part should be 1.75")
    (ok (= 1 (imagpart result)) "Imaginary part should be 1")))

;;; ============================================================
;;; Comparison Across Types
;;; ============================================================

(deftest test-compare-fixnum-ratio
  "Compare fixnum and ratio: 2 = 4/2"
  (ok (clysm/tests:compile-and-run '(= 2 4/2))
      "2 = 4/2 should be T"))

(deftest test-compare-ratio-float
  "Compare ratio and float: 1/2 = 0.5"
  (ok (clysm/tests:compile-and-run '(= 1/2 0.5))
      "1/2 = 0.5 should be T"))

(deftest test-compare-fixnum-bignum
  "Compare fixnum and bignum (different values)"
  (ok (not (clysm/tests:compile-and-run '(= 5 10000000000000)))
      "5 = 10000000000000 should be NIL"))

(deftest test-less-than-mixed
  "Less than across types: 1 < 1.5 < 2"
  (ok (clysm/tests:compile-and-run '(< 1 1.5 2))
      "1 < 1.5 < 2 should be T"))

;;; ============================================================
;;; Special Cases
;;; ============================================================

(deftest test-complex-simplification
  "Complex with zero imag simplifies: #C(3 0) + 2"
  ;; #C(3 0) becomes 3, so 3 + 2 = 5
  (let ((result (clysm/tests:compile-and-run-numeric '(+ #C(3 0) 2))))
    (ok (not (complexp result)) "Result should not be complex")
    (ok (= 5 result) "Result should be 5")))

(deftest test-ratio-simplification
  "Ratio that simplifies to integer: 6/3 + 1"
  ;; 6/3 = 2, so 2 + 1 = 3
  (let ((result (clysm/tests:compile-and-run-numeric '(+ (/ 6 3) 1))))
    (ok (integerp result) "Result should be integer")
    (ok (= 3 result) "Result should be 3")))

(deftest test-nested-arithmetic
  "Nested arithmetic with mixed types"
  ;; ((2 + 1/2) * 3.0) - #C(1 -1)
  ;; = (5/2 * 3.0) - #C(1 -1)
  ;; = 7.5 - #C(1 -1)
  ;; = #C(6.5 1)
  (let ((result (clysm/tests:compile-and-run-numeric '(- (* (+ 2 1/2) 3.0) #C(1 -1)))))
    (ok (complexp result) "Result should be complex")
    (ok (= 6.5 (realpart result)) "Real part should be 6.5")
    (ok (= 1.0 (imagpart result)) "Imaginary part should be 1.0")))
