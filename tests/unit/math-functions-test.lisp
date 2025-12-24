;;;; tests/unit/math-functions-test.lisp
;;;; Unit tests for mathematical functions (010-numeric-tower Phase 7)

(in-package #:clysm/tests/unit/math-functions)

(deftest test-sqrt-perfect-square
  "sqrt of perfect square: (sqrt 4) => 2.0"
  (ok (= 2.0 (clysm/tests:compile-and-run-numeric '(sqrt 4)))
      "(sqrt 4) should equal 2.0"))

(deftest test-sqrt-non-perfect-square
  "sqrt of non-perfect: (sqrt 2) => ~1.414"
  (let ((result (clysm/tests:compile-and-run-numeric '(sqrt 2))))
    (ok (< (abs (- result (sqrt 2))) 0.0001)
        "(sqrt 2) should approximately equal 1.414")))

(deftest test-sqrt-float
  "sqrt of float: (sqrt 2.25) => 1.5"
  (ok (= 1.5 (clysm/tests:compile-and-run-numeric '(sqrt 2.25)))
      "(sqrt 2.25) should equal 1.5"))

(deftest test-sqrt-negative
  "sqrt of negative: (sqrt -1) => complex"
  (let ((result (clysm/tests:compile-and-run-numeric '(sqrt -1))))
    (ok (complexp result) "(sqrt -1) should return complex")
    (ok (= 0 (realpart result)) "Real part should be 0")
    (ok (= 1 (imagpart result)) "Imaginary part should be 1")))

(deftest test-expt-integer-base
  "expt with integer base: (expt 2 10) => 1024"
  (ok (= 1024 (clysm/tests:compile-and-run-numeric '(expt 2 10)))
      "(expt 2 10) should equal 1024"))

(deftest test-expt-bignum-result
  "expt producing bignum: (expt 2 100) => bignum"
  (let ((result (clysm/tests:compile-and-run-numeric '(expt 2 100))))
    (ok (= (expt 2 100) result)
        "(expt 2 100) should equal 2^100")))

(deftest test-expt-fractional-power
  "expt with fractional power: (expt 8 1/3) => 2.0"
  (let ((result (clysm/tests:compile-and-run-numeric '(expt 8 1/3))))
    (ok (< (abs (- result 2.0)) 0.0001)
        "(expt 8 1/3) should approximately equal 2.0")))

(deftest test-expt-float-base
  "expt with float: (expt 2.0 3) => 8.0"
  (ok (= 8.0 (clysm/tests:compile-and-run-numeric '(expt 2.0 3)))
      "(expt 2.0 3) should equal 8.0"))

(deftest test-gcd-two-numbers
  "gcd of two numbers: (gcd 48 18) => 6"
  (ok (= 6 (clysm/tests:compile-and-run-numeric '(gcd 48 18)))
      "(gcd 48 18) should equal 6"))

(deftest test-gcd-multiple-numbers
  "gcd of multiple numbers: (gcd 24 36 60) => 12"
  (ok (= 12 (clysm/tests:compile-and-run-numeric '(gcd 24 36 60)))
      "(gcd 24 36 60) should equal 12"))

(deftest test-gcd-with-zero
  "gcd with zero: (gcd 0 5) => 5"
  (ok (= 5 (clysm/tests:compile-and-run-numeric '(gcd 0 5)))
      "(gcd 0 5) should equal 5"))

(deftest test-lcm-two-numbers
  "lcm of two numbers: (lcm 4 6) => 12"
  (ok (= 12 (clysm/tests:compile-and-run-numeric '(lcm 4 6)))
      "(lcm 4 6) should equal 12"))

(deftest test-lcm-multiple-numbers
  "lcm of multiple numbers: (lcm 2 3 5) => 30"
  (ok (= 30 (clysm/tests:compile-and-run-numeric '(lcm 2 3 5)))
      "(lcm 2 3 5) should equal 30"))

(deftest test-abs-positive
  "abs of positive: (abs 5) => 5"
  (ok (= 5 (clysm/tests:compile-and-run-numeric '(abs 5)))
      "(abs 5) should equal 5"))

(deftest test-abs-negative
  "abs of negative: (abs -5) => 5"
  (ok (= 5 (clysm/tests:compile-and-run-numeric '(abs -5)))
      "(abs -5) should equal 5"))

(deftest test-abs-bignum
  "abs of negative bignum: (abs -10000000000) => bignum"
  (ok (= 10000000000 (clysm/tests:compile-and-run-numeric '(abs -10000000000)))
      "(abs -10000000000) should equal 10000000000"))

(deftest test-abs-float
  "abs of negative float: (abs -3.14) => 3.14"
  (ok (= 3.14 (clysm/tests:compile-and-run-numeric '(abs -3.14)))
      "(abs -3.14) should equal 3.14"))

(deftest test-abs-complex
  "abs of complex (magnitude): (abs #C(3 4)) => 5.0"
  (ok (= 5.0 (clysm/tests:compile-and-run-numeric '(abs #C(3 4))))
      "(abs #C(3 4)) should equal 5.0"))

(deftest test-abs-ratio
  "abs of negative ratio: (abs -3/4) => 3/4"
  (ok (= 3/4 (clysm/tests:compile-and-run-numeric '(abs -3/4)))
      "(abs -3/4) should equal 3/4"))

(deftest test-floor-positive
  "floor of positive: (floor 7 2) => 3"
  (ok (= 3 (clysm/tests:compile-and-run-numeric '(floor 7 2)))
      "(floor 7 2) should equal 3"))

(deftest test-floor-negative
  "floor of negative: (floor -7 2) => -4"
  (ok (= -4 (clysm/tests:compile-and-run-numeric '(floor -7 2)))
      "(floor -7 2) should equal -4"))

(deftest test-ceiling-positive
  "ceiling of positive: (ceiling 7 2) => 4"
  (ok (= 4 (clysm/tests:compile-and-run-numeric '(ceiling 7 2)))
      "(ceiling 7 2) should equal 4"))

(deftest test-ceiling-negative
  "ceiling of negative: (ceiling -7 2) => -3"
  (ok (= -3 (clysm/tests:compile-and-run-numeric '(ceiling -7 2)))
      "(ceiling -7 2) should equal -3"))

(deftest test-truncate-positive
  "truncate of positive: (truncate 7 2) => 3"
  (ok (= 3 (clysm/tests:compile-and-run-numeric '(truncate 7 2)))
      "(truncate 7 2) should equal 3"))

(deftest test-truncate-negative
  "truncate of negative: (truncate -7 2) => -3"
  (ok (= -3 (clysm/tests:compile-and-run-numeric '(truncate -7 2)))
      "(truncate -7 2) should equal -3"))

(deftest test-round-down
  "round down: (round 7 2) => 4 (banker's rounding)"
  ;; 7/2 = 3.5, rounds to 4 (away from zero when exactly half)
  (ok (= 4 (clysm/tests:compile-and-run-numeric '(round 7 2)))
      "(round 7 2) should equal 4"))

(deftest test-round-up
  "round up: (round 9 2) => 4 (banker's rounding)"
  ;; 9/2 = 4.5, rounds to 4 (banker's rounding to even)
  (ok (= 4 (clysm/tests:compile-and-run-numeric '(round 9 2)))
      "(round 9 2) should equal 4 (banker's rounding)"))

(deftest test-mod-positive
  "mod positive: (mod 7 3) => 1"
  (ok (= 1 (clysm/tests:compile-and-run-numeric '(mod 7 3)))
      "(mod 7 3) should equal 1"))

(deftest test-mod-negative-dividend
  "mod negative dividend: (mod -7 3) => 2"
  (ok (= 2 (clysm/tests:compile-and-run-numeric '(mod -7 3)))
      "(mod -7 3) should equal 2"))

(deftest test-rem-positive
  "rem positive: (rem 7 3) => 1"
  (ok (= 1 (clysm/tests:compile-and-run-numeric '(rem 7 3)))
      "(rem 7 3) should equal 1"))

(deftest test-rem-negative-dividend
  "rem negative dividend: (rem -7 3) => -1"
  (ok (= -1 (clysm/tests:compile-and-run-numeric '(rem -7 3)))
      "(rem -7 3) should equal -1"))
