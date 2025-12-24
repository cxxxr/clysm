;;;; tests/unit/numeric-predicates-test.lisp
;;;; Unit tests for numeric type predicates (010-numeric-tower Phase 8)

(in-package #:clysm/tests/unit/numeric-predicates)

;;; numberp tests

(deftest test-numberp-fixnum
  "numberp on fixnum: (numberp 42) => T"
  (ok (clysm/tests:compile-and-run '(numberp 42))
      "(numberp 42) should be T"))

(deftest test-numberp-bignum
  "numberp on bignum: (numberp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(numberp 10000000000000))
      "(numberp 10000000000000) should be T"))

(deftest test-numberp-ratio
  "numberp on ratio: (numberp 1/2) => T"
  (ok (clysm/tests:compile-and-run '(numberp 1/2))
      "(numberp 1/2) should be T"))

(deftest test-numberp-float
  "numberp on float: (numberp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(numberp 3.14))
      "(numberp 3.14) should be T"))

(deftest test-numberp-complex
  "numberp on complex: (numberp #C(1 2)) => T"
  (ok (clysm/tests:compile-and-run '(numberp #C(1 2)))
      "(numberp #C(1 2)) should be T"))

(deftest test-numberp-non-number
  "numberp on non-number: (numberp nil) => NIL"
  (ok (not (clysm/tests:compile-and-run '(numberp nil)))
      "(numberp nil) should be NIL"))

;;; integerp tests

(deftest test-integerp-fixnum
  "integerp on fixnum: (integerp 42) => T"
  (ok (clysm/tests:compile-and-run '(integerp 42))
      "(integerp 42) should be T"))

(deftest test-integerp-bignum
  "integerp on bignum: (integerp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(integerp 10000000000000))
      "(integerp 10000000000000) should be T"))

(deftest test-integerp-ratio
  "integerp on ratio: (integerp 1/2) => NIL"
  (ok (not (clysm/tests:compile-and-run '(integerp 1/2)))
      "(integerp 1/2) should be NIL"))

(deftest test-integerp-float
  "integerp on float: (integerp 3.14) => NIL"
  (ok (not (clysm/tests:compile-and-run '(integerp 3.14)))
      "(integerp 3.14) should be NIL"))

;;; rationalp tests

(deftest test-rationalp-fixnum
  "rationalp on fixnum: (rationalp 42) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 42))
      "(rationalp 42) should be T"))

(deftest test-rationalp-bignum
  "rationalp on bignum: (rationalp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 10000000000000))
      "(rationalp 10000000000000) should be T"))

(deftest test-rationalp-ratio
  "rationalp on ratio: (rationalp 1/2) => T"
  (ok (clysm/tests:compile-and-run '(rationalp 1/2))
      "(rationalp 1/2) should be T"))

(deftest test-rationalp-float
  "rationalp on float: (rationalp 3.14) => NIL"
  (ok (not (clysm/tests:compile-and-run '(rationalp 3.14)))
      "(rationalp 3.14) should be NIL"))

;;; realp tests

(deftest test-realp-fixnum
  "realp on fixnum: (realp 42) => T"
  (ok (clysm/tests:compile-and-run '(realp 42))
      "(realp 42) should be T"))

(deftest test-realp-float
  "realp on float: (realp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(realp 3.14))
      "(realp 3.14) should be T"))

(deftest test-realp-complex
  "realp on complex: (realp #C(1 2)) => NIL"
  (ok (not (clysm/tests:compile-and-run '(realp #C(1 2))))
      "(realp #C(1 2)) should be NIL"))

(deftest test-realp-complex-zero-imag
  "realp on complex with zero imag: (realp #C(5 0)) => T"
  ;; #C(5 0) simplifies to 5 in Common Lisp
  (ok (clysm/tests:compile-and-run '(realp #C(5 0)))
      "(realp #C(5 0)) should be T (simplifies to 5)"))

;;; floatp tests

(deftest test-floatp-float
  "floatp on float: (floatp 3.14) => T"
  (ok (clysm/tests:compile-and-run '(floatp 3.14))
      "(floatp 3.14) should be T"))

(deftest test-floatp-integer
  "floatp on integer: (floatp 42) => NIL"
  (ok (not (clysm/tests:compile-and-run '(floatp 42)))
      "(floatp 42) should be NIL"))

;;; complexp tests

(deftest test-complexp-complex
  "complexp on complex: (complexp #C(1 2)) => T"
  (ok (clysm/tests:compile-and-run '(complexp #C(1 2)))
      "(complexp #C(1 2)) should be T"))

(deftest test-complexp-integer
  "complexp on integer: (complexp 42) => NIL"
  (ok (not (clysm/tests:compile-and-run '(complexp 42)))
      "(complexp 42) should be NIL"))

(deftest test-complexp-complex-zero-imag
  "complexp on complex with zero imag: (complexp #C(5 0)) => NIL"
  ;; #C(5 0) simplifies to 5 in Common Lisp
  (ok (not (clysm/tests:compile-and-run '(complexp #C(5 0))))
      "(complexp #C(5 0)) should be NIL (simplifies to 5)"))

;;; zerop tests

(deftest test-zerop-zero
  "zerop on zero: (zerop 0) => T"
  (ok (clysm/tests:compile-and-run '(zerop 0))
      "(zerop 0) should be T"))

(deftest test-zerop-nonzero
  "zerop on nonzero: (zerop 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(zerop 5)))
      "(zerop 5) should be NIL"))

(deftest test-zerop-float-zero
  "zerop on float zero: (zerop 0.0) => T"
  (ok (clysm/tests:compile-and-run '(zerop 0.0))
      "(zerop 0.0) should be T"))

(deftest test-zerop-complex-zero
  "zerop on complex zero: (zerop #C(0 0)) => T"
  (ok (clysm/tests:compile-and-run '(zerop #C(0 0)))
      "(zerop #C(0 0)) should be T"))

;;; plusp tests

(deftest test-plusp-positive
  "plusp on positive: (plusp 5) => T"
  (ok (clysm/tests:compile-and-run '(plusp 5))
      "(plusp 5) should be T"))

(deftest test-plusp-zero
  "plusp on zero: (plusp 0) => NIL"
  (ok (not (clysm/tests:compile-and-run '(plusp 0)))
      "(plusp 0) should be NIL"))

(deftest test-plusp-negative
  "plusp on negative: (plusp -5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(plusp -5)))
      "(plusp -5) should be NIL"))

;;; minusp tests

(deftest test-minusp-negative
  "minusp on negative: (minusp -5) => T"
  (ok (clysm/tests:compile-and-run '(minusp -5))
      "(minusp -5) should be T"))

(deftest test-minusp-zero
  "minusp on zero: (minusp 0) => NIL"
  (ok (not (clysm/tests:compile-and-run '(minusp 0)))
      "(minusp 0) should be NIL"))

(deftest test-minusp-positive
  "minusp on positive: (minusp 5) => NIL"
  (ok (not (clysm/tests:compile-and-run '(minusp 5)))
      "(minusp 5) should be NIL"))

;;; evenp tests

(deftest test-evenp-even
  "evenp on even: (evenp 4) => T"
  (ok (clysm/tests:compile-and-run '(evenp 4))
      "(evenp 4) should be T"))

(deftest test-evenp-odd
  "evenp on odd: (evenp 3) => NIL"
  (ok (not (clysm/tests:compile-and-run '(evenp 3)))
      "(evenp 3) should be NIL"))

(deftest test-evenp-zero
  "evenp on zero: (evenp 0) => T"
  (ok (clysm/tests:compile-and-run '(evenp 0))
      "(evenp 0) should be T"))

(deftest test-evenp-negative-even
  "evenp on negative even: (evenp -4) => T"
  (ok (clysm/tests:compile-and-run '(evenp -4))
      "(evenp -4) should be T"))

(deftest test-evenp-bignum
  "evenp on bignum: (evenp 10000000000000) => T"
  (ok (clysm/tests:compile-and-run '(evenp 10000000000000))
      "(evenp 10000000000000) should be T"))

;;; oddp tests

(deftest test-oddp-odd
  "oddp on odd: (oddp 3) => T"
  (ok (clysm/tests:compile-and-run '(oddp 3))
      "(oddp 3) should be T"))

(deftest test-oddp-even
  "oddp on even: (oddp 4) => NIL"
  (ok (not (clysm/tests:compile-and-run '(oddp 4)))
      "(oddp 4) should be NIL"))

(deftest test-oddp-negative-odd
  "oddp on negative odd: (oddp -3) => T"
  (ok (clysm/tests:compile-and-run '(oddp -3))
      "(oddp -3) should be T"))
