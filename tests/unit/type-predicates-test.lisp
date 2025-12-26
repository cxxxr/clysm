;;;; type-predicates-test.lisp - Unit tests for ANSI CL type predicates
;;;; Feature: 023-type-predicates

(in-package #:clysm/tests/unit/type-predicates)

;;; ============================================================
;;; User Story 1: Type Checking Predicates
;;; ============================================================

;;; T011: integerp tests
(deftest integerp-fixnum
  (ok (eql t (compile-and-run '(integerp 42)))
      "integerp should return T for fixnum"))

(deftest integerp-negative-fixnum
  (ok (eql t (compile-and-run '(integerp -100)))
      "integerp should return T for negative fixnum"))

(deftest integerp-float
  (ok (null (compile-and-run '(integerp 3.14)))
      "integerp should return NIL for float"))

(deftest integerp-ratio
  (ok (null (compile-and-run '(integerp 2/3)))
      "integerp should return NIL for ratio"))

;;; T012: floatp tests
(deftest floatp-float
  (ok (eql t (compile-and-run '(floatp 3.14)))
      "floatp should return T for float"))

(deftest floatp-negative-float
  (ok (eql t (compile-and-run '(floatp -2.5)))
      "floatp should return T for negative float"))

(deftest floatp-integer
  (ok (null (compile-and-run '(floatp 42)))
      "floatp should return NIL for integer"))

;;; T013: rationalp tests
(deftest rationalp-fixnum
  (ok (eql t (compile-and-run '(rationalp 42)))
      "rationalp should return T for fixnum"))

(deftest rationalp-ratio
  (ok (eql t (compile-and-run '(rationalp 2/3)))
      "rationalp should return T for ratio"))

(deftest rationalp-float
  (ok (null (compile-and-run '(rationalp 3.14)))
      "rationalp should return NIL for float"))

;;; T014: complexp tests
(deftest complexp-complex
  (ok (eql t (compile-and-run '(complexp #C(1 2))))
      "complexp should return T for complex"))

(deftest complexp-real
  (ok (null (compile-and-run '(complexp 42)))
      "complexp should return NIL for real"))

(deftest complexp-float
  (ok (null (compile-and-run '(complexp 3.14)))
      "complexp should return NIL for float"))

;;; T015: numberp tests
(deftest numberp-fixnum
  (ok (eql t (compile-and-run '(numberp 42)))
      "numberp should return T for fixnum"))

(deftest numberp-float
  (ok (eql t (compile-and-run '(numberp 3.14)))
      "numberp should return T for float"))

(deftest numberp-ratio
  (ok (eql t (compile-and-run '(numberp 2/3)))
      "numberp should return T for ratio"))

(deftest numberp-complex
  (ok (eql t (compile-and-run '(numberp #C(1 2))))
      "numberp should return T for complex"))

(deftest numberp-symbol
  (ok (null (compile-and-run '(numberp 'foo)))
      "numberp should return NIL for symbol"))

;;; T016: symbolp tests
(deftest symbolp-symbol
  (ok (eql t (compile-and-run '(symbolp 'foo)))
      "symbolp should return T for symbol"))

(deftest symbolp-nil
  (ok (eql t (compile-and-run '(symbolp nil)))
      "symbolp should return T for NIL (NIL is a symbol)"))

(deftest symbolp-number
  (ok (null (compile-and-run '(symbolp 42)))
      "symbolp should return NIL for number"))

;;; T017: functionp tests
(deftest functionp-closure
  (ok (eql t (compile-and-run '(functionp #'car)))
      "functionp should return T for function"))

(deftest functionp-lambda
  (ok (eql t (compile-and-run '(functionp (lambda (x) x))))
      "functionp should return T for lambda"))

(deftest functionp-symbol
  (ok (null (compile-and-run '(functionp 'car)))
      "functionp should return NIL for symbol"))

;;; T018: characterp tests
(deftest characterp-char
  (ok (eql t (compile-and-run '(characterp #\a)))
      "characterp should return T for character"))

(deftest characterp-integer
  (ok (null (compile-and-run '(characterp 65)))
      "characterp should return NIL for integer"))

;;; ============================================================
;;; User Story 2: Numeric Predicates
;;; ============================================================

;;; T031: zerop tests
(deftest zerop-zero-integer
  (ok (eql t (compile-and-run '(zerop 0)))
      "zerop should return T for integer zero"))

(deftest zerop-nonzero-integer
  (ok (null (compile-and-run '(zerop 1)))
      "zerop should return NIL for nonzero integer"))

(deftest zerop-zero-float
  (ok (eql t (compile-and-run '(zerop 0.0)))
      "zerop should return T for float zero"))

(deftest zerop-negative-zero-float
  (ok (eql t (compile-and-run '(zerop -0.0)))
      "zerop should return T for negative zero (IEEE 754)"))

;;; T032: plusp tests
(deftest plusp-positive
  (ok (eql t (compile-and-run '(plusp 5)))
      "plusp should return T for positive integer"))

(deftest plusp-negative
  (ok (null (compile-and-run '(plusp -3)))
      "plusp should return NIL for negative integer"))

(deftest plusp-zero
  (ok (null (compile-and-run '(plusp 0)))
      "plusp should return NIL for zero"))

(deftest plusp-positive-float
  (ok (eql t (compile-and-run '(plusp 2.5)))
      "plusp should return T for positive float"))

;;; T033: minusp tests
(deftest minusp-negative
  (ok (eql t (compile-and-run '(minusp -5)))
      "minusp should return T for negative integer"))

(deftest minusp-positive
  (ok (null (compile-and-run '(minusp 3)))
      "minusp should return NIL for positive integer"))

(deftest minusp-zero
  (ok (null (compile-and-run '(minusp 0)))
      "minusp should return NIL for zero"))

(deftest minusp-negative-float
  (ok (eql t (compile-and-run '(minusp -2.5)))
      "minusp should return T for negative float"))

;;; T034: oddp tests
(deftest oddp-odd
  (ok (eql t (compile-and-run '(oddp 7)))
      "oddp should return T for odd integer"))

(deftest oddp-even
  (ok (null (compile-and-run '(oddp 8)))
      "oddp should return NIL for even integer"))

(deftest oddp-negative-odd
  (ok (eql t (compile-and-run '(oddp -3)))
      "oddp should return T for negative odd integer"))

;;; T035: evenp tests
(deftest evenp-even
  (ok (eql t (compile-and-run '(evenp 8)))
      "evenp should return T for even integer"))

(deftest evenp-odd
  (ok (null (compile-and-run '(evenp 7)))
      "evenp should return NIL for odd integer"))

(deftest evenp-zero
  (ok (eql t (compile-and-run '(evenp 0)))
      "evenp should return T for zero (zero is even)"))

;;; ============================================================
;;; User Story 3: Signum Function
;;; ============================================================

;;; T045: signum integer tests
(deftest signum-negative-integer
  (ok (eql -1 (compile-and-run '(signum -42)))
      "signum should return -1 for negative integer"))

(deftest signum-zero-integer
  (ok (eql 0 (compile-and-run '(signum 0)))
      "signum should return 0 for zero"))

(deftest signum-positive-integer
  (ok (eql 1 (compile-and-run '(signum 100)))
      "signum should return 1 for positive integer"))

;;; T046: signum float tests
(deftest signum-negative-float
  (ok (eql -1.0 (compile-and-run '(signum -3.14)))
      "signum should return -1.0 for negative float"))

(deftest signum-zero-float
  (ok (eql 0.0 (compile-and-run '(signum 0.0)))
      "signum should return 0.0 for zero float"))

(deftest signum-positive-float
  (ok (eql 1.0 (compile-and-run '(signum 2.5)))
      "signum should return 1.0 for positive float"))
