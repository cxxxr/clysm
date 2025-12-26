;;;; predicates-wasm-test.lisp - Contract tests for predicate Wasm output
;;;; Feature: 023-type-predicates

(in-package #:clysm/tests/contract/predicates-wasm)

;;; Contract tests verify that generated Wasm modules:
;;; 1. Validate with wasm-tools validate
;;; 2. Have correct section structure
;;; 3. Export expected function signatures

(deftest integerp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(integerp 42))
      "integerp should generate valid Wasm"))

(deftest floatp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(floatp 3.14))
      "floatp should generate valid Wasm"))

(deftest rationalp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(rationalp 2/3))
      "rationalp should generate valid Wasm"))

(deftest complexp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(complexp #C(1 2)))
      "complexp should generate valid Wasm"))

(deftest numberp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(numberp 42))
      "numberp should generate valid Wasm"))

(deftest symbolp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(symbolp 'foo))
      "symbolp should generate valid Wasm"))

(deftest functionp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(functionp #'car))
      "functionp should generate valid Wasm"))

(deftest characterp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(characterp #\a))
      "characterp should generate valid Wasm"))

(deftest zerop-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(zerop 0))
      "zerop should generate valid Wasm"))

(deftest plusp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(plusp 5))
      "plusp should generate valid Wasm"))

(deftest minusp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(minusp -5))
      "minusp should generate valid Wasm"))

(deftest oddp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(oddp 7))
      "oddp should generate valid Wasm"))

(deftest evenp-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(evenp 8))
      "evenp should generate valid Wasm"))

(deftest signum-wasm-validates
  (ok (clysm/tests:validate-wasm-silent '(signum -42))
      "signum should generate valid Wasm"))
