;;;; equality-wasm-test.lisp - Contract tests for equality predicate Wasm output
;;;; Feature: 024-equality-predicates

(in-package #:clysm/tests/contract/equality-wasm)

;;; Contract tests verify that generated Wasm modules:
;;; 1. Validate with wasm-tools validate
;;; 2. Have correct section structure
;;; 3. Export expected function signatures

;;; ============================================================
;;; not predicate Wasm validation
;;; ============================================================

(defun compile-validates (expr)
  "Compile expression and validate the resulting Wasm."
  (handler-case
      (let ((bytes (clysm/compiler:compile-to-wasm expr)))
        (clysm/tests:validate-wasm-silent bytes))
    (error () nil)))

(deftest not-wasm-validates
  (ok (compile-validates '(not nil))
      "not should generate valid Wasm"))

(deftest not-symbol-wasm-validates
  (ok (compile-validates '(not 'foo))
      "not with symbol should generate valid Wasm"))

;;; ============================================================
;;; eq predicate Wasm validation
;;; ============================================================

(deftest eq-symbols-wasm-validates
  (ok (compile-validates '(eq 'foo 'foo))
      "eq with symbols should generate valid Wasm"))

(deftest eq-fixnums-wasm-validates
  (ok (compile-validates '(eq 42 42))
      "eq with fixnums should generate valid Wasm"))

(deftest eq-cons-wasm-validates
  (ok (compile-validates '(eq (cons 1 2) (cons 1 2)))
      "eq with cons cells should generate valid Wasm"))

;;; ============================================================
;;; eql predicate Wasm validation
;;; ============================================================

(deftest eql-fixnums-wasm-validates
  (ok (compile-validates '(eql 42 42))
      "eql with fixnums should generate valid Wasm"))

(deftest eql-floats-wasm-validates
  (ok (compile-validates '(eql 3.14 3.14))
      "eql with floats should generate valid Wasm"))

(deftest eql-mixed-wasm-validates
  (ok (compile-validates '(eql 1 1.0))
      "eql with mixed types should generate valid Wasm"))

(deftest eql-characters-wasm-validates
  (ok (compile-validates '(eql #\a #\a))
      "eql with characters should generate valid Wasm"))

;;; ============================================================
;;; equal predicate Wasm validation
;;; ============================================================

(deftest equal-cons-wasm-validates
  (ok (compile-validates '(equal (cons 1 2) (cons 1 2)))
      "equal with cons cells should generate valid Wasm"))

(deftest equal-strings-wasm-validates
  (ok (compile-validates '(equal "hello" "hello"))
      "equal with strings should generate valid Wasm"))

(deftest equal-nested-wasm-validates
  (ok (compile-validates '(equal '(1 (2 3)) '(1 (2 3))))
      "equal with nested lists should generate valid Wasm"))

;;; ============================================================
;;; equalp predicate Wasm validation
;;; ============================================================

(deftest equalp-strings-wasm-validates
  (ok (compile-validates '(equalp "Hello" "hello"))
      "equalp with strings should generate valid Wasm"))

(deftest equalp-numbers-wasm-validates
  (ok (compile-validates '(equalp 1 1.0))
      "equalp with mixed numbers should generate valid Wasm"))

(deftest equalp-characters-wasm-validates
  (ok (compile-validates '(equalp #\A #\a))
      "equalp with characters should generate valid Wasm"))

;;; ============================================================
;;; and/or special forms Wasm validation
;;; ============================================================

(deftest and-wasm-validates
  (ok (compile-validates '(and t t t))
      "and should generate valid Wasm"))

(deftest and-empty-wasm-validates
  (ok (compile-validates '(and))
      "and with no args should generate valid Wasm"))

(deftest or-wasm-validates
  (ok (compile-validates '(or nil t))
      "or should generate valid Wasm"))

(deftest or-empty-wasm-validates
  (ok (compile-validates '(or))
      "or with no args should generate valid Wasm"))

(deftest and-or-nested-wasm-validates
  (ok (compile-validates '(and (or nil t) t))
      "nested and/or should generate valid Wasm"))
