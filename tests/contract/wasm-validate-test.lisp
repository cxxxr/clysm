;;;; wasm-validate-test.lisp - Wasm binary validation tests

(in-package #:clysm/tests/contract/wasm-validate)

(deftest test-module-header
  (let ((header (emit-module-header)))
    ;; Magic number: \0asm
    (ok (= #x00 (aref header 0)))
    (ok (= #x61 (aref header 1)))
    (ok (= #x73 (aref header 2)))
    (ok (= #x6d (aref header 3)))
    ;; Version: 1
    (ok (= #x01 (aref header 4)))
    (ok (= #x00 (aref header 5)))
    (ok (= #x00 (aref header 6)))
    (ok (= #x00 (aref header 7)))))

(deftest test-empty-module-generation
  (let ((module (emit-empty-module)))
    (ok (= 8 (length module)))
    (ok (equalp #(#x00 #x61 #x73 #x6d #x01 #x00 #x00 #x00) module))))

(deftest test-empty-module-validates
  ;; This test requires wasm-tools to be available
  (let ((module (emit-empty-module)))
    ;; Write to temp file and validate
    (clysm/tests/helpers:with-temp-wasm-file (path module)
      (ok (zerop (nth-value 2 (uiop:run-program
                               (list "wasm-tools" "validate" path)
                               :ignore-error-status t)))))))

;;; ============================================================
;;; Compiled Expression Validation Tests (T067)
;;; ============================================================

(deftest test-validate-simple-arithmetic
  "Validate Wasm for simple arithmetic expressions"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(+ 1 2)))
      "(+ 1 2) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(- 10 3)))
      "(- 10 3) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(* 4 5)))
      "(* 4 5) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(/ 20 4)))
      "(/ 20 4) should produce valid Wasm"))

(deftest test-validate-nested-arithmetic
  "Validate Wasm for nested arithmetic"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(+ (* 2 3) (- 10 5))))
      "Nested arithmetic should produce valid Wasm"))

(deftest test-validate-comparisons
  "Validate Wasm for comparison operations"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(< 1 2)))
      "(< 1 2) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(= 5 5)))
      "(= 5 5) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(/= 1 2)))
      "(/= 1 2) should produce valid Wasm"))

(deftest test-validate-conditionals
  "Validate Wasm for conditional expressions"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(if t 1 2)))
      "(if t 1 2) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(if nil 1 2)))
      "(if nil 1 2) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(if (< 1 2) 10 20)))
      "(if (< 1 2) 10 20) should produce valid Wasm"))

(deftest test-validate-let-bindings
  "Validate Wasm for let expressions"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(let ((x 42)) x)))
      "(let ((x 42)) x) should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(let ((x 10) (y 20)) (+ x y))))
      "Multiple let bindings should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(let* ((x 2) (y (+ x 1))) (+ x y))))
      "let* bindings should produce valid Wasm"))

(deftest test-validate-function-definitions
  "Validate Wasm for function definitions and calls"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm
        '(progn
           (defun add-fn (a b) (+ a b))
           (add-fn 5 10))))
      "Function definition and call should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm
        '(progn
           (defun fact (n)
             (if (= n 0) 1 (* n (fact (- n 1)))))
           (fact 5))))
      "Recursive function should produce valid Wasm"))

(deftest test-validate-nil-handling
  "Validate Wasm for NIL-related expressions"
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm 'nil))
      "NIL literal should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(let ((x nil)) x)))
      "NIL in let should produce valid Wasm")
  (ok (clysm/tests:validate-wasm-silent
       (clysm/compiler:compile-to-wasm '(if nil 1 2)))
      "NIL in condition should produce valid Wasm"))
