;;;; tests/e2e/literals-test.lisp - E2E Tests for Literals and Data Types
;;;;
;;;; Tests that literal values are correctly compiled and printed.

(in-package #:clysm/tests)

(defsuite e2e-literals-suite "E2E tests for literals and data types")

;;; ============================================================
;;; Fixnum Tests
;;; ============================================================

(deftest test-e2e-fixnum-zero ()
  "Zero evaluates to \"0\""
  (is-e2e "0" "0"))

(deftest test-e2e-fixnum-positive ()
  "Positive integers"
  (is-e2e "1" "1")
  (is-e2e "42" "42")
  (is-e2e "100" "100")
  (is-e2e "1000000" "1000000"))

(deftest test-e2e-fixnum-negative ()
  "Negative integers"
  (is-e2e "-1" "-1")
  (is-e2e "-42" "-42")
  (is-e2e "-123" "-123")
  (is-e2e "-999" "-999"))

(deftest test-e2e-fixnum-large ()
  "Large fixnums near i31ref limits"
  ;; i31ref range: -2^30 to 2^30-1
  (is-e2e "1073741823" "1073741823")    ; 2^30 - 1 (max positive)
  (is-e2e "-1073741824" "-1073741824")) ; -2^30 (min negative)

;;; ============================================================
;;; NIL and T Tests
;;; ============================================================

(deftest test-e2e-nil ()
  "NIL literal"
  (is-e2e "nil" "NIL"))

(deftest test-e2e-nil-quoted ()
  "Quoted NIL"
  (is-e2e "'nil" "NIL"))

(deftest test-e2e-empty-list ()
  "Empty list () is NIL"
  (is-e2e "()" "NIL"))

(deftest test-e2e-t ()
  "T literal"
  (is-e2e "t" "T"))

(deftest test-e2e-t-quoted ()
  "Quoted T"
  (is-e2e "'t" "T"))

;;; ============================================================
;;; Quoted Symbol Tests
;;; ============================================================

(deftest test-e2e-quoted-symbol-simple ()
  "Simple quoted symbols"
  (is-e2e "'foo" "FOO")
  (is-e2e "'bar" "BAR")
  (is-e2e "'x" "X"))

(deftest test-e2e-quoted-symbol-hyphenated ()
  "Hyphenated symbols"
  (is-e2e "'hello-world" "HELLO-WORLD")
  (is-e2e "'my-var" "MY-VAR"))

(deftest test-e2e-quoted-symbol-with-numbers ()
  "Symbols containing numbers"
  (is-e2e "'foo1" "FOO1")
  (is-e2e "'x123" "X123"))

;;; ============================================================
;;; Quoted List Tests
;;; ============================================================

(deftest test-e2e-quoted-list-simple ()
  "Simple quoted lists"
  (is-e2e "'(1 2 3)" "(1 2 3)")
  (is-e2e "'(a b c)" "(A B C)"))

(deftest test-e2e-quoted-list-single ()
  "Single-element lists"
  (is-e2e "'(1)" "(1)")
  (is-e2e "'(foo)" "(FOO)"))

(deftest test-e2e-quoted-list-nested ()
  "Nested lists"
  (is-e2e "'((1 2) (3 4))" "((1 2) (3 4))")
  (is-e2e "'((a b) (c d))" "((A B) (C D))")
  (is-e2e "'(1 (2 3) 4)" "(1 (2 3) 4)"))

(deftest test-e2e-quoted-list-deeply-nested ()
  "Deeply nested lists"
  (is-e2e "'(((1)))" "(((1)))")
  (is-e2e "'((a (b (c))))" "((A (B (C))))"))

(deftest test-e2e-quoted-dotted-pair ()
  "Dotted pairs"
  (is-e2e "'(a . b)" "(A . B)")
  (is-e2e "'(1 . 2)" "(1 . 2)"))

(deftest test-e2e-quoted-dotted-list ()
  "Improper lists (dotted at end)"
  (is-e2e "'(1 2 . 3)" "(1 2 . 3)")
  (is-e2e "'(a b . c)" "(A B . C)"))

(deftest test-e2e-quoted-mixed ()
  "Mixed content lists"
  (is-e2e "'(1 foo 2 bar)" "(1 FOO 2 BAR)")
  (is-e2e "'(nil t 42)" "(NIL T 42)"))

;;; ============================================================
;;; Quote Special Form Tests
;;; ============================================================

(deftest test-e2e-quote-form ()
  "Explicit (quote ...) form"
  (is-e2e "(quote foo)" "FOO")
  (is-e2e "(quote (1 2 3))" "(1 2 3)"))
