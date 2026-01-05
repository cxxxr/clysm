;;;; tests/e2e/symbol-test.lisp - E2E Tests for Symbol Operations
;;;;
;;;; Tests that symbol primitives work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-symbol-suite "E2E tests for symbol operations")

;;; ============================================================
;;; SYMBOL-NAME Tests
;;; ============================================================

(deftest test-e2e-symbol-name-simple ()
  "SYMBOL-NAME - simple symbols"
  (is-e2e "(symbol-name 'foo)" "\"FOO\"")
  (is-e2e "(symbol-name 'bar)" "\"BAR\"")
  (is-e2e "(symbol-name 'x)" "\"X\""))

(deftest test-e2e-symbol-name-hyphenated ()
  "SYMBOL-NAME - hyphenated symbols"
  (is-e2e "(symbol-name 'hello-world)" "\"HELLO-WORLD\"")
  (is-e2e "(symbol-name 'my-var)" "\"MY-VAR\""))

(deftest test-e2e-symbol-name-with-numbers ()
  "SYMBOL-NAME - symbols with numbers"
  (is-e2e "(symbol-name 'foo1)" "\"FOO1\"")
  (is-e2e "(symbol-name 'x123)" "\"X123\""))

(deftest test-e2e-symbol-name-nil ()
  "SYMBOL-NAME of NIL"
  (is-e2e "(symbol-name nil)" "\"NIL\""))

(deftest test-e2e-symbol-name-t ()
  "SYMBOL-NAME of T"
  (is-e2e "(symbol-name t)" "\"T\""))

;;; ============================================================
;;; Symbol Identity Tests
;;; ============================================================

(deftest test-e2e-symbol-interning ()
  "Same symbol name produces same symbol"
  (is-e2e "(eq 'foo 'foo)" "T")
  (is-e2e "(eq 'hello-world 'hello-world)" "T"))

(deftest test-e2e-symbol-case-insensitive ()
  "Symbol names are case-insensitive (upcased)"
  ;; Note: The reader upcases by default
  (is-e2e "(eq 'foo 'FOO)" "T")
  (is-e2e "(eq 'Hello 'HELLO)" "T"))

;;; ============================================================
;;; Symbol in Data Structures
;;; ============================================================

(deftest test-e2e-symbol-in-list ()
  "Symbols in lists"
  (is-e2e "(car '(foo bar baz))" "FOO")
  (is-e2e "(car (cdr '(a b c)))" "B"))

(deftest test-e2e-symbol-in-dotted ()
  "Symbols in dotted pairs"
  (is-e2e "(car '(x . y))" "X")
  (is-e2e "(cdr '(x . y))" "Y"))

;;; ============================================================
;;; Symbol Comparison
;;; ============================================================

(deftest test-e2e-symbol-eq ()
  "EQ comparison of symbols"
  (is-e2e "(eq 'a 'a)" "T")
  (is-e2e "(eq 'a 'b)" "NIL"))

(deftest test-e2e-symbol-eql ()
  "EQL comparison of symbols"
  (is-e2e "(eql 'a 'a)" "T")
  (is-e2e "(eql 'a 'b)" "NIL"))
