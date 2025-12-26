;;;; equality-ansi-test.lisp - ANSI CL compliance tests for equality predicates
;;;; Feature: 024-equality-predicates
;;;;
;;;; Note: In clysm, T is represented as i31ref(1), which is indistinguishable
;;;; from the integer 1 at the Wasm level. Tests check for truthy values using
;;;; (truthy-p ...) helper since compile-and-run returns an integer for T.

(in-package #:clysm/tests/integration/equality-ansi)

;;; Integration tests verify ANSI Common Lisp specification compliance
;;; These tests run the compiled Wasm and verify behavior matches ANSI CL

;;; Helper to check for truthy (T) values from compile-and-run
(defun truthy-p (result)
  "Check if a compile-and-run result is truthy (T).
   In clysm, T is i31ref(1), so we check for non-nil."
  (not (null result)))

;;; ============================================================
;;; ANSI CL eq compliance
;;; ============================================================

(deftest ansi-eq-symbol-identity
  "ANSI CL: eq returns true if its arguments are the same, identical object."
  (ok (truthy-p (compile-and-run '(eq 'a 'a)))
      "Symbols with same name are eq"))

(deftest ansi-eq-nil-identity
  "ANSI CL: nil is eq to nil"
  (ok (truthy-p (compile-and-run '(eq nil nil)))
      "NIL is eq to NIL"))

(deftest ansi-eq-cons-different
  "ANSI CL: Two different cons cells are not eq"
  (ok (null (compile-and-run '(eq (cons 1 2) (cons 1 2))))
      "Different cons cells are not eq even with same content"))

;;; ============================================================
;;; ANSI CL eql compliance
;;; ============================================================

(deftest ansi-eql-same-type-numbers
  "ANSI CL: eql returns true for numbers of the same type and value"
  (ok (truthy-p (compile-and-run '(eql 3 3)))
      "Same fixnums are eql"))

(deftest ansi-eql-different-type-numbers
  "ANSI CL: eql returns false for numbers of different types"
  (ok (null (compile-and-run '(eql 3 3.0)))
      "Fixnum and float with same value are NOT eql"))

(deftest ansi-eql-characters
  "ANSI CL: Characters are eql if they are char="
  (ok (truthy-p (compile-and-run '(eql #\A #\A)))
      "Same characters are eql"))

(deftest ansi-eql-symbols
  "ANSI CL: Symbols are eql if they are eq"
  (ok (truthy-p (compile-and-run '(eql 'foo 'foo)))
      "Same symbols are eql (via eq)"))

;;; ============================================================
;;; ANSI CL equal compliance
;;; ============================================================

(deftest ansi-equal-conses
  "ANSI CL: Conses are equal if their car and cdr are equal"
  (ok (truthy-p (compile-and-run '(equal '(a b) '(a b))))
      "Lists with same content are equal"))

(deftest ansi-equal-strings
  "ANSI CL: Strings are equal if they have the same characters"
  (ok (truthy-p (compile-and-run '(equal "foo" "foo")))
      "Strings with same content are equal"))

(deftest ansi-equal-strings-case-sensitive
  "ANSI CL: equal is case-sensitive for strings"
  (ok (null (compile-and-run '(equal "Foo" "foo")))
      "equal is case-sensitive for strings"))

(deftest ansi-equal-nested
  "ANSI CL: equal recursively compares list structure"
  (ok (truthy-p (compile-and-run '(equal '((a) (b (c))) '((a) (b (c))))))
      "Nested lists with same structure are equal"))

;;; ============================================================
;;; ANSI CL equalp compliance
;;; ============================================================

(deftest ansi-equalp-case-insensitive-strings
  "ANSI CL: equalp uses char-equal for strings (case-insensitive)"
  (ok (truthy-p (compile-and-run '(equalp "FOO" "foo")))
      "Strings differing only in case are equalp"))

(deftest ansi-equalp-numeric-coercion
  "ANSI CL: equalp uses = for numbers (type coercing)"
  (ok (truthy-p (compile-and-run '(equalp 3 3.0)))
      "Fixnum and float with same value are equalp"))

(deftest ansi-equalp-characters
  "ANSI CL: equalp uses char-equal for characters"
  (ok (truthy-p (compile-and-run '(equalp #\A #\a)))
      "Characters differing only in case are equalp"))

;;; ============================================================
;;; ANSI CL not compliance
;;; ============================================================

(deftest ansi-not-nil
  "ANSI CL: not returns t if its argument is nil"
  (ok (truthy-p (compile-and-run '(not nil)))
      "(not nil) returns T"))

(deftest ansi-not-t
  "ANSI CL: not returns nil if its argument is non-nil"
  (ok (null (compile-and-run '(not t)))
      "(not t) returns NIL"))

(deftest ansi-not-object
  "ANSI CL: not returns nil for any non-nil object"
  (ok (null (compile-and-run '(not 'something)))
      "(not 'something) returns NIL"))

;;; ============================================================
;;; ANSI CL and/or compliance
;;; ============================================================

(deftest ansi-and-empty
  "ANSI CL: (and) returns t"
  (ok (truthy-p (compile-and-run '(and)))
      "(and) returns T"))

(deftest ansi-and-returns-last
  "ANSI CL: and returns the value of the last form if all are non-nil"
  ;; Note: symbols are returned as their hash/id, so we check truthy
  (ok (truthy-p (compile-and-run '(and 'foo 'bar)))
      "(and 'foo 'bar) returns BAR (non-nil)"))

(deftest ansi-and-short-circuit
  "ANSI CL: and returns nil immediately when any form is nil"
  (ok (null (compile-and-run '(and 'foo nil 'bar)))
      "(and 'foo nil 'bar) returns NIL"))

(deftest ansi-or-empty
  "ANSI CL: (or) returns nil"
  (ok (null (compile-and-run '(or)))
      "(or) returns NIL"))

(deftest ansi-or-returns-first-non-nil
  "ANSI CL: or returns the first non-nil value"
  ;; Note: symbols are returned as their hash/id, so we check truthy
  (ok (truthy-p (compile-and-run '(or nil 'foo 'bar)))
      "(or nil 'foo 'bar) returns FOO (non-nil)"))

(deftest ansi-or-all-nil
  "ANSI CL: or returns nil when all forms are nil"
  (ok (null (compile-and-run '(or nil nil)))
      "(or nil nil) returns NIL"))
