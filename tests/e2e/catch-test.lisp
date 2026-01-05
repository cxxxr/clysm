;;;; tests/e2e/catch-test.lisp - E2E Tests for Catch/Throw
;;;;
;;;; Tests that CATCH and THROW work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-catch-suite "E2E tests for catch/throw")

;;; ============================================================
;;; Basic CATCH Tests
;;; ============================================================

(deftest test-e2e-catch-no-throw ()
  "CATCH with no throw returns body value"
  (is-e2e "(catch 'tag 42)" "42")
  (is-e2e "(catch 'error 1 2 3)" "3"))

(deftest test-e2e-catch-nil-tag ()
  "CATCH with NIL tag"
  (is-e2e "(catch nil 42)" "42"))

;;; ============================================================
;;; THROW Tests
;;; ============================================================

(deftest test-e2e-throw-basic ()
  "Basic throw to matching catch"
  (is-e2e "(catch 'tag (throw 'tag 100))" "100"))

(deftest test-e2e-throw-skips-rest ()
  "Throw skips remaining forms"
  (is-e2e "(catch 'tag 1 (throw 'tag 2) 3)" "2"))

(deftest test-e2e-throw-nil-value ()
  "Throw with NIL value"
  (is-e2e "(catch 'tag (throw 'tag nil))" "NIL"))

;;; ============================================================
;;; Nested CATCH Tests
;;; ============================================================

(deftest test-e2e-catch-nested-no-throw ()
  "Nested catches with no throw"
  (is-e2e "(catch 'outer (catch 'inner 42))" "42"))

(deftest test-e2e-catch-throw-inner ()
  "Throw to inner catch"
  (is-e2e "(catch 'outer (catch 'inner (throw 'inner 'in) 'a) 'b)" "B"))

(deftest test-e2e-catch-throw-outer ()
  "Throw to outer catch from nested"
  (is-e2e "(catch 'outer (catch 'inner (throw 'outer 'out) 'a) 'b)" "OUT"))

;;; ============================================================
;;; CATCH with Control Flow
;;; ============================================================

(deftest test-e2e-catch-conditional-throw ()
  "Conditional throw"
  (is-e2e "(catch 'bail (if t (throw 'bail 'escaped) 'normal))" "ESCAPED")
  (is-e2e "(catch 'bail (if nil (throw 'bail 'escaped) 'normal))" "NORMAL"))

(deftest test-e2e-catch-throw-computed ()
  "Throw with computed value"
  (is-e2e "(catch 'result (throw 'result (+ 10 20)))" "30"))
