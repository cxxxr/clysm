;;;; tests/e2e/block-test.lisp - E2E Tests for Block/Return-from
;;;;
;;;; Tests that BLOCK and RETURN-FROM work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-block-suite "E2E tests for block/return-from")

;;; ============================================================
;;; Basic BLOCK Tests
;;; ============================================================

(deftest test-e2e-block-simple ()
  "BLOCK with no return-from returns last value"
  (is-e2e "(block foo 1 2 3)" "3")
  (is-e2e "(block bar 'a 'b 'c)" "C"))

(deftest test-e2e-block-single ()
  "BLOCK with single form"
  (is-e2e "(block foo 42)" "42"))

(deftest test-e2e-block-empty ()
  "BLOCK with no body returns NIL"
  (is-e2e "(block foo)" "NIL"))

;;; ============================================================
;;; RETURN-FROM Tests
;;; ============================================================

(deftest test-e2e-return-from-immediate ()
  "RETURN-FROM exits immediately"
  (is-e2e "(block foo (return-from foo 42) 99)" "42"))

(deftest test-e2e-return-from-nil ()
  "RETURN-FROM with NIL value"
  (is-e2e "(block foo (return-from foo nil) 99)" "NIL"))

(deftest test-e2e-return-from-skips-rest ()
  "RETURN-FROM skips remaining forms"
  (is-e2e "(block foo 1 (return-from foo 2) 3 4 5)" "2"))

;;; ============================================================
;;; Nested BLOCK Tests
;;; ============================================================

(deftest test-e2e-block-nested ()
  "Nested blocks with different names"
  (is-e2e "(block outer (block inner 1 2) 3)" "3"))

(deftest test-e2e-block-return-outer ()
  "Return from outer block from nested block"
  (is-e2e "(block outer (block inner (return-from outer 'out)) 'end)" "OUT"))

(deftest test-e2e-block-return-inner ()
  "Return from inner block"
  (is-e2e "(block outer (block inner (return-from inner 'in) 'inner-end) 'outer-end)" "OUTER-END"))

(deftest test-e2e-block-same-name-shadow ()
  "Inner block with same name shadows outer"
  ;; return-from should go to the inner block
  (is-e2e "(block foo (block foo (return-from foo 'inner) 'a) 'outer)" "OUTER"))

;;; ============================================================
;;; BLOCK with Control Flow
;;; ============================================================

(deftest test-e2e-block-conditional-return ()
  "Return-from inside conditional"
  (is-e2e "(block foo (if t (return-from foo 'yes)) 'no)" "YES")
  (is-e2e "(block foo (if nil (return-from foo 'yes)) 'no)" "NO"))

(deftest test-e2e-block-loop-exit ()
  "Using block to exit a loop pattern"
  (is-e2e "(let ((x 0))
            (block done
              (tagbody
                loop
                (if (= x 5) (return-from done x))
                (setq x (+ x 1))
                (go loop))))"
          "5"))

;;; ============================================================
;;; BLOCK with Expressions
;;; ============================================================

(deftest test-e2e-block-return-computed ()
  "Return-from with computed value"
  (is-e2e "(block foo (return-from foo (+ 1 2)))" "3")
  (is-e2e "(block foo (return-from foo (cons 'a 'b)))" "(A . B)"))

(deftest test-e2e-block-in-let ()
  "Block inside let"
  (is-e2e "(let ((x 10))
            (block foo
              (if (> x 5)
                  (return-from foo 'big)
                  'small)))"
          "BIG"))
