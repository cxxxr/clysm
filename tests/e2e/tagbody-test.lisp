;;;; tests/e2e/tagbody-test.lisp - E2E Tests for Tagbody/Go
;;;;
;;;; Tests that TAGBODY and GO work correctly end-to-end.

(in-package #:clysm/tests)

(defsuite e2e-tagbody-suite "E2E tests for tagbody/go")

;;; ============================================================
;;; Basic TAGBODY Tests
;;; ============================================================

(deftest test-e2e-tagbody-simple ()
  "Simple tagbody with no jumps"
  (is-e2e "(let ((x 0)) (tagbody (setq x 1)) x)" "1"))

(deftest test-e2e-tagbody-returns-nil ()
  "Tagbody always returns NIL"
  (is-e2e "(tagbody 1 2 3)" "NIL"))

(deftest test-e2e-tagbody-multiple-forms ()
  "Tagbody with multiple forms"
  (is-e2e "(let ((x 0))
            (tagbody
              (setq x 1)
              (setq x 2)
              (setq x 3))
            x)"
          "3"))

;;; ============================================================
;;; GO Tests
;;; ============================================================

(deftest test-e2e-tagbody-forward-go ()
  "GO jumps forward to tag"
  (is-e2e "(let ((x 0))
            (tagbody
              (go skip)
              (setq x 1)
              skip
              (setq x 2))
            x)"
          "2"))

(deftest test-e2e-tagbody-backward-go ()
  "GO jumps backward for looping"
  (is-e2e "(let ((x 0))
            (tagbody
              start
              (if (= x 3) (go end))
              (setq x (+ x 1))
              (go start)
              end)
            x)"
          "3"))

;;; ============================================================
;;; Loop Patterns
;;; ============================================================

(deftest test-e2e-tagbody-count-to-5 ()
  "Loop counting to 5"
  (is-e2e "(let ((x 0))
            (tagbody
              loop
              (if (= x 5) (go end))
              (setq x (+ x 1))
              (go loop)
              end)
            x)"
          "5"))

(deftest test-e2e-tagbody-sum-1-to-4 ()
  "Sum of 0+1+2+3+4 = 10"
  (is-e2e "(let ((sum 0) (i 0))
            (tagbody
              start
              (if (> i 4) (go done))
              (setq sum (+ sum i))
              (setq i (+ i 1))
              (go start)
              done)
            sum)"
          "10"))

(deftest test-e2e-tagbody-countdown ()
  "Countdown from 5 to 0"
  (is-e2e "(let ((x 5))
            (tagbody
              loop
              (if (zerop x) (go end))
              (setq x (- x 1))
              (go loop)
              end)
            x)"
          "0"))

;;; ============================================================
;;; Multiple Tags
;;; ============================================================

(deftest test-e2e-tagbody-multiple-tags ()
  "Tagbody with multiple tags"
  (is-e2e "(let ((x 0))
            (tagbody
              a (setq x 1)
              b (setq x (+ x 1))
              c (setq x (+ x 1)))
            x)"
          "3"))

(deftest test-e2e-tagbody-skip-to-middle ()
  "Skip to middle tag"
  (is-e2e "(let ((x 0))
            (tagbody
              (go middle)
              first (setq x 1)
              middle (setq x 2)
              last (setq x 3))
            x)"
          "3"))

;;; ============================================================
;;; Tagbody with Block
;;; ============================================================

(deftest test-e2e-tagbody-early-exit ()
  "Exit tagbody early using block/return-from"
  (is-e2e "(block outer
            (let ((x 0))
              (tagbody
                loop
                (setq x (+ x 1))
                (if (= x 10) (return-from outer x))
                (go loop))))"
          "10"))

;;; ============================================================
;;; Complex Tagbody Patterns
;;; ============================================================

(deftest test-e2e-tagbody-nested-conditions ()
  "Tagbody with nested conditions"
  (is-e2e "(let ((x 0) (found nil))
            (tagbody
              search
              (if (= x 7)
                  (progn (setq found t) (go done)))
              (setq x (+ x 1))
              (if (> x 20) (go done))
              (go search)
              done)
            (if found x 'not-found))"
          "7"))

(deftest test-e2e-tagbody-factorial ()
  "Factorial using tagbody loop"
  (is-e2e "(let ((n 5) (result 1))
            (tagbody
              loop
              (if (zerop n) (go done))
              (setq result (* result n))
              (setq n (- n 1))
              (go loop)
              done)
            result)"
          "120"))
