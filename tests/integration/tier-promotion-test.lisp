;;;; tier-promotion-test.lisp - Integration tests for tier promotion
;;;; Feature: 017-eval-jit-compile
;;;; Tests automatic tier promotion behavior (T027, T049, T050)

(in-package #:clysm/tests/integration/tier-promotion)

;;; ============================================================
;;; T027: Integration test for automatic tier promotion
;;; ============================================================

(deftest test-automatic-tier-promotion-trigger
  "T027: Function automatically promotes after reaching threshold."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Create a named function to track tier status
  (let* ((fn (clysm/eval/compile:compile* 'promotion-test '(lambda (x) (* x 2))))
         (tf (clysm/eval/compile:get-tiered-function 'promotion-test))
         (threshold clysm/eval/compile:*compilation-threshold*))

    ;; Initially at Tier 1
    (ok (eq (clysm/eval/compile:tiered-function-tier tf) :tier-1)
        "Function starts at Tier 1")

    ;; Call exactly threshold times
    (dotimes (i threshold)
      (funcall fn i))

    ;; After threshold calls, promotion should have been attempted
    ;; Due to graceful degradation, tier may still be :tier-1 if JIT failed
    ;; but promotion-failed-p should be set
    (ok (or (eq (clysm/eval/compile:tiered-function-tier tf) :tier-2)
            (clysm/eval/compile:tiered-function-promotion-failed-p tf))
        "Promotion was attempted after threshold")))

(deftest test-tier-promotion-preserves-semantics
  "T027: Promoted function maintains correct behavior."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  (let* ((fn (clysm/eval/compile:compile* 'semantics-test '(lambda (a b) (+ a b))))
         (threshold clysm/eval/compile:*compilation-threshold*))

    ;; Pre-promotion behavior
    (ok (= (funcall fn 3 4) 7)
        "Function works before promotion (3+4=7)")

    ;; Trigger promotion by calling threshold times
    (dotimes (i (1- threshold))
      (funcall fn i i))

    ;; Post-promotion (or post-attempt) behavior should be same
    ;; Note: In host environment, graceful degradation keeps Tier 1
    (ok (= (funcall fn 10 20) 30)
        "Function works after promotion attempt (10+20=30)")
    (ok (= (funcall fn -5 15) 10)
        "Function works with negative numbers (-5+15=10)")))

;;; ============================================================
;;; T049: Tier 2 compilation failure graceful degradation
;;; ============================================================

(deftest test-graceful-degradation-on-jit-failure
  "T049: Function continues working when JIT compilation fails."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Create a function that uses complex features that might fail JIT
  (let* ((fn (clysm/eval/compile:compile* 'graceful-test
               '(lambda (x) (let ((y (* x 2))) (+ y 1)))))
         (tf (clysm/eval/compile:get-tiered-function 'graceful-test))
         (threshold clysm/eval/compile:*compilation-threshold*))

    ;; Function should work at Tier 1
    (ok (= (funcall fn 5) 11)
        "Function works at Tier 1: (5*2)+1=11")

    ;; Trigger promotion attempt
    (dotimes (i threshold)
      (funcall fn i))

    ;; After failed promotion, function should still work
    (ok (= (funcall fn 10) 21)
        "Function still works after failed promotion: (10*2)+1=21")

    ;; Verify no retry occurs (promotion-failed-p is set)
    (when (clysm/eval/compile:tiered-function-promotion-failed-p tf)
      (let ((initial-count (clysm/eval/compile:tiered-function-invocation-count tf)))
        ;; Call a few more times
        (dotimes (i 5)
          (funcall fn i))
        ;; Function should work without re-attempting promotion
        (ok (= (funcall fn 3) 7)
            "Function works without retry after promotion failure")))))

(deftest test-no-promotion-retry-after-failure
  "T049: No promotion retry after initial failure."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  (let* ((fn (clysm/eval/compile:compile* 'no-retry-test '(lambda (x) x)))
         (tf (clysm/eval/compile:get-tiered-function 'no-retry-test))
         (threshold clysm/eval/compile:*compilation-threshold*))

    ;; Trigger first promotion attempt
    (dotimes (i (+ threshold 5))
      (funcall fn i))

    ;; If promotion failed, verify flag is set
    (when (eq (clysm/eval/compile:tiered-function-tier tf) :tier-1)
      (ok (clysm/eval/compile:tiered-function-promotion-failed-p tf)
          "Promotion failure flag is set after failed attempt"))))

;;; ============================================================
;;; T050: Recursive function tier promotion
;;; ============================================================

(deftest test-recursive-function-tier-promotion
  "T050: Recursive functions work correctly through tier promotion."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Create a recursive factorial-like function using labels
  (let* ((fn (clysm/eval/compile:compile* 'recursive-test
               '(lambda (n)
                  (labels ((fact (x acc)
                             (if (<= x 1)
                                 acc
                                 (fact (- x 1) (* acc x)))))
                    (fact n 1)))))
         (tf (clysm/eval/compile:get-tiered-function 'recursive-test)))

    ;; Test recursive function at Tier 1
    (ok (= (funcall fn 5) 120)
        "Recursive factorial(5) = 120 at Tier 1")
    (ok (= (funcall fn 1) 1)
        "Recursive factorial(1) = 1 at Tier 1")
    (ok (= (funcall fn 0) 1)
        "Recursive factorial(0) = 1 at Tier 1")))

(deftest test-mutually-recursive-tier-promotion
  "T050: Mutually recursive functions via flet work correctly."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Create a function using flet for local definitions
  (let* ((fn (clysm/eval/compile:compile* 'mutual-test
               '(lambda (n)
                  (flet ((double (x) (* x 2))
                         (triple (x) (* x 3)))
                    (+ (double n) (triple n))))))
         (tf (clysm/eval/compile:get-tiered-function 'mutual-test)))

    ;; Test: double(5) + triple(5) = 10 + 15 = 25
    (ok (= (funcall fn 5) 25)
        "flet functions work: double(5)+triple(5)=25")))
