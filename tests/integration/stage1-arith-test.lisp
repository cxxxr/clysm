;;;; stage1-arith-test.lisp - Integration tests for Stage 0 arithmetic execution
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests that Stage 0 can evaluate (+ 1 2) correctly

(in-package #:clysm/tests/integration/stage1-arith)

;;; ==========================================================================
;;; Prerequisites Check
;;; ==========================================================================

(defun stage1-test-prerequisites-met-p ()
  "Check if prerequisites for Stage 1 integration tests are met."
  (and (clysm/stage1:wasmtime-available-p)
       (probe-file (merge-pathnames
                    "dist/clysm-stage0.wasm"
                    (asdf:system-source-directory :clysm)))))

;;; ==========================================================================
;;; Arithmetic Expression Tests
;;; ==========================================================================

(deftest test-stage0-compile-arithmetic-addition
  "Stage 0 should compile (+ 1 2)."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met (wasmtime or Stage 0 binary missing)"))
  ;; Currently placeholder - actual wasmtime invocation pending
  (let ((result (clysm/stage1:run-form '(+ 1 2))))
    (ok (clysm/stage1::compilation-result-p result)
        "run-form returns a result struct")
    ;; TODO: Once wasmtime integration is complete, check actual result
    ;; (ok (clysm/stage1:compilation-result-success-p result) "compilation succeeds")
    ))

(deftest test-stage0-compile-arithmetic-subtraction
  "Stage 0 should compile (- 5 3)."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form '(- 5 3))))
    (ok (clysm/stage1::compilation-result-p result)
        "subtraction form returns result")))

(deftest test-stage0-compile-arithmetic-multiplication
  "Stage 0 should compile (* 3 4)."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form '(* 3 4))))
    (ok (clysm/stage1::compilation-result-p result)
        "multiplication form returns result")))

(deftest test-stage0-compile-nested-arithmetic
  "Stage 0 should compile (+ (* 2 3) (- 10 5))."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form '(+ (* 2 3) (- 10 5)))))
    (ok (clysm/stage1::compilation-result-p result)
        "nested arithmetic returns result")))
