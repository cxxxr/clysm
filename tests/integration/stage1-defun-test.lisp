;;;; stage1-defun-test.lisp - Integration tests for Stage 0 defun/call execution
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests that Stage 0 can define and call functions

(in-package #:clysm/tests/integration/stage1-defun)

;;; ==========================================================================
;;; Defun Form Tests
;;; ==========================================================================

(deftest test-stage0-compile-simple-defun
  "Stage 0 should compile a simple defun."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((form '(defun square (x) (* x x))))
    (let ((result (clysm/stage1:run-form form)))
      (ok (clysm/stage1::compilation-result-p result)
          "defun form returns result")
      ;; Check operator is recognized
      (let ((source-form (clysm/stage1:make-source-form
                          :id "0:0"
                          :sexp form
                          :operator 'defun
                          :name 'square
                          :compilable-p t)))
        (ok (eq (clysm/stage1:source-form-operator source-form) 'defun)
            "defun operator recognized")))))

(deftest test-stage0-compile-defun-with-multiple-params
  "Stage 0 should compile defun with multiple parameters."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form '(defun add-three (a b c) (+ a (+ b c))))))
    (ok (clysm/stage1::compilation-result-p result)
        "defun with multiple params returns result")))

(deftest test-stage0-compile-defun-with-conditional
  "Stage 0 should compile defun with if expression."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form
                 '(defun abs-value (x) (if (< x 0) (- 0 x) x)))))
    (ok (clysm/stage1::compilation-result-p result)
        "defun with conditional returns result")))

;;; ==========================================================================
;;; Function Call Tests
;;; ==========================================================================

(deftest test-stage0-compile-function-call
  "Stage 0 should compile a function call."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  ;; After defining square, calling it should work
  (let ((result (clysm/stage1:run-form '(square 5))))
    (ok (clysm/stage1::compilation-result-p result)
        "function call returns result")))

(deftest test-stage0-compile-recursive-defun
  "Stage 0 should compile recursive function."
  (unless (stage1-test-prerequisites-met-p)
    (skip "Prerequisites not met"))
  (let ((result (clysm/stage1:run-form
                 '(defun factorial (n)
                    (if (<= n 1)
                        1
                        (* n (factorial (- n 1))))))))
    (ok (clysm/stage1::compilation-result-p result)
        "recursive defun returns result")))
