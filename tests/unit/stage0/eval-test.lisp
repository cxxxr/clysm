;;;; eval-test.lisp - Unit tests for Stage 0 evaluator
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Tests T007: Core evaluator skeleton
;;;;
;;;; TDD: Write tests FIRST, ensure they FAIL before implementation

(defpackage #:clysm/tests/unit/stage0/eval-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:eval-form
                #:make-env
                #:extend-env))

(in-package #:clysm/tests/unit/stage0/eval-test)

;;; ============================================================
;;; T007: Evaluator Skeleton Tests
;;; ============================================================

(deftest test-eval-form-exists
  "Verify eval-form function is exported"
  (ok (fboundp 'eval-form) "eval-form should be a function"))

;;; ============================================================
;;; T013 [US1]: Fixnum Evaluation Tests
;;; ============================================================

(deftest test-eval-fixnum-literal
  "Verify fixnum evaluates to itself - (eval-form 42 nil) -> 42"
  (let ((result (eval-form 42 (make-env))))
    (ok (eql result 42) "42 should evaluate to 42")))

(deftest test-eval-zero
  "Verify zero evaluates correctly"
  (let ((result (eval-form 0 (make-env))))
    (ok (eql result 0) "0 should evaluate to 0")))

(deftest test-eval-negative-fixnum
  "Verify negative fixnum evaluates correctly"
  (let ((result (eval-form -10 (make-env))))
    (ok (eql result -10) "-10 should evaluate to -10")))

;;; ============================================================
;;; T018 [US1]: Nested Arithmetic Tests
;;; ============================================================

(deftest test-eval-nested-arithmetic
  "Verify nested arithmetic: (+ 1 (* 2 3)) -> 7"
  (let ((result (eval-form '(+ 1 (* 2 3)) (make-env))))
    (ok (eql result 7) "(+ 1 (* 2 3)) should evaluate to 7")))

(deftest test-eval-deeply-nested-arithmetic
  "Verify deeply nested: (+ (* 2 3) (- 10 5)) -> 11"
  (let ((result (eval-form '(+ (* 2 3) (- 10 5)) (make-env))))
    (ok (eql result 11) "(+ (* 2 3) (- 10 5)) should evaluate to 11")))

;;; ============================================================
;;; T030 [US2]: Defun Parsing Tests (skeleton)
;;; ============================================================

(deftest test-eval-defun-basic
  "Verify defun creates function binding"
  ;; After (defun f (x) x), calling (f 42) should return 42
  (let* ((env (make-env))
         (name (eval-form '(defun f (x) x) env)))
    ;; defun should return the function name
    (ok (eq name 'f) "defun should return function name")
    ;; Now test calling the function (uses global function table)
    (let ((result (eval-form '(f 42) env)))
      (ok (eql result 42) "(f 42) should return 42 after defun"))))

;;; ============================================================
;;; T052-T057 [US4]: Control Structure Tests (skeleton)
;;; ============================================================

(deftest test-eval-if-true-branch
  "Verify if with true condition: (if t 1 2) -> 1"
  (let ((result (eval-form '(if t 1 2) (make-env))))
    (ok (eql result 1) "(if t 1 2) should evaluate to 1")))

(deftest test-eval-if-false-branch
  "Verify if with nil condition: (if nil 1 2) -> 2"
  (let ((result (eval-form '(if nil 1 2) (make-env))))
    (ok (eql result 2) "(if nil 1 2) should evaluate to 2")))

(deftest test-eval-let-basic
  "Verify let binding: (let ((x 5)) x) -> 5"
  (let ((result (eval-form '(let ((x 5)) x) (make-env))))
    (ok (eql result 5) "(let ((x 5)) x) should evaluate to 5")))

(deftest test-eval-let-with-body
  "Verify let with expression body: (let ((x 5)) (+ x 3)) -> 8"
  (let ((result (eval-form '(let ((x 5)) (+ x 3)) (make-env))))
    (ok (eql result 8) "(let ((x 5)) (+ x 3)) should evaluate to 8")))

(deftest test-eval-let*-sequential
  "Verify let* sequential binding: (let* ((x 1) (y (+ x 1))) y) -> 2"
  (let ((result (eval-form '(let* ((x 1) (y (+ x 1))) y) (make-env))))
    (ok (eql result 2) "(let* ((x 1) (y (+ x 1))) y) should evaluate to 2")))

;;; ============================================================
;;; T066-T069 [US5]: Lambda Tests (skeleton)
;;; ============================================================

(deftest test-eval-lambda-application
  "Verify lambda application: ((lambda (x) x) 42) -> 42"
  (let ((result (eval-form '((lambda (x) x) 42) (make-env))))
    (ok (eql result 42) "((lambda (x) x) 42) should evaluate to 42")))

(deftest test-eval-lambda-with-arithmetic
  "Verify lambda with arithmetic: ((lambda (x) (+ x 1)) 5) -> 6"
  (let ((result (eval-form '((lambda (x) (+ x 1)) 5) (make-env))))
    (ok (eql result 6) "((lambda (x) (+ x 1)) 5) should evaluate to 6")))

;;; ============================================================
;;; T075 [US6]: Quote Tests (skeleton)
;;; ============================================================

(deftest test-eval-quote-symbol
  "Verify quote returns unevaluated symbol"
  (let ((result (eval-form '(quote a) (make-env))))
    (ok (eq result 'a) "(quote a) should return symbol A")))

(deftest test-eval-quote-shorthand
  "Verify quote shorthand 'a"
  (let ((result (eval-form ''a (make-env))))
    (ok (eq result 'a) "'a should return symbol A")))

;;; ============================================================
;;; Variable Reference Tests
;;; ============================================================

(deftest test-eval-variable-lookup
  "Verify variable evaluates to bound value"
  (let* ((env (make-env))
         (env2 (extend-env 'x 100 env)))
    ;; This tests eval-form dispatching to lookup
    (let ((result (eval-form 'x env2)))
      (ok (eql result 100) "x should evaluate to 100 in env with (x . 100)"))))
