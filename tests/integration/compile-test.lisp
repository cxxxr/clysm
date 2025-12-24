;;;; compile-test.lisp - Integration tests for compile function
;;;; Feature: 017-eval-jit-compile
;;;; TDD: Tests written FIRST per Constitution Principle VII

(in-package #:clysm/tests/integration/compile)

;;; ============================================================
;;; User Story 1 Integration Test (T012)
;;; ============================================================

(deftest test-anonymous-lambda-compilation-end-to-end
  "T012: Anonymous lambda compilation works end-to-end."
  ;; Reset state
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Test 1: Simple arithmetic
  (let ((add-one (clysm/eval/compile:compile* nil '(lambda (x) (+ x 1)))))
    (ok (functionp add-one)
        "Compile returns a function")
    (ok (= (funcall add-one 5) 6)
        "add-one(5) = 6")
    (ok (= (funcall add-one 0) 1)
        "add-one(0) = 1")
    (ok (= (funcall add-one -5) -4)
        "add-one(-5) = -4"))

  ;; Test 2: Multi-argument function
  (let ((multiply (clysm/eval/compile:compile* nil '(lambda (a b) (* a b)))))
    (ok (= (funcall multiply 3 4) 12)
        "multiply(3, 4) = 12")
    (ok (= (funcall multiply 0 100) 0)
        "multiply(0, 100) = 0"))

  ;; Test 3: Zero-argument function
  (let ((constant (clysm/eval/compile:compile* nil '(lambda () 42))))
    (ok (= (funcall constant) 42)
        "constant() = 42"))

  ;; Test 4: Function with conditional
  (let ((abs-fn (clysm/eval/compile:compile* nil '(lambda (x) (if (< x 0) (- x) x)))))
    (ok (= (funcall abs-fn 5) 5)
        "abs(5) = 5")
    (ok (= (funcall abs-fn -5) 5)
        "abs(-5) = 5")
    (ok (= (funcall abs-fn 0) 0)
        "abs(0) = 0")))

;;; ============================================================
;;; User Story 2 Integration Test (T019)
;;; ============================================================

(deftest test-tier-1-special-forms
  "T019: All special forms work via Tier 1."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; if
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (if x 1 0)))))
    (ok (= (funcall fn t) 1) "if: true branch")
    (ok (= (funcall fn nil) 0) "if: false branch"))

  ;; let
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (let ((y (+ x 1))) y)))))
    (ok (= (funcall fn 5) 6) "let: binding works"))

  ;; let*
  (let ((fn (clysm/eval/compile:compile* nil '(lambda (x) (let* ((y x) (z (+ y 1))) z)))))
    (ok (= (funcall fn 5) 6) "let*: sequential binding"))

  ;; progn
  (let ((fn (clysm/eval/compile:compile* nil '(lambda () (progn 1 2 3)))))
    (ok (= (funcall fn) 3) "progn: returns last form"))

  ;; setq
  (let ((fn (clysm/eval/compile:compile* nil '(lambda () (let ((x 1)) (setq x 2) x)))))
    (ok (= (funcall fn) 2) "setq: mutates binding")))

;;; ============================================================
;;; User Story 4 Integration Test (T036)
;;; ============================================================

(deftest test-named-function-compile-and-call
  "T036: Named function compilation and invocation works."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Compile named function
  (let ((fn (clysm/eval/compile:compile* 'my-double '(lambda (x) (* x 2)))))
    ;; Can call returned function
    (ok (= (funcall fn 5) 10)
        "Direct call returns 10")

    ;; Can lookup via function slot
    (let ((retrieved (clysm/eval/jit:get-function-slot 'my-double)))
      (ok retrieved "Function slot has registered function")
      (ok (functionp retrieved) "Retrieved value is callable")
      (ok (= (funcall retrieved 7) 14)
          "Lookup and call returns 14"))))

;;; ============================================================
;;; Edge Case Tests (T047-T051)
;;; ============================================================

(deftest test-compile-invalid-syntax
  "T047: compile* handles invalid syntax gracefully."
  (ok (signals (clysm/eval/compile:compile* nil '(defun foo () 1)) 'error)
      "Signals error for defun instead of lambda")
  (ok (signals (clysm/eval/compile:compile* nil 'symbol) 'error)
      "Signals error for symbol")
  (ok (signals (clysm/eval/compile:compile* nil nil) 'error)
      "Signals error for nil"))

(deftest test-closure-environment-capture
  "T048: Closures capture their environment correctly."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Create a function that returns a closure
  (let ((make-adder (clysm/eval/compile:compile*
                     nil
                     '(lambda (n)
                       (lambda (x) (+ x n))))))
    ;; Call make-adder to get a closure
    (let ((add-5 (funcall make-adder 5))
          (add-10 (funcall make-adder 10)))
      (ok (functionp add-5) "make-adder returns a function")
      (ok (= (funcall add-5 3) 8) "add-5(3) = 8")
      (ok (= (funcall add-10 3) 13) "add-10(3) = 13"))))

;;; ============================================================
;;; T051: Hot-patch during execution test
;;; ============================================================

(deftest test-hot-patch-during-execution
  "T051: Hot-patching updates function behavior immediately."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Compile initial version
  (let ((fn (clysm/eval/compile:compile* 'hot-patch-test '(lambda (x) (* x 2)))))
    ;; Initial behavior: double
    (ok (= (funcall fn 5) 10)
        "Initial function doubles: 5 * 2 = 10")

    ;; Hot-patch with new definition (triple instead of double)
    (clysm/eval/compile:compile* 'hot-patch-test '(lambda (x) (* x 3)))

    ;; Get fresh reference via function slot
    (let ((updated-fn (clysm/eval/jit:get-function-slot 'hot-patch-test)))
      (ok (= (funcall updated-fn 5) 15)
          "Hot-patched function triples: 5 * 3 = 15"))))

(deftest test-hot-patch-preserves-other-functions
  "T051: Hot-patching one function does not affect others."
  (clysm/eval/compile:reset-invocation-counts)
  (clysm/eval/compile:reset-tiered-functions)
  (clysm/eval/jit:reset-function-slots)

  ;; Compile two functions
  (clysm/eval/compile:compile* 'fn-a '(lambda (x) (+ x 1)))
  (clysm/eval/compile:compile* 'fn-b '(lambda (x) (+ x 10)))

  ;; Verify both work
  (let ((a (clysm/eval/jit:get-function-slot 'fn-a))
        (b (clysm/eval/jit:get-function-slot 'fn-b)))
    (ok (= (funcall a 5) 6) "fn-a: 5 + 1 = 6")
    (ok (= (funcall b 5) 15) "fn-b: 5 + 10 = 15"))

  ;; Hot-patch fn-a
  (clysm/eval/compile:compile* 'fn-a '(lambda (x) (+ x 100)))

  ;; Verify fn-a changed but fn-b unchanged
  (let ((a (clysm/eval/jit:get-function-slot 'fn-a))
        (b (clysm/eval/jit:get-function-slot 'fn-b)))
    (ok (= (funcall a 5) 105) "fn-a after hot-patch: 5 + 100 = 105")
    (ok (= (funcall b 5) 15) "fn-b unchanged: 5 + 10 = 15")))
