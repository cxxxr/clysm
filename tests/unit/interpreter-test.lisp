;;;; interpreter-test.lisp - Tier 1 interpreter tests (Phase 9)
(in-package #:clysm/tests/unit/interpreter)

;;; Self-evaluating Forms (T184)

(deftest interpret-self-evaluating
  (testing "numbers are self-evaluating"
    (ok (= 42 (clysm/eval/interpreter:interpret 42)))
    (ok (= -17 (clysm/eval/interpreter:interpret -17)))
    (ok (= 0 (clysm/eval/interpreter:interpret 0))))

  (testing "strings are self-evaluating"
    (ok (string= "hello" (clysm/eval/interpreter:interpret "hello")))
    (ok (string= "" (clysm/eval/interpreter:interpret ""))))

  (testing "keywords are self-evaluating"
    (ok (eq :foo (clysm/eval/interpreter:interpret :foo)))
    (ok (eq :bar (clysm/eval/interpreter:interpret :bar))))

  (testing "nil is self-evaluating"
    (ok (null (clysm/eval/interpreter:interpret nil))))

  (testing "t is self-evaluating"
    (ok (eq t (clysm/eval/interpreter:interpret t)))))

;;; Quote Special Form

(deftest interpret-quote
  (testing "quote symbol"
    (ok (eq 'foo (clysm/eval/interpreter:interpret '(quote foo)))))

  (testing "quote list"
    (ok (equal '(1 2 3) (clysm/eval/interpreter:interpret '(quote (1 2 3))))))

  (testing "quote nested list"
    (ok (equal '((a b) (c d)) (clysm/eval/interpreter:interpret '(quote ((a b) (c d))))))))

;;; If Special Form

(deftest interpret-if
  (testing "if with true condition"
    (ok (= 1 (clysm/eval/interpreter:interpret '(if t 1 2)))))

  (testing "if with false condition"
    (ok (= 2 (clysm/eval/interpreter:interpret '(if nil 1 2)))))

  (testing "if with non-nil condition"
    (ok (= 1 (clysm/eval/interpreter:interpret '(if 42 1 2)))))

  (testing "if without else returns nil"
    (ok (null (clysm/eval/interpreter:interpret '(if nil 1))))))

;;; Progn Special Form

(deftest interpret-progn
  (testing "empty progn returns nil"
    (ok (null (clysm/eval/interpreter:interpret '(progn)))))

  (testing "progn returns last value"
    (ok (= 3 (clysm/eval/interpreter:interpret '(progn 1 2 3)))))

  (testing "single form progn"
    (ok (= 42 (clysm/eval/interpreter:interpret '(progn 42))))))

;;; Lambda Special Form

(deftest interpret-lambda
  (testing "lambda returns function"
    (let ((fn (clysm/eval/interpreter:interpret '(lambda (x) x))))
      (ok (functionp fn))))

  (testing "lambda with no args"
    (let ((fn (clysm/eval/interpreter:interpret '(lambda () 42))))
      (ok (functionp fn)))))

;;; Function Application

(deftest interpret-application
  (testing "apply identity lambda"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      (clysm/eval/interpreter:env-bind env 'identity
                                        (clysm/eval/interpreter:interpret '(lambda (x) x) env))
      (ok (= 42 (clysm/eval/interpreter:interpret '(identity 42) env)))))

  (testing "apply add lambda"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      ;; Use built-in + which should be available
      (ok (= 3 (clysm/eval/interpreter:interpret '(+ 1 2) env)))))

  (testing "nested application"
    (ok (= 6 (clysm/eval/interpreter:interpret '(+ 1 (+ 2 3)))))))

;;; Let Special Form

(deftest interpret-let
  (testing "simple let binding"
    (ok (= 42 (clysm/eval/interpreter:interpret '(let ((x 42)) x)))))

  (testing "multiple bindings"
    (ok (= 3 (clysm/eval/interpreter:interpret '(let ((x 1) (y 2)) (+ x y))))))

  (testing "nested let"
    (ok (= 3 (clysm/eval/interpreter:interpret '(let ((x 1)) (let ((y 2)) (+ x y)))))))

  (testing "let* sequential binding"
    (ok (= 2 (clysm/eval/interpreter:interpret '(let* ((x 1) (y x)) (+ x y)))))))

;;; Built-in Functions

(deftest interpret-builtins
  (testing "arithmetic operations"
    (ok (= 5 (clysm/eval/interpreter:interpret '(+ 2 3))))
    (ok (= 1 (clysm/eval/interpreter:interpret '(- 3 2))))
    (ok (= 6 (clysm/eval/interpreter:interpret '(* 2 3))))
    (ok (= 2 (clysm/eval/interpreter:interpret '(/ 6 3)))))

  (testing "comparison operations"
    (ok (clysm/eval/interpreter:interpret '(< 1 2)))
    (ok (not (clysm/eval/interpreter:interpret '(< 2 1))))
    (ok (clysm/eval/interpreter:interpret '(= 1 1)))
    (ok (not (clysm/eval/interpreter:interpret '(= 1 2)))))

  (testing "list operations"
    (ok (equal '(1 . 2) (clysm/eval/interpreter:interpret '(cons 1 2))))
    (ok (= 1 (clysm/eval/interpreter:interpret '(car (quote (1 2 3))))))
    (ok (equal '(2 3) (clysm/eval/interpreter:interpret '(cdr (quote (1 2 3))))))))

;;; Environment Tests

(deftest interpret-environment
  (testing "create interpreter environment"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      (ok env)))

  (testing "bind and lookup in environment"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      (clysm/eval/interpreter:env-bind env 'x 42)
      (ok (= 42 (clysm/eval/interpreter:env-lookup env 'x)))))

  (testing "environment shadowing"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      (clysm/eval/interpreter:env-bind env 'x 1)
      (let ((child-env (clysm/eval/interpreter:extend-env env '((x . 2)))))
        (ok (= 2 (clysm/eval/interpreter:env-lookup child-env 'x)))
        (ok (= 1 (clysm/eval/interpreter:env-lookup env 'x)))))))

;;; Complex Expressions

(deftest interpret-complex
  (testing "factorial-like recursion"
    ;; Using labels for recursion
    (ok (= 120 (clysm/eval/interpreter:interpret
                '(labels ((fact (n)
                            (if (= n 0)
                                1
                                (* n (fact (- n 1))))))
                   (fact 5))))))

  (testing "higher-order function"
    (ok (= 2 (clysm/eval/interpreter:interpret
              '(funcall (lambda (f x) (funcall f x))
                        (lambda (y) (+ y 1))
                        1))))))

;;; Error Handling

(deftest interpret-errors
  (testing "unbound variable signals error"
    (ok (signals (clysm/eval/interpreter:interpret 'undefined-variable))))

  (testing "calling non-function signals error"
    (ok (signals (clysm/eval/interpreter:interpret '(42 1 2))))))
