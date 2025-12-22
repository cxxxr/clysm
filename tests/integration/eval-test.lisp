;;;; eval-test.lisp - Eval function tests (Phase 9 - T185)
(in-package #:clysm/tests/integration/eval)

;;; Basic Eval Tests

(deftest eval-basic
  (testing "eval number"
    (ok (= 42 (clysm/eval:eval* 42))))

  (testing "eval string"
    (ok (string= "hello" (clysm/eval:eval* "hello"))))

  (testing "eval nil"
    (ok (null (clysm/eval:eval* nil))))

  (testing "eval t"
    (ok (eq t (clysm/eval:eval* t))))

  (testing "eval keyword"
    (ok (eq :foo (clysm/eval:eval* :foo)))))

;;; Eval with Expressions

(deftest eval-expressions
  (testing "eval arithmetic"
    (ok (= 3 (clysm/eval:eval* '(+ 1 2)))))

  (testing "eval nested arithmetic"
    (ok (= 10 (clysm/eval:eval* '(+ 1 2 3 4)))))

  (testing "eval quoted list"
    (ok (equal '(1 2 3) (clysm/eval:eval* '(quote (1 2 3))))))

  (testing "eval if form"
    (ok (= 1 (clysm/eval:eval* '(if t 1 2))))
    (ok (= 2 (clysm/eval:eval* '(if nil 1 2))))))

;;; Eval with Environment

(deftest eval-environment
  (testing "eval with custom environment"
    (let ((env (clysm/eval/interpreter:make-interpreter-env)))
      (clysm/eval/interpreter:env-bind env 'x 42)
      (ok (= 42 (clysm/eval:eval* 'x env)))))

  (testing "eval let creates local binding"
    (ok (= 10 (clysm/eval:eval* '(let ((x 10)) x)))))

  (testing "eval let* with sequential bindings"
    (ok (= 3 (clysm/eval:eval* '(let* ((x 1) (y (+ x 1)) (z (+ y 1))) z))))))

;;; Eval with Lambda

(deftest eval-lambda
  (testing "eval lambda returns function"
    (let ((fn (clysm/eval:eval* '(lambda (x) x))))
      (ok (functionp fn))))

  (testing "eval funcall with lambda"
    (ok (= 42 (clysm/eval:eval* '(funcall (lambda (x) x) 42)))))

  (testing "eval lambda with closure"
    (ok (= 52 (clysm/eval:eval* '(let ((y 10))
                                   (funcall (lambda (x) (+ x y)) 42)))))))

;;; Eval with Defun-like

(deftest eval-functions
  (testing "eval labels for local function"
    (ok (= 6 (clysm/eval:eval* '(labels ((add (a b) (+ a b)))
                                  (add 2 4))))))

  (testing "eval flet for local function"
    (ok (= 10 (clysm/eval:eval* '(flet ((double (x) (+ x x)))
                                   (double 5)))))))

;;; Eval Special Forms

(deftest eval-special-forms
  (testing "eval progn"
    (ok (= 3 (clysm/eval:eval* '(progn 1 2 3)))))

  (testing "eval block"
    (ok (= 42 (clysm/eval:eval* '(block foo 1 2 42)))))

  (testing "eval return-from"
    (ok (= 10 (clysm/eval:eval* '(block foo 1 (return-from foo 10) 3))))))

;;; Eval Error Handling

(deftest eval-errors
  (testing "eval unbound variable signals error"
    (ok (signals (clysm/eval:eval* 'nonexistent-variable))))

  (testing "eval invalid form signals error"
    (ok (signals (clysm/eval:eval* '(123 456))))))

;;; Acceptance Test (T204)

(deftest eval-acceptance
  (testing "(eval '(+ 1 2)) => 3"
    (ok (= 3 (clysm/eval:eval* '(+ 1 2))))))
