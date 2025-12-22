;;;; variable-scope-test.lisp - Variable scope tests (T259-T261)
(in-package #:clysm/tests/integration/variable-scope)

;;; Lexical scope tests

(deftest lexical-scope-basic
  (testing "let creates lexical binding"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 10)) x))))
      (ok (= 10 result))))

  (testing "nested let shadows outer binding"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 1))
                     (let ((x 2))
                       x)))))
      (ok (= 2 result))))

  (testing "outer binding restored after inner let"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 1))
                     (+ (let ((x 2)) x) x)))))
      (ok (= 3 result)))))

(deftest let-star-sequential
  (testing "let* binds sequentially"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let* ((x 1) (y (+ x 1)))
                     y))))
      (ok (= 2 result))))

  (testing "let* allows reference to earlier bindings"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let* ((a 1) (b 2) (c (+ a b)))
                     c))))
      (ok (= 3 result)))))

;;; Lambda closure scope tests

(deftest closure-scope
  (testing "closure captures lexical variable"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((n 10))
                     (funcall (lambda (x) (+ x n)) 5)))))
      (ok (= 15 result))))

  (testing "closure captures multiple variables"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((a 1) (b 2))
                     (funcall (lambda (x) (+ x a b)) 7)))))
      (ok (= 10 result))))

  (testing "nested lambda captures from outer closure"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((n 10))
                     (funcall
                      (funcall (lambda (a) (lambda (b) (+ a b n))) 5)
                      3)))))
      (ok (= 18 result)))))

;;; Function parameter scope tests

(deftest function-params
  (testing "function parameters are lexical"
    (let ((result (clysm/eval/interpreter:interpret
                   '(labels ((add (x y) (+ x y)))
                     (add 3 4)))))
      (ok (= 7 result))))

  (testing "function params shadow outer bindings"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 100))
                     (labels ((f (x) x))
                       (f 42))))))
      (ok (= 42 result)))))

;;; Block scope tests

(deftest block-scope
  (testing "block body has access to outer scope"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 5))
                     (block foo
                       (+ x 10))))))
      (ok (= 15 result))))

  (testing "return-from preserves value from outer scope"
    (let ((result (clysm/eval/interpreter:interpret
                   '(let ((x 42))
                     (block foo
                       (return-from foo x)
                       0)))))
      (ok (= 42 result)))))

;;; Special case: unbound variable

(deftest unbound-variable
  (testing "referencing unbound variable signals error"
    (ok (signals (clysm/eval/interpreter:interpret 'undefined-variable-xyz)))))

