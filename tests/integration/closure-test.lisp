;;;; closure-test.lisp - Closure integration tests (T071, T072, T074)
(in-package #:clysm/tests/integration/closure)

;;; T071: Environment capture tests
;;; Tests for closures capturing variables from enclosing scope

(deftest test-simple-closure-capture
  "Closure captures variable from enclosing let"
  ;; (let ((x 10))
  ;;   (funcall (lambda () x)))
  ;; => 10
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x 10))
                (funcall (lambda () x)))))
      "Closure should capture x from enclosing let"))

(deftest test-closure-captures-multiple-vars
  "Closure captures multiple variables"
  ;; (let ((x 10) (y 20))
  ;;   (funcall (lambda () (+ x y))))
  ;; => 30
  (ok (= 30 (clysm/tests:compile-and-run
             '(let ((x 10) (y 20))
                (funcall (lambda () (+ x y))))))
      "Closure should capture x and y"))

(deftest test-closure-with-arg
  "Closure with argument accessing captured variable"
  ;; (let ((y 100))
  ;;   (funcall (lambda (x) (+ x y)) 5))
  ;; => 105
  (ok (= 105 (clysm/tests:compile-and-run
              '(let ((y 100))
                 (funcall (lambda (x) (+ x y)) 5))))
      "Closure should use both arg and captured var"))

(deftest test-nested-closure-capture
  "Nested closures with variable capture"
  ;; (let ((x 1))
  ;;   (let ((y 2))
  ;;     (funcall (lambda () (+ x y)))))
  ;; => 3
  (ok (= 3 (clysm/tests:compile-and-run
            '(let ((x 1))
               (let ((y 2))
                 (funcall (lambda () (+ x y)))))))
      "Nested closures should capture from both scopes"))

(deftest test-returned-closure
  "Closure returned from function and called later"
  ;; (defun make-adder (n)
  ;;   (lambda (x) (+ x n)))
  ;; (funcall (make-adder 10) 5)
  ;; => 15
  (ok (= 15 (clysm/tests:compile-and-run
             '(progn
                (defun make-adder (n)
                  (lambda (x) (+ x n)))
                (funcall (make-adder 10) 5))))
      "Returned closure should remember captured n"))

;;; T072: Lambda/funcall tests
;;; Tests for lambda expression compilation and funcall

(deftest test-lambda-identity
  "Lambda identity function"
  ;; (funcall (lambda (x) x) 42)
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(funcall (lambda (x) x) 42)))
      "(funcall (lambda (x) x) 42) should return 42"))

(deftest test-lambda-no-args
  "Lambda with no arguments"
  ;; (funcall (lambda () 42))
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(funcall (lambda () 42))))
      "(funcall (lambda () 42)) should return 42"))

(deftest test-lambda-two-args
  "Lambda with two arguments"
  ;; (funcall (lambda (a b) (+ a b)) 10 20)
  ;; => 30
  (ok (= 30 (clysm/tests:compile-and-run
             '(funcall (lambda (a b) (+ a b)) 10 20)))
      "(funcall (lambda (a b) (+ a b)) 10 20) should return 30"))

(deftest test-lambda-three-args
  "Lambda with three arguments"
  ;; (funcall (lambda (a b c) (+ a b c)) 1 2 3)
  ;; => 6
  (ok (= 6 (clysm/tests:compile-and-run
            '(funcall (lambda (a b c) (+ a b c)) 1 2 3)))
      "Lambda with three args should work"))

(deftest test-lambda-body-multiple-forms
  "Lambda with multiple body forms"
  ;; (funcall (lambda (x) (+ x 1) (+ x 2)) 10)
  ;; => 12 (last form)
  (ok (= 12 (clysm/tests:compile-and-run
             '(funcall (lambda (x) (+ x 1) (+ x 2)) 10)))
      "Lambda should return last form"))

(deftest test-nested-funcall
  "Nested funcall"
  ;; (funcall (lambda (x) (funcall (lambda (y) (+ x y)) 10)) 5)
  ;; => 15
  (ok (= 15 (clysm/tests:compile-and-run
             '(funcall (lambda (x) (funcall (lambda (y) (+ x y)) 10)) 5)))
      "Nested funcall should work"))

(deftest test-funcall-as-argument
  "Funcall result as argument"
  ;; (+ (funcall (lambda (x) x) 10) (funcall (lambda (x) x) 20))
  ;; => 30
  (ok (= 30 (clysm/tests:compile-and-run
             '(+ (funcall (lambda (x) x) 10) (funcall (lambda (x) x) 20))))
      "Funcall results as arguments should work"))

;;; T074: Labels/flet tests
;;; Tests for local function definitions

(deftest test-flet-simple
  "Simple flet with one function"
  ;; (flet ((f (x) (+ x 1)))
  ;;   (f 10))
  ;; => 11
  (ok (= 11 (clysm/tests:compile-and-run
             '(flet ((f (x) (+ x 1)))
                (f 10))))
      "flet function should be callable"))

(deftest test-flet-multiple-functions
  "Flet with multiple functions"
  ;; (flet ((f (x) (+ x 1))
  ;;        (g (x) (* x 2)))
  ;;   (+ (f 10) (g 10)))
  ;; => 11 + 20 = 31
  (ok (= 31 (clysm/tests:compile-and-run
             '(flet ((f (x) (+ x 1))
                     (g (x) (* x 2)))
                (+ (f 10) (g 10)))))
      "Multiple flet functions should work"))

(deftest test-flet-accessing-outer-vars
  "Flet function accessing outer variables"
  ;; (let ((y 100))
  ;;   (flet ((f (x) (+ x y)))
  ;;     (f 5)))
  ;; => 105
  (ok (= 105 (clysm/tests:compile-and-run
              '(let ((y 100))
                 (flet ((f (x) (+ x y)))
                   (f 5)))))
      "flet should access outer variables"))

(deftest test-labels-recursive
  "Labels for recursive local function"
  ;; (labels ((fact (n)
  ;;            (if (= n 0) 1 (* n (fact (- n 1))))))
  ;;   (fact 5))
  ;; => 120
  (ok (= 120 (clysm/tests:compile-and-run
              '(labels ((fact (n)
                          (if (= n 0) 1 (* n (fact (- n 1))))))
                 (fact 5))))
      "labels should allow recursion"))

(deftest test-labels-mutual-recursion
  "Labels for mutually recursive functions"
  ;; (labels ((is-even (n)
  ;;            (if (= n 0) t (is-odd (- n 1))))
  ;;          (is-odd (n)
  ;;            (if (= n 0) nil (is-even (- n 1)))))
  ;;   (is-even 10))
  ;; => t (non-nil)
  (ok (clysm/tests:compile-and-run
       '(labels ((is-even (n)
                   (if (= n 0) t (is-odd (- n 1))))
                 (is-odd (n)
                   (if (= n 0) nil (is-even (- n 1)))))
          (is-even 10)))
      "labels should support mutual recursion"))
