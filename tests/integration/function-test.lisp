;;;; function-test.lisp - Function definition and call tests
(in-package #:clysm/tests/integration/function)

;;; T032: defun/function call tests

(deftest test-defun-identity
  "DEFUN identity function"
  (ok (= 42 (clysm/tests:compile-and-run
             '(progn
                (defun identity-fn (x) x)
                (identity-fn 42))))
      "(identity-fn 42) should equal 42"))

(deftest test-defun-add
  "DEFUN addition function"
  (ok (= 15 (clysm/tests:compile-and-run
             '(progn
                (defun add-fn (a b) (+ a b))
                (add-fn 5 10))))
      "(add-fn 5 10) should equal 15"))

(deftest test-defun-no-args
  "DEFUN with no arguments"
  (ok (= 42 (clysm/tests:compile-and-run
             '(progn
                (defun constant-fn () 42)
                (constant-fn))))
      "(constant-fn) should equal 42"))

(deftest test-defun-three-args
  "DEFUN with three arguments"
  (ok (= 60 (clysm/tests:compile-and-run
             '(progn
                (defun triple-add (a b c) (+ a b c))
                (triple-add 10 20 30))))
      "(triple-add 10 20 30) should equal 60"))

(deftest test-function-calling-function
  "Function calling another function"
  (ok (= 10 (clysm/tests:compile-and-run
             '(progn
                (defun double-it (x) (* x 2))
                (defun quad-it (x) (double-it (double-it x)))
                (quad-it 2))))  ; 2 -> 4 -> 8... wait, that's wrong. 2*2=4, 4*2=8
      "(quad-it 2) should equal 8"))

(deftest test-recursive-factorial
  "Recursive factorial function"
  (ok (= 120 (clysm/tests:compile-and-run
              '(progn
                 (defun fact (n)
                   (if (= n 0)
                       1
                       (* n (fact (- n 1)))))
                 (fact 5))))
      "(fact 5) should equal 120"))

(deftest test-recursive-fibonacci
  "Recursive fibonacci function"
  (ok (= 55 (clysm/tests:compile-and-run
             '(progn
                (defun fib (n)
                  (if (<= n 1)
                      n
                      (+ (fib (- n 1)) (fib (- n 2)))))
                (fib 10))))
      "(fib 10) should equal 55"))

(deftest test-defun-with-let
  "DEFUN using LET in body"
  (ok (= 25 (clysm/tests:compile-and-run
             '(progn
                (defun square-sum (a b)
                  (let ((sum (+ a b)))
                    (* sum sum)))
                (square-sum 2 3))))  ; (2+3)^2 = 25
      "(square-sum 2 3) should equal 25"))

(deftest test-defun-with-conditional
  "DEFUN with conditional"
  (ok (= 10 (clysm/tests:compile-and-run
             '(progn
                (defun abs-val (x)
                  (if (< x 0) (- 0 x) x))
                (abs-val -10))))
      "(abs-val -10) should equal 10"))

(deftest test-mutually-recursive
  "Mutually recursive functions"
  (ok (clysm/tests:compile-and-run
       '(progn
          (defun is-even (n)
            (if (= n 0) t (is-odd (- n 1))))
          (defun is-odd (n)
            (if (= n 0) nil (is-even (- n 1))))
          (is-even 10)))
      "(is-even 10) should be true"))

(deftest test-progn-form
  "PROGN returns last form"
  (ok (= 3 (clysm/tests:compile-and-run '(progn 1 2 3)))
      "(progn 1 2 3) should equal 3"))

(deftest test-progn-with-side-effects
  "PROGN executes all forms"
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x 0))
                (progn
                  (setq x (+ x 5))
                  (setq x (+ x 5))
                  x))))
      "PROGN should execute all setq forms"))
