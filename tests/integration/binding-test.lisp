;;;; binding-test.lisp - Variable binding tests
(in-package #:clysm/tests/integration/binding)

;;; T031: let/let* binding tests

(deftest test-let-single-binding
  "LET with single binding"
  (ok (= 42 (clysm/tests:compile-and-run '(let ((x 42)) x)))
      "(let ((x 42)) x) should equal 42"))

(deftest test-let-multiple-bindings
  "LET with multiple bindings"
  (ok (= 30 (clysm/tests:compile-and-run
             '(let ((x 10) (y 20)) (+ x y))))
      "(let ((x 10) (y 20)) (+ x y)) should equal 30"))

(deftest test-let-shadowing
  "LET shadows outer bindings"
  (ok (= 5 (clysm/tests:compile-and-run
            '(let ((x 10))
               (let ((x 5))
                 x))))
      "Inner let should shadow outer x"))

(deftest test-let-outer-visible
  "Outer binding visible after inner scope"
  (ok (= 10 (clysm/tests:compile-and-run
             '(let ((x 10))
                (let ((y 5))
                  y)
                x)))
      "Outer x should be visible after inner let"))

(deftest test-let-parallel-binding
  "LET bindings are parallel (not sequential)"
  (ok (= 4 (clysm/tests:compile-and-run
            '(let ((x 1))
               (let ((x 2) (y (+ x 1)))
                 (+ x y)))))
      "y sees old x (1), so y=2; new x=2; result: x+y=4"))

(deftest test-let*-sequential-binding
  "LET* bindings are sequential"
  (ok (= 5 (clysm/tests:compile-and-run
            '(let* ((x 2) (y (+ x 1)))
               (+ x y))))
      "(let* ((x 2) (y (+ x 1))) (+ x y)) = 2 + 3 = 5"))

(deftest test-let*-chain
  "LET* with chain of dependencies"
  (ok (= 6 (clysm/tests:compile-and-run
            '(let* ((a 1) (b (+ a 1)) (c (+ b 1)))
               (+ a b c))))
      "(let* ((a 1) (b 2) (c 3)) (+ a b c)) = 6"))

(deftest test-let-body-multiple-forms
  "LET with multiple body forms"
  (ok (= 3 (clysm/tests:compile-and-run
            '(let ((x 1))
               (+ x 1)  ; ignored
               (+ x 2))))  ; returned
      "LET returns last form in body"))

(deftest test-let-with-nil-init
  "LET with NIL initialization"
  (ok (null (clysm/tests:compile-and-run
             '(let ((x nil)) x)))
      "(let ((x nil)) x) should return NIL"))

(deftest test-let-with-arithmetic-init
  "LET with arithmetic in initialization"
  (ok (= 15 (clysm/tests:compile-and-run
             '(let ((x (* 3 5))) x)))
      "(let ((x (* 3 5))) x) should equal 15"))

(deftest test-nested-let-access
  "Nested let accessing outer variable"
  (ok (= 110 (clysm/tests:compile-and-run
              '(let ((x 100))
                 (let ((y 10))
                   (+ x y)))))
      "Inner let should access outer x"))

(deftest test-let-shorthand
  "LET with shorthand (x) for (x nil)"
  (ok (null (clysm/tests:compile-and-run '(let (x) x)))
      "(let (x) x) should equal NIL"))
