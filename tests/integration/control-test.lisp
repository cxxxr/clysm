;;;; control-test.lisp - Control flow tests
(in-package #:clysm/tests/integration/control)

;;; T030: if conditional tests

(deftest test-if-true-branch
  "IF with true condition takes then branch"
  (ok (= 1 (clysm/tests:compile-and-run '(if t 1 2)))
      "(if t 1 2) should equal 1"))

(deftest test-if-false-branch
  "IF with false condition takes else branch"
  (ok (= 2 (clysm/tests:compile-and-run '(if nil 1 2)))
      "(if nil 1 2) should equal 2"))

(deftest test-if-no-else
  "IF without else returns NIL on false"
  (ok (null (clysm/tests:compile-and-run '(if nil 1)))
      "(if nil 1) should return NIL"))

(deftest test-if-with-comparison
  "IF with comparison condition"
  (ok (= 10 (clysm/tests:compile-and-run '(if (< 1 2) 10 20)))
      "(if (< 1 2) 10 20) should equal 10"))

(deftest test-if-with-arithmetic
  "IF condition with arithmetic result"
  (ok (= 100 (clysm/tests:compile-and-run '(if (= (+ 1 2) 3) 100 200)))
      "(if (= (+ 1 2) 3) 100 200) should equal 100"))

(deftest test-nested-if
  "Nested IF expressions"
  (ok (= 3 (clysm/tests:compile-and-run
            '(if t (if nil 1 (if t 3 2)) 0)))
      "Nested if should work correctly"))

(deftest test-if-in-arithmetic
  "IF result used in arithmetic"
  (ok (= 15 (clysm/tests:compile-and-run
             '(+ (if t 5 0) (if nil 0 10))))
      "(+ (if t 5 0) (if nil 0 10)) should equal 15"))

(deftest test-when-form
  "WHEN macro expands to if"
  (ok (= 42 (clysm/tests:compile-and-run '(when t 42)))
      "(when t 42) should equal 42"))

(deftest test-when-false
  "WHEN with false condition returns NIL"
  (ok (null (clysm/tests:compile-and-run '(when nil 42)))
      "(when nil 42) should return NIL"))

(deftest test-unless-form
  "UNLESS macro expands to if-not"
  (ok (= 42 (clysm/tests:compile-and-run '(unless nil 42)))
      "(unless nil 42) should equal 42"))

(deftest test-cond-first-true
  "COND takes first true clause"
  (ok (= 1 (clysm/tests:compile-and-run
            '(cond (t 1) (t 2) (t 3))))
      "(cond (t 1) ...) should equal 1"))

(deftest test-cond-second-true
  "COND skips false clauses"
  (ok (= 2 (clysm/tests:compile-and-run
            '(cond (nil 1) (t 2) (t 3))))
      "(cond (nil 1) (t 2) ...) should equal 2"))

(deftest test-cond-no-match
  "COND with no matching clause returns NIL"
  (ok (null (clysm/tests:compile-and-run
             '(cond (nil 1) (nil 2))))
      "(cond (nil 1) (nil 2)) should return NIL"))

(deftest test-and-short-circuit
  "AND short-circuits on NIL"
  ;; The second form should not affect the result since AND short-circuits
  (ok (null (clysm/tests:compile-and-run '(and nil 999)))
      "(and nil 999) should return NIL"))

(deftest test-and-all-true
  "AND returns last value when all true"
  (ok (= 3 (clysm/tests:compile-and-run '(and 1 2 3)))
      "(and 1 2 3) should equal 3"))

(deftest test-or-short-circuit
  "OR short-circuits on non-NIL"
  ;; The second form should not affect the result since OR short-circuits
  (ok (= 1 (clysm/tests:compile-and-run '(or 1 999)))
      "(or 1 999) should return 1"))

(deftest test-or-all-false
  "OR returns NIL when all false"
  (ok (null (clysm/tests:compile-and-run '(or nil nil nil)))
      "(or nil nil nil) should return NIL"))
