;;;; tco-test.lisp - Tail call optimization tests (T073)
(in-package #:clysm/tests/integration/tco)

;;; T073: Tail call optimization tests
;;; Tests for return_call and return_call_ref generation

(deftest test-simple-tail-call
  "Simple tail recursive function"
  ;; (defun count-down (n)
  ;;   (if (= n 0) 0 (count-down (- n 1))))
  ;; (count-down 10)
  ;; => 0
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun count-down (n)
                 (if (= n 0) 0 (count-down (- n 1))))
               (count-down 10))))
      "Simple tail recursion should work"))

(deftest test-tail-call-with-accumulator
  "Tail recursive factorial with accumulator"
  ;; (defun fact-iter (n acc)
  ;;   (if (= n 0) acc (fact-iter (- n 1) (* n acc))))
  ;; (defun fact (n) (fact-iter n 1))
  ;; (fact 5)
  ;; => 120
  (ok (= 120 (clysm/tests:compile-and-run
              '(progn
                 (defun fact-iter (n acc)
                   (if (= n 0) acc (fact-iter (- n 1) (* n acc))))
                 (defun fact (n) (fact-iter n 1))
                 (fact 5))))
      "Tail recursive factorial should work"))

(deftest test-deep-tail-recursion
  "Deep tail recursion should not stack overflow"
  ;; (defun sum-to (n acc)
  ;;   (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
  ;; (sum-to 1000 0)
  ;; => 500500
  (ok (= 500500 (clysm/tests:compile-and-run
                 '(progn
                    (defun sum-to (n acc)
                      (if (= n 0) acc (sum-to (- n 1) (+ acc n))))
                    (sum-to 1000 0))))
      "Deep tail recursion (1000 calls) should work without stack overflow"))

(deftest test-very-deep-tail-recursion
  "Very deep tail recursion (10000 calls)"
  ;; Test that TCO actually works by going deep
  ;; Without TCO this would stack overflow
  ;; (defun go-deep (n)
  ;;   (if (= n 0) 0 (go-deep (- n 1))))
  ;; (go-deep 10000)
  ;; => 0
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun go-deep (n)
                 (if (= n 0) 0 (go-deep (- n 1))))
               (go-deep 10000))))
      "Very deep tail recursion (10000 calls) should work"))

(deftest test-tail-position-detection-if-then
  "Tail call in then branch of if"
  ;; (defun f (x)
  ;;   (if (= x 0) x (f (- x 1))))
  ;; The recursive call is in tail position
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun f (x)
                 (if (= x 0) x (f (- x 1))))
               (f 100))))
      "Tail call in then branch should be optimized"))

(deftest test-tail-position-detection-if-else
  "Tail call in else branch of if"
  ;; (defun f (x)
  ;;   (if (/= x 0) (f (- x 1)) x))
  ;; The recursive call is in tail position
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun f (x)
                 (if (/= x 0) (f (- x 1)) x))
               (f 100))))
      "Tail call in else branch should be optimized"))

(deftest test-non-tail-call-preserved
  "Non-tail calls should still work correctly"
  ;; (defun fact (n)
  ;;   (if (= n 0) 1 (* n (fact (- n 1)))))
  ;; The (* n ...) means the recursive call is NOT in tail position
  ;; This should still compute correctly, just not be TCO'd
  (ok (= 120 (clysm/tests:compile-and-run
              '(progn
                 (defun fact (n)
                   (if (= n 0) 1 (* n (fact (- n 1)))))
                 (fact 5))))
      "Non-tail recursive factorial should still work"))

(deftest test-tail-call-in-let
  "Tail call in let body"
  ;; (defun f (n)
  ;;   (let ((m (- n 1)))
  ;;     (if (= m 0) m (f m))))
  ;; The recursive call is the last form in let, so tail position
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun f (n)
                 (let ((m (- n 1)))
                   (if (= m 0) m (f m))))
               (f 100))))
      "Tail call in let body should be optimized"))

(deftest test-mutual-tail-recursion
  "Mutually recursive tail calls"
  ;; (defun f (n) (if (= n 0) 0 (g (- n 1))))
  ;; (defun g (n) (if (= n 0) 0 (f (- n 1))))
  ;; Both calls are in tail position
  (ok (= 0 (clysm/tests:compile-and-run
            '(progn
               (defun f (n) (if (= n 0) 0 (g (- n 1))))
               (defun g (n) (if (= n 0) 0 (f (- n 1))))
               (f 1000))))
      "Mutually recursive tail calls should work"))

(deftest test-tail-call-with-closure
  "Tail call through closure"
  ;; (defun make-counter (n f)
  ;;   (if (= n 0)
  ;;       (funcall f)
  ;;       (make-counter (- n 1) f)))
  ;; (make-counter 100 (lambda () 42))
  ;; => 42
  (ok (= 42 (clysm/tests:compile-and-run
             '(progn
                (defun make-counter (n f)
                  (if (= n 0)
                      (funcall f)
                      (make-counter (- n 1) f)))
                (make-counter 100 (lambda () 42)))))
      "Tail call with closure argument should work"))
