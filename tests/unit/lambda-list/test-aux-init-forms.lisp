;;;; test-aux-init-forms.lisp - Unit test for &aux with complex init forms
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T029 [US3] Unit test for &aux with complex init forms

(in-package #:clysm/tests)

(deftest aux-init-arithmetic
    "Test &aux with arithmetic init forms"
  (testing "compiles &aux with arithmetic expression"
    (let ((form '(defun test-aux-arith (x &aux (y (+ x 1)))
                  y)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "&aux with arithmetic compiled successfully"))
        (error (e)
          (fail (format nil "&aux with arithmetic failed: ~A" e)))))))

(deftest aux-init-function-call
    "Test &aux with function call init forms"
  (testing "compiles &aux with function call"
    (let ((form '(defun test-aux-call (x &aux (y (list x)))
                  y)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "&aux with function call compiled successfully"))
        (error (e)
          (fail (format nil "&aux with function call failed: ~A" e)))))))

(deftest aux-init-conditional
    "Test &aux with conditional init forms"
  (testing "compiles &aux with if expression"
    (let ((form '(defun test-aux-if (x &aux (y (if x 1 0)))
                  y)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "&aux with conditional compiled successfully"))
        (error (e)
          (fail (format nil "&aux with conditional failed: ~A" e)))))))

(deftest aux-init-referencing-previous
    "Test &aux init form referencing previous &aux"
  (testing "compiles &aux referencing previous aux var"
    (let ((form '(defun test-aux-chain (x &aux (y (+ x 1)) (z (+ y 1)))
                  z)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "&aux chain compiled successfully"))
        (error (e)
          (fail (format nil "&aux chain failed: ~A" e)))))))

(deftest aux-init-let-form
    "Test &aux with let form initialization"
  (testing "compiles &aux with let expression"
    (let ((form '(defun test-aux-let (x &aux (y (let ((temp x)) (+ temp 1))))
                  y)))
      (handler-case
          (progn
            (clysm:compile-to-wasm form)
            (ok t "&aux with let compiled successfully"))
        (error (e)
          (fail (format nil "&aux with let failed: ~A" e)))))))
