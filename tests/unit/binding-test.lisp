;;;; binding-test.lisp - Binding stack tests (Phase 7 - US5)
(in-package #:clysm/tests/unit/binding)

;;; Basic Binding Stack Operations

(deftest binding-stack-basic
  (testing "binding stack starts empty"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (ok (null clysm/runtime/special-vars:*binding-stack*))))

  (testing "push binding adds to stack"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'x 42)
      (ok (= 1 (length clysm/runtime/special-vars:*binding-stack*)))))

  (testing "push binding stores symbol and old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'test-sym 100)
      (let ((frame (first clysm/runtime/special-vars:*binding-stack*)))
        (ok (eq 'test-sym (car frame)))
        (ok (= 100 (cdr frame))))))

  (testing "pop binding returns old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'y 999)
      (let ((old-val (clysm/runtime/special-vars:pop-binding)))
        (ok (= 999 old-val))))))

;;; Stack Order Tests

(deftest binding-stack-order
  (testing "LIFO order"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'a 1)
      (clysm/runtime/special-vars:push-binding 'b 2)
      (clysm/runtime/special-vars:push-binding 'c 3)
      (ok (= 3 (clysm/runtime/special-vars:pop-binding)))
      (ok (= 2 (clysm/runtime/special-vars:pop-binding)))
      (ok (= 1 (clysm/runtime/special-vars:pop-binding)))))

  (testing "empty stack pop returns nil"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (ok (null (clysm/runtime/special-vars:pop-binding))))))

;;; Multiple Binding Tests

(deftest multiple-bindings
  (testing "multiple bindings of same symbol"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'x 1)
      (clysm/runtime/special-vars:push-binding 'x 10)  ; shadow
      (ok (= 2 (length clysm/runtime/special-vars:*binding-stack*)))
      (ok (= 10 (clysm/runtime/special-vars:pop-binding)))
      (ok (= 1 (clysm/runtime/special-vars:pop-binding)))))

  (testing "interleaved bindings"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'a 1)
      (clysm/runtime/special-vars:push-binding 'b 2)
      (clysm/runtime/special-vars:push-binding 'a 11)  ; shadow a
      (ok (= 3 (length clysm/runtime/special-vars:*binding-stack*)))
      ;; Pop in LIFO order
      (ok (= 11 (clysm/runtime/special-vars:pop-binding)))  ; second a
      (ok (= 2 (clysm/runtime/special-vars:pop-binding)))   ; b
      (ok (= 1 (clysm/runtime/special-vars:pop-binding)))))) ; first a

;;; Shallow Binding Semantics

(deftest shallow-binding
  (testing "push saves old value"
    ;; In shallow binding, we save old value before changing
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding '*test-var* 'old-value)
      (let ((frame (first clysm/runtime/special-vars:*binding-stack*)))
        (ok (eq 'old-value (cdr frame))))))

  (testing "pop restores old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding '*restore-test* 'original)
      (let ((restored (clysm/runtime/special-vars:pop-binding)))
        (ok (eq 'original restored))))))

;;; Cleanup and Unwinding

(deftest binding-cleanup
  (testing "bindings can be popped in bulk"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'a 1)
      (clysm/runtime/special-vars:push-binding 'b 2)
      (clysm/runtime/special-vars:push-binding 'c 3)
      ;; Pop all
      (clysm/runtime/special-vars:pop-binding)
      (clysm/runtime/special-vars:pop-binding)
      (clysm/runtime/special-vars:pop-binding)
      (ok (null clysm/runtime/special-vars:*binding-stack*))))

  (testing "unwind can restore multiple bindings"
    ;; Simulating unwind-protect cleanup
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'x 10)
      (clysm/runtime/special-vars:push-binding 'y 20)
      ;; Unwind both
      (let ((val1 (clysm/runtime/special-vars:pop-binding))
            (val2 (clysm/runtime/special-vars:pop-binding)))
        (ok (= 20 val1))
        (ok (= 10 val2))))))

;;; Edge Cases

(deftest binding-edge-cases
  (testing "nil as old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 'n nil)
      (ok (null (clysm/runtime/special-vars:pop-binding)))))

  (testing "symbol as old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil))
      (clysm/runtime/special-vars:push-binding 's 'some-symbol)
      (ok (eq 'some-symbol (clysm/runtime/special-vars:pop-binding)))))

  (testing "list as old value"
    (let ((clysm/runtime/special-vars:*binding-stack* nil)
          (the-list '(1 2 3)))
      (clysm/runtime/special-vars:push-binding 'l the-list)
      (ok (equal '(1 2 3) (clysm/runtime/special-vars:pop-binding))))))
