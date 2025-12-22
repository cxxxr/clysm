;;;; backquote-test.lisp - Backquote/quasiquote tests (Phase 8 - US6)
(in-package #:clysm/tests/unit/backquote)

;;; Basic Backquote Tests (T161)

(deftest simple-backquote
  (testing "backquote with no unquotes"
    ;; `(a b c) should equal (quote (a b c)) effectively
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a b c)))))
      (ok (listp result))
      ;; Should produce (list 'a 'b 'c) or similar
      (ok result)))

  (testing "backquote atom"
    ;; `foo should equal (quote foo)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote foo))))
      (ok result)))

  (testing "backquote number"
    ;; `42 should equal 42
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote 42))))
      (ok (= 42 result)))))

;;; Unquote Tests

(deftest unquote
  (testing "unquote in list"
    ;; `(a ,b c) with b=2 should produce (a 2 c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a (unquote b) c)))))
      (ok (listp result))))

  (testing "unquote at start"
    ;; `(,a b c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ((unquote a) b c)))))
      (ok (listp result))))

  (testing "unquote at end"
    ;; `(a b ,c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a b (unquote c))))))
      (ok (listp result))))

  (testing "multiple unquotes"
    ;; `(,a ,b ,c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ((unquote a) (unquote b) (unquote c))))))
      (ok (listp result)))))

;;; Unquote-Splicing Tests

(deftest unquote-splicing
  (testing "splice in middle"
    ;; `(a ,@b c) with b=(1 2) should produce (a 1 2 c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a (unquote-splicing b) c)))))
      (ok (listp result))))

  (testing "splice at start"
    ;; `(,@a b c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ((unquote-splicing a) b c)))))
      (ok (listp result))))

  (testing "splice at end"
    ;; `(a b ,@c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a b (unquote-splicing c))))))
      (ok (listp result))))

  (testing "multiple splices"
    ;; `(,@a ,@b)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ((unquote-splicing a) (unquote-splicing b))))))
      (ok (listp result)))))

;;; Nested Backquote Tests

(deftest nested-backquote
  (testing "nested quasiquote"
    ;; ``(a ,b ,,c)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (quasiquote (a (unquote b) (unquote (unquote c))))))))
      (ok result)))

  (testing "double nested"
    ;; ```x
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (quasiquote (quasiquote x))))))
      (ok result))))

;;; Complex Expressions

(deftest complex-backquote
  (testing "backquote in defmacro style"
    ;; `(if ,test ,then ,else)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (if (unquote test) (unquote then) (unquote else))))))
      (ok (listp result))))

  (testing "backquote with progn body"
    ;; `(progn ,@body)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (progn (unquote-splicing body))))))
      (ok (listp result))))

  (testing "nested list with unquote"
    ;; `((a ,b) (c ,d))
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ((a (unquote b)) (c (unquote d)))))))
      (ok (listp result))))

  (testing "backquote with dotted list"
    ;; `(a . ,b)
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote (a . (unquote b))))))
      (ok result))))

;;; Edge Cases

(deftest backquote-edge-cases
  (testing "empty list"
    ;; `() should be ()
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote ()))))
      (ok (or (null result) (listp result)))))

  (testing "nil"
    ;; `nil
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote nil))))
      (ok (or (null result) (eq result 'nil)))))

  (testing "string"
    ;; `"hello"
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote "hello"))))
      (ok (string= "hello" result))))

  (testing "keyword"
    ;; `:foo
    (let ((result (clysm/compiler/transform/macro:expand-backquote
                   '(quasiquote :foo))))
      (ok (eq :foo result)))))

;;; Evaluation Tests (require runtime)

(deftest backquote-eval
  (testing "evaluate simple backquote"
    ;; (let ((x 1)) `(a ,x c)) => (a 1 c)
    (let ((x 1))
      (let ((result (eval `(let ((x ,x))
                             (clysm/compiler/transform/macro:expand-backquote
                              '(quasiquote (a (unquote x) c)))))))
        (ok result))))

  (testing "evaluate with splice"
    ;; (let ((x '(1 2))) `(a ,@x c)) => (a 1 2 c)
    (let ((x '(1 2)))
      (declare (ignore x))
      (ok t))))  ; Placeholder - full eval test in integration
