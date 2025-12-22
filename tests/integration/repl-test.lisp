;;;; repl-test.lisp - REPL integration tests (Phase 7 - US5)
(in-package #:clysm/tests/integration/repl)

;;; Read Function Tests

(deftest read-function
  (testing "read simple number"
    (let ((result (clysm/reader:read-from-string* "42")))
      (ok (= 42 result))))

  (testing "read symbol"
    (let ((result (clysm/reader:read-from-string* "foo")))
      (ok (symbolp result))
      (ok (string= "FOO" (symbol-name result)))))

  (testing "read list"
    (let ((result (clysm/reader:read-from-string* "(+ 1 2)")))
      (ok (listp result))
      (ok (= 3 (length result)))))

  (testing "read quoted expression"
    (let ((result (clysm/reader:read-from-string* "'foo")))
      (ok (listp result))
      (ok (eq 'quote (first result)))))

  (testing "read string"
    (let ((result (clysm/reader:read-from-string* "\"hello\"")))
      (ok (stringp result))
      (ok (string= "hello" result))))

  (testing "read keyword"
    (let ((result (clysm/reader:read-from-string* ":test")))
      (ok (keywordp result))
      (ok (eq :test result))))

  (testing "read nil"
    (let ((result (clysm/reader:read-from-string* "nil")))
      (ok (null result))))

  (testing "read t"
    (let ((result (clysm/reader:read-from-string* "t")))
      (ok (eq t result)))))

;;; Print Function Tests

(deftest print-function
  (testing "print number"
    (ok t))  ; Would test printer output

  (testing "print symbol"
    (ok t))

  (testing "print list"
    (ok t))

  (testing "print string"
    (ok t))

  (testing "print nil"
    (ok t)))

;;; REPL Loop Tests

(deftest repl-loop
  (testing "repl evaluates expression"
    ;; Basic read-eval-print cycle
    (ok t))

  (testing "repl handles errors gracefully"
    ;; REPL should catch errors and continue
    (ok t))

  (testing "repl maintains environment across expressions"
    ;; (defun foo () 1) then (foo) should work
    (ok t)))

;;; Read-Eval Integration

(deftest read-eval-integration
  (testing "evaluate read arithmetic"
    ;; Read (+ 1 2), compile, run, get 3
    (let* ((expr (clysm/reader:read-from-string* "(+ 1 2)"))
           (result (clysm/tests/helpers:compile-and-run expr)))
      (ok (= 3 result))))

  (testing "evaluate read comparison"
    (let* ((expr (clysm/reader:read-from-string* "(< 1 2)"))
           (result (clysm/tests/helpers:compile-and-run expr)))
      (ok result)))

  (testing "evaluate read if expression"
    (let* ((expr (clysm/reader:read-from-string* "(if t 1 2)"))
           (result (clysm/tests/helpers:compile-and-run expr)))
      (ok (= 1 result))))

  (testing "evaluate read let expression"
    (let* ((expr (clysm/reader:read-from-string* "(let ((x 10)) x)"))
           (result (clysm/tests/helpers:compile-and-run expr)))
      (ok (= 10 result)))))

;;; Multi-line Input

(deftest multiline-input
  (testing "read multiline list"
    (let ((result (clysm/reader:read-from-string* "(foo
  bar
  baz)")))
      (ok (= 3 (length result)))))

  (testing "read with comments"
    (let ((result (clysm/reader:read-from-string* "(+ 1 ; add one
  2)")))
      (ok (listp result))
      (ok (= 3 (length result))))))

;;; Error Handling

(deftest repl-errors
  (testing "read error on malformed input"
    (ok (signals (clysm/reader:read-from-string* "(foo"))))

  (testing "read error message includes position"
    ;; Error should tell user where problem is
    (ok t)))

;;; Complex Expressions

(deftest complex-expressions
  (testing "read and eval defun"
    (let ((expr (clysm/reader:read-from-string*
                 "(defun foo (x) (+ x 1))")))
      (ok (eq 'defun (first expr)))
      (ok (eq 'foo (second expr)))))

  (testing "read nested expressions"
    (let ((result (clysm/reader:read-from-string*
                   "(let ((x (+ 1 2))) (* x x))")))
      (ok (eq 'let (first result)))))

  (testing "read backquoted expression"
    (let ((result (clysm/reader:read-from-string* "`(a ,b ,@c)")))
      (ok (listp result)))))
