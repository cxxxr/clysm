;;;; reader-error-test.lisp - Reader error recovery tests (T256-T258)
(in-package #:clysm/tests/unit/reader-error)

;;; Basic error detection tests

(deftest unbalanced-parentheses
  (testing "detects missing closing paren"
    (ok (signals (clysm/reader:read-from-string* "(+ 1 2"))))

  (testing "extra closing paren - reads first form successfully"
    ;; Standard CL behavior: read returns the first complete form
    (let ((result (clysm/reader:read-from-string* "(+ 1 2))")))
      (ok (equal '(+ 1 2) result))))

  (testing "lone closing paren signals error"
    (ok (signals (clysm/reader:read-from-string* ")")))))

(deftest unterminated-string
  (testing "detects unterminated string"
    (ok (signals (clysm/reader:read-from-string* "\"hello")))))

(deftest valid-numbers
  (testing "reads positive number"
    (ok (= 123 (clysm/reader:read-from-string* "123"))))

  (testing "reads negative number"
    (ok (= -42 (clysm/reader:read-from-string* "-42"))))

  (testing "reads zero"
    (ok (= 0 (clysm/reader:read-from-string* "0")))))

;;; Error position reporting tests

(deftest error-position
  (testing "parse error has position information"
    (handler-case
        (clysm/reader:read-from-string* "(+ 1")
      (clysm/reader/parser:parse-error (e)
        (ok (clysm/reader/parser:parse-error-message e))))))

;;; Quote handling tests

(deftest quote-edge-cases
  (testing "quote at end of input signals error"
    (ok (signals (clysm/reader:read-from-string* "'"))))

  (testing "valid quote works"
    (let ((result (clysm/reader:read-from-string* "'foo")))
      (ok (equal '(quote foo) result))))

  (testing "backquote works"
    (let ((result (clysm/reader:read-from-string* "`foo")))
      (ok result))))

;;; Nested structure tests

(deftest deeply-nested
  (testing "handles deeply nested lists"
    (let ((result (clysm/reader:read-from-string* "((((a))))")))
      (ok (equal '((((a)))) result))))

  (testing "handles mixed quotes and lists"
    (let ((result (clysm/reader:read-from-string* "'('a 'b)")))
      (ok result))))

;;; Empty input tests

(deftest empty-input
  (testing "empty string returns nil"
    (ok (null (clysm/reader:read-from-string* ""))))

  (testing "whitespace only returns nil"
    (ok (null (clysm/reader:read-from-string* "   ")))))

;;; Symbol tests

(deftest symbols
  (testing "reads simple symbol"
    (ok (symbolp (clysm/reader:read-from-string* "foo"))))

  (testing "reads symbol with package prefix"
    (let ((result (clysm/reader:read-from-string* "cl:car")))
      (ok (symbolp result)))))

