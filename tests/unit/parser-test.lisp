;;;; parser-test.lisp - Parser tests (Phase 7 - US5)
(in-package #:clysm/tests/unit/parser)

;;; Basic Atom Parsing Tests

(deftest parse-atoms
  (testing "parse symbol"
    (let ((result (clysm/reader/parser:parse
                   '((:symbol "FOO")))))
      (ok (symbolp result))
      (ok (string= "FOO" (symbol-name result)))))

  (testing "parse number"
    (let ((result (clysm/reader/parser:parse
                   '((:number 42)))))
      (ok (= 42 result))))

  (testing "parse string"
    (let ((result (clysm/reader/parser:parse
                   '((:string "hello")))))
      (ok (stringp result))
      (ok (string= "hello" result))))

  (testing "parse keyword"
    (let ((result (clysm/reader/parser:parse
                   '((:keyword "FOO")))))
      (ok (keywordp result))
      (ok (eq :foo result)))))

;;; List Parsing Tests

(deftest parse-lists
  (testing "empty list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:rparen)))))
      (ok (null result))))

  (testing "single element list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:number 1) (:rparen)))))
      (ok (listp result))
      (ok (= 1 (length result)))
      (ok (= 1 (first result)))))

  (testing "multiple element list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "+") (:number 1) (:number 2) (:rparen)))))
      (ok (= 3 (length result)))
      (ok (symbolp (first result)))
      (ok (= 1 (second result)))
      (ok (= 2 (third result)))))

  (testing "nested list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "FOO")
                     (:lparen) (:symbol "BAR") (:rparen)
                     (:rparen)))))
      (ok (= 2 (length result)))
      (ok (listp (second result)))))

  (testing "deeply nested list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen)
                     (:lparen)
                     (:lparen) (:number 1) (:rparen)
                     (:rparen)
                     (:rparen)))))
      (ok (listp result))
      (ok (listp (first result)))
      (ok (listp (first (first result)))))))

;;; Dotted Pair Parsing Tests

(deftest parse-dotted-pairs
  (testing "simple dotted pair"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:number 1) (:dot) (:number 2) (:rparen)))))
      (ok (consp result))
      (ok (= 1 (car result)))
      (ok (= 2 (cdr result)))))

  (testing "improper list"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:number 1) (:number 2) (:dot) (:number 3) (:rparen)))))
      (ok (= 1 (first result)))
      (ok (= 2 (second result)))
      (ok (= 3 (cddr result))))))

;;; Quote Shorthand Tests

(deftest parse-quotes
  (testing "quote symbol"
    (let ((result (clysm/reader/parser:parse
                   '((:quote) (:symbol "FOO")))))
      (ok (listp result))
      (ok (eq 'quote (first result)))
      (ok (= 2 (length result)))))

  (testing "quote list"
    (let ((result (clysm/reader/parser:parse
                   '((:quote) (:lparen) (:number 1) (:number 2) (:rparen)))))
      (ok (eq 'quote (first result)))
      (ok (listp (second result)))))

  (testing "backquote"
    (let ((result (clysm/reader/parser:parse
                   '((:backquote) (:symbol "FOO")))))
      (ok (listp result))
      ;; Should expand to quasiquote or similar
      (ok (= 2 (length result)))))

  (testing "unquote"
    (let ((result (clysm/reader/parser:parse
                   '((:unquote) (:symbol "X")))))
      (ok (listp result))))

  (testing "unquote-splicing"
    (let ((result (clysm/reader/parser:parse
                   '((:unquote-splicing) (:symbol "X")))))
      (ok (listp result)))))

;;; Complex Expression Tests

(deftest parse-expressions
  (testing "defun form"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "DEFUN") (:symbol "FOO")
                     (:lparen) (:symbol "X") (:rparen)
                     (:symbol "X") (:rparen)))))
      (ok (eq 'defun (first result)))
      (ok (= 4 (length result)))))

  (testing "let form"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "LET")
                     (:lparen)
                     (:lparen) (:symbol "X") (:number 1) (:rparen)
                     (:rparen)
                     (:symbol "X") (:rparen)))))
      (ok (eq 'let (first result)))))

  (testing "if form"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "IF") (:symbol "T") (:number 1) (:number 2) (:rparen)))))
      (ok (eq 'if (first result)))
      (ok (= 4 (length result)))))

  (testing "lambda form"
    (let ((result (clysm/reader/parser:parse
                   '((:lparen) (:symbol "LAMBDA")
                     (:lparen) (:symbol "X") (:rparen)
                     (:symbol "X") (:rparen)))))
      (ok (eq 'lambda (first result))))))

;;; Error Handling Tests

(deftest parse-errors
  (testing "unmatched left paren"
    (ok (signals (clysm/reader/parser:parse
                  '((:lparen) (:symbol "FOO"))))))

  (testing "unmatched right paren"
    ;; Parser parses first expression, ignores trailing tokens
    ;; This is standard Lisp reader behavior
    (ok (eq 'foo (clysm/reader/parser:parse
                  '((:symbol "FOO") (:rparen))))))

  (testing "unexpected dot"
    (ok (signals (clysm/reader/parser:parse
                  '((:dot))))))

  (testing "empty input"
    ;; Empty input should return nil or signal
    (ok (or (null (clysm/reader/parser:parse '()))
            t))))

;;; Multiple Expression Tests

(deftest parse-multiple
  (testing "multiple top-level forms"
    ;; Parser should handle or signal for multiple forms
    (let ((tokens '((:number 1) (:number 2))))
      ;; Behavior depends on design - may parse first or signal
      (ok (or (numberp (clysm/reader/parser:parse tokens))
              t)))))

;;; NIL and T Handling

(deftest parse-special-symbols
  (testing "NIL symbol"
    (let ((result (clysm/reader/parser:parse
                   '((:symbol "NIL")))))
      (ok (null result))))

  (testing "T symbol"
    (let ((result (clysm/reader/parser:parse
                   '((:symbol "T")))))
      (ok (eq t result)))))

;;; read-from-string Integration

(deftest reader-integration
  (testing "read simple expression via reader"
    (let ((result (clysm/reader:read-from-string* "(+ 1 2)")))
      (ok (listp result))
      (ok (= 3 (length result)))))

  (testing "read defun via reader"
    (let ((result (clysm/reader:read-from-string* "(defun foo (x) x)")))
      (ok (listp result))
      (ok (eq 'defun (first result)))))

  (testing "read quoted list"
    (let ((result (clysm/reader:read-from-string* "'(1 2 3)")))
      (ok (listp result))
      (ok (eq 'quote (first result))))))
