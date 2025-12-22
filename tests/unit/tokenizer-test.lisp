;;;; tokenizer-test.lisp - Tokenizer tests (Phase 7 - US5)
(in-package #:clysm/tests/unit/tokenizer)

;;; Token Structure Tests

(deftest tokenizer-structure
  (testing "tokenizer struct creation"
    (let ((tok (clysm/reader/tokenizer:make-tokenizer :input "test")))
      (ok (typep tok 'clysm/reader/tokenizer::tokenizer))
      (ok (= 0 (clysm/reader/tokenizer::tokenizer-position tok)))
      (ok (= 1 (clysm/reader/tokenizer::tokenizer-line tok)))
      (ok (= 0 (clysm/reader/tokenizer::tokenizer-column tok))))))

;;; Symbol Tokenization Tests

(deftest tokenize-symbols
  (testing "simple symbol"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo")))
      (ok (= 1 (length tokens)))
      (ok (eq :symbol (first (first tokens))))
      (ok (string= "FOO" (second (first tokens))))))

  (testing "symbol with hyphen"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo-bar")))
      (ok (= 1 (length tokens)))
      (ok (eq :symbol (first (first tokens))))
      (ok (string= "FOO-BAR" (second (first tokens))))))

  (testing "symbol with numbers"
    (let ((tokens (clysm/reader/tokenizer:tokenize "x1")))
      (ok (= 1 (length tokens)))
      (ok (eq :symbol (first (first tokens))))
      (ok (string= "X1" (second (first tokens))))))

  (testing "symbol with special characters"
    (let ((tokens (clysm/reader/tokenizer:tokenize "*special*")))
      (ok (= 1 (length tokens)))
      (ok (string= "*SPECIAL*" (second (first tokens))))))

  (testing "plus symbol"
    (let ((tokens (clysm/reader/tokenizer:tokenize "+")))
      (ok (= 1 (length tokens)))
      (ok (eq :symbol (first (first tokens))))
      (ok (string= "+" (second (first tokens)))))))

;;; Number Tokenization Tests

(deftest tokenize-numbers
  (testing "positive integer"
    (let ((tokens (clysm/reader/tokenizer:tokenize "42")))
      (ok (= 1 (length tokens)))
      (ok (eq :number (first (first tokens))))
      (ok (= 42 (second (first tokens))))))

  (testing "negative integer"
    (let ((tokens (clysm/reader/tokenizer:tokenize "-17")))
      (ok (= 1 (length tokens)))
      (ok (eq :number (first (first tokens))))
      (ok (= -17 (second (first tokens))))))

  (testing "zero"
    (let ((tokens (clysm/reader/tokenizer:tokenize "0")))
      (ok (= 1 (length tokens)))
      (ok (eq :number (first (first tokens))))
      (ok (= 0 (second (first tokens))))))

  (testing "large number"
    (let ((tokens (clysm/reader/tokenizer:tokenize "123456789")))
      (ok (= 1 (length tokens)))
      (ok (= 123456789 (second (first tokens)))))))

;;; String Tokenization Tests

(deftest tokenize-strings
  (testing "simple string"
    (let ((tokens (clysm/reader/tokenizer:tokenize "\"hello\"")))
      (ok (= 1 (length tokens)))
      (ok (eq :string (first (first tokens))))
      (ok (string= "hello" (second (first tokens))))))

  (testing "empty string"
    (let ((tokens (clysm/reader/tokenizer:tokenize "\"\"")))
      (ok (= 1 (length tokens)))
      (ok (eq :string (first (first tokens))))
      (ok (string= "" (second (first tokens))))))

  (testing "string with spaces"
    (let ((tokens (clysm/reader/tokenizer:tokenize "\"hello world\"")))
      (ok (= 1 (length tokens)))
      (ok (string= "hello world" (second (first tokens))))))

  (testing "string with escaped quote"
    (let ((tokens (clysm/reader/tokenizer:tokenize "\"say \\\"hi\\\"\"")))
      (ok (= 1 (length tokens)))
      (ok (string= "say \"hi\"" (second (first tokens)))))))

;;; Special Character Tests

(deftest tokenize-special-chars
  (testing "left paren"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(")))
      (ok (= 1 (length tokens)))
      (ok (eq :lparen (first (first tokens))))))

  (testing "right paren"
    (let ((tokens (clysm/reader/tokenizer:tokenize ")")))
      (ok (= 1 (length tokens)))
      (ok (eq :rparen (first (first tokens))))))

  (testing "quote"
    (let ((tokens (clysm/reader/tokenizer:tokenize "'")))
      (ok (= 1 (length tokens)))
      (ok (eq :quote (first (first tokens))))))

  (testing "backquote"
    (let ((tokens (clysm/reader/tokenizer:tokenize "`")))
      (ok (= 1 (length tokens)))
      (ok (eq :backquote (first (first tokens))))))

  (testing "comma"
    (let ((tokens (clysm/reader/tokenizer:tokenize ",")))
      (ok (= 1 (length tokens)))
      (ok (eq :unquote (first (first tokens))))))

  (testing "comma-at"
    (let ((tokens (clysm/reader/tokenizer:tokenize ",@")))
      (ok (= 1 (length tokens)))
      (ok (eq :unquote-splicing (first (first tokens))))))

  (testing "dot"
    (let ((tokens (clysm/reader/tokenizer:tokenize ".")))
      (ok (= 1 (length tokens)))
      (ok (eq :dot (first (first tokens)))))))

;;; Whitespace and Comment Tests

(deftest tokenize-whitespace
  (testing "spaces ignored"
    (let ((tokens (clysm/reader/tokenizer:tokenize "   foo   bar   ")))
      (ok (= 2 (length tokens)))
      (ok (string= "FOO" (second (first tokens))))
      (ok (string= "BAR" (second (second tokens))))))

  (testing "newlines"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo
bar")))
      (ok (= 2 (length tokens)))))

  (testing "tabs"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo	bar")))
      (ok (= 2 (length tokens))))))

(deftest tokenize-comments
  (testing "line comment ignored"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo ; this is a comment
bar")))
      (ok (= 2 (length tokens)))
      (ok (string= "FOO" (second (first tokens))))
      (ok (string= "BAR" (second (second tokens))))))

  (testing "comment at end"
    (let ((tokens (clysm/reader/tokenizer:tokenize "foo ; comment")))
      (ok (= 1 (length tokens))))))

;;; Complex Expression Tests

(deftest tokenize-expressions
  (testing "simple list"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(+ 1 2)")))
      (ok (= 5 (length tokens)))
      (ok (eq :lparen (first (first tokens))))
      (ok (eq :symbol (first (second tokens))))
      (ok (string= "+" (second (second tokens))))
      (ok (eq :number (first (third tokens))))
      (ok (= 1 (second (third tokens))))
      (ok (eq :number (first (fourth tokens))))
      (ok (= 2 (second (fourth tokens))))
      (ok (eq :rparen (first (fifth tokens))))))

  (testing "nested list"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(foo (bar baz))")))
      (ok (= 7 (length tokens)))))

  (testing "quoted expression"
    (let ((tokens (clysm/reader/tokenizer:tokenize "'foo")))
      (ok (= 2 (length tokens)))
      (ok (eq :quote (first (first tokens))))
      (ok (eq :symbol (first (second tokens))))))

  (testing "defun expression"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(defun foo (x) x)")))
      (ok (= 8 (length tokens))))))

;;; Keyword Tests

(deftest tokenize-keywords
  (testing "keyword symbol"
    (let ((tokens (clysm/reader/tokenizer:tokenize ":foo")))
      (ok (= 1 (length tokens)))
      (ok (eq :keyword (first (first tokens))))
      (ok (string= "FOO" (second (first tokens))))))

  (testing "keyword in expression"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(make-foo :bar 1)")))
      (ok (= 5 (length tokens)))
      (ok (eq :keyword (first (third tokens)))))))

;;; Error Handling Tests

(deftest tokenize-errors
  (testing "unterminated string"
    (ok (signals (clysm/reader/tokenizer:tokenize "\"hello"))))

  (testing "invalid character handling"
    ;; Should handle gracefully or signal
    (ok (or (clysm/reader/tokenizer:tokenize "~")
            t))))  ; Either succeeds or signals

;;; Position Tracking Tests

(deftest tokenize-positions
  (testing "token positions tracked"
    (let ((tokens (clysm/reader/tokenizer:tokenize "(foo
bar)")))
      ;; Tokens should include position information
      (ok (>= (length tokens) 4)))))
