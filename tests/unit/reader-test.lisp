;;;; tests/unit/reader-test.lisp - Reader Tests
;;;;
;;;; Tests for the S-expression lexer and parser.

(in-package #:clysm/tests)

;;; ============================================================
;;; Lexer Tests
;;; ============================================================

(defsuite lexer-suite "Lexer tests")

(deftest test-lexer-basic-tokens ()
  "Test basic token lexing"
  (let ((lexer (clysm:make-lexer-from-string "(+ 1 2)")))
    (let ((token (clysm:read-token lexer)))
      (is-eq :lparen (clysm:token-type token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :symbol (clysm:token-type token))
      (is-equal "+" (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :number (clysm:token-type token))
      (is-eq 1 (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :number (clysm:token-type token))
      (is-eq 2 (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :rparen (clysm:token-type token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :eof (clysm:token-type token)))))

(deftest test-lexer-numbers ()
  "Test number token lexing"
  (let ((lexer (clysm:make-lexer-from-string "123 -45 +67 0")))
    (is-eq 123 (clysm:token-value (clysm:read-token lexer)))
    (is-eq -45 (clysm:token-value (clysm:read-token lexer)))
    (is-eq 67 (clysm:token-value (clysm:read-token lexer)))
    (is-eq 0 (clysm:token-value (clysm:read-token lexer)))))

(deftest test-lexer-float-numbers ()
  "Test float number lexing"
  (let ((lexer (clysm:make-lexer-from-string "3.14 -2.5 0.5")))
    (is (= 3.14 (clysm:token-value (clysm:read-token lexer))))
    (is (= -2.5 (clysm:token-value (clysm:read-token lexer))))
    (is (= 0.5 (clysm:token-value (clysm:read-token lexer))))))

(deftest test-lexer-symbols ()
  "Test symbol token lexing"
  (let ((lexer (clysm:make-lexer-from-string "foo BAR *special* +")))
    (is-equal "FOO" (clysm:token-value (clysm:read-token lexer)))
    (is-equal "BAR" (clysm:token-value (clysm:read-token lexer)))
    (is-equal "*SPECIAL*" (clysm:token-value (clysm:read-token lexer)))
    (is-equal "+" (clysm:token-value (clysm:read-token lexer)))))

(deftest test-lexer-strings ()
  "Test string token lexing"
  (let ((lexer (clysm:make-lexer-from-string "\"hello\" \"world\"")))
    (let ((token (clysm:read-token lexer)))
      (is-eq :string (clysm:token-type token))
      (is-equal "hello" (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :string (clysm:token-type token))
      (is-equal "world" (clysm:token-value token)))))

(deftest test-lexer-string-escapes ()
  "Test string escape sequences"
  (let ((lexer (clysm:make-lexer-from-string "\"line1\\nline2\" \"tab\\there\"")))
    (is-equal (format nil "line1~%line2") (clysm:token-value (clysm:read-token lexer)))
    (is-equal (format nil "tab~Chere" #\Tab) (clysm:token-value (clysm:read-token lexer)))))

(deftest test-lexer-characters ()
  "Test character literal lexing"
  (let ((lexer (clysm:make-lexer-from-string "#\\a #\\Space #\\Newline")))
    (let ((token (clysm:read-token lexer)))
      (is-eq :char (clysm:token-type token))
      (is-eq #\a (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :char (clysm:token-type token))
      (is-eq #\Space (clysm:token-value token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :char (clysm:token-type token))
      (is-eq #\Newline (clysm:token-value token)))))

(deftest test-lexer-quote-chars ()
  "Test quote character tokens"
  (let ((lexer (clysm:make-lexer-from-string "' ` , ,@")))
    (is-eq :quote (clysm:token-type (clysm:read-token lexer)))
    (is-eq :backquote (clysm:token-type (clysm:read-token lexer)))
    (is-eq :comma (clysm:token-type (clysm:read-token lexer)))
    (is-eq :comma-at (clysm:token-type (clysm:read-token lexer)))))

(deftest test-lexer-comments ()
  "Test comment skipping"
  (let ((lexer (clysm:make-lexer-from-string "foo ; comment
bar")))
    (is-equal "FOO" (clysm:token-value (clysm:read-token lexer)))
    (is-equal "BAR" (clysm:token-value (clysm:read-token lexer)))))

(deftest test-lexer-line-column ()
  "Test line and column tracking"
  (let ((lexer (clysm:make-lexer-from-string "a
b c")))
    (let ((token (clysm:read-token lexer)))
      (is-eq 1 (clysm:token-line token))
      (is-eq 1 (clysm:token-column token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq 2 (clysm:token-line token))
      (is-eq 1 (clysm:token-column token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq 2 (clysm:token-line token))
      (is-eq 3 (clysm:token-column token)))))

(deftest test-lexer-peek-token ()
  "Test peeking at tokens"
  (let ((lexer (clysm:make-lexer-from-string "a b")))
    (let ((peeked (clysm:peek-token lexer)))
      (is-equal "A" (clysm:token-value peeked)))
    ;; Peek again should return same token
    (let ((peeked (clysm:peek-token lexer)))
      (is-equal "A" (clysm:token-value peeked)))
    ;; Read should consume
    (let ((read (clysm:read-token lexer)))
      (is-equal "A" (clysm:token-value read)))
    ;; Next token
    (let ((read (clysm:read-token lexer)))
      (is-equal "B" (clysm:token-value read)))))

(deftest test-lexer-function-quote ()
  "Test #' function quote"
  (let ((lexer (clysm:make-lexer-from-string "#'foo")))
    (let ((token (clysm:read-token lexer)))
      (is-eq :function (clysm:token-type token)))
    (let ((token (clysm:read-token lexer)))
      (is-eq :symbol (clysm:token-type token))
      (is-equal "FOO" (clysm:token-value token)))))

;;; ============================================================
;;; Parser Tests
;;; ============================================================

(defsuite parser-suite "Parser tests")

(deftest test-parse-numbers ()
  "Test parsing numbers"
  (is-eq 42 (clysm:clysm-read-from-string "42"))
  (is-eq -123 (clysm:clysm-read-from-string "-123"))
  (is (= 3.14 (clysm:clysm-read-from-string "3.14"))))

(deftest test-parse-strings ()
  "Test parsing strings"
  (is-equal "hello" (clysm:clysm-read-from-string "\"hello\""))
  (is-equal "world" (clysm:clysm-read-from-string "\"world\"")))

(deftest test-parse-symbols ()
  "Test parsing symbols"
  (let ((sym (clysm:clysm-read-from-string "foo")))
    (is (symbolp sym))
    (is-equal "FOO" (symbol-name sym))))

(deftest test-parse-nil ()
  "Test parsing nil"
  (is-false (clysm:clysm-read-from-string "nil"))
  (is-false (clysm:clysm-read-from-string "NIL")))

(deftest test-parse-t ()
  "Test parsing t"
  (is-eq t (clysm:clysm-read-from-string "t"))
  (is-eq t (clysm:clysm-read-from-string "T")))

(deftest test-parse-simple-list ()
  "Test parsing simple lists"
  (let ((result (clysm:clysm-read-from-string "(1 2 3)")))
    (is (listp result))
    (is-eq 3 (length result))
    (is-eq 1 (first result))
    (is-eq 2 (second result))
    (is-eq 3 (third result))))

(deftest test-parse-nested-list ()
  "Test parsing nested lists"
  (let ((result (clysm:clysm-read-from-string "((1 2) (3 4))")))
    (is (listp result))
    (is-eq 2 (length result))
    (is (equal '(1 2) (first result)))
    (is (equal '(3 4) (second result)))))

(deftest test-parse-empty-list ()
  "Test parsing empty list"
  (is-false (clysm:clysm-read-from-string "()")))

(deftest test-parse-dotted-pair ()
  "Test parsing dotted pair"
  (let ((result (clysm:clysm-read-from-string "(a . b)")))
    (is (consp result))
    (is (symbolp (car result)))
    (is (symbolp (cdr result)))))

(deftest test-parse-dotted-list ()
  "Test parsing dotted list"
  (let ((result (clysm:clysm-read-from-string "(1 2 . 3)")))
    (is (consp result))
    (is-eq 1 (car result))
    (is-eq 2 (cadr result))
    (is-eq 3 (cddr result))))

(deftest test-parse-quote ()
  "Test parsing quoted expression"
  (let ((result (clysm:clysm-read-from-string "'foo")))
    (is (listp result))
    (is-eq 'quote (car result))
    (is (symbolp (cadr result)))))

(deftest test-parse-function ()
  "Test parsing #' function"
  (let ((result (clysm:clysm-read-from-string "#'foo")))
    (is (listp result))
    (is-eq 'function (car result))
    (is (symbolp (cadr result)))))

(deftest test-parse-multiple-expressions ()
  "Test reading multiple expressions"
  (let ((results (clysm:clysm-read-all-from-string "1 2 3")))
    (is-eq 3 (length results))
    (is (equal '(1 2 3) results))))

(deftest test-parse-with-comments ()
  "Test parsing with comments"
  (let ((result (clysm:clysm-read-from-string "; comment
42")))
    (is-eq 42 result)))

(deftest test-parse-characters ()
  "Test parsing character literals"
  (is-eq #\a (clysm:clysm-read-from-string "#\\a"))
  (is-eq #\Space (clysm:clysm-read-from-string "#\\Space")))

;;; ============================================================
;;; Symbol Interning Tests
;;; ============================================================

(defsuite intern-suite "Symbol interning tests")

(deftest test-reader-intern ()
  "Test symbol interning"
  (let ((clysm:*reader-state* (clysm:make-reader-state)))
    (let ((sym1 (clysm:reader-intern "foo"))
          (sym2 (clysm:reader-intern "FOO"))
          (sym3 (clysm:reader-intern "bar")))
      ;; Same name should return same symbol
      (is-eq sym1 sym2)
      ;; Different name should return different symbol
      (is (not (eq sym1 sym3))))))

(deftest test-reader-find-symbol ()
  "Test finding symbols without interning"
  (let ((clysm:*reader-state* (clysm:make-reader-state)))
    ;; Symbol not interned yet
    (is-false (clysm:reader-find-symbol "notfound"))
    ;; Intern it
    (clysm:reader-intern "notfound")
    ;; Now it should be found
    (is (clysm:reader-find-symbol "notfound"))))

;;; ============================================================
;;; Backquote Expansion Tests
;;; ============================================================

(defsuite backquote-suite "Backquote expansion tests")

(deftest test-backquote-simple ()
  "Test simple backquote"
  (let ((result (clysm:clysm-read-from-string "`foo")))
    (is (listp result))
    (is-eq 'quote (car result))))

(deftest test-backquote-list ()
  "Test backquoted list"
  (let ((result (clysm:clysm-read-from-string "`(a b c)")))
    ;; Should produce something that evaluates to (a b c)
    (is (listp result))))

(deftest test-backquote-unquote ()
  "Test backquote with unquote"
  ;; `(a ,b c) should produce code that splices b
  (let ((result (clysm:clysm-read-from-string "`(a ,b c)")))
    (is (listp result))))

(deftest test-backquote-unquote-splicing ()
  "Test backquote with splicing"
  ;; `(a ,@b c) should produce code that splices b
  (let ((result (clysm:clysm-read-from-string "`(a ,@b c)")))
    (is (listp result))))

;;; ============================================================
;;; Integration Tests
;;; ============================================================

(defsuite reader-integration-suite "Reader integration tests")

(deftest test-read-defun ()
  "Test reading a defun form"
  (let ((result (clysm:clysm-read-from-string
                 "(defun square (x) (* x x))")))
    (is (listp result))
    (is-eq 4 (length result))))

(deftest test-read-complex-form ()
  "Test reading a complex form"
  (let ((result (clysm:clysm-read-from-string
                 "(let ((x 10)
                        (y 20))
                    (+ x y))")))
    (is (listp result))
    (is-eq 3 (length result))))

(deftest test-read-all-defuns ()
  "Test reading multiple defuns"
  (let ((results (clysm:clysm-read-all-from-string
                  "(defun foo () 1)
                   (defun bar () 2)")))
    (is-eq 2 (length results))))
