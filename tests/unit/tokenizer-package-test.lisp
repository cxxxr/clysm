;;;; tokenizer-package-test.lisp - Package qualifier tokenization tests (013-package-system)
(in-package #:clysm/tests/unit/tokenizer-package)

;;; Package Qualifier Tokenization Tests (US3)
;;; These tests verify the tokenizer correctly handles:
;;; - pkg:symbol (single colon - external symbol)
;;; - pkg::symbol (double colon - internal symbol)
;;; - :keyword (leading colon - keyword)
;;; - Error cases: ::symbol, pkg:, empty symbol

;;; T042: Single colon (external symbol access)
(deftest tokenize-single-colon
  (testing "pkg:symbol tokenizes to qualified-symbol with :external t"
    (let ((tokens (clysm/reader/tokenizer:tokenize "cl:car")))
      (ok (= 1 (length tokens)))
      (let ((token (first tokens)))
        (ok (eq :qualified-symbol (first token)))
        (ok (string= "CL" (getf (second token) :package-name)))
        (ok (string= "CAR" (getf (second token) :symbol-name)))
        (ok (eq t (getf (second token) :external))))))
  (testing "pkg:symbol with longer names"
    (let ((tokens (clysm/reader/tokenizer:tokenize "my-package:some-symbol")))
      (ok (= 1 (length tokens)))
      (let ((token (first tokens)))
        (ok (eq :qualified-symbol (first token)))
        (ok (string= "MY-PACKAGE" (getf (second token) :package-name)))
        (ok (string= "SOME-SYMBOL" (getf (second token) :symbol-name)))
        (ok (eq t (getf (second token) :external)))))))

;;; T043: Double colon (internal symbol access)
(deftest tokenize-double-colon
  (testing "pkg::symbol tokenizes to qualified-symbol with :external nil"
    (let ((tokens (clysm/reader/tokenizer:tokenize "cl::internal-fn")))
      (ok (= 1 (length tokens)))
      (let ((token (first tokens)))
        (ok (eq :qualified-symbol (first token)))
        (ok (string= "CL" (getf (second token) :package-name)))
        (ok (string= "INTERNAL-FN" (getf (second token) :symbol-name)))
        (ok (null (getf (second token) :external))))))
  (testing "pkg::symbol with longer names"
    (let ((tokens (clysm/reader/tokenizer:tokenize "my-package::hidden-symbol")))
      (ok (= 1 (length tokens)))
      (let ((token (first tokens)))
        (ok (eq :qualified-symbol (first token)))
        (ok (string= "MY-PACKAGE" (getf (second token) :package-name)))
        (ok (string= "HIDDEN-SYMBOL" (getf (second token) :symbol-name)))
        (ok (null (getf (second token) :external)))))))

;;; T044: Leading colon (keyword)
(deftest tokenize-keyword
  (testing ":keyword tokenizes to keyword token"
    (let ((tokens (clysm/reader/tokenizer:tokenize ":foo")))
      (ok (= 1 (length tokens)))
      (ok (eq :keyword (first (first tokens))))
      (ok (string= "FOO" (second (first tokens))))))
  (testing ":keyword with hyphenated name"
    (let ((tokens (clysm/reader/tokenizer:tokenize ":some-key")))
      (ok (= 1 (length tokens)))
      (ok (eq :keyword (first (first tokens))))
      (ok (string= "SOME-KEY" (second (first tokens)))))))

;;; T045: Error on ::symbol (invalid - no package name)
(deftest tokenize-double-colon-no-package-error
  (testing "::symbol signals an error (no package name before ::)"
    (ok (signals (clysm/reader/tokenizer:tokenize "::foo")))))

;;; T046: Error on pkg: (no symbol name)
(deftest tokenize-no-symbol-name-error
  (testing "pkg: signals an error (no symbol name after colon)"
    (ok (signals (clysm/reader/tokenizer:tokenize "pkg:"))))
  (testing "pkg:: signals an error (no symbol name after double colon)"
    (ok (signals (clysm/reader/tokenizer:tokenize "pkg::"))))
  (testing "symbol at end of expression is fine"
    ;; pkg followed by space and colon is NOT an error, it's just pkg symbol
    (let ((tokens (clysm/reader/tokenizer:tokenize "pkg :foo")))
      (ok (= 2 (length tokens)))
      (ok (eq :symbol (first (first tokens))))
      (ok (eq :keyword (first (second tokens)))))))
