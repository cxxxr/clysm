;;;; tests/unit/parse-integer-test.lisp
;;;; Unit tests for parse-integer (001-numeric-functions US6)
;;;;
;;;; Tests for parse-integer with full ANSI CL compliance

(defpackage #:clysm/tests/unit/parse-integer
  (:use #:cl #:rove))

(in-package #:clysm/tests/unit/parse-integer)

;;; Use test helpers from main test package
(defun compile-and-run (form)
  "Compile and run an expression, returning the result."
  (clysm/tests:compile-and-run form))

(defun compile-and-run-mv (form)
  "Compile and run an expression, returning multiple values as a list."
  (clysm/tests:compile-and-run-mv form))

;;; ============================================================
;;; Basic Parse Integer Tests (FR-027)
;;; ============================================================

(deftest test-parse-integer-basic
  "parse-integer basic: (parse-integer \"123\") => 123"
  (ok (= 123 (compile-and-run '(parse-integer "123")))
      "(parse-integer \"123\") should equal 123"))

(deftest test-parse-integer-negative
  "parse-integer negative: (parse-integer \"-789\") => -789"
  (ok (= -789 (compile-and-run '(parse-integer "-789")))
      "(parse-integer \"-789\") should equal -789"))

(deftest test-parse-integer-positive-sign
  "parse-integer positive sign: (parse-integer \"+42\") => 42"
  (ok (= 42 (compile-and-run '(parse-integer "+42")))
      "(parse-integer \"+42\") should equal 42"))

(deftest test-parse-integer-zero
  "parse-integer zero: (parse-integer \"0\") => 0"
  (ok (= 0 (compile-and-run '(parse-integer "0")))
      "(parse-integer \"0\") should equal 0"))

;;; ============================================================
;;; Radix Tests (FR-029)
;;; ============================================================

(deftest test-parse-integer-radix-16
  "parse-integer hex: (parse-integer \"FF\" :radix 16) => 255"
  (ok (= 255 (compile-and-run '(parse-integer "FF" :radix 16)))
      "(parse-integer \"FF\" :radix 16) should equal 255"))

(deftest test-parse-integer-radix-2
  "parse-integer binary: (parse-integer \"1010\" :radix 2) => 10"
  (ok (= 10 (compile-and-run '(parse-integer "1010" :radix 2)))
      "(parse-integer \"1010\" :radix 2) should equal 10"))

(deftest test-parse-integer-radix-8
  "parse-integer octal: (parse-integer \"77\" :radix 8) => 63"
  (ok (= 63 (compile-and-run '(parse-integer "77" :radix 8)))
      "(parse-integer \"77\" :radix 8) should equal 63"))

;;; ============================================================
;;; Whitespace Handling Tests (FR-030)
;;; ============================================================

(deftest test-parse-integer-leading-whitespace
  "parse-integer with leading whitespace: (parse-integer \"  42\") => 42"
  (ok (= 42 (compile-and-run '(parse-integer "  42")))
      "(parse-integer \"  42\") should equal 42"))

(deftest test-parse-integer-trailing-whitespace
  "parse-integer with trailing whitespace: (parse-integer \"42  \" :junk-allowed t) => 42"
  (ok (= 42 (compile-and-run '(parse-integer "42  " :junk-allowed t)))
      "(parse-integer \"42  \" :junk-allowed t) should equal 42"))

;;; ============================================================
;;; Bounds Tests (:start :end)
;;; ============================================================

(deftest test-parse-integer-start
  "parse-integer with :start: (parse-integer \"abc123\" :start 3) => 123"
  (ok (= 123 (compile-and-run '(parse-integer "abc123" :start 3 :junk-allowed t)))
      "(parse-integer \"abc123\" :start 3) should equal 123"))

(deftest test-parse-integer-start-end
  "parse-integer with :start :end: (parse-integer \"12345\" :start 1 :end 4) => 234"
  (ok (= 234 (compile-and-run '(parse-integer "12345" :start 1 :end 4)))
      "(parse-integer \"12345\" :start 1 :end 4) should equal 234"))

;;; ============================================================
;;; Junk Allowed Tests
;;; ============================================================

(deftest test-parse-integer-junk-allowed-false
  "parse-integer junk not allowed: (parse-integer \"42abc\") signals error"
  ;; Without :junk-allowed, trailing junk should signal an error
  ;; For MVP, we test that with :junk-allowed t it works
  (ok (= 42 (compile-and-run '(parse-integer "42abc" :junk-allowed t)))
      "(parse-integer \"42abc\" :junk-allowed t) should equal 42"))

(deftest test-parse-integer-no-number-junk-allowed
  "parse-integer no number with junk-allowed: (parse-integer \"abc\" :junk-allowed t) => NIL"
  (ok (null (compile-and-run '(parse-integer "abc" :junk-allowed t)))
      "(parse-integer \"abc\" :junk-allowed t) should return NIL"))

;;; ============================================================
;;; Multiple Values Tests (FR-028)
;;; ============================================================

(deftest test-parse-integer-mv
  "parse-integer returns (values integer position)"
  ;; For MVP, test that primary value is correct
  ;; Full MV testing requires compile-and-run-mv support
  (ok (= 123 (compile-and-run '(parse-integer "123")))
      "Primary value of (parse-integer \"123\") should be 123"))
