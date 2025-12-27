;;;; reader-test.lisp - Unit tests for Stage 0 S-expression reader
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Tests Core Infrastructure: S-expression reading

(defpackage #:clysm/tests/unit/stage0/reader-test
  (:use #:cl #:rove)
  (:import-from #:clysm/stage0
                #:read-source-string
                #:parse-forms))

(in-package #:clysm/tests/unit/stage0/reader-test)

;;; ============================================================
;;; T021: Unit test for S-expression reading
;;; ============================================================

(deftest test-read-atom
  "Verify reading atoms"
  (let ((forms (read-source-string "foo")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (symbolp (first forms)) "Should be a symbol")))

(deftest test-read-number
  "Verify reading numbers"
  (let ((forms (read-source-string "42")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (= 42 (first forms)) "Should be 42")))

(deftest test-read-string
  "Verify reading strings"
  (let ((forms (read-source-string "\"hello\"")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (string= "hello" (first forms)) "Should be 'hello'")))

(deftest test-read-list
  "Verify reading lists"
  (let ((forms (read-source-string "(a b c)")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (listp (first forms)) "Should be a list")
    (ok (= 3 (length (first forms))) "Should have 3 elements")))

(deftest test-read-nested-list
  "Verify reading nested lists"
  (let ((forms (read-source-string "((a b) (c d))")))
    (ok (listp (first forms)) "Outer should be list")
    (ok (listp (first (first forms))) "First element should be list")))

(deftest test-read-quote
  "Verify reading quoted forms"
  (let ((forms (read-source-string "'foo")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (equal '(quote foo) (first forms)) "Should be (quote foo)")))

(deftest test-read-backquote
  "Verify reading backquoted forms"
  (let ((forms (read-source-string "`(a ,b ,@c)")))
    (ok (= 1 (length forms)) "Should have one form")
    ;; Backquote expansion may vary by implementation
    (ok (listp (first forms)) "Should be a list")))

(deftest test-read-character
  "Verify reading character literals"
  (let ((forms (read-source-string "#\\a")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (characterp (first forms)) "Should be a character")
    (ok (char= #\a (first forms)) "Should be #\\a")))

(deftest test-read-keyword
  "Verify reading keywords"
  (let ((forms (read-source-string ":foo")))
    (ok (= 1 (length forms)) "Should have one form")
    (ok (keywordp (first forms)) "Should be a keyword")
    (ok (eq :foo (first forms)) "Should be :foo")))

(deftest test-read-defun
  "Verify reading defun form"
  (let ((forms (read-source-string "(defun foo (x) (+ x 1))")))
    (ok (= 1 (length forms)) "Should have one form")
    (let ((form (first forms)))
      (ok (eq 'defun (first form)) "Should start with defun")
      (ok (eq 'foo (second form)) "Function name should be foo"))))

;;; ============================================================
;;; Parse Forms Tests
;;; ============================================================

(deftest test-parse-forms-returns-list
  "Verify parse-forms returns a list"
  (let ((forms (parse-forms "(+ 1 2)")))
    (ok (listp forms) "Should return a list")))
