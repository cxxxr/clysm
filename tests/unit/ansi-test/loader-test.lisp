;;;; loader-test.lisp - Unit tests for ANSI test loader
;;;;
;;;; T018: parse-deftest tests
;;;; T019: load-category-tests tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T018: parse-deftest tests
;;; ==========================================================================

;; The DEFTEST symbol is interned in the clysm/ansi-test package at read time
(defparameter *deftest-sym* 'clysm/ansi-test::deftest
  "The DEFTEST symbol used in the ansi-test suite")

(deftest parse-deftest-basic
  "Test that parse-deftest correctly parses a simple DEFTEST form"
  (let ((tc (parse-deftest (list *deftest-sym* 'cons.1 '(cons 1 2) '(1 . 2)) "cons")))
    (ok (test-case-p tc) "Returns a test-case struct")
    (ok (eq 'cons.1 (test-case-name tc)) "Name is extracted correctly")
    (ok (string= "cons" (test-case-category tc)) "Category is set")
    (ok (equal '(cons 1 2) (test-case-form tc)) "Form is extracted")
    (ok (equal '((1 . 2)) (test-case-expected-values tc)) "Expected values extracted")))

(deftest parse-deftest-multiple-values
  "Test that parse-deftest handles multiple expected values"
  (let ((tc (parse-deftest (list *deftest-sym* 'values.1 '(values 1 2 3) 1 2 3) "data-and-control-flow")))
    (ok (test-case-p tc) "Returns a test-case struct")
    (ok (equal '(1 2 3) (test-case-expected-values tc)) "Multiple expected values preserved")))

(deftest parse-deftest-with-source-file
  "Test that parse-deftest stores source file path"
  (let ((tc (parse-deftest (list *deftest-sym* 'test.1 t t) "test" #p"/path/to/test.lsp")))
    (ok (equal #p"/path/to/test.lsp" (test-case-source-file tc)) "Source file stored")))

(deftest parse-deftest-invalid-forms
  "Test that parse-deftest returns NIL for invalid forms"
  (ok (null (parse-deftest '(not-deftest foo bar) "test")) "Non-deftest returns nil")
  (ok (null (parse-deftest (list *deftest-sym* 'foo) "test")) "Too few elements returns nil")
  (ok (null (parse-deftest 'atom "test")) "Atom returns nil"))

;;; ==========================================================================
;;; T019: load-category-tests tests
;;; ==========================================================================

(deftest list-categories-returns-list
  "Test that list-categories returns a non-empty list"
  (let ((categories (list-categories)))
    (ok (listp categories) "Returns a list")
    ;; There should be at least some categories in the ansi-test suite
    (ok (> (length categories) 0) "Returns non-empty list")))

(deftest list-categories-structure
  "Test that list-categories returns properly structured entries"
  (let ((categories (list-categories)))
    (when categories
      (let ((first (car categories)))
        (ok (listp first) "Each entry is a list")
        (ok (stringp (car first)) "First element is category name")
        (ok (member :count first) "Contains :count key")
        (ok (member :status first) "Contains :status key")))))

(deftest list-categories-skipped-filtering
  "Test that list-categories respects include-skipped parameter"
  (let ((without-skipped (list-categories))
        (with-skipped (list-categories :include-skipped t)))
    ;; With skipped should include more or equal categories
    (ok (>= (length with-skipped) (length without-skipped))
        "Including skipped adds or keeps same number of categories")))
