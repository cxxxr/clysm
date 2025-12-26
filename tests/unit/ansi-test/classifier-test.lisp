;;;; classifier-test.lisp - Unit tests for ANSI test classifier
;;;;
;;;; T021: classify-result tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T021: classify-result (PASS/FAIL/SKIP) tests
;;; ==========================================================================

(deftest compare-values-match
  "Test that compare-values detects matching values"
  (ok (eq :match (compare-values 42 42)) "Integers match")
  (ok (eq :match (compare-values nil -2147483648)) "NIL sentinel matches nil"))

(deftest compare-values-mismatch
  "Test that compare-values detects mismatching values"
  (ok (eq :mismatch (compare-values 42 99)) "Different integers mismatch")
  (ok (eq :mismatch (compare-values nil 42)) "NIL vs integer mismatch"))

(deftest compare-values-unverifiable
  "Test that compare-values returns unverifiable for non-fixnum sentinel"
  (ok (eq :unverifiable (compare-values 'any-value -2147483647))
      "Non-fixnum sentinel returns unverifiable"))

(deftest compare-values-single-element-list
  "Test that compare-values unwraps single-element expected lists"
  (ok (eq :match (compare-values '(42) 42)) "Single element list matches value"))

(deftest classify-result-pass
  "Test that classify-result returns :pass for matching values"
  (let* ((tc (make-test-case
              :name 'test.pass
              :category "test"
              :form '(+ 1 2)
              :expected-values '(3)))
         (result (classify-result tc 3 nil 10)))
    (ok (test-result-p result) "Returns test-result struct")
    (ok (eq :pass (test-result-status result)) "Status is :pass")
    (ok (= 10 (test-result-execution-time-ms result)) "Execution time preserved")))

(deftest classify-result-fail-mismatch
  "Test that classify-result returns :fail for mismatching values"
  (let* ((tc (make-test-case
              :name 'test.fail
              :category "test"
              :form '(+ 1 2)
              :expected-values '(5)))
         (result (classify-result tc 3 nil 10)))
    (ok (eq :fail (test-result-status result)) "Status is :fail")
    (ok (test-result-error-message result) "Error message is set")))

(deftest classify-result-fail-error
  "Test that classify-result returns :fail for execution errors"
  (let* ((tc (make-test-case
              :name 'test.error
              :category "test"
              :form '(error "boom")
              :expected-values '(nil)))
         (result (classify-result tc nil "execution failed" 10)))
    (ok (eq :fail (test-result-status result)) "Status is :fail")
    (ok (test-result-error-message result) "Error message is set")))

(deftest classify-result-skip-unsupported-form
  "Test that classify-result returns :skip for unsupported forms"
  (let* ((tc (make-test-case
              :name 'test.skip
              :category "test"
              :form '(format nil "~A" 1)
              :expected-values '("1")))
         (result (classify-result tc nil nil 10)))
    (ok (eq :skip (test-result-status result)) "Status is :skip")
    (ok (test-result-skip-reason result) "Skip reason is set")))

(deftest classify-result-skip-unsupported-category
  "Test that classify-result returns :skip for unsupported categories"
  (let* ((tc (make-test-case
              :name 'test.skip-category
              :category "streams"  ;; streams is in unsupported categories
              :form '(+ 1 2)
              :expected-values '(3)))
         (result (classify-result tc 3 nil 10)))
    (ok (eq :skip (test-result-status result)) "Status is :skip for unsupported category")
    (ok (search "unsupported-category" (test-result-skip-reason result))
        "Skip reason mentions unsupported category")))

;;; ==========================================================================
;;; T009-T011, T045-T046: 021-ansi-test-execution classifier tests
;;; ==========================================================================

(deftest expected-value-verifiable-p-symbol
  "T009: Test that expected-value-verifiable-p rejects non-T/NIL symbols"
  (ok (expected-value-verifiable-p 42) "Fixnum is verifiable")
  (ok (expected-value-verifiable-p t) "T is verifiable")
  (ok (expected-value-verifiable-p nil) "NIL is verifiable")
  (ok (not (expected-value-verifiable-p 'a)) "Symbol A is not verifiable")
  (ok (not (expected-value-verifiable-p 'foo)) "Symbol FOO is not verifiable"))

(deftest expected-value-verifiable-p-cons
  "T010: Test that expected-value-verifiable-p rejects cons cells"
  (ok (not (expected-value-verifiable-p '(1 . 2))) "Cons cell is not verifiable")
  (ok (not (expected-value-verifiable-p '(a b c))) "List is not verifiable")
  (ok (not (expected-value-verifiable-p '((nested)))) "Nested list is not verifiable"))

(deftest compare-values-t-nil-from-wasm
  "T011: Test T/NIL comparison with wasmtime output formats"
  ;; Wasmtime outputs "true" for boolean true
  (ok (eq :match (compare-values t "true")) "T matches 'true' string")
  ;; NIL uses sentinel value
  (ok (eq :match (compare-values nil -2147483648)) "NIL matches sentinel")
  ;; T can also be returned as non-zero fixnum (implementation detail)
  ;; but primary test is the "true" string match
  )

(deftest classify-result-unverifiable-symbol-expected
  "T046: Test that classify-result returns :skip for unverifiable expected values"
  (let* ((tc (make-test-case
              :name 'test.symbol-expected
              :category "cons"
              :form '(car '(a b))
              :expected-values '(a)))  ;; Symbol A is not verifiable
         (result (classify-result tc 42 nil 10)))  ;; Any actual value
    (ok (eq :skip (test-result-status result))
        "Status is :skip for unverifiable expected value")
    (ok (search "unverifiable" (test-result-skip-reason result))
        "Skip reason mentions unverifiable")))

(deftest classify-result-compile-error-format
  "T045: Test that compile-error skip reason follows format"
  ;; This tests the format of compile-error reasons from runner.lisp
  ;; The classifier receives error-info for compilation failures
  (let* ((tc (make-test-case
              :name 'test.compile-error
              :category "cons"
              :form '(+ 1 2)
              :expected-values '(3)))
         ;; Simulate a compile error message
         (result (classify-result tc nil "Unknown function: FOO" 10)))
    (ok (eq :fail (test-result-status result))
        "Status is :fail for error (runner converts to skip with compile-error:)")
    (ok (test-result-error-message result)
        "Error message is preserved")))
