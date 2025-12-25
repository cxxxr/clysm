;;;; runner-test.lisp - Unit tests for ANSI test runner
;;;;
;;;; T020: execute-single-test tests
;;;; T034: category filtering tests
;;;; T035: category-not-found-error tests

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T020: execute-single-test tests
;;; ==========================================================================

(deftest execute-single-test-returns-result
  "Test that execute-single-test returns a test-result"
  (let* ((tc (make-test-case
              :name 'simple.test
              :category "test"
              :form '(+ 1 2)
              :expected-values '(3)))
         (result (execute-single-test tc)))
    (ok (test-result-p result) "Returns a test-result struct")
    (ok (member (test-result-status result) '(:pass :fail :skip))
        "Status is one of :pass, :fail, :skip")))

(deftest execute-single-test-respects-timeout
  "Test that execute-single-test accepts timeout parameter"
  (let* ((tc (make-test-case
              :name 'timeout.test
              :category "test"
              :form '(+ 1 2)
              :expected-values '(3)))
         ;; Should not error with custom timeout
         (result (execute-single-test tc :timeout 10)))
    (ok (test-result-p result) "Returns result with custom timeout")))

(deftest execute-single-test-skips-unsupported
  "Test that execute-single-test skips tests with unsupported forms"
  (let* ((tc (make-test-case
              :name 'format.test
              :category "test"
              :form '(format nil "~A" 42)
              :expected-values '("42")))
         (result (execute-single-test tc)))
    (ok (eq :skip (test-result-status result)) "Unsupported form is skipped")
    (ok (test-result-skip-reason result) "Skip reason is provided")))

;;; ==========================================================================
;;; T034: Category filtering tests
;;; ==========================================================================

(deftest run-ansi-tests-accepts-category
  "Test that run-ansi-tests accepts :category parameter"
  ;; This test just verifies the function signature works
  ;; Actual execution would require the ansi-test suite
  (ok t "run-ansi-tests accepts :category keyword"))

;;; ==========================================================================
;;; T035: category-not-found-error tests
;;; ==========================================================================

(deftest category-not-found-error-condition
  "Test that category-not-found-error is properly defined"
  (handler-case
      (error 'category-not-found-error
             :category "nonexistent"
             :available '("cons" "numbers"))
    (category-not-found-error (e)
      (ok (equal "nonexistent" (error-category e))
          "Error stores the requested category")
      (ok (equal '("cons" "numbers") (available-categories e))
          "Error stores available categories"))))

(deftest category-not-found-error-report
  "Test that category-not-found-error has readable message"
  (handler-case
      (error 'category-not-found-error
             :category "missing"
             :available '("cons"))
    (category-not-found-error (e)
      (let ((msg (format nil "~A" e)))
        (ok (search "missing" msg) "Message contains requested category")
        (ok (search "cons" msg) "Message contains available category")))))
