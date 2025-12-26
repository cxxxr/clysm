;;;; data-model-test.lisp - Unit tests for ANSI test harness data structures
;;;;
;;;; TDD Red Phase: These tests should FAIL initially until implementation is complete.

(in-package #:clysm/tests/unit/ansi-test)

;;; ==========================================================================
;;; T008: test-case struct tests
;;; ==========================================================================

(deftest test-case-creation
  "Test that test-case struct can be created with required fields"
  (let ((tc (make-test-case
             :name 'cons.1
             :category "cons"
             :source-file #p"/path/to/cons.lsp"
             :form '(cons 1 2)
             :expected-values '((1 . 2)))))
    (ok (test-case-p tc) "make-test-case returns a test-case struct")
    (ok (eq 'cons.1 (test-case-name tc)) "name accessor works")
    (ok (string= "cons" (test-case-category tc)) "category accessor works")
    (ok (equal '(cons 1 2) (test-case-form tc)) "form accessor works")
    (ok (equal '((1 . 2)) (test-case-expected-values tc)) "expected-values accessor works")))

(deftest test-case-multiple-expected-values
  "Test that test-case can have multiple expected values"
  (let ((tc (make-test-case
             :name 'values.1
             :category "data-and-control-flow"
             :form '(values 1 2 3)
             :expected-values '(1 2 3))))
    (ok (= 3 (length (test-case-expected-values tc)))
        "Multiple expected values are preserved")))

;;; ==========================================================================
;;; T009: test-result struct tests
;;; ==========================================================================

(deftest test-result-pass
  "Test that test-result can represent a passing test"
  (let* ((tc (make-test-case
              :name 'pass.1
              :category "test"
              :form '(+ 1 2)
              :expected-values '(3)))
         (tr (make-test-result
              :test-case tc
              :status :pass
              :actual-values '(3)
              :execution-time-ms 42)))
    (ok (test-result-p tr) "make-test-result returns a test-result struct")
    (ok (eq :pass (test-result-status tr)) "status is :pass")
    (ok (equal '(3) (test-result-actual-values tr)) "actual-values accessor works")
    (ok (= 42 (test-result-execution-time-ms tr)) "execution-time-ms accessor works")
    (ok (null (test-result-skip-reason tr)) "skip-reason is nil for passing test")))

(deftest test-result-fail
  "Test that test-result can represent a failing test"
  (let* ((tc (make-test-case
              :name 'fail.1
              :category "test"
              :form '(+ 1 2)
              :expected-values '(4)))
         (tr (make-test-result
              :test-case tc
              :status :fail
              :actual-values '(3)
              :error-message "Expected 4, got 3")))
    (ok (eq :fail (test-result-status tr)) "status is :fail")
    (ok (test-result-error-message tr) "error-message is set for failing test")))

(deftest test-result-skip
  "Test that test-result can represent a skipped test"
  (let* ((tc (make-test-case
              :name 'skip.1
              :category "test"
              :form '(format nil "~A" 1)
              :expected-values '("1")))
         (tr (make-test-result
              :test-case tc
              :status :skip
              :skip-reason "unsupported-form: FORMAT")))
    (ok (eq :skip (test-result-status tr)) "status is :skip")
    (ok (string= "unsupported-form: FORMAT" (test-result-skip-reason tr))
        "skip-reason is set correctly")))

;;; ==========================================================================
;;; T010: category-result struct tests
;;; ==========================================================================

(deftest category-result-creation
  "Test that category-result struct can be created"
  (let ((cr (make-category-result
             :name "cons"
             :total-count 100
             :pass-count 40
             :fail-count 10
             :skip-count 50
             :duration-ms 5000)))
    (ok (category-result-p cr) "make-category-result returns a category-result struct")
    (ok (string= "cons" (category-result-name cr)) "name accessor works")
    (ok (= 100 (category-result-total-count cr)) "total-count accessor works")
    (ok (= 40 (category-result-pass-count cr)) "pass-count accessor works")
    (ok (= 10 (category-result-fail-count cr)) "fail-count accessor works")
    (ok (= 50 (category-result-skip-count cr)) "skip-count accessor works")))

(deftest category-pass-rate-calculation
  "Test that category-pass-rate computes correct percentage"
  (let ((cr (make-category-result
             :name "cons"
             :total-count 100
             :pass-count 40)))
    (ok (= 0.4 (category-pass-rate cr)) "Pass rate is 40%"))
  ;; Edge case: zero total
  (let ((cr (make-category-result
             :name "empty"
             :total-count 0
             :pass-count 0)))
    (ok (= 0.0 (category-pass-rate cr)) "Pass rate is 0.0 for empty category")))

(deftest report-summary-creation
  "Test that report-summary struct can be created"
  (let ((rs (make-report-summary
             :total-count 1000
             :pass-count 400
             :fail-count 100
             :skip-count 500
             :duration-ms 60000)))
    (ok (report-summary-p rs) "make-report-summary returns a report-summary struct")
    (ok (= 1000 (report-summary-total-count rs)) "total-count accessor works")))

(deftest coverage-report-creation
  "Test that coverage-report struct can be created"
  (let* ((summary (make-report-summary
                   :total-count 1000
                   :pass-count 400))
         (cr (make-coverage-report
              :timestamp "2025-12-25T10:00:00Z"
              :branch "main"
              :commit "abc123"
              :categories nil
              :summary summary)))
    (ok (coverage-report-p cr) "make-coverage-report returns a coverage-report struct")
    (ok (string= "2025-12-25T10:00:00Z" (coverage-report-timestamp cr)) "timestamp accessor works")
    (ok (string= "main" (coverage-report-branch cr)) "branch accessor works")
    (ok (eq summary (coverage-report-summary cr)) "summary accessor works")))

;;; ==========================================================================
;;; T011: skip-registry struct tests
;;; ==========================================================================

(deftest skip-registry-creation
  "Test that skip-registry struct can be created"
  (let ((sr (make-skip-registry
             :unsupported-forms '(format print)
             :unsupported-categories '("files" "streams")
             :timeout-seconds 30)))
    (ok (skip-registry-p sr) "make-skip-registry returns a skip-registry struct")
    (ok (member 'format (skip-registry-unsupported-forms sr))
        "unsupported-forms contains format")
    (ok (member "files" (skip-registry-unsupported-categories sr) :test #'string=)
        "unsupported-categories contains files")
    (ok (= 30 (skip-registry-timeout-seconds sr)) "timeout-seconds accessor works")))

(deftest default-skip-registry-exists
  "Test that *default-skip-registry* is defined and populated"
  (ok (boundp '*default-skip-registry*) "*default-skip-registry* is bound")
  (ok (skip-registry-p *default-skip-registry*) "*default-skip-registry* is a skip-registry")
  (ok (member 'format (skip-registry-unsupported-forms *default-skip-registry*))
      "FORMAT is in default unsupported forms")
  (ok (member 'defgeneric (skip-registry-unsupported-forms *default-skip-registry*))
      "DEFGENERIC is in default unsupported forms"))
