;;;; test-stage1-report.lisp - Contract test for stage1-report.json error_patterns section
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T021 [US2] Contract test for error_patterns section in stage1-report.json

(in-package #:clysm/tests)

(deftest error-patterns-section-generator-exists
    "Test that generate-error-patterns-section function exists"
  (testing "generate-error-patterns-section is defined"
    (ok (fboundp 'clysm/stage0:generate-error-patterns-section))))

(deftest error-patterns-section-format
    "Test that error_patterns section has correct JSON format"
  (testing "JSON section contains required fields"
    ;; Clear and add test errors
    (clysm/stage0:clear-error-analysis)
    (dotimes (i 100)
      (clysm/stage0:collect-defun-error
       (format nil "FUNC-~D" i)
       "src/test.lisp"
       (make-condition 'simple-error :format-control "Unknown function: FOO")))
    ;; Generate the section
    (let ((section (clysm/stage0:generate-error-patterns-section)))
      (ok (stringp section))
      (ok (search "\"error_patterns\"" section))
      (ok (search "\"pattern_id\"" section))
      (ok (search "\"pattern\"" section))
      (ok (search "\"count\"" section))
      (ok (search "\"percentage\"" section))
      (ok (search "\"priority\"" section))
      (ok (search "\"top_patterns_coverage_pct\"" section)))))

(deftest top-patterns-coverage
    "Test that top patterns cover at least 80% of errors"
  (testing "compute-top-patterns-coverage returns reasonable value"
    ;; Clear and add diverse errors
    (clysm/stage0:clear-error-analysis)
    ;; Add 100 errors with the same pattern (should be 100% coverage)
    (dotimes (i 100)
      (clysm/stage0:collect-defun-error
       (format nil "FUNC-~D" i)
       "src/test.lisp"
       (make-condition 'simple-error :format-control "Same error message")))
    ;; Should have 100% coverage with a single pattern
    (let ((coverage (clysm/stage0:compute-top-patterns-coverage 10)))
      (ok (>= coverage 80.0) "Top 10 patterns should cover >= 80% of errors"))))

(deftest aggregate-patterns-sorting
    "Test that aggregate-patterns returns sorted results"
  (testing "patterns are sorted by count descending"
    (clysm/stage0:clear-error-analysis)
    ;; Add errors with different patterns
    (dotimes (i 50)
      (clysm/stage0:collect-defun-error
       "FUNC-A"
       "src/test.lisp"
       (make-condition 'simple-error :format-control "Error type A")))
    (dotimes (i 100)
      (clysm/stage0:collect-defun-error
       "FUNC-B"
       "src/test.lisp"
       (make-condition 'simple-error :format-control "Error type B")))
    ;; Get sorted patterns
    (let ((patterns (clysm/stage0:aggregate-patterns)))
      (ok (>= (length patterns) 1))
      ;; First pattern should have highest count
      (when (> (length patterns) 1)
        (ok (>= (clysm/stage0:error-pattern-category-count (first patterns))
                (clysm/stage0:error-pattern-category-count (second patterns))))))))
