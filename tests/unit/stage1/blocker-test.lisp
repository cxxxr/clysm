;;;; blocker-test.lisp - Unit tests for Stage 1 blocker analysis
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Tests for blocker categorization and impact calculation

(in-package #:clysm/tests/unit/stage1-blocker)

;;; ==========================================================================
;;; BlockerInfo Tests
;;; ==========================================================================

(deftest test-blocker-info-creation
  "BlockerInfo struct should be creatable with required fields."
  (let ((blocker (clysm/stage1::make-blocker-info
                  :operator 'loop
                  :affected-forms 50
                  :impact-pct 10.0
                  :priority "HIGH"
                  :recommendation "Implement LOOP"
                  :examples nil)))
    (ok (clysm/stage1::blocker-info-p blocker)
        "make-blocker-info creates blocker-info struct")
    (ok (eq (clysm/stage1::blocker-info-operator blocker) 'loop)
        "operator is LOOP")
    (ok (= (clysm/stage1::blocker-info-affected-forms blocker) 50)
        "affected-forms is 50")))

;;; ==========================================================================
;;; Blocker Categorization Tests
;;; ==========================================================================

(deftest test-categorize-by-form-type
  "Blockers should be categorized by form type."
  (let* ((failure1 (clysm/stage1::make-failure-group
                    :operator 'loop :count 10 :example "(loop ...)"))
         (failure2 (clysm/stage1::make-failure-group
                    :operator 'format :count 5 :example "(format ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 85 :failed 15 :skipped 0
                 :failures (list failure1 failure2)))
         (summary (clysm/stage1::generate-summary (list stats))))
    (ok (> (length (clysm/stage1::summary-top-blockers summary)) 0)
        "summary has blockers")
    (let ((blockers (clysm/stage1::summary-top-blockers summary)))
      (ok (find 'loop blockers :key #'clysm/stage1::blocker-info-operator)
          "LOOP is in blockers")
      (ok (find 'format blockers :key #'clysm/stage1::blocker-info-operator)
          "FORMAT is in blockers"))))

;;; ==========================================================================
;;; Impact Calculation Tests
;;; ==========================================================================

(deftest test-impact-calculation-basic
  "Impact should be calculated as percentage of total forms."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'loop :count 20 :example "(loop ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 80 :failed 20 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats))))
    ;; Coverage should be 80%
    (ok (< (abs (- (clysm/stage1::summary-coverage-pct summary) 80.0)) 0.01)
        "coverage is 80%")))

(deftest test-impact-ranking
  "Blockers should be ranked by impact (count)."
  (let* ((failure1 (clysm/stage1::make-failure-group
                    :operator 'loop :count 50 :example "(loop ...)"))
         (failure2 (clysm/stage1::make-failure-group
                    :operator 'format :count 30 :example "(format ...)"))
         (failure3 (clysm/stage1::make-failure-group
                    :operator 'defstruct :count 10 :example "(defstruct ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 10 :failed 90 :skipped 0
                 :failures (list failure2 failure3 failure1))) ; unsorted
         (summary (clysm/stage1::generate-summary (list stats))))
    (let ((blockers (clysm/stage1::summary-top-blockers summary)))
      (ok (>= (length blockers) 2) "has at least 2 blockers")
      ;; First blocker should be LOOP (highest count)
      (ok (eq (clysm/stage1::blocker-info-operator (first blockers)) 'loop)
          "LOOP is first (highest impact)"))))

;;; ==========================================================================
;;; Priority Assignment Tests
;;; ==========================================================================

(deftest test-priority-high-for-many-failures
  "HIGH priority should be assigned for >10 failures."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'loop :count 50 :example "(loop ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 50 :failed 50 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats))))
    (let ((blocker (first (clysm/stage1::summary-top-blockers summary))))
      (ok (string= (clysm/stage1::blocker-info-priority blocker) "HIGH")
          "priority is HIGH for 50 failures"))))

(deftest test-priority-low-for-few-failures
  "LOW priority should be assigned for <=5 failures."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'obscure-op :count 3 :example "(obscure-op ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 97 :failed 3 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats))))
    (let ((blocker (first (clysm/stage1::summary-top-blockers summary))))
      (ok (string= (clysm/stage1::blocker-info-priority blocker) "LOW")
          "priority is LOW for 3 failures"))))

;;; ==========================================================================
;;; Recommendation Generation Tests
;;; ==========================================================================

(deftest test-recommendation-includes-operator
  "Recommendations should mention the operator."
  (let* ((failure (clysm/stage1::make-failure-group
                   :operator 'loop :count 10 :example "(loop ...)"))
         (stats (clysm/stage1::make-module-stats
                 :path "test.lisp" :total-forms 100
                 :compiled 90 :failed 10 :skipped 0
                 :failures (list failure)))
         (summary (clysm/stage1::generate-summary (list stats))))
    (let ((blocker (first (clysm/stage1::summary-top-blockers summary))))
      (ok (search "LOOP" (clysm/stage1::blocker-info-recommendation blocker))
          "recommendation mentions LOOP"))))

