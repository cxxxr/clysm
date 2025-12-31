;;;; test-error-pattern.lisp - Contract test for error-pattern-category
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T019 [US2] Contract test for error-pattern-category JSON schema

(in-package #:clysm/tests)

(deftest error-pattern-category-struct-exists
    "Test that error-pattern-category struct is defined"
  (testing "error-pattern-category struct accessors exist"
    (ok (fboundp 'clysm/stage0:make-error-pattern-category))
    (ok (fboundp 'clysm/stage0:error-pattern-category-pattern-id))
    (ok (fboundp 'clysm/stage0:error-pattern-category-pattern))
    (ok (fboundp 'clysm/stage0:error-pattern-category-count))
    (ok (fboundp 'clysm/stage0:error-pattern-category-percentage))
    (ok (fboundp 'clysm/stage0:error-pattern-category-priority))))

(deftest error-pattern-category-creation
    "Test creating an error pattern category"
  (testing "creating pattern with required fields"
    (let ((pattern (clysm/stage0:make-error-pattern-category
                    :pattern-id "P001"
                    :pattern "Unknown function: <NAME>"
                    :count 100
                    :percentage 5.5)))
      (ok pattern)
      (ok (string= "P001" (clysm/stage0:error-pattern-category-pattern-id pattern)))
      (ok (string= "Unknown function: <NAME>" (clysm/stage0:error-pattern-category-pattern pattern)))
      (ok (= 100 (clysm/stage0:error-pattern-category-count pattern)))
      (ok (= 5.5 (clysm/stage0:error-pattern-category-percentage pattern))))))

(deftest error-pattern-priority-computation
    "Test automatic priority computation"
  (testing "HIGH priority for count > 1000"
    (let ((pattern (clysm/stage0:make-error-pattern-category
                    :pattern-id "P001"
                    :pattern "Test"
                    :count 1500
                    :percentage 2.0)))
      (ok (eq :high (clysm/stage0:error-pattern-category-priority pattern)))))
  (testing "HIGH priority for percentage > 5%"
    (let ((pattern (clysm/stage0:make-error-pattern-category
                    :pattern-id "P002"
                    :pattern "Test"
                    :count 500
                    :percentage 6.0)))
      (ok (eq :high (clysm/stage0:error-pattern-category-priority pattern)))))
  (testing "MEDIUM priority for count > 100"
    (let ((pattern (clysm/stage0:make-error-pattern-category
                    :pattern-id "P003"
                    :pattern "Test"
                    :count 200
                    :percentage 0.5)))
      (ok (eq :medium (clysm/stage0:error-pattern-category-priority pattern)))))
  (testing "LOW priority for small counts"
    (let ((pattern (clysm/stage0:make-error-pattern-category
                    :pattern-id "P004"
                    :pattern "Test"
                    :count 50
                    :percentage 0.3)))
      (ok (eq :low (clysm/stage0:error-pattern-category-priority pattern))))))

(deftest error-pattern-json-format
    "Test JSON conversion of error pattern category"
  (testing "error-pattern-to-json produces correct structure"
    (let* ((pattern (clysm/stage0:make-error-pattern-category
                     :pattern-id "P001"
                     :pattern "Unknown function: <NAME>"
                     :count 1500
                     :percentage 8.5
                     :examples (list (list :function "FOO" :module "src/test.lisp"))))
           (json (clysm/stage0:error-pattern-to-json pattern)))
      (ok (listp json))
      (ok (string= "P001" (getf json :pattern_id)))
      (ok (string= "Unknown function: <NAME>" (getf json :pattern)))
      (ok (= 1500 (getf json :count)))
      (ok (= 8.5 (getf json :percentage)))
      (ok (string= "high" (getf json :priority))))))
