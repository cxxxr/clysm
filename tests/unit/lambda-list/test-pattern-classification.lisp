;;;; test-pattern-classification.lisp - Unit test for normalize-error-pattern
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T020 [US2] Unit test for normalize-error-pattern

(in-package #:clysm/tests)

(deftest normalize-error-pattern-basic
    "Test basic pattern normalization"
  (testing "removes newlines, keeps first line"
    (let ((normalized (clysm/stage0:normalize-error-pattern
                       "Error on line 1~%Stack trace here")))
      (ok (stringp normalized))
      (ok (not (find #\Newline normalized))))))

(deftest normalize-error-pattern-numbers
    "Test that numbers are normalized"
  (testing "line numbers are replaced"
    (let ((normalized (clysm/stage0:normalize-error-pattern
                       "Error at line 123")))
      ;; Numbers should be replaced with N
      (ok (not (search "123" normalized))))))

(deftest classify-error-pattern-basic
    "Test error pattern classification"
  (testing "returns pattern-id and normalized pattern"
    (multiple-value-bind (pattern-id normalized)
        (clysm/stage0:classify-error-pattern "Unknown function: FOO")
      (ok (stringp pattern-id))
      (ok (= 4 (length pattern-id))) ; "P" + 3 digits
      (ok (char= #\P (char pattern-id 0)))
      (ok (stringp normalized)))))

(deftest classify-error-pattern-consistency
    "Test that same error produces same pattern"
  (testing "consistent classification"
    (multiple-value-bind (id1 norm1)
        (clysm/stage0:classify-error-pattern "Unknown function: FOO")
      (declare (ignore norm1))
      (multiple-value-bind (id2 norm2)
          (clysm/stage0:classify-error-pattern "Unknown function: FOO")
        (declare (ignore norm2))
        (ok (string= id1 id2) "Same error should produce same pattern ID")))))

(deftest compute-pattern-priority-rules
    "Test priority computation rules"
  (testing "HIGH for count > 1000"
    (ok (eq :high (clysm/stage0:compute-pattern-priority 1500 2.0))))
  (testing "HIGH for percentage > 5"
    (ok (eq :high (clysm/stage0:compute-pattern-priority 500 6.0))))
  (testing "MEDIUM for count > 100"
    (ok (eq :medium (clysm/stage0:compute-pattern-priority 150 0.5))))
  (testing "MEDIUM for percentage > 1"
    (ok (eq :medium (clysm/stage0:compute-pattern-priority 50 1.5))))
  (testing "LOW otherwise"
    (ok (eq :low (clysm/stage0:compute-pattern-priority 50 0.5)))))

(deftest aggregate-patterns-basic
    "Test pattern aggregation"
  (testing "aggregates errors by pattern"
    (clysm/stage0:clear-error-analysis)
    ;; Add multiple errors with same pattern
    (dotimes (i 5)
      (clysm/stage0:collect-defun-error
       (format nil "FUNC-~D" i)
       "src/test.lisp"
       (make-condition 'simple-error :format-control "Same error")))
    ;; Aggregate
    (let ((patterns (clysm/stage0:aggregate-patterns)))
      (ok (>= (length patterns) 1))
      ;; The pattern should have count 5
      (let ((first-pattern (first patterns)))
        (ok (>= (clysm/stage0:error-pattern-category-count first-pattern) 5))))))

(deftest get-top-patterns-limit
    "Test that get-top-patterns respects limit"
  (testing "returns at most N patterns"
    (clysm/stage0:clear-error-analysis)
    ;; Add errors with many different patterns
    (dotimes (i 20)
      (clysm/stage0:collect-defun-error
       (format nil "FUNC-~D" i)
       "src/test.lisp"
       (make-condition 'simple-error :format-control (format nil "Error type ~D" i))))
    ;; Get top 5
    (let ((top-5 (clysm/stage0:get-top-patterns 5)))
      (ok (<= (length top-5) 5)))))
