;;; tests/unit/error-report-test.lisp - Unit tests for enhanced error reporting
;;; Feature 038: Stage 0 Capability Extension
;;; TDD: Tests written FIRST, must FAIL before implementation

(in-package #:clysm/tests/unit/error-report)

;;;; Test Suite Definition
(deftest error-report-tests
  (testing "User Story 4: Enhanced Error Reporting"
    ;; T053: record-failure function
    ;; T054: operator-failures hash-table tracking
    ;; T055: generate-failure-report format
    ;; T056: percentage calculation
    ))

;;;; T053: record-failure function
(deftest record-failure-function-test
  (testing "record-failure tracks failures by operator"
    (ok (fboundp 'clysm::record-failure)
        "record-failure should be defined")

    ;; Create a compile-result struct
    (let ((result (clysm::make-compile-result
                   :total 0
                   :compiled 0
                   :failed 0
                   :operator-failures (make-hash-table :test 'eq)
                   :operator-examples (make-hash-table :test 'eq))))
      ;; Record a failure
      (clysm::record-failure
       '(defstruct point x y)
       (make-condition 'error)
       result)
      ;; Check tracking
      (ok (= (clysm::compile-result-failed result) 1)
          "failed count should be 1")
      (ok (= (gethash 'defstruct (clysm::compile-result-operator-failures result) 0) 1)
          "defstruct failure count should be 1"))))

;;;; T054: operator-failures hash-table tracking
(deftest operator-failures-tracking-test
  (testing "operator-failures groups failures by operator type"
    (let ((result (clysm::make-compile-result
                   :total 0
                   :compiled 0
                   :failed 0
                   :operator-failures (make-hash-table :test 'eq)
                   :operator-examples (make-hash-table :test 'eq))))
      ;; Record multiple failures of same type
      (clysm::record-failure '(defstruct a x) (make-condition 'error) result)
      (clysm::record-failure '(defstruct b y) (make-condition 'error) result)
      (clysm::record-failure '(format t "~A" x) (make-condition 'error) result)

      ;; Check grouping
      (let ((failures (clysm::compile-result-operator-failures result)))
        (ok (= (gethash 'defstruct failures 0) 2)
            "defstruct should have 2 failures")
        (ok (= (gethash 'format failures 0) 1)
            "format should have 1 failure")))))

;;;; T055: generate-failure-report format
(deftest generate-failure-report-format-test
  (testing "generate-failure-report outputs operator-grouped format"
    ;; Scenario 1-2 from spec.md:
    ;; Then: error report shows operator name, form preview, error message
    ;; Then: summary groups failures by operator and shows counts
    (ok (fboundp 'clysm::generate-failure-report)
        "generate-failure-report should be defined")

    (let* ((failures (make-hash-table :test 'eq))
           (examples (make-hash-table :test 'eq)))
      (setf (gethash 'defstruct failures) 15)
      (setf (gethash 'format failures) 10)
      (setf (gethash 'defstruct examples)
            (list "(defstruct point x y)" "(defstruct rect ...)"))

      (let* ((result (clysm::make-compile-result
                      :total 100
                      :compiled 75
                      :failed 25
                      :operator-failures failures
                      :operator-examples examples))
             (report (with-output-to-string (s)
                       (clysm::generate-failure-report result s))))
        ;; Should contain operator names
        (ok (search "defstruct" (string-downcase report))
            "Report should contain 'defstruct'")
        (ok (search "format" (string-downcase report))
            "Report should contain 'format'")
        ;; Should show counts
        (ok (search "15" report)
            "Report should show defstruct count (15)")
        (ok (search "10" report)
            "Report should show format count (10)")))))

;;;; T056: percentage calculation
(deftest percentage-calculation-test
  (testing "percentage calculation shows compilation progress"
    ;; Scenario 3 from spec.md:
    ;; Then: report shows percentage progress (e.g., "427/849 forms compiled (50.3%)")
    (let* ((result (clysm::make-compile-result
                    :total 849
                    :compiled 427
                    :failed 422
                    :operator-failures (make-hash-table :test 'eq)
                    :operator-examples (make-hash-table :test 'eq)))
           (report (with-output-to-string (s)
                     (clysm::generate-failure-report result s))))
      ;; Should contain total
      (ok (search "849" report)
          "Report should show total (849)")
      ;; Should contain compiled count
      (ok (search "427" report)
          "Report should show compiled (427)")
      ;; Should contain percentage (approximately 50%)
      (ok (or (search "50%" report)
              (search "50.3%" report)
              (search "50.29%" report))
          "Report should show percentage (~50%)"))))
