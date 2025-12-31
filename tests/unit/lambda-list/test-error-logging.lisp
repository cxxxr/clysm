;;;; test-error-logging.lisp - Unit test for make-error-log-entry
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T011 [US1] Unit test for make-error-log-entry

(in-package #:clysm/tests)

(deftest make-error-log-entry-basic
    "Test basic error log entry creation"
  (testing "all fields are set correctly"
    (let ((entry (clysm/stage0:make-error-log-entry
                  :function-name "MY-FUNCTION"
                  :module-path "src/clysm/test.lisp"
                  :error-type :compile-error
                  :error-message "Unknown variable: X"
                  :pattern-id "P123"
                  :lambda-list "(x y &optional z)"
                  :failing-subform "(+ x y)")))
      (ok (string= "MY-FUNCTION" (clysm/stage0:error-log-entry-function-name entry)))
      (ok (string= "src/clysm/test.lisp" (clysm/stage0:error-log-entry-module-path entry)))
      (ok (eq :compile-error (clysm/stage0:error-log-entry-error-type entry)))
      (ok (string= "Unknown variable: X" (clysm/stage0:error-log-entry-error-message entry)))
      (ok (string= "P123" (clysm/stage0:error-log-entry-pattern-id entry)))
      (ok (string= "(x y &optional z)" (clysm/stage0:error-log-entry-lambda-list entry)))
      (ok (string= "(+ x y)" (clysm/stage0:error-log-entry-failing-subform entry)))
      ;; Timestamp should be auto-generated
      (ok (stringp (clysm/stage0:error-log-entry-timestamp entry))))))

(deftest make-error-log-entry-defaults
    "Test default values for optional fields"
  (testing "optional fields default to nil"
    (let ((entry (clysm/stage0:make-error-log-entry
                  :function-name "TEST"
                  :module-path "test.lisp"
                  :error-type :compile-error
                  :error-message "Error"
                  :pattern-id "P001")))
      (ok (null (clysm/stage0:error-log-entry-lambda-list entry)))
      (ok (null (clysm/stage0:error-log-entry-failing-subform entry))))))

(deftest collect-defun-error-wrapper
    "Test the collect-defun-error convenience function"
  (testing "wraps condition into error entry"
    (clysm/stage0:clear-error-analysis)
    (let ((condition (make-condition 'simple-error
                                     :format-control "Test error: ~A"
                                     :format-arguments '("foo"))))
      (let ((entry (clysm/stage0:collect-defun-error
                    "TEST-FN"
                    "src/test.lisp"
                    condition
                    :lambda-list "(x)")))
        (ok entry)
        (ok (string= "TEST-FN" (clysm/stage0:error-log-entry-function-name entry)))
        (ok (search "Test error" (clysm/stage0:error-log-entry-error-message entry)))
        ;; Should also be added to *defun-errors*
        (ok (member entry clysm/stage0:*defun-errors*))))))

(deftest get-iso-timestamp-format
    "Test ISO 8601 timestamp generation"
  (testing "timestamp has correct format"
    (let ((ts (clysm/stage0:get-iso-timestamp)))
      (ok (stringp ts))
      ;; Should be like "2025-12-31T12:00:00Z"
      (ok (= 20 (length ts)))
      (ok (char= #\- (char ts 4)))
      (ok (char= #\- (char ts 7)))
      (ok (char= #\T (char ts 10)))
      (ok (char= #\: (char ts 13)))
      (ok (char= #\: (char ts 16)))
      (ok (char= #\Z (char ts 19))))))

(deftest extract-failing-subform-basic
    "Test extraction of failing subform from DEFUN"
  (testing "extracts first body form"
    (let ((form '(defun test-fn (x) (+ x 1))))
      (let ((subform (clysm/stage0:extract-failing-subform form nil)))
        (ok subform)
        (ok (search "+ X" (string-upcase subform)))))))
