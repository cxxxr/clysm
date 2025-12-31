;;;; test-error-log-entry.lisp - Contract test for error-log-entry JSON schema
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T010 [US1] Contract test for error-log-entry JSON schema

(in-package #:clysm/tests)

(deftest error-log-entry-struct-exists
    "Test that error-log-entry struct is defined"
  (testing "error-log-entry struct accessors exist"
    (ok (fboundp 'clysm/stage0:make-error-log-entry))
    (ok (fboundp 'clysm/stage0:error-log-entry-function-name))
    (ok (fboundp 'clysm/stage0:error-log-entry-module-path))
    (ok (fboundp 'clysm/stage0:error-log-entry-error-type))
    (ok (fboundp 'clysm/stage0:error-log-entry-error-message))
    (ok (fboundp 'clysm/stage0:error-log-entry-pattern-id))))

(deftest error-log-entry-creation
    "Test creating an error log entry"
  (testing "creating entry with required fields"
    (let ((entry (clysm/stage0:make-error-log-entry
                  :function-name "TEST-FUNC"
                  :module-path "src/test.lisp"
                  :error-type :compile-error
                  :error-message "Test error"
                  :pattern-id "P001")))
      (ok entry)
      (ok (string= "TEST-FUNC" (clysm/stage0:error-log-entry-function-name entry)))
      (ok (string= "src/test.lisp" (clysm/stage0:error-log-entry-module-path entry)))
      (ok (eq :compile-error (clysm/stage0:error-log-entry-error-type entry)))
      (ok (string= "Test error" (clysm/stage0:error-log-entry-error-message entry)))
      (ok (string= "P001" (clysm/stage0:error-log-entry-pattern-id entry))))))

(deftest error-log-entry-json-format
    "Test JSON conversion of error log entry"
  (testing "error-log-entry-to-json produces correct structure"
    (let* ((entry (clysm/stage0:make-error-log-entry
                   :function-name "MY-FUNC"
                   :module-path "src/clysm/test.lisp"
                   :error-type :compile-error
                   :error-message "Unknown function"
                   :pattern-id "P042"))
           (json (clysm/stage0:error-log-entry-to-json entry)))
      (ok (listp json))
      (ok (string= "MY-FUNC" (getf json :function_name)))
      (ok (string= "src/clysm/test.lisp" (getf json :module_path)))
      (ok (string= "compile_error" (getf json :error_type)))
      (ok (string= "Unknown function" (getf json :error_message)))
      (ok (string= "P042" (getf json :pattern_id))))))
