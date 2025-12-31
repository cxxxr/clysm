;;;; test-defun-errors-json.lisp - Contract test for defun-errors.json output
;;;;
;;;; Phase 13D M4: DEFUN Blocker Analysis
;;;; Tests: T012 [US1] Contract test for defun-errors.json output format

(in-package #:clysm/tests)

(deftest defun-errors-json-function-exists
    "Test that write-defun-errors-json function exists"
  (testing "write-defun-errors-json is defined"
    (ok (fboundp 'clysm/stage0:write-defun-errors-json))))

(deftest defun-errors-json-output-format
    "Test that defun-errors.json output has correct structure"
  (testing "JSON output contains required fields"
    ;; Clear any existing errors
    (clysm/stage0:clear-error-analysis)
    ;; Add a test error
    (clysm/stage0:collect-defun-error
     "TEST-FUNC" "src/test.lisp"
     (make-condition 'simple-error :format-control "Test error"))
    ;; Write to temp file
    (let ((temp-path (merge-pathnames "test-defun-errors.json"
                                      (uiop:temporary-directory))))
      (unwind-protect
           (progn
             (clysm/stage0:write-defun-errors-json (namestring temp-path))
             ;; Verify file exists and has content
             (ok (probe-file temp-path))
             (let ((content (uiop:read-file-string temp-path)))
               (ok (> (length content) 0))
               ;; Check for required JSON fields
               (ok (search "\"total_entries\"" content))
               (ok (search "\"generated_at\"" content))
               (ok (search "\"entries\"" content))
               (ok (search "\"function_name\"" content))
               (ok (search "\"TEST-FUNC\"" content))))
        ;; Cleanup
        (when (probe-file temp-path)
          (delete-file temp-path))))))
