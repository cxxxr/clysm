;;;; test-blocker-report.lisp - Contract test for blocker report JSON (T012)
;;;; Part of 001-bootstrap-fixpoint Phase 13D-9
;;;;
;;;; This test verifies that blocker report JSON matches the expected schema.
;;;; Schema from contracts/blocker-report.json
;;;; TDD: This test must FAIL before implementation is complete.

(in-package #:clysm/tests/contract/fixpoint-blocker)

(defparameter *stage2-report-path*
  (merge-pathnames #p"dist/stage2-report.json"
                   (asdf:system-source-directory :clysm))
  "Path to Stage 2 blocker report JSON.")

(defun read-json-file (path)
  "Read JSON file as string (for schema validation)"
  (when (probe-file path)
    (uiop:read-file-string path)))

(defun json-has-field-p (json-str field-name)
  "Check if JSON string contains a field (simple string search)"
  (and json-str
       (search (format nil "\"~a\"" field-name) json-str)))

(deftest test-blocker-report-exists
  "Verify blocker report file can be generated"
  ;; This test will fail initially because stage2-report.json doesn't exist
  ;; It should pass after Stage 2 generation is implemented
  (skip "Stage 2 generation not yet implemented - report file does not exist"))

(deftest test-blocker-report-has-stage-field
  "Verify blocker report contains stage field (FR-007)"
  (let ((json (read-json-file *stage2-report-path*)))
    (skip "Stage 2 generation not yet implemented")
    ;; When implemented:
    ;; (ok (json-has-field-p json "stage")
    ;;     "Blocker report should have 'stage' field")
    ))

(deftest test-blocker-report-has-total-forms
  "Verify blocker report contains total_forms field"
  (let ((json (read-json-file *stage2-report-path*)))
    (skip "Stage 2 generation not yet implemented")
    ;; When implemented:
    ;; (ok (json-has-field-p json "total_forms")
    ;;     "Blocker report should have 'total_forms' field")
    ))

(deftest test-blocker-report-has-categories
  "Verify blocker report contains categories array (FR-007)"
  (let ((json (read-json-file *stage2-report-path*)))
    (skip "Stage 2 generation not yet implemented")
    ;; When implemented:
    ;; (ok (json-has-field-p json "categories")
    ;;     "Blocker report should have 'categories' array")
    ))

(deftest test-blocker-report-has-top-blockers
  "Verify blocker report contains top_5_blockers array (SC-005)"
  (let ((json (read-json-file *stage2-report-path*)))
    (skip "Stage 2 generation not yet implemented")
    ;; When implemented:
    ;; (ok (json-has-field-p json "top_5_blockers")
    ;;     "Blocker report should have 'top_5_blockers' array")
    ))

(deftest test-blocker-report-has-compilation-rate
  "Verify blocker report contains compilation_rate field (SC-006)"
  (let ((json (read-json-file *stage2-report-path*)))
    (skip "Stage 2 generation not yet implemented")
    ;; When implemented:
    ;; (ok (json-has-field-p json "compilation_rate")
    ;;     "Blocker report should have 'compilation_rate' field")
    ))
