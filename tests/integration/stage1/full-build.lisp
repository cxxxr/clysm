;;;; full-build.lisp - T014: Integration test for full Stage 1 build workflow
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Tests the complete build workflow via generate-stage1

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T014: Full Build Workflow Integration Test
;;; ==========================================================================

(deftest stage1-full-build-completes ()
  "Verify generate-stage1 completes without fatal errors."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (merge-pathnames "dist/clysm-stage1.wasm" root))
         (report (merge-pathnames "dist/stage1-report.json" root)))
    ;; Run generation
    (handler-case
        (progn
          (clysm/stage1:generate-stage1
           :output-path output
           :report-path report
           :validate nil)  ; Skip validation for speed
          (ok t "generate-stage1 completed without error"))
      (error (e)
        (fail "generate-stage1 failed: ~A" e)))))

(deftest stage1-generates-report ()
  "Verify generate-stage1 creates a report file."
  (let* ((root (asdf:system-source-directory :clysm))
         (report (merge-pathnames "dist/stage1-report.json" root)))
    (ok (probe-file report)
        (format nil "Report file exists at ~A" report))))

(deftest stage1-report-has-coverage ()
  "Verify report contains coverage percentage."
  (let* ((root (asdf:system-source-directory :clysm))
         (report (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file report)
      (with-open-file (s report :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "coverage_pct" content)
              "Report contains coverage_pct field"))))))
