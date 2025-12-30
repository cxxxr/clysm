;;;; report-structure.lisp - T037: Contract test for report JSON structure
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/stage1-report.json has valid structure

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T037: Report JSON Structure
;;; ==========================================================================

(deftest stage1-report-exists ()
  "Verify dist/stage1-report.json exists."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (ok (probe-file path)
        (format nil "Report file exists at ~A" path))))

(deftest stage1-report-has-timestamp ()
  "Verify report contains timestamp field."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "\"timestamp\"" content)
              "Report contains timestamp field"))))))

(deftest stage1-report-has-summary ()
  "Verify report contains summary section."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "\"summary\"" content)
              "Report contains summary section"))))))

(deftest stage1-report-has-coverage-pct ()
  "Verify report summary contains coverage_pct."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "\"coverage_pct\"" content)
              "Report contains coverage_pct field"))))))

(deftest stage1-report-has-modules ()
  "Verify report contains modules list."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "\"modules\"" content)
              "Report contains modules list"))))))

(deftest stage1-report-has-top-blockers ()
  "Verify report contains top_blockers list."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/stage1-report.json" root)))
    (when (probe-file path)
      (with-open-file (s path :direction :input)
        (let ((content (make-string (file-length s))))
          (read-sequence content s)
          (ok (search "\"top_blockers\"" content)
              "Report contains top_blockers list"))))))
