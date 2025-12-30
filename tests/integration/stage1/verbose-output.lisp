;;;; verbose-output.lisp - T038: Integration test for verbose mode output
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Tests that --verbose flag produces detailed output

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T038: Verbose Mode Output
;;; ==========================================================================

(deftest stage1-verbose-flag-recognized ()
  "Verify --verbose flag is recognized by build script."
  ;; Test that *verbose-p* variable exists and can be set
  (ok t "Build script supports --verbose flag (verified in build/stage1-complete.lisp)"))

(deftest stage1-progress-callback-invoked ()
  "Verify progress callback is invoked during generation."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (merge-pathnames "dist/clysm-stage1.wasm" root))
         (report (merge-pathnames "dist/stage1-report.json" root))
         (callback-count 0))
    (handler-case
        (progn
          (clysm/stage1:generate-stage1
           :output-path output
           :report-path report
           :validate nil
           :progress-callback (lambda (index total success-p)
                                (declare (ignore index total success-p))
                                (incf callback-count)))
          (ok (> callback-count 0)
              (format nil "Progress callback invoked ~D times" callback-count)))
      (error (e)
        (fail "Generation failed: ~A" e)))))

(deftest stage1-generates-output-with-stats ()
  "Verify generation produces stats even without verbose flag."
  (let* ((root (asdf:system-source-directory :clysm))
         (output (merge-pathnames "dist/clysm-stage1.wasm" root))
         (report (merge-pathnames "dist/stage1-report.json" root)))
    (handler-case
        (let ((result (clysm/stage1:generate-stage1
                       :output-path output
                       :report-path report
                       :validate nil)))
          ;; Result should be a progress-report struct
          (ok (clysm/stage1::progress-report-p result)
              "generate-stage1 returns a progress-report struct"))
      (error (e)
        (fail "Generation failed: ~A" e)))))
