;;;; stage1-timing-test.lisp - Integration test for progress report timing
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; T034: Verifies progress report generation completes within time limit

(in-package #:clysm/tests/integration/stage1-timing)

;;; ==========================================================================
;;; Progress Report Timing Tests
;;; ==========================================================================

(deftest test-progress-report-timing
  "Progress report generation should complete within 5 seconds."
  (let* ((start-time (get-internal-real-time))
         (modules (clysm/stage1:read-all-modules))
         (forms (apply #'append
                       (mapcar #'clysm/stage1:source-module-forms modules)))
         (compilable (remove-if-not #'clysm/stage1:source-form-compilable-p forms))
         ;; Sample first 100 forms for timing test
         (sample-forms (subseq compilable 0 (min 100 (length compilable))))
         (results nil))
    ;; Compile sample forms using test-form-compilation
    (dolist (form sample-forms)
      (multiple-value-bind (success-p wasm)
          (handler-case
              (clysm/stage1:test-form-compilation
               (clysm/stage1:source-form-sexp form))
            (error (e)
              (declare (ignore e))
              (values nil nil)))
        (push (clysm/stage1:make-compilation-result
               :form form
               :form-id (clysm/stage1:source-form-id form)
               :success-p (eq success-p t)
               :wasm-bytes wasm
               :error-message (unless success-p "Compilation failed"))
              results)))
    ;; Generate report
    (let* ((report (clysm/stage1::generate-progress-report modules (nreverse results)))
           (end-time (get-internal-real-time))
           (elapsed-seconds (/ (- end-time start-time)
                               internal-time-units-per-second)))
      (ok (< elapsed-seconds 5.0)
          (format nil "Report generation took ~,2F seconds (limit: 5s)" elapsed-seconds))
      (ok report "Report was generated"))))

(deftest test-module-reading-timing
  "Reading all modules should complete within 2 seconds."
  (let ((start-time (get-internal-real-time)))
    (let ((modules (clysm/stage1:read-all-modules)))
      (let* ((end-time (get-internal-real-time))
             (elapsed-seconds (/ (- end-time start-time)
                                 internal-time-units-per-second)))
        (ok (< elapsed-seconds 2.0)
            (format nil "Module reading took ~,2F seconds (limit: 2s)" elapsed-seconds))
        (ok (> (length modules) 0) "At least one module was read")))))

(deftest test-single-form-compilation-timing
  "Single form compilation should complete within 100ms."
  (let ((sexp '(+ 1 2))
        (start-time (get-internal-real-time)))
    (multiple-value-bind (success-p wasm)
        (clysm/stage1:test-form-compilation sexp)
      (declare (ignore wasm))
      (let* ((end-time (get-internal-real-time))
             (elapsed-ms (* 1000.0 (/ (- end-time start-time)
                                       internal-time-units-per-second))))
        (ok (< elapsed-ms 100.0)
            (format nil "Single form compilation took ~,2F ms (limit: 100ms)" elapsed-ms))
        (ok (eq success-p t) "Simple form compiled successfully")))))

