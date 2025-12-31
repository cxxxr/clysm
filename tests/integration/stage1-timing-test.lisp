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

;;; ==========================================================================
;;; T055: FFI Analysis Overhead Test
;;; Feature: 001-ffi-import-architecture
;;; ==========================================================================

(deftest test-ffi-analysis-overhead
  "T055: FFI analysis should add <15% compilation overhead."
  (testing "Measure FFI analysis overhead vs total compilation"
    ;; Test forms with varying complexity
    (let ((test-forms '((+ 1 2)
                        (* 7 6)
                        (let ((x 1) (y 2)) (+ x y))
                        (if t 1 2)
                        (progn (+ 1 2) (* 3 4))))
          (iterations 10)
          (total-analysis-time 0)
          (total-compile-time 0))
      ;; Run multiple iterations for accuracy
      (dotimes (i iterations)
        (dolist (form test-forms)
          ;; Measure FFI analysis time
          (let ((start (get-internal-real-time)))
            (clysm/compiler/analyzer/ffi-usage:analyze-ffi-usage form)
            (incf total-analysis-time (- (get-internal-real-time) start)))
          ;; Measure total compilation time (includes analysis)
          (let ((start (get-internal-real-time)))
            (clysm/compiler:compile-to-wasm form)
            (incf total-compile-time (- (get-internal-real-time) start)))))
      ;; Calculate overhead percentage
      (let ((overhead-percent (if (> total-compile-time 0)
                                  (* 100.0 (/ total-analysis-time total-compile-time))
                                  0)))
        (ok (< overhead-percent 15.0)
            (format nil "FFI analysis overhead: ~,2F%% (limit: <15%%)" overhead-percent))))))

(deftest test-ffi-analysis-scales-linearly
  "T055: FFI analysis should scale roughly linearly with code size."
  (testing "Nested forms don't cause exponential slowdown"
    (let ((small-form '(+ 1 2))
          (medium-form '(let ((x 1) (y 2) (z 3)) (+ x (+ y z))))
          (large-form '(let ((a 1) (b 2) (c 3) (d 4))
                         (if (> a 0)
                             (progn (+ a b) (+ c d))
                             (progn (- a b) (- c d))))))
      ;; Measure each form 100 times for statistical significance
      (flet ((measure-analysis (form iterations)
               (let ((start (get-internal-real-time)))
                 (dotimes (i iterations)
                   (clysm/compiler/analyzer/ffi-usage:analyze-ffi-usage form))
                 (/ (- (get-internal-real-time) start)
                    internal-time-units-per-second
                    iterations))))
        (let ((small-time (measure-analysis small-form 100))
              (medium-time (measure-analysis medium-form 100))
              (large-time (measure-analysis large-form 100)))
          ;; Large should not be more than 10x small (allows for linear + constant)
          (ok (< large-time (* 10 (max small-time 0.0001)))
              (format nil "Large form (~,6Fs) vs small (~,6Fs): ratio ~,2F"
                      large-time small-time
                      (if (> small-time 0.000001)
                          (/ large-time small-time)
                          0.0))))))))

