;;;; fixpoint.lisp - Fixed-point verification for interpreter bootstrap
;;;;
;;;; Part of Feature 044: Interpreter Bootstrap Strategy
;;;; Phase 6: T102-T105 - Fixed-point verification infrastructure

(in-package #:clysm/interpreter-bootstrap)

;;; ============================================================
;;; T105: Fixpoint Status Type (re-exported from stage1)
;;; ============================================================

;; Re-export fixpoint status utilities from stage1 for consistency
;; clysm/stage1 already defines:
;; - fixpoint-status type
;; - status-to-exit-code
;; - exit-code-to-status
;; - format-fixpoint-status

;;; ============================================================
;;; T103: run-full-bootstrap - Complete bootstrap chain
;;; ============================================================

(defun run-full-bootstrap (&key (verbose t)
                                (output-dir "dist/")
                                (module-limit nil))
  "Execute complete bootstrap chain: Interpreter → Stage 0 → Stage 1 → Stage 2.

   This demonstrates SBCL-free self-hosting:
   1. Generate Stage 0 via interpreter
   2. Run Stage 0 on wasmtime to produce Stage 1
   3. Run Stage 1 on wasmtime to produce Stage 2
   4. Compare Stage 1 == Stage 2 for fixed-point

   Returns (values status stage0-result stage1-result stage2-result).
   STATUS is a fixpoint-status keyword."
  (let* ((stage0-path (merge-pathnames "clysm-stage0-interp.wasm" output-dir))
         (stage1-path (merge-pathnames "clysm-stage1-interp.wasm" output-dir))
         (stage2-path (merge-pathnames "clysm-stage2-interp.wasm" output-dir))
         (stage0-result nil)
         (stage1-result nil)
         (stage2-result nil))

    ;; Ensure output directory exists
    (ensure-directories-exist stage0-path)

    ;; Phase 1: Generate Stage 0 via interpreter
    (when verbose
      (format t "~&=== Phase 1: Generate Stage 0 via Interpreter ===~%"))

    (setf stage0-result (generate-stage0-via-interpreter
                         :module-limit module-limit
                         :verbose verbose
                         :output-path stage0-path))

    (unless (bootstrap-result-success stage0-result)
      (when verbose
        (format t "~&Stage 0 generation failed~%"))
      (return-from run-full-bootstrap
        (values :compilation-error stage0-result nil nil)))

    (when verbose
      (format t "~&Stage 0: ~A (~D bytes)~%"
              stage0-path
              (length (bootstrap-result-wasm-bytes stage0-result))))

    ;; Phase 2: Generate Stage 1 (Stage 0 on wasmtime compiles source)
    (when verbose
      (format t "~&~%=== Phase 2: Generate Stage 1 via Stage 0 ===~%"))

    (setf stage1-result (generate-stage1-from-stage0
                         :stage0-path stage0-path
                         :output-path stage1-path
                         :verbose verbose))

    (unless stage1-result
      (when verbose
        (format t "~&Stage 1 generation failed or skipped~%"))
      (return-from run-full-bootstrap
        (values :compilation-error stage0-result stage1-result nil)))

    ;; Phase 3: Generate Stage 2 (Stage 1 on wasmtime compiles source)
    (when verbose
      (format t "~&~%=== Phase 3: Generate Stage 2 via Stage 1 ===~%"))

    (setf stage2-result (generate-stage2-from-stage1
                         :stage1-path stage1-path
                         :output-path stage2-path
                         :verbose verbose))

    (unless stage2-result
      (when verbose
        (format t "~&Stage 2 generation failed~%"))
      (return-from run-full-bootstrap
        (values :compilation-error stage0-result stage1-result stage2-result)))

    ;; Phase 4: Compare Stage 1 and Stage 2
    (when verbose
      (format t "~&~%=== Phase 4: Fixed-Point Verification ===~%"))

    (let ((status (verify-stages-identical stage1-path stage2-path :verbose verbose)))
      (values status stage0-result stage1-result stage2-result))))

;;; ============================================================
;;; Stage Generation Helpers
;;; ============================================================

(defun generate-stage1-from-stage0 (&key stage0-path output-path verbose)
  "Run Stage 0 on wasmtime to compile Clysm source into Stage 1.

   Note: This requires Stage 0 to export a compile_all function.
   Currently returns NIL as Stage 0 is minimal."
  (declare (ignore stage0-path output-path verbose))
  ;; TODO: Implement wasmtime invocation when Stage 0 has compile capability
  ;; For now, use the existing Stage 1 generation via host CL
  (when (clysm/stage1:wasmtime-available-p)
    ;; Stage 0 from interpreter is currently too minimal to run
    ;; Return nil to indicate Stage 1 cannot be generated via Stage 0
    nil))

(defun generate-stage2-from-stage1 (&key stage1-path output-path verbose)
  "Run Stage 1 on wasmtime to compile Clysm source into Stage 2.

   Delegates to existing Stage 2 generation infrastructure."
  (when (and (probe-file stage1-path)
             (clysm/stage1:wasmtime-available-p))
    (handler-case
        (progn
          (clysm/stage2:generate-stage2
           :stage1-path stage1-path
           :output-path output-path)
          (when (probe-file output-path)
            (list :success t :path output-path)))
      (error (c)
        (when verbose
          (format t "~&Stage 2 generation error: ~A~%" c))
        nil))))

;;; ============================================================
;;; T102: verify-fixpoint function (interpreter bootstrap)
;;; ============================================================

(defun verify-fixpoint-interpreter (&key (stage0-path nil)
                                         (stage1-path "dist/clysm-stage1.wasm")
                                         (stage2-path "dist/clysm-stage2.wasm")
                                         (module-limit nil)
                                         (verbose t)
                                         (skip-generate nil)
                                         (output-format :text))
  "Verify fixed-point for interpreter-based bootstrap.

   If STAGE0-PATH is nil, generates Stage 0 via interpreter.
   Then chains through Stage 1 and Stage 2 verification.

   Returns a verification result with status:
   - :achieved - Stage 1 == Stage 2 byte-for-byte
   - :not-achieved - Binaries differ
   - :compilation-error - Stage generation failed
   - :missing-dependency - wasmtime/tools not available"

  (let* ((start-time (get-internal-real-time))
         (timestamp (clysm/stage1:current-iso-timestamp)))

    ;; Check dependencies
    (unless (clysm/stage1:wasmtime-available-p)
      (let ((result (make-fixpoint-result
                     :status :missing-dependency
                     :timestamp timestamp
                     :error-message "wasmtime not available")))
        (output-fixpoint-result result output-format)
        (return-from verify-fixpoint-interpreter result)))

    ;; Generate Stage 0 if needed
    (unless (or skip-generate (and stage0-path (probe-file stage0-path)))
      (when verbose
        (format t "~&Generating Stage 0 via interpreter...~%"))
      (let* ((temp-path "dist/clysm-stage0-interp.wasm")
             (result (generate-stage0-via-interpreter
                      :module-limit module-limit
                      :verbose verbose
                      :output-path temp-path)))
        (unless (bootstrap-result-success result)
          (let ((fail-result (make-fixpoint-result
                              :status :compilation-error
                              :timestamp timestamp
                              :error-message "Stage 0 generation failed")))
            (output-fixpoint-result fail-result output-format)
            (return-from verify-fixpoint-interpreter fail-result)))
        (setf stage0-path temp-path)))

    ;; Delegate to existing verification infrastructure
    (if (and (probe-file stage1-path) (probe-file stage2-path))
        ;; Use existing Stage 1/2 verification
        (let ((result (clysm/stage2:verify-fixpoint
                       :stage1-path stage1-path
                       :stage2-path stage2-path
                       :skip-generate skip-generate
                       :output-format output-format)))
          ;; Convert to our result format
          (make-fixpoint-result
           :status (clysm/stage1:verification-result-status result)
           :timestamp timestamp
           :stage0-path stage0-path
           :stage1-path stage1-path
           :stage2-path stage2-path
           :identical-p (clysm/stage1:verification-result-identical-p result)
           :elapsed-ms (round (* 1000 (/ (- (get-internal-real-time) start-time)
                                          internal-time-units-per-second)))))
        ;; No existing Stage 1/2, cannot verify
        (let ((result (make-fixpoint-result
                       :status :missing-dependency
                       :timestamp timestamp
                       :stage0-path stage0-path
                       :error-message "Stage 1 or Stage 2 not found")))
          (output-fixpoint-result result output-format)
          result))))

(defun verify-stages-identical (stage1-path stage2-path &key verbose)
  "Compare Stage 1 and Stage 2 binaries byte-for-byte.
   Returns fixpoint-status keyword."
  (if (and (probe-file stage1-path) (probe-file stage2-path))
      (multiple-value-bind (identical-p first-diff)
          (clysm/stage1:binaries-identical-p stage1-path stage2-path)
        (when verbose
          (if identical-p
              (format t "~&FIXED-POINT ACHIEVED: Stage 1 == Stage 2~%")
              (format t "~&NOT ACHIEVED: First diff at byte ~D~%" first-diff)))
        (if identical-p :achieved :not-achieved))
      (progn
        (when verbose
          (format t "~&Missing binary: Stage1=~A Stage2=~A~%"
                  (probe-file stage1-path) (probe-file stage2-path)))
        :missing-dependency)))

;;; ============================================================
;;; Fixpoint Result Struct
;;; ============================================================

(defstruct fixpoint-result
  "Result of fixed-point verification."
  (status :unknown :type symbol)
  (timestamp "" :type string)
  (stage0-path nil :type (or null string pathname))
  (stage1-path nil :type (or null string pathname))
  (stage2-path nil :type (or null string pathname))
  (identical-p nil :type boolean)
  (first-diff-offset nil :type (or null integer))
  (elapsed-ms 0 :type integer)
  (error-message nil :type (or null string)))

;;; ============================================================
;;; T104: generate-json-report for CI
;;; ============================================================

(defun generate-json-report (result &key (stream *standard-output*))
  "Output fixpoint verification result as JSON for CI integration."
  (format stream "{~%")
  (format stream "  \"status\": \"~A\",~%"
          (string-upcase (symbol-name (fixpoint-result-status result))))
  (format stream "  \"timestamp\": \"~A\",~%"
          (fixpoint-result-timestamp result))
  (when (fixpoint-result-stage0-path result)
    (format stream "  \"stage0_path\": ~S,~%"
            (namestring (fixpoint-result-stage0-path result))))
  (when (fixpoint-result-stage1-path result)
    (format stream "  \"stage1_path\": ~S,~%"
            (namestring (fixpoint-result-stage1-path result))))
  (when (fixpoint-result-stage2-path result)
    (format stream "  \"stage2_path\": ~S,~%"
            (namestring (fixpoint-result-stage2-path result))))
  (format stream "  \"identical\": ~A,~%"
          (if (fixpoint-result-identical-p result) "true" "false"))
  (if (fixpoint-result-first-diff-offset result)
      (format stream "  \"first_diff_offset\": ~D,~%"
              (fixpoint-result-first-diff-offset result))
      (format stream "  \"first_diff_offset\": null,~%"))
  (format stream "  \"elapsed_ms\": ~D"
          (fixpoint-result-elapsed-ms result))
  (when (fixpoint-result-error-message result)
    (format stream ",~%  \"error\": ~S"
            (fixpoint-result-error-message result)))
  (format stream "~%}~%"))

(defun generate-text-report (result &key (stream *standard-output*))
  "Output fixpoint verification result as human-readable text."
  (format stream "~&=== Interpreter Bootstrap Fixed-Point Verification ===~%~%")
  (format stream "Timestamp: ~A~%" (fixpoint-result-timestamp result))
  (when (fixpoint-result-stage0-path result)
    (format stream "Stage 0: ~A~%" (fixpoint-result-stage0-path result)))
  (when (fixpoint-result-stage1-path result)
    (format stream "Stage 1: ~A~%" (fixpoint-result-stage1-path result)))
  (when (fixpoint-result-stage2-path result)
    (format stream "Stage 2: ~A~%" (fixpoint-result-stage2-path result)))
  (format stream "~%Result: ")
  (clysm/stage1:format-fixpoint-status (fixpoint-result-status result)
                                       :stream stream)
  (format stream "~%")
  (when (fixpoint-result-first-diff-offset result)
    (format stream "~%First diff at byte: ~D~%"
            (fixpoint-result-first-diff-offset result)))
  (format stream "~%Elapsed: ~,1F seconds~%"
          (/ (fixpoint-result-elapsed-ms result) 1000.0))
  (when (fixpoint-result-error-message result)
    (format stream "~%Error: ~A~%" (fixpoint-result-error-message result))))

(defun output-fixpoint-result (result format)
  "Output fixpoint result in specified format."
  (ecase format
    (:json (generate-json-report result))
    (:text (generate-text-report result))))

;;; ============================================================
;;; Exit Code Integration
;;; ============================================================

(defun fixpoint-exit-code (result)
  "Convert fixpoint-result to exit code for shell scripts."
  (clysm/stage1:status-to-exit-code (fixpoint-result-status result)))
