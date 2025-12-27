;;;; verifier.lisp - Fixed-point verification implementation
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Implements verify-fixpoint function for self-hosting verification

(in-package #:clysm/stage2)

;;; ==========================================================================
;;; Main Verification Entry Point
;;; ==========================================================================

(defun verify-fixpoint (&key (stage1-path "dist/clysm-stage1.wasm")
                              (stage2-path "dist/clysm-stage2.wasm")
                              (source-dir "src/clysm/")
                              (skip-generate nil)
                              (output-format :text))
  "Perform fixed-point verification: Stage 1 vs Stage 2.
Returns a verification-result struct.

If SKIP-GENERATE is nil, generates Stage 2 first.
If SKIP-GENERATE is T, compares existing Stage 1 and Stage 2.
OUTPUT-FORMAT can be :text or :json."
  (let ((start-time (get-internal-real-time))
        (timestamp (clysm/stage1:current-iso-timestamp)))

    ;; T033: Check dependencies
    (let ((dep-result (check-dependencies stage1-path)))
      (when dep-result
        (return-from verify-fixpoint dep-result)))

    ;; T034: Validate Stage 1 binary (FR-008)
    (let ((validation-result (validate-stage1-binary stage1-path)))
      (when validation-result
        (return-from verify-fixpoint validation-result)))

    ;; T035: Generate Stage 2 if not skipping
    (let ((stage2-gen-start (get-internal-real-time))
          (stage2-info nil)
          (modules-compiled 0)
          (modules-total 0)
          (compilation-rate 0.0))
      (unless skip-generate
        (multiple-value-bind (success-p info error-msg)
            (generate-stage2 :stage1-path stage1-path
                             :output-path stage2-path
                             :source-dir source-dir)
          (declare (ignore success-p))
          (setf stage2-info info)
          (when info
            ;; Extract compilation stats from generation
            (setf modules-compiled 0  ; Will be filled from generation
                  modules-total (length (clysm/stage1:get-module-paths))))))

      (let ((stage2-gen-time-ms
             (round (* 1000 (/ (- (get-internal-real-time) stage2-gen-start)
                               internal-time-units-per-second)))))

        ;; Check Stage 2 exists
        (unless (probe-file stage2-path)
          (return-from verify-fixpoint
            (make-verification-result
             :status :compilation-error
             :timestamp timestamp
             :stage1-info (clysm/stage1:extract-binary-info stage1-path)
             :stage2-info nil
             :identical-p nil
             :compilation-rate compilation-rate
             :modules-compiled modules-compiled
             :modules-total modules-total
             :stage2-gen-time-ms stage2-gen-time-ms
             :error-message "Stage 2 generation failed - no output file")))

        ;; T035: Compare binaries
        (let ((comparison-start (get-internal-real-time)))
          (multiple-value-bind (identical-p first-diff-offset)
              (clysm/stage1:binaries-identical-p stage1-path stage2-path)
            (let* ((comparison-time-ms
                    (round (* 1000 (/ (- (get-internal-real-time) comparison-start)
                                      internal-time-units-per-second))))
                   (byte-diff (unless identical-p
                                (clysm/stage1:compute-byte-diff stage1-path stage2-path)))
                   (stage1-info (clysm/stage1:extract-binary-info stage1-path))
                   (final-stage2-info (or stage2-info
                                          (clysm/stage1:extract-binary-info stage2-path)))
                   (status (if identical-p :achieved :not-achieved))
                   (result (make-verification-result
                            :status status
                            :timestamp timestamp
                            :stage1-info stage1-info
                            :stage2-info final-stage2-info
                            :identical-p identical-p
                            :first-diff-offset first-diff-offset
                            :diff-byte-count (if byte-diff
                                                 (clysm/stage1:byte-diff-info-total-diff-bytes byte-diff)
                                                 0)
                            :compilation-rate compilation-rate
                            :modules-compiled modules-compiled
                            :modules-total modules-total
                            :stage2-gen-time-ms stage2-gen-time-ms
                            :comparison-time-ms comparison-time-ms)))

              ;; T036: Output result
              (format-verification-result result :format output-format)
              result)))))))

;;; ==========================================================================
;;; Dependency Checking (T033)
;;; ==========================================================================

(defun check-dependencies (stage1-path)
  "Check if all dependencies are available.
Returns nil if all OK, or a verification-result with error status."
  (let ((timestamp (clysm/stage1:current-iso-timestamp)))
    ;; Check wasmtime
    (unless (clysm/stage1:wasmtime-available-p)
      (return-from check-dependencies
        (make-verification-result
         :status :missing-dependency
         :timestamp timestamp
         :error-message "wasmtime not available. Install: curl https://wasmtime.dev/install.sh -sSf | bash")))
    ;; Check Stage 1 exists
    (unless (probe-file stage1-path)
      (return-from check-dependencies
        (make-verification-result
         :status :missing-dependency
         :timestamp timestamp
         :error-message (format nil "Stage 1 not found: ~A. Run: sbcl --load build/stage1-gen.lisp"
                                stage1-path))))
    nil))

;;; ==========================================================================
;;; Stage 1 Validation (T034, FR-008)
;;; ==========================================================================

(defun validate-stage1-binary (stage1-path)
  "Validate Stage 1 binary using wasm-tools.
Returns nil if valid, or a verification-result with error status."
  (let ((timestamp (clysm/stage1:current-iso-timestamp)))
    (unless (clysm/stage1:validate-binary stage1-path)
      (return-from validate-stage1-binary
        (make-verification-result
         :status :missing-dependency
         :timestamp timestamp
         :stage1-info (clysm/stage1:extract-binary-info stage1-path)
         :error-message (format nil "Stage 1 binary invalid: ~A" stage1-path))))
    nil))

;;; ==========================================================================
;;; Result Formatting (T036)
;;; ==========================================================================

(defun format-verification-result (result &key (format :text)
                                               (stream *standard-output*))
  "Format verification result for output."
  (ecase format
    (:text (format-result-text result stream))
    (:json (format-result-json result stream))))

(defun format-result-text (result stream)
  "Format verification result as human-readable text."
  (let ((status (verification-result-status result))
        (stage1 (verification-result-stage1-info result))
        (stage2 (verification-result-stage2-info result)))
    (format stream "~&=== Fixed-Point Verification ===~%~%")
    (when stage1
      (format stream "Stage 1: ~A (~D bytes, ~A)~%"
              (clysm/stage1:binary-info-path stage1)
              (clysm/stage1:binary-info-size-bytes stage1)
              (if (clysm/stage1:binary-info-valid-p stage1) "valid" "invalid")))
    (when stage2
      (format stream "Stage 2: ~A (~D bytes, ~A)~%"
              (clysm/stage1:binary-info-path stage2)
              (clysm/stage1:binary-info-size-bytes stage2)
              (if (clysm/stage1:binary-info-valid-p stage2) "valid" "invalid")))
    (when (> (verification-result-modules-total result) 0)
      (format stream "~%Compilation: ~D/~D modules (~,1F%)~%"
              (verification-result-modules-compiled result)
              (verification-result-modules-total result)
              (* 100 (verification-result-compilation-rate result))))
    (format stream "Time: Stage 2 generation ~,1Fs, comparison ~,3Fs~%"
            (/ (verification-result-stage2-gen-time-ms result) 1000.0)
            (/ (verification-result-comparison-time-ms result) 1000.0))
    (format stream "~%Result: ")
    (clysm/stage1:format-fixpoint-status status :stream stream)
    (format stream "~%")
    (when (and (eq status :not-achieved)
               (verification-result-first-diff-offset result))
      (format stream "~%First difference at byte offset: 0x~X~%"
              (verification-result-first-diff-offset result))
      (format stream "Total differing bytes: ~D~%"
              (verification-result-diff-byte-count result)))
    (when (verification-result-error-message result)
      (format stream "~%Error: ~A~%"
              (verification-result-error-message result)))))

(defun format-result-json (result stream)
  "Format verification result as JSON."
  (let ((status (verification-result-status result))
        (stage1 (verification-result-stage1-info result))
        (stage2 (verification-result-stage2-info result)))
    (format stream "{~%")
    (format stream "  \"status\": \"~A\",~%"
            (string-upcase (symbol-name status)))
    (format stream "  \"timestamp\": \"~A\",~%"
            (verification-result-timestamp result))
    (when stage1
      (format stream "  \"stage1\": {~%")
      (format stream "    \"path\": ~S,~%"
              (clysm/stage1:binary-info-path stage1))
      (format stream "    \"size_bytes\": ~D,~%"
              (clysm/stage1:binary-info-size-bytes stage1))
      (format stream "    \"valid\": ~A~%"
              (if (clysm/stage1:binary-info-valid-p stage1) "true" "false"))
      (format stream "  },~%"))
    (when stage2
      (format stream "  \"stage2\": {~%")
      (format stream "    \"path\": ~S,~%"
              (clysm/stage1:binary-info-path stage2))
      (format stream "    \"size_bytes\": ~D,~%"
              (clysm/stage1:binary-info-size-bytes stage2))
      (format stream "    \"valid\": ~A,~%"
              (if (clysm/stage1:binary-info-valid-p stage2) "true" "false"))
      (format stream "    \"compilation_rate\": ~,3F,~%"
              (verification-result-compilation-rate result))
      (format stream "    \"modules_compiled\": ~D,~%"
              (verification-result-modules-compiled result))
      (format stream "    \"modules_total\": ~D~%"
              (verification-result-modules-total result))
      (format stream "  },~%"))
    (format stream "  \"comparison\": {~%")
    (format stream "    \"identical\": ~A,~%"
            (if (verification-result-identical-p result) "true" "false"))
    (if (verification-result-first-diff-offset result)
        (format stream "    \"first_diff_offset\": ~D,~%"
                (verification-result-first-diff-offset result))
        (format stream "    \"first_diff_offset\": null,~%"))
    (format stream "    \"diff_bytes\": ~D~%"
            (verification-result-diff-byte-count result))
    (format stream "  },~%")
    (format stream "  \"timing\": {~%")
    (format stream "    \"stage2_generation_ms\": ~D,~%"
            (verification-result-stage2-gen-time-ms result))
    (format stream "    \"comparison_ms\": ~D,~%"
            (verification-result-comparison-time-ms result))
    (format stream "    \"total_ms\": ~D~%"
            (+ (verification-result-stage2-gen-time-ms result)
               (verification-result-comparison-time-ms result)))
    (format stream "  }")
    (when (verification-result-error-message result)
      (format stream ",~%  \"error\": ~S"
              (verification-result-error-message result)))
    (format stream "~%}~%")))
