;;;; stage1-complete.lisp - FR-010: Entry script for Stage 1 generation
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Usage: sbcl --load build/stage1-complete.lisp
;;;;
;;;; Generates a valid Stage 1 Wasm binary containing the compiled
;;;; Clysm compiler. Output: dist/clysm-stage1.wasm (>= 100KB)

(require :asdf)

;;; ==========================================================================
;;; Load Clysm System
;;; ==========================================================================

(format t "~%=== Stage 1 Compiler Generation ===~%")
(format t "Loading Clysm system...~%")

(handler-case
    (asdf:load-system :clysm)
  (error (e)
    (format *error-output* "ERROR: Failed to load Clysm: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Clysm system loaded.~%~%")

;;; ==========================================================================
;;; Parse Command-Line Arguments
;;; ==========================================================================

(defvar *output-path* nil)
(defvar *report-path* nil)
(defvar *validate-p* t)
(defvar *verbose-p* nil)

(let ((args sb-ext:*posix-argv*))
  (loop for arg in (rest args)
        do (cond
             ((string= arg "--no-validate")
              (setf *validate-p* nil))
             ((string= arg "--verbose")
              (setf *verbose-p* t))
             ((and (> (length arg) 9)
                   (string= (subseq arg 0 9) "--output="))
              (setf *output-path* (subseq arg 9)))
             ((and (> (length arg) 9)
                   (string= (subseq arg 0 9) "--report="))
              (setf *report-path* (subseq arg 9))))))

;;; ==========================================================================
;;; Generate Stage 1 Binary
;;; ==========================================================================

(let* ((root (asdf:system-source-directory :clysm))
       (output (or *output-path*
                   (namestring (merge-pathnames "dist/clysm-stage1.wasm" root))))
       (report (or *report-path*
                   (namestring (merge-pathnames "dist/stage1-report.json" root)))))

  (format t "Configuration:~%")
  (format t "  Output: ~A~%" output)
  (format t "  Report: ~A~%" report)
  (format t "  Validate: ~A~%" *validate-p*)
  (format t "~%")

  ;; Ensure dist/ directory exists
  (ensure-directories-exist output)

  ;; Track statistics
  (let ((form-count 0)
        (success-count 0)
        (fail-count 0))

    (flet ((progress-callback (index total success-p)
             (incf form-count)
             (if success-p
                 (incf success-count)
                 (incf fail-count))
             (when *verbose-p*
               (format t "  [~D/~D] ~A~%"
                       index total
                       (if success-p "OK" "FAIL")))))

      (format t "Generating Stage 1 binary...~%")
      (let ((start-time (get-internal-real-time)))

        (handler-case
            (let ((report-obj (clysm/stage1:generate-stage1
                               :output-path output
                               :report-path report
                               :validate *validate-p*
                               :progress-callback #'progress-callback)))
              (declare (ignore report-obj))

              (let* ((end-time (get-internal-real-time))
                     (elapsed (/ (- end-time start-time)
                                 internal-time-units-per-second)))

                (format t "~%=== Generation Complete ===~%")
                (format t "Forms processed: ~D~%" form-count)
                (format t "Compiled: ~D~%" success-count)
                (format t "Failed: ~D~%" fail-count)
                (when (> form-count 0)
                  (format t "Coverage: ~,1F%~%"
                          (* 100.0 (/ success-count form-count))))
                (format t "Time: ~,2F seconds~%" elapsed)
                (format t "Output: ~A~%" output)
                (format t "Report: ~A~%" report)

                ;; Verify output file size
                (when (probe-file output)
                  (with-open-file (s output :element-type '(unsigned-byte 8))
                    (let ((size (file-length s)))
                      (format t "Size: ~D bytes~%" size)
                      (when (< size 102400)
                        (format *error-output*
                                "~%WARNING: Output size ~D bytes is below 100KB target~%"
                                size)))))))

          (error (e)
            (format *error-output* "~%ERROR: Generation failed: ~A~%" e)
            (sb-ext:exit :code 1)))))))

(format t "~%Done.~%")
(sb-ext:exit :code 0)
