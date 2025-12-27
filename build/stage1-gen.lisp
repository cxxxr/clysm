;;;; stage1-gen.lisp - Entry point for Stage 1 generation
;;;;
;;;; Part of Feature 039: Stage 1 Compiler Generation
;;;; Usage: sbcl --load build/stage1-gen.lisp
;;;;
;;;; This script generates the Stage 1 compiler binary by:
;;;; 1. Loading the Clysm compiler system
;;;; 2. Reading all compiler source modules
;;;; 3. Compiling each form using Stage 0
;;;; 4. Writing the accumulated Wasm bytes to dist/clysm-stage1.wasm
;;;; 5. Generating a progress report to dist/stage1-report.json

(require :asdf)

;;; Load Clysm system
(format t "~%=== Stage 1 Compiler Generation ===~%")
(format t "Loading Clysm system...~%")

(handler-case
    (asdf:load-system :clysm)
  (error (e)
    (format *error-output* "ERROR: Failed to load Clysm: ~A~%" e)
    (sb-ext:exit :code 1)))

(format t "Clysm system loaded.~%~%")

;;; Parse command-line arguments
(defvar *output-path* nil)
(defvar *report-path* nil)
(defvar *validate-p* t)
(defvar *verbose-p* nil)

(let ((args sb-ext:*posix-argv*))
  (loop for arg in (rest args) ; skip sbcl itself
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

;;; Default paths
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

  ;; Progress callback
  (let ((current-module nil)
        (form-count 0)
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
                (format t "Report: ~A~%" report)))

          (error (e)
            (format *error-output* "~%ERROR: Generation failed: ~A~%" e)
            (sb-ext:exit :code 1)))))))

(format t "~%Done.~%")
(sb-ext:exit :code 0)
