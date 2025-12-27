;;;; generator.lisp - Stage 2 binary generation via Stage 1
;;;;
;;;; Part of Feature 040: Fixed-Point Verification
;;;; Compiles Clysm source using Stage 1 Wasm compiler to produce Stage 2

(in-package #:clysm/stage2)

;;; ==========================================================================
;;; Configuration
;;; ==========================================================================

(defvar *stage1-path* "dist/clysm-stage1.wasm"
  "Path to Stage 1 Wasm binary.")

(defvar *stage2-path* "dist/clysm-stage2.wasm"
  "Output path for Stage 2 binary.")

(defvar *source-dir* "src/clysm/"
  "Source directory containing Clysm compiler modules.")

;;; ==========================================================================
;;; Main Entry Point
;;; ==========================================================================

(defun generate-stage2 (&key (stage1-path *stage1-path*)
                              (output-path *stage2-path*)
                              (source-dir *source-dir*))
  "Generate Stage 2 binary by compiling Clysm source using Stage 1.
Returns (values success-p stage2-info error-message).
STAGE1-PATH is the path to the Stage 1 Wasm binary.
OUTPUT-PATH is where to write the Stage 2 binary.
SOURCE-DIR is the directory containing source modules."
  (let ((start-time (get-internal-real-time)))
    ;; Check dependencies
    (unless (wasmtime-available-p)
      (return-from generate-stage2
        (values nil nil "wasmtime not available")))

    (unless (probe-file stage1-path)
      (return-from generate-stage2
        (values nil nil (format nil "Stage 1 not found: ~A" stage1-path))))

    ;; Load and validate Stage 1
    (handler-case
        (let* ((modules (get-module-paths))
               (total-modules (length modules))
               (compiled-count 0)
               (failed-count 0)
               (wasm-bytes nil)
               (errors nil))

          (format t "~&=== Stage 2 Generation ===~%")
          (format t "Stage 1: ~A~%" stage1-path)
          (format t "Output:  ~A~%" output-path)
          (format t "Modules: ~D~%" total-modules)
          (format t "~%")

          ;; Compile each module via Stage 1
          (loop for module-path in modules
                for index from 1
                do (format t "[~D/~D] ~A~%" index total-modules module-path)
                   (handler-case
                       (let ((result (compile-module-via-stage1
                                      module-path
                                      :stage1-path stage1-path)))
                         (if (getf result :success)
                             (progn
                               (incf compiled-count)
                               (when (getf result :wasm-bytes)
                                 (push (getf result :wasm-bytes) wasm-bytes)))
                             (progn
                               (incf failed-count)
                               (push (list module-path (getf result :error))
                                     errors))))
                     (error (e)
                       ;; FR-010: Continue on failure
                       (incf failed-count)
                       (push (list module-path (format nil "~A" e)) errors)
                       (format t "  ERROR: ~A~%" e))))

          ;; Calculate elapsed time
          (let* ((end-time (get-internal-real-time))
                 (elapsed-ms (round (* 1000 (/ (- end-time start-time)
                                               internal-time-units-per-second))))
                 (compilation-rate (if (> total-modules 0)
                                       (float (/ compiled-count total-modules))
                                       0.0)))

            ;; Write Stage 2 binary
            (when wasm-bytes
              (write-stage2-binary (nreverse wasm-bytes) output-path))

            ;; Build stage2-info
            (let ((stage2-info (make-binary-info
                                :path output-path
                                :size-bytes (if (probe-file output-path)
                                                (with-open-file (s output-path)
                                                  (file-length s))
                                                0)
                                :valid-p (and (probe-file output-path)
                                              (validate-binary output-path)))))

              (format t "~%=== Summary ===~%")
              (format t "Compiled: ~D/~D (~,1F%)~%"
                      compiled-count total-modules (* 100 compilation-rate))
              (format t "Time: ~,1Fs~%" (/ elapsed-ms 1000.0))
              (format t "Output: ~D bytes~%"
                      (binary-info-size-bytes stage2-info))

              (values (and (= failed-count 0)
                           (probe-file output-path))
                      stage2-info
                      (when errors
                        (format nil "~D modules failed" failed-count))))))

      (error (e)
        (values nil nil (format nil "~A" e))))))

;;; ==========================================================================
;;; Module Compilation
;;; ==========================================================================

(defun compile-module-via-stage1 (module-path &key (stage1-path *stage1-path*))
  "Compile a single module using Stage 1 compiler.
Returns plist with :success, :wasm-bytes, :error, :forms-compiled."
  (let* ((forms (read-source-forms module-path))
         (compilable-forms (remove-if-not #'compilable-form-p forms))
         (total-forms (length compilable-forms))
         (compiled-forms 0)
         (all-wasm-bytes nil))

    (loop for form in compilable-forms
          do (handler-case
                 (let ((result (clysm/stage1:run-form form :stage-path stage1-path)))
                   (when (compilation-result-success-p result)
                     (incf compiled-forms)
                     (when (compilation-result-wasm-bytes result)
                       (push (compilation-result-wasm-bytes result) all-wasm-bytes))))
               (error (e)
                 (format t "    Form error: ~A~%" e))))

    (list :success (> compiled-forms 0)
          :wasm-bytes (when all-wasm-bytes
                        (concatenate-wasm-bytes (nreverse all-wasm-bytes)))
          :forms-compiled compiled-forms
          :forms-total total-forms)))

(defun run-stage1-compiler (form-string &key (stage1-path *stage1-path*))
  "Execute Stage 1 compiler on a form string.
Returns the compiled Wasm bytes or nil on failure."
  (let ((result (clysm/stage1:invoke-wasmtime-compile
                 stage1-path
                 form-string
                 (clysm/stage1:compute-host-shim-path))))
    (when (getf result :success)
      (getf result :wasm-bytes))))

;;; ==========================================================================
;;; Binary Output
;;; ==========================================================================

(defun write-stage2-binary (wasm-chunks output-path)
  "Write Stage 2 binary from compiled Wasm chunks.
WASM-CHUNKS is a list of byte vectors to concatenate."
  (let ((combined (concatenate-wasm-bytes wasm-chunks)))
    (with-open-file (out output-path
                         :direction :output
                         :element-type '(unsigned-byte 8)
                         :if-exists :supersede)
      (write-sequence combined out))
    (format t "Wrote ~D bytes to ~A~%" (length combined) output-path)
    combined))

(defun concatenate-wasm-bytes (chunks)
  "Concatenate list of byte vectors into single vector."
  (if (null chunks)
      (make-wasm-empty-module)
      (let* ((total-size (reduce #'+ chunks :key #'length))
             (result (make-array total-size :element-type '(unsigned-byte 8)))
             (offset 0))
        (dolist (chunk chunks)
          (replace result chunk :start1 offset)
          (incf offset (length chunk)))
        result)))

(defun make-wasm-empty-module ()
  "Create minimal valid Wasm module."
  (make-array 17 :element-type '(unsigned-byte 8)
              :initial-contents '(#x00 #x61 #x73 #x6d   ; magic
                                  #x01 #x00 #x00 #x00   ; version
                                  #x01 #x01 #x00        ; type section
                                  #x03 #x01 #x00        ; function section
                                  #x0a #x01 #x00)))     ; code section

(defun validate-binary (path)
  "Validate Wasm binary using wasm-tools."
  (handler-case
      (let ((result (uiop:run-program
                     (list "wasm-tools" "validate" (namestring path))
                     :output :string
                     :error-output :string
                     :ignore-error-status t)))
        (declare (ignore result))
        t)
    (error () nil)))
