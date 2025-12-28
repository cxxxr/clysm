;;;; stage1-bootstrap-gen.lisp - Generate Stage 1 from bootstrap source
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T047: Update Stage 1 generation to use bootstrap source
;;;;
;;;; This generates Stage 1 by compiling bootstrap forms into a single Wasm module.
;;;; The approach:
;;;; 1. Compile all bootstrap forms using the host compiler
;;;; 2. Merge the compiled functions into a single module
;;;; 3. Write to dist/clysm-stage1.wasm
;;;;
;;;; Usage: sbcl --load build/stage1-bootstrap-gen.lisp

(require :asdf)

(format t "~%=== Stage 1 Bootstrap Generation ===~%")
(format t "Loading Clysm system...~%")
(asdf:load-system :clysm)
(format t "System loaded.~%~%")

;;; ============================================================
;;; Configuration
;;; ============================================================

(defparameter *output-path*
  (merge-pathnames "dist/clysm-stage1.wasm"
                   (asdf:system-source-directory :clysm)))

;;; ============================================================
;;; Compile All Bootstrap Forms into Single Module
;;; ============================================================

(defun generate-stage1-bootstrap ()
  "Generate Stage 1 by compiling all bootstrap forms.
   Returns (values success-p byte-count)."
  (let ((forms (clysm/stage0:get-bootstrap-forms))
        (compiled 0)
        (failed 0)
        (total-bytes 0))

    (format t "Compiling ~D bootstrap forms...~%~%" (length forms))

    ;; Compile a single progn with all defuns
    ;; This produces one Wasm module containing all functions
    (let ((all-defuns (remove-if-not
                       (lambda (form)
                         (and (consp form) (eq (first form) 'defun)))
                       forms)))

      (format t "Found ~D defun forms~%" (length all-defuns))

      ;; Wrap all defuns in a progn
      (let ((combined-form `(progn ,@all-defuns)))
        (format t "Compiling combined form...~%")

        (handler-case
            (let ((bytes (clysm:compile-to-wasm combined-form)))
              (setf total-bytes (length bytes))
              (setf compiled (length all-defuns))
              (format t "Success! Produced ~D bytes~%~%" total-bytes)

              ;; Write output
              (ensure-directories-exist *output-path*)
              (with-open-file (out *output-path*
                                   :direction :output
                                   :element-type '(unsigned-byte 8)
                                   :if-exists :supersede)
                (write-sequence bytes out))
              (format t "Output: ~A~%" *output-path*))

          (error (e)
            (format t "ERROR: ~A~%" e)
            (incf failed)))))

    ;; Summary
    (format t "~%=== Summary ===~%")
    (format t "Compiled defuns: ~D~%" compiled)
    (format t "Failed: ~D~%" failed)
    (format t "Total bytes: ~D~%" total-bytes)
    (format t "Target: >= 1024 bytes~%")
    (format t "Status: ~A~%~%" (if (>= total-bytes 1024) "PASS" "FAIL"))

    (values (and (zerop failed) (>= total-bytes 1024))
            total-bytes)))

;;; ============================================================
;;; Validation
;;; ============================================================

(defun validate-stage1 ()
  "Validate Stage 1 with wasm-tools."
  (format t "Validating Stage 1...~%")
  (let* ((result (uiop:run-program
                  (list "wasm-tools" "validate" (namestring *output-path*))
                  :output :string
                  :error-output :string
                  :ignore-error-status t))
         (status (nth-value 2 (uiop:run-program
                               (list "wasm-tools" "validate" (namestring *output-path*))
                               :ignore-error-status t))))
    (if (zerop status)
        (format t "Validation: PASSED~%")
        (format t "Validation: FAILED~%~A~%" result))
    (zerop status)))

;;; ============================================================
;;; Main Entry Point
;;; ============================================================

(multiple-value-bind (success bytes) (generate-stage1-bootstrap)
  (if success
      (progn
        (validate-stage1)
        (format t "~%Stage 1 generated successfully!~%")
        (format t "Size: ~D bytes (~,1F KB)~%" bytes (/ bytes 1024.0))
        (sb-ext:exit :code 0))
      (progn
        (format t "~%Stage 1 generation failed!~%")
        (sb-ext:exit :code 1))))
