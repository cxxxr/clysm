;;;; output.lisp - Binary output for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Writes Stage 1 binary via FFI

(in-package #:clysm/stage0)

;;; ============================================================
;;; Binary Output
;;; ============================================================

(defvar *output-path* "dist/clysm-stage1.wasm"
  "Default output path for Stage 1 binary")

(defun write-stage1-binary (wasm-bytes &key (path *output-path*))
  "Write Stage 1 Wasm binary to file.
   Uses FFI in Wasm context."
  (report-progress :output-started :path path)
  (handler-case
      (with-open-file (stream path
                              :direction :output
                              :element-type '(unsigned-byte 8)
                              :if-exists :supersede)
        (write-sequence wasm-bytes stream)
        (report-progress :output-complete
                         :path path
                         :size (length wasm-bytes))
        t)
    (error (e)
      (report-progress :output-error
                       :path path
                       :error (format nil "~A" e))
      nil)))

;;; ============================================================
;;; Compilation Finalization
;;; ============================================================

(defun finalize-compilation (functions exports)
  "Finalize compilation and write output.
   Returns stage0-result struct."
  (let* ((wasm-bytes (emit-stage0-module functions exports)))
    ;; Validate before writing
    (if (validate-wasm-bytes wasm-bytes)
        (progn
          (write-stage1-binary wasm-bytes)
          (make-stage0-result
           :success-p t
           :wasm-bytes wasm-bytes
           :form-count (length functions)))
        (make-stage0-result
         :success-p nil
         :error-message "Generated Wasm failed validation"))))

(defun validate-wasm-bytes (bytes)
  "Validate Wasm bytes are well-formed.
   Returns T if valid."
  ;; Basic validation: check magic number
  (and (>= (length bytes) 8)
       (= #x00 (aref bytes 0))
       (= #x61 (aref bytes 1))
       (= #x73 (aref bytes 2))
       (= #x6D (aref bytes 3))
       (= #x01 (aref bytes 4))
       (= #x00 (aref bytes 5))
       (= #x00 (aref bytes 6))
       (= #x00 (aref bytes 7))))
