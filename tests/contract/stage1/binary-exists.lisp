;;;; binary-exists.lisp - T012: Contract test for Stage 1 binary existence
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/clysm-stage1.wasm exists after build

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T012: Stage 1 Binary Existence
;;; ==========================================================================

(deftest stage1-binary-exists ()
  "Verify dist/clysm-stage1.wasm exists."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (ok (probe-file path)
        (format nil "Stage 1 binary exists at ~A" path))))

(deftest stage1-binary-not-empty ()
  "Verify dist/clysm-stage1.wasm is not empty."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (when (probe-file path)
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (let ((size (file-length s)))
          (ok (> size 0)
              (format nil "Stage 1 binary has ~D bytes" size)))))))
