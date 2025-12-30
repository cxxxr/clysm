;;;; binary-size.lisp - T013: Contract test for Stage 1 binary size
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/clysm-stage1.wasm is at least 100KB (102,400 bytes)

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T013: Stage 1 Binary Size >= 100KB
;;; ==========================================================================

(defparameter *minimum-stage1-size* 102400
  "Minimum size for Stage 1 binary (100KB).")

(deftest stage1-binary-minimum-size ()
  "Verify dist/clysm-stage1.wasm is at least 100KB."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (let ((size (file-length s)))
            (ok (>= size *minimum-stage1-size*)
                (format nil "Stage 1 binary size: ~D bytes (minimum: ~D)"
                        size *minimum-stage1-size*))))
        (fail "Stage 1 binary not found at ~A" path))))

(deftest stage1-binary-reasonable-size ()
  "Verify Stage 1 binary is not unreasonably large (< 10MB)."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root))
         (max-size (* 10 1024 1024)))  ; 10MB
    (when (probe-file path)
      (with-open-file (s path :element-type '(unsigned-byte 8))
        (let ((size (file-length s)))
          (ok (< size max-size)
              (format nil "Stage 1 binary size ~D is reasonable (< 10MB)" size)))))))
