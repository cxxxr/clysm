;;;; wasm-header.lisp - T031: Contract test for Wasm magic number and version
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/clysm-stage1.wasm has correct Wasm header

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T031: Wasm Magic Number and Version
;;; ==========================================================================

(deftest stage1-wasm-magic ()
  "Verify dist/clysm-stage1.wasm has correct Wasm magic number."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (let ((magic (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence magic s)
            (ok (equalp magic #(#x00 #x61 #x73 #x6D))
                (format nil "Valid Wasm magic: ~{#x~2,'0X ~}" (coerce magic 'list)))))
        (fail "Stage 1 binary not found at ~A" path))))

(deftest stage1-wasm-version ()
  "Verify dist/clysm-stage1.wasm has Wasm version 1."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (file-position s 4)  ; Skip magic
          (let ((version (make-array 4 :element-type '(unsigned-byte 8))))
            (read-sequence version s)
            (ok (equalp version #(#x01 #x00 #x00 #x00))
                (format nil "Valid Wasm version: ~{#x~2,'0X ~}" (coerce version 'list)))))
        (fail "Stage 1 binary not found at ~A" path))))
