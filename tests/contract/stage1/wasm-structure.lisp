;;;; wasm-structure.lisp - T030: Contract test for Wasm module structure
;;;;
;;;; Phase 13D-7: Stage 1 Compiler Generation
;;;; Verifies that dist/clysm-stage1.wasm has required sections

(in-package #:clysm/tests)

;;; ==========================================================================
;;; T030: Wasm Module Structure
;;; ==========================================================================

(deftest stage1-has-type-section ()
  "Verify Stage 1 Wasm has a type section."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (file-position s 8)  ; Skip magic + version
          (let ((found-type nil))
            (loop while (< (file-position s) (file-length s))
                  do (let ((section-id (read-byte s nil nil)))
                       (when (null section-id) (return))
                       (when (= section-id 1)  ; Type section ID
                         (setf found-type t)
                         (return))))
            (ok found-type "Found type section (ID 1)")))
        (fail "Stage 1 binary not found at ~A" path))))

(deftest stage1-has-function-section ()
  "Verify Stage 1 Wasm has a function section."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (file-position s 8)
          (let ((found-func nil))
            (loop while (< (file-position s) (file-length s))
                  do (let ((section-id (read-byte s nil nil)))
                       (when (null section-id) (return))
                       (when (= section-id 3)  ; Function section ID
                         (setf found-func t)
                         (return))
                       ;; Skip section content
                       (let ((size (clysm/stage1::read-leb128-unsigned s)))
                         (file-position s (+ (file-position s) size)))))
            (ok found-func "Found function section (ID 3)")))
        (fail "Stage 1 binary not found at ~A" path))))

(deftest stage1-has-code-section ()
  "Verify Stage 1 Wasm has a code section."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (file-position s 8)
          (let ((found-code nil))
            (loop while (< (file-position s) (file-length s))
                  do (let ((section-id (read-byte s nil nil)))
                       (when (null section-id) (return))
                       (when (= section-id 10)  ; Code section ID
                         (setf found-code t)
                         (return))
                       ;; Skip section content
                       (let ((size (clysm/stage1::read-leb128-unsigned s)))
                         (file-position s (+ (file-position s) size)))))
            (ok found-code "Found code section (ID 10)")))
        (fail "Stage 1 binary not found at ~A" path))))

(deftest stage1-has-export-section ()
  "Verify Stage 1 Wasm has an export section."
  (let* ((root (asdf:system-source-directory :clysm))
         (path (merge-pathnames "dist/clysm-stage1.wasm" root)))
    (if (probe-file path)
        (with-open-file (s path :element-type '(unsigned-byte 8))
          (file-position s 8)
          (let ((found-export nil))
            (loop while (< (file-position s) (file-length s))
                  do (let ((section-id (read-byte s nil nil)))
                       (when (null section-id) (return))
                       (when (= section-id 7)  ; Export section ID
                         (setf found-export t)
                         (return))
                       ;; Skip section content
                       (let ((size (clysm/stage1::read-leb128-unsigned s)))
                         (file-position s (+ (file-position s) size)))))
            (ok found-export "Found export section (ID 7)")))
        (fail "Stage 1 binary not found at ~A" path))))
