;;;; ffi.lisp - FFI declarations for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements US5: FFI Filesystem Access
;;;;
;;;; T017: Implement FFI import declarations (fs.open, fs.read-all, fs.write-all, fs.close)
;;;; T018: Implement read-file-contents wrapper
;;;; T019: Implement file-error condition signaling
;;;;
;;;; This file generates the import section for Stage 0 Wasm binary,
;;;; declaring FFI functions for filesystem access.

(in-package #:clysm/stage0)

;;; ============================================================
;;; FFI Import Names (exported for reference)
;;; ============================================================

(defvar *fs-open* "fs.open"
  "FFI function name for opening files")
(defvar *fs-read-all* "fs.read-all"
  "FFI function name for reading entire file")
(defvar *fs-write-all* "fs.write-all"
  "FFI function name for writing to file")
(defvar *fs-close* "fs.close"
  "FFI function name for closing file handle")

;;; ============================================================
;;; FFI Import Type Signatures
;;; ============================================================

;; fs.open: (path: string, direction: string, if_exists: string, if_dne: string) -> anyref
;; Returns file handle (anyref) or signals error
(defparameter *fs-open-type*
  (list +wasm-func+
        4 +wasm-anyref+ +wasm-anyref+ +wasm-anyref+ +wasm-anyref+  ; 4 params
        1 +wasm-anyref+))  ; 1 result

;; fs.read-all: (handle: anyref) -> string
;; Returns file contents as string
(defparameter *fs-read-all-type*
  (list +wasm-func+
        1 +wasm-anyref+   ; 1 param (handle)
        1 +wasm-anyref+)) ; 1 result (string)

;; fs.write-all: (handle: anyref, content: string) -> void
;; Writes content to file
(defparameter *fs-write-all-type*
  (list +wasm-func+
        2 +wasm-anyref+ +wasm-anyref+  ; 2 params
        0))  ; 0 results (void)

;; fs.close: (handle: anyref) -> void
;; Closes file handle
(defparameter *fs-close-type*
  (list +wasm-func+
        1 +wasm-anyref+   ; 1 param
        0))  ; 0 results

;;; ============================================================
;;; FFI Type Index Tracking
;;; ============================================================

;; FFI function types start after built-in types
(defvar *ffi-type-base* +total-types+
  "Starting type index for FFI function types")

(defun ffi-type-index (offset)
  "Get type index for FFI function at given offset"
  (+ *ffi-type-base* offset))

;;; ============================================================
;;; Import Section Generation
;;; ============================================================

(defun encode-import-name (module-name field-name)
  "Encode import module and field names"
  (append (encode-name module-name)
          (encode-name field-name)))

(defun encode-func-import (module field type-index)
  "Encode a function import: module_name + field_name + 0x00 + type_index"
  (append (encode-import-name module field)
          (list #x00)  ; import kind: func
          (encode-unsigned-leb128 type-index)))

(defun generate-ffi-imports ()
  "Generate import section for FFI functions.
   Returns byte vector for Wasm import section (section ID 2)."
  (let* ((imports (list
                   ;; fs.open - type index 28
                   (encode-func-import "clysm" "fs.open" (ffi-type-index 0))
                   ;; fs.read-all - type index 29
                   (encode-func-import "clysm" "fs.read-all" (ffi-type-index 1))
                   ;; fs.write-all - type index 30
                   (encode-func-import "clysm" "fs.write-all" (ffi-type-index 2))
                   ;; fs.close - type index 31
                   (encode-func-import "clysm" "fs.close" (ffi-type-index 3))))
         (import-bytes '())
         (import-count (length imports)))
    ;; Encode each import
    (dolist (import imports)
      (dolist (byte import)
        (push byte import-bytes)))
    (setf import-bytes (nreverse import-bytes))
    ;; Build section
    (let* ((count-bytes (encode-unsigned-leb128 import-count))
           (content-size (+ (length count-bytes) (length import-bytes)))
           (size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      (push +section-import+ section)
      (dolist (b size-bytes) (push b section))
      (dolist (b count-bytes) (push b section))
      (dolist (b import-bytes) (push b section))
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; FFI Function Type Section Extension
;;; ============================================================

(defun generate-ffi-types ()
  "Generate additional type definitions for FFI functions.
   These types are appended after the built-in types."
  (list
   *fs-open-type*
   *fs-read-all-type*
   *fs-write-all-type*
   *fs-close-type*))

;;; ============================================================
;;; File Reading Wrapper (Lisp-level API)
;;; ============================================================

(defun read-source-file (path)
  "Read source file contents as string.
   Signals file-error if file cannot be read.

   This function is implemented as Wasm that calls FFI.
   For bootstrap, it will be cross-compiled from SBCL."
  (declare (ignore path))
  ;; Placeholder - actual implementation is in Wasm
  ;; When cross-compiled, this becomes FFI calls
  (error "read-source-file must be called in Wasm context"))

(defun read-source-string (source-string)
  "Read forms from source string.
   Returns list of parsed S-expressions."
  (with-input-from-string (stream source-string)
    (let ((forms '())
          (eof-marker (gensym)))
      (loop
        (let ((form (read stream nil eof-marker)))
          (when (eq form eof-marker)
            (return (nreverse forms)))
          (push form forms))))))

;;; ============================================================
;;; Error Condition for File Operations
;;; ============================================================

(define-condition stage0-file-error (error)
  ((pathname :initarg :pathname :reader stage0-file-error-pathname)
   (message :initarg :message :reader stage0-file-error-message))
  (:report (lambda (condition stream)
             (format stream "Stage 0 file error on ~A: ~A"
                     (stage0-file-error-pathname condition)
                     (stage0-file-error-message condition)))))

(defun signal-file-error (pathname message)
  "Signal a file error condition"
  (error 'stage0-file-error :pathname pathname :message message))
