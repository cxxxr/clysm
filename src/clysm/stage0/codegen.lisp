;;;; codegen.lisp - Wasm binary emitter for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements T027: Wasm binary emitter (cross-compiled)
;;;;
;;;; This emits the final Wasm binary from compiled IR.

(in-package #:clysm/stage0)

;;; ============================================================
;;; Code Section Generation
;;; ============================================================

(defstruct function-body
  "Compiled function body"
  (name nil :type symbol)
  (locals nil :type list)
  (code nil :type list))

(defvar *compiled-functions* nil
  "List of compiled function bodies")

(defun emit-function-body (func-body)
  "Emit bytes for a single function body"
  (let* ((locals (function-body-locals func-body))
         (code (function-body-code func-body))
         (local-decls (emit-local-declarations locals))
         (code-bytes (ir-to-instructions code))
         (body-bytes (append local-decls code-bytes (list +end+)))
         (size-bytes (encode-unsigned-leb128 (length body-bytes))))
    (append size-bytes body-bytes)))

(defun emit-local-declarations (locals)
  "Emit local variable declarations"
  (if (null locals)
      (list 0)  ; 0 local groups
      (let ((groups (group-locals-by-type locals)))
        (cons (length groups)
              (mapcan (lambda (group)
                        (append (encode-unsigned-leb128 (length (cdr group)))
                                (list (encode-valtype (car group)))))
                      groups)))))

(defun group-locals-by-type (locals)
  "Group locals by type for compact encoding"
  (let ((groups '()))
    (dolist (local locals)
      (let* ((type (second local))
             (existing (assoc type groups)))
        (if existing
            (push (first local) (cdr existing))
            (push (list type (first local)) groups))))
    (nreverse groups)))

(defun encode-valtype (type)
  "Encode value type to byte"
  (case type
    (:i32 #x7F)
    (:i64 #x7E)
    (:f32 #x7D)
    (:f64 #x7C)
    (:anyref #x6F)
    (:funcref #x70)
    (t #x6F)))  ; default to anyref

;;; ============================================================
;;; Section Emission
;;; ============================================================

(defun emit-wasm-binary (sections)
  "Emit complete Wasm binary from section list.
   Each section is (section-id . bytes)."
  (let ((result (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0)))
    ;; Magic number
    (dolist (b *wasm-magic*)
      (vector-push-extend b result))
    ;; Version
    (dolist (b *wasm-version*)
      (vector-push-extend b result))
    ;; Sections (already in correct order)
    (dolist (section sections)
      (loop for byte across section
            do (vector-push-extend byte result)))
    result))

(defun emit-type-section ()
  "Emit type section bytes"
  (generate-type-section))

(defun emit-function-section (func-type-indices)
  "Emit function section declaring function types.
   func-type-indices is list of type indices for each function."
  (let* ((count (length func-type-indices))
         (index-bytes (mapcan #'encode-unsigned-leb128 func-type-indices))
         (count-bytes (encode-unsigned-leb128 count))
         (content-bytes (append count-bytes index-bytes))
         (size-bytes (encode-unsigned-leb128 (length content-bytes))))
    (coerce (cons +section-function+
                  (append size-bytes content-bytes))
            '(vector (unsigned-byte 8)))))

(defun emit-export-section (exports)
  "Emit export section.
   exports is list of (name kind index)."
  (generate-export-section exports))

(defun emit-code-section (function-bodies)
  "Emit code section with function bodies"
  (let* ((count (length function-bodies))
         (body-bytes (mapcan #'emit-function-body function-bodies))
         (count-bytes (encode-unsigned-leb128 count))
         (content-bytes (append count-bytes body-bytes))
         (size-bytes (encode-unsigned-leb128 (length content-bytes))))
    (coerce (cons +section-code+
                  (append size-bytes content-bytes))
            '(vector (unsigned-byte 8)))))

;;; ============================================================
;;; Complete Module Emission
;;; ============================================================

(defun emit-stage0-module (functions exports)
  "Emit complete Stage 0 Wasm module.
   functions: list of function-body structs
   exports: list of (name kind index)"
  (let* ((type-section (emit-type-section))
         (import-section (generate-ffi-imports))
         (func-indices (loop for i from 0 below (length functions)
                             collect (+ +total-types+ i)))
         (func-section (emit-function-section func-indices))
         (global-section (generate-global-section))
         (export-section (emit-export-section exports))
         (code-section (emit-code-section functions)))
    (emit-wasm-binary
     (list type-section
           import-section
           func-section
           global-section
           export-section
           code-section))))
