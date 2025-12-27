;;;; runtime.lisp - Runtime initialization for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements US4: Runtime Initialization
;;;;
;;;; T012: Implement _initialize export function
;;;; T013: Implement start function for auto-init
;;;;
;;;; This file generates the complete runtime initialization for Stage 0,
;;;; combining type section, global section, and start function.

(in-package #:clysm/stage0)

;;; ============================================================
;;; Wasm Module Structure Constants
;;; ============================================================

;; Wasm magic number: \0asm
(defparameter *wasm-magic* '(#x00 #x61 #x73 #x6D))

;; Wasm version 1.0
(defparameter *wasm-version* '(#x01 #x00 #x00 #x00))

;; Note: Section IDs are defined in types.lisp

;;; ============================================================
;;; Function Section Generation
;;; ============================================================

(defun generate-empty-function-section ()
  "Generate an empty function section (no user functions initially)."
  ;; Section ID 3, size 1, count 0
  (coerce '(3 1 0) '(vector (unsigned-byte 8))))

;;; ============================================================
;;; Start Function Generation
;;; ============================================================

(defvar *start-function-index* nil
  "Index of the start function in the function section")

(defun generate-start-function ()
  "Generate start function body for auto-initialization.
   The start function runs automatically when the module is instantiated.
   Returns list of instructions."
  ;; Start function currently just returns (no additional init needed)
  ;; All initialization happens in global init expressions
  (list +end+))

(defun generate-start-section (start-func-index)
  "Generate start section pointing to the start function.
   Returns byte vector for Wasm start section (section ID 8)."
  (let* ((index-bytes (encode-unsigned-leb128 start-func-index))
         (size-bytes (encode-unsigned-leb128 (length index-bytes)))
         (section '()))
    ;; Section ID 8
    (push +section-start+ section)
    ;; Section size
    (dolist (b size-bytes) (push b section))
    ;; Function index
    (dolist (b index-bytes) (push b section))
    (coerce (nreverse section) '(vector (unsigned-byte 8)))))

;;; ============================================================
;;; Code Section Generation
;;; ============================================================

(defun generate-empty-code-section ()
  "Generate an empty code section."
  ;; Section ID 10, size 1, count 0
  (coerce '(10 1 0) '(vector (unsigned-byte 8))))

;;; ============================================================
;;; Export Section Generation
;;; ============================================================

(defun encode-name (name)
  "Encode a name as UTF-8 bytes with length prefix."
  (let* ((bytes (map 'list #'char-code name))
         (len-bytes (encode-unsigned-leb128 (length bytes))))
    (append len-bytes bytes)))

(defun generate-initialize-export (func-index)
  "Generate export entry for _initialize function."
  (append (encode-name "_initialize")
          (list #x00)  ; export kind: func
          (encode-unsigned-leb128 func-index)))

(defun generate-export-section (exports)
  "Generate export section for given export list.
   Each export is (name kind index).
   Returns byte vector for Wasm export section (section ID 7)."
  (let* ((export-bytes '())
         (export-count (length exports)))
    ;; Encode each export
    (dolist (export exports)
      (destructuring-bind (name kind index) export
        (dolist (b (encode-name name))
          (push b export-bytes))
        (push kind export-bytes)
        (dolist (b (encode-unsigned-leb128 index))
          (push b export-bytes))))
    (setf export-bytes (nreverse export-bytes))
    ;; Build section
    (let* ((count-bytes (encode-unsigned-leb128 export-count))
           (content-size (+ (length count-bytes) (length export-bytes)))
           (size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      (push +section-export+ section)
      (dolist (b size-bytes) (push b section))
      (dolist (b count-bytes) (push b section))
      (dolist (b export-bytes) (push b section))
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Complete Runtime Generation
;;; ============================================================

(defun concatenate-byte-vectors (&rest vectors)
  "Concatenate multiple byte vectors into one."
  (let* ((total-length (reduce #'+ vectors :key #'length))
         (result (make-array total-length
                             :element-type '(unsigned-byte 8)
                             :initial-element 0))
         (pos 0))
    (dolist (vec vectors)
      (loop for byte across vec
            do (setf (aref result pos) byte)
               (incf pos)))
    result))

;;; ============================================================
;;; Stub Function Generation
;;; ============================================================

(defun generate-function-section (num-funcs)
  "Generate function section declaring NUM-FUNCS functions.
   compile_form: type 9 (externref)->externref
   compile_all: type 8 ()->externref
   _initialize: type 8 ()->externref"
  (let* ((count-bytes (encode-unsigned-leb128 num-funcs))
         ;; Function 0 (compile_form): type 9 - (func (param externref) (result externref))
         ;; Function 1 (compile_all): type 8 - (func (result externref))
         ;; Function 2 (_initialize): type 8 - (func (result externref))
         (type-indices '(9 8 8))
         (type-bytes (mapcar #'(lambda (idx) (car (encode-unsigned-leb128 idx))) type-indices))
         (content-size (+ (length count-bytes) (length type-bytes)))
         (size-bytes (encode-unsigned-leb128 content-size))
         (section '()))
    ;; Section ID 3 (function)
    (push +section-function+ section)
    (dolist (b size-bytes) (push b section))
    (dolist (b count-bytes) (push b section))
    (dolist (b type-bytes) (push b section))
    (coerce (nreverse section) '(vector (unsigned-byte 8)))))

(defun generate-stub-code-section ()
  "Generate code section with 3 stub functions.
   compile_form: (param externref) -> (result externref), returns null
   compile_all: () -> (result externref), returns null
   _initialize: () -> (result externref), returns null"
  (let* ((num-funcs 3)
         ;; Function 0 (compile_form): has 1 param, ignore it and return null
         ;; Body: 0 locals, drop (param is on stack but we ignore it...
         ;; Actually wasmtime passes params differently - the param is local.get 0
         ;; So just return null, the param is in local slot 0 which we don't use
         ;; Body: 0 locals, ref.null extern, end
         (compile-form-body '(#x00 #xD0 #x6F #x0B))  ; 0 locals, ref.null extern, end
         ;; Functions 1 and 2: no params, return null
         (simple-body '(#x00 #xD0 #x6F #x0B))  ; 0 locals, ref.null extern, end
         ;; Build function entries
         (all-funcs '()))
    ;; Function 0 (compile_form)
    (let* ((body compile-form-body)
           (size-leb (encode-unsigned-leb128 (length body))))
      (setf all-funcs (append all-funcs size-leb body)))
    ;; Function 1 (compile_all)
    (let* ((body simple-body)
           (size-leb (encode-unsigned-leb128 (length body))))
      (setf all-funcs (append all-funcs size-leb body)))
    ;; Function 2 (_initialize)
    (let* ((body simple-body)
           (size-leb (encode-unsigned-leb128 (length body))))
      (setf all-funcs (append all-funcs size-leb body)))
    ;; Build section
    (let* ((count-bytes (encode-unsigned-leb128 num-funcs))
           (content-size (+ (length count-bytes) (length all-funcs)))
           (section-size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      ;; Section ID 10 (code)
      (push +section-code+ section)
      (dolist (b section-size-bytes) (push b section))
      (dolist (b count-bytes) (push b section))
      (dolist (b all-funcs) (push b section))
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

(defun generate-runtime-init ()
  "Generate complete runtime initialization as Wasm module.
   Returns byte vector containing:
   - Wasm magic number and version
   - Type section (all WasmGC types)
   - Function section (declares 3 functions)
   - Global section (NIL, UNBOUND, mv-count, mv-buffer)
   - Export section (compile_form, compile_all, _initialize)
   - Code section (stub implementations)

   This creates a minimal valid Wasm module for Stage 0 runtime."
  (let* (;; Generate sections
         (header (coerce (append *wasm-magic* *wasm-version*)
                         '(vector (unsigned-byte 8))))
         (type-section (generate-type-section))
         (function-section (generate-function-section 3))
         (global-section (generate-global-section))
         ;; Export compile_form (func 0), compile_all (func 1), _initialize (func 2)
         (export-section (generate-export-section
                          '(("compile_form" #x00 0)
                            ("compile_all" #x00 1)
                            ("_initialize" #x00 2))))
         (code-section (generate-stub-code-section)))
    ;; Concatenate all sections in order (must be: type, function, global, export, code)
    (concatenate-byte-vectors
     header
     type-section
     function-section
     global-section
     export-section
     code-section)))

;;; ============================================================
;;; Full Stage 0 Module Generation
;;; ============================================================

(defun generate-stage0-module ()
  "Generate complete Stage 0 Wasm module with all sections.
   This is used when building the full compiler binary."
  ;; For now, return runtime init
  ;; Will be extended when compile_form and compile_all are implemented
  (generate-runtime-init))
