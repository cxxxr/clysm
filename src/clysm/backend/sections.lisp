;;;; sections.lisp - Wasm binary section structure
;;;; Reference: WebAssembly Binary Format - Section definitions

(in-package #:clysm/backend/sections)

;;; ============================================================
;;; Wasm Global Structure
;;; ============================================================

(defstruct (wasm-global (:constructor make-wasm-global))
  "Wasm global variable definition"
  (name nil :type symbol)
  (type nil :type (or keyword list))  ; :i32, :anyref, (:ref-null N), etc.
  (mutability :const :type (member :const :var))
  (init-expr nil :type list))  ; initialization expression

;;; Section IDs (must be emitted in ascending order)
(defconstant +section-id-custom+ 0)
(defconstant +section-id-type+ 1)
(defconstant +section-id-import+ 2)
(defconstant +section-id-function+ 3)
(defconstant +section-id-table+ 4)
(defconstant +section-id-memory+ 5)
(defconstant +section-id-global+ 6)
(defconstant +section-id-export+ 7)
(defconstant +section-id-start+ 8)
(defconstant +section-id-element+ 9)
(defconstant +section-id-code+ 10)
(defconstant +section-id-data+ 11)
(defconstant +section-id-data-count+ 12)
(defconstant +section-id-tag+ 13)

(defstruct section
  "A Wasm binary section."
  (id 0 :type (unsigned-byte 8))
  (content #() :type (vector (unsigned-byte 8))))

(defun encode-section (section)
  "Encode a section to bytes: ID + size(LEB128) + content."
  (let* ((content (section-content section))
         (size-bytes (encode-unsigned-leb128 (length content)))
         (result (make-array (+ 1 (length size-bytes) (length content))
                             :element-type '(unsigned-byte 8))))
    ;; Section ID
    (setf (aref result 0) (section-id section))
    ;; Content size (LEB128)
    (loop for i from 0 below (length size-bytes)
          do (setf (aref result (1+ i)) (aref size-bytes i)))
    ;; Content
    (loop for i from 0 below (length content)
          do (setf (aref result (+ 1 (length size-bytes) i)) (aref content i)))
    result))

(defun validate-section-order (sections)
  "Validate that sections are in ascending ID order.
   Returns T if valid, signals error otherwise."
  (loop for (prev curr) on sections
        while curr
        do (when (>= (section-id prev) (section-id curr))
             (error "Section order violation: ID ~D appears after ID ~D"
                    (section-id curr) (section-id prev))))
  t)

(defun encode-vec (items encoder)
  "Encode a vector of items with count prefix."
  (let ((count-bytes (encode-unsigned-leb128 (length items)))
        (encoded-items (mapcar encoder items)))
    (apply #'concatenate '(vector (unsigned-byte 8))
           count-bytes
           encoded-items)))

(defun encode-name (name)
  "Encode a UTF-8 name as length-prefixed byte sequence."
  (let* ((bytes (babel:string-to-octets name :encoding :utf-8))
         (len-bytes (encode-unsigned-leb128 (length bytes))))
    (concatenate '(vector (unsigned-byte 8)) len-bytes bytes)))

;;; ============================================================
;;; Type Section (T038)
;;; ============================================================

(defun make-type-section (types)
  "Create a Type Section from a list of type definitions.
   TYPES should be a list of wasm-struct-type or wasm-array-type."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Type count
    (let ((count-bytes (encode-unsigned-leb128 (length types))))
      (loop for b across count-bytes do (vector-push-extend b content)))
    ;; Each type definition (placeholder - full impl in gc-types.lisp)
    (dolist (type types)
      (declare (ignore type))
      ;; Type definitions encoded by gc-types module
      )
    (make-section :id +section-id-type+
                  :content (coerce content '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Global Section (T041)
;;; ============================================================

(defun make-global-section (globals)
  "Create a Global Section from a list of wasm-global definitions."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Global count
    (let ((count-bytes (encode-unsigned-leb128 (length globals))))
      (loop for b across count-bytes do (vector-push-extend b content)))
    ;; Each global entry
    (dolist (global globals)
      (encode-global global content))
    (make-section :id +section-id-global+
                  :content (coerce content '(vector (unsigned-byte 8))))))

(defun encode-global (global buffer)
  "Encode a global variable definition to the buffer."
  (let ((type (wasm-global-type global))
        (mutability (wasm-global-mutability global))
        (init-expr (wasm-global-init-expr global)))
    ;; Global type: valtype + mutability
    (encode-valtype type buffer)
    (vector-push-extend (if (eq mutability :const) 0 1) buffer)
    ;; Init expression
    (encode-init-expr init-expr buffer)))

(defun encode-valtype (type buffer)
  "Encode a value type to the buffer."
  (cond
    ;; Compound reference type: (:ref-null typeidx)
    ((and (listp type) (eq (car type) :ref-null))
     (vector-push-extend #x63 buffer)  ; refnull
     (let ((idx-bytes (encode-signed-leb128 (cadr type))))
       (loop for b across idx-bytes do (vector-push-extend b buffer))))
    ;; Compound reference type: (:ref typeidx) - non-nullable
    ((and (listp type) (eq (car type) :ref))
     (vector-push-extend #x64 buffer)  ; ref
     (let ((idx-bytes (encode-signed-leb128 (cadr type))))
       (loop for b across idx-bytes do (vector-push-extend b buffer))))
    ;; Simple types
    ((keywordp type)
     (vector-push-extend
      (ecase type
        (:i32 #x7F)
        (:i64 #x7E)
        (:f32 #x7D)
        (:f64 #x7C)
        (:anyref #x6E)     ; any heap type
        (:externref #x6F)  ; external references
        (:funcref #x70)
        (:i31ref #x6C)
        (:eqref #x6D))
      buffer))
    (t
     (error "Unknown value type: ~A" type))))

(defun encode-init-expr (expr buffer)
  "Encode an initialization expression to the buffer."
  ;; Simple init expressions for now
  (dolist (instr expr)
    (encode-instruction instr buffer))
  ;; End marker
  (vector-push-extend #x0B buffer))

(defun encode-instruction (instr buffer)
  "Encode a single instruction to the buffer."
  (cond
    ;; i32.const
    ((and (listp instr) (eq (car instr) :i32.const))
     (vector-push-extend #x41 buffer)
     (let ((val-bytes (encode-signed-leb128 (cadr instr))))
       (loop for b across val-bytes do (vector-push-extend b buffer))))
    ;; i64.const
    ((and (listp instr) (eq (car instr) :i64.const))
     (vector-push-extend #x42 buffer)
     (let ((val-bytes (encode-signed-leb128 (cadr instr))))
       (loop for b across val-bytes do (vector-push-extend b buffer))))
    ;; struct.new
    ((and (listp instr) (eq (car instr) :struct.new))
     (vector-push-extend #xFB buffer)  ; GC prefix
     (vector-push-extend #x00 buffer)  ; struct.new
     (let ((idx-bytes (encode-unsigned-leb128 (cadr instr))))
       (loop for b across idx-bytes do (vector-push-extend b buffer))))
    ;; struct.new_default
    ((and (listp instr) (eq (car instr) :struct.new_default))
     (vector-push-extend #xFB buffer)  ; GC prefix
     (vector-push-extend #x01 buffer)  ; struct.new_default
     (let ((idx-bytes (encode-unsigned-leb128 (cadr instr))))
       (loop for b across idx-bytes do (vector-push-extend b buffer))))
    ;; global.get
    ((eq instr :global.get)
     (vector-push-extend #x23 buffer))
    ;; ref.eq
    ((eq instr :ref.eq)
     (vector-push-extend #xD5 buffer))
    ;; ref.is_null
    ((eq instr :ref.is_null)
     (vector-push-extend #xD1 buffer))
    ;; Keywords for simple instructions
    ((keywordp instr)
     (error "Unknown instruction: ~A" instr))
    (t
     (error "Invalid instruction format: ~A" instr))))

;;; ============================================================
;;; Function Section (T062)
;;; ============================================================

(defun make-function-section (type-indices)
  "Create a Function Section mapping function indices to type indices."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Function count
    (let ((count-bytes (encode-unsigned-leb128 (length type-indices))))
      (loop for b across count-bytes do (vector-push-extend b content)))
    ;; Each type index
    (dolist (idx type-indices)
      (let ((idx-bytes (encode-unsigned-leb128 idx)))
        (loop for b across idx-bytes do (vector-push-extend b content))))
    (make-section :id +section-id-function+
                  :content (coerce content '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Code Section (T062)
;;; ============================================================

(defun make-code-section (function-bodies)
  "Create a Code Section from a list of function bodies.
   Each function body is a list of (locals instructions)."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Function count
    (let ((count-bytes (encode-unsigned-leb128 (length function-bodies))))
      (loop for b across count-bytes do (vector-push-extend b content)))
    ;; Each function body
    (dolist (body function-bodies)
      (encode-function-body body content))
    (make-section :id +section-id-code+
                  :content (coerce content '(vector (unsigned-byte 8))))))

(defun encode-function-body (body buffer)
  "Encode a function body (locals + instructions) to the buffer."
  ;; First encode to a temp buffer to get size
  (let ((temp (make-array 0 :element-type '(unsigned-byte 8)
                            :adjustable t :fill-pointer 0)))
    ;; Locals
    (let ((locals (car body))
          (instructions (cadr body)))
      ;; Local count (groups of same type)
      (let ((count-bytes (encode-unsigned-leb128 (length locals))))
        (loop for b across count-bytes do (vector-push-extend b temp)))
      ;; Each local group: (count type)
      (dolist (local locals)
        (let ((count-bytes (encode-unsigned-leb128 (car local))))
          (loop for b across count-bytes do (vector-push-extend b temp)))
        (encode-valtype (cadr local) temp))
      ;; Instructions
      (dolist (instr instructions)
        (encode-instruction instr temp))
      ;; End marker
      (vector-push-extend #x0B temp))
    ;; Now write size + content to main buffer
    (let ((size-bytes (encode-unsigned-leb128 (length temp))))
      (loop for b across size-bytes do (vector-push-extend b buffer)))
    (loop for b across temp do (vector-push-extend b buffer))))

;;; ============================================================
;;; Export Section (T063)
;;; ============================================================

(defun make-export-section (exports)
  "Create an Export Section from a list of (name kind index)."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Export count
    (let ((count-bytes (encode-unsigned-leb128 (length exports))))
      (loop for b across count-bytes do (vector-push-extend b content)))
    ;; Each export
    (dolist (export exports)
      (encode-export export content))
    (make-section :id +section-id-export+
                  :content (coerce content '(vector (unsigned-byte 8))))))

(defun encode-export (export buffer)
  "Encode an export entry to the buffer."
  (destructuring-bind (name kind index) export
    ;; Name
    (let ((name-bytes (encode-name name)))
      (loop for b across name-bytes do (vector-push-extend b buffer)))
    ;; Export kind
    (vector-push-extend
     (ecase kind
       (:func 0)
       (:table 1)
       (:memory 2)
       (:global 3))
     buffer)
    ;; Index
    (let ((idx-bytes (encode-unsigned-leb128 index)))
      (loop for b across idx-bytes do (vector-push-extend b buffer)))))
