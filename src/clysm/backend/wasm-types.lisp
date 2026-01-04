;;;; backend/wasm-types.lisp - WebAssembly Type Definitions
;;;;
;;;; Data structures representing Wasm modules, types, functions, etc.
;;;; These are used by the emitter to generate binary and text output.

(in-package #:clysm)

;;; ============================================================
;;; Wasm Value Types
;;; ============================================================

(deftype wasm-numtype ()
  "Wasm numeric value types."
  '(member :i32 :i64 :f32 :f64))

(deftype wasm-vectype ()
  "Wasm vector type."
  '(member :v128))

(deftype wasm-reftype ()
  "Wasm reference types."
  '(member :funcref :externref
           ;; GC types
           :anyref :eqref :i31ref :structref :arrayref :nullref
           :nullfuncref :nullexternref))

(deftype wasm-valtype ()
  "Any Wasm value type."
  '(or wasm-numtype wasm-vectype wasm-reftype
       ;; Type index reference (for GC types)
       (cons (eql :ref) t)))

;;; ============================================================
;;; Type Encoding Constants
;;; ============================================================

(defparameter *valtype-encoding*
  '((:i32 . #x7F)
    (:i64 . #x7E)
    (:f32 . #x7D)
    (:f64 . #x7C)
    (:v128 . #x7B)
    (:funcref . #x70)
    (:externref . #x6F)
    ;; GC reference types
    (:anyref . #x6E)
    (:eqref . #x6D)
    (:i31ref . #x6C)
    (:structref . #x6B)
    (:arrayref . #x6A)
    (:nullref . #x71)
    (:nullfuncref . #x73)
    (:nullexternref . #x72))
  "Mapping from value type keywords to their binary encoding.")

(defun encode-valtype (valtype)
  "Encode a value type to its binary representation."
  (if (and (consp valtype) (eq (car valtype) :ref))
      ;; Reference to a defined type: (ref nullable? typeidx)
      (let* ((rest (cdr valtype))
             (nullable (eq (car rest) :null))
             (typeidx (if nullable (cadr rest) (car rest))))
        (append (list (if nullable #x63 #x64))  ; nullable or non-null ref
                (encode-sleb128 typeidx)))
      ;; Simple type
      (let ((encoding (cdr (assoc valtype *valtype-encoding*))))
        (unless encoding
          (error "Unknown value type: ~S" valtype))
        (list encoding))))

;;; ============================================================
;;; Function Types
;;; ============================================================

(defstruct (wasm-functype (:constructor make-functype (params results)))
  "A Wasm function type: params -> results."
  (params nil :type list)    ; List of valtypes
  (results nil :type list))  ; List of valtypes

(defun encode-functype (functype)
  "Encode a function type to bytes."
  (append (list #x60)  ; functype marker
          (encode-vector #'encode-valtype (wasm-functype-params functype))
          (encode-vector #'encode-valtype (wasm-functype-results functype))))

;;; ============================================================
;;; GC Types: Struct and Array
;;; ============================================================

(defstruct (wasm-field (:constructor make-field (type &key mutable)))
  "A field in a struct type."
  (type nil)                     ; valtype
  (mutable nil :type boolean))   ; mutable?

(defstruct (wasm-structtype (:constructor make-structtype (fields)))
  "A Wasm GC struct type."
  (fields nil :type list))  ; List of wasm-field

(defstruct (wasm-arraytype (:constructor make-arraytype (element &key mutable)))
  "A Wasm GC array type."
  (element nil)                  ; valtype
  (mutable nil :type boolean))

(defun encode-field (field)
  "Encode a struct field."
  (append (encode-valtype (wasm-field-type field))
          (list (if (wasm-field-mutable field) 1 0))))

(defun encode-structtype (structtype)
  "Encode a struct type definition."
  (append (list #x5F)  ; struct marker
          (encode-vector #'encode-field (wasm-structtype-fields structtype))))

(defun encode-arraytype (arraytype)
  "Encode an array type definition."
  (append (list #x5E)  ; array marker
          (encode-field (make-field (wasm-arraytype-element arraytype)
                                    :mutable (wasm-arraytype-mutable arraytype)))))

;;; ============================================================
;;; Composite Types (for Type Section)
;;; ============================================================

(defstruct (wasm-type (:constructor %make-wasm-type))
  "A type definition in the type section."
  (index nil :type (or null integer))  ; Type index (set during emit)
  (name nil :type (or null string))    ; Optional debug name
  (definition nil))                     ; functype, structtype, or arraytype

(defun make-wasm-type (definition &key name)
  "Create a wasm-type with a definition."
  (%make-wasm-type :definition definition :name name))

(defun encode-type-definition (typedef)
  "Encode a type definition (func, struct, or array)."
  (let ((def (wasm-type-definition typedef)))
    (etypecase def
      (wasm-functype (encode-functype def))
      (wasm-structtype (encode-structtype def))
      (wasm-arraytype (encode-arraytype def)))))

;;; ============================================================
;;; Functions
;;; ============================================================

(defstruct (wasm-func (:constructor %make-wasm-func))
  "A Wasm function."
  (index nil :type (or null integer))   ; Function index
  (name nil :type (or null string))     ; Debug name
  (type-index nil :type (or null integer))  ; Index into type section
  (locals nil :type list)               ; List of (count . valtype)
  (body nil :type list))                ; List of instructions (bytes or ops)

(defun make-wasm-func (type-index &key name locals body)
  "Create a Wasm function."
  (%make-wasm-func :type-index type-index
                   :name name
                   :locals locals
                   :body body))

;;; ============================================================
;;; Imports and Exports
;;; ============================================================

(deftype wasm-import-kind ()
  '(member :func :table :memory :global))

(defstruct (wasm-import (:constructor make-import (module name kind desc)))
  "A Wasm import."
  (module nil :type string)     ; Module name
  (name nil :type string)       ; Import name
  (kind nil :type wasm-import-kind)
  (desc nil))                   ; Type index or limits

(deftype wasm-export-kind ()
  '(member :func :table :memory :global))

(defstruct (wasm-export (:constructor make-export (name kind index)))
  "A Wasm export."
  (name nil :type string)
  (kind nil :type wasm-export-kind)
  (index nil :type integer))

(defparameter *export-kind-encoding*
  '((:func . #x00)
    (:table . #x01)
    (:memory . #x02)
    (:global . #x03)))

(defun encode-export (export)
  "Encode an export entry."
  (append (encode-name (wasm-export-name export))
          (list (cdr (assoc (wasm-export-kind export) *export-kind-encoding*)))
          (encode-uleb128 (wasm-export-index export))))

;;; ============================================================
;;; Memory and Table
;;; ============================================================

(defstruct (wasm-limits (:constructor make-limits (min &optional max)))
  "Memory or table limits."
  (min 0 :type integer)
  (max nil :type (or null integer)))

(defun encode-limits (limits)
  "Encode limits."
  (if (wasm-limits-max limits)
      (append (list #x01)
              (encode-uleb128 (wasm-limits-min limits))
              (encode-uleb128 (wasm-limits-max limits)))
      (append (list #x00)
              (encode-uleb128 (wasm-limits-min limits)))))

(defstruct (wasm-memory (:constructor make-memory (limits)))
  "A Wasm linear memory."
  (limits nil :type wasm-limits))

(defstruct (wasm-table (:constructor make-table (reftype limits)))
  "A Wasm table."
  (reftype :funcref)
  (limits nil :type wasm-limits))

;;; ============================================================
;;; Globals
;;; ============================================================

(defstruct (wasm-global (:constructor make-global (type init &key mutable name)))
  "A Wasm global variable."
  (name nil :type (or null string))
  (type nil)                     ; valtype
  (mutable nil :type boolean)
  (init nil :type list))         ; Init expression (bytes)

;;; ============================================================
;;; Module
;;; ============================================================

(defstruct (wasm-module (:constructor %make-wasm-module))
  "A complete Wasm module."
  (types nil :type list)         ; List of wasm-type
  (imports nil :type list)       ; List of wasm-import
  (funcs nil :type list)         ; List of wasm-func
  (tables nil :type list)        ; List of wasm-table
  (memories nil :type list)      ; List of wasm-memory
  (globals nil :type list)       ; List of wasm-global
  (exports nil :type list)       ; List of wasm-export
  (start nil :type (or null integer))  ; Start function index
  (elements nil :type list)      ; Element segments
  (data nil :type list)          ; Data segments
  (custom nil :type list))       ; Custom sections

(defun make-wasm-module (&key types imports funcs tables memories
                              globals exports start elements data custom)
  "Create a Wasm module."
  (%make-wasm-module :types types
                     :imports imports
                     :funcs funcs
                     :tables tables
                     :memories memories
                     :globals globals
                     :exports exports
                     :start start
                     :elements elements
                     :data data
                     :custom custom))

;;; ============================================================
;;; Module Builder Helpers
;;; ============================================================

(defun module-add-type (module type)
  "Add a type to module, returning its index."
  (let ((index (length (wasm-module-types module))))
    (setf (wasm-type-index type) index)
    (push type (wasm-module-types module))
    index))

(defun module-add-func (module func)
  "Add a function to module, returning its index."
  (let ((index (+ (length (wasm-module-imports module))
                  (length (wasm-module-funcs module)))))
    (setf (wasm-func-index func) index)
    (push func (wasm-module-funcs module))
    index))

(defun module-add-export (module export)
  "Add an export to module."
  (push export (wasm-module-exports module)))

(defun module-finalize (module)
  "Finalize module: reverse lists to correct order."
  (setf (wasm-module-types module) (nreverse (wasm-module-types module)))
  (setf (wasm-module-funcs module) (nreverse (wasm-module-funcs module)))
  (setf (wasm-module-exports module) (nreverse (wasm-module-exports module)))
  (setf (wasm-module-imports module) (nreverse (wasm-module-imports module)))
  (setf (wasm-module-globals module) (nreverse (wasm-module-globals module)))
  module)
