;;;; type-construction.lisp - Wasm type construction helpers
;;;; Feature: 001-compiler-internal-consolidation (Phase 5, US4)
;;;;
;;;; Implements placeholder functions for Wasm type construction using
;;;; pre-allocated type indices (0-28). Full dynamic type registration deferred.
;;;;
;;;; This resolves the "Undefined function: MAKE-WASM-STRUCT-TYPE" blocker
;;;; (P321 in stage1-report.json, 17 occurrences, 1.90%).
;;;;
;;;; HyperSpec references:
;;;;   N/A - WasmGC-specific functionality

(in-package #:clysm)

;;; ============================================================
;;; T045: Known Type Index Mapping
;;; ============================================================
;;; Pre-allocated WasmGC type indices as documented in CLAUDE.md.
;;; These correspond to types defined in gc-types.lisp.

(defparameter *known-type-indices*
  '((cons . 0)
    (symbol . 1)
    (string . 2)
    (closure . 3)
    (float . 4)
    (ratio . 5)
    (instance . 6)
    (standard-class . 7)
    ;; Function types 8-13
    (func-0 . 8)
    (func-1 . 9)
    (func-2 . 10)
    (func-3 . 11)
    (func-4 . 12)
    (func-5 . 13)
    ;; Binding/frame types
    (binding-frame . 14)
    (special-binding . 15)
    (lexical-binding . 16)
    (stream . 17)
    ;; Container types
    (hash-table . 18)
    (hash-entry . 19)
    (vector . 20)
    (slot-vector . 21)
    (mv-array . 22)
    ;; Compiler types
    (compilation-env . 23)
    (macro-environment . 24)
    (source-location . 25)
    ;; Extended types
    (complex . 26)
    (pathname . 27)
    (mdarray . 28))
  "Association list mapping type names to pre-allocated WasmGC type indices.
   Indices 0-28 are fixed and defined in gc-types.lisp.")

;;; ============================================================
;;; T047: type-index-for-name - Type name to index lookup
;;; ============================================================

(defun type-index-for-name (name)
  "Look up the WasmGC type index for a type NAME.
   NAME can be a symbol or string.
   Returns the type index (0-28) for known types.
   Signals an error for unknown types.

   Usage:
     (type-index-for-name 'cons)    => 0
     (type-index-for-name 'symbol)  => 1
     (type-index-for-name 'closure) => 3"
  (unless name
    (error "Type name cannot be nil"))
  (let* ((name-sym (if (stringp name)
                       (intern (string-upcase name) :clysm)
                       name))
         (entry (assoc name-sym *known-type-indices* :test #'eq)))
    (if entry
        (cdr entry)
        (error "Unknown WasmGC type: ~A. Known types: ~{~A~^, ~}"
               name
               (mapcar #'car *known-type-indices*)))))

;;; ============================================================
;;; T046: make-wasm-struct-type* - Placeholder constructor
;;; ============================================================
;;; This is a Wasm-compilable placeholder for make-wasm-struct-type.
;;; Instead of actually creating a defstruct instance, it returns
;;; a simple structure with the pre-allocated index.

(defstruct (wasm-struct-type-placeholder
            (:conc-name wstph-)
            (:constructor %make-wasm-struct-type-placeholder))
  "Placeholder for wasm-struct-type during Stage 1 compilation."
  (index nil :type (or null fixnum))
  (name nil :type symbol)
  (fields nil :type list)
  (super nil))

(defun make-wasm-struct-type* (&key name fields super index)
  "Create a Wasm struct type placeholder.
   For known types, returns a placeholder with the pre-allocated index.
   For unknown types, signals an error.

   This is a Wasm-compilable replacement for make-wasm-struct-type
   that doesn't require the full defstruct infrastructure.

   Usage:
     (make-wasm-struct-type* :name 'cons)
     => #S(WASM-STRUCT-TYPE-PLACEHOLDER :INDEX 0 :NAME CONS ...)"
  (declare (ignore fields super))
  (let ((type-index (or index (type-index-for-name name))))
    (%make-wasm-struct-type-placeholder
     :index type-index
     :name name)))

;; Accessor for compatibility with code expecting wasm-struct-type-index
(defun wasm-struct-type-index (type-obj)
  "Get the type index from a wasm struct type or placeholder."
  (typecase type-obj
    (wasm-struct-type-placeholder
     (wstph-index type-obj))
    (t
     ;; Try to access as if it's a wasm-struct-type from gc-types.lisp
     (if (and (typep type-obj 'structure-object)
              (slot-exists-p type-obj 'index))
         (slot-value type-obj 'index)
         (error "Cannot get type index from: ~A" type-obj)))))

;;; ============================================================
;;; Convenience constructors for common types
;;; ============================================================

(defun make-cons-type-placeholder ()
  "Create placeholder for cons type (index 0)."
  (make-wasm-struct-type* :name 'cons :index 0))

(defun make-symbol-type-placeholder ()
  "Create placeholder for symbol type (index 1)."
  (make-wasm-struct-type* :name 'symbol :index 1))

(defun make-string-type-placeholder ()
  "Create placeholder for string type (index 2)."
  (make-wasm-struct-type* :name 'string :index 2))

(defun make-closure-type-placeholder ()
  "Create placeholder for closure type (index 3)."
  (make-wasm-struct-type* :name 'closure :index 3))

(defun make-instance-type-placeholder ()
  "Create placeholder for instance type (index 6)."
  (make-wasm-struct-type* :name 'instance :index 6))

;;; ============================================================
;;; Export declarations
;;; ============================================================

(export '(*known-type-indices*
          type-index-for-name
          make-wasm-struct-type*
          wasm-struct-type-placeholder
          wstph-index
          wstph-name
          wasm-struct-type-index
          make-cons-type-placeholder
          make-symbol-type-placeholder
          make-string-type-placeholder
          make-closure-type-placeholder
          make-instance-type-placeholder))
