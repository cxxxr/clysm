;;;; types.lisp - WebAssembly type definitions

(in-package #:cl-wasm/wasm)

;;; WASM Magic and Version
(defconstant +wasm-magic+ #x6d736100)  ; "\0asm" in little-endian
(defconstant +wasm-version+ 1)

;;; Section IDs
(defconstant +section-custom+ 0)
(defconstant +section-type+ 1)
(defconstant +section-import+ 2)
(defconstant +section-function+ 3)
(defconstant +section-table+ 4)
(defconstant +section-memory+ 5)
(defconstant +section-global+ 6)
(defconstant +section-export+ 7)
(defconstant +section-start+ 8)
(defconstant +section-element+ 9)
(defconstant +section-code+ 10)
(defconstant +section-data+ 11)
(defconstant +section-data-count+ 12)

;;; Value Types (numtype, vectype, reftype)
;; Numeric types
(defconstant +type-i32+ #x7f)   ; -0x01
(defconstant +type-i64+ #x7e)   ; -0x02
(defconstant +type-f32+ #x7d)   ; -0x03
(defconstant +type-f64+ #x7c)   ; -0x04
(defconstant +type-v128+ #x7b)  ; -0x05

;; Reference types (MVP)
(defconstant +type-funcref+ #x70)    ; -0x10
(defconstant +type-externref+ #x6f)  ; -0x11

;; Reference types (GC proposal)
(defconstant +type-anyref+ #x6e)     ; -0x12
(defconstant +type-eqref+ #x6d)      ; -0x13
(defconstant +type-i31ref+ #x6c)     ; -0x14
(defconstant +type-structref+ #x6b)  ; -0x15
(defconstant +type-arrayref+ #x6a)   ; -0x16
(defconstant +type-nullref+ #x69)    ; -0x17
(defconstant +type-nullfuncref+ #x68)
(defconstant +type-nullexternref+ #x67)
(defconstant +type-noneref+ #x66)

;; Pseudo types for encoding
(defconstant +type-func+ #x60)  ; Function type constructor
(defconstant +type-void+ #x40)  ; Empty block type

;;; Export kinds
(defconstant +export-func+ 0)
(defconstant +export-table+ 1)
(defconstant +export-memory+ 2)
(defconstant +export-global+ 3)

;;; Import kinds (same as export)
(defconstant +import-func+ 0)
(defconstant +import-table+ 1)
(defconstant +import-memory+ 2)
(defconstant +import-global+ 3)

;;; Mutability
(defconstant +const+ 0)
(defconstant +var+ 1)

;;; Limits
(defconstant +limits-min-only+ 0)
(defconstant +limits-min-max+ 1)

;;; Type Structures

(defstruct (func-type (:constructor make-func-type (params results)))
  "A WebAssembly function type."
  (params nil :type list)    ; List of value types
  (results nil :type list))  ; List of value types

(defstruct (wasm-func (:constructor make-wasm-func (type-idx locals body)))
  "A WebAssembly function."
  (type-idx 0 :type (unsigned-byte 32))   ; Index into type section
  (locals nil :type list)                  ; List of (count . type) pairs
  (body nil :type list))                   ; List of instructions

(defstruct (wasm-export (:constructor make-wasm-export (name kind idx)))
  "A WebAssembly export."
  (name "" :type string)
  (kind 0 :type (unsigned-byte 8))
  (idx 0 :type (unsigned-byte 32)))

(defstruct (wasm-import (:constructor make-wasm-import (module name kind desc)))
  "A WebAssembly import."
  (module "" :type string)
  (name "" :type string)
  (kind 0 :type (unsigned-byte 8))
  (desc nil))  ; Type index for functions, limits for memory/table, etc.

(defstruct (wasm-global (:constructor make-wasm-global (type mutable init)))
  "A WebAssembly global."
  (type 0 :type (unsigned-byte 8))
  (mutable nil :type boolean)
  (init nil :type list))  ; Initialization expression

(defstruct (wasm-memory (:constructor make-wasm-memory (min &optional max)))
  "A WebAssembly memory."
  (min 0 :type (unsigned-byte 32))
  (max nil :type (or null (unsigned-byte 32))))

(defstruct (wasm-table (:constructor make-wasm-table (element-type min &optional max)))
  "A WebAssembly table."
  (element-type +type-funcref+ :type (unsigned-byte 8))
  (min 0 :type (unsigned-byte 32))
  (max nil :type (or null (unsigned-byte 32))))

(defstruct (wasm-element (:constructor make-wasm-element (table-idx offset func-indices)))
  "A WebAssembly element segment for populating tables."
  (table-idx 0 :type (unsigned-byte 32))
  (offset nil :type list)                    ; Init expression for offset
  (func-indices nil :type list))             ; List of function indices

;;; GC Type Definitions (for WasmGC)

(defstruct (gc-struct-type (:constructor make-gc-struct-type (fields)))
  "A WasmGC struct type definition."
  (fields nil :type list))  ; List of (type . mutable-p) pairs

(defstruct (gc-array-type (:constructor make-gc-array-type (element-type mutable)))
  "A WasmGC array type definition."
  (element-type 0 :type (unsigned-byte 8))
  (mutable nil :type boolean))

(defstruct (gc-field (:constructor make-gc-field (type &optional (mutable t))))
  "A WasmGC struct field."
  (type 0 :type t)  ; Can be value type or type index
  (mutable t :type boolean))
