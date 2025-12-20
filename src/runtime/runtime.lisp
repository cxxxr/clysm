;;;; runtime.lisp - Runtime type definitions

(in-package #:clysm/runtime)

;;; Runtime Type Tags
;;; These are used for runtime type checking with WasmGC

(defconstant +tag-fixnum+ 0)
(defconstant +tag-cons+ 1)
(defconstant +tag-symbol+ 2)
(defconstant +tag-string+ 3)
(defconstant +tag-vector+ 4)
(defconstant +tag-closure+ 5)
(defconstant +tag-float+ 6)
(defconstant +tag-bignum+ 7)

;;; Symbol Layout (in linear memory)
;;; Offset 0: name-ptr (i32) - pointer to string
;;; Offset 4: value (i32) - symbol-value
;;; Offset 8: function (i32) - symbol-function
;;; Offset 12: plist (i32) - property list
;;; Total size: 16 bytes

(defconstant +symbol-size+ 16)
(defconstant +symbol-name-offset+ 0)
(defconstant +symbol-value-offset+ 4)
(defconstant +symbol-function-offset+ 8)
(defconstant +symbol-plist-offset+ 12)

;;; String Layout (in linear memory)
;;; Offset 0: length (i32) - byte length
;;; Offset 4: data (variable) - UTF-8 bytes
;;; Header size: 4 bytes

(defconstant +string-header-size+ 4)
(defconstant +string-length-offset+ 0)
(defconstant +string-data-offset+ 4)

;;; Value Representation Notes
;;;
;;; With WasmGC, we use the following representation:
;;;
;;; - Fixnums: i31ref (31-bit signed integers, no allocation)
;;; - Cons cells: struct { car: (ref eq), cdr: (ref eq) }
;;; - Symbols: struct { name, value, function, plist }
;;; - Strings: array i8 (UTF-8 encoded)
;;; - Vectors: array (ref eq)
;;; - Closures: struct { code: funcref, env: array (ref eq) }
;;; - Floats: struct { value: f64 }
;;; - Bignums: struct { sign: i32, digits: array i64 }
;;;
;;; The universal value type is (ref eq), which can hold:
;;; - i31ref for fixnums
;;; - references to any of the struct/array types above
;;;
;;; Type checking uses ref.test and ref.cast instructions.

;;; Special Values

(defparameter *nil-value* nil
  "The NIL value representation.")

(defparameter *t-value* t
  "The T value representation.")

;;; Future: Runtime function prototypes
;;; These will be implemented as WASM functions that get compiled into the module

;; (defun make-cons (car cdr) ...)
;; (defun car (cons) ...)
;; (defun cdr (cons) ...)
;; (defun make-symbol (name) ...)
;; (defun symbol-value (symbol) ...)
;; (defun make-closure (code env) ...)
;; (defun invoke-closure (closure args) ...)
