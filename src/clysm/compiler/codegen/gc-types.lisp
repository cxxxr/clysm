;;;; gc-types.lisp - WasmGC type definitions

(in-package #:clysm/compiler/codegen/gc-types)

;;; Type indices for standard Clysm types
(defconstant +type-nil+ 0)
(defconstant +type-unbound+ 1)
(defconstant +type-cons+ 2)
(defconstant +type-symbol+ 3)
(defconstant +type-string+ 4)
(defconstant +type-closure+ 5)
(defconstant +type-instance+ 6)
(defconstant +type-standard-class+ 7)

(defun define-gc-types ()
  "Generate WasmGC type definitions for Clysm runtime."
  ;; TODO: Implement
  nil)
