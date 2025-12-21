;;;; wasm-ir.lisp - Wasm Intermediate Representation

(in-package #:clysm/compiler/codegen/wasm-ir)

(defstruct (wasm-instr (:conc-name wasm-instr-))
  "Wasm instruction."
  (opcode nil :type keyword)
  (operands nil :type list))

(defstruct (wasm-type (:conc-name wasm-type-))
  "Wasm function type."
  (params nil :type list)
  (results nil :type list))

(defstruct (wasm-func (:conc-name wasm-func-))
  "Wasm function."
  (name nil :type symbol)
  (type-index nil :type (or null fixnum))
  (locals nil :type list)
  (body nil :type list))

(defstruct (wasm-global (:conc-name wasm-global-))
  "Wasm global variable."
  (name nil :type symbol)
  (type nil :type keyword)
  (mutable-p nil :type boolean)
  (init nil :type list))

(defstruct (wasm-module (:conc-name wasm-module-))
  "Wasm module."
  (types nil :type list)
  (imports nil :type list)
  (functions nil :type list)
  (tables nil :type list)
  (globals nil :type list)
  (exports nil :type list)
  (start nil :type (or null fixnum))
  (elements nil :type list)
  (code nil :type list)
  (tags nil :type list))
