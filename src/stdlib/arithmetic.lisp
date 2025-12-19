;;;; arithmetic.lisp - Standard library arithmetic functions

(in-package #:cl-wasm/stdlib)

;;; Primitive Operations Registry

(defparameter *primitives*
  (make-hash-table :test 'eq)
  "Table of primitive operations.")

(defun primitive-p (name)
  "Check if NAME is a primitive operation."
  (gethash name *primitives*))

(defun get-primitive (name)
  "Get the primitive info for NAME."
  (gethash name *primitives*))

(defun register-primitive (name info)
  "Register a primitive operation."
  (setf (gethash name *primitives*) info))

;;; Primitive Info Structure

(defstruct primitive-info
  "Information about a primitive operation."
  (name nil :type symbol)
  (arity nil)                    ; nil for variadic
  (wasm-op nil)                  ; WASM opcode(s)
  (result-type nil))             ; Result type

;;; Register Arithmetic Primitives

(register-primitive '+
  (make-primitive-info :name '+ :arity nil :result-type :i32))

(register-primitive '-
  (make-primitive-info :name '- :arity nil :result-type :i32))

(register-primitive '*
  (make-primitive-info :name '* :arity nil :result-type :i32))

(register-primitive '/
  (make-primitive-info :name '/ :arity nil :result-type :i32))

(register-primitive 'mod
  (make-primitive-info :name 'mod :arity 2 :result-type :i32))

(register-primitive 'rem
  (make-primitive-info :name 'rem :arity 2 :result-type :i32))

;;; Register Comparison Primitives

(register-primitive '<
  (make-primitive-info :name '< :arity 2 :result-type :bool))

(register-primitive '>
  (make-primitive-info :name '> :arity 2 :result-type :bool))

(register-primitive '<=
  (make-primitive-info :name '<= :arity 2 :result-type :bool))

(register-primitive '>=
  (make-primitive-info :name '>= :arity 2 :result-type :bool))

(register-primitive '=
  (make-primitive-info :name '= :arity 2 :result-type :bool))

(register-primitive '/=
  (make-primitive-info :name '/= :arity 2 :result-type :bool))

;;; Register Boolean Primitives

(register-primitive 'not
  (make-primitive-info :name 'not :arity 1 :result-type :bool))

(register-primitive 'null
  (make-primitive-info :name 'null :arity 1 :result-type :bool))
