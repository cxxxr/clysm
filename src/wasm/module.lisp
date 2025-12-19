;;;; module.lisp - WebAssembly module structure

(in-package #:cl-wasm/wasm)

;;; WASM Module Structure

(defstruct (wasm-module (:constructor %make-wasm-module))
  "A WebAssembly module being constructed."
  (types nil :type list)      ; List of func-type or gc-*-type
  (imports nil :type list)    ; List of wasm-import
  (functions nil :type list)  ; List of wasm-func
  (tables nil :type list)     ; List of wasm-table
  (memories nil :type list)   ; List of wasm-memory
  (globals nil :type list)    ; List of wasm-global
  (exports nil :type list)    ; List of wasm-export
  (start nil :type (or null (unsigned-byte 32)))  ; Start function index
  (elements nil :type list)   ; Element segments
  (data nil :type list)       ; Data segments
  ;; Internal tracking
  (type-count 0 :type (unsigned-byte 32))
  (import-func-count 0 :type (unsigned-byte 32))
  (func-count 0 :type (unsigned-byte 32)))

(defun make-wasm-module ()
  "Create a new empty WASM module."
  (%make-wasm-module))

;;; Module Building API

(defun add-type (module type)
  "Add a type definition to the module. Returns the type index."
  (let ((idx (wasm-module-type-count module)))
    (push type (wasm-module-types module))
    (incf (wasm-module-type-count module))
    idx))

(defun add-func-type (module params results)
  "Add a function type to the module. Returns the type index."
  (add-type module (make-func-type params results)))

(defun add-import (module mod-name name kind desc)
  "Add an import to the module. Returns the import index."
  (let ((import (make-wasm-import mod-name name kind desc)))
    (push import (wasm-module-imports module))
    (when (= kind +import-func+)
      (incf (wasm-module-import-func-count module)))
    import))

(defun add-function (module type-idx locals body)
  "Add a function to the module. Returns the function index."
  (let ((func (make-wasm-func type-idx locals body))
        (idx (+ (wasm-module-import-func-count module)
                (wasm-module-func-count module))))
    (push func (wasm-module-functions module))
    (incf (wasm-module-func-count module))
    idx))

(defun add-memory (module min &optional max)
  "Add a memory to the module."
  (let ((mem (make-wasm-memory min max)))
    (push mem (wasm-module-memories module))
    mem))

(defun add-global (module type mutable init)
  "Add a global to the module."
  (let ((global (make-wasm-global type mutable init)))
    (push global (wasm-module-globals module))
    global))

(defun add-export (module name kind idx)
  "Add an export to the module."
  (let ((export (make-wasm-export name kind idx)))
    (push export (wasm-module-exports module))
    export))

(defun add-table (module element-type min &optional max)
  "Add a table to the module."
  (let ((table (make-wasm-table element-type min max)))
    (push table (wasm-module-tables module))
    table))

(defun set-start (module func-idx)
  "Set the start function for the module."
  (setf (wasm-module-start module) func-idx))

;;; Module Finalization

(defun finalize-module (module)
  "Finalize the module by reversing all accumulated lists."
  (setf (wasm-module-types module) (nreverse (wasm-module-types module)))
  (setf (wasm-module-imports module) (nreverse (wasm-module-imports module)))
  (setf (wasm-module-functions module) (nreverse (wasm-module-functions module)))
  (setf (wasm-module-tables module) (nreverse (wasm-module-tables module)))
  (setf (wasm-module-memories module) (nreverse (wasm-module-memories module)))
  (setf (wasm-module-globals module) (nreverse (wasm-module-globals module)))
  (setf (wasm-module-exports module) (nreverse (wasm-module-exports module)))
  module)

;;; Convenience Builders

(defun build-simple-module (forms)
  "Build a simple module from a list of top-level forms.
Each form is (type . data) where type is :func, :export, etc."
  (let ((module (make-wasm-module)))
    (dolist (form forms)
      (ecase (car form)
        (:type
         (apply #'add-func-type module (cdr form)))
        (:func
         (destructuring-bind (type-idx locals body) (cdr form)
           (add-function module type-idx locals body)))
        (:export
         (destructuring-bind (name kind idx) (cdr form)
           (add-export module name kind idx)))
        (:memory
         (apply #'add-memory module (cdr form)))
        (:global
         (destructuring-bind (type mutable init) (cdr form)
           (add-global module type mutable init)))))
    (finalize-module module)))
