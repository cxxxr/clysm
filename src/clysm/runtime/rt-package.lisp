;;;; rt-package.lisp - Runtime Library Package Definition
;;;; Feature 001-runtime-library-system: Layer 2 Runtime Library
;;;;
;;;; This package defines the runtime library infrastructure for Lisp-implemented
;;;; standard library functions that compile to WasmGC using the Clysm compiler.

(in-package #:cl-user)

(defpackage #:clysm/runtime-library
  (:use #:cl)
  (:nicknames #:clysm/rtl)
  (:documentation
   "Clysm Runtime Library - Layer 2 Functions

Layer 2 functions are defined in Lisp source and compiled using the same
compiler as user code. They build on Layer 1 primitives (cons, car, cdr, etc.)
to implement higher-level standard library functions.

This package provides:
- Runtime function structures for compilation
- Runtime module loading and dependency analysis
- Integration with the primitives registry")

  ;; Runtime function structure
  (:export #:runtime-function
           #:make-runtime-function
           #:runtime-function-p
           #:runtime-function-name
           #:runtime-function-lambda-list
           #:runtime-function-body
           #:runtime-function-source-file
           #:runtime-function-compiled-index
           #:runtime-function-dependencies)

  ;; Runtime module structure
  (:export #:runtime-module
           #:make-runtime-module
           #:runtime-module-p
           #:runtime-module-name
           #:runtime-module-source-file
           #:runtime-module-functions
           #:runtime-module-exports)

  ;; Loading and compilation
  (:export #:load-runtime-source
           #:analyze-dependencies
           #:topological-sort
           #:compile-runtime-function
           #:compile-runtime-module
           #:merge-runtime-module
           #:compile-runtime-source-file
           #:list-runtime-functions
           #:get-runtime-function)

  ;; Conditions
  (:export #:undefined-primitive-error
           #:undefined-primitive-error-name
           #:circular-dependency-error
           #:circular-dependency-error-functions))

(in-package #:clysm/runtime-library)

;;; ============================================================================
;;; Runtime Function Structure (T029)
;;; ============================================================================

(defstruct (runtime-function (:constructor make-runtime-function)
                             (:copier nil))
  "A Layer 2 runtime library function defined in Lisp source.

Slots:
  NAME           - Symbol naming the function (e.g., ASSOC, MEMBER)
  LAMBDA-LIST    - Parameter list for the function
  BODY           - List of forms comprising the function body
  SOURCE-FILE    - Pathname of the source file containing this function
  COMPILED-INDEX - Function index in Wasm module after compilation (nil until compiled)
  DEPENDENCIES   - List of function names this function calls (excluding primitives)"
  (name          nil :type symbol :read-only t)
  (lambda-list   nil :type list)
  (body          nil :type list)
  (source-file   nil :type (or pathname string null))
  (compiled-index nil :type (or fixnum null))
  (dependencies  nil :type list))

;;; ============================================================================
;;; Runtime Module Structure (T030)
;;; ============================================================================

(defstruct (runtime-module (:constructor make-runtime-module)
                           (:copier nil))
  "A collection of runtime functions from a single source file.

Slots:
  NAME        - Symbol naming the module (e.g., LIST-OPS, IO)
  SOURCE-FILE - Pathname of the source file
  FUNCTIONS   - List of runtime-function structs
  EXPORTS     - List of function names to export"
  (name        nil :type symbol :read-only t)
  (source-file nil :type (or pathname string null))
  (functions   nil :type list)
  (exports     nil :type list))
