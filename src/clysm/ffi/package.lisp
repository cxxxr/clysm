;;;; package.lisp - Package definition for FFI module
;;;;
;;;; This file defines the FFI (Foreign Function Interface) package for Clysm.
;;;; FFI enables bidirectional interoperability between Lisp code and host
;;;; environments (JavaScript/wasmtime).

(in-package #:cl-user)

(defpackage #:clysm/ffi
  (:use #:cl)
  (:export
   ;; Macros for FFI declaration
   #:define-foreign-function
   #:export-function

   ;; Runtime functions
   #:call-host

   ;; Conditions
   #:ffi-host-error
   #:ffi-host-error-function-name
   #:ffi-host-error-message
   #:ffi-type-error
   #:ffi-type-error-expected-type
   #:ffi-type-error-actual-value

   ;; Types (for documentation and extension)
   #:marshal-type
   #:foreign-function-decl
   #:export-decl
   #:ffi-environment

   ;; FFI declaration structure accessors
   #:ffd-lisp-name
   #:ffd-module-name
   #:ffd-field-name
   #:ffd-param-types
   #:ffd-return-type
   #:ffd-type-index
   #:ffd-func-index      ; 001-numeric-functions: function index for :call
   #:make-foreign-function-decl

   ;; Export declaration structure accessors
   #:ed-lisp-name
   #:ed-export-name
   #:ed-param-types
   #:ed-return-type
   #:ed-wrapper-func-index
   #:make-export-decl

   ;; WasmImport structure accessors
   #:wasm-import
   #:make-wasm-import
   #:wi-module-name
   #:wi-field-name
   #:wi-kind
   #:wi-type-index
   #:wi-table-type
   #:wi-memory-type
   #:wi-global-type

   ;; FFI environment accessors
   #:ffi-env-imports
   #:ffi-env-exports
   #:ffi-env-next-import-func-index
   #:ffi-env-type-cache
   #:make-ffi-environment

   ;; Type validation
   #:valid-marshal-type-p
   #:validate-foreign-function-decl
   #:validate-export-decl

   ;; Environment operations
   #:register-foreign-function
   #:register-export
   #:lookup-foreign-function

   ;; Global FFI environment
   #:*ffi-environment*
   #:reset-ffi-environment

   ;; Math imports (001-numeric-functions)
   #:ensure-math-imports

   ;; Host name parsing
   #:parse-host-name

   ;; Import generation (T022-T025)
   #:generate-import-call
   #:collect-ffi-imports
   #:emit-ffi-imports
   #:get-ffi-import-count
   #:assign-import-indices

   ;; Export generation (T033-T035)
   #:collect-ffi-exports
   #:emit-ffi-exports
   #:get-ffi-export-count
   #:assign-export-indices
   #:generate-export-wrapper

   ;; Type marshalling (T043-T052)
   #:marshal-to-wasm
   #:marshal-from-wasm
   #:marshal-type-to-wasm-type
   #:generate-marshal-instructions

   ;; Inline marshalling optimization (T070)
   #:marshal-to-wasm-inline
   #:marshal-from-wasm-inline
   #:should-inline-marshal-p
   #:marshal-instruction-count

   ;; Declaration collection (T056-T057)
   #:collect-ffi-declarations
   #:generate-type-for-ffi-signature
   #:collect-ffi-types

   ;; Error handling (T068-T069)
   #:generate-import-call-with-error-handling
   #:generate-ffi-try-catch-wrapper
   #:generate-host-error-signal
   #:ffi-error-handler-type

   ;; Dynamic call-host support (T047 - 027-complete-ffi)
   #:*call-host-dynamic-import-index*
   #:make-call-host-dynamic-import-decl
   #:register-call-host-dynamic-import
   #:get-call-host-dynamic-index
   #:ensure-call-host-dynamic-import))
