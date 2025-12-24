;;;; ffi-api.lisp - FFI API Contract Definition
;;;; This file defines the public API contract for the FFI module.
;;;; Implementation must conform to these signatures and behaviors.

(in-package #:cl-user)

;;; ============================================================
;;; Package Definition Contract
;;; ============================================================

;; The FFI package must export the following symbols:
;;
;; Macros:
;;   DEFINE-FOREIGN-FUNCTION - Declare an imported host function
;;   EXPORT-FUNCTION         - Export a Lisp function to host
;;
;; Functions:
;;   CALL-HOST              - Dynamic host function invocation
;;
;; Types:
;;   MARSHAL-TYPE           - Type specifier for marshallable types
;;   FOREIGN-FUNCTION-DECL  - Structure for import declarations
;;   EXPORT-DECL            - Structure for export declarations
;;
;; Conditions:
;;   FFI-HOST-ERROR         - Condition for host call failures
;;   FFI-TYPE-ERROR         - Condition for marshalling failures

;;; ============================================================
;;; Macro Contracts
;;; ============================================================

#|
DEFINE-FOREIGN-FUNCTION lisp-name host-name (param-types...) return-type

Declare a host function import.

Arguments:
  lisp-name    - Symbol, the Lisp function name to create
  host-name    - String, "module.function" format
  param-types  - List of keywords: :fixnum :float :string :boolean :anyref
  return-type  - Keyword: :fixnum :float :string :boolean :anyref :void

Side Effects:
  - Registers the import in the FFI environment
  - Defines a Lisp function that calls the host function

Example:
  (ffi:define-foreign-function console-log "host.console.log" (:string) :void)
  (ffi:define-foreign-function random-int "math.randomInt" (:fixnum :fixnum) :fixnum)

Expansion (conceptual):
  (progn
    (register-foreign-function 'console-log "host" "console.log" '(:string) :void)
    (defun console-log (arg0)
      (%ffi-call "host.console.log" arg0)))
|#

#|
EXPORT-FUNCTION lisp-name &key as signature

Export a Lisp function for host invocation.

Arguments:
  lisp-name  - Quoted symbol, the Lisp function to export
  :as        - String, export name (defaults to Lisp name kebab-cased)
  :signature - List of (param-types...) return-type

Side Effects:
  - Registers the export in the FFI environment
  - Generates a wrapper function in the Wasm module

Example:
  (ffi:export-function 'calculate-tax :as "calculateTax" :signature ((:fixnum) :fixnum))
  (ffi:export-function 'process-data :signature ((:string :anyref) :string))

Notes:
  - The exported function must be defined before compilation
  - Wrapper handles type marshalling automatically
|#

;;; ============================================================
;;; Function Contracts
;;; ============================================================

#|
CALL-HOST host-name &rest args => result

Dynamically invoke a host function.

Arguments:
  host-name - String, "module.function" format
  args      - Arguments to pass (type-checked at runtime)

Returns:
  The host function's return value, marshalled to Lisp

Signals:
  FFI-HOST-ERROR if the host function fails
  FFI-TYPE-ERROR if argument types are incompatible

Example:
  (ffi:call-host "host.console.log" "Hello")
  (let ((n (ffi:call-host "math.random")))
    (format t "Random: ~A~%" n))

Notes:
  - Slower than static DEFINE-FOREIGN-FUNCTION due to runtime dispatch
  - Useful for optional/dynamic host features
|#

;;; ============================================================
;;; Type Contracts
;;; ============================================================

#|
MARSHAL-TYPE

Type specifier: (member :fixnum :float :string :boolean :anyref :void)

:fixnum  - 31-bit signed integer (i31ref range)
:float   - IEEE 754 double-precision float
:string  - UTF-8 string (WasmGC array)
:boolean - t or nil
:anyref  - Any Lisp object (passthrough)
:void    - No value (for return types only)
|#

;;; ============================================================
;;; Condition Contracts
;;; ============================================================

#|
FFI-HOST-ERROR

Condition signaled when a host function call fails.

Slots:
  function-name - String, the host function that failed
  message       - String, error description from host

Inheritance: ERROR

Example handler:
  (handler-case
      (ffi:call-host "host.file.read" "/nonexistent")
    (ffi:ffi-host-error (e)
      (format t "Host error in ~A: ~A~%"
              (ffi:ffi-error-function-name e)
              (ffi:ffi-error-message e))))
|#

#|
FFI-TYPE-ERROR

Condition signaled when type marshalling fails.

Slots:
  expected-type - Keyword, the expected marshal type
  actual-value  - The value that couldn't be marshalled
  direction     - :in or :out

Inheritance: TYPE-ERROR

Example handler:
  (handler-case
      (ffi:call-host "host.print" some-complex-object)
    (ffi:ffi-type-error (e)
      (format t "Cannot marshal ~A as ~A~%"
              (ffi:ffi-type-error-actual-value e)
              (ffi:ffi-type-error-expected-type e))))
|#

;;; ============================================================
;;; Behavioral Contracts
;;; ============================================================

#|
Import Resolution Order:
1. Static imports (DEFINE-FOREIGN-FUNCTION) are resolved at module instantiation
2. Missing imports cause instantiation failure (host must provide all)
3. Dynamic calls (CALL-HOST) verify import availability at runtime

Export Visibility:
1. All EXPORT-FUNCTION declarations appear in Wasm export section
2. Export names must be unique
3. Signature must match actual Lisp function's effective arity

Marshalling Invariants:
- Roundtrip: (marshal-in (marshal-out x type) type) ≡ x for supported types
- Fixnum range: [-2^30, 2^30 - 1] (i31ref limits)
- String encoding: Always UTF-8
- NIL marshals as boolean false (0), T as true (1)
- Unsupported types signal FFI-TYPE-ERROR

Error Propagation:
- Host exceptions are caught via Wasm EH
- Converted to FFI-HOST-ERROR condition
- Stack unwinding respects UNWIND-PROTECT
|#
