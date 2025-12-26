;;;; ffi-dispatch.lisp - Dynamic FFI dispatch infrastructure
;;;; Feature: 027-complete-ffi (T046)
;;;;
;;;; This module provides the runtime infrastructure for dynamic host
;;;; function invocation via ffi:call-host. It defines the Wasm import
;;;; declarations and argument packing utilities.

(in-package #:clysm/ffi)

;;; ============================================================
;;; Dynamic Dispatch Import Index
;;; ============================================================

;; Reserved import index for the dynamic dispatch function.
;; This is a well-known import that must be provided by the host.
(defparameter *call-host-dynamic-import-index* nil
  "The function index for $ffi_call_host_dynamic import.
   Set during module compilation when the import is added.")

;;; ============================================================
;;; Dynamic Dispatch Import Declaration
;;; ============================================================

(defun make-call-host-dynamic-import ()
  "Create the import declaration for $ffi_call_host_dynamic.

   The import signature is:
     (func $ffi_call_host_dynamic
       (param externref)   ; function name string
       (param externref)   ; argument array
       (result externref)) ; result value

   Returns an import declaration suitable for the import section."
  `(:import
    (:module "ffi")
    (:field "call_host_dynamic")
    (:func
     (:type (:param :externref :externref)
            (:result :externref)))))

(defun get-call-host-dynamic-type-signature ()
  "Return the type signature for the dynamic dispatch import.
   Used for type section generation."
  '(:func (:param :externref :externref) (:result :externref)))

;;; ============================================================
;;; Argument Array Generation
;;; ============================================================

(defun generate-args-array-creation (num-args env)
  "Generate Wasm instructions to create an argument array.

   NUM-ARGS: Number of arguments (already on the stack as anyref values)
   ENV: Compilation environment

   Generates instructions to create a fixed-size array of anyref values.
   Arguments must already be compiled and on the stack.

   Returns a list of Wasm instructions."
  (declare (ignore env))
  (if (zerop num-args)
      ;; Empty array
      `(((:array.new_fixed ,clysm/compiler/codegen/gc-types:+type-anyref-array+ 0)))
      ;; Create array with arguments already on stack
      `(((:array.new_fixed ,clysm/compiler/codegen/gc-types:+type-anyref-array+ ,num-args)))))

;;; ============================================================
;;; Dynamic Dispatch Call Generation
;;; ============================================================

(defun generate-call-host-dynamic-invocation (func-name-instrs args-instrs call-host-func-index)
  "Generate Wasm instructions for a dynamic host function call.

   FUNC-NAME-INSTRS: Instructions that put the function name string on stack
   ARGS-INSTRS: List of instruction lists, one per argument
   CALL-HOST-FUNC-INDEX: The function index of $ffi_call_host_dynamic

   Returns instructions that:
   1. Push the function name (convert to externref)
   2. Push each argument and create an array
   3. Call $ffi_call_host_dynamic
   4. Convert result from externref to anyref"
  (let ((num-args (length args-instrs))
        (result '()))
    ;; Step 1: Compile function name and convert to externref
    (dolist (instr func-name-instrs)
      (push instr result))
    ;; Convert string to externref for host
    (push '(:extern.convert_any) result)

    ;; Step 2: Compile arguments
    (if (zerop num-args)
        ;; No args - push null externref for empty array
        (push '(:ref.null :extern) result)
        (progn
          ;; Push each argument
          (dolist (arg-instrs args-instrs)
            (dolist (instr arg-instrs)
              (push instr result)))
          ;; Create anyref array from stack values
          (push `(:array.new_fixed ,clysm/compiler/codegen/gc-types:+type-anyref-array+ ,num-args) result)
          ;; Convert to externref for host
          (push '(:extern.convert_any) result)))

    ;; Step 3: Call the dynamic dispatch function
    (push `(:call ,call-host-func-index) result)

    ;; Step 4: Convert result from externref to anyref
    (push '(:any.convert_extern) result)

    (nreverse result)))

;;; ============================================================
;;; Error Handling for Unknown Functions
;;; ============================================================

(defun generate-unknown-function-error-check ()
  "Generate instructions to check for unknown function error.

   The host's call_host_dynamic should either return a valid result
   or throw an exception (caught by error handling wrapper).

   This function returns nil as error handling is done via
   try_table/catch_all at the call site."
  nil)
