;;;; ffi-runtime.lisp - Runtime support for FFI (T024)
;;;;
;;;; Provides ffi:call-host for dynamic host function invocation.
;;;; In the interpreter, this signals an error since host functions
;;;; are only available in compiled Wasm code.
;;;; The compiler transforms call-host calls into actual Wasm import calls.

(in-package #:cl-user)

(defpackage #:clysm/lib/ffi-runtime
  (:use #:cl)
  (:export #:call-host
           #:*dynamic-ffi-enabled*))

(in-package #:clysm/lib/ffi-runtime)

;;; ============================================================
;;; T024: Dynamic Host Function Invocation
;;; ============================================================

(defvar *dynamic-ffi-enabled* nil
  "When T, dynamic FFI calls are allowed (only meaningful in Wasm runtime).
   In the interpreter, this is always NIL and call-host signals an error.")

(defun call-host (function-name &rest args)
  "Dynamically invoke a host function by name.
   FUNCTION-NAME: String \"module.field\" naming the host function
   ARGS: Arguments to pass to the function

   In compiled Wasm code, this generates a call to the imported function.
   In the interpreter, this signals FFI-HOST-ERROR since host functions
   are not available outside the Wasm runtime.

   Example:
     (ffi:call-host \"host.log\" \"Hello from Lisp\")
     (ffi:call-host \"host.random\")
     (ffi:call-host \"host.add\" 1 2)"
  (declare (ignorable args))
  ;; In interpreter mode, we cannot call host functions
  ;; This function is only meaningful when compiled to Wasm
  (error 'clysm/ffi:ffi-host-error
         :function-name function-name
         :message (format nil "Dynamic FFI call to ~A is only available in compiled Wasm code. ~
                              Use ffi:define-foreign-function for static declarations."
                          function-name)))
