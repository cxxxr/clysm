;;;; macros.lisp - FFI declaration macros (T021, T032)
;;;;
;;;; Provides ffi:define-foreign-function and ffi:export-function macros.

(in-package #:clysm/ffi)

;;; ============================================================
;;; Global FFI Environment
;;; ============================================================

(defvar *ffi-environment* (make-ffi-environment)
  "Global FFI environment for tracking foreign function declarations at compile time.")

(defun reset-ffi-environment ()
  "Reset the global FFI environment. Useful for testing."
  (setf *ffi-environment* (make-ffi-environment)))

;;; ============================================================
;;; T021: ffi:define-foreign-function macro
;;; ============================================================

(defmacro define-foreign-function (lisp-name host-name param-types return-type)
  "Declare a host function for import.
   LISP-NAME: Symbol to bind the function to in Lisp
   HOST-NAME: String \"module.field\" naming the host function
   PARAM-TYPES: List of marshal types for parameters
   RETURN-TYPE: Marshal type for return value

   Example:
     (ffi:define-foreign-function host-log \"host.log\" (:string) :void)
     (ffi:define-foreign-function host-add \"host.add\" (:fixnum :fixnum) :fixnum)

   This macro:
   1. Registers the foreign function declaration in *ffi-environment*
   2. Defines a Lisp function that will be compiled to a Wasm call to the import"
  (multiple-value-bind (module-name field-name)
      (parse-host-name host-name)
    (let ((param-list (loop for i from 0 below (length param-types)
                            collect (intern (format nil "ARG~D" i)))))
      `(progn
         ;; Register the declaration at load time
         (eval-when (:compile-toplevel :load-toplevel :execute)
           (register-foreign-function
            *ffi-environment*
            (make-foreign-function-decl
             :lisp-name ',lisp-name
             :module-name ,module-name
             :field-name ,field-name
             :param-types ',param-types
             :return-type ',return-type)))
         ;; Define a stub function that marks this as an FFI call
         ;; The actual implementation will be handled by the compiler
         (defun ,lisp-name ,param-list
           (declare (ignorable ,@param-list))
           ;; At compile time, this will be replaced with actual Wasm import call
           ;; For now, signal that this is an unresolved FFI call
           (error 'ffi-host-error
                  :function-name ,host-name
                  :message "FFI function called before Wasm compilation"))
         ',lisp-name))))

;;; ============================================================
;;; T024: ffi:call-host function
;;; ============================================================

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
  (error 'ffi-host-error
         :function-name function-name
         :message (format nil "Dynamic FFI call to ~A is only available in compiled Wasm code. ~
                              Use ffi:define-foreign-function for static declarations."
                          function-name)))

;;; ============================================================
;;; T032: ffi:export-function macro
;;; ============================================================

(defmacro export-function (lisp-name &key (as nil) (signature nil))
  "Export a Lisp function for host invocation.
   LISP-NAME: Symbol of the function to export
   AS: String name for export (defaults to lisp-name's string form)
   SIGNATURE: ((param-types...) return-type) for the export

   Example:
     (ffi:export-function my-add :as \"add\" :signature ((:fixnum :fixnum) :fixnum))
     (ffi:export-function my-log :signature ((:string) :void))

   This macro:
   1. Registers the export declaration in *ffi-environment*
   2. The compiler will generate a wrapper function that:
      - Unmarshals arguments from Wasm types
      - Calls the Lisp function
      - Marshals the return value back to Wasm type"
  (let ((export-name (or as (string-downcase (symbol-name lisp-name))))
        (param-types (first signature))
        (return-type (second signature)))
    `(progn
       ;; Register the declaration at load time
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (register-export
          *ffi-environment*
          (make-export-decl
           :lisp-name ',lisp-name
           :export-name ,export-name
           :param-types ',param-types
           :return-type ',return-type)))
       ',lisp-name)))
