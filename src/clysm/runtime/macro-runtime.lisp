;;;; macro-runtime.lisp - Runtime macro expansion support
;;;; Feature 042: Advanced Defmacro

(in-package #:clysm/runtime/macro)

;;; ============================================================
;;; Runtime Macro Registry
;;; ============================================================

(defvar *runtime-macro-registry* (make-hash-table :test 'eq)
  "Runtime registry for macro functions in compiled Wasm code.
   Maps macro names (symbols) to expander functions.
   Note: This registry is separate from the compile-time registry
   and is intended for macros that need to be available at runtime.")

(defvar *runtime-macro-expansion-limit* 1000
  "Maximum expansion depth for runtime macroexpand.")

;;; ============================================================
;;; Registry Management
;;; ============================================================

(defun register-runtime-macro (name expander)
  "Register a macro expander for use at runtime.
   NAME should be a symbol.
   EXPANDER should be a function of (form &optional env) -> expanded-form."
  (setf (gethash name *runtime-macro-registry*) expander)
  name)

(defun clear-runtime-macros ()
  "Clear all runtime macro definitions."
  (clrhash *runtime-macro-registry*))

;;; ============================================================
;;; Runtime Macro Functions
;;; ============================================================

(defun runtime-macro-function (name &optional env)
  "Return the macro expander function for NAME at runtime.
   Returns NIL if NAME is not a macro.
   ENV is currently ignored (reserved for future use)."
  (declare (ignore env))
  (gethash name *runtime-macro-registry*))

(defun runtime-macroexpand-1 (form &optional env)
  "Expand FORM once if it's a macro call.
   Returns two values: (expanded-form expanded-p).
   This is the runtime version for use in compiled Wasm code."
  ;; Handle atoms and nil
  (when (or (null form) (atom form))
    (return-from runtime-macroexpand-1 (values form nil)))
  ;; Handle empty list
  (when (null (first form))
    (return-from runtime-macroexpand-1 (values form nil)))
  ;; Check for macro
  (let ((expander (runtime-macro-function (first form) env)))
    (if expander
        (values (funcall expander form env) t)
        (values form nil))))

(defun runtime-macroexpand (form &optional env)
  "Repeatedly expand FORM until it's not a macro call.
   Returns two values: (expanded-form expanded-p).
   This is the runtime version for use in compiled Wasm code."
  (loop with count = 0
        with ever-expanded = nil
        do (multiple-value-bind (expanded was-expanded)
               (runtime-macroexpand-1 form env)
             (unless was-expanded
               (return (values form ever-expanded)))
             (setf ever-expanded t)
             (incf count)
             (when (> count *runtime-macro-expansion-limit*)
               (error "Runtime macro expansion depth exceeded (~D iterations) for ~S"
                      count (when (consp form) (first form))))
             (setf form expanded))))

;;; ============================================================
;;; Wasm Global Index Constants
;;; ============================================================

;; These constants define the global indices for runtime macro support.
;; Used by the code generator to emit correct global references.

(defconstant +global-macro-registry+ 5
  "Global index for runtime macro registry (anyref).
   Note: Actual index may vary based on module structure.")

;;; ============================================================
;;; Notes on Runtime Macro Expansion
;;; ============================================================

;; Runtime macro expansion in Wasm requires:
;; 1. The macro registry exported as a Wasm global
;; 2. Expander functions stored as closures in the registry
;; 3. The ability to call expanders dynamically
;;
;; Current implementation provides the host-side infrastructure.
;; Full Wasm-native runtime expansion would require:
;; - Storing expander functions in a Wasm table
;; - Implementing lookup via hash table in Wasm
;; - Supporting funcall on arbitrary closures
;;
;; For most use cases, compile-time expansion is sufficient.
;; Runtime expansion is needed for:
;; - eval at runtime
;; - macroexpand-1/macroexpand as callable functions
;; - Debugging and development tools
