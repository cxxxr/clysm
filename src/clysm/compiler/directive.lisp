;;;; directive.lisp - Compile-time directive processing (Phase 13D-3)
;;;; Handles in-package, defpackage, declaim, proclaim, defconstant at compile-time
;;;;
;;;; HyperSpec References:
;;;; - in-package: resources/HyperSpec/Body/m_in_pkg.htm
;;;; - defpackage: resources/HyperSpec/Body/m_defpkg.htm
;;;; - declaim: resources/HyperSpec/Body/m_declai.htm
;;;; - proclaim: resources/HyperSpec/Body/f_procla.htm
;;;; - defconstant: resources/HyperSpec/Body/m_defcon.htm

(in-package #:clysm/compiler)

;;; ============================================================
;;; Constant Bindings Registry (T009-T011)
;;; Feature: 001-type-package-export
;;; ============================================================

(defvar *constant-bindings* (make-hash-table :test 'eq)
  "Hash table mapping constant symbols to their values.
   Used for compile-time constant folding of DEFCONSTANT bindings.
   Key: symbol, Value: constant value (integer, string, etc.)")

(defun register-constant (symbol value)
  "Register a constant binding for compile-time folding.
   SYMBOL is the constant name, VALUE is its compile-time value.
   Per FR-003: Records constant bindings in compilation environment."
  (setf (gethash symbol *constant-bindings*) value)
  symbol)

(defun lookup-constant (symbol)
  "Look up a constant binding. Returns (VALUES value found-p).
   Per FR-004: Used for compile-time constant substitution."
  (gethash symbol *constant-bindings*))

;;; ============================================================
;;; Directive Detection
;;; ============================================================

(defun directive-form-p (form)
  "Return T if FORM is a compile-time directive that should not generate code.
   Directives are evaluated at compile-time and do not produce AST or Wasm output.

   Supported directives:
   - in-package: Changes the current package for subsequent forms
   - defpackage: Creates a new package at compile-time
   - declaim: File-scope declarations (optimize, type, special, etc.)
   - proclaim: Global declarations
   - defmacro: Macro definitions (expanded by host, see HyperSpec m_defmac.htm)
   - define-condition: Condition type definitions (Feature 001-m3-clos-primitives)
   - defconstant: Constant definitions (Feature 001-type-package-export)"
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(cl:in-package cl:defpackage cl:declaim cl:proclaim
                            cl:defmacro cl:define-condition cl:defconstant
                            in-package defpackage declaim proclaim defmacro
                            define-condition defconstant)
               :test #'eq)))

;;; ============================================================
;;; Compile-Time Directive Processing
;;; ============================================================

(defun compile-toplevel-form (form)
  "Process a toplevel form. Return nil for directives, parsed AST otherwise.

   If FORM is a directive (in-package, defpackage, declaim, proclaim):
   - Evaluates it in the host environment (SBCL)
   - Returns nil to indicate no AST/Wasm generation needed

   Otherwise:
   - Returns the form unchanged for normal AST parsing

   Per FR-005: Directive forms MUST NOT generate AST nodes or Wasm bytecode.
   Per FR-007: Directive evaluation MUST modify the compile-time environment."
  (if (directive-form-p form)
      (progn
        (eval-directive form)
        nil)
      form))

(defun eval-directive (form)
  "Evaluate a compile-time directive in the host environment.

   Signals a compile-time error with context if evaluation fails.
   Per FR-008: Errors MUST be signaled as compile-time errors with clear messages."
  (handler-case
      (eval form)
    (package-error (e)
      (error "Compile-time directive error in ~S:~%  Package error: ~A" form e))
    (error (e)
      (error "Compile-time directive error in ~S:~%  ~A" form e))))

;;; ============================================================
;;; DEFCONSTANT Handling (T012-T013)
;;; Feature: 001-type-package-export
;;; HyperSpec: resources/HyperSpec/Body/m_defcon.htm
;;; ============================================================

(defun defconstant-form-p (form)
  "Return T if FORM is a DEFCONSTANT form."
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(cl:defconstant defconstant) :test #'eq)))

(defun handle-defconstant (form env)
  "Handle a DEFCONSTANT form by registering the constant binding.
   FORM is (defconstant name value [doc-string])
   ENV is the compilation environment (currently unused).

   Per FR-003: Records constant name and value in *constant-bindings*.
   Per US4-AC2: Subsequent references substitute the value directly."
  (declare (ignore env))
  (destructuring-bind (defconstant-sym name value &optional doc-string) form
    (declare (ignore defconstant-sym doc-string))
    ;; Evaluate the value form at compile-time
    (let ((evaluated-value (eval value)))
      (register-constant name evaluated-value)
      ;; Also evaluate in host to make constant available
      (eval form)
      :skipped)))

(defun compile-directive (form env)
  "Compile a directive form. Returns :skipped for directives that don't generate code.
   FORM is the directive form.
   ENV is the compilation environment.

   Per FR-003/FR-004: DEFCONSTANT forms are registered and return :skipped.
   Other directives are evaluated and return :skipped."
  (cond
    ((defconstant-form-p form)
     (handle-defconstant form env))
    ((directive-form-p form)
     (eval-directive form)
     :skipped)
    (t
     ;; Not a directive - should not reach here
     nil)))

;;; ============================================================
;;; Pre-populate Type Constants (T021)
;;; Feature: 001-type-package-export
;;; ============================================================

(defun initialize-type-constants ()
  "Pre-populate *constant-bindings* with gc-types constants.
   Called at load time to enable constant folding for type indices.
   Per FR-001: All 28 type constants are registered."
  ;; Core types (0-7)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-nil+ 0)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-unbound+ 1)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-cons+ 2)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-symbol+ 3)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-string+ 4)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-closure+ 5)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-instance+ 6)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-standard-class+ 7)
  ;; Function types (8-12)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-func-0+ 8)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-func-1+ 9)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-func-2+ 10)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-func-3+ 11)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-func-n+ 12)
  ;; Binding frame (13)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-binding-frame+ 13)
  ;; Numeric tower (14-18)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-bignum+ 14)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-ratio+ 15)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-float+ 16)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-complex+ 17)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-limb-array+ 18)
  ;; Stream (19)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-stream+ 19)
  ;; Multiple values (20)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-mv-array+ 20)
  ;; CLOS types (21-24)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-slot-vector+ 21)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-keyword-array+ 22)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-closure-array+ 23)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-macro-environment+ 24)
  ;; Hash table types (25-27)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-hash-entry+ 25)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-hash-table+ 26)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-bucket-array+ 27)
  ;; Multidimensional array (28)
  (register-constant 'clysm/compiler/codegen/gc-types:+type-mdarray+ 28)
  t)

;; Initialize type constants at load time
(initialize-type-constants)
