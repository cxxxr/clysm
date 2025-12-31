;;;; directive.lisp - Compile-time directive processing (Phase 13D-3)
;;;; Handles in-package, defpackage, declaim, proclaim at compile-time
;;;;
;;;; HyperSpec References:
;;;; - in-package: resources/HyperSpec/Body/m_in_pkg.htm
;;;; - defpackage: resources/HyperSpec/Body/m_defpkg.htm
;;;; - declaim: resources/HyperSpec/Body/m_declai.htm
;;;; - proclaim: resources/HyperSpec/Body/f_procla.htm

(in-package #:clysm/compiler)

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
   - defmacro: Macro definitions (expanded by host, see HyperSpec m_defmac.htm)"
  (and (consp form)
       (symbolp (car form))
       (member (car form) '(cl:in-package cl:defpackage cl:declaim cl:proclaim
                            cl:defmacro
                            in-package defpackage declaim proclaim defmacro)
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
