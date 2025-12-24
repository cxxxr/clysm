;;;; Macro System API Contracts
;;;; These signatures define the public interface for macro operations

;;; ============================================================
;;; Macro Registry Operations
;;; ============================================================

;; Register a macro expander function
;; registry: macro-registry
;; name: symbol (the macro name)
;; expander: function (form -> form)
;; returns: expander
(defun register-macro (registry name expander) ...)

;; Get the macro function for a name
;; registry: macro-registry
;; name: symbol
;; returns: function | nil
(defun macro-function* (registry name) ...)

;; Check if a form is a macro call
;; registry: macro-registry
;; form: any
;; returns: boolean
(defun macro-form-p (registry form) ...)

;;; ============================================================
;;; Macro Expansion
;;; ============================================================

;; Expand form once if it's a macro call
;; registry: macro-registry
;; form: any
;; returns: expanded-form
;; signals: macro-expansion-error (on invalid macro)
(defun macroexpand-1* (registry form) ...)

;; Repeatedly expand form until stable
;; registry: macro-registry
;; form: any
;; returns: fully-expanded-form
;; signals: macro-expansion-depth-exceeded (after 1000 iterations)
(defun macroexpand* (registry form) ...)

;; Recursively expand all macros in form and subforms
;; registry: macro-registry
;; form: any
;; returns: fully-expanded-form (no macro calls remain)
(defun macroexpand-all (registry form) ...)

;;; ============================================================
;;; Backquote Operations
;;; ============================================================

;; Expand a quasiquote form
;; form: (quasiquote template)
;; returns: list/quote/append form
(defun expand-backquote (form) ...)

;;; ============================================================
;;; Defmacro Parsing
;;; ============================================================

;; Parse a defmacro form
;; form: (defmacro name lambda-list . body)
;; returns: defmacro-result struct
;; signals: parse-error (on invalid syntax)
(defun parse-defmacro (form) ...)

;; Parse a macro lambda list
;; lambda-list: list of parameters
;; returns: (values required optional rest rest-kind keys allow-other-keys)
;; signals: parse-error (on invalid lambda list)
(defun parse-lambda-list (lambda-list) ...)

;; Compile a parsed defmacro to an expander function
;; result: defmacro-result
;; returns: function (form -> expanded-form)
(defun compile-defmacro (result) ...)

;;; ============================================================
;;; Standard Macros (Expansion Signatures)
;;; ============================================================

;; (when test &body body) => (if test (progn . body) nil)
;; (unless test &body body) => (if test nil (progn . body))
;; (cond &rest clauses) => nested if forms
;; (case keyform &rest clauses) => nested if/eql forms
;; (and &rest forms) => short-circuit conjunction
;; (or &rest forms) => short-circuit disjunction
;; (prog1 first &body body) => (let ((#:g first)) body... #:g)
;; (prog2 first second &body body) => (progn first (prog1 second body...))
;; (dolist (var list &optional result) &body body) => tagbody loop
;; (dotimes (var count &optional result) &body body) => tagbody loop
;; (do vars end-test &body body) => tagbody loop with parallel bindings

;;; ============================================================
;;; Error Conditions
;;; ============================================================

;; Signaled when macro expansion depth exceeds limit
(define-condition macro-expansion-depth-exceeded (error)
  ((macro-name :initarg :macro-name :reader macro-name)
   (depth :initarg :depth :reader expansion-depth)))

;; Signaled when a macro call has wrong number of arguments
(define-condition macro-argument-error (error)
  ((macro-name :initarg :macro-name :reader macro-name)
   (expected :initarg :expected :reader expected-args)
   (actual :initarg :actual :reader actual-args)))

;; Signaled when referencing undefined macro
(define-condition undefined-macro-error (error)
  ((macro-name :initarg :macro-name :reader macro-name)))
