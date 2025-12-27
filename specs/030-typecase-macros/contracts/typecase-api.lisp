;;;; typecase-api.lisp - Contract definitions for type dispatch macros
;;;; Feature: 030-typecase-macros

(in-package #:clysm/lib/macros)

;;; ============================================================
;;; Macro Signatures
;;; ============================================================

;;; typecase - Basic type dispatch
;;; Syntax: (typecase keyform {(type-specifier-or-list form*)}*)
;;; Returns: Value of executed clause body, or NIL if no match

;;; etypecase - Exhaustive type dispatch with error
;;; Syntax: (etypecase keyform {(type-specifier-or-list form*)}+)
;;; Returns: Value of executed clause body
;;; Signals: type-error if no match
;;; Note: otherwise/t clauses NOT allowed

;;; ctypecase - Correctable type dispatch
;;; Syntax: (ctypecase place {(type-specifier-or-list form*)}+)
;;; Returns: Value of executed clause body
;;; Signals: type-error with store-value restart if no match
;;; Note: otherwise/t clauses NOT allowed, place must be setf-able

;;; check-type - Type assertion
;;; Syntax: (check-type place typespec [type-string])
;;; Returns: NIL
;;; Signals: type-error with store-value restart if type check fails

;;; ============================================================
;;; Expander Factory Contracts
;;; ============================================================

;; (defun make-typecase-expander ()
;;   "Create a macro expander for TYPECASE.
;;    (typecase keyform (type form...) ... [(otherwise|t form...)])
;;    Expands to a let binding the keyform, then nested if/typep tests.
;;    Returns: A function (lambda (form) expanded-form)"
;;   ...)

;; (defun make-etypecase-expander ()
;;   "Create a macro expander for ETYPECASE.
;;    (etypecase keyform (type form...) ...)
;;    Like typecase but signals type-error when no clause matches.
;;    Returns: A function (lambda (form) expanded-form)"
;;   ...)

;; (defun make-ctypecase-expander ()
;;   "Create a macro expander for CTYPECASE.
;;    (ctypecase place (type form...) ...)
;;    Like etypecase but with store-value restart for correction.
;;    Returns: A function (lambda (form) expanded-form)"
;;   ...)

;; (defun make-check-type-expander ()
;;   "Create a macro expander for CHECK-TYPE.
;;    (check-type place typespec [type-string])
;;    Signals type-error with store-value restart if place is not of typespec.
;;    Returns: A function (lambda (form) expanded-form)"
;;   ...)

;;; ============================================================
;;; Type Specifier Support Contract
;;; ============================================================

;; (defun type-specifier-to-predicate (typespec value-sym)
;;   "Convert a type specifier to a predicate form.
;;    TYPESPEC: Type specifier (symbol or list)
;;    VALUE-SYM: Symbol to test
;;    Returns: Form that evaluates to T or NIL
;;
;;    Examples:
;;      (type-specifier-to-predicate 'integer 'x)
;;        => (integerp x)
;;      (type-specifier-to-predicate '(or integer symbol) 'x)
;;        => (or (integerp x) (symbolp x))
;;      (type-specifier-to-predicate '(member :a :b) 'x)
;;        => (or (eql x ':a) (eql x ':b))"
;;   ...)

;;; ============================================================
;;; Expected Type Construction Contract
;;; ============================================================

;; (defun construct-expected-type (clauses)
;;   "Construct expected-type from typecase clauses for type-error.
;;    CLAUSES: List of (type-specifier . body-forms)
;;    Returns: Type specifier (or type1 type2 ...)
;;
;;    Example:
;;      (construct-expected-type '((integer ...) (symbol ...) (cons ...)))
;;        => (or integer symbol cons)"
;;   ...)

;;; ============================================================
;;; Validation Contracts
;;; ============================================================

;; (defun validate-etypecase-clauses (clauses)
;;   "Validate that etypecase clauses do not contain otherwise/t.
;;    CLAUSES: List of clause forms
;;    Signals: error if otherwise or t clause found"
;;   ...)

;; (defun validate-ctypecase-clauses (clauses)
;;   "Validate that ctypecase clauses do not contain otherwise/t.
;;    Same as validate-etypecase-clauses."
;;   ...)

;;; ============================================================
;;; Registration Contract
;;; ============================================================

;; (defun install-typecase-macros (registry)
;;   "Install typecase-related macros into REGISTRY.
;;    Registers: typecase, etypecase, ctypecase, check-type"
;;   (clysm/compiler/transform/macro:register-macro
;;    registry 'typecase (make-typecase-expander))
;;   (clysm/compiler/transform/macro:register-macro
;;    registry 'etypecase (make-etypecase-expander))
;;   (clysm/compiler/transform/macro:register-macro
;;    registry 'ctypecase (make-ctypecase-expander))
;;   (clysm/compiler/transform/macro:register-macro
;;    registry 'check-type (make-check-type-expander))
;;   registry)

;;; ============================================================
;;; Integration with install-standard-macros
;;; ============================================================

;; Modify install-standard-macros to call install-typecase-macros:
;;
;; (defun install-standard-macros (registry)
;;   ...existing code...
;;   (install-typecase-macros registry)
;;   registry)

;;; ============================================================
;;; Error Condition Contracts
;;; ============================================================

;; The following conditions are used (from 014-condition-system):
;;
;; type-error
;;   :datum        - The value that failed the type check
;;   :expected-type - The type that was expected
;;
;; Accessors:
;;   (type-error-datum condition)
;;   (type-error-expected-type condition)

;;; ============================================================
;;; Restart Contracts
;;; ============================================================

;; store-value restart (from 014-condition-system):
;;   Established by ctypecase and check-type
;;   Takes one argument: the new value to store
;;   Invoked via (invoke-restart 'store-value new-value)
