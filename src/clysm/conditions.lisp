;;;; conditions.lisp - Clysm Error Conditions
;;;;
;;;; Hierarchical condition system for compiler errors

(in-package #:clysm)

;;; ============================================================
;;; Base Condition
;;; ============================================================

(define-condition clysm-error (error)
  ((message :initarg :message
            :initform nil
            :reader clysm-error-message)
   (context :initarg :context
            :initform nil
            :reader clysm-error-context))
  (:report (lambda (condition stream)
             (format stream "Clysm error: ~A"
                     (or (clysm-error-message condition)
                         "Unknown error"))
             (when (clysm-error-context condition)
               (format stream "~%Context: ~S"
                       (clysm-error-context condition)))))
  (:documentation "Base condition for all Clysm errors."))

;;; ============================================================
;;; Compile-time Errors
;;; ============================================================

(define-condition clysm-compile-error (clysm-error)
  ((form :initarg :form
         :initform nil
         :reader compile-error-form)
   (phase :initarg :phase
          :initform nil
          :reader compile-error-phase))
  (:report (lambda (condition stream)
             (format stream "Clysm compile error")
             (when (compile-error-phase condition)
               (format stream " in ~A" (compile-error-phase condition)))
             (format stream ": ~A"
                     (or (clysm-error-message condition)
                         "Unknown compile error"))
             (when (compile-error-form condition)
               (format stream "~%Form: ~S"
                       (compile-error-form condition)))))
  (:documentation "Condition signaled during compilation."))

(define-condition clysm-syntax-error (clysm-compile-error)
  ((line :initarg :line
         :initform nil
         :reader syntax-error-line)
   (column :initarg :column
           :initform nil
           :reader syntax-error-column))
  (:report (lambda (condition stream)
             (format stream "Syntax error")
             (when (syntax-error-line condition)
               (format stream " at line ~D" (syntax-error-line condition))
               (when (syntax-error-column condition)
                 (format stream ", column ~D" (syntax-error-column condition))))
             (format stream ": ~A"
                     (or (clysm-error-message condition)
                         "Invalid syntax"))))
  (:documentation "Condition for syntax/parsing errors."))

(define-condition clysm-type-error (clysm-compile-error)
  ((expected :initarg :expected
             :initform nil
             :reader type-error-expected)
   (actual :initarg :actual
           :initform nil
           :reader type-error-actual))
  (:report (lambda (condition stream)
             (format stream "Type error: ~A"
                     (or (clysm-error-message condition)
                         "Type mismatch"))
             (when (type-error-expected condition)
               (format stream "~%Expected: ~S" (type-error-expected condition)))
             (when (type-error-actual condition)
               (format stream "~%Actual: ~S" (type-error-actual condition)))))
  (:documentation "Condition for type-related compile errors."))

(define-condition clysm-undefined-error (clysm-compile-error)
  ((name :initarg :name
         :initform nil
         :reader undefined-error-name)
   (kind :initarg :kind
         :initform :variable
         :reader undefined-error-kind))
  (:report (lambda (condition stream)
             (format stream "Undefined ~A: ~S"
                     (undefined-error-kind condition)
                     (undefined-error-name condition))))
  (:documentation "Condition for undefined variable/function references."))

;;; ============================================================
;;; Runtime Errors
;;; ============================================================

(define-condition clysm-runtime-error (clysm-error)
  ()
  (:documentation "Condition signaled during runtime."))

;;; ============================================================
;;; Wasm Backend Errors
;;; ============================================================

(define-condition clysm-wasm-error (clysm-error)
  ((opcode :initarg :opcode
           :initform nil
           :reader wasm-error-opcode))
  (:report (lambda (condition stream)
             (format stream "Wasm error: ~A"
                     (or (clysm-error-message condition)
                         "Unknown Wasm error"))
             (when (wasm-error-opcode condition)
               (format stream "~%Opcode: ~S"
                       (wasm-error-opcode condition)))))
  (:documentation "Condition for WebAssembly generation errors."))

;;; ============================================================
;;; Error Signaling Helpers
;;; ============================================================

(defun compile-error (message &key form phase context)
  "Signal a compile error with MESSAGE."
  (error 'clysm-compile-error
         :message message
         :form form
         :phase phase
         :context context))

(defun syntax-error (message &key line column form)
  "Signal a syntax error with MESSAGE."
  (error 'clysm-syntax-error
         :message message
         :line line
         :column column
         :form form))

(defun type-error* (message &key expected actual form)
  "Signal a type error with MESSAGE."
  (error 'clysm-type-error
         :message message
         :expected expected
         :actual actual
         :form form))

(defun undefined-error (name &key (kind :variable))
  "Signal an undefined reference error."
  (error 'clysm-undefined-error
         :name name
         :kind kind))

(defun wasm-error (message &key opcode context)
  "Signal a Wasm generation error."
  (error 'clysm-wasm-error
         :message message
         :opcode opcode
         :context context))
