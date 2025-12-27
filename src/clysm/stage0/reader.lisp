;;;; reader.lisp - S-expression reader for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements T024: S-expression reader (cross-compiled)
;;;;
;;;; This reader is used to parse Lisp source files in Stage 0.
;;;; It is cross-compiled from SBCL to Wasm.

(in-package #:clysm/stage0)

;;; ============================================================
;;; S-expression Reader API
;;; ============================================================

;; Note: read-source-string is defined in ffi.lisp
;; It uses CL:READ for bootstrap, will be replaced with
;; Wasm-native reader in Stage 1.

(defun parse-forms (source-string)
  "Parse source string into list of S-expression forms.
   This is an alias for read-source-string."
  (read-source-string source-string))

;;; ============================================================
;;; Form Classification
;;; ============================================================

(defun top-level-form-p (form)
  "Check if form is a top-level definition"
  (and (consp form)
       (member (car form)
               '(defun defmacro defvar defparameter defconstant
                 defclass defmethod defgeneric
                 in-package))))

(defun defun-form-p (form)
  "Check if form is a function definition"
  (and (consp form) (eq 'defun (car form))))

(defun defmacro-form-p (form)
  "Check if form is a macro definition"
  (and (consp form) (eq 'defmacro (car form))))

(defun defvar-form-p (form)
  "Check if form is a variable definition"
  (and (consp form)
       (member (car form) '(defvar defparameter defconstant))))

(defun compilable-form-p (form)
  "Check if form can be compiled by Stage 0"
  (and (consp form)
       (member (car form)
               '(defun defmacro defvar defparameter defconstant
                 defclass defmethod defgeneric
                 lambda let let* if cond when unless
                 progn block return-from tagbody go
                 setq setf))))

;;; ============================================================
;;; Source Location Tracking
;;; ============================================================

(defstruct source-location
  "Tracks source location for error reporting"
  (file nil :type (or null string pathname))
  (line 0 :type integer)
  (column 0 :type integer))

(defvar *current-source-location* nil
  "Current source location during parsing")

(defun make-source-error-message (message)
  "Create error message with source location"
  (if *current-source-location*
      (format nil "~A at ~A:~D:~D"
              message
              (source-location-file *current-source-location*)
              (source-location-line *current-source-location*)
              (source-location-column *current-source-location*))
      message))
