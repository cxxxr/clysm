(defpackage :micros
  (:use :cl)
  (:export #:function-name))
(in-package :micros)

(defun function-name (function)
  "The name of FUNCTION, or NIL."
  (nth-value 2 (function-lambda-expression function)))

;;; lem-lisp-syntax: the indentation of macros comes from their lambda lists
;;; in SWANK; here the indentation table of lem-lisp-syntax is used alone.
(defun symbol-indentation (symbol)
  (declare (ignore symbol))
  nil)

;;; lem-lisp-syntax marks the cursor with this symbol when it parses a form
;;; for the argument list of its operator.
(defvar %cursor-marker% '%cursor-marker%)
