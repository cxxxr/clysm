;;; Toplevel forms that are calls of global functions with constant
;;; arguments (which the REPL image calls at once instead of compiling
;;; them, NATIVE=1): their values, their order, errors, and macros that
;;; expand to such calls.
(defvar *log* '())
(defun note (x) (push x *log*) x)
(defun two-values (a b) (values a b))
(defparameter *special* 10)
(defconstant +constant+ 32)
(defmacro register (name &rest options) `(apply 'note-all ',name ',options))
(defun note-all (&rest args) (note args))

(note 1)
(print (multiple-value-list (two-values 'a "b")))
(print (list (note (+ 1 2)) (note #\x) (note :key) (note *special*) (note +constant+)))
(print (funcall #'two-values 3 4))
(print (reverse *log*))
(register "system" :depends-on ("a" "b") :version "1.0")
(print (first *log*))
;; a function redefined is the new one
(defun note (x) (list :new x))
(print (note 5))
;; errors happen when the call is made
(print (handler-case (car '(1 . 2)) (error () :no-error)))
(print (handler-case (funcall 'no-such-function-at-all 1) (undefined-function () :undefined)))
(print (handler-case (+ 'a 1) (type-error () :type-error)))
;; a variable that is not bound, as an argument
(print (handler-case (list (symbol-value '*not-bound-here*)) (unbound-variable () :unbound)))
(print (values (floor 7 2)))
(print (multiple-value-list (floor 7 2)))
(print (string-upcase (format nil "~A-~A" 'x 3)))
(setf *special* 11)
(print (list *special* (symbol-value '*special*)))
;; packages made and chosen at toplevel
(defpackage :toplevel-calls-test (:use :cl) (:export #:exported))
(in-package :toplevel-calls-test)
(print (list (package-name *package*) (symbol-package 'exported)))
(in-package :cl-user)
(print (package-name *package*))
