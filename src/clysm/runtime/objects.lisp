;;;; objects.lisp - Runtime object representations

(in-package #:clysm/runtime/objects)

;;; Singleton sentinels
(defvar +nil+ :clysm-nil
  "The NIL singleton (distinct from Wasm null).")

(defvar +unbound+ :clysm-unbound
  "The UNBOUND sentinel for unbound symbols.")

(defun make-cons (car cdr)
  "Create a cons cell."
  (cons car cdr))

(defun car* (cons)
  "Get the CAR of a cons cell."
  (car cons))

(defun cdr* (cons)
  "Get the CDR of a cons cell."
  (cdr cons))

(defun make-symbol* (name)
  "Create a new symbol."
  (list :symbol name +unbound+ +unbound+ +nil+ +nil+))
