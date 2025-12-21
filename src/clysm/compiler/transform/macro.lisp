;;;; macro.lisp - Macro expansion

(in-package #:clysm/compiler/transform/macro)

(defvar *macro-table* (make-hash-table :test 'eq)
  "Table of registered macros.")

(defun register-macro (name expander)
  "Register a macro function."
  (setf (gethash name *macro-table*) expander))

(defun find-macro (name)
  "Find a macro by name."
  (gethash name *macro-table*))

(defun macroexpand-all (form)
  "Recursively expand all macros in form."
  ;; TODO: Implement
  form)
