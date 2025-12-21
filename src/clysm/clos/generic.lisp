;;;; generic.lisp - Generic functions

(in-package #:clysm/clos/generic)

(defstruct (generic-function (:conc-name gf-))
  "Generic function."
  (name nil :type symbol)
  (methods nil :type list)
  (lambda-list nil :type list))

(defun defgeneric* (name lambda-list &rest options)
  "Define a generic function."
  ;; TODO: Implement
  (declare (ignore name lambda-list options))
  nil)
