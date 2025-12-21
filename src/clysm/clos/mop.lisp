;;;; mop.lisp - Metaobject Protocol

(in-package #:clysm/clos/mop)

(defstruct (standard-class (:conc-name class-))
  "Standard class metaobject."
  (name nil :type symbol)
  (superclasses nil :type list)
  (slots nil :type list)
  (precedence-list nil :type list))
