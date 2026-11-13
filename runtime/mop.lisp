;;;; mop.lisp — the metaobject protocol (AMOP) as far as Clysm supports it,
;;;; and the package CLYSM-MOP that exports it (like SBCL's SB-MOP).
;;;;
;;;; Classes whose metaclass is one of the implementation's (STANDARD-CLASS,
;;;; FUNCALLABLE-STANDARD-CLASS, ...) are defined and accessed by the fast
;;;; internal code of class0.lisp/clos.lisp.  Classes of user-defined
;;;; metaclasses go through the generic functions of the protocol:
;;;; VALIDATE-SUPERCLASS, DIRECT-SLOT-DEFINITION-CLASS, COMPUTE-SLOTS,
;;;; COMPUTE-EFFECTIVE-SLOT-DEFINITION, EFFECTIVE-SLOT-DEFINITION-CLASS,
;;;; INITIALIZE-INSTANCE/REINITIALIZE-INSTANCE of the class metaobject, and
;;;; SLOT-VALUE-USING-CLASS & co. for their instances.  Generic functions of
;;;; user-defined classes call COMPUTE-APPLICABLE-METHODS-USING-CLASSES and
;;;; COMPUTE-APPLICABLE-METHODS.

;;; accessor methods
(defclass standard-accessor-method (standard-method)
  ((%slot-definition :initarg :slot-definition :initform nil :reader accessor-method-slot-definition)))
(defclass standard-reader-method (standard-accessor-method) ())
(defclass standard-writer-method (standard-accessor-method) ())

(defun reader-method-class (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-reader-method))
(defun writer-method-class (class direct-slot &rest initargs)
  (declare (ignore class direct-slot initargs))
  (find-class 'standard-writer-method))

(defmethod print-object ((s slot-definition) stream)
  (print-unreadable-object (s stream :type t :identity t)
    (prin1 (%slotd-name s) stream))
  s)

;;; subclasses
(defun add-direct-subclass (superclass subclass)
  (unless (member subclass (%class-slot superclass +class-subclasses+))
    (%set-class-slot superclass +class-subclasses+ (cons subclass (%class-slot superclass +class-subclasses+))))
  superclass)
(defun remove-direct-subclass (superclass subclass)
  (%set-class-slot superclass +class-subclasses+ (remove subclass (%class-slot superclass +class-subclasses+)))
  superclass)

(defun compute-default-initargs (class) (class-default-initargs class))

;;; specializers and their methods
(defvar *specializer-methods* (make-hash-table :test 'eq))

(defun add-direct-method (specializer method)
  (unless (consp specializer)
    (pushnew method (gethash specializer *specializer-methods*)))
  specializer)
(defun remove-direct-method (specializer method)
  (unless (consp specializer)
    (setf (gethash specializer *specializer-methods*)
          (remove method (gethash specializer *specializer-methods*))))
  specializer)
(defun specializer-direct-methods (specializer)
  (if (consp specializer) nil (gethash specializer *specializer-methods*)))
(defun specializer-direct-generic-functions (specializer)
  (remove-duplicates (remove nil (mapcar #'method-generic-function (specializer-direct-methods specializer)))))

(setq *add-method-hook*
      (lambda (method addp)
        (dolist (s (%method-specializers method))
          (if addp (add-direct-method s method) (remove-direct-method s method)))))

;;; dependents (no updates are sent)
(defvar *dependents* (make-hash-table :test 'eq))
(defun add-dependent (metaobject dependent)
  (pushnew dependent (gethash metaobject *dependents*)) metaobject)
(defun remove-dependent (metaobject dependent)
  (setf (gethash metaobject *dependents*) (remove dependent (gethash metaobject *dependents*))) metaobject)
(defun map-dependents (metaobject function)
  (dolist (d (gethash metaobject *dependents*)) (funcall function d)))
(defgeneric update-dependent (metaobject dependent &rest initargs))
(defmethod update-dependent ((metaobject t) (dependent t) &rest initargs)
  (declare (ignore initargs))
  nil)

;;; generic functions
(defun ensure-generic-function-using-class (generic-function name &rest keys)
  (declare (ignore generic-function))
  (apply #'ensure-generic-function name keys))

(defun find-method-combination (generic-function type options)
  (declare (ignore generic-function))
  (%find-method-combination (cons type options)))

(defun compute-discriminating-function (generic-function)
  (%gf-slot (%gf-info generic-function) +gf-function+))

(defun compute-effective-method (generic-function combination methods)
  "Returns a form that calls the effective method (with the arguments in the
variable %EMF-ARGS)."
  (declare (ignore combination))
  (let ((emf (%effective-method-function generic-function (%gf-info generic-function) methods)))
    (values `(funcall ',emf %emf-args) nil)))

(defun make-method-lambda (generic-function method lambda-expression environment)
  (declare (ignore generic-function method environment))
  (let ((args (gensym "ARGS")) (next (gensym "NEXT")))
    (values `(lambda (,args ,next)
               (declare (ignorable ,next))
               (flet ((call-next-method (&rest cnm-args)
                        (if ,next (funcall (car ,next) (or cnm-args ,args) (cdr ,next))
                            (error "No next method")))
                      (next-method-p () (not (null ,next))))
                 (apply ,lambda-expression ,args)))
            nil)))

;;; funcallable instances
(defvar *funcallable-instance-functions* (make-hash-table :test 'eq))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (let* ((info (%allocate-instance class))
         (cell (list (lambda (&rest args)
                       (declare (ignore args))
                       (error "The funcallable instance function of ~S is not set" info))))
         (f (lambda (&rest args) (apply (car cell) args))))
    (setf (gethash info *funcallable-instance-functions*) cell)
    (%set-closure-name f info)
    f))

(defun set-funcallable-instance-function (funcallable-instance function)
  (let* ((info (%closure-name funcallable-instance))
         (cell (gethash info *funcallable-instance-functions*)))
    (if cell
        (setf (car cell) function)
        (error "~S is not a funcallable instance whose function can be set" funcallable-instance))
    funcallable-instance))

;;; ------------------------------------------------------------
;;; the CLYSM-MOP package
;;; ------------------------------------------------------------

(defpackage "CLYSM-MOP"
  (:use)
  (:import-from "COMMON-LISP"
   "BUILT-IN-CLASS" "CLASS" "GENERIC-FUNCTION" "METHOD" "METHOD-COMBINATION" "STANDARD-CLASS"
   "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD" "STANDARD-OBJECT" "STRUCTURE-CLASS"
   "ENSURE-GENERIC-FUNCTION" "ALLOCATE-INSTANCE" "MAKE-INSTANCE" "ADD-METHOD" "REMOVE-METHOD"
   "FIND-METHOD" "COMPUTE-APPLICABLE-METHODS" "CLASS-NAME" "METHOD-QUALIFIERS" "SLOT-MISSING"
   "SLOT-UNBOUND")
  (:import-from "SYSTEM"
   "DIRECT-SLOT-DEFINITION" "EFFECTIVE-SLOT-DEFINITION" "EQL-SPECIALIZER" "FORWARD-REFERENCED-CLASS"
   "FUNCALLABLE-STANDARD-CLASS" "FUNCALLABLE-STANDARD-OBJECT" "METAOBJECT" "SLOT-DEFINITION"
   "SPECIALIZER" "STANDARD-ACCESSOR-METHOD" "STANDARD-DIRECT-SLOT-DEFINITION"
   "STANDARD-EFFECTIVE-SLOT-DEFINITION" "STANDARD-READER-METHOD" "STANDARD-SLOT-DEFINITION"
   "STANDARD-WRITER-METHOD"
   "ACCESSOR-METHOD-SLOT-DEFINITION" "ADD-DEPENDENT" "ADD-DIRECT-METHOD" "ADD-DIRECT-SUBCLASS"
   "CLASS-DEFAULT-INITARGS" "CLASS-DIRECT-DEFAULT-INITARGS" "CLASS-DIRECT-SLOTS"
   "CLASS-DIRECT-SUBCLASSES" "CLASS-DIRECT-SUPERCLASSES" "CLASS-FINALIZED-P" "CLASS-PRECEDENCE-LIST"
   "CLASS-PROTOTYPE" "CLASS-SLOTS" "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
   "COMPUTE-CLASS-PRECEDENCE-LIST" "COMPUTE-DEFAULT-INITARGS" "COMPUTE-DISCRIMINATING-FUNCTION"
   "COMPUTE-EFFECTIVE-METHOD" "COMPUTE-EFFECTIVE-SLOT-DEFINITION" "COMPUTE-SLOTS"
   "DIRECT-SLOT-DEFINITION-CLASS" "EFFECTIVE-SLOT-DEFINITION-CLASS" "ENSURE-CLASS"
   "ENSURE-CLASS-USING-CLASS" "ENSURE-GENERIC-FUNCTION-USING-CLASS" "EQL-SPECIALIZER-OBJECT"
   "FINALIZE-INHERITANCE" "FIND-METHOD-COMBINATION" "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
   "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER" "GENERIC-FUNCTION-DECLARATIONS"
   "GENERIC-FUNCTION-LAMBDA-LIST" "GENERIC-FUNCTION-METHOD-CLASS" "GENERIC-FUNCTION-METHOD-COMBINATION"
   "GENERIC-FUNCTION-METHODS" "GENERIC-FUNCTION-NAME" "INTERN-EQL-SPECIALIZER" "MAKE-METHOD-LAMBDA"
   "MAP-DEPENDENTS" "METHOD-FUNCTION" "METHOD-GENERIC-FUNCTION" "METHOD-LAMBDA-LIST"
   "METHOD-SPECIALIZERS" "READER-METHOD-CLASS" "REMOVE-DEPENDENT" "REMOVE-DIRECT-METHOD"
   "REMOVE-DIRECT-SUBCLASS" "SET-FUNCALLABLE-INSTANCE-FUNCTION" "SLOT-BOUNDP-USING-CLASS"
   "SLOT-DEFINITION-ALLOCATION" "SLOT-DEFINITION-INITARGS" "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION" "SLOT-DEFINITION-LOCATION" "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-READERS" "SLOT-DEFINITION-WRITERS" "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-DOCUMENTATION"
   "SLOT-MAKUNBOUND-USING-CLASS" "SLOT-VALUE-USING-CLASS" "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "SPECIALIZER-DIRECT-METHODS" "STANDARD-INSTANCE-ACCESS" "UPDATE-DEPENDENT" "VALIDATE-SUPERCLASS"
   "WRITER-METHOD-CLASS" "CLASSP")
  (:export
   "BUILT-IN-CLASS" "CLASS" "GENERIC-FUNCTION" "METHOD" "METHOD-COMBINATION" "STANDARD-CLASS"
   "STANDARD-GENERIC-FUNCTION" "STANDARD-METHOD" "STANDARD-OBJECT" "STRUCTURE-CLASS"
   "ENSURE-GENERIC-FUNCTION" "ALLOCATE-INSTANCE" "MAKE-INSTANCE" "ADD-METHOD" "REMOVE-METHOD"
   "FIND-METHOD" "COMPUTE-APPLICABLE-METHODS" "CLASS-NAME" "METHOD-QUALIFIERS" "SLOT-MISSING"
   "SLOT-UNBOUND"
   "DIRECT-SLOT-DEFINITION" "EFFECTIVE-SLOT-DEFINITION" "EQL-SPECIALIZER" "FORWARD-REFERENCED-CLASS"
   "FUNCALLABLE-STANDARD-CLASS" "FUNCALLABLE-STANDARD-OBJECT" "METAOBJECT" "SLOT-DEFINITION"
   "SPECIALIZER" "STANDARD-ACCESSOR-METHOD" "STANDARD-DIRECT-SLOT-DEFINITION"
   "STANDARD-EFFECTIVE-SLOT-DEFINITION" "STANDARD-READER-METHOD" "STANDARD-SLOT-DEFINITION"
   "STANDARD-WRITER-METHOD"
   "ACCESSOR-METHOD-SLOT-DEFINITION" "ADD-DEPENDENT" "ADD-DIRECT-METHOD" "ADD-DIRECT-SUBCLASS"
   "CLASS-DEFAULT-INITARGS" "CLASS-DIRECT-DEFAULT-INITARGS" "CLASS-DIRECT-SLOTS"
   "CLASS-DIRECT-SUBCLASSES" "CLASS-DIRECT-SUPERCLASSES" "CLASS-FINALIZED-P" "CLASS-PRECEDENCE-LIST"
   "CLASS-PROTOTYPE" "CLASS-SLOTS" "COMPUTE-APPLICABLE-METHODS-USING-CLASSES"
   "COMPUTE-CLASS-PRECEDENCE-LIST" "COMPUTE-DEFAULT-INITARGS" "COMPUTE-DISCRIMINATING-FUNCTION"
   "COMPUTE-EFFECTIVE-METHOD" "COMPUTE-EFFECTIVE-SLOT-DEFINITION" "COMPUTE-SLOTS"
   "DIRECT-SLOT-DEFINITION-CLASS" "EFFECTIVE-SLOT-DEFINITION-CLASS" "ENSURE-CLASS"
   "ENSURE-CLASS-USING-CLASS" "ENSURE-GENERIC-FUNCTION-USING-CLASS" "EQL-SPECIALIZER-OBJECT"
   "FINALIZE-INHERITANCE" "FIND-METHOD-COMBINATION" "FUNCALLABLE-STANDARD-INSTANCE-ACCESS"
   "GENERIC-FUNCTION-ARGUMENT-PRECEDENCE-ORDER" "GENERIC-FUNCTION-DECLARATIONS"
   "GENERIC-FUNCTION-LAMBDA-LIST" "GENERIC-FUNCTION-METHOD-CLASS" "GENERIC-FUNCTION-METHOD-COMBINATION"
   "GENERIC-FUNCTION-METHODS" "GENERIC-FUNCTION-NAME" "INTERN-EQL-SPECIALIZER" "MAKE-METHOD-LAMBDA"
   "MAP-DEPENDENTS" "METHOD-FUNCTION" "METHOD-GENERIC-FUNCTION" "METHOD-LAMBDA-LIST"
   "METHOD-SPECIALIZERS" "READER-METHOD-CLASS" "REMOVE-DEPENDENT" "REMOVE-DIRECT-METHOD"
   "REMOVE-DIRECT-SUBCLASS" "SET-FUNCALLABLE-INSTANCE-FUNCTION" "SLOT-BOUNDP-USING-CLASS"
   "SLOT-DEFINITION-ALLOCATION" "SLOT-DEFINITION-INITARGS" "SLOT-DEFINITION-INITFORM"
   "SLOT-DEFINITION-INITFUNCTION" "SLOT-DEFINITION-LOCATION" "SLOT-DEFINITION-NAME"
   "SLOT-DEFINITION-READERS" "SLOT-DEFINITION-WRITERS" "SLOT-DEFINITION-TYPE"
   "SLOT-DEFINITION-DOCUMENTATION"
   "SLOT-MAKUNBOUND-USING-CLASS" "SLOT-VALUE-USING-CLASS" "SPECIALIZER-DIRECT-GENERIC-FUNCTIONS"
   "SPECIALIZER-DIRECT-METHODS" "STANDARD-INSTANCE-ACCESS" "UPDATE-DEPENDENT" "VALIDATE-SUPERCLASS"
   "WRITER-METHOD-CLASS" "CLASSP"))
