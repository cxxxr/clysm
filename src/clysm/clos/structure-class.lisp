;;; structure-class.lisp - Structure metaclass for Clysm CLOS
;;; Phase 13D-10: DEFSTRUCT Wasm Compilation
;;;
;;; Defines structure-class metaclass that extends standard-class
;;; to mark structure types for [type-of](resources/HyperSpec/Body/f_tp_of.htm)
;;; and [typep](resources/HyperSpec/Body/f_typep.htm) support.
;;;
;;; Reference: [defstruct](resources/HyperSpec/Body/m_defstr.htm)

(in-package #:clysm/clos/mop)

;;;; ============================================================
;;;; Structure Class Metaclass (T014)
;;;; ============================================================

;;; structure-class: Metaclass for structures created by defstruct
;;; Extends standard-class with:
;;;   - copier: symbol naming the copier function (or nil)
;;;   - predicate: symbol naming the predicate function (or nil)
;;;   - constructor: symbol naming the constructor function

(defstruct (structure-class (:include standard-class)
                            (:conc-name structure-class-)
                            (:print-function print-structure-class))
  "Structure class metaobject for [defstruct](resources/HyperSpec/Body/m_defstr.htm).
   Extends standard-class with structure-specific metadata."
  (copier nil :type (or null symbol))
  (predicate nil :type (or null symbol))
  (constructor nil :type symbol))

(defun print-structure-class (sc stream depth)
  "Print a structure-class avoiding circular reference issues."
  (declare (ignore depth))
  (print-unreadable-object (sc stream :type t :identity t)
    (format stream "~S" (class-name sc))))

;;;; ============================================================
;;;; Registry Integration (T015)
;;;; ============================================================

;;; Structure classes are registered in the same *class-registry* as
;;; regular CLOS classes. The structure-class-p predicate can be used
;;; to distinguish structure types from regular classes.

(defun structure-type-p (name)
  "Check if NAME is a registered structure type.
   Returns the structure-class if found, nil otherwise."
  (let ((class (find-class* name nil)))
    (when (structure-class-p class)
      class)))

(defun register-structure-class (name &key copier predicate constructor
                                        superclasses slots)
  "Register a new structure class in the class registry.
   NAME - symbol naming the structure type
   COPIER - symbol for copy function (nil = default copy-NAME)
   PREDICATE - symbol for type predicate (nil = default NAME-p)
   CONSTRUCTOR - symbol for constructor (default make-NAME)
   SUPERCLASSES - list of parent structure classes (from :include)
   SLOTS - list of slot-definition objects"
  (let ((sc (make-structure-class
             :name name
             :superclasses superclasses
             :slots slots
             :copier copier
             :predicate predicate
             :constructor constructor)))
    ;; Finalize the class to compute CPL and slot indices
    (finalize-class sc)
    ;; Register in the global class registry
    (register-class name sc)
    sc))

(defun get-structure-slots (name)
  "Get the slot definitions for structure NAME.
   Returns nil if NAME is not a registered structure type."
  (let ((sc (structure-type-p name)))
    (when sc
      (class-slots sc))))

(defun get-structure-parent (name)
  "Get the parent structure name for structure NAME.
   Returns nil if NAME has no parent or is not a structure type."
  (let ((sc (structure-type-p name)))
    (when (and sc (class-superclasses sc))
      (class-name (first (class-superclasses sc))))))
