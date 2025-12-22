;;;; mop.lisp - Metaobject Protocol (Phase 10 - T215-T220)

(in-package #:clysm/clos/mop)

;;; ============================================================
;;; Class Registry
;;; ============================================================

(defvar *class-registry* (make-hash-table :test 'eq)
  "Registry of defined classes.")

(defun register-class (name class)
  "Register a class in the registry."
  (setf (gethash name *class-registry*) class))

(defun find-class* (name &optional (errorp t))
  "Find a class by name."
  (or (gethash name *class-registry*)
      (when errorp
        (error "Class not found: ~S" name))))

;;; ============================================================
;;; Slot Definition (T215)
;;; ============================================================

(defstruct slot-definition
  "A slot definition in a class."
  (name nil :type symbol)
  (initarg nil :type (or null keyword))
  (initform nil)
  (initform-p nil :type boolean)
  (accessor nil :type (or null symbol))
  (reader nil :type (or null symbol))
  (writer nil :type (or null symbol)))

;;; ============================================================
;;; Standard Class (T215-T217)
;;; ============================================================

(defstruct (standard-class (:conc-name class-)
                           (:print-function print-standard-class))
  "Standard class metaobject."
  (name nil :type symbol)
  (superclasses nil :type list)
  (slots nil :type list)
  (precedence-list nil :type list)
  (slot-index-table (make-hash-table :test 'eq) :type hash-table))

(defun print-standard-class (class stream depth)
  "Print a standard-class avoiding circular reference issues."
  (declare (ignore depth))
  (print-unreadable-object (class stream :type t :identity t)
    (format stream "~S" (class-name class))))

;;; ============================================================
;;; Instance Structure (T217)
;;; ============================================================

(defstruct (standard-instance (:conc-name instance-))
  "An instance of a class."
  (class nil :type (or null standard-class))
  (slots nil :type (or null simple-vector)))

(defun instance-p (obj)
  "Check if OBJ is a standard instance."
  (standard-instance-p obj))

(defun make-instance-struct (class num-slots)
  "Create a new instance structure."
  (make-standard-instance
   :class class
   :slots (make-array num-slots :initial-element nil)))

;;; ============================================================
;;; Class Precedence List (T220)
;;; ============================================================

(defun compute-class-precedence-list (class)
  "Compute and cache the class precedence list for CLASS.
   Uses a simple topological sort algorithm."
  (unless (class-precedence-list class)
    (setf (class-precedence-list class)
          (compute-cpl-internal class)))
  (class-precedence-list class))

(defun compute-cpl-internal (class)
  "Internal function to compute CPL."
  ;; Simple depth-first traversal, class first then superclasses
  (let ((result (list class)))
    (dolist (super (class-superclasses class))
      (let ((super-cpl (compute-class-precedence-list super)))
        (dolist (c super-cpl)
          (unless (member c result :test #'eq)
            (setf result (append result (list c)))))))
    result))

;;; ============================================================
;;; Slot Index Management
;;; ============================================================

(defun compute-slot-indices (class)
  "Compute slot index table for a class."
  (let ((table (class-slot-index-table class))
        (index 0))
    (clrhash table)
    ;; First add slots from superclasses
    (dolist (super (class-superclasses class))
      (compute-slot-indices super)
      (maphash (lambda (name idx)
                 (declare (ignore idx))
                 (unless (gethash name table)
                   (setf (gethash name table) index)
                   (incf index)))
               (class-slot-index-table super)))
    ;; Then add own slots
    (dolist (slot (class-slots class))
      (let ((name (slot-definition-name slot)))
        (unless (gethash name table)
          (setf (gethash name table) index)
          (incf index))))
    index))

(defun slot-index (class slot-name)
  "Get the index of a slot in instances of CLASS."
  (gethash slot-name (class-slot-index-table class)))

(defun total-slot-count (class)
  "Get total number of slots for instances of CLASS."
  (hash-table-count (class-slot-index-table class)))

;;; ============================================================
;;; Class Finalization
;;; ============================================================

(defun finalize-class (class)
  "Finalize a class, computing all derived data."
  (compute-class-precedence-list class)
  (compute-slot-indices class)
  class)

;;; ============================================================
;;; Slot Access (Low-level)
;;; ============================================================

(defun %slot-value (instance slot-name)
  "Low-level slot access."
  (let* ((class (instance-class instance))
         (index (slot-index class slot-name)))
    (if index
        (aref (instance-slots instance) index)
        (error "Slot ~S not found in instance of ~S"
               slot-name (class-name class)))))

(defun (setf %slot-value) (value instance slot-name)
  "Low-level slot setter."
  (let* ((class (instance-class instance))
         (index (slot-index class slot-name)))
    (if index
        (setf (aref (instance-slots instance) index) value)
        (error "Slot ~S not found in instance of ~S"
               slot-name (class-name class)))))

;;; ============================================================
;;; Class-of
;;; ============================================================

(defun class-of* (object)
  "Get the class of an object."
  (if (standard-instance-p object)
      (instance-class object)
      ;; For built-in types, return nil for now
      nil))

