;;;; defclass.lisp - Class definition (Phase 10 - T221-T223)

(in-package #:clysm/clos/defclass)

;;; ============================================================
;;; Slot Specification
;;; ============================================================

(defstruct slot-spec
  "Parsed slot specification."
  (name nil :type symbol)
  (initarg nil :type (or null keyword))
  (initform nil)
  (initform-p nil :type boolean)
  (accessor nil :type (or null symbol))
  (reader nil :type (or null symbol))
  (writer nil :type (or null symbol)))

;;; ============================================================
;;; Defclass Result
;;; ============================================================

(defstruct defclass-result
  "Result of parsing a defclass form."
  (name nil :type symbol)
  (superclasses nil :type list)
  (slots nil :type list)
  (options nil :type list))

;;; ============================================================
;;; Slot Parsing (T222)
;;; ============================================================

(defun parse-slot-spec (slot-form)
  "Parse a slot specification form."
  (if (symbolp slot-form)
      ;; Simple slot name
      (make-slot-spec :name slot-form)
      ;; Slot with options
      (let ((name (first slot-form))
            (options (rest slot-form))
            (spec (make-slot-spec)))
        (setf (slot-spec-name spec) name)
        (loop while options do
          (let ((key (pop options))
                (value (pop options)))
            (case key
              (:initarg (setf (slot-spec-initarg spec) value))
              (:initform
               (setf (slot-spec-initform spec) value)
               (setf (slot-spec-initform-p spec) t))
              (:accessor (setf (slot-spec-accessor spec) value))
              (:reader (setf (slot-spec-reader spec) value))
              (:writer (setf (slot-spec-writer spec) value)))))
        spec)))

;;; ============================================================
;;; Defclass Parsing (T221)
;;; ============================================================

(defun parse-defclass (form)
  "Parse a defclass form.
   Form: (defclass name (superclasses...) (slot-specs...) options...)"
  (unless (and (consp form)
               (eq (first form) 'defclass))
    (error "Not a defclass form: ~S" form))
  (let ((name (second form))
        (superclasses (third form))
        (slot-specs (fourth form))
        (options (nthcdr 4 form)))
    (make-defclass-result
     :name name
     :superclasses superclasses
     :slots (mapcar #'parse-slot-spec slot-specs)
     :options options)))

;;; ============================================================
;;; Slot Definition Creation
;;; ============================================================

(defun slot-spec-to-slot-definition (spec)
  "Convert a slot-spec to a slot-definition."
  (clysm/clos/mop:make-slot-definition
   :name (slot-spec-name spec)
   :initarg (slot-spec-initarg spec)
   :initform (slot-spec-initform spec)
   :initform-p (slot-spec-initform-p spec)
   :accessor (slot-spec-accessor spec)
   :reader (slot-spec-reader spec)
   :writer (slot-spec-writer spec)))

;;; ============================================================
;;; Class Creation (T223)
;;; ============================================================

(defun define-class* (form)
  "Define a class from a defclass form."
  (let* ((result (parse-defclass form))
         (name (defclass-result-name result))
         (superclass-names (defclass-result-superclasses result))
         (slot-specs (defclass-result-slots result)))
    ;; Resolve superclasses
    (let ((superclasses
            (mapcar (lambda (name)
                      (clysm/clos/mop:find-class* name nil))
                    superclass-names)))
      ;; Remove nil entries (for forward-referenced classes)
      (setf superclasses (remove nil superclasses))
      ;; Create slot definitions
      (let ((slots (mapcar #'slot-spec-to-slot-definition slot-specs)))
        ;; Create the class
        (let ((class (clysm/clos/mop:make-standard-class
                      :name name
                      :superclasses superclasses
                      :slots slots)))
          ;; Finalize the class
          (clysm/clos/mop:finalize-class class)
          ;; Register the class
          (clysm/clos/mop:register-class name class)
          class)))))

