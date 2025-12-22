;;;; instance.lisp - Instance creation (Phase 10 - T224-T226)

(in-package #:clysm/clos/instance)

;;; ============================================================
;;; make-instance (T224)
;;; ============================================================

(defun make-instance* (class-name &rest initargs)
  "Create an instance of a class.
   CLASS-NAME is a symbol naming the class.
   INITARGS are keyword arguments for slot initialization."
  (let ((class (if (symbolp class-name)
                   (clysm/clos/mop:find-class* class-name)
                   class-name)))
    (unless class
      (error "Cannot find class: ~S" class-name))
    ;; Create the instance
    (let* ((slot-count (clysm/clos/mop:total-slot-count class))
           (instance (clysm/clos/mop:make-instance-struct class slot-count)))
      ;; Initialize slots
      (initialize-slots instance class initargs)
      instance)))

;;; ============================================================
;;; Slot Initialization (T225)
;;; ============================================================

(defun initialize-slots (instance class initargs)
  "Initialize slots of an instance."
  ;; Process all slots in the class and its superclasses
  (dolist (slot-class (clysm/clos/mop:class-precedence-list class))
    (dolist (slot (clysm/clos/mop:class-slots slot-class))
      (initialize-slot instance class slot initargs))))

(defun initialize-slot (instance class slot initargs)
  "Initialize a single slot."
  (let* ((slot-name (clysm/clos/mop:slot-definition-name slot))
         (initarg (clysm/clos/mop:slot-definition-initarg slot))
         (initform (clysm/clos/mop:slot-definition-initform slot))
         (initform-p (clysm/clos/mop:slot-definition-initform-p slot))
         (slot-index (clysm/clos/mop:slot-index class slot-name)))
    (when slot-index
      (let ((value
              (cond
                ;; Check for initarg in initargs
                ((and initarg (getf initargs initarg :not-found))
                 (let ((v (getf initargs initarg :not-found)))
                   (if (eq v :not-found)
                       (if initform-p initform nil)
                       v)))
                ;; Use initform if provided
                (initform-p
                 (if (functionp initform)
                     (funcall initform)
                     initform))
                ;; Otherwise nil
                (t nil))))
        (setf (aref (clysm/clos/mop:instance-slots instance) slot-index)
              value)))))

;;; ============================================================
;;; Shared Initialize Protocol
;;; ============================================================

(defun shared-initialize* (instance slot-names &rest initargs)
  "Shared initialization protocol."
  (let ((class (clysm/clos/mop:instance-class instance)))
    (dolist (slot-class (clysm/clos/mop:class-precedence-list class))
      (dolist (slot (clysm/clos/mop:class-slots slot-class))
        (let ((slot-name (clysm/clos/mop:slot-definition-name slot)))
          (when (or (eq slot-names t)
                    (member slot-name slot-names))
            (initialize-slot instance class slot initargs))))))
  instance)

