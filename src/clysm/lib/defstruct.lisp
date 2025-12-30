;;; defstruct.lisp - DEFSTRUCT macro implementation for Clysm
;;; Phase 13D-10: DEFSTRUCT Wasm Compilation
;;;
;;; Implements [defstruct](resources/HyperSpec/Body/m_defstr.htm) by expanding
;;; to equivalent [defclass](resources/HyperSpec/Body/m_defcla.htm) definitions.
;;;
;;; Supports: :conc-name, :include, :copier, :predicate, :constructor options

(in-package :clysm/lib)

;;;; ============================================================
;;;; Data Structures (T010-T011)
;;;; ============================================================

;;; slot-definition: Parsed slot within a structure
;;; Fields per data-model.md specification
(defstruct (slot-definition (:conc-name slot-definition-))
  "Slot specification within a [defstruct](resources/HyperSpec/Body/m_defstr.htm)."
  (name nil :type symbol)
  (initform nil)
  (initform-p nil :type boolean)
  (type t)
  (read-only nil :type boolean))

;;; defstruct-definition: Parsed defstruct form
;;; Fields per data-model.md specification
(defstruct (defstruct-definition (:conc-name defstruct-definition-))
  "Parsed representation of a [defstruct](resources/HyperSpec/Body/m_defstr.htm) form."
  (name nil :type symbol)
  (conc-name nil :type (or null symbol))
  (constructor nil :type (or null symbol (cons symbol list)))
  (copier nil :type (or null symbol))
  (predicate nil :type (or null symbol))
  (include nil :type (or null symbol))
  (include-slot-overrides nil :type list)
  (slots nil :type list)
  (docstring nil :type (or null string)))

;;;; ============================================================
;;;; Parsing Functions (T012-T013)
;;;; ============================================================

;;; parse-slot-description: Parse a single slot specification (T013)
(defun parse-slot-description (slot-spec)
  "Parse a slot specification into a slot-definition struct.
   SLOT-SPEC can be:
     - symbol: slot name with no initform
     - (name initform) : slot with initform
     - (name initform :type T :read-only RO) : slot with options

   Reference: [defstruct](resources/HyperSpec/Body/m_defstr.htm)"
  (cond
    ;; Simple slot name
    ((symbolp slot-spec)
     (make-slot-definition :name slot-spec
                           :initform nil
                           :initform-p nil
                           :type t
                           :read-only nil))
    ;; (name) - just a name in list form
    ((and (listp slot-spec) (= 1 (length slot-spec)) (symbolp (first slot-spec)))
     (make-slot-definition :name (first slot-spec)
                           :initform nil
                           :initform-p nil
                           :type t
                           :read-only nil))
    ;; (name initform . options)
    ((listp slot-spec)
     (let ((name (first slot-spec))
           (initform (if (>= (length slot-spec) 2) (second slot-spec) nil))
           (initform-p (>= (length slot-spec) 2))
           (options (if (>= (length slot-spec) 3) (cddr slot-spec) nil))
           (slot-type t)
           (read-only nil))
       ;; Parse keyword options
       (loop for (key val) on options by #'cddr
             do (case key
                  (:type (setf slot-type val))
                  (:read-only (setf read-only val))))
       (make-slot-definition :name name
                             :initform initform
                             :initform-p initform-p
                             :type slot-type
                             :read-only read-only)))
    (t
     (error "Invalid slot specification: ~S" slot-spec))))

;;; parse-defstruct-options: Extract options from name-and-options (T042)
(defun parse-defstruct-options (name-and-options)
  "Parse the name-and-options part of a defstruct form.
   Returns (values name conc-name constructor copier predicate include include-overrides).

   NAME-AND-OPTIONS can be:
     - symbol: just the name
     - (name . options): name with options list

   Reference: [defstruct](resources/HyperSpec/Body/m_defstr.htm)"
  (if (symbolp name-and-options)
      ;; Simple name - use defaults
      ;; Intern all generated symbols in the same package as the struct name
      (let* ((name name-and-options)
             (pkg (symbol-package name))
             (default-conc-name (intern (format nil "~A-" name) pkg)))
        (values name
                default-conc-name             ; conc-name defaults to NAME-
                (intern (format nil "MAKE-~A" name) pkg)  ; constructor defaults to MAKE-NAME
                (intern (format nil "COPY-~A" name) pkg)  ; copier defaults to COPY-NAME
                (intern (format nil "~A-P" name) pkg)     ; predicate defaults to NAME-P
                nil                           ; no :include
                nil))                         ; no include overrides
      ;; (name . options)
      ;; Intern all generated symbols in the same package as the struct name
      (let* ((name (first name-and-options))
             (options (rest name-and-options))
             (pkg (symbol-package name))
             (default-conc-name (intern (format nil "~A-" name) pkg))
             (conc-name default-conc-name)
             (constructor (intern (format nil "MAKE-~A" name) pkg))
             (copier (intern (format nil "COPY-~A" name) pkg))
             (predicate (intern (format nil "~A-P" name) pkg))
             (include nil)
             (include-overrides nil))
        ;; Parse options
        (dolist (opt options)
          (cond
            ((atom opt)
             ;; Standalone keyword option - ignore for now
             nil)
            ((listp opt)
             (case (first opt)
               (:conc-name
                (setf conc-name (if (second opt)
                                    (second opt)
                                    nil)))  ; nil means no prefix
               (:constructor
                (cond
                  ((null (rest opt))        ; (:constructor) - use default
                   nil)
                  ((null (second opt))      ; (:constructor nil) - suppress
                   (setf constructor nil))
                  (t                        ; (:constructor name) or (:constructor name (args))
                   (setf constructor (second opt)))))
               (:copier
                (setf copier (if (rest opt) (second opt) nil)))
               (:predicate
                (setf predicate (if (rest opt) (second opt) nil)))
               (:include
                (setf include (second opt))
                (when (cddr opt)
                  (setf include-overrides (cddr opt))))))))
        (values name conc-name constructor copier predicate include include-overrides))))

;;; parse-defstruct: Parse entire defstruct form (T012)
(defun parse-defstruct (form)
  "Parse a complete [defstruct](resources/HyperSpec/Body/m_defstr.htm) form.
   FORM is (defstruct name-and-options . slot-descriptions).
   Returns a defstruct-definition struct."
  (unless (and (listp form) (eq 'defstruct (first form)))
    (error "Invalid defstruct form: ~S" form))
  (let ((name-and-options (second form))
        (body (cddr form)))
    ;; Parse options
    (multiple-value-bind (name conc-name constructor copier predicate include include-overrides)
        (parse-defstruct-options name-and-options)
      ;; Parse slot descriptions, skipping docstrings
      (let ((docstring nil)
            (slot-descs nil))
        (dolist (item body)
          (cond
            ((stringp item)
             (setf docstring item))
            (t
             (push item slot-descs))))
        (setf slot-descs (nreverse slot-descs))
        ;; Parse each slot
        (let ((slots (mapcar #'parse-slot-description slot-descs)))
          (make-defstruct-definition
           :name name
           :conc-name conc-name
           :constructor constructor
           :copier copier
           :predicate predicate
           :include include
           :include-slot-overrides include-overrides
           :slots slots
           :docstring docstring))))))

;;;; ============================================================
;;;; Code Generation Functions (T022-T030)
;;;; ============================================================

;;; generate-defclass-form: Generate defclass* expansion (T022)
(defun generate-defclass-form (def)
  "Generate a define-class* form from a defstruct-definition.
   Reference: [defclass](resources/HyperSpec/Body/m_defcla.htm)"
  (let* ((name (defstruct-definition-name def))
         (slots (defstruct-definition-slots def))
         (include (defstruct-definition-include def))
         (superclasses (if include (list include) nil)))
    `(clysm/clos/defclass:define-class*
      ',name
      ',superclasses
      ',(mapcar (lambda (slot)
                  (let ((slot-name (slot-definition-name slot))
                        (initform (slot-definition-initform slot))
                        (initform-p (slot-definition-initform-p slot)))
                    (if initform-p
                        `(:name ,slot-name :initform ,initform :initarg ,(intern (symbol-name slot-name) "KEYWORD"))
                        `(:name ,slot-name :initarg ,(intern (symbol-name slot-name) "KEYWORD")))))
                slots))))

;;; generate-constructor: Generate make-NAME function (T023)
(defun generate-constructor (def)
  "Generate constructor function (make-NAME) for a defstruct.
   The constructor accepts keyword arguments for each slot."
  (let* ((name (defstruct-definition-name def))
         (constructor-name (defstruct-definition-constructor def))
         (slots (defstruct-definition-slots def)))
    (when constructor-name
      `(defun ,constructor-name (&key ,@(mapcar (lambda (slot)
                                                   (let ((slot-name (slot-definition-name slot))
                                                         (initform (slot-definition-initform slot)))
                                                     (if (slot-definition-initform-p slot)
                                                         (list slot-name initform)
                                                         slot-name)))
                                                 slots))
         (clysm/clos/instance:make-instance*
          ',name
          ,@(mapcan (lambda (slot)
                      (let ((slot-name (slot-definition-name slot)))
                        (list (intern (symbol-name slot-name) "KEYWORD")
                              slot-name)))
                    slots))))))

;;; generate-accessors: Generate NAME-slot accessor functions (T024)
(defun generate-accessors (def)
  "Generate accessor functions (conc-name + slot-name) for a defstruct.
   Returns a list of defun forms."
  (let* ((name (defstruct-definition-name def))
         (pkg (symbol-package name))
         (conc-name (defstruct-definition-conc-name def))
         (slots (defstruct-definition-slots def)))
    (mapcar (lambda (slot)
              (let* ((slot-name (slot-definition-name slot))
                     (accessor-name (if conc-name
                                        (intern (format nil "~A~A" conc-name slot-name) pkg)
                                        slot-name)))
                `(defun ,accessor-name (instance)
                   (clysm/clos/slot-access:slot-value* instance ',slot-name))))
            slots)))

;;; generate-predicate: Generate NAME-p type predicate (T025)
(defun generate-predicate (def)
  "Generate type predicate function (NAME-p) for a defstruct."
  (let ((name (defstruct-definition-name def))
        (predicate-name (defstruct-definition-predicate def)))
    (when predicate-name
      `(defun ,predicate-name (object)
         (and (clysm/clos/mop:standard-instance-p object)
              (let ((class (clysm/clos/mop:instance-class object)))
                (and class
                     (eq ',name (clysm/clos/mop:class-name class)))))))))

;;; generate-copier: Generate copy-NAME function (T026)
(defun generate-copier (def)
  "Generate copier function (copy-NAME) for a defstruct.
   Creates a shallow copy of the structure instance."
  (let* ((name (defstruct-definition-name def))
         (copier-name (defstruct-definition-copier def))
         (slots (defstruct-definition-slots def)))
    (when copier-name
      `(defun ,copier-name (instance)
         (clysm/clos/instance:make-instance*
          ',name
          ,@(mapcan (lambda (slot)
                      (let ((slot-name (slot-definition-name slot)))
                        (list (intern (symbol-name slot-name) "KEYWORD")
                              `(clysm/clos/slot-access:slot-value* instance ',slot-name))))
                    slots))))))

;;; generate-setf-expanders: Generate setf expanders for accessors (T027)
(defun generate-setf-expanders (def)
  "Generate setf expander registration forms for non-read-only slots.
   Returns a list of register-setf-expander calls."
  (let* ((name (defstruct-definition-name def))
         (pkg (symbol-package name))
         (conc-name (defstruct-definition-conc-name def))
         (slots (defstruct-definition-slots def)))
    (mapcan (lambda (slot)
              (unless (slot-definition-read-only slot)
                (let* ((slot-name (slot-definition-name slot))
                       (accessor-name (if conc-name
                                          (intern (format nil "~A~A" conc-name slot-name) pkg)
                                          slot-name)))
                  (list
                   `(clysm/lib/setf-expanders:register-setf-expander
                     clysm/lib/setf-expanders:*global-setf-expander-registry*
                     ',accessor-name
                     (clysm/clos/slot-access:make-slot-accessor-setf-expander
                      ',accessor-name ',slot-name))))))
            slots)))

;;; expand-defstruct: Combine all generated forms into progn (T028)
(defun expand-defstruct (def)
  "Expand a defstruct-definition into a progn of generated forms.
   Returns the complete macro expansion."
  (let ((forms nil))
    ;; Register structure class
    (push `(clysm/clos/mop:register-structure-class
            ',(defstruct-definition-name def)
            :copier ',(defstruct-definition-copier def)
            :predicate ',(defstruct-definition-predicate def)
            :constructor ',(defstruct-definition-constructor def))
          forms)
    ;; Define the class
    (push (generate-defclass-form def) forms)
    ;; Generate constructor
    (let ((ctor (generate-constructor def)))
      (when ctor (push ctor forms)))
    ;; Generate accessors
    (dolist (accessor (generate-accessors def))
      (push accessor forms))
    ;; Generate predicate
    (let ((pred (generate-predicate def)))
      (when pred (push pred forms)))
    ;; Generate copier
    (let ((copier (generate-copier def)))
      (when copier (push copier forms)))
    ;; Generate setf expanders
    (dolist (setf-form (generate-setf-expanders def))
      (push setf-form forms))
    ;; Return structure name as final value
    (push `',(defstruct-definition-name def) forms)
    ;; Wrap in progn
    `(progn ,@(nreverse forms))))

;;;; ============================================================
;;;; Macro Registration (T029-T030)
;;;; ============================================================

;;; defstruct-expander: The macro expander function (T029)
(defun defstruct-expander (form env)
  "Macro expander for [defstruct](resources/HyperSpec/Body/m_defstr.htm).
   Parses the form and generates the expansion."
  (declare (ignore env))
  (let ((def (parse-defstruct form)))
    (expand-defstruct def)))

;;; install-defstruct-macro: Register with macro registry (T030)
(defun install-defstruct-macro (&optional registry)
  "Install the defstruct macro in REGISTRY (or global registry if nil)."
  (clysm/compiler/transform/macro:register-macro
   (or registry (clysm/compiler/transform/macro:global-macro-registry))
   'defstruct
   #'defstruct-expander))

;;;; ============================================================
;;;; Initialize DEFSTRUCT Macro at Load Time (T031)
;;;; ============================================================
;;; Phase 13D-10: Install defstruct macro into global registry when loaded

(eval-when (:load-toplevel :execute)
  (install-defstruct-macro))
