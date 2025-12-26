;;;; setf-expanders.lisp - Setf Expander Registry and Standard Expanders
;;;; Feature 028: Setf Macros and Generalized References

(in-package #:clysm/lib/setf-expanders)

;;; ============================================================
;;; Setf Expander Registry
;;; ============================================================

(defstruct setf-expander-registry
  "Registry mapping accessor names to setf expander functions.
   Each expander is a function: (form env) -> (values temps vals stores store-form access-form)"
  (table (make-hash-table :test 'eq) :type hash-table))

(defvar *global-setf-expander-registry*
  (make-setf-expander-registry)
  "Global registry for setf expanders.")

(defun register-setf-expander (registry name expander-fn)
  "Register EXPANDER-FN for accessor NAME in REGISTRY.
   EXPANDER-FN: (form env) -> (values temps vals stores store-form access-form)"
  (setf (gethash name (setf-expander-registry-table registry)) expander-fn))

(defun get-setf-expander (registry name)
  "Look up the setf expander for NAME in REGISTRY.
   Returns the expander function or NIL if not found."
  (gethash name (setf-expander-registry-table registry)))

;;; ============================================================
;;; Setf Conditions (Error Types)
;;; ============================================================

(define-condition setf-error (error)
  ()
  (:documentation "Base condition for setf-related errors."))

(define-condition undefined-setf-expander (setf-error)
  ((accessor :initarg :accessor :reader undefined-setf-expander-accessor))
  (:report (lambda (c s)
             (format s "No setf expander defined for ~S"
                     (undefined-setf-expander-accessor c))))
  (:documentation "Signaled when no setf expander is defined for an accessor."))

(define-condition invalid-place (setf-error)
  ((place :initarg :place :reader invalid-place-place)
   (reason :initarg :reason :reader invalid-place-reason :initform nil))
  (:report (lambda (c s)
             (if (invalid-place-reason c)
                 (format s "Invalid place ~S: ~A"
                         (invalid-place-place c)
                         (invalid-place-reason c))
                 (format s "Invalid place: ~S"
                         (invalid-place-place c)))))
  (:documentation "Signaled when an invalid place is used in a setf form."))

(define-condition constant-modification-error (invalid-place)
  ()
  (:report (lambda (c s)
             (format s "Cannot modify constant: ~S"
                     (invalid-place-place c))))
  (:documentation "Signaled when attempting to modify a constant (nil, t, keywords)."))

(define-condition odd-argument-count (setf-error)
  ((macro-name :initarg :macro-name :reader odd-argument-count-macro)
   (argument-count :initarg :argument-count :reader odd-argument-count-count))
  (:report (lambda (c s)
             (format s "~S requires an even number of arguments, got ~D"
                     (odd-argument-count-macro c)
                     (odd-argument-count-count c))))
  (:documentation "Signaled when setf/psetf receives an odd number of arguments."))

;;; ============================================================
;;; Setf Expansion Protocol (Five-Value Tuple)
;;; ============================================================

(defun simple-variable-p (place)
  "Return T if PLACE is a simple variable (symbol that's not a constant)."
  (and (symbolp place)
       (not (keywordp place))
       (not (eq place t))
       (not (eq place nil))))

(defun get-setf-expansion* (place &optional env)
  "Return the setf expansion for PLACE.
   Returns five values:
     temps      - list of temporary variable symbols
     vals       - list of value forms for temps
     stores     - list of store variable symbols (usually 1)
     store-form - form that stores the value and returns it
     access-form - form that reads the current value"
  (declare (ignore env))
  (cond
    ;; Simple variable case
    ((simple-variable-p place)
     (let ((store (gensym "STORE-")))
       (values nil                           ; temps
               nil                           ; vals
               (list store)                  ; stores
               `(setq ,place ,store)        ; store-form
               place)))                      ; access-form

    ;; Compound form - look up expander
    ((consp place)
     (let* ((accessor (first place))
            (expander (get-setf-expander *global-setf-expander-registry* accessor)))
       (if expander
           (funcall expander place nil)
           (error 'undefined-setf-expander :accessor accessor))))

    ;; Invalid place
    (t
     (error 'invalid-place :place place))))

;;; ============================================================
;;; Standard Setf Expanders
;;; ============================================================

;;; CAR/CDR expanders

(defun make-car-setf-expander ()
  "Create setf expander for CAR."
  (lambda (form env)
    (declare (ignore env))
    (let* ((cons-form (second form))
           (cons-temp (gensym "CONS-"))
           (store (gensym "STORE-")))
      (values (list cons-temp)                       ; temps
              (list cons-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca ,cons-temp ,store) ,store)  ; store-form
              `(car ,cons-temp)))))                  ; access-form

(defun make-cdr-setf-expander ()
  "Create setf expander for CDR."
  (lambda (form env)
    (declare (ignore env))
    (let* ((cons-form (second form))
           (cons-temp (gensym "CONS-"))
           (store (gensym "STORE-")))
      (values (list cons-temp)                       ; temps
              (list cons-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplacd ,cons-temp ,store) ,store)  ; store-form
              `(cdr ,cons-temp)))))                  ; access-form

;;; FIRST/REST are aliases for CAR/CDR

(defun make-first-setf-expander ()
  "Create setf expander for FIRST (alias for CAR)."
  (make-car-setf-expander))

(defun make-rest-setf-expander ()
  "Create setf expander for REST (alias for CDR)."
  (make-cdr-setf-expander))

;;; NTH expander

(defun make-nth-setf-expander ()
  "Create setf expander for NTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((index-form (second form))
           (list-form (third form))
           (index-temp (gensym "INDEX-"))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list index-temp list-temp)            ; temps
              (list index-form list-form)            ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr ,index-temp ,list-temp) ,store) ,store)  ; store-form
              `(nth ,index-temp ,list-temp)))))      ; access-form

;;; SECOND through TENTH expanders

(defun make-second-setf-expander ()
  "Create setf expander for SECOND."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (cdr ,list-temp) ,store) ,store)  ; store-form
              `(second ,list-temp)))))               ; access-form

(defun make-third-setf-expander ()
  "Create setf expander for THIRD."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (cddr ,list-temp) ,store) ,store)  ; store-form
              `(third ,list-temp)))))                ; access-form

(defun make-fourth-setf-expander ()
  "Create setf expander for FOURTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (cdddr ,list-temp) ,store) ,store)  ; store-form
              `(fourth ,list-temp)))))               ; access-form

(defun make-fifth-setf-expander ()
  "Create setf expander for FIFTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 4 ,list-temp) ,store) ,store)  ; store-form
              `(fifth ,list-temp)))))                ; access-form

(defun make-sixth-setf-expander ()
  "Create setf expander for SIXTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 5 ,list-temp) ,store) ,store)  ; store-form
              `(sixth ,list-temp)))))                ; access-form

(defun make-seventh-setf-expander ()
  "Create setf expander for SEVENTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 6 ,list-temp) ,store) ,store)  ; store-form
              `(seventh ,list-temp)))))              ; access-form

(defun make-eighth-setf-expander ()
  "Create setf expander for EIGHTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 7 ,list-temp) ,store) ,store)  ; store-form
              `(eighth ,list-temp)))))               ; access-form

(defun make-ninth-setf-expander ()
  "Create setf expander for NINTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 8 ,list-temp) ,store) ,store)  ; store-form
              `(ninth ,list-temp)))))                ; access-form

(defun make-tenth-setf-expander ()
  "Create setf expander for TENTH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((list-form (second form))
           (list-temp (gensym "LIST-"))
           (store (gensym "STORE-")))
      (values (list list-temp)                       ; temps
              (list list-form)                       ; vals
              (list store)                           ; stores
              `(progn (rplaca (nthcdr 9 ,list-temp) ,store) ,store)  ; store-form
              `(tenth ,list-temp)))))                ; access-form

;;; AREF expander

(defun make-aref-setf-expander ()
  "Create setf expander for AREF."
  (lambda (form env)
    (declare (ignore env))
    (let* ((array-form (second form))
           (index-forms (cddr form))
           (array-temp (gensym "ARRAY-"))
           (index-temps (loop for i from 0 below (length index-forms)
                              collect (gensym (format nil "INDEX-~D-" i))))
           (store (gensym "STORE-")))
      (values (cons array-temp index-temps)          ; temps
              (cons array-form index-forms)          ; vals
              (list store)                           ; stores
              `(progn (setf (aref ,array-temp ,@index-temps) ,store) ,store)  ; store-form
              `(aref ,array-temp ,@index-temps)))))  ; access-form

;;; GETHASH expander

(defun make-gethash-setf-expander ()
  "Create setf expander for GETHASH."
  (lambda (form env)
    (declare (ignore env))
    (let* ((key-form (second form))
           (hash-table-form (third form))
           (default-form (fourth form))
           (key-temp (gensym "KEY-"))
           (ht-temp (gensym "HT-"))
           (store (gensym "STORE-")))
      (if default-form
          (let ((default-temp (gensym "DEFAULT-")))
            (values (list key-temp ht-temp default-temp)     ; temps
                    (list key-form hash-table-form default-form)  ; vals
                    (list store)                             ; stores
                    `(progn (setf (gethash ,key-temp ,ht-temp) ,store) ,store)  ; store-form
                    `(gethash ,key-temp ,ht-temp ,default-temp)))  ; access-form
          (values (list key-temp ht-temp)                    ; temps
                  (list key-form hash-table-form)            ; vals
                  (list store)                               ; stores
                  `(progn (setf (gethash ,key-temp ,ht-temp) ,store) ,store)  ; store-form
                  `(gethash ,key-temp ,ht-temp))))))         ; access-form

;;; SYMBOL-VALUE/FUNCTION/PLIST expanders

(defun make-symbol-value-setf-expander ()
  "Create setf expander for SYMBOL-VALUE."
  (lambda (form env)
    (declare (ignore env))
    (let* ((symbol-form (second form))
           (symbol-temp (gensym "SYMBOL-"))
           (store (gensym "STORE-")))
      (values (list symbol-temp)                     ; temps
              (list symbol-form)                     ; vals
              (list store)                           ; stores
              `(progn (set ,symbol-temp ,store) ,store)  ; store-form
              `(symbol-value ,symbol-temp)))))       ; access-form

(defun make-symbol-function-setf-expander ()
  "Create setf expander for SYMBOL-FUNCTION."
  (lambda (form env)
    (declare (ignore env))
    (let* ((symbol-form (second form))
           (symbol-temp (gensym "SYMBOL-"))
           (store (gensym "STORE-")))
      (values (list symbol-temp)                     ; temps
              (list symbol-form)                     ; vals
              (list store)                           ; stores
              `(progn (fset ,symbol-temp ,store) ,store)  ; store-form
              `(symbol-function ,symbol-temp)))))    ; access-form

(defun make-symbol-plist-setf-expander ()
  "Create setf expander for SYMBOL-PLIST."
  (lambda (form env)
    (declare (ignore env))
    (let* ((symbol-form (second form))
           (symbol-temp (gensym "SYMBOL-"))
           (store (gensym "STORE-")))
      (values (list symbol-temp)                     ; temps
              (list symbol-form)                     ; vals
              (list store)                           ; stores
              `(progn (setplist ,symbol-temp ,store) ,store)  ; store-form
              `(symbol-plist ,symbol-temp)))))       ; access-form

;;; ============================================================
;;; Standard Expander Installation
;;; ============================================================

(defun install-standard-setf-expanders (registry)
  "Install all standard setf expanders into REGISTRY."
  ;; CAR/CDR family
  (register-setf-expander registry 'car (make-car-setf-expander))
  (register-setf-expander registry 'cdr (make-cdr-setf-expander))
  (register-setf-expander registry 'first (make-first-setf-expander))
  (register-setf-expander registry 'rest (make-rest-setf-expander))

  ;; SECOND through TENTH
  (register-setf-expander registry 'second (make-second-setf-expander))
  (register-setf-expander registry 'third (make-third-setf-expander))
  (register-setf-expander registry 'fourth (make-fourth-setf-expander))
  (register-setf-expander registry 'fifth (make-fifth-setf-expander))
  (register-setf-expander registry 'sixth (make-sixth-setf-expander))
  (register-setf-expander registry 'seventh (make-seventh-setf-expander))
  (register-setf-expander registry 'eighth (make-eighth-setf-expander))
  (register-setf-expander registry 'ninth (make-ninth-setf-expander))
  (register-setf-expander registry 'tenth (make-tenth-setf-expander))

  ;; NTH
  (register-setf-expander registry 'nth (make-nth-setf-expander))

  ;; AREF
  (register-setf-expander registry 'aref (make-aref-setf-expander))

  ;; GETHASH
  (register-setf-expander registry 'gethash (make-gethash-setf-expander))

  ;; Symbol accessors
  (register-setf-expander registry 'symbol-value (make-symbol-value-setf-expander))
  (register-setf-expander registry 'symbol-function (make-symbol-function-setf-expander))
  (register-setf-expander registry 'symbol-plist (make-symbol-plist-setf-expander))

  registry)

;; Initialize global registry with standard expanders
(install-standard-setf-expanders *global-setf-expander-registry*)

;;; ============================================================
;;; User-Defined Setf Expanders
;;; ============================================================

(defmacro define-setf-expander* (accessor lambda-list &body body)
  "Define a setf expander for ACCESSOR.
   LAMBDA-LIST must include &environment as the last parameter.
   BODY should return five values: temps, vals, stores, store-form, access-form.

   Example:
     (define-setf-expander* my-car (form &environment env)
       (let ((temp (gensym)) (store (gensym)))
         (values (list temp)
                 (list (second form))
                 (list store)
                 `(progn (rplaca ,temp ,store) ,store)
                 `(car ,temp))))"
  (let ((form-var (gensym "FORM-"))
        (env-var (gensym "ENV-")))
    ;; Extract the env parameter from lambda-list
    (multiple-value-bind (regular-params env-param)
        (parse-define-setf-lambda-list lambda-list)
      `(register-setf-expander
        *global-setf-expander-registry*
        ',accessor
        (lambda (,form-var ,env-var)
          (let ,(if env-param
                    `((,(first regular-params) ,form-var)
                      (,env-param ,env-var))
                    `((,(first regular-params) ,form-var)))
            ,@body))))))

(defun parse-define-setf-lambda-list (lambda-list)
  "Parse a define-setf-expander lambda list.
   Returns (values regular-params env-param)."
  (let ((env-pos (position '&environment lambda-list)))
    (if env-pos
        (values (subseq lambda-list 0 env-pos)
                (nth (1+ env-pos) lambda-list))
        (values lambda-list nil))))

(defmacro defsetf* (accessor &rest args)
  "Define a setf expander for ACCESSOR.

   Short form: (defsetf* accessor setter)
     Expands (setf (accessor args...) value) to (setter args... value)

   Long form: (defsetf* accessor lambda-list (store-var) body...)
     BODY should return the store form.

   Examples:
     ;; Short form
     (defsetf* car rplaca)

     ;; Long form
     (defsetf* subseq (sequence start &optional end) (new-value)
       `(progn (replace ,sequence ,new-value :start1 ,start :end1 ,end)
               ,new-value))"
  (cond
    ;; Short form: (defsetf accessor setter)
    ((and (= (length args) 1)
          (symbolp (first args)))
     (let ((setter (first args)))
       `(register-setf-expander
         *global-setf-expander-registry*
         ',accessor
         (make-short-form-setf-expander ',accessor ',setter))))

    ;; Long form: (defsetf accessor lambda-list (store-vars) body...)
    ((and (>= (length args) 2)
          (listp (first args))
          (listp (second args)))
     (let ((lambda-list (first args))
           (store-vars (second args))
           (body (cddr args)))
       `(register-setf-expander
         *global-setf-expander-registry*
         ',accessor
         (make-long-form-setf-expander ',accessor ',lambda-list ',store-vars ',body))))

    (t
     (error "Invalid defsetf syntax for ~S" accessor))))

(defun make-short-form-setf-expander (accessor setter)
  "Create a setf expander for the short form: (defsetf accessor setter).
   The expansion calls (setter args... new-value)."
  (declare (ignore accessor))
  (lambda (form env)
    (declare (ignore env))
    (let* ((arg-forms (rest form))
           (temps (loop for i from 0 below (length arg-forms)
                        collect (gensym (format nil "ARG-~D-" i))))
           (store (gensym "STORE-")))
      (values temps                                    ; temps
              arg-forms                                ; vals
              (list store)                             ; stores
              `(,setter ,@temps ,store)                ; store-form
              `(,accessor ,@temps)))))                 ; access-form

(defun make-long-form-setf-expander (accessor lambda-list store-vars body)
  "Create a setf expander for the long form.
   LAMBDA-LIST is the accessor's parameter list.
   STORE-VARS is a list of store variable names (usually one).
   BODY is the form that produces the store-form."
  (declare (ignore accessor))
  (lambda (form env)
    (declare (ignore env))
    (let* ((arg-forms (rest form))
           ;; Parse lambda-list to extract required and optional parameters
           (param-info (parse-setf-lambda-list lambda-list))
           (required-params (getf param-info :required))
           (optional-params (getf param-info :optional))
           (temps nil)
           (vals nil)
           (bindings nil))
      ;; Generate temps for required args
      (loop for param in required-params
            for arg in arg-forms
            for temp = (gensym (format nil "~A-" param))
            do (push temp temps)
               (push arg vals)
               (push (list param temp) bindings))
      ;; Generate temps for optional args that are provided
      (let ((remaining-args (nthcdr (length required-params) arg-forms)))
        (loop for opt-spec in optional-params
              for arg = (pop remaining-args)
              for param = (if (listp opt-spec) (first opt-spec) opt-spec)
              for temp = (gensym (format nil "~A-" param))
              while arg
              do (push temp temps)
                 (push arg vals)
                 (push (list param temp) bindings)))
      ;; Reverse to maintain order
      (setf temps (nreverse temps))
      (setf vals (nreverse vals))
      (setf bindings (nreverse bindings))
      ;; Evaluate body with parameter bindings to produce store-form
      (let* ((store (first store-vars))
             (store-temp (gensym "STORE-"))
             ;; Substitute temps for params in body
             (store-form (subst-params-in-body body bindings store store-temp)))
        (values temps                                  ; temps
                vals                                   ; vals
                (list store-temp)                      ; stores
                store-form                             ; store-form
                `(,accessor ,@temps))))))              ; access-form

(defun parse-setf-lambda-list (lambda-list)
  "Parse a simplified lambda list for defsetf.
   Returns a plist with :required and :optional keys."
  (let ((required nil)
        (optional nil)
        (state :required))
    (dolist (item lambda-list)
      (cond
        ((eq item '&optional)
         (setf state :optional))
        ((eq item '&rest)
         (setf state :rest))
        ((eq item '&key)
         (setf state :key))
        (t
         (ecase state
           (:required (push item required))
           (:optional (push item optional))
           ;; Ignore &rest and &key for now
           ((:rest :key) nil)))))
    (list :required (nreverse required)
          :optional (nreverse optional))))

(defun subst-params-in-body (body bindings store store-temp)
  "Substitute parameter temps and store var in BODY to produce store-form.
   BINDINGS is list of (param temp) pairs.
   STORE is the original store var name, STORE-TEMP is the gensym."
  ;; Build substitution alist
  (let ((subst-alist (cons (cons store store-temp)
                           (mapcar (lambda (b) (cons (first b) (second b)))
                                   bindings))))
    ;; Apply substitution to body
    (subst-in-tree subst-alist (if (= 1 (length body))
                                    (first body)
                                    `(progn ,@body)))))

(defun subst-in-tree (alist tree)
  "Substitute symbols in TREE according to ALIST.
   ALIST is ((old . new) ...)."
  (cond
    ((null tree) nil)
    ((symbolp tree)
     (let ((pair (assoc tree alist)))
       (if pair (cdr pair) tree)))
    ((consp tree)
     (cons (subst-in-tree alist (car tree))
           (subst-in-tree alist (cdr tree))))
    (t tree)))
