;;;; macro.lisp - Macro expansion system for Clysm
;;;; Phase 8 - US6: Macro system implementation

(in-package #:clysm/compiler/transform/macro)

;;; ============================================================
;;; Macro Registry (T164)
;;; ============================================================

(defstruct macro-registry
  "Registry for macro functions."
  (table (make-hash-table :test 'eq) :type hash-table))

(defun registry-p (obj)
  "Check if OBJ is a macro registry."
  (macro-registry-p obj))

(defun register-macro (registry name expander)
  "Register a macro function in the registry."
  (setf (gethash name (macro-registry-table registry)) expander))

(defun macro-function* (registry name)
  "Get the macro function for NAME from REGISTRY."
  (gethash name (macro-registry-table registry)))

(defun macro-form-p (registry form)
  "Check if FORM is a macro call."
  (and (consp form)
       (symbolp (first form))
       (not (null (macro-function* registry (first form))))))

;;; ============================================================
;;; Error Conditions (T006)
;;; ============================================================

(define-condition macro-expansion-depth-exceeded (error)
  ((macro-name :initarg :macro-name :reader macro-name)
   (depth :initarg :depth :reader expansion-depth))
  (:report (lambda (condition stream)
             (format stream "Macro expansion depth exceeded (~D iterations) for ~S"
                     (expansion-depth condition)
                     (macro-name condition)))))

(defparameter *macro-expansion-limit* 1000
  "Maximum number of macro expansions before signaling an error.")

;;; ============================================================
;;; Compile-time Environment (T165)
;;; ============================================================

(defstruct compile-env
  "Compile-time environment for macro expansion."
  (macros (make-macro-registry) :type macro-registry)
  (symbols (make-hash-table :test 'eq) :type hash-table))

;;; ============================================================
;;; Macro Expansion (T166-T168)
;;; ============================================================

(defun macroexpand-1* (registry form)
  "Expand FORM once if it's a macro call."
  ;; Handle nil and atoms
  (when (or (null form) (atom form))
    (return-from macroexpand-1* form))
  ;; Handle empty list
  (when (null (first form))
    (return-from macroexpand-1* form))
  ;; Check for macro
  (let ((expander (macro-function* registry (first form))))
    (if expander
        (funcall expander form)
        form)))

(defun macroexpand* (registry form)
  "Repeatedly expand FORM until it's not a macro call.
   Signals MACRO-EXPANSION-DEPTH-EXCEEDED if expansion exceeds limit."
  (loop with count = 0
        with original-name = (when (consp form) (first form))
        do (let ((expanded (macroexpand-1* registry form)))
             (when (eq expanded form)
               (return form))
             (incf count)
             (when (> count *macro-expansion-limit*)
               (error 'macro-expansion-depth-exceeded
                      :macro-name original-name
                      :depth count))
             (setf form expanded))))

(defun macroexpand-all (registry form)
  "Recursively expand all macros in FORM."
  ;; Handle atoms
  (when (atom form)
    (return-from macroexpand-all form))
  ;; Handle nil
  (when (null form)
    (return-from macroexpand-all nil))
  ;; First expand the top level
  (let ((expanded (macroexpand* registry form)))
    (when (atom expanded)
      (return-from macroexpand-all expanded))
    ;; Now walk the form based on its type
    (let ((head (first expanded)))
      (cond
        ;; quote - don't expand contents
        ((eq head 'quote)
         expanded)
        ;; quasiquote - expand the backquote
        ((eq head 'quasiquote)
         (expand-backquote expanded))
        ;; if form
        ((eq head 'if)
         (list* 'if
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (rest expanded))))
        ;; progn form
        ((eq head 'progn)
         (list* 'progn
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (rest expanded))))
        ;; let/let* form - expand bindings and body
        ((or (eq head 'let) (eq head 'let*))
         (list* head
                (mapcar (lambda (binding)
                          (if (consp binding)
                              (list (first binding)
                                    (macroexpand-all registry (second binding)))
                              binding))
                        (second expanded))
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (cddr expanded))))
        ;; lambda form
        ((eq head 'lambda)
         (list* 'lambda
                (second expanded)  ; parameters
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (cddr expanded))))
        ;; defun form
        ((eq head 'defun)
         (list* 'defun
                (second expanded)  ; name
                (third expanded)   ; parameters
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (cdddr expanded))))
        ;; block
        ((eq head 'block)
         (list* 'block
                (second expanded)  ; name
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (cddr expanded))))
        ;; flet/labels
        ((or (eq head 'flet) (eq head 'labels))
         (list* head
                (mapcar (lambda (def)
                          (list* (first def)   ; name
                                 (second def)  ; parameters
                                 (mapcar (lambda (e) (macroexpand-all registry e))
                                         (cddr def))))
                        (second expanded))
                (mapcar (lambda (e) (macroexpand-all registry e))
                        (cddr expanded))))
        ;; General case - expand all subforms
        (t
         (mapcar (lambda (e) (macroexpand-all registry e))
                 expanded))))))

;;; ============================================================
;;; Backquote Expansion (T169-T172)
;;; ============================================================

(defun quasiquote-p (sym)
  "Check if SYM is a quasiquote symbol."
  (and (symbolp sym)
       (string= (symbol-name sym) "QUASIQUOTE")))

(defun unquote-p (sym)
  "Check if SYM is an unquote symbol."
  (and (symbolp sym)
       (string= (symbol-name sym) "UNQUOTE")))

(defun unquote-splicing-p (sym)
  "Check if SYM is an unquote-splicing symbol."
  (and (symbolp sym)
       (string= (symbol-name sym) "UNQUOTE-SPLICING")))

(defun expand-backquote (form)
  "Expand a quasiquote form."
  (unless (and (consp form) (quasiquote-p (first form)))
    (return-from expand-backquote form))
  (expand-bq (second form)))

(defun expand-bq (form)
  "Inner backquote expansion."
  (cond
    ;; Atoms pass through
    ((atom form)
     (if (or (numberp form) (stringp form) (keywordp form) (null form))
         form
         (list 'quote form)))
    ;; unquote - return the expression
    ((unquote-p (first form))
     (second form))
    ;; unquote-splicing - error at top level
    ((unquote-splicing-p (first form))
     (error "unquote-splicing not in list context"))
    ;; nested quasiquote
    ((quasiquote-p (first form))
     (expand-bq (expand-bq (second form))))
    ;; List - handle element by element
    (t
     (expand-bq-list form))))

(defun expand-bq-list (lst)
  "Expand a list in backquote context."
  (if (null lst)
      nil
      (let ((parts (mapcar #'expand-bq-element lst)))
        (cond
          ;; If all parts are simple quotes, optimize
          ((every #'constant-form-p parts)
           (list 'quote (mapcar #'extract-constant parts)))
          ;; If any part has splice, use append
          ((some #'splice-form-p parts)
           (cons 'append (mapcar #'wrap-for-append parts)))
          ;; Otherwise use list or list*
          (t
           (cons 'list parts))))))

(defun expand-bq-element (elem)
  "Expand a single element in a backquote list."
  (cond
    ((atom elem)
     (if (or (numberp elem) (stringp elem) (keywordp elem) (null elem))
         elem
         (list 'quote elem)))
    ((unquote-p (first elem))
     (second elem))
    ((unquote-splicing-p (first elem))
     (list '%splice (second elem)))
    ((quasiquote-p (first elem))
     (expand-bq elem))
    (t
     (expand-bq-list elem))))

(defun constant-form-p (form)
  "Check if FORM is a constant (self-evaluating or quoted).
   A constant form is one that can be evaluated at compile time."
  (cond
    ;; Quoted forms are constants
    ((and (consp form) (eq (first form) 'quote))
     t)
    ;; Self-evaluating atoms are constants
    ((or (numberp form) (stringp form) (keywordp form) (null form) (eq form t))
     t)
    ;; Other atoms (symbols) are NOT constants - they are variable references
    ((symbolp form)
     nil)
    ;; Lists that are not quote forms are not constants
    (t nil)))

(defun extract-constant (form)
  "Extract the value from a constant form."
  (if (and (consp form) (eq (first form) 'quote))
      (second form)
      form))

(defun splice-form-p (form)
  "Check if FORM is a splice marker."
  (and (consp form)
       (eq (first form) '%splice)))

(defun wrap-for-append (form)
  "Wrap a form for use in append."
  (cond
    ((splice-form-p form)
     (second form))
    ((constant-form-p form)
     (list 'list form))
    (t
     (list 'list form))))

;;; ============================================================
;;; Defmacro Parsing (T173-T175)
;;; ============================================================

(defstruct defmacro-result
  "Result of parsing a defmacro form."
  (name nil :type symbol)
  (lambda-list nil :type list)
  (body nil :type list)
  (docstring nil :type (or null string)))

(defun parse-defmacro (form)
  "Parse a defmacro form into its components."
  (unless (and (consp form) (eq (first form) 'defmacro))
    (error "Not a defmacro form: ~S" form))
  (let ((name (second form))
        (lambda-list (third form))
        (body (cdddr form))
        (docstring nil))
    ;; Check for docstring
    (when (and (stringp (first body)) (rest body))
      (setf docstring (first body))
      (setf body (rest body)))
    (make-defmacro-result
     :name name
     :lambda-list lambda-list
     :body body
     :docstring docstring)))

(defun parse-lambda-list (lambda-list)
  "Parse a macro lambda list, handling &body, &rest, &optional, &key, &allow-other-keys."
  (let ((required '())
        (optional '())
        (rest-var nil)
        (body-var nil)
        (keys '())
        (allow-other-keys nil)
        (mode :required))
    (dolist (item lambda-list)
      (case item
        (&optional
         (setf mode :optional))
        (&rest
         (setf mode :rest))
        (&body
         (setf mode :body))
        (&key
         (setf mode :key))
        (&allow-other-keys
         (setf allow-other-keys t))
        (otherwise
         (case mode
           (:required (push item required))
           (:optional (push (if (consp item) item (list item nil)) optional))
           (:rest (setf rest-var item mode :after-rest))
           (:body (setf body-var item mode :after-rest))
           (:after-rest
            ;; After &rest, we can have &key
            (cond
              ((eq item '&key) (setf mode :key))
              ((eq item '&allow-other-keys) (setf allow-other-keys t))
              (t (error "Unexpected item after &rest/&body: ~S" item))))
           (:key
            ;; Parse keyword parameter: var, (var default), ((keyword var) default)
            (push (parse-key-param item) keys))
           (:done (error "Unexpected item after parsing: ~S" item))))))
    (values (nreverse required)
            (nreverse optional)
            (or rest-var body-var)
            (if body-var :body :rest)
            (nreverse keys)
            allow-other-keys)))

(defun parse-key-param (param)
  "Parse a keyword parameter specification.
   Returns (keyword-name var-name default-value)."
  (cond
    ;; Simple: var -> (:var var nil)
    ((symbolp param)
     (list (intern (symbol-name param) :keyword) param nil))
    ;; With default: (var default) -> (:var var default)
    ((and (consp param) (symbolp (first param)))
     (list (intern (symbol-name (first param)) :keyword)
           (first param)
           (second param)))
    ;; Full form: ((keyword var) default) -> (keyword var default)
    ((and (consp param) (consp (first param)))
     (list (first (first param))
           (second (first param))
           (second param)))
    (t (error "Invalid keyword parameter: ~S" param))))

(defun compile-defmacro (result)
  "Compile a parsed defmacro into a macro function."
  (let* ((name (defmacro-result-name result))
         (lambda-list (defmacro-result-lambda-list result))
         (body (defmacro-result-body result)))
    (multiple-value-bind (required optional rest-var rest-kind keys allow-other-keys)
        (parse-lambda-list lambda-list)
      (declare (ignore rest-kind allow-other-keys))
      ;; Create a function that destructures the form
      (lambda (form)
        (let ((args (rest form))
              (bindings '()))
          ;; Bind required arguments
          (dolist (var required)
            (unless args
              (error "Not enough arguments for macro ~S" name))
            (push (cons var (pop args)) bindings))
          ;; Bind optional arguments
          (dolist (opt optional)
            (let ((var (if (consp opt) (first opt) opt))
                  (default (if (consp opt) (second opt) nil)))
              (push (cons var (if args (pop args) default)) bindings)))
          ;; Bind rest/body
          (when rest-var
            (push (cons rest-var args) bindings))
          ;; Bind keyword arguments
          (when keys
            (dolist (key-spec keys)
              (destructuring-bind (keyword var default) key-spec
                (let ((val (getf args keyword '%not-found%)))
                  (if (eq val '%not-found%)
                      (push (cons var default) bindings)
                      (push (cons var val) bindings))))))
          ;; Evaluate the body with bindings
          (let ((env (mapcar (lambda (b) (list (car b) (list 'quote (cdr b)))) bindings)))
            (eval `(let ,env
                     ,@body))))))))

;;; ============================================================
;;; Global Macro Registry and User-Facing API
;;; ============================================================

(defvar *global-macro-registry* (make-macro-registry)
  "Global macro registry for standard macros.")

(defun macroexpand-1 (form &optional env)
  "Expand FORM once if it's a macro call, using the global macro registry.
   ENV is currently ignored but provided for CL compatibility."
  (declare (ignore env))
  (macroexpand-1* *global-macro-registry* form))

(defun macroexpand (form &optional env)
  "Repeatedly expand FORM until it's not a macro call, using the global macro registry.
   ENV is currently ignored but provided for CL compatibility."
  (declare (ignore env))
  (macroexpand* *global-macro-registry* form))

(defun global-macro-registry ()
  "Return the global macro registry."
  *global-macro-registry*)

(defun reset-global-macro-registry ()
  "Reset the global macro registry to a fresh state."
  (setf *global-macro-registry* (make-macro-registry)))

;;; ============================================================
;;; Legacy API compatibility
;;; ============================================================

(defvar *macro-table* (make-hash-table :test 'eq)
  "Global table of registered macros for backward compatibility.")

(defun find-macro (name)
  "Find a macro by name in the global table."
  (gethash name *macro-table*))
