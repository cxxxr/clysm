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

(defun unregister-macro (registry name)
  "Remove a macro from the registry. Returns T if removed, NIL if not found."
  (remhash name (macro-registry-table registry)))

;;; ============================================================
;;; Error Conditions (T006, T010-T011)
;;; ============================================================

(define-condition macro-expansion-depth-exceeded (error)
  ((macro-name :initarg :macro-name :reader macro-name)
   (depth :initarg :depth :reader expansion-depth)
   (original-form :initarg :original-form :reader original-form :initform nil))
  (:report (lambda (condition stream)
             (format stream "Macro expansion depth exceeded (~D iterations) for ~S"
                     (expansion-depth condition)
                     (macro-name condition))
             (when (original-form condition)
               (format stream "~%Original form: ~S" (original-form condition))))))

(define-condition macro-lambda-list-malformed (program-error)
  ((lambda-list :initarg :lambda-list :reader mlf-lambda-list)
   (reason :initarg :reason :reader mlf-reason))
  (:report (lambda (c s)
             (format s "Malformed macro lambda-list ~S: ~A"
                     (mlf-lambda-list c) (mlf-reason c))))
  (:documentation
   "Signaled when a macro lambda-list violates ANSI CL syntax rules."))

(defparameter *macro-expansion-limit* 1000
  "Maximum number of macro expansions before signaling an error.")

;;; ============================================================
;;; Macro Lambda-List Info (T007 - 042-advanced-defmacro)
;;; ============================================================

(defstruct macro-lambda-list-info
  "Parsed macro lambda-list information with &whole and &environment support."
  (whole-var nil :type (or null symbol))           ;; &whole binding
  (env-var nil :type (or null symbol))             ;; &environment binding
  (required nil :type list)                         ;; Required parameters
  (optional nil :type list)                         ;; &optional parameters
  (rest-var nil :type (or null symbol))            ;; &rest variable
  (rest-kind nil :type (or null (member :rest :body))) ;; :rest or :body
  (keys nil :type list)                             ;; &key parameters
  (allow-other-keys nil :type boolean))            ;; &allow-other-keys flag

;;; ============================================================
;;; Macro Environment (T008 - 042-advanced-defmacro)
;;; ============================================================

(defstruct macro-environment
  "Compile-time environment for macro expansion with &environment support.
   Supports lexical nesting via parent chain."
  (local-macros nil :type (or null macro-registry))  ;; Locally defined macros (macrolet)
  (parent nil :type (or null macro-environment)))    ;; Parent environment

(defun env-macro-function (env name)
  "Look up a macro function in the environment chain.
   Searches local-macros first, then parent chain, then global registry."
  (cond
    ;; No environment - use global registry only
    ((null env)
     (macro-function* *global-macro-registry* name))
    ;; Check local macros
    ((and (macro-environment-local-macros env)
          (macro-function* (macro-environment-local-macros env) name)))
    ;; Check parent chain
    ((macro-environment-parent env)
     (env-macro-function (macro-environment-parent env) name))
    ;; Fall back to global registry
    (t
     (macro-function* *global-macro-registry* name))))

(defun extend-environment (env &optional local-macros)
  "Create a child environment with new local macro bindings.
   LOCAL-MACROS is a macro-registry for macrolet-defined macros."
  (make-macro-environment :local-macros local-macros :parent env))

;;; ============================================================
;;; Compile-time Environment (T165)
;;; ============================================================

(defstruct compile-env
  "Compile-time environment for macro expansion."
  (macros (make-macro-registry) :type macro-registry)
  (symbols (make-hash-table :test 'eq) :type hash-table))

;;; ============================================================
;;; Macro Expansion (T166-T168, Updated for 042-advanced-defmacro)
;;; ============================================================

(defun macroexpand-1* (registry form &optional env)
  "Expand FORM once if it's a macro call.
   Returns two values: (expanded-form expanded-p) for ANSI CL compliance.
   ENV is an optional macro-environment for local macro lookups."
  ;; Handle nil and atoms
  (when (or (null form) (atom form))
    (return-from macroexpand-1* (values form nil)))
  ;; Handle empty list
  (when (null (first form))
    (return-from macroexpand-1* (values form nil)))
  ;; Check for macro - use env if provided, else registry
  (let ((expander (if env
                      (env-macro-function env (first form))
                      (macro-function* registry (first form)))))
    (if expander
        ;; Feature 043: Try calling with (form env) first for &environment support.
        ;; Fall back to (form) only for legacy one-argument expanders.
        (handler-case
            (values (funcall expander form env) t)
          (error ()
            ;; Retry with only form argument for legacy expanders
            (values (funcall expander form) t)))
        (values form nil))))

(defun macroexpand* (registry form &optional env)
  "Repeatedly expand FORM until it's not a macro call.
   Returns two values: (expanded-form expanded-p) for ANSI CL compliance.
   Signals MACRO-EXPANSION-DEPTH-EXCEEDED if expansion exceeds limit."
  (loop with count = 0
        with original-form = form
        with original-name = (when (consp form) (first form))
        with ever-expanded = nil
        do (multiple-value-bind (expanded was-expanded)
               (macroexpand-1* registry form env)
             (unless was-expanded
               (return (values form ever-expanded)))
             (setf ever-expanded t)
             (incf count)
             (when (> count *macro-expansion-limit*)
               (error 'macro-expansion-depth-exceeded
                      :macro-name original-name
                      :depth count
                      :original-form original-form))
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
  (lambda-info nil :type (or null macro-lambda-list-info))  ;; T012: Parsed lambda-list info
  (body nil :type list)
  (docstring nil :type (or null string)))

(defun parse-defmacro (form)
  "Parse a defmacro form into its components.
   Populates lambda-info with parsed &whole/&environment parameters."
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
    ;; Parse the lambda-list with &whole/&environment support (T021)
    (let ((lambda-info (parse-macro-lambda-list lambda-list)))
      (make-defmacro-result
       :name name
       :lambda-list lambda-list
       :lambda-info lambda-info
       :body body
       :docstring docstring))))

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

;;; ============================================================
;;; Macro Lambda-List Parsing with &whole/&environment (T019-T021)
;;; Feature 042: Advanced Defmacro
;;; ============================================================

(defun extract-whole-param (lambda-list)
  "Extract &whole parameter from the beginning of a macro lambda-list.
   Returns two values: (whole-var remaining-lambda-list).
   Signals MACRO-LAMBDA-LIST-MALFORMED if &whole appears elsewhere."
  (cond
    ;; Empty lambda-list
    ((null lambda-list)
     (values nil nil))
    ;; &whole as first element
    ((eq (first lambda-list) '&whole)
     (unless (and (rest lambda-list)
                  (symbolp (second lambda-list))
                  (not (member (second lambda-list)
                               '(&optional &rest &body &key &allow-other-keys &aux &environment))))
       (error 'macro-lambda-list-malformed
              :lambda-list lambda-list
              :reason "&whole must be followed by a variable name (not a lambda-list keyword)"))
     (values (second lambda-list) (cddr lambda-list)))
    ;; Check that &whole doesn't appear later
    (t
     (when (member '&whole lambda-list)
       (error 'macro-lambda-list-malformed
              :lambda-list lambda-list
              :reason "&whole must appear as the first element"))
     (values nil lambda-list))))

(defun extract-environment-param (lambda-list)
  "Extract &environment parameter from anywhere in the lambda-list.
   Returns two values: (env-var filtered-lambda-list).
   &environment may appear anywhere in the lambda-list per ANSI CL."
  (let ((env-var nil)
        (result '())
        (remaining lambda-list))
    (loop while remaining do
      (let ((item (pop remaining)))
        (cond
          ((eq item '&environment)
           (unless (and remaining (symbolp (first remaining)))
             (error 'macro-lambda-list-malformed
                    :lambda-list lambda-list
                    :reason "&environment must be followed by a variable name"))
           (when env-var
             (error 'macro-lambda-list-malformed
                    :lambda-list lambda-list
                    :reason "Only one &environment parameter allowed"))
           (setf env-var (pop remaining)))
          (t
           (push item result)))))
    (values env-var (nreverse result))))

(defun parse-macro-lambda-list (lambda-list)
  "Parse a macro lambda-list with &whole and &environment support.
   Returns a MACRO-LAMBDA-LIST-INFO struct.
   ANSI CL Reference: Section 3.4.4 (Macro Lambda Lists)"
  ;; Extract &whole (must be first if present)
  (multiple-value-bind (whole-var remaining)
      (extract-whole-param lambda-list)
    ;; Extract &environment (can be anywhere)
    (multiple-value-bind (env-var filtered)
        (extract-environment-param remaining)
      ;; Parse the rest using existing parse-lambda-list
      (multiple-value-bind (required optional rest-var rest-kind keys allow-other-keys)
          (parse-lambda-list filtered)
        (make-macro-lambda-list-info
         :whole-var whole-var
         :env-var env-var
         :required required
         :optional optional
         :rest-var rest-var
         :rest-kind rest-kind
         :keys keys
         :allow-other-keys allow-other-keys)))))

(defun compile-defmacro (result)
  "Compile a parsed defmacro into a macro function.
   Supports &whole and &environment parameters (T022)."
  (let* ((name (defmacro-result-name result))
         (lambda-info (defmacro-result-lambda-info result))
         (body (defmacro-result-body result))
         ;; Extract lambda-list info
         (whole-var (when lambda-info (macro-lambda-list-info-whole-var lambda-info)))
         (env-var (when lambda-info (macro-lambda-list-info-env-var lambda-info)))
         (required (if lambda-info (macro-lambda-list-info-required lambda-info)
                       (parse-lambda-list (defmacro-result-lambda-list result))))
         (optional (when lambda-info (macro-lambda-list-info-optional lambda-info)))
         (rest-var (when lambda-info (macro-lambda-list-info-rest-var lambda-info)))
         (keys (when lambda-info (macro-lambda-list-info-keys lambda-info))))
    ;; For backward compatibility when lambda-info is not populated
    (when (null lambda-info)
      (multiple-value-bind (req opt rest rest-kind key-list allow)
          (parse-lambda-list (defmacro-result-lambda-list result))
        (declare (ignore rest-kind allow))
        (setf required req optional opt rest-var rest keys key-list)))
    ;; Create a function that destructures the form and binds &whole/&environment
    (lambda (form &optional macro-env)
      (let ((args (rest form))
            (bindings '()))
        ;; Bind &whole to the entire form (T022)
        (when whole-var
          (push (cons whole-var form) bindings))
        ;; Bind &environment to the macro expansion environment
        (when env-var
          (push (cons env-var macro-env) bindings))
        ;; Bind required arguments
        (dolist (var required)
          (unless args
            (error "Not enough arguments for macro ~S~%Expected ~D, got ~D~%Form: ~S"
                   name (length required) (- (length required) (length args) -1) form))
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
        (let ((env-form (mapcar (lambda (b) (list (car b) (list 'quote (cdr b)))) bindings)))
          (eval `(let ,env-form
                   ,@body)))))))

;;; ============================================================
;;; Global Macro Registry and User-Facing API
;;; ============================================================

(defvar *global-macro-registry* (make-macro-registry)
  "Global macro registry for standard macros.")

(defun macroexpand-1 (form &optional env)
  "Expand FORM once if it's a macro call.
   Returns two values: (expanded-form expanded-p) per ANSI CL.
   ENV is a macro-environment for local macro lookups."
  (macroexpand-1* *global-macro-registry* form env))

(defun macroexpand (form &optional env)
  "Repeatedly expand FORM until it's not a macro call.
   Returns two values: (expanded-form expanded-p) per ANSI CL.
   ENV is a macro-environment for local macro lookups."
  (macroexpand* *global-macro-registry* form env))

(defun global-macro-registry ()
  "Return the global macro registry."
  *global-macro-registry*)

(defun reset-global-macro-registry ()
  "Reset the global macro registry to a fresh state."
  (setf *global-macro-registry* (make-macro-registry)))

(defun macro-function (symbol &optional env)
  "Return the macro function associated with SYMBOL.
   ANSI CL Reference: Function MACRO-FUNCTION.
   When ENV is nil, searches global registry only.
   When ENV is a macro-environment, searches env chain then global."
  (if env
      (env-macro-function env symbol)
      (macro-function* *global-macro-registry* symbol)))

(defun (setf macro-function) (new-function symbol &optional env)
  "Set the macro function for SYMBOL.
   ANSI CL Reference: (SETF MACRO-FUNCTION).
   ENV must be nil (global registry only per ANSI CL)."
  (when env
    (error "Cannot set macro-function with non-nil environment"))
  (register-macro *global-macro-registry* symbol new-function)
  new-function)

;;; ============================================================
;;; Legacy API compatibility
;;; ============================================================

(defvar *macro-table* (make-hash-table :test 'eq)
  "Global table of registered macros for backward compatibility.")

(defun find-macro (name)
  "Find a macro by name in the global table."
  (gethash name *macro-table*))
