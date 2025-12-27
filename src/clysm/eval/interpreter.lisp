;;;; interpreter.lisp - Tier 1 S-expression interpreter
;;;; Phase 9: Eval/JIT Infrastructure
;;;; Feature 044: Extended for Interpreter Bootstrap

(in-package #:clysm/eval/interpreter)

;;; ============================================================
;;; Error Conditions (Phase 2: T016-T020)
;;; ============================================================

(define-condition interpreter-error (error)
  ((form :initarg :form :reader ie-form :initform nil)
   (message :initarg :message :reader ie-message))
  (:report (lambda (c s)
             (format s "Interpreter error~@[ in ~S~]: ~A"
                     (ie-form c) (ie-message c)))))

(define-condition unbound-variable-error (interpreter-error)
  ((variable :initarg :variable :reader uve-variable))
  (:report (lambda (c s)
             (format s "Unbound variable: ~S" (uve-variable c)))))

(define-condition undefined-function-error (interpreter-error)
  ((function-name :initarg :function-name :reader ufe-function-name))
  (:report (lambda (c s)
             (format s "Undefined function: ~S" (ufe-function-name c)))))

(define-condition unsupported-feature-error (interpreter-error)
  ((feature :initarg :feature :reader usf-feature))
  (:report (lambda (c s)
             (format s "Unsupported feature: ~S~@[ in form ~S~]"
                     (usf-feature c) (ie-form c)))))

(define-condition macro-expansion-depth-error (interpreter-error)
  ((macro-name :initarg :macro-name :reader mede-macro-name)
   (depth :initarg :depth :reader mede-depth))
  (:report (lambda (c s)
             (format s "Macro expansion depth exceeded (~D) for ~S"
                     (mede-depth c) (mede-macro-name c)))))

;;; ============================================================
;;; Global Registries (Phase 2: T007-T009)
;;; ============================================================

(defvar *struct-registry* (make-hash-table :test 'eq)
  "Global hash table mapping struct names to interpreter-struct-type.")

(defvar *special-variables* (make-hash-table :test 'eq)
  "Global hash table for defvar/defparameter bindings.")

(defvar *constants* (make-hash-table :test 'eq)
  "Global hash table for defconstant bindings.")

(defvar *interpreter-macros* (make-hash-table :test 'eq)
  "Simple macro registry for interpreter.")

(defun clear-registries ()
  "Clear all global registries."
  (clrhash *struct-registry*)
  (clrhash *special-variables*)
  (clrhash *constants*))

;;; ============================================================
;;; Lambda-List Info Struct (Phase 2: T011)
;;; ============================================================

(defstruct (lambda-list-info (:constructor %make-lambda-list-info))
  "Parsed representation of a lambda-list."
  (required nil :type list)           ; List of required parameter symbols
  (optional nil :type list)           ; List of (name default supplied-p) specs
  (rest nil :type (or null symbol))   ; &rest parameter symbol
  (keys nil :type list)               ; List of (keyword name default supplied-p) specs
  (allow-other-keys nil :type boolean)
  (aux nil :type list))               ; List of (name value) specs

;;; ============================================================
;;; Struct Type Definition (Phase 2: T012)
;;; ============================================================

(defstruct (interpreter-struct-type (:constructor %make-interpreter-struct-type))
  "Represents a structure type defined by defstruct."
  (name nil :type symbol)
  (slots nil :type list)              ; List of (name default) pairs
  (constructor nil :type symbol)      ; Constructor function name
  (copier nil :type (or null symbol)) ; Copier function name
  (predicate nil :type (or null symbol)) ; Type predicate name
  (include nil :type (or null symbol)))  ; Included parent struct

;;; ============================================================
;;; Struct Instance (Phase 2: T013)
;;; ============================================================

(defstruct (interpreter-struct-instance (:constructor %make-interpreter-struct-instance))
  "Runtime instance of an interpreted structure."
  (type nil :type (or null interpreter-struct-type))
  (slots nil :type (or null simple-vector)))

;;; ============================================================
;;; Lambda-List Parsing (Phase 2: T014)
;;; ============================================================

(defun parse-lambda-list (lambda-list)
  "Parse a lambda-list into a lambda-list-info struct.
   Handles &optional, &rest, &key, &allow-other-keys, &aux, and supplied-p."
  (let ((required nil)
        (optional nil)
        (rest-var nil)
        (keys nil)
        (allow-other-keys nil)
        (aux nil)
        (state :required))
    (dolist (item lambda-list)
      (case item
        (&optional (setf state :optional))
        (&rest (setf state :rest))
        (&key (setf state :key))
        (&allow-other-keys (setf allow-other-keys t))
        (&aux (setf state :aux))
        (otherwise
         (ecase state
           (:required
            (push item required))
           (:optional
            (push (parse-optional-spec item) optional))
           (:rest
            (setf rest-var item)
            (setf state :after-rest))
           (:after-rest
            ;; Next keyword resets state
            nil)
           (:key
            (push (parse-key-spec item) keys))
           (:aux
            (push (parse-aux-spec item) aux))))))
    (%make-lambda-list-info
     :required (nreverse required)
     :optional (nreverse optional)
     :rest rest-var
     :keys (nreverse keys)
     :allow-other-keys allow-other-keys
     :aux (nreverse aux))))

(defun parse-optional-spec (spec)
  "Parse an &optional parameter spec.
   Returns (name default supplied-p)."
  (if (symbolp spec)
      (list spec nil nil)
      (list (first spec)
            (second spec)
            (third spec))))

(defun parse-key-spec (spec)
  "Parse a &key parameter spec.
   Returns (keyword name default supplied-p)."
  (if (symbolp spec)
      (list (intern (symbol-name spec) :keyword) spec nil nil)
      (let* ((name-spec (first spec))
             (keyword (if (consp name-spec)
                         (first name-spec)
                         (intern (symbol-name name-spec) :keyword)))
             (var (if (consp name-spec)
                     (second name-spec)
                     name-spec)))
        (list keyword var (second spec) (third spec)))))

(defun parse-aux-spec (spec)
  "Parse an &aux parameter spec.
   Returns (name value)."
  (if (symbolp spec)
      (list spec nil)
      (list (first spec) (second spec))))

;;; ============================================================
;;; Lambda-List Argument Binding (Phase 2: T015)
;;; ============================================================

(defun bind-lambda-list-args (ll-info args env)
  "Bind ARGS to parameters described by LL-INFO.
   Returns an alist of (name . value) bindings."
  (let ((bindings nil)
        (remaining-args args))
    ;; Bind required parameters
    (dolist (req (lambda-list-info-required ll-info))
      (unless remaining-args
        (error 'interpreter-error
               :message (format nil "Too few arguments: missing ~S" req)))
      (push (cons req (pop remaining-args)) bindings))

    ;; Bind optional parameters
    (dolist (opt-spec (lambda-list-info-optional ll-info))
      (destructuring-bind (name default supplied-p) opt-spec
        (if remaining-args
            (progn
              (push (cons name (pop remaining-args)) bindings)
              (when supplied-p
                (push (cons supplied-p t) bindings)))
            (progn
              (push (cons name (interpret-form default env)) bindings)
              (when supplied-p
                (push (cons supplied-p nil) bindings))))))

    ;; Bind &rest parameter
    (when (lambda-list-info-rest ll-info)
      (push (cons (lambda-list-info-rest ll-info) remaining-args) bindings)
      (setf remaining-args nil))

    ;; Bind keyword parameters
    (when (lambda-list-info-keys ll-info)
      (let ((key-args remaining-args))
        (dolist (key-spec (lambda-list-info-keys ll-info))
          (destructuring-bind (keyword name default supplied-p) key-spec
            (let ((val (getf key-args keyword :not-found)))
              (if (eq val :not-found)
                  (progn
                    (push (cons name (interpret-form default env)) bindings)
                    (when supplied-p
                      (push (cons supplied-p nil) bindings)))
                  (progn
                    (push (cons name val) bindings)
                    (when supplied-p
                      (push (cons supplied-p t) bindings)))))))))

    ;; Note: &aux parameters are NOT bound here
    ;; They must be evaluated after creating the call environment
    ;; because they may reference required/optional/key parameters
    (nreverse bindings)))

;;; ============================================================
;;; Interpreter Environment
;;; ============================================================

(defstruct (interpreter-env (:constructor %make-interpreter-env))
  "Interpreter environment for variable bindings."
  (bindings (make-hash-table :test 'eq) :type hash-table)
  (parent nil :type (or null interpreter-env)))

(defun make-interpreter-env (&optional parent)
  "Create a new interpreter environment, optionally with a parent."
  (let ((env (%make-interpreter-env :parent parent)))
    ;; Initialize with built-in functions
    (when (null parent)
      (install-builtins env))
    env))

(defun env-bind (env name value)
  "Bind NAME to VALUE in ENV."
  (setf (gethash name (interpreter-env-bindings env)) value))

(defun env-lookup (env name)
  "Look up NAME in ENV, searching parent environments.
If not found by exact symbol match, tries name-based lookup for builtins."
  (multiple-value-bind (value found)
      (gethash name (interpreter-env-bindings env))
    (if found
        value
        (if (interpreter-env-parent env)
            (env-lookup (interpreter-env-parent env) name)
            ;; Try name-based lookup for cross-package compatibility
            (env-lookup-by-name env (symbol-name name))))))

(defun env-lookup-by-name (env name-string)
  "Look up by symbol name string in ENV."
  (block nil
    (maphash (lambda (k v)
               (when (string= (symbol-name k) name-string)
                 (return v)))
             (interpreter-env-bindings env))
    (if (interpreter-env-parent env)
        (env-lookup-by-name (interpreter-env-parent env) name-string)
        (error "Unbound variable: ~A" name-string))))

(defun env-bound-p (env name)
  "Check if NAME is bound in ENV."
  (multiple-value-bind (value found)
      (gethash name (interpreter-env-bindings env))
    (declare (ignore value))
    (or found
        (and (interpreter-env-parent env)
             (env-bound-p (interpreter-env-parent env) name)))))

(defun extend-env (env bindings)
  "Create a new environment extending ENV with BINDINGS (alist)."
  (let ((new-env (make-interpreter-env env)))
    (dolist (binding bindings)
      (env-bind new-env (car binding) (cdr binding)))
    new-env))

;;; ============================================================
;;; Built-in Functions
;;; ============================================================

(defun install-builtins (env)
  "Install built-in functions into ENV."
  ;; Arithmetic
  (env-bind env '+ (lambda (&rest args) (apply #'+ args)))
  (env-bind env '- (lambda (&rest args) (apply #'- args)))
  (env-bind env '* (lambda (&rest args) (apply #'* args)))
  (env-bind env '/ (lambda (&rest args) (apply #'/ args)))

  ;; Comparisons
  (env-bind env '< #'<)
  (env-bind env '> #'>)
  (env-bind env '<= #'<=)
  (env-bind env '>= #'>=)
  (env-bind env '= #'=)
  (env-bind env 'eq #'eq)
  (env-bind env 'eql #'eql)
  (env-bind env 'equal #'equal)

  ;; List operations
  (env-bind env 'cons #'cons)
  (env-bind env 'car #'car)
  (env-bind env 'cdr #'cdr)
  (env-bind env 'list #'list)
  (env-bind env 'null #'null)
  (env-bind env 'listp #'listp)
  (env-bind env 'consp #'consp)
  (env-bind env 'atom #'atom)
  (env-bind env 'first #'first)
  (env-bind env 'rest #'rest)
  (env-bind env 'length #'length)
  (env-bind env 'append #'append)
  (env-bind env 'reverse #'reverse)
  (env-bind env 'nth #'nth)
  (env-bind env 'nthcdr #'nthcdr)

  ;; Type predicates
  (env-bind env 'numberp #'numberp)
  (env-bind env 'symbolp #'symbolp)
  (env-bind env 'stringp #'stringp)
  (env-bind env 'functionp #'functionp)

  ;; Other utilities
  (env-bind env 'not #'not)
  (env-bind env 'identity #'identity)
  (env-bind env 'funcall #'funcall)
  (env-bind env 'apply #'apply)
  (env-bind env 'format #'format)
  (env-bind env 'type-of #'type-of)
  (env-bind env 'typep #'typep)

  ;; More list accessors
  (env-bind env 'second #'second)
  (env-bind env 'third #'third)
  (env-bind env 'fourth #'fourth)
  (env-bind env 'fifth #'fifth)
  (env-bind env 'last #'last)
  (env-bind env 'butlast #'butlast)
  (env-bind env 'nbutlast #'nbutlast)
  (env-bind env 'cadr #'cadr)
  (env-bind env 'cddr #'cddr)
  (env-bind env 'caar #'caar)
  (env-bind env 'cdar #'cdar)

  ;; Higher-order list functions
  (env-bind env 'mapcar #'mapcar)
  (env-bind env 'mapc #'mapc)
  (env-bind env 'maplist #'maplist)
  (env-bind env 'reduce #'reduce)
  (env-bind env 'remove #'remove)
  (env-bind env 'remove-if #'remove-if)
  (env-bind env 'remove-if-not #'remove-if-not)
  (env-bind env 'delete #'delete)
  (env-bind env 'delete-if #'delete-if)
  (env-bind env 'find #'find)
  (env-bind env 'find-if #'find-if)
  (env-bind env 'find-if-not #'find-if-not)
  (env-bind env 'position #'position)
  (env-bind env 'position-if #'position-if)
  (env-bind env 'count #'count)
  (env-bind env 'count-if #'count-if)
  (env-bind env 'member #'member)
  (env-bind env 'assoc #'assoc)
  (env-bind env 'rassoc #'rassoc)
  (env-bind env 'subst #'subst)
  (env-bind env 'substitute #'substitute)
  (env-bind env 'copy-list #'copy-list)
  (env-bind env 'nconc #'nconc)
  (env-bind env 'every #'every)
  (env-bind env 'some #'some)
  (env-bind env 'notany #'notany)
  (env-bind env 'notevery #'notevery)

  ;; Hash tables
  (env-bind env 'make-hash-table #'make-hash-table)
  (env-bind env 'hash-table-p #'hash-table-p)
  (env-bind env 'gethash #'gethash)
  ;; puthash is not a CL symbol - intern in CL-USER for compatibility
  (env-bind env (intern "PUTHASH" :cl-user)
            (lambda (key table value)
              (setf (gethash key table) value)))
  (env-bind env 'remhash #'remhash)
  (env-bind env 'maphash #'maphash)
  (env-bind env 'hash-table-count #'hash-table-count)
  (env-bind env 'clrhash #'clrhash)

  ;; String functions
  (env-bind env 'string= #'string=)
  (env-bind env 'string-equal #'string-equal)
  (env-bind env 'string< #'string<)
  (env-bind env 'string> #'string>)
  (env-bind env 'string<= #'string<=)
  (env-bind env 'string>= #'string>=)
  (env-bind env 'string-upcase #'string-upcase)
  (env-bind env 'string-downcase #'string-downcase)
  (env-bind env 'string-capitalize #'string-capitalize)
  (env-bind env 'string-trim #'string-trim)
  (env-bind env 'char #'char)
  (env-bind env 'subseq #'subseq)
  (env-bind env 'concatenate #'concatenate)

  ;; Math functions
  (env-bind env 'floor #'floor)
  (env-bind env 'ceiling #'ceiling)
  (env-bind env 'truncate #'truncate)
  (env-bind env 'round #'round)
  (env-bind env 'mod #'mod)
  (env-bind env 'rem #'rem)
  (env-bind env 'abs #'abs)
  (env-bind env 'signum #'signum)
  (env-bind env 'gcd #'gcd)
  (env-bind env 'lcm #'lcm)
  (env-bind env 'max #'max)
  (env-bind env 'min #'min)
  (env-bind env 'expt #'expt)
  (env-bind env 'sqrt #'sqrt)
  (env-bind env 'log #'log)
  (env-bind env 'sin #'sin)
  (env-bind env 'cos #'cos)
  (env-bind env 'zerop #'zerop)
  (env-bind env 'plusp #'plusp)
  (env-bind env 'minusp #'minusp)
  (env-bind env 'evenp #'evenp)
  (env-bind env 'oddp #'oddp)
  (env-bind env '1+ #'1+)
  (env-bind env '1- #'1-)
  (env-bind env 'integerp #'integerp)
  (env-bind env 'floatp #'floatp)
  (env-bind env 'rationalp #'rationalp)
  (env-bind env 'realp #'realp)

  ;; Symbol functions
  (env-bind env 'symbol-name #'symbol-name)
  (env-bind env 'symbol-package #'symbol-package)
  (env-bind env 'symbol-value #'symbol-value)
  (env-bind env 'symbol-function #'symbol-function)
  (env-bind env 'gensym #'gensym)
  (env-bind env 'gentemp #'gentemp)
  (env-bind env 'intern #'intern)
  (env-bind env 'make-symbol #'make-symbol)

  ;; Array functions
  (env-bind env 'make-array #'make-array)
  (env-bind env 'arrayp #'arrayp)
  (env-bind env 'aref #'aref)
  (env-bind env 'array-dimensions #'array-dimensions)
  (env-bind env 'array-dimension #'array-dimension)
  (env-bind env 'array-total-size #'array-total-size)
  (env-bind env 'vectorp #'vectorp)
  (env-bind env 'vector #'vector)
  (env-bind env 'svref #'svref)
  (env-bind env 'elt #'elt)
  (env-bind env 'coerce #'coerce)

  ;; Constants
  (env-bind env 't t)
  (env-bind env 'nil nil))

;;; ============================================================
;;; Interpreter Core
;;; ============================================================

(defvar *default-env* nil
  "Default interpreter environment.")

(defun get-default-env ()
  "Get or create the default interpreter environment."
  (unless *default-env*
    (setf *default-env* (make-interpreter-env)))
  *default-env*)

(defun interpret (expr &optional env)
  "Interpret an S-expression in the given environment."
  (let ((env (or env (get-default-env))))
    (interpret-form expr env)))

(defun interpret-form (form env)
  "Interpret a form in the given environment."
  (cond
    ;; Self-evaluating forms
    ((self-evaluating-p form)
     form)
    ;; Symbol (variable reference)
    ((symbolp form)
     (env-lookup env form))
    ;; List (special form or function call)
    ((consp form)
     (interpret-list form env))
    ;; Unknown
    (t
     (error "Cannot interpret: ~S" form))))

(defun self-evaluating-p (form)
  "Check if FORM is self-evaluating."
  (or (numberp form)
      (stringp form)
      (keywordp form)
      (characterp form)
      (null form)
      (eq form t)
      (vectorp form)))

;;; ============================================================
;;; Special Forms
;;; ============================================================

(defun interpret-list (form env)
  "Interpret a list form (special form or function call)."
  (let ((op (first form)))
    (case op
      ;; Quote
      (quote
       (second form))
      ;; If
      (if
       (interpret-if form env))
      ;; Progn
      (progn
       (interpret-progn (rest form) env))
      ;; Let
      (let
       (interpret-let (second form) (cddr form) env nil))
      ;; Let*
      (let*
       (interpret-let (second form) (cddr form) env t))
      ;; Lambda
      (lambda
       (interpret-lambda (second form) (cddr form) env))
      ;; Setq
      (setq
       (interpret-setq (second form) (third form) env))
      ;; Block
      (block
       (interpret-block (second form) (cddr form) env))
      ;; Return-from
      (return-from
       (interpret-return-from (second form) (third form) env))
      ;; Tagbody
      (tagbody
       (interpret-tagbody (rest form) env))
      ;; Go
      (go
       (interpret-go (second form)))
      ;; Flet
      (flet
       (interpret-flet (second form) (cddr form) env))
      ;; Labels
      (labels
       (interpret-labels (second form) (cddr form) env))
      ;; Function
      (function
       (interpret-function (second form) env))
      ;; Funcall
      (funcall
       (let ((fn (interpret-form (second form) env))
             (args (mapcar (lambda (arg) (interpret-form arg env))
                           (cddr form))))
         (apply fn args)))
      ;; Defun
      (defun
       (interpret-defun (second form) (third form) (cdddr form) env))
      ;; Defmacro
      (defmacro
       (interpret-defmacro (second form) (third form) (cdddr form) env))
      ;; Defstruct
      (defstruct
       (interpret-defstruct (rest form) env))
      ;; Defvar
      (defvar
       (interpret-defvar (second form) (third form) env))
      ;; Defparameter
      (defparameter
       (interpret-defparameter (second form) (third form) env))
      ;; Defconstant
      (defconstant
       (interpret-defconstant (second form) (third form) env))
      ;; Cond
      (cond
       (interpret-cond (rest form) env))
      ;; Case
      (case
       (interpret-case (second form) (cddr form) env nil))
      ;; Ecase
      (ecase
       (interpret-case (second form) (cddr form) env t))
      ;; Typecase
      (typecase
       (interpret-typecase (second form) (cddr form) env nil))
      ;; Etypecase
      (etypecase
       (interpret-typecase (second form) (cddr form) env t))
      ;; Unwind-protect
      (unwind-protect
       (interpret-unwind-protect (second form) (cddr form) env))
      ;; The (type declaration, ignored in interpreter)
      (the
       (interpret-form (third form) env))
      ;; Locally
      (locally
       (interpret-locally (rest form) env))
      ;; Error
      (error
       (apply #'error (mapcar (lambda (arg) (interpret-form arg env))
                              (rest form))))
      ;; Warn
      (warn
       (apply #'warn (mapcar (lambda (arg) (interpret-form arg env))
                             (rest form))))
      ;; Handler-case
      (handler-case
       (interpret-handler-case (second form) (cddr form) env))
      ;; Handler-bind
      (handler-bind
       (interpret-handler-bind (second form) (cddr form) env))
      ;; Restart-case
      (restart-case
       (interpret-restart-case (second form) (cddr form) env))
      ;; Restart-bind
      (restart-bind
       (interpret-restart-bind (second form) (cddr form) env))
      ;; Invoke-restart
      (invoke-restart
       (apply #'invoke-restart (interpret-form (second form) env)
              (mapcar (lambda (arg) (interpret-form arg env))
                      (cddr form))))
      ;; Cerror
      (cerror
       (apply #'cerror (mapcar (lambda (arg) (interpret-form arg env))
                               (rest form))))
      ;; With-simple-restart
      (with-simple-restart
       (interpret-with-simple-restart (second form) (cddr form) env))
      ;; Muffle-warning
      (muffle-warning
       (muffle-warning))
      ;; Multiple-value-list (for testing)
      (multiple-value-list
       (multiple-value-list (interpret-form (second form) env)))
      ;; Values
      (values
       (apply #'values (mapcar (lambda (arg) (interpret-form arg env))
                               (rest form))))
      ;; Multiple-value-bind
      (multiple-value-bind
       (interpret-mvb (second form) (third form) (cdddr form) env))
      ;; Multiple-value-call
      (multiple-value-call
       (interpret-multiple-value-call (second form) (cddr form) env))
      ;; Multiple-value-prog1
      (multiple-value-prog1
       (interpret-multiple-value-prog1 (second form) (cddr form) env))
      ;; Values-list
      (values-list
       (apply #'values (interpret-form (second form) env)))
      ;; Nth-value
      (nth-value
       (interpret-nth-value (second form) (third form) env))
      ;; Loop - delegate to host CL implementation
      (loop
       (interpret-loop-simple form env))
      ;; Dotimes
      (dotimes
       (interpret-dotimes (second form) (cddr form) env))
      ;; Dolist
      (dolist
       (interpret-dolist (second form) (cddr form) env))
      ;; Function application
      (otherwise
       (interpret-application form env)))))

;;; If

(defun interpret-if (form env)
  "Interpret an if form."
  (let ((test (second form))
        (then (third form))
        (else (fourth form)))
    (if (interpret-form test env)
        (interpret-form then env)
        (when else
          (interpret-form else env)))))

;;; Progn

(defun interpret-progn (forms env)
  "Interpret a progn (sequence of forms)."
  (let ((result nil))
    (dolist (form forms result)
      (setf result (interpret-form form env)))))

;;; Let / Let*

(defun interpret-let (bindings body env sequential-p)
  "Interpret a let or let* form."
  (let ((new-env (if sequential-p
                     (interpret-let*-bindings bindings env)
                     (interpret-let-bindings bindings env))))
    (interpret-progn body new-env)))

(defun interpret-let-bindings (bindings env)
  "Process parallel let bindings."
  (let ((pairs (mapcar (lambda (binding)
                         (if (consp binding)
                             (cons (first binding)
                                   (interpret-form (second binding) env))
                             (cons binding nil)))
                       bindings)))
    (extend-env env pairs)))

(defun interpret-let*-bindings (bindings env)
  "Process sequential let* bindings."
  (let ((current-env env))
    (dolist (binding bindings)
      (let* ((name (if (consp binding) (first binding) binding))
             (value (if (consp binding)
                        (interpret-form (second binding) current-env)
                        nil)))
        (setf current-env (extend-env current-env (list (cons name value))))))
    current-env))

;;; Lambda

(defstruct interpreted-closure
  "A closure created by lambda interpretation."
  (params nil :type list)
  (body nil :type list)
  (env nil :type (or null interpreter-env)))

(defun interpret-lambda (params body env)
  "Interpret a lambda expression, returning a closure."
  ;; Return a host function that captures the closure
  (let ((closure (make-interpreted-closure :params params :body body :env env)))
    (lambda (&rest args)
      (apply-interpreted-closure closure args))))

(defun apply-interpreted-closure (closure args)
  "Apply an interpreted closure to arguments."
  (let* ((params (interpreted-closure-params closure))
         (body (interpreted-closure-body closure))
         (env (interpreted-closure-env closure))
         (bindings (mapcar #'cons params args))
         (new-env (extend-env env bindings)))
    (interpret-progn body new-env)))

;;; Function

(defun interpret-function (name env)
  "Interpret a function form (#'name).
NAME can be a symbol or a lambda expression."
  (if (and (consp name) (eq (first name) 'lambda))
      ;; (function (lambda ...)) -> interpret the lambda
      (interpret-lambda (second name) (cddr name) env)
      ;; (function name) -> lookup the function
      (let ((fn (env-lookup env name)))
        (unless fn
          (error "Undefined function: ~S" name))
        fn)))

;;; Setq

(defun interpret-setq (name value-form env)
  "Interpret a setq form."
  (let ((value (interpret-form value-form env)))
    ;; Find and update the binding in the environment chain
    (labels ((find-and-set (e)
               (when e
                 (multiple-value-bind (old-val found)
                     (gethash name (interpreter-env-bindings e))
                   (declare (ignore old-val))
                   (if found
                       (setf (gethash name (interpreter-env-bindings e)) value)
                       (find-and-set (interpreter-env-parent e)))))))
      (if (env-bound-p env name)
          (find-and-set env)
          (env-bind env name value)))
    value))

;;; Block / Return-from

(define-condition block-return ()
  ((block-name :initarg :block-name :reader block-return-name)
   (value :initarg :value :reader block-return-value)))

(defun interpret-block (name body env)
  "Interpret a block form."
  (handler-case
      (progn
        (env-bind env name (list :block name))
        (interpret-progn body env))
    (block-return (c)
      (if (eq name (block-return-name c))
          (block-return-value c)
          (error c)))))

(defun interpret-return-from (name value-form env)
  "Interpret a return-from form."
  (let ((value (interpret-form value-form env)))
    (error 'block-return :block-name name :value value)))

;;; Tagbody / Go

(define-condition go-tag ()
  ((tag :initarg :tag :reader go-tag-name)))

(defun interpret-tagbody (forms env)
  "Interpret a tagbody form."
  (let ((tag-positions (make-hash-table :test 'eq))
        (current-forms forms))
    ;; Build tag position table
    (loop for remaining on forms
          for form = (car remaining)
          when (symbolp form)
            do (setf (gethash form tag-positions) (cdr remaining)))
    ;; Execute
    (loop
      (handler-case
          (progn
            (dolist (form current-forms)
              (unless (symbolp form)
                (interpret-form form env)))
            (return nil))
        (go-tag (c)
          (let ((pos (gethash (go-tag-name c) tag-positions)))
            (if pos
                (setf current-forms pos)
                (error c))))))))

(defun interpret-go (tag)
  "Interpret a go form."
  (error 'go-tag :tag tag))

;;; Flet / Labels

(defun interpret-flet (definitions body env)
  "Interpret a flet form (non-recursive local functions)."
  (let* ((fns (mapcar (lambda (def)
                        (let ((name (first def))
                              (params (second def))
                              (fn-body (cddr def)))
                          (cons name (interpret-lambda params fn-body env))))
                      definitions))
         (new-env (extend-env env fns)))
    (interpret-progn body new-env)))

(defun interpret-labels (definitions body env)
  "Interpret a labels form (recursive local functions)."
  ;; Create environment with forward references
  (let ((new-env (make-interpreter-env env)))
    ;; First pass: bind names to placeholders
    (dolist (def definitions)
      (env-bind new-env (first def) nil))
    ;; Second pass: create closures with new-env and update bindings
    (dolist (def definitions)
      (let* ((name (first def))
             (params (second def))
             (fn-body (cddr def))
             (closure (interpret-lambda params fn-body new-env)))
        (env-bind new-env name closure)))
    (interpret-progn body new-env)))

;;; Cond

(defun interpret-cond (clauses env)
  "Interpret a cond form."
  (dolist (clause clauses nil)
    (let ((test (first clause))
          (body (rest clause)))
      (when (interpret-form test env)
        (return (if body
                    (interpret-progn body env)
                    (interpret-form test env)))))))

;;; Case / Ecase

(defun interpret-case (keyform clauses env error-p)
  "Interpret a case or ecase form.
When ERROR-P is true, signals error if no clause matches."
  (let ((key (interpret-form keyform env)))
    (dolist (clause clauses
                    (when error-p
                      (error "ECASE fell through: ~S" key)))
      (let ((keys (first clause))
            (body (rest clause)))
        (cond
          ;; Otherwise/t clause
          ((or (eq keys 'otherwise) (eq keys 't))
           (return (interpret-progn body env)))
          ;; List of keys
          ((listp keys)
           (when (member key keys :test #'eql)
             (return (interpret-progn body env))))
          ;; Single key
          ((eql key keys)
           (return (interpret-progn body env))))))))

;;; Typecase / Etypecase

(defun interpret-typecase (keyform clauses env error-p)
  "Interpret a typecase or etypecase form.
When ERROR-P is true, signals error if no clause matches."
  (let ((value (interpret-form keyform env)))
    (dolist (clause clauses
                    (when error-p
                      (error "ETYPECASE fell through: ~S of type ~S"
                             value (type-of value))))
      (let ((typespec (first clause))
            (body (rest clause)))
        (cond
          ;; Otherwise/t clause
          ((or (eq typespec 'otherwise) (eq typespec 't))
           (return (interpret-progn body env)))
          ;; Type check
          ((typep value typespec)
           (return (interpret-progn body env))))))))

;;; Unwind-protect

(defun interpret-unwind-protect (protected-form cleanup-forms env)
  "Interpret an unwind-protect form."
  (unwind-protect
      (interpret-form protected-form env)
    (interpret-progn cleanup-forms env)))

;;; Locally

(defun interpret-locally (forms env)
  "Interpret a locally form.
Declarations are ignored; we just execute the body forms."
  ;; Filter out any declare forms
  (let ((body (remove-if (lambda (f)
                           (and (consp f) (eq (first f) 'declare)))
                         forms)))
    (interpret-progn body env)))

;;; Handler-case

(defun interpret-handler-case (expr clauses env)
  "Interpret a handler-case form."
  (handler-case
      (interpret-form expr env)
    (condition (c)
      ;; Find matching handler
      (dolist (clause clauses)
        (let ((typespec (first clause))
              (var-spec (second clause))
              (body (cddr clause)))
          (when (typep c typespec)
            (let* ((var (if (consp var-spec) (first var-spec) nil))
                   (handler-env (if var
                                    (extend-env env (list (cons var c)))
                                    env)))
              (return-from interpret-handler-case
                (interpret-progn body handler-env)))))))))

;;; Handler-bind

(defun interpret-handler-bind (bindings body env)
  "Interpret a handler-bind form."
  ;; Build handler-bind expression with evaluated handlers
  (let ((handlers (mapcar (lambda (binding)
                            (let ((type (first binding))
                                  (handler-form (second binding)))
                              (list type (interpret-form handler-form env))))
                          bindings)))
    ;; Use host handler-bind
    (eval `(handler-bind ,handlers
             (interpret-progn ',body ,env)))))

;;; Restart-case

(defun interpret-restart-case (expr clauses env)
  "Interpret a restart-case form.
Uses host restart-case mechanism."
  ;; We need to capture env at expansion time
  (let ((captured-env env))
    (let ((restart-exprs
            (mapcar (lambda (clause)
                      (let ((name (first clause))
                            (params (second clause))
                            (body (cddr clause)))
                        ;; Create a restart that binds params in env then runs body
                        `(,name (&rest args)
                           (let ((bindings (loop for p in ',params
                                                 for a in args
                                                 collect (cons p a))))
                             (interpret-progn ',body
                                              (extend-env ,captured-env bindings))))))
                    clauses)))
      (eval `(restart-case
                 (interpret-form ',expr ,captured-env)
               ,@restart-exprs)))))

;;; Restart-bind

(defun interpret-restart-bind (bindings body env)
  "Interpret a restart-bind form."
  (let ((restart-exprs
          (mapcar (lambda (binding)
                    (let ((name (first binding))
                          (fn-form (second binding))
                          (options (cddr binding)))
                      `(,name ,(interpret-form fn-form env) ,@options)))
                  bindings)))
    (eval `(restart-bind ,restart-exprs
             (interpret-progn ',body ,env)))))

;;; With-simple-restart

(defun interpret-with-simple-restart (restart-spec body env)
  "Interpret a with-simple-restart form."
  (let ((restart-name (first restart-spec))
        (format-string (second restart-spec)))
    (eval `(with-simple-restart (,restart-name ,format-string)
             (interpret-progn ',body ,env)))))

;;; Multiple-value-bind

(defun interpret-mvb (vars value-form body env)
  "Interpret a multiple-value-bind form."
  (let ((values (multiple-value-list (interpret-form value-form env))))
    (let ((bindings (loop for var in vars
                          for i from 0
                          collect (cons var (nth i values)))))
      (interpret-progn body (extend-env env bindings)))))

(defun interpret-multiple-value-call (fn-form value-forms env)
  "Interpret a multiple-value-call form."
  (let ((fn (interpret-form fn-form env))
        (all-values nil))
    ;; Collect all values from all forms
    (dolist (vf value-forms)
      (setf all-values
            (append all-values
                    (multiple-value-list (interpret-form vf env)))))
    (apply fn all-values)))

(defun interpret-multiple-value-prog1 (first-form rest-forms env)
  "Interpret a multiple-value-prog1 form.
   Returns all values from first-form, evaluates rest for side effects."
  (let ((result (multiple-value-list (interpret-form first-form env))))
    ;; Evaluate rest for side effects
    (dolist (form rest-forms)
      (interpret-form form env))
    ;; Return all values from first form
    (apply #'values result)))

(defun interpret-nth-value (n-form value-form env)
  "Interpret an nth-value form."
  (let ((n (interpret-form n-form env))
        (values (multiple-value-list (interpret-form value-form env))))
    (nth n values)))

;;; Loop constructs

(defun interpret-loop-simple (form env)
  "Interpret a LOOP form by delegating to host CL.
   Handles interpreter variable binding and mutation."
  (let* ((setq-targets (find-setq-targets form))
         (read-vars (find-read-vars form env))
         (result nil))
    ;; Bind setq target variables as special variables
    (dolist (var setq-targets)
      (handler-case
          (let ((val (env-lookup env var)))
            (proclaim `(special ,var))
            (set var val))
        (error ()
          (proclaim `(special ,var))
          (set var nil))))
    ;; Substitute read-only variables in the form
    (let ((substituted (substitute-read-vars form env setq-targets)))
      (setf result (eval substituted)))
    ;; Copy back setq'd variables
    (dolist (var setq-targets)
      (when (boundp var)
        (env-bind env var (symbol-value var))))
    result))

(defun find-read-vars (form env)
  "Find variables that are read (not written) in FORM."
  (let ((read-vars nil)
        (setq-targets (find-setq-targets form))
        (loop-vars (extract-loop-variables form)))
    (labels ((scan (f in-operator-position)
               (cond
                 ((null f) nil)
                 ((and (symbolp f)
                       (not (keywordp f))
                       (not (member f '(t nil)))
                       (not (member f setq-targets))
                       (not (member f loop-vars))
                       (not in-operator-position))
                  (handler-case
                      (progn
                        (env-lookup env f)
                        (pushnew f read-vars))
                    (error () nil)))
                 ((atom f) nil)
                 ((eq (first f) 'quote) nil)
                 ((eq (first f) 'setq)
                  ;; Skip variable position, scan value
                  (scan (third f) nil))
                 (t
                  ;; First element might be an operator
                  (scan (first f) t)
                  (dolist (sub (rest f))
                    (scan sub nil))))))
      (scan form nil))
    read-vars))

(defun substitute-read-vars (form env exclude-vars)
  "Substitute read variables in FORM, excluding EXCLUDE-VARS."
  (let ((loop-vars (extract-loop-variables form)))
    (subst-vars form env (append loop-vars exclude-vars))))

(defun extract-loop-variables (form)
  "Extract variables introduced by LOOP (for, with, etc.)."
  (let ((vars nil)
        (body (if (and (consp form) (eq (first form) 'loop))
                  (rest form)
                  form)))
    (let ((i 0))
      (loop while (< i (length body)) do
        (let ((item (nth i body)))
          (cond
            ((eq item 'for)
             (push (nth (1+ i) body) vars)
             (incf i 2))
            ((eq item 'with)
             (push (nth (1+ i) body) vars)
             (incf i 2))
            ((eq item 'as)
             (push (nth (1+ i) body) vars)
             (incf i 2))
            (t (incf i))))))
    vars))

(defun subst-vars (form env skip-vars &optional in-operator-pos)
  "Substitute variables in FORM with values from ENV, skipping SKIP-VARS.
   IN-OPERATOR-POS means we're in a position where a function name is expected."
  (cond
    ((null form) nil)
    ((and (symbolp form)
          (not in-operator-pos)
          (not (keywordp form))
          (not (member form '(t nil)))
          (not (member form skip-vars)))
     ;; Try to look up in interpreter env
     (handler-case
         (let ((val (env-lookup env form)))
           ;; Only substitute if it's a data value, not a function
           (if (functionp val)
               form  ; Keep the function name symbol
               `',val))
       (error () form)))
    ((atom form) form)
    ((eq (first form) 'quote) form)
    (t
     ;; For lists, first element is in operator position
     (cons (subst-vars (first form) env skip-vars t)
           (mapcar (lambda (sub) (subst-vars sub env skip-vars nil))
                   (rest form))))))

(defun find-setq-targets (form)
  "Find all setq target variables in FORM."
  (let ((targets nil))
    (labels ((scan (f)
               (cond
                 ((null f) nil)
                 ((atom f) nil)
                 ((eq (first f) 'setq)
                  (push (second f) targets)
                  (scan (cdddr f)))
                 (t (scan (first f))
                    (scan (rest f))))))
      (scan form))
    targets))

(defun interpret-loop (loop-body env)
  "Interpret a LOOP form.
   Implements a subset of CL LOOP sufficient for bootstrapping."
  (let ((clauses (parse-loop-clauses loop-body))
        (result nil)
        (initially-forms nil)
        (finally-forms nil))
    ;; Extract initially/finally
    (setf initially-forms (getf clauses :initially))
    (setf finally-forms (getf clauses :finally))
    ;; Execute initially
    (dolist (form initially-forms)
      (interpret-form form env))
    ;; Execute the main loop
    (setf result (execute-loop-body clauses env))
    ;; Execute finally
    (dolist (form finally-forms)
      (interpret-form form env))
    result))

(defun parse-loop-clauses (body)
  "Parse LOOP clauses into a property list."
  (let ((clauses nil)
        (iteration-vars nil)
        (iteration-specs nil)
        (accumulation nil)
        (body-forms nil)
        (end-test nil)
        (initially nil)
        (finally nil)
        (i 0))
    (loop while (< i (length body)) do
      (let ((item (nth i body)))
        (cond
          ;; FOR var FROM/IN/ON ...
          ((eq item 'for)
           (let ((var (nth (1+ i) body))
                 (prep (nth (+ i 2) body)))
             (cond
               ;; FOR var FROM x TO/BELOW/DOWNTO y [BY step]
               ((member prep '(from downfrom))
                (let* ((start-val (nth (+ i 3) body))
                       (to-prep (nth (+ i 4) body)))
                  (if (member to-prep '(to below downto))
                      ;; Full range specification
                      (let* ((end-val (nth (+ i 5) body))
                             (step-info (when (eq (nth (+ i 6) body) 'by)
                                          (list 'by (nth (+ i 7) body))))
                             (advance (if step-info 8 6)))
                        (push (list :for-numeric var prep start-val to-prep end-val
                                    (if step-info (second step-info) 1))
                              iteration-specs)
                        (push var iteration-vars)
                        (incf i advance))
                      ;; No end value - unbounded iteration
                      (progn
                        (push (list :for-unbounded var start-val 1)
                              iteration-specs)
                        (push var iteration-vars)
                        (incf i 4)))))
               ;; FOR var BELOW x
               ((eq prep 'below)
                (let ((end-val (nth (+ i 3) body)))
                  (push (list :for-numeric var 'from 0 'below end-val 1)
                        iteration-specs)
                  (push var iteration-vars)
                  (incf i 4)))
               ;; FOR var IN list
               ((eq prep 'in)
                (let ((list-form (nth (+ i 3) body)))
                  (push (list :for-in var list-form) iteration-specs)
                  (push var iteration-vars)
                  (incf i 4)))
               ;; FOR var ON list
               ((eq prep 'on)
                (let ((list-form (nth (+ i 3) body)))
                  (push (list :for-on var list-form) iteration-specs)
                  (push var iteration-vars)
                  (incf i 4)))
               (t (incf i)))))
          ;; REPEAT n
          ((eq item 'repeat)
           (let ((count-form (nth (1+ i) body)))
             (push (list :repeat count-form) iteration-specs)
             (incf i 2)))
          ;; WHILE test
          ((eq item 'while)
           (setf end-test (list :while (nth (1+ i) body)))
           (incf i 2))
          ;; UNTIL test
          ((eq item 'until)
           (setf end-test (list :until (nth (1+ i) body)))
           (incf i 2))
          ;; COLLECT form
          ((eq item 'collect)
           (push (list :collect (nth (1+ i) body)) accumulation)
           (incf i 2))
          ;; SUM form
          ((eq item 'sum)
           (push (list :sum (nth (1+ i) body)) accumulation)
           (incf i 2))
          ;; COUNT form
          ((eq item 'count)
           (push (list :count (nth (1+ i) body)) accumulation)
           (incf i 2))
          ;; DO forms
          ((eq item 'do)
           (let ((form (nth (1+ i) body)))
             (push (list :do form) body-forms)
             (incf i 2)))
          ;; WHEN condition forms
          ((eq item 'when)
           (let ((test (nth (1+ i) body))
                 (then (nth (+ i 2) body)))
             ;; Handle RETURN inside WHEN
             (if (and (consp then) (eq (car then) 'return))
                 (progn
                   (push (list :when-return test (second then)) body-forms)
                   (incf i 3))
                 (progn
                   (push (list :when test then) body-forms)
                   (incf i 3)))))
          ;; UNLESS condition forms
          ((eq item 'unless)
           (let ((test (nth (1+ i) body))
                 (then (nth (+ i 2) body)))
             (push (list :unless test then) body-forms)
             (incf i 3)))
          ;; RETURN form
          ((eq item 'return)
           (push (list :return (nth (1+ i) body)) body-forms)
           (incf i 2))
          ;; INITIALLY forms
          ((eq item 'initially)
           (push (nth (1+ i) body) initially)
           (incf i 2))
          ;; FINALLY forms
          ((eq item 'finally)
           (push (nth (1+ i) body) finally)
           (incf i 2))
          (t (incf i)))))
    (list :iteration-vars (nreverse iteration-vars)
          :iteration-specs (nreverse iteration-specs)
          :accumulation (nreverse accumulation)
          :body-forms (nreverse body-forms)
          :end-test end-test
          :initially (nreverse initially)
          :finally (nreverse finally))))

(defun execute-loop-body (clauses env)
  "Execute the parsed loop body."
  (let* ((iteration-specs (getf clauses :iteration-specs))
         (accumulation (getf clauses :accumulation))
         (body-forms (getf clauses :body-forms))
         (end-test (getf clauses :end-test))
         (loop-env (make-interpreter-env env))
         (result nil)
         (collected nil)
         (sum-acc 0)
         (count-acc 0)
         (repeat-count nil)
         (repeat-current 0))
    ;; Initialize iteration variables
    (dolist (spec iteration-specs)
      (let ((kind (first spec)))
        (cond
          ((eq kind :for-numeric)
           (let ((var (second spec))
                 (start (interpret-form (fourth spec) env)))
             (env-bind loop-env var start)))
          ((eq kind :for-unbounded)
           (let ((var (second spec))
                 (start (interpret-form (third spec) env)))
             (env-bind loop-env var start)))
          ((member kind '(:for-in :for-on))
           (let ((var (second spec))
                 (list (interpret-form (third spec) env)))
             (env-bind loop-env (intern (format nil "~A-LIST" var)) list)
             (env-bind loop-env var (if (eq kind :for-in) (car list) list))))
          ((eq kind :repeat)
           (setf repeat-count (interpret-form (second spec) env))))))
    ;; Main loop
    (block loop-exit
      (loop
        ;; Check end conditions
        (when repeat-count
          (when (>= repeat-current repeat-count)
            (return-from loop-exit)))
        (when end-test
          (let ((test-result (interpret-form (second end-test) loop-env)))
            (cond
              ((and (eq (first end-test) :while) (not test-result))
               (return-from loop-exit))
              ((and (eq (first end-test) :until) test-result)
               (return-from loop-exit)))))
        ;; Check for-in/on termination
        (dolist (spec iteration-specs)
          (when (member (first spec) '(:for-in :for-on))
            (let* ((var (second spec))
                   (list-var (intern (format nil "~A-LIST" var)))
                   (current-list (env-lookup loop-env list-var)))
              (when (null current-list)
                (return-from loop-exit)))))
        ;; Check for-numeric termination
        (dolist (spec iteration-specs)
          (when (eq (first spec) :for-numeric)
            (let* ((var (second spec))
                   (prep (third spec))
                   (to-prep (fifth spec))
                   (end (interpret-form (sixth spec) env))
                   (current (env-lookup loop-env var)))
              (cond
                ((and (eq to-prep 'to) (> current end))
                 (return-from loop-exit))
                ((and (eq to-prep 'below) (>= current end))
                 (return-from loop-exit))
                ((and (eq to-prep 'downto) (< current end))
                 (return-from loop-exit))))))
        ;; Execute body forms
        (dolist (bf body-forms)
          (let ((kind (first bf)))
            (cond
              ((eq kind :do)
               (interpret-form (second bf) loop-env))
              ((eq kind :when)
               (when (interpret-form (second bf) loop-env)
                 (interpret-form (third bf) loop-env)))
              ((eq kind :when-return)
               (when (interpret-form (second bf) loop-env)
                 (setf result (interpret-form (third bf) loop-env))
                 (return-from loop-exit)))
              ((eq kind :unless)
               (unless (interpret-form (second bf) loop-env)
                 (interpret-form (third bf) loop-env)))
              ((eq kind :return)
               (setf result (interpret-form (second bf) loop-env))
               (return-from loop-exit)))))
        ;; Accumulate
        (dolist (acc accumulation)
          (let ((kind (first acc)))
            (cond
              ((eq kind :collect)
               (push (interpret-form (second acc) loop-env) collected))
              ((eq kind :sum)
               (incf sum-acc (interpret-form (second acc) loop-env)))
              ((eq kind :count)
               (when (interpret-form (second acc) loop-env)
                 (incf count-acc))))))
        ;; Step iteration variables
        (dolist (spec iteration-specs)
          (let ((kind (first spec)))
            (cond
              ((eq kind :for-numeric)
               (let* ((var (second spec))
                      (prep (third spec))
                      (step (seventh spec))
                      (current (env-lookup loop-env var)))
                 (if (eq prep 'downfrom)
                     (env-bind loop-env var (- current step))
                     (env-bind loop-env var (+ current step)))))
              ((eq kind :for-unbounded)
               (let* ((var (second spec))
                      (step (fourth spec))
                      (current (env-lookup loop-env var)))
                 (env-bind loop-env var (+ current step))))
              ((member kind '(:for-in :for-on))
               (let* ((var (second spec))
                      (list-var (intern (format nil "~A-LIST" var)))
                      (current-list (env-lookup loop-env list-var)))
                 (env-bind loop-env list-var (cdr current-list))
                 (env-bind loop-env var
                           (if (eq kind :for-in)
                               (cadr current-list)
                               (cdr current-list))))))))
        (when repeat-count
          (incf repeat-current))))
    ;; Return result
    (cond
      (result result)
      ((some (lambda (acc) (eq (first acc) :collect)) accumulation)
       (nreverse collected))
      ((some (lambda (acc) (eq (first acc) :sum)) accumulation)
       sum-acc)
      ((some (lambda (acc) (eq (first acc) :count)) accumulation)
       count-acc)
      (t nil))))

(defun interpret-dotimes (var-spec body env)
  "Interpret a DOTIMES form."
  (let* ((var (first var-spec))
         (count-form (second var-spec))
         (result-form (third var-spec))
         (count (interpret-form count-form env))
         (loop-env (make-interpreter-env env))
         (result nil))
    (env-bind loop-env var 0)
    (block nil
      (dotimes (i count)
        (env-bind loop-env var i)
        (setf result (interpret-progn body loop-env))))
    (if result-form
        (interpret-form result-form loop-env)
        nil)))

(defun interpret-dolist (var-spec body env)
  "Interpret a DOLIST form."
  (let* ((var (first var-spec))
         (list-form (second var-spec))
         (result-form (third var-spec))
         (list (interpret-form list-form env))
         (loop-env (make-interpreter-env env))
         (result nil))
    (dolist (item list)
      (env-bind loop-env var item)
      (setf result (interpret-progn body loop-env)))
    (if result-form
        (interpret-form result-form loop-env)
        nil)))

;;; Function Application

(defun interpret-application (form env)
  "Interpret a function application.
First checks if the operator is a macro and expands it if so."
  (let* ((op (first form)))
    ;; Check if it's a macro call first
    (multiple-value-bind (macro-info found)
        (when (symbolp op)
          (gethash op *interpreter-macros*))
      (if found
          ;; Macro call: expand and interpret
          (let ((expanded (expand-macro-call op (rest form) macro-info env)))
            (interpret-form expanded env))
          ;; Not a macro: regular function application
          (let* ((fn (cond
                       ((symbolp op) (env-lookup env op))
                       ((and (consp op) (eq (first op) 'lambda))
                        (interpret-lambda (second op) (cddr op) env))
                       (t (error "Invalid function: ~S" op))))
                 (args (mapcar (lambda (arg) (interpret-form arg env))
                               (rest form))))
            (unless (functionp fn)
              (error "Not a function: ~S" fn))
            (apply fn args))))))

;;; Macro Expansion

(defun expand-macro-call (name args macro-info env)
  "Expand a macro call given the macro info (lambda-list . body)."
  (let* ((lambda-list (car macro-info))
         (body (cdr macro-info))
         (bindings (bind-macro-args lambda-list args (cons name args))))
    ;; Create environment for macro expansion with bindings
    (let ((expansion-env (extend-env env bindings)))
      ;; Evaluate macro body to produce expansion
      (interpret-progn body expansion-env))))

(defun bind-macro-args (lambda-list args &optional whole-form)
  "Bind macro arguments to lambda-list.
Handles &whole, &body, &rest, &optional, &key.
WHOLE-FORM is optional for compatibility with apply-macro-expander.
Returns alist of bindings."
  (let ((bindings nil)
        (remaining lambda-list)
        (remaining-args args))
    ;; Check for &whole
    (when (and remaining (eq (first remaining) '&whole))
      (push (cons (second remaining) whole-form) bindings)
      (setf remaining (cddr remaining)))
    ;; Check for &environment (ignored for now - we don't pass env object to macros)
    (when (member '&environment remaining)
      (let ((pos (position '&environment remaining)))
        (setf remaining (append (subseq remaining 0 pos)
                               (subseq remaining (+ pos 2))))))
    ;; Process rest of lambda-list
    (loop while remaining do
      (let ((item (pop remaining)))
        (cond
          ;; &body or &rest - bind remaining args
          ((member item '(&body &rest))
           (push (cons (pop remaining) remaining-args) bindings)
           (setf remaining-args nil))
          ;; &optional
          ((eq item '&optional)
           (loop while (and remaining
                           (not (member (first remaining) '(&body &rest &key &aux))))
                 do (let* ((spec (pop remaining))
                          (name (if (consp spec) (first spec) spec))
                          (default (when (consp spec) (second spec))))
                      (if remaining-args
                          (push (cons name (pop remaining-args)) bindings)
                          (push (cons name default) bindings)))))
          ;; &key
          ((eq item '&key)
           (loop while (and remaining
                           (not (member (first remaining) '(&aux &allow-other-keys))))
                 do (let* ((spec (pop remaining))
                          (name (if (consp spec) (first spec) spec))
                          (keyword (intern (symbol-name name) :keyword))
                          (default (when (consp spec) (second spec)))
                          (val (getf remaining-args keyword :not-found)))
                      (push (cons name (if (eq val :not-found) default val))
                            bindings))))
          ;; &allow-other-keys - skip
          ((eq item '&allow-other-keys)
           nil)
          ;; &aux
          ((eq item '&aux)
           (loop while remaining
                 do (let* ((spec (pop remaining))
                          (name (if (consp spec) (first spec) spec))
                          (val (when (consp spec) (second spec))))
                      (push (cons name val) bindings))))
          ;; Regular required parameter
          (t
           (push (cons item (pop remaining-args)) bindings)))))
    (nreverse bindings)))

;;; ============================================================
;;; Definition Forms (Phase 3: T029-T034)
;;; ============================================================

;;; Defun

(defun interpret-defun (name lambda-list body env)
  "Interpret a defun form.
Creates a function and binds it in the environment."
  (let* ((ll-info (parse-lambda-list lambda-list))
         (closure (make-interpreted-closure
                   :params lambda-list
                   :body body
                   :env env)))
    ;; Create a wrapper function that uses bind-lambda-list-args
    (let ((fn (lambda (&rest args)
                (let* ((bindings (bind-lambda-list-args ll-info args
                                                        (interpreted-closure-env closure)))
                       (call-env (extend-env (interpreted-closure-env closure) bindings)))
                  ;; Now bind &aux parameters in the new environment
                  (dolist (aux-spec (lambda-list-info-aux ll-info))
                    (destructuring-bind (aux-name aux-form) aux-spec
                      (env-bind call-env aux-name (interpret-form aux-form call-env))))
                  (interpret-progn (interpreted-closure-body closure) call-env)))))
      (env-bind env name fn)
      name)))

;;; Defmacro
;;; Note: Full macro support is in interpreter-macros.lisp
;;; This is a minimal implementation that stores macros for later expansion

(defun interpret-defmacro (name lambda-list body env)
  "Interpret a defmacro form.
Creates a macro expander and registers it in the simple registry."
  (declare (ignore env))
  ;; Store macro as (lambda-list . body) for later expansion
  (setf (gethash name *interpreter-macros*) (cons lambda-list body))
  name)

;;; Defstruct

(defun interpret-defstruct (args env)
  "Interpret a defstruct form.
Generates constructor, predicate, and accessors."
  (let* ((name-spec (first args))
         (name (if (consp name-spec) (first name-spec) name-spec))
         (slot-specs (rest args))
         (slot-names (mapcar (lambda (spec)
                               (if (consp spec) (first spec) spec))
                             slot-specs))
         (slot-defaults (mapcar (lambda (spec)
                                  (if (consp spec) (second spec) nil))
                                slot-specs)))
    ;; Register the struct type with slot name/default pairs
    (let ((slot-pairs (mapcar #'cons slot-names slot-defaults)))
      (setf (gethash name *struct-registry*)
            (%make-interpreter-struct-type
             :name name
             :slots slot-pairs
             :constructor (intern (format nil "MAKE-~A" name))
             :predicate (intern (format nil "~A-P" name)))))
    ;; Create constructor (make-NAME)
    (let ((constructor-name (intern (format nil "MAKE-~A" name)))
          (struct-type (gethash name *struct-registry*)))
      (env-bind env constructor-name
                (lambda (&rest initargs)
                  (let ((slot-values (make-array (length slot-names) :initial-element nil)))
                    ;; Initialize slots to defaults
                    (loop for i from 0
                          for (sn . sd) in (interpreter-struct-type-slots struct-type)
                          do (setf (aref slot-values i)
                                   (if sd (interpret-form sd env) nil)))
                    ;; Apply initargs
                    (loop for (key val) on initargs by #'cddr
                          for slot-name = (intern (symbol-name key))
                          for idx = (position slot-name slot-names)
                          when idx do (setf (aref slot-values idx) val))
                    (%make-interpreter-struct-instance
                     :type struct-type
                     :slots slot-values)))))
    ;; Create predicate (NAME-p)
    (let ((predicate-name (intern (format nil "~A-P" name)))
          (struct-type (gethash name *struct-registry*)))
      (env-bind env predicate-name
                (lambda (obj)
                  (and (interpreter-struct-instance-p obj)
                       (eq (interpreter-struct-instance-type obj) struct-type)))))
    ;; Create accessors (NAME-SLOT)
    (loop for slot in slot-names
          for idx from 0
          for accessor-name = (intern (format nil "~A-~A" name slot))
          do (let ((i idx))  ;; Capture index for closure
               (env-bind env accessor-name
                         (lambda (instance)
                           (aref (interpreter-struct-instance-slots instance) i)))))
    name))

;;; Defvar

(defun interpret-defvar (name initial-value env)
  "Interpret a defvar form.
Only sets the value if not already bound."
  (unless (gethash name *special-variables*)
    (let ((value (when initial-value
                   (interpret-form initial-value env))))
      (setf (gethash name *special-variables*) value)))
  ;; Also bind in current environment for immediate use
  (env-bind env name (gethash name *special-variables*))
  name)

;;; Defparameter

(defun interpret-defparameter (name initial-value env)
  "Interpret a defparameter form.
Always sets the value (unlike defvar)."
  (let ((value (interpret-form initial-value env)))
    (setf (gethash name *special-variables*) value)
    (env-bind env name value))
  name)

;;; Defconstant

(defun interpret-defconstant (name value env)
  "Interpret a defconstant form.
Sets a constant that cannot be changed."
  (let ((computed-value (interpret-form value env)))
    (setf (gethash name *constants*) computed-value)
    (env-bind env name computed-value))
  name)
