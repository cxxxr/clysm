;;;; destructuring.lisp - Destructuring lambda-list parsing utilities
;;;; Phase 9B - destructuring-bind macro support (031-destructuring-bind-macro)

(in-package #:clysm/lib/destructuring)

;;; ============================================================
;;; Parsed Lambda-List Structure
;;; ============================================================

(defstruct parsed-lambda-list
  "Intermediate representation of a destructuring lambda-list after parsing.
   Stores all components extracted from the lambda-list pattern."
  (whole-var nil :type (or null symbol))
  (required-params nil :type list)
  (optional-params nil :type list)
  (rest-var nil :type (or null symbol))
  (key-params nil :type list)
  (allow-other-keys-p nil :type boolean))

;;; ============================================================
;;; Parameter Specification Structures
;;; ============================================================

(defstruct param-spec
  "Represents a required positional parameter.
   TYPE: :variable for simple symbols, :nested for nested patterns
   VAR: The variable symbol (when type = :variable)
   NESTED-LIST: A parsed-lambda-list (when type = :nested)"
  (type :variable :type keyword)
  (var nil :type (or null symbol))
  (nested-list nil :type (or null parsed-lambda-list)))

(defstruct optional-param-spec
  "Represents an &optional parameter with optional default and supplied-p.
   PARAM: The parameter (param-spec - variable or nested)
   DEFAULT-FORM: Expression evaluated if not supplied (default: nil)
   SUPPLIED-P: Variable bound to T/NIL indicating if supplied"
  (param nil :type (or null param-spec))
  (default-form nil :type t)
  (supplied-p nil :type (or null symbol)))

(defstruct key-param-spec
  "Represents a &key parameter with keyword name, default, and supplied-p.
   KEYWORD: The keyword symbol used in the plist (:foo)
   PARAM: The variable or nested pattern (param-spec)
   DEFAULT-FORM: Expression evaluated if not supplied
   SUPPLIED-P: Variable bound to T/NIL indicating if supplied"
  (keyword nil :type (or null keyword))
  (param nil :type (or null param-spec))
  (default-form nil :type t)
  (supplied-p nil :type (or null symbol)))

;;; ============================================================
;;; Lambda-List Keyword Detection
;;; ============================================================

(defun lambda-list-keyword-p (item)
  "Return true if ITEM is a lambda-list keyword."
  (member item '(&whole &optional &rest &body &key &allow-other-keys)))

;;; ============================================================
;;; Parameter Parsing
;;; ============================================================

(defun parse-param (item)
  "Parse a single required parameter.
   ITEM: Either a symbol (variable) or a list (nested pattern).
   Returns: A param-spec structure."
  (cond
    ((symbolp item)
     ;; Simple variable
     (make-param-spec :type :variable :var item))
    ((listp item)
     ;; Nested destructuring pattern
     (make-param-spec :type :nested
                      :nested-list (parse-destructuring-lambda-list item)))
    (t
     (error 'program-error
            :format-control "Invalid destructuring parameter: ~S"
            :format-arguments (list item)))))

(defun parse-optional-param (item)
  "Parse an &optional parameter specification.
   Formats: var | (var) | (var default) | (var default supplied-p)
   Where var can be a symbol or nested pattern.
   Returns: An optional-param-spec structure."
  (cond
    ((symbolp item)
     ;; Simple variable, no default, no supplied-p
     (make-optional-param-spec
      :param (make-param-spec :type :variable :var item)
      :default-form nil
      :supplied-p nil))
    ((listp item)
     (let ((var-part (first item))
           (default (second item))
           (supplied-p (third item)))
       (make-optional-param-spec
        :param (parse-param var-part)
        :default-form default
        :supplied-p supplied-p)))
    (t
     (error 'program-error
            :format-control "Invalid &optional parameter: ~S"
            :format-arguments (list item)))))

(defun parse-key-param (item)
  "Parse a &key parameter specification.
   Formats: var | (var default) | (var default supplied-p) |
            ((keyword var) default) | ((keyword var) default supplied-p)
   Returns: A key-param-spec structure."
  (cond
    ((symbolp item)
     ;; Simple variable, keyword derived from name
     (make-key-param-spec
      :keyword (intern (symbol-name item) :keyword)
      :param (make-param-spec :type :variable :var item)
      :default-form nil
      :supplied-p nil))
    ((listp item)
     (let ((var-part (first item))
           (default (second item))
           (supplied-p (third item)))
       (cond
         ;; ((keyword var) ...) form
         ((and (listp var-part) (= 2 (length var-part)))
          (let ((keyword (first var-part))
                (var (second var-part)))
            (make-key-param-spec
             :keyword (if (keywordp keyword)
                          keyword
                          (intern (symbol-name keyword) :keyword))
             :param (parse-param var)
             :default-form default
             :supplied-p supplied-p)))
         ;; (var ...) form
         ((symbolp var-part)
          (make-key-param-spec
           :keyword (intern (symbol-name var-part) :keyword)
           :param (make-param-spec :type :variable :var var-part)
           :default-form default
           :supplied-p supplied-p))
         ;; (nested-list default) form - not supported
         ((listp var-part)
          (error 'program-error
                 :format-control "Nested patterns in &key require explicit keyword: ~S"
                 :format-arguments (list item)))
         (t
          (error 'program-error
                 :format-control "Invalid &key parameter: ~S"
                 :format-arguments (list item))))))
    (t
     (error 'program-error
            :format-control "Invalid &key parameter: ~S"
            :format-arguments (list item)))))

;;; ============================================================
;;; Lambda-List Parsing (State Machine)
;;; ============================================================

(defun parse-destructuring-lambda-list (lambda-list)
  "Parse a destructuring lambda-list into a parsed-lambda-list structure.
   Uses a state machine as specified in data-model.md:
   START -> AFTER-WHOLE -> REQUIRED -> OPTIONAL -> REST -> KEY -> ALLOW-KEYS -> END

   Signals PROGRAM-ERROR for:
   - Invalid lambda-list keywords
   - Keywords in wrong order
   - Duplicate keywords"
  (let ((state :start)
        (whole-var nil)
        (required-params nil)
        (optional-params nil)
        (rest-var nil)
        (key-params nil)
        (allow-other-keys-p nil)
        (remaining lambda-list))

    ;; Handle dotted list case - convert to proper list with &rest
    (when (and (listp remaining) (not (null remaining)))
      (let ((last-cons (last remaining)))
        (when (and last-cons (cdr last-cons) (not (listp (cdr last-cons))))
          ;; Dotted list: (a b . c) -> (a b &rest c)
          (setf remaining
                (append (butlast remaining)
                        (list (car last-cons) '&rest (cdr last-cons)))))))

    (loop while remaining do
      (let ((item (car remaining)))
        (cond
          ;; &whole - must be first
          ((eq item '&whole)
           (unless (eq state :start)
             (error 'program-error
                    :format-control "&whole must appear first in lambda-list"))
           (setf remaining (cdr remaining))
           (unless remaining
             (error 'program-error
                    :format-control "&whole requires a variable name"))
           (setf whole-var (car remaining))
           (unless (symbolp whole-var)
             (error 'program-error
                    :format-control "&whole variable must be a symbol, got: ~S"
                    :format-arguments (list whole-var)))
           (setf remaining (cdr remaining))
           (setf state :after-whole))

          ;; &optional
          ((eq item '&optional)
           (unless (member state '(:start :after-whole :required))
             (error 'program-error
                    :format-control "&optional in wrong position"))
           (setf remaining (cdr remaining))
           (setf state :optional)
           ;; Collect optional params until next keyword or end
           (loop while (and remaining (not (lambda-list-keyword-p (car remaining))))
                 do (push (parse-optional-param (car remaining)) optional-params)
                    (setf remaining (cdr remaining)))
           (setf optional-params (nreverse optional-params)))

          ;; &rest or &body
          ((member item '(&rest &body))
           (unless (member state '(:start :after-whole :required :optional))
             (error 'program-error
                    :format-control "~S in wrong position" :format-arguments (list item)))
           (setf remaining (cdr remaining))
           (unless remaining
             (error 'program-error
                    :format-control "~S requires a variable name" :format-arguments (list item)))
           (setf rest-var (car remaining))
           (unless (symbolp rest-var)
             (error 'program-error
                    :format-control "~S variable must be a symbol, got: ~S"
                    :format-arguments (list item rest-var)))
           (setf remaining (cdr remaining))
           (setf state :rest))

          ;; &key
          ((eq item '&key)
           (unless (member state '(:start :after-whole :required :optional :rest))
             (error 'program-error
                    :format-control "&key in wrong position"))
           (setf remaining (cdr remaining))
           (setf state :key)
           ;; Collect key params until next keyword or end
           (loop while (and remaining (not (lambda-list-keyword-p (car remaining))))
                 do (push (parse-key-param (car remaining)) key-params)
                    (setf remaining (cdr remaining)))
           (setf key-params (nreverse key-params)))

          ;; &allow-other-keys
          ((eq item '&allow-other-keys)
           (unless (eq state :key)
             (error 'program-error
                    :format-control "&allow-other-keys must follow &key"))
           (setf allow-other-keys-p t)
           (setf remaining (cdr remaining))
           (setf state :allow-keys))

          ;; Regular parameter (required)
          ((not (lambda-list-keyword-p item))
           (unless (member state '(:start :after-whole :required))
             (error 'program-error
                    :format-control "Required parameter ~S in wrong position"
                    :format-arguments (list item)))
           (push (parse-param item) required-params)
           (setf remaining (cdr remaining))
           (setf state :required))

          ;; Unknown lambda-list keyword
          (t
           (error 'program-error
                  :format-control "Unknown lambda-list keyword: ~S"
                  :format-arguments (list item))))))

    (make-parsed-lambda-list
     :whole-var whole-var
     :required-params (nreverse required-params)
     :optional-params optional-params
     :rest-var rest-var
     :key-params key-params
     :allow-other-keys-p allow-other-keys-p)))

;;; ============================================================
;;; Code Generation (Outside-In with List Threading)
;;; ============================================================

(defun generate-destructuring-code (parsed-ll list-form body)
  "Generate code to destructure LIST-FORM according to PARSED-LL.
   Returns a form that binds all variables and evaluates BODY.

   PARSED-LL: A parsed-lambda-list structure
   LIST-FORM: The form to destructure (usually a gensym-bound variable)
   BODY: The body forms to evaluate after binding

   Code generation works OUTSIDE-IN:
   1. &whole binding (outermost)
   2. Required bindings (consume from list, pass remainder)
   3. &optional bindings (consume from remainder, pass new remainder)
   4. &rest/&body binding (bind to final remainder)
   5. &key bindings (extract from plist in &rest or remaining)"
  (let* ((whole-var (parsed-lambda-list-whole-var parsed-ll))
         (required (parsed-lambda-list-required-params parsed-ll))
         (optional (parsed-lambda-list-optional-params parsed-ll))
         (rest-var (parsed-lambda-list-rest-var parsed-ll))
         (key-params (parsed-lambda-list-key-params parsed-ll))
         (allow-other-keys-p (parsed-lambda-list-allow-other-keys-p parsed-ll))
         (initial-list (gensym "LIST-"))
         (has-rest-or-key-p (or rest-var key-params))
         (has-more-p (or optional rest-var key-params)))

    ;; Build the innermost continuation (body + &key if present)
    (labels ((make-final-code (final-list-var)
               ;; Generate the innermost code: &key bindings around body
               (if key-params
                   (generate-key-bindings key-params final-list-var
                                          allow-other-keys-p body)
                   (cons 'progn body)))

             (wrap-rest (list-var continuation-fn)
               ;; Wrap with &rest binding, passing bound var to continuation
               (if rest-var
                   `(let ((,rest-var ,list-var))
                      ,(funcall continuation-fn rest-var))
                   (funcall continuation-fn list-var)))

             (generate-optional-chain (params list-var continuation-fn)
               ;; Generate optional bindings, passing final remainder to continuation
               (if (null params)
                   (wrap-rest list-var continuation-fn)
                   (let* ((param (first params))
                          (remaining (rest params))
                          (pspec (optional-param-spec-param param))
                          (default (optional-param-spec-default-form param))
                          (supplied-p (optional-param-spec-supplied-p param))
                          (next-var (gensym "REST-")))
                     (case (param-spec-type pspec)
                       (:variable
                        (let ((var (param-spec-var pspec)))
                          (if supplied-p
                              `(if (consp ,list-var)
                                   (let ((,var (car ,list-var))
                                         (,supplied-p t)
                                         (,next-var (cdr ,list-var)))
                                     ,(generate-optional-chain remaining next-var continuation-fn))
                                   (let ((,var ,default)
                                         (,supplied-p nil)
                                         (,next-var nil))
                                     ,(generate-optional-chain remaining next-var continuation-fn)))
                              `(let* ((,var (if (consp ,list-var) (car ,list-var) ,default))
                                      (,next-var (if (consp ,list-var) (cdr ,list-var) nil)))
                                 ,(generate-optional-chain remaining next-var continuation-fn)))))
                       (:nested
                        (let ((elem-var (gensym "ELEM-")))
                          `(if (consp ,list-var)
                               (let ((,elem-var (car ,list-var))
                                     (,next-var (cdr ,list-var))
                                     ,@(when supplied-p `((,supplied-p t))))
                                 ,(generate-destructuring-code
                                   (param-spec-nested-list pspec)
                                   elem-var
                                   (list (generate-optional-chain remaining next-var continuation-fn))))
                               (let ((,elem-var ,default)
                                     (,next-var nil)
                                     ,@(when supplied-p `((,supplied-p nil))))
                                 ,(generate-destructuring-code
                                   (param-spec-nested-list pspec)
                                   elem-var
                                   (list (generate-optional-chain remaining next-var continuation-fn)))))))))))

             (generate-required-chain (params list-var continuation-fn)
               ;; Generate required bindings, passing final remainder to continuation
               (if (null params)
                   ;; All required consumed - continue to optional/rest/key or check excess
                   (if has-more-p
                       ;; Has optional/rest/key - continue processing
                       (if optional
                           (generate-optional-chain optional list-var continuation-fn)
                           (wrap-rest list-var continuation-fn))
                       ;; No optional/rest/key - check for excess elements
                       `(if (consp ,list-var)
                            (error 'program-error
                                   :format-control "Too many elements for destructuring")
                            ,(funcall continuation-fn list-var)))
                   (let* ((param (first params))
                          (remaining (rest params))
                          (next-var (gensym "REST-")))
                     (case (param-spec-type param)
                       (:variable
                        `(if (consp ,list-var)
                             (let ((,(param-spec-var param) (car ,list-var))
                                   (,next-var (cdr ,list-var)))
                               ,(generate-required-chain remaining next-var continuation-fn))
                             (error 'program-error
                                    :format-control "Too few elements for destructuring: expected ~A"
                                    :format-arguments (list ',(param-spec-var param)))))
                       (:nested
                        (let ((elem-var (gensym "ELEM-")))
                          `(if (consp ,list-var)
                               (let ((,elem-var (car ,list-var))
                                     (,next-var (cdr ,list-var)))
                                 ,(generate-destructuring-code
                                   (param-spec-nested-list param)
                                   elem-var
                                   (list (generate-required-chain remaining next-var continuation-fn))))
                               (error 'program-error
                                      :format-control "Too few elements for destructuring")))))))))

      ;; Build the code by chaining phases
      (let ((core-code
              (cond
                ;; Has required params - start chain from required
                (required
                 `(let ((,initial-list ,list-form))
                    ,(generate-required-chain required initial-list #'make-final-code)))
                ;; No required, but has optional
                (optional
                 `(let ((,initial-list ,list-form))
                    ,(generate-optional-chain optional initial-list #'make-final-code)))
                ;; No required or optional - just rest/key
                (t
                 `(let ((,initial-list ,list-form))
                    ,(wrap-rest initial-list #'make-final-code))))))

        ;; Wrap with &whole binding
        (if whole-var
            `(let ((,whole-var ,list-form))
               ,core-code)
            core-code)))))

(defun generate-key-bindings (params list-var allow-other-keys-p body)
  "Generate code to bind keyword parameters from LIST-VAR (plist).
   PARAMS: List of key-param-spec structures
   LIST-VAR: Symbol naming the variable holding the plist
   ALLOW-OTHER-KEYS-P: Whether to skip unknown key validation
   BODY: Continuation after binding
   Returns: Code that extracts keyword values from plist."
  (if (null params)
      ;; All keys bound - continue to body
      ;; TODO: Add unknown key validation when allow-other-keys-p is nil
      (cons 'progn body)
      (let* ((param (first params))
             (remaining (rest params))
             (keyword (key-param-spec-keyword param))
             (pspec (key-param-spec-param param))
             (default (key-param-spec-default-form param))
             (supplied-p (key-param-spec-supplied-p param))
             (found-var (gensym "FOUND-"))
             (val-var (gensym "VAL-")))
        (case (param-spec-type pspec)
          (:variable
           (let ((var (param-spec-var pspec)))
             (if supplied-p
                 `(let* ((,found-var (getf ,list-var ,keyword '%not-found%))
                         (,supplied-p (not (eq ,found-var '%not-found%)))
                         (,var (if ,supplied-p ,found-var ,default)))
                    ,(generate-key-bindings remaining list-var allow-other-keys-p body))
                 `(let ((,var (getf ,list-var ,keyword ,default)))
                    ,(generate-key-bindings remaining list-var allow-other-keys-p body)))))
          (:nested
           `(let* ((,found-var (getf ,list-var ,keyword '%not-found%))
                   ,@(when supplied-p `((,supplied-p (not (eq ,found-var '%not-found%)))))
                   (,val-var (if (eq ,found-var '%not-found%) ,default ,found-var)))
              ,(generate-destructuring-code
                (param-spec-nested-list pspec)
                val-var
                (list (generate-key-bindings remaining list-var allow-other-keys-p body)))))))))
