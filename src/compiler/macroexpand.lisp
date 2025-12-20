;;;; macroexpand.lisp - Self-hosted macro expansion engine
;;;;
;;;; Replaces dependency on host Lisp's macroexpand-1 for self-hosting.

(in-package #:clysm/compiler)

;;; Macro Definition Table

(defparameter *macro-table* (make-hash-table :test 'eq)
  "Hash table mapping macro names to their expander functions.
   Each expander is a function (lambda (form) ...) that returns expanded form.")

(defun reset-macro-table ()
  "Reset the macro table for a fresh compilation."
  (setf *macro-table* (make-hash-table :test 'eq)))

(defun macro-function-p (name)
  "Return the macro expander function for NAME, or NIL if not a macro."
  (gethash name *macro-table*))

(defun register-macro (name expander)
  "Register a macro expander function for NAME."
  (setf (gethash name *macro-table*) expander))

;;; Backquote Expansion
;;;
;;; Transforms (BACKQUOTE form) into list-building code.
;;; (UNQUOTE x) => x
;;; (SPLICE-UNQUOTE x) => ,@x (spliced into list)

(defun expand-backquote (form)
  "Expand a backquoted form into list-building code."
  (cond
    ;; SBCL COMMA object - convert to unquote/splice-unquote
    #+sbcl
    ((typep form 'sb-impl::comma)
     (let ((expr (sb-impl::comma-expr form))
           (kind (sb-impl::comma-kind form)))
       (if (= kind 0)
           ;; Regular unquote - return the expression directly
           expr
           ;; Splice unquote - wrap so expand-backquote-list can detect it
           `(splice-unquote ,expr))))
    ;; Atom - just quote it
    ((atom form)
     (if (or (null form) (eq form t) (numberp form) (stringp form))
         form
         `(quote ,form)))
    ;; Unquote - return the form directly
    ((eq (car form) 'unquote)
     (second form))
    ;; Splice-unquote at top level is an error
    ((eq (car form) 'splice-unquote)
     (error "Splice-unquote ,@ not inside list"))
    ;; List - build with list/cons/append
    (t
     (expand-backquote-list form))))

(defun expand-backquote-list (forms)
  "Expand a backquoted list into list-building code."
  (let ((segments nil)
        (current-items nil))
    ;; Group elements into segments
    ;; Each segment is either a list of quoted/unquoted items or a splice
    (dolist (item forms)
      (cond
        ;; SBCL COMMA object with splice (kind != 0)
        #+sbcl
        ((and (typep item 'sb-impl::comma)
              (/= (sb-impl::comma-kind item) 0))
         (when current-items
           (push (cons :items (nreverse current-items)) segments)
           (setf current-items nil))
         (push (cons :splice (sb-impl::comma-expr item)) segments))
        ;; Splice-unquote - flush current items and add splice
        ((and (consp item) (eq (car item) 'splice-unquote))
         (when current-items
           (push (cons :items (nreverse current-items)) segments)
           (setf current-items nil))
         (push (cons :splice (second item)) segments))
        ;; Regular item - add to current
        (t
         (push (expand-backquote item) current-items))))
    ;; Flush remaining items
    (when current-items
      (push (cons :items (nreverse current-items)) segments))
    ;; Build the result
    (setf segments (nreverse segments))
    (cond
      ;; No segments - empty list
      ((null segments)
       nil)
      ;; Single items segment - use list
      ((and (= (length segments) 1)
            (eq (car (first segments)) :items))
       (let ((items (cdr (first segments))))
         (if (every #'constantp-for-backquote items)
             `(quote ,(mapcar #'eval-constant-for-backquote items))
             `(list ,@items))))
      ;; Has splices - use append
      (t
       (let ((args nil))
         (dolist (seg segments)
           (if (eq (car seg) :items)
               (let ((items (cdr seg)))
                 (if (every #'constantp-for-backquote items)
                     (push `(quote ,(mapcar #'eval-constant-for-backquote items)) args)
                     (push `(list ,@items) args)))
               ;; Splice
               (push (cdr seg) args)))
         `(append ,@(nreverse args)))))))

(defun constantp-for-backquote (form)
  "Check if a form is constant for backquote expansion."
  (or (null form)
      (eq form t)
      (numberp form)
      (stringp form)
      (and (consp form)
           (eq (car form) 'quote))))

(defun eval-constant-for-backquote (form)
  "Evaluate a constant form for backquote optimization."
  (cond
    ((or (null form) (eq form t) (numberp form) (stringp form))
     form)
    ((and (consp form) (eq (car form) 'quote))
     (second form))
    (t form)))

;;; Macro Lambda List Parsing
;;;
;;; Supports: required, &optional, &rest, &body, &key, &whole

(defun parse-macro-lambda-list (lambda-list)
  "Parse a macro lambda list. Returns a plist with:
   :whole, :required, :optional, :rest, :body, :key"
  (let ((whole nil)
        (required nil)
        (optional nil)
        (rest nil)
        (body nil)
        (key nil)
        (state :required))
    (dolist (item lambda-list)
      (cond
        ;; &whole - must be first
        ((eq item '&whole)
         (setf state :whole))
        ((eq state :whole)
         (setf whole item)
         (setf state :required))
        ;; &optional
        ((eq item '&optional)
         (setf state :optional))
        ;; &rest
        ((eq item '&rest)
         (setf state :rest))
        ;; &body (same as &rest for macros)
        ((eq item '&body)
         (setf state :body))
        ;; &key
        ((eq item '&key)
         (setf state :key))
        ;; Regular parameter
        (t
         (case state
           (:required (push item required))
           (:optional (push item optional))
           (:rest (setf rest item))
           (:body (setf body item))
           (:key (push item key))))))
    (list :whole whole
          :required (nreverse required)
          :optional (nreverse optional)
          :rest rest
          :body body
          :key (nreverse key))))

(defun generate-macro-bindings (lambda-list form-var)
  "Generate let* bindings to destructure FORM-VAR according to LAMBDA-LIST.
   Returns (bindings . body-var) where bindings is a list for let*."
  (let* ((parsed (parse-macro-lambda-list lambda-list))
         (whole (getf parsed :whole))
         (required (getf parsed :required))
         (optional (getf parsed :optional))
         (rest-var (or (getf parsed :rest) (getf parsed :body)))
         (bindings nil)
         (rest-form form-var))
    ;; &whole binding
    (when whole
      (push (list whole form-var) bindings))
    ;; Skip the macro name (car of form)
    (let ((args-var (gensym "ARGS")))
      (push (list args-var `(cdr ,form-var)) bindings)
      (setf rest-form args-var)
      ;; Required parameters
      (dolist (param required)
        (if (consp param)
            ;; Destructuring - need recursive handling
            (let ((temp (gensym "TEMP")))
              (push (list temp `(car ,rest-form)) bindings)
              ;; TODO: recursive destructuring
              (push (list param temp) bindings))
            ;; Simple binding
            (push (list param `(car ,rest-form)) bindings))
        (let ((next (gensym "REST")))
          (push (list next `(cdr ,rest-form)) bindings)
          (setf rest-form next)))
      ;; Optional parameters
      (dolist (opt-spec optional)
        (let* ((param (if (consp opt-spec) (car opt-spec) opt-spec))
               (default (if (consp opt-spec) (cadr opt-spec) nil))
               (supplied-p (if (and (consp opt-spec) (cddr opt-spec))
                               (caddr opt-spec)
                               nil)))
          (when supplied-p
            (push (list supplied-p `(not (null ,rest-form))) bindings))
          (push (list param `(if ,rest-form (car ,rest-form) ,default)) bindings)
          (let ((next (gensym "REST")))
            (push (list next `(if ,rest-form (cdr ,rest-form) nil)) bindings)
            (setf rest-form next))))
      ;; &rest or &body
      (when rest-var
        (push (list rest-var rest-form) bindings)))
    (nreverse bindings)))

;;; Self-hosted macroexpand-1

(defun clysm-macroexpand-1 (form)
  "Expand one macro call in FORM. Returns (values expanded-form expanded-p)."
  (cond
    ;; Not a cons - no expansion
    ((atom form)
     (values form nil))
    ;; Check for macro
    (t
     (let ((expander (macro-function-p (car form))))
       (if expander
           ;; Call the expander
           (values (funcall expander form) t)
           ;; Not a macro
           (values form nil))))))

;;; defmacro Registration
;;;
;;; Parse defmacro form and register the macro expander.

(defun register-defmacro (form)
  "Process a defmacro form and register the macro.
   Form is (defmacro name lambda-list . body)"
  (destructuring-bind (name lambda-list &rest body) (cdr form)
    (let* ((form-var (gensym "FORM"))
           (bindings (generate-macro-bindings lambda-list form-var))
           ;; Build the expander function
           (expander-body
             `(let* ,bindings
                ,@body)))
      ;; Create and register the expander
      (let ((expander (eval `(lambda (,form-var) ,expander-body))))
        (register-macro name expander)))))

;;; Standard Macros
;;;
;;; Bootstrap macros that should be available without explicit defmacro.

(defun install-standard-macros ()
  "Install the standard CL macros that we support."
  ;; WHEN
  (register-macro 'when
    (lambda (form)
      (let ((test (second form))
            (body (cddr form)))
        `(if ,test (progn ,@body) nil))))
  ;; UNLESS
  (register-macro 'unless
    (lambda (form)
      (let ((test (second form))
            (body (cddr form)))
        `(if ,test nil (progn ,@body)))))
  ;; COND
  (register-macro 'cond
    (lambda (form)
      (expand-cond (cdr form))))
  ;; AND
  (register-macro 'and
    (lambda (form)
      (expand-and (cdr form))))
  ;; OR
  (register-macro 'or
    (lambda (form)
      (expand-or (cdr form))))
  ;; DOTIMES
  (register-macro 'dotimes
    (lambda (form)
      (expand-dotimes (second form) (cddr form))))
  ;; DOLIST
  (register-macro 'dolist
    (lambda (form)
      (expand-dolist (second form) (cddr form))))
  ;; PUSH
  (register-macro 'push
    (lambda (form)
      (let ((item (second form))
            (place (third form)))
        `(setf ,place (cons ,item ,place)))))
  ;; POP
  (register-macro 'pop
    (lambda (form)
      (let ((place (second form)))
        (let ((temp (gensym)))
          `(let ((,temp (car ,place)))
             (setf ,place (cdr ,place))
             ,temp)))))
  ;; INCF
  (register-macro 'incf
    (lambda (form)
      (let ((place (second form))
            (delta (or (third form) 1)))
        `(setf ,place (+ ,place ,delta)))))
  ;; DECF
  (register-macro 'decf
    (lambda (form)
      (let ((place (second form))
            (delta (or (third form) 1)))
        `(setf ,place (- ,place ,delta)))))
  ;; TYPEP - compile-time expansion to predicate calls
  (register-macro 'typep
    (lambda (form)
      (let ((obj (second form))
            (type-spec (third form)))
        (expand-typep obj type-spec))))
  ;; TYPECASE
  (register-macro 'typecase
    (lambda (form)
      (let ((keyform (second form))
            (clauses (cddr form)))
        (expand-typecase keyform clauses nil))))
  ;; ETYPECASE
  (register-macro 'etypecase
    (lambda (form)
      (let ((keyform (second form))
            (clauses (cddr form)))
        (expand-typecase keyform clauses t))))
  ;; CASE
  (register-macro 'case
    (lambda (form)
      (let ((keyform (second form))
            (clauses (cddr form)))
        (expand-case keyform clauses nil))))
  ;; ECASE
  (register-macro 'ecase
    (lambda (form)
      (let ((keyform (second form))
            (clauses (cddr form)))
        (expand-case keyform clauses t)))))

(defun expand-cond (clauses)
  "Expand cond clauses into nested if forms."
  (if (null clauses)
      nil
      (let* ((clause (first clauses))
             (test (first clause))
             (body (rest clause)))
        (if (eq test t)
            `(progn ,@body)
            (if (null body)
                ;; (cond (test)) - return test value if true
                (let ((temp (gensym)))
                  `(let ((,temp ,test))
                     (if ,temp ,temp ,(expand-cond (rest clauses)))))
                `(if ,test
                     (progn ,@body)
                     ,(expand-cond (rest clauses))))))))

(defun expand-case (keyform clauses error-p)
  "Expand CASE or ECASE into cond form.
   If ERROR-P is true, generate error for no match (ecase)."
  (let ((temp (gensym "KEYFORM")))
    `(let ((,temp ,keyform))
       (cond
         ,@(mapcar
            (lambda (clause)
              (let ((keys (first clause))
                    (body (rest clause)))
                (cond
                  ;; otherwise/t clause
                  ((or (eq keys 'otherwise) (eq keys 't))
                   `(t ,@body))
                  ;; Single key
                  ((atom keys)
                   `((eql ,temp ',keys) ,@body))
                  ;; Multiple keys
                  (t
                   `((or ,@(mapcar (lambda (k) `(eql ,temp ',k)) keys))
                     ,@body)))))
            clauses)
         ;; Error clause for ecase
         ,@(when error-p
             `((t (error "~S fell through ECASE expression. Wanted one of ~S"
                         ,temp ',(mapcar #'first clauses)))))))))

(defun expand-and (forms)
  "Expand AND into nested if forms."
  (cond
    ((null forms) t)
    ((null (cdr forms)) (first forms))
    (t `(if ,(first forms)
            ,(expand-and (cdr forms))
            nil))))

(defun expand-or (forms)
  "Expand OR into nested if/let forms."
  (cond
    ((null forms) nil)
    ((null (cdr forms)) (first forms))
    (t (let ((temp (gensym)))
         `(let ((,temp ,(first forms)))
            (if ,temp ,temp ,(expand-or (cdr forms))))))))

(defun expand-dotimes (var-spec body)
  "Expand DOTIMES into block/tagbody/go."
  (let* ((var (first var-spec))
         (count-form (second var-spec))
         (result (if (cddr var-spec) (third var-spec) nil))
         (count-var (gensym "COUNT"))
         (loop-tag (gensym "LOOP"))
         (end-tag (gensym "END")))
    `(block nil
       (let ((,count-var ,count-form)
             (,var 0))
         (tagbody
            ,loop-tag
            (if (>= ,var ,count-var)
                (go ,end-tag))
            ,@body
            (setq ,var (1+ ,var))
            (go ,loop-tag)
            ,end-tag)
         ,result))))

(defun expand-dolist (var-spec body)
  "Expand DOLIST into block/tagbody/go."
  (let* ((var (first var-spec))
         (list-form (second var-spec))
         (result (if (cddr var-spec) (third var-spec) nil))
         (list-var (gensym "LIST"))
         (loop-tag (gensym "LOOP"))
         (end-tag (gensym "END")))
    `(block nil
       (let ((,list-var ,list-form)
             (,var nil))
         (tagbody
            ,loop-tag
            (if (null ,list-var)
                (go ,end-tag))
            (setq ,var (car ,list-var))
            ,@body
            (setq ,list-var (cdr ,list-var))
            (go ,loop-tag)
            ,end-tag)
         ,result))))

;;; Type Predicate Expansion

(defun type-to-predicate (type-name)
  "Map a type name to its predicate function."
  (case type-name
    (cons 'consp)
    (list 'listp)
    (null 'null)
    (atom 'atom)
    (symbol 'symbolp)
    (keyword 'keywordp)
    (number 'numberp)
    (integer 'integerp)
    (fixnum 'integerp)  ; In WASM, all integers are fixnums
    (float 'floatp)
    (single-float 'floatp)
    (double-float 'floatp)
    (string 'stringp)
    (character 'characterp)
    (function 'functionp)
    (hash-table 'hash-table-p)
    (array 'arrayp)
    (vector 'vectorp)
    (t nil)))  ; Return nil for unknown types

(defun expand-typep (obj type-spec)
  "Expand (typep obj type-spec) into a predicate call.
   TYPE-SPEC should be quoted, e.g., (typep x 'cons)"
  (cond
    ;; (typep x 't) - always true
    ((eq type-spec 't)
     t)
    ;; (typep x 'type) - quoted type specifier
    ((and (consp type-spec)
          (eq (car type-spec) 'quote)
          (symbolp (second type-spec)))
     (let* ((type-name (second type-spec))
            (predicate (type-to-predicate type-name)))
       (cond
         ;; Known predicate
         (predicate
          `(,predicate ,obj))
         ;; null special case
         ((eq type-name 'null)
          `(null ,obj))
         ;; t - always true
         ((eq type-name 't)
          t)
         ;; Unknown type - return nil for SBCL-specific types, etc.
         ;; In self-hosted environment, these types won't exist
         (t
          ;; For types from external packages (like sb-impl::comma), return nil
          (warn "Unknown type specifier ~S - will always return NIL" type-name)
          nil))))
    ;; Compound type specifiers like (or ...) (and ...)
    ((and (consp type-spec)
          (eq (car type-spec) 'quote)
          (consp (second type-spec)))
     (let ((compound (second type-spec)))
       (case (car compound)
         ;; (or type1 type2 ...)
         (or
          (let ((temp (gensym "OBJ")))
            `(let ((,temp ,obj))
               (or ,@(mapcar (lambda (subtype)
                               (expand-typep temp `(quote ,subtype)))
                             (cdr compound))))))
         ;; (and type1 type2 ...)
         (and
          (let ((temp (gensym "OBJ")))
            `(let ((,temp ,obj))
               (and ,@(mapcar (lambda (subtype)
                                (expand-typep temp `(quote ,subtype)))
                              (cdr compound))))))
         ;; (not type)
         (not
          `(not ,(expand-typep obj `(quote ,(second compound)))))
         ;; (member item1 item2 ...) - check if obj is one of the items
         (member
          (let ((temp (gensym "OBJ")))
            `(let ((,temp ,obj))
               (or ,@(mapcar (lambda (item)
                               `(eql ,temp ',item))
                             (cdr compound))))))
         ;; (eql value) - check if obj equals value
         (eql
          `(eql ,obj ',(second compound)))
         ;; (unsigned-byte n) - check if obj is an integer in range
         (unsigned-byte
          (let ((bits (second compound)))
            (let ((temp (gensym "OBJ")))
              `(let ((,temp ,obj))
                 (and (integerp ,temp)
                      (>= ,temp 0)
                      (< ,temp ,(expt 2 bits)))))))
         ;; (signed-byte n) - check if obj is an integer in range
         (signed-byte
          (let ((bits (second compound)))
            (let ((temp (gensym "OBJ"))
                  (limit (expt 2 (1- bits))))
              `(let ((,temp ,obj))
                 (and (integerp ,temp)
                      (>= ,temp ,(- limit))
                      (< ,temp ,limit))))))
         ;; Unknown compound type
         (t
          (error "Unknown compound type specifier: ~S" compound)))))
    ;; Unquoted type name (direct symbol)
    ((symbolp type-spec)
     (let ((predicate (type-to-predicate type-spec)))
       (if predicate
           `(,predicate ,obj)
           (error "Unknown type specifier: ~S" type-spec))))
    ;; Other cases
    (t
     (error "Invalid type specifier: ~S" type-spec))))

(defun expand-typecase (keyform clauses error-p)
  "Expand TYPECASE or ETYPECASE into cond form.
   If ERROR-P is true, generate error for no match (etypecase)."
  (let ((temp (gensym "KEYFORM")))
    `(let ((,temp ,keyform))
       (cond
         ,@(mapcar
            (lambda (clause)
              (let ((type-spec (first clause))
                    (body (rest clause)))
                (cond
                  ;; otherwise/t clause
                  ((or (eq type-spec 'otherwise) (eq type-spec 't))
                   `(t ,@body))
                  ;; Regular type clause
                  (t
                   `(,(expand-typep temp `(quote ,type-spec))
                     ,@body)))))
            clauses)
         ;; Error clause for etypecase
         ,@(when error-p
             `((t (error "~S fell through ETYPECASE expression. Wanted one of ~S"
                         ,temp ',(mapcar #'first clauses)))))))))
