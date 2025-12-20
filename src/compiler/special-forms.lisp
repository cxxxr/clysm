;;;; special-forms.lisp - Special form compilation

(in-package #:clysm/compiler)

;;; Multiple value globals - set by setup-runtime in compiler.lisp
(defvar *mv-count-global* nil "Global index for multiple value count.")
(defvar *mv-globals* nil "List of global indices for multiple values (0-based).")
(defconstant +max-multiple-values+ 8 "Maximum number of multiple values supported.")

;;; Catch/throw globals - set by setup-runtime in compiler.lisp
(defvar *throw-pending-global* nil "Global index for throw pending flag.")
(defvar *throw-tag-global* nil "Global index for throw tag.")
(defvar *throw-value-global* nil "Global index for throw value.")

;;; Special Form Handlers

(defparameter *special-forms*
  (make-hash-table :test 'eq)
  "Hash table mapping special form symbols to their compilers.")

(defmacro define-special-form (name (form env) &body body)
  "Define a compiler for a special form."
  `(setf (gethash ',name *special-forms*)
         (lambda (,form ,env)
           ,@body)))

(defun special-form-p (symbol)
  "Check if SYMBOL names a special form."
  (gethash symbol *special-forms*))

(defun compile-special-form (form env)
  "Compile a special form."
  (let ((handler (gethash (car form) *special-forms*)))
    (if handler
        (funcall handler form env)
        (error "Unknown special form: ~A" (car form)))))

;;; IF

(defun env-increment-block-depth (env)
  "Increment block depth for entering a WASM block scope."
  (let ((new-env (copy-compile-env env)))
    (incf (compile-env-block-depth new-env))
    new-env))

(define-special-form if (form env)
  (destructuring-bind (test then &optional else) (cdr form)
    (let* ((test-code (compile-form test env))
           ;; WASM 'if' creates a block scope, so increment depth for branches
           (branch-env (env-increment-block-depth env))
           (then-code (compile-form then branch-env))
           (else-code (if else
                          (compile-form else branch-env)
                          `((,+op-i32-const+ 0)))))  ; nil = 0
      `(,@test-code
        (,+op-if+ ,+type-i32+)
        ,@then-code
        ,+op-else+
        ,@else-code
        ,+op-end+))))

;;; LET

(define-special-form let (form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (let ((new-env env)
          (init-code nil)
          (local-indices nil))
      ;; Process bindings
      (dolist (binding bindings)
        (let* ((name (if (consp binding) (car binding) binding))
               (init (if (consp binding) (cadr binding) nil)))
          (multiple-value-bind (env* index)
              (env-add-local new-env name +type-i32+)
            (setf new-env env*)
            (push index local-indices)
            (when init
              (let ((init-code* (compile-form init env)))
                (setf init-code
                      (append init-code
                              init-code*
                              `((,+op-local-set+ ,index)))))))))
      ;; Compile body
      (let ((body-code (compile-progn body new-env)))
        (append init-code body-code)))))

;;; PROGN

(define-special-form progn (form env)
  (compile-progn (cdr form) env))

(defun compile-progn (forms env)
  "Compile a sequence of forms, returning the value of the last."
  (if (null forms)
      `((,+op-i32-const+ 0))  ; nil
      (let ((code nil))
        (loop for (form . rest) on forms
              for form-code = (compile-form form env)
              do (setf code (append code form-code))
                 ;; Drop intermediate values
                 (when rest
                   (setf code (append code `(,+op-drop+)))))
        code)))

;;; SETQ

(define-special-form setq (form env)
  (destructuring-bind (var value) (cdr form)
    ;; Check for local first, then global
    (let ((local-info (env-lookup env var :variable)))
      (if local-info
          (let ((value-code (compile-form value env)))
            `(,@value-code
              (,+op-local-tee+ ,(local-info-index local-info))))
          ;; Check for global
          (let ((global-info (env-lookup env var :global)))
            (if global-info
                (if (global-info-mutable global-info)
                    (let ((value-code (compile-form value env)))
                      `(,@value-code
                        (,+op-global-set+ ,(global-info-index global-info))
                        (,+op-global-get+ ,(global-info-index global-info))))
                    (error "Cannot assign to constant: ~A" var))
                (error "Undefined variable: ~A" var)))))))

;;; DEFPARAMETER / DEFCONSTANT / DEFVAR

(define-special-form defparameter (form env)
  "Define a global parameter (mutable global variable).
   (defparameter name value [doc-string])"
  (destructuring-bind (name value &optional doc) (cdr form)
    (declare (ignore doc))
    ;; For now, only support constant integer values
    (unless (integerp value)
      (error "defparameter currently only supports integer values: ~A" value))
    (env-add-global env name +type-i32+ t value :constant-p nil)
    ;; Return the symbol name as a value
    `((,+op-i32-const+ 0))))

(define-special-form defconstant (form env)
  "Define a named constant (immutable global).
   (defconstant name value [doc-string])"
  (destructuring-bind (name value &optional doc) (cdr form)
    (declare (ignore doc))
    ;; For now, only support constant integer values
    (unless (integerp value)
      (error "defconstant currently only supports integer values: ~A" value))
    (env-add-global env name +type-i32+ nil value :constant-p t)
    ;; Return the symbol name as a value
    `((,+op-i32-const+ 0))))

(define-special-form defvar (form env)
  "Define a special variable (only if not already defined).
   (defvar name [value [doc-string]])
   For now, acts like defparameter."
  (destructuring-bind (name &optional (value 0) doc) (cdr form)
    (declare (ignore doc))
    ;; Check if already defined
    (let ((existing (env-lookup env name :global)))
      (if existing
          ;; Already defined, do nothing
          `((,+op-i32-const+ 0))
          (progn
            ;; For now, only support constant integer values
            (unless (integerp value)
              (error "defvar currently only supports integer values: ~A" value))
            (env-add-global env name +type-i32+ t value :constant-p nil)
            `((,+op-i32-const+ 0)))))))

;;; QUOTE

(defun compile-string-literal (str env)
  "Compile a string literal. Allocates string in heap at runtime.
   String layout: [length:i32][utf8-bytes...]"
  (declare (ignorable env))
  (let ((bytes (flexi-streams:string-to-octets str :external-format :utf-8)))
    ;; Generate code to allocate and initialize string
    `(;; Save heap pointer (return value = string address)
      (,+op-global-get+ ,*heap-pointer-global*)
      ;; Store length at offset 0
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-i32-const+ ,(length bytes))
      (,+op-i32-store+ 2 0)
      ;; Store each byte at offset 4+i
      ,@(loop for byte across bytes
              for i from 0
              collect `(,+op-global-get+ ,*heap-pointer-global*)
              collect `(,+op-i32-const+ ,byte)
              collect `(,+op-i32-store8+ 0 ,(+ 4 i)))
      ;; Increment heap pointer (4 bytes for length + string bytes, aligned to 4)
      (,+op-global-get+ ,*heap-pointer-global*)
      (,+op-i32-const+ ,(+ 4 (logand (+ (length bytes) 3) (lognot 3))))
      ,+op-i32-add+
      (,+op-global-set+ ,*heap-pointer-global*))))

(defun compile-quoted-value (value env)
  "Compile a quoted value to code that constructs it at runtime."
  (cond
    ;; NIL
    ((null value)
     `((,+op-i32-const+ 0)))
    ;; T (true)
    ((eq value t)
     `((,+op-i32-const+ 1)))
    ;; Integer
    ((integerp value)
     `((,+op-i32-const+ ,value)))
    ;; Float
    ((floatp value)
     `((,+op-f64-const+ ,(float value 1.0d0))))
    ;; String literal
    ((stringp value)
     (compile-string-literal value env))
    ;; List - build using cons at runtime
    ((consp value)
     (let ((car-code (compile-quoted-value (car value) env))
           (cdr-code (compile-quoted-value (cdr value) env)))
       ;; Use the cons primitive to build the cell
       `(;; Get heap pointer (return value)
         (,+op-global-get+ ,*heap-pointer-global*)
         ;; Store car
         (,+op-global-get+ ,*heap-pointer-global*)
         ,@car-code
         (,+op-i32-store+ 2 0)
         ;; Store cdr
         (,+op-global-get+ ,*heap-pointer-global*)
         ,@cdr-code
         (,+op-i32-store+ 2 4)
         ;; Increment heap pointer
         (,+op-global-get+ ,*heap-pointer-global*)
         (,+op-i32-const+ ,*cons-size*)
         ,+op-i32-add+
         (,+op-global-set+ ,*heap-pointer-global*))))
    ;; Symbol - intern at compile time and return address
    ((symbolp value)
     (let* ((sym-info (intern-compile-time-symbol value))
            (sym-addr (first sym-info)))
       `((,+op-i32-const+ ,sym-addr))))
    ;; Other
    (t
     (error "Cannot quote: ~A" value))))

(define-special-form quote (form env)
  (let ((value (cadr form)))
    (compile-quoted-value value env)))

;;; WHEN and UNLESS

(define-special-form when (form env)
  (destructuring-bind (test &rest body) (cdr form)
    ;; (when test body...) => (if test (progn body...) nil)
    (compile-form `(if ,test (progn ,@body) nil) env)))

(define-special-form unless (form env)
  (destructuring-bind (test &rest body) (cdr form)
    ;; (unless test body...) => (if test nil (progn body...))
    (compile-form `(if ,test nil (progn ,@body)) env)))

;;; COND

(define-special-form cond (form env)
  (let ((clauses (cdr form)))
    (if (null clauses)
        ;; No clauses - return NIL
        `((,+op-i32-const+ 0))
        ;; Transform to nested if
        (let ((clause (first clauses)))
          (if (eq (car clause) t)
              ;; (t body...) - always execute
              (compile-progn (cdr clause) env)
              ;; (test body...) => (if test (progn body...) (cond rest...))
              (compile-form
               `(if ,(car clause)
                    (progn ,@(or (cdr clause) (list (car clause))))
                    (cond ,@(rest clauses)))
               env))))))

;;; AND

(define-special-form and (form env)
  (let ((args (cdr form)))
    (cond
      ((null args)
       ;; (and) => t
       `((,+op-i32-const+ 1)))
      ((null (cdr args))
       ;; (and x) => x
       (compile-form (first args) env))
      (t
       ;; (and x y ...) => (if x (and y ...) nil)
       (compile-form `(if ,(first args)
                          (and ,@(rest args))
                          nil)
                     env)))))

;;; OR

(define-special-form or (form env)
  (let ((args (cdr form)))
    (cond
      ((null args)
       ;; (or) => nil
       `((,+op-i32-const+ 0)))
      ((null (cdr args))
       ;; (or x) => x
       (compile-form (first args) env))
      (t
       ;; (or x y ...) => (let ((temp x)) (if temp temp (or y ...)))
       ;; Simplified: since we don't have side effects in expressions yet,
       ;; we can evaluate x twice
       (compile-form `(if ,(first args)
                          ,(first args)
                          (or ,@(rest args)))
                     env)))))

;;; LET*

(define-special-form let* (form env)
  (destructuring-bind (bindings &rest body) (cdr form)
    (if (null bindings)
        ;; No bindings, just compile body
        (compile-progn body env)
        ;; Process first binding, then recurse
        (let* ((binding (first bindings))
               (name (if (consp binding) (car binding) binding))
               (init (if (consp binding) (cadr binding) nil)))
          (multiple-value-bind (new-env index)
              (env-add-local env name +type-i32+)
            (let ((init-code (if init
                                 (append (compile-form init env)
                                         `((,+op-local-set+ ,index)))
                                 nil))
                  ;; Compile rest as nested let*
                  (rest-code (compile-special-form
                              `(let* ,(rest bindings) ,@body)
                              new-env)))
              (append init-code rest-code)))))))

;;; BLOCK and RETURN-FROM

(defun env-add-block (env name)
  "Add a named block to the environment. Returns new env."
  (let ((new-env (copy-compile-env env)))
    (push (cons name (compile-env-block-depth env))
          (compile-env-blocks new-env))
    (incf (compile-env-block-depth new-env))
    new-env))

(defun env-lookup-block (env name)
  "Look up a block by name. Returns the relative branch depth or nil."
  (let ((entry (assoc name (compile-env-blocks env))))
    (when entry
      ;; Calculate relative depth: current depth - block depth - 1
      (- (compile-env-block-depth env) (cdr entry) 1))))

(define-special-form block (form env)
  "Compile a block that can be exited with return-from."
  (destructuring-bind (name &rest body) (cdr form)
    (let* ((block-env (env-add-block env name))
           (body-code (compile-progn body block-env)))
      ;; Wrap in a WASM block
      `((,+op-block+ ,+type-i32+)
        ,@body-code
        ,+op-end+))))

(define-special-form return-from (form env)
  "Compile a return-from that exits a named block."
  (destructuring-bind (name &optional value) (cdr form)
    (let ((depth (env-lookup-block env name)))
      (unless depth
        (error "No block named ~A in scope" name))
      (let ((value-code (if value
                            (compile-form value env)
                            `((,+op-i32-const+ 0)))))
        `(,@value-code
          (,+op-br+ ,depth))))))

(define-special-form return (form env)
  "Compile a return (return-from nil)."
  (compile-special-form `(return-from nil ,@(cdr form)) env))

;;; TAGBODY and GO
;;; Uses state-machine approach since WASM doesn't have arbitrary goto

(defvar *tagbody-tags* nil
  "Dynamically bound alist of (tag-name . state-number) for current tagbody.")

(defvar *tagbody-state-local* nil
  "Dynamically bound index of the state variable local for current tagbody.")

(defun tagbody-tag-p (form)
  "Check if FORM is a tag (symbol or integer, not a list)."
  (or (symbolp form) (integerp form)))

(defun parse-tagbody (body)
  "Parse tagbody body into list of (tag . forms).
   Returns ((nil form1 form2...) (tag1 form3 form4...) (tag2 ...))."
  (let ((sections nil)
        (current-tag nil)
        (current-forms nil))
    (dolist (item body)
      (if (tagbody-tag-p item)
          (progn
            ;; Save previous section
            (push (cons current-tag (nreverse current-forms)) sections)
            ;; Start new section
            (setf current-tag item)
            (setf current-forms nil))
          ;; Add to current section
          (push item current-forms)))
    ;; Save final section
    (push (cons current-tag (nreverse current-forms)) sections)
    (nreverse sections)))

(define-special-form tagbody (form env)
  "Compile a tagbody using state-machine approach."
  (let* ((body (cdr form))
         (sections (parse-tagbody body)))
    (if (null sections)
        ;; Empty tagbody
        `((,+op-i32-const+ 0))  ; returns nil
        ;; Build tag->state mapping
        (let* ((tags-alist nil)
               (state 0))
          ;; First section (before any tag) has state 0
          (dolist (section sections)
            (when (car section)  ; Has a tag
              (push (cons (car section) state) tags-alist))
            (incf state))
          (setf tags-alist (nreverse tags-alist))

          ;; Create state variable
          (multiple-value-bind (env1 state-idx)
              (env-add-local env (gensym "TAGBODY-STATE") +type-i32+)
            (let* ((*tagbody-tags* tags-alist)
                   (*tagbody-state-local* state-idx)
                   ;; Increment block depth for loop structure
                   (loop-env (env-increment-block-depth
                              (env-increment-block-depth env1)))
                   (num-states (length sections)))
              ;; Generate code
              `(;; Initialize state to 0
                (,+op-i32-const+ 0)
                (,+op-local-set+ ,state-idx)
                ;; Outer block for exit
                (,+op-block+ ,+type-void+)
                  ;; Main loop
                  (,+op-loop+ ,+type-void+)
                    ;; Dispatch based on state using br_table
                    ,@(generate-tagbody-dispatch sections loop-env state-idx num-states)
                  ,+op-end+  ; end loop
                ,+op-end+  ; end block
                ;; Tagbody returns nil
                (,+op-i32-const+ 0))))))))

(defun generate-tagbody-dispatch (sections env state-idx num-states)
  "Generate dispatch code for tagbody states."
  ;; For simplicity, use nested if-else instead of br_table
  ;; (br_table would be more efficient for many states)
  (let ((code nil)
        (state 0))
    (dolist (section sections)
      (let* ((forms (cdr section))
             (section-code (if forms
                               (compile-progn forms env)
                               nil)))
        ;; if state == this-state, execute forms
        (setf code
              (append code
                      `((,+op-local-get+ ,state-idx)
                        (,+op-i32-const+ ,state)
                        ,+op-i32-eq+
                        (,+op-if+ ,+type-void+)
                          ,@(if section-code
                                (append section-code `(,+op-drop+))
                                nil)
                          ;; Advance to next state (or exit if last)
                          ,(if (= state (1- num-states))
                               ;; Last state - exit loop
                               `(,+op-br+ 2)  ; break to outer block
                               ;; Advance to next state and continue
                               `(progn
                                  (,+op-i32-const+ ,(1+ state))
                                  (,+op-local-set+ ,state-idx)
                                  (,+op-br+ 1)))  ; continue loop
                        ,+op-end+)))
        (incf state)))
    ;; Flatten any progn markers - use mapcan to splice lists
    (mapcan (lambda (item)
              (if (and (consp item) (eq (car item) 'progn))
                  (copy-list (cdr item))  ; splice the progn contents
                  (list item)))           ; wrap single items in list
            code)))

(define-special-form go (form env)
  "Compile a go that jumps to a tag in the enclosing tagbody."
  (declare (ignore env))
  (let* ((tag (second form))
         (entry (assoc tag *tagbody-tags*)))
    (unless entry
      (error "No tag ~A in enclosing tagbody" tag))
    (let ((state (cdr entry)))
      `(;; Set state and branch to loop start
        (,+op-i32-const+ ,state)
        (,+op-local-set+ ,*tagbody-state-local*)
        (,+op-br+ 0)))))  ; branch to loop (innermost)

;;; DOTIMES

(define-special-form dotimes (form env)
  "Compile (dotimes (var count [result]) body...).
  Executes body count times with var bound to 0, 1, ..., count-1."
  (destructuring-bind ((var count &optional result) &rest body) (cdr form)
    ;; We need two locals: one for var, one for limit
    (multiple-value-bind (env1 var-idx)
        (env-add-local env var +type-i32+)
      (multiple-value-bind (env2 limit-idx)
          (env-add-local env1 (gensym "LIMIT") +type-i32+)
        ;; Increment block depth for the block and loop
        (let* ((loop-env (env-increment-block-depth
                          (env-increment-block-depth env2)))
               (body-code (compile-progn body loop-env))
               (result-code (if result
                                (compile-form result env2)
                                `((,+op-i32-const+ 0)))))
          `(;; Initialize var to 0
            (,+op-i32-const+ 0)
            (,+op-local-set+ ,var-idx)
            ;; Evaluate count and store in limit
            ,@(compile-form count env)
            (,+op-local-set+ ,limit-idx)
            ;; Loop structure: block wraps loop for exit
            (,+op-block+ ,+type-void+)  ; exit block
            (,+op-loop+ ,+type-void+)   ; continue loop
            ;; Check: if var >= limit, exit
            (,+op-local-get+ ,var-idx)
            (,+op-local-get+ ,limit-idx)
            ,+op-i32-ge-s+
            (,+op-br-if+ 1)  ; br to exit block (depth 1)
            ;; Execute body (drop result since dotimes is for side effects)
            ,@body-code
            ,+op-drop+
            ;; Increment var
            (,+op-local-get+ ,var-idx)
            (,+op-i32-const+ 1)
            ,+op-i32-add+
            (,+op-local-set+ ,var-idx)
            ;; Branch back to loop start
            (,+op-br+ 0)  ; br to loop (depth 0)
            ,+op-end+  ; end loop
            ,+op-end+  ; end block
            ;; Return result
            ,@result-code))))))

;;; DOLIST

(define-special-form dolist (form env)
  "Compile (dolist (var list [result]) body...).
  Iterates over list elements."
  (destructuring-bind ((var list-form &optional result) &rest body) (cdr form)
    ;; We need two locals: one for var (current element), one for the list tail
    (multiple-value-bind (env1 var-idx)
        (env-add-local env var +type-i32+)
      (multiple-value-bind (env2 tail-idx)
          (env-add-local env1 (gensym "TAIL") +type-i32+)
        (let* ((loop-env (env-increment-block-depth
                          (env-increment-block-depth env2)))
               (body-code (compile-progn body loop-env))
               (result-code (if result
                                (compile-form result env2)
                                `((,+op-i32-const+ 0)))))
          `(;; Evaluate list and store in tail
            ,@(compile-form list-form env)
            (,+op-local-set+ ,tail-idx)
            ;; Loop structure
            (,+op-block+ ,+type-void+)  ; exit block
            (,+op-loop+ ,+type-void+)   ; continue loop
            ;; Check: if tail is null (0), exit
            (,+op-local-get+ ,tail-idx)
            ,+op-i32-eqz+
            (,+op-br-if+ 1)  ; br to exit block
            ;; Get car of tail into var
            (,+op-local-get+ ,tail-idx)
            (,+op-i32-load+ 2 0)  ; load car
            (,+op-local-set+ ,var-idx)
            ;; Execute body (drop result)
            ,@body-code
            ,+op-drop+
            ;; Move to cdr
            (,+op-local-get+ ,tail-idx)
            (,+op-i32-load+ 2 4)  ; load cdr
            (,+op-local-set+ ,tail-idx)
            ;; Continue loop
            (,+op-br+ 0)
            ,+op-end+  ; end loop
            ,+op-end+  ; end block
            ;; Return result
            ,@result-code))))))

;;; LAMBDA and FUNCALL - Closure support

;;; Free Variable Analysis

(defun find-free-variables (form bound-vars)
  "Find variables in FORM that are not in BOUND-VARS.
   Returns a list of free variable names."
  (let ((free-vars nil))
    (labels ((collect (expr bound)
               (cond
                 ;; Atoms
                 ((null expr) nil)
                 ((eq expr t) nil)
                 ((numberp expr) nil)
                 ;; Variable reference
                 ((symbolp expr)
                  (when (and (not (member expr bound))
                             (not (member expr free-vars))
                             (not (special-form-p expr))
                             (not (primitive-p expr)))
                    (push expr free-vars)))
                 ;; Special forms
                 ((and (consp expr) (eq (car expr) 'quote))
                  nil)  ; Don't look inside quote
                 ((and (consp expr) (eq (car expr) 'lambda))
                  ;; Lambda introduces new bindings
                  (destructuring-bind (params &rest body) (cdr expr)
                    (let ((new-bound (append params bound)))
                      (dolist (b body)
                        (collect b new-bound)))))
                 ((and (consp expr) (eq (car expr) 'let))
                  (destructuring-bind (bindings &rest body) (cdr expr)
                    ;; First process init forms with current bindings
                    (dolist (b bindings)
                      (when (consp b)
                        (collect (second b) bound)))
                    ;; Then body with extended bindings
                    (let ((new-bound (append (mapcar (lambda (b)
                                                       (if (consp b) (car b) b))
                                                     bindings)
                                             bound)))
                      (dolist (b body)
                        (collect b new-bound)))))
                 ((and (consp expr) (eq (car expr) 'let*))
                  (destructuring-bind (bindings &rest body) (cdr expr)
                    ;; Process sequentially
                    (let ((current-bound bound))
                      (dolist (b bindings)
                        (when (consp b)
                          (collect (second b) current-bound))
                        (push (if (consp b) (car b) b) current-bound))
                      (dolist (b body)
                        (collect b current-bound)))))
                 ((and (consp expr) (member (car expr) '(dotimes dolist)))
                  ;; Loop variable is bound in body
                  (destructuring-bind ((var init &optional result) &rest body)
                      (cdr expr)
                    (collect init bound)
                    (let ((new-bound (cons var bound)))
                      (dolist (b body)
                        (collect b new-bound))
                      (when result
                        (collect result new-bound)))))
                 ;; General list - process all elements
                 ((consp expr)
                  (dolist (e expr)
                    (collect e bound))))))
      (collect form bound-vars))
    (nreverse free-vars)))

;;; Closure Representation
;;; A closure is stored on the heap as:
;;;   [func_index: i32, env_size: i32, captured_val1, captured_val2, ...]
;;; Total size = 8 + 4 * num_captured bytes

(defparameter *closure-header-size* 8
  "Size of closure header (func_index + env_size) in bytes.")

(defparameter *closure-func-offset* 0
  "Offset of function index in closure.")

(defparameter *closure-envsize-offset* 4
  "Offset of environment size in closure.")

(defparameter *closure-env-offset* 8
  "Offset of first captured variable in closure.")

;;; Lambda compilation creates a closure
;;; For non-capturing lambdas, we still create a closure structure for uniformity

(define-special-form lambda (form env)
  "Compile a lambda expression. Creates a closure on the heap."
  (destructuring-bind (params &rest body) (cdr form)
    ;; Find free variables
    (let* ((free-vars (find-free-variables `(progn ,@body) params))
           (num-captured (length free-vars))
           (closure-size (+ *closure-header-size* (* 4 num-captured)))
           (module (compile-env-module env))
           (arity (length params)))

      ;; Get the shared type index for this arity
      (let* ((type-idx (get-closure-type-index module arity))
             ;; Calculate function index: defuns first, then lambdas
             (defun-count (compile-env-func-count env))
             (lambda-idx (if *pending-lambdas*
                             (length (car *pending-lambdas*))
                             (wasm-module-func-count module)))
             (func-idx (+ defun-count lambda-idx)))

          ;; Create body environment with parameters as locals
          (let ((body-env (make-compile-env :module module)))
            ;; Add closure-env as local 0
            (setf body-env (env-add-local body-env 'closure-env +type-i32+))
            ;; Add declared parameters as locals
            (dolist (param params)
              (setf body-env (env-add-local body-env param +type-i32+)))

            ;; Add captured variables as real locals
            ;; and initialize them from the closure at function start
            (let* ((init-code nil))

              ;; Add each captured variable as a local
              (dolist (var free-vars)
                (multiple-value-bind (env* idx)
                    (env-add-local body-env var +type-i32+)
                  (setf body-env env*)
                  ;; Load from closure into local
                  (let ((offset (+ *closure-env-offset*
                                   (* 4 (position var free-vars)))))
                    (setf init-code
                          (append init-code
                                  `((,+op-local-get+ 0)  ; closure-env
                                    (,+op-i32-load+ 2 ,offset)
                                    (,+op-local-set+ ,idx)))))))

              ;; Compile the body
              (let* ((body-code (compile-progn body body-env))
                     (full-code (append init-code body-code))
                     ;; Find max local index
                     (max-local-idx (find-max-local-index full-code))
                     ;; Locals = indices beyond params (closure-env + declared params)
                     (param-count (1+ arity))  ; closure-env + params
                     (num-extra-locals (max 0 (- max-local-idx param-count -1)))
                     (locals (if (> num-extra-locals 0)
                                 (list (cons num-extra-locals +type-i32+))
                                 nil)))

                ;; Add function to pending list if available, else directly to module
                (if *pending-lambdas*
                    (push (list type-idx locals full-code) (car *pending-lambdas*))
                    (add-function module type-idx locals full-code))

                ;; Now generate code to create the closure structure
                `(;; Reserve space for closure
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-add+
                  (,+op-global-set+ ,*heap-pointer-global*)

                  ;; Push closure address (return value)
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+

                  ;; Store function index
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+
                  (,+op-i32-const+ ,func-idx)
                  (,+op-i32-store+ 2 ,*closure-func-offset*)

                  ;; Store env size
                  (,+op-global-get+ ,*heap-pointer-global*)
                  (,+op-i32-const+ ,closure-size)
                  ,+op-i32-sub+
                  (,+op-i32-const+ ,num-captured)
                  (,+op-i32-store+ 2 ,*closure-envsize-offset*)

                  ;; Store captured variables
                  ,@(loop for var in free-vars
                          for i from 0
                          for var-info = (env-lookup env var)
                          for offset = (+ *closure-env-offset* (* 4 i))
                          when var-info
                          append `((,+op-global-get+ ,*heap-pointer-global*)
                                   (,+op-i32-const+ ,closure-size)
                                   ,+op-i32-sub+
                                   (,+op-local-get+ ,(local-info-index var-info))
                                   (,+op-i32-store+ 2 ,offset)))
                  ;; Closure address is already on stack
                  ))))))))

;;; FUNCALL - Call a closure

(define-special-form funcall (form env)
  "Call a closure. The closure is the first argument."
  (let* ((closure-form (second form))
         (args (cddr form))
         (arity (length args))
         (module (compile-env-module env))
         (type-idx (get-closure-type-index module arity)))
    ;; Compile closure to get its address
    (let ((closure-code (compile-form closure-form env)))
      ;; Need a local to hold the closure address
      (multiple-value-bind (env* closure-idx)
          (env-add-local env (gensym "CLOSURE") +type-i32+)
        (let ((args-code nil))
          ;; Compile all arguments
          (dolist (arg args)
            (setf args-code (append args-code (compile-form arg env*))))

          `(;; Evaluate closure and save address
            ,@closure-code
            (,+op-local-set+ ,closure-idx)
            ;; Push closure address as first argument (for captured vars)
            (,+op-local-get+ ,closure-idx)
            ,@args-code
            ;; Get function index from closure and call indirectly
            (,+op-local-get+ ,closure-idx)
            (,+op-i32-load+ 2 ,*closure-func-offset*)
            (,+op-call-indirect+ ,type-idx 0)))))))

;;; LABELS - Local function definitions

(define-special-form labels (form env)
  "Compile (labels ((name1 (params) body1) ...) body).
  Each local function is compiled as a closure and bound to its name."
  (destructuring-bind (definitions &rest body) (cdr form)
    ;; First pass: create locals for all function closures
    (let ((func-locals nil)
          (env* env))
      ;; Create a local variable for each labeled function
      (dolist (def definitions)
        (let ((name (car def)))
          (multiple-value-bind (new-env idx)
              (env-add-local env* name +type-i32+)
            (setf env* new-env)
            (push (cons name idx) func-locals))))

      ;; Now all function names are in scope as variables
      ;; Second pass: compile each function as a lambda and store in its local
      (let ((init-code nil))
        (dolist (def definitions)
          (let* ((name (car def))
                 (params (second def))
                 (func-body (cddr def))
                 (local-idx (cdr (assoc name func-locals)))
                 ;; Compile as lambda
                 (lambda-form `(lambda ,params ,@func-body))
                 (lambda-code (compile-form lambda-form env*)))
            ;; Store closure in local
            (setf init-code
                  (append init-code
                          lambda-code
                          `((,+op-local-set+ ,local-idx))))))

        ;; Compile the body with all functions in scope
        (let ((body-code (compile-progn body env*)))
          (append init-code body-code))))))

;;; VALUES - Return multiple values

(define-special-form values (form env)
  "Return multiple values. Stores values in global variables."
  (let* ((values-forms (cdr form))
         (num-values (length values-forms)))
    (if (zerop num-values)
        ;; (values) => nil with count 0
        `((,+op-i32-const+ 0)
          (,+op-global-set+ ,*mv-count-global*)
          (,+op-i32-const+ 0))
        ;; Store each value in its global
        (let ((code nil))
          ;; Compile and store each value
          (loop for form in values-forms
                for i from 0 below +max-multiple-values+
                for global-idx in *mv-globals*
                do (setf code
                         (append code
                                 (compile-form form env)
                                 `((,+op-global-set+ ,global-idx)))))
          ;; Set count
          (setf code
                (append code
                        `((,+op-i32-const+ ,num-values)
                          (,+op-global-set+ ,*mv-count-global*))))
          ;; Return first value
          (append code
                  `((,+op-global-get+ ,(first *mv-globals*))))))))

;;; MULTIPLE-VALUE-BIND - Bind multiple return values

(define-special-form multiple-value-bind (form env)
  "Bind variables to multiple values from a form."
  (destructuring-bind (vars value-form &rest body) (cdr form)
    (if (null vars)
        ;; No vars - just evaluate form and run body
        (let ((value-code (compile-form value-form env)))
          (append value-code
                  `(,+op-drop+)
                  (compile-progn body env)))
        ;; Bind each var to its corresponding global
        (let ((env* env)
              (var-indices nil))
          ;; Add locals for each variable
          (dolist (var vars)
            (multiple-value-bind (new-env idx)
                (env-add-local env* var +type-i32+)
              (setf env* new-env)
              (push (cons var idx) var-indices)))
          (setf var-indices (nreverse var-indices))
          ;; Generate code
          (let ((value-code (compile-form value-form env*))
                (bind-code nil))
            ;; Bind each variable from its global (or nil if past count)
            (loop for (var . idx) in var-indices
                  for i from 0
                  for global-idx in *mv-globals*
                  do (setf bind-code
                           (append bind-code
                                   (if (zerop i)
                                       ;; First var gets the primary value (already on stack)
                                       `((,+op-local-set+ ,idx))
                                       ;; Other vars get from globals
                                       `((,+op-global-get+ ,global-idx)
                                         (,+op-local-set+ ,idx))))))
            (append value-code bind-code (compile-progn body env*)))))))

;;; Setf special form for general place assignment

(define-special-form setf (form env)
  "Set a place to a value. Handles variables, car/cdr, gethash, and struct accessors."
  (let ((place (second form))
        (value (third form)))
    (cond
      ;; Simple variable: (setf x val) -> (setq x val)
      ((symbolp place)
       (compile-special-form `(setq ,place ,value) env))

      ;; Compound place
      ((consp place)
       (let ((accessor (car place)))
         (cond
           ;; (setf (car x) val) -> rplaca, but return val
           ((member accessor '(car first) :test #'string-equal :key #'symbol-name)
            (let* ((target (second place))
                   (env-count (compile-env-local-count env))
                   (val-local env-count))
              `(;; Evaluate value, store in local for return
                ,@(compile-form value env)
                (,+op-local-tee+ ,val-local)
                ;; Evaluate target, perform rplaca
                ,@(compile-form target env)
                (,+op-local-get+ ,val-local)
                (,+op-i32-store+ 2 0)  ; store at car
                ;; Return the value
                (,+op-local-get+ ,val-local))))

           ;; (setf (cdr x) val) -> rplacd, but return val
           ((member accessor '(cdr rest) :test #'string-equal :key #'symbol-name)
            (let* ((target (second place))
                   (env-count (compile-env-local-count env))
                   (val-local env-count))
              `(;; Evaluate value, store in local for return
                ,@(compile-form value env)
                (,+op-local-tee+ ,val-local)
                ;; Evaluate target, perform rplacd
                ,@(compile-form target env)
                (,+op-local-get+ ,val-local)
                (,+op-i32-store+ 2 4)  ; store at cdr
                ;; Return the value
                (,+op-local-get+ ,val-local))))

           ;; (setf (second x) val) -> (setf (car (cdr x)) val)
           ((string-equal (symbol-name accessor) "SECOND")
            (compile-special-form
             `(setf (car (cdr ,(second place))) ,value) env))

           ;; (setf (third x) val) -> (setf (car (cdr (cdr x))) val)
           ((string-equal (symbol-name accessor) "THIRD")
            (compile-special-form
             `(setf (car (cdr (cdr ,(second place)))) ,value) env))

           ;; (setf (nth n x) val) - compile to loop equivalent
           ((string-equal (symbol-name accessor) "NTH")
            (let* ((n-form (second place))
                   (list-form (third place)))
              (compile-special-form
               `(setf (car (nthcdr ,n-form ,list-form)) ,value) env)))

           ;; (setf (gethash key ht) val) -> (sethash key val ht)
           ((string-equal (symbol-name accessor) "GETHASH")
            (let* ((key-form (second place))
                   (ht-form (third place)))
              (compile-form `(sethash ,key-form ,value ,ht-form) env)))

           ;; Check for struct accessor (name-slot)
           ;; Struct setf accessors set field and return value
           (t
            (let* ((accessor-name (symbol-name accessor))
                   ;; Try to find a struct info where this is an accessor
                   (struct-info nil)
                   (slot-idx nil))
              ;; Search struct registry for matching accessor
              ;; Slots are stored as ((slot-name default) ...) so we need (first slot)
              (maphash (lambda (name info)
                         (declare (ignore name))
                         (loop for slot in (struct-info-slots info)
                               for i from 0
                               when (string-equal accessor-name
                                                  (format nil "~A-~A"
                                                          (struct-info-name info)
                                                          (first slot)))  ; slot is (name default)
                               do (setf struct-info info
                                        slot-idx i)
                                  (return)))
                       *struct-registry*)
              (if struct-info
                  ;; Found struct accessor - compile setf for it
                  (let* ((struct-form (second place))
                         (slot-offset (* (1+ slot-idx) 4))  ; +1 for type-id, *4 for i32
                         (env-count (compile-env-local-count env))
                         (val-local env-count))
                    `(;; Evaluate value
                      ,@(compile-form value env)
                      (,+op-local-tee+ ,val-local)
                      ;; Evaluate struct, store at slot offset
                      ,@(compile-form struct-form env)
                      (,+op-local-get+ ,val-local)
                      (,+op-i32-store+ 2 ,slot-offset)
                      ;; Return value
                      (,+op-local-get+ ,val-local)))
                  ;; Unknown place
                  (error "Cannot compile setf for place: ~A" place)))))))

      (t
       (error "Invalid setf place: ~A" place)))))

;;; Format special form
;;; Only supports (format nil ...) with ~A, ~%, and ~~ directives

(defun parse-format-string (format-string)
  "Parse a format string into segments.
   Returns a list of (:literal \"text\") or (:directive char) elements."
  (let ((result nil)
        (current-literal nil)
        (i 0)
        (len (length format-string)))
    (loop while (< i len) do
      (let ((ch (char format-string i)))
        (if (char= ch #\~)
            (progn
              ;; Flush current literal if any
              (when current-literal
                (push (list :literal (coerce (nreverse current-literal) 'string)) result)
                (setf current-literal nil))
              ;; Check next character for directive
              (incf i)
              (when (< i len)
                (let ((directive (char format-string i)))
                  (push (list :directive directive) result))))
            ;; Regular character
            (push ch current-literal)))
      (incf i))
    ;; Flush remaining literal
    (when current-literal
      (push (list :literal (coerce (nreverse current-literal) 'string)) result))
    (nreverse result)))

(define-special-form format (form env)
  "Compile format. Only (format nil format-string args...) is supported.
   Supported directives: ~A (aesthetic), ~% (newline), ~~ (tilde)."
  (let ((destination (second form))
        (format-string (third form))
        (args (cdddr form)))
    ;; Only support (format nil ...)
    (unless (null destination)
      (error "format: only nil destination is supported"))
    (unless (stringp format-string)
      (error "format: format string must be a literal string"))
    (let* ((segments (parse-format-string format-string))
           (arg-index 0)
           (pieces nil))
      ;; Build list of pieces to concatenate
      (dolist (seg segments)
        (ecase (first seg)
          (:literal
           (let ((text (second seg)))
             (when (plusp (length text))
               (push text pieces))))
          (:directive
           (let ((ch (second seg)))
             (case ch
               ((#\A #\a)
                ;; ~A: use next argument (assumed to be a string)
                (when (>= arg-index (length args))
                  (error "format: not enough arguments for ~~A"))
                (push (nth arg-index args) pieces)
                (incf arg-index))
               ((#\%)
                ;; ~%: newline character (ASCII 10)
                (push '(make-string-from-char 10) pieces))
               ((#\~)
                ;; ~~: literal tilde
                (push "~" pieces))
               (t
                (error "format: unsupported directive ~~~A" ch)))))))
      ;; Reverse pieces to get correct order
      (setf pieces (nreverse pieces))
      ;; Generate concatenation code
      (if (null pieces)
          ;; Empty format string
          (compile-form "" env)
          ;; Concatenate all pieces
          (let ((result-form (first pieces)))
            (dolist (piece (rest pieces))
              (setf result-form `(string-append ,result-form ,piece)))
            (compile-form result-form env))))))

;;; CATCH - Establish a catch point for dynamic control transfer
;;;
;;; (catch tag body...) evaluates tag and body. If throw is called during
;;; body evaluation with a matching tag, catch returns the thrown value.
;;; Otherwise, it returns the value of the last body form.

(define-special-form catch (form env)
  "Compile catch. Establishes a catch point with evaluated tag."
  (destructuring-bind (tag-form &rest body) (cdr form)
    (let* ((env-count (compile-env-local-count env))
           (tag-local env-count)
           (result-local (1+ env-count)))
      `(;; Evaluate and save the catch tag
        ,@(compile-form tag-form env)
        (,+op-local-set+ ,tag-local)
        ;; Evaluate body
        ,@(compile-progn body env)
        (,+op-local-set+ ,result-local)
        ;; Check if a throw is pending
        (,+op-global-get+ ,*throw-pending-global*)
        (,+op-if+ ,+type-i32+)
          ;; Throw is pending - check if tag matches
          (,+op-global-get+ ,*throw-tag-global*)
          (,+op-local-get+ ,tag-local)
          ,+op-i32-eq+
          (,+op-if+ ,+type-i32+)
            ;; Tag matches - clear throw and return thrown value
            (,+op-i32-const+ 0)
            (,+op-global-set+ ,*throw-pending-global*)
            (,+op-global-get+ ,*throw-value-global*)
          ,+op-else+
            ;; Tag doesn't match - return throw value (propagate)
            (,+op-global-get+ ,*throw-value-global*)
          ,+op-end+
        ,+op-else+
          ;; No throw - return body result
          (,+op-local-get+ ,result-local)
        ,+op-end+))))

;;; THROW - Transfer control to a matching catch
;;;
;;; (throw tag value) searches for a catch with a matching tag and
;;; transfers control to it, returning the value from the catch.

(define-special-form throw (form env)
  "Compile throw. Sets throw globals and returns the value."
  (destructuring-bind (tag-form value-form) (cdr form)
    `(;; Evaluate tag and save to global
      ,@(compile-form tag-form env)
      (,+op-global-set+ ,*throw-tag-global*)
      ;; Evaluate value and save to global
      ,@(compile-form value-form env)
      (,+op-global-set+ ,*throw-value-global*)
      ;; Set throw-pending flag
      (,+op-i32-const+ 1)
      (,+op-global-set+ ,*throw-pending-global*)
      ;; Return the throw value
      (,+op-global-get+ ,*throw-value-global*))))

;;; UNWIND-PROTECT - Ensure cleanup forms are always executed
;;;
;;; (unwind-protect protected-form cleanup-forms...)
;;; Executes protected-form, then always executes cleanup-forms regardless
;;; of whether protected-form returns normally or exits via throw.

(define-special-form unwind-protect (form env)
  "Compile unwind-protect. Cleanup forms always execute."
  (destructuring-bind (protected-form &rest cleanup-forms) (cdr form)
    (let* ((env-count (compile-env-local-count env))
           (result-local env-count)
           (saved-pending-local (1+ env-count))
           (saved-tag-local (+ env-count 2))
           (saved-value-local (+ env-count 3)))
      `(;; Execute protected form and save result
        ,@(compile-form protected-form env)
        (,+op-local-set+ ,result-local)
        ;; Save current throw state
        (,+op-global-get+ ,*throw-pending-global*)
        (,+op-local-set+ ,saved-pending-local)
        (,+op-global-get+ ,*throw-tag-global*)
        (,+op-local-set+ ,saved-tag-local)
        (,+op-global-get+ ,*throw-value-global*)
        (,+op-local-set+ ,saved-value-local)
        ;; Clear throw-pending so cleanup runs normally
        (,+op-i32-const+ 0)
        (,+op-global-set+ ,*throw-pending-global*)
        ;; Execute cleanup forms (discard results)
        ,@(if cleanup-forms
              (let ((cleanup-code (compile-progn cleanup-forms env)))
                (append cleanup-code `(,+op-drop+)))
              nil)
        ;; Restore throw state if it was set
        (,+op-local-get+ ,saved-pending-local)
        (,+op-global-set+ ,*throw-pending-global*)
        (,+op-local-get+ ,saved-tag-local)
        (,+op-global-set+ ,*throw-tag-global*)
        (,+op-local-get+ ,saved-value-local)
        (,+op-global-set+ ,*throw-value-global*)
        ;; Return protected form result
        (,+op-local-get+ ,result-local)))))
