;;;; compiler/env.lisp - Compilation Environment
;;;;
;;;; Manages the compilation context including local variables,
;;;; closure captures, and control flow constructs.

(in-package #:clysm)

;;; ============================================================
;;; Variable Binding Information
;;; ============================================================

(defstruct (binding (:constructor make-binding (name kind index)))
  "Information about a variable binding."
  name      ; Symbol naming the variable
  kind      ; :local, :param, :closure, :global
  index     ; Wasm local index (for :local/:param) or closure index
  mutable-p ; Can be mutated?
  type)     ; Type hint (or nil)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct (compile-env (:constructor %make-compile-env))
  "The compilation environment."
  ;; Variable bindings
  (locals nil :type list)        ; List of binding structs
  (local-count 0 :type integer)  ; Number of Wasm locals allocated

  ;; Closure information
  (captures nil :type list)      ; Variables captured from outer scope
  (closure-depth 0 :type integer) ; Nesting depth of lambdas

  ;; Control flow
  (tail-position-p nil)          ; Are we in tail position?
  (blocks nil :type list)        ; Stack of (name . label-index) for block/return-from
  (tags nil :type list)          ; Stack of (tag . depth) for tagbody/go
  (catch-tags nil :type list)    ; Stack of (tag-local . handler-depth) for catch/throw
  (unwind-depth 0 :type integer) ; Depth of unwind-protect nesting

  ;; Special variables (dynamic scope)
  (specials nil :type list)      ; List of special variable names

  ;; Module context
  (type-registry nil)            ; Type registry for this compilation
  (functions nil :type list)     ; Functions defined in this module
  (codegen-context nil)          ; Codegen context for compiling nested lambdas

  ;; Parent environment (for closures)
  (parent nil))

(defun make-compile-env (&key type-registry parent codegen-context)
  "Create a new compilation environment."
  (%make-compile-env :type-registry type-registry
                     :parent parent
                     :codegen-context (or codegen-context
                                          (and parent
                                               (compile-env-codegen-context parent)))
                     :closure-depth (if parent
                                        (1+ (compile-env-closure-depth parent))
                                        0)))

;;; ============================================================
;;; Variable Lookup
;;; ============================================================

(defun env-lookup (env name)
  "Look up a variable binding in the environment.
Returns (values binding found-p)."
  (loop for binding in (compile-env-locals env)
        when (eq (binding-name binding) name)
          do (return-from env-lookup (values binding t)))
  ;; Not found locally, check parent
  (when (compile-env-parent env)
    (multiple-value-bind (binding found-p)
        (env-lookup (compile-env-parent env) name)
      (when found-p
        ;; Found in parent - this is a closure capture
        (return-from env-lookup (values binding t)))))
  (values nil nil))

(defun env-lookup-local (env name)
  "Look up a local variable (not in parent scope).
Returns (values binding found-p)."
  (loop for binding in (compile-env-locals env)
        when (eq (binding-name binding) name)
          do (return (values binding t)))
  (values nil nil))

;;; ============================================================
;;; Variable Binding
;;; ============================================================

(defun env-bind-local (env name &key mutable type)
  "Bind a new local variable in ENV.
Returns the new binding."
  (let* ((index (compile-env-local-count env))
         (binding (make-binding name :local index)))
    (setf (binding-mutable-p binding) mutable)
    (setf (binding-type binding) type)
    (push binding (compile-env-locals env))
    (incf (compile-env-local-count env))
    binding))

(defun env-bind-param (env name index &key type)
  "Bind a function parameter in ENV.
Returns the new binding."
  (let ((binding (make-binding name :param index)))
    (setf (binding-type binding) type)
    (push binding (compile-env-locals env))
    ;; Note: params don't increment local-count as they're already allocated
    binding))

(defun env-bind-closure (env name capture-index)
  "Bind a closure-captured variable in ENV.
Returns the new binding."
  (let ((binding (make-binding name :closure capture-index)))
    (push binding (compile-env-locals env))
    (push name (compile-env-captures env))
    binding))

;;; ============================================================
;;; Scope Management
;;; ============================================================

(defun env-push-scope (env)
  "Create a new nested scope (for let blocks).
Returns a new environment that shares the parent's state."
  ;; For now, we just extend the same environment
  ;; A more sophisticated implementation would track scope boundaries
  env)

(defun env-pop-scope (env saved-local-count)
  "Restore environment to a previous scope.
SAVED-LOCAL-COUNT is the local count before entering the scope."
  (setf (compile-env-locals env)
        (nthcdr (- (compile-env-local-count env) saved-local-count)
                (compile-env-locals env)))
  (setf (compile-env-local-count env) saved-local-count)
  env)

;;; ============================================================
;;; Block Management
;;; ============================================================

(defvar *block-label-counter* 0
  "Counter for generating unique block labels.")

(defun env-push-block (env name)
  "Push a new block onto the block stack.
Returns the label index for this block."
  (let ((label (incf *block-label-counter*)))
    (push (cons name label) (compile-env-blocks env))
    label))

(defun env-pop-block (env)
  "Pop the top block from the block stack."
  (pop (compile-env-blocks env)))

(defun env-find-block (env name)
  "Find a block by name.
Returns (values label depth) or (values nil nil)."
  (loop for (block-name . label) in (compile-env-blocks env)
        for depth from 0
        when (eq block-name name)
          do (return-from env-find-block (values label depth)))
  (values nil nil))

;;; ============================================================
;;; Tagbody/Go Management
;;; ============================================================

(defun env-push-tagbody (env tags)
  "Push tagbody tags onto the tag stack.
TAGS is a list of tag symbols.
Returns an alist of (tag . label-index) pairs."
  (let ((base-depth (length (compile-env-blocks env)))  ; Wasm control depth
        (tag-entries nil))
    ;; Each tag gets an index within this tagbody
    (loop for tag in tags
          for i from 0
          do (let ((entry (cons tag (cons i base-depth))))
               (push entry tag-entries)
               (push entry (compile-env-tags env))))
    (nreverse tag-entries)))

(defun env-pop-tagbody (env tag-count)
  "Pop TAG-COUNT tags from the tag stack."
  (dotimes (i tag-count)
    (pop (compile-env-tags env))))

(defun env-find-tag (env tag)
  "Find a tag in the current tagbody chain.
Returns (values tag-index block-depth) or (values nil nil)."
  (loop for (t-name . (t-index . t-depth)) in (compile-env-tags env)
        when (eql t-name tag)
          do (return-from env-find-tag (values t-index t-depth)))
  (values nil nil))

;;; ============================================================
;;; Catch/Throw Management
;;; ============================================================

(defun env-push-catch (env tag-local-index)
  "Push a catch handler onto the catch stack.
TAG-LOCAL-INDEX is the Wasm local storing the catch tag.
Returns the handler depth."
  (let ((depth (length (compile-env-catch-tags env))))
    (push (cons tag-local-index depth) (compile-env-catch-tags env))
    depth))

(defun env-pop-catch (env)
  "Pop the top catch handler from the stack."
  (pop (compile-env-catch-tags env)))

(defun env-catch-depth (env)
  "Return the current catch nesting depth."
  (length (compile-env-catch-tags env)))

;;; ============================================================
;;; Special Variable Management
;;; ============================================================

(defun env-declare-special (env name)
  "Declare NAME as a special variable in ENV."
  (pushnew name (compile-env-specials env)))

(defun env-special-p (env name)
  "Check if NAME is a special variable.
A variable is special only if explicitly declared via defvar/defparameter
or (declare (special ...)). Naming conventions like *foo* do not imply special."
  (or (member name (compile-env-specials env))
      (and (compile-env-parent env)
           (env-special-p (compile-env-parent env) name))
      ;; Check global specials in codegen-context
      (and (compile-env-codegen-context env)
           (member name (codegen-context-specials
                         (compile-env-codegen-context env))))))

;;; ============================================================
;;; Tail Position Tracking
;;; ============================================================

(defun env-in-tail-position (env)
  "Return a new environment in tail position."
  (let ((new-env (copy-compile-env env)))
    (setf (compile-env-tail-position-p new-env) t)
    new-env))

(defun env-not-in-tail-position (env)
  "Return a new environment not in tail position."
  (if (compile-env-tail-position-p env)
      (let ((new-env (copy-compile-env env)))
        (setf (compile-env-tail-position-p new-env) nil)
        new-env)
      env))

;;; ============================================================
;;; Closure Analysis
;;; ============================================================

(defun analyze-free-variables (ast env)
  "Analyze an AST node to find free variables.
Returns a list of variable names that are free in AST."
  (let ((free-vars nil))
    (labels ((analyze (node bound-vars)
               (etypecase node
                 (ast-literal nil)
                 (ast-quote nil)

                 (ast-var
                  (let ((name (ast-var-name node)))
                    (unless (or (member name bound-vars)
                                (env-lookup-local env name))
                      (pushnew name free-vars))))

                 (ast-setq
                  (let ((name (ast-setq-name node)))
                    (unless (or (member name bound-vars)
                                (env-lookup-local env name))
                      (pushnew name free-vars)))
                  (analyze (ast-setq-value node) bound-vars))

                 (ast-if
                  (analyze (ast-if-test node) bound-vars)
                  (analyze (ast-if-then node) bound-vars)
                  (when (ast-if-else node)
                    (analyze (ast-if-else node) bound-vars)))

                 (ast-progn
                  (dolist (form (ast-progn-forms node))
                    (analyze form bound-vars)))

                 (ast-let
                  (let ((new-bound bound-vars))
                    ;; For let*, bindings are sequential
                    (if (ast-let-sequential-p node)
                        (dolist (binding (ast-let-bindings node))
                          (analyze (cdr binding) new-bound)
                          (push (car binding) new-bound))
                        ;; For let, all bindings see original scope
                        (progn
                          (dolist (binding (ast-let-bindings node))
                            (analyze (cdr binding) bound-vars))
                          (dolist (binding (ast-let-bindings node))
                            (push (car binding) new-bound))))
                    (analyze (ast-let-body node) new-bound)))

                 (ast-lambda
                  ;; Lambda introduces new bindings
                  (let ((new-bound (append (ast-lambda-params node) bound-vars)))
                    (analyze (ast-lambda-body node) new-bound)))

                 (ast-call
                  (analyze (ast-call-func node) bound-vars)
                  (dolist (arg (ast-call-args node))
                    (analyze arg bound-vars)))

                 (ast-primitive-call
                  (dolist (arg (ast-primitive-call-args node))
                    (analyze arg bound-vars)))

                 (ast-block
                  (analyze (ast-block-body node) bound-vars))

                 (ast-return-from
                  (analyze (ast-return-from-value node) bound-vars))

                 (ast-defun
                  (let ((new-bound (append (ast-defun-params node) bound-vars)))
                    (analyze (ast-defun-body node) new-bound)))

                 (ast-defvar
                  (when (ast-defvar-value node)
                    (analyze (ast-defvar-value node) bound-vars)))

                 ;; Control flow forms
                 (ast-tagbody
                  (dolist (segment (ast-tagbody-segments node))
                    (analyze (cdr segment) bound-vars)))

                 (ast-go
                  ;; go has no free variables (tag is not a variable)
                  nil)

                 (ast-catch
                  (analyze (ast-catch-tag node) bound-vars)
                  (analyze (ast-catch-body node) bound-vars))

                 (ast-throw
                  (analyze (ast-throw-tag node) bound-vars)
                  (analyze (ast-throw-value node) bound-vars))

                 (ast-unwind-protect
                  (analyze (ast-unwind-protect-protected node) bound-vars)
                  (analyze (ast-unwind-protect-cleanup node) bound-vars)))))
      (analyze ast nil))
    (nreverse free-vars)))

;;; ============================================================
;;; Local Variable Types for Wasm
;;; ============================================================

(defun env-collect-local-types (env)
  "Collect the Wasm local types for this environment.
Returns a list of (count . valtype) pairs for the locals declaration."
  ;; Collect only :local bindings (not :param or :closure)
  (let* ((local-bindings (remove-if-not
                          (lambda (b) (eq (binding-kind b) :local))
                          (compile-env-locals env)))
         ;; Sort by index to ensure correct ordering
         (sorted (sort (copy-list local-bindings)
                       #'< :key #'binding-index))
         (result nil)
         (current-type nil)
         (current-count 0))
    ;; Group consecutive locals with the same type
    (dolist (binding sorted)
      (let ((type (or (binding-type binding) :anyref)))
        (if (equal type current-type)
            (incf current-count)
            (progn
              (when current-type
                (push (cons current-count current-type) result))
              (setf current-type type)
              (setf current-count 1)))))
    ;; Push final group
    (when current-type
      (push (cons current-count current-type) result))
    (nreverse result)))
