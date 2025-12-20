;;;; convert.lisp - AST to IR conversion
;;;; Converted from CLOS generic functions to defstruct-based dispatch

(in-package #:clysm/ir)

;;; Conversion Context

(defstruct convert-context
  "Context for AST to IR conversion."
  (locals nil :type list)           ; Alist of (name . index)
  (local-count 0 :type integer)     ; Next local index
  (label-count 0 :type integer))    ; For generating unique labels

(defun context-lookup (ctx name)
  "Look up a variable in the context. Returns local index or nil."
  (cdr (assoc name (convert-context-locals ctx))))

(defun context-extend (ctx name)
  "Extend context with a new local variable. Returns (new-ctx . index)."
  (let ((index (convert-context-local-count ctx)))
    (values
     (make-convert-context
      :locals (acons name index (convert-context-locals ctx))
      :local-count (1+ index)
      :label-count (convert-context-label-count ctx))
     index)))

(defun context-fresh-label (ctx)
  "Generate a fresh label. Returns (new-ctx . label)."
  (let ((label (convert-context-label-count ctx)))
    (values
     (make-convert-context
      :locals (convert-context-locals ctx)
      :local-count (convert-context-local-count ctx)
      :label-count (1+ label))
     label)))

;;; AST to IR Conversion

(defun ast-to-ir (node ctx)
  "Convert an AST node to IR."
  (etypecase node
    (const-node
     (make-ir-const :value (const-node-value node)))
    (var-node
     (let ((index (context-lookup ctx (var-node-name node))))
       (if index
           (make-ir-local-ref :index index)
           (error "Undefined variable: ~A" (var-node-name node)))))
    (if-node
     (make-ir-if
      :test (ast-to-ir (if-node-test node) ctx)
      :then (ast-to-ir (if-node-then node) ctx)
      :else (if (if-node-else node)
                (ast-to-ir (if-node-else node) ctx)
                (make-ir-const :value nil))))
    (let-node
     (let ((new-ctx ctx)
           (init-forms nil))
       ;; Process bindings
       (dolist (binding (let-node-bindings node))
         (let ((init-ir (ast-to-ir (cdr binding) new-ctx)))
           (multiple-value-bind (ctx* index)
               (context-extend new-ctx (car binding))
             (setf new-ctx ctx*)
             (push (make-ir-local-set :index index :value init-ir) init-forms))))
       ;; Process body
       (let ((body-ir (mapcar (lambda (f) (ast-to-ir f new-ctx))
                              (let-node-body node))))
         (make-ir-seq :forms (append (nreverse init-forms) body-ir)))))
    (call-node
     (let ((func (call-node-func node)))
       (if (and (var-node-p func)
                (primitive-op-p (var-node-name func)))
           ;; Primitive operation
           (make-ir-primop
            :op (var-node-name func)
            :args (mapcar (lambda (a) (ast-to-ir a ctx))
                          (call-node-args node)))
           ;; Regular function call
           (make-ir-call
            :func (ast-to-ir func ctx)
            :args (mapcar (lambda (a) (ast-to-ir a ctx))
                          (call-node-args node))))))
    (progn-node
     (make-ir-seq
      :forms (mapcar (lambda (f) (ast-to-ir f ctx))
                     (progn-node-forms node))))
    (lambda-node
     ;; Lambda nodes need special handling - for now just error
     (error "Lambda conversion not implemented in IR layer"))
    (setq-node
     (let ((index (context-lookup ctx (setq-node-var node))))
       (if index
           (make-ir-local-set
            :index index
            :value (ast-to-ir (setq-node-value node) ctx))
           (error "Undefined variable: ~A" (setq-node-var node)))))
    (quote-node
     (make-ir-const :value (quote-node-value node)))
    (defun-node
     ;; Defun nodes are handled at top-level, not in IR conversion
     (error "Defun cannot appear in expression context"))))

;;; Primitive Operations

(defparameter *primitive-ops*
  '(+ - * / < > <= >= = /= mod rem
    car cdr cons list
    null not and or)
  "List of primitive operations.")

(defun primitive-op-p (name)
  "Check if NAME is a primitive operation."
  (member name *primitive-ops*))
