;;;; convert.lisp - AST to IR conversion

(in-package #:cl-wasm/ir)

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

(defgeneric ast-to-ir (node ctx)
  (:documentation "Convert an AST node to IR."))

(defmethod ast-to-ir ((node const-node) ctx)
  (declare (ignore ctx))
  (make-ir-const (const-node-value node)))

(defmethod ast-to-ir ((node var-node) ctx)
  (let ((index (context-lookup ctx (var-node-name node))))
    (if index
        (make-ir-local-ref index)
        (error "Undefined variable: ~A" (var-node-name node)))))

(defmethod ast-to-ir ((node if-node) ctx)
  (make-ir-if
   (ast-to-ir (if-node-test node) ctx)
   (ast-to-ir (if-node-then node) ctx)
   (if (if-node-else node)
       (ast-to-ir (if-node-else node) ctx)
       (make-ir-const nil))))

(defmethod ast-to-ir ((node let-node) ctx)
  (let ((new-ctx ctx)
        (init-forms nil))
    ;; Process bindings
    (dolist (binding (let-node-bindings node))
      (let ((init-ir (ast-to-ir (cdr binding) new-ctx)))
        (multiple-value-bind (ctx* index)
            (context-extend new-ctx (car binding))
          (setf new-ctx ctx*)
          (push (make-ir-local-set index init-ir) init-forms))))
    ;; Process body
    (let ((body-ir (mapcar (lambda (f) (ast-to-ir f new-ctx))
                           (let-node-body node))))
      (make-ir-seq (append (nreverse init-forms) body-ir)))))

(defmethod ast-to-ir ((node call-node) ctx)
  (let ((func (call-node-func node)))
    (if (and (typep func 'var-node)
             (primitive-op-p (var-node-name func)))
        ;; Primitive operation
        (make-ir-primop
         (var-node-name func)
         (mapcar (lambda (a) (ast-to-ir a ctx))
                 (call-node-args node)))
        ;; Regular function call
        (make-ir-call
         (ast-to-ir func ctx)
         (mapcar (lambda (a) (ast-to-ir a ctx))
                 (call-node-args node))))))

(defmethod ast-to-ir ((node progn-node) ctx)
  (make-ir-seq
   (mapcar (lambda (f) (ast-to-ir f ctx))
           (progn-node-forms node))))

;;; Primitive Operations

(defparameter *primitive-ops*
  '(+ - * / < > <= >= = /= mod rem
    car cdr cons list
    null not and or)
  "List of primitive operations.")

(defun primitive-op-p (name)
  "Check if NAME is a primitive operation."
  (member name *primitive-ops*))
