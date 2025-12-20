;;;; walker.lisp - AST traversal utilities

(in-package #:clysm/ast)

;;; Generic AST Walker

(defgeneric walk-ast (node fn)
  (:documentation "Walk an AST node, calling FN on each node."))

(defmethod walk-ast ((node const-node) fn)
  (funcall fn node))

(defmethod walk-ast ((node var-node) fn)
  (funcall fn node))

(defmethod walk-ast ((node if-node) fn)
  (funcall fn node)
  (walk-ast (if-node-test node) fn)
  (walk-ast (if-node-then node) fn)
  (when (if-node-else node)
    (walk-ast (if-node-else node) fn)))

(defmethod walk-ast ((node let-node) fn)
  (funcall fn node)
  (dolist (binding (let-node-bindings node))
    (walk-ast (cdr binding) fn))
  (dolist (form (let-node-body node))
    (walk-ast form fn)))

(defmethod walk-ast ((node lambda-node) fn)
  (funcall fn node)
  (dolist (form (lambda-node-body node))
    (walk-ast form fn)))

(defmethod walk-ast ((node call-node) fn)
  (funcall fn node)
  (walk-ast (call-node-func node) fn)
  (dolist (arg (call-node-args node))
    (walk-ast arg fn)))

(defmethod walk-ast ((node progn-node) fn)
  (funcall fn node)
  (dolist (form (progn-node-forms node))
    (walk-ast form fn)))

;;; AST Mapper

(defgeneric map-ast (node fn)
  (:documentation "Map a function over an AST, returning a new AST."))

(defmethod map-ast ((node const-node) fn)
  (funcall fn (make-const-node (const-node-value node))))

(defmethod map-ast ((node var-node) fn)
  (funcall fn (make-var-node (var-node-name node))))

(defmethod map-ast ((node if-node) fn)
  (funcall fn (make-if-node
               (map-ast (if-node-test node) fn)
               (map-ast (if-node-then node) fn)
               (when (if-node-else node)
                 (map-ast (if-node-else node) fn)))))

(defmethod map-ast ((node let-node) fn)
  (funcall fn (make-let-node
               (mapcar (lambda (b)
                         (cons (car b) (map-ast (cdr b) fn)))
                       (let-node-bindings node))
               (mapcar (lambda (f) (map-ast f fn))
                       (let-node-body node)))))

(defmethod map-ast ((node lambda-node) fn)
  (funcall fn (make-lambda-node
               (lambda-node-params node)
               (mapcar (lambda (f) (map-ast f fn))
                       (lambda-node-body node)))))

(defmethod map-ast ((node call-node) fn)
  (funcall fn (make-call-node
               (map-ast (call-node-func node) fn)
               (mapcar (lambda (a) (map-ast a fn))
                       (call-node-args node)))))

(defmethod map-ast ((node progn-node) fn)
  (funcall fn (make-progn-node
               (mapcar (lambda (f) (map-ast f fn))
                       (progn-node-forms node)))))
