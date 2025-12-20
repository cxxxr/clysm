;;;; walker.lisp - AST traversal utilities
;;;; Converted from CLOS generic functions to defstruct-based dispatch

(in-package #:clysm/ast)

;;; AST Walker - traverse AST calling FN on each node

(defun walk-ast (node fn)
  "Walk an AST node, calling FN on each node."
  (etypecase node
    (const-node
     (funcall fn node))
    (var-node
     (funcall fn node))
    (if-node
     (funcall fn node)
     (walk-ast (if-node-test node) fn)
     (walk-ast (if-node-then node) fn)
     (when (if-node-else node)
       (walk-ast (if-node-else node) fn)))
    (let-node
     (funcall fn node)
     (dolist (binding (let-node-bindings node))
       (walk-ast (cdr binding) fn))
     (dolist (form (let-node-body node))
       (walk-ast form fn)))
    (lambda-node
     (funcall fn node)
     (dolist (form (lambda-node-body node))
       (walk-ast form fn)))
    (call-node
     (funcall fn node)
     (walk-ast (call-node-func node) fn)
     (dolist (arg (call-node-args node))
       (walk-ast arg fn)))
    (progn-node
     (funcall fn node)
     (dolist (form (progn-node-forms node))
       (walk-ast form fn)))
    (setq-node
     (funcall fn node)
     (walk-ast (setq-node-value node) fn))
    (quote-node
     (funcall fn node))
    (defun-node
     (funcall fn node)
     (dolist (form (defun-node-body node))
       (walk-ast form fn)))))

;;; AST Mapper - map a function over AST returning new AST

(defun map-ast (node fn)
  "Map a function over an AST, returning a new AST."
  (etypecase node
    (const-node
     (funcall fn (make-const-node :value (const-node-value node))))
    (var-node
     (funcall fn (make-var-node :name (var-node-name node))))
    (if-node
     (funcall fn (make-if-node
                  :test (map-ast (if-node-test node) fn)
                  :then (map-ast (if-node-then node) fn)
                  :else (when (if-node-else node)
                          (map-ast (if-node-else node) fn)))))
    (let-node
     (funcall fn (make-let-node
                  :bindings (mapcar (lambda (b)
                                      (cons (car b) (map-ast (cdr b) fn)))
                                    (let-node-bindings node))
                  :body (mapcar (lambda (f) (map-ast f fn))
                                (let-node-body node)))))
    (lambda-node
     (funcall fn (make-lambda-node
                  :params (lambda-node-params node)
                  :body (mapcar (lambda (f) (map-ast f fn))
                                (lambda-node-body node)))))
    (call-node
     (funcall fn (make-call-node
                  :func (map-ast (call-node-func node) fn)
                  :args (mapcar (lambda (a) (map-ast a fn))
                                (call-node-args node)))))
    (progn-node
     (funcall fn (make-progn-node
                  :forms (mapcar (lambda (f) (map-ast f fn))
                                 (progn-node-forms node)))))
    (setq-node
     (funcall fn (make-setq-node
                  :var (setq-node-var node)
                  :value (map-ast (setq-node-value node) fn))))
    (quote-node
     (funcall fn (make-quote-node :value (quote-node-value node))))
    (defun-node
     (funcall fn (make-defun-node
                  :name (defun-node-name node)
                  :params (defun-node-params node)
                  :body (mapcar (lambda (f) (map-ast f fn))
                                (defun-node-body node)))))))
