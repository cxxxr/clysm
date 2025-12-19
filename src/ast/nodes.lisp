;;;; nodes.lisp - AST node definitions for cl-wasm

(in-package #:cl-wasm/ast)

;;; Base AST Node

(defclass ast-node ()
  ((source-info :initarg :source-info :accessor node-source-info :initform nil))
  (:documentation "Base class for all AST nodes."))

;;; Literal/Constant Node

(defclass const-node (ast-node)
  ((value :initarg :value :accessor const-node-value))
  (:documentation "A constant value (number, string, etc.)."))

(defun make-const-node (value)
  (make-instance 'const-node :value value))

;;; Variable Reference Node

(defclass var-node (ast-node)
  ((name :initarg :name :accessor var-node-name))
  (:documentation "A variable reference."))

(defun make-var-node (name)
  (make-instance 'var-node :name name))

;;; If Node

(defclass if-node (ast-node)
  ((test :initarg :test :accessor if-node-test)
   (then :initarg :then :accessor if-node-then)
   (else :initarg :else :accessor if-node-else :initform nil))
  (:documentation "A conditional expression."))

(defun make-if-node (test then &optional else)
  (make-instance 'if-node :test test :then then :else else))

;;; Let Node

(defclass let-node (ast-node)
  ((bindings :initarg :bindings :accessor let-node-bindings)
   (body :initarg :body :accessor let-node-body))
  (:documentation "A let binding expression."))

(defun make-let-node (bindings body)
  (make-instance 'let-node :bindings bindings :body body))

;;; Lambda Node

(defclass lambda-node (ast-node)
  ((params :initarg :params :accessor lambda-node-params)
   (body :initarg :body :accessor lambda-node-body))
  (:documentation "A lambda expression."))

(defun make-lambda-node (params body)
  (make-instance 'lambda-node :params params :body body))

;;; Function Call Node

(defclass call-node (ast-node)
  ((func :initarg :func :accessor call-node-func)
   (args :initarg :args :accessor call-node-args))
  (:documentation "A function call."))

(defun make-call-node (func args)
  (make-instance 'call-node :func func :args args))

;;; Progn Node (Sequence)

(defclass progn-node (ast-node)
  ((forms :initarg :forms :accessor progn-node-forms))
  (:documentation "A sequence of expressions."))

(defun make-progn-node (forms)
  (make-instance 'progn-node :forms forms))

;;; Setq Node

(defclass setq-node (ast-node)
  ((var :initarg :var :accessor setq-node-var)
   (value :initarg :value :accessor setq-node-value))
  (:documentation "A variable assignment."))

(defun make-setq-node (var value)
  (make-instance 'setq-node :var var :value value))

;;; Quote Node

(defclass quote-node (ast-node)
  ((value :initarg :value :accessor quote-node-value))
  (:documentation "A quoted form."))

(defun make-quote-node (value)
  (make-instance 'quote-node :value value))

;;; Defun Node

(defclass defun-node (ast-node)
  ((name :initarg :name :accessor defun-node-name)
   (params :initarg :params :accessor defun-node-params)
   (body :initarg :body :accessor defun-node-body))
  (:documentation "A function definition."))

(defun make-defun-node (name params body)
  (make-instance 'defun-node :name name :params params :body body))
