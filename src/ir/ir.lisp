;;;; ir.lisp - Intermediate Representation for clysm

(in-package #:clysm/ir)

;;; IR Node Base

(defclass ir-node ()
  ((type :initarg :type :accessor ir-node-type :initform nil))
  (:documentation "Base class for IR nodes."))

;;; IR Constant

(defclass ir-const (ir-node)
  ((value :initarg :value :accessor ir-const-value))
  (:documentation "A constant value in IR."))

(defun make-ir-const (value &optional type)
  (make-instance 'ir-const :value value :type type))

;;; IR Local Variable Reference

(defclass ir-local-ref (ir-node)
  ((index :initarg :index :accessor ir-local-index))
  (:documentation "A reference to a local variable."))

(defun make-ir-local-ref (index &optional type)
  (make-instance 'ir-local-ref :index index :type type))

;;; IR Local Variable Set

(defclass ir-local-set (ir-node)
  ((index :initarg :index :accessor ir-local-set-index)
   (value :initarg :value :accessor ir-local-set-value))
  (:documentation "Set a local variable."))

(defun make-ir-local-set (index value)
  (make-instance 'ir-local-set :index index :value value))

;;; IR If

(defclass ir-if (ir-node)
  ((test :initarg :test :accessor ir-if-test)
   (then :initarg :then :accessor ir-if-then)
   (else :initarg :else :accessor ir-if-else))
  (:documentation "A conditional in IR."))

(defun make-ir-if (test then else &optional type)
  (make-instance 'ir-if :test test :then then :else else :type type))

;;; IR Block

(defclass ir-block (ir-node)
  ((label :initarg :label :accessor ir-block-label)
   (body :initarg :body :accessor ir-block-body))
  (:documentation "A labeled block for control flow."))

(defun make-ir-block (label body &optional type)
  (make-instance 'ir-block :label label :body body :type type))

;;; IR Loop

(defclass ir-loop (ir-node)
  ((label :initarg :label :accessor ir-loop-label)
   (body :initarg :body :accessor ir-loop-body))
  (:documentation "A loop construct."))

(defun make-ir-loop (label body)
  (make-instance 'ir-loop :label label :body body))

;;; IR Branch

(defclass ir-br (ir-node)
  ((label :initarg :label :accessor ir-br-label)
   (condition :initarg :condition :accessor ir-br-condition :initform nil))
  (:documentation "A branch to a label."))

(defun make-ir-br (label &optional condition)
  (make-instance 'ir-br :label label :condition condition))

;;; IR Function Call

(defclass ir-call (ir-node)
  ((func :initarg :func :accessor ir-call-func)
   (args :initarg :args :accessor ir-call-args))
  (:documentation "A function call."))

(defun make-ir-call (func args &optional type)
  (make-instance 'ir-call :func func :args args :type type))

;;; IR Return

(defclass ir-return (ir-node)
  ((value :initarg :value :accessor ir-return-value))
  (:documentation "Return from function."))

(defun make-ir-return (value)
  (make-instance 'ir-return :value value))

;;; IR Sequence

(defclass ir-seq (ir-node)
  ((forms :initarg :forms :accessor ir-seq-forms))
  (:documentation "A sequence of IR nodes."))

(defun make-ir-seq (forms)
  (make-instance 'ir-seq :forms forms))

;;; IR Primitive Operation

(defclass ir-primop (ir-node)
  ((op :initarg :op :accessor ir-primop-op)
   (args :initarg :args :accessor ir-primop-args))
  (:documentation "A primitive operation (+, -, *, etc.)."))

(defun make-ir-primop (op args &optional type)
  (make-instance 'ir-primop :op op :args args :type type))
