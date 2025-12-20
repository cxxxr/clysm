;;;; ir.lisp - Intermediate Representation for clysm
;;;; Converted from CLOS to defstruct for bootstrap compatibility

(in-package #:clysm/ir)

;;; IR Node Base

(defstruct ir-node
  "Base structure for IR nodes."
  (type nil))

;;; IR Constant

(defstruct (ir-const (:include ir-node))
  "A constant value in IR."
  (value nil))

;;; IR Local Variable Reference

(defstruct (ir-local-ref (:include ir-node))
  "A reference to a local variable."
  (index 0))

;;; IR Local Variable Set

(defstruct (ir-local-set (:include ir-node))
  "Set a local variable."
  (index 0)
  (value nil))

;;; IR If

(defstruct (ir-if (:include ir-node))
  "A conditional in IR."
  (test nil)
  (then nil)
  (else nil))

;;; IR Block

(defstruct (ir-block (:include ir-node))
  "A labeled block for control flow."
  (label nil)
  (body nil))

;;; IR Loop

(defstruct (ir-loop (:include ir-node))
  "A loop construct."
  (label nil)
  (body nil))

;;; IR Branch

(defstruct (ir-br (:include ir-node))
  "A branch to a label."
  (label nil)
  (condition nil))

;;; IR Function Call

(defstruct (ir-call (:include ir-node))
  "A function call."
  (func nil)
  (args nil))

;;; IR Return

(defstruct (ir-return (:include ir-node))
  "Return from function."
  (value nil))

;;; IR Sequence

(defstruct (ir-seq (:include ir-node))
  "A sequence of IR nodes."
  (forms nil))

;;; IR Primitive Operation

(defstruct (ir-primop (:include ir-node))
  "A primitive operation (+, -, *, etc.)."
  (op nil)
  (args nil))
