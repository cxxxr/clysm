;;;; nodes.lisp - AST node definitions for clysm
;;;; Converted from CLOS to defstruct for bootstrap compatibility

(in-package #:clysm/ast)

;;; Base AST Node

(defstruct ast-node
  "Base structure for all AST nodes."
  (source-info nil))

;;; Literal/Constant Node

(defstruct (const-node (:include ast-node))
  "A constant value (number, string, etc.)."
  (value nil))

;;; Variable Reference Node

(defstruct (var-node (:include ast-node))
  "A variable reference."
  (name nil))

;;; If Node

(defstruct (if-node (:include ast-node))
  "A conditional expression."
  (test nil)
  (then nil)
  (else nil))

;;; Let Node

(defstruct (let-node (:include ast-node))
  "A let binding expression."
  (bindings nil)
  (body nil))

;;; Lambda Node

(defstruct (lambda-node (:include ast-node))
  "A lambda expression."
  (params nil)
  (body nil))

;;; Function Call Node

(defstruct (call-node (:include ast-node))
  "A function call."
  (func nil)
  (args nil))

;;; Progn Node (Sequence)

(defstruct (progn-node (:include ast-node))
  "A sequence of expressions."
  (forms nil))

;;; Setq Node

(defstruct (setq-node (:include ast-node))
  "A variable assignment."
  (var nil)
  (value nil))

;;; Quote Node

(defstruct (quote-node (:include ast-node))
  "A quoted form."
  (value nil))

;;; Defun Node

(defstruct (defun-node (:include ast-node))
  "A function definition."
  (name nil)
  (params nil)
  (body nil))
