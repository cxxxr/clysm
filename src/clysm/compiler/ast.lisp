;;;; ast.lisp - Abstract Syntax Tree definitions

(in-package #:clysm/compiler/ast)

(defstruct (source-location (:conc-name sl-))
  "Source code location for error reporting."
  (file nil :type (or null string))
  (line 0 :type fixnum)
  (column 0 :type fixnum))

(defstruct (ast-node (:conc-name ast-node-))
  "Base AST node."
  (source-location nil :type (or null source-location)))

(defstruct (ast-literal (:include ast-node) (:conc-name ast-literal-))
  "Literal value node."
  (value nil :type t))

(defstruct (ast-var-ref (:include ast-node) (:conc-name ast-var-ref-))
  "Variable reference node."
  (name nil :type symbol)
  (binding nil :type t))  ; Will be filled during analysis

(defstruct (ast-call (:include ast-node) (:conc-name ast-call-))
  "Function call node."
  (function nil :type t)
  (arguments nil :type list))

(defstruct (ast-lambda (:include ast-node) (:conc-name ast-lambda-))
  "Lambda expression node."
  (parameters nil :type list)
  (body nil :type list)
  (free-vars nil :type list))

(defstruct (ast-let (:include ast-node) (:conc-name ast-let-))
  "Let binding node."
  (bindings nil :type list)  ; ((name . value-ast) ...)
  (body nil :type list)
  (sequential-p nil :type boolean))  ; T for let*, NIL for let

(defstruct (ast-if (:include ast-node) (:conc-name ast-if-))
  "Conditional node."
  (test nil :type t)
  (then nil :type t)
  (else nil :type t))

(defstruct (ast-block (:include ast-node) (:conc-name ast-block-))
  "Block node for non-local exits."
  (name nil :type symbol)
  (body nil :type list))

(defstruct (ast-return-from (:include ast-node) (:conc-name ast-return-from-))
  "Return-from node."
  (block-name nil :type symbol)
  (value nil :type t))
