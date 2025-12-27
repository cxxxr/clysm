;;;; modules.lisp - Module list for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Lists the 45 source modules in compilation order

(in-package #:clysm/stage0)

;;; ============================================================
;;; Compiler Module List
;;; ============================================================

(defvar *compiler-modules*
  '("src/clysm/package.lisp"
    "src/clysm/lib/utf8.lisp"
    "src/clysm/backend/leb128.lisp"
    "src/clysm/backend/sections.lisp"
    "src/clysm/backend/wasm-emit.lisp"
    "src/clysm/backend/wat-print.lisp"
    "src/clysm/compiler/ast.lisp"
    "src/clysm/compiler/env.lisp"
    "src/clysm/compiler/analyzer/free-vars.lisp"
    "src/clysm/compiler/analyzer/tail-call.lisp"
    "src/clysm/compiler/analyzer/type-infer.lisp"
    "src/clysm/compiler/analyzer/io-usage.lisp"
    "src/clysm/compiler/transform/closure.lisp"
    "src/clysm/compiler/transform/macro.lisp"
    "src/clysm/compiler/codegen/wasm-ir.lisp"
    "src/clysm/compiler/codegen/gc-types.lisp"
    "src/clysm/compiler/codegen/type-section.lisp"
    "src/clysm/compiler/codegen/func-section.lisp"
    "src/clysm/compiler/compiler.lisp"
    "src/clysm/reader/tokenizer.lisp"
    "src/clysm/reader/parser.lisp"
    "src/clysm/reader/package.lisp"
    "src/clysm/reader/reader.lisp"
    "src/clysm/runtime/objects.lisp"
    "src/clysm/runtime/special-vars.lisp"
    "src/clysm/runtime/multi-value.lisp"
    "src/clysm/runtime/printer.lisp"
    "src/clysm/runtime/condition-runtime.lisp"
    "src/clysm/eval/interpreter.lisp"
    "src/clysm/eval/interpreter-macros.lisp"
    "src/clysm/eval/interpreter-builtins.lisp"
    "src/clysm/eval/interpreter-file.lisp"
    "src/clysm/eval/jit.lisp"
    "src/clysm/eval/eval.lisp"
    "src/clysm/eval/compile.lisp"
    "src/clysm/clos/mop.lisp"
    "src/clysm/clos/defclass.lisp"
    "src/clysm/clos/instance.lisp"
    "src/clysm/clos/slot-access.lisp"
    "src/clysm/clos/generic.lisp"
    "src/clysm/clos/defmethod.lisp"
    "src/clysm/clos/combination.lisp"
    "src/clysm/clos/dispatch.lisp"
    "src/clysm/clos/method-combination.lisp"
    "src/clysm/repl.lisp")
  "List of compiler source modules in dependency order")

(defun get-module-order ()
  "Get list of module paths in compilation order"
  *compiler-modules*)

(defun get-module-path (module-name)
  "Get full path for module name"
  (find module-name *compiler-modules*
        :test #'string-equal))

(defun module-count ()
  "Get total number of modules"
  (length *compiler-modules*))
