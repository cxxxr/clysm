;;;; package.lisp - Package definitions for Clysm

(defpackage #:clysm/backend/leb128
  (:use #:cl)
  (:export #:encode-unsigned-leb128
           #:encode-signed-leb128
           #:decode-unsigned-leb128
           #:decode-signed-leb128))

(defpackage #:clysm/backend/sections
  (:use #:cl #:clysm/backend/leb128)
  (:export #:+section-id-custom+
           #:+section-id-type+
           #:+section-id-import+
           #:+section-id-function+
           #:+section-id-table+
           #:+section-id-memory+
           #:+section-id-global+
           #:+section-id-export+
           #:+section-id-start+
           #:+section-id-element+
           #:+section-id-code+
           #:+section-id-data+
           #:+section-id-data-count+
           #:+section-id-tag+
           #:make-section
           #:section-id
           #:section-content
           #:encode-section))

(defpackage #:clysm/backend/wasm-emit
  (:use #:cl #:clysm/backend/leb128 #:clysm/backend/sections)
  (:export #:+wasm-magic+
           #:+wasm-version+
           #:emit-module-header
           #:emit-empty-module
           #:emit-module))

(defpackage #:clysm/backend/wat-print
  (:use #:cl)
  (:export #:print-wat
           #:wat-to-string))

(defpackage #:clysm/compiler/ast
  (:use #:cl)
  (:export #:ast-node
           #:ast-node-source-location
           #:ast-literal
           #:ast-literal-value
           #:ast-var-ref
           #:ast-var-ref-name
           #:ast-var-ref-binding
           #:ast-call
           #:ast-call-function
           #:ast-call-arguments
           #:ast-lambda
           #:ast-lambda-parameters
           #:ast-lambda-body
           #:ast-lambda-free-vars
           #:ast-let
           #:ast-let-bindings
           #:ast-let-body
           #:ast-if
           #:ast-if-test
           #:ast-if-then
           #:ast-if-else
           #:ast-block
           #:ast-block-name
           #:ast-block-body
           #:ast-return-from
           #:ast-return-from-block-name
           #:ast-return-from-value))

(defpackage #:clysm/compiler/env
  (:use #:cl)
  (:export #:binding
           #:binding-name
           #:binding-kind
           #:binding-index
           #:binding-mutable-p
           #:lexical-env
           #:lexical-env-bindings
           #:lexical-env-parent
           #:make-lexical-env
           #:lookup-binding
           #:extend-env))

(defpackage #:clysm/compiler/analyzer/free-vars
  (:use #:cl #:clysm/compiler/ast #:clysm/compiler/env)
  (:export #:collect-free-variables))

(defpackage #:clysm/compiler/analyzer/tail-call
  (:use #:cl #:clysm/compiler/ast)
  (:export #:analyze-tail-positions))

(defpackage #:clysm/compiler/analyzer/type-infer
  (:use #:cl #:clysm/compiler/ast)
  (:export #:infer-types))

(defpackage #:clysm/compiler/transform/closure
  (:use #:cl #:clysm/compiler/ast #:clysm/compiler/env)
  (:export #:closure-convert))

(defpackage #:clysm/compiler/transform/macro
  (:use #:cl)
  (:export #:macroexpand-all
           #:register-macro
           #:find-macro))

(defpackage #:clysm/compiler/codegen/wasm-ir
  (:use #:cl)
  (:export #:wasm-instr
           #:wasm-instr-opcode
           #:wasm-instr-operands
           #:wasm-func
           #:wasm-func-name
           #:wasm-func-type-index
           #:wasm-func-locals
           #:wasm-func-body
           #:wasm-type
           #:wasm-type-params
           #:wasm-type-results
           #:wasm-global
           #:wasm-module))

(defpackage #:clysm/compiler/codegen/gc-types
  (:use #:cl #:clysm/compiler/codegen/wasm-ir)
  (:export #:define-gc-types
           #:+type-nil+
           #:+type-unbound+
           #:+type-cons+
           #:+type-symbol+
           #:+type-string+
           #:+type-closure+
           #:+type-instance+
           #:+type-standard-class+))

(defpackage #:clysm/compiler/codegen/type-section
  (:use #:cl #:clysm/compiler/codegen/wasm-ir #:clysm/compiler/codegen/gc-types)
  (:export #:generate-type-section))

(defpackage #:clysm/compiler/codegen/func-section
  (:use #:cl #:clysm/compiler/ast #:clysm/compiler/codegen/wasm-ir)
  (:export #:generate-func-section
           #:compile-expression))

(defpackage #:clysm/compiler
  (:use #:cl
        #:clysm/compiler/ast
        #:clysm/compiler/env
        #:clysm/compiler/codegen/wasm-ir
        #:clysm/backend/wasm-emit)
  (:export #:compile-to-wasm
           #:compile-to-wat
           #:compile-expression))

(defpackage #:clysm/reader/tokenizer
  (:use #:cl)
  (:export #:tokenize
           #:make-tokenizer
           #:next-token))

(defpackage #:clysm/reader/parser
  (:use #:cl #:clysm/reader/tokenizer)
  (:export #:parse
           #:parse-error))

(defpackage #:clysm/reader/package
  (:use #:cl)
  (:export #:intern-symbol
           #:find-symbol*
           #:make-package*
           #:find-package*))

(defpackage #:clysm/reader
  (:use #:cl #:clysm/reader/tokenizer #:clysm/reader/parser #:clysm/reader/package)
  (:export #:read-from-string*
           #:read*))

(defpackage #:clysm/runtime/objects
  (:use #:cl)
  (:export #:+nil+
           #:+unbound+
           #:make-cons
           #:car*
           #:cdr*
           #:make-symbol*))

(defpackage #:clysm/runtime/special-vars
  (:use #:cl)
  (:export #:push-binding
           #:pop-binding
           #:*binding-stack*))

(defpackage #:clysm/runtime/multi-value
  (:use #:cl)
  (:export #:*mv-count*
           #:*mv-buffer*
           #:set-values
           #:get-values))

(defpackage #:clysm/runtime/printer
  (:use #:cl)
  (:export #:print*
           #:prin1*
           #:princ*))

(defpackage #:clysm/eval/interpreter
  (:use #:cl)
  (:export #:interpret))

(defpackage #:clysm/eval/jit
  (:use #:cl #:clysm/compiler #:clysm/backend/wasm-emit)
  (:export #:jit-compile))

(defpackage #:clysm/eval
  (:use #:cl #:clysm/eval/interpreter #:clysm/eval/jit)
  (:export #:eval*))

(defpackage #:clysm/eval/compile
  (:use #:cl #:clysm/eval/jit)
  (:export #:compile*))

(defpackage #:clysm/clos/mop
  (:use #:cl)
  (:export #:standard-class
           #:class-name
           #:class-superclasses
           #:class-slots
           #:class-precedence-list))

(defpackage #:clysm/clos/defclass
  (:use #:cl #:clysm/clos/mop)
  (:export #:parse-defclass))

(defpackage #:clysm/clos/instance
  (:use #:cl #:clysm/clos/mop)
  (:export #:make-instance*))

(defpackage #:clysm/clos/slot-access
  (:use #:cl #:clysm/clos/instance)
  (:export #:slot-value*
           #:set-slot-value*))

(defpackage #:clysm/clos/generic
  (:use #:cl)
  (:export #:defgeneric*
           #:generic-function
           #:generic-function-methods))

(defpackage #:clysm/clos/defmethod
  (:use #:cl #:clysm/clos/generic)
  (:export #:parse-defmethod
           #:add-method*))

(defpackage #:clysm/clos/dispatch
  (:use #:cl #:clysm/clos/generic)
  (:export #:compute-applicable-methods
           #:dispatch))

(defpackage #:clysm/clos/combination
  (:use #:cl #:clysm/clos/dispatch)
  (:export #:call-next-method*
           #:next-method-p*))

(defpackage #:clysm/clos/method-combination
  (:use #:cl #:clysm/clos/combination)
  (:export #:standard-method-combination))

(defpackage #:clysm/lib/macros
  (:use #:cl)
  (:export #:when*
           #:unless*
           #:cond*
           #:dolist*
           #:dotimes*))

(defpackage #:clysm/repl
  (:use #:cl #:clysm/reader #:clysm/eval #:clysm/runtime/printer)
  (:export #:repl
           #:start-repl))

(defpackage #:clysm
  (:use #:cl)
  (:import-from #:clysm/compiler
                #:compile-to-wasm
                #:compile-to-wat)
  (:import-from #:clysm/backend/wasm-emit
                #:emit-empty-module)
  (:import-from #:clysm/backend/leb128
                #:encode-unsigned-leb128
                #:encode-signed-leb128)
  (:import-from #:clysm/repl
                #:repl)
  (:export #:compile-to-wasm
           #:compile-to-wat
           #:emit-empty-module
           #:encode-unsigned-leb128
           #:encode-signed-leb128
           #:repl))
