;;;; package.lisp - Clysm Package Definitions
;;;;
;;;; No external dependencies - all utilities are self-contained

(defpackage #:clysm
  (:use #:cl)
  (:export
   ;; Utilities (util.lisp)
   #:hash-table-keys
   #:hash-table-values
   #:flatten
   #:mappend
   #:lastcar
   #:ensure-list
   #:alist-get
   #:plist-get
   #:split-sequence
   #:if-let
   #:when-let
   #:if-let*
   #:with-gensyms
   #:once-only
   #:bytes-to-vector
   #:vector-to-bytes
   #:concat-bytes
   #:string-join
   #:string-prefix-p
   #:string-suffix-p
   #:read-file-bytes
   #:write-file-bytes

   ;; Conditions (conditions.lisp)
   #:clysm-error
   #:clysm-compile-error
   #:clysm-runtime-error
   #:clysm-syntax-error
   #:clysm-type-error
   #:clysm-undefined-error
   #:clysm-wasm-error
   #:compile-error
   #:syntax-error
   #:type-error*
   #:undefined-error
   #:wasm-error

   ;; LEB128 encoding (backend/leb128.lisp)
   #:encode-uleb128
   #:encode-sleb128
   #:decode-uleb128
   #:decode-sleb128
   #:encode-u32
   #:encode-s32
   #:encode-s33
   #:encode-u64
   #:encode-s64
   #:encode-vector
   #:encode-byte-vector
   #:encode-name
   #:encode-f32
   #:encode-f64

   ;; Wasm types (backend/wasm-types.lisp)
   #:encode-valtype
   #:make-functype
   #:wasm-functype
   #:wasm-functype-params
   #:wasm-functype-results
   #:encode-functype
   #:make-field
   #:wasm-field
   #:wasm-field-type
   #:wasm-field-mutable
   #:make-structtype
   #:wasm-structtype
   #:wasm-structtype-fields
   #:encode-structtype
   #:make-arraytype
   #:wasm-arraytype
   #:wasm-arraytype-element
   #:wasm-arraytype-mutable
   #:encode-arraytype
   #:make-wasm-type
   #:wasm-type
   #:wasm-type-index
   #:wasm-type-name
   #:wasm-type-definition
   #:make-wasm-func
   #:wasm-func
   #:wasm-func-index
   #:wasm-func-name
   #:wasm-func-type-index
   #:wasm-func-locals
   #:wasm-func-body
   #:make-import
   #:wasm-import
   #:make-export
   #:wasm-export
   #:encode-export
   #:make-limits
   #:wasm-limits
   #:wasm-limits-min
   #:wasm-limits-max
   #:encode-limits
   #:make-memory
   #:wasm-memory
   #:make-table
   #:wasm-table
   #:make-global
   #:wasm-global
   #:make-wasm-module
   #:wasm-module
   #:wasm-module-types
   #:wasm-module-imports
   #:wasm-module-funcs
   #:wasm-module-tables
   #:wasm-module-memories
   #:wasm-module-globals
   #:wasm-module-exports
   #:wasm-module-start
   #:wasm-module-elements
   #:wasm-module-data
   #:wasm-module-custom
   #:module-add-type
   #:module-add-func
   #:module-add-export
   #:module-finalize

   ;; Sections (backend/sections.lisp)
   #:emit-section
   #:emit-type-section
   #:emit-import-section
   #:emit-function-section
   #:emit-table-section
   #:emit-memory-section
   #:emit-global-section
   #:emit-export-section
   #:emit-start-section
   #:emit-code-section
   #:emit-custom-section

   ;; Wasm emit (backend/wasm-emit.lisp)
   #:emit-wasm-binary
   #:emit-wasm-to-file
   #:emit-wasm-to-vector
   #:opcode
   #:emit-op
   #:emit-i32.const
   #:emit-i64.const
   #:emit-local.get
   #:emit-local.set
   #:emit-call
   #:emit-end
   #:encode-blocktype

   ;; WAT print (backend/wat-print.lisp)
   #:print-wat
   #:wat-to-string

   ;; Runtime types (runtime/types.lisp)
   ;; Type registry
   #:type-registry
   #:make-type-registry
   #:type-registry-func-0
   #:type-registry-func-1
   #:type-registry-func-2
   #:type-registry-func-n
   #:type-registry-cons
   #:type-registry-symbol
   #:type-registry-closure
   #:type-registry-env
   #:type-registry-string
   #:type-registry-vector
   #:type-registry-nil-type
   #:type-registry-unbound
   #:register-core-types

   ;; Type index accessors
   #:cons-type-index
   #:symbol-type-index
   #:closure-type-index
   #:env-type-index
   #:string-type-index
   #:vector-type-index
   #:nil-type-index
   #:unbound-type-index

   ;; Field index constants
   #:+cons-car+
   #:+cons-cdr+
   #:+symbol-name+
   #:+symbol-value+
   #:+symbol-function+
   #:+symbol-plist+
   #:+closure-code-0+
   #:+closure-code-1+
   #:+closure-code-2+
   #:+closure-code-n+
   #:+closure-env+
   #:+nil-car+
   #:+nil-cdr+
   #:+nil-name+
   #:+nil-value+
   #:+nil-function+
   #:+nil-plist+

   ;; GC instruction emitters
   #:emit-struct.new
   #:emit-struct.get
   #:emit-struct.set
   #:emit-array.new
   #:emit-array.new-fixed
   #:emit-array.get
   #:emit-array.set
   #:emit-array.len
   #:emit-ref.i31
   #:emit-i31.get-s
   #:emit-i31.get-u
   #:emit-ref.test
   #:emit-ref.cast

   ;; High-level object emitters
   #:emit-make-cons
   #:emit-car
   #:emit-cdr
   #:emit-rplaca
   #:emit-rplacd
   #:emit-fixnum-from-i32
   #:emit-fixnum-to-i32
   #:emit-consp
   #:emit-symbolp
   #:emit-functionp

   ;; Primitives (compiler/primitives.lisp)
   ;; Primitive registry
   #:*primitives*
   #:primitive
   #:primitive-name
   #:primitive-arity
   #:primitive-generator
   #:primitive-pure-p
   #:primitive-foldable-p
   #:register-primitive
   #:find-primitive
   #:primitivep
   #:init-primitives

   ;; Fixnum arithmetic emitters
   #:emit-fixnum-add
   #:emit-fixnum-sub
   #:emit-fixnum-mul
   #:emit-fixnum-div
   #:emit-fixnum-rem
   #:emit-fixnum-negate

   ;; Fixnum comparison emitters
   #:emit-fixnum-lt
   #:emit-fixnum-le
   #:emit-fixnum-gt
   #:emit-fixnum-ge
   #:emit-fixnum-eq
   #:emit-fixnum-neq
   #:emit-fixnum-zerop
   #:emit-fixnum-plusp
   #:emit-fixnum-minusp

   ;; Cons operation emitters
   #:emit-cons
   #:emit-car-op
   #:emit-cdr-op
   #:emit-rplaca-op
   #:emit-rplacd-op
   #:emit-set-car
   #:emit-set-cdr

   ;; Type predicate emitters
   #:emit-null-p
   #:emit-consp-op
   #:emit-atom-op
   #:emit-symbolp-op
   #:emit-numberp-op
   #:emit-fixnump-op
   #:emit-functionp-op
   #:emit-stringp-op
   #:emit-vectorp-op
   #:emit-listp-op

   ;; Equality emitters
   #:emit-eq-op
   #:emit-eql-op

   ;; Symbol operation emitters
   #:emit-symbol-name
   #:emit-symbol-value
   #:emit-symbol-function
   #:emit-symbol-plist
   #:emit-set-symbol-value
   #:emit-set-symbol-function
   #:emit-set-symbol-plist

   ;; Vector operation emitters
   #:emit-make-vector
   #:emit-vector-ref
   #:emit-vector-set
   #:emit-vector-length

   ;; Closure operation emitters
   #:emit-closure-env
   #:emit-make-env
   #:emit-env-ref
   #:emit-env-set

   ;; Boolean conversion
   #:emit-i32-to-boolean
   #:emit-boolean-to-i32

   ;; AST (compiler/ast.lisp)
   #:ast-node
   #:ast-node-source
   #:ast-literal
   #:make-ast-literal
   #:ast-literal-value
   #:ast-literal-type
   #:ast-quote
   #:make-ast-quote
   #:ast-quote-value
   #:ast-var
   #:make-ast-var
   #:ast-var-name
   #:ast-setq
   #:make-ast-setq
   #:ast-setq-name
   #:ast-setq-value
   #:ast-if
   #:make-ast-if
   #:ast-if-test
   #:ast-if-then
   #:ast-if-else
   #:ast-progn
   #:make-ast-progn
   #:ast-progn-forms
   #:ast-block
   #:make-ast-block
   #:ast-block-name
   #:ast-block-body
   #:ast-return-from
   #:make-ast-return-from
   #:ast-return-from-name
   #:ast-return-from-value
   #:ast-let
   #:make-ast-let
   #:ast-let-bindings
   #:ast-let-body
   #:ast-let-sequential-p
   #:ast-lambda
   #:make-ast-lambda
   #:ast-lambda-params
   #:ast-lambda-body
   #:ast-lambda-name
   #:ast-call
   #:make-ast-call
   #:ast-call-func
   #:ast-call-args
   #:ast-primitive-call
   #:make-ast-primitive-call
   #:ast-primitive-call-name
   #:ast-primitive-call-args
   #:ast-defun
   #:make-ast-defun
   #:ast-defun-name
   #:ast-defun-params
   #:ast-defun-body
   #:ast-defvar
   #:make-ast-defvar
   #:ast-defvar-name
   #:ast-defvar-value
   #:ast-defvar-special-p
   #:*special-forms*
   #:register-special-form
   #:special-form-p
   #:parse-sexp
   #:init-special-forms

   ;; Compile environment (compiler/env.lisp)
   #:binding
   #:make-binding
   #:binding-name
   #:binding-kind
   #:binding-index
   #:binding-mutable-p
   #:binding-type
   #:compile-env
   #:compile-env-p
   #:make-compile-env
   #:compile-env-locals
   #:compile-env-local-count
   #:compile-env-captures
   #:compile-env-closure-depth
   #:compile-env-tail-position-p
   #:compile-env-blocks
   #:compile-env-type-registry
   #:compile-env-functions
   #:compile-env-parent
   #:env-lookup
   #:env-lookup-local
   #:env-bind-local
   #:env-bind-param
   #:env-bind-closure
   #:env-push-scope
   #:env-pop-scope
   #:env-push-block
   #:env-pop-block
   #:env-find-block
   #:env-in-tail-position
   #:env-not-in-tail-position
   #:analyze-free-variables
   #:env-collect-local-types

   ;; Code generation (compiler/codegen.lisp)
   #:codegen-context
   #:codegen-context-p
   #:make-codegen-context
   #:codegen-context-module
   #:codegen-context-type-registry
   #:compile-toplevel
   #:compile-expression
   #:compile-defun
   #:compile-forms
   #:compile-to-wasm))

(defpackage #:clysm/backend
  (:use #:cl #:clysm)
  (:documentation "WebAssembly binary generation backend"))

(defpackage #:clysm/runtime
  (:use #:cl #:clysm)
  (:documentation "Runtime type definitions for Lisp objects"))

(defpackage #:clysm/compiler
  (:use #:cl #:clysm)
  (:documentation "Common Lisp to WebAssembly compiler"))
