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
   #:module-add-global
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
   #:emit-local.tee
   #:emit-global.get
   #:emit-global.set
   #:emit-call
   #:emit-end
   #:emit-ref.func
   #:emit-call-ref
   #:emit-return-call-ref
   #:emit-ref.null
   #:encode-blocktype
   ;; Exception handling
   #:emit-throw
   #:emit-throw-ref
   #:emit-try-table
   #:encode-catch-clause

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

   ;; Closure operations
   #:emit-make-closure
   #:emit-closure-get-env
   #:emit-closure-get-code
   #:emit-create-env
   #:emit-env-get
   #:emit-env-set-instr

   ;; Function type accessors
   #:func-0-type-index
   #:func-1-type-index
   #:func-2-type-index
   #:func-n-type-index

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
   #:ast-node-p
   #:ast-node-source
   #:ast-literal
   #:ast-literal-p
   #:make-ast-literal
   #:ast-literal-value
   #:ast-literal-type
   #:ast-quote
   #:ast-quote-p
   #:make-ast-quote
   #:ast-quote-value
   #:ast-var
   #:ast-var-p
   #:make-ast-var
   #:ast-var-name
   #:ast-setq
   #:ast-setq-p
   #:make-ast-setq
   #:ast-setq-name
   #:ast-setq-value
   #:ast-if
   #:ast-if-p
   #:make-ast-if
   #:ast-if-test
   #:ast-if-then
   #:ast-if-else
   #:ast-progn
   #:ast-progn-p
   #:make-ast-progn
   #:ast-progn-forms
   #:ast-block
   #:ast-block-p
   #:make-ast-block
   #:ast-block-name
   #:ast-block-body
   #:ast-return-from
   #:ast-return-from-p
   #:make-ast-return-from
   #:ast-return-from-name
   #:ast-return-from-value
   #:ast-let
   #:ast-let-p
   #:make-ast-let
   #:ast-let-bindings
   #:ast-let-body
   #:ast-let-sequential-p
   #:ast-lambda
   #:ast-lambda-p
   #:make-ast-lambda
   #:ast-lambda-params
   #:ast-lambda-body
   #:ast-lambda-name
   #:ast-call
   #:ast-call-p
   #:make-ast-call
   #:ast-call-func
   #:ast-call-args
   #:ast-primitive-call
   #:ast-primitive-call-p
   #:make-ast-primitive-call
   #:ast-primitive-call-name
   #:ast-primitive-call-args
   #:ast-defun
   #:ast-defun-p
   #:make-ast-defun
   #:ast-defun-name
   #:ast-defun-params
   #:ast-defun-body
   #:ast-defvar
   #:ast-defvar-p
   #:make-ast-defvar
   #:ast-defvar-name
   #:ast-defvar-value
   #:ast-defvar-special-p
   #:ast-defmacro
   #:ast-defmacro-p
   #:make-ast-defmacro
   #:ast-defmacro-name
   #:ast-defmacro-lambda-list
   #:ast-defmacro-body
   ;; Macro system
   #:*macros*
   #:register-macro
   #:unregister-macro
   #:get-macro-expander
   #:macrop
   #:clysm-macroexpand-1
   #:clysm-macroexpand
   ;; Control flow AST nodes
   #:ast-tagbody
   #:make-ast-tagbody
   #:ast-tagbody-p
   #:ast-tagbody-segments
   #:ast-go
   #:make-ast-go
   #:ast-go-p
   #:ast-go-tag
   #:ast-catch
   #:make-ast-catch
   #:ast-catch-p
   #:ast-catch-tag
   #:ast-catch-body
   #:ast-throw
   #:make-ast-throw
   #:ast-throw-p
   #:ast-throw-tag
   #:ast-throw-value
   #:ast-unwind-protect
   #:make-ast-unwind-protect
   #:ast-unwind-protect-p
   #:ast-unwind-protect-protected
   #:ast-unwind-protect-cleanup
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
   #:compile-env-codegen-context
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
   ;; Tagbody/Go management
   #:compile-env-tags
   #:env-push-tagbody
   #:env-pop-tagbody
   #:env-find-tag
   ;; Catch/Throw management
   #:compile-env-catch-tags
   #:compile-env-unwind-depth
   #:env-push-catch
   #:env-pop-catch
   #:env-catch-depth
   ;; Special variable management
   #:compile-env-specials
   #:env-declare-special
   #:env-special-p
   ;; Position tracking
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
   #:codegen-context-lambda-counter
   #:codegen-context-specials
   #:codegen-context-special-globals
   #:context-declare-special
   #:context-special-p
   #:context-special-global-index
   #:context-register-special-global
   #:compile-toplevel
   #:compile-expression
   #:compile-defun
   #:compile-forms
   #:compile-to-wasm

   ;; Reader (reader/lexer.lisp, reader/parser.lisp)
   ;; Token types
   #:token
   #:make-token
   #:token-type
   #:token-value
   #:token-line
   #:token-column
   ;; Lexer
   #:lexer
   #:make-lexer
   #:make-lexer-from-string
   #:lexer-input
   #:lexer-position
   #:lexer-length
   #:lexer-line
   #:lexer-column
   #:lexer-eof-p
   #:lexer-peek-char
   #:lexer-read-char
   #:read-token
   #:peek-token
   ;; Parser
   #:reader-state
   #:make-reader-state
   #:*reader-state*
   #:reader-intern
   #:reader-find-symbol
   #:parse-sexpr
   #:expand-backquote
   #:clysm-read-from-string
   #:clysm-read-all-from-string
   #:clysm-read))

;; User package for interned symbols
(defpackage #:clysm-user
  (:use)
  (:documentation "User package for Clysm interned symbols"))

(defpackage #:clysm/backend
  (:use #:cl #:clysm)
  (:documentation "WebAssembly binary generation backend"))

(defpackage #:clysm/runtime
  (:use #:cl #:clysm)
  (:documentation "Runtime type definitions for Lisp objects"))

(defpackage #:clysm/compiler
  (:use #:cl #:clysm)
  (:documentation "Common Lisp to WebAssembly compiler"))
