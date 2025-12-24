;;;; package.lisp - Package definitions for Clysm

(defpackage #:clysm/backend/leb128
  (:use #:cl)
  (:export #:encode-unsigned-leb128
           #:encode-signed-leb128
           #:decode-unsigned-leb128
           #:decode-signed-leb128
           #:encode-unsigned-leb128-to-stream
           #:encode-signed-leb128-to-stream))

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
           ;; Section structure
           #:make-section
           #:section-id
           #:section-content
           #:encode-section
           ;; Wasm global structure
           #:wasm-global
           #:make-wasm-global
           #:wasm-global-name
           #:wasm-global-type
           #:wasm-global-mutability
           #:wasm-global-init-expr
           ;; Section builders
           #:make-type-section
           #:make-import-section
           #:make-global-section
           #:make-function-section
           #:make-code-section
           #:make-export-section
           ;; Import/Export encoding (FFI Foundation)
           #:encode-import
           ;; Validation
           #:validate-section-order))

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
           ;; Literals
           #:ast-literal
           #:ast-literal-value
           #:ast-literal-literal-type
           #:make-ast-literal
           #:make-fixnum-literal
           #:make-nil-literal
           #:make-t-literal
           ;; Numeric tower literals (010-numeric-tower)
           #:+i31-min+
           #:+i31-max+
           #:i31-range-p
           #:make-bignum-literal
           #:make-ratio-literal
           #:make-float-literal
           #:make-complex-literal
           #:make-integer-literal
           ;; Variable references
           #:ast-var-ref
           #:ast-var-ref-name
           #:ast-var-ref-binding
           #:make-ast-var-ref
           #:make-var-ref
           ;; Function calls
           #:ast-call
           #:ast-call-function
           #:ast-call-arguments
           #:ast-call-call-type
           #:make-ast-call
           #:make-call
           ;; Lambda
           #:ast-lambda
           #:ast-lambda-parameters
           #:ast-lambda-body
           #:ast-lambda-free-vars
           #:make-ast-lambda
           ;; Defun
           #:ast-defun
           #:ast-defun-name
           #:ast-defun-parameters
           #:ast-defun-body
           #:ast-defun-docstring
           #:make-ast-defun
           ;; Let binding
           #:ast-let
           #:ast-let-bindings
           #:ast-let-body
           #:ast-let-sequential-p
           #:make-ast-let
           ;; Setq
           #:ast-setq
           #:ast-setq-name
           #:ast-setq-value
           #:make-ast-setq
           ;; Conditionals
           #:ast-if
           #:ast-if-test
           #:ast-if-then
           #:ast-if-else
           #:make-ast-if
           ;; Progn
           #:ast-progn
           #:ast-progn-forms
           #:make-ast-progn
           ;; Block/return-from
           #:ast-block
           #:ast-block-name
           #:ast-block-body
           #:make-ast-block
           #:ast-return-from
           #:ast-return-from-block-name
           #:ast-return-from-value
           #:make-ast-return-from
           ;; Flet/labels
           #:ast-flet
           #:ast-flet-definitions
           #:ast-flet-body
           #:make-ast-flet
           #:ast-labels
           #:ast-labels-definitions
           #:ast-labels-body
           #:make-ast-labels
           ;; Tagbody/go
           #:ast-tagbody
           #:ast-tagbody-tags
           #:ast-tagbody-segments
           #:make-ast-tagbody
           #:ast-go
           #:ast-go-tag
           #:make-ast-go
           ;; Catch/throw
           #:ast-catch
           #:ast-catch-tag
           #:ast-catch-body
           #:make-ast-catch
           #:ast-throw
           #:ast-throw-tag
           #:ast-throw-value
           #:make-ast-throw
           ;; Unwind-protect
           #:ast-unwind-protect
           #:ast-unwind-protect-protected-form
           #:ast-unwind-protect-cleanup-forms
           #:make-ast-unwind-protect
           ;; Special variable definitions (T017-T018)
           #:ast-defvar
           #:ast-defvar-name
           #:ast-defvar-init-form
           #:ast-defvar-docstring
           #:make-ast-defvar
           #:ast-defparameter
           #:ast-defparameter-name
           #:ast-defparameter-init-form
           #:ast-defparameter-docstring
           #:make-ast-defparameter
           ;; Parsing
           #:parse-expr))

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
           #:extend-env
           ;; Special variable registry (T005-T008)
           #:*special-variables*
           #:special-info
           #:special-info-declared-p
           #:special-info-has-init-form
           #:special-info-source-location
           #:register-special-variable
           #:special-variable-p
           #:get-special-info
           #:clear-special-variables))

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
  (:export ;; Registry
           #:make-macro-registry
           #:registry-p
           #:register-macro
           #:macro-function*
           #:macro-form-p
           ;; Compile-time environment
           #:make-compile-env
           #:compile-env-p
           ;; Expansion
           #:macroexpand-1*
           #:macroexpand*
           #:macroexpand-all
           ;; Backquote
           #:expand-backquote
           ;; Defmacro parsing
           #:parse-defmacro
           #:defmacro-result-p))

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
           #:+type-standard-class+
           ;; Dynamic binding support (T003-T004)
           #:+type-binding-frame+
           #:make-binding-frame-type
           #:emit-binding-stack-global
           ;; Function type indices
           #:+type-func-0+
           #:+type-func-1+
           #:+type-func-2+
           #:+type-func-3+
           #:+type-func-n+
           ;; Type structures
           #:gc-type
           #:gc-type-index
           #:wasm-struct-type
           #:wasm-array-type
           #:wasm-func-type
           #:wasm-field
           #:make-wasm-struct-type
           #:make-wasm-array-type
           #:make-wasm-func-type
           #:make-wasm-field
           #:struct-fields
           #:wasm-struct-type-fields
           ;; Type constructors
           #:make-nil-type
           #:make-unbound-type
           #:make-cons-type
           #:make-symbol-type
           #:make-string-type
           #:make-closure-type
           ;; Function type constructors (T075)
           #:make-func-type-0
           #:make-func-type-1
           #:make-func-type-2
           #:make-func-type-n
           ;; Numeric tower type indices (010-numeric-tower)
           #:+type-bignum+
           #:+type-ratio+
           #:+type-float+
           #:+type-complex+
           #:+type-limb-array+
           ;; Numeric tower type constructors
           #:make-bignum-type
           #:make-ratio-type
           #:make-float-type
           #:make-complex-type
           #:make-limb-array-type
           ;; Exception tags (010-numeric-tower)
           #:+tag-division-by-zero+
           #:emit-division-by-zero-tag
           ;; Stream type (015-ffi-stream-io)
           #:+type-stream+
           #:make-stream-type
           #:emit-value-type-extended
           ;; Type predicates for struct/array
           #:wasm-struct-type-p
           #:wasm-array-type-p
           #:wasm-struct-type-name
           #:wasm-array-type-name
           #:wasm-array-type-element-type
           #:wasm-array-type-mutable
           #:wasm-field-name
           #:wasm-field-type
           #:wasm-field-mutable
           ;; Type environment
           #:make-type-environment
           #:register-type
           #:get-type-index
           #:fixnum-representation
           #:generate-type-definitions))

(defpackage #:clysm/compiler/codegen/type-section
  (:use #:cl #:clysm/compiler/codegen/wasm-ir #:clysm/compiler/codegen/gc-types)
  (:export #:generate-type-section))

(defpackage #:clysm/compiler/codegen/func-section
  (:use #:cl #:clysm/compiler/ast #:clysm/compiler/codegen/wasm-ir)
  (:export #:generate-func-section
           #:compile-expression
           #:compile-to-instructions
           #:compile-defun
           #:make-env
           #:compilation-env
           #:make-compilation-env
           #:cenv-local-counter
           #:env-add-local
           #:env-lookup-local
           #:env-local-type
           #:env-add-function
           #:env-lookup-function
           #:env-set-function-counter
           ;; Lambda support (T081-T087)
           #:reset-lambda-state
           #:compile-pending-lambdas
           #:*pending-lambdas*
           ;; Special variable global tracking (T025)
           #:reset-special-var-globals
           #:allocate-special-var-global
           #:get-special-var-global-index
           #:get-all-special-var-globals
           ;; Tail position management (TCO)
           #:env-with-tail-position
           #:env-with-non-tail
           #:env-with-tail))

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
  (:shadow #:parse-error)
  (:export #:parse
           #:parse-all
           #:parse-error
           #:parse-error-message
           #:parse-error-line
           #:parse-error-column))

(defpackage #:clysm/reader/package
  (:use #:cl)
  (:export ;; Package error condition
           #:clysm-package-error
           #:clysm-package-error-package
           #:clysm-package-error-message
           ;; Package predicates
           #:packagep*
           #:symbol-package*
           ;; Package accessors (US3 support)
           #:package-name*
           #:package-nicknames*
           #:package-internal-symbols*
           #:package-external-symbols*
           #:package-use-list*
           #:package-used-by-list*
           #:package-shadowing-symbols*
           ;; Core functions
           #:intern-symbol
           #:find-symbol*
           #:make-package*
           #:find-package*
           #:delete-package*
           #:rename-package*
           #:list-all-packages*
           ;; Export/Import/Shadow functions (US2)
           #:export*
           #:unexport*
           #:import*
           #:shadow*
           #:shadowing-import*
           ;; Multiple-value intern/find-symbol (US5)
           #:intern*
           #:find-symbol**
           #:unintern*
           ;; Use-package/Unuse-package (US6)
           #:use-package*
           #:unuse-package*
           ;; Special variables
           #:*current-package*
           #:*packages*))

(defpackage #:clysm/reader
  (:use #:cl #:clysm/reader/tokenizer #:clysm/reader/parser #:clysm/reader/package)
  (:shadowing-import-from #:clysm/reader/parser #:parse-error)
  (:export #:read-from-string*
           #:read*
           #:read-all*))

(defpackage #:clysm/runtime/objects
  (:use #:cl #:clysm/backend/sections)
  (:export #:+nil+
           #:+unbound+
           #:make-cons
           #:car*
           #:cdr*
           #:make-symbol*
           ;; NIL/UNBOUND singletons
           #:make-nil-global
           #:make-unbound-global
           #:nil-global-index
           #:unbound-global-index
           #:global-mutability
           #:emit-nil-check
           ;; Symbol/function management
           #:register-function
           #:lookup-function
           #:generate-runtime-globals
           #:allocate-global
           #:reset-global-counter))

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
  (:export #:interpret
           ;; Environment functions
           #:make-interpreter-env
           #:env-bind
           #:env-lookup
           #:extend-env))

(defpackage #:clysm/eval/jit
  (:use #:cl #:clysm/compiler #:clysm/backend/wasm-emit)
  (:export #:jit-compile
           #:generate-wasm
           #:validate-wasm
           #:instantiate-wasm
           #:extract-function
           #:hotpatch-function))

(defpackage #:clysm/eval
  (:use #:cl #:clysm/eval/interpreter #:clysm/eval/jit)
  (:export #:eval*))

(defpackage #:clysm/eval/compile
  (:use #:cl #:clysm/eval/jit)
  (:export #:compile*))

(defpackage #:clysm/clos/mop
  (:use #:cl)
  (:shadow #:standard-class
           #:class-name
           #:class-precedence-list)
  (:export ;; Class structure
           #:standard-class
           #:make-standard-class
           #:standard-class-p
           #:class-name
           #:class-superclasses
           #:class-slots
           #:class-precedence-list
           #:class-slot-index-table
           ;; Slot definition
           #:slot-definition
           #:make-slot-definition
           #:slot-definition-name
           #:slot-definition-initarg
           #:slot-definition-initform
           #:slot-definition-initform-p
           #:slot-definition-accessor
           #:slot-definition-reader
           #:slot-definition-writer
           ;; Instance structure
           #:standard-instance
           #:standard-instance-p
           #:instance-p
           #:instance-class
           #:instance-slots
           #:make-instance-struct
           ;; Class registry
           #:register-class
           #:find-class*
           ;; CPL computation
           #:compute-class-precedence-list
           #:finalize-class
           ;; Slot index
           #:slot-index
           #:total-slot-count
           #:compute-slot-indices
           ;; Low-level slot access
           #:%slot-value
           #:class-of*))

(defpackage #:clysm/clos/defclass
  (:use #:cl #:clysm/clos/mop)
  (:shadowing-import-from #:clysm/clos/mop
                          #:standard-class #:class-name #:class-precedence-list)
  (:export #:parse-defclass
           #:defclass-result
           #:defclass-result-name
           #:defclass-result-superclasses
           #:defclass-result-slots
           #:slot-spec
           #:slot-spec-name
           #:slot-spec-initarg
           #:slot-spec-initform
           #:slot-spec-accessor
           #:define-class*))

(defpackage #:clysm/clos/instance
  (:use #:cl #:clysm/clos/mop)
  (:shadowing-import-from #:clysm/clos/mop
                          #:standard-class #:class-name #:class-precedence-list)
  (:export #:make-instance*))

(defpackage #:clysm/clos/slot-access
  (:use #:cl #:clysm/clos/instance)
  (:export #:slot-value*
           #:set-slot-value*))

(defpackage #:clysm/clos/generic
  (:use #:cl)
  (:shadow #:generic-function)
  (:export #:defgeneric*
           #:generic-function
           #:generic-function-p
           #:make-generic-function
           #:gf-name
           #:gf-methods
           #:gf-lambda-list
           #:gf-add-method
           #:find-gf
           #:register-gf
           #:method*
           #:make-method*
           #:method*-specializers
           #:method*-qualifier
           #:method*-function
           #:method*-lambda-list))

(defpackage #:clysm/clos/defmethod
  (:use #:cl #:clysm/clos/generic)
  (:shadowing-import-from #:clysm/clos/generic #:generic-function)
  (:export #:parse-defmethod
           #:add-method*
           #:method-result
           #:method-result-name
           #:method-result-qualifier
           #:method-result-specializers
           #:method-result-lambda-list
           #:define-method*)
  ;; Re-export make-method* from generic
  (:import-from #:clysm/clos/generic #:make-method*)
  (:export #:make-method*))

(defpackage #:clysm/clos/dispatch
  (:use #:cl #:clysm/clos/generic #:clysm/clos/mop)
  (:shadowing-import-from #:clysm/clos/generic #:generic-function)
  (:shadowing-import-from #:clysm/clos/mop
                          #:standard-class #:class-name #:class-precedence-list)
  (:shadow #:compute-applicable-methods)
  (:export #:compute-applicable-methods
           #:dispatch))

(defpackage #:clysm/clos/combination
  (:use #:cl #:clysm/clos/generic)
  (:shadowing-import-from #:clysm/clos/generic #:generic-function)
  (:export #:call-next-method*
           #:next-method-p*
           #:*next-methods*
           #:*current-args*
           #:standard-method-combination))

(defpackage #:clysm/clos/method-combination
  (:use #:cl #:clysm/clos/combination)
  (:shadowing-import-from #:clysm/clos/dispatch #:compute-applicable-methods)
  (:shadowing-import-from #:clysm/clos/generic #:generic-function)
  (:export #:standard-method-combination))

(defpackage #:clysm/lib/macros
  (:use #:cl)
  (:export #:when*
           #:unless*
           #:cond*
           #:dolist*
           #:dotimes*
           #:and*
           #:or*
           #:install-standard-macros))

(defpackage #:clysm/lib/package-macros
  (:use #:cl)
  (:export #:defpackage*
           #:in-package*))

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
