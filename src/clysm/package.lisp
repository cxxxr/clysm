;;;; package.lisp - Package definitions for Clysm

;;; Forward declaration for FFI package (required by compiler/ast.lisp and codegen)
;;; Full package definition is in ffi/package.lisp
(defpackage #:clysm/ffi
  (:use #:cl)
  (:export #:call-host
           #:*ffi-environment*
           #:lookup-foreign-function
           ;; Foreign function declaration accessors (used by codegen)
           #:ffd-param-types
           #:ffd-return-type
           #:ffd-type-index
           #:ffd-func-index  ; 001-numeric-functions: function index for :call
           ;; Marshalling (used by codegen)
           #:marshal-to-wasm
           #:marshal-from-wasm))

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
           #:ast-literal-p
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
           #:ast-var-ref-p
           #:ast-var-ref-name
           #:ast-var-ref-binding
           #:make-ast-var-ref
           #:make-var-ref
           ;; Function calls
           #:ast-call
           #:ast-call-p
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
           ;; Function reference (043-self-hosting-blockers)
           #:ast-function
           #:ast-function-name
           #:ast-function-local-p
           #:make-ast-function
           ;; Defun
           #:ast-defun
           #:ast-defun-name
           #:ast-defun-parameters
           #:ast-defun-body
           #:ast-defun-docstring
           #:make-ast-defun
           ;; Parameter Info (043-self-hosting-blockers)
           #:ast-param-info
           #:ast-param-info-name
           #:ast-param-info-kind
           #:ast-param-info-default-form
           #:ast-param-info-supplied-p
           #:ast-param-info-keyword
           #:make-ast-param-info
           #:ast-param-info-p
           ;; Keyword Arg Info (043-self-hosting-blockers)
           #:keyword-arg-info
           #:keyword-arg-info-keyword
           #:keyword-arg-info-value
           #:make-keyword-arg-info
           ;; Parsed Lambda List (043-self-hosting-blockers)
           #:ast-parsed-lambda-list
           #:ast-parsed-lambda-list-required
           #:ast-parsed-lambda-list-optional
           #:ast-parsed-lambda-list-rest
           #:ast-parsed-lambda-list-keys
           #:ast-parsed-lambda-list-allow-other-keys
           #:ast-parsed-lambda-list-aux
           #:make-ast-parsed-lambda-list
           #:parse-lambda-list
           #:parse-optional-param
           #:parse-key-param
           #:parse-aux-param
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
           ;; Handler-case (001-control-structure-extension US4)
           #:handler-clause
           #:handler-clause-type
           #:handler-clause-var
           #:handler-clause-body
           #:make-handler-clause
           #:ast-handler-case
           #:ast-handler-case-expression
           #:ast-handler-case-handlers
           #:make-ast-handler-case
           ;; Special variable definitions (T017-T018)
           #:ast-defvar
           #:ast-defvar-p
           #:ast-defvar-name
           #:ast-defvar-init-form
           #:ast-defvar-docstring
           #:make-ast-defvar
           #:ast-defparameter
           #:ast-defparameter-p
           #:ast-defparameter-name
           #:ast-defparameter-init-form
           #:ast-defparameter-docstring
           #:make-ast-defparameter
           ;; Feature 038: defconstant AST
           #:ast-defconstant
           #:ast-defconstant-name
           #:ast-defconstant-value-form
           #:ast-defconstant-docstring
           #:make-ast-defconstant
           #:ast-defconstant-p
           ;; Feature 038: Declaration filtering
           #:filter-declare-forms
           ;; Macro introspection AST (016-macro-system, 042-advanced-defmacro)
           #:ast-macroexpand-1
           #:ast-macroexpand-1-form
           #:make-ast-macroexpand-1
           #:ast-macroexpand
           #:ast-macroexpand-form
           #:make-ast-macroexpand
           #:ast-macro-function
           #:ast-macro-function-name
           #:ast-macro-function-env
           #:make-ast-macro-function
           ;; Multiple values AST (025-multiple-values)
           #:ast-values
           #:ast-values-forms
           #:make-ast-values
           #:ast-multiple-value-bind
           #:ast-mvb-vars
           #:ast-mvb-values-form
           #:ast-mvb-body
           #:make-ast-multiple-value-bind
           #:ast-multiple-value-list
           #:ast-mvl-form
           #:make-ast-multiple-value-list
           #:ast-nth-value
           #:ast-nth-value-index
           #:ast-nth-value-form
           #:make-ast-nth-value
           #:ast-values-list
           #:ast-values-list-form
           #:make-ast-values-list
           #:ast-multiple-value-prog1
           #:ast-mvp1-first-form
           #:ast-mvp1-body
           #:make-ast-multiple-value-prog1
           #:ast-multiple-value-call
           #:ast-mvc-function
           #:ast-mvc-forms
           #:make-ast-multiple-value-call
           ;; CLOS defclass AST (026-clos-foundation)
           #:ast-slot-definition
           #:ast-slot-definition-p
           #:ast-slot-definition-name
           #:ast-slot-definition-initarg
           #:ast-slot-definition-accessor
           #:ast-slot-definition-initform
           #:ast-slot-definition-initform-p
           #:make-ast-slot-definition
           #:ast-defclass
           #:ast-defclass-p
           #:ast-defclass-name
           #:ast-defclass-superclass
           #:ast-defclass-slots
           #:make-ast-defclass
           #:parse-defclass-to-ast
           ;; CLOS make-instance AST (026-clos-foundation)
           #:ast-make-instance
           #:ast-make-instance-p
           #:ast-make-instance-class-name
           #:ast-make-instance-initargs
           #:make-ast-make-instance
           #:parse-make-instance-to-ast
           ;; CLOS defmethod AST (026-clos-foundation)
           #:ast-defmethod
           #:ast-defmethod-p
           #:ast-defmethod-name
           #:ast-defmethod-qualifier
           #:ast-defmethod-specializers
           #:ast-defmethod-lambda-list
           #:ast-defmethod-body
           #:make-ast-defmethod
           #:parse-defmethod-to-ast
           ;; FFI Call AST (027-complete-ffi)
           #:ast-ffi-call
           #:ast-ffi-call-p
           #:ast-ffi-call-declaration
           #:ast-ffi-call-arguments
           #:make-ast-ffi-call
           #:ast-call-host
           #:ast-call-host-p
           #:ast-call-host-function-name
           #:ast-call-host-arguments
           #:make-ast-call-host
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

(defpackage #:clysm/compiler/analyzer/io-usage
  (:use #:cl)
  (:export #:analyze-io-usage
           #:*io-function-names*))

;; Feature 001-ffi-import-architecture: FFI usage analyzer
(defpackage #:clysm/compiler/analyzer/ffi-usage
  (:use #:cl)
  (:export ;; FFI Analysis Result Structure (T003)
           #:ffi-analysis
           #:make-ffi-analysis
           #:ffi-analysis-p
           #:ffi-analysis-used-ffis
           #:ffi-analysis-has-dynamic-call-p
           #:ffi-analysis-static-funcalls
           #:ffi-analysis-dynamic-sites
           ;; FFI Function Registry Access (T006)
           #:get-ffi-function-names
           #:ffi-function-p
           ;; Static/Dynamic Call Detection (T022-T024, T031)
           #:quoted-symbol-p
           #:function-ref-p
           #:detect-static-funcall-p
           #:detect-dynamic-call-p
           ;; Main Analysis Function (T010-T011, T021, T025, T031-T033)
           #:analyze-ffi-usage))

(defpackage #:clysm/compiler/transform/closure
  (:use #:cl #:clysm/compiler/ast #:clysm/compiler/env)
  (:export #:closure-convert))

(defpackage #:clysm/compiler/transform/macro
  (:use #:cl)
  (:shadow #:macroexpand #:macroexpand-1 #:macro-function)
  (:export ;; Registry
           #:make-macro-registry
           #:registry-p
           #:register-macro
           #:macro-function*
           #:macro-form-p
           ;; Conditions
           #:macro-expansion-depth-exceeded
           #:expansion-depth
           #:macro-name
           #:*macro-expansion-limit*
           ;; Compile-time environment
           #:make-compile-env
           #:compile-env-p
           ;; Expansion (registry-based)
           #:macroexpand-1*
           #:macroexpand*
           #:macroexpand-all
           ;; Expansion (global registry)
           #:macroexpand
           #:macroexpand-1
           #:*global-macro-registry*
           #:global-macro-registry
           #:reset-global-macro-registry
           ;; ANSI CL macro-function (042-advanced-defmacro)
           #:macro-function
           ;; Backquote
           #:expand-backquote
           ;; Defmacro parsing
           #:parse-defmacro
           #:defmacro-result-p
           #:defmacro-result-lambda-info
           #:compile-defmacro
           ;; Macro lambda-list parsing (042-advanced-defmacro)
           #:parse-macro-lambda-list
           #:extract-whole-param
           #:extract-environment-param
           #:macro-lambda-list-info
           #:macro-lambda-list-info-p
           #:macro-lambda-list-info-whole-var
           #:macro-lambda-list-info-env-var
           #:macro-lambda-list-info-required
           #:macro-lambda-list-info-optional
           #:macro-lambda-list-info-rest-var
           #:macro-lambda-list-info-rest-kind
           #:macro-lambda-list-info-keys
           #:macro-lambda-list-info-allow-other-keys
           ;; Macro environment (042-advanced-defmacro)
           #:make-macro-environment
           #:macro-environment-p
           #:macro-environment-local-macros
           #:macro-environment-parent
           #:env-macro-function
           #:extend-environment
           ;; Conditions (042-advanced-defmacro)
           #:macro-lambda-list-malformed
           #:mlf-lambda-list
           #:mlf-reason
           #:original-form
           ;; Registry extension
           #:unregister-macro))

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
           ;; Multiple values array type (025-multiple-values)
           #:+type-mv-array+
           #:make-mv-array-type
           ;; CLOS Foundation type indices (026-clos-foundation)
           #:+type-slot-vector+
           #:+type-keyword-array+
           #:+type-closure-array+
           ;; FFI type indices (027-complete-ffi)
           #:+type-anyref-array+
           ;; Macro environment type (042-advanced-defmacro)
           #:+type-macro-environment+
           #:make-macro-environment-type
           ;; Hash table types (043-self-hosting-blockers)
           #:+type-hash-entry+
           #:+type-hash-table+
           #:+type-bucket-array+
           ;; Multidimensional array type (001-ansi-array-ops)
           #:+type-mdarray+
           #:make-mdarray-type
           ;; Hash table type constructors (043-self-hosting-blockers)
           #:make-hash-entry-type
           #:make-hash-table-type
           #:make-bucket-array-type
           ;; CLOS Foundation type constructors (026-clos-foundation)
           #:make-instance-type
           #:make-standard-class-type
           #:make-slot-vector-type
           #:make-keyword-array-type
           #:make-closure-array-type
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
           #:env-with-tail
           ;; CLOS compile-time class registry (026-clos-foundation)
           #:*class-id-counter*
           #:*class-registry*
           #:reset-class-id-counter
           #:reset-class-registry
           #:allocate-class-id
           #:register-compile-time-class
           #:find-compile-time-class
           #:class-info
           #:class-info-name
           #:class-info-superclass
           #:class-info-slots
           #:class-info-class-id
           #:class-info-finalized-p
           #:slot-info
           #:slot-info-name
           #:slot-info-initarg
           #:slot-info-accessor
           #:slot-info-initform
           #:slot-info-initform-p
           #:slot-info-index
           #:compile-defclass
           ;; CLOS compile-time generic function registry (026-clos-foundation)
           #:*generic-function-registry*
           #:reset-generic-function-registry
           #:find-generic-function
           #:register-generic-function
           #:gf-info
           #:gf-info-name
           #:gf-info-methods
           #:gf-info-lambda-list
           #:method-info
           #:method-info-specializers
           #:method-info-qualifier
           #:method-info-lambda-list
           #:method-info-body
           #:compile-defmethod
           ;; Internal compiler functions (001-internal-function-consolidation)
           #:compile-unary-math-ffi
           #:compile-cxr-chain))

;; Phase 13D-4: Global variable compilation
(defpackage #:clysm/compiler/codegen/globals
  (:use #:cl
        #:clysm/compiler/ast
        #:clysm/compiler/codegen/func-section)
  (:export ;; Global declaration structure
           #:global-declaration
           #:make-global-declaration
           #:global-decl-name
           #:global-decl-kind
           #:global-decl-init-form
           #:global-decl-init-type
           #:global-decl-global-index
           #:global-decl-docstring
           ;; Registry management
           #:*global-declarations*
           #:*deferred-inits*
           #:reset-global-declarations
           #:register-global-declaration
           #:get-global-declaration
           ;; Init type classification
           #:classify-init-type
           ;; Compilation
           #:compile-defvar
           #:compile-defparameter
           #:compile-global-definition
           ;; Code generation
           #:generate-special-var-globals
           #:generate-init-function-body
           #:has-deferred-inits-p))

;; Phase 13D-4: Wasm global section helpers
(defpackage #:clysm/compiler/codegen/wasm-global
  (:use #:cl)
  (:export ;; Type encoding
           #:encode-ref-null-any-type
           #:encode-global-type
           ;; Init expression encoding
           #:encode-ref-null-any-init
           #:encode-global-get-init
           #:encode-i31-const-init
           ;; Global entry encoding
           #:encode-global-entry
           #:encode-special-var-global
           ;; Section assembly
           #:assemble-global-section))

(defpackage #:clysm/compiler
  (:use #:cl
        #:clysm/compiler/ast
        #:clysm/compiler/env
        #:clysm/compiler/codegen/wasm-ir
        #:clysm/backend/wasm-emit)
  (:export #:compile-to-wasm
           #:compile-to-wat
           #:compile-expression
           ;; Phase 13D-3: Compile-time directive processing
           #:directive-form-p
           #:compile-toplevel-form
           #:eval-directive
           ;; Phase 13D-7: Module compilation for Stage 1
           #:compiled-module
           #:make-compiled-module
           #:compiled-module-functions
           #:compiled-module-globals
           #:compiled-module-exports
           #:compiled-module-main-func-idx
           #:emit-module))

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
           ;; Multiple values globals (025-multiple-values)
           #:*mv-count-global-index*
           #:*mv-buffer-global-index*
           #:make-mv-count-global
           #:make-mv-buffer-global
           #:mv-count-global-index
           #:mv-buffer-global-index
           #:*global-counter*
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

(defpackage #:clysm/runtime/macro
  (:use #:cl)
  (:documentation "Runtime macro expansion support for compiled Wasm code.
   Feature 042: Advanced Defmacro.")
  (:export ;; Runtime macro functions
           #:*runtime-macro-registry*
           #:runtime-macro-function
           #:runtime-macroexpand-1
           #:runtime-macroexpand
           ;; Registry management
           #:register-runtime-macro
           #:clear-runtime-macros))

(defpackage #:clysm/eval/interpreter
  (:use #:cl)
  (:export #:interpret
           ;; Environment functions
           #:make-interpreter-env
           #:env-bind
           #:env-lookup
           #:env-bound-p
           #:extend-env
           #:get-default-env
           ;; Registries (Feature 044)
           #:*struct-registry*
           #:*special-variables*
           #:*constants*
           #:clear-registries
           ;; Struct types (Feature 044)
           #:lambda-list-info
           #:parse-lambda-list
           #:bind-lambda-list-args
           #:interpreter-struct-type
           #:interpreter-struct-instance
           #:interpreted-closure
           ;; Error conditions (Feature 044)
           #:interpreter-error
           #:unbound-variable-error
           #:undefined-function-error
           #:unsupported-feature-error
           #:macro-expansion-depth-error
           ;; File loading (Feature 044)
           #:interpret-file
           #:read-file-forms
           #:filter-body-declarations
           #:load-compiler-modules
           #:interpret-progn))

(defpackage #:clysm/eval/jit
  (:use #:cl #:clysm/compiler #:clysm/backend/wasm-emit)
  (:export #:jit-compile
           #:generate-wasm
           #:validate-wasm
           #:instantiate-wasm
           #:extract-function
           #:hotpatch-function
           ;; Function slot management
           #:*function-slots*
           #:set-function-slot
           #:get-function-slot
           #:reset-function-slots
           ;; Runtime imports
           #:*runtime-imports*
           #:register-runtime-import
           #:get-runtime-import
           ;; GC heap
           #:*gc-heap*
           #:allocate-gc-object
           #:read-gc-object))

(defpackage #:clysm/eval
  (:use #:cl #:clysm/eval/interpreter #:clysm/eval/jit)
  (:export #:eval*))

(defpackage #:clysm/eval/compile
  (:use #:cl #:clysm/eval/jit)
  (:export ;; Main compile function
           #:compile*
           #:compile-with-tier
           ;; Tiered function struct
           #:tiered-function
           #:make-tiered-function
           #:tiered-function-p
           #:tiered-function-name
           #:tiered-function-definition
           #:tiered-function-tier
           #:tiered-function-implementation
           #:tiered-function-invocation-count
           #:tiered-function-promotion-failed-p
           ;; Tier management
           #:*compilation-threshold*
           #:should-promote-to-tier-2-p
           #:record-invocation
           #:promote-to-tier-2
           #:get-current-tier
           ;; Tiered function registry (separate from jit's function slots)
           #:*tiered-functions*
           #:get-tiered-function
           #:lookup-compiled-function
           ;; Testing utilities
           #:reset-invocation-counts
           #:reset-tiered-functions))

(defpackage #:clysm/clos/mop
  (:use #:cl)
  (:shadow #:standard-class
           #:class-name
           #:class-precedence-list
           #:structure-class)
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
           #:class-of*
           ;; Phase 13D-10: Structure class metaclass
           #:structure-class
           #:make-structure-class
           #:structure-class-p
           #:structure-class-copier
           #:structure-class-predicate
           #:structure-class-constructor
           #:structure-type-p
           #:register-structure-class
           #:get-structure-slots
           #:get-structure-parent))

(defpackage #:clysm/clos/defclass
  (:use #:cl #:clysm/clos/mop)
  (:shadowing-import-from #:clysm/clos/mop
                          #:standard-class #:class-name #:class-precedence-list
                          #:structure-class)
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
                          #:standard-class #:class-name #:class-precedence-list
                          #:structure-class)
  (:export #:make-instance*))

(defpackage #:clysm/clos/slot-access
  (:use #:cl #:clysm/clos/instance)
  (:export #:slot-value*
           #:set-slot-value*
           ;; Setf expander generation (028-setf)
           #:make-slot-accessor-setf-expander
           #:register-slot-accessor-setf-expander))

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
                          #:standard-class #:class-name #:class-precedence-list
                          #:structure-class)
  (:shadow #:compute-applicable-methods)
  (:export #:compute-applicable-methods
           #:sort-methods
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

(defpackage #:clysm/lib/setf-expanders
  (:use #:cl)
  (:export ;; Registry
           #:setf-expander-registry
           #:make-setf-expander-registry
           #:setf-expander-registry-table
           #:*global-setf-expander-registry*
           #:register-setf-expander
           #:get-setf-expander
           ;; Expansion protocol
           #:get-setf-expansion*
           #:simple-variable-p
           ;; Standard expander factories
           #:make-car-setf-expander
           #:make-cdr-setf-expander
           #:make-first-setf-expander
           #:make-rest-setf-expander
           #:make-nth-setf-expander
           #:make-second-setf-expander
           #:make-third-setf-expander
           #:make-fourth-setf-expander
           #:make-fifth-setf-expander
           #:make-sixth-setf-expander
           #:make-seventh-setf-expander
           #:make-eighth-setf-expander
           #:make-ninth-setf-expander
           #:make-tenth-setf-expander
           #:make-aref-setf-expander
           #:make-gethash-setf-expander
           #:make-symbol-value-setf-expander
           #:make-symbol-function-setf-expander
           #:make-symbol-plist-setf-expander
           ;; Installation
           #:install-standard-setf-expanders
           ;; User-defined expanders (define-setf-expander*, defsetf*)
           #:define-setf-expander*
           #:defsetf*
           #:make-short-form-setf-expander
           #:make-long-form-setf-expander
           #:parse-define-setf-lambda-list
           #:parse-setf-lambda-list
           ;; Error conditions
           #:setf-error
           #:undefined-setf-expander
           #:undefined-setf-expander-accessor
           #:invalid-place
           #:invalid-place-place
           #:invalid-place-reason
           #:constant-modification-error
           #:odd-argument-count
           #:odd-argument-count-macro
           #:odd-argument-count-count
           ;; Setf primitives for compiler (001-ansi-array-primitives)
           #:%setf-aref
           #:%setf-svref
           #:%setf-schar
           #:%setf-elt))

(defpackage #:clysm/lib/destructuring
  (:use #:cl)
  (:export ;; Parsed lambda-list struct (031-destructuring-bind-macro)
           #:parsed-lambda-list
           #:make-parsed-lambda-list
           #:parsed-lambda-list-p
           #:parsed-lambda-list-whole-var
           #:parsed-lambda-list-required-params
           #:parsed-lambda-list-optional-params
           #:parsed-lambda-list-rest-var
           #:parsed-lambda-list-key-params
           #:parsed-lambda-list-allow-other-keys-p
           ;; Param-spec struct
           #:param-spec
           #:make-param-spec
           #:param-spec-p
           #:param-spec-type
           #:param-spec-var
           #:param-spec-nested-list
           ;; Optional-param-spec struct
           #:optional-param-spec
           #:make-optional-param-spec
           #:optional-param-spec-p
           #:optional-param-spec-param
           #:optional-param-spec-default-form
           #:optional-param-spec-supplied-p
           ;; Key-param-spec struct
           #:key-param-spec
           #:make-key-param-spec
           #:key-param-spec-p
           #:key-param-spec-keyword
           #:key-param-spec-param
           #:key-param-spec-default-form
           #:key-param-spec-supplied-p
           ;; Parser functions
           #:parse-destructuring-lambda-list
           ;; Code generation
           #:generate-destructuring-code))

(defpackage #:clysm/lib/macros
  (:use #:cl)
  (:export #:when*
           #:unless*
           #:cond*
           #:dolist*
           #:dotimes*
           #:and*
           #:or*
           #:install-standard-macros
           ;; Setf macros (028-setf-generalized-refs)
           #:make-setf-expander
           #:make-psetf-expander
           #:make-incf-expander
           #:make-decf-expander
           #:make-push-expander
           #:make-pop-expander
           #:make-pushnew-expander
           #:make-rotatef-expander
           #:make-shiftf-expander
           #:install-setf-macros
           ;; Destructuring-bind (031-destructuring-bind-macro)
           #:make-destructuring-bind-expander
           ;; LOOP macro infrastructure (029-loop-macro)
           #:loop-context
           #:make-loop-context
           #:loop-context-p
           #:loop-context-name
           #:loop-context-iteration-clauses
           #:loop-context-accumulation-clauses
           #:loop-context-termination-clauses
           #:loop-context-body-clauses
           #:loop-context-initially-forms
           #:loop-context-finally-forms
           #:loop-context-with-bindings
           #:loop-context-result-form
           #:loop-context-gensym-counter
           ;; Iteration clause structs
           #:loop-iteration-clause
           #:loop-iteration-clause-p
           #:loop-iteration-clause-var
           #:loop-iteration-clause-clause-type
           #:loop-iter-arithmetic
           #:make-loop-iter-arithmetic
           #:loop-iter-arithmetic-p
           #:loop-iter-arithmetic-from
           #:loop-iter-arithmetic-to
           #:loop-iter-arithmetic-below
           #:loop-iter-arithmetic-above
           #:loop-iter-arithmetic-downto
           #:loop-iter-arithmetic-downfrom
           #:loop-iter-arithmetic-upfrom
           #:loop-iter-arithmetic-by
           #:loop-iter-in
           #:make-loop-iter-in
           #:loop-iter-in-p
           #:loop-iter-in-list-form
           #:loop-iter-in-step-fn
           #:loop-iter-in-list-var
           #:loop-iter-on
           #:make-loop-iter-on
           #:loop-iter-on-p
           #:loop-iter-on-list-form
           #:loop-iter-on-step-fn
           #:loop-iter-on-list-var
           #:loop-iter-across
           #:make-loop-iter-across
           #:loop-iter-across-p
           #:loop-iter-across-vector-form
           #:loop-iter-across-index-var
           #:loop-iter-across-vec-var
           #:loop-iter-hash
           #:make-loop-iter-hash
           #:loop-iter-hash-p
           #:loop-iter-hash-hash-form
           #:loop-iter-hash-value-var
           #:loop-iter-hash-key-var
           #:loop-iter-hash-mode
           #:loop-iter-equals
           #:make-loop-iter-equals
           #:loop-iter-equals-p
           #:loop-iter-equals-init-form
           #:loop-iter-equals-then-form
           ;; Accumulation clause struct
           #:loop-accumulation-clause
           #:make-loop-accumulation-clause
           #:loop-accumulation-clause-p
           #:loop-accumulation-clause-type
           #:loop-accumulation-clause-expr
           #:loop-accumulation-clause-into-var
           #:loop-accumulation-clause-acc-var
           ;; Termination clause struct
           #:loop-termination-clause
           #:make-loop-termination-clause
           #:loop-termination-clause-p
           #:loop-termination-clause-type
           #:loop-termination-clause-expr
           ;; Conditional clause struct
           #:loop-conditional-clause
           #:make-loop-conditional-clause
           #:loop-conditional-clause-p
           #:loop-conditional-clause-type
           #:loop-conditional-clause-condition
           #:loop-conditional-clause-then-clauses
           #:loop-conditional-clause-else-clauses
           ;; LOOP parser functions
           #:*loop-keywords*
           #:loop-keyword-p
           #:loop-keyword-eq
           #:make-loop-gensym
           #:parse-loop-clauses
           #:expand-loop
           #:make-loop-expander
           ;; Typecase macro infrastructure (030-typecase-macros)
           #:type-specifier-to-predicate
           #:construct-expected-type
           #:validate-exhaustive-clauses
           #:make-typecase-expander
           #:make-etypecase-expander
           #:make-check-type-expander
           #:make-ctypecase-expander
           #:install-typecase-macros))

(defpackage #:clysm/lib/package-macros
  (:use #:cl)
  (:export #:defpackage*
           #:in-package*))

;; Phase 13D-10: DEFSTRUCT Wasm Compilation
(defpackage #:clysm/lib
  (:use #:cl)
  (:export ;; slot-definition struct
           #:slot-definition
           #:make-slot-definition
           #:slot-definition-p
           #:slot-definition-name
           #:slot-definition-initform
           #:slot-definition-initform-p
           #:slot-definition-type
           #:slot-definition-read-only
           ;; defstruct-definition struct
           #:defstruct-definition
           #:make-defstruct-definition
           #:defstruct-definition-p
           #:defstruct-definition-name
           #:defstruct-definition-conc-name
           #:defstruct-definition-constructor
           #:defstruct-definition-copier
           #:defstruct-definition-predicate
           #:defstruct-definition-include
           #:defstruct-definition-include-slot-overrides
           #:defstruct-definition-slots
           #:defstruct-definition-docstring
           ;; Parsing functions
           #:parse-slot-description
           #:parse-defstruct-options
           #:parse-defstruct
           ;; Code generation functions
           #:generate-defclass-form
           #:generate-constructor
           #:generate-accessors
           #:generate-predicate
           #:generate-copier
           #:generate-setf-expanders
           #:expand-defstruct
           ;; Macro registration
           #:defstruct-expander
           #:install-defstruct-macro))

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
  ;; Internal compiler functions (001-internal-function-consolidation)
  (:import-from #:clysm/compiler/codegen/func-section
                #:compile-unary-math-ffi
                #:compile-cxr-chain
                #:compile-to-instructions)  ; 001-internal-function-export
  ;; Internal compiler functions (001-internal-function-export)
  (:import-from #:clysm/compiler/env
                #:lexical-env-parent
                #:lexical-env-bindings
                #:make-lexical-env)
  (:import-from #:clysm/compiler/codegen/gc-types
                #:make-wasm-struct-type
                #:wasm-struct-type-p
                #:wasm-struct-type-fields)
  (:import-from #:clysm/compiler/ast
                #:ast-literal-value
                #:ast-literal-p)
  (:export #:compile-to-wasm
           #:compile-to-wat
           #:emit-empty-module
           #:encode-unsigned-leb128
           #:encode-signed-leb128
           #:repl
           ;; Internal compiler functions (001-internal-function-consolidation)
           #:compile-unary-math-ffi
           #:compile-cxr-chain
           ;; Internal compiler functions (001-internal-function-export)
           #:compile-to-instructions
           #:lexical-env-parent
           #:lexical-env-bindings
           #:make-lexical-env
           #:make-wasm-struct-type
           #:wasm-struct-type-p
           #:wasm-struct-type-fields
           #:ast-literal-value
           #:ast-literal-p))

;;; Forward declarations for special variables used across compilation units.
;;; These are defined here (after packages exist) to avoid "undefined variable"
;;; warnings during ASDF loading. The actual values are set in their respective
;;; source files.

;; Multiple values globals (runtime/objects.lisp) - used by compiler/codegen/func-section.lisp
(defvar clysm/runtime/objects::*mv-count-global-index*)
(defvar clysm/runtime/objects::*mv-buffer-global-index*)

;; Global macro registry (transform/macro.lisp) - used by compiler/codegen/func-section.lisp
(defvar clysm/compiler/transform/macro::*global-macro-registry*)

;; Setf expander registry (lib/setf-expanders.lisp) - used by compiler/codegen/func-section.lisp
(defvar clysm/lib/setf-expanders::*global-setf-expander-registry*)

;; FFI environment (ffi/macros.lisp) - used by compiler/codegen/func-section.lisp
(defvar clysm/ffi::*ffi-environment*)

;; Pending lambdas (compiler/codegen/func-section.lisp) - forward declared for within-file references
(defvar clysm/compiler/codegen/func-section::*pending-lambdas*)
