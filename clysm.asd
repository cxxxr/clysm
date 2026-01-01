(defsystem "clysm"
  :version "0.1.0"
  :author "Clysm Project"
  :license "MIT"
  :description "WebAssembly GC Common Lisp Compiler"
  :long-description "Clysm compiles Common Lisp to WebAssembly using WasmGC for memory management."
  :depends-on ("alexandria"
               "trivial-gray-streams")
  :pathname "src/clysm/"
  :serial t
  :components
  (;; Package definitions
   (:file "package")

   ;; UTF-8 encoding (034-portable-utf8) - must load before backend
   (:file "lib/utf8")

   ;; Backend: Wasm binary emission
   (:module "backend"
    :serial t
    :components
    ((:file "leb128")
     (:file "sections")
     (:file "wasm-emit")
     (:file "wat-print")))

   ;; FFI: Foreign Function Interface (moved before compiler for math imports)
   (:module "ffi"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "macros")         ; Defines *ffi-environment* - must come before import-gen
     (:file "marshalling")
     (:file "import-gen")
     (:file "export-gen")))

   ;; Compiler: Core compilation pipeline
   (:module "compiler"
    :serial t
    :components
    ((:file "ast")
     (:file "env")
     (:module "analyzer"
      :serial t
      :components
      ((:file "free-vars")
       (:file "tail-call")
       (:file "type-infer")
       (:file "io-usage")
       (:file "ffi-usage")))
     (:module "transform"
      :serial t
      :components
      ((:file "closure")
       (:file "macro")))
     (:module "codegen"
      :serial t
      :components
      ((:file "wasm-ir")
       (:file "gc-types")
       (:file "type-section")
       (:file "func-section")
       (:file "globals")       ; Phase 13D-4: Global variable compilation
       (:file "wasm-global")   ; Phase 13D-4: Wasm global section helpers
       (:file "primitives")))  ; 001-runtime-library-system: Layer 1 primitives registry
     (:file "compiler")
     (:file "directive")))

   ;; Reader: S-expression reader
   (:module "reader"
    :serial t
    :components
    ((:file "tokenizer")
     (:file "parser")
     (:file "package")
     (:file "reader")))

   ;; Runtime: Runtime support
   (:module "runtime"
    :serial t
    :components
    ((:file "rt-package")       ; 001-runtime-library-system: Runtime library package
     (:file "loader")           ; 001-runtime-library-system: Source loader & dependency analysis
     (:file "compiler")         ; 001-runtime-library-system: Runtime function compilation
     (:file "objects")
     (:file "special-vars")
     (:file "multi-value")
     (:file "printer")
     (:file "condition-runtime")))

   ;; Eval: Dynamic evaluation
   (:module "eval"
    :serial t
    :components
    ((:file "interpreter")
     (:file "interpreter-macros")    ; Feature 044: Macro system for interpreter
     (:file "interpreter-builtins")  ; Feature 044: Extended built-in functions
     (:file "interpreter-file")      ; Feature 044: File loading support
     (:file "jit")
     (:file "eval")
     (:file "compile")))

   ;; CLOS: Object system
   (:module "clos"
    :serial t
    :components
    ((:file "mop")
     (:file "defclass")
     (:file "instance")
     (:file "slot-access")
     (:file "generic")
     (:file "defmethod")
     (:file "combination")  ; Must come before dispatch for *next-methods* special declaration
     (:file "dispatch")
     (:file "method-combination")
     (:file "structure-class")))  ; Phase 13D-10: Structure metaclass for defstruct

   ;; Conditions: ANSI CL Condition System (014-condition-system)
   (:module "conditions"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "handlers")
     (:file "restarts")
     (:file "signaling")
     (:file "standard")))

   ;; Streams: FFI-based stream I/O (015-ffi-stream-io)
   (:module "streams"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "ffi-io")
     (:file "write")
     (:file "read")
     (:file "format")))

   ;; Filesystem: FFI-based file I/O (035-ffi-filesystem)
   (:module "filesystem"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "ffi")
     (:file "open")     ; open-file, close-file - must come before read/write
     (:file "read")
     (:file "write")
     (:file "macros"))) ; with-open-file* macro

   ;; Standard library
   (:module "lib"
    :serial t
    :components
    ((:file "setf-expanders")  ; Must come before macros for setf expander registry
     (:file "destructuring")   ; Must come before macros for destructuring-bind
     (:file "macros")
     ;; Note: utf8 is loaded earlier as top-level component (before backend)
     (:file "ffi-runtime")
     (:file "package-macros")
     (:file "defstruct")        ; Phase 13D-10: DEFSTRUCT macro implementation
     (:file "list-ops")         ; Phase 15A: ANSI list operations
     (:file "sequences-util")   ; Phase 15B: Sequence utility functions
     (:file "sequences")        ; Phase 15B: ANSI sequence generic functions
     (:file "io-runtime")       ; Phase 13D-1f: I/O runtime functions (001-io-list-runtime)
     (:file "list-runtime")))   ; Phase 13D-1f: List runtime functions (001-io-list-runtime)

   ;; REPL
   (:file "repl")

   ;; Stage 1: Self-compilation infrastructure (039-stage1-compiler-gen)
   (:module "stage1"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "conditions")
     (:file "logging")
     (:file "reader")
     (:file "runner")
     (:file "progress")
     (:file "blocker")
     (:file "diff")
     (:file "fixpoint")
     (:file "generator")))

   ;; Stage 2: Fixed-point verification (040-fixed-point-verification)
   (:module "stage2"
    :serial t
    :components
    ((:file "package")
     (:file "generator")
     (:file "verifier")))

   ;; Stage 0: Complete compiler for self-hosting (045-stage0-complete-compiler)
   ;; Extended for Phase 13D: True Self-Hosting (001-true-self-hosting)
   (:module "stage0"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "globals")
     (:file "env")           ; T004: Environment management
     (:file "primitives")    ; T008: Primitive operations
     (:file "eval")          ; T006: Core evaluator
     (:file "ffi")
     (:file "runtime")
     (:file "reader")
     (:file "ast")
     (:file "ir")
     (:file "codegen")
     (:file "modules")
     (:file "loader")
     (:file "compiler")
     (:file "progress")
     (:file "output")
     (:file "exports")
     (:file "entry")
     (:file "bootstrap-source")    ; T044: Bootstrap source forms
     (:file "error-analysis")))    ; M4: DEFUN error analysis infrastructure

   ;; Bootstrap: Interpreter-based bootstrap (044-interpreter-bootstrap)
   (:module "interpreter-bootstrap"
    :pathname "bootstrap/"
    :serial t
    :components
    ((:file "package")
     (:file "interpreter-stage0")
     (:file "fixpoint")))

   ;; Workflow: Development workflow infrastructure (041-dev-workflow)
   (:module "workflow"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "platform")
     (:file "deps")
     (:file "cache")
     (:file "compiler")
     (:file "repl")))

   ;; CLI: Command-line interface (041-dev-workflow)
   (:module "cli"
    :serial t
    :components
    ((:file "package")
     (:file "args")
     (:file "main"))))

  :in-order-to ((test-op (test-op "clysm/tests"))))

(defsystem "clysm/tests"
  :description "Tests for Clysm compiler"
  :depends-on ("clysm" "clysm/ansi-test" "clysm/bootstrap" "rove")
  :pathname "tests/"
  :serial t
  :components
  ((:file "package")
   (:file "helpers")

   ;; Contract tests: Wasm structure validation
   (:module "contract"
    :serial t
    :components
    ((:file "leb128-test")
     (:file "sections-test")
     (:file "wasm-validate-test")
     (:file "special-vars-codegen-test")
     (:file "numeric-types-test")
     (:file "ffi-section-test")
     (:file "module-linking-test")
     (:file "tier-promotion-test")
     ;; Import section optimization (022-wasm-import-optimization)
     (:file "import-section-test")
     ;; Predicates Wasm validation (023-type-predicates)
     (:file "predicates-wasm-test")
     ;; Equality predicates Wasm validation (024-equality-predicates)
     (:file "equality-wasm-test")
     ;; Multiple values Wasm validation (025-multiple-values)
     (:file "mv-wasm-test")
     ;; CLOS Foundation Wasm validation (026-clos-foundation)
     (:file "clos-wasm-test")
     ;; CLOS Primitives Wasm validation (001-m3-clos-primitives)
     (:file "clos-primitives-wasm-test")
     ;; FFI Complete Wasm validation (027-complete-ffi)
     (:file "ffi-import-wasm-test")
     (:file "ffi-call-wasm-test")
     (:file "ffi-export-wasm-test")
     (:file "ffi-error-wasm-test")
     (:file "ffi-dynamic-wasm-test")
     (:file "ffi-callback-wasm-test")
     ;; Setf Wasm validation (028-setf-generalized-refs)
     (:file "setf-wasm-test")
     ;; LOOP macro Wasm validation (029-loop-macro)
     (:file "loop-wasm-test")
     ;; Typecase macro Wasm validation (030-typecase-macros)
     (:file "typecase-wasm-test")
     ;; FORMAT function Wasm validation (032-format-function)
     (:file "format-wasm-test")
     ;; Filesystem FFI validation (035-ffi-filesystem)
     (:file "filesystem-ffi-test")
     ;; Bootstrap compilation validation (037-cross-compile-stage0)
     (:file "bootstrap-compile-test")
     (:file "bootstrap-validate-test")
     ;; Stage 0 extend contract tests (038-stage0-extend)
     (:file "stage0-extend-test")
     ;; Stage 1 compiler generation contract tests (039-stage1-compiler-gen)
     (:file "stage1-load-test")
     (:file "stage1-fs-test")
     (:file "stage1-report-test")
     (:file "stage1-validate-test")
     (:file "stage1-blocker-test")
     (:file "stage1-diff-test")
     ;; Workflow contract tests (041-dev-workflow)
     (:module "workflow"
      :serial t
      :components
      ((:file "workflow-args-test")
       (:file "workflow-compile-test")))
     ;; Interpreter bootstrap contract tests (044-interpreter-bootstrap)
     (:file "interpreter-compile-test")
     (:file "interpreter-stage0-test")
     ;; Stage 0 complete compiler contract tests (045-stage0-complete-compiler)
     (:module "stage0"
      :serial t
      :components
      ((:file "runtime-valid-test")
       (:file "ffi-valid-test")
       (:file "exports-test")))
     ;; Array primitives Wasm validation (001-ansi-array-primitives)
     (:file "array-wasm-test")
     ;; Sequence operations Wasm validation (001-ansi-sequence-operations)
     (:file "sequence-wasm-test")
     ;; Array operations Wasm validation (001-ansi-array-ops)
     (:file "array-ops-wasm-test")
     ;; Compile-time directives contract tests (001-compile-time-directives)
     (:file "directive-output-test")
     ;; Global variable Wasm validation (001-global-variable-defs)
     (:file "wasm-globals-test")
     ;; DEFSTRUCT Wasm validation (Phase 13D-10)
     (:file "defstruct-wasm-test")
     ;; Error analysis contract tests (Phase 13D M4)
     (:module "error-analysis"
      :serial t
      :components
      ((:file "test-error-log-entry")
       (:file "test-defun-errors-json")
       (:file "test-error-pattern")
       (:file "test-stage1-report")))
     ;; Runtime library contract tests (001-runtime-library-system)
     (:module "runtime"
      :serial t
      :components
      ((:file "primitives-struct-test")
       (:file "registry-test")
       (:file "module-load-test")
       (:file "dependency-test")
       (:file "wasm-valid-test")))))

   ;; Unit tests: Individual components
   (:module "unit"
    :serial t
    :components
    ((:file "gc-types-test")
     (:file "objects-test")
     (:file "ast-test")
     (:file "analyzer-test")
     (:file "codegen-test")
     (:file "closure-test")
     (:file "exception-test")
     (:file "binding-test")
     (:file "tokenizer-test")
     (:file "parser-test")
     (:file "package-test")
     (:file "macro-test")
     (:file "backquote-test")
     (:file "interpreter-test")
     (:file "clos-test")
     (:file "reader-error-test")
     (:file "special-vars-ast-test")
     (:file "tagbody-test")
     (:file "cons-test")
     (:file "tail-position-test")
     (:file "math-functions-test")
     (:file "numeric-predicates-test")
     ;; ANSI Numeric Functions (001-numeric-functions)
     (:file "basic-functions-test")     ; US1: signum, max, min
     (:file "trig-functions-test")      ; US2: sin, cos, tan, asin, acos, atan
     (:file "bitwise-functions-test")   ; US3: logcount, integer-length
     (:file "hyperbolic-functions-test") ; US5: sinh, cosh, tanh, asinh, acosh, atanh
     (:file "complex-functions-test")   ; US6: complex, realpart, imagpart, conjugate, phase
     (:file "ffi-types-test")
     (:file "ffi-marshalling-test")
     (:file "ffi-codegen-test")
     (:file "tokenizer-package-test")
     (:file "condition-types-test")
     (:file "handler-test")
     (:file "restart-test")
     ;; Stream unit tests (015-ffi-stream-io)
     (:file "stream-types-test")
     (:file "stream-write-test")
     (:file "stream-read-test")
     (:file "stream-format-test")
     ;; Compile tests (017-eval-jit-compile)
     (:file "compile-test")
     ;; (:file "tier-promotion-test")  ; Moved to contract module
     ;; (:file "module-linking-test")  ; Moved to contract module
     (:file "jit-test")
     ;; I/O usage analyzer (022-wasm-import-optimization)
     (:file "io-usage-test")
     ;; FFI usage analyzer (001-ffi-import-architecture)
     (:file "ffi-usage-test")
     ;; Type predicates (023-type-predicates)
     (:file "type-predicates-test")
     ;; Equality predicates (024-equality-predicates)
     (:file "equality-predicates-test")
     (:file "logical-operators-test")
     ;; Multiple values (025-multiple-values)
     (:file "multiple-values-test")
     ;; ANSI test harness (020-ansi-test)
     (:module "ansi-test"
      :serial t
      :components
      ((:file "package")
       (:file "data-model-test")
       (:file "loader-test")
       (:file "classifier-test")
       (:file "skip-registry-test")
       (:file "runner-test")
       (:file "reporter-test")
       (:file "baseline-test")))
     ;; CLOS Foundation unit tests (026-clos-foundation)
     (:module "clos"
      :serial t
      :components
      ((:file "defclass-test")
       (:file "make-instance-test")
       (:file "accessor-test")
       (:file "defmethod-test")))
     ;; FFI Complete unit tests (027-complete-ffi)
     (:module "ffi"
      :serial t
      :components
      ((:file "define-foreign-function-test")
       (:file "ffi-call-parse-test")
       (:file "marshal-test")
       (:file "export-function-test")
       (:file "export-wrapper-test")
       (:file "error-handling-test")
       (:file "ffi-condition-test")
       (:file "call-host-parse-test")
       (:file "call-host-args-test")
       (:file "callback-test")))
     ;; Setf macros unit tests (028-setf-generalized-refs)
     (:file "setf-test")
     (:file "setf-expander-test")
     ;; LOOP macro unit tests (029-loop-macro)
     (:file "loop-test")
     ;; Typecase macro unit tests (030-typecase-macros)
     (:module "typecase"
      :serial t
      :components
      ((:file "typecase-test")
       (:file "etypecase-test")
       (:file "check-type-test")
       (:file "ctypecase-test")
       (:file "compound-types-test")))
     ;; Destructuring-bind unit tests (031-destructuring-bind-macro)
     (:file "destructuring-bind-test")
     ;; Portable UTF-8 unit tests (034-portable-utf8)
     (:file "utf8-test")
     ;; FORMAT function unit tests (032-format-function)
     (:module "format"
      :serial t
      :components
      ((:file "basic-test")
       (:file "iteration-test")
       (:file "conditional-test")
       (:file "recursive-test")))
     ;; Filesystem unit tests (035-ffi-filesystem)
     (:module "filesystem"
      :serial t
      :components
      ((:file "file-error-test")
       (:file "file-stream-test")
       (:file "read-contents-test")
       (:file "write-contents-test")
       (:file "open-close-test")
       (:file "with-open-file-test")))
     ;; Validation unit tests (036-compiler-subset-validation)
     (:module "validation"
      :serial t
      :components
      ((:file "feature-registry-test")
       (:file "analyzer-test")))
     ;; Bootstrap unit tests (037-cross-compile-stage0)
     (:module "bootstrap"
      :serial t
      :components
      ((:file "read-forms-test")
       (:file "filter-forms-test")
       (:file "context-test")))
     ;; Stage 0 extension unit tests (038-stage0-extend)
     (:file "defconstant-test")
     (:file "declare-skip-test")
     (:file "defstruct-expand-test")
     (:file "condition-expand-test")
     (:file "error-report-test")
     ;; Stage 1 compiler generation unit tests (039-stage1-compiler-gen)
     (:module "stage1"
      :serial t
      :components
      ((:file "runner-test")
       (:file "file-reader-test")
       (:file "progress-test")
       (:file "generator-test")
       (:file "blocker-test")
       (:file "diff-test")))
     ;; Workflow unit tests (041-dev-workflow)
     (:module "workflow"
      :serial t
      :components
      ((:file "types-test")
       (:file "platform-test")
       (:file "deps-test")
       (:file "cache-test")))
     ;; Default values unit tests (043-self-hosting-blockers)
     (:module "default-values"
      :serial t
      :components
      ((:file "optional-test")
       (:file "key-test")))
     ;; Hash table unit tests (043-self-hosting-blockers)
     (:module "hash-table"
      :serial t
      :components
      ((:file "make-hash-table-test")
       (:file "gethash-test")
       (:file "remhash-test")
       (:file "maphash-test")))
     ;; List operations unit tests (043-self-hosting-blockers, 001-ansi-list-ops)
     (:module "list-ops"
      :serial t
      :components
      ((:file "list-tail-test")      ; Phase 15A: last, butlast, nth, nthcdr
       (:file "assoc-test")
       (:file "member-test")
       (:file "set-ops-test")
       (:file "alist-construct-test"))) ; Phase 15A: pairlis, acons, copy-alist
     ;; Sequence extension unit tests (043-self-hosting-blockers)
     (:module "sequence-ext"
      :serial t
      :components
      ((:file "position-test")
       (:file "find-test")
       (:file "remove-test")
       (:file "substitute-test")))
     ;; Interpreter bootstrap unit tests (044-interpreter-bootstrap)
     (:module "interpreter"
      :serial t
      :components
      ((:file "defun-test")
       (:file "defmacro-test")
       (:file "defstruct-test")
       (:file "loop-test")
       (:file "handler-case-test")
       (:file "builtins-test")
       (:file "special-forms-test")
       (:file "multiple-values-test")
       (:file "file-loading-test")))
     ;; Stage 0 complete compiler unit tests (045-stage0-complete-compiler)
     ;; Extended for Phase 13D: True Self-Hosting (001-true-self-hosting)
     (:module "stage0"
      :serial t
      :components
      ((:file "types-test")
       (:file "globals-test")
       (:file "env-test")         ; T005: Environment tests
       (:file "eval-test")        ; T007: Evaluator tests
       (:file "primitives-test")  ; T009: Primitives tests
       (:file "reader-test")
       (:file "ast-test")
       (:file "ir-test")
       (:file "ffi-test")
       (:file "fs-read-test")))
     ;; Array primitives unit tests (001-ansi-array-primitives)
     (:file "array-primitives-test")
     ;; Sequence operations codegen unit tests (001-ansi-sequence-operations)
     (:file "sequence-codegen-test")
     ;; Compile-time directives unit tests (001-compile-time-directives)
     (:file "directive-test")
     ;; Global variable compilation unit tests (001-global-variable-defs)
     (:file "globals-test")
     ;; DEFSTRUCT unit tests (Phase 13D-10)
     (:file "defstruct-test")
     ;; Sequence generic functions unit tests (Phase 15B)
     (:module "sequences"
      :serial t
      :components
      ((:file "util-test")
       (:file "count-test")
       (:file "find-test")
       (:file "position-test")
       (:file "mismatch-test")
       (:file "search-test")
       (:file "substitute-test")
       (:file "remove-duplicates-test")
       (:file "fill-test")
       (:file "replace-test")))
     ;; Array operations unit tests (001-ansi-array-ops)
     (:file "array-ops-test")
     ;; Lambda-list unit tests (Phase 13D M4)
     (:module "lambda-list"
      :serial t
      :components
      ((:file "test-error-logging")
       (:file "test-pattern-classification")
       (:file "test-aux-params")
       (:file "test-aux-init-forms")
       (:file "test-allow-other-keys")))
     ;; Runtime library unit tests (001-runtime-library-system)
     (:module "runtime"
      :serial t
      :components
      ((:file "register-test")
       (:file "lookup-test")
       (:file "function-struct-test")
       (:file "compile-fn-test")
       (:file "undefined-prim-test")))))

   ;; Stream integration tests (015-ffi-stream-io)
   (:module "streams"
    :serial t
    :components
    ((:file "package")
     (:file "stream-test")
     (:file "integration-test")))

   ;; Integration tests: End-to-end compilation and execution
   (:module "integration"
    :serial t
    :components
    ((:file "nix-env-test")
     (:file "arithmetic-test")
     (:file "control-test")
     (:file "binding-test")
     (:file "function-test")
     (:file "closure-test")
     (:file "tco-test")
     (:file "control-flow-test")
     (:file "special-var-test")
     (:file "repl-test")
     (:file "macro-test")
     (:file "eval-test")
     (:file "jit-test")
     (:file "clos-test")
     (:file "variable-scope-test")
     (:file "clos-edge-test")
     (:file "list-test")
     (:file "sequence-test")
     (:file "character-test")
     (:file "string-test")
     (:file "bignum-test")
     (:file "ratio-test")
     (:file "float-test")
     (:file "complex-test")
     (:file "mixed-arithmetic-test")
     (:file "ffi-import-test")
     (:file "ffi-export-test")
     (:file "ffi-multi-host-test")
     (:file "package-integration-test")
     (:file "condition-test")
     ;; Compile tests (017-eval-jit-compile)
     (:file "compile-test")
     ;; (:file "tier-promotion-test")  ; Moved to contract module
     ;; Wasmtime execution tests (022-wasm-import-optimization)
     (:file "wasmtime-test")
     ;; ANSI predicates integration tests (023-type-predicates)
     (:file "ansi-predicates-test")
     ;; ANSI equality predicates integration tests (024-equality-predicates)
     (:file "equality-ansi-test")
     ;; FFI Complete integration tests (027-complete-ffi)
     (:file "ffi-import-call-test")
     (:file "ffi-export-test-027")
     (:file "ffi-error-handling-test")
     (:file "ffi-call-host-test")
     (:file "ffi-callback-test")
     ;; Dynamic call integration tests (001-ffi-import-architecture)
     (:file "dynamic-call-test")
     ;; Backward compatibility tests (001-ffi-import-architecture)
     (:file "backward-compat-test")
     ;; Setf ANSI integration tests (028-setf-generalized-refs)
     (:file "setf-ansi-test")
     ;; LOOP macro ANSI integration tests (029-loop-macro)
     (:file "loop-ansi-test")
     ;; Typecase macro ANSI integration tests (030-typecase-macros)
     (:file "typecase-ansi-test")
     ;; FORMAT function ANSI integration tests (032-format-function)
     (:file "format-ansi-test")
     ;; FORMAT function self-hosting tests (032-format-function)
     (:file "format-self-host-test")
     ;; Filesystem integration tests (035-ffi-filesystem)
     (:file "filesystem-test")
     ;; Bootstrap integration tests (037-cross-compile-stage0)
     (:file "bootstrap-full-test")
     ;; Stage 1 compiler generation integration tests (039-stage1-compiler-gen)
     (:file "stage1-arith-test")
     (:file "stage1-defun-test")
     (:file "stage1-error-test")
     (:file "stage1-modules-test")
     (:file "stage1-timing-test")
     ;; TODO: Fix these tests to use existing stage1 APIs
     ;; (:file "stage1-gen-test")    ; Uses non-existent compile-all-forms
     ;; (:file "stage1-full-test")   ; Uses non-existent compile-all-forms
     ;; Interpreter bootstrap integration tests (044-interpreter-bootstrap)
     (:file "stage0-wasm-valid-test")
     (:file "bootstrap-fixpoint-test")
     (:file "sbcl-free-test")
     ;; Stage 0 complete compiler integration tests (045-stage0-complete-compiler)
     (:module "stage0"
      :serial t
      :components
      ((:file "simple-expr-test")
       (:file "defun-test")
       (:file "error-test")))
     ;; Workflow integration tests (041-dev-workflow)
     (:module "workflow"
      :serial t
      :components
      ((:file "workflow-cli-test")
       (:file "workflow-incremental-test")
       (:file "workflow-error-recovery-test")
       (:file "workflow-repl-test")
       (:file "workflow-selfhost-test")))
     ;; Array primitives integration tests (001-ansi-array-primitives)
     (:file "array-test")
     ;; DEFSTRUCT integration tests (Phase 13D-10)
     (:file "defstruct-usage-test")
     ;; Array operations integration tests (001-ansi-array-ops)
     (:file "array-ops-test")
     ;; Module compilation integration tests (Phase 13D M4)
     (:file "test-backend-compile")
     (:file "test-reader-compile")
     ;; Runtime library integration tests (001-runtime-library-system)
     (:module "runtime"
      :serial t
      :components
      ((:file "assoc-test"))))))

  :perform (test-op (o c)
             (symbol-call :rove :run c)))

;; ANSI Common Lisp Test Harness (020-ansi-test)
(defsystem "clysm/ansi-test"
  :description "ANSI Common Lisp compliance test harness for Clysm"
  :depends-on ("clysm" "alexandria" "uiop")
  :pathname "src/clysm/ansi-test/"
  :serial t
  :components
  ((:file "package")
   (:file "data-model")
   (:file "conditions")
   (:file "skip-registry")
   (:file "loader")
   (:file "classifier")
   (:file "runner")
   (:file "reporter")
   (:file "baseline")))

;; Compiler Subset Validation (036-compiler-subset-validation)
(defsystem "clysm/validation"
  :description "Compiler self-hosting validation tools"
  :depends-on ("clysm" "alexandria" "uiop" "rove")
  :pathname "src/clysm/validation/"
  :serial t
  :components
  ((:file "package")
   (:file "feature-registry")
   (:file "analyzer")
   (:file "reporter")
   (:file "compiler-order")))

;; ANSI Test Harness Tests
(defsystem "clysm/ansi-test/tests"
  :description "Tests for ANSI test harness"
  :depends-on ("clysm/ansi-test" "rove")
  :pathname "tests/"
  :serial t
  :components
  ((:module "unit/ansi-test"
    :serial t
    :components
    ((:file "package")
     (:file "data-model-test")
     (:file "loader-test")
     (:file "classifier-test")
     (:file "skip-registry-test")
     (:file "runner-test")
     (:file "reporter-test")
     (:file "baseline-test")))
   (:module "integration/ansi-test"
    :serial t
    :components
    ((:file "runner-test")
     (:file "baseline-test")))
   (:module "contract/ansi-test"
    :serial t
    :components
    ((:file "report-format-test"))))
  :perform (test-op (o c)
             (symbol-call :rove :run c)))

;; Stage 0 Bootstrap Script (037-cross-compile-stage0)
(defsystem "clysm/bootstrap"
  :description "Stage 0 cross-compilation bootstrap script"
  :depends-on ("clysm" "clysm/validation" "uiop")
  :pathname "build/"
  :serial t
  :components
  ((:file "bootstrap")))

;; Executable CLI (041-dev-workflow)
;; Build with: (asdf:make :clysm/executable)
(defsystem "clysm/executable"
  :description "Clysm CLI executable"
  :depends-on ("clysm")
  :build-operation "program-op"
  :build-pathname "clysm"
  :entry-point "clysm/cli:entry-point")
