(defsystem "clysm"
  :version "0.1.0"
  :author "Clysm Project"
  :license "MIT"
  :description "WebAssembly GC Common Lisp Compiler"
  :long-description "Clysm compiles Common Lisp to WebAssembly using WasmGC for memory management."
  :depends-on ("alexandria"
               "babel"
               "trivial-gray-streams")
  :pathname "src/clysm/"
  :serial t
  :components
  (;; Package definitions
   (:file "package")

   ;; Backend: Wasm binary emission
   (:module "backend"
    :serial t
    :components
    ((:file "leb128")
     (:file "sections")
     (:file "wasm-emit")
     (:file "wat-print")))

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
       (:file "io-usage")))
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
       (:file "func-section")))
     (:file "compiler")))

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
    ((:file "objects")
     (:file "special-vars")
     (:file "multi-value")
     (:file "printer")
     (:file "condition-runtime")))

   ;; Eval: Dynamic evaluation
   (:module "eval"
    :serial t
    :components
    ((:file "interpreter")
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
     (:file "method-combination")))

   ;; FFI: Foreign Function Interface
   (:module "ffi"
    :serial t
    :components
    ((:file "package")
     (:file "types")
     (:file "macros")         ; Defines *ffi-environment* - must come before import-gen
     (:file "marshalling")
     (:file "import-gen")
     (:file "export-gen")))

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

   ;; Standard library
   (:module "lib"
    :serial t
    :components
    ((:file "setf-expanders")  ; Must come before macros for setf expander registry
     (:file "macros")
     (:file "ffi-runtime")
     (:file "package-macros")))

   ;; REPL
   (:file "repl"))

  :in-order-to ((test-op (test-op "clysm/tests"))))

(defsystem "clysm/tests"
  :description "Tests for Clysm compiler"
  :depends-on ("clysm" "clysm/ansi-test" "rove")
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
     (:file "loop-wasm-test")))

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
     (:file "loop-test")))

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
     ;; Setf ANSI integration tests (028-setf-generalized-refs)
     (:file "setf-ansi-test")
     ;; LOOP macro ANSI integration tests (029-loop-macro)
     (:file "loop-ansi-test"))))

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
