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
       (:file "type-infer")))
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
     (:file "printer")))

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

   ;; Standard library
   (:module "lib"
    :serial t
    :components
    ((:file "macros")
     (:file "ffi-runtime")))

   ;; REPL
   (:file "repl"))

  :in-order-to ((test-op (test-op "clysm/tests"))))

(defsystem "clysm/tests"
  :description "Tests for Clysm compiler"
  :depends-on ("clysm" "rove")
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
     (:file "ffi-section-test")))

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
     (:file "ffi-codegen-test")))

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
     (:file "ffi-multi-host-test"))))

  :perform (test-op (o c)
             (symbol-call :rove :run c)))
