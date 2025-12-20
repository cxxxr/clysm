;;;; clysm.asd - ASDF system definition for clysm

(defsystem "clysm"
  :version "0.1.0"
  :license "MIT"
  :description "Common Lisp to WebAssembly compiler using WasmGC"
  :long-description "A Common Lisp implementation that compiles to WebAssembly,
leveraging WasmGC for garbage collection and targeting browser execution."
  :depends-on ("alexandria"
               "serapeum"
               "trivia"
               "flexi-streams")
  :pathname "src/"
  :serial t
  :components ((:file "package")
               (:module "utils"
                :serial t
                :components ((:file "binary")))
               (:module "wasm"
                :serial t
                :components ((:file "types")
                             (:file "instructions")
                             (:file "gc-types")
                             (:file "module")
                             (:file "encoder")))
               (:module "reader"
                :components ((:file "reader")))
               (:module "ast"
                :serial t
                :components ((:file "nodes")
                             (:file "walker")))
               (:module "ir"
                :serial t
                :components ((:file "ir")
                             (:file "convert")
                             (:file "optimize")))
               (:module "compiler"
                :serial t
                :components ((:file "environment")
                             (:file "special-forms")
                             (:file "primitives")
                             (:file "codegen")
                             (:file "compiler")))
               (:module "runtime"
                :serial t
                :components ((:file "runtime")))
               (:module "stdlib"
                :serial t
                :components ((:file "arithmetic"))))
  :in-order-to ((test-op (test-op "clysm/tests"))))

(defsystem "clysm/tests"
  :description "Test suite for clysm"
  :depends-on ("clysm"
               "fiveam")
  :pathname "tests/"
  :serial t
  :components ((:file "package")
               (:file "suite")
               (:module "wasm"
                :components ((:file "encoder-tests")))
               (:module "compiler"
                :components ((:file "special-forms-tests")
                             (:file "list-tests")))
               (:module "integration"
                :components ((:file "compilation-tests"))))
  :perform (test-op (o c)
             (symbol-call :fiveam :run! :clysm)))

(defsystem "clysm/ansi-tests"
  :description "ANSI Common Lisp conformance tests for clysm"
  :depends-on ("clysm"
               "uiop")
  :pathname "tests/ansi/"
  :serial t
  :components ((:file "package")
               (:file "config")
               (:file "feature-map")
               (:file "transformer")
               (:file "comparator")
               (:file "runner")
               (:file "reporter")))
