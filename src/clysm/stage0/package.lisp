;;;; package.lisp - Package definition for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Provides the complete Stage 0 compiler that runs on wasmtime and can
;;;; compile Clysm source code to produce Stage 1, enabling fixed-point verification.

(defpackage #:clysm/stage0
  (:use #:cl)
  (:documentation "Stage 0 complete compiler for self-hosting.
Provides:
- compile_form: Compile a single Lisp expression to Wasm bytes
- compile_all: Compile all 45 source modules to produce Stage 1 binary
- Runtime initialization with WasmGC types and globals
- FFI for filesystem access

This module generates the Stage 0 Wasm binary that runs on wasmtime.
Stage 0 compiles Clysm source → Stage 1, which compiles → Stage 2.
Fixed-point is achieved when Stage 1 == Stage 2 (byte-identical).")

  ;; Entry points (from entry.lisp)
  (:export #:compile-form
           #:compile-all)

  ;; Types (from types.lisp)
  (:export #:stage0-result
           #:make-stage0-result
           #:stage0-result-success-p
           #:stage0-result-wasm-bytes
           #:stage0-result-error-message
           #:stage0-result-form-count

           #:compile-context
           #:make-compile-context
           #:compile-context-forms
           #:compile-context-current-module
           #:compile-context-compiled-count
           #:compile-context-failed-count)

  ;; Runtime (from runtime.lisp)
  (:export #:generate-runtime-init
           #:generate-type-section
           #:generate-global-section
           #:generate-start-function)

  ;; Reader (from reader.lisp)
  (:export #:read-source-file
           #:read-source-string
           #:parse-forms)

  ;; AST (from ast.lisp)
  (:export #:parse-expression
           #:ast-to-ir)

  ;; IR (from ir.lisp)
  (:export #:generate-wasm-ir
           #:ir-to-instructions)

  ;; Codegen (from codegen.lisp)
  (:export #:emit-wasm-binary
           #:emit-type-section
           #:emit-function-section
           #:emit-export-section
           #:emit-code-section)

  ;; FFI (from ffi.lisp)
  (:export #:generate-ffi-imports
           #:*fs-open*
           #:*fs-read-all*
           #:*fs-write-all*
           #:*fs-close*)

  ;; Globals (from globals.lisp)
  (:export #:*nil-index*
           #:*unbound-index*
           #:*mv-count-index*
           #:*mv-buffer-index*
           #:generate-global-init
           ;; Symbol interning (T011)
           #:*symbol-table*
           #:stage0-symbol
           #:make-stage0-symbol
           #:sym-name
           #:sym-value
           #:sym-function
           #:sym-plist
           #:intern-symbol
           #:find-symbol*
           #:clear-symbol-table
           #:symbol-count)

  ;; Modules (from modules.lisp)
  (:export #:*compiler-modules*
           #:get-module-order
           #:get-module-path)

  ;; Loader (from loader.lisp)
  (:export #:load-module
           #:load-all-modules)

  ;; Compiler (from compiler.lisp)
  (:export #:compile-expression
           #:compile-module
           #:compile-forms)

  ;; Progress (from progress.lisp)
  (:export #:report-progress
           #:*progress-callback*)

  ;; Output (from output.lisp)
  (:export #:write-stage1-binary
           #:finalize-compilation
           #:validate-wasm-bytes)

  ;; Exports (from exports.lisp)
  (:export #:generate-exports
           #:*exported-functions*)

  ;; Environment (from env.lisp) - Phase 13D True Self-Hosting
  (:export #:make-env
           #:empty-env-p
           #:extend-env
           #:extend-env*
           #:lookup
           #:env-bindings
           #:env-symbols)

  ;; Primitives (from primitives.lisp) - Phase 13D True Self-Hosting
  (:export #:*primitives*
           #:register-primitive
           #:get-primitive
           #:primitive-p
           #:initialize-primitives
           #:prim-add
           #:prim-sub
           #:prim-mul
           #:prim-div
           #:prim-lt
           #:prim-gt
           #:prim-num-eq
           #:prim-cons
           #:prim-car
           #:prim-cdr
           #:prim-eq)

  ;; Evaluator (from eval.lisp) - Phase 13D True Self-Hosting
  (:export #:eval-form
           #:*global-functions*
           #:get-global-function
           #:set-global-function
           #:clear-global-functions
           #:closure
           #:make-closure
           #:closure-p
           #:closure-params
           #:closure-body
           #:closure-env)

  ;; Bootstrap source (from bootstrap-source.lisp) - Phase 13D True Self-Hosting
  (:export #:*bootstrap-source-forms*
           #:get-bootstrap-forms
           #:bootstrap-form-count
           #:compile-bootstrap-forms
           #:estimate-bootstrap-size))
