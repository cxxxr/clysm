;;;; package.lisp - Test package definitions

(defpackage #:clysm/tests/helpers
  (:use #:cl)
  (:export #:compile-and-run
           #:compile-and-run-numeric
           #:validate-wasm
           #:validate-wasm-silent
           #:with-temp-wasm-file
           #:assert-compiles
           #:assert-validates
           #:assert-equals
           #:wasm-runtime-error
           ;; Floating-point comparison helpers (001-numeric-functions)
           #:*float-epsilon*
           #:approx=
           #:assert-approx=
           ;; Global variable helpers (001-global-variable-defs)
           #:wasm-valid-p
           #:wasm-has-init-function-p
           #:wasm-has-global-section-p
           #:wasm-global-count))

(defpackage #:clysm/tests
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers
                #:compile-and-run
                #:compile-and-run-numeric
                #:validate-wasm
                #:validate-wasm-silent
                #:with-temp-wasm-file
                #:assert-compiles
                #:assert-validates
                #:assert-equals
                ;; Global variable helpers (001-global-variable-defs)
                #:wasm-valid-p
                #:wasm-has-init-function-p
                #:wasm-has-global-section-p
                #:wasm-global-count)
  (:export #:run-all-tests
           #:compile-and-run
           #:compile-and-run-numeric
           #:validate-wasm
           #:validate-wasm-silent
           #:with-temp-wasm-file
           #:assert-compiles
           #:assert-validates
           #:assert-equals
           ;; Global variable helpers (001-global-variable-defs)
           #:wasm-valid-p
           #:wasm-has-init-function-p
           #:wasm-has-global-section-p
           #:wasm-global-count))

(defpackage #:clysm/tests/contract/leb128
  (:use #:cl #:rove #:clysm/backend/leb128))

(defpackage #:clysm/tests/contract/sections
  (:use #:cl #:rove #:clysm/backend/sections))

(defpackage #:clysm/tests/contract/wasm-validate
  (:use #:cl #:rove #:clysm/backend/wasm-emit))

(defpackage #:clysm/tests/unit/gc-types
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/objects
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/ast
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/analyzer
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/codegen
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/closure
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/exception
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/binding
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/tokenizer
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/parser
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/package
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/macro
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/backquote
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/interpreter
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/clos
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/reader-error
  (:use #:cl #:rove))

;; Special variables AST tests (T010-T011)
(defpackage #:clysm/tests/unit/special-vars-ast
  (:use #:cl #:rove #:clysm/compiler/ast #:clysm/compiler/env))

;; Special variables codegen contract tests (T012-T013)
(defpackage #:clysm/tests/contract/special-vars-codegen
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/nix-env
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/arithmetic
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/control
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/binding
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/function
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/closure
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/tco
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/control-flow
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/special-var
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/repl
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/macro
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/eval
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/jit
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/clos
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/variable-scope
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/clos-edge
  (:use #:cl #:rove))

;; Tagbody/Go strategy analysis unit tests
(defpackage #:clysm/tests/unit/tagbody
  (:use #:cl #:rove))

;; Cons cell operations unit tests (006-cons-list-ops)
(defpackage #:clysm/tests/unit/cons
  (:use #:cl #:rove))

;; List operations integration tests (006-cons-list-ops)
(defpackage #:clysm/tests/integration/list
  (:use #:cl #:rove))

;; Sequence functions integration tests (007-sequence-functions)
(defpackage #:clysm/tests/integration/sequence
  (:use #:cl #:rove))

;; Character unit tests (008-character-string)
(defpackage #:clysm/tests/unit/character
  (:use #:cl #:rove))

;; Character integration tests (008-character-string)
(defpackage #:clysm/tests/integration/character
  (:use #:cl #:rove))

;; String integration tests (008-character-string)
(defpackage #:clysm/tests/integration/string
  (:use #:cl #:rove))

;; Tail position detection unit tests (009-tail-call-optimization)
(defpackage #:clysm/tests/unit/tail-position
  (:use #:cl #:rove))

;; Numeric tower contract tests (010-numeric-tower)
(defpackage #:clysm/tests/contract/numeric-types
  (:use #:cl #:rove))

;; Numeric tower integration tests (010-numeric-tower)
(defpackage #:clysm/tests/integration/bignum
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/ratio
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/float
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/complex
  (:use #:cl #:rove))

;; Math functions unit tests (010-numeric-tower Phase 7)
(defpackage #:clysm/tests/unit/math-functions
  (:use #:cl #:rove))

;; Numeric predicates unit tests (010-numeric-tower Phase 8)
(defpackage #:clysm/tests/unit/numeric-predicates
  (:use #:cl #:rove))

;; Mixed arithmetic integration tests (010-numeric-tower Phase 9)
(defpackage #:clysm/tests/integration/mixed-arithmetic
  (:use #:cl #:rove))

;; FFI unit tests (012-ffi-foundation)
(defpackage #:clysm/tests/unit/ffi-types
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/ffi-marshalling
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/ffi-codegen
  (:use #:cl #:rove))

;; FFI contract tests (012-ffi-foundation)
(defpackage #:clysm/tests/contract/ffi-section
  (:use #:cl #:rove))

;; FFI integration tests (012-ffi-foundation)
(defpackage #:clysm/tests/integration/ffi-import
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/ffi-export
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/ffi-multi-host
  (:use #:cl #:rove))

;; Package system tests (013-package-system)
(defpackage #:clysm/tests/unit/tokenizer-package
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/package
  (:use #:cl #:rove))

;; Compile tests (017-eval-jit-compile)
(defpackage #:clysm/tests/unit/compile
  (:use #:cl #:rove))

(defpackage #:clysm/tests/contract/tier-promotion
  (:use #:cl #:rove))

(defpackage #:clysm/tests/contract/module-linking
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/compile
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/tier-promotion
  (:use #:cl #:rove))

(defpackage #:clysm/tests/unit/jit
  (:use #:cl #:rove))

(defpackage #:clysm/tests/integration/jit-module
  (:use #:cl #:rove))

;; I/O usage analyzer unit tests (022-wasm-import-optimization)
(defpackage #:clysm/tests/unit/io-usage
  (:use #:cl #:rove #:clysm/compiler/analyzer/io-usage))

;; Import section contract tests (022-wasm-import-optimization)
(defpackage #:clysm/tests/contract/import-section
  (:use #:cl #:rove))

;; Wasmtime execution integration tests (022-wasm-import-optimization)
(defpackage #:clysm/tests/integration/wasmtime
  (:use #:cl #:rove))

;; Type predicates unit tests (023-type-predicates)
(defpackage #:clysm/tests/unit/type-predicates
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Predicates Wasm contract tests (023-type-predicates)
(defpackage #:clysm/tests/contract/predicates-wasm
  (:use #:cl #:rove))

;; ANSI predicates integration tests (023-type-predicates)
(defpackage #:clysm/tests/integration/ansi-predicates
  (:use #:cl #:rove))

;; Equality predicates unit tests (024-equality-predicates)
(defpackage #:clysm/tests/unit/equality-predicates
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Logical operators unit tests (024-equality-predicates)
(defpackage #:clysm/tests/unit/logical-operators
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Equality Wasm contract tests (024-equality-predicates)
(defpackage #:clysm/tests/contract/equality-wasm
  (:use #:cl #:rove))

;; Equality ANSI integration tests (024-equality-predicates)
(defpackage #:clysm/tests/integration/equality-ansi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; Multiple Values Tests (025-multiple-values)
;;; ============================================================

;; Multiple Values Wasm contract tests (025-multiple-values)
(defpackage #:clysm/tests/contract/mv-wasm
  (:use #:cl #:rove))

;; Multiple Values unit tests (025-multiple-values)
(defpackage #:clysm/tests/unit/multiple-values
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Multiple Values ANSI integration tests (025-multiple-values)
(defpackage #:clysm/tests/integration/mv-ansi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; CLOS Foundation Tests (026-clos-foundation)
;;; ============================================================

;; CLOS codegen unit tests (026-clos-foundation)
;; Tests for defclass, make-instance, accessor, defmethod WasmGC codegen
(defpackage #:clysm/tests/unit/clos-codegen
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run #:validate-wasm-silent))

;; CLOS Wasm contract tests (026-clos-foundation)
(defpackage #:clysm/tests/contract/clos-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; CLOS ANSI integration tests (026-clos-foundation)
(defpackage #:clysm/tests/integration/clos-ansi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; LOOP Macro Tests (029-loop-macro)
;;; ============================================================

;; LOOP macro unit tests (029-loop-macro)
;; Tests for clause parsing, struct construction, expansion
(defpackage #:clysm/tests/unit/loop
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; LOOP Wasm contract tests (029-loop-macro)
(defpackage #:clysm/tests/contract/loop-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; LOOP ANSI integration tests (029-loop-macro)
(defpackage #:clysm/tests/integration/loop-ansi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; LOOP Extension Tests (001-loop-extension)
;;; ============================================================

;; Hash-table iteration tests (001-loop-extension)
;; Tests for BEING THE HASH-KEYS/VALUES, USING clause
(defpackage #:clysm/tests/unit/loop-extension/hash-iteration
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; WITH clause tests (001-loop-extension)
(defpackage #:clysm/tests/unit/loop-extension/with-clause
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; FINALLY clause tests (001-loop-extension)
(defpackage #:clysm/tests/unit/loop-extension/finally-clause
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; INTO clause tests (001-loop-extension)
(defpackage #:clysm/tests/unit/loop-extension/into-clause
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; Typecase Macro Tests (030-typecase-macros)
;;; ============================================================

;; Typecase macro unit tests (030-typecase-macros)
;; Tests for typecase, etypecase, ctypecase, check-type expansion
(defpackage #:clysm/tests/unit/typecase
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Typecase Wasm contract tests (030-typecase-macros)
(defpackage #:clysm/tests/contract/typecase-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Typecase ANSI integration tests (030-typecase-macros)
(defpackage #:clysm/tests/integration/typecase-ansi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run)
  (:import-from #:alexandria #:flatten))

;;; ============================================================
;;; Filesystem Tests (035-ffi-filesystem)
;;; ============================================================

;; Filesystem file-error condition unit tests (035-ffi-filesystem)
(defpackage #:clysm/tests/unit/filesystem/file-error
  (:use #:cl #:rove))

;; Filesystem file-stream struct unit tests (035-ffi-filesystem)
(defpackage #:clysm/tests/unit/filesystem/file-stream
  (:use #:cl #:rove))

;; Filesystem FFI contract tests (035-ffi-filesystem)
(defpackage #:clysm/tests/contract/filesystem-ffi
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Bootstrap Tests (037-cross-compile-stage0)
;;; ============================================================

;; Bootstrap compile contract tests (T014)
(defpackage #:clysm/tests/contract/bootstrap-compile
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Bootstrap validate contract tests (T015)
(defpackage #:clysm/tests/contract/bootstrap-validate
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers #:with-temp-wasm-file))

;; Bootstrap full integration tests (T016)
(defpackage #:clysm/tests/integration/bootstrap-full
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers #:with-temp-wasm-file))

;;; ============================================================
;;; Stage 0 Capability Extension Tests (038-stage0-extend)
;;; ============================================================

;; defconstant unit tests (038-stage0-extend User Story 1)
(defpackage #:clysm/tests/unit/defconstant
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; declare-skip unit tests (038-stage0-extend User Story 3)
(defpackage #:clysm/tests/unit/declare-skip
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; defstruct-expand unit tests (038-stage0-extend User Story 5)
(defpackage #:clysm/tests/unit/defstruct-expand
  (:use #:cl #:rove))

;; condition-expand unit tests (038-stage0-extend User Story 2)
(defpackage #:clysm/tests/unit/condition-expand
  (:use #:cl #:rove))

;; error-report unit tests (038-stage0-extend User Story 4)
(defpackage #:clysm/tests/unit/error-report
  (:use #:cl #:rove))

;; Stage 0 extend contract tests (038-stage0-extend combined validation)
(defpackage #:clysm/tests/contract/stage0-extend
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Stage 0 compile rate integration tests (038-stage0-extend final validation)
(defpackage #:clysm/tests/integration/stage0-compile-rate
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers #:with-temp-wasm-file))

;;; ============================================================
;;; Stage 1 Compiler Generation Tests (039-stage1-compiler-gen)
;;; ============================================================

;; Stage 1 runner unit tests (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-runner
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:wasmtime-available-p
                #:load-stage0
                #:run-form
                #:error-from-wasm
                #:make-source-form
                #:source-form-operator
                #:compilation-result-form-id
                #:stage1-runtime-error
                #:stage1-stage0-invalid
                #:stage1-file-not-found
                #:stage1-unsupported-feature))

;; Stage 1 load contract tests (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-load
  (:use #:cl #:rove))

;; Stage 1 arithmetic integration tests (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-arith
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:wasmtime-available-p
                #:run-form))

;; Stage 1 defun integration tests (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-defun
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:wasmtime-available-p
                #:run-form
                #:make-source-form
                #:source-form-operator))

;; Stage 1 error integration tests (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-error
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:wasmtime-available-p
                #:run-form
                #:stage1-file-not-found
                #:stage1-unsupported-feature
                #:stage1-runtime-error))

;; Stage 1 reader unit tests - User Story 2 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-reader
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:get-module-paths
                #:read-source-forms
                #:stage1-file-not-found))

;; Stage 1 filesystem contract tests - User Story 2 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-fs
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:get-module-paths))

;; Stage 1 modules integration tests - User Story 2 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-modules
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:get-module-paths))

;; Stage 1 progress unit tests - User Story 3 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-progress
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:make-source-form
                #:make-compilation-result
                #:start-module-tracking
                #:record-form-result
                #:complete-module-tracking
                #:generate-summary
                #:write-progress-report))

;; Stage 1 report contract tests - User Story 3 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-report
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:write-progress-report))

;; Stage 1 generator unit tests - User Story 4 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-generator
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:make-source-form
                #:make-compilation-result
                #:write-stage1-binary))

;; Stage 1 blocker unit tests - User Story 5 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-blocker
  (:use #:cl #:rove))

;; Stage 1 diff unit tests - User Story 6 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/unit/stage1-diff
  (:use #:cl #:rove))

;; Stage 1 timing integration tests - User Story 3 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-timing
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:read-all-modules
                #:source-module-forms
                #:source-form-compilable-p
                #:source-form-id
                #:make-compilation-result
                #:make-source-form))

;; Backward compatibility integration tests (001-ffi-import-architecture T060)
(defpackage #:clysm/tests/integration/backward-compat
  (:use #:cl #:rove))

;; Stage 1 validate contract tests - User Story 4 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-validate
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:validate-stage1
                #:write-stage1-binary))

;; Stage 1 generation integration tests - User Story 4 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-gen
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:make-source-form
                #:compilation-result-success-p
                #:write-stage1-binary))

;; Stage 1 blocker contract tests - User Story 5 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-blocker
  (:use #:cl #:rove))

;; Stage 1 diff contract tests - User Story 6 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/contract/stage1-diff
  (:use #:cl #:rove))

;; Stage 1 full integration tests - Phase 9 (039-stage1-compiler-gen)
(defpackage #:clysm/tests/integration/stage1-full
  (:use #:cl #:rove)
  (:import-from #:clysm/stage1
                #:read-all-modules
                #:source-module-forms
                #:source-form-compilable-p
                #:make-source-form
                #:compilation-result-success-p
                #:write-stage1-binary
                #:validate-stage1))

;;; ============================================================
;;; ANSI Numeric Functions Tests (001-numeric-functions)
;;; ============================================================

;; Basic functions unit tests (001-numeric-functions US1)
;; Tests for signum, max, min (abs, gcd, lcm already in math-functions)
(defpackage #:clysm/tests/unit/basic-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run #:compile-and-run-numeric))

;; Trigonometric functions unit tests (001-numeric-functions US2)
;; Tests for sin, cos, tan, asin, acos, atan
(defpackage #:clysm/tests/unit/trig-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run-numeric)
  (:import-from #:clysm/tests/helpers #:approx= #:*float-epsilon*))

;; Bitwise operations unit tests (001-numeric-functions US3)
;; Tests for logcount, integer-length (ash, logand etc. already exist)
(defpackage #:clysm/tests/unit/bitwise-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run #:compile-and-run-numeric))

;; Hyperbolic functions unit tests (001-numeric-functions US5)
;; Tests for sinh, cosh, tanh, asinh, acosh, atanh
(defpackage #:clysm/tests/unit/hyperbolic-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run-numeric)
  (:import-from #:clysm/tests/helpers #:approx= #:*float-epsilon*))

;; Complex number operations unit tests (001-numeric-functions US6)
;; Tests for complex, realpart, imagpart, conjugate, phase
(defpackage #:clysm/tests/unit/complex-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run-numeric)
  (:import-from #:clysm/tests/helpers #:approx= #:*float-epsilon*))

;; Numeric compliance suite integration tests (001-numeric-functions)
;; Type contagion tests and ANSI numbers category compliance
(defpackage #:clysm/tests/integration/numeric-suite
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run-numeric)
  (:import-from #:clysm/tests/helpers #:approx= #:*float-epsilon*))

;; Byte operations integration tests (001-numeric-predicates)
;; Tests for logbitp, logtest, byte, byte-size, byte-position, ldb, dpb, mask-field, deposit-field
(defpackage #:clysm/tests/integration/byte-ops
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; Array Primitives Tests (001-ansi-array-primitives)
;;; Phase 13D-1: ANSI CL Array/Sequence Primitives
;;; ============================================================

;; Array primitives unit tests (001-ansi-array-primitives)
;; Tests for aref, svref, schar, elt codegen
(defpackage #:clysm/tests/unit/array-primitives
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Array primitives Wasm contract tests (001-ansi-array-primitives)
(defpackage #:clysm/tests/contract/array-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Array primitives integration tests (001-ansi-array-primitives)
(defpackage #:clysm/tests/integration/array
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; Array Operations Tests (001-ansi-array-ops)
;;; Phase 15C: ANSI CL Array Operations Enhancement
;;; ============================================================

;; Array operations unit tests (001-ansi-array-ops)
;; Tests for array-rank, array-dimension, array-dimensions, array-total-size,
;; array-row-major-index, row-major-aref, adjustable-array-p, adjust-array codegen
(defpackage #:clysm/tests/unit/array-ops
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Array operations Wasm contract tests (001-ansi-array-ops)
(defpackage #:clysm/tests/contract/array-ops-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Array operations integration tests (001-ansi-array-ops)
(defpackage #:clysm/tests/integration/array-ops
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;;; ============================================================
;;; Sequence Operations Tests (001-ansi-sequence-operations)
;;; Phase 13D-2: ANSI CL Sequence Operations
;;; ============================================================

;; Sequence operations codegen unit tests (001-ansi-sequence-operations)
;; Tests for compile-subseq, compile-concatenate, compile-make-string,
;; compile-copy-seq codegen
(defpackage #:clysm/tests/unit/sequence-codegen
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Sequence operations Wasm contract tests (001-ansi-sequence-operations)
(defpackage #:clysm/tests/contract/sequence-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Bootstrap Fixpoint Tests (001-bootstrap-fixpoint)
;;; Phase 13D-9: Fixed-point achievement
;;; ============================================================

;; Fixpoint exports contract tests - T010 (001-bootstrap-fixpoint US1)
;; Verifies Stage 1 exports compile_form function
(defpackage #:clysm/tests/contract/fixpoint-exports
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers #:with-temp-wasm-file))

;; Fixpoint signature contract tests - T011 (001-bootstrap-fixpoint US1)
;; Verifies compile_form has correct signature (anyref -> anyref)
(defpackage #:clysm/tests/contract/fixpoint-signature
  (:use #:cl #:rove)
  (:import-from #:clysm/tests/helpers #:with-temp-wasm-file))

;; Blocker report contract tests - T012 (001-bootstrap-fixpoint US4)
;; Verifies blocker report JSON schema
(defpackage #:clysm/tests/contract/fixpoint-blocker
  (:use #:cl #:rove))

;;; ============================================================
;;; Numeric Predicates Tests (001-numeric-predicates)
;;; Phase 14B: Numeric Type Predicates Enhancement
;;; ============================================================

;; Byte operations Wasm contract tests (001-numeric-predicates)
;; Validates Wasm output for logbitp, logtest, byte, ldb, dpb, etc.
(defpackage #:clysm/tests/contract/byte-ops-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Numeric Format Tests (001-numeric-format)
;;; Phase 14C: Numeric Conversion and Formatting
;;; ============================================================

;; Rationalize unit tests (001-numeric-format US1)
(defpackage #:clysm/tests/unit/numeric/rationalize
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Write-to-string unit tests (001-numeric-format US2/US3)
(defpackage #:clysm/tests/unit/numeric/write-to-string
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Rationalize Wasm contract tests (001-numeric-format)
(defpackage #:clysm/tests/contract/rationalize-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Write-to-string Wasm contract tests (001-numeric-format)
(defpackage #:clysm/tests/contract/write-to-string-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;; Numeric format integration tests (001-numeric-format)
;; End-to-end tests for rationalize and write-to-string
(defpackage #:clysm/tests/integration/numeric-format
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run #:validate-wasm-silent)
  (:import-from #:clysm/tests/helpers #:approx=))

;;; ============================================================
;;; Sequence Generic Functions Tests (001-ansi-sequence-functions)
;;; Phase 15B: ANSI CL Sequence Generic Functions
;;; ============================================================

;; Sequence utility unit tests (001-ansi-sequence-functions)
;; Tests for validate-bounding-indices, %sequence-length, %sequence-ref, etc.
(defpackage #:clysm/tests/unit/sequences/util
  (:use #:cl #:rove))

;; Count functions unit tests (001-ansi-sequence-functions US1)
(defpackage #:clysm/tests/unit/sequences/count
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Find functions unit tests (001-ansi-sequence-functions US2)
(defpackage #:clysm/tests/unit/sequences/find
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Position functions unit tests (001-ansi-sequence-functions US2)
(defpackage #:clysm/tests/unit/sequences/position
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Mismatch functions unit tests (001-ansi-sequence-functions US3)
(defpackage #:clysm/tests/unit/sequences/mismatch
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Search functions unit tests (001-ansi-sequence-functions US3)
(defpackage #:clysm/tests/unit/sequences/search
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Substitute functions unit tests (001-ansi-sequence-functions US4)
(defpackage #:clysm/tests/unit/sequences/substitute
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Remove-duplicates functions unit tests (001-ansi-sequence-functions US5)
(defpackage #:clysm/tests/unit/sequences/remove-duplicates
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Fill functions unit tests (001-ansi-sequence-functions US6)
(defpackage #:clysm/tests/unit/sequences/fill
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Replace functions unit tests (001-ansi-sequence-functions US6)
(defpackage #:clysm/tests/unit/sequences/replace
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Sequence functions Wasm contract tests (001-ansi-sequence-functions)
(defpackage #:clysm/tests/contract/sequences
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Character Functions Tests (001-ansi-char-functions)
;;; Phase 16A: ANSI CL Character Functions
;;; ============================================================

;; Character functions unit tests (001-ansi-char-functions)
;; Tests for graphic-char-p, standard-char-p, both-case-p,
;; char-name, name-char, digit-char, char-int
(defpackage #:clysm/tests/unit/character-functions
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; Character functions Wasm contract tests (001-ansi-char-functions)
;; Validates Wasm output for character predicate and conversion functions
(defpackage #:clysm/tests/contract/character-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; String Trim Functions Tests (001-ansi-string-trim)
;;; Phase 16B: ANSI CL String Trim and nstring Functions
;;; ============================================================

;; String trim functions unit tests (001-ansi-string-trim)
;; Tests for string-trim, string-left-trim, string-right-trim,
;; nstring-upcase, nstring-downcase, nstring-capitalize
(defpackage #:clysm/tests/unit/string-trim
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:compile-and-run))

;; String trim functions Wasm contract tests (001-ansi-string-trim)
;; Validates Wasm output for trim and destructive case functions
(defpackage #:clysm/tests/contract/string-trim-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Character Literal Compilation Tests (001-char-literal-compile)
;;; Phase 13D-1a: Character Literal Compilation Support
;;; ============================================================

;; Character literal compilation unit tests (001-char-literal-compile)
;; Tests for compile-quoted-element with character literals
(defpackage #:clysm/tests/unit/char-literal
  (:use #:cl #:rove))

;; Character literal Wasm contract tests (001-char-literal-compile)
;; Validates Wasm output for quoted character expressions
(defpackage #:clysm/tests/contract/char-wasm
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; Arithmetic Primitives Tests (001-arithmetic-primitives)
;;; Phase 13D-1b: 1- and 1+ Arithmetic Primitives
;;; ============================================================

;; Arithmetic primitives unit tests (001-arithmetic-primitives)
;; Tests for compile-1- and compile-1+ codegen
(defpackage #:clysm/tests/unit/arithmetic-primitives
  (:use #:cl #:rove))

;; Arithmetic primitives Wasm contract tests (001-arithmetic-primitives)
;; Validates Wasm output for 1- and 1+ expressions
(defpackage #:clysm/tests/contract/arithmetic-primitives
  (:use #:cl #:rove)
  (:import-from #:clysm/tests #:validate-wasm-silent))

;;; ============================================================
;;; FFI Import Architecture Tests (001-ffi-import-architecture)
;;; Phase: Selective FFI import emission
;;; ============================================================

;; FFI usage analyzer unit tests (001-ffi-import-architecture)
;; Tests for analyze-ffi-usage, ffi-function-p, static/dynamic detection
(defpackage #:clysm/tests/unit/ffi-usage
  (:use #:cl #:rove #:clysm/compiler/analyzer/ffi-usage))

;; Dynamic call integration tests (001-ffi-import-architecture)
;; Tests for $dynamic-call runtime resolution
(defpackage #:clysm/tests/integration/dynamic-call
  (:use #:cl #:rove))

