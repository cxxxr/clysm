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
           #:wasm-runtime-error))

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
                #:assert-equals)
  (:export #:run-all-tests
           #:compile-and-run
           #:compile-and-run-numeric
           #:validate-wasm
           #:validate-wasm-silent
           #:with-temp-wasm-file
           #:assert-compiles
           #:assert-validates
           #:assert-equals))

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
