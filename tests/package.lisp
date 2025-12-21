;;;; package.lisp - Test package definitions

(defpackage #:clysm/tests
  (:use #:cl #:rove)
  (:export #:run-all-tests))

(defpackage #:clysm/tests/helpers
  (:use #:cl)
  (:export #:compile-and-run
           #:validate-wasm
           #:with-temp-wasm-file))

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
