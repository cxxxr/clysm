;;;; package.lisp - Package definition for division/rounding contract tests
;;;; Phase 13D-1e: Division/Rounding Function Primitives
(in-package :cl-user)

(defpackage #:clysm/tests/contract/rounding-primitives
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler
                #:compile-to-wasm)
  (:import-from #:clysm/tests/utils
                #:validate-wasm-silent))
