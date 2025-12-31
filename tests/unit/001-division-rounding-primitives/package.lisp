;;;; package.lisp - Package definition for division/rounding primitives tests
;;;; Phase 13D-1e: Division/Rounding Function Primitives
(in-package :cl-user)

(defpackage #:clysm/tests/unit/rounding-primitives
  (:use #:cl #:rove)
  (:import-from #:clysm/compiler
                #:compile-to-wasm
                #:compile-to-wat))
