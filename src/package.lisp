;;;; package.lisp — host-side packages of the Clysm cross compiler

(defpackage :clysm-wasm
  (:use :cl)
  (:export #:make-wasm-module #:wasm-module #:add-type #:add-func #:add-import-func
           #:add-global #:add-tag #:add-export #:add-data #:encode-module
           #:module-start #:type-index #:func-index #:global-index #:declare-func-ref
           #:wasm-func #:wasm-func-name #:wasm-func-type #:wasm-func-locals #:wasm-func-body
           #:wasm-func-local-names #:func-defined-p #:global-defined-p #:find-func
           #:add-import-global #:add-import-tag #:wasm-module-base-types #:wasm-module-nonnull-globals))

;;; Host package that holds the compiler itself.
(defpackage :clysm
  (:use :cl :clysm-wasm)
  (:export #:compile-program))

;;; Target system-internal package. Runtime library helpers live here.
;;; It is a *host* package whose symbols are also used as target symbols
;;; (mapped to the target package "SYSTEM").
(defpackage :clysm-sys
  (:use :cl)
  (:nicknames))
