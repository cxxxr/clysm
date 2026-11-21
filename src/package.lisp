;;;; package.lisp — host-side packages of the Clysm cross compiler

(defpackage :clysm-wasm
  (:use :cl)
  (:export #:make-wasm-module #:wasm-module #:add-type #:add-func #:add-import-func
           #:add-global #:add-tag #:add-export #:add-data #:encode-module
           #:module-start #:type-index #:func-index #:global-index #:declare-func-ref
           #:wasm-func #:wasm-func-name #:wasm-func-type #:wasm-func-locals #:wasm-func-body
           #:wasm-func-local-names #:func-defined-p #:global-defined-p #:find-func
           #:add-import-global #:add-import-tag #:wasm-module-base-types #:wasm-module-nonnull-globals
           #:wasm-module-cast-rewrites #:wasm-module-image-defaults #:wasm-module-eager-funcs #:remove-unreachable-functions #:wasm-module-funcs
           #:wasm-module-memory #:wasm-module-memory-import #:wasm-module-table #:wasm-module-code-base #:wasm-module-globals #:shake-module #:*shake-why* #:*func-body-sizes* #:write-shake-why #:*func-source*
           #:*func-segment* #:wasm-func-segment #:wasm-func-refs #:body-refs #:wasm-func-p
           #:wasm-module-data #:wasm-func-ref-list #:order-functions-by-use
           #:*shake-heap-hook* #:*shake-mark* #:*shake-walking* #:shake-edge #:wasm-module-declared-refs
           #:adopt-func #:copy-module-types))

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
