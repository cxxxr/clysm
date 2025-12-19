;;;; package.lisp - Test package definitions

(defpackage #:cl-wasm/tests
  (:use #:cl #:fiveam)
  (:import-from #:cl-wasm/utils
                #:encode-uleb128
                #:encode-sleb128
                #:make-byte-buffer
                #:buffer-contents
                #:buffer-write-byte
                #:buffer-write-bytes
                #:buffer-write-u32)
  (:import-from #:cl-wasm/wasm
                #:make-wasm-module
                #:add-func-type
                #:add-function
                #:add-export
                #:finalize-module
                #:encode-module
                #:+type-i32+
                #:+export-func+
                #:+op-i32-const+
                #:+op-i32-add+
                #:+op-end+)
  (:import-from #:cl-wasm/compiler
                #:compile-form
                #:compile-module
                #:make-compile-env
                #:make-initial-env)
  (:export #:run-tests))
