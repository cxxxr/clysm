;;;; package.lisp - Test package definitions

(defpackage #:clysm/tests
  (:use #:cl #:fiveam)
  (:import-from #:clysm/utils
                #:encode-uleb128
                #:encode-sleb128
                #:make-byte-buffer
                #:buffer-contents
                #:buffer-write-byte
                #:buffer-write-bytes
                #:buffer-write-u32)
  (:import-from #:clysm/wasm
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
                #:+op-end+
                ;; WAT output
                #:opcode-name
                #:gc-opcode-name
                #:value-type-name
                #:build-expression-tree
                #:module-to-wat
                #:save-module-as-wat)
  (:import-from #:clysm/compiler
                #:compile-form
                #:compile-module
                #:make-compile-env
                #:make-initial-env
                ;; WAT output
                #:compile-to-wat
                #:disassemble-form
                #:disassemble-to-string
                #:save-as-wat)
  (:export #:run-tests))
