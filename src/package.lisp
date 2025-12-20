;;;; package.lisp - Package definitions for clysm

(defpackage #:clysm/utils
  (:use #:cl)
  (:export
   ;; LEB128 encoding
   #:encode-uleb128
   #:encode-sleb128
   #:decode-uleb128
   #:decode-sleb128
   ;; Byte vector utilities
   #:make-byte-buffer
   #:byte-buffer
   #:byte-buffer-data
   #:buffer-contents
   #:buffer-write-byte
   #:buffer-write-bytes
   #:buffer-write-u32
   #:buffer-write-f32
   #:buffer-write-f64
   #:buffer-write-uleb128
   #:buffer-write-sleb128
   #:buffer-write-name
   #:buffer-length))

(defpackage #:clysm/wasm
  (:use #:cl #:clysm/utils)
  (:export
   ;; WASM types
   #:+type-i32+
   #:+type-i64+
   #:+type-f32+
   #:+type-f64+
   #:+type-v128+
   #:+type-funcref+
   #:+type-externref+
   #:+type-anyref+
   #:+type-eqref+
   #:+type-i31ref+
   #:+type-structref+
   #:+type-arrayref+
   #:+type-nullref+
   #:+type-func+
   #:+type-void+
   ;; Section IDs
   #:+section-custom+
   #:+section-type+
   #:+section-import+
   #:+section-function+
   #:+section-table+
   #:+section-memory+
   #:+section-global+
   #:+section-export+
   #:+section-start+
   #:+section-element+
   #:+section-code+
   #:+section-data+
   #:+section-data-count+
   ;; Instructions
   #:+op-unreachable+
   #:+op-nop+
   #:+op-block+
   #:+op-loop+
   #:+op-if+
   #:+op-else+
   #:+op-end+
   #:+op-br+
   #:+op-br-if+
   #:+op-br-table+
   #:+op-return+
   #:+op-call+
   #:+op-call-indirect+
   #:+op-return-call+
   #:+op-return-call-indirect+
   #:+op-drop+
   #:+op-select+
   #:+op-local-get+
   #:+op-local-set+
   #:+op-local-tee+
   #:+op-global-get+
   #:+op-global-set+
   #:+op-i32-const+
   #:+op-i64-const+
   #:+op-f32-const+
   #:+op-f64-const+
   #:+op-i32-add+
   #:+op-i32-sub+
   #:+op-i32-mul+
   #:+op-i32-div-s+
   #:+op-i32-div-u+
   #:+op-i32-rem-s+
   #:+op-i32-rem-u+
   #:+op-i32-eqz+
   #:+op-i32-eq+
   #:+op-i32-ne+
   #:+op-i32-lt-s+
   #:+op-i32-lt-u+
   #:+op-i32-gt-s+
   #:+op-i32-gt-u+
   #:+op-i32-le-s+
   #:+op-i32-le-u+
   #:+op-i32-ge-s+
   #:+op-i32-ge-u+
   ;; Bitwise operations
   #:+op-i32-and+
   #:+op-i32-or+
   #:+op-i32-xor+
   #:+op-i32-shl+
   #:+op-i32-shr-s+
   #:+op-i32-shr-u+
   ;; Memory instructions
   #:+op-i32-load+
   #:+op-i32-load8-s+
   #:+op-i32-load8-u+
   #:+op-i32-load16-s+
   #:+op-i32-load16-u+
   #:+op-i32-store+
   #:+op-i32-store8+
   #:+op-i32-store16+
   ;; GC opcodes
   #:+gc-prefix+
   #:+op-struct-new+
   #:+op-struct-new-default+
   #:+op-struct-get+
   #:+op-struct-get-s+
   #:+op-struct-get-u+
   #:+op-struct-set+
   #:+op-array-new+
   #:+op-array-new-default+
   #:+op-array-get+
   #:+op-array-get-s+
   #:+op-array-get-u+
   #:+op-array-set+
   #:+op-array-len+
   #:+op-ref-null+
   #:+op-ref-is-null+
   #:+op-ref-func+
   #:+op-ref-eq+
   #:+op-ref-as-non-null+
   #:+op-ref-cast+
   #:+op-ref-test+
   #:+op-ref-i31+
   #:+op-i31-get-s+
   #:+op-i31-get-u+
   ;; Module building
   #:make-wasm-module
   #:wasm-module
   #:wasm-module-types
   #:wasm-module-functions
   #:wasm-module-exports
   #:wasm-module-memories
   #:wasm-module-globals
   #:add-type
   #:add-func-type
   #:add-function
   #:add-export
   #:add-memory
   #:add-global
   #:add-table
   #:add-element
   #:add-data
   #:wasm-module-tables
   #:wasm-module-elements
   #:wasm-module-data
   #:wasm-module-import-func-count
   #:wasm-module-func-count
   #:finalize-module
   ;; Data segment structure
   #:make-wasm-data
   #:wasm-data
   #:wasm-data-memory-idx
   #:wasm-data-offset
   #:wasm-data-data
   ;; Type definitions
   #:make-func-type
   #:func-type
   #:func-type-params
   #:func-type-results
   ;; Function definitions
   #:make-wasm-func
   #:wasm-func
   #:wasm-func-type-idx
   #:wasm-func-locals
   #:wasm-func-body
   ;; Export definitions
   #:make-wasm-export
   #:wasm-export
   #:+export-func+
   #:+export-table+
   #:+export-memory+
   #:+export-global+
   ;; Encoding
   #:encode-module
   #:save-module))

(defpackage #:clysm/reader
  (:use #:cl)
  (:export
   #:read-source
   #:read-file))

(defpackage #:clysm/ast
  (:use #:cl)
  (:export
   ;; AST nodes
   #:ast-node
   #:const-node
   #:const-node-value
   #:var-node
   #:var-node-name
   #:if-node
   #:if-node-test
   #:if-node-then
   #:if-node-else
   #:let-node
   #:let-node-bindings
   #:let-node-body
   #:lambda-node
   #:lambda-node-params
   #:lambda-node-body
   #:call-node
   #:call-node-func
   #:call-node-args
   #:progn-node
   #:progn-node-forms
   ;; AST construction
   #:make-const-node
   #:make-var-node
   #:make-if-node
   #:make-let-node
   #:make-lambda-node
   #:make-call-node
   #:make-progn-node
   ;; Walker
   #:walk-ast
   #:map-ast))

(defpackage #:clysm/ir
  (:use #:cl #:clysm/ast)
  (:export
   ;; IR nodes
   #:ir-node
   #:ir-const
   #:ir-local-ref
   #:ir-local-set
   #:ir-if
   #:ir-block
   #:ir-loop
   #:ir-br
   #:ir-call
   #:ir-return
   #:ir-seq
   #:ir-primop
   ;; Conversion
   #:ast-to-ir
   ;; Optimization
   #:optimize-ir))

(defpackage #:clysm/compiler
  (:use #:cl #:clysm/wasm #:clysm/ast #:clysm/ir)
  (:export
   ;; Environment
   #:make-compile-env
   #:compile-env
   #:make-initial-env
   #:env-lookup
   #:env-extend
   #:env-add-local
   #:env-add-function
   #:local-info
   #:local-info-index
   ;; Compilation
   #:compile-form
   #:compile-toplevel
   #:compile-module
   #:compile-progn
   ;; Code generation
   #:generate-code
   ;; Runtime
   #:*heap-pointer-global*
   #:*cons-size*
   #:setup-runtime))

(defpackage #:clysm/runtime
  (:use #:cl)
  (:export
   ;; Runtime type tags
   #:+tag-fixnum+
   #:+tag-cons+
   #:+tag-symbol+
   #:+tag-string+
   #:+tag-vector+
   #:+tag-closure+
   #:+tag-float+
   ;; Symbol layout
   #:+symbol-size+
   #:+symbol-name-offset+
   #:+symbol-value-offset+
   #:+symbol-function-offset+
   #:+symbol-plist-offset+
   ;; String layout
   #:+string-header-size+
   #:+string-length-offset+
   #:+string-data-offset+))

(defpackage #:clysm/stdlib
  (:use #:cl)
  (:export
   ;; Primitive operations
   #:primitive-p
   #:get-primitive))

(defpackage #:clysm
  (:use #:cl
        #:clysm/utils
        #:clysm/wasm
        #:clysm/reader
        #:clysm/ast
        #:clysm/ir
        #:clysm/compiler
        #:clysm/runtime
        #:clysm/stdlib)
  (:export
   ;; Main API
   #:compile-to-wasm
   #:compile-file-to-wasm
   #:save-wasm-module))
