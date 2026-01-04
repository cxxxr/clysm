;;;; package.lisp - Clysm Package Definitions
;;;;
;;;; No external dependencies - all utilities are self-contained

(defpackage #:clysm
  (:use #:cl)
  (:export
   ;; Utilities (util.lisp)
   #:hash-table-keys
   #:hash-table-values
   #:flatten
   #:mappend
   #:lastcar
   #:ensure-list
   #:alist-get
   #:plist-get
   #:split-sequence
   #:if-let
   #:when-let
   #:if-let*
   #:with-gensyms
   #:once-only
   #:bytes-to-vector
   #:vector-to-bytes
   #:concat-bytes
   #:string-join
   #:string-prefix-p
   #:string-suffix-p
   #:read-file-bytes
   #:write-file-bytes

   ;; Conditions (conditions.lisp)
   #:clysm-error
   #:clysm-compile-error
   #:clysm-runtime-error
   #:clysm-syntax-error
   #:clysm-type-error
   #:clysm-undefined-error
   #:clysm-wasm-error
   #:compile-error
   #:syntax-error
   #:type-error*
   #:undefined-error
   #:wasm-error

   ;; LEB128 encoding (backend/leb128.lisp)
   #:encode-uleb128
   #:encode-sleb128
   #:decode-uleb128
   #:decode-sleb128
   #:encode-u32
   #:encode-s32
   #:encode-s33
   #:encode-u64
   #:encode-s64
   #:encode-vector
   #:encode-byte-vector
   #:encode-name
   #:encode-f32
   #:encode-f64

   ;; Wasm types (backend/wasm-types.lisp)
   #:encode-valtype
   #:make-functype
   #:wasm-functype
   #:wasm-functype-params
   #:wasm-functype-results
   #:encode-functype
   #:make-field
   #:wasm-field
   #:wasm-field-type
   #:wasm-field-mutable
   #:make-structtype
   #:wasm-structtype
   #:wasm-structtype-fields
   #:encode-structtype
   #:make-arraytype
   #:wasm-arraytype
   #:wasm-arraytype-element
   #:wasm-arraytype-mutable
   #:encode-arraytype
   #:make-wasm-type
   #:wasm-type
   #:wasm-type-index
   #:wasm-type-name
   #:wasm-type-definition
   #:make-wasm-func
   #:wasm-func
   #:wasm-func-index
   #:wasm-func-name
   #:wasm-func-type-index
   #:wasm-func-locals
   #:wasm-func-body
   #:make-import
   #:wasm-import
   #:make-export
   #:wasm-export
   #:encode-export
   #:make-limits
   #:wasm-limits
   #:wasm-limits-min
   #:wasm-limits-max
   #:encode-limits
   #:make-memory
   #:wasm-memory
   #:make-table
   #:wasm-table
   #:make-global
   #:wasm-global
   #:make-wasm-module
   #:wasm-module
   #:wasm-module-types
   #:wasm-module-imports
   #:wasm-module-funcs
   #:wasm-module-tables
   #:wasm-module-memories
   #:wasm-module-globals
   #:wasm-module-exports
   #:wasm-module-start
   #:wasm-module-elements
   #:wasm-module-data
   #:wasm-module-custom
   #:module-add-type
   #:module-add-func
   #:module-add-export
   #:module-finalize

   ;; Sections (backend/sections.lisp)
   #:emit-section
   #:emit-type-section
   #:emit-import-section
   #:emit-function-section
   #:emit-table-section
   #:emit-memory-section
   #:emit-global-section
   #:emit-export-section
   #:emit-start-section
   #:emit-code-section
   #:emit-custom-section

   ;; Wasm emit (backend/wasm-emit.lisp)
   #:emit-wasm-binary
   #:emit-wasm-to-file
   #:emit-wasm-to-vector
   #:opcode
   #:emit-op
   #:emit-i32.const
   #:emit-i64.const
   #:emit-local.get
   #:emit-local.set
   #:emit-call
   #:emit-end
   #:encode-blocktype

   ;; WAT print (backend/wat-print.lisp)
   #:print-wat
   #:wat-to-string

   ;; Runtime types (runtime/types.lisp)
   ;; Type registry
   #:type-registry
   #:make-type-registry
   #:type-registry-func-0
   #:type-registry-func-1
   #:type-registry-func-2
   #:type-registry-func-n
   #:type-registry-cons
   #:type-registry-symbol
   #:type-registry-closure
   #:type-registry-env
   #:type-registry-string
   #:type-registry-vector
   #:type-registry-nil-type
   #:type-registry-unbound
   #:register-core-types

   ;; Type index accessors
   #:cons-type-index
   #:symbol-type-index
   #:closure-type-index
   #:env-type-index
   #:string-type-index
   #:vector-type-index
   #:nil-type-index
   #:unbound-type-index

   ;; Field index constants
   #:+cons-car+
   #:+cons-cdr+
   #:+symbol-name+
   #:+symbol-value+
   #:+symbol-function+
   #:+symbol-plist+
   #:+closure-code-0+
   #:+closure-code-1+
   #:+closure-code-2+
   #:+closure-code-n+
   #:+closure-env+
   #:+nil-car+
   #:+nil-cdr+
   #:+nil-name+
   #:+nil-value+
   #:+nil-function+
   #:+nil-plist+

   ;; GC instruction emitters
   #:emit-struct.new
   #:emit-struct.get
   #:emit-struct.set
   #:emit-array.new
   #:emit-array.new-fixed
   #:emit-array.get
   #:emit-array.set
   #:emit-array.len
   #:emit-ref.i31
   #:emit-i31.get-s
   #:emit-i31.get-u
   #:emit-ref.test
   #:emit-ref.cast

   ;; High-level object emitters
   #:emit-make-cons
   #:emit-car
   #:emit-cdr
   #:emit-rplaca
   #:emit-rplacd
   #:emit-fixnum-from-i32
   #:emit-fixnum-to-i32
   #:emit-consp
   #:emit-symbolp
   #:emit-functionp))

(defpackage #:clysm/backend
  (:use #:cl #:clysm)
  (:documentation "WebAssembly binary generation backend"))

(defpackage #:clysm/runtime
  (:use #:cl #:clysm)
  (:documentation "Runtime type definitions for Lisp objects"))
