;;;; compiler.lisp - Main compiler interface (T064)
;;;; Top-level entry points for compiling Lisp to Wasm

(in-package #:clysm/compiler)

;;; ============================================================
;;; Main Compilation Entry Point
;;; ============================================================

(defun compile-to-wasm (expr &key output)
  "Compile a Lisp expression to Wasm binary.
   If OUTPUT is provided, writes to file and returns pathname.
   Otherwise returns byte vector.

   Examples:
     (compile-to-wasm '(+ 1 2))
     (compile-to-wasm '(+ 1 2) :output \"add.wasm\")"
  (let* ((module (compile-to-module expr))
         (bytes (emit-module module)))
    (if output
        (progn
          (with-open-file (stream output
                                  :direction :output
                                  :element-type '(unsigned-byte 8)
                                  :if-exists :supersede)
            (write-sequence bytes stream))
          (pathname output))
        bytes)))

(defun compile-to-wat (expr)
  "Compile a Lisp expression to WAT text format.
   Useful for debugging."
  (let ((module (compile-to-module expr)))
    (emit-module-wat module)))

;;; ============================================================
;;; Module Compilation
;;; ============================================================

(defstruct (compiled-module (:conc-name compiled-module-))
  "Compiled Wasm module (compiler's internal representation)"
  (types nil :type list)
  (functions nil :type list)
  (globals nil :type list)
  (exports nil :type list)
  (main-func-idx nil :type (or null fixnum)))

(defun compile-to-module (expr)
  "Compile a Lisp expression to a module structure.
   Extracts defuns as separate functions and compiles the main body."
  ;; Reset lambda state for fresh compilation
  (clysm/compiler/codegen/func-section:reset-lambda-state)
  ;; Reset special variable global tracking
  (clysm/compiler/codegen/func-section:reset-special-var-globals)
  ;; Clear special variable registry from previous compilations
  (clysm/compiler/env:clear-special-variables)
  ;; Parse expression to AST (this registers special variables)
  (let* ((ast (clysm/compiler/ast:parse-expr expr))
         (env (clysm/compiler/codegen/func-section:make-env))
         (module (make-compiled-module))
         (functions '())
         (defuns '())
         (main-forms '()))
    ;; Pre-allocate globals for all special variables registered during parsing
    ;; This ensures defuns can reference special variables before defvar is compiled
    (maphash (lambda (name info)
               (declare (ignore info))
               (clysm/compiler/codegen/func-section:allocate-special-var-global name))
             clysm/compiler/env:*special-variables*)
    ;; Set up runtime globals
    (setf (compiled-module-globals module)
          (list (clysm/runtime/objects:make-nil-global)
                (clysm/runtime/objects:make-unbound-global)))
    ;; Reserve index 0 for main function
    (clysm/compiler/codegen/func-section:env-set-function-counter env 1)
    ;; First pass: extract defuns and register them
    (let ((forms (if (typep ast 'clysm/compiler/ast:ast-progn)
                     (clysm/compiler/ast:ast-progn-forms ast)
                     (list ast))))
      (dolist (form forms)
        (if (typep form 'clysm/compiler/ast:ast-defun)
            (progn
              (push form defuns)
              ;; Register function - indices start at 1 (0 is $main)
              (let ((name (clysm/compiler/ast:ast-defun-name form)))
                (clysm/compiler/codegen/func-section:env-add-function env name)))
            (push form main-forms)))
      (setf defuns (nreverse defuns))
      (setf main-forms (nreverse main-forms)))
    ;; Second pass: compile defuns as separate functions
    (dolist (defun-ast defuns)
      (let ((func-info (clysm/compiler/codegen/func-section:compile-defun defun-ast env)))
        (push func-info functions)))
    (setf functions (nreverse functions))
    ;; Compile main function (entry point)
    (let* ((main-ast (if (= 1 (length main-forms))
                         (first main-forms)
                         (clysm/compiler/ast:make-ast-progn :forms main-forms)))
           (main-func (compile-main-function main-ast env)))
      ;; Main function goes first (index 0)
      (setf (compiled-module-functions module) (cons main-func functions))
      ;; Compile any pending lambdas and append them
      (let ((lambda-funcs (clysm/compiler/codegen/func-section:compile-pending-lambdas)))
        (when lambda-funcs
          (setf (compiled-module-functions module)
                (append (compiled-module-functions module) lambda-funcs))))
      (setf (compiled-module-main-func-idx module) 0)
      (setf (compiled-module-exports module)
            (list (list "_start" :func 0)))
      ;; Add special variable symbol globals (T025)
      ;; get-all-special-var-globals returns ((name . index) ...) sorted by index
      (let ((special-var-globals (clysm/compiler/codegen/func-section:get-all-special-var-globals)))
        ;; Append symbol globals in order (indices start at 2)
        (dolist (entry special-var-globals)
          (let ((name (car entry)))
            (setf (compiled-module-globals module)
                  (append (compiled-module-globals module)
                          (list (make-symbol-global name))))))))
    module))

(defun make-symbol-global (name)
  "Create a symbol global for a special variable.
   The symbol struct is: (name, value, function, plist)
   Uses struct.new_default to initialize all fields to null.
   A null value field indicates the variable is unbound.
   The type is (ref null 3) for struct.get/set compatibility."
  (clysm/backend/sections:make-wasm-global
   :name (intern (format nil "$sym_~A" (symbol-name name)))
   :type '(:ref-null 3)  ; Nullable reference to symbol struct type
   :mutability :var  ; Symbol's value field is mutable
   :init-expr '((:struct.new_default 3))))  ; struct.new_default $symbol (type 3)

(defun compile-main-function (ast env)
  "Compile an AST as the main function."
  (let ((instrs (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
    ;; We need one extra local for the result value to handle null check
    (let ((num-locals (clysm/compiler/codegen/func-section:cenv-local-counter env))
          (result-local-idx (clysm/compiler/codegen/func-section:cenv-local-counter env)))
      (list :name '$main
            :params nil
            :result :i32  ; Return i32 for command-line compatibility
            ;; Include locals allocated during body compilation + one for result
            :locals (loop for i from 0 to num-locals
                          collect (list (gensym "local")
                                        (clysm/compiler/codegen/func-section:env-local-type env i)))
            :body (append instrs
                          ;; Save result to local, then check for null
                          ;; If null (NIL), return 0
                          ;; Otherwise get from local, cast to i31, extract
                          (list (list :local.tee result-local-idx))
                          '(:ref.is_null
                            (:if (:result :i32))
                            (:i32.const -2147483648))  ; NIL = MIN_INT32 (sentinel)
                          '(:else)
                          (list (list :local.get result-local-idx))
                          '((:ref.cast :i31)
                            :i31.get_s
                            :end))))))

;;; ============================================================
;;; Module Binary Emission
;;; ============================================================

(defun emit-module (module)
  "Emit a compiled module as a Wasm binary byte vector."
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (functions (compiled-module-functions module)))
    ;; Module header (magic + version)
    (emit-header buffer)
    ;; Type section
    (emit-type-section buffer functions)
    ;; Function section
    (emit-function-section buffer functions)
    ;; Tag section (for exception handling)
    (emit-tag-section buffer)
    ;; Global section
    (when (compiled-module-globals module)
      (emit-global-section buffer (compiled-module-globals module)))
    ;; Export section
    (emit-export-section buffer (compiled-module-exports module))
    ;; Element section (for ref.func declarations)
    (emit-element-section buffer functions)
    ;; Code section
    (emit-code-section buffer functions)
    ;; Return as simple vector
    (coerce buffer '(simple-array (unsigned-byte 8) (*)))))

(defun emit-header (buffer)
  "Emit Wasm module header."
  ;; Magic number: \0asm
  (vector-push-extend #x00 buffer)
  (vector-push-extend #x61 buffer)
  (vector-push-extend #x73 buffer)
  (vector-push-extend #x6D buffer)
  ;; Version: 1
  (vector-push-extend #x01 buffer)
  (vector-push-extend #x00 buffer)
  (vector-push-extend #x00 buffer)
  (vector-push-extend #x00 buffer))

(defun is-lambda-function-p (func)
  "Check if a function is a lambda (has $closure as first param)."
  (let* ((params (getf func :params))
         (first-param-name (when (consp (first params))
                             (first (first params)))))
    (and first-param-name
         (string= (symbol-name first-param-name) "$CLOSURE"))))

(defun emit-type-section (buffer functions)
  "Emit Type section with GC types and function types.
   Layout:
   - Types 0-5: GC types ($nil, $unbound, $cons, $symbol, $string, $closure)
   - Type 6-7: Reserved
   - Types 8-12: Function types ($func_0, $func_1, $func_2, $func_3, $func_N)
   - Type 13: Exception tag type (anyref, anyref) -> () for $lisp-throw
   - Types 14+: Regular (non-lambda) function types"
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
        ;; Count non-lambda functions (lambdas reuse types 8-12)
        (regular-func-count (count-if-not #'is-lambda-function-p functions)))
    ;; Number of types = 16 (base types) + regular functions
    ;; Base: 0-5 struct, 6-7 reserved, 8-12 func types, 13 exception tag, 14-15 catch types
    (emit-leb128-unsigned (+ 16 regular-func-count) content)
    ;; Type 0: $nil (empty struct)
    (emit-gc-struct-type content '())
    ;; Type 1: $unbound (empty struct)
    (emit-gc-struct-type content '())
    ;; Type 2: $cons (car, cdr)
    (emit-gc-struct-type content '((:anyref t) (:anyref t)))
    ;; Type 3: $symbol (name, value, function, plist)
    (emit-gc-struct-type content '((:anyref nil) (:anyref t) (:anyref t) (:anyref t)))
    ;; Type 4: $string (array of i8) - actually needs to be array type
    (emit-gc-array-type content :i8 nil)
    ;; Type 5: $closure (code_0, code_1, code_2, code_N, env)
    (emit-gc-struct-type content '((:funcref nil) (:funcref nil)
                                   (:funcref nil) (:funcref nil)
                                   (:anyref t)))
    ;; Types 6-7: Reserved (empty placeholders)
    (emit-gc-struct-type content '())
    (emit-gc-struct-type content '())
    ;; Type 8: $func_0 - (ref $closure) -> anyref
    (emit-func-type-bytes content '(:anyref) '(:anyref))
    ;; Type 9: $func_1 - (ref $closure, anyref) -> anyref
    (emit-func-type-bytes content '(:anyref :anyref) '(:anyref))
    ;; Type 10: $func_2 - (ref $closure, anyref, anyref) -> anyref
    (emit-func-type-bytes content '(:anyref :anyref :anyref) '(:anyref))
    ;; Type 11: $func_3 - (ref $closure, anyref, anyref, anyref) -> anyref
    (emit-func-type-bytes content '(:anyref :anyref :anyref :anyref) '(:anyref))
    ;; Type 12: $func_N - (ref $closure, anyref) -> anyref (list as second param)
    (emit-func-type-bytes content '(:anyref :anyref) '(:anyref))
    ;; Type 13: $tag_lisp_throw - (anyref, anyref) -> () for exception tag
    (emit-func-type-bytes content '(:anyref :anyref) '())
    ;; Type 14: $catch_handler - (anyref, anyref) -> anyref (unused, kept for compatibility)
    (emit-func-type-bytes content '(:anyref :anyref) '(:anyref))
    ;; Type 15: $catch_result - () -> (anyref, anyref) for catch handler block result
    (emit-func-type-bytes content '() '(:anyref :anyref))
    ;; Regular function types (indices 16+) - skip lambdas (they use types 8-12)
    (dolist (func functions)
      (unless (is-lambda-function-p func)
        ;; func type indicator
        (vector-push-extend #x60 content)
        ;; params
        (let ((params (getf func :params)))
          (emit-leb128-unsigned (length params) content)
          (dolist (p params)
            (emit-valtype (second p) content)))
        ;; results
        (let ((result (getf func :result)))
          (if result
              (progn
                (emit-leb128-unsigned 1 content)
                (emit-valtype result content))
              (emit-leb128-unsigned 0 content)))))
    ;; Write section
    (vector-push-extend 1 buffer)  ; Type section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-gc-struct-type (buffer fields)
  "Emit a GC struct type. Fields is ((valtype mutable-p) ...)"
  ;; struct type with subtype: 0x50 0x00 0x5F fields...
  ;; Simplified: 0x5F field_count fields...
  (vector-push-extend #x5F buffer)  ; struct
  (emit-leb128-unsigned (length fields) buffer)
  (dolist (field fields)
    (emit-valtype (first field) buffer)
    (vector-push-extend (if (second field) 1 0) buffer)))  ; mutability

(defun emit-gc-array-type (buffer elem-type mutable-p)
  "Emit a GC array type."
  ;; 0x5E elem_type mutability
  (vector-push-extend #x5E buffer)
  (emit-valtype elem-type buffer)
  (vector-push-extend (if mutable-p 1 0) buffer))

(defun emit-func-type-bytes (buffer params results)
  "Emit a function type."
  (vector-push-extend #x60 buffer)  ; func
  (emit-leb128-unsigned (length params) buffer)
  (dolist (p params)
    (emit-valtype p buffer))
  (emit-leb128-unsigned (length results) buffer)
  (dolist (r results)
    (emit-valtype r buffer)))

(defun emit-function-section (buffer functions)
  "Emit Function section.
   Lambda functions use predefined types 8-12 ($func_0/1/2/3/N).
   Regular functions use types 16+ (13 = exception tag, 14-15 = catch types)."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
        (regular-func-idx 16))  ; Start regular function types at 16
    ;; Number of functions
    (emit-leb128-unsigned (length functions) content)
    ;; Type index for each function
    (dolist (func functions)
      (if (is-lambda-function-p func)
          ;; Lambda functions: use predefined func types 8-12 based on arity
          ;; Arity is (length params) - 1 (excluding $closure param)
          (let* ((params (getf func :params))
                 (arity (1- (length params)))
                 (type-idx (case arity
                             (0 8)   ; $func_0
                             (1 9)   ; $func_1
                             (2 10)  ; $func_2
                             (3 11)  ; $func_3
                             (t 12)))) ; $func_N
            (emit-leb128-unsigned type-idx content))
          ;; Regular functions: use unique types starting at 14
          (progn
            (emit-leb128-unsigned regular-func-idx content)
            (incf regular-func-idx))))
    ;; Write section
    (vector-push-extend 3 buffer)  ; Function section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-tag-section (buffer)
  "Emit Tag section for exception handling.
   Defines $lisp-throw tag with (param anyref anyref) for (tag-symbol, value).
   Tag section has ID 13 and must come after function section."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of tags: 1
    (emit-leb128-unsigned 1 content)
    ;; Tag 0: $lisp-throw
    ;; Format: attribute (0 = exception) + type index
    ;; We need a function type for the tag parameters
    ;; Create inline function type: (anyref anyref) -> ()
    (vector-push-extend #x00 content)  ; attribute: exception
    ;; Type index - we need to add a function type for the tag
    ;; For now, use a type index that matches (anyref anyref) -> ()
    ;; We'll need to ensure this type exists in the type section
    ;; Using type index 14 (after the standard func types 8-12 and type 13 for main)
    ;; Actually, let's emit the function type inline using the block type encoding
    ;; Tag type uses the same encoding as block type
    ;; For 2 params (anyref, anyref) with no results, we need a type index
    ;; Let's use a dedicated type index - we'll add it to the type section
    (emit-leb128-unsigned 13 content)  ; Use type index 13 (first regular function type slot)
    ;; Write section
    (vector-push-extend 13 buffer)  ; Tag section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-global-section (buffer globals)
  "Emit Global section (Section ID 6).
   Globals: list of wasm-global structs with type, mutability, and init-expr."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of globals
    (emit-leb128-unsigned (length globals) content)
    ;; Each global: type + mutability + init_expr + end
    (dolist (global globals)
      (let ((type (clysm/backend/sections:wasm-global-type global))
            (mutability (clysm/backend/sections:wasm-global-mutability global))
            (init-expr (clysm/backend/sections:wasm-global-init-expr global)))
        ;; Global type: valtype
        (emit-valtype type content)
        ;; Mutability: 0=const, 1=mutable
        (vector-push-extend (if (eq mutability :const) 0 1) content)
        ;; Init expression
        (emit-instructions init-expr content)
        ;; End marker
        (vector-push-extend #x0B content)))
    ;; Write section: ID 6 + size + content
    (vector-push-extend 6 buffer)  ; Global section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-export-section (buffer exports)
  "Emit Export section."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of exports
    (emit-leb128-unsigned (length exports) content)
    ;; Each export
    (dolist (export exports)
      (destructuring-bind (name kind index) export
        ;; Name (length-prefixed UTF-8)
        (let ((name-bytes (babel:string-to-octets name :encoding :utf-8)))
          (emit-leb128-unsigned (length name-bytes) content)
          (loop for b across name-bytes do (vector-push-extend b content)))
        ;; Export kind
        (vector-push-extend (ecase kind (:func 0) (:table 1) (:memory 2) (:global 3)) content)
        ;; Index
        (emit-leb128-unsigned index content)))
    ;; Write section
    (vector-push-extend 7 buffer)  ; Export section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-element-section (buffer functions)
  "Emit Element section with declarative segment for lambda functions.
   This allows ref.func to reference lambda functions."
  ;; Collect lambda function indices
  (let ((lambda-indices '()))
    (loop for func in functions
          for i from 0
          do (when (is-lambda-function-p func)
               (push i lambda-indices)))
    ;; Only emit if there are lambda functions
    (when lambda-indices
      (setf lambda-indices (nreverse lambda-indices))
      (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
        ;; Number of element segments
        (emit-leb128-unsigned 1 content)
        ;; Declarative element segment (flag 0x03)
        ;; Format: 0x03 elemkind vec(funcidx)
        (vector-push-extend #x03 content)  ; declarative, uses indices
        (vector-push-extend #x00 content)  ; elemkind = funcref
        ;; Number of function indices
        (emit-leb128-unsigned (length lambda-indices) content)
        ;; Function indices
        (dolist (idx lambda-indices)
          (emit-leb128-unsigned idx content))
        ;; Write section
        (vector-push-extend 9 buffer)  ; Element section ID
        (emit-leb128-unsigned (length content) buffer)
        (loop for b across content do (vector-push-extend b buffer))))))

(defun emit-code-section (buffer functions)
  "Emit Code section."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of function bodies
    (emit-leb128-unsigned (length functions) content)
    ;; Each function body
    (dolist (func functions)
      (emit-function-body func content))
    ;; Write section
    (vector-push-extend 10 buffer)  ; Code section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-function-body (func buffer)
  "Emit a function body."
  (let ((body-content (make-array 0 :element-type '(unsigned-byte 8)
                                    :adjustable t :fill-pointer 0)))
    ;; Locals (grouped by type)
    (let ((locals (getf func :locals)))
      (emit-leb128-unsigned (length locals) body-content)
      (dolist (local locals)
        (emit-leb128-unsigned 1 body-content)
        (emit-valtype (second local) body-content)))
    ;; Instructions
    (let ((instrs (getf func :body)))
      (emit-instructions instrs body-content))
    ;; End
    (vector-push-extend #x0B body-content)
    ;; Write size + content
    (emit-leb128-unsigned (length body-content) buffer)
    (loop for b across body-content do (vector-push-extend b buffer))))

(defun emit-instructions (instrs buffer)
  "Emit a list of instructions."
  (dolist (instr instrs)
    (emit-instruction instr buffer)))

(defun emit-instruction (instr buffer)
  "Emit a single instruction."
  (cond
    ;; List instructions (with operands)
    ((consp instr)
     (let ((op (car instr)))
       (case op
         (:i32.const
          (vector-push-extend #x41 buffer)
          (emit-leb128-signed (cadr instr) buffer))
         (:i64.const
          (vector-push-extend #x42 buffer)
          (emit-leb128-signed (cadr instr) buffer))
         (:local.get
          (vector-push-extend #x20 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:local.set
          (vector-push-extend #x21 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:local.tee
          (vector-push-extend #x22 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:global.get
          (vector-push-extend #x23 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:call
          (vector-push-extend #x10 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:if
          (vector-push-extend #x04 buffer)
          ;; Block type
          (emit-block-type (cadr instr) buffer))
         (:ref.null
          ;; ref.null <heaptype>
          (vector-push-extend #xD0 buffer)
          (emit-heaptype (cadr instr) buffer))
         (:ref.cast
          ;; ref.cast <reftype> - cast reference to specific type
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x17 buffer)  ; ref.cast opcode
          (emit-heaptype (cadr instr) buffer))
         (:ref.func
          ;; ref.func <funcidx> - get function reference
          (vector-push-extend #xD2 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:struct.new
          ;; struct.new <typeidx> - create new struct
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x00 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:struct.new_default
          ;; struct.new_default <typeidx> - create struct with default values
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x01 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:struct.get
          ;; struct.get <typeidx> <fieldidx> - get field from struct
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x02 buffer)
          (emit-leb128-unsigned (cadr instr) buffer)   ; type index
          (emit-leb128-unsigned (caddr instr) buffer)) ; field index
         (:struct.set
          ;; struct.set <typeidx> <fieldidx> - set field in struct
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x05 buffer)
          (emit-leb128-unsigned (cadr instr) buffer)   ; type index
          (emit-leb128-unsigned (caddr instr) buffer)) ; field index
         (:call_ref
          ;; call_ref <typeidx> - call through function reference
          (vector-push-extend #x14 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:block
          ;; block <blocktype> - start block
          (vector-push-extend #x02 buffer)
          (emit-block-type (cadr instr) buffer))
         (:loop
          ;; loop <blocktype> - start loop
          (vector-push-extend #x03 buffer)
          (emit-block-type (cadr instr) buffer))
         (:br
          ;; br <labelidx> - branch
          (vector-push-extend #x0C buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:br_if
          ;; br_if <labelidx> - conditional branch
          (vector-push-extend #x0D buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:br_table
          ;; br_table <vec(labelidx)> <labelidx> - table branch
          (vector-push-extend #x0E buffer)
          (let ((labels (cdr instr)))
            ;; vec length (excluding default)
            (emit-leb128-unsigned (1- (length labels)) buffer)
            ;; label indices
            (dolist (label labels)
              (emit-leb128-unsigned label buffer))))
         ;; Exception handling instructions
         (:try_table
          ;; try_table <blocktype> <vec(catch)> - structured exception handling
          ;; Format: 0x1F blocktype vec(catch_clause)
          ;; catch_clause: 0x00 tagidx labelidx (catch)
          ;;            or 0x01 tagidx labelidx (catch_ref)
          ;;            or 0x02 labelidx (catch_all)
          ;;            or 0x03 labelidx (catch_all_ref)
          (vector-push-extend #x1F buffer)
          ;; Block type (result type)
          (emit-block-type (cadr instr) buffer)
          ;; Catch clauses - rest of args are catch clauses
          (let ((clauses (cddr instr)))
            (emit-leb128-unsigned (length clauses) buffer)
            (dolist (clause clauses)
              (ecase (car clause)
                (:catch
                 ;; (:catch tagidx labelidx)
                 (vector-push-extend #x00 buffer)
                 (emit-leb128-unsigned (cadr clause) buffer)
                 (emit-leb128-unsigned (caddr clause) buffer))
                (:catch_ref
                 ;; (:catch_ref tagidx labelidx)
                 (vector-push-extend #x01 buffer)
                 (emit-leb128-unsigned (cadr clause) buffer)
                 (emit-leb128-unsigned (caddr clause) buffer))
                (:catch_all
                 ;; (:catch_all labelidx)
                 (vector-push-extend #x02 buffer)
                 (emit-leb128-unsigned (cadr clause) buffer))
                (:catch_all_ref
                 ;; (:catch_all_ref labelidx)
                 (vector-push-extend #x03 buffer)
                 (emit-leb128-unsigned (cadr clause) buffer))))))
         (:throw
          ;; throw <tagidx> - throw exception
          (vector-push-extend #x08 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:throw_ref
          ;; throw_ref - rethrow exception from exnref
          (vector-push-extend #x0A buffer))
         (otherwise
          (error "Unknown compound instruction: ~A" op)))))
    ;; Simple keyword instructions
    ((keywordp instr)
     (case instr
       (:unreachable (vector-push-extend #x00 buffer))
       (:nop (vector-push-extend #x01 buffer))
       (:end (vector-push-extend #x0B buffer))
       (:else (vector-push-extend #x05 buffer))
       (:drop (vector-push-extend #x1A buffer))
       (:return (vector-push-extend #x0F buffer))
       ;; i32 operations
       (:i32.eqz (vector-push-extend #x45 buffer))
       (:i32.eq (vector-push-extend #x46 buffer))
       (:i32.ne (vector-push-extend #x47 buffer))
       (:i32.lt_s (vector-push-extend #x48 buffer))
       (:i32.gt_s (vector-push-extend #x4A buffer))
       (:i32.le_s (vector-push-extend #x4C buffer))
       (:i32.ge_s (vector-push-extend #x4E buffer))
       (:i32.add (vector-push-extend #x6A buffer))
       (:i32.sub (vector-push-extend #x6B buffer))
       (:i32.mul (vector-push-extend #x6C buffer))
       (:i32.div_s (vector-push-extend #x6D buffer))
       ;; Reference operations
       (:ref.eq (vector-push-extend #xD3 buffer))
       (:ref.is_null (vector-push-extend #xD1 buffer))
       ;; GC operations (0xFB prefix)
       (:ref.i31
        (vector-push-extend #xFB buffer)
        (vector-push-extend #x1C buffer))
       (:i31.get_s
        (vector-push-extend #xFB buffer)
        (vector-push-extend #x1D buffer))
       (:i31.get_u
        (vector-push-extend #xFB buffer)
        (vector-push-extend #x1E buffer))
       (otherwise
        (error "Unknown instruction: ~A" instr))))
    (t
     (error "Invalid instruction format: ~A" instr))))

(defun emit-block-type (block-type buffer)
  "Emit a block type."
  (cond
    ((null block-type)
     ;; Empty block type
     (vector-push-extend #x40 buffer))
    ((and (listp block-type) (eq (car block-type) :result))
     ;; Result type
     (emit-valtype (cadr block-type) buffer))
    ((and (listp block-type) (eq (car block-type) :type))
     ;; Type index for multi-value blocks
     ;; (:type N) means use type index N
     (emit-leb128-signed (cadr block-type) buffer))
    (t
     (error "Unknown block type: ~A" block-type))))

(defun emit-valtype (type buffer)
  "Emit a value type."
  (cond
    ;; Compound reference type: (:ref-null typeidx)
    ((and (listp type) (eq (car type) :ref-null))
     (vector-push-extend #x63 buffer)  ; refnull
     (emit-leb128-signed (cadr type) buffer))
    ;; Compound reference type: (:ref typeidx) - non-nullable
    ((and (listp type) (eq (car type) :ref))
     (vector-push-extend #x64 buffer)  ; ref
     (emit-leb128-signed (cadr type) buffer))
    ;; Simple types
    ((keywordp type)
     (vector-push-extend
      (ecase type
        (:i32 #x7F)
        (:i64 #x7E)
        (:f32 #x7D)
        (:f64 #x7C)
        (:i8 #x78)         ; packed i8 for arrays
        (:i16 #x77)        ; packed i16 for arrays
        (:anyref #x6E)     ; any heap type - parent of GC types
        (:externref #x6F)  ; external references - separate hierarchy
        (:funcref #x70)
        (:i31ref #x6C)
        (:eqref #x6D)
        (:exnref #x69))    ; exception reference (exception handling proposal)
      buffer))
    (t
     (error "Unknown value type: ~A" type))))

(defun emit-heaptype (type buffer)
  "Emit a heap type for ref.null, ref.cast, etc.
   Can be a keyword (abstract type) or a number (type index)."
  (cond
    ;; Numeric type index - emit as signed LEB128
    ((integerp type)
     (emit-leb128-signed type buffer))
    ;; Keyword type
    ((keywordp type)
     (vector-push-extend
      (ecase type
        (:none #x71)       ; bottom type for null
        (:nofunc #x73)     ; bottom type for func
        (:noextern #x72)   ; bottom type for extern
        (:any #x6E)
        (:eq #x6D)
        (:i31 #x6C)
        (:struct #x6B)
        (:array #x6A)
        (:func #x70)
        (:extern #x6F)
        (:exn #x69))       ; exception heap type
      buffer))
    (t
     (error "Unknown heap type: ~A" type))))

(defun emit-leb128-unsigned (value buffer)
  "Emit an unsigned LEB128 encoded value."
  (loop
    (let ((byte (logand value #x7F)))
      (setf value (ash value -7))
      (if (zerop value)
          (progn
            (vector-push-extend byte buffer)
            (return))
          (vector-push-extend (logior byte #x80) buffer)))))

(defun emit-leb128-signed (value buffer)
  "Emit a signed LEB128 encoded value."
  (let ((more t))
    (loop while more do
      (let ((byte (logand value #x7F)))
        (setf value (ash value -7))
        (let ((sign-bit (if (logbitp 6 byte) 1 0)))
          (if (or (and (zerop value) (zerop sign-bit))
                  (and (= value -1) (= sign-bit 1)))
              (progn
                (vector-push-extend byte buffer)
                (setf more nil))
              (vector-push-extend (logior byte #x80) buffer)))))))

;;; ============================================================
;;; WAT Output (for debugging)
;;; ============================================================

(defun emit-module-wat (module)
  "Emit a compiled module as WAT text."
  (with-output-to-string (s)
    (format s "(module~%")
    ;; Types
    (loop for func in (compiled-module-functions module)
          for i from 0
          do (format s "  (type $t~D (func" i)
             (let ((params (getf func :params)))
               (when params
                 (format s " (param")
                 (dolist (p params)
                   (format s " ~A" (keyword-to-wat-type (second p))))
                 (format s ")")))
             (let ((result (getf func :result)))
               (when result
                 (format s " (result ~A)" (keyword-to-wat-type result))))
             (format s "))~%"))
    ;; Functions
    (loop for func in (compiled-module-functions module)
          for i from 0
          do (format s "  (func $f~D (type $t~D)~%" i i)
             (format s "    ;; body~%")
             (format s "  )~%"))
    ;; Exports
    (dolist (export (compiled-module-exports module))
      (destructuring-bind (name kind index) export
        (format s "  (export ~S (~A $f~D))~%"
                name
                (ecase kind (:func "func") (:table "table") (:memory "memory") (:global "global"))
                index)))
    (format s ")~%")))

(defun keyword-to-wat-type (kw)
  "Convert a type keyword to WAT format."
  (ecase kw
    (:i32 "i32")
    (:i64 "i64")
    (:f32 "f32")
    (:f64 "f64")
    (:anyref "anyref")
    (:funcref "funcref")
    (:i31ref "i31ref")
    (:eqref "eqref")))

;;; ============================================================
;;; Convenience Function
;;; ============================================================

(defun compile-expression (expr)
  "Compile a single expression to Wasm instructions.
   Returns the instruction list (for debugging)."
  (let* ((ast (clysm/compiler/ast:parse-expr expr))
         (env (clysm/compiler/codegen/func-section:make-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
