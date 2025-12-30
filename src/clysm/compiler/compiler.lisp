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

   Feature 022-wasm-import-optimization: Analyzes I/O usage to conditionally
   emit Import section only when the compiled code uses I/O functions.

   Feature 043: Macro expansion is performed before compilation using the
   global macro registry. This handles LOOP, DO, DOLIST, etc.

   Phase 13D-3: Compile-time directives (in-package, defpackage, declaim,
   proclaim) are evaluated at compile-time and return nil (no Wasm output).

   Examples:
     (compile-to-wasm '(+ 1 2))      ; No Import section (no I/O)
     (compile-to-wasm '(print 42))   ; Has Import section (uses I/O)
     (compile-to-wasm '(+ 1 2) :output \"add.wasm\")
     (compile-to-wasm '(in-package :cl))  ; Returns nil (directive)"
  ;; Phase 13D-3: Check for compile-time directives BEFORE macro expansion
  ;; Directives are evaluated in the host environment and return nil
  (let ((processed-expr (compile-toplevel-form expr)))
    (when (null processed-expr)
      ;; Directive was processed, no Wasm output needed
      (return-from compile-to-wasm nil)))
  ;; Feature 043: Expand all macros before compilation
  (let* ((expanded-expr (clysm/compiler/transform/macro:macroexpand-all
                          (clysm/compiler/transform/macro:global-macro-registry)
                          expr))
         (uses-io (clysm/compiler/analyzer/io-usage:analyze-io-usage expanded-expr))
         (module (compile-to-module expanded-expr))
         (bytes (emit-module module :uses-io uses-io)))
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
   Useful for debugging.
   Feature 043: Macro expansion is performed before compilation.
   Phase 13D-3: Compile-time directives return nil (no WAT output)."
  ;; Phase 13D-3: Check for compile-time directives BEFORE macro expansion
  (let ((processed-expr (compile-toplevel-form expr)))
    (when (null processed-expr)
      ;; Directive was processed, no WAT output needed
      (return-from compile-to-wat nil)))
  ;; Feature 043: Expand all macros before compilation
  (let* ((expanded-expr (clysm/compiler/transform/macro:macroexpand-all
                          (clysm/compiler/transform/macro:global-macro-registry)
                          expr))
         (module (compile-to-module expanded-expr)))
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
    ;; Set up runtime globals (NIL, UNBOUND, mv-count, mv-buffer)
    (setf (compiled-module-globals module)
          (clysm/runtime/objects:generate-runtime-globals))
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
                          ;; Save result to local for type checking
                          (list (list :local.tee result-local-idx))
                          ;; Check for null (NIL)
                          '(:ref.is_null
                            (:if (:result :i32))
                            (:i32.const -2147483648))  ; NIL = MIN_INT32 (sentinel)
                          '(:else)
                          ;; Check if result is a fixnum (i31ref)
                          (list (list :local.get result-local-idx))
                          '((:ref.test :i31)
                            (:if (:result :i32)))
                          ;; Is fixnum: extract the value
                          (list (list :local.get result-local-idx))
                          '((:ref.cast :i31)
                            :i31.get_s)
                          '(:else)
                          ;; Not a fixnum (bignum, ratio, float, complex)
                          ;; For now, return a special sentinel to indicate non-fixnum
                          ;; TODO: Implement proper printing for bignums
                          ;; We'll use MIN_INT32 + 1 (-2147483647) as "non-fixnum" sentinel
                          '((:i32.const -2147483647))
                          '(:end)
                          '(:end))))))

;;; ============================================================
;;; Module Binary Emission
;;; ============================================================

(defun get-ffi-import-count ()
  "Get the count of FFI imports registered.
   001-numeric-functions: Helper for function index offsetting."
  (let ((ffi-pkg (find-package :clysm/ffi)))
    (if ffi-pkg
        (let ((count-fn (find-symbol "GET-FFI-IMPORT-COUNT" ffi-pkg)))
          (if count-fn (funcall count-fn) 0))
        0)))

(defun emit-module (module &key uses-io)
  "Emit a compiled module as a Wasm binary byte vector.
   USES-IO controls whether Import section is emitted (Feature 022)."
  (let ((buffer (make-array 0 :element-type '(unsigned-byte 8)
                              :adjustable t :fill-pointer 0))
        (functions (compiled-module-functions module))
        ;; 001-numeric-functions: Get import count for function index offsetting
        ;; In Wasm, imports get indices 0 to N-1, local functions get N onwards
        (import-offset (get-ffi-import-count)))
    ;; Module header (magic + version)
    (emit-header buffer)
    ;; Type section
    (emit-type-section buffer functions)
    ;; Import section (T025: FFI imports come after Type, before Function)
    ;; Per Wasm spec, section order: Type(1), Import(2), Function(3), ...
    ;; 001-numeric-functions: Always check for FFI imports (not just I/O)
    ;; Math functions (sin, cos, exp, etc.) are FFI imports but not I/O
    ;; Pass function count for correct type index calculation
    (emit-import-section-if-needed buffer functions)
    ;; Function section
    (emit-function-section buffer functions)
    ;; Tag section (for exception handling)
    (emit-tag-section buffer)
    ;; Global section
    (when (compiled-module-globals module)
      (emit-global-section buffer (compiled-module-globals module)))
    ;; Export section (T035: includes both regular exports and FFI exports)
    ;; 001-numeric-functions: Offset function indices by import count
    (let ((all-exports (append (compiled-module-exports module)
                               (or (collect-ffi-exports-for-section) '()))))
      (emit-export-section buffer all-exports import-offset))
    ;; Element section (for ref.func declarations)
    ;; 001-numeric-functions: Offset function indices by import count
    (emit-element-section buffer functions import-offset)
    ;; Code section
    ;; 001-numeric-functions: Pass import-offset for :call instruction handling
    (emit-code-section buffer functions import-offset)
    ;; Return as simple vector
    (coerce buffer '(simple-array (unsigned-byte 8) (*)))))

(defun emit-import-section-if-needed (buffer functions)
  "Emit Import section if there are FFI imports registered.
   T025: Integrate FFI imports into compiler's module generation.
   001-numeric-functions: Pass regular function count for correct type indexing.
   Uses runtime symbol lookup to avoid compile-time dependency on FFI package."
  (let ((ffi-pkg (find-package :clysm/ffi)))
    (when ffi-pkg
      (let ((env-sym (find-symbol "*FFI-ENVIRONMENT*" ffi-pkg))
            (count-fn (find-symbol "GET-FFI-IMPORT-COUNT" ffi-pkg))
            (assign-fn (find-symbol "ASSIGN-IMPORT-INDICES" ffi-pkg))
            (emit-fn (find-symbol "EMIT-FFI-IMPORTS" ffi-pkg)))
        (when (and env-sym (boundp env-sym) count-fn assign-fn emit-fn)
          (let ((env (symbol-value env-sym))
                ;; 001-numeric-functions: Count non-lambda functions for type index base
                (regular-func-count (count-if-not #'is-lambda-function-p functions)))
            (when (> (funcall count-fn) 0)
              ;; Assign type indices to imports before emitting
              ;; 001-numeric-functions: FFI types come after regular function types
              (funcall assign-fn env regular-func-count)
              ;; Emit the import section
              (funcall emit-fn env buffer))))))))

(defun collect-ffi-exports-for-section ()
  "Collect FFI exports and return them in the format expected by emit-export-section.
   T035: Integrate FFI exports into compiler's module generation.
   Uses runtime symbol lookup to avoid compile-time dependency on FFI package.
   Returns a list of (name kind index) entries."
  (let ((ffi-pkg (find-package :clysm/ffi)))
    (when ffi-pkg
      (let ((env-sym (find-symbol "*FFI-ENVIRONMENT*" ffi-pkg))
            (count-fn (find-symbol "GET-FFI-EXPORT-COUNT" ffi-pkg))
            (assign-fn (find-symbol "ASSIGN-EXPORT-INDICES" ffi-pkg))
            (collect-fn (find-symbol "COLLECT-FFI-EXPORTS" ffi-pkg))
            (ed-export-name (find-symbol "ED-EXPORT-NAME" ffi-pkg))
            (ed-wrapper-func-index (find-symbol "ED-WRAPPER-FUNC-INDEX" ffi-pkg)))
        (when (and env-sym (boundp env-sym) count-fn assign-fn collect-fn
                   ed-export-name ed-wrapper-func-index)
          (let ((env (symbol-value env-sym)))
            (when (> (funcall count-fn) 0)
              ;; Assign wrapper function indices
              (funcall assign-fn env)
              ;; Collect exports and convert to section format
              (let ((exports (funcall collect-fn env)))
                (mapcar (lambda (decl)
                          (list (funcall ed-export-name decl)
                                :func
                                (or (funcall ed-wrapper-func-index decl) 0)))
                        exports)))))))))

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

(defun collect-ffi-type-count ()
  "Get the number of FFI types that need to be added to the Type section.
   T058: Integration with Type section - count unique FFI function types."
  (let ((ffi-pkg (find-package :clysm/ffi)))
    (if ffi-pkg
        (let ((collect-types-fn (find-symbol "COLLECT-FFI-TYPES" ffi-pkg))
              (env-sym (find-symbol "*FFI-ENVIRONMENT*" ffi-pkg)))
          (if (and collect-types-fn env-sym (boundp env-sym))
              (length (funcall collect-types-fn (symbol-value env-sym)))
              0))
        0)))

(defun emit-ffi-types-to-section (content)
  "Emit FFI function types to the Type section content buffer.
   T058: Adds function types for FFI imports/exports after regular types.
   Each type is emitted as: 0x60 params results (standard func type format)."
  (let ((ffi-pkg (find-package :clysm/ffi)))
    (when ffi-pkg
      (let ((collect-types-fn (find-symbol "COLLECT-FFI-TYPES" ffi-pkg))
            (env-sym (find-symbol "*FFI-ENVIRONMENT*" ffi-pkg))
            (marshal-type-fn (find-symbol "MARSHAL-TYPE-TO-WASM-TYPE" ffi-pkg)))
        (when (and collect-types-fn env-sym (boundp env-sym) marshal-type-fn)
          (let ((types (funcall collect-types-fn (symbol-value env-sym))))
            (dolist (type-def types)
              ;; type-def is (:func (params...) (results...))
              (when (and (listp type-def) (eq (first type-def) :func))
                (let ((params (second type-def))
                      (results (third type-def)))
                  ;; Emit function type indicator
                  (vector-push-extend #x60 content)
                  ;; Emit params
                  (emit-leb128-unsigned (length params) content)
                  (dolist (p params)
                    (emit-ffi-wasm-valtype p content))
                  ;; Emit results
                  (emit-leb128-unsigned (length results) content)
                  (dolist (r results)
                    (emit-ffi-wasm-valtype r content)))))))))))

(defun emit-ffi-wasm-valtype (type content)
  "Emit an FFI Wasm value type to buffer.
   T058: Converts FFI Wasm type symbols to Wasm binary encoding.
   Handles symbols from any package by comparing symbol names."
  (vector-push-extend
   (let ((name (if (symbolp type) (symbol-name type) (string type))))
     (cond
       ((string-equal name "I31REF") #x6C)
       ((string-equal name "I32") #x7F)
       ((string-equal name "I64") #x7E)
       ((string-equal name "F32") #x7D)
       ((string-equal name "F64") #x7C)
       ((string-equal name "ANYREF") #x6E)
       ((string-equal name "EXTERNREF") #x6F)
       ((string-equal name "FUNCREF") #x70)
       (t (error "Unknown FFI Wasm type: ~A" type))))
   content))

(defun emit-type-section (buffer functions)
  "Emit Type section with GC types and function types.
   Layout:
   - Types 0-5: GC types ($nil, $unbound, $cons, $symbol, $string, $closure)
   - Type 6-7: Reserved
   - Types 8-12: Function types ($func_0, $func_1, $func_2, $func_3, $func_N)
   - Type 13: Binding frame for special variables
   - Type 14: $bignum - arbitrary precision integer (010-numeric-tower)
   - Type 15: $ratio - exact rational number (010-numeric-tower)
   - Type 16: $float - IEEE 754 double-precision (010-numeric-tower)
   - Type 17: $complex - complex number (010-numeric-tower)
   - Type 18: $limb_array - array of i32 for bignum limbs (010-numeric-tower)
   - Type 19: $tag_lisp_throw - (anyref, anyref) -> () for exception tag
   - Type 20: $catch_handler - (anyref, anyref) -> anyref (unused, kept for compatibility)
   - Type 21: $catch_result - () -> (anyref, anyref) for catch handler block result
   - Type 22: $mv_array - (array (mut anyref)) for multiple values buffer (025-multiple-values)
   - Types 23+: Regular (non-lambda) function types + FFI function types (T058)"
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
        ;; Count non-lambda functions (lambdas reuse types 8-12)
        (regular-func-count (count-if-not #'is-lambda-function-p functions))
        ;; T058: Count FFI function types
        (ffi-type-count (collect-ffi-type-count)))
    ;; Number of types = 23 (base types) + regular functions + FFI types
    (emit-leb128-unsigned (+ 23 regular-func-count ffi-type-count) content)
    ;; Type 0: $nil (empty struct)
    (emit-gc-struct-type content '())
    ;; Type 1: $unbound (empty struct)
    (emit-gc-struct-type content '())
    ;; Type 2: $cons (car, cdr)
    (emit-gc-struct-type content '((:anyref t) (:anyref t)))
    ;; Type 3: $symbol (name, value, function, plist)
    (emit-gc-struct-type content '((:anyref nil) (:anyref t) (:anyref t) (:anyref t)))
    ;; Type 4: $string (array of i8) - mutable to support make-string
    (emit-gc-array-type content :i8 t)
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
    ;; Type 13: $binding_frame - (symbol-ref, old_value, prev) for dynamic bindings
    ;; Note: Using anyref for symbol-ref since we can't forward-reference types
    (emit-gc-struct-type content '((:anyref nil) (:anyref nil) (:anyref nil)))
    ;; Type 14: $bignum - (sign:i32, limbs:ref $limb_array) (010-numeric-tower)
    ;; Note: limbs field references type 18, but since that's forward, use anyref
    (emit-gc-struct-type content '((:i32 nil) (:anyref nil)))
    ;; Type 15: $ratio - (numerator:anyref, denominator:anyref) (010-numeric-tower)
    (emit-gc-struct-type content '((:anyref nil) (:anyref nil)))
    ;; Type 16: $float - (value:f64) (010-numeric-tower)
    (emit-gc-struct-type content '((:f64 nil)))
    ;; Type 17: $complex - (real:anyref, imag:anyref) (010-numeric-tower)
    (emit-gc-struct-type content '((:anyref nil) (:anyref nil)))
    ;; Type 18: $limb_array - (array (mut i32)) for bignum limbs (010-numeric-tower)
    (emit-gc-array-type content :i32 t)
    ;; Type 19: $tag_lisp_throw - (anyref, anyref) -> () for exception tag
    (emit-func-type-bytes content '(:anyref :anyref) '())
    ;; Type 20: $catch_handler - (anyref, anyref) -> anyref (unused, kept for compatibility)
    (emit-func-type-bytes content '(:anyref :anyref) '(:anyref))
    ;; Type 21: $catch_result - () -> (anyref, anyref) for catch handler block result
    (emit-func-type-bytes content '() '(:anyref :anyref))
    ;; Type 22: $mv_array - (array (mut anyref)) for multiple values buffer (025-multiple-values)
    (emit-gc-array-type content :anyref t)
    ;; Regular function types (indices 23+) - skip lambdas (they use types 8-12)
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
    ;; T058: FFI function types (after regular function types)
    (emit-ffi-types-to-section content)
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
   Regular functions use types 23+ (13 = binding_frame, 14-18 = numeric tower, 19-21 = exception types, 22 = mv_array)."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0))
        (regular-func-idx 23))  ; Start regular function types at 23 (after mv_array)
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
          ;; Regular functions: use unique types starting at 23
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
    ;; Type index 19 is $tag_lisp_throw - (anyref, anyref) -> ()
    (vector-push-extend #x00 content)  ; attribute: exception
    (emit-leb128-unsigned 19 content)  ; Use type index 19 ($tag_lisp_throw)
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

(defun emit-export-section (buffer exports &optional (import-offset 0))
  "Emit Export section.
   IMPORT-OFFSET is added to function indices to account for FFI imports.
   001-numeric-functions: FFI imports get indices 0 to N-1, local functions get N onwards."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of exports
    (emit-leb128-unsigned (length exports) content)
    ;; Each export
    (dolist (export exports)
      (destructuring-bind (name kind index) export
        ;; Name (length-prefixed UTF-8)
        (let ((name-bytes (clysm/lib/utf8:string-to-utf8-octets name)))
          (emit-leb128-unsigned (length name-bytes) content)
          (loop for b across name-bytes do (vector-push-extend b content)))
        ;; Export kind
        (vector-push-extend (ecase kind (:func 0) (:table 1) (:memory 2) (:global 3)) content)
        ;; Index - offset function indices by import count
        (let ((final-index (if (eq kind :func)
                               (+ index import-offset)
                               index)))
          (emit-leb128-unsigned final-index content))))
    ;; Write section
    (vector-push-extend 7 buffer)  ; Export section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-element-section (buffer functions &optional (import-offset 0))
  "Emit Element section with declarative segment for lambda functions.
   This allows ref.func to reference lambda functions.
   IMPORT-OFFSET is added to function indices to account for FFI imports.
   001-numeric-functions: FFI imports get indices 0 to N-1, local functions get N onwards."
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
        ;; Function indices - offset by import count
        (dolist (idx lambda-indices)
          (emit-leb128-unsigned (+ idx import-offset) content))
        ;; Write section
        (vector-push-extend 9 buffer)  ; Element section ID
        (emit-leb128-unsigned (length content) buffer)
        (loop for b across content do (vector-push-extend b buffer))))))

(defun emit-code-section (buffer functions &optional (import-offset 0))
  "Emit Code section.
   IMPORT-OFFSET is added to local function :call indices.
   001-numeric-functions: Distinguishes FFI :call-import from local :call."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of function bodies
    (emit-leb128-unsigned (length functions) content)
    ;; Each function body
    (dolist (func functions)
      (emit-function-body func content import-offset))
    ;; Write section
    (vector-push-extend 10 buffer)  ; Code section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-function-body (func buffer &optional (import-offset 0))
  "Emit a function body.
   IMPORT-OFFSET is added to local function :call indices."
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
      (emit-instructions instrs body-content import-offset))
    ;; End
    (vector-push-extend #x0B body-content)
    ;; Write size + content
    (emit-leb128-unsigned (length body-content) buffer)
    (loop for b across body-content do (vector-push-extend b buffer))))

(defun emit-instructions (instrs buffer &optional (import-offset 0))
  "Emit a list of instructions after resolving symbolic labels.
   IMPORT-OFFSET is added to local function :call indices."
  (let ((resolved (resolve-labels instrs)))
    (dolist (instr resolved)
      (emit-instruction instr buffer import-offset))))

;;; ============================================================
;;; Label Resolution
;;; ============================================================
;;;
;;; Wasm uses relative branch indices - an index of 0 means the innermost
;;; enclosing block/loop, 1 means the next outer, etc.
;;;
;;; Instructions like (:block $label type) and (:loop $label type) introduce
;;; labels. (:br $label) and (:br_if $label) reference them.
;;;
;;; This pass converts symbolic labels to numeric indices.

(defun resolve-labels (instrs)
  "Resolve symbolic labels in instructions to numeric branch indices."
  (let ((label-stack '()))  ; Stack of (label . depth-introduced)
    (resolve-labels-in-list instrs label-stack 0)))

(defun resolve-labels-in-list (instrs label-stack depth)
  "Resolve labels in a flat list of instructions."
  (let ((result '())
        (current-depth depth)
        (current-labels label-stack))
    (dolist (instr instrs)
      (cond
        ;; Block with label: (:block $label type)
        ((and (consp instr) (eq (car instr) :block)
              (symbolp (cadr instr))
              (not (null (cadr instr))))
         (let* ((label (cadr instr))
                (block-type (caddr instr)))
           ;; Push label with current depth
           (push (cons label current-depth) current-labels)
           (incf current-depth)
           ;; Emit block with just the type
           (push (list :block block-type) result)))
        ;; Loop with label: (:loop $label type)
        ((and (consp instr) (eq (car instr) :loop)
              (symbolp (cadr instr))
              (not (null (cadr instr))))
         (let* ((label (cadr instr))
                (loop-type (caddr instr)))
           ;; Push label with current depth
           (push (cons label current-depth) current-labels)
           (incf current-depth)
           ;; Emit loop with just the type
           (push (list :loop loop-type) result)))
        ;; Block/loop without label (just type)
        ((and (consp instr) (member (car instr) '(:block :loop))
              (or (null (cadr instr))
                  (and (listp (cadr instr)) (eq (caar (cdr instr)) :result))
                  (and (listp (cadr instr)) (eq (caar (cdr instr)) :type))))
         (incf current-depth)
         (push instr result))
        ;; If also introduces a block scope
        ((and (consp instr) (eq (car instr) :if))
         (incf current-depth)
         (push instr result))
        ;; try_table also introduces a block scope
        ((and (consp instr) (eq (car instr) :try_table))
         (incf current-depth)
         (push instr result))
        ;; End decreases depth
        ((eq instr :end)
         (decf current-depth)
         ;; Pop labels that were at this depth
         (loop while (and current-labels
                          (= (cdar current-labels) current-depth))
               do (pop current-labels))
         (push instr result))
        ;; Branch with symbolic label: (:br $label)
        ((and (consp instr) (eq (car instr) :br)
              (symbolp (cadr instr)))
         (let* ((label (cadr instr))
                (entry (assoc label current-labels)))
           (if entry
               ;; Calculate relative index
               (let ((target-depth (cdr entry)))
                 (push (list :br (- current-depth target-depth 1)) result))
               (error "Unknown branch label: ~A" label))))
        ;; Branch-if with symbolic label: (:br_if $label)
        ((and (consp instr) (eq (car instr) :br_if)
              (symbolp (cadr instr)))
         (let* ((label (cadr instr))
                (entry (assoc label current-labels)))
           (if entry
               (let ((target-depth (cdr entry)))
                 (push (list :br_if (- current-depth target-depth 1)) result))
               (error "Unknown branch label: ~A" label))))
        ;; Everything else passes through unchanged
        (t
         (push instr result))))
    (nreverse result)))

(defun emit-instruction (instr buffer &optional (import-offset 0))
  "Emit a single instruction.
   IMPORT-OFFSET is added to local function indices for :call, :return_call, :ref.func.
   001-numeric-functions: :call-import is used for FFI calls (no offset)."
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
         ;; Float constants (010-numeric-tower)
         (:f32.const
          (vector-push-extend #x43 buffer)
          (emit-f32 (cadr instr) buffer))
         (:f64.const
          (vector-push-extend #x44 buffer)
          (emit-f64 (cadr instr) buffer))
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
         (:global.set
          (vector-push-extend #x24 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:call
          ;; Call to local function - add import-offset
          ;; 001-numeric-functions: Local functions start at index import-offset
          (vector-push-extend #x10 buffer)
          (emit-leb128-unsigned (+ (cadr instr) import-offset) buffer))
         (:call-import
          ;; Call to FFI import - no offset needed
          ;; 001-numeric-functions: Imports use indices 0 to N-1 directly
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
          ;; Uses nullable variant (0xFB 0x16) to handle nil inputs
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x16 buffer)  ; ref.cast nullable opcode
          (emit-heaptype-for-gc (cadr instr) buffer))
         (:ref.test
          ;; ref.test <reftype> - test if reference is of type
          ;; Uses nullable variant (0xFB 0x14) to handle nil inputs
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x14 buffer)  ; ref.test nullable opcode
          (emit-heaptype-for-gc (cadr instr) buffer))
         (:ref.func
          ;; ref.func <funcidx> - get function reference
          ;; 001-numeric-functions: Add import-offset for local functions
          (vector-push-extend #xD2 buffer)
          (emit-leb128-unsigned (+ (cadr instr) import-offset) buffer))
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
         (:array.new_fixed
          ;; array.new_fixed <typeidx> <n> - create array with n elements from stack
          ;; 008-character-string: Used for string literals
          ;; Opcode: 0xFB 0x08
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x08 buffer)
          (emit-leb128-unsigned (cadr instr) buffer)   ; type index
          (emit-leb128-unsigned (caddr instr) buffer)) ; n elements
         (:array.new_default
          ;; array.new_default <typeidx> - create array with default values
          ;; 008-character-string: Used for make-string
          ;; Stack: [size:i32] -> [arrayref]
          ;; Opcode: 0xFB 0x07
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x07 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))  ; type index
         (:array.set
          ;; array.set <typeidx> - set element in array
          ;; 008-character-string: Used for make-string fill
          ;; Stack: [arrayref, idx:i32, value] -> []
          ;; Opcode: 0xFB 0x0E
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x0E buffer)
          (emit-leb128-unsigned (cadr instr) buffer))  ; type index
         (:array.len
          ;; array.len - get length of array
          ;; 008-character-string: Used for string length
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x0F buffer))
         (:array.get
          ;; array.get <typeidx> - get element from array (for anyref)
          ;; 025-multiple-values: Used for accessing mv-buffer
          ;; Stack: [arrayref, idx:i32] -> [value]
          ;; Opcode: 0xFB 0x0B
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x0B buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
        (:array.get_u
          ;; array.get_u <typeidx> - get unsigned element from array
          ;; 008-character-string: Used for char/schar
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x0D buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:array.copy
          ;; array.copy <dst_typeidx> <src_typeidx> - copy elements between arrays
          ;; 001-ansi-sequence-operations: Used for subseq, copy-seq, concatenate
          ;; Stack: [dstarray, dstoff:i32, srcarray, srcoff:i32, len:i32] -> []
          ;; Opcode: 0xFB 0x11
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x11 buffer)
          (emit-leb128-unsigned (cadr instr) buffer)   ; destination type index
          (emit-leb128-unsigned (caddr instr) buffer)) ; source type index
         (:array.new
          ;; array.new <typeidx> - create array with initial value
          ;; 001-ansi-sequence-operations: Used for make-string, make-array :initial-element
          ;; Stack: [value, size:i32] -> [arrayref]
          ;; Opcode: 0xFB 0x06
          (vector-push-extend #xFB buffer)
          (vector-push-extend #x06 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))  ; type index
         (:call_ref
          ;; call_ref <typeidx> - call through function reference
          (vector-push-extend #x14 buffer)
          (emit-leb128-unsigned (cadr instr) buffer))
         (:return_call
          ;; return_call <funcidx> - tail call to known function (TCO)
          ;; 001-numeric-functions: Add import-offset for local functions
          ;; Opcode: 0x12
          (vector-push-extend #x12 buffer)
          (emit-leb128-unsigned (+ (cadr instr) import-offset) buffer))
         (:return_call_ref
          ;; return_call_ref <typeidx> - tail call through function reference (TCO)
          ;; Opcode: 0x15
          (vector-push-extend #x15 buffer)
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
       (:i32.lt_u (vector-push-extend #x49 buffer))
       (:i32.gt_s (vector-push-extend #x4A buffer))
       (:i32.gt_u (vector-push-extend #x4B buffer))
       (:i32.le_s (vector-push-extend #x4C buffer))
       (:i32.le_u (vector-push-extend #x4D buffer))
       (:i32.ge_s (vector-push-extend #x4E buffer))
       (:i32.ge_u (vector-push-extend #x4F buffer))
       (:i32.add (vector-push-extend #x6A buffer))
       (:i32.sub (vector-push-extend #x6B buffer))
       (:i32.mul (vector-push-extend #x6C buffer))
       (:i32.div_s (vector-push-extend #x6D buffer))
       (:i32.div_u (vector-push-extend #x6E buffer))
       (:i32.rem_s (vector-push-extend #x6F buffer))
       (:i32.rem_u (vector-push-extend #x70 buffer))
       (:i32.and (vector-push-extend #x71 buffer))
       (:i32.or (vector-push-extend #x72 buffer))
       (:i32.xor (vector-push-extend #x73 buffer))
       (:i32.shl (vector-push-extend #x74 buffer))
       (:i32.shr_s (vector-push-extend #x75 buffer))
       (:i32.shr_u (vector-push-extend #x76 buffer))
       ;; i32 unary operations (001-numeric-functions)
       (:i32.clz (vector-push-extend #x67 buffer))
       (:i32.ctz (vector-push-extend #x68 buffer))
       (:i32.popcnt (vector-push-extend #x69 buffer))
       ;; i64 operations (024-equality-predicates)
       (:i64.eq (vector-push-extend #x51 buffer))
       (:i64.ne (vector-push-extend #x52 buffer))
       ;; f64 operations (023-type-predicates)
       (:f64.eq (vector-push-extend #x61 buffer))
       (:f64.ne (vector-push-extend #x62 buffer))
       (:f64.lt (vector-push-extend #x63 buffer))
       (:f64.gt (vector-push-extend #x64 buffer))
       (:f64.le (vector-push-extend #x65 buffer))
       (:f64.ge (vector-push-extend #x66 buffer))
       ;; f64 conversion (024-equality-predicates)
       (:f64.convert_i32_s (vector-push-extend #xB7 buffer))
       (:f64.convert_i32_u (vector-push-extend #xB8 buffer))
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
   Can be a keyword (abstract type), a number (type index), or (ref N) for concrete types."
  (cond
    ;; Numeric type index - emit as signed LEB128
    ((integerp type)
     (emit-leb128-signed type buffer))
    ;; List form: (ref <type-index>) for concrete reference types
    ((and (listp type) (eq (first type) :ref))
     (let ((type-idx (second type)))
       (emit-leb128-signed type-idx buffer)))
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

(defun emit-heaptype-for-gc (type buffer)
  "Emit a heaptype for ref.cast/ref.test GC instructions.
   Extracts the heaptype from reftype forms like (:ref 2)."
  (cond
    ;; List form: (:ref <type-idx>) - extract type index
    ((and (listp type) (eq (first type) :ref))
     (emit-leb128-signed (second type) buffer))
    ;; List form: (:ref-null <type-idx>) - extract type index
    ((and (listp type) (eq (first type) :ref-null))
     (emit-leb128-signed (second type) buffer))
    ;; Abstract heap types (i31, eq, any, etc.)
    ((keywordp type)
     (emit-heaptype type buffer))
    ;; Numeric type index
    ((integerp type)
     (emit-leb128-signed type buffer))
    (t
     (error "Unknown ref type: ~A" type))))

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
;;; Float Encoding (010-numeric-tower)
;;; ============================================================

(defun emit-f32 (value buffer)
  "Emit an IEEE 754 single-precision (32-bit) float in little-endian."
  (let* ((float32 (coerce value 'single-float))
         (bits (sb-kernel:single-float-bits float32)))
    (vector-push-extend (logand bits #xFF) buffer)
    (vector-push-extend (logand (ash bits -8) #xFF) buffer)
    (vector-push-extend (logand (ash bits -16) #xFF) buffer)
    (vector-push-extend (logand (ash bits -24) #xFF) buffer)))

(defun emit-f64 (value buffer)
  "Emit an IEEE 754 double-precision (64-bit) float in little-endian."
  (let* ((float64 (coerce value 'double-float))
         (bits (sb-kernel:double-float-bits float64)))
    (vector-push-extend (logand bits #xFF) buffer)
    (vector-push-extend (logand (ash bits -8) #xFF) buffer)
    (vector-push-extend (logand (ash bits -16) #xFF) buffer)
    (vector-push-extend (logand (ash bits -24) #xFF) buffer)
    (vector-push-extend (logand (ash bits -32) #xFF) buffer)
    (vector-push-extend (logand (ash bits -40) #xFF) buffer)
    (vector-push-extend (logand (ash bits -48) #xFF) buffer)
    (vector-push-extend (logand (ash bits -56) #xFF) buffer)))

;;; ============================================================
;;; WAT Output (for debugging)
;;; ============================================================

(defun emit-instr-wat (stream instr indent)
  "Emit a single instruction in WAT format."
  (let ((spaces (make-string indent :initial-element #\Space)))
    (cond
      ;; Keyword (simple instruction like :ref.is_null)
      ((keywordp instr)
       (format stream "~A~A~%" spaces (string-downcase (symbol-name instr))))
      ;; List starting with keyword (instruction with operands)
      ((and (listp instr) (keywordp (first instr)))
       (let ((op (first instr))
             (args (rest instr)))
         (cond
           ;; Block instructions (if, block, loop)
           ((member op '(:if :block :loop))
            (format stream "~A(~A" spaces (string-downcase (symbol-name op)))
            (when (and args (listp (first args)) (eq (first (first args)) :result))
              (format stream " (result ~A)" (keyword-to-wat-type (second (first args))))
              (setf args (rest args)))
            (format stream "~%")
            (dolist (sub-instr args)
              (emit-instr-wat stream sub-instr (+ indent 2)))
            (format stream "~A)~%" spaces))
           ;; else is special
           ((eq op :else)
            (format stream "~Aelse~%" spaces))
           ;; end is special
           ((eq op :end)
            (format stream "~Aend~%" spaces))
           ;; ref.cast with type argument
           ((eq op :ref.cast)
            (if (listp (first args))
                (format stream "~Aref.cast (ref ~A)~%" spaces (second (first args)))
                ;; :i31 should be emitted as i31ref for WAT text format
                (let ((type-arg (first args)))
                  (format stream "~Aref.cast ~A~%" spaces
                          (if (eq type-arg :i31) "i31ref" type-arg)))))
           ;; ref.test with type argument
           ((eq op :ref.test)
            (if (listp (first args))
                (format stream "~Aref.test (ref ~A)~%" spaces (second (first args)))
                ;; :i31 should be emitted as i31ref for WAT text format
                (let ((type-arg (first args)))
                  (format stream "~Aref.test ~A~%" spaces
                          (if (eq type-arg :i31) "i31ref" type-arg)))))
           ;; ref.null
           ((eq op :ref.null)
            (format stream "~Aref.null ~A~%" spaces (string-downcase (symbol-name (first args)))))
           ;; Regular instruction with operands
           (t
            (format stream "~A~A" spaces (string-downcase (symbol-name op)))
            (dolist (arg args)
              (format stream " ~A" arg))
            (format stream "~%")))))
      ;; Symbol (like :else or :end without parens)
      ((symbolp instr)
       (format stream "~A~A~%" spaces (string-downcase (symbol-name instr))))
      ;; Shouldn't happen
      (t
       (format stream "~A;; unknown: ~S~%" spaces instr)))))

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
             ;; Emit locals
             (let ((locals (getf func :locals)))
               (when locals
                 (dolist (local locals)
                   (format s "    (local ~A)~%" (keyword-to-wat-type (second local))))))
             ;; Emit body instructions
             (let ((body (getf func :body)))
               (dolist (instr body)
                 (emit-instr-wat s instr 4)))
             (format s "  )~%"))
    ;; Exports
    (dolist (export (compiled-module-exports module))
      (destructuring-bind (name kind index) export
        (format s "  (export ~S (~A $f~D))~%"
                name
                (ecase kind (:func "func") (:table "table") (:memory "memory") (:global "global"))
                index)))
    (format s ")~%")))

(defun keyword-to-wat-type (type)
  "Convert a type to WAT format.
   Handles both keyword types and list-form reference types."
  (cond
    ;; List-form reference types: (:ref-null 2) -> (ref null 2)
    ((and (listp type) (eq (first type) :ref-null))
     (format nil "(ref null ~A)" (second type)))
    ;; List-form non-null reference types: (:ref 2) -> (ref 2)
    ((and (listp type) (eq (first type) :ref))
     (format nil "(ref ~A)" (second type)))
    ;; Simple keyword types
    ((keywordp type)
     (ecase type
       (:i32 "i32")
       (:i64 "i64")
       (:f32 "f32")
       (:f64 "f64")
       (:anyref "anyref")
       (:funcref "funcref")
       (:i31ref "i31ref")
       (:eqref "eqref")))
    (t (error "Unknown type: ~A" type))))

;;; ============================================================
;;; Convenience Function
;;; ============================================================

(defun compile-expression (expr)
  "Compile a single expression to Wasm instructions.
   Returns the instruction list (for debugging)."
  (let* ((ast (clysm/compiler/ast:parse-expr expr))
         (env (clysm/compiler/codegen/func-section:make-env)))
    (clysm/compiler/codegen/func-section:compile-to-instructions ast env)))
