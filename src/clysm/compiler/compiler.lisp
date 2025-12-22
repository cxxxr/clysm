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
  ;; Parse expression to AST
  (let* ((ast (clysm/compiler/ast:parse-expr expr))
         (env (clysm/compiler/codegen/func-section:make-env))
         (module (make-compiled-module))
         (functions '())
         (defuns '())
         (main-forms '()))
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
      (setf (compiled-module-main-func-idx module) 0)
      (setf (compiled-module-exports module)
            (list (list "_start" :func 0))))
    module))

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
                          collect (list (gensym "local") :anyref))
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
                              :adjustable t :fill-pointer 0)))
    ;; Module header (magic + version)
    (emit-header buffer)
    ;; Type section
    (emit-type-section buffer (compiled-module-functions module))
    ;; Function section
    (emit-function-section buffer (compiled-module-functions module))
    ;; Global section
    (when (compiled-module-globals module)
      (emit-global-section buffer (compiled-module-globals module)))
    ;; Export section
    (emit-export-section buffer (compiled-module-exports module))
    ;; Code section
    (emit-code-section buffer (compiled-module-functions module))
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

(defun emit-type-section (buffer functions)
  "Emit Type section."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of types
    (emit-leb128-unsigned (length functions) content)
    ;; Each function type
    (dolist (func functions)
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
            (emit-leb128-unsigned 0 content))))
    ;; Write section
    (vector-push-extend 1 buffer)  ; Type section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-function-section (buffer functions)
  "Emit Function section."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Number of functions
    (emit-leb128-unsigned (length functions) content)
    ;; Type index for each function
    (dotimes (i (length functions))
      (emit-leb128-unsigned i content))
    ;; Write section
    (vector-push-extend 3 buffer)  ; Function section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))))

(defun emit-global-section (buffer globals)
  "Emit Global section."
  (declare (ignore globals))
  ;; Skip for now - globals require GC types which are complex
  buffer)

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
       (:ref.eq (vector-push-extend #xD5 buffer))
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
    (t
     (error "Unknown block type: ~A" block-type))))

(defun emit-valtype (type buffer)
  "Emit a value type."
  (vector-push-extend
   (ecase type
     (:i32 #x7F)
     (:i64 #x7E)
     (:f32 #x7D)
     (:f64 #x7C)
     (:anyref #x6E)     ; any heap type - parent of GC types
     (:externref #x6F)  ; external references - separate hierarchy
     (:funcref #x70)
     (:i31ref #x6C)
     (:eqref #x6D))
   buffer))

(defun emit-heaptype (type buffer)
  "Emit a heap type for ref.null."
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
     (:extern #x6F))
   buffer))

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
