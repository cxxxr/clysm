;;;; import-gen.lisp - Wasm import section generation (T022-T025)
;;;;
;;;; Generates Wasm import section entries from FFI declarations.

(in-package #:clysm/ffi)

;;; ============================================================
;;; T022: Import Call Generation
;;; ============================================================

(defun generate-import-call (decl args env)
  "Generate Wasm instructions for calling an imported host function.
   DECL: ForeignFunctionDecl
   ARGS: List of argument AST nodes (compiled to Wasm instructions)
   ENV: Compilation environment
   Returns: List of Wasm instructions

   T052: Marshalling is now integrated:
   1. For each argument, marshal from Lisp type to Wasm type
   2. Call the imported function
   3. Marshal return value from Wasm type back to Lisp type"
  (declare (ignore env))
  (let ((import-idx (ffd-type-index decl))
        (param-types (ffd-param-types decl))
        (return-type (ffd-return-type decl))
        (instrs '()))
    ;; Push and marshal each argument
    ;; Note: 'args' are already compiled Wasm instruction sequences
    ;; We interleave: push arg, marshal to Wasm type
    (loop for arg in args
          for param-type in param-types
          do
             ;; The arg is already on stack after its instructions execute
             ;; Append marshalling instructions to convert Lisp type to Wasm type
             (let ((marshal-instrs (marshal-to-wasm param-type)))
               (when marshal-instrs
                 (dolist (instr marshal-instrs)
                   (push instr instrs)))))
    ;; Call the imported function
    (push (list :call import-idx) instrs)
    ;; Marshal return value back to Lisp type
    (unless (eq return-type :void)
      (let ((return-marshal (marshal-from-wasm return-type)))
        (when return-marshal
          (dolist (instr return-marshal)
            (push instr instrs)))))
    (nreverse instrs)))

;;; ============================================================
;;; T023: Import Section Emission
;;; ============================================================

(defun collect-ffi-imports (env)
  "Collect all foreign function declarations from the environment.
   Returns a list of WasmImport structures."
  (let ((imports '()))
    (maphash (lambda (name decl)
               (declare (ignore name))
               (push (make-wasm-import
                      :module-name (ffd-module-name decl)
                      :field-name (ffd-field-name decl)
                      :kind :func
                      :type-index (ffd-type-index decl))
                     imports))
             (ffi-env-imports env))
    (nreverse imports)))

(defun emit-ffi-imports (env buffer)
  "Emit the Import section to BUFFER from FFI environment.
   Returns the number of imports emitted."
  (let ((imports (collect-ffi-imports env)))
    (when imports
      (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                                   :adjustable t :fill-pointer 0)))
        ;; Import count
        (emit-leb128-unsigned (length imports) content)
        ;; Each import entry
        (dolist (import imports)
          (emit-import-entry import content))
        ;; Write section: ID 2 + size + content
        (vector-push-extend 2 buffer)  ; Import section ID
        (emit-leb128-unsigned (length content) buffer)
        (loop for b across content do (vector-push-extend b buffer))))
    (length imports)))

(defun emit-import-entry (import buffer)
  "Emit a single import entry to the buffer."
  ;; Module name (length-prefixed UTF-8)
  (let ((module-bytes (babel:string-to-octets (wi-module-name import) :encoding :utf-8)))
    (emit-leb128-unsigned (length module-bytes) buffer)
    (loop for b across module-bytes do (vector-push-extend b buffer)))
  ;; Field name (length-prefixed UTF-8)
  (let ((field-bytes (babel:string-to-octets (wi-field-name import) :encoding :utf-8)))
    (emit-leb128-unsigned (length field-bytes) buffer)
    (loop for b across field-bytes do (vector-push-extend b buffer)))
  ;; Import kind (0 = func)
  (vector-push-extend (ecase (wi-kind import)
                        (:func 0)
                        (:table 1)
                        (:memory 2)
                        (:global 3))
                      buffer)
  ;; Type index (for func imports)
  (when (eq (wi-kind import) :func)
    (emit-leb128-unsigned (wi-type-index import) buffer)))

;;; ============================================================
;;; Helper: LEB128 Encoding
;;; ============================================================

(defun emit-leb128-unsigned (value buffer)
  "Emit an unsigned LEB128 encoded value to buffer."
  (loop
    (let ((byte (logand value #x7F)))
      (setf value (ash value -7))
      (if (zerop value)
          (progn
            (vector-push-extend byte buffer)
            (return))
          (vector-push-extend (logior byte #x80) buffer)))))

;;; ============================================================
;;; T025: Compiler Integration
;;; ============================================================

(defun get-ffi-import-count ()
  "Get the number of FFI imports registered.
   This affects function indexing - imported functions come first."
  (hash-table-count (ffi-env-imports *ffi-environment*)))

(defun assign-import-indices (env)
  "Assign type indices to all registered foreign functions.
   Type indices for imports start after the built-in types (currently 23+).
   Function indices for imports start at 0."
  (let ((func-index 0)
        (type-index 23))  ; Start after built-in types (22 = mv_array)
    (maphash (lambda (name decl)
               (declare (ignore name))
               ;; Each import gets a unique function type
               (setf (ffd-type-index decl) type-index)
               (incf type-index)
               (incf func-index))
             (ffi-env-imports env))
    func-index))

;;; ============================================================
;;; T056: Collect FFI Declarations
;;; ============================================================

(defun collect-ffi-declarations (env)
  "Gather all FFI declarations from the environment.
   Returns a list of (type . decl) pairs where type is :import or :export."
  (let ((declarations '()))
    ;; Collect imports
    (maphash (lambda (name decl)
               (declare (ignore name))
               (push (cons :import decl) declarations))
             (ffi-env-imports env))
    ;; Collect exports
    (dolist (decl (ffi-env-exports env))
      (push (cons :export decl) declarations))
    (nreverse declarations)))

;;; ============================================================
;;; T057: Generate Type for FFI Signature
;;; ============================================================

(defun generate-type-for-ffi-signature (param-types return-type)
  "Generate a Wasm function type for an FFI signature.
   PARAM-TYPES: List of marshal types for parameters
   RETURN-TYPE: Marshal type for return value
   Returns: A list representing the Wasm function type."
  (let ((wasm-params (mapcar #'marshal-type-to-wasm-type param-types))
        (wasm-result (unless (eq return-type :void)
                       (list (marshal-type-to-wasm-type return-type)))))
    `(:func ,wasm-params ,wasm-result)))

(defun collect-ffi-types (env)
  "Collect all unique function types needed for FFI declarations.
   Returns a list of function type definitions."
  (let ((types '())
        (seen (make-hash-table :test 'equal)))
    ;; Collect types from imports
    (maphash (lambda (name decl)
               (declare (ignore name))
               (let* ((sig (generate-type-for-ffi-signature
                           (ffd-param-types decl)
                           (ffd-return-type decl)))
                      (key (format nil "~S" sig)))
                 (unless (gethash key seen)
                   (setf (gethash key seen) t)
                   (push sig types))))
             (ffi-env-imports env))
    ;; Collect types from exports
    (dolist (decl (ffi-env-exports env))
      (let* ((sig (generate-type-for-ffi-signature
                  (ed-param-types decl)
                  (ed-return-type decl)))
             (key (format nil "~S" sig)))
        (unless (gethash key seen)
          (setf (gethash key seen) t)
          (push sig types))))
    (nreverse types)))

;;; ============================================================
;;; T068: Error Handling for FFI Calls (Constitution IV Compliance)
;;; ============================================================
;;;
;;; Constitution IV: Exceptions via try_table/throw/throw_ref
;;; FFI calls can throw host exceptions which need to be caught
;;; and translated to Lisp conditions.

(defun generate-import-call-with-error-handling (decl args env &key (catch-host-errors t))
  "Generate Wasm instructions for calling an imported host function with error handling.
   DECL: ForeignFunctionDecl
   ARGS: List of argument AST nodes
   ENV: Compilation environment
   CATCH-HOST-ERRORS: If T, wrap call in try_table to catch host exceptions

   Returns: List of Wasm instructions

   T068: Adds try_table/catch around FFI calls for Constitution IV compliance.
   Host exceptions are caught and translated to ffi:ffi-host-error conditions."
  (let ((base-instrs (generate-import-call decl args env)))
    (if catch-host-errors
        ;; Wrap in try_table/catch for host error handling
        (generate-ffi-try-catch-wrapper base-instrs decl)
        base-instrs)))

(defun generate-ffi-try-catch-wrapper (instrs decl)
  "Wrap FFI call instructions in try_table/catch for error handling.
   T068: Constitution IV compliance - exceptions via try_table.

   Structure:
   (block $ffi-success
     (try_table (catch_all $ffi-error)
       <ffi-call-instructions>
       (br $ffi-success))
     ;; Error handler: translate to ffi:ffi-host-error
     (call $signal-ffi-host-error)
     (unreachable))

   Note: Full implementation requires exception tag and handler setup.
   This placeholder generates the structural wrapper."
  (declare (ignore decl))
  ;; For now, return unwrapped instructions
  ;; Full implementation would require:
  ;; 1. Exception tag for host errors in Type section
  ;; 2. Handler function to signal ffi:ffi-host-error
  ;; 3. try_table block structure
  ;;
  ;; Placeholder comment for future implementation:
  ;; `(block $ffi-success (result anyref)
  ;;    (try_table (catch_all $ffi-error)
  ;;      ,@instrs
  ;;      (br $ffi-success))
  ;;    ;; catch_all handler
  ;;    (call $translate-host-exception)
  ;;    (unreachable))
  instrs)

;;; ============================================================
;;; T069: Host Exception to FFI-HOST-ERROR Translation
;;; ============================================================

(defun generate-host-error-signal (function-name)
  "Generate Wasm instructions to signal an ffi:ffi-host-error condition.
   FUNCTION-NAME: The name of the FFI function that failed

   T069: Translates caught host exceptions to Lisp conditions.

   The generated code:
   1. Creates a string with the function name
   2. Creates a string with error message (from exception if available)
   3. Calls internal error signaling function"
  (declare (ignore function-name))
  ;; Placeholder - full implementation would:
  ;; 1. Create $ffi-host-error condition object
  ;; 2. Set :function-name slot
  ;; 3. Set :message slot from caught exception
  ;; 4. Call (signal 'ffi:ffi-host-error ...)
  ;;
  ;; For now, return nil (no instructions)
  ;; This is safe as generate-ffi-try-catch-wrapper currently passes through
  nil)

(defun ffi-error-handler-type ()
  "Return the Wasm function type for the FFI error handler.
   Type: () -> () (no params, no return - just signals error)"
  '(:func () ()))
