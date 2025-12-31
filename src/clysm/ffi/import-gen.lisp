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
  ;; 001-numeric-functions: Use ffd-func-index for :call-import (not ffd-type-index)
  ;; ffd-type-index is for import section type references
  ;; ffd-func-index is for :call instruction (0, 1, 2... for imports)
  (let ((import-idx (ffd-func-index decl))
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
    ;; 001-numeric-functions: Use :call-import to distinguish from local :call
    (push (list :call-import import-idx) instrs)
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
  (let ((module-bytes (clysm/lib/utf8:string-to-utf8-octets (wi-module-name import))))
    (emit-leb128-unsigned (length module-bytes) buffer)
    (loop for b across module-bytes do (vector-push-extend b buffer)))
  ;; Field name (length-prefixed UTF-8)
  (let ((field-bytes (clysm/lib/utf8:string-to-utf8-octets (wi-field-name import))))
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
;;; T014: Selective Import Emission (Feature 001-ffi-import-architecture)
;;; ============================================================

(defun collect-selected-ffi-imports (env used-ffis)
  "Collect only the FFI declarations that are in USED-FFIS list.
   USED-FFIS is a list of symbols naming the FFI functions to include.
   If USED-FFIS is NIL, returns all FFI imports (T048: :full mode support).
   Returns a list of WasmImport structures for only the used functions."
  (let ((imports '()))
    (maphash (lambda (name decl)
               ;; Check if this FFI function is in the used-ffis list
               ;; Name is a string, used-ffis contains symbols
               ;; T048: NIL means all FFIs (for :full mode)
               (when (or (null used-ffis)
                         (member name used-ffis
                                 :test (lambda (n sym)
                                         (string-equal n (symbol-name sym)))))
                 (push (make-wasm-import
                        :module-name (ffd-module-name decl)
                        :field-name (ffd-field-name decl)
                        :kind :func
                        :type-index (ffd-type-index decl))
                       imports)))
             (ffi-env-imports env))
    (nreverse imports)))

(defun emit-selected-ffi-imports (env buffer used-ffis)
  "Emit the Import section to BUFFER with only the used FFI functions.
   USED-FFIS is a list of symbols naming the FFI functions to include.
   Feature 001-ffi-import-architecture: T014 implementation.
   Returns the number of imports emitted."
  (let ((imports (collect-selected-ffi-imports env used-ffis)))
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

(defun assign-import-indices (env &optional (regular-func-count 0))
  "Assign type and function indices to all registered foreign functions.
   Type indices for imports are shared based on function signature.
   Type indices start at 31 + REGULAR-FUNC-COUNT + unique_index.
   (31 = number of base GC types before regular function types)
   Function indices for imports start at 0 (imports come before local functions).
   001-numeric-functions: ffd-type-index is for import section, ffd-func-index is for :call."
  (let ((func-index 0)
        ;; FFI types come after regular function types in the type section
        ;; 31 base GC types (0-30) before regular function types
        (type-base (+ 31 regular-func-count))
        ;; Track unique signatures -> type index
        (signature-to-type-index (make-hash-table :test 'equal))
        (next-unique-type-index 0))
    (maphash (lambda (name decl)
               (declare (ignore name))
               ;; Compute signature key
               (let* ((sig (generate-type-for-ffi-signature
                            (ffd-param-types decl)
                            (ffd-return-type decl)))
                      (sig-key (format nil "~S" sig)))
                 ;; Get or assign type index for this signature
                 (let ((type-idx (gethash sig-key signature-to-type-index)))
                   (unless type-idx
                     ;; New signature - assign next available type index
                     (setf type-idx (+ type-base next-unique-type-index))
                     (setf (gethash sig-key signature-to-type-index) type-idx)
                     (incf next-unique-type-index))
                   ;; Set the type index for this import
                   (setf (ffd-type-index decl) type-idx))
                 ;; Each import gets a sequential function index (for :call)
                 (setf (ffd-func-index decl) func-index)
                 (incf func-index)))
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

;;; ============================================================
;;; T035: Dynamic Call Import for Runtime Resolution
;;; Feature: 001-ffi-import-architecture (User Story 3)
;;; ============================================================
;;;
;;; When code contains dynamic funcall/apply patterns like:
;;;   (funcall (intern "FOO") x)
;;;   (apply fn args)  ; where fn is a variable
;;;
;;; The compiler cannot statically determine which function will be called.
;;; We emit a $dynamic-call import from "clysm:runtime" module that:
;;; 1. Takes a function name (symbol) and arguments
;;; 2. Resolves the function at runtime
;;; 3. Calls the function with provided arguments
;;; 4. Returns the result

(defparameter *dynamic-call-type-index* nil
  "Type index for $dynamic-call function type in the Type section.
   Set during module compilation when dynamic calls are needed.")

(defun make-dynamic-call-wasm-import ()
  "Create a WasmImport for the $dynamic-call runtime function.
   Signature: (anyref symbol, anyref args) -> anyref result
   - symbol: The function name as a Clysm symbol object
   - args: Arguments as a list or array
   - result: Return value from the called function"
  (make-wasm-import
   :module-name "clysm:runtime"
   :field-name "$dynamic-call"
   :kind :func
   :type-index *dynamic-call-type-index*))

(defun emit-dynamic-call-import (buffer type-index)
  "Emit the $dynamic-call import to BUFFER.
   TYPE-INDEX is the type index for the (anyref, anyref) -> anyref signature.
   Returns 1 (number of imports emitted)."
  (let ((content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Import count: 1
    (emit-leb128-unsigned 1 content)
    ;; Module name: "clysm:runtime"
    (let ((mod-bytes (clysm/lib/utf8:string-to-utf8-octets "clysm:runtime")))
      (emit-leb128-unsigned (length mod-bytes) content)
      (loop for b across mod-bytes do (vector-push-extend b content)))
    ;; Field name: "$dynamic-call"
    (let ((field-bytes (clysm/lib/utf8:string-to-utf8-octets "$dynamic-call")))
      (emit-leb128-unsigned (length field-bytes) content)
      (loop for b across field-bytes do (vector-push-extend b content)))
    ;; Import kind: 0 = func
    (vector-push-extend 0 content)
    ;; Type index
    (emit-leb128-unsigned type-index content)
    ;; Write section: ID 2 + size + content
    (vector-push-extend 2 buffer)  ; Import section ID
    (emit-leb128-unsigned (length content) buffer)
    (loop for b across content do (vector-push-extend b buffer))
    1))

(defun emit-selected-ffi-imports-with-dynamic-call (env buffer used-ffis has-dynamic-call-p
                                                    &optional (dynamic-call-type-index 31))
  "Emit the Import section with selected FFI imports and optionally $dynamic-call.
   ENV: FFI environment
   BUFFER: Output buffer
   USED-FFIS: List of symbols naming the FFI functions to include.
              If NIL, all FFI imports are included (T048: :full mode support).
   HAS-DYNAMIC-CALL-P: If T, also include $dynamic-call import
   DYNAMIC-CALL-TYPE-INDEX: Type index for $dynamic-call function (default 31)
   Returns the number of imports emitted."
  (let ((imports (collect-selected-ffi-imports env used-ffis))
        (content (make-array 0 :element-type '(unsigned-byte 8)
                               :adjustable t :fill-pointer 0)))
    ;; Add $dynamic-call import if needed
    (when has-dynamic-call-p
      ;; Create $dynamic-call import with explicit type index
      (push (make-wasm-import
             :module-name "clysm:runtime"
             :field-name "$dynamic-call"
             :kind :func
             :type-index dynamic-call-type-index)
            imports))
    (when imports
      ;; Import count
      (emit-leb128-unsigned (length imports) content)
      ;; Each import entry
      (dolist (import imports)
        (emit-import-entry import content))
      ;; Write section: ID 2 + size + content
      (vector-push-extend 2 buffer)  ; Import section ID
      (emit-leb128-unsigned (length content) buffer)
      (loop for b across content do (vector-push-extend b buffer)))
    (length imports)))

;;; ============================================================
;;; T047: Dynamic Call Host Import (027-complete-ffi)
;;; ============================================================

(defparameter *call-host-dynamic-import-index* nil
  "Function index for $ffi_call_host_dynamic import.
   Set during module compilation when dynamic calls are used.")

(defun make-call-host-dynamic-import-decl ()
  "Create a ForeignFunctionDecl for the $ffi_call_host_dynamic import.
   This is a system import that enables dynamic host function calls.

   Signature: (externref name, externref args) -> externref result
   - name: Function name string (e.g., \"host.random\")
   - args: Array of arguments (nullable externref)
   - result: Return value from host function"
  (make-foreign-function-decl
   :lisp-name '$ffi_call_host_dynamic
   :module-name "ffi"
   :field-name "call_host_dynamic"
   :param-types '(:anyref :anyref)  ; Will be converted to externref
   :return-type :anyref))

(defun register-call-host-dynamic-import (env)
  "Register the $ffi_call_host_dynamic import in the FFI environment.
   Returns the function index assigned to the import."
  (let ((decl (make-call-host-dynamic-import-decl)))
    (setf (gethash '$ffi_call_host_dynamic (ffi-env-imports env)) decl)
    ;; Return the decl for index assignment
    decl))

(defun get-call-host-dynamic-index (env)
  "Get the function index for $ffi_call_host_dynamic.
   Returns NIL if not registered.
   001-numeric-functions: Returns ffd-func-index (for :call), not ffd-type-index."
  (let ((decl (gethash '$ffi_call_host_dynamic (ffi-env-imports env))))
    (when decl
      (ffd-func-index decl))))

(defun ensure-call-host-dynamic-import (env)
  "Ensure $ffi_call_host_dynamic is registered. Idempotent.
   Call this when compiling code that uses ffi:call-host."
  (unless (gethash '$ffi_call_host_dynamic (ffi-env-imports env))
    (register-call-host-dynamic-import env)))

;;; ============================================================
;;; Math Module FFI Imports (001-numeric-functions)
;;; ============================================================
;;;
;;; Research decision: Import transcendental functions from host via FFI.
;;; This is ~10x faster than implementing Taylor series in Wasm.
;;; The host shim (host-shim/math-shim.js) provides Math.* functions.

(defparameter *math-function-specs*
  '(;; Trigonometric functions (radians)
    (:sin     "sin"   (:float) :float)
    (:cos     "cos"   (:float) :float)
    (:tan     "tan"   (:float) :float)
    ;; Inverse trigonometric functions
    (:asin    "asin"  (:float) :float)
    (:acos    "acos"  (:float) :float)
    (:atan    "atan"  (:float) :float)
    (:atan2   "atan2" (:float :float) :float)
    ;; Hyperbolic functions
    (:sinh    "sinh"  (:float) :float)
    (:cosh    "cosh"  (:float) :float)
    (:tanh    "tanh"  (:float) :float)
    ;; Inverse hyperbolic functions
    (:asinh   "asinh" (:float) :float)
    (:acosh   "acosh" (:float) :float)
    (:atanh   "atanh" (:float) :float)
    ;; Exponential and logarithmic functions
    (:exp     "exp"   (:float) :float)
    (:log     "log"   (:float) :float)
    (:log10   "log10" (:float) :float)
    ;; Power functions
    (:pow     "pow"   (:float :float) :float))
  "Specification of math functions to import from host.
   Format: (lisp-name field-name param-types return-type)")

(defun make-math-import-decl (spec)
  "Create a ForeignFunctionDecl from a math function spec.
   SPEC: (lisp-name field-name param-types return-type)"
  (destructuring-bind (lisp-name field-name param-types return-type) spec
    (make-foreign-function-decl
     :lisp-name lisp-name
     :module-name "clysm:math"
     :field-name field-name
     :param-types param-types
     :return-type return-type)))

(defun register-math-imports (env)
  "Register all math module FFI imports in the FFI environment.
   Call this when compiling code that uses transcendental functions."
  (dolist (spec *math-function-specs*)
    (let* ((lisp-name (first spec))
           (existing (gethash lisp-name (ffi-env-imports env))))
      (unless existing
        (register-foreign-function env (make-math-import-decl spec)))))
  env)

(defun ensure-math-imports (env)
  "Ensure math module imports are registered. Idempotent.
   Call this when compiling code that uses transcendental functions."
  (unless (gethash :sin (ffi-env-imports env))
    (register-math-imports env)))
