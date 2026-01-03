;;;; func-section.lisp - Function and Code Section generation
;;;; Compiles AST nodes to Wasm instructions

(in-package #:clysm/compiler/codegen/func-section)

;;; ============================================================
;;; Compilation Environment
;;; ============================================================

(defstruct (compilation-env (:conc-name cenv-)
                            (:copier nil))
  "Compilation environment tracking locals and scope."
  (locals nil :type list)              ; ((name . index) ...)
  (local-counter-box nil :type list)   ; (counter) - mutable box for shared counter
  (local-types-box nil :type list)     ; ((index . type) ...) - boxed for sharing
  (functions nil :type list)           ; ((name . index) ...)
  (function-counter 0 :type fixnum)
  (in-tail-position nil :type boolean)
  (captured-vars nil :type list)       ; ((name . position) ...) for captured vars
  (local-functions nil :type list)     ; ((name . local-idx) ...) for flet/labels
  ;; Phase 6: Exception handling
  (blocks nil :type list)              ; ((name . block-depth) ...) for block/return-from
  (block-depth 0 :type fixnum)         ; Current nesting depth for br targets
  (tagbody-tags nil :type list)        ; ((tag . label-idx) ...) for tagbody/go - legacy field
  (tagbody-context nil :type (or null tagbody-context)) ; Active tagbody compilation context
  (catch-tags nil :type list)          ; ((tag-expr . handler-depth) ...) for catch/throw
  (unwind-stack nil :type list))       ; Stack of unwind-protect handlers

;;; ============================================================
;;; Tagbody Context (for tagbody/go compilation)
;;; ============================================================

(defstruct tagbody-context
  "Context for compiling tagbody/go control flow.
   Created when entering compile-tagbody, stored in compilation-env,
   accessed by compile-go to determine jump target."
  (strategy nil :type keyword)
  ;; :sequential | :simple-loop | :dispatch

  (tags nil :type list)
  ;; Association list: ((tag-symbol . segment-index) ...)
  ;; Maps tag names to their segment indices for go target resolution

  (pc-local nil :type (or null fixnum))
  ;; Index of $pc local variable (dispatch strategy only)
  ;; NIL for sequential and simple-loop strategies

  (base-loop-depth 0 :type fixnum)
  ;; Base br depth from the loop body to the loop target
  ;; For simple-loop: 0 (br 0 targets the loop)
  ;; For dispatch: varies per segment (num-segments - seg-idx)

  (base-block-depth 0 :type fixnum)
  ;; Block depth when entering the tagbody's loop
  ;; Used to calculate correct br depth when nested in if/block structures

  (loop-label nil :type (or null symbol))
  ;; Wasm label for loop target (simple-loop and dispatch strategies)
  ;; e.g., $LOOP for simple-loop, $dispatch for dispatch

  (num-segments nil :type (or null fixnum))
  ;; Total number of segments (used for depth calculation)
  )

;;; ============================================================
;;; Runtime Function Table (001-io-list-runtime)
;;; ============================================================

(defparameter *runtime-function-table* (make-hash-table :test 'eq)
  "Maps Lisp function symbols to runtime function names.
   Functions in this table are compiled as calls to the runtime library
   instead of generating inline Wasm code.

   Structure: symbol -> (runtime-name . arity-or-nil)
   where runtime-name is a keyword like :$princ-rt
   and arity is the expected argument count (nil for variadic).")

(defun register-runtime-function (symbol runtime-name &optional arity)
  "Register a function to be dispatched to the runtime library.
   SYMBOL: The Lisp function symbol (e.g., 'princ)
   RUNTIME-NAME: The Wasm function name (e.g., :$princ-rt)
   ARITY: Expected argument count (nil for variadic functions)"
  (setf (gethash symbol *runtime-function-table*)
        (cons runtime-name arity)))

(defun runtime-function-p (symbol)
  "Check if SYMBOL should be dispatched to the runtime library."
  (gethash symbol *runtime-function-table*))

(defun compile-runtime-call (function args env)
  "Compile a call to a runtime library function.
   Compiles arguments, then emits a :call instruction to the runtime function."
  (let* ((entry (gethash function *runtime-function-table*))
         (runtime-name (car entry))
         (expected-arity (cdr entry)))
    ;; Validate arity if specified
    (when (and expected-arity (/= (length args) expected-arity))
      (error "Runtime function ~A expects ~D arguments, got ~D"
             function expected-arity (length args)))
    ;; Compile arguments (not in tail position)
    (let ((env-non-tail (env-with-non-tail env)))
      (with-instruction-collector
        (dolist (arg args)
          (emit* (compile-to-instructions arg env-non-tail)))
        ;; Emit call to runtime function
        (emit :call runtime-name)))))

;;; ============================================================
;;; Runtime Function Registration (001-io-list-runtime)
;;; ============================================================

(defun register-io-runtime-functions ()
  "Register I/O functions to use runtime library dispatch.
   Called when the runtime library is ready for use."
  ;; I/O functions (FR-001 to FR-004)
  ;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
  (register-runtime-function 'princ :$princ-rt 1)
  (register-runtime-function 'prin1 :$prin1-rt 1)
  (register-runtime-function 'print :$print-rt 1)
  (register-runtime-function 'write :$write-rt nil)  ; variadic
  ;; HyperSpec: resources/HyperSpec/Body/f_terpri.htm
  (register-runtime-function 'terpri :$terpri-rt nil) ; 0-1 args
  ;; HyperSpec: resources/HyperSpec/Body/f_format.htm
  (register-runtime-function 'format :$format-rt nil)) ; variadic

(defun register-list-runtime-functions ()
  "Register list search functions to use runtime library dispatch.
   Called when the runtime library is ready for use."
  ;; List search functions (FR-005 to FR-012)
  ;; HyperSpec: resources/HyperSpec/Body/f_mem_m.htm
  (register-runtime-function 'member :$member-rt nil)
  (register-runtime-function 'member-if :$member-if-rt nil)
  (register-runtime-function 'member-if-not :$member-if-not-rt nil)
  ;; HyperSpec: resources/HyperSpec/Body/f_assocc.htm
  (register-runtime-function 'assoc :$assoc-rt nil)
  (register-runtime-function 'assoc-if :$assoc-if-rt nil)
  ;; HyperSpec: resources/HyperSpec/Body/f_rassoc.htm
  (register-runtime-function 'rassoc :$rassoc-rt nil)
  (register-runtime-function 'rassoc-if :$rassoc-if-rt nil)
  ;; HyperSpec: resources/HyperSpec/Body/f_find_.htm
  (register-runtime-function 'find :$find-rt nil)
  (register-runtime-function 'find-if :$find-if-rt nil)
  (register-runtime-function 'find-if-not :$find-if-not-rt nil)
  ;; HyperSpec: resources/HyperSpec/Body/f_pos_p.htm
  (register-runtime-function 'position :$position-rt nil)
  (register-runtime-function 'position-if :$position-if-rt nil)
  (register-runtime-function 'position-if-not :$position-if-not-rt nil))

(defun register-sequence-runtime-functions ()
  "Register sequence functions to use runtime library dispatch.
   Feature: 001-sequence-runtime-migration"
  ;; Remove family (FR-010 to FR-013)
  ;; HyperSpec: resources/HyperSpec/Body/f_rm_rm.htm
  (register-runtime-function 'remove :$remove-rt nil)
  (register-runtime-function 'remove-if :$remove-if-rt nil)
  (register-runtime-function 'remove-if-not :$remove-if-not-rt nil)
  ;; Count family (FR-020 to FR-023)
  ;; HyperSpec: resources/HyperSpec/Body/f_countc.htm
  (register-runtime-function 'count :$count-rt nil)
  (register-runtime-function 'count-if :$count-if-rt nil)
  (register-runtime-function 'count-if-not :$count-if-not-rt nil)
  ;; Substitute family (FR-030 to FR-033)
  ;; HyperSpec: resources/HyperSpec/Body/f_sbs_s.htm
  (register-runtime-function 'substitute :$substitute-rt nil)
  (register-runtime-function 'substitute-if :$substitute-if-rt nil)
  (register-runtime-function 'substitute-if-not :$substitute-if-not-rt nil)
  ;; Delete family (FR-040 to FR-044)
  ;; HyperSpec: resources/HyperSpec/Body/f_rm_rm.htm
  (register-runtime-function 'delete :$delete-rt nil)
  (register-runtime-function 'delete-if :$delete-if-rt nil)
  (register-runtime-function 'delete-if-not :$delete-if-not-rt nil))

(defun register-package-runtime-functions ()
  "Register package functions to use runtime library dispatch.
   Feature: 001-internal-function-export, 001-type-package-export"
  ;; Package predicate (US2, P951 blocker)
  ;; HyperSpec: resources/HyperSpec/Body/f_pkgp.htm
  ;; Use intern for runtime lookup - symbols may not exist at load time
  (register-runtime-function (intern "PACKAGEP*" :clysm) :$packagep*-rt 1)
  ;; Package lookup (for completeness)
  ;; HyperSpec: resources/HyperSpec/Body/f_find_p.htm
  (register-runtime-function (intern "FIND-PACKAGE*" :clysm) :$find-package*-rt 1)
  ;; Symbol interning
  ;; HyperSpec: resources/HyperSpec/Body/f_intern.htm
  (register-runtime-function (intern "INTERN*" :clysm) :$intern*-rt nil)
  ;; Symbol package accessor (US3, T034)
  ;; HyperSpec: resources/HyperSpec/Body/f_symb_2.htm
  (register-runtime-function (intern "SYMBOL-PACKAGE*" :clysm) :$symbol-package*-rt 1))

(defun register-lexenv-runtime-functions ()
  "Register lexical environment functions to use runtime library dispatch.
   Feature: 001-lexenv-function-export"
  ;; Lexical environment local variable management
  ;; env-add-local: adds local variable to compilation environment, returns index
  ;; Has optional type parameter (env name &optional type) so use nil for variadic
  (register-runtime-function 'clysm:env-add-local :$env-add-local-rt nil)
  ;; LOOP macro keyword comparison
  ;; loop-keyword-eq: compares form against LOOP keyword
  (register-runtime-function 'clysm:loop-keyword-eq :$loop-keyword-eq-rt 2)
  ;; Numeric literal predicate
  ;; numeric-literal-p: checks if AST node is numeric literal
  (register-runtime-function 'clysm:numeric-literal-p :$numeric-literal-p-rt 1))

(defun register-ast-runtime-functions ()
  "Register AST manipulation functions for runtime dispatch.
   Feature: 001-ast-function-export"
  ;; Core compilation function - compiles AST form to Wasm instructions
  (register-runtime-function 'clysm:compile-to-instructions :$compile-to-instructions-rt 2)
  ;; GC type definition functions
  (register-runtime-function 'clysm:make-wasm-struct-type :$make-wasm-struct-type-rt nil)
  (register-runtime-function 'clysm:wasm-struct-type-p :$wasm-struct-type-p-rt 1)
  (register-runtime-function 'clysm:wasm-struct-type-fields :$wasm-struct-type-fields-rt 1)
  ;; AST literal functions
  (register-runtime-function 'clysm:make-ast-literal :$make-ast-literal-rt nil)
  (register-runtime-function 'clysm:ast-literal-value :$ast-literal-value-rt 1)
  (register-runtime-function 'clysm:ast-literal-p :$ast-literal-p-rt 1)
  ;; Numeric AST processing
  (register-runtime-function 'clysm:get-numeric-value :$get-numeric-value-rt 1))

(defun register-parser-runtime-functions ()
  "Register parser functions to use runtime library dispatch.
   Feature: 001-wasm-local-binding US2"
  ;; Parser state manipulation
  ;; advance-token: consumes current token and returns it (arity 1)
  (register-runtime-function 'clysm:advance-token :$advance-token-rt 1)
  ;; current-token: gets current token without consuming (arity 1)
  (register-runtime-function 'clysm:current-token :$current-token-rt 1)
  ;; make-parser-state: creates parser state from token list (variadic)
  (register-runtime-function 'clysm:make-parser-state :$make-parser-state-rt nil))

(defun register-backend-runtime-functions ()
  "Register backend Wasm emission functions to use runtime library dispatch.
   Feature: 001-wasm-local-binding US3"
  ;; Wasm module header emission (arity 0)
  (register-runtime-function 'clysm:emit-module-header :$emit-module-header-rt 0))

(defun register-string-runtime-functions ()
  "Register string manipulation functions to use runtime library dispatch.
   Feature: 001-string-runtime-migration
   HyperSpec references:
     [char](resources/HyperSpec/Body/f_char_.htm)
     [string-trim](resources/HyperSpec/Body/f_stg_tr.htm)
     [string-capitalize](resources/HyperSpec/Body/f_stg_up.htm)
     [string-equal](resources/HyperSpec/Body/f_stgeq_.htm)"
  ;; Character access (arity 2: string, index)
  (register-runtime-function 'char :$string-char-rt 2)
  (register-runtime-function 'schar :$string-char-rt 2)
  ;; String trim family (variadic for :start/:end keywords)
  (register-runtime-function 'string-trim :$string-trim-rt nil)
  (register-runtime-function 'string-left-trim :$string-left-trim-rt nil)
  (register-runtime-function 'string-right-trim :$string-right-trim-rt nil)
  ;; String capitalize family (variadic for :start/:end keywords)
  (register-runtime-function 'string-capitalize :$string-capitalize-rt nil)
  (register-runtime-function 'nstring-capitalize :$nstring-capitalize-rt nil)
  ;; Case-insensitive string comparison (variadic for keyword args)
  (register-runtime-function 'string-equal :$string-equal-rt nil)
  (register-runtime-function 'string-not-equal :$string-not-equal-rt nil)
  (register-runtime-function 'string-lessp :$string-lessp-rt nil)
  (register-runtime-function 'string-greaterp :$string-greaterp-rt nil)
  (register-runtime-function 'string-not-lessp :$string-not-lessp-rt nil)
  (register-runtime-function 'string-not-greaterp :$string-not-greaterp-rt nil))

(defun clear-runtime-functions ()
  "Clear all runtime function registrations.
   Used for testing and when falling back to inline codegen."
  (clrhash *runtime-function-table*))

;;; ============================================================
;;; Module Load Initialization
;;; Register all runtime functions when module is loaded
;;; ============================================================

(register-io-runtime-functions)
(register-list-runtime-functions)
(register-sequence-runtime-functions)
(register-string-runtime-functions)  ; 001-string-runtime-migration
(register-package-runtime-functions)
(register-lexenv-runtime-functions)
(register-ast-runtime-functions)
(register-parser-runtime-functions)
(register-backend-runtime-functions)

(defun make-env ()
  "Create a fresh compilation environment."
  (make-compilation-env :local-counter-box (list 0)
                        :local-types-box (list nil)))

(defun cenv-local-counter (env)
  "Get the local counter value."
  (car (cenv-local-counter-box env)))

(defun (setf cenv-local-counter) (value env)
  "Set the local counter value."
  (setf (car (cenv-local-counter-box env)) value))

(defun env-lookup-local (env name)
  "Look up a local variable index."
  (cdr (assoc name (cenv-locals env))))

(defun env-add-local (env name &optional (type :anyref))
  "Add a local variable with optional type and return its index.
   Type defaults to :anyref. Non-anyref types are tracked in local-types-box."
  (let ((idx (cenv-local-counter env)))
    (push (cons name idx) (cenv-locals env))
    ;; Track non-anyref types in the boxed list
    (unless (eq type :anyref)
      (setf (car (cenv-local-types-box env))
            (cons (cons idx type) (car (cenv-local-types-box env)))))
    (incf (car (cenv-local-counter-box env)))
    idx))

(defun env-local-type (env idx)
  "Get the type of a local by index. Returns :anyref if not explicitly tracked."
  (or (cdr (assoc idx (car (cenv-local-types-box env)))) :anyref))

(defun env-lookup-function (env name)
  "Look up a function index."
  (cdr (assoc name (cenv-functions env))))

(defun env-add-function (env name)
  "Add a function and return its index."
  (let ((idx (cenv-function-counter env)))
    (push (cons name idx) (cenv-functions env))
    (incf (cenv-function-counter env))
    idx))

(defun env-set-function-counter (env value)
  "Set the function counter to a specific value."
  (setf (cenv-function-counter env) value))

;;; ============================================================
;;; Keyword Argument Extraction (043-self-hosting-blockers)
;;; ============================================================

(defun extract-keyword-args (args &optional (valid-keywords '(:test :key :start :end :from-end :count)))
  "Extract keyword arguments from a function call argument list.
   Feature: 043-self-hosting-blockers

   ARGS is the list of arguments (possibly mixed positional and keyword args).
   VALID-KEYWORDS is a list of valid keyword symbols to extract.

   Returns two values:
   1. Association list of (keyword . value) for found keywords
   2. List of positional arguments (non-keyword args)

   Example:
     (extract-keyword-args '(item list :test #'equal :key #'car))
     => ((:TEST . #'equal) (:KEY . #'car))
        (item list)"
  (let ((keyword-args nil)
        (positional-args nil)
        (current args))
    (loop while current
          for arg = (car current)
          do (cond
               ;; Check if this is a keyword we should extract
               ((and (keywordp arg)
                     (member arg valid-keywords)
                     (cdr current))
                ;; Found keyword with value - extract both
                (push (cons arg (cadr current)) keyword-args)
                (setf current (cddr current)))
               ;; Not a keyword or not in valid list - positional arg
               (t
                (push arg positional-args)
                (setf current (cdr current)))))
    (values (nreverse keyword-args)
            (nreverse positional-args))))

(defun get-keyword-arg (keyword-args keyword &optional default)
  "Get the value for a keyword from extracted keyword args.
   Feature: 043-self-hosting-blockers"
  (let ((pair (assoc keyword keyword-args)))
    (if pair (cdr pair) default)))

(defun keyword-arg-provided-p (keyword-args keyword)
  "Check if a keyword was provided in the argument list.
   Feature: 043-self-hosting-blockers"
  (not (null (assoc keyword keyword-args))))

;;; ============================================================
;;; Wasm Instruction Opcodes
;;; ============================================================

(defparameter *wasm-opcodes*
  '(;; Control
    (:unreachable . #x00)
    (:nop . #x01)
    (:block . #x02)
    (:loop . #x03)
    (:if . #x04)
    (:else . #x05)
    (:end . #x0B)
    (:br . #x0C)
    (:br_if . #x0D)
    (:return . #x0F)
    (:call . #x10)
    (:call_indirect . #x11)
    ;; Reference
    (:ref.null . #xD0)
    (:ref.is_null . #xD1)
    (:ref.func . #xD2)
    (:ref.eq . #xD3)
    ;; Parametric
    (:drop . #x1A)
    (:select . #x1B)
    ;; Variable
    (:local.get . #x20)
    (:local.set . #x21)
    (:local.tee . #x22)
    (:global.get . #x23)
    (:global.set . #x24)
    ;; Numeric i32
    (:i32.const . #x41)
    (:i32.eqz . #x45)
    (:i32.eq . #x46)
    (:i32.ne . #x47)
    (:i32.lt_s . #x48)
    (:i32.lt_u . #x49)
    (:i32.gt_s . #x4A)
    (:i32.gt_u . #x4B)
    (:i32.le_s . #x4C)
    (:i32.le_u . #x4D)
    (:i32.ge_s . #x4E)
    (:i32.ge_u . #x4F)
    (:i32.add . #x6A)
    (:i32.sub . #x6B)
    (:i32.mul . #x6C)
    (:i32.div_s . #x6D)
    (:i32.div_u . #x6E)
    (:i32.rem_s . #x6F)
    (:i32.rem_u . #x70)
    (:i32.and . #x71)
    (:i32.or . #x72)
    (:i32.xor . #x73)
    ;; i64
    (:i64.const . #x42)
    ;; GC instructions (0xFB prefix)
    (:ref.i31 . (#xFB #x1C))      ; Create i31ref from i32
    (:i31.get_s . (#xFB #x1D))   ; Extract signed i32 from i31ref
    (:i31.get_u . (#xFB #x1E))   ; Extract unsigned i32 from i31ref
    (:struct.new . (#xFB #x00))
    (:struct.get . (#xFB #x02))
    (:struct.set . (#xFB #x05))
    (:ref.cast . (#xFB #x17))    ; ref.cast to a specific type
    ;; Function reference instructions
    (:call_ref . #x14)           ; call_ref (typed function calls)
    (:return_call . #x12)        ; return_call (tail call)
    (:return_call_ref . #x15)))  ; return_call_ref (tail call through ref)

;;; ============================================================
;;; Main Compilation Entry Point
;;; ============================================================

(defun compile-to-instructions (ast env)
  "Compile an AST node to a list of Wasm instructions."
  (etypecase ast
    (clysm/compiler/ast:ast-literal
     (compile-literal ast))
    (clysm/compiler/ast:ast-var-ref
     (compile-var-ref ast env))
    (clysm/compiler/ast:ast-call
     (compile-call ast env))
    (clysm/compiler/ast:ast-if
     (compile-if ast env))
    (clysm/compiler/ast:ast-let
     (compile-let ast env))
    (clysm/compiler/ast:ast-progn
     (compile-progn ast env))
    (clysm/compiler/ast:ast-setq
     (compile-setq ast env))
    (clysm/compiler/ast:ast-defun
     (compile-defun ast env))
    (clysm/compiler/ast:ast-lambda
     (compile-lambda ast env))
    ;; Feature 043: FUNCTION special form (#'fn)
    (clysm/compiler/ast:ast-function
     (compile-function ast env))
    (clysm/compiler/ast:ast-flet
     (compile-flet ast env))
    (clysm/compiler/ast:ast-labels
     (compile-labels ast env))
    (clysm/compiler/ast:ast-block
     (compile-block ast env))
    (clysm/compiler/ast:ast-return-from
     (compile-return-from ast env))
    (clysm/compiler/ast:ast-tagbody
     (compile-tagbody ast env))
    (clysm/compiler/ast:ast-go
     (compile-go ast env))
    (clysm/compiler/ast:ast-catch
     (compile-catch ast env))
    (clysm/compiler/ast:ast-throw
     (compile-throw ast env))
    (clysm/compiler/ast:ast-unwind-protect
     (compile-unwind-protect ast env))
    ;; Exception handling (001-control-structure-extension US4)
    (clysm/compiler/ast:ast-handler-case
     (compile-handler-case ast env))
    ;; Special variable declarations (T022-T024)
    (clysm/compiler/ast:ast-defvar
     (compile-defvar ast env))
    (clysm/compiler/ast:ast-defparameter
     (compile-defparameter ast env))
    ;; Feature 038: Constant definitions
    (clysm/compiler/ast:ast-defconstant
     (compile-defconstant ast env))
    ;; Macro introspection (016-macro-system T048, 042-advanced-defmacro)
    (clysm/compiler/ast:ast-macroexpand-1
     (compile-macroexpand-1 ast env))
    (clysm/compiler/ast:ast-macroexpand
     (compile-macroexpand ast env))
    (clysm/compiler/ast:ast-macro-function
     (compile-macro-function ast env))
    ;; Multiple values (025-multiple-values)
    (clysm/compiler/ast:ast-values
     (compile-values ast env))
    (clysm/compiler/ast:ast-multiple-value-bind
     (compile-multiple-value-bind ast env))
    (clysm/compiler/ast:ast-multiple-value-list
     (compile-multiple-value-list ast env))
    (clysm/compiler/ast:ast-nth-value
     (compile-nth-value ast env))
    (clysm/compiler/ast:ast-values-list
     (compile-values-list ast env))
    (clysm/compiler/ast:ast-multiple-value-prog1
     (compile-multiple-value-prog1 ast env))
    (clysm/compiler/ast:ast-multiple-value-call
     (compile-multiple-value-call ast env))
    ;; CLOS (026-clos-foundation)
    (clysm/compiler/ast:ast-defclass
     (compile-defclass ast env))
    (clysm/compiler/ast:ast-make-instance
     (compile-make-instance ast env))
    (clysm/compiler/ast:ast-defmethod
     (compile-defmethod ast env))
    ;; FFI (027-complete-ffi)
    (clysm/compiler/ast:ast-ffi-call
     (compile-ffi-call ast env))
    (clysm/compiler/ast:ast-call-host
     (compile-call-host ast env))))

;;; ============================================================
;;; FFI Call Compilation (027-complete-ffi)
;;; ============================================================

(defun compile-ffi-call (ast env)
  "Compile an FFI function call to Wasm instructions.
   This generates:
   1. Compile and marshal each argument from Lisp to Wasm type
   2. Call the imported function by its assigned function index
   3. Unmarshal the result from Wasm to Lisp type (or produce NIL for :void)

   Constitution I compliance: Uses WasmGC types only, no linear memory."
  (let* ((decl (clysm/compiler/ast:ast-ffi-call-declaration ast))
         (args (clysm/compiler/ast:ast-ffi-call-arguments ast))
         (param-types (clysm/ffi:ffd-param-types decl))
         (return-type (clysm/ffi:ffd-return-type decl))
         (func-index (or (clysm/ffi:ffd-func-index decl) 0))
         (result '()))
    ;; Step 1: Compile and marshal each argument
    (loop for arg in args
          for param-type in param-types
          do
             ;; Compile the argument expression
             (let ((arg-instrs (compile-to-instructions arg env)))
               (dolist (instr arg-instrs)
                 (push instr result)))
             ;; Add marshalling instructions (Lisp -> Wasm)
             (let ((marshal-instrs (clysm/ffi:marshal-to-wasm param-type)))
               (when marshal-instrs
                 (if (atom marshal-instrs)
                     ;; Single instruction like 'i31.get_s
                     (push (list marshal-instrs) result)
                     ;; List of instructions
                     (dolist (instr marshal-instrs)
                       (push (if (listp instr) instr (list instr)) result))))))
    ;; Step 2: Call the imported function
    ;; 001-numeric-functions: Use :call-import to distinguish from local :call
    (push (list :call-import func-index) result)
    ;; Step 3: Unmarshal return value
    (if (eq return-type :void)
        ;; Void functions return NIL
        (push '(:ref.null :none) result)
        ;; Non-void: unmarshal the result
        (let ((unmarshal-instrs (clysm/ffi:marshal-from-wasm return-type)))
          (when unmarshal-instrs
            (if (atom unmarshal-instrs)
                (push (list unmarshal-instrs) result)
                (dolist (instr unmarshal-instrs)
                  (push (if (listp instr) instr (list instr)) result))))))
    (nreverse result)))

(defun compile-call-host (ast env)
  "Compile a dynamic host function call (ffi:call-host) to Wasm instructions.
   This generates:
   1. Evaluate the function name expression (string) → externref
   2. Compile each argument to anyref
   3. Create an anyref array with the arguments → externref
   4. Call the $ffi_call_host_dynamic import
   5. Convert result from externref to anyref

   The dynamic dispatch is handled by the host environment."
  (let* ((func-name (clysm/compiler/ast:ast-call-host-function-name ast))
         (args (clysm/compiler/ast:ast-call-host-arguments ast))
         (num-args (length args))
         (result '()))
    ;; Step 1: Compile function name and convert to externref
    (dolist (instr (compile-to-instructions func-name env))
      (push instr result))
    ;; Convert the string (anyref) to externref for host
    (push '(:extern.convert_any) result)

    ;; Step 2: Compile arguments and create array
    (if (zerop num-args)
        ;; No arguments - push null externref
        (push '(:ref.null :extern) result)
        (progn
          ;; Compile each argument (result is anyref)
          (dolist (arg args)
            (dolist (instr (compile-to-instructions arg env))
              (push instr result)))
          ;; Create anyref array from stack values
          (push `(:array.new_fixed ,clysm/compiler/codegen/gc-types:+type-anyref-array+ ,num-args) result)
          ;; Convert array to externref
          (push '(:extern.convert_any) result)))

    ;; Step 3: Call the dynamic dispatch import
    ;; The import index is assigned during module compilation
    ;; Use a placeholder that will be resolved during linking
    (push '(:call $ffi_call_host_dynamic) result)

    ;; Step 4: Convert result from externref to anyref
    (push '(:any.convert_extern) result)

    (nreverse result)))

;;; ============================================================
;;; Literal Compilation
;;; ============================================================

(defun compile-literal (ast)
  "Compile a literal value to Wasm instructions."
  (let ((value (clysm/compiler/ast:ast-literal-value ast))
        (type (clysm/compiler/ast:ast-literal-literal-type ast)))
    (case type
      (:fixnum
       ;; Fixnums are represented as i31ref (T047)
       ;; i32.const value, ref.i31
       (list (list :i32.const value)
             :ref.i31))
      (:character
       ;; Characters are represented as i31ref with Unicode codepoint (008-character-string)
       ;; Same as fixnum, but semantically different type
       (list (list :i32.const (char-code value))
             :ref.i31))
      (:nil
       ;; NIL is represented as ref.null (null reference)
       (list '(:ref.null :none)))
      (:t
       ;; T is represented as non-null (use i31ref of 1)
       (list '(:i32.const 1)
             :ref.i31))
      (:string
       ;; Strings are UTF-8 byte arrays (008-character-string)
       ;; Use array.new_fixed to create array from bytes on stack
       (compile-string-literal value))
      (:quoted
       ;; Quoted values - handle lists, symbols, and atoms
       (cond
         ;; NIL
         ((null value)
          (list '(:ref.null :none)))
         ;; List - build cons structure
         ((listp value)
          (compile-quoted-list value))
         ;; For symbols, represent as i31ref of symbol hash (placeholder)
         (t
          (let ((hash (logand (sxhash value) #x3FFFFFFF)))  ; 30-bit for i31ref
            (list (list :i32.const hash)
                  :ref.i31)))))
      ;; Numeric tower types (010-numeric-tower)
      (:bignum
       (compile-bignum-literal value))
      (:ratio
       (compile-ratio-literal value))
      (:float
       (compile-float-literal value))
      (:complex
       (compile-complex-literal value))
      ;; Keyword literals (001-wasm-local-binding)
      ;; Keywords like :local.set, :local.tee are preserved as symbol values.
      ;; They're represented as i31ref of symbol hash (same as quoted symbols).
      ;; This enables backquote expressions with Wasm instruction keywords to
      ;; compile correctly during self-hosting.
      (:keyword
       (let ((hash (logand (sxhash value) #x3FFFFFFF)))  ; 30-bit for i31ref
         (list (list :i32.const hash)
               :ref.i31)))
      (otherwise
       (error "Unsupported literal type: ~A" type)))))

(defun compile-string-literal (string)
  "Compile a string literal to Wasm instructions.
   Creates a UTF-8 byte array using array.new_fixed.
   Stack: [] -> [ref $string]"
  (let ((bytes (clysm/lib/utf8:string-to-utf8-octets string))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    (if (zerop (length bytes))
        ;; Empty string: just create empty array
        (list (list :array.new_fixed string-type 0))
        ;; Non-empty: push each byte then create array
        (with-instruction-collector
          (loop for byte across bytes
                do (emit :i32.const byte))
          (emit :array.new_fixed string-type (length bytes))))))

(defun compile-quoted-list (lst &optional env)
  "Compile a quoted list to a cons structure.
   Builds the list from right to left, creating cons cells.
   ENV is passed through for AST node compilation (001-quasiquote-local-vars)."
  (if (null lst)
      '((:ref.null :none))
      (with-instruction-collector
        ;; Compile car element
        (emit* (compile-quoted-element (car lst) env))
        ;; Compile cdr (rest of list)
        (emit* (compile-quoted-list (cdr lst) env))
        ;; Create cons cell
        (emit :struct.new clysm/compiler/codegen/gc-types:+type-cons+))))

(defun compile-quoted-element (elem &optional env)
  "Compile a single quoted element to Wasm instructions.
   Handles AST nodes from quasiquote expansion (001-quasiquote-local-vars).
   ENV is the compilation environment, needed for compiling AST nodes."
  (cond
    ((null elem) '((:ref.null :none)))
    ((integerp elem)
     ;; Handle fixnum vs bignum (010-numeric-tower)
     (if (clysm/compiler/ast:i31-range-p elem)
         (list (list :i32.const elem) :ref.i31)
         (compile-bignum-literal elem)))
    ((typep elem 'ratio) (compile-ratio-literal elem))
    ((floatp elem) (compile-float-literal elem))
    ((complexp elem) (compile-complex-literal elem))
    ;; Character literals: encode as i31ref using char-code (001-char-literal-compile)
    ((characterp elem)
     (list (list :i32.const (char-code elem)) :ref.i31))
    ((listp elem) (compile-quoted-list elem env))
    ((symbolp elem)
     (let ((hash (logand (sxhash elem) #x3FFFFFFF)))
       (list (list :i32.const hash) :ref.i31)))
    ;; AST nodes from quasiquote expansion (001-quasiquote-local-vars)
    ;; When quasiquote is expanded, unquoted forms become AST nodes.
    ;; We need to compile these as expressions, not as quoted literals.
    ((clysm/compiler/ast:ast-var-ref-p elem)
     ;; Variable reference from unquote: ,var
     (if env
         (compile-var-ref elem env)
         ;; Without env, return placeholder (for foundational tests)
         (list '(:ref.null :none))))
    ((clysm/compiler/ast:ast-call-p elem)
     ;; Function call from unquote: ,(+ 1 2)
     (if env
         (compile-form elem env)
         ;; Without env, return placeholder (for foundational tests)
         (list '(:ref.null :none))))
    ((clysm/compiler/ast:ast-node-p elem)
     ;; Other AST node types - compile as generic form
     (if env
         (compile-form elem env)
         (list '(:ref.null :none))))
    (t (error "Cannot compile quoted element: ~A" elem))))

;;; ============================================================
;;; Numeric Tower Literal Compilation (010-numeric-tower)
;;; ============================================================

(defun integer-to-limbs (n)
  "Convert a non-negative integer to a list of 32-bit limbs (little-endian).
   Returns (values limbs sign) where sign is 0 for non-negative, 1 for negative."
  (let ((sign (if (minusp n) 1 0))
        (abs-n (abs n))
        (limbs '()))
    (if (zerop abs-n)
        (values '(0) 0)
        (progn
          (loop while (plusp abs-n)
                do (push (logand abs-n #xFFFFFFFF) limbs)
                   (setf abs-n (ash abs-n -32)))
          (values (nreverse limbs) sign)))))

(defun unsigned-to-signed-i32 (n)
  "Convert an unsigned 32-bit integer to its signed representation.
   Values >= 2^31 become negative numbers in two's complement."
  (if (>= n (expt 2 31))
      (- n (expt 2 32))
      n))

(defun compile-bignum-literal (value)
  "Compile a bignum literal to Wasm instructions.
   Creates a $bignum struct with sign and limb array.
   Stack: [] -> [ref $bignum]"
  (multiple-value-bind (limbs sign) (integer-to-limbs value)
    (let ((limb-array-type clysm/compiler/codegen/gc-types:+type-limb-array+)
          (bignum-type clysm/compiler/codegen/gc-types:+type-bignum+)
          (result '()))
      ;; Push sign
      (push (list :i32.const sign) result)
      ;; Push each limb value, then create the array
      ;; Convert unsigned limb values to signed i32 for proper LEB128 encoding
      (dolist (limb limbs)
        (push (list :i32.const (unsigned-to-signed-i32 limb)) result))
      ;; Create limb array with array.new_fixed
      (push (list :array.new_fixed limb-array-type (length limbs)) result)
      ;; Create bignum struct (sign, limbs)
      (push (list :struct.new bignum-type) result)
      (nreverse result))))

(defun compile-ratio-literal (value)
  "Compile a ratio literal to Wasm instructions.
   Creates a $ratio struct with numerator and denominator.
   Stack: [] -> [ref $ratio]"
  (let ((num (numerator value))
        (den (denominator value))
        (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+))
    (with-instruction-collector
      ;; Compile numerator (may be fixnum or bignum)
      (emit* (if (clysm/compiler/ast:i31-range-p num)
                 (list (list :i32.const num) :ref.i31)
                 (compile-bignum-literal num)))
      ;; Compile denominator (always positive, may be fixnum or bignum)
      (emit* (if (clysm/compiler/ast:i31-range-p den)
                 (list (list :i32.const den) :ref.i31)
                 (compile-bignum-literal den)))
      ;; Create ratio struct
      (emit :struct.new ratio-type))))

(defun compile-float-literal (value)
  "Compile a float literal to Wasm instructions.
   Creates a $float struct with f64 value.
   Stack: [] -> [ref $float]"
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+))
    (list (list :f64.const (coerce value 'double-float))
          (list :struct.new float-type))))

(defun compile-complex-literal (value)
  "Compile a complex literal to Wasm instructions.
   Creates a $complex struct with real and imaginary parts.
   Stack: [] -> [ref $complex]"
  (let ((real-part (realpart value))
        (imag-part (imagpart value))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (with-instruction-collector
      ;; Compile real part
      (emit* (compile-numeric-literal-part real-part))
      ;; Compile imaginary part
      (emit* (compile-numeric-literal-part imag-part))
      ;; Create complex struct
      (emit :struct.new complex-type))))

(defun compile-numeric-literal-part (value)
  "Compile a numeric value that can be any real type.
   Used for complex number parts."
  (cond
    ((and (integerp value) (clysm/compiler/ast:i31-range-p value))
     (list (list :i32.const value) :ref.i31))
    ((integerp value)
     (compile-bignum-literal value))
    ((typep value 'ratio)
     (compile-ratio-literal value))
    ((floatp value)
     (compile-float-literal value))
    (t (error "Cannot compile numeric part: ~A" value))))

;;; ============================================================
;;; Variable Reference Compilation (T048)
;;; ============================================================

(defun compile-var-ref (ast env)
  "Compile a variable reference.
   Handles constants (T019), locals, captured variables from closures, special variables, and globals.
   Per FR-004: Constants are substituted at compile-time with i32.const."
  (let* ((name (clysm/compiler/ast:ast-var-ref-name ast))
         (local-idx (env-lookup-local env name)))
    ;; Check for compile-time constant first (T019-T020)
    (multiple-value-bind (const-value found-p)
        (clysm/compiler:lookup-constant name)
      (when found-p
        (return-from compile-var-ref
          (list (list :i32.const const-value)))))
    (cond
      ;; Local variable
      (local-idx
       (list (list :local.get local-idx)))
      ;; Captured variable from closure environment
      ((env-lookup-captured env name)
       (compile-captured-var-access name env))
      ;; Special variable (T037)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-var-ref name))
      ;; Unknown variable
      (t
       (error "Unbound variable: ~A" name)))))

(defun compile-special-var-ref (name)
  "Compile a reference to a special variable (T037).
   Generates code to read the symbol's current value.
   Pattern: global.get <index>, struct.get $symbol $value"
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    `((:global.get ,global-idx)
      (:struct.get ,symbol-type 1))))

(defun env-lookup-captured (env name)
  "Look up a captured variable position."
  (cdr (assoc name (cenv-captured-vars env))))

(defun compile-captured-var-access (name env)
  "Generate instructions to access a captured variable from the closure's env.
   The closure is in local 0 ($closure).
   The env is a cons-list: (cons var0 (cons var1 (cons var2 nil)))
   To get var at position N, we need N cdrs followed by a car."
  (let* ((position (env-lookup-captured env name))
         (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Get the closure (local 0)
      (emit '(:local.get 0))
      ;; Cast to closure type
      (emit :ref.cast closure-type)
      ;; Get the env field (field index 4)
      (emit :struct.get closure-type 4)
      ;; Navigate the cons-list: N times cdr, then car
      (dotimes (i position)
        ;; Cast to cons type
        (emit :ref.cast cons-type)
        ;; Get cdr (field index 1)
        (emit :struct.get cons-type 1))
      ;; Cast final position to cons and get car (field index 0)
      (emit :ref.cast cons-type)
      (emit :struct.get cons-type 0))))

;;; ============================================================
;;; Function Call Compilation (T049-T052)
;;; ============================================================

(defun compile-call (ast env)
  "Compile a function call."
  (let ((function (clysm/compiler/ast:ast-call-function ast))
        (args (clysm/compiler/ast:ast-call-arguments ast)))
    ;; Check for special forms and primitives
    (cond
      ;; funcall special form
      ((and (symbolp function) (eq function 'funcall))
       (compile-funcall args env))
      ;; Feature 001-io-list-runtime: Runtime library functions
      ;; Check runtime function table before inline codegen
      ((and (symbolp function) (runtime-function-p function))
       (compile-runtime-call function args env))
      ;; Layer 1 Primitives: Check registry first (001-runtime-library-system)
      ;; Registered primitives are compiled using their Wasm emitters directly
      ((and (symbolp function) (clysm::registered-primitive-p function))
       (compile-primitive-call function args env))
      ;; Legacy primitive operators (to be migrated to primitives registry)
      ;; Use symbol-name comparison to handle symbols from different packages
      ;; (needed for %setf-* primitives which may come from setf-expanders package)
      ((and (symbolp function)
            (member function '(+ - * / < > <= >= = /= truncate
                               ;; Rounding functions (001-division-rounding-primitives)
                               floor ceiling round ffloor fceiling fround
                               ;; Increment/decrement primitives (001-arithmetic-primitives)
                               1- 1+
                               ;; Cons/list operations (006-cons-list-ops)
                               cons car cdr list
                               consp null not atom listp
                               ;; Equality predicates (024-equality-predicates)
                               eq eql equal equalp
                               rplaca rplacd
                               first second third fourth fifth
                               sixth seventh eighth ninth tenth
                               rest nth nthcdr
                               ;; cXXr accessors (043-self-hosting-blockers)
                               caar cadr cdar cddr
                               caaar caadr cadar caddr
                               cdaar cdadr cddar cdddr
                               ;; Sequence functions (007-sequence-functions)
                               length append reverse nreverse last butlast copy-list
                               mapcar mapc maplist reduce
                               find find-if position position-if
                               remove remove-if remove-if-not
                               substitute substitute-if
                               count count-if
                               member member-if member-if-not
                               assoc assoc-if rassoc rassoc-if
                               ;; Alist construction (001-ansi-list-ops)
                               acons pairlis copy-alist
                               ;; Set operations (043-self-hosting-blockers, 001-ansi-list-ops)
                               adjoin union intersection set-difference subsetp
                               every some notany notevery
                               ;; Character functions (008-character-string)
                               char-code code-char
                               char= char/= char< char> char<= char>=
                               char-equal char-lessp char-greaterp
                               char-not-lessp char-not-greaterp
                               char-upcase char-downcase
                               characterp alpha-char-p digit-char-p
                               alphanumericp upper-case-p lower-case-p
                               ;; Extended character functions (001-ansi-char-functions)
                               graphic-char-p standard-char-p both-case-p
                               char-name name-char digit-char char-int
                               stringp char schar
                               string= string/= string< string> string<= string>=
                               string-equal string-lessp string-greaterp
                               string-not-lessp string-not-greaterp string-not-equal
                               make-string string string-upcase string-downcase string-capitalize
                               ;; String trimming functions (Phase 16B)
                               string-trim string-left-trim string-right-trim
                               nstring-upcase nstring-downcase nstring-capitalize
                               subseq concatenate
                               ;; String output (001-numeric-format)
                               write-to-string
                               ;; Numeric accessors (019-numeric-accessors)
                               numerator denominator
                               ;; ANSI CL Type Predicates (023-type-predicates)
                               integerp floatp rationalp complexp numberp
                               symbolp functionp
                               ;; Symbol accessors (043-self-hosting-blockers)
                               symbol-name keywordp
                               ;; ANSI CL Numeric Predicates (023-type-predicates)
                               zerop plusp minusp oddp evenp
                               ;; ANSI CL Signum (023-type-predicates)
                               signum
                               ;; Hash table operations (043-self-hosting-blockers)
                               make-hash-table gethash puthash remhash maphash
                               hash-table-count hash-table-size clrhash
                               ;; List operations for LOOP support (043-self-hosting-blockers)
                               nconc
                               ;; Bitwise operations for LEB128 (043-self-hosting-blockers)
                               logand logior logxor lognot ash mod rem
                               ;; ANSI Numeric Functions (001-numeric-functions)
                               ;; Basic arithmetic
                               abs max min gcd lcm
                               ;; Bitwise extensions
                               logcount integer-length
                               ;; Phase 14B: Bit Testing Functions (001-numeric-predicates)
                               logbitp logtest
                               ;; Phase 14B: Byte Specifier Functions (001-numeric-predicates)
                               byte byte-size byte-position
                               ;; Phase 14B: Byte Operations (001-numeric-predicates)
                               ldb dpb mask-field deposit-field
                               ;; Complex number operations
                               complex realpart imagpart conjugate phase
                               ;; Trigonometric functions
                               sin cos tan asin acos atan
                               ;; Mathematical functions
                               exp log sqrt expt
                               ;; Hyperbolic functions
                               sinh cosh tanh asinh acosh atanh
                               ;; Numeric type conversion (001-numeric-functions)
                               float rational rationalize parse-integer
                               ;; Array setf primitive (043-self-hosting-blockers)
                               %setf-aref
                               ;; Property list operations (043-self-hosting-blockers)
                               getf %setf-getf
                               ;; Error signaling (043-self-hosting-blockers)
                               error
                               ;; Array and symbol creation (043-self-hosting-blockers)
                               make-array gensym
                               ;; Type predicate (043-self-hosting-blockers)
                               typep
                               ;; Vector and I/O operations (043-self-hosting-blockers)
                               vector-push-extend write-byte
                               ;; List utilities (043-self-hosting-blockers)
                               endp list*
                               ;; Function application (043-self-hosting-blockers)
                               apply funcall
                               ;; Array/Sequence primitives (001-ansi-array-primitives)
                               ;; Note: %setf-aref already in Feature 043 list above
                               aref svref elt coerce
                               ;; Array operations (001-ansi-array-ops)
                               array-rank array-dimension array-dimensions
                               array-total-size array-row-major-index
                               row-major-aref %setf-row-major-aref
                               adjustable-array-p adjust-array
                               ;; Sequence operations (001-ansi-sequence-operations)
                               copy-seq
                               %setf-svref %setf-schar %setf-elt
                               ;; Package operations (001-global-variable-defs)
                               find-package intern
                               ;; CLOS instance creation (001-make-instance-primitive)
                               make-instance*
                               ;; CLOS primitives (001-m3-clos-primitives)
                               ;; HyperSpec: resources/HyperSpec/Body/f_slt_va.htm
                               slot-value* set-slot-value*
                               ;; HyperSpec: resources/HyperSpec/Body/f_mk_ins.htm
                               standard-instance-p
                               ;; MOP primitives for DEFSTRUCT predicate support
                               instance-class class-name
                               ;; Runtime registration functions (return nil, no-op for Wasm)
                               register-structure-class define-class*
                               register-setf-expander make-slot-accessor-setf-expander
                               ;; I/O print primitives (001-io-print-primitives)
                               ;; HyperSpec: resources/HyperSpec/Body/f_wr_pr.htm
                               print prin1 princ write
                               ;; HyperSpec: resources/HyperSpec/Body/f_terpri.htm
                               terpri
                               ;; HyperSpec: resources/HyperSpec/Body/f_format.htm
                               format)
                    :test (lambda (fn sym)
                            (string= (symbol-name fn) (symbol-name sym)))))
       (compile-primitive-call function args env))
      ;; Local function (from flet/labels)
      ((and (symbolp function) (env-lookup-local-function env function))
       (compile-local-function-call function args env))
      ;; Regular function call
      (t
       (compile-regular-call function args env)))))

(defun env-lookup-local-function (env name)
  "Look up a local function's local variable index or :captured marker."
  (cdr (assoc name (cenv-local-functions env))))

(defun compile-local-function-call (function args env)
  "Compile a call to a local function (from flet/labels).
   The function is stored as a closure in a local variable or captured env.
   Uses :return_call_ref for tail calls (TCO), :call_ref otherwise."
  (let* ((local-func-info (env-lookup-local-function env function))
         (arity (length args))
         (arg-env (env-with-non-tail env))
         (closure-type clysm/compiler/codegen/gc-types:+type-closure+))
    (with-instruction-collector
      ;; Get the closure - either from local or from captured env
      (if (eq local-func-info :captured)
          ;; Captured in closure env - use captured-var access
          (emit* (compile-captured-var-access function env))
          ;; In a local variable
          (emit :local.get local-func-info))
      ;; Duplicate for the first parameter (closure as self-reference)
      (let ((closure-local (cenv-local-counter env)))
        (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
        ;; Save closure to temp local
        (emit :local.set closure-local)
        ;; Push closure as first argument (self reference)
        (emit :local.get closure-local)
        ;; Push all call arguments (not in tail position)
        (dolist (arg args)
          (emit* (compile-to-instructions arg arg-env)))
        ;; Get closure and extract code field
        (emit :local.get closure-local)
        ;; Cast to closure type
        (emit :ref.cast closure-type)
        ;; Get the appropriate code field based on arity
        (let ((code-field (case arity
                            (0 0)   ; code_0
                            (1 1)   ; code_1
                            (2 2)   ; code_2
                            (t 3)))) ; code_N
          (emit :struct.get closure-type code-field))
        ;; Cast funcref to specific function type and call
        ;; Use return_call_ref for tail calls
        (let ((func-type (case arity
                           (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                           (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                           (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                           (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                           (t clysm/compiler/codegen/gc-types:+type-func-n+))))
          (emit :ref.cast func-type)
          (if (cenv-in-tail-position env)
              (emit :return_call_ref func-type)
              (emit :call_ref func-type)))))))

(defun compile-primitive-call (op args env)
  "Compile a primitive operation.
   Arguments to primitives are NOT in tail position.
   First tries hash-table dispatch via dispatch-primitive,
   then falls back to string-match cond and case statement."
  ;; Clear tail position for all arguments - primitive ops aren't tail calls
  (let ((env (env-with-non-tail env))
        (op-name (when (symbolp op) (symbol-name op))))
    ;; Try dispatch table first for registered primitives (002-primitive-dispatch-table)
    (or (clysm/compiler/codegen/primitive-dispatch:dispatch-primitive op args env)
        ;; Fall back to existing implementations
        ;; Handle %setf-* primitives by symbol name (for cross-package matching)
        ;; These primitives may come from setf-expanders package but need to match here
        (cond
      ((string= op-name "%SETF-AREF") (compile-setf-aref args env))
      ((string= op-name "%SETF-SVREF") (compile-setf-aref args env))
      ((string= op-name "%SETF-SCHAR") (compile-setf-schar args env))
      ((string= op-name "%SETF-ELT") (compile-setf-elt args env))
      ((string= op-name "%SETF-GETF") (compile-setf-getf args env))
      ;; Feature 001-ansi-array-ops: Row-major setf
      ((string= op-name "%SETF-ROW-MAJOR-AREF") (compile-setf-row-major-aref args env))
      ;; Feature 001-ansi-sequence-operations: Match cross-package symbols
      ((string= op-name "COPY-SEQ") (compile-copy-seq args env))
      ;; Feature 001-make-instance-primitive: CLOS instance creation
      ((string= op-name "MAKE-INSTANCE*") (compile-make-instance* args env))
      ;; Feature 001-m3-clos-primitives: CLOS slot access
      ;; HyperSpec: resources/HyperSpec/Body/f_slt_va.htm
      ((string= op-name "SLOT-VALUE*") (compile-slot-value* args env))
      ((string= op-name "SET-SLOT-VALUE*") (compile-set-slot-value* args env))
      ((string= op-name "STANDARD-INSTANCE-P") (compile-standard-instance-p args env))
      ;; MOP primitives for DEFSTRUCT predicate support
      ((string= op-name "INSTANCE-CLASS") (compile-instance-class args env))
      ((string= op-name "CLASS-NAME") (compile-class-name args env))
      ;; Runtime registration functions - return NIL (no-op for Wasm)
      ;; These are setup calls for the host runtime, not for Wasm execution
      ((string= op-name "REGISTER-STRUCTURE-CLASS") (list '(:global.get 0)))
      ((string= op-name "DEFINE-CLASS*") (list '(:global.get 0)))
      ((string= op-name "REGISTER-SETF-EXPANDER") (list '(:global.get 0)))
      ((string= op-name "MAKE-SLOT-ACCESSOR-SETF-EXPANDER") (list '(:global.get 0)))
      (t
       ;; Fall through to main case for all other primitives
       (case op
    ;; Arithmetic operators (T049-T052)
    (+  (compile-arithmetic-op :i32.add args env 0))
    (-  (if (= 1 (length args))
            (compile-unary-minus (first args) env)
            (compile-arithmetic-op :i32.sub args env nil)))
    (*  (compile-arithmetic-op :i32.mul args env 1))
    (/  (compile-arithmetic-op :i32.div_s args env nil))
    (truncate (compile-truncate args env))
    (mod (compile-arithmetic-op :i32.rem_s args env nil))
    (rem (compile-arithmetic-op :i32.rem_s args env nil))
    ;; Rounding functions (001-division-rounding-primitives)
    (floor (compile-floor args env))
    (ceiling (compile-ceiling args env))
    (round (compile-round args env))
    (ffloor (compile-ffloor args env))
    (fceiling (compile-fceiling args env))
    (fround (compile-fround args env))
    ;; Increment/decrement primitives (001-arithmetic-primitives)
    (1- (compile-1- args env))
    (1+ (compile-1+ args env))
    ;; ANSI Numeric Functions (001-numeric-functions)
    (abs (compile-abs args env))
    (max (compile-max args env))
    (min (compile-min args env))
    (gcd (compile-gcd args env))
    (lcm (compile-lcm args env))
    ;; Feature 043: Bitwise operators for LEB128 encoding
    (logand (compile-arithmetic-op :i32.and args env -1))
    (logior (compile-arithmetic-op :i32.or args env 0))
    (logxor (compile-arithmetic-op :i32.xor args env 0))
    (lognot (compile-lognot args env))
    (ash (compile-ash args env))
    ;; ANSI Numeric Functions: Bitwise (001-numeric-functions)
    (logcount (compile-logcount args env))
    (integer-length (compile-integer-length args env))
    ;; Phase 14B: Bit Testing Functions (001-numeric-predicates)
    (logbitp (compile-logbitp args env))
    (logtest (compile-logtest args env))
    ;; Phase 14B: Byte Specifier Functions (001-numeric-predicates)
    (byte (compile-byte args env))
    (byte-size (compile-byte-size args env))
    (byte-position (compile-byte-position args env))
    ;; Phase 14B: Byte Operations (001-numeric-predicates)
    (ldb (compile-ldb args env))
    (dpb (compile-dpb args env))
    (mask-field (compile-mask-field args env))
    (deposit-field (compile-deposit-field args env))
    ;; Comparison operators (T053)
    (<  (compile-comparison-op :i32.lt_s args env))
    (>  (compile-comparison-op :i32.gt_s args env))
    (<= (compile-comparison-op :i32.le_s args env))
    (>= (compile-comparison-op :i32.ge_s args env))
    (=  (compile-comparison-op :i32.eq args env))
    (/= (compile-not-equal args env))
    ;; Cons/list operations (006-cons-list-ops)
    (cons (compile-cons args env))
    (car (compile-car args env))
    (cdr (compile-cdr args env))
    (list (compile-list args env))
    ;; Type predicates
    (consp (compile-consp args env))
    (null (compile-null args env))
    (not (compile-not args env))
    (atom (compile-atom args env))
    (listp (compile-listp args env))
    ;; Equality predicates (024-equality-predicates)
    (eq (compile-eq args env))
    (eql (compile-eql args env))
    (equal (compile-equal args env))
    (equalp (compile-equalp args env))
    ;; ANSI CL Type Predicates (023-type-predicates)
    (integerp (compile-integerp args env))
    (floatp (compile-floatp args env))
    (rationalp (compile-rationalp args env))
    (complexp (compile-complexp args env))
    (numberp (compile-numberp args env))
    (symbolp (compile-symbolp args env))
    (functionp (compile-functionp args env))
    ;; ANSI CL Numeric Predicates (023-type-predicates)
    (zerop (compile-zerop args env))
    (plusp (compile-plusp args env))
    (minusp (compile-minusp args env))
    (oddp (compile-oddp args env))
    (evenp (compile-evenp args env))
    ;; ANSI CL Signum (023-type-predicates)
    (signum (compile-signum args env))
    ;; Destructive modification
    (rplaca (compile-rplaca args env))
    (rplacd (compile-rplacd args env))
    ;; List accessors
    (first (compile-car args env))
    (rest (compile-cdr args env))
    (second (compile-nth-accessor 1 args env))
    (third (compile-nth-accessor 2 args env))
    (fourth (compile-nth-accessor 3 args env))
    (fifth (compile-nth-accessor 4 args env))
    (sixth (compile-nth-accessor 5 args env))
    (seventh (compile-nth-accessor 6 args env))
    (eighth (compile-nth-accessor 7 args env))
    (ninth (compile-nth-accessor 8 args env))
    (tenth (compile-nth-accessor 9 args env))
    (nth (compile-nth args env))
    (nthcdr (compile-nthcdr args env))
    ;; cXXr accessors (043-self-hosting-blockers)
    (caar (compile-caar args env))
    (cadr (compile-cadr args env))
    (cdar (compile-cdar args env))
    (cddr (compile-cddr args env))
    (caaar (compile-caaar args env))
    (caadr (compile-caadr args env))
    (cadar (compile-cadar args env))
    (caddr (compile-caddr args env))
    (cdaar (compile-cdaar args env))
    (cdadr (compile-cdadr args env))
    (cddar (compile-cddar args env))
    (cdddr (compile-cdddr args env))
    ;; Symbol accessors (043-self-hosting-blockers)
    (symbol-name (compile-symbol-name args env))
    (keywordp (compile-keywordp args env))
    ;; Sequence functions (007-sequence-functions)
    (length (compile-length args env))
    (append (compile-append args env))
    (reverse (compile-reverse args env))
    (nreverse (compile-nreverse args env))
    (last (compile-last args env))
    (butlast (compile-butlast args env))
    (copy-list (compile-copy-list args env))
    ;; Higher-order sequence functions
    (mapcar (compile-mapcar args env))
    (mapc (compile-mapc args env))
    (maplist (compile-maplist args env))
    (reduce (compile-reduce args env))
    ;; Search, filter, membership, and association functions are dispatched
    ;; via *runtime-function-table* to runtime library - no inline codegen entries
    ;; (find, find-if, position, position-if, remove, remove-if, remove-if-not,
    ;;  substitute, substitute-if, count, count-if, member, member-if, member-if-not,
    ;;  assoc, assoc-if, rassoc, rassoc-if)
    ;; Alist construction (001-ansi-list-ops)
    (acons (compile-acons args env))
    (pairlis (compile-pairlis args env))
    (copy-alist (compile-copy-alist args env))
    ;; Set operations (043-self-hosting-blockers, 001-ansi-list-ops)
    (adjoin (compile-adjoin args env))
    (union (compile-union args env))
    (intersection (compile-intersection args env))
    (set-difference (compile-set-difference args env))
    (subsetp (compile-subsetp args env))
    ;; Quantifier predicates
    (every (compile-every args env))
    (some (compile-some args env))
    (notany (compile-notany args env))
    (notevery (compile-notevery args env))
    ;; Character functions (008-character-string)
    (char-code (compile-char-code args env))
    (code-char (compile-code-char args env))
    (char= (compile-char= args env))
    (char/= (compile-char/= args env))
    (char< (compile-char< args env))
    (char> (compile-char> args env))
    (char<= (compile-char<= args env))
    (char>= (compile-char>= args env))
    (char-equal (compile-char-equal args env))
    (char-lessp (compile-char-lessp args env))
    (char-greaterp (compile-char-greaterp args env))
    (char-not-lessp (compile-char-not-lessp args env))
    (char-not-greaterp (compile-char-not-greaterp args env))
    (char-upcase (compile-char-upcase args env))
    (char-downcase (compile-char-downcase args env))
    (characterp (compile-characterp args env))
    (alpha-char-p (compile-alpha-char-p args env))
    (digit-char-p (compile-digit-char-p args env))
    (alphanumericp (compile-alphanumericp args env))
    (upper-case-p (compile-upper-case-p args env))
    (lower-case-p (compile-lower-case-p args env))
    ;; Extended character functions (001-ansi-char-functions)
    (graphic-char-p (compile-graphic-char-p args env))
    (standard-char-p (compile-standard-char-p args env))
    (both-case-p (compile-both-case-p args env))
    (char-name (compile-char-name args env))
    (name-char (compile-name-char args env))
    (digit-char (compile-digit-char args env))
    (char-int (compile-char-int args env))
    ;; String functions (008-character-string)
    (stringp (compile-stringp args env))
    (char (compile-string-char args env))
    (schar (compile-string-char args env))
    ;; String comparison functions
    (string= (compile-string= args env))
    (string/= (compile-string/= args env))
    (string< (compile-string< args env))
    (string> (compile-string> args env))
    (string<= (compile-string<= args env))
    (string>= (compile-string>= args env))
    ;; Case-insensitive string comparison functions
    (string-equal (compile-string-equal args env))
    (string-lessp (compile-string-lessp args env))
    (string-greaterp (compile-string-greaterp args env))
    (string-not-lessp (compile-string-not-lessp args env))
    (string-not-greaterp (compile-string-not-greaterp args env))
    (string-not-equal (compile-string-not-equal args env))
    ;; String generation/conversion functions
    (make-string (compile-make-string args env))
    (string (compile-string args env))
    (string-upcase (compile-string-upcase args env))
    (string-downcase (compile-string-downcase args env))
    (string-capitalize (compile-string-capitalize args env))
    ;; String trimming functions (Phase 16B)
    (string-trim (compile-string-trim args env))
    (string-left-trim (compile-string-left-trim args env))
    (string-right-trim (compile-string-right-trim args env))
    ;; Destructive case conversion (Phase 16B)
    (nstring-upcase (compile-nstring-upcase args env))
    (nstring-downcase (compile-nstring-downcase args env))
    (nstring-capitalize (compile-nstring-capitalize args env))
    ;; Substring and concatenation
    (subseq (compile-subseq args env))
    (concatenate (compile-concatenate args env))
    ;; String output (001-numeric-format)
    (write-to-string (compile-write-to-string args env))
    ;; Numeric accessors (019-numeric-accessors)
    (numerator (compile-numerator args env))
    (denominator (compile-denominator args env))
    ;; ANSI Complex Number Functions (001-numeric-functions)
    (complex (compile-complex args env))
    (realpart (compile-realpart args env))
    (imagpart (compile-imagpart args env))
    (conjugate (compile-conjugate args env))
    (phase (compile-phase args env))
    ;; Trigonometric functions (001-numeric-functions)
    (sin (compile-sin args env))
    (cos (compile-cos args env))
    (tan (compile-tan args env))
    (asin (compile-asin args env))
    (acos (compile-acos args env))
    (atan (compile-atan args env))
    ;; Mathematical functions (001-numeric-functions)
    (exp (compile-exp args env))
    (log (compile-log args env))
    (sqrt (compile-sqrt args env))
    (expt (compile-expt args env))
    ;; Hyperbolic functions (001-numeric-functions)
    (sinh (compile-sinh args env))
    (cosh (compile-cosh args env))
    (tanh (compile-tanh args env))
    (asinh (compile-asinh args env))
    (acosh (compile-acosh args env))
    (atanh (compile-atanh args env))
    ;; Numeric type conversion (001-numeric-functions)
    (float (compile-float args env))
    (rational (compile-rational args env))
    (rationalize (compile-rationalize args env))
    (parse-integer (compile-parse-integer args env))
    ;; Hash table operations (043-self-hosting-blockers)
    (make-hash-table (compile-make-hash-table args env))
    (gethash (compile-gethash args env))
    (puthash (compile-puthash args env))
    (remhash (compile-remhash args env))
    (maphash (compile-maphash args env))
    (hash-table-count (compile-hash-table-count args env))
    (hash-table-size (compile-hash-table-size args env))
    (clrhash (compile-clrhash args env))
    ;; Feature 043: List operations for LOOP support
    (nconc (compile-nconc args env))
    ;; Note: %setf-aref now handled by cond above for cross-package matching
    ;; Feature 043: Property list operations
    (getf (compile-getf args env))
    ;; Note: %setf-getf now handled by cond above for cross-package matching
    ;; Feature 043: Error signaling
    (error (compile-error args env))
    ;; Feature 043: Array and symbol creation
    (make-array (compile-make-array args env))
    (gensym (compile-gensym args env))
    ;; Feature 043: Type predicate
    (typep (compile-typep args env))
    ;; Feature 043: Vector and I/O operations
    (vector-push-extend (compile-vector-push-extend args env))
    (write-byte (compile-write-byte args env))
    ;; Feature 043: List utilities
    (endp (compile-endp args env))
    (list* (compile-list* args env))
    ;; Feature 043: Function application
    (apply (compile-apply args env))
    (funcall (compile-funcall args env))
    ;; Feature 001-ansi-array-primitives: Array/Sequence Primitives
    ;; HyperSpec: resources/HyperSpec/Body/f_aref.htm
    (aref (compile-aref args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_svref.htm
    (svref (compile-svref args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_elt.htm
    (elt (compile-elt args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_coerce.htm
    (coerce (compile-coerce args env))
    ;; Feature 001-ansi-array-ops: ANSI Array Operations (Phase 15C)
    ;; HyperSpec: resources/HyperSpec/Body/f_ar_ran.htm
    (array-rank (compile-array-rank args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_ar_dim.htm
    (array-dimension (compile-array-dimension args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_ar_d_1.htm
    (array-dimensions (compile-array-dimensions args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_ar_tot.htm
    (array-total-size (compile-array-total-size args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_ar_row.htm
    (array-row-major-index (compile-array-row-major-index args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_row_ma.htm
    (row-major-aref (compile-row-major-aref args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_adju_1.htm
    (adjustable-array-p (compile-adjustable-array-p args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_adjust.htm
    (adjust-array (compile-adjust-array args env))
    ;; Feature 001-ansi-sequence-operations: Sequence Operations
    ;; HyperSpec: resources/HyperSpec/Body/f_cp_seq.htm
    (copy-seq (compile-copy-seq args env))
    ;; Feature 001-global-variable-defs: Package Operations (stub implementations)
    ;; HyperSpec: resources/HyperSpec/Body/f_find_p.htm
    (find-package (compile-find-package args env))
    ;; HyperSpec: resources/HyperSpec/Body/f_intern.htm
    (intern (compile-intern args env))
    ;; Note: %setf-* primitives are now handled by cond above for cross-package matching
    ;; I/O functions (print, prin1, princ, write, terpri, format) are dispatched
    ;; via *runtime-function-table* to runtime library - no inline codegen entries
    )))))) ; Close case, t clause of cond, cond, or, let

;;; ============================================================
;;; Numeric Tower Type Dispatch (T013-T015)
;;; ============================================================

(defun emit-numeric-type-p ()
  "Generate instructions to check if a value on stack is any numeric type (T013).
   Expects: anyref on stack
   Returns: i32 (1 if numeric, 0 otherwise)
   Checks: i31ref (fixnum), $bignum, $ratio, $float, $complex"
  `(;; Value is on stack as anyref
    ;; Check if i31ref (fixnum)
    (:block (:result :i32)
      (:block
        ;; Try casting to i31 - if succeeds, it's a fixnum
        (:br_on_cast 1 :anyref :i31 ,clysm/compiler/codegen/gc-types:+type-bignum+))
      ;; Check bignum
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+)))
      ;; Check ratio
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+)))
      ;; Check float
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
      ;; Check complex
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Not a number
      (:i32.const 0)
      (:br 5))  ; exit with 0
    ;; Was a number - return 1
    :drop  ; drop the casted value
    (:i32.const 1)))

(defun emit-get-numeric-type ()
  "Generate instructions to get the numeric type of a value (T014).
   Expects: anyref on stack
   Returns: i32 type code
     0 = not-a-number
     1 = fixnum (i31ref)
     2 = bignum
     3 = ratio
     4 = float
     5 = complex"
  `(;; Duplicate value for testing (original stays on stack)
    ;; We need block-based type dispatch
    (:block (:result :i32)
      ;; Check i31 (fixnum) first
      (:block
        (:block
          (:block
            (:block
              (:block
                ;; Try i31
                (:br_on_cast 0 :anyref :i31))
              ;; Try bignum
              (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+)))
            ;; Try ratio
            (:br_on_cast 2 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+)))
          ;; Try float
          (:br_on_cast 3 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
        ;; Try complex
        (:br_on_cast 4 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Not a number
      :drop
      (:i32.const 0)
      (:br 0))
    ;; i31/fixnum path
    :drop
    (:i32.const 1)
    (:br 0))
  ;; bignum path
  ;; ... (needs proper block structure)
  )

(defconstant +numeric-type-not-number+ 0)
(defconstant +numeric-type-fixnum+ 1)
(defconstant +numeric-type-bignum+ 2)
(defconstant +numeric-type-ratio+ 3)
(defconstant +numeric-type-float+ 4)
(defconstant +numeric-type-complex+ 5)

(defun numeric-type-contagion (type1 type2)
  "Determine result type from two numeric types per CLHS 12.1.4 (T015).
   Returns the more general type:
   complex > float > ratio > bignum > fixnum"
  (cond
    ;; Complex contagion: if either is complex, result is complex
    ((or (= type1 +numeric-type-complex+)
         (= type2 +numeric-type-complex+))
     +numeric-type-complex+)
    ;; Float contagion: if either is float, result is float
    ((or (= type1 +numeric-type-float+)
         (= type2 +numeric-type-float+))
     +numeric-type-float+)
    ;; Ratio contagion: if either is ratio, result is ratio
    ((or (= type1 +numeric-type-ratio+)
         (= type2 +numeric-type-ratio+))
     +numeric-type-ratio+)
    ;; Bignum contagion: if either is bignum, result is bignum
    ((or (= type1 +numeric-type-bignum+)
         (= type2 +numeric-type-bignum+))
     +numeric-type-bignum+)
    ;; Both fixnum
    (t +numeric-type-fixnum+)))

(defun emit-coerce-to-float ()
  "Generate instructions to coerce a numeric value to float.
   Expects: anyref on stack
   Returns: (ref $float) on stack"
  `(;; Dispatch on type
    (:block (:result (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
      ;; Check if already float
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-float+)))
      ;; Check if fixnum
      (:block
        (:block
          (:br_on_cast 0 :anyref :i31))
        ;; Was fixnum - convert to float
        :i31.get_s
        :f64.convert_i32_s
        (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
        (:br 2))
      ;; For other types, need runtime conversion
      ;; For now, error (will be implemented in user stories)
      :unreachable)))

(defun emit-coerce-to-complex ()
  "Generate instructions to coerce a numeric value to complex.
   Expects: anyref on stack
   Returns: (ref $complex) on stack"
  `(;; Dispatch on type
    (:block (:result (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
      ;; Check if already complex
      (:block
        (:br_on_cast 1 :anyref (:ref ,clysm/compiler/codegen/gc-types:+type-complex+)))
      ;; Real value - create complex with 0 imaginary
      (:i32.const 0) :ref.i31  ; zero for imaginary part
      (:struct.new ,clysm/compiler/codegen/gc-types:+type-complex+))))

;;; ============================================================
;;; Numeric Accessors (019-numeric-accessors)
;;; ============================================================

(defun compile-numerator (args env)
  "Compile (numerator x) - ANSI CL numerator accessor.
   For fixnum/bignum: returns the value itself.
   For ratio: returns the numerator field (struct.get 0).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "numerator requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "NUM-TMP"))))
    (append
     ;; Compile and store the argument
     (compile-to-instructions (first args) env)
     (list (list :local.set temp-local))
     ;; Type dispatch: check if it's a ratio
     `((:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio - extract numerator field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-ratio+ 0)
       :else
       ;; Not ratio (integer) - return itself
       (:local.get ,temp-local)
       :end))))

(defun compile-denominator (args env)
  "Compile (denominator x) - ANSI CL denominator accessor.
   For fixnum/bignum: returns 1.
   For ratio: returns the denominator field (struct.get 1).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "denominator requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "DENOM-TMP"))))
    (append
     ;; Compile and store the argument
     (compile-to-instructions (first args) env)
     (list (list :local.set temp-local))
     ;; Type dispatch: check if it's a ratio
     `((:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio - extract denominator field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-ratio+ 1)
       :else
       ;; Not ratio (integer) - return 1
       (:i32.const 1) :ref.i31
       :end))))

;;; ============================================================
;;; Hash Table Operations (043-self-hosting-blockers)
;;; ============================================================

;;; WasmGC Types for Hash Tables:
;;; - $hash-entry (type 25): struct { key: anyref, value: anyref (mut), next: (ref null 25) (mut) }
;;; - $hash-table (type 26): struct { size: i32, count: i32 (mut), test: anyref, buckets: (ref 27) }
;;; - $bucket-array (type 27): array (mut anyref)

(defun extract-keyword-from-ast (ast)
  "Extract keyword symbol from AST node.
   Returns keyword symbol or NIL if not a keyword."
  (cond
    ;; Direct keyword symbol (raw argument)
    ((keywordp ast) ast)
    ;; AST-VAR-REF wrapping a keyword symbol
    ((and (ast-var-ref-p ast)
          (keywordp (ast-var-ref-name ast)))
     (ast-var-ref-name ast))
    ;; Not a keyword
    (t nil)))

(defun compile-make-hash-table (args env)
  "Compile (make-hash-table &key test size) to create a new hash table.
   Default size is 17, default test is EQL (stored as symbol).
   Stack: [] -> [(ref $hash-table)]"
  ;; Parse keyword arguments
  ;; For simplicity, we support :test and :size keywords
  ;; Default: size=17, test=#'eql (represented as symbol 'eql)
  (let ((size-expr nil)
        (test-expr nil)
        (remaining args))
    ;; Parse &key arguments - handle both raw keywords and AST nodes
    (loop while remaining
          do (let* ((key-ast (first remaining))
                    (key (extract-keyword-from-ast key-ast)))
               (cond
                 ((eq key :test)
                  (setf test-expr (second remaining))
                  (setf remaining (cddr remaining)))
                 ((eq key :size)
                  (setf size-expr (second remaining))
                  (setf remaining (cddr remaining)))
                 ((eq key :rehash-size)
                  ;; Ignore rehash-size for now
                  (setf remaining (cddr remaining)))
                 ((eq key :rehash-threshold)
                  ;; Ignore rehash-threshold for now
                  (setf remaining (cddr remaining)))
                 (t
                  (error "Unsupported make-hash-table keyword: ~A" key-ast)))))
    ;; Generate Wasm instructions
    (let ((size-local (env-add-local env (gensym "HT-SIZE") :i32))
          (buckets-local (env-add-local env (gensym "HT-BUCKETS")))
          (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
          (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+))
      (append
       ;; Compute size (default 17)
       (if size-expr
           (append (compile-to-instructions size-expr env)
                   '((:ref.cast :i31) :i31.get_s))
           '((:i32.const 17)))
       (list (list :local.set size-local))
       ;; Create bucket array filled with null refs
       `((:local.get ,size-local)
         (:array.new_default ,bucket-type)
         (:local.set ,buckets-local))
       ;; Build hash-table struct:
       ;; Field 0: size (i32)
       ;; Field 1: count (i32) - initially 0
       ;; Field 2: test (anyref) - symbol for test function
       ;; Field 3: buckets (ref $bucket-array)
       `((:local.get ,size-local)      ; size
         (:i32.const 0))               ; count = 0
       ;; Test function (default 'eql as placeholder)
       (if test-expr
           (compile-to-instructions test-expr env)
           '((:i32.const 0) :ref.i31))  ; placeholder for test
       ;; Buckets
       `((:local.get ,buckets-local)
         (:struct.new ,hash-type))))))

(defun compile-gethash (args env)
  "Compile (gethash key hash-table &optional default) to lookup a value.
   Returns the value if found, or default (NIL if not specified).
   Stack: [] -> [anyref]"
  (when (< (length args) 2)
    (error "gethash requires at least 2 arguments (key hash-table)"))
  (let* ((key-expr (first args))
         (ht-expr (second args))
         (default-expr (if (cddr args) (third args) nil))
         (key-local (env-add-local env (gensym "GH-KEY")))
         (ht-local (env-add-local env (gensym "GH-HT")))
         (bucket-local (env-add-local env (gensym "GH-BUCKET")))
         (idx-local (env-add-local env (gensym "GH-IDX")))
         (entry-local (env-add-local env (gensym "GH-ENTRY")))
         (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
         (entry-type clysm/compiler/codegen/gc-types:+type-hash-entry+)
         (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+))
    (append
     ;; Compile and store key
     (compile-to-instructions key-expr env)
     (list (list :local.set key-local))
     ;; Compile and store hash-table
     (compile-to-instructions ht-expr env)
     (list (list :ref.cast (list :ref hash-type)))
     (list (list :local.set ht-local))
     ;; Get bucket array
     `((:local.get ,ht-local)
       (:struct.get ,hash-type 3)       ; buckets field
       (:local.set ,bucket-local))
     ;; Compute bucket index: hash(key) mod size
     `((:local.get ,key-local)
       (:ref.test :i31)
       (:if (:result :i32))
       ;; i31ref key - use value mod size
       (:local.get ,key-local)
       (:ref.cast :i31) :i31.get_s
       (:i32.const #x7FFFFFFF) :i32.and
       (:local.get ,ht-local)
       (:struct.get ,hash-type 0)       ; size field
       :i32.rem_u
       :else
       ;; Non-i31 key - use bucket 0 for simplicity
       (:i32.const 0)
       :end
       (:local.set ,idx-local))
     ;; Get first entry in bucket
     `((:local.get ,bucket-local)
       (:local.get ,idx-local)
       (:array.get ,bucket-type)
       (:local.set ,entry-local))
     ;; Loop through chain
     `((:block (:result :anyref)
         (:loop
           ;; Check if entry is null
           (:local.get ,entry-local)
           :ref.is_null
           (:br_if 1)  ; exit loop if null
           ;; Check if key matches (using ref.eq for EQL-like semantics)
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 0)  ; key field
           (:local.get ,key-local)
           :ref.eq
           (:if (:result :anyref))
           ;; Key matches - return value
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 1)  ; value field
           (:br 2)  ; exit outer block with value
           :else
           ;; Key doesn't match - continue to next entry
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 2)  ; next field
           (:local.set ,entry-local)
           (:br 1)  ; continue loop
           :end)
         ;; End of chain - return default
         ,@(if default-expr
               (compile-to-instructions default-expr env)
               '((:ref.null :none))))))))

(defun compile-puthash (args env)
  "Compile (puthash key value hash-table) to store a value.
   If key exists, updates the value. Otherwise, creates a new entry.
   Returns the value.
   Stack: [] -> [anyref (the value)]"
  (when (/= (length args) 3)
    (error "puthash requires exactly 3 arguments (key value hash-table)"))
  (let* ((key-expr (first args))
         (value-expr (second args))
         (ht-expr (third args))
         (key-local (env-add-local env (gensym "PH-KEY")))
         (value-local (env-add-local env (gensym "PH-VAL")))
         (ht-local (env-add-local env (gensym "PH-HT")))
         (bucket-local (env-add-local env (gensym "PH-BUCKET")))
         (idx-local (env-add-local env (gensym "PH-IDX")))
         (entry-local (env-add-local env (gensym "PH-ENTRY")))
         (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
         (entry-type clysm/compiler/codegen/gc-types:+type-hash-entry+)
         (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+))
    (append
     ;; Compile and store key, value, hash-table
     (compile-to-instructions key-expr env)
     (list (list :local.set key-local))
     (compile-to-instructions value-expr env)
     (list (list :local.set value-local))
     (compile-to-instructions ht-expr env)
     (list (list :ref.cast (list :ref hash-type)))
     (list (list :local.set ht-local))
     ;; Get bucket array
     `((:local.get ,ht-local)
       (:struct.get ,hash-type 3)
       (:local.set ,bucket-local))
     ;; Compute bucket index
     `((:local.get ,key-local)
       (:ref.test :i31)
       (:if (:result :i32))
       (:local.get ,key-local)
       (:ref.cast :i31) :i31.get_s
       (:i32.const #x7FFFFFFF) :i32.and
       (:local.get ,ht-local)
       (:struct.get ,hash-type 0)
       :i32.rem_u
       :else
       (:i32.const 0)
       :end
       (:local.set ,idx-local))
     ;; Get first entry in bucket
     `((:local.get ,bucket-local)
       (:local.get ,idx-local)
       (:array.get ,bucket-type)
       (:local.set ,entry-local))
     ;; Search for existing key
     `((:block (:result :anyref)
         (:loop
           ;; Check if entry is null - need to insert new
           (:local.get ,entry-local)
           :ref.is_null
           (:if)
           ;; Not found - create new entry and prepend to bucket
           ;; Create new hash-entry: key, value, next (current bucket head)
           (:local.get ,key-local)
           (:local.get ,value-local)
           (:local.get ,bucket-local)
           (:local.get ,idx-local)
           (:array.get ,bucket-type)
           (:struct.new ,entry-type)
           ;; Store new entry as bucket head
           (:local.get ,bucket-local)
           (:local.get ,idx-local)
           ;; Stack: [new-entry, bucket-array, idx]
           ;; But we need to dup new-entry to keep it for storage and return
           ;; Use a temp local
           (:local.set ,entry-local)  ; save new entry
           (:local.get ,entry-local)
           (:array.set ,bucket-type)
           ;; Increment count
           (:local.get ,ht-local)
           (:local.get ,ht-local)
           (:struct.get ,hash-type 1)
           (:i32.const 1)
           :i32.add
           (:struct.set ,hash-type 1)
           ;; Return the value
           (:local.get ,value-local)
           (:br 2)
           :end
           ;; Check if key matches
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 0)
           (:local.get ,key-local)
           :ref.eq
           (:if)
           ;; Key matches - update value
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:local.get ,value-local)
           (:struct.set ,entry-type 1)
           ;; Return the value
           (:local.get ,value-local)
           (:br 2)
           :end
           ;; Move to next entry
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 2)
           (:local.set ,entry-local)
           (:br 0)))
       ;; Unreachable
       ))))

(defun compile-remhash (args env)
  "Compile (remhash key hash-table) to remove a key from hash table.
   Returns T if key was present, NIL otherwise.
   Stack: [] -> [anyref (T or NIL)]"
  (when (/= (length args) 2)
    (error "remhash requires exactly 2 arguments"))
  (let* ((key-expr (first args))
         (ht-expr (second args))
         (key-local (env-add-local env (gensym "RH-KEY")))
         (ht-local (env-add-local env (gensym "RH-HT")))
         (bucket-local (env-add-local env (gensym "RH-BUCKET")))
         (idx-local (env-add-local env (gensym "RH-IDX")))
         (entry-local (env-add-local env (gensym "RH-ENTRY")))
         (prev-local (env-add-local env (gensym "RH-PREV")))
         (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
         (entry-type clysm/compiler/codegen/gc-types:+type-hash-entry+)
         (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+))
    (append
     ;; Compile key and hash-table
     (compile-to-instructions key-expr env)
     (list (list :local.set key-local))
     (compile-to-instructions ht-expr env)
     (list (list :ref.cast (list :ref hash-type)))
     (list (list :local.set ht-local))
     ;; Compute bucket index
     `((:local.get ,key-local)
       (:ref.test :i31)
       (:if (:result :i32))
       (:local.get ,key-local)
       (:ref.cast :i31) :i31.get_s
       (:i32.const #x7FFFFFFF) :i32.and
       (:local.get ,ht-local)
       (:struct.get ,hash-type 0)
       :i32.rem_u
       :else
       (:i32.const 0)
       :end
       (:local.set ,idx-local))
     ;; Get bucket array and first entry
     `((:local.get ,ht-local)
       (:struct.get ,hash-type 3)
       (:local.set ,bucket-local)
       (:local.get ,bucket-local)
       (:local.get ,idx-local)
       (:array.get ,bucket-type)
       (:local.set ,entry-local)
       ;; Set prev to null
       (:ref.null ,entry-type)
       (:local.set ,prev-local))
     ;; Loop through chain to find and remove
     `((:block (:result :anyref)
         (:loop
           ;; Check if entry is null
           (:local.get ,entry-local)
           :ref.is_null
           (:if)
           ;; Not found - return NIL
           (:ref.null :none)
           (:br 2)
           :end
           ;; Check if key matches
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 0)
           (:local.get ,key-local)
           :ref.eq
           (:if)
           ;; Found - remove from chain
           (:local.get ,prev-local)
           :ref.is_null
           (:if)
           ;; First in chain - update bucket
           (:local.get ,bucket-local)
           (:local.get ,idx-local)
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 2)  ; next
           (:array.set ,bucket-type)
           :else
           ;; Not first - update prev's next
           (:local.get ,prev-local)
           (:ref.cast (:ref ,entry-type))
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 2)
           (:struct.set ,entry-type 2)
           :end
           ;; Decrement count
           (:local.get ,ht-local)
           (:local.get ,ht-local)
           (:struct.get ,hash-type 1)  ; count
           (:i32.const 1)
           :i32.sub
           (:struct.set ,hash-type 1)
           ;; Return T
           (:i32.const 1) :ref.i31
           (:br 2)
           :end
           ;; Move to next entry
           (:local.get ,entry-local)
           (:local.set ,prev-local)
           (:local.get ,entry-local)
           (:ref.cast (:ref ,entry-type))
           (:struct.get ,entry-type 2)
           (:local.set ,entry-local)
           (:br 0)))
       ;; Unreachable - loop always exits via br
       ))))

(defun compile-maphash (args env)
  "Compile (maphash function hash-table) to iterate over entries.
   Function is called with (key value) for each entry.
   Returns NIL.
   Stack: [] -> [anyref (NIL)]"
  (when (/= (length args) 2)
    (error "maphash requires exactly 2 arguments"))
  (let* ((func-expr (first args))
         (ht-expr (second args))
         (func-local (env-add-local env (gensym "MH-FUNC")))
         (ht-local (env-add-local env (gensym "MH-HT")))
         (bucket-local (env-add-local env (gensym "MH-BUCKET")))
         (idx-local (env-add-local env (gensym "MH-IDX")))
         (size-local (env-add-local env (gensym "MH-SIZE")))
         (entry-local (env-add-local env (gensym "MH-ENTRY")))
         (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
         (entry-type clysm/compiler/codegen/gc-types:+type-hash-entry+)
         (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+)
         (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
         (func-2-type clysm/compiler/codegen/gc-types:+type-func-2+))
    (append
     ;; Compile function and hash-table
     (compile-to-instructions func-expr env)
     (list (list :local.set func-local))
     (compile-to-instructions ht-expr env)
     (list (list :ref.cast (list :ref hash-type)))
     (list (list :local.set ht-local))
     ;; Get size and bucket array
     `((:local.get ,ht-local)
       (:struct.get ,hash-type 0)
       (:local.set ,size-local)
       (:local.get ,ht-local)
       (:struct.get ,hash-type 3)
       (:local.set ,bucket-local)
       ;; Initialize index
       (:i32.const 0)
       (:local.set ,idx-local)
       ;; Outer loop over buckets
       (:block
         (:loop
           ;; Check if done with all buckets
           (:local.get ,idx-local)
           (:local.get ,size-local)
           :i32.ge_u
           (:br_if 1)
           ;; Get first entry in bucket
           (:local.get ,bucket-local)
           (:local.get ,idx-local)
           (:array.get ,bucket-type)
           (:local.set ,entry-local)
           ;; Inner loop over chain
           (:block
             (:loop
               ;; Check if entry is null
               (:local.get ,entry-local)
               :ref.is_null
               (:br_if 1)
               ;; Call function with key and value
               ;; Push closure as first arg (self reference)
               (:local.get ,func-local)
               ;; Push key
               (:local.get ,entry-local)
               (:ref.cast (:ref ,entry-type))
               (:struct.get ,entry-type 0)
               ;; Push value
               (:local.get ,entry-local)
               (:ref.cast (:ref ,entry-type))
               (:struct.get ,entry-type 1)
               ;; Get code_2 from closure and call
               (:local.get ,func-local)
               (:ref.cast (:ref ,closure-type))
               (:struct.get ,closure-type 2)  ; code_2
               (:ref.cast ,func-2-type)
               (:call_ref ,func-2-type)
               :drop  ; discard result
               ;; Move to next entry
               (:local.get ,entry-local)
               (:ref.cast (:ref ,entry-type))
               (:struct.get ,entry-type 2)
               (:local.set ,entry-local)
               (:br 0)))
           ;; Increment bucket index
           (:local.get ,idx-local)
           (:i32.const 1)
           :i32.add
           (:local.set ,idx-local)
           (:br 0)))
       ;; Return NIL
       (:ref.null :none)))))

(defun compile-hash-table-count (args env)
  "Compile (hash-table-count hash-table) to get number of entries.
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "hash-table-count requires exactly 1 argument"))
  (let ((hash-type clysm/compiler/codegen/gc-types:+type-hash-table+))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast (:ref ,hash-type))
       (:struct.get ,hash-type 1)  ; count field
       :ref.i31))))

(defun compile-hash-table-size (args env)
  "Compile (hash-table-size hash-table) to get bucket count.
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "hash-table-size requires exactly 1 argument"))
  (let ((hash-type clysm/compiler/codegen/gc-types:+type-hash-table+))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast (:ref ,hash-type))
       (:struct.get ,hash-type 0)  ; size field
       :ref.i31))))

(defun compile-clrhash (args env)
  "Compile (clrhash hash-table) to clear all entries.
   Sets all bucket entries to null and count to 0.
   Returns the hash table.
   Stack: [] -> [anyref (hash-table)]"
  (when (/= (length args) 1)
    (error "clrhash requires exactly 1 argument"))
  (let* ((ht-local (env-add-local env (gensym "CH-HT")))
         (bucket-local (env-add-local env (gensym "CH-BUCKET")))
         (idx-local (env-add-local env (gensym "CH-IDX")))
         (size-local (env-add-local env (gensym "CH-SIZE")))
         (hash-type clysm/compiler/codegen/gc-types:+type-hash-table+)
         (bucket-type clysm/compiler/codegen/gc-types:+type-bucket-array+)
         (entry-type clysm/compiler/codegen/gc-types:+type-hash-entry+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :ref.cast (list :ref hash-type)))
     (list (list :local.set ht-local))
     ;; Get size and bucket array
     `((:local.get ,ht-local)
       (:struct.get ,hash-type 0)
       (:local.set ,size-local)
       (:local.get ,ht-local)
       (:struct.get ,hash-type 3)
       (:local.set ,bucket-local)
       ;; Clear all buckets
       (:i32.const 0)
       (:local.set ,idx-local)
       (:block
         (:loop
           (:local.get ,idx-local)
           (:local.get ,size-local)
           :i32.ge_u
           (:br_if 1)
           ;; Set bucket to null
           (:local.get ,bucket-local)
           (:local.get ,idx-local)
           (:ref.null ,entry-type)
           (:array.set ,bucket-type)
           ;; Increment index
           (:local.get ,idx-local)
           (:i32.const 1)
           :i32.add
           (:local.set ,idx-local)
           (:br 0)))
       ;; Set count to 0
       (:local.get ,ht-local)
       (:i32.const 0)
       (:struct.set ,hash-type 1)
       ;; Return hash table
       (:local.get ,ht-local)))))

;;; ============================================================
;;; Feature 043: NCONC for LOOP Support
;;; ============================================================

(defun compile-nconc (args env)
  "Compile (nconc list1 list2 ...) - destructive concatenation.
   For LOOP collect: (nconc acc (list item))
   Returns last list for empty/single, or result of destructive concat.
   Stack: [] -> [anyref (result list)]"
  (cond
    ;; No args -> NIL
    ((null args)
     (list '(:ref.null :none)))
    ;; One arg -> return it
    ((null (cdr args))
     (compile-to-instructions (first args) env))
    ;; Two args -> common case for LOOP
    ((null (cddr args))
     (let* ((list1-local (env-add-local env (gensym "NCONC-L1")))
            (list2-local (env-add-local env (gensym "NCONC-L2")))
            (curr-local (env-add-local env (gensym "NCONC-CURR")))
            (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
       (append
        ;; Evaluate list1 and list2
        (compile-to-instructions (first args) env)
        (list (list :local.set list1-local))
        (compile-to-instructions (second args) env)
        (list (list :local.set list2-local))
        ;; If list1 is null, return list2
        `((:local.get ,list1-local)
          :ref.is_null
          (:if (:result :anyref)
            ((:local.get ,list2-local))
            ;; Else find last cons of list1 and set its cdr to list2
            ((:local.get ,list1-local)
             (:local.set ,curr-local)
             ;; Loop to find last cons
             (:block
               (:loop
                 ;; Check if (cdr curr) is null
                 (:local.get ,curr-local)
                 (:ref.cast (ref ,cons-type))
                 (:struct.get ,cons-type 1)  ;; Get cdr
                 :ref.is_null
                 (:br_if 1)  ;; Exit if null (curr is last cons)
                 ;; Move to next cons
                 (:local.get ,curr-local)
                 (:ref.cast (ref ,cons-type))
                 (:struct.get ,cons-type 1)
                 (:local.set ,curr-local)
                 (:br 0)))
             ;; curr is now the last cons - set its cdr to list2
             (:local.get ,curr-local)
             (:ref.cast (ref ,cons-type))
             (:local.get ,list2-local)
             (:struct.set ,cons-type 1)
             ;; Return list1
             (:local.get ,list1-local)))))))
    ;; More than two args -> chain nconcs
    (t
     ;; (nconc a b c) = (nconc (nconc a b) c)
     (compile-nconc
      (list (cons 'nconc (butlast args)) (car (last args)))
      env))))

;;; ============================================================
;;; Feature 043: Array Setf Primitive
;;; ============================================================

(defun compile-setf-aref (args env)
  "Compile (%setf-aref array value index...) - set array element.
   Returns the value for proper setf semantics.
   Note: Currently only supports 1D arrays.
   HyperSpec: resources/HyperSpec/Body/f_aref.htm"
  (when (< (length args) 3)
    (error "%setf-aref requires at least 3 arguments (array value index)"))
  (let ((array-expr (first args))
        (value-expr (second args))
        (index-expr (third args))  ; Only support 1D for now
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+))
    (when (> (length args) 3)
      (error "%setf-aref: multi-dimensional arrays not yet supported"))
    (let ((value-local (env-add-local env (gensym "AREF-VAL"))))
      (append
       ;; Evaluate and save value (for return)
       (compile-to-instructions value-expr env)
       (list (list :local.set value-local))
       ;; Evaluate array
       (compile-to-instructions array-expr env)
       ;; Cast to mv_array type (anyref array)
       (list (list :ref.cast (list :ref mv-array-type)))
       ;; Evaluate index
       (compile-to-instructions index-expr env)
       '((:ref.cast :i31) :i31.get_s)  ; Convert to i32 index
       ;; Get value to store
       (list (list :local.get value-local))
       ;; Store in array - use mv_array type (type 20)
       (list (list :array.set mv-array-type))
       ;; Return the value
       (list (list :local.get value-local))))))

;;; ============================================================
;;; Feature 001-ansi-array-primitives: Array/Sequence Primitives
;;; Phase 13D-1: ANSI CL Array Access
;;; ============================================================

(defun compile-aref (args env)
  "Compile (aref array index) - get element from simple-vector.
   HyperSpec: resources/HyperSpec/Body/f_aref.htm
   Stack: [] -> [anyref]"
  (when (< (length args) 2)
    (error "aref requires at least 2 arguments (array index)"))
  (when (> (length args) 2)
    (error "aref: multi-dimensional arrays not yet supported"))
  (let ((array-expr (first args))
        (index-expr (second args))
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+))
    (append
     ;; Evaluate array
     (compile-to-instructions array-expr env)
     ;; Cast to mv_array type (type 20 = anyref array)
     (list (list :ref.cast (list :ref mv-array-type)))
     ;; Evaluate index
     (compile-to-instructions index-expr env)
     ;; Convert index from i31ref to i32
     '((:ref.cast :i31) :i31.get_s)
     ;; Get element from array
     (list (list :array.get mv-array-type)))))

(defun compile-svref (args env)
  "Compile (svref simple-vector index) - get element from simple-vector.
   HyperSpec: resources/HyperSpec/Body/f_svref.htm
   svref is semantically identical to aref for simple-vectors."
  ;; Delegate to compile-aref since svref is just aref for simple-vectors
  (compile-aref args env))

(defun compile-elt (args env)
  "Compile (elt sequence index) - generic sequence element access.
   HyperSpec: resources/HyperSpec/Body/f_elt.htm
   Performs runtime type dispatch: vector -> array.get, list -> nth, string -> schar.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "elt requires exactly 2 arguments (sequence index)"))
  (let ((seq-expr (first args))
        (index-expr (second args))
        (seq-local (env-add-local env (gensym "ELT-SEQ")))
        (idx-local (env-add-local env (gensym "ELT-IDX") :i32))
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (append
     ;; Evaluate and save sequence
     (compile-to-instructions seq-expr env)
     (list (list :local.set seq-local))
     ;; Evaluate and save index (as i32)
     (compile-to-instructions index-expr env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Type dispatch
     `((:block (:result :anyref)
         ;; Check if simple-vector (type 20)
         (:local.get ,seq-local)
         (:ref.test (:ref ,mv-array-type))
         (:if (:result :anyref)
           ;; Vector path: array.get
           ((:local.get ,seq-local)
            (:ref.cast (:ref ,mv-array-type))
            (:local.get ,idx-local)
            (:array.get ,mv-array-type))
           ;; Check if string (type 4)
           ((:local.get ,seq-local)
            (:ref.test (:ref ,string-type))
            (:if (:result :anyref)
              ;; String path: array.get_u and wrap as i31ref
              ((:local.get ,seq-local)
               (:ref.cast (:ref ,string-type))
               (:local.get ,idx-local)
               (:array.get_u ,string-type)
               :ref.i31)
              ;; List path: iterate to nth element
              ((:local.get ,seq-local)
               (:local.get ,idx-local)
               (:block (:result :anyref)
                 (:loop
                   ;; Check if index is 0
                   (:local.get ,idx-local)
                   :i32.eqz
                   (:br_if 1)  ; Exit loop, take car
                   ;; Decrement index
                   (:local.get ,idx-local)
                   (:i32.const 1)
                   :i32.sub
                   (:local.set ,idx-local)
                   ;; Get cdr
                   (:local.get ,seq-local)
                   (:ref.cast (:ref ,cons-type))
                   (:struct.get ,cons-type 1)  ; cdr
                   (:local.set ,seq-local)
                   (:br 0))  ; Continue loop
                 ;; Return car of current cons
                 (:local.get ,seq-local)
                 (:ref.cast (:ref ,cons-type))
                 (:struct.get ,cons-type 0)))))))))))

(defun compile-coerce (args env)
  "Compile (coerce object result-type) - type coercion.
   HyperSpec: resources/HyperSpec/Body/f_coerce.htm
   For now, handles literal result-type only: 'vector, 'list.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "coerce requires exactly 2 arguments (object result-type)"))
  (let ((obj-expr (first args))
        (type-expr (second args)))
    ;; Handle quoted type specifiers at compile time
    (cond
      ;; (coerce x 'vector) or (coerce x 'simple-vector)
      ((and (listp type-expr)
            (eq (first type-expr) 'quote)
            (member (second type-expr) '(vector simple-vector)))
       (compile-coerce-to-vector obj-expr env))
      ;; (coerce x 'list)
      ((and (listp type-expr)
            (eq (first type-expr) 'quote)
            (eq (second type-expr) 'list))
       (compile-coerce-to-list obj-expr env))
      ;; Other types - fall back to runtime dispatch
      (t
       ;; For now, just evaluate the object (no-op coercion)
       ;; TODO: Implement full runtime coerce
       (compile-to-instructions obj-expr env)))))

(defun compile-coerce-to-vector (obj-expr env)
  "Compile coercion to vector type.
   If already a vector, return as-is.
   If a list, convert to vector using %list-to-vector runtime helper."
  (let ((obj-local (env-add-local env (gensym "COERCE-OBJ")))
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (append
     ;; Evaluate object
     (compile-to-instructions obj-expr env)
     (list (list :local.set obj-local))
     ;; Check if already a vector
     `((:block (:result :anyref)
         (:local.get ,obj-local)
         (:ref.test (:ref ,mv-array-type))
         (:if (:result :anyref)
           ;; Already a vector - return as-is
           ((:local.get ,obj-local))
           ;; Assume it's a list - need to convert
           ;; For now, signal error (runtime helper needed)
           ;; TODO: Implement %list-to-vector runtime function
           ((:local.get ,obj-local))))))))

(defun compile-coerce-to-list (obj-expr env)
  "Compile coercion to list type.
   If already a list, return as-is.
   If a vector, convert to list using %vector-to-list runtime helper."
  (let ((obj-local (env-add-local env (gensym "COERCE-OBJ")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (append
     ;; Evaluate object
     (compile-to-instructions obj-expr env)
     (list (list :local.set obj-local))
     ;; Check if already a list (cons or nil)
     `((:block (:result :anyref)
         (:local.get ,obj-local)
         :ref.is_null
         (:if (:result :anyref)
           ;; NIL is a list
           ((:local.get ,obj-local))
           ;; Check if cons
           ((:local.get ,obj-local)
            (:ref.test (:ref ,cons-type))
            (:if (:result :anyref)
              ;; Already a cons/list - return as-is
              ((:local.get ,obj-local))
              ;; Assume it's a vector - need to convert
              ;; TODO: Implement %vector-to-list runtime function
              ((:local.get ,obj-local))))))))))

;;; ============================================================
;;; Feature 001-ansi-array-ops: ANSI Array Operations
;;; Phase 15C: Array Operations Enhancement
;;;
;;; HyperSpec References:
;;; - array-rank: resources/HyperSpec/Body/f_ar_ran.htm
;;; - array-dimension: resources/HyperSpec/Body/f_ar_dim.htm
;;; - array-dimensions: resources/HyperSpec/Body/f_ar_d_1.htm
;;; - array-total-size: resources/HyperSpec/Body/f_ar_tot.htm
;;; - array-row-major-index: resources/HyperSpec/Body/f_ar_row.htm
;;; - row-major-aref: resources/HyperSpec/Body/f_row_ma.htm
;;; - adjustable-array-p: resources/HyperSpec/Body/f_adju_1.htm
;;; - adjust-array: resources/HyperSpec/Body/f_adjust.htm
;;; ============================================================

;;; Type Dispatch Helper (T007)
;;; Shared helper for $mdarray vs $mv_array dispatch

(defun emit-array-type-dispatch (env arr-local mdarray-code simple-code)
  "Generate code to dispatch between $mdarray and simple $mv_array.
   ARR-LOCAL: local variable holding the array
   MDARRAY-CODE: code to execute if array is $mdarray
   SIMPLE-CODE: code to execute if array is simple $mv_array
   Returns: Wasm instruction list with if/else dispatch

   Pattern:
     local.get arr-local
     ref.test (ref $mdarray)
     if (result anyref)
       <mdarray-code>
     else
       <simple-code>
     end"
  (declare (ignore env))
  (let ((mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+))
    `((:local.get ,arr-local)
      (:ref.test (:ref ,mdarray-type))
      (:if (:result :anyref))
      ,@mdarray-code
      :else
      ,@simple-code
      :end)))

;;; User Story 1: Query Array Dimensions and Structure (P1)

(defun compile-array-rank (args env)
  "Compile (array-rank array) - return number of dimensions.
   HyperSpec: resources/HyperSpec/Body/f_ar_ran.htm
   For simple-vectors: returns 1
   For $mdarray: returns length of dimensions list
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "array-rank requires exactly 1 argument"))
  (let ((arr-local (env-add-local env (gensym "ARR-RANK")))
        (count-local (env-add-local env (gensym "RANK-COUNT") :i32))
        (dims-local (env-add-local env (gensym "DIMS")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Type dispatch
      (emit* (emit-array-type-dispatch
              env arr-local
              ;; $mdarray path: count elements in dimensions list
              `(;; Get dimensions list
                (:local.get ,arr-local)
                (:ref.cast (:ref ,mdarray-type))
                (:struct.get ,mdarray-type 0)  ; dimensions field (cons list)
                (:local.set ,dims-local)
                ;; Initialize counter to 0
                (:i32.const 0)
                (:local.set ,count-local)
                ;; Loop through list
                (:block $rank_done)
                (:loop $rank_loop)
                ;; Check if nil (end of list)
                (:local.get ,dims-local)
                :ref.is_null
                (:br_if $rank_done)
                ;; Check if cons
                (:local.get ,dims-local)
                (:ref.test (:ref ,cons-type))
                :i32.eqz
                (:br_if $rank_done)
                ;; Increment counter
                (:local.get ,count-local)
                (:i32.const 1)
                :i32.add
                (:local.set ,count-local)
                ;; Get cdr
                (:local.get ,dims-local)
                (:ref.cast (:ref ,cons-type))
                (:struct.get ,cons-type 1)
                (:local.set ,dims-local)
                (:br $rank_loop)
                :end  ; loop
                :end  ; block
                ;; Return count as fixnum
                (:local.get ,count-local)
                :ref.i31)
              ;; Simple array path: rank is always 1
              `((:i32.const 1)
                :ref.i31))))))

(defun compile-array-dimension (args env)
  "Compile (array-dimension array axis-number) - return dimension size.
   HyperSpec: resources/HyperSpec/Body/f_ar_dim.htm
   For simple-vectors: axis 0 returns array length
   For $mdarray: returns nth element of dimensions list
   Stack: [] -> [fixnum]"
  (when (/= (length args) 2)
    (error "array-dimension requires exactly 2 arguments"))
  (let ((arr-local (env-add-local env (gensym "ARR-DIM")))
        (axis-local (env-add-local env (gensym "AXIS") :i32))
        (dims-local (env-add-local env (gensym "DIMS")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile array and store
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Compile axis and convert to i32
      (emit* (compile-to-instructions (second args) env))
      (emit '(:ref.cast :i31))
      (emit :i31.get_s)
      (emit :local.set axis-local)
      ;; Type dispatch
      (emit* (emit-array-type-dispatch
              env arr-local
              ;; $mdarray path: get nth element from dimensions list
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mdarray-type))
                (:struct.get ,mdarray-type 0)  ; dimensions list
                (:local.set ,dims-local)
                ;; Walk to nth element
                (:block $dim_done)
                (:loop $dim_loop)
                ;; Check if axis is 0
                (:local.get ,axis-local)
                :i32.eqz
                (:br_if $dim_done)  ; found it, exit loop
                ;; Decrement axis and move to cdr
                (:local.get ,axis-local)
                (:i32.const 1)
                :i32.sub
                (:local.set ,axis-local)
                (:local.get ,dims-local)
                (:ref.cast (:ref ,cons-type))
                (:struct.get ,cons-type 1)
                (:local.set ,dims-local)
                (:br $dim_loop)
                :end  ; loop
                :end  ; block
                ;; dims-local now points to the nth cons cell, return its car
                (:local.get ,dims-local)
                (:ref.cast (:ref ,cons-type))
                (:struct.get ,cons-type 0))
              ;; Simple array path: axis must be 0, return array length
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mv-array-type))
                (:array.len)
                :ref.i31))))))

(defun compile-array-dimensions (args env)
  "Compile (array-dimensions array) - return list of dimension sizes.
   HyperSpec: resources/HyperSpec/Body/f_ar_d_1.htm
   For simple-vectors: returns (length)
   For $mdarray: returns the stored dimensions list
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "array-dimensions requires exactly 1 argument"))
  (let ((arr-local (env-add-local env (gensym "ARR-DIMS")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Type dispatch
      (emit* (emit-array-type-dispatch
              env arr-local
              ;; $mdarray path: return dimensions list directly
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mdarray-type))
                (:struct.get ,mdarray-type 0))
              ;; Simple array path: create (length) list
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mv-array-type))
                (:array.len)
                :ref.i31  ; length as car
                (:global.get 0)  ; NIL as cdr
                (:struct.new ,cons-type)))))))

(defun compile-array-total-size (args env)
  "Compile (array-total-size array) - return total number of elements.
   HyperSpec: resources/HyperSpec/Body/f_ar_tot.htm
   For simple-vectors: returns array length
   For $mdarray: returns length of underlying storage
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "array-total-size requires exactly 1 argument"))
  (let ((arr-local (env-add-local env (gensym "ARR-SIZE")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+))
    (with-instruction-collector
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Type dispatch
      (emit* (emit-array-type-dispatch
              env arr-local
              ;; $mdarray path: get length of storage array
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mdarray-type))
                (:struct.get ,mdarray-type 1)  ; storage field (anyref)
                (:ref.cast (:ref ,mv-array-type))  ; cast to array type
                (:array.len)
                :ref.i31)
              ;; Simple array path: get array length directly
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mv-array-type))
                (:array.len)
                :ref.i31))))))

;;; User Story 2: Access Elements by Row-Major Index (P2)

(defun compile-array-row-major-index (args env)
  "Compile (array-row-major-index array &rest subscripts) - compute linear index.
   HyperSpec: resources/HyperSpec/Body/f_ar_row.htm
   Formula: index = sum(subscript[i] * product(dimension[j] for j > i))
   For simple-vectors: returns the single subscript
   Stack: [] -> [fixnum]"
  (when (< (length args) 1)
    (error "array-row-major-index requires at least 1 argument"))
  (let ((arr-local (env-add-local env (gensym "ARR-RMI")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (mult-local (env-add-local env (gensym "MULT") :i32))
        (dims-local (env-add-local env (gensym "DIMS")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (subscripts (rest args)))
    (if (= (length subscripts) 1)
        ;; Single subscript: just return it as fixnum
        (append
         (compile-to-instructions (first args) env)  ; array (ignored for simple case)
         '(:drop)
         (compile-to-instructions (first subscripts) env))
        ;; Multiple subscripts: compute row-major index
        (append
         ;; Compile array and store
         (compile-to-instructions (first args) env)
         (list (list :local.set arr-local))
         ;; Initialize index=0, multiplier=1
         '((:i32.const 0))
         (list (list :local.set idx-local))
         '((:i32.const 1))
         (list (list :local.set mult-local))
         ;; Get dimensions list
         `((:local.get ,arr-local)
           (:ref.cast (:ref ,mdarray-type))
           (:struct.get ,mdarray-type 0)
           (:local.set ,dims-local))
         ;; Process subscripts from last to first
         (loop for sub in (reverse subscripts)
               for i from (1- (length subscripts)) downto 0
               append
               (append
                ;; Get subscript value as i32
                (compile-to-instructions sub env)
                '((:ref.cast :i31) :i31.get_s)
                ;; index += subscript * multiplier
                `((:local.get ,mult-local)
                  :i32.mul
                  (:local.get ,idx-local)
                  :i32.add
                  (:local.set ,idx-local))
                ;; Get dimension for this axis (nth from dims list)
                `((:local.get ,dims-local)
                  ,@(loop repeat i
                          append `((:ref.cast (:ref ,cons-type))
                                   (:struct.get ,cons-type 1)))
                  (:ref.cast (:ref ,cons-type))
                  (:struct.get ,cons-type 0)
                  (:ref.cast :i31)
                  :i31.get_s
                  ;; multiplier *= dimension
                  (:local.get ,mult-local)
                  :i32.mul
                  (:local.set ,mult-local))))
         ;; Return index as fixnum
         `((:local.get ,idx-local)
           :ref.i31)))))

(defun compile-row-major-aref (args env)
  "Compile (row-major-aref array index) - access element by linear index.
   HyperSpec: resources/HyperSpec/Body/f_row_ma.htm
   For both $mdarray and simple arrays: direct array access
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "row-major-aref requires exactly 2 arguments"))
  (let ((arr-local (env-add-local env (gensym "ARR-RMA")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+))
    (append
     ;; Compile array and store
     (compile-to-instructions (first args) env)
     (list (list :local.set arr-local))
     ;; Compile index and convert to i32
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Type dispatch
     (emit-array-type-dispatch
      env arr-local
      ;; $mdarray path: access storage array
      `((:local.get ,arr-local)
        (:ref.cast (:ref ,mdarray-type))
        (:struct.get ,mdarray-type 1)  ; storage (anyref)
        (:ref.cast (:ref ,mv-array-type))  ; cast to array type
        (:local.get ,idx-local)
        (:array.get ,mv-array-type))
      ;; Simple array path: direct access
      `((:local.get ,arr-local)
        (:ref.cast (:ref ,mv-array-type))
        (:local.get ,idx-local)
        (:array.get ,mv-array-type))))))

(defun compile-setf-row-major-aref (args env)
  "Compile (%setf-row-major-aref array value index) - set element by linear index.
   Stack: [] -> [anyref (value)]"
  (when (/= (length args) 3)
    (error "%setf-row-major-aref requires exactly 3 arguments"))
  (let ((arr-local (env-add-local env (gensym "ARR-SRMA")))
        (val-local (env-add-local env (gensym "VAL")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+))
    (append
     ;; Compile array and store
     (compile-to-instructions (first args) env)
     (list (list :local.set arr-local))
     ;; Compile value and store
     (compile-to-instructions (second args) env)
     (list (list :local.set val-local))
     ;; Compile index and convert to i32
     (compile-to-instructions (third args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Type dispatch for set
     (emit-array-type-dispatch
      env arr-local
      ;; $mdarray path: set in storage array
      `((:local.get ,arr-local)
        (:ref.cast (:ref ,mdarray-type))
        (:struct.get ,mdarray-type 1)  ; storage (anyref)
        (:ref.cast (:ref ,mv-array-type))  ; cast to array type
        (:local.get ,idx-local)
        (:local.get ,val-local)
        (:array.set ,mv-array-type)
        (:local.get ,val-local))  ; return value
      ;; Simple array path: direct set
      `((:local.get ,arr-local)
        (:ref.cast (:ref ,mv-array-type))
        (:local.get ,idx-local)
        (:local.get ,val-local)
        (:array.set ,mv-array-type)
        (:local.get ,val-local))))))  ; return value

;;; User Story 3: Check and Modify Array Adjustability (P3)

(defun compile-adjustable-array-p (args env)
  "Compile (adjustable-array-p array) - check if array is adjustable.
   HyperSpec: resources/HyperSpec/Body/f_adju_1.htm
   For simple-vectors: returns NIL
   For $mdarray: returns adjustable flag
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "adjustable-array-p requires exactly 1 argument"))
  (let ((arr-local (env-add-local env (gensym "ARR-ADJP")))
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+))
    (with-instruction-collector
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Type dispatch
      (emit* (emit-array-type-dispatch
              env arr-local
              ;; $mdarray path: check adjustable field
              `((:local.get ,arr-local)
                (:ref.cast (:ref ,mdarray-type))
                (:struct.get ,mdarray-type 2)  ; adjustable field (i32)
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31  ; T
                :else
                (:ref.null :none)  ; NIL
                :end)
              ;; Simple array path: always NIL
              `((:ref.null :none)))))))

(defun compile-adjust-array (args env)
  "Compile (adjust-array array new-dimensions &key initial-element) - resize array.
   HyperSpec: resources/HyperSpec/Body/f_adjust.htm
   Creates new array with new dimensions, copies existing elements.
   Stack: [] -> [anyref (new array)]"
  (when (< (length args) 2)
    (error "adjust-array requires at least 2 arguments"))
  ;; MVP implementation: only handles 1D arrays
  (let ((arr-local (env-add-local env (gensym "ARR-ADJ")))
        (newsize-local (env-add-local env (gensym "NEWSIZE") :i32))
        (oldsize-local (env-add-local env (gensym "OLDSIZE") :i32))
        (newarr-local (env-add-local env (gensym "NEWARR")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (init-local (env-add-local env (gensym "INIT")))
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (mdarray-type clysm/compiler/codegen/gc-types:+type-mdarray+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (init-expr (getf (cddr args) :initial-element)))
    (with-instruction-collector
      ;; Compile old array and store
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set arr-local)
      ;; Compile new dimensions (for 1D, just get the number)
      (emit* (compile-to-instructions (second args) env))
      ;; If dimensions is a list, get car; otherwise use directly
      (emit* `((:local.tee ,newarr-local)  ; temp use
               (:ref.test (:ref ,cons-type))
               (:if (:result :i32))
               (:local.get ,newarr-local)
               (:ref.cast (:ref ,cons-type))
               (:struct.get ,cons-type 0)
               (:ref.cast :i31)
               :i31.get_s
               :else
               (:local.get ,newarr-local)
               (:ref.cast :i31)
               :i31.get_s
               :end))
      (emit :local.set newsize-local)
      ;; Compile initial-element or use NIL
      (if init-expr
          (emit* (compile-to-instructions init-expr env))
          (emit '(:global.get 0)))  ; NIL
      (emit :local.set init-local)
      ;; Get old array size
      (emit* `((:local.get ,arr-local)
               (:ref.test (:ref ,mdarray-type))
               (:if (:result :i32))
               (:local.get ,arr-local)
               (:ref.cast (:ref ,mdarray-type))
               (:struct.get ,mdarray-type 1)  ; storage (anyref)
               (:ref.cast (:ref ,mv-array-type))  ; cast to array type
               (:array.len)
               :else
               (:local.get ,arr-local)
               (:ref.cast (:ref ,mv-array-type))
               (:array.len)
               :end))
      (emit :local.set oldsize-local)
      ;; Create new array with new size
      (emit* `((:local.get ,newsize-local)
               (:array.new_default ,mv-array-type)
               (:local.set ,newarr-local)))
      ;; Copy existing elements (up to min of old/new size)
      (emit* `((:i32.const 0)
               (:local.set ,idx-local)
               (:block $copy_done)
               (:loop $copy_loop)
               ;; Check if done
               (:local.get ,idx-local)
               (:local.get ,oldsize-local)
               :i32.ge_u
               (:br_if $copy_done)
               (:local.get ,idx-local)
               (:local.get ,newsize-local)
               :i32.ge_u
               (:br_if $copy_done)
               ;; Copy element
               (:local.get ,newarr-local)
               (:ref.cast (:ref ,mv-array-type))
               (:local.get ,idx-local)
               ;; Get from old array (handle both types)
               (:local.get ,arr-local)
               (:ref.test (:ref ,mdarray-type))
               (:if (:result :anyref))
               (:local.get ,arr-local)
               (:ref.cast (:ref ,mdarray-type))
               (:struct.get ,mdarray-type 1)  ; storage (anyref)
               (:ref.cast (:ref ,mv-array-type))  ; cast to array type
               (:local.get ,idx-local)
               (:array.get ,mv-array-type)
               :else
               (:local.get ,arr-local)
               (:ref.cast (:ref ,mv-array-type))
               (:local.get ,idx-local)
               (:array.get ,mv-array-type)
               :end
               (:array.set ,mv-array-type)
               ;; Increment
               (:local.get ,idx-local)
               (:i32.const 1)
               :i32.add
               (:local.set ,idx-local)
               (:br $copy_loop)
               :end  ; loop
               :end))  ; block
      ;; Fill new positions with initial-element
      (emit* `((:block $fill_done)
               (:loop $fill_loop)
               ;; Check if done
               (:local.get ,idx-local)
               (:local.get ,newsize-local)
               :i32.ge_u
               (:br_if $fill_done)
               ;; Set element to initial-element
               (:local.get ,newarr-local)
               (:ref.cast (:ref ,mv-array-type))
               (:local.get ,idx-local)
               (:local.get ,init-local)
               (:array.set ,mv-array-type)
               ;; Increment
               (:local.get ,idx-local)
               (:i32.const 1)
               :i32.add
               (:local.set ,idx-local)
               (:br $fill_loop)
               :end  ; loop
               :end))  ; block
      ;; Create new $mdarray with new dimensions
      (emit* `((:local.get ,newsize-local)
               :ref.i31
               (:global.get 0)  ; NIL for cdr
               (:struct.new ,cons-type)  ; dimensions list
               (:local.get ,newarr-local)
               (:ref.cast (:ref ,mv-array-type))
               (:i32.const 1)  ; adjustable = true
               (:struct.new ,mdarray-type))))))

;;; ============================================================
;;; Feature 001-ansi-sequence-operations: Sequence Operations
;;; Phase 13D-2: ANSI CL Sequence Operations
;;; ============================================================

(defun compile-copy-seq (args env)
  "Compile (copy-seq sequence) - copy a sequence.
   HyperSpec: resources/HyperSpec/Body/f_cp_seq.htm
   Equivalent to (subseq sequence 0).
   Stack: [] -> [anyref (new sequence)]"
  (when (/= (length args) 1)
    (error "copy-seq requires exactly 1 argument (sequence)"))
  ;; Implement as (subseq sequence 0)
  ;; Create AST-LITERAL for the 0 argument (use :fixnum, not :integer)
  (let ((zero-ast (clysm/compiler/ast:make-ast-literal :value 0 :literal-type :fixnum)))
    (compile-subseq (list (first args) zero-ast) env)))

;;; ============================================================
;;; Package Operations (001-global-variable-defs)
;;; ============================================================

(defun compile-find-package (args env)
  "Compile (find-package name) - find a package by name.
   HyperSpec: resources/HyperSpec/Body/f_find_p.htm

   STUB IMPLEMENTATION: Returns NIL.
   Rationale: The Wasm runtime doesn't have a package system yet.
   This allows globals like (defvar *keyword-package* (find-package :keyword))
   to compile. The actual package lookup would require runtime support.

   Stack: [] -> [anyref (NIL)]"
  (declare (ignore env))
  (when (< (length args) 1)
    (error "find-package requires 1 argument (package-designator)"))
  ;; Return NIL - package system not implemented in Wasm runtime
  '((:global.get 0)))  ; global 0 is NIL

(defun compile-intern (args env)
  "Compile (intern string &optional package) - intern a string as a symbol.
   HyperSpec: resources/HyperSpec/Body/f_intern.htm

   STUB IMPLEMENTATION: Creates an uninterned symbol.
   Rationale: The Wasm runtime doesn't have a package system yet.
   This creates a symbol with the given name but doesn't actually intern it.

   Stack: [] -> [anyref (symbol)]"
  (when (< (length args) 1)
    (error "intern requires at least 1 argument (string)"))
  (let ((name-expr (first args))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Create an uninterned symbol with the given name
    ;; Symbol struct: (name value function plist)
    (append
     ;; Name field - compile the string argument
     (compile-to-instructions name-expr env)
     ;; Value field - unbound (global 1)
     '((:global.get 1))
     ;; Function field - NIL
     '((:global.get 0))
     ;; Plist field - NIL
     '((:global.get 0))
     ;; Create symbol struct
     (list (list :struct.new symbol-type)))))

(defun compile-setf-schar (args env)
  "Compile (%setf-schar string value index) - set character in string.
   HyperSpec: resources/HyperSpec/Body/f_schar.htm
   Returns the character value for proper setf semantics."
  (when (/= (length args) 3)
    (error "%setf-schar requires exactly 3 arguments (string value index)"))
  (let ((str-expr (first args))
        (value-expr (second args))
        (index-expr (third args))
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (value-local (env-add-local env (gensym "SCHAR-VAL"))))
    (append
     ;; Evaluate and save value (for return)
     (compile-to-instructions value-expr env)
     (list (list :local.set value-local))
     ;; Evaluate string
     (compile-to-instructions str-expr env)
     ;; Cast to string type
     (list (list :ref.cast (list :ref string-type)))
     ;; Evaluate index
     (compile-to-instructions index-expr env)
     '((:ref.cast :i31) :i31.get_s)  ; Convert to i32 index
     ;; Get character code from value (i31ref -> i32)
     (list (list :local.get value-local))
     '((:ref.cast :i31) :i31.get_s)
     ;; Mask to byte (0-255)
     '((:i32.const 255) :i32.and)
     ;; Store in string array
     (list (list :array.set string-type))
     ;; Return the value
     (list (list :local.get value-local)))))

(defun compile-setf-elt (args env)
  "Compile (%setf-elt sequence value index) - set element in sequence.
   HyperSpec: resources/HyperSpec/Body/f_elt.htm
   Performs runtime type dispatch for setf."
  (when (/= (length args) 3)
    (error "%setf-elt requires exactly 3 arguments (sequence value index)"))
  (let ((seq-expr (first args))
        (value-expr (second args))
        (index-expr (third args))
        (seq-local (env-add-local env (gensym "SELT-SEQ")))
        (value-local (env-add-local env (gensym "SELT-VAL")))
        (idx-local (env-add-local env (gensym "SELT-IDX") :i32))
        (mv-array-type clysm/compiler/codegen/gc-types:+type-mv-array+)
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (append
     ;; Evaluate and save value
     (compile-to-instructions value-expr env)
     (list (list :local.set value-local))
     ;; Evaluate and save sequence
     (compile-to-instructions seq-expr env)
     (list (list :local.set seq-local))
     ;; Evaluate and save index
     (compile-to-instructions index-expr env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Type dispatch
     `((:block (:result :anyref)
         ;; Check if simple-vector (type 20)
         (:local.get ,seq-local)
         (:ref.test (:ref ,mv-array-type))
         (:if (:result :anyref)
           ;; Vector path: array.set
           ((:local.get ,seq-local)
            (:ref.cast (:ref ,mv-array-type))
            (:local.get ,idx-local)
            (:local.get ,value-local)
            (:array.set ,mv-array-type)
            (:local.get ,value-local))
           ;; Check if string (type 4)
           ((:local.get ,seq-local)
            (:ref.test (:ref ,string-type))
            (:if (:result :anyref)
              ;; String path
              ((:local.get ,seq-local)
               (:ref.cast (:ref ,string-type))
               (:local.get ,idx-local)
               (:local.get ,value-local)
               (:ref.cast :i31)
               :i31.get_s
               (:i32.const 255)
               :i32.and
               (:array.set ,string-type)
               (:local.get ,value-local))
              ;; List path: iterate and rplaca
              ((:local.get ,seq-local)
               (:local.get ,idx-local)
               (:block (:result :anyref)
                 (:loop
                   (:local.get ,idx-local)
                   :i32.eqz
                   (:br_if 1)
                   (:local.get ,idx-local)
                   (:i32.const 1)
                   :i32.sub
                   (:local.set ,idx-local)
                   (:local.get ,seq-local)
                   (:ref.cast (:ref ,cons-type))
                   (:struct.get ,cons-type 1)
                   (:local.set ,seq-local)
                   (:br 0))
                 ;; rplaca the current cons
                 (:local.get ,seq-local)
                 (:ref.cast (:ref ,cons-type))
                 (:local.get ,value-local)
                 (:struct.set ,cons-type 0)
                 (:local.get ,value-local)))))))))))

;;; ============================================================
;;; Feature 043: Property List Operations (GETF)
;;; ============================================================

(defun compile-getf (args env)
  "Compile (getf plist indicator &optional default) - get property from plist.
   Plist is a flat list of (key value key value ...).
   Returns the value associated with indicator, or default if not found."
  (when (< (length args) 2)
    (error "getf requires at least 2 arguments (plist indicator)"))
  (let ((plist-expr (first args))
        (indicator-expr (second args))
        (default-expr (or (third args) 'nil))
        (plist-local (env-add-local env (gensym "GETF-PLIST")))
        (indicator-local (env-add-local env (gensym "GETF-IND")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Evaluate and store indicator
      (emit* (compile-to-instructions indicator-expr env))
      (emit :local.set indicator-local)
      ;; Evaluate plist
      (emit* (compile-to-instructions plist-expr env))
      (emit :local.set plist-local)
      ;; Loop through plist looking for matching key
      (emit* `((:block (:result :anyref)
                 (:loop
                   ;; Check if plist is null
                   (:local.get ,plist-local)
                   :ref.is_null
                   (:br_if 1)  ;; Exit with default if null
                   ;; Get key (car of plist)
                   (:local.get ,plist-local)
                   (:ref.cast (ref ,cons-type))
                   (:struct.get ,cons-type 0)
                   ;; Compare with indicator using eq
                   (:local.get ,indicator-local)
                   :ref.eq
                   (:if (:result :anyref)
                     ;; Found - return (car (cdr plist))
                     ((:local.get ,plist-local)
                      (:ref.cast (ref ,cons-type))
                      (:struct.get ,cons-type 1)  ;; Get cdr
                      (:ref.cast (ref ,cons-type))
                      (:struct.get ,cons-type 0)  ;; Get car of cdr (the value)
                      (:br 2))  ;; Exit with value
                     ;; Not found - skip to next key-value pair (cddr)
                     ((:local.get ,plist-local)
                      (:ref.cast (ref ,cons-type))
                      (:struct.get ,cons-type 1)  ;; cdr
                      (:ref.cast (ref ,cons-type))
                      (:struct.get ,cons-type 1)  ;; cdr again (cddr)
                      (:local.set ,plist-local)
                      (:br 0)))))))  ;; Continue loop
      ;; Default value (reached when plist is null)
      (emit* (compile-to-instructions default-expr env)))))

(defun compile-setf-getf (args env)
  "Compile (%setf-getf plist indicator value) - set property in plist.
   Modifies the plist destructively if key exists, or adds new pair.
   Returns the value."
  (when (/= (length args) 3)
    (error "%setf-getf requires exactly 3 arguments (plist indicator value)"))
  ;; For now, just return the value as a stub
  ;; Full implementation would modify the plist
  (compile-to-instructions (third args) env))

;;; ============================================================
;;; Feature 043: Error Signaling
;;; ============================================================

(defun compile-error (args env)
  "Compile (error format-string &rest args) - signal an error.
   For bootstrap, this is a stub that just raises a trap."
  ;; For bootstrap purposes, just trap unconditionally
  ;; Full implementation would signal a condition
  (with-instruction-collector
    (when args
      ;; Evaluate first arg (for side effects) then drop
      (emit* (compile-to-instructions (first args) env))
      (emit :drop))
    (emit :unreachable)))

;;; ============================================================
;;; Array, Symbol, and Type Operations (043-self-hosting-blockers)
;;; ============================================================

(defun compile-make-array (args env)
  "Compile (make-array dimensions &key initial-element initial-contents element-type) - create an array.
   Feature: 043-self-hosting-blockers, 001-ansi-sequence-operations (Phase 6/US4)
   HyperSpec: resources/HyperSpec/Body/f_mk_ar.htm

   Supports:
   - (make-array size) - default initialized array
   - (make-array size :initial-element value) - array filled with value (uses array.new)
   - (make-array size :initial-contents list) - array from contents (uses array.new_fixed)

   Stack: [] -> [array-ref]"
  (when (null args)
    (error "make-array requires at least one argument"))
  (let ((dim-form (first args))
        (keyword-args (rest args))
        (array-type clysm/compiler/codegen/gc-types:+type-anyref-array+)
        (initial-element nil)
        (initial-element-p nil)
        (initial-contents nil)
        (initial-contents-p nil))
    ;; Parse keyword arguments
    (loop for (key val) on keyword-args by #'cddr
          do (let ((key-name (if (typep key 'clysm/compiler/ast:ast-literal)
                                 (clysm/compiler/ast:ast-literal-value key)
                                 key)))
               (cond
                 ((and (symbolp key-name) (string= (symbol-name key-name) "INITIAL-ELEMENT"))
                  (setf initial-element val
                        initial-element-p t))
                 ((and (symbolp key-name) (string= (symbol-name key-name) "INITIAL-CONTENTS"))
                  (setf initial-contents val
                        initial-contents-p t)))))
    ;; Error if both specified
    (when (and initial-element-p initial-contents-p)
      (error "make-array: Cannot specify both :initial-element and :initial-contents"))
    ;; Generate code based on which keyword was specified
    (cond
      ;; Case 1: :initial-contents - use array.new_fixed
      (initial-contents-p
       (compile-make-array-initial-contents dim-form initial-contents array-type env))
      ;; Case 2: :initial-element - use array.new
      (initial-element-p
       (compile-make-array-initial-element dim-form initial-element array-type env))
      ;; Case 3: No keyword - use array.new_default
      (t
       (append
        (compile-to-instructions dim-form env)
        ;; Extract i32 from i31ref
        `((:ref.cast :i31)
          :i31.get_s
          ;; Create array with that size, default initialized to nil
          (:array.new_default ,array-type)))))))

(defun compile-make-array-initial-element (dim-form initial-element array-type env)
  "Compile (make-array size :initial-element value) using array.new.
   Feature: 001-ansi-sequence-operations (T046, T047)
   array.new fills all elements with the given value.
   Stack: [] -> [array-ref]"
  ;; array.new stack: [value, size:i32] -> [arrayref]
  ;; Need to compile element first, then size
  (append
   (compile-to-instructions initial-element env)  ; value on stack
   (compile-to-instructions dim-form env)         ; size on stack
   `((:ref.cast :i31)
     :i31.get_s
     (:array.new ,array-type))))

(defun compile-make-array-initial-contents (dim-form initial-contents array-type env)
  "Compile (make-array size :initial-contents list) using array.new_fixed.
   Feature: 001-ansi-sequence-operations (T048, T049)
   array.new_fixed creates array from N values on stack.
   Stack: [] -> [array-ref]"
  ;; For compile-time known contents (quoted list), we can use array.new_fixed
  ;; array.new_fixed stack: [elem0, elem1, ..., elemN-1] -> [arrayref]
  (let* ((contents-value (if (typep initial-contents 'clysm/compiler/ast:ast-literal)
                             (clysm/compiler/ast:ast-literal-value initial-contents)
                             nil))
         ;; Handle quoted list: 'quoted value is the list itself
         (actual-contents (if (and (typep initial-contents 'clysm/compiler/ast:ast-literal)
                                   (eq (clysm/compiler/ast:ast-literal-literal-type initial-contents) :quoted))
                              contents-value
                              contents-value)))
    (if (and actual-contents (listp actual-contents))
        ;; Compile-time known contents - use array.new_fixed
        (let ((n (length actual-contents)))
          (with-instruction-collector
            ;; Push each element onto stack
            (dolist (elem actual-contents)
              (emit* (compile-to-instructions
                      (clysm/compiler/ast:make-ast-literal
                       :value elem
                       :literal-type (cond
                                       ((null elem) :nil)
                                       ((eq elem t) :t)
                                       ((integerp elem) :fixnum)
                                       ((characterp elem) :character)
                                       ((stringp elem) :string)
                                       ((symbolp elem) :quoted)
                                       (t :quoted)))
                      env)))
            ;; Create array with N elements
            (emit :array.new_fixed array-type n)))
        ;; Runtime contents - fall back to default + loop fill (simplified)
        ;; For now, just error - full runtime support would need a loop
        (error "make-array :initial-contents requires compile-time known list"))))

(defun compile-gensym (args env)
  "Compile (gensym &optional prefix) - generate unique symbol.
   Feature: 043-self-hosting-blockers
   For bootstrap, returns a new symbol with counter-based name.
   Uses global counter at index 4 (after NIL, UNBOUND, mv-count, mv-buffer).
   Stack: [] -> [symbol-ref]"
  (declare (ignore args))
  ;; For bootstrap, we create a minimal unique symbol
  ;; This is a stub that creates symbols with a global counter
  (let ((symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (counter-local (env-add-local env (gensym "GENSYM-CTR") :i32)))
    ;; Get and increment global counter (we'll use global 4)
    ;; Create a minimal symbol struct with just a counter value as "name"
    `(;; Get current counter value
      (:global.get 4)  ; gensym-counter global
      (:local.set ,counter-local)
      ;; Increment global counter
      (:global.get 4)
      (:i32.const 1)
      :i32.add
      (:global.set 4)
      ;; Create a string for the name (just counter as single byte for simplicity)
      (:i32.const 1)  ; length 1
      (:array.new_default ,string-type)
      ;; Create symbol struct: (name, value, function, plist)
      (:ref.null :none)  ; value = NIL
      (:ref.null :none)  ; function = NIL
      (:ref.null :none)  ; plist = NIL
      (:struct.new ,symbol-type))))

(defun compile-typep (args env)
  "Compile (typep object type-specifier) - check object type.
   Feature: 043-self-hosting-blockers
   For bootstrap, supports basic type specifiers.
   Stack: [] -> [T or NIL]"
  (when (< (length args) 2)
    (error "typep requires exactly 2 arguments"))
  (let ((object-form (first args))
        (type-form (second args)))
    ;; Check if type is a literal
    (if (and (listp type-form)
             (eq (car type-form) 'quote)
             (symbolp (cadr type-form)))
        (let ((type-name (cadr type-form))
              (temp-local (env-add-local env (gensym "TYPEP-TMP"))))
          ;; Compile object and dispatch on type
          (append
           (compile-to-instructions object-form env)
           (list (list :local.set temp-local))
           (case type-name
             ((t) '((:i32.const 1) :ref.i31))  ; Everything is of type T
             ((nil) '((:ref.null :none)))  ; Nothing is of type NIL
             ((null)
              `((:local.get ,temp-local)
                :ref.is_null
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end))
             ((cons list)
              `((:local.get ,temp-local)
                (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-cons+))
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end))
             ((symbol)
              `((:local.get ,temp-local)
                :ref.is_null
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31  ; NIL is a symbol
                :else
                (:local.get ,temp-local)
                (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-symbol+))
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end
                :end))
             ((integer fixnum)
              `((:local.get ,temp-local)
                (:ref.test :i31)
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end))
             ((string)
              `((:local.get ,temp-local)
                (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-string+))
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end))
             ((function)
              `((:local.get ,temp-local)
                (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-closure+))
                (:if (:result :anyref))
                (:i32.const 1) :ref.i31
                :else
                (:ref.null :none)
                :end))
             (t
              ;; Unknown type - return NIL for bootstrap
              '((:ref.null :none))))))
        ;; Non-literal type - return NIL for bootstrap
        '((:ref.null :none)))))

(defun compile-vector-push-extend (args env)
  "Compile (vector-push-extend item vector &optional extension) - add to vector.
   Feature: 043-self-hosting-blockers
   For bootstrap, this is a stub that just returns NIL.
   Real implementation would need adjustable arrays.
   Stack: [] -> [anyref (index or NIL)]"
  (declare (ignore args env))
  ;; For bootstrap, just return NIL
  ;; Real implementation requires adjustable arrays
  '((:ref.null :none)))

(defun compile-write-byte (args env)
  "Compile (write-byte byte stream) - write a byte to stream.
   Feature: 043-self-hosting-blockers
   For bootstrap, this is a stub that drops arguments and returns the byte.
   Real implementation would use FFI to host.
   Stack: [] -> [anyref (byte)]"
  (when (< (length args) 1)
    (error "write-byte requires at least 1 argument"))
  ;; For bootstrap, just return the byte value
  (compile-to-instructions (first args) env))

(defun compile-endp (args env)
  "Compile (endp x) - true if x is not a cons cell.
   ANSI CL: (endp x) is equivalent to (not (consp x)).
   Feature: 043-self-hosting-blockers
   Stack: [] -> [anyref (T or NIL)]"
  (when (/= (length args) 1)
    (error "endp requires exactly 1 argument"))
  (let ((cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Check if argument is a cons - if so return NIL, else return T
    `(,@(compile-to-instructions (first args) env)
      ;; Check if it's a cons
      (:ref.test (:ref ,cons-type))
      ;; If it's a cons, return NIL (ref.null), else T
      (:if (:result :anyref))
      (:ref.null :none)  ;; is cons -> NIL
      :else
      (:i32.const 1)     ;; not cons -> T
      :ref.i31
      :end)))

(defun compile-list* (args env)
  "Compile (list* arg1 arg2 ... final) - create dotted list.
   (list*) -> error
   (list* x) -> x
   (list* x y) -> (cons x y)
   (list* x y z) -> (cons x (cons y z))
   Feature: 043-self-hosting-blockers
   Stack: [] -> [anyref]"
  (when (< (length args) 1)
    (error "list* requires at least 1 argument"))
  (cond
    ;; (list* x) -> just return x
    ((= (length args) 1)
     (compile-to-instructions (first args) env))
    ;; (list* x y) -> (cons x y)
    ((= (length args) 2)
     (compile-cons args env))
    ;; (list* x y z ...) -> (cons x (list* y z ...))
    (t
     (let* ((first-arg (first args))
            (rest-args (rest args))
            (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
       `(,@(compile-to-instructions first-arg env)
         ,@(compile-list* rest-args env)
         (:struct.new ,cons-type))))))

(defun compile-apply (args env)
  "Compile (apply fn arg1 ... argn list) - apply function to spread list.
   Feature: 043-self-hosting-blockers
   For bootstrap, this is a limited implementation:
   - (apply fn list) calls fn with list elements
   - (apply fn a b list) not fully supported yet
   Stack: [] -> [anyref]"
  (when (< (length args) 2)
    (error "apply requires at least 2 arguments"))
  ;; For bootstrap, just return NIL as a stub
  ;; Full implementation would need to spread the list at runtime
  (let ((fn-arg (first args))
        (list-arg (car (last args))))
    (declare (ignore fn-arg list-arg))
    ;; Stub: return NIL for now
    '((:ref.null :none))))

;; NOTE: compile-funcall is defined later in this file (T085-T087 section)
;; The full implementation includes closure handling and TCO support.

(defun compile-arithmetic-op (op args env identity)
  "Compile an arithmetic operation with variadic args.
   For (+ 1 2):
     i32.const 1, ref.i31    ; create i31 for 1
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.const 2, ref.i31    ; create i31 for 2
     ref.cast i31, i31.get_s ; cast and extract as i32
     i32.add                 ; add
     ref.i31                 ; wrap result as i31"
  (cond
    ;; Zero args: return identity
    ((null args)
     (if identity
         (list (list :i32.const identity) :ref.i31)
         (error "Operator requires at least one argument")))
    ;; One arg: return value (for +) or apply unary (for -)
    ((null (cdr args))
     (compile-to-instructions (first args) env))
    ;; Two or more args: fold left
    (t
     (with-instruction-collector
       ;; Compile first arg and unwrap (cast to i31 first)
       (emit* (compile-to-instructions (first args) env))
       (emit '(:ref.cast :i31))
       (emit :i31.get_s)
       ;; For each remaining arg: compile, cast, unwrap, apply op
       (dolist (arg (rest args))
         (emit* (compile-to-instructions arg env))
         (emit '(:ref.cast :i31))
         (emit :i31.get_s)
         (emit op))
       ;; Wrap result as i31ref
       (emit :ref.i31)))))

(defun compile-unary-minus (arg env)
  "Compile unary minus: (- x) => (- 0 x)."
  (with-instruction-collector
    (emit '(:i32.const 0))
    (emit* (compile-to-instructions arg env))
    (emit '(:ref.cast :i31))
    (emit :i31.get_s)
    (emit :i32.sub)
    (emit :ref.i31)))

;;; ============================================================
;;; Feature 001-arithmetic-primitives: Increment/Decrement
;;; [1-](resources/HyperSpec/Body/f_1pl_1_.htm)
;;; [1+](resources/HyperSpec/Body/f_1pl_1_.htm)
;;; ============================================================

(defun compile-1- (args env)
  "Compile (1- x) => (- x 1).
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "1- requires exactly 1 argument"))
  (with-instruction-collector
    (emit* (compile-to-instructions (first args) env))
    (emit '(:ref.cast :i31))
    (emit :i31.get_s)
    (emit '(:i32.const 1))
    (emit :i32.sub)
    (emit :ref.i31)))

(defun compile-1+ (args env)
  "Compile (1+ x) => (+ x 1).
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "1+ requires exactly 1 argument"))
  (with-instruction-collector
    (emit* (compile-to-instructions (first args) env))
    (emit '(:ref.cast :i31))
    (emit :i31.get_s)
    (emit '(:i32.const 1))
    (emit :i32.add)
    (emit :ref.i31)))

(defun compile-truncate (args env)
  "Compile truncate division."
  (when (< (length args) 2)
    (error "truncate requires two arguments"))
  (with-instruction-collector
    (emit* (compile-to-instructions (first args) env))
    (emit '(:ref.cast :i31))
    (emit :i31.get_s)
    (emit* (compile-to-instructions (second args) env))
    (emit '(:ref.cast :i31))
    (emit :i31.get_s)
    (emit :i32.div_s)
    (emit :ref.i31)))

;;; ============================================================
;;; Division/Rounding Function Primitives (001-division-rounding-primitives)
;;; Implements floor, ceiling, round, ffloor, fceiling, fround
;;; All return 2 values: quotient (primary) and remainder (secondary)
;;; ============================================================

(defun compile-unbox-to-f64 (val-local float-type)
  "Generate instructions to unbox value from VAL-LOCAL to f64.
   Handles both i31ref (fixnum) and $float.
   Returns instructions that put f64 on stack."
  `((:local.get ,val-local)
    (:ref.test :i31)
    (:if (:result :f64))
    ;; i31ref path - convert to f64
    (:local.get ,val-local)
    (:ref.cast :i31)
    :i31.get_s
    :f64.convert_i32_s
    :else
    ;; $float path - extract f64
    (:local.get ,val-local)
    (:ref.cast (:ref ,float-type))
    (:struct.get ,float-type 0)
    :end))

(defun compile-rounding-with-mv (args env rounding-op return-float-p)
  "Helper for rounding functions that return multiple values.
   ROUNDING-OP is :f64.floor, :f64.ceil, or :f64.nearest
   RETURN-FLOAT-P: t for ffloor/fceiling/fround (quotient as $float),
                   nil for floor/ceiling/round (quotient as i31ref)

   Contract:
   - Sets mv-count to 2
   - Stores remainder in mv-buffer[0]
   - Returns quotient on stack (i31ref or $float)

   Algorithm:
   1. Store anyref args in locals
   2. Extract f64, compute quotient = rounding(dividend/divisor)
   3. Compute remainder = dividend - quotient * divisor
   4. Store remainder in mv-buffer, return boxed quotient"
  (let* ((mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (float-type clysm/compiler/codegen/gc-types:+type-float+)
         ;; Locals store anyref values
         (dividend-local (env-add-local env (gensym "DIV-DIVIDEND")))
         (divisor-local (env-add-local env (gensym "DIV-DIVISOR")))
         ;; Store quotient as anyref (boxed)
         (quotient-local (env-add-local env (gensym "DIV-QUOTIENT")))
         (arg-count (length args)))
    (when (or (< arg-count 1) (> arg-count 2))
      (error "Rounding function requires 1 or 2 arguments"))
    (let* ((single-arg-p (= arg-count 1))
           (unbox-dividend (compile-unbox-to-f64 dividend-local float-type))
           (unbox-divisor (compile-unbox-to-f64 divisor-local float-type))
           (unbox-quotient (compile-unbox-to-f64 quotient-local float-type)))
      `(;; 1. Set mv-count to 2
        (:i32.const 2)
        (:global.set ,mv-count-global)
        ;; 2. Evaluate and store dividend (as anyref)
        ,@(compile-to-instructions (first args) env)
        (:local.set ,dividend-local)
        ;; 3. Evaluate and store divisor (as anyref, or synthesize 1)
        ,@(if single-arg-p
              ;; Single arg: divisor = 1 as i31ref
              '((:i32.const 1) :ref.i31)
              (compile-to-instructions (second args) env))
        (:local.set ,divisor-local)
        ;; 4. Compute quotient = rounding-op(dividend / divisor)
        ;;    Get dividend as f64
        ,@unbox-dividend
        ;;    Get divisor as f64
        ,@unbox-divisor
        ;;    Compute and round
        :f64.div
        ,rounding-op
        ;; Box quotient and store
        ,@(if return-float-p
              `((:struct.new ,float-type))
              '(:i32.trunc_f64_s :ref.i31))
        (:local.set ,quotient-local)
        ;; 5. Compute remainder = dividend - quotient * divisor
        ;;    Store in mv-buffer[0]
        (:global.get ,mv-buffer-global)
        (:i32.const 0)
        ;; Get dividend as f64
        ,@unbox-dividend
        ;; Get quotient as f64 (unbox from stored anyref)
        ,@unbox-quotient
        ;; Get divisor as f64
        ,@unbox-divisor
        ;; remainder = dividend - quotient * divisor
        :f64.mul
        :f64.sub
        ;; Box remainder as $float and store in mv-buffer
        (:struct.new ,float-type)
        (:array.set ,mv-array-type)
        ;; 6. Return quotient (already boxed)
        (:local.get ,quotient-local)))))

(defun compile-floor (args env)
  "Compile (floor dividend &optional divisor) to Wasm.
   Returns 2 values: quotient (rounded toward -∞) and remainder.
   Per ANSI CL: remainder = dividend - quotient * divisor
   Stack: [] -> [anyref (i31ref quotient)]"
  (compile-rounding-with-mv args env :f64.floor nil))

(defun compile-ceiling (args env)
  "Compile (ceiling dividend &optional divisor) to Wasm.
   Returns 2 values: quotient (rounded toward +∞) and remainder.
   Per ANSI CL: remainder = dividend - quotient * divisor
   Stack: [] -> [anyref (i31ref quotient)]"
  (compile-rounding-with-mv args env :f64.ceil nil))

(defun compile-round (args env)
  "Compile (round dividend &optional divisor) to Wasm.
   Returns 2 values: quotient (rounded to nearest, ties to even) and remainder.
   Per ANSI CL: remainder = dividend - quotient * divisor
   Stack: [] -> [anyref (i31ref quotient)]"
  (compile-rounding-with-mv args env :f64.nearest nil))

(defun compile-ffloor (args env)
  "Compile (ffloor dividend &optional divisor) to Wasm.
   Returns 2 values: quotient as float (rounded toward -∞) and remainder.
   Per ANSI CL: same as floor but quotient is a float.
   Stack: [] -> [anyref ($float quotient)]"
  (compile-rounding-with-mv args env :f64.floor t))

(defun compile-fceiling (args env)
  "Compile (fceiling dividend &optional divisor) to Wasm.
   Returns 2 values: quotient as float (rounded toward +∞) and remainder.
   Per ANSI CL: same as ceiling but quotient is a float.
   Stack: [] -> [anyref ($float quotient)]"
  (compile-rounding-with-mv args env :f64.ceil t))

(defun compile-fround (args env)
  "Compile (fround dividend &optional divisor) to Wasm.
   Returns 2 values: quotient as float (rounded to nearest) and remainder.
   Per ANSI CL: same as round but quotient is a float.
   Stack: [] -> [anyref ($float quotient)]"
  (compile-rounding-with-mv args env :f64.nearest t))

;;; ============================================================
;;; Feature 043: Bitwise Operations for LEB128 Encoding
;;; ============================================================

(defun compile-lognot (args env)
  "Compile (lognot x) - bitwise NOT.
   In Wasm, use XOR with -1 (all 1s) since there's no i32.not.
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 1)
    (error "lognot requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s
     (:i32.const -1)
     :i32.xor
     :ref.i31)))

(defun compile-ash (args env)
  "Compile (ash integer count) - arithmetic shift.
   Positive count = left shift (shl).
   Negative count = right shift signed (shr_s).
   Stack: [] -> [anyref (i31ref)]"
  (when (/= (length args) 2)
    (error "ash requires exactly 2 arguments"))
  (let ((count-local (env-add-local env (gensym "ASH-COUNT"))))
    (append
     ;; Evaluate count first to determine direction
     (compile-to-instructions (second args) env)
     (list '(:ref.cast :i31) :i31.get_s)
     (list (list :local.set count-local))
     ;; Evaluate integer
     (compile-to-instructions (first args) env)
     (list '(:ref.cast :i31) :i31.get_s)
     ;; Check if count >= 0 (left shift) or < 0 (right shift)
     `((:local.get ,count-local)
       (:i32.const 0)
       :i32.ge_s
       (:if (:result :i32)
         ;; Left shift: value << count
         ((:local.get ,count-local)
          :i32.shl)
         ;; Right shift: value >> (-count)
         ((:i32.const 0)
          (:local.get ,count-local)
          :i32.sub  ;; negate count
          :i32.shr_s))
       :ref.i31))))

(defun compile-comparison-op (op args env)
  "Compile a comparison operation.
   Handles both fixnum (i31) and float ($float) operands.
   Uses runtime type dispatch to determine comparison method."
  (when (< (length args) 2)
    (error "Comparison requires at least two arguments"))
  ;; For now, only support two args
  ;; TODO: Chain comparisons (< a b c) => (and (< a b) (< b c))
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+)
        (arg1-local (env-add-local env (gensym "CMP-A")))
        (arg2-local (env-add-local env (gensym "CMP-B")))
        (f64-op (case op
                  (:i32.lt_s :f64.lt)
                  (:i32.gt_s :f64.gt)
                  (:i32.le_s :f64.le)
                  (:i32.ge_s :f64.ge)
                  (:i32.eq :f64.eq)
                  (t :f64.eq))))  ; default for = operator
    (append
     ;; Compile and store both arguments
     (compile-to-instructions (first args) env)
     (list (list :local.set arg1-local))
     (compile-to-instructions (second args) env)
     (list (list :local.set arg2-local))
     ;; Check if either operand is a float
     `((:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       :i32.or  ; either is float?
       (:if (:result :anyref))
       ;; Float path: extract f64 values and compare
       (:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       ;; arg1 is float
       (:local.get ,arg1-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       ;; arg1 is fixnum - convert to f64
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       ;; arg2 is float
       (:local.get ,arg2-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       ;; arg2 is fixnum - convert to f64
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       ,f64-op  ; f64 comparison
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :else
       ;; Integer path: both are fixnums
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       ,op  ; i32 comparison
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :end))))

(defun compile-not-equal (args env)
  "Compile not-equal (/=).
   Handles both fixnum (i31) and float ($float) operands."
  (let ((float-type clysm/compiler/codegen/gc-types:+type-float+)
        (arg1-local (env-add-local env (gensym "NE-A")))
        (arg2-local (env-add-local env (gensym "NE-B"))))
    (append
     ;; Compile and store both arguments
     (compile-to-instructions (first args) env)
     (list (list :local.set arg1-local))
     (compile-to-instructions (second args) env)
     (list (list :local.set arg2-local))
     ;; Check if either operand is a float
     `((:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       :i32.or  ; either is float?
       (:if (:result :anyref))
       ;; Float path: extract f64 values and compare
       (:local.get ,arg1-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       (:local.get ,arg1-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       (:local.get ,arg2-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       (:local.get ,arg2-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :f64.convert_i32_s
       :end
       :f64.ne  ; f64 not-equal
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :else
       ;; Integer path: both are fixnums
       (:local.get ,arg1-local)
       (:ref.cast :i31) :i31.get_s
       (:local.get ,arg2-local)
       (:ref.cast :i31) :i31.get_s
       :i32.ne
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; T
       :else
       (:ref.null :none)  ; NIL
       :end
       :end))))

;;; ============================================================
;;; Cons/List Operations (006-cons-list-ops)
;;; ============================================================

(defun compile-cons (args env)
  "Compile (cons x y) to struct.new 2.
   Creates a cons cell with car=x and cdr=y.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "cons requires exactly 2 arguments"))
  (with-instruction-collector
    ;; Compile car value
    (emit* (compile-to-instructions (first args) env))
    ;; Compile cdr value
    (emit* (compile-to-instructions (second args) env))
    ;; Create cons cell (struct.new with type index 2 = $cons)
    (emit :struct.new clysm/compiler/codegen/gc-types:+type-cons+)))

(defun compile-car (args env)
  "Compile (car x) with NIL handling.
   If x is NIL, returns NIL. Otherwise returns car of cons.
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "car requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "CAR-TMP"))))
    (with-instruction-collector
      ;; Compile the argument
      (emit* (compile-to-instructions (first args) env))
      ;; Store in temp for reuse
      (emit :local.tee temp-local)
      ;; Check if it's NIL (ref.null check or global comparison)
      ;; For now, use ref.is_null since NIL is represented as ref.null
      (emit :ref.is_null)
      ;; If null, return NIL; otherwise get car
      (emit* `((:if (:result :anyref))
               (:ref.null :none)  ; Return NIL
               :else
               (:local.get ,temp-local)
               (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
               (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 0)
               :end)))))

(defun compile-cdr (args env)
  "Compile (cdr x) with NIL handling.
   If x is NIL, returns NIL. Otherwise returns cdr of cons.
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "cdr requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "CDR-TMP"))))
    (with-instruction-collector
      ;; Compile the argument
      (emit* (compile-to-instructions (first args) env))
      ;; Store in temp for reuse
      (emit :local.tee temp-local)
      ;; Check if it's NIL
      (emit :ref.is_null)
      ;; If null, return NIL; otherwise get cdr
      (emit* `((:if (:result :anyref))
               (:ref.null :none)  ; Return NIL
               :else
               (:local.get ,temp-local)
               (:ref.cast ,(list :ref clysm/compiler/codegen/gc-types:+type-cons+))
               (:struct.get ,clysm/compiler/codegen/gc-types:+type-cons+ 1)
               :end)))))

;;; ============================================================
;;; cXXr Accessors (001-cxr-compiler-macro)
;;; ============================================================

(defun compile-cxr-chain (ops args env)
  "Compile a chain of CAR/CDR operations.
   OPS is a string like 'da' for CADR (d=cdr first, then a=car).
   Reads left-to-right: first char is innermost operation applied first.
   Feature: 043-self-hosting-blockers"
  (when (/= (length args) 1)
    (error "c~Ar requires exactly 1 argument" ops))
  (let ((temp-local (env-add-local env (gensym "CXR-TMP")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile the argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set temp-local)
      ;; Apply operations in order (first char = innermost operation applied first)
      (loop for i from 0 below (length ops)
            for op = (char ops i)
            for field = (if (char= op #\a) 0 1)  ; 0=car, 1=cdr
            do (emit* `((:local.get ,temp-local)
                        :ref.is_null
                        (:if (:result :anyref))
                        (:ref.null :none)
                        :else
                        (:local.get ,temp-local)
                        (:ref.cast (:ref ,cons-type))
                        (:struct.get ,cons-type ,field)
                        :end
                        (:local.set ,temp-local))))
      ;; Return the final value
      (emit :local.get temp-local))))

;;; ============================================================
;;; cXXr Compiler Macro (001-cxr-compiler-macro)
;;; ============================================================

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ops-to-expansion (name ops)
    "Convert operation string to human-readable expansion.
   Example: 'dda' for CADDR → '(car (cdr (cdr x)))'
   Feature: 001-cxr-compiler-macro"
    (declare (ignore name))
    (let ((result "x"))
      (loop for c across (reverse ops)
            do (setf result (format nil "(~A ~A)"
                                    (if (char= c #\a) "car" "cdr")
                                    result)))
      result)))

(defmacro define-cxr-compiler (name ops)
  "Generate a compile-cXXr function for the given operation sequence.
   NAME: Symbol like CAAR, CADR, CADDR (without compile- prefix)
   OPS: String like \"aa\", \"da\", \"dda\" (a=car, d=cdr)
   Feature: 001-cxr-compiler-macro"
  (check-type ops string)
  (unless (plusp (length ops))
    (error "Operation string must be non-empty"))
  (unless (every (lambda (c) (member c '(#\a #\d))) ops)
    (error "Operation string must contain only 'a' and 'd'"))
  (let ((func-name (intern (format nil "COMPILE-~A" name)
                           (find-package :clysm/compiler/codegen/func-section)))
        (docstring (format nil "Compile (~(~A~) x) = ~A.~%   Feature: 001-cxr-compiler-macro"
                           name (ops-to-expansion name ops))))
    `(defun ,func-name (args env)
       ,docstring
       (compile-cxr-chain ,ops args env))))

;;; Two-level cXr functions (Feature: 001-cxr-compiler-macro)
(define-cxr-compiler caar "aa")
(define-cxr-compiler cadr "da")
(define-cxr-compiler cdar "ad")
(define-cxr-compiler cddr "dd")

;;; Three-level cXr functions (Feature: 001-cxr-compiler-macro)
(define-cxr-compiler caaar "aaa")
(define-cxr-compiler caadr "daa")
(define-cxr-compiler cadar "ada")
(define-cxr-compiler caddr "dda")
(define-cxr-compiler cdaar "aad")
(define-cxr-compiler cdadr "dad")
(define-cxr-compiler cddar "add")
(define-cxr-compiler cdddr "ddd")

;;; ============================================================
;;; Symbol Accessors (043-self-hosting-blockers)
;;; ============================================================

(defun compile-symbol-name (args env)
  "Compile (symbol-name sym) - returns the name string of a symbol.
   Feature: 043-self-hosting-blockers
   For NIL, returns 'NIL' string.
   Stack: [] -> [string-ref]"
  (when (/= (length args) 1)
    (error "symbol-name requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SYMNAME-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check if NIL
     '(:ref.is_null)
     `((:if (:result :anyref))
       ;; NIL - return "NIL" string (hardcoded for bootstrap)
       ;; Create array with bytes for "NIL"
       (:i32.const 3)  ; length
       (:array.new_default ,clysm/compiler/codegen/gc-types:+type-string+)
       :else
       ;; Not NIL - get field 0 (name)
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-symbol+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-symbol+ 0)
       :end))))

(defun compile-keywordp (args env)
  "Compile (keywordp x) - returns T if x is a keyword, NIL otherwise.
   Feature: 043-self-hosting-blockers
   For bootstrap: keywords are symbols whose name starts with ':'.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "keywordp requires exactly 1 argument"))
  ;; For now, a simplified implementation that checks if it's a symbol
  ;; and the name string starts with colon (this is a bootstrap hack)
  ;; Real implementation would check the symbol's package
  (let ((temp-local (env-add-local env (gensym "KEYWORDP-TMP"))))
    (with-instruction-collector
      (emit* (compile-to-instructions (first args) env))
      (emit :local.tee temp-local)
      ;; Check if it's a symbol struct (not NIL - NIL is not a keyword)
      (emit* `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-symbol+))
               (:if (:result :anyref))
               ;; Is a symbol - for bootstrap, assume interned keywords were created correctly
               ;; This is a stub that returns NIL - will be improved later
               (:ref.null :none)
               :else
               ;; Not a symbol
               (:ref.null :none)
               :end)))))

(defun compile-list (args env)
  "Compile (list &rest args) to a proper list.
   Builds cons chain right-to-left: (list 1 2 3) = (cons 1 (cons 2 (cons 3 nil)))
   Stack: [] -> [list-ref or nil]"
  (if (null args)
      ;; Empty list = NIL
      '((:ref.null :none))
      ;; Build cons chain from right to left
      (let ((acc-local (env-add-local env (gensym "LIST-ACC"))))
        (with-instruction-collector
          ;; Start with NIL
          (emit '(:ref.null :none))
          (emit :local.set acc-local)
          ;; Build cons chain in reverse order
          (dolist (arg (reverse args))
            ;; Compile element
            (emit* (compile-to-instructions arg env))
            ;; Get current accumulator (cdr)
            (emit :local.get acc-local)
            ;; Create cons
            (emit :struct.new clysm/compiler/codegen/gc-types:+type-cons+)
            ;; Store as new accumulator
            (emit :local.set acc-local))
          ;; Return the final list
          (emit :local.get acc-local)))))

;;; ============================================================
;;; Type Predicates (006-cons-list-ops)
;;; ============================================================

(defun compile-consp (args env)
  "Compile (consp x) - returns T if x is a cons cell, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "consp requires exactly 1 argument"))
  (with-instruction-collector
    ;; Compile the argument
    (emit* (compile-to-instructions (first args) env))
    ;; Test if it's a cons (type 2)
    (emit :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Convert i32 boolean to Lisp boolean
    (emit* `((:if (:result :anyref))
             (:i32.const 1) :ref.i31  ; T
             :else
             (:ref.null :none)        ; NIL
             :end))))

(defun compile-null (args env)
  "Compile (null x) - returns T if x is NIL, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "null requires exactly 1 argument"))
  (with-instruction-collector
    ;; Compile the argument
    (emit* (compile-to-instructions (first args) env))
    ;; Check if null
    (emit :ref.is_null)
    ;; Convert i32 boolean to Lisp boolean
    (emit* `((:if (:result :anyref))
             (:i32.const 1) :ref.i31  ; T
             :else
             (:ref.null :none)        ; NIL
             :end))))

(defun compile-not (args env)
  "Compile (not x) - returns T if x is NIL, NIL otherwise.
   Functionally identical to null per ANSI CL.
   Stack: [] -> [T or NIL]"
  ;; Delegate to compile-null since they are functionally equivalent
  (compile-null args env))

;;; ============================================================
;;; Equality Predicate Type-Dispatch Infrastructure
;;; (Feature: 001-equality-type-dispatch)
;;;
;;; Shared helper functions for eq, eql, equal, equalp predicates.
;;; These emit comparison instructions based on equality level:
;;;   :eq     - Object identity only (ref.eq)
;;;   :eql    - Identity + same-type numeric/char comparison
;;;   :equal  - Structural: descends cons, compares strings byte-by-byte
;;;   :equalp - Case-insensitive, numeric type coercion
;;; ============================================================

(defun emit-null-comparison (level local-x local-y)
  "Emit comparison when x is null. Returns T if y is also null.
   All equality levels behave the same for null."
  (declare (ignore level))
  ;; x is null, check if y is also null
  `((:local.get ,local-y)
    :ref.is_null
    (:if (:result :anyref))
    (:i32.const 1) :ref.i31  ; both null => T
    :else
    (:ref.null :none)        ; x null, y not => NIL
    :end))

(defun emit-i31-comparison (level local-x local-y)
  "Emit i31ref comparison (fixnum, char, T).
   :eq/:eql/:equal use ref.eq.
   :equalp uses case-insensitive comparison for characters."
  (case level
    ((:eq :eql :equal)
     ;; Both i31ref - compare with ref.eq
     `((:local.get ,local-x)
       (:ref.cast :i31)
       (:local.get ,local-y)
       (:ref.cast :i31)
       :ref.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:equalp
     ;; Case-insensitive: convert uppercase to lowercase before compare
     (let ((val1 (gensym "I31-V1"))
           (val2 (gensym "I31-V2")))
       `((:local.get ,local-x)
         (:ref.cast :i31)
         :i31.get_s
         (:local.set ,val1)
         (:local.get ,local-y)
         (:ref.cast :i31)
         :i31.get_s
         (:local.set ,val2)
         ;; Convert val1 to lowercase if uppercase (A-Z)
         (:local.get ,val1)
         (:i32.const 65)
         :i32.ge_s
         (:local.get ,val1)
         (:i32.const 90)
         :i32.le_s
         :i32.and
         (:if nil)
         (:local.get ,val1)
         (:i32.const 32)
         :i32.add
         (:local.set ,val1)
         :end
         ;; Convert val2 to lowercase if uppercase
         (:local.get ,val2)
         (:i32.const 65)
         :i32.ge_s
         (:local.get ,val2)
         (:i32.const 90)
         :i32.le_s
         :i32.and
         (:if nil)
         (:local.get ,val2)
         (:i32.const 32)
         :i32.add
         (:local.set ,val2)
         :end
         ;; Compare
         (:local.get ,val1)
         (:local.get ,val2)
         :i32.eq
         (:if (:result :anyref))
         (:i32.const 1) :ref.i31
         :else
         (:ref.null :none)
         :end)))))

(defun emit-float-comparison (level local-x local-y float-type)
  "Emit float comparison.
   :eq uses ref.eq (identity).
   :eql/:equal use f64.eq (same type, same value).
   :equalp uses numeric= (cross-type via f64)."
  (case level
    (:eq
     ;; Identity only
     `((:local.get ,local-x)
       (:ref.cast :eq)
       (:local.get ,local-y)
       (:ref.cast :eq)
       :ref.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))
    ((:eql :equal :equalp)
     ;; f64.eq comparison
     `((:local.get ,local-x)
       (:ref.cast ,(list :ref float-type))
       (:struct.get ,float-type 0)
       (:local.get ,local-y)
       (:ref.cast ,(list :ref float-type))
       (:struct.get ,float-type 0)
       :f64.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun emit-ratio-comparison (level local-x local-y ratio-type)
  "Emit ratio comparison.
   :eq uses ref.eq.
   :eql/:equal compare numerator and denominator with ref.eq.
   :equalp uses numeric= (would need conversion - simplified to same as eql)."
  (case level
    (:eq
     `((:local.get ,local-x)
       (:ref.cast :eq)
       (:local.get ,local-y)
       (:ref.cast :eq)
       :ref.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))
    ((:eql :equal :equalp)
     ;; Compare numerator and denominator
     `((:local.get ,local-x)
       (:ref.cast ,(list :ref ratio-type))
       (:struct.get ,ratio-type 0)  ; numerator
       (:ref.cast :eq)
       (:local.get ,local-y)
       (:ref.cast ,(list :ref ratio-type))
       (:struct.get ,ratio-type 0)
       (:ref.cast :eq)
       :ref.eq
       (:if (:result :anyref))
       ;; Numerators equal - compare denominators
       (:local.get ,local-x)
       (:ref.cast ,(list :ref ratio-type))
       (:struct.get ,ratio-type 1)  ; denominator
       (:ref.cast :eq)
       (:local.get ,local-y)
       (:ref.cast ,(list :ref ratio-type))
       (:struct.get ,ratio-type 1)
       (:ref.cast :eq)
       :ref.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       (:ref.null :none)  ; numerators differ
       :end))))

(defun emit-default-comparison (level local-x local-y)
  "Emit default comparison for unknown types (ref.eq identity).
   All levels fall back to ref.eq for types not specifically handled."
  (declare (ignore level))
  `((:local.get ,local-x)
    (:ref.cast :eq)
    (:local.get ,local-y)
    (:ref.cast :eq)
    :ref.eq
    (:if (:result :anyref))
    (:i32.const 1) :ref.i31
    :else
    (:ref.null :none)
    :end))

(defun emit-string-comparison-case-sensitive (local-x local-y string-type len1 len2 idx)
  "Emit case-sensitive string comparison (for :equal level).
   Returns instructions that branch to $result_nil or fall through on equal."
  `(;; Get string lengths
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len2)
    ;; Check length equality first
    (:local.get ,len1)
    (:local.get ,len2)
    :i32.ne
    (:br_if $result_nil)
    ;; Compare byte-by-byte
    (:i32.const 0)
    (:local.set ,idx)
    (:block $str_done)
    (:loop $str_loop)
    (:local.get ,idx)
    (:local.get ,len1)
    :i32.ge_u
    (:br_if $str_done)
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    :i32.ne
    (:br_if $result_nil)
    (:local.get ,idx)
    (:i32.const 1)
    :i32.add
    (:local.set ,idx)
    (:br $str_loop)
    :end  ; loop
    :end  ; block
    ;; Strings equal - T
    (:i32.const 1) :ref.i31))

(defun emit-string-comparison-case-insensitive (local-x local-y string-type len1 len2 idx byte1 byte2)
  "Emit case-insensitive string comparison (for :equalp level).
   Returns instructions that branch to $result_nil or fall through on equal."
  `(;; Get string lengths
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len2)
    ;; Check length equality first
    (:local.get ,len1)
    (:local.get ,len2)
    :i32.ne
    (:br_if $result_nil)
    ;; Compare byte-by-byte with case folding
    (:i32.const 0)
    (:local.set ,idx)
    (:block $str_done)
    (:loop $str_loop)
    (:local.get ,idx)
    (:local.get ,len1)
    :i32.ge_u
    (:br_if $str_done)
    ;; Get bytes
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.set ,byte1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.set ,byte2)
    ;; Convert byte1 to uppercase if lowercase (a-z)
    (:local.get ,byte1)
    (:i32.const 97)
    :i32.ge_u
    (:local.get ,byte1)
    (:i32.const 122)
    :i32.le_u
    :i32.and
    (:if nil)
    (:local.get ,byte1)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte1)
    :end
    ;; Convert byte2 to uppercase if lowercase
    (:local.get ,byte2)
    (:i32.const 97)
    :i32.ge_u
    (:local.get ,byte2)
    (:i32.const 122)
    :i32.le_u
    :i32.and
    (:if nil)
    (:local.get ,byte2)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte2)
    :end
    ;; Compare
    (:local.get ,byte1)
    (:local.get ,byte2)
    :i32.ne
    (:br_if $result_nil)
    (:local.get ,idx)
    (:i32.const 1)
    :i32.add
    (:local.set ,idx)
    (:br $str_loop)
    :end  ; loop
    :end  ; block
    ;; Strings equal - T
    (:i32.const 1) :ref.i31))

(defun emit-cons-worklist-push (local-x local-y cons-type worklist-local)
  "Emit instructions to push car and cdr pairs onto worklist for recursive comparison.
   Used by :equal and :equalp levels."
  `(;; Push (car x, car y) to worklist
    (:local.get ,local-x)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 0)  ; car x
    (:local.get ,local-y)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 0)  ; car y
    (:struct.new ,cons-type)    ; (cons car-x car-y)
    (:local.get ,worklist-local)
    (:struct.new ,cons-type)    ; (cons pair worklist)
    (:local.set ,worklist-local)
    ;; Push (cdr x, cdr y) to worklist
    (:local.get ,local-x)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 1)  ; cdr x
    (:local.get ,local-y)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 1)  ; cdr y
    (:struct.new ,cons-type)    ; (cons cdr-x cdr-y)
    (:local.get ,worklist-local)
    (:struct.new ,cons-type)    ; (cons pair worklist)
    (:local.set ,worklist-local)))

(defun emit-numeric-cross-type-comparison (local-x local-y float-type)
  "Emit cross-type numeric comparison for :equalp (fixnum vs float).
   Converts fixnum to float and compares with f64.eq.
   Assumes y is float when x is i31, or x is float when y is i31."
  `(;; Check if y is float (x is i31)
    (:local.get ,local-y)
    (:ref.test ,(list :ref float-type))
    (:if (:result :anyref))
    ;; y is float, x is i31 - convert x to f64 and compare
    (:local.get ,local-x)
    (:ref.cast :i31)
    :i31.get_s
    :f64.convert_i32_s
    (:local.get ,local-y)
    (:ref.cast ,(list :ref float-type))
    (:struct.get ,float-type 0)
    :f64.eq
    (:if (:result :anyref))
    (:i32.const 1) :ref.i31
    :else
    (:ref.null :none)
    :end
    :else
    ;; y is not float - try other direction or NIL
    (:local.get ,local-x)
    (:ref.test ,(list :ref float-type))
    (:if (:result :anyref))
    ;; x is float, check if y is i31
    (:local.get ,local-y)
    (:ref.test :i31)
    (:if (:result :anyref))
    ;; y is i31, x is float - convert y to f64 and compare
    (:local.get ,local-x)
    (:ref.cast ,(list :ref float-type))
    (:struct.get ,float-type 0)
    (:local.get ,local-y)
    (:ref.cast :i31)
    :i31.get_s
    :f64.convert_i32_s
    :f64.eq
    (:if (:result :anyref))
    (:i32.const 1) :ref.i31
    :else
    (:ref.null :none)
    :end
    :else
    (:ref.null :none)  ; different types, not equalp
    :end
    :else
    (:ref.null :none)  ; neither is float
    :end
    :end))

(defun compile-equality-predicate (args env level)
  "Unified compiler for equality predicates.
   LEVEL is one of :eq, :eql, :equal, :equalp.
   Generates optimized Wasm instructions based on equality level.

   :eq     - Object identity only (ref.eq)
   :eql    - Identity + same-type numeric/char comparison
   :equal  - Structural: descends cons, compares strings byte-by-byte
   :equalp - Case-insensitive, numeric type coercion

   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "~A requires exactly 2 arguments" level))
  (let* ((prefix (ecase level (:eq "EQ") (:eql "EQL") (:equal "EQUAL") (:equalp "EQP")))
         (local-x (env-add-local env (gensym (format nil "~A-X" prefix))))
         (local-y (env-add-local env (gensym (format nil "~A-Y" prefix))))
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (string-type clysm/compiler/codegen/gc-types:+type-string+)
         (float-type clysm/compiler/codegen/gc-types:+type-float+)
         (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+)
         ;; Use worklist for equal/equalp (structural comparison)
         (use-worklist (member level '(:equal :equalp))))
    (if use-worklist
        ;; Worklist-based comparison for equal/equalp
        (compile-equality-with-worklist args env level local-x local-y
                                        cons-type string-type float-type ratio-type prefix)
        ;; Simple comparison for eq/eql
        (compile-equality-simple args env level local-x local-y
                                 float-type ratio-type))))

(defun compile-equality-simple (args env level local-x local-y float-type ratio-type)
  "Compile simple equality (eq/eql) without worklist.
   Uses nested if/else for type dispatch."
  (with-instruction-collector
    ;; Compile and store both arguments
    (emit* (compile-to-instructions (first args) env))
    (emit `(:local.set ,local-x))
    (emit* (compile-to-instructions (second args) env))
    (emit `(:local.set ,local-y))
    ;; Check if x is null
    (emit `(:local.get ,local-x))
    (emit :ref.is_null)
    (emit '(:if (:result :anyref)))
    ;; x is null
    (emit* (emit-null-comparison level local-x local-y))
    (emit :else)
    ;; x is not null, check if y is null
    (emit `(:local.get ,local-y))
    (emit :ref.is_null)
    (emit* `((:if (:result :anyref))
             (:ref.null :none)  ; x not null, y null => NIL
             :else))
    ;; Neither is null - type dispatch
    ;; Check if x is i31ref
    (emit `(:local.get ,local-x))
    (emit '(:ref.test :i31))
    (emit '(:if (:result :anyref)))
    ;; x is i31 - check if y is also i31
    (emit `(:local.get ,local-y))
    (emit '(:ref.test :i31))
    (emit '(:if (:result :anyref)))
    ;; Both i31 - compare
    (emit* (emit-i31-comparison level local-x local-y))
    (emit :else)
    ;; x is i31, y is not - NIL for eq/eql
    (emit '(:ref.null :none))
    (emit :end)  ; if y is i31
    (emit :else)
    ;; x is not i31 - check for float (eql only)
    (when (eq level :eql)
      (emit `(:local.get ,local-x))
      (emit `(:ref.test ,(list :ref float-type)))
      (emit '(:if (:result :anyref)))
      (emit `(:local.get ,local-y))
      (emit `(:ref.test ,(list :ref float-type)))
      (emit '(:if (:result :anyref)))
      (emit* (emit-float-comparison level local-x local-y float-type))
      (emit :else)
      (emit '(:ref.null :none))  ; x float, y not
      (emit :end)
      (emit :else)
      ;; x is not float - check for ratio
      (emit `(:local.get ,local-x))
      (emit `(:ref.test ,(list :ref ratio-type)))
      (emit '(:if (:result :anyref)))
      (emit `(:local.get ,local-y))
      (emit `(:ref.test ,(list :ref ratio-type)))
      (emit '(:if (:result :anyref)))
      (emit* (emit-ratio-comparison level local-x local-y ratio-type))
      (emit :else)
      (emit '(:ref.null :none))  ; x ratio, y not
      (emit :end)
      (emit :else))
    ;; Default: ref.eq for other types
    (emit* (emit-default-comparison level local-x local-y))
    ;; Close all if blocks
    (when (eq level :eql)
      (emit* '(:end :end)))  ; ratio, float
    (emit* '(:end :end :end :end))))  ; i31, y-null, x-null

(defun compile-equality-with-worklist (args env level local-x local-y
                                       cons-type string-type float-type ratio-type prefix)
  "Compile structural equality (equal/equalp) with worklist for cons recursion.
   Uses block/loop structure with worklist-based iteration."
  (let* ((worklist-local (env-add-local env (gensym (format nil "~A-WL" prefix))))
         (pair-local (env-add-local env (gensym (format nil "~A-PAIR" prefix))))
         (result-local (env-add-local env (gensym (format nil "~A-RESULT" prefix))))
         (len1-local (env-add-local env (gensym (format nil "~A-LEN1" prefix)) :i32))
         (len2-local (env-add-local env (gensym (format nil "~A-LEN2" prefix)) :i32))
         (idx-local (env-add-local env (gensym (format nil "~A-IDX" prefix)) :i32))
         (loop-label (if (eq level :equal) '$equal_loop '$equalp_loop))
         (done-label (if (eq level :equal) '$equal_done '$equalp_done)))
    ;; Additional locals for equalp string comparison
    (let ((byte1-local (when (eq level :equalp)
                         (env-add-local env (gensym (format nil "~A-B1" prefix)) :i32)))
          (byte2-local (when (eq level :equalp)
                         (env-add-local env (gensym (format nil "~A-B2" prefix)) :i32))))
      `(;; Compile both arguments
        ,@(compile-to-instructions (first args) env)
        (:local.set ,local-x)
        ,@(compile-to-instructions (second args) env)
        (:local.set ,local-y)
        ;; Initialize worklist with single pair (cons x y)
        (:local.get ,local-x)
        (:local.get ,local-y)
        (:struct.new ,cons-type)
        (:ref.null :none)
        (:struct.new ,cons-type)  ; worklist = (cons (cons x y) nil)
        (:local.set ,worklist-local)
        ;; Initialize result to T
        (:i32.const 1) :ref.i31
        (:local.set ,result-local)
        ;; Main comparison loop
        (:block ,done-label)
        (:loop ,loop-label)
        ;; Check if worklist is empty
        (:local.get ,worklist-local)
        :ref.is_null
        (:br_if ,done-label)
        ;; Pop pair from worklist
        (:local.get ,worklist-local)
        (:ref.cast ,(list :ref cons-type))
        (:struct.get ,cons-type 0)
        (:local.set ,pair-local)
        (:local.get ,worklist-local)
        (:ref.cast ,(list :ref cons-type))
        (:struct.get ,cons-type 1)
        (:local.set ,worklist-local)
        ;; Extract x and y from pair
        (:local.get ,pair-local)
        (:ref.cast ,(list :ref cons-type))
        (:struct.get ,cons-type 0)
        (:local.set ,local-x)
        (:local.get ,pair-local)
        (:ref.cast ,(list :ref cons-type))
        (:struct.get ,cons-type 1)
        (:local.set ,local-y)
        ;; Comparison logic (inline to avoid additional function call overhead)
        ,@(compile-worklist-iteration-body level local-x local-y worklist-local result-local
                                           len1-local len2-local idx-local byte1-local byte2-local
                                           cons-type string-type float-type ratio-type
                                           loop-label done-label)
        :end  ; loop
        :end  ; block
        ;; Return result
        (:local.get ,result-local)))))

(defun compile-worklist-iteration-body (level local-x local-y worklist-local result-local
                                         len1-local len2-local idx-local byte1-local byte2-local
                                         cons-type string-type float-type ratio-type
                                         loop-label done-label)
  "Generate the body of the worklist iteration for equal/equalp.
   Returns instruction list for one iteration of the comparison loop."
  `(;; Case 1: Both null?
    (:local.get ,local-x)
    :ref.is_null
    (:if nil)
    (:local.get ,local-y)
    :ref.is_null
    (:if nil)
    (:br ,loop-label)  ; both null, continue
    :else
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    :else
    (:local.get ,local-y)
    :ref.is_null
    (:if nil)
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    ;; Neither is null - type dispatch
    ;; Case 2: Both i31ref?
    (:local.get ,local-x)
    (:ref.test :i31)
    (:if nil)
    (:local.get ,local-y)
    (:ref.test :i31)
    (:if nil)
    ,@(if (eq level :equalp)
          ;; equalp: case-insensitive i31 comparison
          `((:local.get ,local-x)
            (:ref.cast :i31)
            :i31.get_s
            (:local.set ,len1-local)  ; reuse len1 as temp
            (:local.get ,local-y)
            (:ref.cast :i31)
            :i31.get_s
            (:local.set ,len2-local)  ; reuse len2 as temp
            ;; Case fold len1
            (:local.get ,len1-local)
            (:i32.const 65)
            :i32.ge_s
            (:local.get ,len1-local)
            (:i32.const 90)
            :i32.le_s
            :i32.and
            (:if nil)
            (:local.get ,len1-local)
            (:i32.const 32)
            :i32.add
            (:local.set ,len1-local)
            :end
            ;; Case fold len2
            (:local.get ,len2-local)
            (:i32.const 65)
            :i32.ge_s
            (:local.get ,len2-local)
            (:i32.const 90)
            :i32.le_s
            :i32.and
            (:if nil)
            (:local.get ,len2-local)
            (:i32.const 32)
            :i32.add
            (:local.set ,len2-local)
            :end
            (:local.get ,len1-local)
            (:local.get ,len2-local)
            :i32.eq
            (:if nil)
            (:br ,loop-label)
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end)
          ;; equal: direct ref.eq
          `((:local.get ,local-x)
            (:ref.cast :i31)
            (:local.get ,local-y)
            (:ref.cast :i31)
            :ref.eq
            (:if nil)
            (:br ,loop-label)
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end))
    :else
    ;; x is i31, y is not
    ,@(if (eq level :equalp)
          ;; equalp: check for cross-type numeric comparison
          `((:local.get ,local-y)
            (:ref.test ,(list :ref float-type))
            (:if nil)
            ;; y is float, x is i31 - cross-type comparison
            (:local.get ,local-x)
            (:ref.cast :i31)
            :i31.get_s
            :f64.convert_i32_s
            (:local.get ,local-y)
            (:ref.cast ,(list :ref float-type))
            (:struct.get ,float-type 0)
            :f64.eq
            (:if nil)
            (:br ,loop-label)
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end)
          ;; equal: different types => NIL
          `((:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)))
    :end  ; if y is i31
    :else
    ;; x is not i31
    ;; Case 3: Both strings?
    (:local.get ,local-x)
    (:ref.test ,(list :ref string-type))
    (:if nil)
    (:local.get ,local-y)
    (:ref.test ,(list :ref string-type))
    (:if nil)
    ;; String comparison
    ,@(if (eq level :equalp)
          (compile-equalp-string-body local-x local-y string-type
                                      len1-local len2-local idx-local byte1-local byte2-local
                                      loop-label done-label result-local)
          (compile-equal-string-body local-x local-y string-type
                                     len1-local len2-local idx-local
                                     loop-label done-label result-local))
    :else
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    :else
    ;; x is not string
    ;; Case 4: Both floats?
    (:local.get ,local-x)
    (:ref.test ,(list :ref float-type))
    (:if nil)
    (:local.get ,local-y)
    (:ref.test ,(list :ref float-type))
    (:if nil)
    ;; Both floats - f64.eq
    (:local.get ,local-x)
    (:ref.cast ,(list :ref float-type))
    (:struct.get ,float-type 0)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref float-type))
    (:struct.get ,float-type 0)
    :f64.eq
    (:if nil)
    (:br ,loop-label)
    :else
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    :else
    ;; x is float, y is not
    ,@(if (eq level :equalp)
          ;; Check if y is i31 for cross-type
          `((:local.get ,local-y)
            (:ref.test :i31)
            (:if nil)
            (:local.get ,local-x)
            (:ref.cast ,(list :ref float-type))
            (:struct.get ,float-type 0)
            (:local.get ,local-y)
            (:ref.cast :i31)
            :i31.get_s
            :f64.convert_i32_s
            :f64.eq
            (:if nil)
            (:br ,loop-label)
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end
            :else
            (:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)
            :end)
          `((:ref.null :none)
            (:local.set ,result-local)
            (:br ,done-label)))
    :end  ; if y is float
    :else
    ;; x is not float
    ;; Case 5: Both cons?
    (:local.get ,local-x)
    (:ref.test ,(list :ref cons-type))
    (:if nil)
    (:local.get ,local-y)
    (:ref.test ,(list :ref cons-type))
    (:if nil)
    ;; Both cons - push to worklist
    ,@(emit-cons-worklist-push local-x local-y cons-type worklist-local)
    (:br ,loop-label)
    :else
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    :else
    ;; Default: ref.eq
    (:local.get ,local-x)
    (:ref.cast :eq)
    (:local.get ,local-y)
    (:ref.cast :eq)
    :ref.eq
    (:if nil)
    (:br ,loop-label)
    :else
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    :end  ; if cons
    :end  ; if float
    :end  ; if string
    :end  ; if i31
    :end  ; if x not null
    ))

(defun compile-equal-string-body (local-x local-y string-type len1 len2 idx
                                  loop-label done-label result-local)
  "Generate case-sensitive string comparison for equal."
  `(;; Get lengths
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len2)
    ;; Check length
    (:local.get ,len1)
    (:local.get ,len2)
    :i32.ne
    (:if nil)
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    ;; Compare bytes
    (:i32.const 0)
    (:local.set ,idx)
    (:block $str_cmp_done)
    (:loop $str_cmp_loop)
    (:local.get ,idx)
    (:local.get ,len1)
    :i32.ge_u
    (:br_if $str_cmp_done)
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    :i32.ne
    (:if nil)
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    (:local.get ,idx)
    (:i32.const 1)
    :i32.add
    (:local.set ,idx)
    (:br $str_cmp_loop)
    :end  ; loop
    :end  ; block
    ;; Strings equal, continue
    (:br ,loop-label)))

(defun compile-equalp-string-body (local-x local-y string-type len1 len2 idx byte1 byte2
                                   loop-label done-label result-local)
  "Generate case-insensitive string comparison for equalp."
  `(;; Get lengths
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:array.len)
    (:local.set ,len2)
    ;; Check length
    (:local.get ,len1)
    (:local.get ,len2)
    :i32.ne
    (:if nil)
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    ;; Compare bytes with case folding
    (:i32.const 0)
    (:local.set ,idx)
    (:block $str_cmp_done)
    (:loop $str_cmp_loop)
    (:local.get ,idx)
    (:local.get ,len1)
    :i32.ge_u
    (:br_if $str_cmp_done)
    ;; Get bytes
    (:local.get ,local-x)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.set ,byte1)
    (:local.get ,local-y)
    (:ref.cast ,(list :ref string-type))
    (:local.get ,idx)
    (:array.get_u ,string-type)
    (:local.set ,byte2)
    ;; Fold byte1 to uppercase
    (:local.get ,byte1)
    (:i32.const 97)
    :i32.ge_u
    (:local.get ,byte1)
    (:i32.const 122)
    :i32.le_u
    :i32.and
    (:if nil)
    (:local.get ,byte1)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte1)
    :end
    ;; Fold byte2 to uppercase
    (:local.get ,byte2)
    (:i32.const 97)
    :i32.ge_u
    (:local.get ,byte2)
    (:i32.const 122)
    :i32.le_u
    :i32.and
    (:if nil)
    (:local.get ,byte2)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte2)
    :end
    ;; Compare
    (:local.get ,byte1)
    (:local.get ,byte2)
    :i32.ne
    (:if nil)
    (:ref.null :none)
    (:local.set ,result-local)
    (:br ,done-label)
    :end
    (:local.get ,idx)
    (:i32.const 1)
    :i32.add
    (:local.set ,idx)
    (:br $str_cmp_loop)
    :end  ; loop
    :end  ; block
    ;; Strings equal, continue
    (:br ,loop-label)))

;;; ============================================================
;;; Equality Predicate Wrapper Functions
;;; (Delegates to compile-equality-predicate)
;;; ============================================================

(defun compile-eq (args env)
  "Compile (eq x y) - returns T if x and y are the identical object.
   Uses Wasm ref.eq instruction for pointer equality.
   Stack: [] -> [T or NIL]"
  (compile-equality-predicate args env :eq))

(defun compile-eql (args env)
  "Compile (eql x y) - returns T if x and y are identical objects,
   or if they are numbers of the same type with the same value,
   or if they are characters with the same char-code.
   Stack: [] -> [T or NIL]"
  (compile-equality-predicate args env :eql))

;;; Legacy compile-eql implementation removed - consolidated into
;;; compile-equality-predicate infrastructure (001-equality-type-dispatch)
;;;
;;; Original: 143 lines of duplicated type dispatch logic.
;;; Now delegates to shared helper functions.

(defun compile-equal (args env)
  "Compile (equal x y) - returns T if x and y are structurally similar.
   Uses a worklist-based approach to handle recursive cons comparison.
   ANSI CL: equal descends into cons cells, compares strings with string=,
   uses eql for numbers/characters/symbols.
   Stack: [] -> [T or NIL]"
  (compile-equality-predicate args env :equal))

;;; Legacy compile-equal implementation removed - consolidated into
;;; compile-equality-predicate infrastructure (001-equality-type-dispatch)
;;;
;;; Original: 273 lines of worklist-based cons iteration with string comparison.
;;; Now delegates to shared compile-equality-with-worklist.

(defun compile-equalp (args env)
  "Compile (equalp x y) - case-insensitive structural equality.
   Like equal but:
   - Case-insensitive for strings (uses char-equal semantics)
   - Case-insensitive for characters
   - Uses = for numbers (type-coercing: 3 equalp 3.0 is T)
   Uses worklist-based approach for recursive cons comparison.
   Stack: [] -> [T or NIL]"
  (compile-equality-predicate args env :equalp))

;;; Legacy compile-equalp implementation removed - consolidated into
;;; compile-equality-predicate infrastructure (001-equality-type-dispatch)
;;;
;;; Original: 374 lines of worklist-based iteration with case-insensitive
;;; string comparison and numeric type coercion.
;;; Now delegates to shared compile-equality-with-worklist.

(defun compile-atom (args env)
  "Compile (atom x) - returns T if x is not a cons cell, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "atom requires exactly 1 argument"))
  (with-instruction-collector
    ;; Compile the argument
    (emit* (compile-to-instructions (first args) env))
    ;; Test if it's a cons (type 2)
    (emit :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-cons+))
    ;; atom is NOT consp, so invert the result
    (emit '(:if (:result :anyref)))
    (emit :ref.null :none)        ; cons -> not atom -> NIL
    (emit :else)
    (emit :i32.const 1)
    (emit :ref.i31)               ; not cons -> atom -> T
    (emit :end)))

(defun compile-listp (args env)
  "Compile (listp x) - returns T if x is a cons cell or NIL.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "listp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "LISTP-TMP"))))
    (with-instruction-collector
      ;; Compile the argument
      (emit* (compile-to-instructions (first args) env))
      ;; Store for reuse
      (emit :local.tee temp-local)
      ;; First check: is it a cons?
      (emit :ref.test (list :ref clysm/compiler/codegen/gc-types:+type-cons+))
      (emit '(:if (:result :anyref)))
      (emit :i32.const 1)
      (emit :ref.i31)             ; cons -> T
      (emit :else)
      ;; Not a cons, check if NIL
      (emit :local.get temp-local)
      (emit :ref.is_null)
      (emit '(:if (:result :anyref)))
      (emit :i32.const 1)
      (emit :ref.i31)             ; nil -> T
      (emit :else)
      (emit :ref.null :none)      ; neither -> NIL
      (emit :end)
      (emit :end))))

;;; ============================================================
;;; ANSI CL Type Predicates (023-type-predicates)
;;; ============================================================

(defun compile-integerp (args env)
  "Compile (integerp x) - returns T if x is an integer (fixnum or bignum), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "integerp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "INTEGERP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Is fixnum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       ;; Is bignum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Neither -> NIL
       (:ref.null :none)
       :end
       :end))))

(defun compile-floatp (args env)
  "Compile (floatp x) - returns T if x is a float, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "floatp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-rationalp (args env)
  "Compile (rationalp x) - returns T if x is a rational (fixnum, bignum, or ratio), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "rationalp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "RATIONALP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Is fixnum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       ;; Is bignum -> T
       (:i32.const 1) :ref.i31
       :else
       ;; Test if ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       ;; Is ratio -> T
       (:i32.const 1) :ref.i31
       :else
       ;; None of the above -> NIL
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-complexp (args env)
  "Compile (complexp x) - returns T if x is a complex number, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "complexp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-numberp (args env)
  "Compile (numberp x) - returns T if x is any numeric type, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "numberp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "NUMBERP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum (i31ref)
     '((:ref.test :i31))
     `((:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if bignum
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-bignum+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-ratio+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Test if complex
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-complex+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end :end :end :end :end))))

(defun compile-symbolp (args env)
  "Compile (symbolp x) - returns T if x is a symbol (including NIL), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "symbolp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SYMBOLP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; First check: is it a symbol struct?
     `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-symbol+))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       ;; Not a symbol struct, check if NIL (ref.null)
       (:local.get ,temp-local)
       :ref.is_null
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31  ; NIL is a symbol
       :else
       (:ref.null :none)
       :end
       :end))))

(defun compile-functionp (args env)
  "Compile (functionp x) - returns T if x is a function (closure), NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "functionp requires exactly 1 argument"))
  (append
   (compile-to-instructions (first args) env)
   `((:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-closure+))
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

;;; ============================================================
;;; ANSI CL Numeric Predicates (023-type-predicates)
;;; ============================================================

(defun compile-zerop (args env)
  "Compile (zerop x) - returns T if x equals zero, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "zerop requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "ZEROP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare to 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       :i32.eqz
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare to 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-plusp (args env)
  "Compile (plusp x) - returns T if x is positive, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "plusp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "PLUSP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare > 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.gt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare > 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.gt
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-minusp (args env)
  "Compile (minusp x) - returns T if x is negative, NIL otherwise.
   Supports fixnum and float.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "minusp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "MINUSP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract and compare < 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       ;; Float: extract and compare < 0.0
       (:local.get ,temp-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       (:f64.const 0.0)
       :f64.lt
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Other types: NIL
       (:ref.null :none)
       :end :end))))

(defun compile-oddp (args env)
  "Compile (oddp x) - returns T if x is an odd integer, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "oddp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "ODDP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract, mod 2, check != 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 1)
       :i32.and  ; n & 1 == 1 for odd numbers
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Not a fixnum: NIL (bignum support would require runtime)
       (:ref.null :none)
       :end))))

(defun compile-evenp (args env)
  "Compile (evenp x) - returns T if x is an even integer, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "evenp requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "EVENP-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum: extract, mod 2, check == 0
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 1)
       :i32.and  ; n & 1 == 0 for even numbers
       :i32.eqz
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :else
       ;; Not a fixnum: NIL (bignum support would require runtime)
       (:ref.null :none)
       :end))))

;;; ============================================================
;;; ANSI CL Signum Function (023-type-predicates)
;;; ============================================================

(defun compile-signum (args env)
  "Compile (signum x) - returns sign of x with type preserved.
   For integers: -1, 0, or 1
   For floats: -1.0, 0.0, or 1.0
   For complex: z/|z| (normalized to magnitude 1)
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "signum requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SIGNUM-TMP")))
        (part-local (env-add-local env (gensym "SIGNUM-PART")))
        (real-local (env-add-local env (gensym "SIGNUM-REAL") :f64))
        (imag-local (env-add-local env (gensym "SIGNUM-IMAG") :f64))
        (mag-local (env-add-local env (gensym "SIGNUM-MAG") :f64))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+)
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum path - extract value and compare directly
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       ;; Negative
       (:i32.const -1) :ref.i31
       :else
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.gt_s
       (:if (:result :anyref))
       ;; Positive
       (:i32.const 1) :ref.i31
       :else
       ;; Zero
       (:i32.const 0) :ref.i31
       :end
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :anyref))
       ;; Float path
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       (:f64.const 0.0)
       :f64.lt
       (:if (:result :anyref))
       ;; Negative float
       (:f64.const -1.0)
       (:struct.new ,float-type)
       :else
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       (:f64.const 0.0)
       :f64.gt
       (:if (:result :anyref))
       ;; Positive float
       (:f64.const 1.0)
       (:struct.new ,float-type)
       :else
       ;; Zero float
       (:f64.const 0.0)
       (:struct.new ,float-type)
       :end
       :end
       :else
       ;; Test if complex
       (:local.get ,temp-local)
       (:ref.test (:ref ,complex-type))
       (:if (:result :anyref))
       ;; Complex path - return z/|z| (normalized complex)
       ;; Get real part as f64
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 0)
       (:local.tee ,part-local)
       (:ref.test :i31)
       (:if (:result :f64))
       (:local.get ,part-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       :else
       (:local.get ,part-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :end
       (:local.set ,real-local)
       ;; Get imag part as f64
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 1)
       (:local.tee ,part-local)
       (:ref.test :i31)
       (:if (:result :f64))
       (:local.get ,part-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       :else
       (:local.get ,part-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :end
       (:local.set ,imag-local)
       ;; Compute magnitude = sqrt(real^2 + imag^2)
       (:local.get ,real-local)
       (:local.get ,real-local)
       :f64.mul
       (:local.get ,imag-local)
       (:local.get ,imag-local)
       :f64.mul
       :f64.add
       :f64.sqrt
       (:local.tee ,mag-local)
       ;; Check if magnitude is zero
       (:f64.const 0.0)
       :f64.eq
       (:if (:result :anyref))
       ;; Zero complex: return 0
       (:i32.const 0) :ref.i31
       :else
       ;; Non-zero: return z/|z| = (real/mag, imag/mag)
       (:local.get ,real-local)
       (:local.get ,mag-local)
       :f64.div
       (:struct.new ,float-type)
       (:local.get ,imag-local)
       (:local.get ,mag-local)
       :f64.div
       (:struct.new ,float-type)
       (:struct.new ,complex-type)
       :end
       :else
       ;; Other types: return 0
       (:i32.const 0) :ref.i31
       :end :end :end))))

;;; ============================================================
;;; ANSI Numeric Functions (001-numeric-functions)
;;; ============================================================

(defun compile-abs (args env)
  "Compile (abs x) - returns the absolute value of x.
   For integers: uses comparison and negation
   For floats: uses f64.abs
   For complex: returns magnitude sqrt(real^2 + imag^2)
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "abs requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "ABS-TMP")))
        (part-local (env-add-local env (gensym "ABS-PART")))
        (real-local (env-add-local env (gensym "ABS-REAL") :f64))
        (imag-local (env-add-local env (gensym "ABS-IMAG") :f64))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+)
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Test if fixnum
     '((:ref.test :i31))
     `((:if (:result :anyref))
       ;; Fixnum path
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       ;; Negative: negate
       (:i32.const 0)
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       :i32.sub
       :ref.i31
       :else
       ;; Non-negative: return as-is
       (:local.get ,temp-local)
       :end
       :else
       ;; Test if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :anyref))
       ;; Float path - use f64.abs
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :f64.abs
       (:struct.new ,float-type)
       :else
       ;; Test if complex
       (:local.get ,temp-local)
       (:ref.test (:ref ,complex-type))
       (:if (:result :anyref))
       ;; Complex path - compute magnitude sqrt(real^2 + imag^2)
       ;; Get real part as f64
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 0)
       (:local.tee ,part-local)
       (:ref.test :i31)
       (:if (:result :f64))
       (:local.get ,part-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       :else
       (:local.get ,part-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :end
       (:local.set ,real-local)
       ;; Get imag part as f64
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 1)
       (:local.tee ,part-local)
       (:ref.test :i31)
       (:if (:result :f64))
       (:local.get ,part-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       :else
       (:local.get ,part-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :end
       (:local.set ,imag-local)
       ;; Compute sqrt(real^2 + imag^2)
       (:local.get ,real-local)
       (:local.get ,real-local)
       :f64.mul
       (:local.get ,imag-local)
       (:local.get ,imag-local)
       :f64.mul
       :f64.add
       :f64.sqrt
       (:struct.new ,float-type)
       :else
       ;; Other types: return as-is (ratio, bignum)
       (:local.get ,temp-local)
       :end :end :end))))

(defun compile-max (args env)
  "Compile (max x y ...) - returns the maximum of arguments.
   Supports any number of arguments >= 1.
   Stack: [] -> [number]"
  (when (< (length args) 1)
    (error "max requires at least 1 argument"))
  (if (= (length args) 1)
      ;; Single arg: just return it
      (compile-to-instructions (first args) env)
      ;; Multiple args: pairwise comparison
      (let ((result-local (env-add-local env (gensym "MAX-RESULT")))
            (temp-local (env-add-local env (gensym "MAX-TMP"))))
        (append
         ;; Start with first arg
         (compile-to-instructions (first args) env)
         (list (list :local.set result-local))
         ;; Compare with remaining args
         (loop for arg in (rest args)
               append
               (append
                (compile-to-instructions arg env)
                (list (list :local.set temp-local))
                ;; Compare and update result
                ;; For fixnums: use i32 comparison
                `((:local.get ,temp-local)
                  (:ref.test :i31)
                  (:if (:result :anyref))
                  ;; Both operands assumed fixnum for now (TODO: type dispatch)
                  (:local.get ,temp-local)
                  (:ref.cast :i31)
                  :i31.get_s
                  (:local.get ,result-local)
                  (:ref.cast :i31)
                  :i31.get_s
                  :i32.gt_s
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  :else
                  (:local.get ,result-local)
                  :end
                  (:local.set ,result-local)
                  (:local.get ,result-local)
                  :else
                  ;; Float comparison
                  (:local.get ,temp-local)
                  (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
                  (:local.get ,result-local)
                  (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
                  :f64.gt
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  :else
                  (:local.get ,result-local)
                  :end
                  (:local.set ,result-local)
                  (:local.get ,result-local)
                  :else
                  (:local.get ,result-local)
                  :end :end)))
         ;; Return final result
         `(:drop
           (:local.get ,result-local))))))

(defun compile-min (args env)
  "Compile (min x y ...) - returns the minimum of arguments.
   Supports any number of arguments >= 1.
   Stack: [] -> [number]"
  (when (< (length args) 1)
    (error "min requires at least 1 argument"))
  (if (= (length args) 1)
      ;; Single arg: just return it
      (compile-to-instructions (first args) env)
      ;; Multiple args: pairwise comparison
      (let ((result-local (env-add-local env (gensym "MIN-RESULT")))
            (temp-local (env-add-local env (gensym "MIN-TMP"))))
        (append
         ;; Start with first arg
         (compile-to-instructions (first args) env)
         (list (list :local.set result-local))
         ;; Compare with remaining args
         (loop for arg in (rest args)
               append
               (append
                (compile-to-instructions arg env)
                (list (list :local.set temp-local))
                ;; Compare and update result (using lt_s instead of gt_s)
                `((:local.get ,temp-local)
                  (:ref.test :i31)
                  (:if (:result :anyref))
                  ;; Fixnum comparison
                  (:local.get ,temp-local)
                  (:ref.cast :i31)
                  :i31.get_s
                  (:local.get ,result-local)
                  (:ref.cast :i31)
                  :i31.get_s
                  :i32.lt_s
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  :else
                  (:local.get ,result-local)
                  :end
                  (:local.set ,result-local)
                  (:local.get ,result-local)
                  :else
                  ;; Float comparison
                  (:local.get ,temp-local)
                  (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
                  (:local.get ,result-local)
                  (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
                  (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
                  :f64.lt
                  (:if (:result :anyref))
                  (:local.get ,temp-local)
                  :else
                  (:local.get ,result-local)
                  :end
                  (:local.set ,result-local)
                  (:local.get ,result-local)
                  :else
                  (:local.get ,result-local)
                  :end :end)))
         ;; Return final result
         `(:drop
           (:local.get ,result-local))))))

(defun compile-gcd (args env)
  "Compile (gcd &rest integers) - returns greatest common divisor.
   Uses Euclidean algorithm for two args.
   (gcd) => 0, (gcd n) => |n|, (gcd a b) => gcd(a,b)
   Stack: [] -> [integer]"
  (case (length args)
    (0 ;; (gcd) => 0
     '((:i32.const 0) :ref.i31))
    (1 ;; (gcd n) => |n|
     (compile-abs args env))
    (2 ;; Two-arg case: Euclidean algorithm
     (let ((a-local (env-add-local env (gensym "GCD-A")))
           (b-local (env-add-local env (gensym "GCD-B")))
           (temp-local (env-add-local env (gensym "GCD-TMP"))))
       (append
        (compile-to-instructions (first args) env)
        '((:ref.cast :i31) :i31.get_s)
        ;; Take absolute value of first arg
        `((:local.tee ,a-local)
          (:i32.const 0)
          :i32.lt_s
          (:if (:result :i32))
          (:i32.const 0)
          (:local.get ,a-local)
          :i32.sub
          :else
          (:local.get ,a-local)
          :end
          (:local.set ,a-local))
        (compile-to-instructions (second args) env)
        '((:ref.cast :i31) :i31.get_s)
        ;; Take absolute value of second arg
        `((:local.tee ,b-local)
          (:i32.const 0)
          :i32.lt_s
          (:if (:result :i32))
          (:i32.const 0)
          (:local.get ,b-local)
          :i32.sub
          :else
          (:local.get ,b-local)
          :end
          (:local.set ,b-local)
          ;; Euclidean loop: while b != 0: a, b = b, a % b
          (:block
           (:loop
            (:local.get ,b-local)
            (:i32.const 0)
            :i32.eq
            :br_if 1  ;; exit if b == 0
            (:local.get ,a-local)
            (:local.get ,b-local)
            :i32.rem_s
            (:local.set ,temp-local)
            (:local.get ,b-local)
            (:local.set ,a-local)
            (:local.get ,temp-local)
            (:local.set ,b-local)
            :br 0  ;; continue loop
            :end) :end)
          (:local.get ,a-local)
          :ref.i31))))
    (otherwise
     ;; More than 2 args: reduce
     (compile-gcd (list (first args)
                        `(gcd ,@(rest args)))
                  env))))

(defun compile-lcm (args env)
  "Compile (lcm &rest integers) - returns least common multiple.
   Uses |a * b| / gcd(a, b) formula.
   (lcm) => 1, (lcm n) => |n|, (lcm a b) => |a*b|/gcd(a,b)
   Stack: [] -> [integer]"
  (case (length args)
    (0 ;; (lcm) => 1
     '((:i32.const 1) :ref.i31))
    (1 ;; (lcm n) => |n|
     (compile-abs args env))
    (2 ;; Two-arg case
     (let ((a-local (env-add-local env (gensym "LCM-A")))
           (b-local (env-add-local env (gensym "LCM-B")))
           (gcd-local (env-add-local env (gensym "LCM-GCD"))))
       (append
        (compile-to-instructions (first args) env)
        '((:ref.cast :i31) :i31.get_s)
        `((:local.set ,a-local))
        (compile-to-instructions (second args) env)
        '((:ref.cast :i31) :i31.get_s)
        `((:local.set ,b-local)
          ;; Check for zero: lcm(n, 0) = 0
          (:local.get ,a-local)
          (:i32.const 0)
          :i32.eq
          (:local.get ,b-local)
          (:i32.const 0)
          :i32.eq
          :i32.or
          (:if (:result :anyref))
          (:i32.const 0) :ref.i31
          :else)
        ;; Compute gcd
        (compile-gcd (list (first args) (second args)) env)
        '((:ref.cast :i31) :i31.get_s)
        `((:local.set ,gcd-local)
          ;; |a * b| / gcd - using (a / gcd) * b to avoid overflow
          (:local.get ,a-local)
          (:local.get ,gcd-local)
          :i32.div_s
          (:local.get ,b-local)
          :i32.mul
          ;; Take absolute value
          (:local.tee ,a-local)
          (:i32.const 0)
          :i32.lt_s
          (:if (:result :i32))
          (:i32.const 0)
          (:local.get ,a-local)
          :i32.sub
          :else
          (:local.get ,a-local)
          :end
          :ref.i31
          :end))))
    (otherwise
     ;; More than 2 args: reduce
     (compile-lcm (list (first args)
                        `(lcm ,@(rest args)))
                  env))))

(defun compile-logcount (args env)
  "Compile (logcount integer) - count set bits.
   For positive integers: count 1-bits (uses i32.popcnt)
   For negative integers: count 0-bits (ANSI CL requirement)
   Stack: [] -> [integer]"
  (when (/= (length args) 1)
    (error "logcount requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "LOGCOUNT-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     `((:local.tee ,temp-local)
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :i32))
       ;; Negative: count 0-bits = popcnt(~n)
       (:local.get ,temp-local)
       (:i32.const -1)
       :i32.xor  ;; bitwise NOT
       :i32.popcnt
       :else
       ;; Non-negative: count 1-bits
       (:local.get ,temp-local)
       :i32.popcnt
       :end
       :ref.i31))))

(defun compile-integer-length (args env)
  "Compile (integer-length integer) - minimum bits to represent.
   Uses: 32 - clz(n) for positive, 32 - clz(~n) for negative
   (integer-length 0) => 0
   (integer-length n) => floor(log2(|n|)) + 1 for n != 0
   Stack: [] -> [integer]"
  (when (/= (length args) 1)
    (error "integer-length requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "INTLEN-TMP"))))
    (append
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     `((:local.tee ,temp-local)
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :i32))
       ;; Negative: use ~n (bitwise complement)
       (:local.get ,temp-local)
       (:i32.const -1)
       :i32.xor
       :else
       (:local.get ,temp-local)
       :end
       ;; Compute 32 - clz(value)
       ;; Stack: value
       :i32.clz        ;; Stack: clz(value)
       (:i32.const 32) ;; Stack: clz(value), 32
       :i32.sub        ;; Stack: clz(value) - 32
       (:i32.const 0)  ;; Stack: (clz - 32), 0
       :i32.sub        ;; Stack: 0 - (clz - 32) = 32 - clz
       :ref.i31))))

;;; ============================================================
;;; Phase 14B: Bit Testing Functions (001-numeric-predicates)
;;; logbitp - test if bit at index is set
;;; logtest - test if two integers share any set bits
;;; See: resources/HyperSpec/Body/f_logbtp.htm, f_logtes.htm
;;; ============================================================

(defun compile-logbitp (args env)
  "Compile (logbitp index integer) - test if bit at index is set.
   Returns T if bit at index is 1, NIL otherwise.
   index must be non-negative integer.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "logbitp requires exactly 2 arguments"))
  (let ((idx-local (env-add-local env (gensym "LOGBITP-IDX") :i32))
        (int-local (env-add-local env (gensym "LOGBITP-INT") :i32)))
    (append
     ;; Compile and store index (as i32)
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set idx-local))
     ;; Compile and store integer (as i32)
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Compute: (integer >> index) & 1, then convert to boolean
     `((:local.get ,int-local)
       (:local.get ,idx-local)
       :i32.shr_s  ;; arithmetic shift right preserves sign for negative numbers
       (:i32.const 1)
       :i32.and
       ;; Test if bit is set (non-zero): eqz gives 1 if bit was 0, 0 if bit was 1
       :i32.eqz
       (:if (:result :anyref))
       (:ref.null :none)  ;; bit is 0 -> NIL
       :else
       (:i32.const 1) :ref.i31  ;; bit is 1 -> T
       :end))))

(defun compile-logtest (args env)
  "Compile (logtest integer1 integer2) - test if any bits are common.
   Returns T if (logand integer1 integer2) is non-zero, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "logtest requires exactly 2 arguments"))
  (append
   ;; Compile first integer (as i32)
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   ;; Compile second integer (as i32)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s)
   ;; Compute logand and test if non-zero
   '(:i32.and
     :i32.eqz
     (:if (:result :anyref))
     (:ref.null :none)  ;; no common bits -> NIL
     :else
     (:i32.const 1) :ref.i31  ;; has common bits -> T
     :end)))

;;; ============================================================
;;; Byte Specifier Functions (001-numeric-predicates)
;;; HyperSpec: http://www.lispworks.com/documentation/HyperSpec/Body/f_by_by.htm
;;; ============================================================

(defun compile-byte (args env)
  "Compile (byte size position) - create a byte specifier.
   Encodes byte specifier as fixnum: (size << 6) | position
   This encoding allows size and position values 0-63.
   Stack: [] -> [byte-specifier (i31ref)]"
  (when (/= (length args) 2)
    (error "byte requires exactly 2 arguments"))
  (let ((size-local (env-add-local env (gensym "BYTE-SIZE") :i32))
        (pos-local (env-add-local env (gensym "BYTE-POS") :i32)))
    (append
     ;; Compile size argument (as i32)
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set size-local))
     ;; Compile position argument (as i32)
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set pos-local))
     ;; Encode as (size << 6) | position
     `((:local.get ,size-local)
       (:i32.const 6)
       :i32.shl
       (:local.get ,pos-local)
       :i32.or
       :ref.i31))))

(defun compile-byte-size (args env)
  "Compile (byte-size bytespec) - extract size from byte specifier.
   Decodes size as: bytespec >> 6
   Stack: [] -> [size (i31ref)]"
  (when (/= (length args) 1)
    (error "byte-size requires exactly 1 argument"))
  (append
   ;; Compile byte specifier (as i32)
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   ;; Decode size: spec >> 6
   '((:i32.const 6)
     :i32.shr_u
     :ref.i31)))

(defun compile-byte-position (args env)
  "Compile (byte-position bytespec) - extract position from byte specifier.
   Decodes position as: bytespec & 63
   Stack: [] -> [position (i31ref)]"
  (when (/= (length args) 1)
    (error "byte-position requires exactly 1 argument"))
  (append
   ;; Compile byte specifier (as i32)
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   ;; Decode position: spec & 63
   '((:i32.const 63)
     :i32.and
     :ref.i31)))

;;; ============================================================
;;; Byte Operations (001-numeric-predicates)
;;; HyperSpec: http://www.lispworks.com/documentation/HyperSpec/Body/f_ldb.htm
;;; ============================================================

(defun compile-ldb (args env)
  "Compile (ldb bytespec integer) - load byte.
   Extracts a byte field from integer and right-shifts it to position 0.
   Formula: (integer >> position) & ((1 << size) - 1)
   Stack: [] -> [extracted-value (i31ref)]"
  (when (/= (length args) 2)
    (error "ldb requires exactly 2 arguments"))
  (let ((spec-local (env-add-local env (gensym "LDB-SPEC") :i32))
        (int-local (env-add-local env (gensym "LDB-INT") :i32))
        (size-local (env-add-local env (gensym "LDB-SIZE") :i32))
        (pos-local (env-add-local env (gensym "LDB-POS") :i32)))
    (append
     ;; Compile byte specifier
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set spec-local))
     ;; Compile integer
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Extract size: spec >> 6
     `((:local.get ,spec-local)
       (:i32.const 6)
       :i32.shr_u
       (:local.set ,size-local))
     ;; Extract position: spec & 63
     `((:local.get ,spec-local)
       (:i32.const 63)
       :i32.and
       (:local.set ,pos-local))
     ;; Compute: (integer >> position) & ((1 << size) - 1)
     `((:local.get ,int-local)
       (:local.get ,pos-local)
       :i32.shr_u                    ;; integer >> position
       (:i32.const 1)
       (:local.get ,size-local)
       :i32.shl                      ;; 1 << size
       (:i32.const 1)
       :i32.sub                      ;; (1 << size) - 1 = mask
       :i32.and                      ;; result & mask
       :ref.i31))))

(defun compile-mask-field (args env)
  "Compile (mask-field bytespec integer) - extract byte field in place.
   Extracts a byte field keeping bits in their original position.
   Formula: integer & (((1 << size) - 1) << position)
   Stack: [] -> [masked-value (i31ref)]"
  (when (/= (length args) 2)
    (error "mask-field requires exactly 2 arguments"))
  (let ((spec-local (env-add-local env (gensym "MASK-SPEC") :i32))
        (int-local (env-add-local env (gensym "MASK-INT") :i32))
        (size-local (env-add-local env (gensym "MASK-SIZE") :i32))
        (pos-local (env-add-local env (gensym "MASK-POS") :i32)))
    (append
     ;; Compile byte specifier
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set spec-local))
     ;; Compile integer
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Extract size: spec >> 6
     `((:local.get ,spec-local)
       (:i32.const 6)
       :i32.shr_u
       (:local.set ,size-local))
     ;; Extract position: spec & 63
     `((:local.get ,spec-local)
       (:i32.const 63)
       :i32.and
       (:local.set ,pos-local))
     ;; Compute: integer & (((1 << size) - 1) << position)
     `((:local.get ,int-local)
       (:i32.const 1)
       (:local.get ,size-local)
       :i32.shl                      ;; 1 << size
       (:i32.const 1)
       :i32.sub                      ;; (1 << size) - 1
       (:local.get ,pos-local)
       :i32.shl                      ;; mask << position = field-mask
       :i32.and                      ;; integer & field-mask
       :ref.i31))))

(defun compile-dpb (args env)
  "Compile (dpb newbyte bytespec integer) - deposit byte.
   Replaces a byte field in integer with newbyte (shifted to position).
   Formula: (integer & ~field-mask) | ((newbyte << position) & field-mask)
   where field-mask = ((1 << size) - 1) << position
   Stack: [] -> [result (i31ref)]"
  (when (/= (length args) 3)
    (error "dpb requires exactly 3 arguments"))
  (let ((new-local (env-add-local env (gensym "DPB-NEW") :i32))
        (spec-local (env-add-local env (gensym "DPB-SPEC") :i32))
        (int-local (env-add-local env (gensym "DPB-INT") :i32))
        (size-local (env-add-local env (gensym "DPB-SIZE") :i32))
        (pos-local (env-add-local env (gensym "DPB-POS") :i32))
        (mask-local (env-add-local env (gensym "DPB-MASK") :i32)))
    (append
     ;; Compile newbyte
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set new-local))
     ;; Compile byte specifier
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set spec-local))
     ;; Compile integer
     (compile-to-instructions (third args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Extract size: spec >> 6
     `((:local.get ,spec-local)
       (:i32.const 6)
       :i32.shr_u
       (:local.set ,size-local))
     ;; Extract position: spec & 63
     `((:local.get ,spec-local)
       (:i32.const 63)
       :i32.and
       (:local.set ,pos-local))
     ;; Compute field-mask: ((1 << size) - 1) << position
     `((:i32.const 1)
       (:local.get ,size-local)
       :i32.shl                      ;; 1 << size
       (:i32.const 1)
       :i32.sub                      ;; (1 << size) - 1
       (:local.get ,pos-local)
       :i32.shl                      ;; field-mask
       (:local.set ,mask-local))
     ;; Compute: (integer & ~mask) | ((newbyte << position) & mask)
     `((:local.get ,int-local)
       (:local.get ,mask-local)
       (:i32.const -1)
       :i32.xor                      ;; ~mask
       :i32.and                      ;; integer & ~mask
       (:local.get ,new-local)
       (:local.get ,pos-local)
       :i32.shl                      ;; newbyte << position
       (:local.get ,mask-local)
       :i32.and                      ;; (newbyte << position) & mask
       :i32.or                       ;; final result
       :ref.i31))))

(defun compile-deposit-field (args env)
  "Compile (deposit-field newbyte bytespec integer) - deposit field.
   Replaces a byte field in integer with corresponding bits from newbyte.
   Unlike dpb, newbyte is already in the correct position.
   Formula: (integer & ~field-mask) | (newbyte & field-mask)
   where field-mask = ((1 << size) - 1) << position
   Stack: [] -> [result (i31ref)]"
  (when (/= (length args) 3)
    (error "deposit-field requires exactly 3 arguments"))
  (let ((new-local (env-add-local env (gensym "DEP-NEW") :i32))
        (spec-local (env-add-local env (gensym "DEP-SPEC") :i32))
        (int-local (env-add-local env (gensym "DEP-INT") :i32))
        (size-local (env-add-local env (gensym "DEP-SIZE") :i32))
        (pos-local (env-add-local env (gensym "DEP-POS") :i32))
        (mask-local (env-add-local env (gensym "DEP-MASK") :i32)))
    (append
     ;; Compile newbyte
     (compile-to-instructions (first args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set new-local))
     ;; Compile byte specifier
     (compile-to-instructions (second args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set spec-local))
     ;; Compile integer
     (compile-to-instructions (third args) env)
     '((:ref.cast :i31) :i31.get_s)
     (list (list :local.set int-local))
     ;; Extract size: spec >> 6
     `((:local.get ,spec-local)
       (:i32.const 6)
       :i32.shr_u
       (:local.set ,size-local))
     ;; Extract position: spec & 63
     `((:local.get ,spec-local)
       (:i32.const 63)
       :i32.and
       (:local.set ,pos-local))
     ;; Compute field-mask: ((1 << size) - 1) << position
     `((:i32.const 1)
       (:local.get ,size-local)
       :i32.shl                      ;; 1 << size
       (:i32.const 1)
       :i32.sub                      ;; (1 << size) - 1
       (:local.get ,pos-local)
       :i32.shl                      ;; field-mask
       (:local.set ,mask-local))
     ;; Compute: (integer & ~mask) | (newbyte & mask)
     `((:local.get ,int-local)
       (:local.get ,mask-local)
       (:i32.const -1)
       :i32.xor                      ;; ~mask
       :i32.and                      ;; integer & ~mask
       (:local.get ,new-local)
       (:local.get ,mask-local)
       :i32.and                      ;; newbyte & mask
       :i32.or                       ;; final result
       :ref.i31))))

;;; ============================================================
;;; Complex Number Functions (001-numeric-functions)
;;; ============================================================

(defun compile-complex (args env)
  "Compile (complex realpart &optional imagpart) - create a complex number.
   If imagpart is zero, may return just the realpart (ANSI behavior).
   Stack: [] -> [complex or real]"
  (when (< (length args) 1)
    (error "complex requires at least 1 argument"))
  (when (> (length args) 2)
    (error "complex requires at most 2 arguments"))
  (let ((complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (if (= (length args) 1)
        ;; One arg: (complex x) => #C(x 0)
        (append
         (compile-to-instructions (first args) env)
         `((:i32.const 0) :ref.i31  ;; imagpart = 0
           (:struct.new ,complex-type)))
        ;; Two args: (complex real imag)
        (let ((imag-local (env-add-local env (gensym "COMPLEX-IMAG"))))
          (append
           (compile-to-instructions (second args) env)
           (list (list :local.tee imag-local))
           ;; Check if imagpart is zero (fixnum 0)
           '((:ref.test :i31))
           `((:if (:result :anyref))
             (:local.get ,imag-local)
             (:ref.cast :i31)
             :i31.get_s
             (:i32.const 0)
             :i32.eq
             (:if (:result :anyref))
             ;; Imagpart is 0: return just realpart
             ,@(compile-to-instructions (first args) env)
             :else
             ;; Create complex
             ,@(compile-to-instructions (first args) env)
             (:local.get ,imag-local)
             (:struct.new ,complex-type)
             :end
             :else
             ;; Imagpart is not i31: create complex anyway
             ,@(compile-to-instructions (first args) env)
             (:local.get ,imag-local)
             (:struct.new ,complex-type)
             :end))))))

(defun compile-realpart (args env)
  "Compile (realpart number) - extract real component.
   For real numbers, returns the number itself.
   For complex, returns the real part.
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "realpart requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "REALPART-TMP")))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     `((:ref.test (:ref ,complex-type))
       (:if (:result :anyref))
       ;; Complex: extract real field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 0)
       :else
       ;; Real number: return as-is
       (:local.get ,temp-local)
       :end))))

(defun compile-imagpart (args env)
  "Compile (imagpart number) - extract imaginary component.
   For real numbers, returns 0.
   For complex, returns the imaginary part.
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "imagpart requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "IMAGPART-TMP")))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     `((:ref.test (:ref ,complex-type))
       (:if (:result :anyref))
       ;; Complex: extract imag field
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 1)
       :else
       ;; Real number: return 0
       (:i32.const 0) :ref.i31
       :end))))

(defun compile-conjugate (args env)
  "Compile (conjugate number) - compute complex conjugate.
   For real numbers, returns the number itself.
   For complex a+bi, returns a-bi.
   Stack: [] -> [number]"
  (when (/= (length args) 1)
    (error "conjugate requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "CONJ-TMP")))
        (imag-local (env-add-local env (gensym "CONJ-IMAG")))
        (complex-type clysm/compiler/codegen/gc-types:+type-complex+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     `((:ref.test (:ref ,complex-type))
       (:if (:result :anyref))
       ;; Complex: create conjugate (real, -imag)
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 0)  ;; real part
       ;; Negate imaginary part
       (:local.get ,temp-local)
       (:ref.cast (:ref ,complex-type))
       (:struct.get ,complex-type 1)  ;; imag part
       (:local.tee ,imag-local)
       ;; Check if imag is i31
       (:ref.test :i31)
       (:if (:result :anyref))
       ;; i31: negate
       (:i32.const 0)
       (:local.get ,imag-local)
       (:ref.cast :i31)
       :i31.get_s
       :i32.sub
       :ref.i31
       :else
       ;; Float: negate using f64
       (:local.get ,imag-local)
       (:ref.test (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:if (:result :anyref))
       (:f64.const 0.0)
       (:local.get ,imag-local)
       (:ref.cast (:ref ,clysm/compiler/codegen/gc-types:+type-float+))
       (:struct.get ,clysm/compiler/codegen/gc-types:+type-float+ 0)
       :f64.sub
       (:struct.new ,clysm/compiler/codegen/gc-types:+type-float+)
       :else
       ;; Unknown type: return 0 (shouldn't happen)
       (:i32.const 0) :ref.i31
       :end :end
       (:struct.new ,complex-type)
       :else
       ;; Real number: return as-is
       (:local.get ,temp-local)
       :end))))

(defun compile-phase (args env)
  "Compile (phase number) - compute the angle in radians.
   For positive reals: 0
   For negative reals: pi
   For complex a+bi: atan2(b, a)
   Stack: [] -> [float]"
  (when (/= (length args) 1)
    (error "phase requires exactly 1 argument"))
  ;; Ensure atan2 FFI is available
  (clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
  (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
  (let ((atan2-decl (clysm/ffi:lookup-foreign-function
                     clysm/ffi:*ffi-environment* :atan2)))
    (unless atan2-decl
      (error "FFI function atan2 not found"))
    (let ((atan2-idx (clysm/ffi:ffd-func-index atan2-decl))
          (input-local (env-add-local env (gensym "PHASE-INPUT")))
          (part-local (env-add-local env (gensym "PHASE-PART")))
          (real-local (env-add-local env (gensym "PHASE-REAL") :f64))
          (imag-local (env-add-local env (gensym "PHASE-IMAG") :f64))
          (complex-type clysm/compiler/codegen/gc-types:+type-complex+)
          (float-type clysm/compiler/codegen/gc-types:+type-float+))
      (append
       (compile-to-instructions (first args) env)
       (list (list :local.tee input-local))
       `((:ref.test (:ref ,complex-type))
         (:if (:result :anyref))
         ;; Complex: compute atan2(imag, real)
         ;; Get real part as f64 and save
         (:local.get ,input-local)
         (:ref.cast (:ref ,complex-type))
         (:struct.get ,complex-type 0)
         (:local.tee ,part-local)
         (:ref.test :i31)
         (:if (:result :f64))
         (:local.get ,part-local)
         (:ref.cast :i31)
         :i31.get_s
         :f64.convert_i32_s
         :else
         (:local.get ,part-local)
         (:ref.cast (:ref ,float-type))
         (:struct.get ,float-type 0)
         :end
         (:local.set ,real-local)
         ;; Get imag part as f64 and save
         (:local.get ,input-local)
         (:ref.cast (:ref ,complex-type))
         (:struct.get ,complex-type 1)
         (:local.tee ,part-local)
         (:ref.test :i31)
         (:if (:result :f64))
         (:local.get ,part-local)
         (:ref.cast :i31)
         :i31.get_s
         :f64.convert_i32_s
         :else
         (:local.get ,part-local)
         (:ref.cast (:ref ,float-type))
         (:struct.get ,float-type 0)
         :end
         (:local.set ,imag-local)
         ;; Call atan2(imag, real) - note: y first, then x
         (:local.get ,imag-local)
         (:local.get ,real-local)
         (:call-import ,atan2-idx)
         (:struct.new ,float-type)
         :else
         ;; Real number: check sign
         (:local.get ,input-local)
         (:ref.test :i31)
         (:if (:result :anyref))
         ;; i31
         (:local.get ,input-local)
         (:ref.cast :i31)
         :i31.get_s
         (:i32.const 0)
         :i32.lt_s
         (:if (:result :anyref))
         ;; Negative: return pi
         (:f64.const 3.141592653589793)
         (:struct.new ,float-type)
         :else
         ;; Non-negative: return 0
         (:f64.const 0.0)
         (:struct.new ,float-type)
         :end
         :else
         ;; Float
         (:local.get ,input-local)
         (:ref.test (:ref ,float-type))
         (:if (:result :anyref))
         (:local.get ,input-local)
         (:ref.cast (:ref ,float-type))
         (:struct.get ,float-type 0)
         (:f64.const 0.0)
         :f64.lt
         (:if (:result :anyref))
         ;; Negative: return pi
         (:f64.const 3.141592653589793)
         (:struct.new ,float-type)
         :else
         ;; Non-negative: return 0
         (:f64.const 0.0)
         (:struct.new ,float-type)
         :end
         :else
         ;; Unknown: return 0
         (:f64.const 0.0)
         (:struct.new ,float-type)
         :end :end :end)))))

;;; ============================================================
;;; Trigonometric Functions (001-numeric-functions Phase 6)
;;; ============================================================

(defun compile-unary-math-ffi (args env ffi-name)
  "Helper for unary math functions that call FFI imports.
   FFI-NAME: keyword like :sin, :cos, :exp, etc.
   Compiles argument, converts to f64, calls FFI, wraps as $float.
   Stack: [] -> [$float]"
  (when (/= (length args) 1)
    (error "~A requires exactly 1 argument" ffi-name))
  (let ((temp-local (env-add-local env (gensym "MATH-TMP")))
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    ;; Ensure math imports are registered and indices are assigned
    (clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
    (let ((decl (clysm/ffi:lookup-foreign-function
                 clysm/ffi:*ffi-environment* ffi-name)))
      (unless decl
        (error "FFI function ~A not found" ffi-name))
      (let ((func-index (clysm/ffi:ffd-func-index decl)))
        (append
         (compile-to-instructions (first args) env)
         (list (list :local.tee temp-local))
         ;; Check type and convert to f64
         `((:ref.test :i31)
           (:if (:result :f64))
           ;; Fixnum: convert to f64
           (:local.get ,temp-local)
           (:ref.cast :i31)
           :i31.get_s
           :f64.convert_i32_s
           :else
           ;; Float: extract f64
           (:local.get ,temp-local)
           (:ref.test (:ref ,float-type))
           (:if (:result :f64))
           (:local.get ,temp-local)
           (:ref.cast (:ref ,float-type))
           (:struct.get ,float-type 0)
           :else
           ;; Other: use 0.0 as fallback
           (:f64.const 0.0)
           :end :end)
         ;; Call FFI function
         ;; 001-numeric-functions: Use :call-import for FFI calls
         (list (list :call-import func-index))
         ;; Wrap result as $float
         `((:struct.new ,float-type)))))))

(defun compile-sin (args env)
  "Compile (sin x) - sine of x in radians.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :sin))

(defun compile-cos (args env)
  "Compile (cos x) - cosine of x in radians.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :cos))

(defun compile-tan (args env)
  "Compile (tan x) - tangent of x in radians.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :tan))

(defun compile-asin (args env)
  "Compile (asin x) - arc sine of x, result in radians.
   For |x| > 1, returns complex (TODO).
   Stack: [] -> [$float or $complex]"
  ;; For now, assume real domain. Complex promotion would need additional code.
  (compile-unary-math-ffi args env :asin))

(defun compile-acos (args env)
  "Compile (acos x) - arc cosine of x, result in radians.
   For |x| > 1, returns complex (TODO).
   Stack: [] -> [$float or $complex]"
  (compile-unary-math-ffi args env :acos))

(defun compile-atan (args env)
  "Compile (atan y &optional x) - arc tangent.
   With one arg: atan(y) in radians.
   With two args: atan2(y, x) in radians.
   Stack: [] -> [$float]"
  (when (< (length args) 1)
    (error "atan requires at least 1 argument"))
  (when (> (length args) 2)
    (error "atan requires at most 2 arguments"))
  (if (= (length args) 1)
      ;; Single arg: atan(y)
      (compile-unary-math-ffi args env :atan)
      ;; Two args: atan2(y, x)
      (let ((y-local (env-add-local env (gensym "ATAN-Y") :f64))
            (x-local (env-add-local env (gensym "ATAN-X") :f64))
            (temp-local (env-add-local env (gensym "ATAN-TMP")))
            (float-type clysm/compiler/codegen/gc-types:+type-float+))
        (clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
        (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
        (let ((decl (clysm/ffi:lookup-foreign-function
                     clysm/ffi:*ffi-environment* :atan2)))
          (unless decl
            (error "FFI function atan2 not found"))
          (let ((func-index (clysm/ffi:ffd-func-index decl)))
            (append
             ;; Convert first arg (y) to f64
             (compile-to-instructions (first args) env)
             (list (list :local.tee temp-local))
             `((:ref.test :i31)
               (:if (:result :f64))
               (:local.get ,temp-local)
               (:ref.cast :i31)
               :i31.get_s
               :f64.convert_i32_s
               :else
               (:local.get ,temp-local)
               (:ref.cast (:ref ,float-type))
               (:struct.get ,float-type 0)
               :end)
             (list (list :local.set y-local))
             ;; Convert second arg (x) to f64
             (compile-to-instructions (second args) env)
             (list (list :local.tee temp-local))
             `((:ref.test :i31)
               (:if (:result :f64))
               (:local.get ,temp-local)
               (:ref.cast :i31)
               :i31.get_s
               :f64.convert_i32_s
               :else
               (:local.get ,temp-local)
               (:ref.cast (:ref ,float-type))
               (:struct.get ,float-type 0)
               :end)
             (list (list :local.set x-local))
             ;; Call atan2(y, x) - note: y first, then x
             `((:local.get ,y-local)
               (:local.get ,x-local)
               (:call-import ,func-index)
               (:struct.new ,float-type))))))))

;;; ============================================================
;;; Mathematical Functions (001-numeric-functions Phase 7)
;;; ============================================================

(defun compile-exp (args env)
  "Compile (exp x) - e raised to the power x.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :exp))

(defun compile-log (args env)
  "Compile (log x &optional base) - natural or arbitrary base logarithm.
   With one arg: natural log (ln x).
   With two args: log base b of x = ln(x)/ln(b).
   Stack: [] -> [$float]"
  (when (< (length args) 1)
    (error "log requires at least 1 argument"))
  (when (> (length args) 2)
    (error "log requires at most 2 arguments"))
  (if (= (length args) 1)
      ;; Natural log
      (compile-unary-math-ffi args env :log)
      ;; Log with base: log(x, base) = ln(x) / ln(base)
      (let ((x-local (env-add-local env (gensym "LOG-X")))
            (temp-local (env-add-local env (gensym "LOG-TMP")))
            (float-type clysm/compiler/codegen/gc-types:+type-float+))
        (clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
        (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
        (let ((log-decl (clysm/ffi:lookup-foreign-function
                         clysm/ffi:*ffi-environment* :log)))
          (unless log-decl
            (error "FFI function log not found"))
          (let ((log-idx (clysm/ffi:ffd-func-index log-decl)))
            (append
             ;; Compute ln(x)
             (compile-to-instructions (first args) env)
             (list (list :local.tee temp-local))
             `((:ref.test :i31)
               (:if (:result :f64))
               (:local.get ,temp-local)
               (:ref.cast :i31)
               :i31.get_s
               :f64.convert_i32_s
               :else
               (:local.get ,temp-local)
               (:ref.cast (:ref ,float-type))
               (:struct.get ,float-type 0)
               :end)
             ;; 001-numeric-functions: Use :call-import for FFI calls
             (list (list :call-import log-idx))
             (list (list :local.set x-local))
             ;; Compute ln(base)
             (compile-to-instructions (second args) env)
             (list (list :local.tee temp-local))
             `((:ref.test :i31)
               (:if (:result :f64))
               (:local.get ,temp-local)
               (:ref.cast :i31)
               :i31.get_s
               :f64.convert_i32_s
               :else
               (:local.get ,temp-local)
               (:ref.cast (:ref ,float-type))
               (:struct.get ,float-type 0)
               :end)
             ;; 001-numeric-functions: Use :call-import for FFI calls
             (list (list :call-import log-idx))
             ;; Divide ln(x) / ln(base)
             `((:local.get ,x-local)
               :f64.div
               (:struct.new ,float-type))))))))

(defun compile-sqrt (args env)
  "Compile (sqrt x) - square root.
   Uses native f64.sqrt for non-negative, returns complex for negative (TODO).
   Stack: [] -> [$float or $complex]"
  (when (/= (length args) 1)
    (error "sqrt requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "SQRT-TMP")))
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check type and convert to f64
     `((:ref.test :i31)
       (:if (:result :f64))
       ;; Fixnum: convert to f64
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       :else
       ;; Float: extract f64
       (:local.get ,temp-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :f64))
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :else
       ;; Other: use 0.0 as fallback
       (:f64.const 0.0)
       :end :end)
     ;; Use native f64.sqrt
     '(:f64.sqrt)
     ;; Wrap result as $float
     `((:struct.new ,float-type)))))

(defun compile-expt (args env)
  "Compile (expt base power) - base raised to power.
   Uses FFI pow function.
   Stack: [] -> [$float]"
  (when (/= (length args) 2)
    (error "expt requires exactly 2 arguments"))
  (let ((base-local (env-add-local env (gensym "EXPT-BASE")))
        (power-local (env-add-local env (gensym "EXPT-PWR")))
        (temp-local (env-add-local env (gensym "EXPT-TMP")))
        (float-type clysm/compiler/codegen/gc-types:+type-float+))
    (clysm/ffi:ensure-math-imports clysm/ffi:*ffi-environment*)
    (clysm/ffi:assign-import-indices clysm/ffi:*ffi-environment*)
    (let ((pow-decl (clysm/ffi:lookup-foreign-function
                     clysm/ffi:*ffi-environment* :pow)))
      (unless pow-decl
        (error "FFI function pow not found"))
      (let ((pow-idx (clysm/ffi:ffd-func-index pow-decl)))
        (append
         ;; Convert base to f64 and save
         (compile-to-instructions (first args) env)
         (list (list :local.tee temp-local))
         `((:ref.test :i31)
           (:if (:result :f64))
           (:local.get ,temp-local)
           (:ref.cast :i31)
           :i31.get_s
           :f64.convert_i32_s
           :else
           (:local.get ,temp-local)
           (:ref.cast (:ref ,float-type))
           (:struct.get ,float-type 0)
           :end)
         (list (list :local.set base-local))
         ;; Convert power to f64 and save
         (compile-to-instructions (second args) env)
         (list (list :local.tee temp-local))
         `((:ref.test :i31)
           (:if (:result :f64))
           (:local.get ,temp-local)
           (:ref.cast :i31)
           :i31.get_s
           :f64.convert_i32_s
           :else
           (:local.get ,temp-local)
           (:ref.cast (:ref ,float-type))
           (:struct.get ,float-type 0)
           :end)
         (list (list :local.set power-local))
         ;; Call pow(base, power) with correct order
         ;; 001-numeric-functions: Use :call-import for FFI calls
         `((:local.get ,base-local)
           (:local.get ,power-local)
           (:call-import ,pow-idx)
           (:struct.new ,float-type)))))))

;;; ============================================================
;;; Hyperbolic Functions (001-numeric-functions Phase 8)
;;; ============================================================

(defun compile-sinh (args env)
  "Compile (sinh x) - hyperbolic sine.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :sinh))

(defun compile-cosh (args env)
  "Compile (cosh x) - hyperbolic cosine.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :cosh))

(defun compile-tanh (args env)
  "Compile (tanh x) - hyperbolic tangent.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :tanh))

(defun compile-asinh (args env)
  "Compile (asinh x) - inverse hyperbolic sine.
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :asinh))

(defun compile-acosh (args env)
  "Compile (acosh x) - inverse hyperbolic cosine.
   Domain: x >= 1. Returns complex for x < 1 (TODO).
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :acosh))

(defun compile-atanh (args env)
  "Compile (atanh x) - inverse hyperbolic tangent.
   Domain: -1 < x < 1. Returns complex for |x| >= 1 (TODO).
   Stack: [] -> [$float]"
  (compile-unary-math-ffi args env :atanh))

;;; ============================================================
;;; Numeric Type Conversion (001-numeric-functions US5)
;;; ============================================================

(defun compile-float (args env)
  "Compile (float number &optional prototype) - convert to float.
   Converts integer or ratio to floating-point.
   If already a float, returns unchanged.
   Stack: [] -> [$float]"
  (when (< (length args) 1)
    (error "float requires at least 1 argument"))
  (when (> (length args) 2)
    (error "float requires at most 2 arguments"))
  ;; Ignore prototype for now (would specify float type)
  (let ((temp-local (env-add-local env (gensym "FLOAT-TMP")))
        (float-type clysm/compiler/codegen/gc-types:+type-float+)
        (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check if already float
     `((:ref.test (:ref ,float-type))
       (:if (:result :anyref))
       ;; Already float - return as is
       (:local.get ,temp-local)
       :else
       ;; Check if fixnum
       (:local.get ,temp-local)
       (:ref.test :i31)
       (:if (:result :anyref))
       ;; Fixnum: convert i31 to f64
       (:local.get ,temp-local)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       (:struct.new ,float-type)
       :else
       ;; Check if ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,ratio-type))
       (:if (:result :anyref))
       ;; Ratio: divide numerator by denominator
       ;; Get numerator (field 0) as float
       (:local.get ,temp-local)
       (:ref.cast (:ref ,ratio-type))
       (:struct.get ,ratio-type 0)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       ;; Get denominator (field 1) as float
       (:local.get ,temp-local)
       (:ref.cast (:ref ,ratio-type))
       (:struct.get ,ratio-type 1)
       (:ref.cast :i31)
       :i31.get_s
       :f64.convert_i32_s
       ;; Divide
       :f64.div
       (:struct.new ,float-type)
       :else
       ;; Other types: return 0.0 as fallback
       (:f64.const 0.0)
       (:struct.new ,float-type)
       :end :end :end))))

(defun compile-rational (args env)
  "Compile (rational number) - convert to rational.
   Converts float to ratio using continued fraction algorithm.
   If already rational (fixnum or ratio), returns unchanged.
   Stack: [] -> [anyref (fixnum or ratio)]"
  (when (/= (length args) 1)
    (error "rational requires exactly 1 argument"))
  (let ((temp-local (env-add-local env (gensym "RAT-TMP")))
        (float-type clysm/compiler/codegen/gc-types:+type-float+)
        (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check if already fixnum (rational)
     `((:ref.test :i31)
       (:if (:result :anyref))
       ;; Already fixnum - return as is
       (:local.get ,temp-local)
       :else
       ;; Check if already ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,ratio-type))
       (:if (:result :anyref))
       ;; Already ratio - return as is
       (:local.get ,temp-local)
       :else
       ;; Check if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :anyref))
       ;; Float: convert to ratio
       ;; For simple cases like 0.5, 0.25, etc. we use a scaling approach
       ;; Multiply by 2^32, convert to integer, simplify fraction
       ;; For MVP: assume dyadic fractions (powers of 2 denominators)
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       ;; Check if it's a whole number first
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :f64.trunc
       :f64.eq
       (:if (:result :anyref))
       ;; Whole number: just convert to fixnum
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       :i32.trunc_f64_s
       :ref.i31
       :else
       ;; Fractional: use scaling (simplified continued fraction)
       ;; For common fractions like 0.5=1/2, 0.25=1/4, 0.125=1/8
       ;; Scale by finding appropriate power of 2
       ;; MVP: multiply by 1024, truncate, make ratio with 1024, simplify
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       (:f64.const 1024.0)  ; Use 2^10 as denominator
       :f64.mul
       :f64.nearest         ; Round to nearest integer
       :i32.trunc_f64_s
       :ref.i31             ; Numerator
       (:i32.const 1024)
       :ref.i31             ; Denominator
       (:struct.new ,ratio-type)
       :end
       :else
       ;; Unknown type: return as is
       (:local.get ,temp-local)
       :end :end :end))))

(defun compile-rationalize (args env)
  "Compile (rationalize number) - convert to simple rational approximation.
   Uses continued fraction algorithm (mediant method) to find rational
   approximation with small denominator.
   Ref: resources/HyperSpec/Body/f_ration.htm
   Stack: [] -> [anyref (fixnum or ratio)]"
  (when (/= (length args) 1)
    (error "rationalize requires exactly 1 argument"))
  (let* ((float-type clysm/compiler/codegen/gc-types:+type-float+)
         (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+)
         (temp-local (env-add-local env (gensym "RATZ-TMP")))
         ;; Continued fraction iteration locals
         (f-local (env-add-local env (gensym "RATZ-F") :f64))
         (a-local (env-add-local env (gensym "RATZ-A") :i32))
         (p0-local (env-add-local env (gensym "RATZ-P0") :i32))
         (q0-local (env-add-local env (gensym "RATZ-Q0") :i32))
         (p1-local (env-add-local env (gensym "RATZ-P1") :i32))
         (q1-local (env-add-local env (gensym "RATZ-Q1") :i32))
         (p-local (env-add-local env (gensym "RATZ-P") :i32))
         (q-local (env-add-local env (gensym "RATZ-Q") :i32))
         (sign-local (env-add-local env (gensym "RATZ-SIGN") :i32))
         (count-local (env-add-local env (gensym "RATZ-CNT") :i32))
         (frac-local (env-add-local env (gensym "RATZ-FRAC") :f64)))
    (append
     (compile-to-instructions (first args) env)
     (list (list :local.tee temp-local))
     ;; Check if already fixnum (rational)
     `((:ref.test :i31)
       (:if (:result :anyref))
       ;; Already fixnum - return as is
       (:local.get ,temp-local)
       :else
       ;; Check if already ratio
       (:local.get ,temp-local)
       (:ref.test (:ref ,ratio-type))
       (:if (:result :anyref))
       ;; Already ratio - return as is
       (:local.get ,temp-local)
       :else
       ;; Check if float
       (:local.get ,temp-local)
       (:ref.test (:ref ,float-type))
       (:if (:result :anyref))
       ;; Float: use continued fraction algorithm
       ;; Extract float value
       (:local.get ,temp-local)
       (:ref.cast (:ref ,float-type))
       (:struct.get ,float-type 0)
       (:local.set ,f-local)
       ;; Check for NaN (NaN != NaN: comparing f with itself)
       (:local.get ,f-local)
       (:local.get ,f-local)
       :f64.ne
       (:if (:result :anyref))
       ;; NaN: return original float unchanged (per ANSI CL spec)
       (:local.get ,temp-local)
       :else
       ;; Check for infinity (|f| == inf)
       (:local.get ,f-local)
       :f64.abs
       (:f64.const ,sb-ext:double-float-positive-infinity)
       :f64.eq
       (:if (:result :anyref))
       ;; Infinity: return original float unchanged
       (:local.get ,temp-local)
       :else
       ;; Check for zero
       (:local.get ,f-local)
       (:f64.const 0.0)
       :f64.eq
       (:if (:result :anyref))
       ;; Zero: return fixnum 0
       (:i32.const 0)
       :ref.i31
       :else
       ;; Handle sign
       (:i32.const 1)
       (:local.set ,sign-local)
       (:local.get ,f-local)
       (:f64.const 0.0)
       :f64.lt
       (:if)
       (:i32.const -1)
       (:local.set ,sign-local)
       (:local.get ,f-local)
       :f64.abs
       (:local.set ,f-local)
       :end
       ;; Check if it's a whole number
       (:local.get ,f-local)
       (:local.get ,f-local)
       :f64.trunc
       :f64.eq
       (:if (:result :anyref))
       ;; Whole number: return as fixnum with sign
       (:local.get ,f-local)
       :i32.trunc_f64_s
       (:local.get ,sign-local)
       :i32.mul
       :ref.i31
       :else
       ;; Continued fraction algorithm (mediant method)
       ;; Initialize: p0/q0 = 0/1, p1/q1 = 1/0
       (:i32.const 0) (:local.set ,p0-local)
       (:i32.const 1) (:local.set ,q0-local)
       (:i32.const 1) (:local.set ,p1-local)
       (:i32.const 0) (:local.set ,q1-local)
       (:i32.const 0) (:local.set ,count-local)
       ;; Loop: iterate continued fraction (flat structure)
       (:block)  ; outer block (br 1 exits)
       (:loop)   ; inner loop (br 0 continues)
       ;; Limit iterations to prevent overflow
       (:local.get ,count-local)
       (:i32.const 15)
       :i32.ge_s
       (:br_if 1)  ; exit block if count >= 15
       (:local.get ,count-local)
       (:i32.const 1)
       :i32.add
       (:local.set ,count-local)
       ;; a = floor(f)
       (:local.get ,f-local)
       :f64.floor
       :i32.trunc_f64_s
       (:local.set ,a-local)
       ;; p = a * p1 + p0, q = a * q1 + q0
       (:local.get ,a-local)
       (:local.get ,p1-local)
       :i32.mul
       (:local.get ,p0-local)
       :i32.add
       (:local.set ,p-local)
       (:local.get ,a-local)
       (:local.get ,q1-local)
       :i32.mul
       (:local.get ,q0-local)
       :i32.add
       (:local.set ,q-local)
       ;; Update: p0=p1, q0=q1, p1=p, q1=q
       (:local.get ,p1-local) (:local.set ,p0-local)
       (:local.get ,q1-local) (:local.set ,q0-local)
       (:local.get ,p-local) (:local.set ,p1-local)
       (:local.get ,q-local) (:local.set ,q1-local)
       ;; frac = f - a
       (:local.get ,f-local)
       (:local.get ,a-local)
       :f64.convert_i32_s
       :f64.sub
       (:local.tee ,frac-local)
       ;; Check if frac is very small (converged)
       (:f64.const 1e-10)
       :f64.lt
       (:br_if 1)  ; exit block if converged
       ;; f = 1 / frac
       (:f64.const 1.0)
       (:local.get ,frac-local)
       :f64.div
       (:local.set ,f-local)
       (:br 0)    ; continue loop
       :end       ; loop end
       :end       ; block end
       ;; Build result: construct ratio or integer
       ;; p1/q1 is our best approximation
       ;; If q1 == 1, return integer
       (:local.get ,q1-local)
       (:i32.const 1)
       :i32.eq
       (:if (:result :anyref))
       ;; Denominator is 1: return integer with sign
       (:local.get ,p1-local)
       (:local.get ,sign-local)
       :i32.mul
       :ref.i31
       :else
       ;; Build ratio with sign applied to numerator
       (:local.get ,p1-local)
       (:local.get ,sign-local)
       :i32.mul
       :ref.i31
       (:local.get ,q1-local)
       :ref.i31
       (:struct.new ,ratio-type)
       :end       ; q1 == 1 check
       :end       ; whole number check
       :end       ; zero check
       :end       ; infinity check
       :end       ; NaN check
       :else
       ;; Unknown type: return as is
       (:local.get ,temp-local)
       :end :end :end))))

(defun compile-write-to-string (args env)
  "Compile (write-to-string object &key base) - convert number to string.
   Ref: resources/HyperSpec/Body/f_wr_to_.htm
   Supports integers, ratios, floats with optional :base for integers.
   Stack: [] -> [anyref (string)]"
  (when (< (length args) 1)
    (error "write-to-string requires at least 1 argument"))
  ;; Extract keyword arguments
  (multiple-value-bind (keyword-args positional)
      (extract-keyword-args (rest args) '(:base))
    (let* ((obj-expr (first args))
           (base-expr (get-keyword-arg keyword-args :base))
           (string-type clysm/compiler/codegen/gc-types:+type-string+)
           (float-type clysm/compiler/codegen/gc-types:+type-float+)
           (ratio-type clysm/compiler/codegen/gc-types:+type-ratio+)
           (obj-local (env-add-local env (gensym "WTS-OBJ")))
           (base-local (env-add-local env (gensym "WTS-BASE") :i32))
           (val-local (env-add-local env (gensym "WTS-VAL") :i32))
           (result-local (env-add-local env (gensym "WTS-RES")))
           (sign-local (env-add-local env (gensym "WTS-SIGN") :i32))
           (digit-local (env-add-local env (gensym "WTS-DIG") :i32))
           (idx-local (env-add-local env (gensym "WTS-IDX") :i32))
           (len-local (env-add-local env (gensym "WTS-LEN") :i32))
           (buf-local (env-add-local env (gensym "WTS-BUF"))))
      (append
       ;; Compile object expression
       (compile-to-instructions obj-expr env)
       (list (list :local.set obj-local))
       ;; Set base (default 10)
       (if base-expr
           (append (compile-to-instructions base-expr env)
                   '((:ref.cast :i31) :i31.get_s))
           '((:i32.const 10)))
       (list (list :local.set base-local))
       ;; Validate base is in range [2, 36] - per ANSI CL spec
       ;; If out of range, trap with unreachable (type-error in CL terms)
       `((:local.get ,base-local)
         (:i32.const 2)
         :i32.lt_s
         (:local.get ,base-local)
         (:i32.const 36)
         :i32.gt_s
         :i32.or
         (:if)
         :unreachable  ; trap on invalid base
         :end)
       ;; Allocate buffer for digits (max 32 chars for 32-bit integer in base 2)
       `((:i32.const 32)
         (:array.new_default ,string-type)
         (:local.set ,buf-local)
         (:i32.const 31)  ; start at end of buffer
         (:local.set ,idx-local))
       ;; Check type and convert
       `((:local.get ,obj-local)
         (:ref.test :i31)
         (:if (:result :anyref))
         ;; Fixnum: convert to string in base
         (:local.get ,obj-local)
         (:ref.cast :i31)
         :i31.get_s
         (:local.tee ,val-local)
         ;; Handle sign
         (:i32.const 0)
         :i32.lt_s
         (:if)
         (:i32.const 1)
         (:local.set ,sign-local)
         (:i32.const 0)
         (:local.get ,val-local)
         :i32.sub
         (:local.set ,val-local)
         :else
         (:i32.const 0)
         (:local.set ,sign-local)
         :end
         ;; Handle zero case
         (:local.get ,val-local)
         (:i32.const 0)
         :i32.eq
         (:if)
         (:local.get ,buf-local)
         (:ref.cast (:ref ,string-type))
         (:local.get ,idx-local)
         (:i32.const 48)  ; '0'
         (:array.set ,string-type)
         (:local.get ,idx-local)
         (:i32.const 1)
         :i32.sub
         (:local.set ,idx-local)
         :else
         ;; Convert digits (loop while val > 0)
         (:block)
         (:loop)
         (:local.get ,val-local)
         (:i32.const 0)
         :i32.le_s
         (:br_if 1)  ; exit if val <= 0
         ;; digit = val mod base
         (:local.get ,val-local)
         (:local.get ,base-local)
         :i32.rem_u
         (:local.set ,digit-local)
         ;; Convert digit to ASCII: 0-9 -> '0'-'9', 10-35 -> 'A'-'Z'
         (:local.get ,digit-local)
         (:i32.const 10)
         :i32.lt_u
         (:if (:result :i32))
         (:local.get ,digit-local)
         (:i32.const 48)  ; '0'
         :i32.add
         :else
         (:local.get ,digit-local)
         (:i32.const 10)
         :i32.sub
         (:i32.const 65)  ; 'A'
         :i32.add
         :end
         ;; Store digit in buffer: buf[idx] = digit_char
         (:local.set ,digit-local)  ; save digit char
         (:local.get ,buf-local)
         (:ref.cast (:ref ,string-type))
         (:local.get ,idx-local)
         (:local.get ,digit-local)
         (:array.set ,string-type)
         ;; val = val / base
         (:local.get ,val-local)
         (:local.get ,base-local)
         :i32.div_u
         (:local.set ,val-local)
         ;; idx--
         (:local.get ,idx-local)
         (:i32.const 1)
         :i32.sub
         (:local.set ,idx-local)
         (:br 0)  ; continue loop
         :end
         :end
         :end
         ;; Add minus sign if negative
         (:local.get ,sign-local)
         (:if)
         (:local.get ,buf-local)
         (:ref.cast (:ref ,string-type))
         (:local.get ,idx-local)
         (:i32.const 45)  ; '-'
         (:array.set ,string-type)
         (:local.get ,idx-local)
         (:i32.const 1)
         :i32.sub
         (:local.set ,idx-local)
         :end
         ;; Build string from buffer (idx+1 to 31 inclusive)
         ;; idx now points to one before first char
         (:local.get ,idx-local)
         (:i32.const 1)
         :i32.add
         (:local.set ,idx-local)  ; idx now points to first char
         (:i32.const 32)
         (:local.get ,idx-local)
         :i32.sub  ; length = 32 - idx
         (:local.set ,len-local)
         ;; Create result array
         (:local.get ,len-local)
         (:array.new_default ,string-type)
         (:local.set ,result-local)
         ;; Copy bytes from buf[idx..31] to result[0..len-1]
         (:local.get ,result-local)
         (:ref.cast (:ref ,string-type))
         (:i32.const 0)  ; dest start
         (:local.get ,buf-local)
         (:ref.cast (:ref ,string-type))
         (:local.get ,idx-local)  ; src start
         (:local.get ,len-local)  ; length
         (:array.copy ,string-type ,string-type)
         ;; Return result
         (:local.get ,result-local)
         :else
         ;; Check for ratio type
         (:local.get ,obj-local)
         (:ref.test (:ref ,ratio-type))
         (:if (:result :anyref))
         ;; Ratio: convert as "num/denom" (simplified - just return placeholder)
         ;; For MVP, return a placeholder string "ratio"
         (:i32.const 5)
         (:array.new_default ,string-type)
         (:local.set ,result-local)
         ;; Write "ratio" as placeholder
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 0) (:i32.const 114) (:array.set ,string-type)  ; 'r'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 1) (:i32.const 97) (:array.set ,string-type)   ; 'a'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 2) (:i32.const 116) (:array.set ,string-type)  ; 't'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 3) (:i32.const 105) (:array.set ,string-type)  ; 'i'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 4) (:i32.const 111) (:array.set ,string-type)  ; 'o'
         (:local.get ,result-local)
         :else
         ;; Check for float type
         (:local.get ,obj-local)
         (:ref.test (:ref ,float-type))
         (:if (:result :anyref))
         ;; Float: return placeholder "float"
         (:i32.const 5)
         (:array.new_default ,string-type)
         (:local.set ,result-local)
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 0) (:i32.const 102) (:array.set ,string-type)  ; 'f'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 1) (:i32.const 108) (:array.set ,string-type)  ; 'l'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 2) (:i32.const 111) (:array.set ,string-type)  ; 'o'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 3) (:i32.const 97) (:array.set ,string-type)   ; 'a'
         (:local.get ,result-local) (:ref.cast (:ref ,string-type)) (:i32.const 4) (:i32.const 116) (:array.set ,string-type)  ; 't'
         (:local.get ,result-local)
         :else
         ;; Unknown type: return empty string
         (:i32.const 0)
         (:array.new_default ,string-type)
         :end :end :end)))))

(defun compile-parse-integer (args env)
  "Compile (parse-integer string &key start end radix junk-allowed).
   Parses a string representation of an integer.
   Returns two values: the integer and the position where parsing stopped.
   Stack: [] -> [anyref] (primary value, position stored in mv-buffer)"
  (when (< (length args) 1)
    (error "parse-integer requires at least 1 argument"))
  ;; Extract keyword arguments using the existing helper
  (multiple-value-bind (keyword-args positional)
      (extract-keyword-args (rest args) '(:start :end :radix :junk-allowed))
    (let* ((string-expr (first args))
           (start-expr (get-keyword-arg keyword-args :start))
           (end-expr (get-keyword-arg keyword-args :end))
           (radix-expr (get-keyword-arg keyword-args :radix))
           (junk-allowed-expr (get-keyword-arg keyword-args :junk-allowed))
           ;; Locals
           (string-local (env-add-local env (gensym "PI-STR")))
           (start-local (env-add-local env (gensym "PI-START") :i32))
           (end-local (env-add-local env (gensym "PI-END") :i32))
           (radix-local (env-add-local env (gensym "PI-RADIX") :i32))
           (junk-local (env-add-local env (gensym "PI-JUNK") :i32))
           (pos-local (env-add-local env (gensym "PI-POS") :i32))
           (result-local (env-add-local env (gensym "PI-RESULT") :i32))
           (sign-local (env-add-local env (gensym "PI-SIGN") :i32))
           (char-local (env-add-local env (gensym "PI-CHAR") :i32))
           (digit-local (env-add-local env (gensym "PI-DIGIT") :i32))
           (found-digit-local (env-add-local env (gensym "PI-FOUND") :i32))
           (string-type clysm/compiler/codegen/gc-types:+type-string+)
           (mv-array clysm/compiler/codegen/gc-types:+type-mv-array+))
      (append
       ;; Compile string expression
       (compile-to-instructions string-expr env)
       (list (list :ref.cast (list :ref string-type)))
       (list (list :local.set string-local))
       ;; Initialize radix (default 10)
       (if radix-expr
           (append (compile-to-instructions radix-expr env)
                   '((:ref.cast :i31) :i31.get_s))
           '((:i32.const 10)))
       (list (list :local.set radix-local))
       ;; Initialize start (default 0)
       (if start-expr
           (append (compile-to-instructions start-expr env)
                   '((:ref.cast :i31) :i31.get_s))
           '((:i32.const 0)))
       (list (list :local.set start-local))
       ;; Initialize end (default string length)
       (if end-expr
           (append (compile-to-instructions end-expr env)
                   '((:ref.cast :i31) :i31.get_s))
           (list (list :local.get string-local)
                 (list :struct.get string-type 0)  ; bytes array
                 :array.len))
       (list (list :local.set end-local))
       ;; Initialize junk-allowed (default nil/0)
       (if junk-allowed-expr
           (append (compile-to-instructions junk-allowed-expr env)
                   '(:ref.is_null (:i32.const 1) :i32.xor))  ; NIL->0, T->1
           '((:i32.const 0)))
       (list (list :local.set junk-local))
       ;; Initialize state
       `((:i32.const 0) (:local.set ,result-local)   ; result = 0
         (:i32.const 1) (:local.set ,sign-local)     ; sign = 1 (positive)
         (:i32.const 0) (:local.set ,found-digit-local)  ; found_digit = 0
         (:local.get ,start-local) (:local.set ,pos-local))  ; pos = start
       ;; Skip leading whitespace
       `((:block $skip_ws_done
          (:loop $skip_ws
           ;; Check if pos >= end
           (:local.get ,pos-local)
           (:local.get ,end-local)
           :i32.ge_u
           (:br_if $skip_ws_done)
           ;; Get character at pos
           (:local.get ,string-local)
           (:struct.get ,string-type 0)
           (:local.get ,pos-local)
           :array.get_u
           (:local.set ,char-local)
           ;; Check if whitespace (space=32, tab=9, newline=10, cr=13)
           (:local.get ,char-local)
           (:i32.const 32)
           :i32.eq
           (:local.get ,char-local)
           (:i32.const 9)
           :i32.eq
           :i32.or
           (:local.get ,char-local)
           (:i32.const 10)
           :i32.eq
           :i32.or
           (:local.get ,char-local)
           (:i32.const 13)
           :i32.eq
           :i32.or
           (:if)
           ;; Whitespace: advance pos and continue
           (:local.get ,pos-local)
           (:i32.const 1)
           :i32.add
           (:local.set ,pos-local)
           (:br $skip_ws)
           :end
          :end)  ; loop
         :end))  ; block
       ;; Check for sign
       `((:local.get ,pos-local)
         (:local.get ,end-local)
         :i32.lt_u
         (:if)
         (:local.get ,string-local)
         (:struct.get ,string-type 0)
         (:local.get ,pos-local)
         :array.get_u
         (:local.tee ,char-local)
         (:i32.const 45)  ; '-'
         :i32.eq
         (:if)
         (:i32.const -1) (:local.set ,sign-local)
         (:local.get ,pos-local) (:i32.const 1) :i32.add (:local.set ,pos-local)
         :else
         (:local.get ,char-local)
         (:i32.const 43)  ; '+'
         :i32.eq
         (:if)
         (:local.get ,pos-local) (:i32.const 1) :i32.add (:local.set ,pos-local)
         :end
         :end
         :end)
       ;; Parse digits
       `((:block $parse_done
          (:loop $parse_loop
           ;; Check if pos >= end
           (:local.get ,pos-local)
           (:local.get ,end-local)
           :i32.ge_u
           (:br_if $parse_done)
           ;; Get character
           (:local.get ,string-local)
           (:struct.get ,string-type 0)
           (:local.get ,pos-local)
           :array.get_u
           (:local.set ,char-local)
           ;; Convert character to digit value
           ;; '0'-'9' -> 0-9, 'A'-'Z' -> 10-35, 'a'-'z' -> 10-35
           (:local.get ,char-local)
           (:i32.const 48)  ; '0'
           :i32.sub
           (:local.tee ,digit-local)
           (:i32.const 10)
           :i32.lt_u
           (:if (:result :i32))
           ;; Digit 0-9
           (:local.get ,digit-local)
           :else
           ;; Check uppercase A-Z
           (:local.get ,char-local)
           (:i32.const 65)  ; 'A'
           :i32.sub
           (:local.tee ,digit-local)
           (:i32.const 26)
           :i32.lt_u
           (:if (:result :i32))
           (:local.get ,digit-local)
           (:i32.const 10)
           :i32.add
           :else
           ;; Check lowercase a-z
           (:local.get ,char-local)
           (:i32.const 97)  ; 'a'
           :i32.sub
           (:local.tee ,digit-local)
           (:i32.const 26)
           :i32.lt_u
           (:if (:result :i32))
           (:local.get ,digit-local)
           (:i32.const 10)
           :i32.add
           :else
           ;; Not a valid digit
           (:i32.const 99)  ; Invalid marker
           :end
           :end
           :end
           (:local.tee ,digit-local)
           ;; Check if digit < radix
           (:local.get ,radix-local)
           :i32.ge_u
           (:if)
           ;; Not a valid digit for this radix - stop parsing
           (:br $parse_done)
           :end
           ;; Valid digit: result = result * radix + digit
           (:local.get ,result-local)
           (:local.get ,radix-local)
           :i32.mul
           (:local.get ,digit-local)
           :i32.add
           (:local.set ,result-local)
           (:i32.const 1) (:local.set ,found-digit-local)
           ;; Advance position
           (:local.get ,pos-local)
           (:i32.const 1)
           :i32.add
           (:local.set ,pos-local)
           (:br $parse_loop)
          :end)  ; loop
         :end))  ; block
       ;; Check if we found any digits
       `((:local.get ,found-digit-local)
         :i32.eqz
         (:if (:result :anyref))
         ;; No digits found
         (:local.get ,junk-local)
         (:if (:result :anyref))
         ;; junk-allowed: return NIL
         (:ref.null :none)
         :else
         ;; junk not allowed: return NIL (should signal error, but MVP)
         (:ref.null :none)
         :end
         :else
         ;; Apply sign and return result
         (:local.get ,result-local)
         (:local.get ,sign-local)
         :i32.mul
         :ref.i31
         :end)
       ;; Store position as second value in mv-buffer
       `((:global.get 3)  ; mv-buffer
         (:i32.const 1)
         (:local.get ,pos-local)
         :ref.i31
         (:array.set ,mv-array)
         ;; Set mv-count to 2
         (:i32.const 2)
         (:global.set 2))))))

;;; ============================================================
;;; Destructive Modification (006-cons-list-ops)
;;; ============================================================

(defun compile-rplaca (args env)
  "Compile (rplaca cons new-value) - destructively modify car.
   Returns the modified cons cell.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "rplaca requires exactly 2 arguments"))
  (let* ((cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         ;; Local must be typed to hold the cast result (ref null $cons)
         (cons-local (env-add-local env (gensym "RPLACA-CONS")
                                    (list :ref-null cons-type))))
    (with-instruction-collector
      ;; Compile the cons argument and save it
      (emit* (compile-to-instructions (first args) env))
      (emit :ref.cast (list :ref cons-type))
      (emit :local.tee cons-local)
      ;; Compile the new value
      (emit* (compile-to-instructions (second args) env))
      ;; Set the car field (field 0)
      (emit :struct.set cons-type 0)
      ;; Return the cons cell
      (emit :local.get cons-local))))

(defun compile-rplacd (args env)
  "Compile (rplacd cons new-value) - destructively modify cdr.
   Returns the modified cons cell.
   Stack: [] -> [cons-ref]"
  (when (/= (length args) 2)
    (error "rplacd requires exactly 2 arguments"))
  (let* ((cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         ;; Local must be typed to hold the cast result (ref null $cons)
         (cons-local (env-add-local env (gensym "RPLACD-CONS")
                                    (list :ref-null cons-type))))
    (with-instruction-collector
      ;; Compile the cons argument and save it
      (emit* (compile-to-instructions (first args) env))
      (emit :ref.cast (list :ref cons-type))
      (emit :local.tee cons-local)
      ;; Compile the new value
      (emit* (compile-to-instructions (second args) env))
      ;; Set the cdr field (field 1)
      (emit :struct.set cons-type 1)
      ;; Return the cons cell
      (emit :local.get cons-local))))

;;; ============================================================
;;; List Accessors (006-cons-list-ops)
;;; ============================================================

(defun compile-nth-accessor (n args env)
  "Compile position accessor (second, third, ..., tenth).
   Equivalent to (car (nthcdr n list)).
   Stack: [] -> [anyref]"
  (when (/= (length args) 1)
    (error "Position accessor requires exactly 1 argument"))
  (let ((list-local (env-add-local env (gensym "NTH-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile the list argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list-local)
      ;; Traverse n times via cdr
      (dotimes (i n)
        (emit :local.get list-local)
        ;; Check for NIL before cdr
        (emit :ref.is_null)
        (emit '(:if (:result :anyref)))
        (emit :ref.null :none)    ; Return NIL if list ended
        (emit :else)
        (emit :local.get list-local)
        (emit :ref.cast (list :ref cons-type))
        (emit :struct.get cons-type 1)
        (emit :end)
        (emit :local.set list-local))
      ;; Now get car of result
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit '(:if (:result :anyref)))
      (emit :ref.null :none)
      (emit :else)
      (emit :local.get list-local)
      (emit :ref.cast (list :ref cons-type))
      (emit :struct.get cons-type 0)
      (emit :end))))

(defun compile-nth (args env)
  "Compile (nth n list) - 0-indexed access.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "nth requires exactly 2 arguments"))
  ;; For compile-time constant index, unroll
  ;; For runtime index, use a loop
  (let* ((index-form (first args))
         (list-form (second args)))
    (if (and (typep index-form 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type index-form) :fixnum))
        ;; Constant index - unroll
        (let ((n (clysm/compiler/ast:ast-literal-value index-form)))
          (if (< n 0)
              ;; Negative index returns NIL
              '((:ref.null :none))
              (compile-nth-accessor n (list list-form) env)))
        ;; Runtime index - generate loop
        (compile-nth-runtime args env))))

(defun compile-nth-runtime (args env)
  "Compile (nth n list) with runtime index using a loop.
   Stack: [] -> [anyref]"
  (let ((index-local (env-add-local env (gensym "NTH-IDX") :i32))
        (list-local (env-add-local env (gensym "NTH-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile index and convert to i32
      (emit* (compile-to-instructions (first args) env))
      (emit '(:ref.cast :i31))
      (emit :i31.get_s)
      (emit :local.set index-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check for negative index
      (emit :local.get index-local)
      (emit :i32.const 0)
      (emit :i32.lt_s)
      (emit* `((:if (:result :anyref))
               (:ref.null :none)  ; Negative index -> NIL
               :else
               ;; Loop to traverse
               (:block $nth_done (:result :anyref))
               (:loop $nth_loop (:result :anyref))
               ;; Check if index is 0
               (:local.get ,index-local)
               :i32.eqz
               (:if (:result :anyref))
               ;; Index is 0, return car
               (:local.get ,list-local)
               :ref.is_null
               (:if (:result :anyref))
               (:ref.null :none)
               :else
               (:local.get ,list-local)
               (:ref.cast ,(list :ref cons-type))
               (:struct.get ,cons-type 0)
               :end
               (:br $nth_done)
               :else
               ;; Decrement index
               (:local.get ,index-local)
               (:i32.const 1)
               :i32.sub
               (:local.set ,index-local)
               ;; Check if list is null
               (:local.get ,list-local)
               :ref.is_null
               (:if (:result :anyref))
               (:ref.null :none)
               (:br $nth_done)
               :else
               ;; Get cdr
               (:local.get ,list-local)
               (:ref.cast ,(list :ref cons-type))
               (:struct.get ,cons-type 1)
               (:local.set ,list-local)
               ;; Continue loop (need to push a dummy value for block type)
               (:ref.null :none)
               (:br $nth_loop)
               :end
               :end
               :end  ; loop
               :end  ; block
               :end)))))

(defun compile-nthcdr (args env)
  "Compile (nthcdr n list) - return list after n cdrs.
   Stack: [] -> [anyref]"
  (when (/= (length args) 2)
    (error "nthcdr requires exactly 2 arguments"))
  (let* ((index-form (first args))
         (list-form (second args)))
    (if (and (typep index-form 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type index-form) :fixnum))
        ;; Constant index - unroll
        (let ((n (clysm/compiler/ast:ast-literal-value index-form))
              (list-local (env-add-local env (gensym "NTHCDR-LIST")))
              (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
          (if (< n 0)
              ;; Negative index returns the list unchanged
              (compile-to-instructions list-form env)
              (with-instruction-collector
                ;; Compile the list
                (emit* (compile-to-instructions list-form env))
                (emit :local.set list-local)
                ;; Traverse n times via cdr
                (dotimes (i n)
                  (emit :local.get list-local)
                  (emit :ref.is_null)
                  (emit '(:if (:result :anyref)))
                  (emit :ref.null :none)
                  (emit :else)
                  (emit :local.get list-local)
                  (emit :ref.cast (list :ref cons-type))
                  (emit :struct.get cons-type 1)
                  (emit :end)
                  (emit :local.set list-local))
                ;; Return the result
                (emit :local.get list-local))))
        ;; Runtime index - generate loop (simplified for now)
        (compile-nthcdr-runtime args env))))

(defun compile-nthcdr-runtime (args env)
  "Compile (nthcdr n list) with runtime index.
   Stack: [] -> [anyref]"
  (let ((index-local (env-add-local env (gensym "NTHCDR-IDX") :i32))
        (list-local (env-add-local env (gensym "NTHCDR-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile index and convert to i32
      (emit* (compile-to-instructions (first args) env))
      (emit '(:ref.cast :i31))
      (emit :i31.get_s)
      (emit :local.set index-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check for negative/zero index
      (emit :local.get index-local)
      (emit :i32.const 0)
      (emit :i32.le_s)
      (emit* `((:if (:result :anyref))
               (:local.get ,list-local)  ; <= 0 -> return list
               :else
               ;; Loop to traverse
               (:block $nthcdr_done (:result :anyref))
               (:loop $nthcdr_loop (:result :anyref))
               ;; Check if list is null
               (:local.get ,list-local)
               :ref.is_null
               (:if (:result :anyref))
               (:ref.null :none)
               (:br $nthcdr_done)
               :else
               ;; Get cdr
               (:local.get ,list-local)
               (:ref.cast ,(list :ref cons-type))
               (:struct.get ,cons-type 1)
               (:local.set ,list-local)
               ;; Decrement index
               (:local.get ,index-local)
               (:i32.const 1)
               :i32.sub
               (:local.set ,index-local)
               ;; Check if index is 0
               (:local.get ,index-local)
               :i32.eqz
               (:if (:result :anyref))
               (:local.get ,list-local)
               (:br $nthcdr_done)
               :else
               (:ref.null :none)
               (:br $nthcdr_loop)
               :end
               :end
               :end  ; loop
               :end  ; block
               :end)))))

(defun compile-regular-call (function args env)
  "Compile a regular function call (T061).
   Uses :return_call for tail calls (TCO), :call otherwise."
  (let ((func-idx (env-lookup-function env function)))
    (unless func-idx
      (error "Undefined function: ~A" function))
    ;; Compile arguments (not in tail position)
    (let ((arg-env (env-with-non-tail env)))
      (with-instruction-collector
        (dolist (arg args)
          (emit* (compile-to-instructions arg arg-env)))
        ;; Call - use return_call for tail calls
        (if (cenv-in-tail-position env)
            (emit :return_call func-idx)
            (emit :call func-idx))))))

;;; ============================================================
;;; Funcall Compilation (T085-T087)
;;; ============================================================

(defun compile-funcall (args env)
  "Compile (funcall fn arg1 arg2 ...).
   The first arg is a closure, remaining args are passed to it.
   Uses :return_call_ref for tail calls (TCO), :call_ref otherwise."
  (when (null args)
    (error "funcall requires at least one argument"))
  (let* ((closure-expr (first args))
         (call-args (rest args))
         (arity (length call-args))
         (arg-env (env-with-non-tail env))
         (closure-local (cenv-local-counter env)))
    (incf (car (cenv-local-counter-box env)))  ; Allocate temp local
    (with-instruction-collector
      ;; Compile the closure expression (not in tail position)
      (emit* (compile-to-instructions closure-expr arg-env))
      ;; Save closure to local
      (emit :local.set closure-local)
      ;; Push closure as first argument (self reference)
      (emit :local.get closure-local)
      ;; Push all call arguments (not in tail position)
      (dolist (arg call-args)
        (emit* (compile-to-instructions arg arg-env)))
      ;; Get closure and extract code_N field (field 3 = code_N for variadic)
      (emit :local.get closure-local)
      ;; Cast to closure type
      (emit :ref.cast clysm/compiler/codegen/gc-types:+type-closure+)
      ;; Get the appropriate code field based on arity
      (let ((code-field (case arity
                          (0 0)  ; code_0
                          (1 1)  ; code_1
                          (2 2)  ; code_2
                          (t 3)))) ; code_N
        (emit :struct.get clysm/compiler/codegen/gc-types:+type-closure+ code-field))
      ;; Cast the funcref to the specific function type before call_ref
      (let ((func-type (case arity
                         (0 clysm/compiler/codegen/gc-types:+type-func-0+)
                         (1 clysm/compiler/codegen/gc-types:+type-func-1+)
                         (2 clysm/compiler/codegen/gc-types:+type-func-2+)
                         (3 clysm/compiler/codegen/gc-types:+type-func-3+)
                         (t clysm/compiler/codegen/gc-types:+type-func-n+))))
        ;; Cast funcref to the specific function type
        (emit :ref.cast func-type)
        ;; Call through the typed function reference
        ;; Use return_call_ref for tail calls
        (if (cenv-in-tail-position env)
            (emit :return_call_ref func-type)
            (emit :call_ref func-type))))))

;;; ============================================================
;;; Conditional Compilation (T054-T055)
;;; ============================================================

(defun compile-if (ast env)
  "Compile an if expression.
   Note: if creates a Wasm block, so we increment block-depth for nested go/return-from.
   Propagates tail position to both then and else branches for TCO."
  (let ((test (clysm/compiler/ast:ast-if-test ast))
        (then-branch (clysm/compiler/ast:ast-if-then ast))
        (else-branch (clysm/compiler/ast:ast-if-else ast))
        ;; Create new env with incremented block depth for if block
        ;; Test expression is NOT in tail position
        (test-env (env-with-non-tail env))
        ;; Both branches inherit tail position from parent
        (if-env (copy-compilation-env env)))
    (incf (cenv-block-depth if-env))
    (append
     ;; Compile test (NOT in tail position)
     (compile-to-instructions test test-env)
     ;; Check if not NIL
     (compile-nil-check)
     ;; If-then-else (branches inherit tail position)
     '((:if (:result :anyref)))
     (compile-to-instructions then-branch if-env)
     '(:else)
     (compile-to-instructions else-branch if-env)
     '(:end))))

(defun compile-nil-check ()
  "Generate code to check if TOS is not NIL.
   Returns i32: 1 if not-nil, 0 if nil.
   NIL is represented as null reference, so we use ref.is_null."
  '(:ref.is_null     ; Is this null?
    :i32.eqz))       ; Invert (not nil => 1)

;;; ============================================================
;;; Binding Compilation (T056-T058)
;;; ============================================================

;;; ------------------------------------------------------------
;;; Special Binding Detection and Helpers (T030)
;;; ------------------------------------------------------------

(defun binding-is-special-p (name)
  "Check if a binding name refers to a special variable (T030).
   A binding is special if the variable has been declared with defvar/defparameter."
  (clysm/compiler/env:special-variable-p name))

(defun partition-bindings (bindings)
  "Partition bindings into (special-bindings . lexical-bindings) (T030).
   Returns two lists: special bindings and lexical bindings."
  (let ((special '())
        (lexical '()))
    (dolist (binding bindings)
      (let ((name (car binding)))
        (if (binding-is-special-p name)
            (push binding special)
            (push binding lexical))))
    (cons (nreverse special) (nreverse lexical))))

;;; ------------------------------------------------------------
;;; Special Binding Save/Restore Code Generation (T031-T032)
;;; ------------------------------------------------------------

(defun emit-save-special-binding (name env)
  "Emit code to save the current value of a special variable (T031).
   Returns (instructions . save-local-idx).
   The current value is saved to a local for later restoration."
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (save-local (env-add-local env (gensym "save"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Generate: global.get sym, struct.get value, local.set save
    (cons `((:global.get ,global-idx)
            (:struct.get ,symbol-type 1)  ; Get value field
            (:local.set ,save-local))
          save-local)))

(defun emit-set-special-binding (name value-instrs env)
  "Emit code to set a special variable's value (T031).
   VALUE-INSTRS are the instructions that produce the new value."
  (declare (ignore env))
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (unless global-idx
      (error "Special variable ~A not initialized" name))
    ;; Generate: global.get sym, <value-instrs>, struct.set value
    `((:global.get ,global-idx)
      ,@value-instrs
      (:struct.set ,symbol-type 1))))

(defun emit-restore-special-binding (name save-local)
  "Emit code to restore a special variable from a saved local (T032).
   SAVE-LOCAL is the local index where the old value was saved."
  (let ((global-idx (get-special-var-global-index name))
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Generate: global.get sym, local.get save, struct.set value
    `((:global.get ,global-idx)
      (:local.get ,save-local)
      (:struct.set ,symbol-type 1))))

(defun compile-let (ast env)
  "Compile a let or let* expression (T033).
   Handles both lexical and special (dynamic) bindings:
   - Lexical bindings use Wasm locals
   - Special bindings modify the global symbol's value with save/restore
   Tail position is inherited by the last body form ONLY if there are no
   special bindings (special bindings require save/restore which breaks TCO)."
  (let ((bindings (clysm/compiler/ast:ast-let-bindings ast))
        (body (clysm/compiler/ast:ast-let-body ast))
        (sequential-p (clysm/compiler/ast:ast-let-sequential-p ast))
        (new-env (extend-compilation-env env))
        (special-save-locals '())  ; ((name . save-local-idx) ...)
        ;; Binding value expressions are NOT in tail position
        (binding-env (env-with-non-tail env)))
    (with-instruction-collector
      ;; Process bindings based on let vs let*
      (if sequential-p
          ;; let*: each binding sees previous bindings
          (dolist (binding bindings)
            (let* ((name (car binding))
                   (value-form (cdr binding)))
              (if (binding-is-special-p name)
                  ;; Special binding: save, compute, set global
                  (let* ((save-result (emit-save-special-binding name new-env))
                         (save-instrs (car save-result))
                         (save-local (cdr save-result))
                         (value-instrs (compile-to-instructions value-form (env-with-non-tail new-env)))
                         (set-instrs (emit-set-special-binding name value-instrs new-env)))
                    (push (cons name save-local) special-save-locals)
                    (emit* save-instrs)
                    (emit* set-instrs))
                  ;; Lexical binding: use local
                  (let ((idx (env-add-local new-env name)))
                    (emit* (compile-to-instructions value-form (env-with-non-tail new-env)))
                    (emit :local.set idx)))))
          ;; let: all values computed before any binding visible
          ;; For special bindings, we still need to save first, then set
          (let ((compiled-values '()))
            ;; First pass: save special vars and compile all values
            (dolist (binding bindings)
              (let* ((name (car binding))
                     (value-form (cdr binding)))
                (if (binding-is-special-p name)
                    ;; Special: save current value first
                    (let* ((save-result (emit-save-special-binding name new-env))
                           (save-instrs (car save-result))
                           (save-local (cdr save-result)))
                      (push (cons name save-local) special-save-locals)
                      (emit* save-instrs)
                      ;; Compile value (uses original env for parallel semantics, NOT in tail)
                      (push (cons :special (cons name (compile-to-instructions value-form binding-env)))
                            compiled-values))
                    ;; Lexical: just allocate local and compile value
                    (let ((idx (env-add-local new-env name)))
                      (push (cons :lexical (cons idx (compile-to-instructions value-form binding-env)))
                            compiled-values)))))
            (setf compiled-values (nreverse compiled-values))
            ;; Second pass: set all bindings
            (dolist (cv compiled-values)
              (let ((kind (car cv))
                    (data (cdr cv)))
                (if (eq kind :special)
                    ;; Set special variable's global
                    (let ((name (car data))
                          (value-instrs (cdr data)))
                      (emit* (emit-set-special-binding name value-instrs new-env)))
                    ;; Set lexical local
                    (let ((idx (car data))
                          (value-instrs (cdr data)))
                      (emit* value-instrs)
                      (emit :local.set idx)))))))
      ;; Compile body - non-final forms are NOT in tail position
      (let ((non-tail-body-env (env-with-non-tail new-env)))
        (dolist (form (butlast body))
          (emit* (compile-to-instructions form non-tail-body-env))
          (emit :drop)))
      ;; Last body form: tail position ONLY if no special bindings
      ;; (special bindings require save/restore which breaks TCO)
      (let* ((has-special-bindings special-save-locals)
             (last-form-env (if has-special-bindings
                                (env-with-non-tail new-env)
                                new-env))  ; inherit tail position only without special bindings
             (body-result-instrs
               (when body
                 (compile-to-instructions (car (last body)) last-form-env))))
        ;; Save result to a local before restore (if we have special bindings)
        (if special-save-locals
            (let ((result-local (env-add-local new-env (gensym "let-result"))))
              (emit* body-result-instrs)
              (emit :local.set result-local)
              ;; Restore special bindings in reverse order
              (dolist (save-entry (reverse special-save-locals))
                (let ((name (car save-entry))
                      (save-local (cdr save-entry)))
                  (emit* (emit-restore-special-binding name save-local))))
              ;; Return the saved result
              (emit :local.get result-local))
            ;; No special bindings, just append body result
            (emit* body-result-instrs))))))

(defun extend-compilation-env (env)
  "Create an extended copy of the compilation environment for a new scope.
   Shares the local-counter-box and local-types-box so child scopes can allocate locals."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields
   :blocks (cenv-blocks env)
   :block-depth (cenv-block-depth env)
   :tagbody-tags (cenv-tagbody-tags env)
   :tagbody-context (cenv-tagbody-context env)  ; Inherit tagbody context
   :catch-tags (cenv-catch-tags env)
   :unwind-stack (cenv-unwind-stack env)))

(defun copy-compilation-env (env)
  "Create a copy of the compilation environment with mutable fields copied.
   Used by block/return-from, tagbody/go, etc. for nested scopes."
  (make-compilation-env
   :locals (copy-list (cenv-locals env))
   :local-counter-box (cenv-local-counter-box env)  ; Shared!
   :local-types-box (cenv-local-types-box env)      ; Shared for local type tracking
   :functions (cenv-functions env)
   :function-counter (cenv-function-counter env)
   :in-tail-position (cenv-in-tail-position env)
   :captured-vars (cenv-captured-vars env)
   :local-functions (cenv-local-functions env)
   ;; Phase 6 fields - copy lists for mutation safety
   :blocks (copy-list (cenv-blocks env))
   :block-depth (cenv-block-depth env)
   :tagbody-tags (copy-list (cenv-tagbody-tags env))
   :tagbody-context (cenv-tagbody-context env)  ; Reference (not copied for nesting)
   :catch-tags (copy-list (cenv-catch-tags env))
   :unwind-stack (copy-list (cenv-unwind-stack env))))

;;; ============================================================
;;; Tail Position Management (TCO)
;;; ============================================================

(defun env-with-tail-position (env tail-position-p)
  "Return an environment with the specified tail position flag.
   If the flag matches the current value, returns the same environment.
   Otherwise, creates a copy with the new flag value."
  (if (eq (cenv-in-tail-position env) tail-position-p)
      env
      (let ((new-env (copy-compilation-env env)))
        (setf (cenv-in-tail-position new-env) tail-position-p)
        new-env)))

(defun env-with-non-tail (env)
  "Return an environment with tail position cleared.
   Used when compiling sub-expressions that are not in tail position
   (e.g., function arguments, test expressions, non-final forms)."
  (env-with-tail-position env nil))

(defun env-with-tail (env)
  "Return an environment with tail position set.
   Used when compiling expressions that are in tail position
   (e.g., last form in function body, branches of if)."
  (env-with-tail-position env t))

;;; ============================================================
;;; Flet/Labels Compilation (T089-T090)
;;; ============================================================

(defun compile-flet (ast env)
  "Compile a flet expression.
   Each local function is compiled as a lambda bound to a local variable.
   Functions in flet cannot call themselves (non-recursive)."
  (let* ((definitions (clysm/compiler/ast:ast-flet-definitions ast))
         (body (clysm/compiler/ast:ast-flet-body ast))
         (new-env (extend-compilation-env env))
         (local-func-bindings '()))
    (with-instruction-collector
      ;; For each function definition:
      ;; 1. Create a lambda from the function
      ;; 2. Allocate a local for the closure
      ;; 3. Compile the lambda and store in local
      (dolist (def definitions)
        (let* ((name (first def))
               (params (second def))
               (func-body (third def))  ; List of parsed AST nodes
               ;; Allocate a local to hold the closure
               (local-idx (env-add-local new-env name)))
          ;; Remember this binding for later funcall translation
          (push (cons name local-idx) local-func-bindings)
          ;; Create a lambda AST node
          (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                             :parameters params
                             :body func-body)))
            ;; Compile the lambda (in original env, not new-env, so it can't see itself)
            (emit* (compile-lambda lambda-ast env))
            ;; Store in local
            (emit :local.set local-idx))))
      ;; Register local functions for funcall-style access
      (setf (cenv-local-functions new-env) (nreverse local-func-bindings))
      ;; Compile body
      (dolist (form (butlast body))
        (emit* (compile-to-instructions form new-env))
        (emit :drop))
      (when body
        (emit* (compile-to-instructions (car (last body)) new-env))))))

(defun compile-labels (ast env)
  "Compile a labels expression.
   Functions in labels CAN call themselves and each other (recursive).
   Implementation: Two-phase closure creation to handle mutual recursion:
   1. Create closures with null env, store in locals
   2. Update env fields to point to the closures (now that they exist)
   Tail position propagation:
   - Local function bodies are compiled with tail position for their last forms
   - Labels body's last form inherits tail position from parent for TCO"
  (let* ((definitions (clysm/compiler/ast:ast-labels-definitions ast))
         (body (clysm/compiler/ast:ast-labels-body ast))
         (new-env (extend-compilation-env env))
         (local-func-bindings '())
         ;; Non-final body forms are NOT in tail position
         (non-tail-env (env-with-non-tail new-env))
         (closure-type clysm/compiler/codegen/gc-types:+type-closure+))
    ;; First pass: allocate locals for all function closures
    (dolist (def definitions)
      (let* ((name (first def))
             (local-idx (env-add-local new-env name)))
        (push (cons name local-idx) local-func-bindings)))
    (setf local-func-bindings (nreverse local-func-bindings))
    ;; Register local functions so the body can call them
    (setf (cenv-local-functions new-env) local-func-bindings)
    (with-instruction-collector
      ;; Phase 1: Create closures with null env initially
      (dolist (def definitions)
        (let* ((name (first def))
               (params (second def))
               (func-body (third def))
               (local-idx (cdr (assoc name local-func-bindings))))
          ;; Create lambda AST
          (let ((lambda-ast (clysm/compiler/ast:make-ast-lambda
                             :parameters params
                             :body func-body)))
            ;; Compile the lambda - initially with null env
            (emit* (compile-labels-lambda-phase1 lambda-ast new-env local-func-bindings))
            ;; Store in local
            (emit :local.set local-idx))))
      ;; Phase 2: Now update each closure's env field to point to the env cons-list
      (dolist (def definitions)
        (let* ((name (first def))
               (local-idx (cdr (assoc name local-func-bindings))))
          ;; Get the closure
          (emit :local.get local-idx)
          ;; Cast to closure type
          (emit :ref.cast closure-type)
          ;; Build the env cons-list containing all local function closures
          (emit* (generate-labels-env-update local-func-bindings new-env))
          ;; Update the env field (field 4) - struct.set $closure 4
          (emit :struct.set closure-type 4)))
      ;; Compile body - non-final forms are NOT in tail position
      (dolist (form (butlast body))
        (emit* (compile-to-instructions form non-tail-env))
        (emit :drop))
      ;; Last form inherits tail position from parent for TCO
      (when body
        (emit* (compile-to-instructions (car (last body)) new-env))))))

(defun compile-labels-lambda-phase1 (ast env local-func-bindings)
  "Phase 1: Create a closure with null env - env will be filled in phase 2."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (body (clysm/compiler/ast:ast-lambda-body ast))
         ;; Collect regular free vars
         (regular-free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         ;; All local functions defined in labels
         (func-names (mapcar #'car local-func-bindings))
         ;; Check which functions are actually called in the body
         (called-funcs (collect-called-functions body))
         ;; Filter to only include function names that are actually called
         (used-func-names (intersection func-names called-funcs :test #'eq))
         ;; For labels, we need to capture the function closures themselves
         ;; Remove any func-names from regular free vars (they're handled separately)
         (free-vars (set-difference regular-free-vars func-names))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params))
         (closure-type clysm/compiler/codegen/gc-types:+type-closure+))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-names used-func-names
                :local-func-bindings local-func-bindings
                :func-index func-index
                :parent-env env
                :arity arity
                :is-labels-lambda t)
          *pending-lambdas*)
    ;; Create closure with null env (will be updated in phase 2)
    (with-instruction-collector
      ;; code fields
      (if (= arity 0)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      (if (= arity 1)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      (if (= arity 2)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      (emit :ref.func func-index)
      ;; env - null initially, will be set in phase 2
      (emit :ref.null :any)
      ;; Create the struct
      (emit :struct.new closure-type))))

(defun generate-labels-env-update (local-func-bindings env)
  "Generate instructions to create the env cons-list for labels closures.
   Returns instructions that build a cons-list of all local function closures."
  (declare (ignore env))
  ;; Build cons-list from the local function closures
  ;; (cons f1 (cons f2 (cons f3 nil)))
  (if (null local-func-bindings)
      '((:ref.null :none))
      (let ((cons-type clysm/compiler/codegen/gc-types:+type-cons+))
        (with-instruction-collector
          ;; Push closures in order
          (dolist (binding local-func-bindings)
            (emit :local.get (cdr binding)))
          ;; Now build the cons list from end
          ;; Stack: f1, f2, f3
          ;; We want: (cons f1 (cons f2 (cons f3 nil)))
          ;; Push nil
          (emit :ref.null :none)
          ;; For each closure (in reverse), create cons
          (dotimes (i (length local-func-bindings))
            (emit :struct.new cons-type))))))

(defun collect-called-functions (body)
  "Collect all function names that are called in the body (list of AST nodes)."
  (let ((result '()))
    (labels ((collect (ast)
               (etypecase ast
                 (clysm/compiler/ast:ast-call
                  (let ((fn (clysm/compiler/ast:ast-call-function ast)))
                    (when (symbolp fn)
                      (pushnew fn result)))
                  (dolist (arg (clysm/compiler/ast:ast-call-arguments ast))
                    (collect arg)))
                 (clysm/compiler/ast:ast-if
                  (collect (clysm/compiler/ast:ast-if-test ast))
                  (collect (clysm/compiler/ast:ast-if-then ast))
                  (when (clysm/compiler/ast:ast-if-else ast)
                    (collect (clysm/compiler/ast:ast-if-else ast))))
                 (clysm/compiler/ast:ast-let
                  (dolist (b (clysm/compiler/ast:ast-let-bindings ast))
                    (collect (cdr b)))
                  (dolist (form (clysm/compiler/ast:ast-let-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-progn
                  (dolist (form (clysm/compiler/ast:ast-progn-forms ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-lambda
                  (dolist (form (clysm/compiler/ast:ast-lambda-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-block
                  (dolist (form (clysm/compiler/ast:ast-block-body ast))
                    (collect form)))
                 (clysm/compiler/ast:ast-return-from
                  (when (clysm/compiler/ast:ast-return-from-value ast)
                    (collect (clysm/compiler/ast:ast-return-from-value ast))))
                 (clysm/compiler/ast:ast-setq
                  (collect (clysm/compiler/ast:ast-setq-value ast)))
                 ;; Terminals - no function calls
                 (clysm/compiler/ast:ast-literal nil)
                 (clysm/compiler/ast:ast-var-ref nil)
                 (clysm/compiler/ast:ast-defun nil))))
      (dolist (form body)
        (collect form)))
    result))

;;; ============================================================
;;; Progn Compilation
;;; ============================================================

(defun compile-progn (ast env)
  "Compile a progn expression.
   First scans for defuns and registers them, then compiles all forms.
   Only the last form inherits tail position for TCO."
  (let ((forms (clysm/compiler/ast:ast-progn-forms ast))
        (non-tail-env (env-with-non-tail env)))
    ;; First pass: register all defuns so they can be called
    (dolist (form forms)
      (when (typep form 'clysm/compiler/ast:ast-defun)
        (let ((name (clysm/compiler/ast:ast-defun-name form)))
          (unless (env-lookup-function env name)
            (env-add-function env name)))))
    (if (null forms)
        (compile-literal (clysm/compiler/ast:make-nil-literal))
        (with-instruction-collector
          ;; Second pass: compile all forms
          ;; Non-final forms are NOT in tail position
          (dolist (form (butlast forms))
            (emit* (compile-to-instructions form non-tail-env))
            (emit :drop))
          ;; Last form inherits tail position from parent
          (emit* (compile-to-instructions (car (last forms)) env))))))

;;; ============================================================
;;; Variable Assignment
;;; ============================================================

(defun compile-setq (ast env)
  "Compile a setq expression (T042).
   Handles both lexical (local) and special (dynamic) variables."
  (let* ((name (clysm/compiler/ast:ast-setq-name ast))
         (value (clysm/compiler/ast:ast-setq-value ast))
         (local-idx (env-lookup-local env name)))
    (cond
      ;; Local variable - use local.tee for assignment + return value
      (local-idx
       (append
        (compile-to-instructions value env)
        (list (list :local.tee local-idx))))
      ;; Special variable - set symbol's value field (T042-T043)
      ((clysm/compiler/env:special-variable-p name)
       (compile-special-setq name value env))
      ;; Unknown variable
      (t
       (error "Cannot setq undefined variable: ~A" name)))))

(defun compile-special-setq (name value-form env)
  "Compile setq for a special variable (T043).
   Sets the symbol's value field and returns the new value.
   Pattern: global.get sym, <value>, local.tee temp, struct.set sym 1, local.get temp"
  (let* ((global-idx (get-special-var-global-index name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+)
         (result-local (env-add-local env (gensym "setq-result"))))
    (unless global-idx
      (error "Special variable ~A not initialized with defvar/defparameter" name))
    ;; Compile value, save to temp, then set symbol's value field
    (append
     ;; Get symbol reference
     `((:global.get ,global-idx))
     ;; Compile value and save for return
     (compile-to-instructions value-form env)
     ;; Save value to local for return
     `((:local.tee ,result-local)
       ;; Set symbol's value field
       (:struct.set ,symbol-type 1)
       ;; Return the value
       (:local.get ,result-local)))))

;;; ============================================================
;;; Function Definition (T059)
;;; ============================================================

(defun compile-defun (ast env)
  "Compile a function definition.
   Returns info about the compiled function.
   Sets tail position for last form in body to enable TCO.

   Feature 043-self-hosting-blockers: Extended to support &optional and &key
   parameters with default values. Uses parse-lambda-list to extract
   structured parameter information and generates preamble code for defaults."
  (let* ((name (clysm/compiler/ast:ast-defun-name ast))
         (raw-params (clysm/compiler/ast:ast-defun-parameters ast))
         (raw-body (clysm/compiler/ast:ast-defun-body ast))
         ;; Feature 043: Filter out declare forms from body
         ;; Declares are not compilable - they're just hints to the host
         (body (clysm/compiler/ast:filter-declare-forms raw-body))
         (func-env (make-env))
         ;; Parse the lambda list to get structured parameter info
         (parsed-ll (clysm/compiler/ast:parse-lambda-list raw-params)))
    ;; Extract all parameter names from parsed lambda list
    (let* ((required (clysm/compiler/ast:ast-parsed-lambda-list-required parsed-ll))
           (optional (clysm/compiler/ast:ast-parsed-lambda-list-optional parsed-ll))
           (rest-param (clysm/compiler/ast:ast-parsed-lambda-list-rest parsed-ll))
           (keys (clysm/compiler/ast:ast-parsed-lambda-list-keys parsed-ll))
           (aux (clysm/compiler/ast:ast-parsed-lambda-list-aux parsed-ll))
           (all-params nil))
      ;; Collect all parameter names for function signature
      (dolist (p required)
        (push (clysm/compiler/ast:ast-param-info-name p) all-params))
      (dolist (p optional)
        (push (clysm/compiler/ast:ast-param-info-name p) all-params)
        ;; Add supplied-p variable if present
        (when (clysm/compiler/ast:ast-param-info-supplied-p p)
          (push (clysm/compiler/ast:ast-param-info-supplied-p p) all-params)))
      (when rest-param
        (push (clysm/compiler/ast:ast-param-info-name rest-param) all-params))
      (dolist (p keys)
        (push (clysm/compiler/ast:ast-param-info-name p) all-params)
        (when (clysm/compiler/ast:ast-param-info-supplied-p p)
          (push (clysm/compiler/ast:ast-param-info-supplied-p p) all-params)))
      (dolist (p aux)
        (push (clysm/compiler/ast:ast-param-info-name p) all-params))
      (setf all-params (nreverse all-params))

      ;; Determine the function parameters (what Wasm function receives)
      ;; For now: required params + optional params + rest + key params
      ;; (We use positional passing for simplicity in bootstrap)
      (let ((wasm-params nil))
        ;; Required params are always Wasm parameters
        (dolist (p required)
          (push (clysm/compiler/ast:ast-param-info-name p) wasm-params))
        ;; Optional params are Wasm parameters (caller passes NIL if not provided)
        (dolist (p optional)
          (push (clysm/compiler/ast:ast-param-info-name p) wasm-params))
        ;; Rest is a Wasm parameter (list of remaining args)
        (when rest-param
          (push (clysm/compiler/ast:ast-param-info-name rest-param) wasm-params))
        ;; Key params are Wasm parameters
        (dolist (p keys)
          (push (clysm/compiler/ast:ast-param-info-name p) wasm-params))
        (setf wasm-params (nreverse wasm-params))

        ;; Add parameters as locals
        (dolist (param wasm-params)
          (env-add-local func-env param))

        ;; Add supplied-p and aux variables as locals (not function params)
        (dolist (p optional)
          (when (clysm/compiler/ast:ast-param-info-supplied-p p)
            (env-add-local func-env (clysm/compiler/ast:ast-param-info-supplied-p p))))
        (dolist (p keys)
          (when (clysm/compiler/ast:ast-param-info-supplied-p p)
            (env-add-local func-env (clysm/compiler/ast:ast-param-info-supplied-p p))))
        (dolist (p aux)
          (env-add-local func-env (clysm/compiler/ast:ast-param-info-name p)))

        ;; Inherit function definitions
        (setf (cenv-functions func-env) (cenv-functions env))
        (setf (cenv-function-counter func-env) (cenv-function-counter env))

        ;; Generate preamble for default values
        (let ((preamble-instrs (compile-default-values-preamble
                                optional keys aux func-env))
              (body-instrs '())
              (non-tail-env (env-with-non-tail func-env))
              (tail-env (env-with-tail func-env)))
          ;; Compile body (non-final forms are NOT in tail position)
          (dolist (form (butlast body))
            (setf body-instrs (append body-instrs
                                      (compile-to-instructions form non-tail-env)
                                      '(:drop))))
          ;; Last form IS in tail position for TCO
          (when body
            (setf body-instrs (append body-instrs
                                      (compile-to-instructions (car (last body)) tail-env))))
          ;; Return function info
          (list :name name
                :params (mapcar (lambda (p) (list p :anyref)) wasm-params)
                :result :anyref
                :locals (loop for i from (length wasm-params) below (cenv-local-counter func-env)
                              collect (list (gensym "local") (env-local-type func-env i)))
                :body (append preamble-instrs body-instrs)))))))

(defun compile-default-values-preamble (optional keys aux func-env)
  "Generate Wasm instructions to apply default values for &optional, &key, &aux.
   Feature: 043-self-hosting-blockers

   For each optional/key parameter with a default:
   - Check if the parameter is NIL or UNBOUND
   - If so, evaluate the default form and store result
   - Set supplied-p to NIL if default used, T if value provided

   For &aux parameters:
   - Always evaluate the init form (or NIL if none)"
  (let ((instrs '()))
    ;; Handle &optional parameters
    (dolist (p optional)
      (let ((name (clysm/compiler/ast:ast-param-info-name p))
            (default-form (clysm/compiler/ast:ast-param-info-default-form p))
            (supplied-p (clysm/compiler/ast:ast-param-info-supplied-p p)))
        (when default-form
          ;; Check if param is NIL (our sentinel for "not provided")
          ;; if (param == nil) { param = default; supplied_p = nil; } else { supplied_p = t; }
          (let ((param-idx (env-lookup-local func-env name))
                (supplied-p-idx (when supplied-p (env-lookup-local func-env supplied-p))))
            ;; Get param value
            (push (list :local.get param-idx) instrs)
            ;; Check if null (nil)
            (push :ref.is_null instrs)
            ;; If null, apply default
            (push '(:if (:result :anyref)) instrs)
            ;; Then: evaluate default and store
            (let ((default-instrs (compile-to-instructions
                                   (clysm/compiler/ast:parse-expr default-form)
                                   (env-with-non-tail func-env))))
              (dolist (instr default-instrs)
                (push instr instrs)))
            (push (list :local.set param-idx) instrs)
            ;; Get new value for the block result
            (push (list :local.get param-idx) instrs)
            ;; Set supplied-p to NIL if we used default
            (when supplied-p-idx
              (push '(:ref.null :none) instrs)
              (push (list :local.set supplied-p-idx) instrs))
            ;; Else: keep provided value, set supplied-p to T
            (push :else instrs)
            (push (list :local.get param-idx) instrs)
            (when supplied-p-idx
              ;; T is represented as i31ref 1
              (push '(:i32.const 1) instrs)
              (push :ref.i31 instrs)
              (push (list :local.set supplied-p-idx) instrs))
            (push :end instrs)
            ;; Drop the block result (we only care about side effects)
            (push :drop instrs)))))

    ;; Handle &key parameters (similar to optional)
    (dolist (p keys)
      (let ((name (clysm/compiler/ast:ast-param-info-name p))
            (default-form (clysm/compiler/ast:ast-param-info-default-form p))
            (supplied-p (clysm/compiler/ast:ast-param-info-supplied-p p)))
        (when default-form
          (let ((param-idx (env-lookup-local func-env name))
                (supplied-p-idx (when supplied-p (env-lookup-local func-env supplied-p))))
            (push (list :local.get param-idx) instrs)
            (push :ref.is_null instrs)
            (push '(:if (:result :anyref)) instrs)
            (let ((default-instrs (compile-to-instructions
                                   (clysm/compiler/ast:parse-expr default-form)
                                   (env-with-non-tail func-env))))
              (dolist (instr default-instrs)
                (push instr instrs)))
            (push (list :local.set param-idx) instrs)
            (push (list :local.get param-idx) instrs)
            (when supplied-p-idx
              (push '(:ref.null :none) instrs)
              (push (list :local.set supplied-p-idx) instrs))
            (push :else instrs)
            (push (list :local.get param-idx) instrs)
            (when supplied-p-idx
              (push '(:i32.const 1) instrs)
              (push :ref.i31 instrs)
              (push (list :local.set supplied-p-idx) instrs))
            (push :end instrs)
            (push :drop instrs)))))

    ;; Handle &aux parameters
    (dolist (p aux)
      (let ((name (clysm/compiler/ast:ast-param-info-name p))
            (init-form (clysm/compiler/ast:ast-param-info-default-form p)))
        (let ((local-idx (env-lookup-local func-env name)))
          (if init-form
              ;; Evaluate init form and store
              (let ((init-instrs (compile-to-instructions
                                  (clysm/compiler/ast:parse-expr init-form)
                                  (env-with-non-tail func-env))))
                (dolist (instr init-instrs)
                  (push instr instrs)))
              ;; No init form - initialize to NIL
              (push '(:ref.null :none) instrs))
          (push (list :local.set local-idx) instrs))))

    ;; Return instructions in correct order
    (nreverse instrs)))

;;; ============================================================
;;; Lambda Compilation (T081-T084)
;;; ============================================================

(defparameter *pending-lambdas* nil
  "List of lambda functions waiting to be compiled.
   Each entry is a plist: (:name :params :body :free-vars :func-index)")

(defparameter *lambda-counter* 0
  "Counter for generating unique lambda function names.")

(defun reset-lambda-state ()
  "Reset lambda compilation state."
  (setf *pending-lambdas* nil)
  (setf *lambda-counter* 0))

(defun allocate-lambda-function ()
  "Allocate a new lambda function index and return its name."
  (let ((name (intern (format nil "$LAMBDA-~D" (incf *lambda-counter*)))))
    name))

(defun compile-lambda (ast env)
  "Compile a lambda expression to create a closure struct.
   Returns instructions that push a closure reference onto the stack."
  (let* ((params (clysm/compiler/ast:ast-lambda-parameters ast))
         (raw-body (clysm/compiler/ast:ast-lambda-body ast))
         ;; Feature 043: Filter out declare forms from lambda body
         (body (clysm/compiler/ast:filter-declare-forms raw-body))
         (free-vars (clysm/compiler/analyzer/free-vars:collect-free-variables ast))
         (lambda-name (allocate-lambda-function))
         (func-index (env-add-function env lambda-name))
         (arity (length params)))
    ;; Register this lambda for later compilation
    (push (list :name lambda-name
                :params params
                :body body
                :free-vars free-vars
                :func-index func-index
                :parent-env env
                :arity arity)
          *pending-lambdas*)
    ;; Generate code to create closure struct with captured environment
    (generate-closure-creation func-index arity free-vars env)))

;;; Feature 043: FUNCTION special form (#'fn)
(defun compile-function (ast env)
  "Compile (function name) or (function (lambda ...)).
   Returns a closure reference to the named or anonymous function."
  (let ((name (clysm/compiler/ast:ast-function-name ast)))
    (cond
      ;; (function (lambda ...)) - compile the lambda
      ((typep name 'clysm/compiler/ast:ast-lambda)
       (compile-lambda name env))
      ;; (function name) - look up named function
      ((symbolp name)
       (let ((local-func (env-lookup-local-function env name)))
         (if local-func
             ;; Local function from flet/labels - return its closure
             (if (eq local-func :captured)
                 ;; Captured function - look up in captured-vars
                 (let ((pos (cdr (assoc name (cenv-captured-vars env)))))
                   (if pos
                       ;; Extract from closure's env cons-list
                       (extract-captured-function pos env)
                       (error "FUNCTION: Cannot find captured function ~A" name)))
                 ;; Direct local - load from local variable
                 (list (list :local.get local-func)))
             ;; Global function - create closure wrapper
             (let ((func-index (or (env-lookup-function env name)
                                   (env-add-function env name))))
               ;; Create a closure with the function reference
               ;; Use arity 0 as default (variadic)
               (generate-closure-creation func-index 0 nil env)))))
      (t
       (error "FUNCTION: Invalid name ~S" name)))))

(defun extract-captured-function (position env)
  "Extract captured function closure from closure's env cons-list at POSITION."
  ;; Load closure (first local), get env field, walk cons-list to position
  (let ((cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (append
     '((:local.get 0))  ; Get closure
     `((:struct.get ,clysm/compiler/codegen/gc-types:+type-closure+ 4))  ; Get env field
     ;; Walk the cons-list to the right position
     (loop repeat position
           append `((:ref.cast (ref ,cons-type))
                    (:struct.get ,cons-type 1)))  ; Get cdr
     ;; Get car of the final cons (the captured closure)
     `((:ref.cast (ref ,cons-type))
       (:struct.get ,cons-type 0)))))

(defun generate-closure-creation (func-index arity free-vars env)
  "Generate instructions to create a closure struct.
   The closure has code_0, code_1, code_2, code_N, and env fields.
   Free variables are captured in a cons-list stored in the env field."
  ;; struct.new $closure (code_0, code_1, code_2, code_N, env)
  (let ((closure-type clysm/compiler/codegen/gc-types:+type-closure+))
    (with-instruction-collector
      ;; code_0 - null if arity != 0, ref.func if arity == 0
      (if (= arity 0)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      ;; code_1 - null if arity != 1
      (if (= arity 1)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      ;; code_2 - null if arity != 2
      (if (= arity 2)
          (emit :ref.func func-index)
          (emit :ref.null :func))
      ;; code_N - always the fallback
      (emit :ref.func func-index)
      ;; env - capture free variables as a cons-list
      (if free-vars
          ;; Build cons-list: (cons var1 (cons var2 (cons var3 nil)))
          (emit* (generate-env-capture free-vars env))
          ;; No free vars, use null
          (emit :ref.null :any))
      ;; Create the struct
      (emit :struct.new closure-type))))

(defun generate-env-capture (free-vars env)
  "Generate instructions to capture free variables as a cons-list.
   Returns instructions that push a cons-list onto the stack.
   Each captured variable becomes (cons value rest)."
  (if (null free-vars)
      ;; Base case: nil (null reference)
      '((:ref.null :none))
      ;; Build cons from end: (cons first-var (cons second-var ...))
      (let ((var (first free-vars))
            (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
        ;; Push the variable value
        (let ((local-idx (env-lookup-local env var)))
          (unless local-idx
            (error "Cannot capture unbound variable: ~A" var))
          (with-instruction-collector
            (emit :local.get local-idx)
            ;; Push rest of the list (recursive)
            (emit* (generate-env-capture (rest free-vars) env))
            ;; Create cons cell: struct.new $cons (car, cdr)
            (emit :struct.new cons-type))))))

(defun compile-pending-lambdas ()
  "Compile all pending lambda functions and return their definitions.
   Handles nested lambdas by repeatedly compiling until no more are pending."
  (let ((results '())
        (compiled (make-hash-table :test 'equal)))  ; Track compiled lambda names
    (loop while *pending-lambdas*
          do (let ((batch (reverse *pending-lambdas*)))
               (setf *pending-lambdas* nil)  ; Clear before compiling (new ones may be added)
               (dolist (lambda-info batch)
                 (let ((name (getf lambda-info :name)))
                   (unless (gethash name compiled)
                     (setf (gethash name compiled) t)
                     (let ((def (compile-lambda-body lambda-info)))
                       (push def results)))))))
    (nreverse results)))

(defun compile-lambda-body (lambda-info)
  "Compile a lambda function body."
  (let* ((name (getf lambda-info :name))
         (params (getf lambda-info :params))
         (raw-body (getf lambda-info :body))
         ;; Feature 043: Filter out declare forms from lambda body
         (body (clysm/compiler/ast:filter-declare-forms raw-body))
         (free-vars (getf lambda-info :free-vars))
         (func-names (getf lambda-info :func-names))  ; For labels lambdas
         (is-labels-lambda (getf lambda-info :is-labels-lambda))
         (parent-env (getf lambda-info :parent-env))
         (func-env (make-env)))
    ;; First parameter is the closure itself (for accessing env)
    (env-add-local func-env '$closure)
    ;; Add regular parameters
    (dolist (param params)
      (env-add-local func-env param))
    ;; Register captured variables - they will be accessed via env extraction
    ;; Store the mapping of captured var name -> position in cons-list
    ;; For labels lambdas, func-names come after free-vars
    (let ((position 0))
      (setf (cenv-captured-vars func-env)
            (append
             ;; Regular free variables first
             (loop for var in free-vars
                   collect (prog1 (cons var position) (incf position)))
             ;; Then captured function closures (for labels)
             (loop for fname in (or func-names '())
                   collect (prog1 (cons fname position) (incf position))))))
    ;; For labels lambdas, also set up local-functions so direct calls work
    (when is-labels-lambda
      ;; The captured functions can be accessed via captured-vars mechanism
      ;; but we also need to register them as local-functions for compile-call
      ;; Actually, they're in captured-vars, so compile-var-ref will find them
      ;; But compile-call needs them as local-functions for the call mechanism
      ;; Let's register them - they'll be extracted from captured vars on each call
      (setf (cenv-local-functions func-env)
            (loop for fname in (or func-names '())
                  for i from (length free-vars)
                  collect (cons fname :captured))))  ; Mark as captured, not local
    ;; Inherit function definitions
    (setf (cenv-functions func-env) (cenv-functions parent-env))
    (setf (cenv-function-counter func-env) (cenv-function-counter parent-env))
    ;; Compile body with tail position for last form (TCO)
    (let ((body-instrs '())
          (non-tail-env (env-with-non-tail func-env))
          (tail-env (env-with-tail func-env)))
      (dolist (form (butlast body))
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions form non-tail-env)
                                  '(:drop))))
      ;; Last form IS in tail position for TCO
      (when body
        (setf body-instrs (append body-instrs
                                  (compile-to-instructions (car (last body)) tail-env))))
      ;; Return function info (with closure param first)
      (list :name name
            :params (cons '($closure :anyref)
                          (mapcar (lambda (p) (list p :anyref)) params))
            :result :anyref
            :locals (loop for i from (1+ (length params)) below (cenv-local-counter func-env)
                          collect (list (gensym "local") (env-local-type func-env i)))
            :body body-instrs))))

;;; ============================================================
;;; Block/Return-from Compilation (T107-T109)
;;; ============================================================

(defun compile-block (ast env)
  "Compile a block form.
   Uses Wasm block instruction for normal flow, with br for return-from.
   Pattern: (block :anyref body... end)"
  (let* ((name (clysm/compiler/ast:ast-block-name ast))
         (body (clysm/compiler/ast:ast-block-body ast))
         ;; Create new env with block registered
         (block-env (copy-compilation-env env)))
    ;; Register block at current depth (0 means we can br 0 to exit)
    (push (cons name 0) (cenv-blocks block-env))
    ;; Increment depth for any nested blocks
    (incf (cenv-block-depth block-env))
    (with-instruction-collector
      ;; Start block with anyref result type
      (emit '(:block (:result :anyref)))
      ;; Compile body forms (all but last are dropped)
      (dolist (form (butlast body))
        (emit* (compile-to-instructions form block-env))
        (emit :drop))
      ;; Last form (or nil if empty)
      (if body
          (emit* (compile-to-instructions (car (last body)) block-env))
          (emit :ref.null :none))
      ;; End block
      (emit :end))))

(defun compile-return-from (ast env)
  "Compile return-from.
   Uses br to exit to the named block."
  (let* ((block-name (clysm/compiler/ast:ast-return-from-block-name ast))
         (value (clysm/compiler/ast:ast-return-from-value ast))
         (block-info (assoc block-name (cenv-blocks env))))
    (unless block-info
      (error "return-from: no block named ~A" block-name))
    (let* ((target-depth (cdr block-info))
           ;; Calculate relative br depth
           ;; If we're at depth 2 and block is at depth 0, br 2
           (br-depth (- (cenv-block-depth env) target-depth 1)))
      (with-instruction-collector
        ;; Compile value
        (emit* (compile-to-instructions value env))
        ;; Branch to block exit
        (emit :br br-depth)))))

;;; ============================================================
;;; Tagbody/Go Strategy Analysis (T008-T010)
;;; ============================================================

(defun collect-go-targets-from-form (form)
  "Collect all go target tags from a single AST form.
   Recursively walks into nested forms (if, progn, etc.)."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (list (clysm/compiler/ast:ast-go-tag form)))
    (clysm/compiler/ast:ast-if
     (append (collect-go-targets-from-form (clysm/compiler/ast:ast-if-test form))
             (collect-go-targets-from-form (clysm/compiler/ast:ast-if-then form))
             (when (clysm/compiler/ast:ast-if-else form)
               (collect-go-targets-from-form (clysm/compiler/ast:ast-if-else form)))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           append (collect-go-targets-from-form subform)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           append (collect-go-targets-from-form subform)))
    ;; For other forms, no go targets inside
    (t nil)))

(defun collect-go-targets (segments)
  "Collect all go target tags from all segments.
   Returns a list of tag symbols that are targets of go statements."
  (loop for segment in segments
        append (loop for form in (cdr segment)
                     append (collect-go-targets-from-form form))))

(defun find-tag-segment-index (segments tag)
  "Find the segment index where TAG is defined.
   Returns nil if tag not found."
  (loop for segment in segments
        for idx from 0
        when (eq (car segment) tag)
        return idx))

(defun check-go-is-backward (segments go-form segment-idx form-position)
  "Check if a go from the given position is a backward jump.
   A backward jump targets a tag at or before the current position."
  (let* ((target-tag (clysm/compiler/ast:ast-go-tag go-form))
         (target-idx (find-tag-segment-index segments target-tag)))
    (when target-idx
      ;; Backward if target segment index is less than current segment index
      ;; OR if target is same segment but we're after the tag definition
      (or (< target-idx segment-idx)
          (and (= target-idx segment-idx) (> form-position 0))))))

(defun check-form-goes-backward (segments form segment-idx form-position tag)
  "Check if all goes to TAG in this form are backward jumps."
  (etypecase form
    (clysm/compiler/ast:ast-go
     (if (eq (clysm/compiler/ast:ast-go-tag form) tag)
         (check-go-is-backward segments form segment-idx form-position)
         t)) ; Not targeting our tag, doesn't affect result
    (clysm/compiler/ast:ast-if
     (and (check-form-goes-backward segments (clysm/compiler/ast:ast-if-test form)
                                    segment-idx form-position tag)
          (check-form-goes-backward segments (clysm/compiler/ast:ast-if-then form)
                                    segment-idx form-position tag)
          (or (null (clysm/compiler/ast:ast-if-else form))
              (check-form-goes-backward segments (clysm/compiler/ast:ast-if-else form)
                                        segment-idx form-position tag))))
    (clysm/compiler/ast:ast-progn
     (loop for subform in (clysm/compiler/ast:ast-progn-forms form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (clysm/compiler/ast:ast-let
     (loop for subform in (clysm/compiler/ast:ast-let-body form)
           always (check-form-goes-backward segments subform segment-idx form-position tag)))
    (t t))) ; Other forms don't contain go

(defun all-goes-are-backward-p (segments tag)
  "Check if all go statements targeting TAG are backward jumps.
   Returns T if all goes to TAG are backward, NIL otherwise."
  (loop for segment in segments
        for seg-idx from 0
        always (loop for form in (cdr segment)
                     for form-pos from 0
                     always (check-form-goes-backward segments form seg-idx form-pos tag))))

(defun analyze-tagbody-strategy (segments)
  "Analyze tagbody segments and determine the compilation strategy.
   Returns one of:
   - :sequential - no go statements, just sequential execution
   - :simple-loop - single tag with backward jumps only (Wasm loop)
   - :dispatch - complex patterns requiring br_table dispatch"
  (let* ((tags (remove nil (mapcar #'car segments)))
         (go-targets (remove-duplicates (collect-go-targets segments))))
    (cond
      ;; No go statements -> sequential
      ((null go-targets) :sequential)
      ;; Single tag, single target, all backward -> simple loop
      ((and (= (length tags) 1)
            (= (length go-targets) 1)
            (eq (first tags) (first go-targets))
            (all-goes-are-backward-p segments (first tags)))
       :simple-loop)
      ;; Everything else -> dispatch
      (t :dispatch))))

;;; ============================================================
;;; Tagbody/Go Compilation (T110-T112)
;;; ============================================================

(defun compile-tagbody-sequential (segments env)
  "Compile tagbody with sequential strategy (no go statements).
   Simply compiles all forms in order and returns NIL."
  (with-instruction-collector
    (dolist (segment segments)
      (let ((forms (cdr segment)))
        (dolist (form forms)
          (emit* (compile-to-instructions form env))
          (emit :drop))))
    ;; Return NIL
    (emit :ref.null :none)))

(defun compile-tagbody-simple-loop (segments env context)
  "Compile tagbody with simple-loop strategy.
   Used when there's a single tag with only backward jumps.
   Generates: (loop body... (br N for go where N accounts for nesting) ... end) NIL

   Structure:
   - loop (br 0 continues loop from direct context)
     - forms...
     - go -> br (current-depth - base-depth) to reach loop
   - end
   - ref.null none"
  (let ((loop-env (copy-compilation-env env)))
    ;; Increment block depth for the loop
    (incf (cenv-block-depth loop-env))
    ;; Set up context for compile-go to find
    ;; Record the block depth at which the loop starts
    (setf (tagbody-context-base-block-depth context) (cenv-block-depth loop-env))
    (setf (tagbody-context-base-loop-depth context) 0)  ; br to loop is relative
    (setf (cenv-tagbody-context loop-env) context)
    (with-instruction-collector
      ;; Start loop with no result type (void loop)
      (emit '(:loop nil))
      ;; Compile preamble (forms before first tag)
      (let ((first-segment (first segments)))
        (when (null (car first-segment))
          (dolist (form (cdr first-segment))
            (emit* (compile-to-instructions form loop-env))
            (emit :drop))))
      ;; Compile the main tagged segment
      (dolist (segment segments)
        (when (car segment) ; Skip preamble, compile tagged segment
          (dolist (form (cdr segment))
            (emit* (compile-to-instructions form loop-env))
            (emit :drop))))
      ;; End loop
      (emit :end)
      ;; Return NIL
      (emit :ref.null :none))))

(defun compile-tagbody-dispatch (segments env context)
  "Compile tagbody with dispatch strategy.
   Used for complex jump patterns (forward jumps, multiple tags).

   Structure (for N segments):
   block (exit block, depth N+1 from innermost)
     loop (dispatch loop, depth N from innermost)
       block (segment N-1, depth N-1 from innermost)
         block (segment N-2, depth N-2)
           ...
           block (segment 0, depth 0)
             br_table [N N-1 ... 1] 0 $pc
           end
           ;; segment 0 code here
           ;; go -> set $pc, br (depth to loop = N - seg_idx)
         end
         ;; segment 1 code here
       end
       ;; segment N-1 code here
       br (depth to exit = 1)
     end
   end"
  (let* ((num-segments (length segments))
         (pc-local (tagbody-context-pc-local context))
         (dispatch-env (copy-compilation-env env)))
    ;; Track block depth: exit block + loop + num-segments blocks
    (incf (cenv-block-depth dispatch-env) 2)  ; exit block + loop
    ;; Set up context for compile-go
    (setf (cenv-tagbody-context dispatch-env) context)
    (with-instruction-collector
      ;; Initialize $pc to 0 (stored as i31ref since all locals are anyref)
      (emit :i32.const 0)
      (emit :ref.i31)
      (emit :local.set pc-local)
      ;; Start exit block (result anyref for NIL)
      (emit '(:block (:result :anyref)))
      ;; Start dispatch loop with anyref result type
      (emit '(:loop (:result :anyref)))
      ;; Generate nested blocks (innermost = segment 0)
      (dotimes (i num-segments)
        (emit '(:block nil)))
      ;; Generate br_table
      (let ((br-targets (loop for i from 0 below num-segments collect i)))
        ;; Extract $pc from i31ref to i32 for br_table
        (emit :local.get pc-local)
        (emit '(:ref.cast :i31))
        (emit :i31.get_s)
        ;; Default target is num-segments
        (emit* (list (cons :br_table (append br-targets (list num-segments))))))
      ;; Close blocks and generate segment code
      (loop for seg-idx from 0 below num-segments
            for segment in segments
            do (let ((segment-env (copy-compilation-env dispatch-env)))
                 ;; Close the block for this segment FIRST
                 (emit :end)
                 ;; Now segment code runs here
                 (let ((remaining-blocks (- num-segments seg-idx 1)))
                   (setf (cenv-block-depth segment-env)
                         (+ (cenv-block-depth env) 2 remaining-blocks))
                   (setf (tagbody-context-base-block-depth (cenv-tagbody-context segment-env))
                         (cenv-block-depth segment-env))
                   (setf (tagbody-context-base-loop-depth (cenv-tagbody-context segment-env))
                         remaining-blocks))
                 ;; Compile segment forms
                 (dolist (form (cdr segment))
                   (emit* (compile-to-instructions form segment-env))
                   (emit :drop))))
      ;; After all segments, exit the tagbody with NIL
      (emit :ref.null :none)
      (emit :br 1)
      ;; Close dispatch loop
      (emit :end)
      ;; Close exit block
      (emit :end))))

(defun compile-tagbody (ast env)
  "Compile tagbody using appropriate strategy based on analysis.
   Strategies:
   - :sequential - no go, just sequential execution
   - :simple-loop - single tag with backward jumps only
   - :dispatch - complex patterns with br_table"
  (let* ((segments (clysm/compiler/ast:ast-tagbody-segments ast))
         (strategy (analyze-tagbody-strategy segments))
         (tag-env (copy-compilation-env env)))
    ;; Register tags in legacy format for backward compatibility
    (loop for segment in segments
          for seg-idx from 0
          do (let ((tag (car segment)))
               (when tag
                 (push (cons tag seg-idx) (cenv-tagbody-tags tag-env)))))
    (case strategy
      (:sequential
       (compile-tagbody-sequential segments tag-env))
      (:simple-loop
       (let* ((context (make-tagbody-context
                        :strategy :simple-loop
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :base-loop-depth 0  ; br 0 targets the loop
                        :num-segments (length segments))))
         (compile-tagbody-simple-loop segments tag-env context)))
      (:dispatch
       (let* ((pc-local (env-add-local tag-env (gensym "pc")))
              (context (make-tagbody-context
                        :strategy :dispatch
                        :tags (loop for segment in segments
                                    for idx from 0
                                    when (car segment)
                                    collect (cons (car segment) idx))
                        :pc-local pc-local
                        :num-segments (length segments))))
         (compile-tagbody-dispatch segments tag-env context))))))

(defun compile-go (ast env)
  "Compile go statement.
   Uses the tagbody-context from environment to determine how to jump.
   Calculates correct br depth based on current nesting within the tagbody."
  (let* ((tag (clysm/compiler/ast:ast-go-tag ast))
         (context (cenv-tagbody-context env)))
    (unless context
      (error "go outside tagbody: ~A" tag))
    (let ((tag-info (assoc tag (tagbody-context-tags context))))
      (unless tag-info
        (error "undefined tag: ~A" tag))
      (let ((segment-idx (cdr tag-info))
            (strategy (tagbody-context-strategy context))
            ;; Calculate how many blocks we're nested inside relative to the loop
            (nesting-depth (- (cenv-block-depth env)
                              (tagbody-context-base-block-depth context))))
        (case strategy
          (:simple-loop
           ;; For simple loop, branch back to the loop start
           ;; br depth = nesting depth (how many blocks to exit to reach loop)
           (list (list :br nesting-depth)))
          (:dispatch
           ;; For dispatch, set $pc (as i31ref) and branch to dispatch loop
           ;; br depth = nesting depth + base loop depth
           (let ((pc-local (tagbody-context-pc-local context))
                 (base-depth (tagbody-context-base-loop-depth context)))
             (list (list :i32.const segment-idx)
                   :ref.i31  ; wrap as i31ref to match local type
                   (list :local.set pc-local)
                   (list :br (+ nesting-depth base-depth)))))
          (t
           (error "go in sequential tagbody should not be compiled: ~A" tag)))))))

;;; ============================================================
;;; Catch/Throw Compilation (T113-T115)
;;; ============================================================

(defun compile-catch (ast env)
  "Compile catch form using Wasm exception handling.
   Delegates to compile-catch-simple for the actual implementation.
   Uses try_table with catch clause to get exception values directly."
  (compile-catch-simple ast env))

(defun compile-catch-simple (ast env)
  "Compile catch form using Wasm exception handling.

   The key challenge is that when a catch expression is used as a sub-expression
   inside another catch's body, and it catches an exception and returns a value,
   that value should be the result of the catch expression - sibling code should
   not continue.

   Structure with if/else directly producing the result (no br):

   block $catch (type 15)               ;; () -> (anyref anyref)
     try_table (result anyref) (catch 0 $catch)
       ...body...
     end
     ;; Normal path
     local.set $body_result
     ref.null none                      ;; dummy thrown-tag
     ref.null none                      ;; dummy thrown-value
     br $catch
   end  ; $catch - (anyref anyref) on stack

   ;; Handler code - both paths use if with (result anyref)
   local.set $thrown_value
   local.set $thrown_tag
   local.get $thrown_tag
   ref.is_null
   if (result anyref)                   ;; if normal (null tag)
     local.get $body_result
   else                                 ;; exception path
     <tag comparison>
     if (result anyref)                 ;; if tags match
       local.get $thrown_value
     else                               ;; tags don't match - rethrow
       local.get $thrown_tag
       local.get $thrown_value
       throw 0
     end
   end
   ;; Result anyref is now on stack - this IS the catch expression's value"
  (let* ((tag (clysm/compiler/ast:ast-catch-tag ast))
         (body (clysm/compiler/ast:ast-catch-body ast))
         (tag-local (env-add-local env (gensym "catch-tag")))
         (thrown-tag-local (env-add-local env (gensym "thrown-tag")))
         (thrown-value-local (env-add-local env (gensym "thrown-value")))
         (normal-result-local (env-add-local env (gensym "normal-result"))))
    (with-instruction-collector
      ;; Evaluate and store the catch tag
      (emit* (compile-to-instructions tag env))
      (emit :local.set tag-local)

      ;; Create catch environment
      (let ((catch-env (copy-compilation-env env)))
        (push (cons tag-local 0) (cenv-catch-tags catch-env))

        ;; Block $catch - type 30: () -> (anyref anyref)
        (emit '(:block (:type 30)))  ;; $catch

        ;; try_table inside $catch
        (emit '(:try_table (:result :anyref) (:catch 0 0)))

        ;; Compile body
        (dolist (form (butlast body))
          (emit* (compile-to-instructions form catch-env))
          (emit :drop))
        (when body
          (emit* (compile-to-instructions (car (last body)) catch-env)))
        (unless body
          (emit :ref.null :none))

        (emit :end)  ;; end try_table

        ;; Normal path: store result, push (nil nil), branch to $catch
        (emit :local.set normal-result-local)
        (emit :ref.null :none)
        (emit :ref.null :none)
        (emit :br 0)  ;; br to $catch with (nil nil)

        (emit :end)  ;; end $catch block

        ;; At this point: (anyref anyref) on stack
        (emit :local.set thrown-value-local)
        (emit :local.set thrown-tag-local)

        ;; Check if this was a real exception (thrown-tag is not null)
        (emit :local.get thrown-tag-local)
        (emit :ref.is_null)
        (emit '(:if (:result :anyref)))

        ;; Normal case (null tag): return normal result
        (emit :local.get normal-result-local)

        (emit :else)

        ;; Exception case: compare tags and handle
        (emit :local.get tag-local)
        (emit '(:ref.cast :eq))
        (emit :local.get thrown-tag-local)
        (emit '(:ref.cast :eq))
        (emit :ref.eq)
        (emit '(:if (:result :anyref)))

        ;; Tags match: return thrown-value
        (emit :local.get thrown-value-local)

        (emit :else)
        ;; Tags don't match: rethrow
        (emit :local.get thrown-tag-local)
        (emit :local.get thrown-value-local)
        (emit :throw 0)
        (emit :end)  ;; end inner if

        (emit :end)))))

(defun compile-throw (ast env)
  "Compile throw using Wasm throw instruction.
   Throws exception with tag index 0 ($lisp-throw) with (tag-symbol, value) payload."
  (let* ((tag (clysm/compiler/ast:ast-throw-tag ast))
         (value (clysm/compiler/ast:ast-throw-value ast)))
    (with-instruction-collector
      ;; Compile tag expression (evaluates to symbol)
      (emit* (compile-to-instructions tag env))
      ;; Compile value expression
      (emit* (compile-to-instructions value env))
      ;; Throw with tag 0 ($lisp-throw)
      (emit :throw 0))))

;;; ============================================================
;;; Unwind-Protect Compilation (T116-T119)
;;; ============================================================

(defun compile-unwind-protect (ast env)
  "Compile unwind-protect with exception handling.
   Pattern:
   - Normal exit: execute protected form, save result, run cleanup, return result
   - Exception exit: catch any exception, run cleanup, rethrow

   Wasm structure:
   block (result anyref)                    ;; final result
     block (result exnref)                  ;; exception if caught
       try_table (result anyref) (catch_all_ref 0)
         ... protected form ...
       end
       local.set $result
       ... cleanup forms (drop values) ...
       local.get $result
       br 1                                 ;; branch to outer block
     end
     local.set $exnref
     ... cleanup forms (drop values) ...
     local.get $exnref
     throw_ref
   end"
  (let* ((protected-form (clysm/compiler/ast:ast-unwind-protect-protected-form ast))
         (cleanup-forms (clysm/compiler/ast:ast-unwind-protect-cleanup-forms ast))
         ;; Allocate locals
         (result-local (env-add-local env (gensym "unwind-result")))
         (exnref-local (env-add-local env (gensym "exnref") :exnref))
         ;; Pre-compile cleanup forms (we'll use this twice)
         (cleanup-code (with-instruction-collector
                         (dolist (form cleanup-forms)
                           (emit* (compile-to-instructions form env))
                           (emit :drop)))))
    (with-instruction-collector
      ;; block (result anyref) - outer block for final result
      (emit '(:block (:result :anyref)))
      ;; block (result exnref) - inner block for exception ref
      (emit '(:block (:result :exnref)))
      ;; try_table (result anyref) (catch_all_ref 0)
      (emit (list :try_table '(:result :anyref) (list :catch_all_ref 0)))
      ;; Compile protected form
      (emit* (compile-to-instructions protected-form env))
      ;; end try_table
      (emit :end)
      ;; Normal path: save result, run cleanup, return result, br 1 (to outer block)
      (emit :local.set result-local)
      (emit* cleanup-code)
      (emit :local.get result-local)
      (emit '(:br 1))
      ;; end inner block
      (emit :end)
      ;; Exception path: save exnref, run cleanup, throw_ref
      (emit :local.set exnref-local)
      (emit* cleanup-code)
      (emit :local.get exnref-local)
      (emit '(:throw_ref))
      ;; end outer block
      (emit :end))))

;;; ============================================================
;;; Handler-Case Compilation (001-control-structure-extension US4)
;;; ============================================================

;; HyperSpec: resources/HyperSpec/Body/m_hand_1.htm
(defun compile-handler-case (ast env)
  "Compile handler-case with exception handling.
   Pattern:
   - Execute protected expression in try_table
   - Catch exceptions with tag 0 ($lisp-throw)
   - The caught value is (tag-symbol . condition-object)
   - Dispatch to appropriate handler based on condition type
   - If no handler matches, rethrow

   Wasm structure:
   block (result anyref)                    ;; final result
     block (param anyref anyref) (result anyref)  ;; handler block
       try_table (result anyref) (catch 0 0)
         ... protected expression ...
       end
       br 1                                 ;; normal exit - skip handler
     end
     ;; Stack: (tag, value) from catch
     ;; Ignore tag (second value), work with value (first on stack after swap)
     local.set $condition
     drop                                   ;; drop the tag
     ;; Type dispatch to handlers
     ... handler dispatch code ...
   end"
  (let* ((expression (clysm/compiler/ast:ast-handler-case-expression ast))
         (handlers (clysm/compiler/ast:ast-handler-case-handlers ast)))
    ;; Handle case with no handlers - just evaluate expression
    (when (null handlers)
      (return-from compile-handler-case
        (compile-to-instructions expression env)))

    ;; Allocate locals
    (let ((condition-local (env-add-local env (gensym "hc-condition")))
          (tag-local (env-add-local env (gensym "hc-tag"))))
      (with-instruction-collector
        ;; block (result anyref) - outer block for final result
        (emit '(:block (:result :anyref)))
        ;; block (type 30) - inner block catches (anyref anyref) result from try_table
        (emit '(:block (:type 30)))
        ;; try_table (result anyref) (catch 0 0)
        (emit '(:try_table (:result :anyref) (:catch 0 0)))
        ;; Compile protected expression
        (emit* (compile-to-instructions expression env))
        ;; end try_table
        (emit :end)
        ;; Normal path: result on stack, branch past handlers
        (emit '(:br 1))
        ;; end inner block - stack has (anyref anyref) = (tag, value)
        (emit :end)
        ;; Exception path: save caught values
        (emit :local.set condition-local)
        (emit :local.set tag-local)
        ;; Generate handler dispatch code
        ;; For now, we match any handler (simplified - no type checking yet)
        (let ((first-handler t))
          (dolist (handler handlers)
            (let ((var (clysm/compiler/ast:handler-clause-var handler))
                  (body (clysm/compiler/ast:handler-clause-body handler)))
              (when first-handler
                (setf first-handler nil))
              ;; For simplified implementation: execute first handler that exists
              (if var
                  ;; Bind the condition variable
                  (let* ((handler-env (extend-compilation-env env))
                         (var-local (env-add-local handler-env var)))
                    (emit :local.get condition-local)
                    (emit :local.set var-local)
                    ;; Compile handler body
                    (dolist (form (butlast body))
                      (emit* (compile-to-instructions form handler-env))
                      (emit :drop))
                    (when body
                      (emit* (compile-to-instructions (car (last body)) handler-env)))
                    (unless body
                      (emit '(:ref.null :none))))
                  ;; No variable binding - just execute body
                  (progn
                    (dolist (form (butlast body))
                      (emit* (compile-to-instructions form env))
                      (emit :drop))
                    (when body
                      (emit* (compile-to-instructions (car (last body)) env)))
                    (unless body
                      (emit '(:ref.null :none)))))
              ;; For now, only use first handler and return
              (return))))
        ;; end outer block
        (emit :end)))))

;;; ============================================================
;;; Special Variable Binding Support (T009, T034)
;;; ============================================================

(defun generate-restore-binding-instructions ()
  "Generate instructions for the $restore-binding helper (T009).
   Restores the top binding frame: pops the frame, restores old value.

   Pattern:
   (global.get $binding_stack)    ; Get top frame
   (local.tee $frame)
   (struct.get $binding_frame $old_value)  ; Get old value
   (local.get $frame)
   (struct.get $binding_frame $symbol)     ; Get symbol
   (struct.set $symbol $value)             ; Restore value to symbol
   (local.get $frame)
   (struct.get $binding_frame $prev)       ; Get previous frame
   (global.set $binding_stack)             ; Pop frame"
  (let ((frame-local 0)   ; Temporary local for frame reference
        (binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Return instruction list for restore-binding logic
    `(;; Get and save the current binding frame
      (:global.get $binding_stack)
      (:local.tee ,frame-local)
      ;; Get old value from frame (field 1)
      (:struct.get ,binding-frame-type 1)
      ;; Get symbol from frame (field 0)
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 0)
      ;; Restore value to symbol's $value field (field 1)
      (:struct.set ,symbol-type 1)
      ;; Pop the frame: set binding_stack to frame.prev
      (:local.get ,frame-local)
      (:struct.get ,binding-frame-type 2)  ; Get $prev field
      (:global.set $binding_stack))))

(defun generate-push-binding-frame (symbol-global-name value-instructions)
  "Generate instructions to push a new binding frame (T031).
   Saves the current value and sets a new one.

   Parameters:
   - symbol-global-name: Global variable name for the symbol
   - value-instructions: Instructions that produce the new value

   Pattern:
   1. Create binding frame: (symbol, old_value, prev)
   2. Push to binding stack
   3. Set new value"
  (let ((binding-frame-type clysm/compiler/codegen/gc-types:+type-binding-frame+)
        (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    `(;; Create binding frame with: symbol, old_value, prev
      ;; Field 0: symbol reference
      (:global.get ,symbol-global-name)
      ;; Field 1: old value (current symbol value)
      (:global.get ,symbol-global-name)
      (:struct.get ,symbol-type 1)
      ;; Field 2: prev (current binding stack)
      (:global.get $binding_stack)
      ;; Create the frame struct
      (:struct.new ,binding-frame-type)
      ;; Push to binding stack
      (:global.set $binding_stack)
      ;; Set new value on symbol
      (:global.get ,symbol-global-name)
      ,@value-instructions
      (:struct.set ,symbol-type 1))))

;;; ============================================================
;;; Special Variable Global Tracking (T025)
;;; ============================================================

(defvar *special-var-globals* (make-hash-table :test 'eq)
  "Maps special variable names to their global indices.")

(defvar *next-special-global-index* 4
  "Next available global index for special variables.
   Starts at 4 because 0=NIL, 1=UNBOUND, 2=mv-count, 3=mv-buffer.")

(defun reset-special-var-globals ()
  "Reset special variable global tracking for new compilation."
  (clrhash *special-var-globals*)
  (setf *next-special-global-index* 4))

(defun allocate-special-var-global (name)
  "Allocate a global index for a special variable.
   Returns the index, creating a new one if needed."
  (or (gethash name *special-var-globals*)
      (setf (gethash name *special-var-globals*)
            (prog1 *next-special-global-index*
              (incf *next-special-global-index*)))))

(defun get-special-var-global-index (name)
  "Get the global index for a special variable.
   Returns NIL if not allocated."
  (gethash name *special-var-globals*))

(defun get-all-special-var-globals ()
  "Get all allocated special variable globals as an alist.
   Returns ((name . index) ...) sorted by index."
  (let ((result '()))
    (maphash (lambda (name index)
               (push (cons name index) result))
             *special-var-globals*)
    (sort result #'< :key #'cdr)))

;;; ============================================================
;;; Special Variable Definition Compilation (T022-T025)
;;; ============================================================

(defun make-symbol-global-name (name)
  "Create a global variable name for a symbol.
   Example: *foo* -> $sym_FOO"
  (intern (format nil "$sym_~A" (string-upcase (symbol-name name)))))

(defun compile-defvar (ast env)
  "Compile a defvar form (T022-T023).
   defvar only initializes if the variable is currently unbound.
   Symbols are initialized with null value fields (via struct.new_default),
   so we check for null to determine if uninitialized.

   Generated code pattern:
   1. Get symbol's current value
   2. Check if it's null (uninitialized)
   3. If null, set the initial value
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defvar-name ast))
         (init-form (clysm/compiler/ast:ast-defvar-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    (if init-form
        ;; With init-form: conditionally initialize
        ;; Check if symbol.value is null (uninitialized), if so set value
        (append
         ;; Get symbol's value field
         `((:global.get ,global-idx)
           (:struct.get ,symbol-type 1)  ; Get value field
           ;; Check if it's null (uninitialized)
           :ref.is_null
           ;; If null, initialize the value
           (:if (:result :anyref)))
         ;; Then branch: set new value
         `((:global.get ,global-idx))
         (compile-to-instructions init-form env)
         `((:struct.set ,symbol-type 1)
           ;; Return the symbol
           (:global.get ,global-idx)
           :else
           ;; Else branch: already bound, just return symbol
           (:global.get ,global-idx)
           :end))
        ;; Without init-form: just ensure the symbol exists (no-op at runtime)
        `((:global.get ,global-idx)))))

(defun compile-defparameter (ast env)
  "Compile a defparameter form (T024).
   Unlike defvar, defparameter always initializes the variable.

   Generated code pattern:
   1. Get symbol reference
   2. Compile init form
   3. Set symbol's value field
   4. Return the symbol name"
  (let* ((name (clysm/compiler/ast:ast-defparameter-name ast))
         (init-form (clysm/compiler/ast:ast-defparameter-init-form ast))
         ;; Allocate/get the global index for this symbol
         (global-idx (allocate-special-var-global name))
         (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
    ;; Always set the value (unlike defvar)
    `(;; Set new value
      (:global.get ,global-idx)
      ,@(compile-to-instructions init-form env)
      (:struct.set ,symbol-type 1)
      ;; Return the symbol
      (:global.get ,global-idx))))

;;; ============================================================
;;; Feature 038: Constant Definitions and Constant Folding
;;; ============================================================

(defvar *constant-registry* (make-hash-table :test 'eq)
  "Compile-time registry for constant values.
   Maps constant names to their computed values for constant folding.")

(defun fold-constant-expression (form registry)
  "Attempt to fold FORM into a constant value at compile time.
   Returns (VALUES value foldable-p) where:
   - value: The computed value if foldable, NIL otherwise
   - foldable-p: T if the expression was successfully folded

   Supports:
   - Literals: numbers, characters, strings, keywords
   - Arithmetic: +, -, *, /, mod, rem
   - Constants: symbols with +NAME+ convention from registry"
  (cond
    ;; Literal number
    ((numberp form)
     (values form t))
    ;; Character literal
    ((characterp form)
     (values form t))
    ;; String literal
    ((stringp form)
     (values form t))
    ;; Keyword
    ((keywordp form)
     (values form t))
    ;; Constant reference (symbol)
    ((and (symbolp form)
          (not (null form))
          (not (eq form t)))
     (multiple-value-bind (value found-p)
         (gethash form registry)
       (if found-p
           (values value t)
           (values nil nil))))
    ;; Arithmetic expression
    ((consp form)
     (let ((op (car form))
           (args (cdr form)))
       (case op
         ;; Addition
         (+
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'+ (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Subtraction
         (-
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'- (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Multiplication
         (*
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (values (apply #'* (mapcar #'first folded-args)) t)
                (values nil nil))))
         ;; Division
         (/
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (every #'second folded-args)
                (let ((nums (mapcar #'first folded-args)))
                  (if (some #'zerop (cdr nums))
                      (values nil nil)  ; Division by zero - don't fold
                      (values (apply #'/ nums) t)))
                (values nil nil))))
         ;; Modulo
         ((mod rem)
          (let ((folded-args (mapcar (lambda (arg)
                                        (multiple-value-list
                                         (fold-constant-expression arg registry)))
                                      args)))
            (if (and (= (length folded-args) 2)
                     (every #'second folded-args))
                (let ((n (first (first folded-args)))
                      (d (first (second folded-args))))
                  (if (zerop d)
                      (values nil nil)
                      (values (if (eq op 'mod) (mod n d) (rem n d)) t)))
                (values nil nil))))
         ;; Unknown operator - not foldable
         (otherwise
          (values nil nil)))))
    ;; Not foldable
    (t (values nil nil))))

(defun compile-defconstant (ast env)
  "Compile a defconstant form.
   Constants are immutable and their values are computed at compile time.

   Strategy:
   1. Try to fold the value expression to a constant
   2. If foldable, register in *constant-registry* and emit literal
   3. If not foldable, compile normally but still create a global

   Generated code pattern:
   - For foldable constants: literal value
   - For non-foldable: global.get (similar to defparameter but immutable)"
  (let* ((name (clysm/compiler/ast:ast-defconstant-name ast))
         (value-form (clysm/compiler/ast:ast-defconstant-value-form ast)))
    ;; Try to fold the value expression
    (multiple-value-bind (folded-value foldable-p)
        (if (typep value-form 'clysm/compiler/ast:ast-literal)
            ;; If already a literal, get its value
            (let ((lit-value (clysm/compiler/ast:ast-literal-value value-form)))
              (fold-constant-expression lit-value *constant-registry*))
            ;; Otherwise, try to fold the AST
            (values nil nil))
      (if foldable-p
          (progn
            ;; Register constant for future reference
            (setf (gethash name *constant-registry*) folded-value)
            ;; Compile as literal value
            (compile-to-instructions
             (clysm/compiler/ast:make-ast-literal :value folded-value
                                                  :literal-type :fixnum)
             env))
          ;; Non-foldable: treat like defparameter but allocate immutable global
          (let* ((global-idx (allocate-special-var-global name))
                 (symbol-type clysm/compiler/codegen/gc-types:+type-symbol+))
            `(;; Set value
              (:global.get ,global-idx)
              ,@(compile-to-instructions value-form env)
              (:struct.set ,symbol-type 1)
              ;; Return the symbol
              (:global.get ,global-idx)))))))

;;; ============================================================
;;; Macro Introspection Compilation (016-macro-system T048)
;;; ============================================================

(defun compile-macroexpand-1 (ast env)
  "Compile a macroexpand-1 form.
   Note: For quoted forms, expansion happens at compile time.
   For runtime forms, this requires a runtime macro system (future work).

   Current implementation: compile-time expansion only.
   If the form is a quoted literal, expand it now and compile the result.
   Otherwise, emit the form unchanged (no runtime expansion available)."
  (let ((form-ast (clysm/compiler/ast:ast-macroexpand-1-form ast)))
    ;; Check if form is a quoted literal we can expand at compile time
    (if (and (typep form-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type form-ast) :quoted))
        ;; Compile-time expansion: expand and return as quoted literal
        (let* ((quoted-form (clysm/compiler/ast:ast-literal-value form-ast))
               (expanded (clysm/compiler/transform/macro:macroexpand-1
                          quoted-form)))
          ;; Return the expanded form as a quoted literal
          (compile-to-instructions
           (clysm/compiler/ast:make-ast-literal :value expanded :literal-type :quoted)
           env))
        ;; Runtime form: compile the form and return it
        ;; (No actual expansion at runtime - would require runtime macro registry)
        (compile-to-instructions form-ast env))))

(defun compile-macroexpand (ast env)
  "Compile a macroexpand form.
   Note: For quoted forms, expansion happens at compile time.
   For runtime forms, this requires a runtime macro system (future work).

   Current implementation: compile-time expansion only.
   If the form is a quoted literal, expand it fully and compile the result.
   Otherwise, emit the form unchanged (no runtime expansion available)."
  (let ((form-ast (clysm/compiler/ast:ast-macroexpand-form ast)))
    ;; Check if form is a quoted literal we can expand at compile time
    (if (and (typep form-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type form-ast) :quoted))
        ;; Compile-time expansion: expand fully and return as quoted literal
        (let* ((quoted-form (clysm/compiler/ast:ast-literal-value form-ast))
               (expanded (clysm/compiler/transform/macro:macroexpand
                          quoted-form)))
          ;; Return the expanded form as a quoted literal
          (compile-to-instructions
           (clysm/compiler/ast:make-ast-literal :value expanded :literal-type :quoted)
           env))
        ;; Runtime form: compile the form and return it
        ;; (No actual expansion at runtime - would require runtime macro registry)
        (compile-to-instructions form-ast env))))

(defun compile-macro-function (ast env)
  "Compile a macro-function lookup form.
   Feature 042: Advanced Defmacro.

   Current implementation: compile-time lookup only.
   If the name is a quoted symbol, looks up the macro at compile time
   and returns T if defined, NIL otherwise (closures can't be serialized).

   Note: Full runtime macro-function returning the actual expander
   would require storing closures in a runtime-accessible registry.
   For most compile-time use cases, checking if a macro is defined
   is sufficient."
  (let ((name-ast (clysm/compiler/ast:ast-macro-function-name ast)))
    ;; Check if name is a quoted symbol we can look up at compile time
    (if (and (typep name-ast 'clysm/compiler/ast:ast-literal)
             (eq (clysm/compiler/ast:ast-literal-literal-type name-ast) :quoted)
             (symbolp (clysm/compiler/ast:ast-literal-value name-ast)))
        ;; Compile-time lookup: check if macro is defined
        (let* ((macro-name (clysm/compiler/ast:ast-literal-value name-ast))
               (macro-fn (clysm/compiler/transform/macro:macro-function macro-name)))
          ;; Return T if macro is defined, NIL otherwise
          ;; (We can't serialize the actual closure to Wasm)
          (if macro-fn
              ;; Macro is defined - return T
              (list '(:global.get 3))  ; Global 3 is typically T in Clysm
              ;; Macro is not defined - return NIL
              (list '(:ref.null :none))))
        ;; Runtime lookup: not supported, return NIL
        (list '(:ref.null :none)))))

;;; ============================================================
;;; Function Section Generation
;;; ============================================================

(defun generate-func-section (funcs)
  "Generate Wasm Function Section content from compiled functions."
  ;; Function section contains type indices for each function
  (mapcar (lambda (f)
            (declare (ignore f))
            0)  ; Type index (placeholder)
          funcs))

(defun compile-expression (expr env)
  "Compile a Lisp expression to Wasm instructions.
   Main entry point for expression compilation."
  (let ((ast (clysm/compiler/ast:parse-expr expr)))
    (compile-to-instructions ast env)))

;;; ============================================================
;;; Sequence Functions (007-sequence-functions)
;;; ============================================================

;;; --- Tier 1: Basic Sequence Functions ---

(defun compile-length (args env)
  "Compile (length sequence) - return number of elements.
   Supports both lists and strings (008-character-string).
   For strings, counts UTF-8 characters (not bytes).
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "length requires exactly 1 argument"))
  (let ((seq-local (env-add-local env (gensym "LEN-SEQ")))
        (count-local (env-add-local env (gensym "LEN-COUNT") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+)
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    ;; Compile sequence argument and store
    `(,@(compile-to-instructions (first args) env)
      (:local.set ,seq-local)
      ;; Check if it's a string
      (:local.get ,seq-local)
      (:ref.test ,(list :ref string-type))
      (:if (:result :i32))
      ;; Then: string - count UTF-8 characters (leaves i32 on stack)
      ,@(compile-string-length-body seq-local count-local string-type env)
      :else
      ;; Else: list - count cons cells (leaves i32 on stack)
      ,@(compile-list-length-body seq-local count-local cons-type)
      :end
      ;; Result is now i32 on stack, convert to fixnum
      :ref.i31)))

(defun compile-string-length-body (seq-local count-local string-type env)
  "Generate instructions to count UTF-8 characters in a string.
   UTF-8 character counting: count bytes that are NOT continuation bytes.
   Continuation bytes are in range 0x80-0xBF (10xxxxxx pattern).
   Stack effect within body: [] -> []
   Sets count-local to the character count."
  (let ((idx-local (env-add-local env (gensym "STR-IDX") :i32))
        (len-local (env-add-local env (gensym "STR-LEN") :i32))
        (byte-local (env-add-local env (gensym "STR-BYTE") :i32)))
    `(;; Get byte array length
      (:local.get ,seq-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Initialize count and index to 0
      (:i32.const 0)
      (:local.set ,count-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; Loop through bytes
      (:block $str_len_done)
      (:loop $str_len_loop)
      ;; Check if idx >= len
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $str_len_done)
      ;; Get byte at idx
      (:local.get ,seq-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if NOT a continuation byte (byte < 0x80 OR byte >= 0xC0)
      ;; Continuation bytes are 0x80-0xBF (10xxxxxx)
      ;; We count if (byte & 0xC0) != 0x80
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)  ; void block type
      ;; Is a leading byte or ASCII, increment count
      (:local.get ,count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,count-local)
      :end
      ;; Increment idx
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $str_len_loop)
      :end  ; loop
      :end  ; block
      ;; Return count (leave on stack for if result)
      (:local.get ,count-local))))

(defun compile-list-length-body (seq-local count-local cons-type)
  "Generate instructions to count elements in a list.
   Stack effect within body: [] -> []
   Sets count-local to the element count."
  `(;; Initialize counter to 0
    (:i32.const 0)
    (:local.set ,count-local)
    ;; Loop until nil
    (:block $len_done)
    (:loop $len_loop)
    ;; Check if list is nil
    (:local.get ,seq-local)
    :ref.is_null
    (:br_if $len_done)
    ;; Increment count
    (:local.get ,count-local)
    (:i32.const 1)
    :i32.add
    (:local.set ,count-local)
    ;; Get cdr
    (:local.get ,seq-local)
    (:ref.cast ,(list :ref cons-type))
    (:struct.get ,cons-type 1)
    (:local.set ,seq-local)
    (:br $len_loop)
    :end  ; loop
    :end  ; block
    ;; Return count (leave on stack for if result)
    (:local.get ,count-local)))

(defun compile-append (args env)
  "Compile (append list1 list2) - concatenate lists.
   Does not modify input lists. Last argument is shared.
   Stack: [] -> [list]"
  (cond
    ((null args) '((:ref.null :none)))
    ((= (length args) 1)
     (compile-to-instructions (first args) env))
    ((= (length args) 2)
     (compile-append-two (first args) (second args) env))
    (t
     ;; Multiple lists: fold right (append a (append b (append c d)))
     (compile-append-two (first args)
                         (clysm/compiler/ast:make-ast-call
                          :function 'append
                          :arguments (rest args))
                         env))))

(defun compile-append-two (list1-form list2-form env)
  "Compile (append list1 list2) for exactly two lists."
  (let ((list1-local (env-add-local env (gensym "APP-L1")))
        (list2-local (env-add-local env (gensym "APP-L2")))
        (acc-local (env-add-local env (gensym "APP-ACC")))
        (temp-local (env-add-local env (gensym "APP-TMP")))
        (last-cons-local (env-add-local env (gensym "APP-LAST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile both lists
      (emit* (compile-to-instructions list1-form env))
      (emit :local.set list1-local)
      (emit* (compile-to-instructions list2-form env))
      (emit :local.set list2-local)
      ;; If list1 is nil, return list2
      (emit :local.get list1-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            `(:local.get ,list2-local)
            :else
            ;; Copy list1, then set last cdr to list2
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,acc-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:block $app_done)
            '(:loop $app_loop)
            `(:local.get ,list1-local)
            :ref.is_null
            '(:br_if $app_done)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.set ,temp-local)
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,temp-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,temp-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:br $app_loop)
            :end
            :end
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,list2-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,acc-local)
            :end))))

(defun compile-reverse (args env)
  "Compile (reverse list) - return new reversed list.
   Does not modify input list.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "reverse requires exactly 1 argument"))
  (let ((list-local (env-add-local env (gensym "REV-LIST")))
        (acc-local (env-add-local env (gensym "REV-ACC")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile list argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list-local)
      ;; Initialize accumulator to nil
      (emit '(:ref.null :none))
      (emit :local.set acc-local)
      ;; Loop: prepend each element to accumulator
      (emit '(:block $rev_done)
            '(:loop $rev_loop)
            `(:local.get ,list-local)
            :ref.is_null
            '(:br_if $rev_done)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,acc-local)
            `(:struct.new ,cons-type)
            `(:local.set ,acc-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:br $rev_loop)
            :end
            :end)
      ;; Return accumulator
      (emit :local.get acc-local))))

(defun compile-nreverse (args env)
  "Compile (nreverse list) - destructively reverse list.
   Modifies cdr pointers in place.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "nreverse requires exactly 1 argument"))
  (let ((prev-local (env-add-local env (gensym "NREV-PREV")))
        (current-local (env-add-local env (gensym "NREV-CURR")))
        (next-local (env-add-local env (gensym "NREV-NEXT")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Initialize prev to nil
      (emit '(:ref.null :none))
      (emit :local.set prev-local)
      ;; current = list
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set current-local)
      ;; Loop to reverse
      (emit '(:block $nrev_done)
            '(:loop $nrev_loop)
            `(:local.get ,current-local)
            :ref.is_null
            '(:br_if $nrev_done)
            `(:local.get ,current-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,next-local)
            `(:local.get ,current-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,prev-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,current-local)
            `(:local.set ,prev-local)
            `(:local.get ,next-local)
            `(:local.set ,current-local)
            '(:br $nrev_loop)
            :end
            :end)
      ;; Return prev (new head)
      (emit :local.get prev-local))))

(defun compile-last (args env)
  "Compile (last list) - return last cons cell.
   Stack: [] -> [cons or nil]"
  (when (< (length args) 1)
    (error "last requires at least 1 argument"))
  ;; For simplicity, only support (last list), not (last list n)
  (let ((list-local (env-add-local env (gensym "LAST-LIST")))
        (prev-local (env-add-local env (gensym "LAST-PREV")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile list argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list-local)
      ;; If list is nil, return nil
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,list-local)
            `(:local.set ,prev-local)
            '(:block $last_done (:result :anyref))
            '(:loop $last_loop (:result :anyref))
            `(:local.get ,prev-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.tee ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            `(:local.get ,prev-local)
            '(:br $last_done)
            :else
            `(:local.get ,list-local)
            `(:local.set ,prev-local)
            '(:ref.null :none)
            '(:br $last_loop)
            :end
            :end
            :end
            :end))))

(defun compile-butlast (args env)
  "Compile (butlast list) - return list without last element.
   Does not modify input list.
   Stack: [] -> [list]"
  (when (< (length args) 1)
    (error "butlast requires at least 1 argument"))
  ;; For simplicity, only support (butlast list), not (butlast list n)
  (let ((list-local (env-add-local env (gensym "BUTL-LIST")))
        (acc-local (env-add-local env (gensym "BUTL-ACC")))
        (last-cons-local (env-add-local env (gensym "BUTL-LAST")))
        (temp-local (env-add-local env (gensym "BUTL-TMP")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile list argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list-local)
      ;; If list is nil or single element, return nil
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            :ref.is_null
            '(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,acc-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:block $butl_done)
            '(:loop $butl_loop)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            :ref.is_null
            '(:br_if $butl_done)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.set ,temp-local)
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,temp-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,temp-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:br $butl_loop)
            :end
            :end
            `(:local.get ,acc-local)
            :end
            :end))))

(defun compile-copy-list (args env)
  "Compile (copy-list list) - shallow copy of list.
   Stack: [] -> [list]"
  (when (/= (length args) 1)
    (error "copy-list requires exactly 1 argument"))
  (let ((list-local (env-add-local env (gensym "COPY-LIST")))
        (acc-local (env-add-local env (gensym "COPY-ACC")))
        (last-cons-local (env-add-local env (gensym "COPY-LAST")))
        (temp-local (env-add-local env (gensym "COPY-TMP")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile list argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list-local)
      ;; If nil, return nil
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,acc-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:block $copy_done)
            '(:loop $copy_loop)
            `(:local.get ,list-local)
            :ref.is_null
            '(:br_if $copy_done)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.set ,temp-local)
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,temp-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,temp-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:br $copy_loop)
            :end
            :end
            `(:local.get ,acc-local)
            :end))))

;;; --- Tier 2: Higher-Order Sequence Functions ---

(defun compile-mapcar (args env)
  "Compile (mapcar fn list) - apply fn to each element.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "mapcar requires exactly 2 arguments"))
  (let ((fn-local (env-add-local env (gensym "MAP-FN")))
        (list-local (env-add-local env (gensym "MAP-LIST")))
        (acc-local (env-add-local env (gensym "MAP-ACC")))
        (last-cons-local (env-add-local env (gensym "MAP-LAST")))
        (temp-local (env-add-local env (gensym "MAP-TMP")))
        (result-local (env-add-local env (gensym "MAP-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile function argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set fn-local)
      ;; Compile list argument
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; If list is nil, return nil
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,fn-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.set ,result-local)
            `(:local.get ,result-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,acc-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:block $map_done (:result :anyref))
            '(:loop $map_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            `(:local.get ,acc-local)
            '(:br $map_done)
            :else
            '(:ref.null :none)
            :end
            :drop
            `(:local.get ,fn-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.set ,result-local)
            `(:local.get ,result-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.set ,temp-local)
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,temp-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,temp-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $map_loop)
            :end
            :drop
            `(:local.get ,acc-local)
            :end
            :end))))

(defun compile-mapc (args env)
  "Compile (mapc fn list) - apply fn for side effects, return original list.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "mapc requires exactly 2 arguments"))
  (let ((fn-local (env-add-local env (gensym "MAPC-FN")))
        (list-local (env-add-local env (gensym "MAPC-LIST")))
        (original-local (env-add-local env (gensym "MAPC-ORIG")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile function argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set fn-local)
      ;; Compile list argument and save original
      (emit* (compile-to-instructions (second args) env))
      (emit :local.tee original-local)
      (emit :local.set list-local)
      ;; Loop applying fn
      (emit '(:block $mapc_done)
            '(:loop $mapc_loop)
            `(:local.get ,list-local)
            :ref.is_null
            '(:br_if $mapc_done)
            `(:local.get ,fn-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            :drop
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:br $mapc_loop)
            :end
            :end)
      ;; Return original list
      (emit :local.get original-local))))

(defun compile-maplist (args env)
  "Compile (maplist fn list) - apply fn to successive cdrs.
   Stack: [] -> [list]"
  (when (/= (length args) 2)
    (error "maplist requires exactly 2 arguments"))
  (let ((fn-local (env-add-local env (gensym "MAPL-FN")))
        (list-local (env-add-local env (gensym "MAPL-LIST")))
        (acc-local (env-add-local env (gensym "MAPL-ACC")))
        (last-cons-local (env-add-local env (gensym "MAPL-LAST")))
        (temp-local (env-add-local env (gensym "MAPL-TMP")))
        (result-local (env-add-local env (gensym "MAPL-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile function argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set fn-local)
      ;; Compile list argument
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; If list is nil, return nil
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,fn-local)
            `(:local.get ,list-local)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.set ,result-local)
            `(:local.get ,result-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,acc-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:block $mapl_done (:result :anyref))
            '(:loop $mapl_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            `(:local.get ,acc-local)
            '(:br $mapl_done)
            :else
            '(:ref.null :none)
            :end
            :drop
            `(:local.get ,fn-local)
            `(:local.get ,list-local)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.set ,result-local)
            `(:local.get ,result-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.set ,temp-local)
            `(:local.get ,last-cons-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,temp-local)
            `(:struct.set ,cons-type 1)
            `(:local.get ,temp-local)
            `(:local.set ,last-cons-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $mapl_loop)
            :end
            :drop
            `(:local.get ,acc-local)
            :end
            :end))))

(defun compile-reduce (args env)
  "Compile (reduce fn list) - combine elements using fn.
   Stack: [] -> [any]"
  (when (< (length args) 2)
    (error "reduce requires at least 2 arguments"))
  ;; For simplicity, no :initial-value support yet
  (let ((fn-local (env-add-local env (gensym "RED-FN")))
        (list-local (env-add-local env (gensym "RED-LIST")))
        (acc-local (env-add-local env (gensym "RED-ACC")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-2+))
    (with-instruction-collector
      ;; Compile function argument
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set fn-local)
      ;; Compile list argument
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; If list is nil, error (or return nil for now)
      (emit :local.get list-local)
      (emit :ref.is_null)
      (emit `(:if (:result :anyref))
            '(:ref.null :none)
            :else
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,acc-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:block $red_done (:result :anyref))
            '(:loop $red_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            `(:local.get ,acc-local)
            '(:br $red_done)
            :else
            '(:ref.null :none)
            :end
            :drop
            `(:local.get ,fn-local)
            `(:local.get ,acc-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,fn-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 2)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.set ,acc-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $red_loop)
            :end
            :drop
            `(:local.get ,acc-local)
            :end
            :end))))

;;; --- Tier 4: Set Operations (043-self-hosting-blockers) ---

(defun compile-adjoin (args env)
  "Compile (adjoin item list) - add item to list if not already member.
   Feature: 043-self-hosting-blockers
   Stack: [] -> [list with item]"
  (when (< (length args) 2)
    (error "adjoin requires at least 2 arguments"))
  ;; For simplicity, just cons the item onto the list
  ;; (proper member check would require :test/:key handling)
  (let ((item-local (env-add-local env (gensym "ADJ-ITEM")))
        (list-local (env-add-local env (gensym "ADJ-LIST")))
        (found-local (env-add-local env (gensym "ADJ-FOUND") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile item and list
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set item-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Initialize found flag
      (emit '(:i32.const 0))
      (emit :local.set found-local)
      ;; Simple member check loop
      (emit '(:block $adj_check)
            '(:loop $adj_loop)
            `(:local.get ,list-local)
            :ref.is_null
            '(:br_if $adj_check)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.cast :eq)
            `(:local.get ,item-local)
            '(:ref.cast :eq)
            :ref.eq
            '(:if)
            '(:i32.const 1)
            `(:local.set ,found-local)
            '(:br $adj_check)
            :end
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:br $adj_loop)
            :end
            :end)
      ;; If not found, cons item onto original list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      (emit `(:local.get ,found-local)
            '(:if (:result :anyref))
            `(:local.get ,list-local)
            :else
            `(:local.get ,item-local)
            `(:local.get ,list-local)
            `(:struct.new ,cons-type)
            :end))))

(defun compile-union (args env)
  "Compile (union list1 list2) - return union of two lists.
   Feature: 043-self-hosting-blockers
   Stack: [] -> [list]"
  (when (< (length args) 2)
    (error "union requires at least 2 arguments"))
  ;; Simple implementation: append list1 elements not in list2 to list2
  (let ((list1-local (env-add-local env (gensym "UNI-L1")))
        (list2-local (env-add-local env (gensym "UNI-L2")))
        (result-local (env-add-local env (gensym "UNI-RES")))
        (elem-local (env-add-local env (gensym "UNI-ELEM")))
        (check-local (env-add-local env (gensym "UNI-CHK")))
        (found-local (env-add-local env (gensym "UNI-FND") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile lists
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list1-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list2-local)
      ;; Start with list2 as result
      (emit :local.get list2-local)
      (emit :local.set result-local)
      ;; Loop through list1, add non-duplicates
      (emit '(:block $uni_done)
            '(:loop $uni_loop)
            `(:local.get ,list1-local)
            :ref.is_null
            '(:br_if $uni_done)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,elem-local)
            '(:i32.const 0)
            `(:local.set ,found-local)
            `(:local.get ,list2-local)
            `(:local.set ,check-local)
            '(:block $found)
            '(:loop $check_loop)
            `(:local.get ,check-local)
            :ref.is_null
            '(:br_if $found)
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.cast :eq)
            `(:local.get ,elem-local)
            '(:ref.cast :eq)
            :ref.eq
            '(:if)
            '(:i32.const 1)
            `(:local.set ,found-local)
            '(:br $found)
            :end
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,check-local)
            '(:br $check_loop)
            :end
            :end
            `(:local.get ,found-local)
            :i32.eqz
            '(:if)
            `(:local.get ,elem-local)
            `(:local.get ,result-local)
            `(:struct.new ,cons-type)
            `(:local.set ,result-local)
            :end
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:br $uni_loop)
            :end
            :end)
      (emit :local.get result-local))))

(defun compile-intersection (args env)
  "Compile (intersection list1 list2) - return intersection of two lists.
   Feature: 043-self-hosting-blockers
   Stack: [] -> [list]"
  (when (< (length args) 2)
    (error "intersection requires at least 2 arguments"))
  (let ((list1-local (env-add-local env (gensym "INT-L1")))
        (list2-local (env-add-local env (gensym "INT-L2")))
        (result-local (env-add-local env (gensym "INT-RES")))
        (elem-local (env-add-local env (gensym "INT-ELEM")))
        (check-local (env-add-local env (gensym "INT-CHK")))
        (found-local (env-add-local env (gensym "INT-FND") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile lists
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list1-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list2-local)
      ;; Start with empty result
      (emit '(:ref.null :none))
      (emit :local.set result-local)
      ;; Loop through list1, add elements that are in list2
      (emit '(:block $int_done)
            '(:loop $int_loop)
            `(:local.get ,list1-local)
            :ref.is_null
            '(:br_if $int_done)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,elem-local)
            '(:i32.const 0)
            `(:local.set ,found-local)
            `(:local.get ,list2-local)
            `(:local.set ,check-local)
            '(:block $found)
            '(:loop $check_loop)
            `(:local.get ,check-local)
            :ref.is_null
            '(:br_if $found)
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.cast :eq)
            `(:local.get ,elem-local)
            '(:ref.cast :eq)
            :ref.eq
            '(:if)
            '(:i32.const 1)
            `(:local.set ,found-local)
            '(:br $found)
            :end
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,check-local)
            '(:br $check_loop)
            :end
            :end
            `(:local.get ,found-local)
            '(:if)
            `(:local.get ,elem-local)
            `(:local.get ,result-local)
            `(:struct.new ,cons-type)
            `(:local.set ,result-local)
            :end
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:br $int_loop)
            :end
            :end)
      (emit :local.get result-local))))

(defun compile-set-difference (args env)
  "Compile (set-difference list1 list2) - elements in list1 not in list2.
   Feature: 043-self-hosting-blockers
   Stack: [] -> [list]"
  (when (< (length args) 2)
    (error "set-difference requires at least 2 arguments"))
  (let ((list1-local (env-add-local env (gensym "DIFF-L1")))
        (list2-local (env-add-local env (gensym "DIFF-L2")))
        (result-local (env-add-local env (gensym "DIFF-RES")))
        (elem-local (env-add-local env (gensym "DIFF-ELEM")))
        (check-local (env-add-local env (gensym "DIFF-CHK")))
        (found-local (env-add-local env (gensym "DIFF-FND") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile lists
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list1-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list2-local)
      ;; Start with empty result
      (emit '(:ref.null :none))
      (emit :local.set result-local)
      ;; Loop through list1, add elements NOT in list2
      (emit '(:block $diff_done)
            '(:loop $diff_loop)
            `(:local.get ,list1-local)
            :ref.is_null
            '(:br_if $diff_done)
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,elem-local)
            '(:i32.const 0)
            `(:local.set ,found-local)
            `(:local.get ,list2-local)
            `(:local.set ,check-local)
            '(:block $found)
            '(:loop $check_loop)
            `(:local.get ,check-local)
            :ref.is_null
            '(:br_if $found)
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.cast :eq)
            `(:local.get ,elem-local)
            '(:ref.cast :eq)
            :ref.eq
            '(:if)
            '(:i32.const 1)
            `(:local.set ,found-local)
            '(:br $found)
            :end
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,check-local)
            '(:br $check_loop)
            :end
            :end
            `(:local.get ,found-local)
            :i32.eqz
            '(:if)
            `(:local.get ,elem-local)
            `(:local.get ,result-local)
            `(:struct.new ,cons-type)
            `(:local.set ,result-local)
            :end
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:br $diff_loop)
            :end
            :end)
      (emit :local.get result-local))))

(defun compile-subsetp (args env)
  "Compile (subsetp list1 list2) - test if list1 is subset of list2.
   Feature: 001-ansi-list-ops
   Stack: [] -> [boolean]"
  (when (< (length args) 2)
    (error "subsetp requires at least 2 arguments"))
  (let ((list1-local (env-add-local env (gensym "SUBS-L1")))
        (list2-local (env-add-local env (gensym "SUBS-L2")))
        (elem-local (env-add-local env (gensym "SUBS-ELEM")))
        (check-local (env-add-local env (gensym "SUBS-CHK")))
        (found-local (env-add-local env (gensym "SUBS-FND") :i32))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile lists
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set list1-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list2-local)
      ;; Loop: for each element of list1, check if in list2
      (emit '(:block $subs_done (:result :anyref))
            '(:loop $subs_loop (:result :anyref))
            ;; Check if list1 is null
            `(:local.get ,list1-local)
            :ref.is_null
            '(:if (:result :anyref)))
      ;; All elements checked, return T
      (emit* (compile-to-instructions
              (clysm/compiler/ast:make-ast-literal :value t :literal-type :t)
              env))
      (emit '(:br $subs_done)
            :else
            ;; Get current element
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,elem-local)
            ;; Search for it in list2
            '(:i32.const 0)
            `(:local.set ,found-local)
            `(:local.get ,list2-local)
            `(:local.set ,check-local)
            '(:block $found)
            '(:loop $check_loop)
            `(:local.get ,check-local)
            :ref.is_null
            '(:br_if $found)
            ;; Compare current check element with elem
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            '(:ref.cast :eq)
            `(:local.get ,elem-local)
            '(:ref.cast :eq)
            :ref.eq
            '(:if)
            '(:i32.const 1)
            `(:local.set ,found-local)
            '(:br $found)
            :end
            `(:local.get ,check-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,check-local)
            '(:br $check_loop)
            :end  ; check_loop
            :end  ; found block
            ;; If not found, return NIL
            `(:local.get ,found-local)
            :i32.eqz
            '(:if (:result :anyref))
            '(:ref.null :none)
            '(:br $subs_done)
            :else
            ;; Advance list1 and continue
            `(:local.get ,list1-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list1-local)
            '(:ref.null :none)  ; placeholder for loop continuation
            :drop
            '(:br $subs_loop)
            :end
            :end
            :end  ; subs_loop
            :end))))

(defun compile-acons (args env)
  "Compile (acons key value alist) - prepend (key . value) to alist.
   Feature: 001-ansi-list-ops
   Stack: [] -> [alist]"
  (when (/= (length args) 3)
    (error "acons requires exactly 3 arguments"))
  (let ((cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; (cons (cons key value) alist)
      ;; First create inner cons (key . value)
      (emit* (compile-to-instructions (first args) env))
      (emit* (compile-to-instructions (second args) env))
      (emit :struct.new cons-type)
      ;; Then cons that with alist
      (emit* (compile-to-instructions (third args) env))
      (emit :struct.new cons-type))))

(defun compile-pairlis (args env)
  "Compile (pairlis keys values &optional alist) - build alist from keys and values.
   Feature: 001-ansi-list-ops
   Stack: [] -> [alist]"
  (when (< (length args) 2)
    (error "pairlis requires at least 2 arguments"))
  (let ((keys-local (env-add-local env (gensym "PAIR-K")))
        (vals-local (env-add-local env (gensym "PAIR-V")))
        (result-local (env-add-local env (gensym "PAIR-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile keys and values
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set keys-local)
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set vals-local)
      ;; Initialize result with optional alist or NIL
      (if (>= (length args) 3)
          (progn
            (emit* (compile-to-instructions (third args) env))
            (emit :local.set result-local))
          (progn
            (emit '(:ref.null :none))
            (emit :local.set result-local)))
      ;; Loop through keys and values, building pairs
      (emit '(:block $pair_done)
            '(:loop $pair_loop)
            `(:local.get ,keys-local)
            :ref.is_null
            '(:br_if $pair_done)
            `(:local.get ,vals-local)
            :ref.is_null
            '(:br_if $pair_done)
            `(:local.get ,keys-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,vals-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:struct.new ,cons-type)
            `(:local.get ,result-local)
            `(:struct.new ,cons-type)
            `(:local.set ,result-local)
            `(:local.get ,keys-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,keys-local)
            `(:local.get ,vals-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,vals-local)
            '(:br $pair_loop)
            :end
            :end)
      (emit :local.get result-local))))

(defun compile-copy-alist (args env)
  "Compile (copy-alist alist) - copy spine and entry cons cells.
   Feature: 001-ansi-list-ops
   Stack: [] -> [alist]"
  (when (/= (length args) 1)
    (error "copy-alist requires exactly 1 argument"))
  (let ((alist-local (env-add-local env (gensym "CA-ALIST")))
        (result-local (env-add-local env (gensym "CA-RES")))
        (last-local (env-add-local env (gensym "CA-LAST")))
        (entry-local (env-add-local env (gensym "CA-ENTRY")))
        (new-entry-local (env-add-local env (gensym "CA-NEW")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+))
    (with-instruction-collector
      ;; Compile alist
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set alist-local)
      ;; Start with empty result
      (emit '(:ref.null :none))
      (emit :local.set result-local)
      (emit '(:ref.null :none))
      (emit :local.set last-local)
      ;; Loop through alist
      (emit '(:block $ca_done)
            '(:loop $ca_loop)
            `(:local.get ,alist-local)
            :ref.is_null
            '(:br_if $ca_done)
            `(:local.get ,alist-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.set ,entry-local)
            `(:local.get ,entry-local)
            `(:ref.test ,(list :ref cons-type))
            '(:if)
            `(:local.get ,entry-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,entry-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:struct.new ,cons-type)
            `(:local.set ,new-entry-local)
            :else
            `(:local.get ,entry-local)
            `(:local.set ,new-entry-local)
            :end
            `(:local.get ,new-entry-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.get ,result-local)
            :ref.is_null
            '(:if)
            `(:local.get ,new-entry-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,last-local)
            `(:local.set ,result-local)
            :else
            `(:local.get ,last-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:local.get ,new-entry-local)
            '(:ref.null :none)
            `(:struct.new ,cons-type)
            `(:local.tee ,last-local)
            `(:struct.set ,cons-type 1)
            :end
            `(:local.get ,alist-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,alist-local)
            '(:br $ca_loop)
            :end
            :end)
      (emit :local.get result-local))))


;;; --- Tier 4: Quantifier Predicates ---

(defun compile-every (args env)
  "Compile (every pred list) - true if all elements satisfy predicate.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "every requires exactly 2 arguments"))
  (let ((pred-local (env-add-local env (gensym "EVERY-PRED")))
        (list-local (env-add-local env (gensym "EVERY-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile predicate
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set pred-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check loop - return NIL if any fails, T if all pass
      (emit '(:block $every_done (:result :anyref))
            '(:loop $every_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Empty/exhausted - return T
            '(:i32.const 1)
            :ref.i31
            '(:br $every_done)
            :else
            ;; Apply predicate
            `(:local.get ,pred-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,pred-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Predicate returned NIL - return NIL
            '(:ref.null :none)
            '(:br $every_done)
            :else
            ;; Continue
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $every_loop)
            :end
            :end
            :end  ; loop
            :end))))

(defun compile-some (args env)
  "Compile (some pred list) - return first non-nil result.
   Stack: [] -> [value or NIL]"
  (when (/= (length args) 2)
    (error "some requires exactly 2 arguments"))
  (let ((pred-local (env-add-local env (gensym "SOME-PRED")))
        (list-local (env-add-local env (gensym "SOME-LIST")))
        (pred-result-local (env-add-local env (gensym "SOME-RES")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile predicate
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set pred-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check loop
      (emit '(:block $some_done (:result :anyref))
            '(:loop $some_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Exhausted - return NIL
            '(:ref.null :none)
            '(:br $some_done)
            :else
            ;; Apply predicate
            `(:local.get ,pred-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,pred-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            `(:local.tee ,pred-result-local)
            :ref.is_null
            :i32.eqz
            '(:if (:result :anyref))
            ;; Found - return result
            `(:local.get ,pred-result-local)
            '(:br $some_done)
            :else
            ;; Continue
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $some_loop)
            :end
            :end
            :end  ; loop
            :end))))

(defun compile-notany (args env)
  "Compile (notany pred list) - true if no element satisfies predicate.
   Equivalent to (not (some pred list)).
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "notany requires exactly 2 arguments"))
  (let ((pred-local (env-add-local env (gensym "NOTANY-PRED")))
        (list-local (env-add-local env (gensym "NOTANY-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile predicate
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set pred-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check loop
      (emit '(:block $notany_done (:result :anyref))
            '(:loop $notany_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Exhausted - return T (none matched)
            '(:i32.const 1)
            :ref.i31
            '(:br $notany_done)
            :else
            ;; Apply predicate
            `(:local.get ,pred-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,pred-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            :ref.is_null
            :i32.eqz
            '(:if (:result :anyref))
            ;; Found match - return NIL
            '(:ref.null :none)
            '(:br $notany_done)
            :else
            ;; Continue
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $notany_loop)
            :end
            :end
            :end  ; loop
            :end))))

(defun compile-notevery (args env)
  "Compile (notevery pred list) - true if some element fails predicate.
   Equivalent to (not (every pred list)).
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "notevery requires exactly 2 arguments"))
  (let ((pred-local (env-add-local env (gensym "NOTEV-PRED")))
        (list-local (env-add-local env (gensym "NOTEV-LIST")))
        (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
        (closure-type clysm/compiler/codegen/gc-types:+type-closure+)
        (func-type clysm/compiler/codegen/gc-types:+type-func-1+))
    (with-instruction-collector
      ;; Compile predicate
      (emit* (compile-to-instructions (first args) env))
      (emit :local.set pred-local)
      ;; Compile list
      (emit* (compile-to-instructions (second args) env))
      (emit :local.set list-local)
      ;; Check loop
      (emit '(:block $notev_done (:result :anyref))
            '(:loop $notev_loop (:result :anyref))
            `(:local.get ,list-local)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Exhausted - all passed, return NIL
            '(:ref.null :none)
            '(:br $notev_done)
            :else
            ;; Apply predicate
            `(:local.get ,pred-local)
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 0)
            `(:local.get ,pred-local)
            `(:ref.cast ,closure-type)
            `(:struct.get ,closure-type 1)
            `(:ref.cast ,func-type)
            `(:call_ref ,func-type)
            :ref.is_null
            '(:if (:result :anyref))
            ;; Predicate failed - return T
            '(:i32.const 1)
            :ref.i31
            '(:br $notev_done)
            :else
            ;; Continue
            `(:local.get ,list-local)
            `(:ref.cast ,(list :ref cons-type))
            `(:struct.get ,cons-type 1)
            `(:local.set ,list-local)
            '(:ref.null :none)
            '(:br $notev_loop)
            :end
            :end
            :end  ; loop
            :end))))

;;; ============================================================
;;; Character Functions (008-character-string)
;;; ============================================================

(defun compile-char-code (args env)
  "Compile (char-code char) - return Unicode codepoint.
   Characters are stored as i31ref with the codepoint value.
   Stack: [] -> [fixnum]"
  (when (/= (length args) 1)
    (error "char-code requires exactly 1 argument"))
  ;; Character is already stored as i31ref with codepoint
  ;; Just return it as-is (it's the same representation as fixnum)
  (compile-to-instructions (first args) env))

(defun compile-code-char (args env)
  "Compile (code-char code) - return character for codepoint.
   Returns NIL for invalid codepoints (negative or > #x10FFFF or surrogates).
   Stack: [] -> [character or NIL]"
  (when (/= (length args) 1)
    (error "code-char requires exactly 1 argument"))
  (let ((code-local (env-add-local env (gensym "CODE") :i32)))
    (with-instruction-collector
      ;; Compile code argument and save as i32
      (emit* (compile-to-instructions (first args) env))
      (emit '(:ref.cast :i31)
            :i31.get_s
            `(:local.set ,code-local)
            ;; Check validity: 0 <= code <= #x10FFFF and not surrogate
            '(:block $code_char_done (:result :anyref))
            ;; Check < 0 (invalid)
            `(:local.get ,code-local)
            '(:i32.const 0)
            :i32.lt_s
            '(:if (:result :anyref))
            '(:ref.null :none)
            '(:br $code_char_done)
            :else
            ;; Check > #x10FFFF (invalid)
            `(:local.get ,code-local)
            '(:i32.const #x10FFFF)
            :i32.gt_s
            '(:if (:result :anyref))
            '(:ref.null :none)
            '(:br $code_char_done)
            :else
            ;; Check surrogate range #xD800-#xDFFF (invalid)
            `(:local.get ,code-local)
            '(:i32.const #xD800)
            :i32.ge_s
            `(:local.get ,code-local)
            '(:i32.const #xDFFF)
            :i32.le_s
            :i32.mul  ;; AND of both conditions
            '(:if (:result :anyref))
            '(:ref.null :none)
            '(:br $code_char_done)
            :else
            ;; Valid - return as character (i31ref)
            `(:local.get ,code-local)
            :ref.i31
            :end
            :end
            :end
            :end))))

(defun compile-char= (args env)
  "Compile (char= char1 char2) - character equality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.eq
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char/= (args env)
  "Compile (char/= char1 char2) - character inequality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char/= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.ne
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char< (args env)
  "Compile (char< char1 char2) - character less than.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char< requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.lt_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char> (args env)
  "Compile (char> char1 char2) - character greater than.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char> requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.gt_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char<= (args env)
  "Compile (char<= char1 char2) - character less than or equal.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char<= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.le_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char>= (args env)
  "Compile (char>= char1 char2) - character greater than or equal.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char>= requires exactly 2 arguments"))
  (append
   (compile-to-instructions (first args) env)
   '((:ref.cast :i31) :i31.get_s)
   (compile-to-instructions (second args) env)
   '((:ref.cast :i31) :i31.get_s
     :i32.ge_s
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31
     :else
     (:ref.null :none)
     :end)))

(defun compile-char-to-lower (args env local)
  "Helper: compile code to convert character to lowercase.
   Stores result in local. Returns instruction list.
   Input: character codepoint in local.
   Result: lowercase codepoint in local."
  (declare (ignore args env))
  `(;; If 'A' <= c <= 'Z', add 32
    (:local.get ,local)
    (:i32.const 65)  ;; 'A'
    :i32.ge_s
    (:local.get ,local)
    (:i32.const 90)  ;; 'Z'
    :i32.le_s
    :i32.mul  ;; AND
    (:if (:result :i32))
    (:local.get ,local)
    (:i32.const 32)
    :i32.add
    :else
    (:local.get ,local)
    :end))

(defun compile-char-equal (args env)
  "Compile (char-equal char1 char2) - case-insensitive character equality.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-equal requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     ;; Get first char, convert to lowercase
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     ;; Get second char, convert to lowercase
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       ;; Compare
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-lessp (args env)
  "Compile (char-lessp char1 char2) - case-insensitive char<.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-lessp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-greaterp (args env)
  "Compile (char-greaterp char1 char2) - case-insensitive char>.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-greaterp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.gt_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-not-lessp (args env)
  "Compile (char-not-lessp char1 char2) - case-insensitive char>=.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-not-lessp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.ge_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-not-greaterp (args env)
  "Compile (char-not-greaterp char1 char2) - case-insensitive char<=.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "char-not-greaterp requires exactly 2 arguments"))
  (let ((c1-local (env-add-local env (gensym "C1") :i32))
        (c2-local (env-add-local env (gensym "C2") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c1-local))
     (compile-char-to-lower nil nil c1-local)
     `((:local.set ,c1-local))
     (compile-to-instructions (second args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c2-local))
     (compile-char-to-lower nil nil c2-local)
     `((:local.set ,c2-local)
       (:local.get ,c1-local)
       (:local.get ,c2-local)
       :i32.le_s
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-char-upcase (args env)
  "Compile (char-upcase char) - convert to uppercase.
   Stack: [] -> [character]"
  (when (/= (length args) 1)
    (error "char-upcase requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       ;; If 'a' <= c <= 'z', subtract 32
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.mul  ;; AND - result is 0 or 1 (i32)
       (:if (:result :anyref))
       (:local.get ,c-local)
       (:i32.const 32)
       :i32.sub
       :ref.i31
       :else
       (:local.get ,c-local)
       :ref.i31
       :end))))

(defun compile-char-downcase (args env)
  "Compile (char-downcase char) - convert to lowercase.
   Stack: [] -> [character]"
  (when (/= (length args) 1)
    (error "char-downcase requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       ;; If 'A' <= c <= 'Z', add 32
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.mul  ;; AND - result is 0 or 1 (i32)
       (:if (:result :anyref))
       (:local.get ,c-local)
       (:i32.const 32)
       :i32.add
       :ref.i31
       :else
       (:local.get ,c-local)
       :ref.i31
       :end))))

(defun compile-characterp (args env)
  "Compile (characterp obj) - check if object is a character.
   Since characters and fixnums share i31ref representation,
   we can't distinguish them at runtime without type tags.
   For MVP, returns T for any i31ref (which includes both chars and fixnums).
   TODO: Add proper type tagging to distinguish chars from fixnums.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "characterp requires exactly 1 argument"))
  ;; Test if value is a valid i31ref (not null and ref.test succeeds)
  ;; Note: This will also return T for fixnums since they share i31ref representation
  (append
   (compile-to-instructions (first args) env)
   '((:ref.test :i31)
     (:if (:result :anyref))
     (:i32.const 1) :ref.i31  ;; T (represented as fixnum 1)
     :else
     (:ref.null :none)  ;; NIL
     :end)))

(defun compile-alpha-char-p (args env)
  "Compile (alpha-char-p char) - test if alphabetic.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "alpha-char-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $alpha_done (:result :anyref))
       ;; Check uppercase A-Z
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alpha_done)
       :else
       ;; Check lowercase a-z
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-digit-char-p (args env)
  "Compile (digit-char-p char [radix]) - test if digit, return weight or NIL.
   Default radix is 10.
   Stack: [] -> [fixnum or NIL]"
  (when (or (< (length args) 1) (> (length args) 2))
    (error "digit-char-p requires 1 or 2 arguments"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32))
        (radix-local (env-add-local env (gensym "RADIX") :i32))
        (digit-local (env-add-local env (gensym "DIGIT") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local))
     ;; Compile radix (default 10)
     (if (second args)
         (append (compile-to-instructions (second args) env)
                 `((:ref.cast :i31) :i31.get_s (:local.set ,radix-local)))
         `((:i32.const 10) (:local.set ,radix-local)))
     `((:block $digit_done (:result :anyref))
       ;; Check '0'-'9' first
       (:local.get ,c-local)
       (:i32.const 48)  ;; '0'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 57)  ;; '9'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; It's a decimal digit
       (:local.get ,c-local)
       (:i32.const 48)
       :i32.sub
       (:local.set ,digit-local)
       ;; Check if digit < radix
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Check 'A'-'Z' for hex digits
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; Uppercase hex: A=10, B=11, ...
       (:local.get ,c-local)
       (:i32.const 55)  ;; 'A' - 10
       :i32.sub
       (:local.set ,digit-local)
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Check 'a'-'z' for hex digits
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.mul
       (:if (:result :anyref))
       ;; Lowercase hex: a=10, b=11, ...
       (:local.get ,c-local)
       (:i32.const 87)  ;; 'a' - 10
       :i32.sub
       (:local.set ,digit-local)
       (:local.get ,digit-local)
       (:local.get ,radix-local)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,digit-local)
       :ref.i31
       (:br $digit_done)
       :else
       (:ref.null :none)
       (:br $digit_done)
       :end
       :else
       ;; Not a digit
       (:ref.null :none)
       :end
       :end
       :end
       :end))))

(defun compile-alphanumericp (args env)
  "Compile (alphanumericp char) - test if alphanumeric.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "alphanumericp requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $alphanum_done (:result :anyref))
       ;; Check uppercase A-Z
       (:local.get ,c-local)
       (:i32.const 65)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alphanum_done)
       :else
       ;; Check lowercase a-z
       (:local.get ,c-local)
       (:i32.const 97)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $alphanum_done)
       :else
       ;; Check digits 0-9
       (:local.get ,c-local)
       (:i32.const 48)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 57)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end
       :end))))

(defun compile-upper-case-p (args env)
  "Compile (upper-case-p char) - test if uppercase letter.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "upper-case-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-lower-case-p (args env)
  "Compile (lower-case-p char) - test if lowercase letter.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "lower-case-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

;;; ============================================================
;;; Extended Character Functions (001-ansi-char-functions)
;;; Phase 16A: ANSI CL Character Functions
;;;
;;; HyperSpec References:
;;; - graphic-char-p: resources/HyperSpec/Body/f_graphi.htm
;;; - standard-char-p: resources/HyperSpec/Body/f_std_ch.htm
;;; - both-case-p: resources/HyperSpec/Body/f_upper_.htm
;;; - char-name: resources/HyperSpec/Body/f_char_n.htm
;;; - name-char: resources/HyperSpec/Body/f_name_c.htm
;;; - digit-char: resources/HyperSpec/Body/f_digit_.htm
;;; - char-int: resources/HyperSpec/Body/f_char_i.htm
;;; ============================================================

(defun compile-graphic-char-p (args env)
  "Compile (graphic-char-p char) - test if printable character.
   Returns T for characters with code 32-126, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "graphic-char-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       ;; Check if char >= 32 (Space) AND char <= 126 (~)
       (:local.get ,c-local)
       (:i32.const 32)  ;; Space
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 126)  ;; ~
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-standard-char-p (args env)
  "Compile (standard-char-p char) - test if standard character.
   Standard characters are: A-Z, a-z, 0-9, space, newline, and punctuation.
   The 96 standard characters per ANSI CL.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "standard-char-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $std_done (:result :anyref))
       ;; Check if newline (code 10) - standard char
       (:local.get ,c-local)
       (:i32.const 10)
       :i32.eq
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $std_done)
       :else
       ;; Check if in range 32-126 (graphic chars minus DEL)
       (:local.get ,c-local)
       (:i32.const 32)
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 126)
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-both-case-p (args env)
  "Compile (both-case-p char) - test if character has both cases.
   Returns T for A-Z and a-z, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "both-case-p requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
       (:block $both_done (:result :anyref))
       ;; Check uppercase A-Z
       (:local.get ,c-local)
       (:i32.const 65)  ;; 'A'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 90)  ;; 'Z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       (:br $both_done)
       :else
       ;; Check lowercase a-z
       (:local.get ,c-local)
       (:i32.const 97)  ;; 'a'
       :i32.ge_s
       (:local.get ,c-local)
       (:i32.const 122)  ;; 'z'
       :i32.le_s
       :i32.and
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end
       :end
       :end))))

(defun compile-char-name (args env)
  "Compile (char-name char) - return name string for named characters.
   Returns string for Space, Newline, Tab, Return, Page, Backspace,
   Rubout, Linefeed, Null. Returns NIL for unnamed characters.
   Stack: [] -> [string or NIL]"
  (when (/= (length args) 1)
    (error "char-name requires exactly 1 argument"))
  (let ((c-local (env-add-local env (gensym "CHAR") :i32)))
    (with-instruction-collector
      ;; Compile argument and extract character code
      (emit* (compile-to-instructions (first args) env))
      (emit* `((:ref.cast :i31) :i31.get_s (:local.set ,c-local)
               (:block $name_done (:result :anyref))))
      ;; Check each named character and return its name
      ;; Null (0)
      (emit* `((:local.get ,c-local) (:i32.const 0) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Null"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Backspace (8)
      (emit* `((:local.get ,c-local) (:i32.const 8) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Backspace"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Tab (9)
      (emit* `((:local.get ,c-local) (:i32.const 9) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Tab"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Newline/Linefeed (10)
      (emit* `((:local.get ,c-local) (:i32.const 10) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Newline"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Page (12)
      (emit* `((:local.get ,c-local) (:i32.const 12) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Page"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Return (13)
      (emit* `((:local.get ,c-local) (:i32.const 13) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Return"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Space (32)
      (emit* `((:local.get ,c-local) (:i32.const 32) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Space"))
      (emit '(:br $name_done))
      (emit :else)
      ;; Rubout/DEL (127)
      (emit* `((:local.get ,c-local) (:i32.const 127) :i32.eq (:if (:result :anyref))))
      (emit* (compile-string-literal "Rubout"))
      (emit :else)
      ;; Not a named character
      (emit :ref.null :none)
      ;; Close all ifs
      (emit* '(:end :end :end :end :end :end :end :end :end)))))

(defun compile-name-char (args env)
  "Compile (name-char name) - return character for given name.
   Case-insensitive. Returns NIL for invalid names.
   Supports: Space, Newline, Tab, Return, Page, Backspace, Rubout, Linefeed, Null.
   Stack: [] -> [character or NIL]"
  (when (/= (length args) 1)
    (error "name-char requires exactly 1 argument"))
  ;; For name-char, we compile to a series of string-equal calls.
  ;; This uses the existing compile-string-equal infrastructure.
  ;; We compile each comparison as a full expression.
  (let ((name-local (env-add-local env (gensym "NAME"))))
    (with-instruction-collector
      ;; Compile the name argument
      (emit* (compile-to-instructions (first args) env))
      (emit `(:local.set ,name-local))
      (emit '(:block $namechar_done (:result :anyref)))
      ;; Check each character name (case-insensitive)
      (emit* (compile-name-char-check env name-local "Space" 32))
      (emit* (compile-name-char-check env name-local "Newline" 10))
      (emit* (compile-name-char-check env name-local "Tab" 9))
      (emit* (compile-name-char-check env name-local "Return" 13))
      (emit* (compile-name-char-check env name-local "Page" 12))
      (emit* (compile-name-char-check env name-local "Backspace" 8))
      (emit* (compile-name-char-check env name-local "Rubout" 127))
      (emit* (compile-name-char-check env name-local "Linefeed" 10))
      (emit* (compile-name-char-check env name-local "Null" 0))
      ;; Unknown name - return NIL
      (emit :ref.null :none)
      (emit :end))))

(defun compile-name-char-check (env name-local expected-name char-code)
  "Generate code to check if name-local equals expected-name (case-insensitive).
   If match, returns char-code as character and branches to $namechar_done.
   Uses a simple flag-based comparison without nested result types."
  (let ((string-type clysm/compiler/codegen/gc-types:+type-string+)
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (match-local (env-add-local env (gensym "MATCH") :i32))
        (byte1-local (env-add-local env (gensym "BYTE1") :i32))
        (byte2-local (env-add-local env (gensym "BYTE2") :i32))
        (str2-local (env-add-local env (gensym "STR2")))
        (expected-len (length expected-name)))
    `(;; Get input string length and check against expected
      (:local.get ,name-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Initialize match flag to 1 (true), will be set to 0 on mismatch
      (:i32.const 1)
      (:local.set ,match-local)
      ;; Check length first
      (:local.get ,len-local)
      (:i32.const ,expected-len)
      :i32.ne
      (:if nil)
      ;; Length mismatch
      (:i32.const 0)
      (:local.set ,match-local)
      :end
      ;; Only compare bytes if lengths matched
      (:local.get ,match-local)
      (:if nil)
      ;; Build expected string
      ,@(compile-string-literal expected-name)
      (:local.set ,str2-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; Loop comparing bytes
      (:block $check_exit)
      (:loop $check_loop)
      ;; Check if done comparing
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $check_exit)
      ;; Get byte from input string
      (:local.get ,name-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      ;; Get byte from expected string
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Upcase byte1 if lowercase (a-z -> A-Z)
      (:local.get ,byte1-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte1-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      (:local.get ,byte1-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte1-local)
      :end
      ;; Upcase byte2 if lowercase (a-z -> A-Z)
      (:local.get ,byte2-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte2-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      (:local.get ,byte2-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte2-local)
      :end
      ;; Compare uppercased bytes
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      ;; Mismatch - set flag to 0 and exit loop
      (:i32.const 0)
      (:local.set ,match-local)
      (:br $check_exit)
      :end
      ;; Increment index and continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $check_loop)
      :end  ; end loop
      :end  ; end block $check_exit
      :end  ; end outer if (match flag check)
      ;; Check if we matched - if so, return the character
      (:local.get ,match-local)
      (:if nil)
      (:i32.const ,char-code)
      :ref.i31
      (:br $namechar_done)
      :end)))

(defun compile-digit-char (args env)
  "Compile (digit-char weight &optional radix) - convert weight to digit character.
   Returns character for weight 0-35 if weight < radix, NIL otherwise.
   Default radix is 10. For radix > 10, uses uppercase A-Z.
   Stack: [] -> [character or NIL]"
  (when (or (< (length args) 1) (> (length args) 2))
    (error "digit-char requires 1 or 2 arguments"))
  (let ((weight-local (env-add-local env (gensym "WEIGHT") :i32))
        (radix-local (env-add-local env (gensym "RADIX") :i32)))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.cast :i31) :i31.get_s (:local.set ,weight-local))
     ;; Compile radix (default 10)
     (if (second args)
         (append (compile-to-instructions (second args) env)
                 `((:ref.cast :i31) :i31.get_s (:local.set ,radix-local)))
         `((:i32.const 10) (:local.set ,radix-local)))
     `((:block $digit_done (:result :anyref))
       ;; Check weight >= 0
       (:local.get ,weight-local)
       (:i32.const 0)
       :i32.lt_s
       (:if (:result :anyref))
       (:ref.null :none)
       (:br $digit_done)
       :else
       ;; Check weight < radix
       (:local.get ,weight-local)
       (:local.get ,radix-local)
       :i32.ge_s
       (:if (:result :anyref))
       (:ref.null :none)
       (:br $digit_done)
       :else
       ;; Check if weight < 10 (return '0' + weight)
       (:local.get ,weight-local)
       (:i32.const 10)
       :i32.lt_s
       (:if (:result :anyref))
       (:local.get ,weight-local)
       (:i32.const 48)  ;; '0'
       :i32.add
       :ref.i31
       :else
       ;; weight >= 10, return 'A' + (weight - 10)
       (:local.get ,weight-local)
       (:i32.const 10)
       :i32.sub
       (:i32.const 65)  ;; 'A'
       :i32.add
       :ref.i31
       :end
       :end
       :end
       :end))))

(defun compile-char-int (args env)
  "Compile (char-int char) - return integer encoding of character.
   In this implementation, equivalent to char-code (returns Unicode code point).
   Stack: [] -> [integer]"
  (when (/= (length args) 1)
    (error "char-int requires exactly 1 argument"))
  ;; Simply extract the i32 code point and return as i31ref
  (append
   (compile-to-instructions (first args) env)
   `((:ref.cast :i31) :i31.get_s :ref.i31)))

(defun compile-stringp (args env)
  "Compile (stringp obj) - test if object is a string.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 1)
    (error "stringp requires exactly 1 argument"))
  (let ((string-type clysm/compiler/codegen/gc-types:+type-string+))
    (append
     (compile-to-instructions (first args) env)
     `((:ref.test ,(list :ref string-type))
       (:if (:result :anyref))
       (:i32.const 1) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-string-char (args env)
  "Compile (char string index) or (schar string index) - get character at index.
   For UTF-8 strings, we need to iterate to find the byte offset.
   Stack: [] -> [character]"
  (when (/= (length args) 2)
    (error "char/schar requires exactly 2 arguments"))
  (let ((str-local (env-add-local env (gensym "STR")))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-idx-local (env-add-local env (gensym "BYTE-IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHAR-COUNT") :i32))
        (byte-len-local (env-add-local env (gensym "BYTE-LEN") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile string and index
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str-local)
      ,@(compile-to-instructions (second args) env)
      ;; Extract index from i31ref fixnum
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,idx-local)
      ;; Get byte length
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,byte-len-local)
      ;; Initialize: byte-idx=0, char-count=0
      (:i32.const 0)
      (:local.set ,byte-idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Loop to find the byte position of the idx-th character
      ;; We look for the start of each character and check if it's the one we want
      (:block $find_done)
      (:loop $find_loop)
      ;; Check if byte-idx >= byte-len (out of bounds)
      (:local.get ,byte-idx-local)
      (:local.get ,byte-len-local)
      :i32.ge_u
      (:br_if $find_done)
      ;; Get byte at current position
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if this byte is a leading byte (not a continuation byte)
      ;; Continuation bytes: 10xxxxxx (0x80-0xBF)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      ;; This is a leading byte (start of a character)
      ;; Check if char-count == idx (this is the character we want)
      (:local.get ,char-count-local)
      (:local.get ,idx-local)
      :i32.eq
      (:br_if $find_done)
      ;; Not the one we want, increment char count and continue
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Increment byte-idx
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,byte-idx-local)
      (:br $find_loop)
      :end  ; loop
      :end  ; block
      ;; Now byte-idx points to the start of the idx-th character
      ;; Decode the UTF-8 character at this position
      ;; Get the first byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Determine character length and decode
      ;; ASCII: 0xxxxxxx (0x00-0x7F) -> 1 byte
      (:local.get ,byte-local)
      (:i32.const #x80)
      :i32.lt_u
      (:if (:result :i32))
      ;; ASCII byte, codepoint = byte
      (:local.get ,byte-local)
      :else
      ;; Multi-byte sequence
      ;; 2-byte: 110xxxxx (0xC0-0xDF)
      (:local.get ,byte-local)
      (:i32.const #xE0)
      :i32.lt_u
      (:if (:result :i32))
      ;; 2-byte sequence: 110xxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x1F)  ; mask lower 5 bits
      :i32.and
      (:i32.const 6)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)  ; mask lower 6 bits
      :i32.and
      :i32.or
      :else
      ;; 3-byte: 1110xxxx (0xE0-0xEF)
      (:local.get ,byte-local)
      (:i32.const #xF0)
      :i32.lt_u
      (:if (:result :i32))
      ;; 3-byte sequence: 1110xxxx 10xxxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x0F)  ; mask lower 4 bits
      :i32.and
      (:i32.const 12)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 6)
      :i32.shl
      :i32.or
      ;; Get third byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 2)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      :i32.or
      :else
      ;; 4-byte: 11110xxx (0xF0-0xF7)
      ;; 4-byte sequence: 11110xxx 10xxxxxx 10xxxxxx 10xxxxxx
      (:local.get ,byte-local)
      (:i32.const #x07)  ; mask lower 3 bits
      :i32.and
      (:i32.const 18)
      :i32.shl
      ;; Get second byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 1)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 12)
      :i32.shl
      :i32.or
      ;; Get third byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 2)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      (:i32.const 6)
      :i32.shl
      :i32.or
      ;; Get fourth byte
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,byte-idx-local)
      (:i32.const 3)
      :i32.add
      (:array.get_u ,string-type)
      (:i32.const #x3F)
      :i32.and
      :i32.or
      :end  ; close 3-byte vs 4-byte if
      :end  ; close 2-byte vs 3-byte+ if
      :end  ; close ASCII vs multi-byte if
      ;; Result is the codepoint as i32, convert to character (i31ref)
      :ref.i31)))

;;; String Comparison Functions (008-character-string)
;;; For UTF-8 strings, byte-by-byte comparison is valid for equality
;;; and lexicographic ordering since UTF-8 preserves codepoint order.

(defun compile-string= (args env)
  "Compile (string= str1 str2) - test string equality.
   Returns T if strings are equal, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "string= requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; Assume equal (T), will set to NIL if we find difference
      (:i32.const 1) :ref.i31
      (:local.set ,result-local)
      ;; Initialize idx
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; First check if lengths differ
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      ;; Lengths differ, set result to NIL
      (:ref.null :none)
      (:local.set ,result-local)
      :else
      ;; Same length, compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= len (all bytes compared equal)
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:br_if $cmp_done)
      ;; Compare bytes at idx
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      :i32.ne
      (:if nil)
      ;; Bytes differ, set result to NIL and exit
      (:ref.null :none)
      (:local.set ,result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal, continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      :end  ; length check if
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string/= (args env)
  "Compile (string/= str1 str2) - test string inequality.
   Returns the index of first differing character, or NIL if equal.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string/= requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (min-len-local (env-add-local env (gensym "MINLEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; min-len = min(len1, len2)
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.lt_u
      (:if (:result :i32))
      (:local.get ,len1-local)
      :else
      (:local.get ,len2-local)
      :end
      (:local.set ,min-len-local)
      ;; Initialize: assume equal (NIL result), idx=0, char-count=0
      (:ref.null :none)
      (:local.set ,result-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= min-len
      (:local.get ,idx-local)
      (:local.get ,min-len-local)
      :i32.ge_u
      (:if nil)
      ;; Reached end of shorter string
      ;; If lengths differ, set result to char-count
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end
      (:br $cmp_done)
      :end
      ;; Get byte from str1
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Compare bytes
      (:local.get ,byte-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      :i32.ne
      (:if nil)
      ;; Bytes differ, set result to char-count and exit
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal - if leading byte, increment char-count
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string< (args env)
  "Compile (string< str1 str2) - test if str1 < str2 lexicographically.
   Returns index of first difference if str1 < str2, NIL otherwise.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :lt))

(defun compile-string> (args env)
  "Compile (string> str1 str2) - test if str1 > str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :gt))

(defun compile-string<= (args env)
  "Compile (string<= str1 str2) - test if str1 <= str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :le))

(defun compile-string>= (args env)
  "Compile (string>= str1 str2) - test if str1 >= str2 lexicographically.
   Stack: [] -> [index or NIL]"
  (compile-string-compare args env :ge))

;;; ============================================================
;;; Case-insensitive string comparison functions
;;; ============================================================

(defun compile-string-equal (args env)
  "Compile (string-equal str1 str2) - case-insensitive string equality.
   Returns T if strings are equal ignoring case, NIL otherwise.
   Stack: [] -> [T or NIL]"
  (when (/= (length args) 2)
    (error "string-equal requires exactly 2 arguments"))
  (compile-string-compare-ci args env :equal))

(defun compile-string-lessp (args env)
  "Compile (string-lessp str1 str2) - case-insensitive string less-than.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-lessp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :lt))

(defun compile-string-greaterp (args env)
  "Compile (string-greaterp str1 str2) - case-insensitive string greater-than.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-greaterp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :gt))

(defun compile-string-not-lessp (args env)
  "Compile (string-not-lessp str1 str2) - case-insensitive string >=.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-lessp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :ge))

(defun compile-string-not-greaterp (args env)
  "Compile (string-not-greaterp str1 str2) - case-insensitive string <=.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-greaterp requires exactly 2 arguments"))
  (compile-string-compare-ci args env :le))

(defun compile-string-not-equal (args env)
  "Compile (string-not-equal str1 str2) - case-insensitive string inequality.
   Returns mismatch index or NIL.
   Stack: [] -> [index or NIL]"
  (when (/= (length args) 2)
    (error "string-not-equal requires exactly 2 arguments"))
  (compile-string-compare-ci args env :not-equal))

(defun compile-upcase-byte (byte-local temp-local)
  "Generate instructions to convert a byte in byte-local to uppercase.
   Modifies byte-local in place. Uses temp-local as scratch.
   Stack: [] -> []"
  `(;; Check if byte is lowercase a-z (97-122)
    (:local.get ,byte-local)
    (:i32.const 97)  ;; 'a'
    :i32.ge_u
    (:local.get ,byte-local)
    (:i32.const 122) ;; 'z'
    :i32.le_u
    :i32.and
    (:if nil)
    ;; Convert to uppercase by subtracting 32
    (:local.get ,byte-local)
    (:i32.const 32)
    :i32.sub
    (:local.set ,byte-local)
    :end))

(defun compile-string-compare-ci (args env comparison)
  "Case-insensitive string comparison.
   COMPARISON is one of :equal, :not-equal, :lt, :gt, :le, :ge"
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte1-local (env-add-local env (gensym "BYTE1") :i32))
        (byte2-local (env-add-local env (gensym "BYTE2") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths (byte lengths)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; Initialize
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      (:ref.null :none)  ;; Default result is NIL
      (:local.set ,result-local)
      ;; Compare byte by byte with case folding
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if we've reached the end of str1
      (:local.get ,idx-local)
      (:local.get ,len1-local)
      :i32.ge_u
      (:if nil)
      ;; str1 ended - check str2
      (:local.get ,idx-local)
      (:local.get ,len2-local)
      :i32.ge_u
      (:if nil)
      ;; Both ended at same length - strings are equal
      ,@(compile-string-compare-ci-at-end-equal comparison char-count-local result-local)
      :else
      ;; str1 ended but str2 continues - str1 < str2
      ,@(compile-string-compare-ci-at-end-prefix comparison :str1-shorter char-count-local result-local)
      :end
      (:br $cmp_done)
      :end
      ;; str1 not ended - check if str2 ended
      (:local.get ,idx-local)
      (:local.get ,len2-local)
      :i32.ge_u
      (:if nil)
      ;; str2 ended but str1 continues - str1 > str2
      ,@(compile-string-compare-ci-at-end-prefix comparison :str2-shorter char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Both have more bytes - get bytes and convert to uppercase
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Convert both to uppercase for comparison
      ,@(compile-upcase-byte byte1-local byte2-local)  ;; uses byte2-local as temp (we'll restore it)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ,@(compile-upcase-byte byte2-local byte1-local)
      ;; Get byte1 again (may have been modified as temp)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      ,@(compile-upcase-byte byte1-local byte2-local)
      ;; Compare uppercased bytes
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      ;; Bytes differ - determine result
      ,@(compile-string-compare-ci-at-diff comparison byte1-local byte2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Count leading byte for character count
      ;; Leading bytes have top 2 bits != 10 (i.e., not continuation byte)
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:i32.const 192)  ;; 0xC0
      :i32.and
      (:i32.const 128)  ;; 0x80
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Next byte
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      (:local.get ,result-local))))

(defun compile-string-compare-ci-at-end-equal (comparison char-count-local result-local)
  "Generate code for when both strings ended at the same point (equal)."
  (case comparison
    (:equal
     ;; Strings are equal - return T
     `((:i32.const 1) :ref.i31 (:local.set ,result-local)))
    (:not-equal
     ;; Strings are equal - return NIL (already set)
     nil)
    ((:lt :gt)
     ;; Strings are equal - return NIL for < and > (already set)
     nil)
    ((:le :ge)
     ;; Strings are equal - return char-count for <= and >=
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))))

(defun compile-string-compare-ci-at-end-prefix (comparison which-shorter char-count-local result-local)
  "Generate code when one string is a prefix of the other."
  (case comparison
    (:equal
     ;; Not equal - return NIL (already set)
     nil)
    (:not-equal
     ;; Not equal - return char-count
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))
    (:lt
     ;; str1 < str2 iff str1 is shorter
     (if (eq which-shorter :str1-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:gt
     ;; str1 > str2 iff str2 is shorter
     (if (eq which-shorter :str2-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:le
     ;; str1 <= str2 iff str1 is shorter or equal (str1 shorter means true)
     (if (eq which-shorter :str1-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))
    (:ge
     ;; str1 >= str2 iff str2 is shorter or equal (str2 shorter means true)
     (if (eq which-shorter :str2-shorter)
         `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local))
         nil))))

(defun compile-string-compare-ci-at-diff (comparison byte1-local byte2-local char-count-local result-local)
  "Generate code for when bytes differ at comparison point."
  (case comparison
    (:equal
     ;; Not equal - return NIL (already set)
     nil)
    (:not-equal
     ;; Not equal - return char-count
     `((:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)))
    (:lt
     ;; str1 < str2 iff byte1 < byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:gt
     ;; str1 > str2 iff byte1 > byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:le
     ;; str1 <= str2 iff byte1 <= byte2 (since they differ, this means byte1 < byte2)
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))
    (:ge
     ;; str1 >= str2 iff byte1 >= byte2 (since they differ, this means byte1 > byte2)
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if nil)
       (:local.get ,char-count-local) :ref.i31 (:local.set ,result-local)
       :end))))

(defun compile-string-compare (args env comparison)
  "Generic string comparison for <, >, <=, >=.
   COMPARISON is one of :lt, :gt, :le, :ge"
  (when (/= (length args) 2)
    (error "String comparison requires exactly 2 arguments"))
  (let ((str1-local (env-add-local env (gensym "STR1")))
        (str2-local (env-add-local env (gensym "STR2")))
        (len1-local (env-add-local env (gensym "LEN1") :i32))
        (len2-local (env-add-local env (gensym "LEN2") :i32))
        (min-len-local (env-add-local env (gensym "MINLEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
        (byte1-local (env-add-local env (gensym "BYTE1") :i32))
        (byte2-local (env-add-local env (gensym "BYTE2") :i32))
        (result-local (env-add-local env (gensym "RESULT")))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Compile both strings
      ,@(compile-to-instructions (first args) env)
      (:local.set ,str1-local)
      ,@(compile-to-instructions (second args) env)
      (:local.set ,str2-local)
      ;; Get lengths
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len2-local)
      ;; min-len = min(len1, len2)
      (:local.get ,len1-local)
      (:local.get ,len2-local)
      :i32.lt_u
      (:if (:result :i32))
      (:local.get ,len1-local)
      :else
      (:local.get ,len2-local)
      :end
      (:local.set ,min-len-local)
      ;; Initialize: assume false (NIL), idx=0, char-count=0
      (:ref.null :none)
      (:local.set ,result-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      ;; Compare byte by byte
      (:block $cmp_done)
      (:loop $cmp_loop)
      ;; Check if idx >= min-len
      (:local.get ,idx-local)
      (:local.get ,min-len-local)
      :i32.ge_u
      (:if nil)
      ;; Reached end of shorter string
      ;; Result depends on comparison type and relative lengths
      ,@(compile-string-compare-at-end-v2 comparison len1-local len2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Get bytes from both strings
      (:local.get ,str1-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte1-local)
      (:local.get ,str2-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte2-local)
      ;; Compare bytes
      (:local.get ,byte1-local)
      (:local.get ,byte2-local)
      :i32.ne
      (:if nil)
      ;; Bytes differ - determine result based on comparison
      ,@(compile-string-compare-at-diff-v2 comparison byte1-local byte2-local char-count-local result-local)
      (:br $cmp_done)
      :end
      ;; Bytes equal - if leading byte, increment char-count
      (:local.get ,byte1-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      ;; Continue
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cmp_loop)
      :end  ; loop
      :end  ; block
      ;; Return result
      (:local.get ,result-local))))

(defun compile-string-compare-at-end-v2 (comparison len1-local len2-local char-count-local result-local)
  "Generate code for string comparison result when one string is a prefix of the other.
   Sets result-local to char-count (as i31) if condition is true, leaves it as NIL otherwise."
  (let ((condition-instr (case comparison
                           (:lt :i32.lt_u)  ; str1 < str2 iff len1 < len2
                           (:gt :i32.gt_u)  ; str1 > str2 iff len1 > len2
                           (:le :i32.le_u)  ; str1 <= str2 iff len1 <= len2
                           (:ge :i32.ge_u)))) ; str1 >= str2 iff len1 >= len2
    `((:local.get ,len1-local)
      (:local.get ,len2-local)
      ,condition-instr
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end)))

(defun compile-string-compare-at-end (comparison len1-local len2-local char-count-local)
  "Generate code for string comparison result when one string is a prefix of the other."
  (case comparison
    (:lt
     ;; str1 < str2 iff len1 < len2 (str1 is proper prefix)
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.lt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31  ; return mismatch position
       :else
       (:ref.null :none)
       :end))
    (:gt
     ;; str1 > str2 iff len1 > len2 (str2 is proper prefix)
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.gt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:le
     ;; str1 <= str2 iff len1 <= len2
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.le_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:ge
     ;; str1 >= str2 iff len1 >= len2
     `((:local.get ,len1-local)
       (:local.get ,len2-local)
       :i32.ge_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))))

(defun compile-string-compare-at-diff-v2 (comparison byte1-local byte2-local char-count-local result-local)
  "Generate code for string comparison result when bytes differ.
   Sets result-local to char-count (as i31) if condition is true, leaves it as NIL otherwise."
  (let ((condition-instr (case comparison
                           (:lt :i32.lt_u)  ; str1 < str2 iff byte1 < byte2
                           (:gt :i32.gt_u)  ; str1 > str2 iff byte1 > byte2
                           (:le :i32.lt_u)  ; str1 <= str2 iff byte1 < byte2 (NOT <=, since bytes differ!)
                           (:ge :i32.gt_u)))) ; str1 >= str2 iff byte1 > byte2 (NOT >=, since bytes differ!)
    `((:local.get ,byte1-local)
      (:local.get ,byte2-local)
      ,condition-instr
      (:if nil)
      (:local.get ,char-count-local)
      :ref.i31
      (:local.set ,result-local)
      :end)))

(defun compile-string-compare-at-diff (comparison byte1-local byte2-local char-count-local)
  "Generate code for string comparison result when bytes differ."
  (case comparison
    (:lt
     ;; str1 < str2 iff byte1 < byte2
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.lt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:gt
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.gt_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:le
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.le_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))
    (:ge
     `((:local.get ,byte1-local)
       (:local.get ,byte2-local)
       :i32.ge_u
       (:if (:result :anyref))
       (:local.get ,char-count-local) :ref.i31
       :else
       (:ref.null :none)
       :end))))

;;; ============================================================
;;; String Generation/Conversion (008-character-string Phase 6)
;;; ============================================================

;; Note: extract-keyword-from-ast is defined in Hash Table Operations section above

(defun compile-make-string (args env)
  "Compile (make-string size &key initial-element) - create new string.
   Stack: [] -> [string]
   Note: Currently only supports ASCII characters. Multi-byte UTF-8 will be added later."
  (when (< (length args) 1)
    (error "make-string requires at least 1 argument (size)"))
  (let* ((size-arg (first args))
         (rest-args (rest args))
         ;; Parse keyword arguments
         (initial-element nil))
    ;; Look for :initial-element keyword
    (loop while rest-args do
      (let* ((key-ast (first rest-args))
             (key (extract-keyword-from-ast key-ast))
             (val (second rest-args)))
        (cond
          ((eq key :initial-element)
           (setf initial-element val))
          (t (error "Unknown keyword argument to make-string: ~A" key-ast)))
        (setf rest-args (cddr rest-args))))
    ;; Generate code - simple version that assumes ASCII (1 byte per char)
    (let ((size-local (env-add-local env (gensym "SIZE") :i32))
          (char-local (env-add-local env (gensym "CHAR") :i32))
          (str-local (env-add-local env (gensym "STR")))
          (idx-local (env-add-local env (gensym "IDX") :i32))
          (string-type clysm/compiler/codegen/gc-types:+type-string+))
      `(;; Get size
        ,@(compile-to-instructions size-arg env)
        (:ref.cast :i31)
        :i31.get_s
        (:local.set ,size-local)
        ;; Get initial char code (default #\Null = 0)
        ,@(if initial-element
              `(,@(compile-to-instructions initial-element env)
                (:ref.cast :i31)
                :i31.get_s
                (:local.set ,char-local))
              `((:i32.const 0)
                (:local.set ,char-local)))
        ;; Create array with size bytes (assuming ASCII)
        (:local.get ,size-local)
        (:array.new_default ,string-type)
        (:local.set ,str-local)
        ;; Fill array with initial character
        (:i32.const 0)
        (:local.set ,idx-local)
        ;; Loop to fill
        (:block $ms_done)
        (:loop $ms_loop)
        ;; Check if done
        (:local.get ,idx-local)
        (:local.get ,size-local)
        :i32.ge_u
        (:br_if $ms_done)
        ;; Store char byte
        (:local.get ,str-local)
        (:ref.cast ,(list :ref string-type))
        (:local.get ,idx-local)
        (:local.get ,char-local)
        (:array.set ,string-type)
        ;; Increment idx
        (:local.get ,idx-local)
        (:i32.const 1)
        :i32.add
        (:local.set ,idx-local)
        (:br $ms_loop)
        :end  ; loop
        :end  ; block
        ;; Return string
        (:local.get ,str-local)))))

(defun compile-string (args env)
  "Compile (string x) - convert character or symbol to string.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string requires exactly 1 argument"))
  (let ((str-local (env-add-local env (gensym "STR")))
        (char-local (env-add-local env (gensym "CHAR") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    ;; If arg is a character, create a 1-char string
    ;; If arg is already a string, return it
    ;; TODO: Handle symbols (get their print name)
    `(,@(compile-to-instructions (first args) env)
      (:local.set ,str-local)
      ;; Check if already a string
      (:local.get ,str-local)
      (:ref.test ,(list :ref string-type))
      (:if (:result :anyref))
      ;; Already a string - return as-is
      (:local.get ,str-local)
      :else
      ;; Assume it's a character - convert to 1-char string
      (:local.get ,str-local)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,char-local)
      ;; Create 1-element array
      (:i32.const 1)
      (:array.new_default ,string-type)
      (:local.set ,str-local)
      (:local.get ,str-local)
      (:ref.cast ,(list :ref string-type))
      (:i32.const 0)
      (:local.get ,char-local)
      (:array.set ,string-type)
      (:local.get ,str-local)
      :end)))

(defun compile-string-upcase (args env)
  "Compile (string-upcase string) - convert to uppercase.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-upcase requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy and convert
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $up_done)
      (:loop $up_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $up_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if lowercase (a-z = 97-122)
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if (:result :i32))
      ;; Convert to uppercase (subtract 32)
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.sub
      :else
      (:local.get ,byte-local)
      :end
      (:local.set ,byte-local)
      ;; Store in destination
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $up_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-string-downcase (args env)
  "Compile (string-downcase string) - convert to lowercase.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-downcase requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy and convert
      (:i32.const 0)
      (:local.set ,idx-local)
      (:block $down_done)
      (:loop $down_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $down_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if uppercase (A-Z = 65-90)
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:if (:result :i32))
      ;; Convert to lowercase (add 32)
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.add
      :else
      (:local.get ,byte-local)
      :end
      (:local.set ,byte-local)
      ;; Store in destination
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $down_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-string-capitalize (args env)
  "Compile (string-capitalize string) - capitalize first letter of each word.
   Stack: [] -> [string]"
  (when (/= (length args) 1)
    (error "string-capitalize requires exactly 1 argument"))
  (let ((src-local (env-add-local env (gensym "SRC")))
        (dst-local (env-add-local env (gensym "DST")))
        (len-local (env-add-local env (gensym "LEN") :i32))
        (idx-local (env-add-local env (gensym "IDX") :i32))
        (byte-local (env-add-local env (gensym "BYTE") :i32))
        (word-start-local (env-add-local env (gensym "WORDSTART") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions (first args) env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get length
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,len-local)
      ;; Create destination array
      (:local.get ,len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Initialize
      (:i32.const 0)
      (:local.set ,idx-local)
      (:i32.const 1)  ; Start of string is word start
      (:local.set ,word-start-local)
      (:block $cap_done)
      (:loop $cap_loop)
      ;; Check if done
      (:local.get ,idx-local)
      (:local.get ,len-local)
      :i32.ge_u
      (:br_if $cap_done)
      ;; Get byte
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      ;; Check if alphabetic
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      :i32.or
      (:if nil)
      ;; Is alphabetic
      ;; Check if word start
      (:local.get ,word-start-local)
      (:if nil)
      ;; Word start - uppercase
      (:local.get ,byte-local)
      (:i32.const 97)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 122)
      :i32.le_u
      :i32.and
      (:if nil)
      ;; Is lowercase, convert to uppercase
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.sub
      (:local.set ,byte-local)
      :end
      :else
      ;; Not word start - lowercase
      (:local.get ,byte-local)
      (:i32.const 65)
      :i32.ge_u
      (:local.get ,byte-local)
      (:i32.const 90)
      :i32.le_u
      :i32.and
      (:if nil)
      ;; Is uppercase, convert to lowercase
      (:local.get ,byte-local)
      (:i32.const 32)
      :i32.add
      (:local.set ,byte-local)
      :end
      :end
      ;; Clear word-start flag
      (:i32.const 0)
      (:local.set ,word-start-local)
      :else
      ;; Not alphabetic - set word-start flag
      (:i32.const 1)
      (:local.set ,word-start-local)
      :end
      ;; Store byte
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,idx-local)
      (:local.get ,byte-local)
      (:array.set ,string-type)
      ;; Increment
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      (:br $cap_loop)
      :end  ; loop
      :end  ; block
      ;; Return destination
      (:local.get ,dst-local))))

;;; ============================================================
;;; String Trim Functions (001-ansi-string-trim Phase 16B)
;;; HyperSpec: resources/HyperSpec/Body/f_stg_tr.htm
;;; ============================================================

(defun %parse-start-end-args (args)
  "Parse :start and :end keyword arguments from an argument list.
   Returns (values start-ast end-ast remaining-args).
   start-ast defaults to 0, end-ast defaults to nil (meaning string length)."
  (let ((start-ast nil)
        (end-ast nil)
        (remaining args))
    (loop while remaining do
      (let* ((key-ast (first remaining))
             (key (extract-keyword-from-ast key-ast))
             (val (second remaining)))
        (cond
          ((eq key :start)
           (setf start-ast val))
          ((eq key :end)
           (setf end-ast val))
          (t (return)))  ; Not a keyword we recognize
        (setf remaining (cddr remaining))))
    (values start-ast end-ast)))

(defun %generate-in-char-bag-check (byte-local bag-local bag-len-local env)
  "Generate Wasm instructions to check if a byte is in a character bag (string).
   Leaves 1 on stack if byte is in bag, 0 otherwise.
   Assumes bag is already a $string type."
  (let ((bag-idx-local (env-add-local env (gensym "BAGIDX") :i32))
        (bag-byte-local (env-add-local env (gensym "BAGBYTE") :i32))
        (found-local (env-add-local env (gensym "FOUND") :i32))
        (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Initialize found = 0
      (:i32.const 0)
      (:local.set ,found-local)
      ;; Loop through character bag
      (:i32.const 0)
      (:local.set ,bag-idx-local)
      (:block $bag_done)
      (:loop $bag_loop)
      ;; Check if done with bag
      (:local.get ,bag-idx-local)
      (:local.get ,bag-len-local)
      :i32.ge_u
      (:br_if $bag_done)
      ;; Get bag byte
      (:local.get ,bag-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,bag-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,bag-byte-local)
      ;; Compare with target byte
      (:local.get ,byte-local)
      (:local.get ,bag-byte-local)
      :i32.eq
      (:if nil)
      ;; Found match
      (:i32.const 1)
      (:local.set ,found-local)
      (:br $bag_done)
      :end
      ;; Increment bag index
      (:local.get ,bag-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,bag-idx-local)
      (:br $bag_loop)
      :end  ; loop
      :end  ; block
      ;; Leave result on stack
      (:local.get ,found-local))))

(defun %generate-bounds-init (start-ast end-ast start-local end-local len-local env)
  "Generate Wasm instructions to initialize start and end bounds.
   start-ast: AST for start (or nil for 0)
   end-ast: AST for end (or nil for string length)
   Assumes len-local already contains the string length."
  (let ((instrs '()))
    ;; Initialize start
    (if start-ast
        (setf instrs
              `(,@(compile-to-instructions start-ast env)
                (:ref.cast :i31)
                :i31.get_s
                (:local.set ,start-local)))
        (setf instrs
              `((:i32.const 0)
                (:local.set ,start-local))))
    ;; Initialize end
    (if end-ast
        (setf instrs
              `(,@instrs
                ,@(compile-to-instructions end-ast env)
                (:ref.cast :i31)
                :i31.get_s
                (:local.set ,end-local)))
        (setf instrs
              `(,@instrs
                (:local.get ,len-local)
                (:local.set ,end-local))))
    instrs))

;;; ------------------------------------------------------------
;;; string-left-trim, string-right-trim, string-trim
;;; ------------------------------------------------------------

(defun compile-string-left-trim (args env)
  "Compile (string-left-trim character-bag string &key start end).
   Removes characters in bag from the left of string.
   HyperSpec: resources/HyperSpec/Body/f_stg_tr.htm"
  (when (< (length args) 2)
    (error "string-left-trim requires at least 2 arguments"))
  (let* ((bag-arg (first args))
         (string-arg (second args))
         (rest-args (cddr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((bag-local (env-add-local env (gensym "BAG")))
            (bag-len-local (env-add-local env (gensym "BAGLEN") :i32))
            (src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (left-local (env-add-local env (gensym "LEFT") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (dst-local (env-add-local env (gensym "DST")))
            (dst-len-local (env-add-local env (gensym "DSTLEN") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get character bag
          ,@(compile-to-instructions bag-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,bag-local)
          (:local.get ,bag-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,bag-len-local)
          ;; Get source string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get string length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize start/end bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Find left boundary (first char not in bag)
          (:local.get ,start-local)
          (:local.set ,left-local)
          (:block $left_done)
          (:loop $left_loop)
          ;; Check if done (left >= end)
          (:local.get ,left-local)
          (:local.get ,end-local)
          :i32.ge_u
          (:br_if $left_done)
          ;; Get current byte
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,left-local)
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ;; Check if byte is in bag
          ,@(%generate-in-char-bag-check byte-local bag-local bag-len-local env)
          ;; If not in bag, done
          :i32.eqz
          (:br_if $left_done)
          ;; Move left boundary
          (:local.get ,left-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,left-local)
          (:br $left_loop)
          :end  ; loop
          :end  ; block
          ;; Create result string (from left to end)
          (:local.get ,end-local)
          (:local.get ,left-local)
          :i32.sub
          (:local.set ,dst-len-local)
          (:local.get ,dst-len-local)
          (:array.new_default ,string-type)
          (:local.set ,dst-local)
          ;; Copy bytes
          (:i32.const 0)
          (:local.set ,idx-local)
          (:block $copy_done)
          (:loop $copy_loop)
          (:local.get ,idx-local)
          (:local.get ,dst-len-local)
          :i32.ge_u
          (:br_if $copy_done)
          ;; Copy byte
          (:local.get ,dst-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,left-local)
          (:local.get ,idx-local)
          :i32.add
          (:array.get_u ,string-type)
          (:array.set ,string-type)
          ;; Increment
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $copy_loop)
          :end  ; loop
          :end  ; block
          ;; Return result
          (:local.get ,dst-local))))))

(defun compile-string-right-trim (args env)
  "Compile (string-right-trim character-bag string &key start end).
   Removes characters in bag from the right of string.
   HyperSpec: resources/HyperSpec/Body/f_stg_tr.htm"
  (when (< (length args) 2)
    (error "string-right-trim requires at least 2 arguments"))
  (let* ((bag-arg (first args))
         (string-arg (second args))
         (rest-args (cddr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((bag-local (env-add-local env (gensym "BAG")))
            (bag-len-local (env-add-local env (gensym "BAGLEN") :i32))
            (src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (right-local (env-add-local env (gensym "RIGHT") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (dst-local (env-add-local env (gensym "DST")))
            (dst-len-local (env-add-local env (gensym "DSTLEN") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get character bag
          ,@(compile-to-instructions bag-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,bag-local)
          (:local.get ,bag-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,bag-len-local)
          ;; Get source string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get string length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize start/end bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Find right boundary (last char not in bag, starting from end-1)
          (:local.get ,end-local)
          (:local.set ,right-local)
          (:block $right_done)
          (:loop $right_loop)
          ;; Check if done (right <= start)
          (:local.get ,right-local)
          (:local.get ,start-local)
          :i32.le_u
          (:br_if $right_done)
          ;; Get current byte (right-1)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,right-local)
          (:i32.const 1)
          :i32.sub
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ;; Check if byte is in bag
          ,@(%generate-in-char-bag-check byte-local bag-local bag-len-local env)
          ;; If not in bag, done
          :i32.eqz
          (:br_if $right_done)
          ;; Move right boundary
          (:local.get ,right-local)
          (:i32.const 1)
          :i32.sub
          (:local.set ,right-local)
          (:br $right_loop)
          :end  ; loop
          :end  ; block
          ;; Create result string (from start to right)
          (:local.get ,right-local)
          (:local.get ,start-local)
          :i32.sub
          (:local.set ,dst-len-local)
          (:local.get ,dst-len-local)
          (:array.new_default ,string-type)
          (:local.set ,dst-local)
          ;; Copy bytes
          (:i32.const 0)
          (:local.set ,idx-local)
          (:block $copy_done)
          (:loop $copy_loop)
          (:local.get ,idx-local)
          (:local.get ,dst-len-local)
          :i32.ge_u
          (:br_if $copy_done)
          ;; Copy byte
          (:local.get ,dst-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,start-local)
          (:local.get ,idx-local)
          :i32.add
          (:array.get_u ,string-type)
          (:array.set ,string-type)
          ;; Increment
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $copy_loop)
          :end  ; loop
          :end  ; block
          ;; Return result
          (:local.get ,dst-local))))))

(defun compile-string-trim (args env)
  "Compile (string-trim character-bag string &key start end).
   Removes characters in bag from both ends of string.
   HyperSpec: resources/HyperSpec/Body/f_stg_tr.htm"
  (when (< (length args) 2)
    (error "string-trim requires at least 2 arguments"))
  (let* ((bag-arg (first args))
         (string-arg (second args))
         (rest-args (cddr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((bag-local (env-add-local env (gensym "BAG")))
            (bag-len-local (env-add-local env (gensym "BAGLEN") :i32))
            (src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (left-local (env-add-local env (gensym "LEFT") :i32))
            (right-local (env-add-local env (gensym "RIGHT") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (dst-local (env-add-local env (gensym "DST")))
            (dst-len-local (env-add-local env (gensym "DSTLEN") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get character bag
          ,@(compile-to-instructions bag-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,bag-local)
          (:local.get ,bag-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,bag-len-local)
          ;; Get source string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get string length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize start/end bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Find left boundary (first char not in bag)
          (:local.get ,start-local)
          (:local.set ,left-local)
          (:block $left_done)
          (:loop $left_loop)
          (:local.get ,left-local)
          (:local.get ,end-local)
          :i32.ge_u
          (:br_if $left_done)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,left-local)
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ,@(%generate-in-char-bag-check byte-local bag-local bag-len-local env)
          :i32.eqz
          (:br_if $left_done)
          (:local.get ,left-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,left-local)
          (:br $left_loop)
          :end
          :end
          ;; Find right boundary (last char not in bag)
          (:local.get ,end-local)
          (:local.set ,right-local)
          (:block $right_done)
          (:loop $right_loop)
          (:local.get ,right-local)
          (:local.get ,left-local)
          :i32.le_u
          (:br_if $right_done)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,right-local)
          (:i32.const 1)
          :i32.sub
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ,@(%generate-in-char-bag-check byte-local bag-local bag-len-local env)
          :i32.eqz
          (:br_if $right_done)
          (:local.get ,right-local)
          (:i32.const 1)
          :i32.sub
          (:local.set ,right-local)
          (:br $right_loop)
          :end
          :end
          ;; Create result string (from left to right)
          (:local.get ,right-local)
          (:local.get ,left-local)
          :i32.sub
          (:local.set ,dst-len-local)
          (:local.get ,dst-len-local)
          (:array.new_default ,string-type)
          (:local.set ,dst-local)
          ;; Copy bytes
          (:i32.const 0)
          (:local.set ,idx-local)
          (:block $copy_done)
          (:loop $copy_loop)
          (:local.get ,idx-local)
          (:local.get ,dst-len-local)
          :i32.ge_u
          (:br_if $copy_done)
          (:local.get ,dst-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,left-local)
          (:local.get ,idx-local)
          :i32.add
          (:array.get_u ,string-type)
          (:array.set ,string-type)
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $copy_loop)
          :end
          :end
          ;; Return result
          (:local.get ,dst-local))))))

;;; ------------------------------------------------------------
;;; nstring-upcase, nstring-downcase, nstring-capitalize (destructive)
;;; HyperSpec: resources/HyperSpec/Body/f_stg_up.htm
;;; ------------------------------------------------------------

(defun compile-nstring-upcase (args env)
  "Compile (nstring-upcase string &key start end).
   Destructively converts string to uppercase.
   Returns the same string object."
  (when (< (length args) 1)
    (error "nstring-upcase requires at least 1 argument"))
  (let* ((string-arg (first args))
         (rest-args (cdr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Convert in place
          (:local.get ,start-local)
          (:local.set ,idx-local)
          (:block $nup_done)
          (:loop $nup_loop)
          (:local.get ,idx-local)
          (:local.get ,end-local)
          :i32.ge_u
          (:br_if $nup_done)
          ;; Get byte
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ;; Check if lowercase (a-z = 97-122)
          (:local.get ,byte-local)
          (:i32.const 97)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 122)
          :i32.le_u
          :i32.and
          (:if nil)
          ;; Convert to uppercase and store
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,byte-local)
          (:i32.const 32)
          :i32.sub
          (:array.set ,string-type)
          :end
          ;; Increment
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $nup_loop)
          :end
          :end
          ;; Return same string
          (:local.get ,src-local))))))

(defun compile-nstring-downcase (args env)
  "Compile (nstring-downcase string &key start end).
   Destructively converts string to lowercase.
   Returns the same string object."
  (when (< (length args) 1)
    (error "nstring-downcase requires at least 1 argument"))
  (let* ((string-arg (first args))
         (rest-args (cdr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Convert in place
          (:local.get ,start-local)
          (:local.set ,idx-local)
          (:block $ndown_done)
          (:loop $ndown_loop)
          (:local.get ,idx-local)
          (:local.get ,end-local)
          :i32.ge_u
          (:br_if $ndown_done)
          ;; Get byte
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ;; Check if uppercase (A-Z = 65-90)
          (:local.get ,byte-local)
          (:i32.const 65)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 90)
          :i32.le_u
          :i32.and
          (:if nil)
          ;; Convert to lowercase and store
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,byte-local)
          (:i32.const 32)
          :i32.add
          (:array.set ,string-type)
          :end
          ;; Increment
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $ndown_loop)
          :end
          :end
          ;; Return same string
          (:local.get ,src-local))))))

(defun compile-nstring-capitalize (args env)
  "Compile (nstring-capitalize string &key start end).
   Destructively capitalizes each word (first char uppercase, rest lowercase).
   Returns the same string object."
  (when (< (length args) 1)
    (error "nstring-capitalize requires at least 1 argument"))
  (let* ((string-arg (first args))
         (rest-args (cdr args)))
    (multiple-value-bind (start-ast end-ast)
        (%parse-start-end-args rest-args)
      (let ((src-local (env-add-local env (gensym "SRC")))
            (len-local (env-add-local env (gensym "LEN") :i32))
            (start-local (env-add-local env (gensym "START") :i32))
            (end-local (env-add-local env (gensym "END") :i32))
            (idx-local (env-add-local env (gensym "IDX") :i32))
            (byte-local (env-add-local env (gensym "BYTE") :i32))
            (word-start-local (env-add-local env (gensym "WORDSTART") :i32))
            (string-type clysm/compiler/codegen/gc-types:+type-string+))
        `(;; Get string
          ,@(compile-to-instructions string-arg env)
          (:ref.cast ,(list :ref string-type))
          (:local.set ,src-local)
          ;; Get length
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:array.len)
          (:local.set ,len-local)
          ;; Initialize bounds
          ,@(%generate-bounds-init start-ast end-ast start-local end-local len-local env)
          ;; Initialize
          (:local.get ,start-local)
          (:local.set ,idx-local)
          (:i32.const 1)  ; Start of range is word start
          (:local.set ,word-start-local)
          (:block $ncap_done)
          (:loop $ncap_loop)
          (:local.get ,idx-local)
          (:local.get ,end-local)
          :i32.ge_u
          (:br_if $ncap_done)
          ;; Get byte
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:array.get_u ,string-type)
          (:local.set ,byte-local)
          ;; Check if alphabetic
          (:local.get ,byte-local)
          (:i32.const 65)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 90)
          :i32.le_u
          :i32.and
          (:local.get ,byte-local)
          (:i32.const 97)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 122)
          :i32.le_u
          :i32.and
          :i32.or
          (:if nil)
          ;; Is alphabetic
          (:local.get ,word-start-local)
          (:if nil)
          ;; Word start - uppercase
          (:local.get ,byte-local)
          (:i32.const 97)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 122)
          :i32.le_u
          :i32.and
          (:if nil)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,byte-local)
          (:i32.const 32)
          :i32.sub
          (:array.set ,string-type)
          :end
          :else
          ;; Not word start - lowercase
          (:local.get ,byte-local)
          (:i32.const 65)
          :i32.ge_u
          (:local.get ,byte-local)
          (:i32.const 90)
          :i32.le_u
          :i32.and
          (:if nil)
          (:local.get ,src-local)
          (:ref.cast ,(list :ref string-type))
          (:local.get ,idx-local)
          (:local.get ,byte-local)
          (:i32.const 32)
          :i32.add
          (:array.set ,string-type)
          :end
          :end
          ;; Clear word-start
          (:i32.const 0)
          (:local.set ,word-start-local)
          :else
          ;; Not alphabetic - set word-start
          (:i32.const 1)
          (:local.set ,word-start-local)
          :end
          ;; Increment
          (:local.get ,idx-local)
          (:i32.const 1)
          :i32.add
          (:local.set ,idx-local)
          (:br $ncap_loop)
          :end
          :end
          ;; Return same string
          (:local.get ,src-local))))))

;;; ============================================================
;;; Substring and Concatenation (008-character-string Phase 7)
;;; ============================================================

(defun compile-subseq (args env)
  "Compile (subseq string start &optional end) - get substring.
   Stack: [] -> [string]"
  (when (< (length args) 2)
    (error "subseq requires at least 2 arguments"))
  (let* ((string-arg (first args))
         (start-arg (second args))
         (end-arg (if (>= (length args) 3) (third args) nil))
         (src-local (env-add-local env (gensym "SRC")))
         (dst-local (env-add-local env (gensym "DST")))
         (start-local (env-add-local env (gensym "START") :i32))
         (end-local (env-add-local env (gensym "END") :i32))
         (byte-len-local (env-add-local env (gensym "BYTELEN") :i32))
         (src-byte-len-local (env-add-local env (gensym "SRCBYTELEN") :i32))
         (char-count-local (env-add-local env (gensym "CHARCOUNT") :i32))
         (src-idx-local (env-add-local env (gensym "SRCIDX") :i32))
         (dst-idx-local (env-add-local env (gensym "DSTIDX") :i32))
         (byte-local (env-add-local env (gensym "BYTE") :i32))
         (start-byte-local (env-add-local env (gensym "STARTBYTE") :i32))
         (string-type clysm/compiler/codegen/gc-types:+type-string+))
    `(;; Get source string
      ,@(compile-to-instructions string-arg env)
      (:ref.cast ,(list :ref string-type))
      (:local.set ,src-local)
      ;; Get start index
      ,@(compile-to-instructions start-arg env)
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,start-local)
      ;; Get end index (or string length)
      ,@(if end-arg
            `(,@(compile-to-instructions end-arg env)
              (:ref.cast :i31)
              :i31.get_s
              (:local.set ,end-local))
            `(;; Calculate string char length for default end
              (:local.get ,src-local)
              (:ref.cast ,(list :ref string-type))
              (:array.len)
              (:local.set ,src-byte-len-local)
              ;; Count characters (UTF-8 aware)
              (:i32.const 0)
              (:local.set ,char-count-local)
              (:i32.const 0)
              (:local.set ,src-idx-local)
              (:block $len_done)
              (:loop $len_loop)
              (:local.get ,src-idx-local)
              (:local.get ,src-byte-len-local)
              :i32.ge_u
              (:br_if $len_done)
              (:local.get ,src-local)
              (:ref.cast ,(list :ref string-type))
              (:local.get ,src-idx-local)
              (:array.get_u ,string-type)
              (:i32.const #xC0)
              :i32.and
              (:i32.const #x80)
              :i32.ne
              (:if nil)
              (:local.get ,char-count-local)
              (:i32.const 1)
              :i32.add
              (:local.set ,char-count-local)
              :end
              (:local.get ,src-idx-local)
              (:i32.const 1)
              :i32.add
              (:local.set ,src-idx-local)
              (:br $len_loop)
              :end
              :end
              (:local.get ,char-count-local)
              (:local.set ,end-local)))
      ;; Find byte position of start char
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:array.len)
      (:local.set ,src-byte-len-local)
      (:i32.const 0)
      (:local.set ,char-count-local)
      (:i32.const 0)
      (:local.set ,src-idx-local)
      (:block $find_start_done)
      (:loop $find_start_loop)
      (:local.get ,src-idx-local)
      (:local.get ,src-byte-len-local)
      :i32.ge_u
      (:br_if $find_start_done)
      (:local.get ,char-count-local)
      (:local.get ,start-local)
      :i32.ge_u
      (:br_if $find_start_done)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,src-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      (:local.get ,src-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,src-idx-local)
      (:br $find_start_loop)
      :end
      :end
      (:local.get ,src-idx-local)
      (:local.set ,start-byte-local)
      ;; Find byte position of end char and calculate byte length
      (:block $find_end_done)
      (:loop $find_end_loop)
      (:local.get ,src-idx-local)
      (:local.get ,src-byte-len-local)
      :i32.ge_u
      (:br_if $find_end_done)
      (:local.get ,char-count-local)
      (:local.get ,end-local)
      :i32.ge_u
      (:br_if $find_end_done)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,src-idx-local)
      (:array.get_u ,string-type)
      (:local.set ,byte-local)
      (:local.get ,byte-local)
      (:i32.const #xC0)
      :i32.and
      (:i32.const #x80)
      :i32.ne
      (:if nil)
      (:local.get ,char-count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,char-count-local)
      :end
      (:local.get ,src-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,src-idx-local)
      (:br $find_end_loop)
      :end
      :end
      (:local.get ,src-idx-local)
      (:local.get ,start-byte-local)
      :i32.sub
      (:local.set ,byte-len-local)
      ;; Create destination array
      (:local.get ,byte-len-local)
      (:array.new_default ,string-type)
      (:local.set ,dst-local)
      ;; Copy bytes
      (:i32.const 0)
      (:local.set ,dst-idx-local)
      (:block $copy_done)
      (:loop $copy_loop)
      (:local.get ,dst-idx-local)
      (:local.get ,byte-len-local)
      :i32.ge_u
      (:br_if $copy_done)
      (:local.get ,dst-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,dst-idx-local)
      (:local.get ,src-local)
      (:ref.cast ,(list :ref string-type))
      (:local.get ,start-byte-local)
      (:local.get ,dst-idx-local)
      :i32.add
      (:array.get_u ,string-type)
      (:array.set ,string-type)
      (:local.get ,dst-idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,dst-idx-local)
      (:br $copy_loop)
      :end
      :end
      ;; Return destination
      (:local.get ,dst-local))))

(defun compile-concatenate (args env)
  "Compile (concatenate 'string str1 str2 ...) - concatenate strings.
   Stack: [] -> [string]"
  (when (< (length args) 1)
    (error "concatenate requires at least 1 argument (result type)"))
  ;; Check that first arg is 'string
  (let ((type-arg (first args))
        (string-args (rest args)))
    ;; For now, only support 'string result type
    ;; Skip type check at compile time - we'll just produce a string
    (if (null string-args)
        ;; No strings to concatenate - return empty string
        `((:i32.const 0)
          (:array.new_default ,clysm/compiler/codegen/gc-types:+type-string+))
        ;; Concatenate strings
        (let ((str-locals (loop for i from 0 below (length string-args)
                                collect (env-add-local env (gensym "STR"))))
              (len-local (env-add-local env (gensym "TOTALLEN") :i32))
              (result-local (env-add-local env (gensym "RESULT")))
              (dst-idx-local (env-add-local env (gensym "DSTIDX") :i32))
              (src-idx-local (env-add-local env (gensym "SRCIDX") :i32))
              (src-len-local (env-add-local env (gensym "SRCLEN") :i32))
              (string-type clysm/compiler/codegen/gc-types:+type-string+))
          `(;; Compile each string and store in locals
            ,@(loop for arg in string-args
                    for local in str-locals
                    append `(,@(compile-to-instructions arg env)
                             (:ref.cast ,(list :ref string-type))
                             (:local.set ,local)))
            ;; Calculate total length
            (:i32.const 0)
            (:local.set ,len-local)
            ,@(loop for local in str-locals
                    append `((:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:array.len)
                             (:local.get ,len-local)
                             :i32.add
                             (:local.set ,len-local)))
            ;; Create result array
            (:local.get ,len-local)
            (:array.new_default ,string-type)
            (:local.set ,result-local)
            ;; Copy each string
            (:i32.const 0)
            (:local.set ,dst-idx-local)
            ,@(loop for local in str-locals
                    append `(;; Get source length
                             (:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:array.len)
                             (:local.set ,src-len-local)
                             ;; Copy bytes from this string
                             (:i32.const 0)
                             (:local.set ,src-idx-local)
                             (:block $concat_copy_done)
                             (:loop $concat_copy_loop)
                             (:local.get ,src-idx-local)
                             (:local.get ,src-len-local)
                             :i32.ge_u
                             (:br_if $concat_copy_done)
                             (:local.get ,result-local)
                             (:ref.cast ,(list :ref string-type))
                             (:local.get ,dst-idx-local)
                             (:local.get ,local)
                             (:ref.cast ,(list :ref string-type))
                             (:local.get ,src-idx-local)
                             (:array.get_u ,string-type)
                             (:array.set ,string-type)
                             (:local.get ,src-idx-local)
                             (:i32.const 1)
                             :i32.add
                             (:local.set ,src-idx-local)
                             (:local.get ,dst-idx-local)
                             (:i32.const 1)
                             :i32.add
                             (:local.set ,dst-idx-local)
                             (:br $concat_copy_loop)
                             :end
                             :end))
            ;; Return result
            (:local.get ,result-local))))))

;;; ============================================================
;;; Multiple Values Compilation (025-multiple-values)
;;; ============================================================

(defun compile-values (ast env)
  "Compile a values form to Wasm instructions.

   T015: (values) - Returns NIL with mv-count=0
   T016: (values x) - Returns x with mv-count=1
   T017: (values x y z...) - Returns x with y,z,... in mv-buffer, mv-count=n

   Global layout:
     Global 2 ($mv_count): i32 - number of values
     Global 3 ($mv_buffer): ref $mv_array - array holding secondary values

   Primary value stays on stack, secondary values go in mv-buffer."
  (let* ((forms (clysm/compiler/ast:ast-values-forms ast))
         (count (length forms))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20))  ; Type index for $mv_array
    (cond
      ;; T015: Zero values - return NIL, set count to 0
      ((zerop count)
       `((:i32.const 0)
         (:global.set ,mv-count-global)
         (:ref.null :none)))

      ;; T016: Single value - return it directly, set count to 1
      ((= count 1)
       `((:i32.const 1)
         (:global.set ,mv-count-global)
         ,@(compile-to-instructions (first forms) env)))

      ;; T017: Multiple values - first on stack, rest in mv-buffer
      (t
       (let ((first-local (env-add-local env (gensym "MV-FIRST"))))
         `(;; Set mv-count to number of values
           (:i32.const ,count)
           (:global.set ,mv-count-global)
           ;; Compile first value and save to local
           ,@(compile-to-instructions (first forms) env)
           (:local.set ,first-local)
           ;; Store secondary values (2nd, 3rd, ...) in mv-buffer
           ,@(loop for form in (rest forms)
                   for idx from 0
                   append `(;; Get mv-buffer
                            (:global.get ,mv-buffer-global)
                            ;; Buffer index
                            (:i32.const ,idx)
                            ;; Compile the value
                            ,@(compile-to-instructions form env)
                            ;; Store in array
                            (:array.set ,mv-array-type)))
           ;; Return first value
           (:local.get ,first-local)))))))

(defun compile-multiple-value-bind (ast env)
  "Compile a multiple-value-bind form to Wasm instructions.

   (multiple-value-bind (a b c) values-form body...)

   T024-T027: Binds variables from values returned by values-form.
   - First variable gets primary value (on stack)
   - Remaining variables get values from mv-buffer
   - Variables beyond mv-count get NIL

   Algorithm:
   1. Execute values-form (puts primary on stack, sets mv-count and mv-buffer)
   2. Bind first var to primary value
   3. For each remaining var, check mv-count and bind from mv-buffer or NIL
   4. Compile body in extended environment

   Note: env-add-local mutates env by adding bindings, so after calling it
   for all vars, env already contains all the bindings."
  (let* ((vars (clysm/compiler/ast:ast-mvb-vars ast))
         (values-form (clysm/compiler/ast:ast-mvb-values-form ast))
         (body (clysm/compiler/ast:ast-mvb-body ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (var-count (length vars)))
    (cond
      ;; No variables - just execute values-form (discarding values) and body
      ((zerop var-count)
       `(,@(compile-to-instructions values-form env)
         :drop
         ,@(compile-mvb-body body env)))

      ;; One variable - bind to primary value
      ((= var-count 1)
       (let* ((var (first vars))
              (local-idx (env-add-local env var)))
         `(;; Execute values form - puts primary value on stack
           ,@(compile-to-instructions values-form env)
           ;; Store in local
           (:local.set ,local-idx)
           ;; Compile body (env already has the binding from env-add-local)
           ,@(compile-mvb-body body env))))

      ;; Multiple variables
      (t
       (let* ((local-indices (loop for var in vars
                                   collect (env-add-local env var)))
              (first-local (first local-indices)))
         ;; After the loop, env already has all bindings from env-add-local
         `(;; Execute values form - puts primary value on stack
           ,@(compile-to-instructions values-form env)
           ;; Store first value in first local
           (:local.set ,first-local)
           ;; Bind remaining variables from mv-buffer
           ,@(loop for var in (rest vars)
                   for local in (rest local-indices)
                   for idx from 0
                   append `(;; Check if mv-count > (idx + 1)
                            (:global.get ,mv-count-global)
                            (:i32.const ,(1+ idx))
                            :i32.gt_s
                            (:if (:result :anyref))
                            ;; True: get from mv-buffer
                            (:global.get ,mv-buffer-global)
                            (:i32.const ,idx)
                            (:array.get ,mv-array-type)
                            :else
                            ;; False: use NIL
                            (:ref.null :none)
                            :end
                            ;; Store in local
                            (:local.set ,local)))
           ;; Compile body
           ,@(compile-mvb-body body env)))))))

(defun compile-mvb-body (body env)
  "Compile a list of body forms for MVB, returning value of last form."
  (if (null body)
      '((:ref.null :none))  ; Empty body returns NIL
      (loop for form in body
            for i from 0
            for is-last = (= i (1- (length body)))
            if is-last
              append (compile-to-instructions form env)
            else
              append `(,@(compile-to-instructions form env) :drop))))

;;; ============================================================
;;; multiple-value-list (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-list (ast env)
  "Compile (multiple-value-list form) to Wasm instructions.

   Collects all values from form into a proper list.
   Uses a loop to iterate over mv-buffer and build list in reverse,
   then cons primary value at front.

   Algorithm:
   1. Execute form, save primary value in local
   2. If mv-count = 0, return NIL
   3. Build list by consing values from mv-buffer[count-2] down to [0]
   4. Cons primary value onto front
   5. Return the list"
  (let* ((form (clysm/compiler/ast:ast-mvl-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (primary-local (env-add-local env (gensym "MVL-PRIMARY")))
         (count-local (env-add-local env (gensym "MVL-COUNT") :i32))
         (idx-local (env-add-local env (gensym "MVL-IDX") :i32))
         (result-local (env-add-local env (gensym "MVL-RESULT"))))
    `(;; Execute form and save primary value
      ,@(compile-to-instructions form env)
      (:local.set ,primary-local)
      ;; Get mv-count and save
      (:global.get ,mv-count-global)
      (:local.set ,count-local)
      ;; Check if count = 0 -> return NIL
      (:local.get ,count-local)
      (:i32.const 0)
      :i32.eq
      (:if (:result :anyref))
      (:ref.null :none)
      :else
      ;; Start with NIL as result
      (:ref.null :none)
      (:local.set ,result-local)
      ;; idx = count - 2 (start from last secondary value)
      (:local.get ,count-local)
      (:i32.const 2)
      :i32.sub
      (:local.set ,idx-local)
      ;; Loop while idx >= 0 (void block/loop)
      (:block)
      (:loop)
      ;; Check if idx < 0, exit if so
      (:local.get ,idx-local)
      (:i32.const 0)
      :i32.lt_s
      (:br_if 1)  ; Exit loop if idx < 0
      ;; Cons mv-buffer[idx] with result using struct.new
      (:global.get ,mv-buffer-global)
      (:local.get ,idx-local)
      (:array.get ,mv-array-type)
      (:local.get ,result-local)
      (:struct.new ,cons-type)
      (:local.set ,result-local)
      ;; idx = idx - 1
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.sub
      (:local.set ,idx-local)
      ;; Continue loop
      (:br 0)
      :end  ; end loop
      :end  ; end block
      ;; Now cons primary value onto front using struct.new
      (:local.get ,primary-local)
      (:local.get ,result-local)
      (:struct.new ,cons-type)
      :end)))

;;; ============================================================
;;; nth-value (025-multiple-values)
;;; ============================================================

(defun compile-nth-value (ast env)
  "Compile (nth-value n form) to Wasm instructions.

   Returns the nth value from form, or NIL if n >= mv-count.

   Algorithm:
   1. Evaluate index expression
   2. Execute form
   3. If index = 0, return primary value
   4. If index >= mv-count, return NIL
   5. Otherwise, return mv-buffer[index - 1]"
  (let* ((index-ast (clysm/compiler/ast:ast-nth-value-index ast))
         (form (clysm/compiler/ast:ast-nth-value-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (index-local (env-add-local env (gensym "NTH-IDX") :i32))
         (primary-local (env-add-local env (gensym "NTH-PRIMARY"))))
    `(;; Evaluate and save index
      ,@(compile-to-instructions index-ast env)
      ;; Cast to i31 and unwrap to i32
      (:ref.cast :i31)
      :i31.get_s
      (:local.set ,index-local)
      ;; Execute form
      ,@(compile-to-instructions form env)
      (:local.set ,primary-local)
      ;; Check if index = 0 -> return primary
      (:local.get ,index-local)
      (:i32.const 0)
      :i32.eq
      (:if (:result :anyref))
      (:local.get ,primary-local)
      :else
      ;; Check if index >= mv-count -> return NIL
      (:local.get ,index-local)
      (:global.get ,mv-count-global)
      :i32.ge_s
      (:if (:result :anyref))
      (:ref.null :none)
      :else
      ;; Return mv-buffer[index - 1]
      (:global.get ,mv-buffer-global)
      (:local.get ,index-local)
      (:i32.const 1)
      :i32.sub
      (:array.get ,mv-array-type)
      :end
      :end)))

;;; ============================================================
;;; values-list (025-multiple-values)
;;; ============================================================

(defun compile-values-list (ast env)
  "Compile (values-list list) to Wasm instructions.

   Spreads a list as multiple values.

   Algorithm:
   1. If list is NIL, set mv-count=0, return NIL
   2. Otherwise, car is primary value, iterate cdr to fill mv-buffer
   3. Set mv-count = list length, return primary value"
  (let* ((form (clysm/compiler/ast:ast-values-list-form ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (list-local (env-add-local env (gensym "VL-LIST")))
         (count-local (env-add-local env (gensym "VL-COUNT") :i32))
         (idx-local (env-add-local env (gensym "VL-IDX") :i32))
         (current-local (env-add-local env (gensym "VL-CURRENT")))
         (primary-local (env-add-local env (gensym "VL-PRIMARY"))))
    `(;; Evaluate list expression
      ,@(compile-to-instructions form env)
      (:local.set ,list-local)
      ;; Check if nil
      (:local.get ,list-local)
      :ref.is_null
      (:if (:result :anyref))
      ;; NIL case: mv-count=0, return NIL
      (:i32.const 0)
      (:global.set ,mv-count-global)
      (:ref.null :none)
      :else
      ;; Get car as primary using struct.get
      (:local.get ,list-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:local.set ,primary-local)
      ;; Start counting and iterating
      (:i32.const 1)
      (:local.set ,count-local)
      (:i32.const 0)
      (:local.set ,idx-local)
      ;; current = cdr(list) using struct.get
      (:local.get ,list-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,current-local)
      ;; Loop while current != nil (void block/loop)
      (:block)
      (:loop)
      ;; Check if current is nil
      (:local.get ,current-local)
      :ref.is_null
      (:br_if 1)  ; Exit if nil
      ;; Store car(current) in mv-buffer[idx]
      (:global.get ,mv-buffer-global)
      (:local.get ,idx-local)
      (:local.get ,current-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 0)
      (:array.set ,mv-array-type)
      ;; count++, idx++
      (:local.get ,count-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,count-local)
      (:local.get ,idx-local)
      (:i32.const 1)
      :i32.add
      (:local.set ,idx-local)
      ;; current = cdr(current) using struct.get
      (:local.get ,current-local)
      (:ref.cast ,(list :ref cons-type))
      (:struct.get ,cons-type 1)
      (:local.set ,current-local)
      ;; Continue loop
      (:br 0)
      :end  ; end loop
      :end  ; end block
      ;; Set mv-count
      (:local.get ,count-local)
      (:global.set ,mv-count-global)
      ;; Return primary
      (:local.get ,primary-local)
      :end)))

;;; ============================================================
;;; multiple-value-prog1 (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-prog1 (ast env)
  "Compile (multiple-value-prog1 first-form body...) to Wasm instructions.

   Saves all values from first-form, executes body for side effects,
   then restores and returns the saved values.

   Algorithm:
   1. Execute first-form, save primary value and mv-count
   2. Copy mv-buffer contents to local array
   3. Execute body forms (drop results)
   4. Restore mv-count from local
   5. Copy local array back to mv-buffer
   6. Return saved primary value"
  (let* ((first-form (clysm/compiler/ast:ast-mvp1-first-form ast))
         (body (clysm/compiler/ast:ast-mvp1-body ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (primary-local (env-add-local env (gensym "MVP1-PRIMARY")))
         (count-local (env-add-local env (gensym "MVP1-COUNT") :i32)))
    `(;; Execute first form
      ,@(compile-to-instructions first-form env)
      (:local.set ,primary-local)
      ;; Save mv-count
      (:global.get ,mv-count-global)
      (:local.set ,count-local)
      ;; Execute body forms for side effects
      ,@(loop for form in body
              append `(,@(compile-to-instructions form env) :drop))
      ;; Restore mv-count
      (:local.get ,count-local)
      (:global.set ,mv-count-global)
      ;; Return primary value (note: secondary values in mv-buffer may be stale,
      ;; but for simple cases this is acceptable - full preservation would need
      ;; a local array copy which is complex in WasmGC)
      (:local.get ,primary-local))))

;;; ============================================================
;;; multiple-value-call (025-multiple-values)
;;; ============================================================

(defun compile-multiple-value-call (ast env)
  "Compile (multiple-value-call fn form1 form2 ...) to Wasm instructions.

   NOTE: This is a simplified implementation that only works with + and
   a single form for now. Full dynamic apply is complex and deferred.

   For (multiple-value-call #'+ (values 1 2 3)):
   - Collects all values and sums them using a loop

   TODO: Full implementation requires dynamic function application."
  (let* ((function-ast (clysm/compiler/ast:ast-mvc-function ast))
         (forms (clysm/compiler/ast:ast-mvc-forms ast))
         (mv-count-global clysm/runtime/objects:*mv-count-global-index*)
         (mv-buffer-global clysm/runtime/objects:*mv-buffer-global-index*)
         (mv-array-type 20)
         (cons-type clysm/compiler/codegen/gc-types:+type-cons+)
         (sum-local (env-add-local env (gensym "MVC-SUM") :i32))
         (idx-local (env-add-local env (gensym "MVC-IDX") :i32))
         (count-local (env-add-local env (gensym "MVC-COUNT") :i32)))
    ;; For now, only handle single-form case with + function
    ;; This computes sum of all values
    (when (and (= 1 (length forms))
               (typep function-ast 'clysm/compiler/ast:ast-call)
               (eq 'function (clysm/compiler/ast:ast-call-function function-ast)))
      `(;; Execute the form
        ,@(compile-to-instructions (first forms) env)
        ;; Cast to i31 and unwrap to i32 for sum
        (:ref.cast :i31)
        :i31.get_s
        (:local.set ,sum-local)
        ;; Get count
        (:global.get ,mv-count-global)
        (:local.set ,count-local)
        ;; Initialize idx = 0
        (:i32.const 0)
        (:local.set ,idx-local)
        ;; Loop to add secondary values (void block/loop)
        (:block)
        (:loop)
        ;; Check if idx >= count - 1
        (:local.get ,idx-local)
        (:local.get ,count-local)
        (:i32.const 1)
        :i32.sub
        :i32.ge_s
        (:br_if 1)
        ;; Add mv-buffer[idx] to sum
        (:global.get ,mv-buffer-global)
        (:local.get ,idx-local)
        (:array.get ,mv-array-type)
        (:ref.cast :i31)
        :i31.get_s
        (:local.get ,sum-local)
        :i32.add
        (:local.set ,sum-local)
        ;; idx++
        (:local.get ,idx-local)
        (:i32.const 1)
        :i32.add
        (:local.set ,idx-local)
        ;; Continue
        (:br 0)
        :end
        :end
        ;; Return sum as i31ref
        (:local.get ,sum-local)
        :ref.i31))))

;;; ============================================================
;;; CLOS: defclass Compilation (026-clos-foundation)
;;; ============================================================

;;; Class ID allocation counter for dispatch
(defvar *class-id-counter* 0
  "Counter for allocating unique class IDs.")

(defun reset-class-id-counter ()
  "Reset the class ID counter. Called when starting a new compilation."
  (setf *class-id-counter* 0))

(defun allocate-class-id ()
  "Allocate a new unique class ID."
  (prog1 *class-id-counter*
    (incf *class-id-counter*)))

;;; Class registry for compile-time class tracking
(defvar *class-registry* (make-hash-table :test 'eq)
  "Compile-time registry mapping class names to class info.")

(defstruct class-info
  "Compile-time information about a CLOS class."
  (name nil :type symbol)
  (superclass nil :type (or null symbol))
  (slots nil :type list)       ; List of slot-info structs
  (class-id nil :type (or null fixnum))
  (finalized-p nil :type boolean))

(defstruct slot-info
  "Compile-time information about a slot."
  (name nil :type symbol)
  (initarg nil :type (or null keyword))
  (accessor nil :type (or null symbol))
  (initform nil :type t)
  (initform-p nil :type boolean)
  (index nil :type (or null fixnum)))  ; Slot index in instance

(defun reset-class-registry ()
  "Reset the class registry. Called when starting a new compilation."
  (clrhash *class-registry*)
  (reset-class-id-counter))

(defun register-compile-time-class (name superclass slots)
  "Register a class at compile time.
   NAME is the class symbol.
   SUPERCLASS is the superclass name or NIL.
   SLOTS is a list of ast-slot-definition structs.

   Handles single inheritance:
   - Inherits slots from superclass (prepended to own slots)
   - Child slots with same name shadow parent slots
   - Recomputes slot indices"
  (let* ((class-id (allocate-class-id))
         ;; Get parent slots if superclass exists
         (parent-slots
           (when superclass
             (let ((parent-info (find-compile-time-class superclass)))
               (if parent-info
                   (class-info-slots parent-info)
                   (error "Superclass ~S not defined" superclass)))))
         ;; Get own slot names for shadowing check
         (own-slot-names
           (mapcar #'clysm/compiler/ast:ast-slot-definition-name slots))
         ;; Filter out shadowed parent slots
         (inherited-slots
           (remove-if (lambda (pslot)
                        (member (slot-info-name pslot) own-slot-names))
                      parent-slots))
         ;; Convert own slots to slot-info (index will be set later)
         (own-slot-infos
           (mapcar (lambda (slot)
                     (make-slot-info
                      :name (clysm/compiler/ast:ast-slot-definition-name slot)
                      :initarg (clysm/compiler/ast:ast-slot-definition-initarg slot)
                      :accessor (clysm/compiler/ast:ast-slot-definition-accessor slot)
                      :initform (clysm/compiler/ast:ast-slot-definition-initform slot)
                      :initform-p (clysm/compiler/ast:ast-slot-definition-initform-p slot)
                      :index 0))  ; Temporary, will be recomputed
                   slots))
         ;; Combine: inherited first, then own slots
         (all-slots (append inherited-slots own-slot-infos))
         ;; Recompute indices
         (slot-infos
           (loop for slot in all-slots
                 for index from 0
                 collect (make-slot-info
                          :name (slot-info-name slot)
                          :initarg (slot-info-initarg slot)
                          :accessor (slot-info-accessor slot)
                          :initform (slot-info-initform slot)
                          :initform-p (slot-info-initform-p slot)
                          :index index)))
         (info (make-class-info
                :name name
                :superclass superclass
                :slots slot-infos
                :class-id class-id
                :finalized-p t)))
    (setf (gethash name *class-registry*) info)
    info))

(defun find-compile-time-class (name)
  "Find a class in the compile-time registry."
  (gethash name *class-registry*))

(defun compile-defclass (ast env)
  "Compile a defclass form.
   Registers the class at compile time and returns NIL.
   The actual class global will be emitted in the global section.

   For now, this is a compile-time only operation - the class metadata
   is registered for use by make-instance and accessor codegen, but
   no runtime code is emitted in the expression position."
  (declare (ignore env))
  (let* ((name (clysm/compiler/ast:ast-defclass-name ast))
         (superclass (clysm/compiler/ast:ast-defclass-superclass ast))
         (slots (clysm/compiler/ast:ast-defclass-slots ast)))
    ;; Register the class at compile time
    (register-compile-time-class name superclass slots)
    ;; Also register with the existing CLOS infrastructure for interpreter compatibility
    ;; This uses the existing compile-time CLOS in clysm/clos/
    (let ((form `(defclass ,name ,(if superclass (list superclass) nil)
                   ,(mapcar (lambda (slot)
                              (let ((slot-name (clysm/compiler/ast:ast-slot-definition-name slot))
                                    (initarg (clysm/compiler/ast:ast-slot-definition-initarg slot))
                                    (accessor (clysm/compiler/ast:ast-slot-definition-accessor slot))
                                    (initform (clysm/compiler/ast:ast-slot-definition-initform slot))
                                    (initform-p (clysm/compiler/ast:ast-slot-definition-initform-p slot)))
                                (append (list slot-name)
                                        (when initarg (list :initarg initarg))
                                        (when accessor (list :accessor accessor))
                                        (when initform-p (list :initform initform)))))
                            slots))))
      (clysm/clos/defclass:define-class* form))
    ;; Return NIL (defclass returns class name, but as a quoted symbol we return NIL for now)
    ;; In a full implementation, this would return a reference to the class object
    (list '(:ref.null :none))))

;;; ============================================================
;;; CLOS: make-instance Compilation (026-clos-foundation)
;;; ============================================================

(defun compile-make-instance (ast env)
  "Compile a make-instance form.
   Creates an instance of the specified class with the given initargs.

   The compilation emits code that:
   1. Looks up the class (error if not found)
   2. Creates a slot vector with the correct size
   3. Fills slots based on initargs, initforms, or UNBOUND
   4. Creates the $instance struct

   For now, this is a placeholder that returns NIL.
   Full WasmGC codegen will be added in the next iteration."
  (declare (ignore env))
  (let* ((class-name (clysm/compiler/ast:ast-make-instance-class-name ast))
         (initargs (clysm/compiler/ast:ast-make-instance-initargs ast))
         (class-info (find-compile-time-class class-name)))
    ;; Validate class exists
    (unless class-info
      (error "Cannot make instance of undefined class: ~S" class-name))
    ;; For now, use the interpreter CLOS to create the instance
    ;; This is a compile-time evaluation - not ideal but works for MVP
    ;; In a full implementation, we would emit WasmGC instructions
    (let ((initarg-plist (loop for (key . val-ast) in initargs
                               collect key
                               collect (if (typep val-ast 'clysm/compiler/ast:ast-literal)
                                           (clysm/compiler/ast:ast-literal-value val-ast)
                                           0))))  ; Default to 0 for non-literals
      (declare (ignore initarg-plist))
      ;; For MVP, just return a null reference
      ;; TODO: Emit actual struct.new $instance instructions
      (list '(:ref.null :none)))))

;;; ============================================================
;;; CLOS: make-instance* Primitive (001-make-instance-primitive)
;;; ============================================================

(defun compile-make-instance* (args env)
  "Compile a make-instance* primitive call.
   Feature: 001-m3-clos-primitives (FR-003)

   This handles both forms:
   1. Compile-time: (make-instance* 'class-name :slot1 val1 ...)
   2. Runtime: (make-instance* class-info)  or  (make-instance* class-info slot-count)

   Arguments:
     args - List of AST nodes: (class-ref &optional slot-count &rest initarg-pairs)
     env  - Compilation environment

   Returns:
     List of Wasm instructions creating an instance.

   For MVP, emits struct.new $instance with empty slot vector.
   Full WasmGC implementation will follow."
  (when (null args)
    (error "make-instance* requires at least a class reference argument"))
  (let* ((class-ref-ast (first args))
         (slot-count-ast (when (>= (length args) 2) (second args)))
         (instance-type clysm/compiler/codegen/gc-types:+type-instance+)
         (slot-vector-type clysm/compiler/codegen/gc-types:+type-slot-vector+)
         (result nil))
    ;; For MVP: Generate struct.new $instance with slot vector
    ;; The instance structure has:
    ;;   field 0: class reference (anyref)
    ;;   field 1: slot vector (array of anyref)
    ;;
    ;; Wasm sequence:
    ;;   1. Evaluate class-ref (or use ref.null for unknown class)
    ;;   2. Create slot vector (array.new_default)
    ;;   3. Create instance struct (struct.new $instance)

    ;; First, evaluate or produce class reference
    (cond
      ;; Case 1: Quoted symbol at compile time
      ((and (typep class-ref-ast 'clysm/compiler/ast:ast-literal)
            (eq (clysm/compiler/ast:ast-literal-literal-type class-ref-ast) :quoted))
       ;; For MVP, use ref.null as placeholder for class ref
       (push '(:ref.null :none) result))
      ;; Case 2: Runtime class-info variable
      (t
       ;; Compile the class-info expression
       (setf result (compile-to-instructions class-ref-ast env))))

    ;; Determine slot count
    (let ((slot-count 0))  ; Default to 0 slots for MVP
      (when slot-count-ast
        ;; If slot count provided, try to extract it
        (when (and (typep slot-count-ast 'clysm/compiler/ast:ast-literal)
                   (integerp (clysm/compiler/ast:ast-literal-value slot-count-ast)))
          (setf slot-count (clysm/compiler/ast:ast-literal-value slot-count-ast))))

      ;; Create slot vector: array.new_default $slot-vector slot_count
      (push `(:i32.const ,slot-count) result)
      (push `(:array.new_default ,slot-vector-type) result))

    ;; Create instance: struct.new $instance (pops class-ref and slot-vector)
    (push `(:struct.new ,instance-type) result)
    (nreverse result)))

;;; ============================================================
;;; CLOS Primitives: Slot Access (001-m3-clos-primitives)
;;; HyperSpec: resources/HyperSpec/Body/f_slt_va.htm
;;; ============================================================

(defun compile-slot-value* (args env)
  "Compile (slot-value* instance slot-name [slot-index]) to Wasm.
   Feature: 001-m3-clos-primitives (FR-001)

   Generates:
     1. Evaluate instance -> anyref
     2. Cast to $instance type
     3. Get $slots field (field 1) -> slot-vector ref
     4. Get element at slot-index -> anyref

   Arguments:
     args - List of AST nodes: (instance slot-name [slot-index])
     env  - Compilation environment

   Note: If slot-index is not provided, defaults to 0 for MVP compatibility.
         slot-name is currently ignored at compile time (may be used for debugging)."
  ;; Validate arguments - minimum 2 (instance, slot-name)
  (unless (>= (length args) 2)
    (error "slot-value* requires at least 2 arguments: instance, slot-name"))
  (let* ((instance-ast (first args))
         (slot-index-ast (when (>= (length args) 3) (third args)))
         ;; Get slot index as compile-time constant, default to 0 for MVP
         (slot-index (cond
                       ;; No slot-index provided - default to 0
                       ((null slot-index-ast) 0)
                       ;; Compile-time integer literal
                       ((and (typep slot-index-ast 'clysm/compiler/ast:ast-literal)
                             (integerp (clysm/compiler/ast:ast-literal-value slot-index-ast)))
                        (clysm/compiler/ast:ast-literal-value slot-index-ast))
                       ;; Not a constant - use 0 for MVP
                       (t 0)))
         (instance-type clysm/compiler/codegen/gc-types:+type-instance+)
         (slot-vector-type clysm/compiler/codegen/gc-types:+type-slot-vector+)
         (result nil))
    ;; Compile instance expression
    (setf result (compile-to-instructions instance-ast env))
    ;; Cast to $instance type (ref.cast (ref $instance))
    (push `(:ref.cast (:ref ,instance-type)) result)
    ;; Get $slots field (field index 1)
    (push `(:struct.get ,instance-type 1) result)
    ;; Cast to slot-vector type
    (push `(:ref.cast (:ref ,slot-vector-type)) result)
    ;; Push slot index
    (push `(:i32.const ,slot-index) result)
    ;; Get slot value (array.get $slot-vector)
    (push `(:array.get ,slot-vector-type) result)
    (nreverse result)))

(defun compile-set-slot-value* (args env)
  "Compile (set-slot-value* instance slot-name [slot-index] value) to Wasm.
   Feature: 001-m3-clos-primitives (FR-002)

   Generates:
     1. Evaluate instance -> anyref
     2. Cast to $instance type
     3. Get $slots field (field 1) -> slot-vector ref
     4. Push slot-index
     5. Evaluate value -> anyref
     6. array.set $slot-vector
     7. Return value (for setf semantics)

   Arguments:
     args - List of AST nodes: (instance slot-name [slot-index] value)
            3 args: (instance slot-name value) - slot-index defaults to 0
            4 args: (instance slot-name slot-index value)
     env  - Compilation environment"
  ;; Validate arguments - minimum 3 (instance, slot-name, value)
  (unless (>= (length args) 3)
    (error "set-slot-value* requires at least 3 arguments: instance, slot-name, value"))
  ;; Determine if 3-arg or 4-arg form
  (let* ((instance-ast (first args))
         (has-explicit-index (>= (length args) 4))
         (slot-index-ast (when has-explicit-index (third args)))
         (value-ast (if has-explicit-index (fourth args) (third args)))
         ;; Get slot index as compile-time constant, default to 0
         (slot-index (cond
                       ;; No explicit slot-index
                       ((null slot-index-ast) 0)
                       ;; Compile-time integer literal
                       ((and (typep slot-index-ast 'clysm/compiler/ast:ast-literal)
                             (integerp (clysm/compiler/ast:ast-literal-value slot-index-ast)))
                        (clysm/compiler/ast:ast-literal-value slot-index-ast))
                       ;; Not a constant - use 0 for MVP
                       (t 0)))
         (instance-type clysm/compiler/codegen/gc-types:+type-instance+)
         (slot-vector-type clysm/compiler/codegen/gc-types:+type-slot-vector+)
         ;; Allocate a local for the value (to return after setting)
         (value-local (env-add-local env (gensym "slot-value")))
         (result nil))
    ;; Compile instance expression
    (setf result (compile-to-instructions instance-ast env))
    ;; Cast to $instance type
    (push `(:ref.cast (:ref ,instance-type)) result)
    ;; Get $slots field
    (push `(:struct.get ,instance-type 1) result)
    ;; Cast to slot-vector type
    (push `(:ref.cast (:ref ,slot-vector-type)) result)
    ;; Push slot index
    (push `(:i32.const ,slot-index) result)
    ;; Compile value expression
    (dolist (instr (compile-to-instructions value-ast env))
      (push instr result))
    ;; Store value in local for return
    (push `(:local.tee ,value-local) result)
    ;; Set slot value (array.set $slot-vector)
    (push `(:array.set ,slot-vector-type) result)
    ;; Return the value that was set (setf semantics)
    (push `(:local.get ,value-local) result)
    (nreverse result)))

(defun compile-standard-instance-p (args env)
  "Compile (standard-instance-p object) to Wasm.
   Feature: 001-m3-clos-primitives (FR-004)

   Generates:
     1. Evaluate object -> anyref
     2. ref.test $instance -> i32 (0 or 1)
     3. Convert to Lisp boolean (i31ref)

   Arguments:
     args - List of AST nodes: (object)
     env  - Compilation environment"
  ;; Validate arguments
  (unless (>= (length args) 1)
    (error "standard-instance-p requires 1 argument: object"))
  (let* ((object-ast (first args))
         (instance-type clysm/compiler/codegen/gc-types:+type-instance+)
         (result nil))
    ;; Compile object expression
    (setf result (compile-to-instructions object-ast env))
    ;; Test if it's an instance (ref.test (ref $instance))
    (push `(:ref.test (:ref ,instance-type)) result)
    ;; Convert i32 result (0/1) to Lisp boolean
    ;; Non-zero -> T (represented as non-nil), zero -> NIL
    ;; NIL is global 0, truthy value can be (ref.i31 1)
    ;; Emit if/then/else block
    (push '(:if (:result :anyref)) result)
    ;; Then block: return truthy value (i31ref of 1)
    (push '(:i32.const 1) result)
    (push :ref.i31 result)
    (push :else result)
    ;; Else block: return NIL
    (push '(:global.get 0) result)
    (push :end result)
    (nreverse result)))

(defun compile-instance-class (args env)
  "Compile (instance-class instance) to Wasm.
   Feature: 001-m3-clos-primitives (DEFSTRUCT predicate support)

   Generates:
     1. Evaluate instance -> anyref
     2. Cast to $instance type
     3. Get $class field (field 0) -> anyref

   Arguments:
     args - List of AST nodes: (instance)
     env  - Compilation environment"
  (unless (>= (length args) 1)
    (error "instance-class requires 1 argument: instance"))
  (let* ((instance-ast (first args))
         (instance-type clysm/compiler/codegen/gc-types:+type-instance+)
         (result nil))
    ;; Compile instance expression
    (setf result (compile-to-instructions instance-ast env))
    ;; Cast to $instance type
    (push `(:ref.cast (:ref ,instance-type)) result)
    ;; Get $class field (field index 0)
    (push `(:struct.get ,instance-type 0) result)
    (nreverse result)))

(defun compile-class-name (args env)
  "Compile (class-name class) to Wasm.
   Feature: 001-m3-clos-primitives (DEFSTRUCT predicate support)

   Generates:
     1. Evaluate class -> anyref
     2. Cast to $standard-class type
     3. Get $name field (field 0) -> anyref

   Arguments:
     args - List of AST nodes: (class)
     env  - Compilation environment"
  (unless (>= (length args) 1)
    (error "class-name requires 1 argument: class"))
  (let* ((class-ast (first args))
         (standard-class-type clysm/compiler/codegen/gc-types:+type-standard-class+)
         (result nil))
    ;; Compile class expression
    (setf result (compile-to-instructions class-ast env))
    ;; Cast to $standard-class type
    (push `(:ref.cast (:ref ,standard-class-type)) result)
    ;; Get $name field (field index 0)
    (push `(:struct.get ,standard-class-type 0) result)
    (nreverse result)))

;;; ============================================================
;;; CLOS: Generic Function Registry (026-clos-foundation)
;;; ============================================================

(defvar *generic-function-registry* (make-hash-table :test 'eq)
  "Compile-time registry mapping generic function names to GF info.")

(defstruct gf-info
  "Compile-time information about a generic function."
  (name nil :type symbol)
  (methods nil :type list)        ; List of method-info structs
  (lambda-list nil :type list))   ; Expected lambda-list

(defstruct method-info
  "Compile-time information about a method."
  (specializers nil :type list)   ; List of class names
  (qualifier nil :type (or null keyword))
  (lambda-list nil :type list)
  (body nil :type list))          ; Body as list of AST nodes

(defun reset-generic-function-registry ()
  "Reset the generic function registry."
  (clrhash *generic-function-registry*))

(defun find-generic-function (name)
  "Find a generic function in the compile-time registry."
  (gethash name *generic-function-registry*))

(defun register-generic-function (name)
  "Register a new generic function."
  (let ((gf (make-gf-info :name name)))
    (setf (gethash name *generic-function-registry*) gf)
    gf))

(defun ensure-compile-time-gf (name)
  "Ensure a compile-time generic function exists, creating if necessary."
  (or (find-generic-function name)
      (register-generic-function name)))

(defun add-method-to-gf (gf method-info)
  "Add a method to a generic function."
  (push method-info (gf-info-methods gf)))

;;; ============================================================
;;; CLOS: defmethod Compilation (026-clos-foundation)
;;; ============================================================

(defun compile-defmethod (ast env)
  "Compile a defmethod form.
   Registers the method with the generic function.
   Returns NIL for now (method registration is compile-time only)."
  (declare (ignore env))
  (let* ((name (clysm/compiler/ast:ast-defmethod-name ast))
         (qualifier (clysm/compiler/ast:ast-defmethod-qualifier ast))
         (specializers (clysm/compiler/ast:ast-defmethod-specializers ast))
         (lambda-list (clysm/compiler/ast:ast-defmethod-lambda-list ast))
         (body (clysm/compiler/ast:ast-defmethod-body ast)))
    ;; Ensure generic function exists
    (let ((gf (ensure-compile-time-gf name)))
      ;; Create method info
      (let ((method (make-method-info
                     :specializers specializers
                     :qualifier qualifier
                     :lambda-list lambda-list
                     :body body)))
        ;; Add method to GF
        (add-method-to-gf gf method)))
    ;; Also register with existing CLOS infrastructure
    (let ((form `(defmethod ,name ,@(when qualifier (list qualifier))
                   ,(mapcar (lambda (param spec)
                              (if (eq spec t)
                                  param
                                  (list param spec)))
                            lambda-list specializers)
                   ,@(mapcar (lambda (body-ast)
                               (if (typep body-ast 'clysm/compiler/ast:ast-literal)
                                   (clysm/compiler/ast:ast-literal-value body-ast)
                                   nil))  ; Simplified for now
                             body))))
      (clysm/clos/defmethod:define-method* form))
    ;; Return NIL
    (list '(:ref.null :none))))
