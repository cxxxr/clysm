;;;; globals.lisp - Global variable initialization for Stage 0 complete compiler
;;;;
;;;; Part of Feature 045: Stage 0 Complete Compiler
;;;; Implements US4: Runtime Initialization - Global variable initialization
;;;;
;;;; This file generates the global section for Stage 0 Wasm binary,
;;;; defining and initializing the required global variables:
;;;; - NIL (index 0): The empty list / false value singleton
;;;; - UNBOUND (index 1): Sentinel for unbound variable slots
;;;; - mv-count (index 2): Multiple values count
;;;; - mv-buffer (index 3): Multiple values buffer array

(in-package #:clysm/stage0)

;;; ============================================================
;;; Global Indices (exported for use by other modules)
;;; ============================================================

(defvar *nil-index* 0 "Global index for NIL singleton")
(defvar *unbound-index* 1 "Global index for UNBOUND sentinel")
(defvar *mv-count-index* 2 "Global index for multiple values count")
(defvar *mv-buffer-index* 3 "Global index for multiple values buffer")

(defconstant +total-globals+ 4 "Total number of global variables")

;;; ============================================================
;;; Wasm Instruction Encoding Constants
;;; ============================================================

;; Control instructions
(defconstant +end+ #x0B)

;; Constant instructions
(defconstant +i32-const+ #x41)
(defconstant +i64-const+ #x42)
(defconstant +f64-const+ #x44)

;; Reference instructions
(defconstant +ref-null+ #xD0)

;; GC instructions (prefixed with #xFB)
(defconstant +struct-new+ #x00)       ; struct.new $type
(defconstant +struct-new-default+ #x01) ; struct.new_default $type
(defconstant +array-new+ #x06)        ; array.new $type
(defconstant +array-new-default+ #x07) ; array.new_default $type
(defconstant +array-new-fixed+ #x08)  ; array.new_fixed $type $len

;;; ============================================================
;;; Global Type Definitions
;;; ============================================================

(defun global-nil-type ()
  "Return type bytes for NIL global: (ref $nil)"
  ;; ref $nil is a non-nullable reference to struct type 0
  (list +non-null+ +type-nil+))

(defun global-unbound-type ()
  "Return type bytes for UNBOUND global: (ref $unbound)"
  (list +non-null+ +type-unbound+))

(defun global-mv-count-type ()
  "Return type bytes for mv-count global: i32"
  (list +wasm-i32+))

(defun global-mv-buffer-type ()
  "Return type bytes for mv-buffer global: (ref $mv_array)"
  (list +non-null+ +type-mv-array+))

;;; ============================================================
;;; Global Initialization Expressions
;;; ============================================================

(defun init-nil-global ()
  "Generate initialization expression for NIL global.
   Creates NIL singleton using struct.new_default."
  ;; struct.new_default $nil
  (list +gc-prefix+ +struct-new-default+ +type-nil+ +end+))

(defun init-unbound-global ()
  "Generate initialization expression for UNBOUND global.
   Creates UNBOUND sentinel using struct.new_default."
  ;; struct.new_default $unbound
  (list +gc-prefix+ +struct-new-default+ +type-unbound+ +end+))

(defun init-mv-count-global ()
  "Generate initialization expression for mv-count global.
   Initializes to 1 (single value return is default)."
  ;; i32.const 1
  (list +i32-const+ 1 +end+))

(defun init-mv-buffer-global ()
  "Generate initialization expression for mv-buffer global.
   Creates empty array of size 20 (MAX_VALUES)."
  ;; i32.const 20 ; push size first
  ;; array.new_default $mv_array ; consumes size
  (list +i32-const+ 20
        +gc-prefix+ +array-new-default+ +type-mv-array+
        +end+))

;;; ============================================================
;;; Global Section Generation
;;; ============================================================

(defun encode-global (type-bytes mutability init-bytes)
  "Encode a single global definition.
   Returns list of bytes for: valtype mut init_expr"
  (append type-bytes
          (list mutability)
          init-bytes))

(defun generate-all-globals ()
  "Generate list of all global definitions in order.
   Each global is: (type-bytes mutability init-bytes)"
  (list
   ;; Global 0: NIL - immutable
   (encode-global (global-nil-type) +immutable+ (init-nil-global))
   ;; Global 1: UNBOUND - immutable
   (encode-global (global-unbound-type) +immutable+ (init-unbound-global))
   ;; Global 2: mv-count - mutable
   (encode-global (global-mv-count-type) +mutable+ (init-mv-count-global))
   ;; Global 3: mv-buffer - mutable
   (encode-global (global-mv-buffer-type) +mutable+ (init-mv-buffer-global))))

(defun generate-global-section ()
  "Generate complete global section as byte vector.
   Returns vector of bytes for Wasm global section (section ID 6)."
  (let* ((globals (generate-all-globals))
         (global-bytes '())
         (global-count (length globals)))
    ;; Encode each global definition
    (dolist (global-def globals)
      (dolist (byte global-def)
        (push byte global-bytes)))
    ;; Reverse to get correct order
    (setf global-bytes (nreverse global-bytes))
    ;; Build section: ID + size + count + global definitions
    (let* ((count-bytes (encode-unsigned-leb128 global-count))
           (content-size (+ (length count-bytes) (length global-bytes)))
           (size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      ;; Section ID 6 (global)
      (push 6 section)
      ;; Section size
      (dolist (b size-bytes) (push b section))
      ;; Global count
      (dolist (b count-bytes) (push b section))
      ;; Global definitions
      (dolist (b global-bytes) (push b section))
      ;; Return as byte vector
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Global Initialization Instructions (for start function)
;;; ============================================================

(defun generate-global-init ()
  "Generate instructions for initializing globals in start function.
   Returns list of instruction forms for the compiler.

   Note: Most globals use init expressions in the global section,
   but some complex initialization may need to happen in the start
   function. This returns any additional initialization needed."
  ;; Currently, all globals can be initialized in their definition
  ;; This function is here for future extensibility
  '())

;;; ============================================================
;;; Special Variable Global Encoding (T009 - Phase 13D-4)
;;; ============================================================

;; Type encoding for (ref null any)
;; Note: +ref-null-type+ is the type prefix (0x63), different from
;; +ref-null+ which is the instruction opcode (0xD0) defined earlier
(defconstant +ref-null-type+ #x63 "ref null type prefix")
(defconstant +any-type+ #x6E "any type")

(defun global-special-var-type ()
  "Return type bytes for special variable global: (ref null any)"
  (list +ref-null-type+ +any-type+))

(defun init-ref-null-any ()
  "Generate initialization expression for ref.null any.
   Used for deferred initialization globals."
  (list #xD0 +any-type+ +end+))  ; ref.null any, end

(defun init-global-get (global-idx)
  "Generate initialization expression for global.get.
   Used for referencing other globals like NIL or UNBOUND."
  (append (list #x23)  ; global.get
          (encode-unsigned-leb128 global-idx)
          (list +end+)))

(defun init-i31-const (value)
  "Generate initialization expression for i32.const + ref.i31.
   Used for fixnum constants."
  (append (list +i32-const+)
          (encode-signed-leb128 value)
          (list +gc-prefix+ #x1C)  ; ref.i31
          (list +end+)))

(defun encode-special-var-global (init-type &optional init-value)
  "Encode a special variable global entry (T009).
   INIT-TYPE: :constant, :deferred, or :none
   INIT-VALUE: Value for constant init, or NIL

   Returns list of bytes for the global entry."
  (let* ((type-bytes (global-special-var-type))
         (init-bytes (case init-type
                       (:constant
                        (cond
                          ((null init-value)
                           (init-global-get *nil-index*))
                          ((integerp init-value)
                           (init-i31-const init-value))
                          (t (init-ref-null-any))))
                       (:deferred
                        (init-ref-null-any))
                       (:none
                        (init-global-get *unbound-index*))
                       (t (init-ref-null-any)))))
    (encode-global type-bytes +mutable+ init-bytes)))

;;; ============================================================
;;; $init Function Generation (T010 - Phase 13D-4)
;;; ============================================================

(defvar *deferred-global-inits* '()
  "List of (global-index . init-form) for deferred initialization.
   Populated during global section generation, used by $init function.")

(defun reset-deferred-global-inits ()
  "Clear deferred initialization list for new compilation."
  (setf *deferred-global-inits* '()))

(defun register-deferred-init (global-idx init-form)
  "Register a deferred initialization for a global.
   Will be compiled into the $init function."
  (push (cons global-idx init-form) *deferred-global-inits*))

(defun has-deferred-inits-p ()
  "Check if there are any pending deferred initializations."
  (not (null *deferred-global-inits*)))

(defun generate-init-function-instructions ()
  "Generate Wasm instructions for the $init function body.
   Sets all deferred globals to their initial values."
  (let ((instrs '()))
    ;; Process in reverse order to match declaration order
    (dolist (pair (nreverse *deferred-global-inits*))
      (destructuring-bind (global-idx . init-form) pair
        ;; For now, just emit a placeholder
        ;; Full compilation requires the compiler environment
        (declare (ignore init-form))
        ;; Pattern: compile init-form, then global.set
        (push `(:ref.null :any) instrs)
        (push `(:global.set ,global-idx) instrs)))
    (nreverse instrs)))

;;; ============================================================
;;; Extended Global Section Generation (T011 - Phase 13D-4)
;;; ============================================================

(defun generate-all-globals-with-specials (special-vars)
  "Generate list of all global definitions including special variables (T011).
   SPECIAL-VARS: alist of (name . init-spec) where init-spec is
                 (:constant value) or (:deferred form) or (:none)

   Returns list of global entries (4 reserved + N special vars)."
  (let ((base-globals (generate-all-globals))
        (special-globals '()))
    ;; Generate special variable globals (starting at index 4)
    (dolist (spec special-vars)
      (destructuring-bind (name . init-spec) spec
        (declare (ignore name))
        (let* ((init-type (first init-spec))
               (init-value (second init-spec))
               (global-entry (encode-special-var-global init-type init-value)))
          (push global-entry special-globals))))
    ;; Return base globals + special globals
    (append base-globals (nreverse special-globals))))

(defun generate-global-section-with-specials (special-vars)
  "Generate complete global section including special variables.
   SPECIAL-VARS: alist of (name . init-spec) for special variables.

   Returns vector of bytes for Wasm global section (section ID 6)."
  (let* ((globals (generate-all-globals-with-specials special-vars))
         (global-bytes '())
         (global-count (length globals)))
    ;; Encode each global definition
    (dolist (global-def globals)
      (dolist (byte global-def)
        (push byte global-bytes)))
    ;; Reverse to get correct order
    (setf global-bytes (nreverse global-bytes))
    ;; Build section: ID + size + count + global definitions
    (let* ((count-bytes (encode-unsigned-leb128 global-count))
           (content-size (+ (length count-bytes) (length global-bytes)))
           (size-bytes (encode-unsigned-leb128 content-size))
           (section '()))
      ;; Section ID 6 (global)
      (push 6 section)
      ;; Section size
      (dolist (b size-bytes) (push b section))
      ;; Global count
      (dolist (b count-bytes) (push b section))
      ;; Global definitions
      (dolist (b global-bytes) (push b section))
      ;; Return as byte vector
      (coerce (nreverse section) '(vector (unsigned-byte 8))))))

;;; ============================================================
;;; Symbol Interning (T011: intern-symbol for host-side evaluation)
;;; ============================================================

(defvar *symbol-table* (make-hash-table :test 'equal)
  "Symbol table for interned symbols. Uses string-based equality
   to allow symbols to be looked up by name string.")

(defstruct (stage0-symbol (:conc-name sym-))
  "Representation of an interned symbol for Stage 0.
   Holds the symbol name and optional value/function bindings."
  (name "" :type string)
  (value nil)
  (function nil)
  (plist nil))

(defun intern-symbol (name)
  "Intern a symbol by name string. Returns existing symbol if already
   interned, otherwise creates and stores a new symbol.
   Uses string-based equality (case-sensitive)."
  (let ((name-string (if (stringp name)
                         name
                         (symbol-name name))))
    (or (gethash name-string *symbol-table*)
        (setf (gethash name-string *symbol-table*)
              (make-stage0-symbol :name name-string)))))

(defun find-symbol* (name)
  "Find an already-interned symbol by name. Returns NIL if not found."
  (let ((name-string (if (stringp name)
                         name
                         (symbol-name name))))
    (gethash name-string *symbol-table*)))

(defun clear-symbol-table ()
  "Clear all interned symbols. Useful for testing."
  (clrhash *symbol-table*))

(defun symbol-count ()
  "Return count of interned symbols."
  (hash-table-count *symbol-table*))
