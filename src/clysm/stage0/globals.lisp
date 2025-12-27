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
