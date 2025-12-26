;;;; objects.lisp - Runtime object representations
;;;; Defines how Lisp objects are represented in Wasm globals

(in-package #:clysm/runtime/objects)

;;; ============================================================
;;; Singleton Sentinels (Host-side representation)
;;; ============================================================

(defvar +nil+ :clysm-nil
  "The NIL singleton (distinct from Wasm null).")

(defvar +unbound+ :clysm-unbound
  "The UNBOUND sentinel for unbound symbols.")

;;; ============================================================
;;; Global Index Management
;;; ============================================================
;;; Note: wasm-global struct is defined in clysm/backend/sections

;; Global indices (assigned during module construction)
(defvar *nil-global-index* 0
  "Global index for NIL singleton")

(defvar *unbound-global-index* 1
  "Global index for UNBOUND sentinel")

;;; ============================================================
;;; Multiple Values Globals (025-multiple-values)
;;; ============================================================

(defvar *mv-count-global-index* 2
  "Global index for multiple values count (i32)")

(defvar *mv-buffer-global-index* 3
  "Global index for multiple values buffer (ref $mv_array)")

(defvar *global-counter* 4
  "Next available global index")

;;; ============================================================
;;; NIL Singleton (T039)
;;; ============================================================

(defun make-nil-global ()
  "Create the NIL singleton global.
   NIL is represented as a struct.new for the $nil type."
  (make-wasm-global
   :name '$nil
   :type :anyref  ; Reference to $nil struct
   :mutability :const
   :init-expr '((:struct.new 0))))  ; struct.new $nil

(defun nil-global-index ()
  "Get the global index for NIL"
  *nil-global-index*)

(defun global-mutability (global)
  "Get the mutability of a global"
  (wasm-global-mutability global))

;;; ============================================================
;;; UNBOUND Sentinel (T040)
;;; ============================================================

(defun make-unbound-global ()
  "Create the UNBOUND sentinel global.
   Used to detect unbound variables at runtime."
  (make-wasm-global
   :name '$unbound
   :type :anyref  ; Reference to $unbound struct
   :mutability :const
   :init-expr '((:struct.new 1))))  ; struct.new $unbound

(defun unbound-global-index ()
  "Get the global index for UNBOUND"
  *unbound-global-index*)

;;; ============================================================
;;; Multiple Values Globals (025-multiple-values, T004-T005)
;;; ============================================================

(defun make-mv-count-global ()
  "Create the multiple values count global.
   Tracks how many values were returned by the most recent function.
   Default is 1 (single-value context)."
  (make-wasm-global
   :name '$mv_count
   :type :i32
   :mutability :var
   :init-expr '((:i32.const 1))))

(defun make-mv-buffer-global ()
  "Create the multiple values buffer global.
   Stores secondary values (values beyond the primary).
   Fixed size of 20 elements (anyref) for multiple-value storage.
   Type 22 is $mv_array defined in the type section."
  (make-wasm-global
   :name '$mv_buffer
   :type '(:ref 22)  ; (ref $mv_array) - type 22 in type section
   :mutability :var
   :init-expr '((:i32.const 20) (:array.new_default 22))))  ; array.new_default $mv_array 20

(defun mv-count-global-index ()
  "Get the global index for mv-count"
  *mv-count-global-index*)

(defun mv-buffer-global-index ()
  "Get the global index for mv-buffer"
  *mv-buffer-global-index*)

;;; ============================================================
;;; NIL Check Emission
;;; ============================================================

(defun emit-nil-check ()
  "Generate Wasm instructions to check if TOS is NIL.
   Returns a list of Wasm instructions.
   Uses ref.is_null or comparison with NIL global."
  ;; Strategy: compare with the NIL global using ref.eq
  ;; Stack before: [value]
  ;; Stack after: [i32] (1 if NIL, 0 otherwise)
  (list :global.get *nil-global-index*  ; get NIL global
        :ref.eq))                        ; compare with TOS

;;; ============================================================
;;; Symbol Management
;;; ============================================================

(defstruct (runtime-symbol (:constructor make-runtime-symbol))
  "Runtime symbol representation"
  (name "" :type string)
  (value-index nil :type (or null fixnum))  ; global index for value
  (function-index nil :type (or null fixnum))  ; function index
  (plist-index nil :type (or null fixnum)))  ; global index for plist

(defvar *symbol-table* (make-hash-table :test 'equal)
  "Table mapping symbol names to runtime-symbol structs")

(defvar *function-registry* (make-hash-table :test 'eq)
  "Registry of compiled functions")

(defun register-function (name index)
  "Register a function with its index (T060)"
  (setf (gethash name *function-registry*) index))

(defun lookup-function (name)
  "Look up a function index by name"
  (gethash name *function-registry*))

;;; ============================================================
;;; Helper Functions for Object Creation
;;; ============================================================

(defun make-cons (car cdr)
  "Create a cons cell."
  (cons car cdr))

(defun car* (cons)
  "Get the CAR of a cons cell."
  (car cons))

(defun cdr* (cons)
  "Get the CDR of a cons cell."
  (cdr cons))

(defun make-symbol* (name)
  "Create a new symbol."
  (list :symbol name +unbound+ +unbound+ +nil+ +nil+))

;;; ============================================================
;;; Runtime Globals Collection
;;; ============================================================

(defun generate-runtime-globals ()
  "Generate all runtime globals for the module.
   Global indices:
     0: NIL singleton
     1: UNBOUND sentinel
     2: mv-count (i32, mutable)
     3: mv-buffer (ref $mv_array, mutable)"
  (list (make-nil-global)
        (make-unbound-global)
        (make-mv-count-global)
        (make-mv-buffer-global)))

(defun allocate-global ()
  "Allocate a new global index"
  (prog1 *global-counter*
    (incf *global-counter*)))

(defun reset-global-counter ()
  "Reset the global counter for a new compilation"
  (setf *global-counter* 4)  ; 0=NIL, 1=UNBOUND, 2=mv-count, 3=mv-buffer
  (clrhash *symbol-table*)
  (clrhash *function-registry*))
