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

(defvar *global-counter* 2
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
  "Generate all runtime globals for the module"
  (list (make-nil-global)
        (make-unbound-global)))

(defun allocate-global ()
  "Allocate a new global index"
  (prog1 *global-counter*
    (incf *global-counter*)))

(defun reset-global-counter ()
  "Reset the global counter for a new compilation"
  (setf *global-counter* 2)  ; 0=NIL, 1=UNBOUND
  (clrhash *symbol-table*)
  (clrhash *function-registry*))
