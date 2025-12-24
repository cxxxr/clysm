;;;; compile.lisp - Compile function with tiered execution
;;;; Feature: 017-eval-jit-compile
;;;; Implements Constitution Principle VI: Tiered Dynamic Compilation

(in-package #:clysm/eval/compile)

;;; ============================================================
;;; Tiered Function Structure (T003)
;;; ============================================================

(defstruct tiered-function
  "A function that can be promoted from Tier 1 to Tier 2.
   - Tier 1: Interpreted execution via interpreter.lisp
   - Tier 2: JIT compiled Wasm execution"
  (name nil :type (or null symbol))           ; NIL for anonymous, symbol for named
  (definition nil :type cons)                  ; Original lambda expression
  (tier :tier-1 :type (member :tier-1 :tier-2))
  (implementation nil :type (or null function)) ; Current callable function
  (invocation-count 0 :type fixnum)
  (promotion-failed-p nil :type boolean))      ; Avoid retry on failure

;;; ============================================================
;;; Configuration
;;; ============================================================

(defvar *compilation-threshold* 10
  "Number of invocations before promoting to Tier 2.")

(defvar *invocation-counts* (make-hash-table :test 'eq)
  "Table tracking invocation counts for functions.
   Key: tiered-function, Value: fixnum count.")

;;; ============================================================
;;; Tiered Function Registry (for named functions)
;;; This is separate from jit's *function-slots* which stores
;;; callable functions. This registry stores tiered-function structs.
;;; ============================================================

(defvar *tiered-functions* (make-hash-table :test 'eq)
  "Maps symbol -> tiered-function for named functions.")

(defun get-tiered-function (symbol)
  "Get the tiered-function registered for a symbol."
  (gethash symbol *tiered-functions*))

(defun (setf get-tiered-function) (value symbol)
  "Set the tiered-function for a symbol."
  (setf (gethash symbol *tiered-functions*) value))

(defun lookup-compiled-function (symbol)
  "Lookup a compiled function by symbol name.
   Returns the callable implementation, not the tiered-function wrapper."
  (let ((tf (get-tiered-function symbol)))
    (when tf
      (tiered-function-implementation tf))))

;;; ============================================================
;;; Testing Utilities (T005)
;;; ============================================================

(defun reset-invocation-counts ()
  "Reset all invocation counts. For testing purposes."
  (clrhash *invocation-counts*))

(defun reset-tiered-functions ()
  "Reset all tiered function registrations. For testing purposes."
  (clrhash *tiered-functions*))

;;; ============================================================
;;; Tier Management
;;; ============================================================

(defun get-current-tier (tiered-fn)
  "Get the current tier of a tiered-function."
  (tiered-function-tier tiered-fn))

(defun should-promote-to-tier-2-p (tiered-fn)
  "Check if a function should be promoted to Tier 2 compilation."
  (and (eq (tiered-function-tier tiered-fn) :tier-1)
       (not (tiered-function-promotion-failed-p tiered-fn))
       (>= (tiered-function-invocation-count tiered-fn) *compilation-threshold*)))

(defun record-invocation (tiered-fn)
  "Record an invocation of a tiered function and return new count."
  (incf (tiered-function-invocation-count tiered-fn)))

(defun attempt-tier-promotion (tiered-fn)
  "Attempt to promote a function to Tier 2.
   Returns T on success, NIL on failure.
   On failure, marks promotion-failed-p to avoid retrying."
  (handler-case
      (let* ((definition (tiered-function-definition tiered-fn))
             (jit-fn (clysm/eval/jit:jit-compile definition)))
        ;; Success: update implementation and tier
        (setf (tiered-function-implementation tiered-fn) jit-fn)
        (setf (tiered-function-tier tiered-fn) :tier-2)
        ;; Hot-patch function slot if named
        (when (tiered-function-name tiered-fn)
          (clysm/eval/jit:hotpatch-function
           (tiered-function-name tiered-fn)
           jit-fn))
        t)
    (error (e)
      ;; Graceful degradation: stay in Tier 1, don't retry
      (declare (ignore e))
      (setf (tiered-function-promotion-failed-p tiered-fn) t)
      nil)))

(defun promote-to-tier-2 (tiered-fn)
  "Promote a function to Tier 2 (JIT compiled Wasm).
   For explicit promotion requests."
  (attempt-tier-promotion tiered-fn))

;;; ============================================================
;;; Tiered Function Wrapper (T004)
;;; ============================================================

(defmacro tiered-function-wrapper (tiered-fn)
  "Create a wrapper function that tracks invocations and handles promotion.
   The wrapper is callable and delegates to the current implementation."
  (let ((tf (gensym "TIERED-FN-")))
    `(let ((,tf ,tiered-fn))
       (lambda (&rest args)
         ;; Record invocation
         (record-invocation ,tf)
         ;; Check for promotion
         (when (should-promote-to-tier-2-p ,tf)
           (attempt-tier-promotion ,tf))
         ;; Execute current implementation
         (apply (tiered-function-implementation ,tf) args)))))

(defun make-tiered-wrapper (tiered-fn)
  "Create a wrapper function for a tiered-function.
   Returns a callable function that handles tier promotion."
  (lambda (&rest args)
    ;; Record invocation
    (record-invocation tiered-fn)
    ;; Check for promotion
    (when (should-promote-to-tier-2-p tiered-fn)
      (attempt-tier-promotion tiered-fn))
    ;; Execute current implementation
    (apply (tiered-function-implementation tiered-fn) args)))

;;; ============================================================
;;; Lambda Validation
;;; ============================================================

(defun validate-lambda-expr (definition)
  "Validate that DEFINITION is a proper lambda expression.
   Signals an error if invalid."
  (unless (and (consp definition)
               (eq (first definition) 'lambda))
    (error "COMPILE requires a lambda expression, got: ~S" definition))
  (unless (and (cdr definition)
               (listp (second definition)))
    (error "COMPILE: lambda must have a parameter list, got: ~S" definition))
  t)

;;; ============================================================
;;; Main Compile Function
;;; ============================================================

(defun compile* (name definition)
  "Compile a function definition.
   NAME is optional - if NIL, compile an anonymous function.
   DEFINITION should be a lambda expression.
   Returns a callable function that participates in tier promotion.

   Per Constitution Principle VI (Tiered Dynamic Compilation):
   - Tier 1: Immediate execution via interpreter (no compile overhead)
   - Tier 2: JIT compilation to Wasm after hot spot detection"
  ;; Validate input
  (validate-lambda-expr definition)

  ;; Create Tier 1 implementation using interpreter
  (let* ((tier1-impl (clysm/eval/interpreter:interpret definition))
         ;; Create tiered-function structure
         (tiered-fn (make-tiered-function
                     :name name
                     :definition definition
                     :tier :tier-1
                     :implementation tier1-impl
                     :invocation-count 0
                     :promotion-failed-p nil))
         ;; Create callable wrapper
         (wrapper (make-tiered-wrapper tiered-fn)))
    ;; Register in tiered-functions registry if named
    (when name
      (setf (get-tiered-function name) tiered-fn)
      ;; Also register the wrapper in jit's function slots for callable lookup
      (clysm/eval/jit:set-function-slot name wrapper))
    ;; Return the wrapper
    wrapper))

;;; ============================================================
;;; Advanced Compile Options
;;; ============================================================

(defun compile-with-tier (tier definition)
  "Compile a definition directly at a specific tier.
   TIER can be :tier-1 (interpreter) or :tier-2 (JIT).
   Bypasses the normal tier promotion mechanism."
  (validate-lambda-expr definition)
  (ecase tier
    (:tier-1
     (clysm/eval/interpreter:interpret definition))
    (:tier-2
     (clysm/eval/jit:jit-compile definition))))
