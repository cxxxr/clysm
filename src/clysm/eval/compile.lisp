;;;; compile.lisp - Compile function
;;;; Phase 9: Eval/JIT Infrastructure (T202-T203)

(in-package #:clysm/eval/compile)

;;; ============================================================
;;; Compile Function (T202)
;;; ============================================================

(defun compile* (name definition)
  "Compile a function definition.
   NAME is optional - if NIL, just compile the definition.
   DEFINITION should be a lambda expression.
   Returns a compiled function."
  (declare (ignore name))
  ;; For now, use the interpreter to create a callable function
  ;; This is Tier 1 - fast path using host Lisp
  ;;
  ;; In a full implementation, Tier 2 would JIT compile to Wasm
  ;; and return a function that calls into the Wasm module.
  (unless (and (consp definition)
               (eq (first definition) 'lambda))
    (error "COMPILE requires a lambda expression, got: ~S" definition))
  ;; Use the interpreter to create the function
  (clysm/eval/interpreter:interpret definition))

;;; ============================================================
;;; Tier Switching Logic (T203)
;;; ============================================================

(defvar *compilation-threshold* 10
  "Number of invocations before promoting to Tier 2.")

(defvar *invocation-counts* (make-hash-table :test 'eq)
  "Table tracking invocation counts for functions.")

(defun should-promote-to-tier-2-p (symbol)
  "Check if a function should be promoted to Tier 2 compilation."
  (let ((count (gethash symbol *invocation-counts* 0)))
    (> count *compilation-threshold*)))

(defun record-invocation (symbol)
  "Record an invocation of a function."
  (incf (gethash symbol *invocation-counts* 0)))

(defun promote-to-tier-2 (symbol definition)
  "Promote a function to Tier 2 (JIT compiled Wasm)."
  (let ((compiled-fn (clysm/eval/jit:jit-compile definition)))
    (clysm/eval/jit:hotpatch-function symbol compiled-fn)
    compiled-fn))

;;; ============================================================
;;; Advanced Compile Options
;;; ============================================================

(defun compile-with-tier (tier definition)
  "Compile a definition at a specific tier.
   TIER can be :tier-1 (interpreter) or :tier-2 (JIT)."
  (ecase tier
    (:tier-1
     (clysm/eval/interpreter:interpret definition))
    (:tier-2
     (clysm/eval/jit:jit-compile definition))))
