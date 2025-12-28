;;;; env.lisp - Environment management for Stage 0 interpreter
;;;;
;;;; Part of Feature 001: Phase 13D True Self-Hosting
;;;; Task T004: Environment module with extend-env and lookup
;;;;
;;;; Environment representation: association list
;;;; env = NIL | ((symbol . value) . env)
;;;;
;;;; Per research.md, we use simple association list for O(1) extension
;;;; and O(n) lookup, which is acceptable for bootstrap scope.

(in-package #:clysm/stage0)

;;; ============================================================
;;; Environment Creation
;;; ============================================================

(defun make-env ()
  "Create a fresh empty environment.
   Returns NIL representing the empty environment."
  nil)

(defun empty-env-p (env)
  "Check if environment is empty.
   Returns T if env is NIL, NIL otherwise."
  (null env))

;;; ============================================================
;;; Environment Extension
;;; ============================================================

(defun extend-env (symbol value env)
  "Extend environment with a new binding.
   Returns a new environment with (symbol . value) prepended.
   Does not modify the original environment."
  (cons (cons symbol value) env))

(defun extend-env* (bindings env)
  "Extend environment with multiple bindings at once.
   BINDINGS is a list of (symbol value) pairs.
   Returns new environment with all bindings added."
  (dolist (binding bindings)
    (setf env (extend-env (first binding) (second binding) env)))
  env)

;;; ============================================================
;;; Environment Lookup
;;; ============================================================

(defun lookup (symbol env)
  "Look up a symbol in the environment.
   Returns two values: (value found-p)
   If found, returns (value T).
   If not found, returns (NIL NIL)."
  (let ((pair (assoc symbol env :test #'eq)))
    (if pair
        (values (cdr pair) t)
        (values nil nil))))

;;; ============================================================
;;; Environment Utilities (for debugging)
;;; ============================================================

(defun env-bindings (env)
  "Return list of all bindings in environment as ((sym . val) ...)"
  env)

(defun env-symbols (env)
  "Return list of all bound symbols in environment"
  (mapcar #'car env))
