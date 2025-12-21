;;;; eval.lisp - Eval function

(in-package #:clysm/eval)

(defun eval* (expr &optional env)
  "Evaluate an expression."
  ;; Start with Tier 1 interpreter
  (interpret expr env))
