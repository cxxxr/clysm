;;;; special-vars.lisp - Special variable support (shallow binding)

(in-package #:clysm/runtime/special-vars)

(defvar *binding-stack* nil
  "The binding stack for shallow binding.")

(defun push-binding (symbol old-value)
  "Push a binding onto the stack."
  (push (cons symbol old-value) *binding-stack*))

(defun pop-binding ()
  "Pop a binding from the stack and restore the old value."
  (let ((frame (pop *binding-stack*)))
    (when frame
      ;; Return the old value
      (cdr frame))))
