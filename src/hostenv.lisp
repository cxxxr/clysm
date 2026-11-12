;;;; hostenv.lisp — host-side definitions used while expanding target macros
;;;;
;;;; Target macros are expanded on the host (SBCL).  Their expanders run host
;;;; code; this file provides host versions of the few SYSTEM functions that
;;;; expanders call and that cannot be written portably.

(in-package :clysm)

(defun clysm-sys::%put (sym indicator value)
  (setf (get sym indicator) value))

(defmacro clysm-sys::%ct-constant (sym form)
  `(handler-case (clysm-sys::%compile-time-constant ',sym ,form)
     (error () (clysm-sys::%compile-time-special ',sym))))

(defun clysm-sys::%host-macro-function* (sym env)
  (macro-function-in sym (if (cenv-p env) env nil)))

(defun clysm-sys::%define-symbol-macro (sym expansion)
  (setf (gethash sym *symbol-macros*) expansion))

(defmacro clysm-sys::%host-define-compiler-macro (name ll &body body)
  `(setf (gethash ',name *compiler-macros*)
         (host-compile ',(make-host-macro-expander name ll body))))

(defvar *initial-symbol-values* (make-hash-table :test 'eq))

(defmacro clysm-sys::%ct-initial-value (sym form)
  ;; record constant initial values so that the symbol is bound from the start
  (let ((value (cond ((or (numberp form) (characterp form) (stringp form) (keywordp form)
                          (null form) (eq form t))
                      (list form))
                     ((and (consp form) (eq (car form) 'quote)) (list (second form)))
                     (t nil))))
    (if value
        `(setf (gethash ',sym *initial-symbol-values*) ',value)
        `(remhash ',sym *initial-symbol-values*))))
