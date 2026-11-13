;;;; hostenv.lisp — host-side definitions used while expanding target macros
;;;;
;;;; Target macros are expanded on the host (SBCL).  Their expanders run host
;;;; code; this file provides host versions of the few SYSTEM functions that
;;;; expanders call and that cannot be written portably.

(in-package :clysm)

(define-host-function clysm-sys::%put (sym indicator value)
  (setf (get sym indicator) value))

#-clysm
(defmacro clysm-sys::%ct-constant (sym form)
  ;; record the constant for the compiler and, for non-CL symbols, also define
  ;; it on the host so that macro expanders can use it
  `(handler-case
       (let ((value ,form))
         (clysm-sys::%compile-time-constant ',sym value)
         ,@(unless (eq (symbol-package sym) (find-package :cl))
             `((proclaim '(special ,sym))
               (setf (symbol-value ',sym) value))))
     (error () (clysm-sys::%compile-time-special ',sym))))

;;; Package functions called by host-evaluated macro code: target package
;;; names (e.g. :ALEXANDRIA) designate the host packages that mirror them.
(defun clysm-sys::%host-find-package (designator)
  (cond ((packagep designator) designator)
        ((find-target-package (string designator) nil))
        (t (find-package designator))))
(defun %host-package-or-die (designator)
  (or (clysm-sys::%host-find-package designator)
      (error 'package-error :package designator)))
(defun clysm-sys::%host-intern (name &optional (package *package*))
  (intern name (%host-package-or-die package)))
(defun clysm-sys::%host-find-symbol (name &optional (package *package*))
  (find-symbol name (%host-package-or-die package)))
(defun clysm-sys::%host-package-name (package)
  (let ((p (%host-package-or-die package)))
    (or (gethash p *host->target-name*) (package-name p))))

(defun clysm-sys::%host-macro-function* (sym env)
  (macro-function-in sym (if (cenv-p env) env nil)))

(define-host-function clysm-sys::%define-symbol-macro (sym expansion)
  (setf (gethash sym *symbol-macros*) expansion)
  (setf (get sym 'clysm-sys::%symbol-macro) (list expansion))
  sym)

#-clysm
(defmacro clysm-sys::%host-define-compiler-macro (name ll &body body)
  `(setf (gethash ',name *compiler-macros*)
         (host-compile ',(make-host-macro-expander name ll body))))


;;; DEFSTRUCT accessors are compiled inline (as compiler macros) at call sites
;;; compiled after the DEFSTRUCT, unless declared NOTINLINE.
(defun register-struct-accessor-inline (acc name idx writable)
  (flet ((ref (obj) `(clysm-sys::%struct-ref ,obj (load-time-value (find-class ',name)) ,idx)))
    (setf (gethash acc *compiler-macros*)
          (lambda (form env)
            (declare (ignore env))
            (if (and (consp (cdr form)) (null (cddr form))) (ref (second form)) form)))
    (when writable
      (setf (gethash (setf-function-symbol acc) *compiler-macros*)
            (lambda (form env)
              (declare (ignore env))
              (if (and (consp (cdr form)) (consp (cddr form)) (null (cdddr form)))
                  `(clysm-sys::%struct-set ,(third form) (load-time-value (find-class ',name)) ,idx
                                           ,(second form))
                  form))))
    acc))

#-clysm
(defmacro clysm-sys::%host-inline-struct-accessor (acc name idx writable)
  `(register-struct-accessor-inline ',acc ',name ',idx ',writable))

#-clysm
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

;;; When the compiler runs inside a Clysm image these host macros are needed
;;; there too; install them at load time (see the #-clysm definitions above).
#+clysm
(progn
  (setf (macro-function 'clysm-sys::%ct-constant)
        (lambda (form env)
          (declare (ignore env))
          (let ((sym (second form)) (value-form (third form)))
            `(handler-case
                 (let ((value ,value-form))
                   (clysm-sys::%compile-time-constant ',sym value)
                   ,@(unless (eq (symbol-package sym) (find-package :cl))
                       `((proclaim '(special ,sym))
                         (setf (symbol-value ',sym) value))))
               (error () (clysm-sys::%compile-time-special ',sym))))))
  (setf (macro-function 'clysm-sys::%host-define-compiler-macro)
        (lambda (form env)
          (declare (ignore env))
          (destructuring-bind (name ll &rest body) (cdr form)
            `(setf (gethash ',name *compiler-macros*)
                   (host-compile ',(make-host-macro-expander name ll body))))))
  (setf (macro-function 'clysm-sys::%host-inline-struct-accessor)
        (lambda (form env)
          (declare (ignore env))
          `(register-struct-accessor-inline ,@(mapcar (lambda (x) `',x) (cdr form)))))
  (setf (macro-function 'clysm-sys::%ct-initial-value)
        (lambda (form env)
          (declare (ignore env))
          (let* ((sym (second form)) (vform (third form))
                 (value (cond ((or (numberp vform) (characterp vform) (stringp vform) (keywordp vform)
                                   (null vform) (eq vform t))
                               (list vform))
                              ((and (consp vform) (eq (car vform) 'quote)) (list (second vform)))
                              (t nil))))
            (if value
                `(setf (gethash ',sym *initial-symbol-values*) ',value)
                `(remhash ',sym *initial-symbol-values*))))))
