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

(defun clysm-sys::%host-macro-function* (sym &optional env)
  (macro-function-in sym (if (cenv-p env) env nil)))

(define-host-function clysm-sys::%define-symbol-macro (sym expansion)
  (setf (gethash sym *symbol-macros*) expansion)
  (setf (get sym 'clysm-sys::%symbol-macro) (list expansion))
  sym)

;;; (a compiler macro of (SETF NAME) is that of the symbol calls of the
;;; function (SETF NAME) become)
(defun compiler-macro-key (name)
  (if (and (consp name) (eq (car name) 'setf)) (setf-function-symbol (second name)) name))

#-clysm
(defmacro clysm-sys::%host-define-compiler-macro (name ll &body body)
  `(table-put '*compiler-macros* ',(compiler-macro-key name)
              (host-compile ',(make-host-macro-expander name ll body))))


;;; DEFSTRUCT accessors are compiled inline (as compiler macros) at call sites
;;; compiled after the DEFSTRUCT, unless declared NOTINLINE.
(defun register-struct-accessor-inline (acc name idx writable &optional fields kinds)
  ;; FIELDS: the shape of the structure (runtime/struct.lisp): its number of
  ;; slots when they are fields of its own (src/layout.lisp), else 0, plus 64
  ;; times its depth
  ;; KINDS: the kinds of the slots of a structure with typed slots
  (flet ((ref (obj) (if kinds
                        `(clysm-sys::%struct-tref ,obj (load-time-value (find-class ',name)) ,idx ',kinds)
                      (if (and fields (plusp (mod fields 64)))
                        `(clysm-sys::%struct-iref ,obj (load-time-value (find-class ',name)) ,idx ,fields)
                        `(clysm-sys::%struct-ref ,obj (load-time-value (find-class ',name)) ,idx
                                                 ,@(when fields (list fields)))))))
    (table-put '*compiler-macros* acc
          (lambda (form env)
            (declare (ignore env))
            (if (and (consp (cdr form)) (null (cddr form))) (ref (second form)) form)))
    (when writable
      (table-put '*compiler-macros* (setf-function-symbol acc)
            (lambda (form env)
              (if (and (consp (cdr form)) (consp (cddr form)) (null (cdddr form)))
                  (let ((form (list (first form)
                                    (if (and (cenv-p env) (trust-types-p env))
                                        (second form)
                                        (struct-slot-checked-value name idx (second form)))
                                    (third form))))
                  (cond
                    (kinds `(clysm-sys::%struct-tset ,(third form) (load-time-value (find-class ',name)) ,idx
                                                     ,(second form) ',kinds))
                    ((and fields (plusp (mod fields 64)))
                      `(clysm-sys::%struct-iset ,(third form) (load-time-value (find-class ',name)) ,idx
                                                ,(second form) ,fields))
                    (t
                      `(clysm-sys::%struct-set ,(third form) (load-time-value (find-class ',name)) ,idx
                                               ,(second form) ,@(when fields (list fields))))))
                  form))))
    acc))

(defun struct-slot-checked-value (name idx val)
  "VAL checked against the type of the slot IDX of the structure NAME, when
it is one that compiled reads trust (runtime/struct.lisp)."
  (let* ((desc (get name 'clysm-sys::%struct-description))
         (slot (and (consp desc) (listp (second desc))
                    (find idx (second desc) :key (lambda (s) (and (consp s) (nth 4 s))))))
         (clysm-sys::*proclaimed-safety* 1))
    (if (consp slot)
        (clysm-sys::%slot-store-check-form slot val name (nth 4 desc))
        val)))

(defun register-struct-predicate-inline (predicate name &optional fields)
  (table-put '*compiler-macros* predicate
        (lambda (form env)
          (declare (ignore env))
          (if (and (consp (cdr form)) (null (cddr form)))
              `(clysm-sys::%structure-typep ,(second form) (load-time-value (find-class ',name))
                                            ,@(when fields (list fields)))
              form)))
  predicate)

#-clysm
(defmacro clysm-sys::%host-inline-struct-accessor (acc name idx writable &optional fields kinds)
  `(register-struct-accessor-inline ',acc ',name ',idx ',writable ',fields ',kinds))

#-clysm
(defmacro clysm-sys::%ct-initial-value (sym form)
  ;; record constant initial values so that the symbol is bound from the start
  (let ((value (cond ((or (numberp form) (characterp form) (stringp form) (keywordp form)
                          (null form) (eq form t))
                      (list form))
                     ((and (consp form) (eq (car form) 'quote)
                           ;; (not an object only a fasl can hold: a program built in the image)
                           (or (not *static-build*) (plain-constant-p (second form))))
                      (list (second form)))
                     (t nil))))
    (if value
        `(table-put '*initial-symbol-values* ',sym ',value)
        `(table-remove '*initial-symbol-values* ',sym))))

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
                   ;; (not while a program is built in this image: the
                   ;; variable is defined when its file is loaded, a constant)
                   ,@(unless (eq (symbol-package sym) (find-package :cl))
                       `((unless *static-build*
                           (proclaim '(special ,sym))
                           (setf (symbol-value ',sym) value)))))
               (error () (clysm-sys::%compile-time-special ',sym))))))
  (setf (macro-function 'clysm-sys::%host-define-compiler-macro)
        (lambda (form env)
          (declare (ignore env))
          (destructuring-bind (name ll &rest body) (cdr form)
            `(table-put '*compiler-macros* ',(compiler-macro-key name)
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
                              ((and (consp vform) (eq (car vform) 'quote)
                                    (or (not *static-build*) (plain-constant-p (second vform))))
                               (list (second vform)))
                              (t nil))))
            (if value
                `(table-put '*initial-symbol-values* ',sym ',value)
                `(table-remove '*initial-symbol-values* ',sym))))))
