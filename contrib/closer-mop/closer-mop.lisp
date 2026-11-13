;;;; closer-mop.lisp — Closer to MOP's packages for Clysm

(macrolet ((define-closer-mop-package ()
             (let ((mop-symbols (let ((r nil))
                                  (do-external-symbols (s "CLYSM-MOP") (push (symbol-name s) r))
                                  (sort r #'string<))))
               `(defpackage #:closer-mop
                  (:use #:common-lisp)
                  (:nicknames #:c2mop)
                  (:import-from #:clysm-mop ,@mop-symbols)
                  (:export ,@mop-symbols
                           #:defclass #:defgeneric #:define-method-combination #:defmethod
                           #:ensure-generic-function #:standard-generic-function #:standard-class
                           #:standard-method #:standard-object #:built-in-class #:class #:generic-function
                           #:method #:method-combination
                           #:classp #:ensure-finalized #:ensure-method #:fix-slot-initargs #:required-args
                           #:subclassp #:typep #:subtypep #:compute-effective-method-function
                           #:warn-on-defmethod-without-generic-function)))))
  (define-closer-mop-package))

(in-package #:closer-mop)

(define-symbol-macro warn-on-defmethod-without-generic-function nil)

(defun required-args (lambda-list &optional (collector #'identity))
  (loop for arg in lambda-list
        until (member arg lambda-list-keywords)
        collect (funcall collector arg)))

(defun ensure-finalized (class &optional (errorp t))
  (if (cl:typep class 'class)
      (unless (class-finalized-p class)
        (finalize-inheritance class))
      (when errorp (error "~S is not a class." class)))
  class)

(defun subclassp (class superclass)
  (flet ((get-class (class) (etypecase class
                              (class class)
                              (symbol (find-class class)))))
    (not (null (member (get-class superclass) (class-precedence-list (get-class class)))))))

(defun ensure-method (gf lambda-expression
                      &key (qualifiers ())
                           (lambda-list (cadr lambda-expression))
                           (specializers (required-args lambda-list (constantly (find-class 't)))))
  (let ((form `(defmethod ,(generic-function-name gf) ,@qualifiers
                 ,(loop for specializer in specializers
                        for (arg . rest) on lambda-list
                        collect `(,arg ,specializer) into args
                        finally (return (nconc args rest)))
                 ,@(cddr lambda-expression))))
    (eval form)))

(defun fix-slot-initargs (initargs) initargs)

(defun compute-effective-method-function (gf effective-method options)
  (declare (ignore options))
  (let ((f (eval `(lambda (sys::%emf-args) (declare (ignorable sys::%emf-args)) ,effective-method))))
    (declare (ignore gf))
    (lambda (&rest args) (funcall f args))))

(defun extract-lambda-list (specialized-lambda-list)
  (loop for (arg . rest) on specialized-lambda-list
        if (member arg lambda-list-keywords) append (cons arg rest) and do (loop-finish)
        else collect (if (consp arg) (car arg) arg)))

(defun extract-specializer-names (specialized-lambda-list)
  (loop for arg in specialized-lambda-list
        until (member arg lambda-list-keywords)
        collect (if (consp arg) (cadr arg) t)))

(export '(extract-lambda-list extract-specializer-names))

(macrolet ((define-closer-common-lisp-package ()
             (loop with symbols = (nunion (loop for sym being the external-symbols of :common-lisp
                                                if (find-symbol (symbol-name sym) :c2mop)
                                                  collect it
                                                else collect sym)
                                          (loop for sym being the external-symbols of :c2mop
                                                collect sym))
                   with map = '()
                   for symbol in symbols do
                     (push (symbol-name symbol)
                           (getf map (symbol-package symbol)))
                   finally (return
                             `(defpackage #:closer-common-lisp
                                (:nicknames #:c2cl)
                                (:use)
                                ,@(loop for (package symbols) on map by #'cddr
                                        collect `(:import-from ,(package-name package) ,@symbols))
                                (:export ,@(mapcar #'symbol-name symbols)))))))
  (define-closer-common-lisp-package))

(defpackage #:closer-common-lisp-user
  (:nicknames #:c2cl-user)
  (:use #:closer-common-lisp))
