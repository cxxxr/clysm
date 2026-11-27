;;; Packages as literal objects of a compiled file (CLHS 3.2.4.2.2): read
;;; at read time by #., alone, in a list and in a vector, and loaded from
;;; the fasl as the packages of the same names (cxxxr/clysm#123).
(defpackage :cf-package-literal (:use :cl))

(defun run ()
  (with-open-file (s "build/cf-package-literal.lisp" :direction :output :if-exists :supersede)
    (write-string "(defun literal-package () #.(find-package :cl))
(defun literal-packages () '(#.(find-package :keyword) #.(find-package :cf-package-literal)))
(defun literal-package-vector () #(#.(find-package :cl-user) 1 \"x\"))
(defparameter *literal-package* #.(find-package :cf-package-literal))" s))
  (multiple-value-bind (fasl warnings-p failure-p)
      (compile-file "build/cf-package-literal.lisp")
    (print (list warnings-p failure-p))
    (load fasl))
  (print (list (eq (funcall 'literal-package) (find-package :cl))
               (equal (funcall 'literal-packages)
                      (list (find-package :keyword) (find-package :cf-package-literal)))
               (eq (svref (funcall 'literal-package-vector) 0) (find-package :cl-user))
               (eq (symbol-value '*literal-package*) (find-package :cf-package-literal)))))

(run)
