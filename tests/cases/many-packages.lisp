;;; Exercise both sides of the compact symbol table's package-index escape.
;;; Every package has the same symbol names, so a truncated index aliases them.
(defmacro many-package-definitions ()
  `(progn
     ,@(loop for i below 300
             collect `(defpackage ,(format nil "MANY-~D" i)
                        (:use :cl) (:export #:value #:*special*)))))
(many-package-definitions)

(defmacro many-package-values ()
  `(progn
     ,@(loop for i below 300
             for package = (find-package (format nil "MANY-~D" i))
             for special = (intern "*SPECIAL*" package)
             for public = (intern "VALUE" package)
             for internal = (intern "INTERNAL" package)
             append `((defparameter ,special ,i)
                      (defun ,public () (list ,special ',internal ',public))))))
(many-package-values)

(let ((count 0))
  (dotimes (i 300)
    (let* ((package (find-package (format nil "MANY-~D" i)))
           (public (find-symbol "VALUE" package))
           (special (find-symbol "*SPECIAL*" package))
           (values (funcall public)))
      (assert (= i (first values)))
      (assert (eq (symbol-package (second values)) package))
      (assert (eq (symbol-package (third values)) package))
      (assert (eq :internal (nth-value 1 (find-symbol "INTERNAL" package))))
      (assert (eq :external (nth-value 1 (find-symbol "VALUE" package))))
      (assert (eq :external (nth-value 1 (find-symbol "*SPECIAL*" package))))
      (assert (= i (symbol-value special)))
      (incf count)))
  (print count))
(assert (null (symbol-package '#:uninterned)))
(assert (eq :keyword (symbol-value :keyword)))
(terpri)
