;;; A package that shadows a name of a package it uses, defined before the
;;; used package exports the name (in another form of the program).
(defpackage :ps-user
  (:use :cl)
  (:shadow #:frob)
  (:export #:frob))
(defpackage :ps-lib
  (:use :cl)
  (:export #:frob #:other))
(defpackage :ps-both
  (:use :cl :ps-lib)
  (:shadowing-import-from :ps-user #:frob))
(in-package :ps-lib)
(defun frob () :lib)
(defun other () :other)
(in-package :ps-user)
(defun frob () :user)
(in-package :ps-both)
(print (list (frob) (other) (eq 'frob 'ps-user:frob)
             (mapcar #'symbol-name (package-shadowing-symbols :ps-both))))
(terpri)
