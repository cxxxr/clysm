;;; tests/fasl-images/run.sh, the first image: classes of a package of its
;;; own (as Lem's are in the image that compiles micros' contribs), then a
;;; file whose generic function specializes STANDARD-OBJECT compiled here
(defpackage :fi-extra (:use :cl))
(in-package :fi-extra)
(defclass extra-1 () ())
(defclass extra-2 () ())
(defclass extra-3 (extra-2) ())
(in-package :cl-user)
(compile-file "tests/fasl-images/gf.lisp" :output-file (merge-pathnames "gf.fasl" cl-user::*fi-dir*))
