;;; tests/fasl-images/run.sh: compiled in an image with the package
;;; FI-EXTRA, loaded in one without it (as micros' inspector: one method on
;;; STANDARD-OBJECT, whose subclasses in the image the tree may name)
(defpackage :fi-gf (:use :cl))
(in-package :fi-gf)
(defgeneric describe-it (object))
(defmethod describe-it ((object standard-object)) (list :standard-object (type-of object)))
(defun use-it (object) (describe-it object))
