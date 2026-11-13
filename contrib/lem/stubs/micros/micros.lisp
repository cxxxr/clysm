(defpackage :micros
  (:use :cl)
  (:export #:function-name))
(in-package :micros)

(defun function-name (function)
  "The name of FUNCTION, or NIL."
  (nth-value 2 (function-lambda-expression function)))
