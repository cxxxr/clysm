(defpackage :sysdev (:use :cl) (:export #:g))
(in-package :sysdev)
(eval-when (:compile-toplevel) (incf (symbol-value (find-symbol "*SYSDEV-CT*" "CL-USER"))))
(defun f (x) (* x 10))
