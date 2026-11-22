(in-package :sysdev)
(eval-when (:compile-toplevel) (incf (symbol-value (find-symbol "*SYSDEV-CT*" "CL-USER"))))
(defun g (x) (+ (f x) 1))
