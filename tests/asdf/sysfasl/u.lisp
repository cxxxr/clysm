(in-package :sysfasl)
(eval-when (:compile-toplevel) (incf (symbol-value (find-symbol "*SYSFASL-CT*" "CL-USER"))))
(defun use () (list (scaled 3) (twice (pt-x (origin))) (scale 2) +k+))
(incf *loads*)
