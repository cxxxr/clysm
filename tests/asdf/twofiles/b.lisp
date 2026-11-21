(in-package :twofiles)
(defun g (x) (+ (h x) 2))
(defun total (n) (let ((s 0)) (dotimes (i n s) (incf s (f i)))))
(incf *loads*)
