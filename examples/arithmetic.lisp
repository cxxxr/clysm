;;; Arithmetic operations example
(defun square (x)
  (* x x))

(defun cube (x)
  (* x (* x x)))

(defun abs-val (x)
  (if (< x 0)
      (- 0 x)
      x))
