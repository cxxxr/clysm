(defun sum3 (a b c) (+ a b c))
(defun sum4 (a b c d) (+ a b c d))
(defun diff4 (a b c d) (- a b c d))
(defun mixed4 (a b c d) (+ (- a b) (- c d)))
(defvar *numbers* '(0 1 -1 536870911 -536870912 536870912 -536870913
                    2147483647 -2147483648 9223372036854775807
                    -9223372036854775808 9223372036854775808
                    1/3 0.25 0.5d0 #c(1 2)))
(dolist (a *numbers*)
  (dolist (b '(536870911 -536870912 1/2 0.5d0))
    (print (list (sum3 a b a) (sum4 a b a b) (diff4 a b a b) (mixed4 a b a b)))))
(print (mapcar (lambda (x) (handler-case (sum3 1 x 2) (type-error () :bad)))
               '(nil #\a x)))
(defvar *events* nil)
(defun record-value (x) (push x *events*) x)
(handler-case (+ (+ (record-value :bad) 1) (record-value 2)) (type-error () nil))
(print *events*)
