;;; Floating-point overflow inside trees of arithmetic on doubles (undeclared
;;; and declared operands): signalled wherever in the tree it happens, and
;;; the values are right when it does not.

(defmacro trap (form)
  `(handler-case ,form
     (floating-point-overflow () :overflow)
     (division-by-zero () :division-by-zero)
     (arithmetic-error (c) (type-of c))))

(defun t1 (a b c) (+ (* a b) c))
(defun t2 (a b c d) (* (+ a b) (- c d)))
(defun t3 (a b c) (/ c (+ a b)))
(defun t4 (a b c) (sqrt (+ (* a a) (* b b) c)))
(defun t5 (a b c d) (- (+ a b c) d))
(defun t6 (a b) (+ (/ a b) 1d0))
(defun t7 (v i) (+ (* (aref v i) (aref v (1+ i))) (aref v 0)))
(defun t8 (a b) (let ((x (+ a b))) (* x 0.5d0)))

(defvar *big* 1d300)
(defvar *huge* most-positive-double-float)

(print (list (t1 2d0 3d0 4d0) (trap (t1 *big* *big* 1d0)) (trap (t1 1d0 1d0 *huge*))
             (trap (t1 *huge* 2d0 (- *huge*)))))
(print (list (t2 1d0 2d0 5d0 3d0) (trap (t2 *huge* *huge* 1d0 0d0)) (trap (t2 *huge* *huge* 0d0 0d0))
             (trap (t2 *huge* *huge* 1d0 1d0))))
(print (list (t3 1d0 3d0 8d0) (trap (t3 *huge* *huge* 1d0)) (trap (t3 1d0 -1d0 1d0))))
(print (list (t4 3d0 4d0 0d0) (trap (t4 *big* 1d0 0d0)) (trap (t4 1d200 1d200 1d0))))
(print (list (t5 1d0 2d0 3d0 4d0) (trap (t5 *huge* *huge* 0d0 *huge*)) (trap (t5 *huge* 1d0 1d0 *huge*))))
(print (list (t6 6d0 3d0) (trap (t6 1d0 0d0)) (trap (t6 *big* 1d-300))))
(let ((v (make-array 3 :initial-contents (list 2d0 *big* *big*))))
  (print (list (t7 v 0) (trap (t7 v 1)))))
(print (list (t8 1d0 2d0) (trap (t8 *huge* *huge*))))
(print (list (t1 2 3d0 4) (t2 1 2 3d0 1) (trap (t1 *big* 1d300 0))))

(defun d1 (a b c)
  (declare (double-float a b c))
  (+ (* a b) (* b c) (- a c)))
(defun d2 (a b c)
  (declare (double-float a b c))
  (* (+ a b) (+ b c) c))
(print (list (d1 1d0 2d0 3d0) (trap (d1 *big* *big* 0d0)) (trap (d1 *huge* 1d0 (- *huge*)))))
(print (list (d2 1d0 2d0 3d0) (trap (d2 *huge* *huge* 1d0)) (trap (d2 1d200 0d0 1d200))))
