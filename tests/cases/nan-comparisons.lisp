;;; No comparison with a NaN is true (IEEE 754): <= and >= are their own
;;; comparisons, not the negations of > and <. And an infinite operand gives
;;; an infinite sum or product without FLOATING-POINT-OVERFLOW, which is for
;;; an infinite result of finite operands (SBCL's default traps). The
;;; infinity and the NaN are made by the runtime's unchecked double
;;; operations, so the expected output is in nan-comparisons.expected.

(defvar *inf* (sys::%dexpt2 2000))
(defvar *nan* (sys::%d- *inf* *inf*))

(defun cmp (x y) (list (< x y) (> x y) (<= x y) (>= x y) (= x y) (/= x y)))
(defun cmp-d (x y)
  (declare (double-float x y))
  (list (< x y) (> x y) (<= x y) (>= x y) (= x y) (/= x y)))

(print (cmp *nan* 0))
(print (cmp 0 *nan*))
(print (cmp *nan* *nan*))
(print (cmp *nan* 1d0))
(print (cmp 1.5d0 *nan*))
(print (cmp *nan* (expt 2 100)))
(print (cmp *nan* 1/3))
(print (cmp-d *nan* 1d0))
(print (list (<= 1 2) (<= 2 2) (<= 3 2) (>= 1 2) (>= 2 2) (>= 3 2)))
(print (list (<= 1d0 2) (<= 2 2d0) (<= 3d0 2d0) (>= 1/2 1d0) (>= 2d0 2) (>= (expt 2 70) 1d0)))
(print (list (<= 1 2 3) (<= 1 3 2) (>= 3 2 2) (>= 3 2 *nan*) (<= *nan* 1 2)))
(print (funcall #'>= *nan* 0))
(print (list (sort (list 3 1 2) #'<=) (sort (list 3 1 2) #'>=)))

(defmacro trap (form)
  `(handler-case ,form (floating-point-overflow () :overflow)))
(print (list (trap (= *inf* (+ *inf* 1)))
             (trap (= *inf* (+ 1d0 *inf*)))
             (trap (= *inf* (* *inf* 2)))
             (trap (= (- *inf*) (- 1d0 *inf*)))
             (trap (= *inf* (/ *inf* 2)))
             (trap (+ most-positive-double-float most-positive-double-float))
             (trap (* 1d300 1d300))))
