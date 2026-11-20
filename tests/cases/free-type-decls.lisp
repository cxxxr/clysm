;;; Type declarations about variables bound outside the form that holds
;;; them (free declarations, CLHS 3.3.4) apply to the references within
;;; that form only; an integer type is checked there.

(defun make-matcher (next-fn n)
  (let ((test (lambda (x) (> x n))))
    (declare (function next-fn test))
    (lambda (x) (if (funcall test x) (funcall next-fn x) :no))))
(print (list (funcall (make-matcher #'1+ 3) 5) (funcall (make-matcher #'1+ 3) 2)))

(defun sum-to (n)
  (let ((s 0))
    (dotimes (i n)
      (locally (declare (fixnum s i))
        (setq s (+ s i))))
    s))
(print (sum-to 100))

;; outside the scope of the declaration the variable may hold anything
(defun maybe (x)
  (if (functionp x)
      (locally (declare (function x)) (funcall x 2))
      (list :not x)))
(print (list (maybe #'1+) (maybe 7) (maybe "s")))

;; an integer type declared free is checked where it holds
(defun bad (x)
  (locally (declare (fixnum x)) (+ x 1)))
(print (list (bad 3) (handler-case (bad "str") (type-error () :type-error))))

;; a declaration about a variable bound by the same form is its own
(defun mixed (v s)
  (let ((n (length v)))
    (declare (simple-vector v) (fixnum n) (simple-string s))
    (list n (svref v 0) (schar s 0))))
(print (mixed (vector 1 2) "ab"))

;; shadowing: the inner binding is not under the outer declaration
(defun shadow-test (f)
  (let ((g f))
    (declare (function g))
    (let ((g 5)) (list g (funcall f 1)))))
(print (shadow-test #'1+))

;; multiple-value-bind, let* and a lambda with a free declaration
(defun mvb (f)
  (multiple-value-bind (a b) (floor 7 2)
    (declare (function f))
    (funcall f a b)))
(defun lstar (v)
  (let* ((a 1) (b 2))
    (declare (simple-vector v))
    (+ a b (length v))))
(defun lam (k)
  (funcall (lambda (x) (declare (fixnum k)) (+ x k)) 10))
(print (list (mvb #'+) (lstar (vector 1 2 3)) (lam 5)))
