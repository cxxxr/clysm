;;; Calls of known functions with fixnum and double arguments (passed and
;;; returned unboxed in their copies for those types): values, multiple
;;; values, tail calls, recursion.

(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(print (fib 25))

;; a self tail call a million deep (it must stay a tail call)
(defun count-to (n acc)
  (declare (fixnum n acc))
  (if (= n 0) acc (count-to (1- n) (logand (+ acc n) #xffffff))))
(print (count-to 1000000 0))

(defun sum-down (n acc)
  (if (<= n 0) acc (sum-down (- n 1) (+ acc 0.5d0))))
(print (sum-down 100000 0d0))

;; the result type depends on the arguments' types
(defun twice (x) (+ x x))
(print (list (twice 3) (twice 1.5d0) (twice 1/3) (+ 1 (twice 20)) (* 2d0 (twice 0.25d0))))

;; multiple values from a function also called for one value
(defun divmod (a b) (declare (fixnum a b)) (floor a b))
(print (list (divmod 17 5) (multiple-value-list (divmod 17 5))
             (+ 1 (divmod 17 5))
             (multiple-value-bind (q r) (divmod 23 4) (list q r))))

(defun dist (x y) (declare (double-float x y)) (sqrt (+ (* x x) (* y y))))
(defun sum-dists (n)
  (let ((s 0d0))
    (dotimes (i n) (incf s (dist (float i 1d0) 1d0)))
    (round s)))
(print (list (dist 3d0 4d0) (sum-dists 1000)))

;; mutual recursion with fixnums
(defun ev-p (n) (declare (fixnum n)) (if (= n 0) t (od-p (1- n))))
(defun od-p (n) (declare (fixnum n)) (if (= n 0) nil (ev-p (1- n))))
(print (list (ev-p 100000) (od-p 7)))

;; a function that returns a fixnum or NIL, and one leaving by RETURN-FROM
(defun find-pos (v x)
  (declare (simple-vector v))
  (dotimes (i (length v) nil) (when (eql (svref v i) x) (return-from find-pos i))))
(print (list (find-pos #(a b c) 'c) (find-pos #(a b c) 'd)))
(defun clamp (x lo hi)
  (declare (fixnum x lo hi))
  (cond ((< x lo) lo) ((> x hi) hi) (t x)))
(print (let ((s 0)) (dotimes (i 100 s) (incf s (clamp (- i 50) -10 10)))))

;; errors from inside such calls are still handled
(defun safe-div (a b) (declare (fixnum a b)) (truncate a b))
(print (handler-case (+ 1 (safe-div 10 0)) (division-by-zero () :division-by-zero)))
(defun tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x)) z (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))
(print (tak 18 12 6))
