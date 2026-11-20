;;; Code declared (OPTIMIZE (SAFETY 0)) computes what it computes at the
;;; default safety when its declarations hold; code at the default safety
;;; still signals the errors.

;;; ------------------------------------------------------------ (safety 0)

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) tak0))
(defun tak0 (x y z)
  (declare (optimize (speed 3) (safety 0)) (fixnum x y z))
  (if (not (< y x))
      z
      (tak0 (tak0 (1- x) y z) (tak0 (1- y) z x) (tak0 (1- z) x y))))
(print (tak0 18 12 6))

;; a self call with a declared parameter, no FTYPE; its result through THE
(defun count-down (n acc)
  (declare (optimize (safety 0)) (fixnum n acc))
  (if (= n 0) acc (the fixnum (count-down (1- n) (+ acc n)))))
(print (count-down 1000 0))

(defun sum-fixnums (v)
  (declare (optimize (speed 3) (safety 0)) (type (simple-array fixnum (*)) v))
  (let ((s 0) (n 0))
    (declare (fixnum s n))
    (dotimes (i (length v))
      (incf s (aref v i))
      (incf n))
    (decf s 3)
    (list s n)))
(print (sum-fixnums (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 -100))))

(defun count-elements (v)
  (declare (optimize (safety 0)) (type (simple-array fixnum (*)) v))
  (dotimes (i (length v)) (decf (aref v i)) (incf (aref v i) 10))
  (let ((b (make-array 3 :element-type '(unsigned-byte 8) :initial-element 7)))
    (declare (type (simple-array (unsigned-byte 8) (*)) b))
    (setf (aref b 1) (+ (aref b 0) 200))
    (list v b)))
(print (count-elements (make-array 4 :element-type 'fixnum :initial-contents '(0 5 -5 100))))

(defun copy-perm (a b)
  (declare (optimize (safety 0)) (type (simple-array fixnum (*)) a b))
  (replace a b)
  a)
(let ((a (make-array 5 :element-type 'fixnum :initial-contents '(1 2 3 4 5)))
      (b (make-array 3 :element-type 'fixnum :initial-contents '(7 8 9))))
  (print (copy-perm a b))
  (print (copy-perm b (make-array 6 :element-type 'fixnum :initial-element 4)))
  (print (copy-perm a a)))

(defun float-ops (x y)
  (declare (optimize (speed 3) (safety 0)) (double-float x y))
  (list (+ x y) (- x y) (* x y) (/ x y) (* (+ x 1d0) (- y 0.5d0))))
(print (float-ops 1.5d0 0.25d0))

(declaim (inline recip))
(defun recip (i j)
  (declare (fixnum i j))
  (/ 1d0 (the fixnum (+ (ash (the fixnum (* (+ i j) (+ i j 1))) -1) i 1))))
(defun recip-sum (n)
  (declare (optimize (speed 3) (safety 0)) (fixnum n))
  (let ((s 0d0))
    (declare (double-float s))
    (dotimes (i n) (dotimes (j n) (incf s (* (recip i j) 2d0))))
    (round (* s 1d6))))
(print (recip-sum 50))

(defun single-ops (x)
  (declare (optimize (safety 0)) (single-float x))
  (list (* x 2.0) (+ x 0.5) (/ x 4.0)))
(print (single-ops 3.0))

;; blocks, with and without RETURN-FROM
(defun find-first-even (list)
  (declare (optimize (safety 0)))
  (dolist (x list :none)
    (when (evenp x) (return-from find-first-even x))))
(print (list (find-first-even '(1 3 4 5)) (find-first-even '(1 3))))
(defun exit-from-closure (list)
  (declare (optimize (safety 0)))
  (block outer
    (mapc (lambda (x) (when (> x 2) (return-from outer (* x 10)))) list)
    :done))
(print (list (exit-from-closure '(1 2 3)) (exit-from-closure '(1))))

(defun mixed (x)
  (declare (fixnum x))
  ;; (SAFETY 0) only in this LOCALLY
  (locally (declare (optimize (safety 0)))
    (let ((y (* x 3)))
      (declare (fixnum y))
      (+ y 1))))
(print (mixed 14))

;;; ------------------------------------------------------- default safety

;; (the checks of declared LET and SETQ variables are not tested here: the
;; evaluator of the image, INTERP=1 tools/run-tests.sh, does not make them)
(declaim (ftype (function (fixnum) fixnum) ftyped))
(defun ftyped (x) (1+ x))
(print (handler-case (ftyped (read-from-string "foo")) (type-error () :type-error)))

(defun overflow (x y)
  (declare (double-float x y))
  (* x y))
(print (handler-case (overflow most-positive-double-float 2d0)
         (floating-point-overflow () :floating-point-overflow)))

(defun aref-out (v i)
  (declare (type (simple-array fixnum (*)) v) (fixnum i))
  (aref v i))
(print (handler-case (aref-out (make-array 3 :element-type 'fixnum :initial-element 0) 3)
         (error () :error)))

(defun replace-default (a b)
  (declare (type (simple-array fixnum (*)) a b))
  (replace a b))
(print (replace-default (make-array 4 :element-type 'fixnum :initial-element 0)
                        (make-array 2 :element-type 'fixnum :initial-element 9)))
