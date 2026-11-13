;;; Clysm benchmark suite — each benchmark is run on SBCL and on Clysm
;;; (tools/bench.sh) and the times compared.

(defvar *bench-filter* nil)

(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn
     (defun ,name () ,@body)
     (run-bench ',name ,reps #',name)))

(defun run-bench (name reps fn)
  (funcall fn)                          ; warm up
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                     internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))

;;; ---- function calls / fixnum arithmetic
(defun fib (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(defbench b-fib (3) (fib 32))

(defun tak (x y z) (if (not (< y x)) z (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))
(defbench b-tak (20) (tak 24 16 8))

;;; ---- loops / arrays
(defun count-primes (n)
  (let ((sieve (make-array (1+ n) :initial-element t)) (count 0))
    (loop for i from 2 to n
          when (aref sieve i)
            do (incf count)
               (loop for j from (* i i) to n by i do (setf (aref sieve j) nil)))
    count))
(defbench b-sieve (5) (count-primes 2000000))

(defun matmul (n)
  (let ((a (make-array (list n n) :initial-element 1))
        (b (make-array (list n n) :initial-element 2))
        (c (make-array (list n n) :initial-element 0)))
    (dotimes (i n)
      (dotimes (j n)
        (let ((s 0))
          (dotimes (k n) (incf s (* (aref a i k) (aref b k j))))
          (setf (aref c i j) s))))
    (aref c (1- n) (1- n))))
(defbench b-matmul (20) (matmul 120))

;;; ---- floats
(defun float-loop (n)
  (let ((x 0d0))
    (dotimes (i n) (setq x (+ (* x 0.999d0) (sqrt (float i 1d0)))))
    x))
(defbench b-float (3) (float-loop 2000000))

(defun mandel (size)
  (let ((count 0))
    (dotimes (py size)
      (dotimes (px size)
        (let ((cr (- (/ (* 3.0d0 px) size) 2.0d0))
              (ci (- (/ (* 2.0d0 py) size) 1.0d0))
              (zr 0d0) (zi 0d0) (i 0))
          (loop while (and (< i 50) (< (+ (* zr zr) (* zi zi)) 4d0))
                do (psetq zr (+ (- (* zr zr) (* zi zi)) cr)
                          zi (+ (* 2d0 zr zi) ci))
                   (incf i))
          (when (= i 50) (incf count)))))
    count))
(defbench b-mandel (5) (mandel 200))

;;; ---- lists / consing
(defun deriv (a)
  (cond ((not (consp a)) (if (eq a 'x) 1 0))
        ((eq (car a) '+) (cons '+ (mapcar #'deriv (cdr a))))
        ((eq (car a) '-) (cons '- (mapcar #'deriv (cdr a))))
        ((eq (car a) '*) (list '* a (cons '+ (mapcar (lambda (b) (list '/ (deriv b) b)) (cdr a)))))
        ((eq (car a) '/) (list '- (list '/ (deriv (cadr a)) (caddr a))
                               (list '/ (cadr a) (list '* (caddr a) (caddr a) (deriv (caddr a))))))
        (t 'error)))
(defbench b-deriv (10)
  (let ((r nil))
    (dotimes (i 200000) (setq r (deriv '(+ (* 3 x x) (* a x x) (* b x) 5))))
    (length r)))

(defbench b-list (20)
  (let ((total 0))
    (dotimes (k 50)
      (let ((l (loop for i below 10000 collect i)))
        (setq l (mapcar #'1+ l))
        (setq l (remove-if #'evenp l))
        (setq l (reverse l))
        (incf total (logand (reduce #'+ l) #xFFFF))
        (incf total (length (append l l)))
        (incf total (count-if #'oddp l))
        (when (member 9999 l) (incf total))))
    total))

(defbench b-sort (5)
  (let ((l (loop for i below 100000 collect (mod (* i 7919) 100003))))
    (setq l (sort l #'<))
    (let ((v (coerce (loop for i below 100000 collect (mod (* i 7919) 100003)) 'vector)))
      (setq v (sort v #'>))
      (+ (first l) (aref v 0)))))

;;; ---- hash tables
(defbench b-hash (20)
  (let ((h (make-hash-table)) (s 0))
    (dotimes (i 200000) (setf (gethash i h) (* i 2)))
    (dotimes (i 200000) (setq s (logand (+ s (gethash i h)) #xFFFFFF)))
    s))

(defbench b-hash-string (30)
  (let ((h (make-hash-table :test 'equal)) (s 0)
        (keys (loop for i below 20000 collect (format nil "key-~D" i))))
    (dolist (k keys) (setf (gethash k h) (length k)))
    (dotimes (j 5) (dolist (k keys) (incf s (gethash k h))))
    s))

;;; ---- strings / characters
(defbench b-string (25)
  (let ((s (make-string 100000 :initial-element #\a)) (n 0))
    (dotimes (i (length s)) (setf (char s i) (code-char (+ 97 (mod i 26)))))
    (dotimes (k 10)
      (incf n (count #\e s))
      (incf n (length (string-upcase s)))
      (incf n (or (search "xyz" s) 0))
      (incf n (position #\z s)))
    n))

(defbench b-format (50)
  (length (with-output-to-string (o)
            (dotimes (i 20000) (format o "~D ~A ~S~%" i "x" 'foo)))))

(defbench b-print (7)
  (let ((n 0))
    (dotimes (i 20000) (incf n (length (prin1-to-string (list i 1.5d0 "s" 'sym)))))
    n))

(defbench b-read (20)
  (let ((n 0))
    (dotimes (i 5000)
      (incf n (length (read-from-string "(defun foo (x y) (let ((z (+ x y 12345))) (list z \"str\" 'sym #\\a)))"))))
    n))

;;; ---- bignums / ratios
(defun fact (n) (if (< n 2) 1 (* n (fact (1- n)))))
(defbench b-bignum (50) (integer-length (fact 2000)))

(defbench b-ratio (50)
  (let ((s 0)) (dotimes (i 2000) (incf s (/ 1 (1+ i)))) (integer-length (denominator s))))

;;; ---- structures / CLOS
(defstruct pt x y)
(defbench b-struct (20)
  (let ((p (make-pt :x 0 :y 0)))
    (dotimes (i 3000000) (setf (pt-x p) (+ (pt-x p) 1) (pt-y p) (logand (+ (pt-y p) (pt-x p)) #xFFFFFF)))
    (pt-y p)))

(defclass shape () ())
(defclass circle (shape) ((r :initarg :r :accessor r)))
(defclass rect (shape) ((w :initarg :w :accessor w) (h :initarg :h :accessor h)))
(defgeneric area (s))
(defmethod area ((c circle)) (* 3 (r c) (r c)))
(defmethod area ((x rect)) (* (w x) (h x)))
(defbench b-clos (12)
  (let ((shapes (list (make-instance 'circle :r 2) (make-instance 'rect :w 3 :h 4))) (s 0))
    (dotimes (i 500000) (dolist (x shapes) (incf s (area x))))
    s))

(defbench b-make-instance (50)
  (let ((s 0)) (dotimes (i 100000) (incf s (r (make-instance 'circle :r (logand i 1023))))) s))

;;; ---- integers beyond 30-bit fixnums (fixnums in SBCL)
(defbench b-int64 (10)
  (let ((s 0) (p 1))
    (dotimes (i 1000000)
      (setq s (+ s (* i 1000)))
      (setq p (logand (* p 31) #xFFFFFFFFFFF)))
    (+ s p)))

;;; ---- closures, funcall, apply, &key, &rest, MV, specials, non-local exits
(defun kfun (a &key (b 1) (c 2)) (+ a b c))
(defun rfun (&rest xs) (length xs))
(defbench b-call (12)
  (let ((s 0) (f (lambda (x) (+ x 1))))
    (dotimes (i 1000000)
      (setq s (logand s #xFFFFFF))
      (incf s (funcall f i))
      (incf s (kfun i :c 3))
      (incf s (rfun 1 2 3))
      (incf s (apply #'+ (list i 1)))
      (multiple-value-bind (q r) (floor i 7) (incf s (+ q r))))
    s))

(defvar *sp* 0)
(defun use-sp () *sp*)
(defbench b-special (30)
  (let ((s 0))
    (dotimes (i 1000000) (let ((*sp* i)) (setq s (logand (+ s (use-sp)) #xFFFFFF))))
    s))

(defun thrower (i) (if (oddp i) (throw 'tg i) 0))
(defbench b-catch (25)
  (let ((s 0))
    (dotimes (i 300000)
      (setq s (logand s #xFFFFFF))
      (incf s (catch 'tg (thrower i)))
      (incf s (block b (unwind-protect (if (evenp i) (return-from b 1) 2) (incf s)))))
    s))
