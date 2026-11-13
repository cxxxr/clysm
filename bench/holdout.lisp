;;; Held-out programs — ordinary Common Lisp code outside bench/suite.lisp and
;;; bench/gabriel.lisp, to check that optimizations are not specific to those
;;; files (tools/bench.sh bench/holdout.lisp). Do not tune the compiler to the
;;; shape of this code; results must be single tokens (bench.sh compares them).
(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn (defun ,name () ,@body) (run-bench ',name ,reps #',name)))

(defun run-bench (name reps fn)
  (funcall fn)
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                     internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))

;;; ---------- A. perturbations of suite benchmarks ----------
;; fib through a closure passed as an argument (no direct self-call)
(defun cfib (f n) (if (< n 2) n (+ (funcall f f (- n 1)) (funcall f f (- n 2)))))
(defbench h-fib-funcall (12) (cfib #'cfib 30))

;; fib as a generic function
(defgeneric gfib (n))
(defmethod gfib ((n integer)) (if (< n 2) n (+ (gfib (- n 1)) (gfib (- n 2)))))
(defbench h-fib-generic (20) (gfib 30))

;; fib redefinable through a global variable holding a function
(defvar *fibfn*)
(defun vfib (n) (if (< n 2) n (+ (funcall *fibfn* (- n 1)) (funcall *fibfn* (- n 2)))))
(defbench h-fib-symbol (12) (setq *fibfn* 'vfib) (vfib 30))

;; mandel with single floats and complex numbers
(defun mandel-c (size)
  (let ((count 0))
    (dotimes (py size)
      (dotimes (px size)
        (let ((c (complex (- (/ (* 3.0 px) size) 2.0) (- (/ (* 2.0 py) size) 1.0)))
              (z #c(0.0 0.0)) (i 0))
          (loop while (and (< i 50) (< (abs z) 2.0))
                do (setq z (+ (* z z) c)) (incf i))
          (when (= i 50) (incf count)))))
    count))
(defbench h-mandel-complex (8) (mandel-c 120))

;; sieve with a bit vector
(defbench h-sieve-bits (20)
  (let* ((n 2000000) (s (make-array (1+ n) :element-type 'bit :initial-element 1)) (c 0))
    (loop for i from 2 to n
          when (= 1 (sbit s i))
            do (incf c) (loop for j from (* i i) to n by i do (setf (sbit s j) 0)))
    c))

;; matrix multiply with double-float 2-D arrays
(defbench h-matmul-double (120)
  (let* ((n 100)
         (a (make-array (list n n) :element-type 'double-float :initial-element 1.5d0))
         (b (make-array (list n n) :element-type 'double-float :initial-element 2.5d0))
         (c (make-array (list n n) :element-type 'double-float :initial-element 0d0)))
    (dotimes (i n)
      (dotimes (j n)
        (let ((s 0d0))
          (dotimes (k n) (incf s (* (aref a i k) (aref b k j))))
          (setf (aref c i j) s))))
    (aref c 7 7)))

;;; ---------- B. ordinary programs ----------
;; n-body: structures with double slots
(defstruct body x y z vx vy vz m)
(defun make-bodies ()
  (loop for i below 5
        collect (make-body :x (float i 1d0) :y (* 0.5d0 i) :z (- 1d0 i)
                           :vx 0d0 :vy (* 0.01d0 i) :vz 0d0 :m (+ 1d0 i))))
(defun advance (bodies dt)
  (loop for (a . rest) on bodies
        do (dolist (b rest)
             (let* ((dx (- (body-x a) (body-x b))) (dy (- (body-y a) (body-y b)))
                    (dz (- (body-z a) (body-z b)))
                    (d2 (+ (* dx dx) (* dy dy) (* dz dz) 0.01d0))
                    (mag (/ dt (* d2 (sqrt d2)))))
               (decf (body-vx a) (* dx (body-m b) mag)) (decf (body-vy a) (* dy (body-m b) mag))
               (decf (body-vz a) (* dz (body-m b) mag))
               (incf (body-vx b) (* dx (body-m a) mag)) (incf (body-vy b) (* dy (body-m a) mag))
               (incf (body-vz b) (* dz (body-m a) mag)))))
  (dolist (b bodies)
    (incf (body-x b) (* dt (body-vx b))) (incf (body-y b) (* dt (body-vy b)))
    (incf (body-z b) (* dt (body-vz b)))))
(defbench h-nbody (2)
  (let ((bs (make-bodies)))
    (dotimes (i 100000) (advance bs 0.001d0))
    (round (* 1000 (body-x (first bs))))))

;; a small Lisp interpreter (alists, closures, CASE on symbols)
(defun ev (x env)
  (cond ((symbolp x) (cdr (assoc x env)))
        ((atom x) x)
        (t (case (car x)
             (quote (cadr x))
             (if (if (ev (cadr x) env) (ev (caddr x) env) (ev (cadddr x) env)))
             (lambda (let ((params (cadr x)) (body (caddr x)))
                       (lambda (&rest args) (ev body (pairlis params args env)))))
             (t (let ((f (ev (car x) env)) (args (mapcar (lambda (a) (ev a env)) (cdr x))))
                  (apply f args)))))))
(defbench h-interp (50)
  (let* ((env (list (cons '< #'<) (cons '+ #'+) (cons '- #'-) (cons 'fib nil)))
         (fib (ev '(lambda (n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) env)))
    (setf (cdr (assoc 'fib env)) fib)
    (funcall fib 20)))

;; tokenizer + word frequency (strings, equal hash table, sort)
(defun words (text)
  (let ((out '()) (start nil))
    (dotimes (i (length text))
      (let ((c (char text i)))
        (if (alpha-char-p c)
            (unless start (setq start i))
            (when start (push (string-downcase (subseq text start i)) out) (setq start nil)))))
    (when start (push (string-downcase (subseq text start)) out))
    (nreverse out)))
(defparameter *text*
  (with-output-to-string (o)
    (dotimes (i 20000)
      (format o "~A ~A, ~A. " (nth (mod i 7) '("The" "quick" "Brown" "fox" "jumps" "over" "lazy"))
              (nth (mod (* i 3) 11) '("dog" "cat" "Emu" "yak" "gnu" "eel" "owl" "ant" "bee" "cow" "ram"))
              (format nil "w~D" (mod i 97))))))
(defbench h-wordfreq (20)
  (let ((h (make-hash-table :test 'equal)))
    (dolist (w (words *text*)) (incf (gethash w h 0)))
    (let ((l '())) (maphash (lambda (k v) (push (cons k v) l)) h)
      (setq l (sort l #'> :key #'cdr))
      (+ (* 1000 (length l)) (cdr (first l))))))

;; Dijkstra on a grid graph with a binary heap in an adjustable vector
(defun heap-push (h item key)
  (vector-push-extend (cons key item) h)
  (let ((i (1- (fill-pointer h))))
    (loop while (and (> i 0) (< (car (aref h i)) (car (aref h (floor (1- i) 2)))))
          do (rotatef (aref h i) (aref h (floor (1- i) 2))) (setq i (floor (1- i) 2)))))
(defun heap-pop (h)
  (let ((top (aref h 0)) (last (vector-pop h)) (n (fill-pointer h)))
    (when (> n 0)
      (setf (aref h 0) last)
      (let ((i 0))
        (loop (let* ((l (+ 1 (* 2 i))) (r (+ 2 (* 2 i))) (m i))
                (when (and (< l n) (< (car (aref h l)) (car (aref h m)))) (setq m l))
                (when (and (< r n) (< (car (aref h r)) (car (aref h m)))) (setq m r))
                (when (= m i) (return))
                (rotatef (aref h i) (aref h m)) (setq i m)))))
    top))
(defbench h-dijkstra (25)
  (let* ((w 150) (n (* w w)) (dist (make-array n :initial-element most-positive-fixnum))
         (h (make-array 16 :adjustable t :fill-pointer 0)))
    (setf (aref dist 0) 0) (heap-push h 0 0)
    (loop while (> (fill-pointer h) 0)
          do (destructuring-bind (d . u) (heap-pop h)
               (when (<= d (aref dist u))
                 (multiple-value-bind (y x) (floor u w)
                   (dolist (v (list (and (> x 0) (1- u)) (and (< x (1- w)) (1+ u))
                                    (and (> y 0) (- u w)) (and (< y (1- w)) (+ u w))))
                     (when v
                       (let ((nd (+ d 1 (mod (* v 31) 7))))
                         (when (< nd (aref dist v)) (setf (aref dist v) nd) (heap-push h v nd)))))))))
    (aref dist (1- n))))

;; CLOS with inheritance, :around/:before methods, call-next-method, slot-value
(defclass account () ((balance :initform 0 :initarg :balance) (log :initform 0)))
(defclass savings (account) ((rate :initform 2 :initarg :rate)))
(defclass checking (account) ((fee :initform 1)))
(defgeneric deposit (a amt))
(defmethod deposit ((a account) amt) (incf (slot-value a 'balance) amt))
(defmethod deposit :before ((a account) amt) (declare (ignore amt)) (incf (slot-value a 'log)))
(defmethod deposit ((a savings) amt) (call-next-method a (+ amt (slot-value a 'rate))))
(defmethod deposit :around ((a checking) amt)
  (if (> amt 100) (call-next-method a (- amt (slot-value a 'fee))) (call-next-method)))
(defbench h-clos-methods (15)
  (let ((accts (list (make-instance 'account) (make-instance 'savings :rate 3)
                     (make-instance 'checking))))
    (dotimes (i 100000) (dolist (a accts) (deposit a (mod i 200))))
    (reduce #'+ accts :key (lambda (a) (logand (slot-value a 'balance) #xFFFFFF)))))

;; sequence functions with :key/:test on vectors of conses
(defbench h-seqfns (10)
  (let ((v (coerce (loop for i below 20000 collect (cons (mod (* i 37) 1000) i)) 'vector)) (s 0))
    (dotimes (k 20)
      (incf s (count-if (lambda (x) (> x 500)) v :key #'car))
      (incf s (or (position 999 v :key #'car :from-end t) 0))
      (incf s (length (remove-duplicates (map 'list #'car (subseq v 0 2000)))))
      (incf s (reduce #'max v :key #'cdr))
      (incf s (length (delete-if #'oddp (map 'vector #'cdr v)))))
    s))

;; generic arithmetic: rationals and mixed float/integer, multiple values
(defbench h-numeric-mix (250)
  (let ((acc 0) (f 0d0))
    (dotimes (i 100000)
      (multiple-value-bind (q r) (truncate (* i 17) 13)
        (incf acc (+ q (* r 3)))
        (incf f (/ (float r) (1+ q)))
        (when (zerop (mod i 1000)) (incf acc (numerator (/ i 6))))))
    (+ (mod acc 1000003) (round f))))

;; FORMAT with iteration/float directives, string streams
(defbench h-report (40)
  (length (with-output-to-string (o)
            (dotimes (i 3000)
              (format o "~5D | ~10A | ~8,2F | ~{~A~^, ~}~%" i (format nil "item~D" i)
                      (/ i 7.0d0) (list i (* 2 i) 'x))))))

;; handler-case / conditions in a loop
(define-condition bad-value (error) ((v :initarg :v :reader bad-v)))
(defun check (x) (if (zerop (mod x 5)) (error 'bad-value :v x) x))
(defbench h-conditions (50)
  (let ((s 0))
    (dotimes (i 100000)
      (incf s (handler-case (check i) (bad-value (c) (- (bad-v c))))))
    s))

;; closures with captured mutable state, labels, apply
(defun make-counter (step) (let ((n 0)) (lambda () (incf n step))))
(defbench h-closures (25)
  (let ((cs (loop for i from 1 to 10 collect (make-counter i))) (s 0))
    (dotimes (k 100000)
      (labels ((sum (l) (if l (+ (funcall (car l)) (sum (cdr l))) 0)))
        (setq s (logand (+ s (sum cs)) #xFFFFFFF))))
    s))

;; symbols, plists, intern, assoc lists: a symbolic rule rewriter
(defun simplify (e)
  (if (atom e) e
      (let ((e (mapcar #'simplify e)))
        (destructuring-bind (op . args) e
          (case op
            (+ (let ((args (remove 0 args))) (cond ((null args) 0) ((null (cdr args)) (car args)) (t (cons '+ args)))))
            (* (cond ((member 0 args) 0)
                     (t (let ((args (remove 1 args))) (cond ((null args) 1) ((null (cdr args)) (car args)) (t (cons '* args)))))))
            (t e))))))
(defbench h-simplify (5)
  (let ((n 0))
    (dotimes (i 30000)
      (incf n (length (princ-to-string
                       (simplify `(+ (* 1 x ,(mod i 3)) (* y 1) (+ 0 (* z 0) ,(mod i 2)) (* (+ 0 a) 1)))))))
    n))
