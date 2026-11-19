;;; Clysm benchmarks — arithmetic in ordinary undeclared code: numbers in
;;; structure slots, lists, vectors and hash tables, parameters of unknown
;;; type, loops bounded by LENGTH, mixed fixnum / double code, integer sums
;;; that overflow into bignums.  No type declarations anywhere.
;;; Run with tools/bench.sh bench/arith.lisp.

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

;;; ---------------------------------------------------------------- n-body
;;; bodies in structures without slot types, forces through accessors
(defstruct body x y z vx vy vz mass)

(defun make-bodies (n)
  (loop for i below n
        collect (make-body :x (float i 1d0) :y (* 0.5d0 i) :z (- 3d0 i)
                           :vx 0d0 :vy 0.01d0 :vz 0d0 :mass (+ 1d0 (* 0.1d0 i)))))

(defun advance (bodies dt)
  (loop for (b . rest) on bodies
        do (dolist (b2 rest)
             (let* ((dx (- (body-x b) (body-x b2)))
                    (dy (- (body-y b) (body-y b2)))
                    (dz (- (body-z b) (body-z b2)))
                    (d2 (+ (* dx dx) (* dy dy) (* dz dz) 0.01d0))
                    (mag (/ dt (* d2 (sqrt d2)))))
               (decf (body-vx b) (* dx (body-mass b2) mag))
               (decf (body-vy b) (* dy (body-mass b2) mag))
               (decf (body-vz b) (* dz (body-mass b2) mag))
               (incf (body-vx b2) (* dx (body-mass b) mag))
               (incf (body-vy b2) (* dy (body-mass b) mag))
               (incf (body-vz b2) (* dz (body-mass b) mag)))))
  (dolist (b bodies)
    (incf (body-x b) (* dt (body-vx b)))
    (incf (body-y b) (* dt (body-vy b)))
    (incf (body-z b) (* dt (body-vz b)))))

(defun energy (bodies)
  (let ((e 0))
    (loop for (b . rest) on bodies
          do (incf e (* 0.5 (body-mass b)
                        (+ (expt (body-vx b) 2) (expt (body-vy b) 2) (expt (body-vz b) 2))))
             (dolist (b2 rest)
               (let ((dx (- (body-x b) (body-x b2))) (dy (- (body-y b) (body-y b2)))
                     (dz (- (body-z b) (body-z b2))))
                 (decf e (/ (* (body-mass b) (body-mass b2))
                            (sqrt (+ (* dx dx) (* dy dy) (* dz dz) 0.01)))))))
    e))

(defbench a-nbody-struct (5)
  (let ((bodies (make-bodies 8)))
    (dotimes (i 20000) (advance bodies 0.001d0))
    (round (* 1000 (energy bodies)))))

;;; ------------------------------------------------------------ statistics
;;; numbers in lists: fixnums, doubles and a mix of both

(defun mean (xs) (/ (reduce #'+ xs) (length xs)))
(defun variance (xs)
  (let ((m (mean xs)) (s 0))
    (dolist (x xs) (incf s (* (- x m) (- x m))))
    (/ s (length xs))))
(defun dot (xs ys)
  (let ((s 0)) (loop for x in xs for y in ys do (incf s (* x y))) s))
(defun normalize (xs)
  (let ((lo (reduce #'min xs)) (hi (reduce #'max xs)))
    (mapcar (lambda (x) (/ (- x lo) (- hi lo))) xs)))

(defbench a-stats-double (20)
  (let ((xs (loop for i below 5000 collect (* 0.37d0 (mod (* i 7919) 1000))))
        (r 0))
    (dotimes (k 10)
      (incf r (variance xs))
      (incf r (dot xs xs))
      (incf r (reduce #'+ (normalize xs))))
    (round r)))

(defbench a-stats-fixnum (20)
  (let ((xs (loop for i below 5000 collect (mod (* i 7919) 1000)))
        (r 0))
    (dotimes (k 10)
      (incf r (floor (variance (mapcar (lambda (x) (* 1d0 x)) xs))))
      (incf r (dot xs xs))
      (incf r (reduce #'max xs)))
    r))

(defbench a-mixed-poly (20)
  ;; Horner on a list of coefficients mixing fixnums and doubles
  (let ((coefs (list 3 -2.5d0 1 0.125d0 -7 2)) (s 0d0))
    (dotimes (i 100000)
      (let ((x (* 0.001d0 i)) (acc 0))
        (dolist (c coefs) (setq acc (+ (* acc x) c)))
        (setq s (+ s acc))))
    (round s)))

;;; -------------------------------------------------- integer loops, vectors

(defun vsum (v) (let ((s 0)) (dotimes (i (length v) s) (incf s (aref v i)))))
(defun vmax-index (v)
  (let ((best 0))
    (loop for i from 1 below (length v)
          when (> (aref v i) (aref v best)) do (setq best i))
    best))
(defun prefix-sums (v)
  (let ((out (make-array (length v))) (s 0))
    (dotimes (i (length v) out) (setf (aref out i) (incf s (aref v i))))))
(defun histogram (v nbins)
  (let ((h (make-array nbins :initial-element 0)))
    (loop for x across v do (incf (aref h (mod x nbins))))
    h))

(defbench a-vector-ints (20)
  (let ((v (make-array 20000)) (r 0))
    (dotimes (i (length v)) (setf (aref v i) (mod (* i 104729) 9973)))
    (dotimes (k 10)
      (incf r (vsum v))
      (incf r (vmax-index v))
      (incf r (aref (prefix-sums v) (1- (length v))))
      (incf r (reduce #'+ (histogram v 17))))
    r))

(defun collatz-len (n)
  (let ((len 1)) (loop until (= n 1) do (setq n (if (evenp n) (floor n 2) (+ (* 3 n) 1))) (incf len)) len))
(defun digit-sum (n) (let ((s 0)) (loop while (> n 0) do (multiple-value-bind (q r) (floor n 10) (incf s r) (setq n q))) s))

(defbench a-int-funcs (10)
  (let ((r 0))
    (loop for i from 1 to 30000 do (incf r (collatz-len i)) (incf r (digit-sum (* i i))))
    r))

(defbench a-sum-overflow (20)
  ;; sums that leave the fixnum range, then the 64-bit range
  (let ((s 0) (q 0))
    (dotimes (i 200000) (incf s (* i i)) (incf q (* i i i i)))
    (list s (integer-length q))))

;;; ------------------------------------------------------ small interpreter

(defun run-vm (code arg)
  ;; a stack machine over a list program: integers only
  (let ((stack (list arg)) (pc 0) (n (length code)))
    (loop while (< pc n)
          do (let ((op (svref code pc)))
               (incf pc)
               (case op
                 (:push (push (svref code pc) stack) (incf pc))
                 (:add (push (+ (pop stack) (pop stack)) stack))
                 (:sub (let ((b (pop stack)) (a (pop stack))) (push (- a b) stack)))
                 (:mul (push (* (pop stack) (pop stack)) stack))
                 (:mod (let ((b (pop stack)) (a (pop stack))) (push (mod a b) stack)))
                 (:dup (push (car stack) stack))
                 (:jnz (let ((target (svref code pc)))
                         (incf pc)
                         (unless (zerop (pop stack)) (setq pc target))))
                 (:swap (rotatef (first stack) (second stack))))))
    (car stack)))

(defbench a-interp (10)
  ;; acc := 0; while x /= 0: acc := (acc * 31 + 7) mod 1000003, x := x - 1
  (let ((code (vector :push 0 :swap               ; 0: (x acc)
                      :dup :jnz 10                ; 3: x /= 0: to 10
                      :push 1 :jnz 28             ; 6: else to the end
                      :swap :push 31 :mul         ; 10: (acc*31 x)
                      :push 7 :add :push 1000003 :mod
                      :swap :push 1 :sub          ; 20: (x-1 acc')
                      :push 1 :jnz 3              ; 24
                      :swap))                     ; 28: (acc 0)
        (r 0))
    (dotimes (i 20) (incf r (run-vm code (+ 5000 i))))
    r))

;;; ------------------------------------------------- hash tables, structures

(defstruct counter (n 0) (total 0) (max 0))

(defbench a-hash-counts (20)
  (let ((h (make-hash-table)) (c (make-counter)))
    (dotimes (i 100000)
      (let ((k (mod (* i 31) 1009)))
        (incf (gethash k h 0) i)
        (incf (counter-n c))
        (incf (counter-total c) (gethash k h))
        (when (> (gethash k h) (counter-max c)) (setf (counter-max c) (gethash k h)))))
    (list (counter-n c) (counter-total c) (counter-max c))))

(defun dist (p q)
  (sqrt (+ (expt (- (car p) (car q)) 2) (expt (- (cdr p) (cdr q)) 2))))

(defbench a-points (10)
  ;; points as conses of doubles; a small function returning a double
  (let ((pts (loop for i below 300 collect (cons (* 1.5d0 (mod (* i 37) 101)) (* 0.5d0 (mod (* i 53) 97)))))
        (s 0))
    (dolist (p pts) (dolist (q pts) (incf s (dist p q))))
    (round s)))

(defun lcg (x) (mod (+ (* x 1103515245) 12345) 2147483648))
(defbench a-lcg (10)
  ;; a function returning integers above the fixnum range
  (let ((x 1) (s 0))
    (dotimes (i 300000) (setq x (lcg x)) (setq s (logxor s x)))
    s))
