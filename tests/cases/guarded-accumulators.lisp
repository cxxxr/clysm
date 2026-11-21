;;; A variable assigned the value of a function of another form called on
;;; it, (setq x (f x)): in the image, F's guarded copy is integer arithmetic
;;; kept in the variable's i64; beyond 64 bits, after F is redefined, or for
;;; values that are not integers the generic code.
(defun lcg (x) (mod (+ (* x 1103515245) 12345) 2147483648))
(defun run-lcg (n) (let ((x 1) (s 0)) (dotimes (i n) (setq x (lcg x)) (setq s (logxor s x))) (list x s)))
(print (run-lcg 100000))
(defun step3 (x) (+ (* x 3) 1))
(defun grow (n) (let ((x 1)) (dotimes (i n) (setq x (step3 x))) x))
(print (list (grow 10) (grow 50) (logand (grow 200) #xFFFF)))
(defun half (x) (+ (* x 1/2) 1))
(defun halves (n) (let ((x 7)) (dotimes (i n) (setq x (half x))) x))
(print (halves 5))
(defun inc2 (x) (+ x 2))
(defun mixed (start n) (let ((x start)) (dotimes (i n) (setq x (inc2 x))) x))
(print (list (mixed 1 10) (mixed 1.5d0 10) (mixed (expt 2 62) 10) (mixed 1/3 3)))
(defun lcg (x) (logand (* x 2) #xFFFFFFFFFFFF))
(print (run-lcg 100))
(defun step3 (x) (list x))
(print (grow 2))
