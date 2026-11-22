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
;; a function of several statements whose value is an integer within 64 bits
;; (its copy's value goes to the variable's i64 as it is), on a start that
;; is a fixnum, beyond a fixnum, beyond 64 bits, not an integer, and after
;; the function is redefined to return something else
(defun xs (x)
  (setq x (logand (logxor x (ash x 13)) #xFFFFFFFF))
  (setq x (logxor x (ash x -17)))
  (logand (logxor x (ash x 5)) #xFFFFFFFF))
(defun run-xs (start n) (let ((x start) (s 0)) (dotimes (i n) (setq x (xs x)) (when (oddp x) (incf s))) (list x s)))
(print (list (run-xs 2463534242 1000) (run-xs 1 1000) (run-xs (expt 2 70) 10) (run-xs -5 10)))
(defun clamp8 (x) (if (integerp x) (logand x 255) 0))
(defun run-clamp (start n) (let ((x start)) (dotimes (i n) (setq x (clamp8 x)) (setq x (+ x 300))) x))
(print (list (run-clamp 1000 5) (run-clamp 1.5d0 5) (run-clamp 'a 3) (run-clamp (expt 3 50) 2)))
(defun use-after (n) (let ((x 4000000000) (l nil)) (dotimes (i n) (setq x (xs x)) (push x l)) (list x (length l) (car l))))
(print (use-after 5))
(defun xs (x) (if (> x 100) (list x) (* x 1000)))
(print (list (run-clamp 7 2) (ignore-errors (run-xs 3 2)) (let ((x 5)) (dotimes (i 1) (setq x (xs x))) x)))
