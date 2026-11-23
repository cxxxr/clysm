;;; Bounded fixnum sums (i32) beside unsigned 64-bit trees and accumulator
;;; state: the two fast paths are chosen per tree and must agree with the
;;; generic arithmetic at their boundaries.

(defun mix (state a b)
  ;; a sum of unknowns under a mask of 64 bits, the state above 2^63
  (logand (logxor state (+ a b)) #xFFFFFFFFFFFFFFFF))

(defun step-sum (n a b)
  ;; an unsigned accumulator read by a bounded sum and by ordinary code
  (let ((s #x9E3779B97F4A7C15) (k 0) (acc nil))
    (dotimes (i n)
      (setf s (logand (logxor (* s 31) (ash s -7) i) #xFFFFFFFFFFFFFFFF))
      (setf k (+ k (- a b) 1))
      (when (< i 3) (push (+ s k) acc)))
    (list s k (+ a b k) acc)))

(defun sum3 (a b c) (+ a (- b c)))

(defun masked-sum (x a b)
  (+ (logand x #xFFFFFFFFFFFFFFFF) (+ a b)))

(print (mix #xFFFFFFFFFFFFFFF0 3 4))
(print (mix #x8000000000000000 1073741823 1073741823))
(print (mix 0 -1 0))
(print (mix (expt 2 70) (expt 2 40) -2))
(print (step-sum 10 7 3))
(print (step-sum 5 1073741823 -1073741824))
(print (step-sum 4 (expt 2 64) 1))
(print (sum3 1073741823 1073741823 -1073741824))
(print (sum3 -1073741824 -1073741824 1073741823))
(print (sum3 1 2.5d0 1))
(print (sum3 (expt 2 63) 1 -1))
(print (masked-sum -1 1 2))
(print (masked-sum (1- (expt 2 64)) 1073741823 1))
(print (masked-sum (expt 2 65) -5 1/2))

;;; an accumulator beside a logical operation that cannot reach 2^63 (no
;;; unsigned state in the function) and beside one that can
(defun small-mask (n big)
  (let ((s 0))
    (dotimes (i n) (setq s (logand s #xFFFFFF)) (incf s (+ i big)) (setq s (logxor s (logand i 7))))
    s))
(defun wide-xor (n x)
  (let ((s 1))
    (dotimes (i n) (setq s (logand s #xFFFF)) (setq s (logxor (* s 65537) x)) (incf s i))
    s))
(print (list (small-mask 10 5) (small-mask 3 (expt 2 63)) (small-mask 3 (1- (expt 2 64))) (small-mask 2 -7)))
(print (list (wide-xor 5 3) (wide-xor 5 (expt 2 63)) (wide-xor 4 (1- (expt 2 64))) (wide-xor 3 -1) (wide-xor 3 (expt 2 70))))
