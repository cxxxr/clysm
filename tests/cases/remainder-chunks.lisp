;;; Chunked remainders must preserve signs and work across 32/48/96-bit
;;; boundaries, including the largest divisor eligible for 48-bit chunks.
(defun check-remainders ()
  (let ((h 0) (seed 7654321))
    (loop for n from 2 to 70 do
      (let ((a 0))
        (dotimes (i n)
          (setq seed (mod (+ (* seed 1664525) 1013904223) 4294967296))
          (setq a (+ (ash a 32) seed)))
        (dolist (x (list a (- a) (1- (ash 1 (* n 32))) (- 1 (ash 1 (* n 32)))))
          (dolist (d '(1 2 3 7 31 256 2000 32767 32768 65535 65536 65537 4294967295))
            (dolist (b (list d (- d)))
              (setq h (mod (+ (* h 31) (rem x b) (* 13 (mod x b)) (gcd x b)) 1000000007)))))))
    h))
(print (check-remainders))
(terpri)
