;;; small functions of other forms copied into loops (guarded; in the image a
;;; loop that calls nothing else runs in a version where the guards are
;;; tested once): a redefinition before the loop is seen
(defun f (x) (* x 2))
(defun g (n) (let ((s 0)) (dotimes (i n s) (incf s (f i)))))
(defun h (n) (let ((s 0)) (dotimes (j 3 s) (dotimes (i n) (incf s (f i))))))
(print (list (g 10) (h 10)))
(defun f (x) (* x 3))
(print (list (g 10) (h 10)))
;; doubles on one of their paths
(defun sq (x) (* x x))
(defun clamp01 (x) (cond ((< x 0d0) 0d0) ((> x 1d0) 1d0) (t x)))
(defun lerp (a b u) (+ a (* u (- b a))))
(defun hl (n) (let ((acc 0d0))
                (dotimes (i n acc)
                  (let ((x (* i 0.25d0)))
                    (incf acc (clamp01 (lerp (sq x) (- x) 0.5d0)))))))
(print (hl 10))
(print (list (clamp01 -2) (clamp01 1/2) (clamp01 3) (sq 3) (lerp 1 2 1/2)))
(defun sq (x) (list x))
(print (handler-case (hl 3) (error () :error)))
(print (list (< 1 2d0) (> 2 1.5d0) (= 1 1d0) (< 1/2 0.75d0) (/ 7 2) (/ 8 2)))
