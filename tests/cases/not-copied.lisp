;;; A function proclaimed SYS::NOT-COPIED is called directly, and gives
;;; what it gives when copied; INLINE takes the proclamation back.
(declaim (sys::not-copied twice))
(defun twice (x) (* 2 x))
(defun use-twice (x) (+ (twice x) 1))
(print (use-twice 20))
(print (mapcar #'twice '(1 2 3)))
(declaim (inline twice))
(print (use-twice 1))
