;;; APPLY of a global function of required parameters and &REST: in the image
;;; the call checks the rest layout of the function it finds, which may have
;;; been redefined with another lambda list.
(defun r1 (a &rest xs) (list :r1 a xs))
(defun k1 (a &key b) (list :k1 a b))
(defun s2 (a b) (list :s2 a b))
(print (list (apply #'r1 1 2 '(3)) (apply 'r1 1 nil) (apply #'r1 1 2 3 nil) (apply #'r1 '(5 6))))
(defun use-r1 (x) (apply #'r1 x x (list x)))
(print (use-r1 7))
(defun r1 (a b &rest xs) (list :new-r1 a b xs))
(print (list (use-r1 8) (apply #'r1 1 2 '(3))))
(defun r1 (&key a) (list :key-r1 a))
(print (handler-case (use-r1 9) (error () :err)))
(print (list (apply #'k1 1 '(:b 2)) (apply #'s2 1 '(2))))
