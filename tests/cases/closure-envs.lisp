;;; Closures with every number of free variables around the number held in
;;; fields of the closure itself (more go into a vector): reading them,
;;; assigning captured variables, LABELS functions referring to each other
;;; (their environments are completed after they are made), and generic
;;; functions (their closures are copied with other entries).

(defmacro make-n (n)
  (let ((vars (loop for i below n collect (intern (format nil "V~D" i)))))
    `(lambda ,vars
       (list (lambda () (list ,@vars))
             (lambda (k) (setq ,(car vars) (+ ,(car vars) k)) (list ,@vars))))))

(defmacro test-n (n)
  `(let ((fs (apply (make-n ,n) (loop for i below ,n collect (* i 10)))))
     (print (list ,n (funcall (first fs)) (funcall (second fs) 3) (funcall (first fs))))))

(test-n 1) (test-n 2) (test-n 7) (test-n 8) (test-n 9) (test-n 12)

(defun mutual (a b c d e f g h i j)
  (labels ((ev (n) (if (= n 0) (list :ev a b c d e f g h i j) (od (1- n))))
           (od (n) (if (= n 0) (list :od a b) (ev (1- n)))))
    (list (ev 3) (od 3) (mapcar #'ev '(0 1)))))
(print (mutual 1 2 3 4 5 6 7 8 9 10))

(defun mutual2 (a b)
  (labels ((ev (n) (if (= n 0) (list :ev a) (od (1- n))))
           (od (n) (if (= n 0) (list :od b) (ev (1- n)))))
    (funcall (if (> a b) #'ev #'od) 5)))
(print (list (mutual2 1 2) (mutual2 2 1)))

;; a closure over many variables that are assigned
(defun counters ()
  (let ((a 0) (b 0) (c 0) (d 0) (e 0) (f 0) (g 0) (h 0) (i 0) (j 0))
    (lambda (k)
      (case k (0 (incf a)) (1 (incf b)) (2 (incf c)) (3 (incf d)) (4 (incf e))
            (5 (incf f)) (6 (incf g)) (7 (incf h)) (8 (incf i)) (t (incf j)))
      (list a b c d e f g h i j))))
(let ((c (counters))) (dotimes (k 12) (funcall c k)) (print (funcall c 9)))

;; closures as methods and as generic functions
(defgeneric g2 (x y))
(let ((p 1) (q 2) (r 3))
  (defmethod g2 ((x integer) y) (list x y p q r)))
(defmethod g2 ((x symbol) y) (list :sym x y))
(print (list (g2 1 2) (g2 'a 3) (funcall #'g2 4 5) (apply #'g2 '(b 6))))
(print (functionp (let ((x 1) (y 2)) (lambda () (+ x y)))))
