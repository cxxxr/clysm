;;; Generic functions of two and three specialized arguments with more
;;; entries than the linear cache holds (the table searched by the dispatch
;;; itself), methods added and redefined after the table was made, EQL
;;; specializers among them, and arguments that are not instances.

(defclass base () ())
(defmacro defs (&rest names)
  `(progn ,@(loop for n in names collect `(defclass ,n (base) ()))
          ,@(loop for n in names for i from 1
                  append (loop for m in names for j from 1
                               when (/= i j)
                                 collect `(defmethod collide ((x ,n) (y ,m)) (+ ,i (* 10 ,j)))))))
(defgeneric collide (x y))
(defmethod collide ((x base) (y base)) :same)
(defmethod collide ((x base) y) (list :other y))
(defmethod collide ((x base) (y (eql 7))) :seven)
(defs a1 a2 a3 a4 a5 a6 a7)
(defparameter *objs* (mapcar #'make-instance '(a1 a2 a3 a4 a5 a6 a7)))
(defun all () (loop for x in *objs* collect (loop for y in (append *objs* '(7 8 "s")) collect (collide x y))))
(print (all))
(print (all))
(defmethod collide ((x a3) (y a5)) (list :new (call-next-method)))
(defmethod collide ((x a1) (y a1)) :one-one)
(print (all))
(print (all))

(defgeneric tri (x y z))
(defmacro deftri (&rest names)
  `(progn ,@(loop for n in names for i from 1
                  append (loop for m in names for j from 1
                               collect `(defmethod tri ((x ,n) (y ,m) z) (list ,i ,j z))))))
(deftri a1 a2 a3 a4 a5 a6)
(defmethod tri ((x a2) (y a2) (z integer)) (list :int z (call-next-method)))
(defun all3 () (loop for x in *objs* when (ignore-errors (tri x x 1))
                     collect (loop for y in (subseq *objs* 0 6) collect (list (tri x y 'z) (tri x y 3)))))
(print (all3))
(print (all3))
