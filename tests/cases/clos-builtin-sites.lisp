;;; Call sites of generic functions with EQL specializers (cached by the
;;; classes of the arguments, the entry testing the EQL objects), the vector of
;;; built-in classes of a generic function of one argument (and its emptying
;;; when methods change), readers called through FUNCALL with many classes,
;;; and a compiler macro defined in the same toplevel PROGN as its callers.

;; one argument, EQL objects among keywords, fixnums, characters, NIL
(defgeneric e1 (x))
(defmethod e1 ((x (eql :a))) :a)
(defmethod e1 ((x (eql 3))) :three)
(defmethod e1 ((x (eql #\z))) :z)
(defmethod e1 ((x (eql nil))) :nil)
(defmethod e1 ((x symbol)) (list :symbol x))
(defmethod e1 ((x integer)) (list :integer x))
(defmethod e1 ((x t)) :other)
(defun call-e1 (l) (mapcar (lambda (x) (e1 x)) l))
(print (call-e1 '(:b :a 3 4 #\z #\y nil t :a 3 "s" 2.5 :c 4 nil)))
(print (call-e1 '(3 4 :a :b nil t #\z)))

;; two arguments, the second specialized by EQL objects
(defgeneric e2 (x y))
(defmethod e2 ((x symbol) (y (eql 0))) (list :zero x))
(defmethod e2 ((x symbol) (y integer)) (list :int x y))
(defmethod e2 ((x string) y) (list :string x y))
(defmethod e2 (x y) (list :other x y))
(print (loop for (x y) in '((a 0) (a 1) (:k 0) ("s" 0) (a 0) (3 0) (b 2) (nil 0) (nil 5))
             collect (e2 x y)))

;; built-in classes of all kinds, then a method added and one removed
(defgeneric b1 (x))
(defmethod b1 ((x integer)) :integer)
(defmethod b1 ((x string)) :string)
(defmethod b1 ((x symbol)) :symbol)
(defmethod b1 ((x cons)) :cons)
(defmethod b1 ((x float)) :float)
(defmethod b1 ((x character)) :character)
(defmethod b1 ((x vector)) :vector)
(defmethod b1 ((x t)) :t)
(defparameter *things*
  (list 1 "a" 'b :k nil '(c) 1.5 2.5d0 #\d (vector 1) (make-array 3 :fill-pointer 1 :element-type 'character)
        (expt 10 30) 1/2 #'car (make-hash-table) (make-array '(2 2))))
(defun all-b1 () (loop for x in *things* collect (b1 x)))
(print (all-b1))
(print (all-b1))
(defmethod b1 ((x null)) :null)
(print (all-b1))
(remove-method #'b1 (find-method #'b1 nil (list (find-class 'integer))))
(print (all-b1))
(defmethod b1 ((x (eql #\d))) :d)
(print (all-b1))

;; a reader of many classes called through FUNCALL and MAPCAR
(defclass r1 () ((n :initform 1 :reader rn)))
(defclass r2 () ((n :initform 2 :reader rn)))
(defclass r3 (r1) ((m :initform 3)))
(defclass r4 () ((n :initform 4 :reader rn)))
(defclass r5 () ((n :initarg :n :reader rn)))
(defparameter *objs* (list (make-instance 'r1) (make-instance 'r2) (make-instance 'r3) (make-instance 'r4)
                           (make-instance 'r5 :n 5) (make-instance 'r1)))
(print (loop repeat 3 collect (mapcar #'rn *objs*)))
(print (handler-case (rn (make-instance 'r5)) (error () :unbound)))

;; DEFCLASS and its callers in one toplevel PROGN
(progn
  (defclass p1 () ((v :initform 10 :accessor pv)))
  (defun use-p1 (o) (incf (pv o)) (pv o)))
(print (let ((o (make-instance 'p1))) (list (use-p1 o) (use-p1 o))))
