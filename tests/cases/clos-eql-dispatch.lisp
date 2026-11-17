;;; Dispatch on EQL specializers: the entries of the objects and of their
;;; classes in the cache, in every order of first calls, after a change of the
;;; methods, and CALL-NEXT-METHOD's check of its arguments.

;; an EQL specializer object called first, then another object of its class
(defgeneric f1 (x))
(defmethod f1 ((x (eql 7))) :seven)
(defmethod f1 ((x integer)) (list :integer x))
(defmethod f1 ((x t)) :other)
(print (list (f1 7) (f1 7) (f1 8) (f1 7) (f1 8) (f1 'a) (f1 7)))

;; another object of the class first
(defgeneric f2 (x))
(defmethod f2 ((x (eql 7))) :seven)
(defmethod f2 ((x integer)) (list :integer x))
(print (list (f2 8) (f2 7) (f2 8) (f2 7)))

;; keywords, other symbols, NIL; several objects of one class
(defgeneric f3 (x y))
(defmethod f3 ((x (eql :a)) y) (list :a y))
(defmethod f3 ((x (eql :b)) y) (list :b y))
(defmethod f3 ((x (eql 'c)) y) (list 'c y))
(defmethod f3 ((x (eql nil)) y) (list :nil y))
(defmethod f3 ((x symbol) y) (list :symbol x y))
(print (loop for x in '(:a :b c d :e nil :a c nil t :b) collect (f3 x 1)))

;; objects that are EQL but not EQ: characters, floats, bignums
(defgeneric f4 (x))
(defmethod f4 ((x (eql #\a))) :char-a)
(defmethod f4 ((x (eql 1.5d0))) :one-and-a-half)
(defmethod f4 ((x (eql 100000000000000000000))) :big)
(defmethod f4 ((x t)) :other)
(print (list (f4 #\a) (f4 #\b) (f4 (* 0.5d0 3)) (f4 2d0) (f4 (expt 10 20)) (f4 (expt 10 21)) (f4 #\a)))

;; the second parameter specialized
(defgeneric f5 (x y))
(defmethod f5 (x (y (eql 0))) (list x :zero))
(defmethod f5 (x (y integer)) (list x :integer))
(print (list (f5 1 0) (f5 1 2) (f5 'a 0) (f5 'a 3) (f5 1 0)))

;; a method added after the calls; then removed
(defmethod f1 ((x (eql 8))) :eight)
(print (list (f1 7) (f1 8) (f1 9)))
(remove-method #'f1 (find-method #'f1 '() '((eql 8))))
(print (list (f1 7) (f1 8) (f1 9)))

;; CALL-NEXT-METHOD with arguments, checked at safety 3: the applicable
;; methods of an object of the class that is not an EQL specializer object
(defgeneric f6 (x))
(defmethod f6 ((x integer)) (list :integer x))
(defmethod f6 ((x (eql 1)))
  (declare (optimize (safety 3)))
  (handler-case (call-next-method 2) (error () :refused)))
(defmethod f6 ((x (eql 3)))
  (declare (optimize (safety 3)))
  (call-next-method 3))
;; (1 first: the entry of the class of 2 is made with its methods unknown)
(print (list (f6 1) (f6 2) (f6 3) (f6 4) (f6 1)))
(terpri)

;; EQL specializer objects in both parameters: entries guarded twice
(defgeneric f7 (x y))
(defmethod f7 ((x (eql :a)) (y (eql 0))) :a-0)
(defmethod f7 ((x (eql :a)) y) (list :a y))
(defmethod f7 ((x (eql :b)) (y (eql 1))) :b-1)
(defmethod f7 (x (y (eql 1))) (list x :one))
(defmethod f7 ((x symbol) (y integer)) (list :symbol x y))
(print (loop for (x y) in '((:c 5) (:a 0) (:a 1) (:b 1) (:b 0) (:c 1) (:a 0) (:a 7) (:b 1) (d 0) (:a 1))
             collect (f7 x y)))
(terpri)
