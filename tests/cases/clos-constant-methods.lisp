;;; Methods whose body is a constant (the dispatch takes the value without
;;; calling them): with other methods, redefined, with qualified methods
;;; added and removed, with call-next-method from a more specific method,
;;; over built-in classes (bignums and ratios too) and instances.

(defgeneric kind (x))
(defmethod kind ((x integer)) :integer)
(defmethod kind ((x string)) "string")
(defmethod kind ((x symbol)) 'sym)
(defmethod kind ((x cons)) '(a cons))
(defmethod kind ((x float)) 1.5)
(defmethod kind ((x ratio)) #\r)
(defmethod kind ((x t)) nil)
(defun kinds () (mapcar (lambda (x) (kind x)) (list 1 (expt 10 30) 1/3 "a" 'b '(c) 2.5 #\d nil (vector 1))))
(print (kinds))
(print (kinds))

;; redefined with a body that is not a constant
(defmethod kind ((x integer)) (list :integer x))
(print (kinds))
;; back to a constant
(defmethod kind ((x integer)) 7)
(print (kinds))

;; a :before method makes the effective method more than the constant
(defvar *log* nil)
(defmethod kind :before ((x string)) (push x *log*))
(print (kinds))
(print *log*)
(remove-method #'kind (find-method #'kind '(:before) (list (find-class 'string))))
(print (kinds))
(print *log*)

;; a more specific method calling the constant one
(defmethod kind ((x null)) (list :null (call-next-method)))
(print (kinds))

;; instances, two arguments
(defclass node () ())
(defclass leaf (node) ())
(defclass branch (node) ())
(defgeneric arity (x y))
(defmethod arity ((x leaf) y) 0)
(defmethod arity ((x branch) (y integer)) 2)
(defmethod arity ((x branch) y) (list :branch y))
(defmethod arity ((x node) (y (eql :k))) :eql)
(let ((objs (list (make-instance 'leaf) (make-instance 'branch) (make-instance 'node))))
  (dotimes (i 2)
    (print (loop for o in objs
                 append (loop for y in '(1 a :k)
                              collect (if (typep o 'branch) (arity o y)
                                          (ignore-errors (arity o y))))))))
;; through FUNCALL and APPLY
(print (funcall #'arity (make-instance 'leaf) 3))
(print (apply #'kind '(5)))

;; constant default initargs (kept in the constructor's template)
(defvar *counter* 0)
(defclass p1 () ((a :initarg :a :initform 0) (b :initarg :b) (c :initarg :c :initform (incf *counter*)))
  (:default-initargs :b 'bee))
(defclass p2 (p1) () (:default-initargs :a 10 :c (list :c (incf *counter*))))
(defun p-slots (o) (list (slot-value o 'a) (slot-value o 'b) (slot-value o 'c)))
(dotimes (i 2)
  (print (list (p-slots (make-instance 'p1)) (p-slots (make-instance 'p1 :b 2))
               (p-slots (make-instance 'p2)) (p-slots (make-instance 'p2 :a 3 :c 4))
               (p-slots (make-instance (if (evenp i) 'p2 'p1) :b i)))))
(defclass p2 (p1) () (:default-initargs :a 20 :b "b"))
(print (list (p-slots (make-instance 'p2)) (p-slots (make-instance 'p2 :a 1))))
