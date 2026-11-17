;;; MAKE-INSTANCE with constant initarg keys and a class known at run time:
;;; the constructors a call site keeps for its classes, and the cases where
;;; the generic function must be called instead.

(defclass a1 () ((p :initarg :p :initform 1 :reader p) (q :initarg :q :initform (list 1) :reader q)))
(defclass a2 (a1) ((s :initarg :s :reader s)) (:default-initargs :s 4))
(defun make (c &rest r) (declare (ignore r)) (make-instance c :p 10))
(defun make2 (c x) (make-instance c :p x :q (* x 2)))
(defun show (o) (list (class-name (class-of o)) (p o) (q o) (and (slot-exists-p o 's) (slot-boundp o 's) (s o))))

;; names and classes, the same site
(print (mapcar #'show (list (make 'a1) (make (find-class 'a1)) (make 'a2) (make (find-class 'a2)) (make 'a1))))
(print (mapcar #'show (list (make2 'a2 3) (make2 (find-class 'a1) 4) (make2 (find-class 'a2) 5))))

;; default initargs evaluated in order, once per instance
(defvar *log* nil)
(defclass a3 () ((u :initarg :u :reader u) (v :initarg :v :reader v))
  (:default-initargs :v (progn (push :v *log*) 2) :u (progn (push :u *log*) 1)))
(defun make3 (c) (make-instance c))
(print (list (u (make3 'a3)) (v (make3 (find-class 'a3))) (reverse *log*)))

;; an :AFTER method of INITIALIZE-INSTANCE, then a method of MAKE-INSTANCE
(defmethod initialize-instance :after ((o a1) &key) (setf (slot-value o 'q) :after))
(print (mapcar #'show (list (make 'a1) (make (find-class 'a2)))))
(defmethod make-instance ((c (eql (find-class 'a2))) &rest initargs)
  (declare (ignore initargs))
  :made-by-a-method)
(print (list (make (find-class 'a2)) (show (make 'a1))))

;; the name given to another class: an instance of the class object passed
(defclass b1 () ((p :initarg :p :reader p)))
(defclass b2 () ((p :initarg :p :reader p)))
(defvar *old-b1* (find-class 'b1))
(print (list (class-name (class-of (make *old-b1*))) (p (make *old-b1*))))
(setf (find-class 'b1) (find-class 'b2))
(print (list (eq (class-of (make *old-b1*)) *old-b1*) (eq (class-of (make 'b1)) (find-class 'b2))))

;; more classes than the site keeps
(defmacro define-many (n)
  `(progn ,@(loop for i below n collect `(defclass ,(intern (format nil "M~D" i)) () ((p :initarg :p :reader p))))))
(define-many 12)
(print (loop for i below 12 for c = (find-class (intern (format nil "M~D" i)))
             collect (list (eq (class-of (make c)) c) (p (make c)))))

;; a class redefined with another slot
(defclass r1 () ((p :initarg :p :reader p)))
(print (p (make 'r1)))
(defclass r1 () ((p :initarg :p :reader p) (z :initform :z :reader z)))
(let ((o (make (find-class 'r1)))) (print (list (p o) (z o))))

;; errors: an invalid initarg, a class that is not one
(defclass c1 () ((p :initarg :p)))
(defun make-bad (c) (make-instance c :nope 1))
(print (list (handler-case (make-bad 'c1) (error () :error))
             (handler-case (make-bad (find-class 'c1)) (error () :error))
             (handler-case (make 42) (error () :error))
             (handler-case (make 'no-such-class-at-all) (error () :error))))
(terpri)
