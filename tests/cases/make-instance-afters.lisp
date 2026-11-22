;;; MAKE-INSTANCE of classes with :AFTER methods on INITIALIZE-INSTANCE and
;;; SHARED-INITIALIZE (the constructors of the call sites fill the slots and
;;; call the methods' functions), and what must leave that path.

(defvar *log* nil)
(defun note (&rest xs) (push xs *log*))
(defun flush-log () (prog1 (reverse *log*) (setq *log* nil)))

(defclass account ()
  ((id :initarg :id :reader account-id)
   (balance :initarg :balance :initform 0 :accessor balance)
   (history :initform nil :accessor history)))
(defmethod initialize-instance :after ((a account) &key (bonus 0))
  (incf (balance a) bonus))

(defun make-account (i) (make-instance 'account :id i :balance 10 :bonus 5))
(defun make-account-0 (i) (make-instance 'account :id i))
(let ((a (make-account 1)) (b (make-account-0 2)))
  (print (list (account-id a) (balance a) (history a) (account-id b) (balance b))))
(print (handler-case (make-instance 'account :id 1 :bogus 2) (error () :error)))
(print (balance (make-instance 'account :id 1 :bonus 2 :allow-other-keys t :zzz 1)))
;; a repeated key: the first wins, in the slot and in the method
(print (let ((a (make-instance 'account :id 1 :id 2 :bonus 3 :bonus 4))) (list (account-id a) (balance a))))

;; default initargs: evaluated once, in order, and seen by the methods
(defvar *n* 0)
(defclass node ()
  ((name :initarg :name :reader node-name)
   (kids :initarg :kids :initform (progn (note :kids-initform) (list 1)) :accessor node-kids)
   (count :accessor node-count)
   (serial :initarg :serial :reader node-serial))
  (:default-initargs :name (progn (note :name-default) "none") :serial (incf *n*) :unused (note :unused-default)))
(defmethod initialize-instance :after ((n node) &rest initargs &key unused &allow-other-keys)
  (declare (ignore unused))
  (note :node-after (length initargs) (getf initargs :name) (getf initargs :serial))
  (setf (node-count n) (length (node-kids n))))
(defun make-node-1 () (make-instance 'node :kids nil))
(defun make-node-2 () (make-instance 'node :name "x"))
(defun make-node-3 (s) (make-instance 'node :serial s :name "y" :kids '(a b)))
(dotimes (i 2)
  (let ((a (make-node-1)) (b (make-node-2)) (c (make-node-3 100)))
    (print (list (node-count a) (node-name a) (node-serial a)
                 (node-count b) (node-name b) (node-serial b)
                 (node-count c) (node-name c) (node-serial c)))
    (print (flush-log))))

;; subclasses, SHARED-INITIALIZE :AFTER, the order of the methods
(defclass sub (node) ((extra :initarg :extra :initform 0 :accessor sub-extra)))
(defmethod shared-initialize :after ((n sub) slot-names &key extra)
  (note :sub-shared slot-names extra (slot-boundp n 'count))
  (when extra (incf (sub-extra n))))
(defmethod initialize-instance :after ((n sub) &key)
  (note :sub-after (node-count n)))
(defun make-sub (i) (make-instance 'sub :extra i))
(dotimes (i 2)
  (let ((a (make-sub 7)))
    (print (list (node-count a) (sub-extra a) (node-name a)))
    (print (flush-log))))
(let ((a (make-sub 1)))
  (reinitialize-instance a :extra 5)
  (print (list (sub-extra a) (flush-log))))

;; a class given at run time
(defun make-of (class) (make-instance class :id 3 :bonus 1))
(print (list (balance (make-of 'account)) (balance (make-of (find-class 'account)))))

;; &rest alone, and a slot left unbound by the constructor
(defclass pt () ((x :initarg :x :accessor pt-x) (y :initarg :y :accessor pt-y)))
(defmethod initialize-instance :after ((p pt) &rest initargs)
  (note :pt initargs)
  (unless (slot-boundp p 'y) (setf (pt-y p) 0)))
(defun make-pt (x) (make-instance 'pt :x x))
(defun make-pt-0 () (make-instance 'pt))
(print (list (pt-y (make-pt 1)) (pt-y (make-pt-0)) (slot-boundp (make-pt-0) 'x) (flush-log)))

;; the methods change: a :BEFORE method, an :AROUND method, a redefinition, removal
(defmethod initialize-instance :before ((p pt) &key) (note :pt-before (slot-boundp p 'x)))
(print (list (pt-y (make-pt 1)) (flush-log)))
(defmethod initialize-instance :around ((p pt) &key (x 0)) (note :pt-around x) (call-next-method p :x (* 2 x)))
(print (list (pt-x (make-pt 4)) (flush-log)))
(remove-method #'initialize-instance (find-method #'initialize-instance '(:around) (list (find-class 'pt))))
(remove-method #'initialize-instance (find-method #'initialize-instance '(:before) (list (find-class 'pt))))
(defmethod initialize-instance :after ((p pt) &rest initargs)
  (note :pt-again (length initargs))
  (setf (pt-y p) 9))
(print (list (pt-y (make-pt 1)) (pt-x (make-pt 2)) (flush-log)))
(remove-method #'initialize-instance (find-method #'initialize-instance '(:after) (list (find-class 'pt))))
(print (list (slot-boundp (make-pt 1) 'y) (flush-log)))

;; a primary method of the class: not the constructor's path
(defclass own () ((a :initarg :a :accessor own-a)))
(defmethod initialize-instance ((o own) &key a) (note :own a) (call-next-method) (setf (own-a o) (list (own-a o))))
(defmethod initialize-instance :after ((o own) &key) (note :own-after (own-a o)))
(defun make-own (a) (make-instance 'own :a a))
(print (list (own-a (make-own 1)) (own-a (make-own 2)) (flush-log)))

;; a method that is evaluated, a class redefined
(eval '(defclass ev () ((a :initarg :a :initform 1 :accessor ev-a))))
(eval '(defmethod initialize-instance :after ((e ev) &key (scale 2)) (setf (ev-a e) (* scale (ev-a e)))))
(defun make-ev () (list (ev-a (make-instance 'ev)) (ev-a (make-instance 'ev :a 5 :scale 3))))
(print (make-ev))
(eval '(defclass ev () ((b :initform 7 :accessor ev-b) (a :initarg :a :initform 10 :accessor ev-a))))
(print (make-ev))
(print (ev-b (make-instance 'ev :a 5)))

;; an :AFTER method of a superclass defined after the constructor was used
(defclass late () ((v :initarg :v :accessor late-v)))
(defclass later (late) ())
(defun make-later (v) (make-instance 'later :v v))
(print (late-v (make-later 1)))
(defmethod initialize-instance :after ((l late) &key) (setf (late-v l) (- (late-v l))))
(print (late-v (make-later 1)))
(defmethod shared-initialize :after ((l later) names &key (twice nil)) (declare (ignore names)) (when twice (setf (late-v l) (* 2 (late-v l)))))
(print (list (late-v (make-later 1)) (late-v (make-instance 'later :v 3 :twice t))))
