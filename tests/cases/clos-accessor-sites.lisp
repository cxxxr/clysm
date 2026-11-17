;;; Call sites of slot readers and writers (SETF of an accessor, :WRITER
;;; functions) keep the slot locations of the classes they see: four classes
;;; per site, the slots at other locations in subclasses, more classes than a
;;; site keeps, methods added to the accessors, redefined classes, and the
;;; cases where the generic function is called instead.

(defclass base () ((a :initarg :a :initform 1 :accessor a-of) (b :initform 2 :accessor b-of)))
(defclass mid (base) ((c :initform 3 :accessor c-of)))
(defclass top (mid) ((d :initform 4 :accessor d-of) (e :initform 5 :reader e-of :writer set-e)))
;; the slot A at another location
(defclass other () ((z :initform 0) (a :initarg :a :initform 7 :accessor a-of)))

(defun bump (o) (setf (a-of o) (+ (a-of o) (b-of-or-0 o))))
(defun b-of-or-0 (o) (if (typep o 'base) (b-of o) 0))
(defun put-e (o v) (set-e v o))

(let ((objs (list (make-instance 'top) (make-instance 'mid) (make-instance 'base) (make-instance 'other))))
  ;; the value of SETF and of a writer is the new value
  (print (list (setf (a-of (first objs)) 10) (put-e (first objs) 11) (e-of (first objs))))
  (dotimes (i 5) (dolist (o objs) (bump o)))
  (print (mapcar #'a-of objs)))

;; more classes than a site keeps
(defmacro define-many (n)
  `(progn ,@(loop for i below n collect `(defclass ,(intern (format nil "K~D" i)) ()
                                           (,@(loop repeat i collect (gensym)) (a :initform ,i :accessor a-of))))))
(define-many 9)
(let ((objs (loop for i below 9 collect (make-instance (intern (format nil "K~D" i))))))
  (dotimes (j 3) (dolist (o objs) (bump o)))
  (print (mapcar #'a-of objs)))

;; an unbound slot through a reader site; a writer making it bound
(defclass u () ((s :accessor s-of)))
(defun read-s (o) (handler-case (s-of o) (unbound-slot () :unbound)))
(let ((o (make-instance 'u)))
  (print (list (read-s o) (read-s o) (setf (s-of o) :bound) (read-s o))))

;; a method added to the writer and the reader after the sites cached them
(defvar *log* nil)
(defmethod (setf a-of) :after (v (o mid)) (push (list :after v) *log*))
(defmethod a-of :around ((o top)) (list :around (call-next-method)))
(let ((objs (list (make-instance 'top) (make-instance 'mid) (make-instance 'base))))
  (dolist (o objs) (setf (a-of o) 5))
  (print (list (mapcar #'a-of objs) (reverse *log*))))

;; a class redefined with its slots in other places (the sites cached the
;; old locations; instances made before the redefinition are not updated
;; unless instances are tracked, see README)
(defclass r () ((p :initform :p :accessor p-of)))
(defun set-p (o v) (setf (p-of o) v))
(set-p (make-instance 'r) 1)
(print (p-of (make-instance 'r)))
(defclass r () ((q :initform :q :accessor q-of) (p :initform :p :accessor p-of)))
(let ((o (make-instance 'r)))
  (set-p o 2)
  (print (list (p-of o) (q-of o))))

;; a slot with :ALLOCATION :CLASS
(defclass shared () ((n :initform 0 :allocation :class :accessor n-of)))
(let ((x (make-instance 'shared)) (y (make-instance 'shared)))
  (setf (n-of x) 42)
  (print (list (n-of y) (setf (n-of y) 43) (n-of x))))

;; FUNCALL and APPLY of the writer; not an instance; NOTINLINE
(let ((o (make-instance 'base)))
  (print (list (funcall #'(setf a-of) 8 o) (apply #'(setf a-of) (list 9 o)) (a-of o)
               (handler-case (setf (a-of 42) 1) (error () :no-method))
               (locally (declare (notinline (setf a-of))) (setf (a-of o) 12))
               (a-of o))))

;; a generic function (SETF NAME) of the program's own, and a local function
(defgeneric (setf thing) (new-value place))
(defmethod (setf thing) (v (place cons)) (setf (car place) v))
(defmethod (setf thing) (v (place vector)) (setf (aref place 0) v))
(defun store-thing (p v) (setf (thing p) v))
(let ((c (list 0)) (v (vector 0)))
  (print (list (store-thing c 1) (store-thing v 2) c v
               (flet (((setf thing) (v p) (declare (ignore p)) (list :local v)))
                 (setf (thing c) 3))
               c)))
(terpri)
