(in-package :geom)
(push :registry *load-order*)

(defvar *shapes* (make-hash-table :test 'equal))
(defmacro defshape (name class &rest initargs)
  `(setf (gethash ,(string-downcase name) *shapes*)
         (make-instance ',class :name ,(string-downcase name) ,@initargs)))
(defshape disc circle :r 2)
(defshape slab rect :w 3 :h 4)

(defun shape (name) (or (gethash name *shapes*) (error "no shape ~S" name)))

;;; literals like those that tests/static/util.lisp changes in place
(defun journal-like () (list '(nil) '(:a nil) "scratch buffer"))
