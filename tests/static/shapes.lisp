(in-package :geom)
(push :shapes *load-order*)

(defclass shape () ((name :initarg :name :reader shape-name)))
(defclass circle (shape) ((r :initarg :r :reader circle-r)))
(defclass rect (shape) ((w :initarg :w :reader rect-w) (h :initarg :h :reader rect-h)))

(defgeneric area (shape))
(defmethod area ((s circle)) (* 3 (square (circle-r s))))
(defmethod area ((s rect)) (* (rect-w s) (rect-h s)))
(defmethod area :around ((s shape)) (* geom-util::+meter+ (call-next-method)))

;;; a method of the library's generic function
(defmethod describe-thing ((thing shape))
  (format nil "~A of area ~D" (shape-name thing) (area thing)))

;;; constants that are instances: one object each, however often referred to
(defun origin-tag () '#.(geom-util::make-tag :origin))
(defun tags () '(#.(geom-util::make-tag :a) 1 "x" #.(geom-util::make-tag :b)))

;;; Plain literal lists that look like the cells of the objects above when
;;; they are compiled ((NIL), or a list whose last cons is one): the cells
;;; are changed when the program runs, these are not.
(defun plain-lists () (list '(nil) '(1 2 nil) '((nil))))
