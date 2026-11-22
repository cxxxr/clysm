(in-package :geom-util)
(push :util *load-order*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setq *readtable* (copy-readtable))
  (set-macro-character #\! #'read-squared))

(declaim (inline square))
(defun square (x) !x)

(define-unit meter 1)
(define-unit kilometer 1000)

(defstruct point (x 0) (y 0))
(defun norm2 (p) (+ (square (point-x p)) (square (point-y p))))

;;; objects that only a fasl can hold as constants: a hash table read with
;;; #., a vector of octets, a pathname
(defun unit-table ()
  #.(let ((h (make-hash-table :test 'equal)))
      (setf (gethash "m" h) 1 (gethash "km" h) 1000)
      h))
(defun octets () #.(make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 250)))
(defun home () #p"/tmp/geom/")

(defgeneric describe-thing (thing)
  (:method ((thing t)) (format nil "a thing: ~S" thing))
  (:method ((thing point)) (format nil "a point at ~D,~D" (point-x thing) (point-y thing))))

;;; a structure whose instances can be constants of a file (MAKE-LOAD-FORM)
(defstruct (tag (:constructor make-tag (name))) name)
(defmethod make-load-form ((tag tag) &optional environment)
  (declare (ignore environment))
  `(make-tag ',(tag-name tag)))

;;; a function the application redefines when it runs
(declaim (notinline tolerance))
(defun tolerance () 1/1000)
(defun within-tolerance-p (x) (< (abs x) (tolerance)))
(export 'within-tolerance-p)
