(in-package :geom-util)

;;; a function that macros call when they are expanded, a constant, and a
;;; variable set when the file is compiled and when it is loaded
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun unit-symbol (name) (intern (format nil "+~A+" (string-upcase name)) :geom-util))
  (defparameter *known-units* '()))

(defconstant +scale+ 100)
(defvar *units* '())
(defvar *load-order* '())

(defmacro define-unit (name factor)
  (pushnew name *known-units*)
  `(progn (defconstant ,(unit-symbol name) ,(* factor +scale+))
          (push (cons ',name ,(unit-symbol name)) *units*)
          ',name))

(defmacro with-collector ((collect) &body body)
  (let ((list (gensym "LIST")))
    `(let ((,list '()))
       (flet ((,collect (x) (push x ,list) x))
         (declare (ignorable #',collect))
         ,@body)
       (nreverse ,list))))

(defmacro twice (form) `(progn ,form ,form))

;;; (a reader macro that util.lisp puts in its readtable)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun read-squared (stream char)
    (declare (ignore char))
    (let ((x (read stream t nil t))) `(* ,x ,x))))
