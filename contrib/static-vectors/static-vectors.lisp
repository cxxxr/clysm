;;;; static-vectors.lisp — static-vectors' interface for Clysm

(defpackage #:static-vectors
  (:use #:cl)
  (:export #:make-static-vector #:free-static-vector #:with-static-vector #:with-static-vectors
           #:static-vector-pointer #:static-vector-p #:fill-foreign-memory #:replace-foreign-memory
           #:+array-header-size+))

(in-package #:static-vectors)

(defconstant +array-header-size+ 0)

(defun make-static-vector (length &key (element-type '(unsigned-byte 8))
                                    (initial-element nil initial-element-p)
                                    (initial-contents nil initial-contents-p))
  (cond (initial-contents-p
         (make-array length :element-type element-type :initial-contents initial-contents))
        (initial-element-p
         (make-array length :element-type element-type :initial-element initial-element))
        (t (make-array length :element-type element-type))))

(defun free-static-vector (vector)
  (declare (ignore vector))
  nil)

(defun static-vector-p (object)
  (typep object '(simple-array * (*))))

(defun static-vector-pointer (vector &key (offset 0))
  (declare (ignore offset))
  (error "Clysm has no foreign memory: no pointer to ~S" vector))

(defun fill-foreign-memory (pointer length value)
  (declare (ignore length value))
  (error "Clysm has no foreign memory: ~S" pointer))

(defun replace-foreign-memory (dst-ptr src-ptr length)
  (declare (ignore src-ptr length))
  (error "Clysm has no foreign memory: ~S" dst-ptr))

(defmacro with-static-vector ((var length &rest args) &body body)
  `(let ((,var (make-static-vector ,length ,@args)))
     ,@body))

(defmacro with-static-vectors (((var length &rest args) &rest more) &body body)
  (if more
      `(with-static-vector (,var ,length ,@args) (with-static-vectors ,more ,@body))
      `(with-static-vector (,var ,length ,@args) ,@body)))
