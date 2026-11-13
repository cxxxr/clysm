;;;; trivial-garbage.lisp — trivial-garbage's interface for Clysm

(defpackage #:trivial-garbage
  (:use #:cl)
  (:shadow #:make-hash-table)
  (:nicknames #:tg)
  (:export #:gc #:make-weak-pointer #:weak-pointer-value #:weak-pointer-p
           #:make-weak-hash-table #:hash-table-weakness
           #:finalize #:cancel-finalization))

(in-package #:trivial-garbage)

(defun gc (&key full verbose)
  (declare (ignore full verbose))
  nil)

(defstruct (weak-pointer (:constructor %make-weak-pointer (value)))
  value)

(defun make-weak-pointer (object) (%make-weak-pointer object))

(defvar *weakness* (cl:make-hash-table :test 'eq))

(defun make-weak-hash-table (&rest args &key weakness weakness-matters &allow-other-keys)
  (declare (ignore weakness-matters))
  (let ((table (apply #'cl:make-hash-table
                      (loop for (k v) on args by #'cddr
                            unless (member k '(:weakness :weakness-matters))
                              append (list k v)))))
    (when weakness (setf (gethash table *weakness*) weakness))
    table))

(defun make-hash-table (&rest args)
  (apply #'make-weak-hash-table args))

(defun hash-table-weakness (ht)
  (values (gethash ht *weakness*)))

(defun finalize (object function)
  (declare (ignore function))
  object)

(defun cancel-finalization (object)
  (declare (ignore object))
  nil)
