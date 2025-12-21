;;;; multi-value.lisp - Multiple values support

(in-package #:clysm/runtime/multi-value)

(defvar *mv-count* 1
  "Number of values returned.")

(defvar *mv-buffer* (make-array 20 :initial-element nil)
  "Buffer for secondary values.")

(defun set-values (&rest values)
  "Set multiple return values."
  (setf *mv-count* (length values))
  (loop for v in values
        for i from 0
        do (setf (aref *mv-buffer* i) v))
  (first values))

(defun get-values ()
  "Get multiple return values."
  (loop for i from 0 below *mv-count*
        collect (aref *mv-buffer* i)))
