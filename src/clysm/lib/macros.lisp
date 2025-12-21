;;;; macros.lisp - Standard macros

(in-package #:clysm/lib/macros)

(defmacro when* (test &body body)
  "Execute body if test is true."
  `(if ,test (progn ,@body) nil))

(defmacro unless* (test &body body)
  "Execute body if test is false."
  `(if ,test nil (progn ,@body)))

(defmacro cond* (&rest clauses)
  "Conditional with multiple branches."
  (if clauses
      (let ((clause (first clauses)))
        (if (eq (first clause) t)
            `(progn ,@(rest clause))
            `(if ,(first clause)
                 (progn ,@(rest clause))
                 (cond* ,@(rest clauses)))))
      nil))

(defmacro dolist* ((var list &optional result) &body body)
  "Iterate over a list."
  (let ((lst (gensym)))
    `(let ((,lst ,list))
       (loop while ,lst
             do (let ((,var (car ,lst)))
                  ,@body)
                (setf ,lst (cdr ,lst)))
       ,result)))

(defmacro dotimes* ((var count &optional result) &body body)
  "Iterate a fixed number of times."
  (let ((cnt (gensym)))
    `(let ((,cnt ,count))
       (loop for ,var from 0 below ,cnt
             do (progn ,@body))
       ,result)))
