;;; COMPILE-FILE and LOAD of the fasl: uninterned symbols shared by the
;;; forms of a file, INLINE functions across files, DEFUNs and DEFSTRUCTs
;;; evaluated at compile time too, calls between the functions of a file.
(defun write-forms (path forms)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-circle* t))
        (dolist (f forms) (prin1 f s) (terpri s))))))

(write-forms "build/cf-test-a.lisp"
  '((eval-when (:compile-toplevel :load-toplevel :execute)
      (defvar *cf-gsym* (make-symbol "CF-THING")))
    (defmacro cf-def-class () `(defclass ,*cf-gsym* () ((a :initform 7 :accessor cf-a))))
    (cf-def-class)
    (defvar *cf-other* 1)
    (defmacro cf-mk () `(make-instance ',*cf-gsym*))
    (defmacro cf-class-name () `',*cf-gsym*)
    (defun cf-make () (cf-mk))
    (defparameter *cf-names* (list (cf-class-name) (cf-class-name)))
    (declaim (inline cf-add))
    (defun cf-add (x &optional (n 1) &key (k 0)) (+ x n k))
    (eval-when (:compile-toplevel :load-toplevel :execute)
      (defun cf-helper (x) (list 'quote (list x x)))
      (defstruct cf-pt x (y 0)))
    (defmacro cf-mac (x) (cf-helper x))
    (defun cf-twice (x) (* 2 x))
    (defun cf-uses (x) (list (cf-twice x) (cf-mac 3) (cf-helper 4)
                             (let ((p (make-cf-pt :x x))) (incf (cf-pt-y p) 5) (list (cf-pt-x p) (cf-pt-y p)))))))

(write-forms "build/cf-test-b.lisp"
  '((defun cf-use-add (x) (cf-add x 2 :k 3))))

(load (compile-file "build/cf-test-a.lisp" :verbose nil :print nil))
(load (compile-file "build/cf-test-b.lisp" :verbose nil :print nil))
(print (list (cf-a (cf-make))
             (symbol-name (class-name (class-of (cf-make))))
             (eq (first *cf-names*) (second *cf-names*))
             (eq (first *cf-names*) (class-name (class-of (cf-make))))))
(print (cf-uses 10))
(print (cf-use-add 1))
(terpri)
