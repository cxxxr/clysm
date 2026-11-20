;;; FTYPE proclamations whose types are not all ones TYPEP takes: function
;;; types (also inside OR), VALUES and SATISFIES types, types defined after
;;; the proclamation, and checked ones (fixnum ranges, simple vectors).

(declaim (ftype (function ((function (list &key &allow-other-keys) list) list) list) apply-to))
(defun apply-to (f l) (funcall f l))
(print (apply-to #'reverse '(1 2 3)))

(declaim (ftype (function (list) (values (function (t) t) &optional)) adder))
(defun adder (l) (lambda (x) (cons x l)))
(print (funcall (adder '(b)) 'a))
(defun use-adder (l) (let ((f (adder l))) (funcall f 0)))
(print (use-adder '(1)))

(declaim (ftype (function ((or null (function (t) t)) t) (values t &optional)) maybe-call))
(defun maybe-call (f x) (if f (funcall f x) x))
(print (list (maybe-call nil 1) (maybe-call #'1+ 1)))

(declaim (ftype (function (t) (values t t)) two-values))
(defun two-values (x) (values x (list x)))
(print (multiple-value-list (two-values 3)))
(defun use-two (x) (let ((a (two-values x))) a))
(print (use-two 4))

(declaim (ftype (function ((satisfies evenp)) (values (satisfies oddp) &optional)) next-odd))
(defun next-odd (n) (1+ n))
(print (+ 1 (next-odd 4)))

;; types defined after the proclamation
(declaim (ftype (function (later-type later-struct) (values later-type &optional)) use-later))
(deftype later-type () '(integer 0 100))
(defstruct later-struct (n 1))
(defun use-later (x s) (+ x (later-struct-n s)))
(print (use-later 5 (make-later-struct :n 2)))
(defun call-later (x) (let ((y (use-later x (make-later-struct)))) (* y 2)))
(print (call-later 7))

;; a type defined as a function type
(deftype callback () '(function (fixnum) fixnum))
(declaim (ftype (function (callback fixnum) (values fixnum &optional)) run-callback))
(defun run-callback (cb n) (funcall cb n))
(print (run-callback (lambda (n) (* n n)) 7))

;; checked types
(deftype index () '(integer 0 1000))
(declaim (ftype (function (index simple-vector) (values t &optional)) vref))
(defun vref (i v) (svref v i))
(print (vref 1 #(a b c)))
(print (handler-case (vref -1 #(a)) (type-error () :type-error)))
(print (handler-case (vref 0 "a") (type-error () :type-error)))
(declaim (ftype (function (t) (values fixnum &optional)) ident))
(defun ident (x) x)
(defun use-ident (x) (+ 1 (ident x)))
(print (use-ident 41))
(print (handler-case (use-ident "x") (type-error () :type-error)))
(defun tail-ident (x) (ident x))
(print (tail-ident 5))

;; malformed or partial proclamations are ignored
(declaim (ftype function loose))
(defun loose (x) x)
(print (loose 1))
(declaim (ftype (function (&rest t) (values list &optional)) rest-list))
(defun rest-list (&rest xs) xs)
(print (length (rest-list 1 2 3)))
