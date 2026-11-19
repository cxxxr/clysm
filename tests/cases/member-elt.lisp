;;; MEMBER with a constant list or only :TEST, and ELT, compiled at the call
;;; site: what they return, the order the arguments are evaluated in, errors.
(defvar *order* '())
(defun note (x) (push x *order*) x)

(defun sep-p (c) (member c '(#\: #\T #\- #\+ #\Z #\.)))
(print (list (sep-p #\-) (sep-p #\a) (sep-p #\Z)))
(print (list (member 'b '(a b c)) (member 3 '(1 2 3 4)) (member nil '(a nil b)) (member 'z '(a b))))
(print (member 2.0 '(1 2 3)))                     ; EQL: not =
(print (list (member "ab" '("x" "ab" "y") :test #'string=)
             (member #\A '(#\x #\a) :test #'char-equal)
             (member "AB" (list "x" "ab") :test 'string-equal)
             (member 3 '(1 5 9) :test #'<)
             (member 3 '(1 5 9) :test (lambda (a b) (= (* a 3) b)))))
(print (let ((l (list 1 2 3))) (eq (member 2 l :test #'eql) (cdr l))))
(print (progn (member (note 1) (note (list 0 1)) :test (note #'eql)) (reverse *order*)))
(print (handler-case (member 'x '(a b . c) :test #'eq) (error () :not-a-list)))
(print (handler-case (member 'x (cons 'a 'b)) (error () :not-a-list)))

(defvar *v* (vector 10 20 30))
(defvar *s* "hello")
(defvar *b* (make-array 4 :element-type '(unsigned-byte 8) :initial-contents '(1 2 3 4)))
(defvar *f* (let ((v (make-array 5 :fill-pointer 2 :initial-element 9))) v))
(print (list (elt *v* 1) (elt *s* 4) (elt *b* 3) (elt '(a b c) 2) (elt *f* 1)))
(print (list (handler-case (elt *f* 3) (error () :beyond-fill-pointer))
             (handler-case (elt *v* 3) (error () :out-of-bounds))
             (handler-case (elt '(a b) 5) (error () :out-of-bounds))
             (handler-case (elt 5 0) (error () :not-a-sequence))))
(let ((v (vector 1 2 3))) (setf (elt v 0) 'x) (print v))
