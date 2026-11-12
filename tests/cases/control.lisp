(defvar *trace* nil)
(defun note (x) (push x *trace*))
;; return-from across unwind-protect in the same function
(defun f1 ()
  (block b
    (unwind-protect (return-from b 'returned) (note 'cleanup1))
    'not-reached))
(print (list (f1) *trace*))
;; go across unwind-protect
(setq *trace* nil)
(defun f2 ()
  (let ((n 0))
    (tagbody
     again
       (unwind-protect (when (< (incf n) 3) (go again)) (note n)))
    n))
(print (list (f2) (reverse *trace*)))
;; special binding restored by non-local exit
(defvar *sp* 'outer)
(defun f3 () (catch 'c (let ((*sp* 'inner)) (throw 'c *sp*))))
(print (list (f3) *sp*))
(defun f4 () (block b (let ((*sp* 'inner2)) (return-from b *sp*))))
(print (list (f4) *sp*))
(defun f5 () (dolist (x '(1 2 3)) (let ((*sp* x)) (when (= x 2) (return *sp*)))))
(print (list (f5) *sp*))
;; nested unwind-protect order
(setq *trace* nil)
(catch 'out
  (unwind-protect
       (unwind-protect (throw 'out 1) (note 'inner))
    (note 'outer)))
(print (reverse *trace*))
;; multiple values through unwind-protect and catch
(print (multiple-value-list (unwind-protect (values 1 2 3) (values 4 5))))
(print (multiple-value-list (catch 'mv (throw 'mv (values :a :b)))))
(print (multiple-value-list (block blk (return-from blk (values 'x 'y)))))
(defun mv-nonlocal () (block bb (mapc (lambda (x) (when x (return-from bb (values x 'second)))) '(nil 7)) nil))
(print (multiple-value-list (mv-nonlocal)))
(print (multiple-value-list (multiple-value-prog1 (values 1 2) (print 'side))))
;; throw through several functions
(defun thrower (n) (if (= n 0) (throw 'deep 'bottom) (thrower (- n 1))))
(print (catch 'deep (thrower 100)))
;; catch tag is evaluated
(let ((tag (list 'x))) (print (catch tag (throw tag 'dynamic-tag))))
;; error in cleanup-protected code with handler
(setq *trace* nil)
(print (handler-case (unwind-protect (error "e1") (note 'cleaned)) (error () 'handled)))
(print *trace*)
;; progv
(print (progv '(*sp*) '(progv-val) *sp*))
(print *sp*)
;; dynamic binding in recursion
(defvar *depth* 0)
(defun rec (n) (if (= n 0) *depth* (let ((*depth* (1+ *depth*))) (rec (1- n)))))
(print (list (rec 50) *depth*))
;; closures in labels referencing each other and outer variables
(defun make-ops (base)
  (labels ((add (x) (+ x base)) (twice (x) (add (add x))))
    (list #'add #'twice)))
(print (mapcar (lambda (f) (funcall f 1)) (make-ops 10)))
;; closure over loop variable (dotimes reuses its binding)
(print (let (fns) (dotimes (i 3) (push (lambda () i) fns)) (mapcar #'funcall fns)))
(print (let (fns) (dolist (x '(a b c)) (let ((y x)) (push (lambda () y) fns))) (mapcar #'funcall fns)))
;; mutation of captured variables
(defun counter-pair ()
  (let ((n 0)) (values (lambda () (incf n)) (lambda () n))))
(multiple-value-bind (inc get) (counter-pair)
  (funcall inc) (funcall inc) (funcall inc)
  (print (funcall get)))
;; tagbody with numeric tags and nested tagbodies
(print (let ((r nil))
         (tagbody
            1 (push 1 r) (go 3)
            2 (push 2 r)
            3 (push 3 r)
              (tagbody (go inner) (push 'skip r) inner (push 'inner r)))
         (nreverse r)))
;; return from nested blocks with the same name
(print (block a (block a (return-from a 1)) 2))
;; apply with many arguments
(print (apply #'+ (loop for i below 1000 collect i)))
(print (length (apply #'list (loop for i below 5000 collect i))))
(print (multiple-value-list (values-list (loop for i below 20 collect i))))
;; recursion through funcall/apply in tail position
(defun loop-apply (n) (if (= n 0) 'done (apply #'loop-apply (list (- n 1)))))
(print (loop-apply 100000))
(defun loop-funcall (n) (if (= n 0) 'done (funcall #'loop-funcall (- n 1))))
(print (loop-funcall 100000))
;; mutual tail recursion
(defun ev? (n) (if (= n 0) t (od? (- n 1))))
(defun od? (n) (if (= n 0) nil (ev? (- n 1))))
(print (ev? 1000001))
;; let over lambda over let
(print (funcall (let ((a 1)) (let ((b 2)) (lambda (c) (list a b c)))) 3))
;; setq of special inside lambda
(defun set-special () (setq *sp* 'changed))
(let ((*sp* 'bound)) (set-special) (print *sp*))
(print *sp*)
(terpri)
