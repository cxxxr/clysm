;;; ASSERT, whose error and CONTINUE restart are out of line; the test of a
;;; variable always bound to a lambda (true, so that a copy given a lambda
;;; makes no closure); the compiler macro of a reader that DEFCLASS defines,
;;; which the fasl keeps for the files compiled after it.
(defvar *x* 0)
(print (handler-bind ((error (lambda (c) (setf *x* (+ *x* 1)) (continue c))))
         (assert (= *x* 2))
         (list :ok *x*)))
(print (handler-case (assert (> 1 2) () "bad ~A ~A" 1 2) (error (c) (princ-to-string c))))
(print (handler-case (assert nil) (error (c) (type-of c))))
(print (assert t))
(print (let ((n 0)) (assert (progn (incf n) t)) n))

(defun call-maybe (fn zero-fn n)
  (cond ((zerop n) (if zero-fn (funcall zero-fn) :none))
        (t (funcall fn n))))
(defun use (x n) (call-maybe (lambda (k) (+ k x)) (lambda () (list x)) n))
(defun use-nil (n) (call-maybe (lambda (k) (* 2 k)) nil n))
(print (list (use 5 0) (use 5 3) (use-nil 0) (use-nil 4)))
(defun use2 (x n) (let ((f (lambda () x))) (if f (funcall f) n)))
(defun use3 (x) (flet ((g (fn) (when fn (funcall fn)))) (list (g (lambda () x)) (g nil))))
(print (list (use2 7 1) (use3 8)))

(defun write-forms (path forms)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (dolist (f forms) (prin1 f s) (terpri s)))))
(write-forms "build/alt-test-a.lisp"
  '((defclass alt-box () ((v :initarg :v :accessor alt-v) (w :initform 3 :reader alt-w)))))
(write-forms "build/alt-test-b.lisp"
  '((defun alt-sum (b) (setf (alt-v b) (+ (alt-v b) (alt-w b))) (alt-v b))))
(load (compile-file "build/alt-test-a.lisp"))
(when (member :clysm *features*)
  ;; (as in a later session, which only loads the fasl)
  (setf (compiler-macro-function 'alt-v) nil (compiler-macro-function 'alt-w) nil)
  (load "build/alt-test-a.fasl"))
(print (if (member :clysm *features*)
           (and (compiler-macro-function 'alt-v) (compiler-macro-function 'alt-w) t)
           t))
(load (compile-file "build/alt-test-b.lisp"))
(print (let ((b (make-instance 'alt-box :v 4))) (list (alt-sum b) (alt-sum b))))
