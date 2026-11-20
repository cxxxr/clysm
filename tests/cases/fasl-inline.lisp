;;; Small functions of a compiled file called from files compiled after it
;;; (in the image the callers contain a guarded copy of the callee): the
;;; values, a redefinition after and before the caller is loaded, NOTINLINE,
;;; a callee that keeps its lexical environment, a non-local exit.
(defun write-forms (path forms)
  (with-open-file (s path :direction :output :if-exists :supersede)
    (with-standard-io-syntax
      (let ((*print-circle* t))
        (dolist (f forms) (prin1 f s) (terpri s))))))

(write-forms "build/fi-test-a.lisp"
  '((defvar *fi-v* 10)
    (defclass fi-line () ((str :initarg :str :reader fi-str)))
    (defun fi-length (l) (length (fi-str l)))
    (defun fi-add (x) (+ x *fi-v*))
    (defun fi-opt (x &optional (y 2) &key (z 3)) (list x y z))
    (defun fi-exit (x) (if (> x 5) (return-from fi-exit :big) x))
    (declaim (notinline fi-noinl))
    (defun fi-noinl (x) (* x 3))
    (let ((counter 0)) (defun fi-count () (incf counter)))
    (defun fi-later (x) (list :first x))))

(write-forms "build/fi-test-b.lisp"
  '((defun fi-use (l x)
      (list (fi-length l) (fi-add x) (fi-opt x) (fi-opt x 5 :z 6) (fi-exit x) (fi-exit (* 3 x))
            (fi-noinl x) (fi-count) (fi-later x)))
    (defun fi-shadow (x) (let ((*fi-v* 1)) (fi-add x)))))

(load (compile-file "build/fi-test-a.lisp" :verbose nil :print nil))
;; redefined before the file that calls it is compiled and loaded
(defun fi-later (x) (list :second x))
(load (compile-file "build/fi-test-b.lisp" :verbose nil :print nil))
(defvar *l* (make-instance 'fi-line :str "hello"))
(print (fi-use *l* 2))
(print (fi-shadow 2))
;; redefined after
(defun fi-add (x) (list :new x))
(setf (symbol-function 'fi-length) (lambda (l) (list :lambda (fi-str l))))
(print (fi-use *l* 2))
;; the fasls loaded again: the copies are used again
(load "build/fi-test-a.fasl")
(load "build/fi-test-b.fasl")
(print (fi-use *l* 3))
(terpri)
