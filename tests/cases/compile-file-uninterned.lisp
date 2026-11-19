;;; A file whose forms share an uninterned symbol (a class named by
;;; MAKE-SYMBOL in a macro, as Lem's DEFINE-MAJOR-MODE does), compiled with
;;; COMPILE-FILE and loaded: the DEFCLASS, evaluated at compile time too,
;;; and the forms after it name the same class.  Only with the compiler in
;;; the image (NATIVE=1): a fasl of source forms (without it) reads each
;;; form with its own uninterned symbols.
(defvar *things* (make-hash-table))
(defgeneric describe-thing (thing))
(defun register-thing (name thing) (setf (gethash name *things*) thing))
(defmacro define-thing (name)
  (let ((class (make-symbol (format nil "~A-CLASS" name))))
    `(progn
       (defclass ,class () ((x :initform ',name :reader thing-x)))
       (defmethod describe-thing ((thing ,class)) (list :thing (thing-x thing)))
       (register-thing ',name (make-instance ',class))
       (defun ,name () (thing-x (make-instance ',class))))))

(defun run ()
  (with-open-file (s "build/cf-uninterned.lisp" :direction :output :if-exists :supersede)
    (write-string "(define-thing first-thing) (define-thing second-thing)" s))
  (load (compile-file "build/cf-uninterned.lisp" :output-file
                      (merge-pathnames "build/cf-uninterned.fasl")))
  (print (list (funcall 'first-thing) (funcall 'second-thing)
               (describe-thing (gethash 'first-thing *things*))
               (describe-thing (gethash 'second-thing *things*)))))

(if (or #-clysm t #+clysm sys::*native-compile-form-hook*)
    (run)
    (print '(first-thing second-thing (:thing first-thing) (:thing second-thing))))
