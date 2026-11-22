;;; Three files compiled and loaded in turn: the second calls a generic
;;; function of the first, the third adds methods and a class.  The
;;; discriminating function of the second file was made from the methods of
;;; the first; with the compiler in the image (NATIVE=1) it is replaced by
;;; one made from the methods as they are (%GF-DISC-REGENERATE, see
;;; "Dispatch compiled as code" in docs/INTERNALS.md).  The results are the
;;; same either way.
(defvar *rg-log* nil)
(defun write-file (name text)
  (with-open-file (s name :direction :output :if-exists :supersede) (write-string text s))
  name)
(defun load-compiled (name text)
  (load (compile-file (write-file (format nil "build/~A.lisp" name) text))))

(load-compiled "cd-regen-a" "
(defclass rg1 () ())
(defclass rg2 (rg1) ())
(defclass rg3 (rg2) ())
(defgeneric rg-walk (x y))
(defmethod rg-walk ((x rg1) y) (list :rg1 y))
(defmethod rg-walk ((x rg2) y) (list :rg2 (call-next-method)))
(defmethod rg-walk :around ((x rg3) y) (list :around (call-next-method x (+ y 1))))
(defmethod rg-walk :before ((x rg2) y) (push y *rg-log*))
(defgeneric rg-kind (x))
(defmethod rg-kind ((x integer)) :integer)
(defmethod rg-kind ((x (eql :k))) :k)
")
(load-compiled "cd-regen-b" "
(defun rg-objects () (list (make-instance 'rg1) (make-instance 'rg2) (make-instance 'rg3)))
(defun rg-run (objects)
  (let ((r nil))
    (dolist (o objects) (push (handler-case (rg-walk o 1) (error () :error)) r))
    (dolist (x '(1 :k \"s\" nil)) (push (handler-case (rg-kind x) (error () :error)) r))
    (nreverse r)))
")
(defvar *objects* (rg-objects))
(print (rg-run *objects*))
(dotimes (i 20) (rg-run *objects*))
(load-compiled "cd-regen-c" "
(defclass rg4 (rg1) ())
(defmethod rg-walk ((x rg4) y) (list :rg4 (call-next-method)))
(defmethod rg-walk ((x integer) y) (list :int x y))
(defmethod rg-kind ((x string)) :string)
(defmethod rg-kind ((x null)) :null)
")
(setq *objects* (append *objects* (list (make-instance 'rg4) 7)))
(print (rg-run *objects*))
(dotimes (i 400) (rg-run *objects*))
(print (rg-run *objects*))
(print (length *rg-log*))
;; the functions of the second file call ones made from the methods as they are now
(print #-clysm t
       #+clysm (or (not sys::*native-compile-form-hook*)
                   (let ((n 0))
                     (dolist (c sys::*gf-discs*) (when (functionp (svref c 0)) (incf n)))
                     (or (>= n 2) n))))
;; a method is redefined: the new one is called
(defmethod rg-kind ((x integer)) :int)
(print (rg-run *objects*))
(dotimes (i 400) (rg-run *objects*))
(print (rg-run *objects*))
