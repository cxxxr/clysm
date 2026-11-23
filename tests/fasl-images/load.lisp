;;; tests/fasl-images/run.sh, the second image: no FI-EXTRA
(load (merge-pathnames "gf.fasl" cl-user::*fi-dir*))
(defclass other-class () ())
(print (list (fi-gf::use-it (make-instance 'other-class))
             (handler-case (fi-gf::use-it 3) (error () :no-method))))
(terpri)
