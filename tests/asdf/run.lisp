;;;; tests/asdf/run.lisp — load a small system with the bundled ASDF
;;;;   bin/clysm script tests/asdf/run.lisp
(require :asdf)
(format t "~&ASDF ~A~%" (asdf:asdf-version))
(push (merge-pathnames "greet/" (uiop:pathname-directory-pathname *load-truename*))
      asdf:*central-registry*)
(asdf:load-system "greet" :force t)
(format t "~A~%" (greet:greet "world"))
(format t "version ~A~%" (asdf:component-version (asdf:find-system "greet")))
(asdf:test-system "greet")
(format t "~{~A~^ ~}~%" (mapcar #'pathname-name (directory (merge-pathnames "greet/**/*.lisp" (uiop:pathname-directory-pathname *load-truename*)))))

;;; a system of two files: loaded again, from its system fasl (one module);
;;; the calls between its files still see a redefinition
(push (merge-pathnames "twofiles/" (uiop:pathname-directory-pathname *load-truename*))
      asdf:*central-registry*)
(asdf:load-system "twofiles" :force t)
(format t "~A ~A~%" (twofiles:total 10) (symbol-value (find-symbol "*LOADS*" "TWOFILES")))
(asdf:clear-system "twofiles")
(asdf:load-system "twofiles")
(format t "system fasl: ~A~%" (and (member "twofiles" asdf::*clysm-system-fasls-loaded* :test #'string=) t))
(format t "~A ~A~%" (twofiles:total 10) (symbol-value (find-symbol "*LOADS*" "TWOFILES")))
(eval '(defun twofiles:g (x) (- x)))
(eval '(defun twofiles:h (x) (* x 100)))
(format t "~A ~A~%" (twofiles:total 10) (twofiles:f 3))
