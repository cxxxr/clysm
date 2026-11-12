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
