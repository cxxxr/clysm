;;;; Run instruction collector tests
;;;; sbcl --load tests/contract/instruction-collector/run-tests.lisp

(require :asdf)

(format t "~%Loading clysm...~%")
(asdf:load-system :clysm)

(format t "~%Running full test suite...~%")
(handler-case
    (progn
      (asdf:test-system :clysm)
      (format t "~%Tests completed successfully.~%")
      (sb-ext:exit :code 0))
  (error (c)
    (format t "~%Test error: ~A~%" c)
    (sb-ext:exit :code 1)))
