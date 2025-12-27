;;;; test-many-defuns.lisp - Test combining many defuns to find the NAME issue

(ql:quickload :clysm :silent t)

(in-package :cl-user)

(defun generate-many-defuns (n)
  "Generate N simple defun forms"
  (loop for i from 1 to n
        collect `(defun ,(intern (format nil "FUNC-~D" i)) ()
                   ,i)))

(defun test-increasing-defuns ()
  "Test with increasing numbers of defuns to find the threshold"
  (let ((compile-fn #'clysm/compiler:compile-to-wasm))
    (loop for n in '(10 20 50 100 150 185 200)
          do (format t "Testing ~D defuns...~%" n)
             (let ((forms (generate-many-defuns n)))
               (handler-case
                   (let ((bytes (funcall compile-fn `(progn ,@forms))))
                     (format t "  OK: ~D bytes~%" (length bytes)))
                 (error (e)
                   (format t "  FAIL: ~A~%" e)
                   (return-from test-increasing-defuns)))))))

(defun test-key-params-many ()
  "Test many defuns with &key parameters"
  (let ((compile-fn #'clysm/compiler:compile-to-wasm))
    (format t "~%Testing defuns with &key NAME parameter:~%")
    (loop for n in '(5 10 20 50)
          do (format t "Testing ~D defuns with &key name...~%" n)
             (let ((forms (loop for i from 1 to n
                               collect `(defun ,(intern (format nil "MAKE-THING-~D" i))
                                            (&key name value)
                                          (list ',i name value)))))
               (handler-case
                   (let ((bytes (funcall compile-fn `(progn ,@forms))))
                     (format t "  OK: ~D bytes~%" (length bytes)))
                 (error (e)
                   (format t "  FAIL: ~A~%" e)
                   (return-from test-key-params-many)))))))

(test-increasing-defuns)
(test-key-params-many)
