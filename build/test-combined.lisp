;;;; test-combined.lisp - Debug combined form compilation
;;;; Tests why combining compilable forms fails with "Unknown instruction: NAME"

(ql:quickload :clysm :silent t)

(in-package :cl-user)

(defun test-combined-forms ()
  ;; Test with simple defuns (no declares)
  (let ((test-forms
          '((defun test-add (a b) (+ a b))
            (defun test-mul (a b) (* a b))
            (defun test-sub (a b) (- a b))
            (defun test-inc (x) (+ x 1)))))
    (format t "Testing ~D simple forms~%" (length test-forms))
    (let ((compile-fn #'clysm/compiler:compile-to-wasm))
      ;; Try each individually
      (format t "Testing individual forms...~%")
      (loop for form in test-forms
            for i from 0
            do (handler-case
                   (progn
                     (funcall compile-fn form)
                     (format t "Form #~D OK: ~S~%" i (cadr form)))
                 (error (err)
                   (format t "Form #~D FAIL: ~A~%Form: ~S~%" i err form))))
      ;; Try combined
      (format t "~%Testing combined (progn ...)~%")
      (handler-case
          (let ((bytes (funcall compile-fn `(progn ,@test-forms))))
            (format t "Combined OK! ~D bytes~%" (length bytes)))
        (error (e)
          (format t "Combined FAIL: ~A~%" e))))))

(defun test-with-keywords ()
  "Test defun with keyword parameters"
  (let ((test-forms
          '((defun test-key1 (&key name) name)
            (defun test-key2 (&key value) value))))
    (format t "~%Testing keyword parameter forms~%")
    (let ((compile-fn #'clysm/compiler:compile-to-wasm))
      (loop for form in test-forms
            for i from 0
            do (handler-case
                   (progn
                     (funcall compile-fn form)
                     (format t "Form #~D OK: ~S~%" i (cadr form)))
                 (error (err)
                   (format t "Form #~D FAIL: ~A~%Form: ~S~%" i err form))))
      ;; Try combined
      (format t "~%Testing combined...~%")
      (handler-case
          (let ((bytes (funcall compile-fn `(progn ,@test-forms))))
            (format t "Combined OK! ~D bytes~%" (length bytes)))
        (error (e)
          (format t "Combined FAIL: ~A~%" e))))))

(defun test-multiple-key-params ()
  "Test defun with multiple keyword parameters"
  (let ((form '(defun make-thing (&key name type) (list name type))))
    (format t "~%Testing multiple &key params: ~S~%" form)
    (let ((compile-fn #'clysm/compiler:compile-to-wasm))
      (handler-case
          (let ((bytes (funcall compile-fn form)))
            (format t "OK! ~D bytes~%" (length bytes)))
        (error (e)
          (format t "FAIL: ~A~%" e))))))

(test-combined-forms)
(test-with-keywords)
(test-multiple-key-params)
