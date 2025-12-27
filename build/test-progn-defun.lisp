;;;; test-progn-defun.lisp - Debug the specific progn+defun failure

(ql:quickload :clysm :silent t)

(in-package :cl-user)

(defun test-progn-with-key-params ()
  "Test progn containing defuns with &key parameters"
  (let ((compile-fn #'clysm/compiler:compile-to-wasm))
    ;; Test individual defun with &key name
    (format t "Test 1: Individual defun with &key name~%")
    (handler-case
        (let ((bytes (funcall compile-fn
                       '(defun make-thing (&key name type)
                          (list 'thing name type)))))
          (format t "  OK: ~D bytes~%" (length bytes)))
      (error (e)
        (format t "  FAIL: ~A~%" e)))

    ;; Test progn with single defun
    (format t "~%Test 2: Progn with single defun~%")
    (handler-case
        (let ((bytes (funcall compile-fn
                       '(progn
                          (defun make-thing-2 (&key name type)
                            (list 'thing name type))))))
          (format t "  OK: ~D bytes~%" (length bytes)))
      (error (e)
        (format t "  FAIL: ~A~%" e)))

    ;; Test progn with two defuns
    (format t "~%Test 3: Progn with two defuns~%")
    (handler-case
        (let ((bytes (funcall compile-fn
                       '(progn
                          (defun make-thing-3a (&key name type)
                            (list 'thing name type))
                          (defun make-thing-3b (&key value)
                            (list 'other value))))))
          (format t "  OK: ~D bytes~%" (length bytes)))
      (error (e)
        (format t "  FAIL: ~A~%" e)))

    ;; Test with accessor-like defuns (from defstruct expansion)
    (format t "~%Test 4: Defstruct-like expansion~%")
    (handler-case
        (let ((bytes (funcall compile-fn
                       '(progn
                          (defun make-wasm-global (&key name type)
                            (list 'wasm-global name type))
                          (defun wasm-global-name (struct)
                            (nth 1 struct))
                          (defun wasm-global-type (struct)
                            (nth 2 struct))
                          (defun wasm-global-p (obj)
                            (and (consp obj) (eq (car obj) 'wasm-global)))))))
          (format t "  OK: ~D bytes~%" (length bytes)))
      (error (e)
        (format t "  FAIL: ~A~%" e)))

    ;; Test simpler forms only
    (format t "~%Test 5: Simple defuns only~%")
    (handler-case
        (let ((bytes (funcall compile-fn
                       '(progn
                          (defun foo1 () 1)
                          (defun foo2 () 2)
                          (defun foo3 () 3)
                          (defun foo4 () 4)))))
          (format t "  OK: ~D bytes~%" (length bytes)))
      (error (e)
        (format t "  FAIL: ~A~%" e)))))

(test-progn-with-key-params)
