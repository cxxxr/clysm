;;; package-local nicknames (DEFPACKAGE :LOCAL-NICKNAMES), read by the
;;; compiler's reader as by FIND-PACKAGE (issue #114)
(defpackage #:pln-a (:use #:cl) (:export #:hello))
(in-package #:pln-a)
(defun hello () "hello from A")

(defpackage #:pln-c (:use #:cl) (:export #:hello))
(in-package #:pln-c)
(defun hello () "hello from C")

;; X names PLN-A; the local nickname PLN-C hides the global package PLN-C
(defpackage #:pln-b (:use #:cl) (:local-nicknames (#:x #:pln-a) (#:pln-c #:pln-a)))
(in-package #:pln-b)
(print (x:hello))
(print (pln-c:hello))
(print (funcall 'x::hello))
(print (package-name (find-package "X")))
(print (package-name (find-package "PLN-C")))
(let ((s (read-from-string "x:hello")))
  (print (list (symbol-name s) (package-name (symbol-package s)))))

;; only in PLN-B
(in-package #:cl-user)
(print (find-package "X"))
(print (pln-c:hello))
