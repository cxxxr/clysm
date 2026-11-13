;;; read-time and compile-time evaluation with user packages
(defpackage :mylib (:use :cl) (:nicknames :mylib.1) (:export #:twice))
(in-package :mylib)
(defun twice (x) (* 2 x))
;; a skipped form may name packages that do not exist
#+no-such-feature (no-such-package:frob #.(no-such-package:x) #\no-such-char)
;; #. forms are evaluated with the target package names
(print '#.(package-name (find-package :mylib.1)))
(print #.(length (let (r) (do-external-symbols (s :mylib.1) (push s r)) r)))
;; macro bodies that call FIND-PACKAGE / INTERN with target package names
(defmacro def-getter (name)
  `(defun ,(intern (concatenate 'string "GET-" (symbol-name name)) (find-package :mylib)) ()
     (list ',name (find-package "MYLIB"))))
(def-getter foo)
(print (first (get-foo)))
(print (package-name (second (get-foo))))
(in-package :cl-user)
(print (mylib:twice 21))
(terpri)
(print (read-from-string "(#A((3) BASE-CHAR . \"abc\") #A((2 2) T . ((1 2) (3 4))) #2A((1 2) (3 4)))"))
(print (stringp (car (read-from-string "(#A((3) BASE-CHAR . \"abc\"))"))))
