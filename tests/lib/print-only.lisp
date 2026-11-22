;;; A library that prints symbols but never reads Lisp or names *READTABLE*.
;;; Its printer must not retain the standard reader macro functions.
(js:export "printed" (lambda (name) (prin1-to-string (make-symbol name))))
