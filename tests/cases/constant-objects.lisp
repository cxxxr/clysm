;;; Constants whose values are objects a fasl cannot write (a package, a
;;; hash table): code that uses them reads the constant's value.
(defconstant +keyword-package+ (find-package :keyword))
(defconstant +numbers+ '(1 2 3))
(defun intern-key (name) (intern name +keyword-package+))
(defun sum-numbers () (reduce #'+ +numbers+))
(print (list (intern-key "ABC") (eq (symbol-package (intern-key "X")) +keyword-package+)
             (sum-numbers)))
