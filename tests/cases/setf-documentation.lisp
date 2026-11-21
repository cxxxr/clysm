;;; (SETF DOCUMENTATION), which the runtime calls as (FDEFINITION '(SETF
;;; DOCUMENTATION)): kept in a shaken program (its symbol is the one #'(SETF
;;; DOCUMENTATION) names).
(defvar *x* 1)
(setf (documentation '*x* 'variable) "The x.")
(defun f () "A function." 1)
(setf (documentation 'f 'function) "Another doc.")
(print (list (documentation '*x* 'variable) (documentation 'f 'function)))
(terpri)
