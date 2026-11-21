;;; #S reads a structure through the keyword constructor of its name (the
;;; property %STRUCT-CONSTRUCTOR, kept in a shaken program when #S is).
(defstruct pt x y)
(defstruct (named-pt (:constructor new-named-pt)) name x)
(let ((p (read-from-string "#S(PT :X 1 :Y (2 3))")))
  (print (list (pt-p p) (pt-x p) (pt-y p))))
(let ((p (read-from-string "#S(NAMED-PT :NAME \"a\" :X 4)")))
  (print (list (named-pt-name p) (named-pt-x p))))
(print (pt-x (read-from-string (prin1-to-string (make-pt :x 5)))))
(terpri)
