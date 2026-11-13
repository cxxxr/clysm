(print (let ((i 0)) (loop for nil from 10 to 15 collect (incf i))))
(print (let (z) (values (loop for x from 1 to 10 count (< x 5) into foo float finally (setq z foo)) z)))
(print (loop named foo for i in '(a b c) by (return-from foo :good) return :bad))
(print (loop for x on '(1 2 3 4) by #'cddr collect x))
(print (handler-case (macroexpand '(loop for x in '(a) for x in '(b) collect x)) (program-error () 'program-error)))
(print (loop for i from 0 to 10 by 2 sum i into s double-float finally (return s)))
(terpri)

;; a FOR clause after WHILE is stepped where it is (Lem's display relies on it)
(print (let ((s "abc"))
         (loop :with pos := 0
               :while (< pos (length s))
               :for c := (char s pos)
               :collect c
               :do (incf pos))))
(print (loop for i below 5 while (< i 3) for j = (* i i) collect j))
(print (loop for x in '(1 2 3) until (> x 2) for y = x then (+ y 10) collect y))
(print (list (char-code #\cr) (char-code #\lf) (char-code #\ht) (char-code #\ff) (char-code #\bs)))
