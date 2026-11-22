;;; The program of tests/static/run.sh: compiled after the systems, not a
;;; file of one.
(in-package :geom)

(defun report (name) (format t "~A~%" (describe-thing (shape name))))

(defun main ()
  (format t "load order: ~S~%" (reverse *load-order*))
  (format t "units: ~S scale ~D~%" (reverse *units*) +scale+)
  (report "disc") (report "slab")
  (format t "~A~%" (describe-thing (make-point :x 3 :y 4)))
  (format t "~A~%" (describe-thing 42))
  (format t "norm2 ~D~%" (norm2 (make-point :x 3 :y 4)))
  (format t "table ~D ~D octets ~S ~S~%" (gethash "km" (unit-table)) (hash-table-count (unit-table))
          (geom-util::octets) (type-of (geom-util::home)))
  (format t "collected ~S~%" (with-collector (keep) (twice (keep 1)) (keep 2)))
  (format t "tags ~S ~S ~S~%" (geom-util::tag-name (origin-tag)) (eq (origin-tag) (origin-tag))
          (mapcar (lambda (x) (if (geom-util::tag-p x) (geom-util::tag-name x) x)) (tags)))
  ;; names looked up when the program runs, and a function redefined
  (format t "found ~S ~S~%" (find-symbol "AREA" :geom) (funcall (intern "SQUARE" :geom-util) 7))
  (format t "within ~S" (geom-util:within-tolerance-p 1/100))
  (setf (symbol-function 'tolerance) (lambda () 1/10))
  (format t " ~S~%" (geom-util:within-tolerance-p 1/100))
  (format t "error: ~A~%" (handler-case (shape "cube") (error (e) e)))
  ;; the objects made when the program runs are each their own, and the
  ;; literal lists that end as their cells began are as they were written
  (format t "literals ~S ~S ~S ~S~%" (plain-lists) (eq (origin-tag) (first (tags)))
          (eq (first (tags)) (fourth (tags))) (equal (geom-util::octets) (geom-util::home)))
  #+clysm-static (format t "static~%"))
