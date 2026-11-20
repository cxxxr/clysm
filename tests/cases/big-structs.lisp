;;; A structure of more than 32 slots that includes a structure of field
;;; slots (a "big" structure, runtime/struct.lisp): the accessors of the
;;; included structure take its instances, however their class is known
;;; (undeclared after an access, in an inline copy, declared, in a fasl).
(defstruct bpa x y)
(defstruct (bpb (:include bpa)) s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20
  s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 (s33 33) (s34 34))
(defstruct (bpc (:include bpb)) (z 'zz))
(defstruct bpd (v 0d0 :type double-float) w)
(defstruct (bpe (:include bpd)) s1 s2 s3 s4 s5 s6 s7 s8 s9 s10 s11 s12 s13 s14 s15 s16 s17 s18 s19 s20
  s21 s22 s23 s24 s25 s26 s27 s28 s29 s30 s31 s32 (s33 33))
;; undeclared, the class known after the first access (and copied inline at the calls)
(defun bfq (o) (setf (bpa-y o) 7) (list (bpa-x o) (bpa-y o)))
(defun bfu (o) (incf (bpa-x o)) (incf (bpa-x o)) (bpa-x o))
(defun bfd (o) (incf (bpd-v o) 1.5d0) (setf (bpd-w o) (bpd-v o)) (list (bpd-v o) (bpd-w o)))
(defun bfdecl (o) (declare (bpa o)) (setf (bpa-y o) 8) (list (bpa-x o) (bpa-y o)))
(declaim (notinline bfn))
(defun bfn (o) (setf (bpa-y o) 9) (list (bpa-x o) (bpa-y o)))
(print (list (bfq (make-bpa :x 1)) (bfq (make-bpb :x 2)) (bfq (make-bpc :x 3))))
(print (list (bfu (make-bpb :x 2)) (bfdecl (make-bpc :x 4)) (bfn (make-bpb :x 5))))
(print (list (bfd (make-bpd :v 1d0)) (bfd (make-bpe :v 2d0))))
(let ((xs (list (make-bpa :x 1) (make-bpb :x 2) (make-bpc :x 3))) (s 0))
  (dolist (o xs) (incf s (bpa-x o)) (incf s (bpa-x o)))
  (print s))
(let ((b (make-bpb :x 5 :s33 'a)))
  (print (list (bpb-s33 b) (bpb-s34 b) (bpb-s1 b) (bpa-x b) (bpb-p b) (bpa-p b) (bpc-p b) (typep b 'bpa)))
  (setf (bpb-s34 b) 99 (bpb-s1 b) 11)
  (let ((c (copy-bpb b))) (setf (bpb-s34 c) 100) (print (list (bpb-s34 b) (bpb-s34 c) (bpb-s1 c))))
  (print (equalp b (copy-structure b)))
  (print (bpb-s34 (read-from-string (prin1-to-string b)))))
(let ((c (make-bpc :x 1)))
  (print (list (bpc-z c) (bpb-s34 c) (bpa-x c) (bpc-p c) (bpb-p c)))
  (print (equalp c (make-bpc :x 1))))
(print (handler-case (bpb-s33 (make-bpa)) (type-error () :type-error)))
;; in a fasl
(with-open-file (s "build/big-structs-fasl.lisp" :direction :output :if-exists :supersede)
  (print '(defun bfasl (o) (setf (bpa-y o) 6) (incf (bpa-x o)) (list (bpa-x o) (bpa-y o) (bpb-s34 o))) s))
(load (compile-file "build/big-structs-fasl.lisp"))
(print (funcall 'bfasl (make-bpb :x 1)))
