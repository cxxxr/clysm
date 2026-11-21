;;; Slots declared of integer types: checked where they are written (the
;;; constructors, the SETF of the accessors, SLOT-VALUE), as SBCL does, and
;;; their compiled reads are of the type (a FIXNUM slot is SBCL's 62 bits).
(defstruct cnt
  (n 0 :type fixnum) (b 0 :type (unsigned-byte 8)) (h 0 :type (unsigned-byte 32))
  (i 0 :type integer) (m 0 :type (mod 10)) (s -1 :type (signed-byte 16)) (r 5 :type (integer 1 (10))))
(defstruct (cb (:constructor mk-cb (a &optional (z 7)))) a (z 0 :type (unsigned-byte 16)))
(defstruct (sub (:include cnt (b 1 :type bit))) x)

;; compiled code checks declarations (the evaluator of the image, INTERP=1
;; tools/run-tests.sh, does not check SLOT-VALUE)
(defmacro try (f) `(handler-case ,f (type-error () :te)))
(defun hash-step (c x) (setf (cnt-h c) (logand (+ (* (cnt-h c) 31) x) #xffffffff)))
(defun bump (c) (incf (cnt-b c)))
(let ((c (make-cnt)))
  (print (list (try (setf (cnt-b c) 256)) (try (setf (cnt-b c) -1)) (try (setf (cnt-h c) (expt 2 32)))
               (setf (cnt-h c) (1- (expt 2 32))) (setf (cnt-n c) (expt 2 40)) (try (setf (cnt-n c) (expt 2 62)))
               (try (setf (cnt-i c) 1.5)) (setf (cnt-i c) (expt 2 100)) (try (setf (cnt-m c) 10))
               (try (setf (cnt-s c) 40000)) (setf (cnt-s c) -32768) (try (setf (cnt-r c) 10)) (try (setf (cnt-r c) 0))
               (try (make-cnt :b "x")) (try (make-cnt :n 'a)) (cb-z (mk-cb 1)) (try (mk-cb 1 -1))
               (try (make-sub :b 2)) (sub-b (make-sub)) (try (setf (cnt-b (make-sub)) 2))
               (logand (cnt-h c) 255) (cnt-n c) (cnt-i c))))
(let ((c (make-cnt)))
  (dotimes (i 1000) (hash-step c i))
  (print (list (cnt-h c) (try (dotimes (i 300) (bump c))) (cnt-b c))))
(terpri)
