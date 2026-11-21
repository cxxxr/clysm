;;; EQ and EQL of an element of a vector of integers (compared as the i32 read
;;; from the storage) with constants, variables and other elements, at the
;;; default policy and at (speed 3) (safety 0); values beyond the fixnums
;;; of an (unsigned-byte 32) vector are EQL to an equal integer, and EQ only
;;; where both are the same fixnum.

(defun count-eql (v x)
  (declare (type (simple-array fixnum (*)) v) (fixnum x))
  (let ((n 0))
    (dotimes (i (length v) n)
      (when (eql (aref v i) x) (incf n))
      (when (eq (aref v i) 1) (incf n 10)))))

(defun count-u32 (v x)
  (declare (type (simple-array (unsigned-byte 32) (*)) v))
  (let ((n 0))
    (dotimes (i (length v) n)
      (when (eql (aref v i) x) (incf n))
      (when (eql (aref v i) 4000000000) (incf n 100))
      (when (eql (aref v i) (aref v 0)) (incf n 1000)))))

(defun fast-pairs (v)
  (declare (optimize (speed 3) (safety 0)) (type (simple-array fixnum (*)) v))
  (let ((n 0))
    (declare (fixnum n))
    (dotimes (i (length v) n)
      (dotimes (j (length v))
        (when (and (eql (aref v i) (aref v j)) (eq (aref v i) 0)) (incf n))))))

(defun fast-s32 (v)
  (declare (optimize (speed 3) (safety 0)) (type (simple-array (signed-byte 32) (*)) v))
  (list (eql (aref v 0) (aref v 1)) (eql (aref v 0) -2000000000) (eql (aref v 2) 5)
        (eq (aref v 2) 5) (eql (aref v 0) 2000000000)))

(let ((v (make-array 6 :element-type 'fixnum :initial-contents '(1 2 3 1 -5 0)))
      (u (make-array 5 :element-type '(unsigned-byte 32)
                       :initial-contents '(4000000000 7 4000000000 0 4294967295)))
      (s (make-array 3 :element-type '(signed-byte 32)
                       :initial-contents '(-2000000000 -2000000000 5))))
  (print (list (count-eql v 1) (count-eql v -5) (count-eql v 9)))
  (print (list (count-u32 u 7) (count-u32 u 4294967295) (count-u32 u (expt 2 32))))
  (print (fast-pairs v))
  (print (fast-s32 s)))

;;; A search whose loop recurses on few of its moves (the branch with the
;;; call is laid out of the way of the loop: the same counts)
(defun place (cols n row)
  (declare (simple-vector cols) (fixnum n row))
  (if (= row n)
      1
      (let ((count 0))
        (dotimes (c n count)
          (when (loop for r below row
                      never (let ((d (svref cols r)))
                              (or (eql d c) (eql (abs (- d c)) (- row r)))))
            (setf (svref cols row) c)
            (incf count (place cols n (1+ row))))))))
(print (loop for n from 1 to 7 collect (place (make-array 8 :initial-element 0) n 0)))
