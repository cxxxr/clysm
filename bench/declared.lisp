;;; Clysm benchmarks -- programs written the way one writes Lisp for speed in
;;; SBCL: (speed 3) (safety 0), full type declarations (fixnum, (unsigned-byte
;;; 32), double-float, specialised simple arrays, simple strings), typed
;;; structure slots, FTYPE proclamations, INLINE helpers and DYNAMIC-EXTENT,
;;; but nothing particular to SBCL, so the same source runs in Clysm.
;;; Benchmarks Game style programs, Gabriel-style programs with declarations
;;; and ordinary optimised code.  Run with tools/bench.sh bench/declared.lisp.
;;;
;;; A FIXNUM declaration is only made where the value fits in 30 bits (the
;;; smallest fixnum among the implementations compared, Clysm's); larger
;;; integers are declared (UNSIGNED-BYTE 32) as portable code does.

(declaim (optimize (speed 3) (safety 0) (debug 0)))

(defmacro defbench (name (&optional (reps 1)) &body body)
  `(progn
     (defun ,name () ,@body)
     (run-bench ',name ,reps #',name)))

(defun run-bench (name reps fn)
  (funcall fn)                          ; warm up
  (let ((start (get-internal-real-time)) (r nil))
    (dotimes (i reps) (setq r (funcall fn)))
    (let ((ms (round (* 1000 (- (get-internal-real-time) start))
                     internal-time-units-per-second)))
      (format t "~(~A~) ~D ~A~%" name ms
              (let ((s (prin1-to-string r))) (if (> (length s) 40) (subseq s 0 40) s))))))

(deftype index () '(integer 0 #.(ash 1 28)))

;;; ------------------------------------------------------------------ n-body

(defstruct (body (:constructor make-body (x y z vx vy vz mass)))
  (x 0d0 :type double-float) (y 0d0 :type double-float) (z 0d0 :type double-float)
  (vx 0d0 :type double-float) (vy 0d0 :type double-float) (vz 0d0 :type double-float)
  (mass 0d0 :type double-float))

(defconstant +solar-mass+ (* 4d0 pi pi))
(defconstant +days-per-year+ 365.24d0)

(defun make-system ()
  (flet ((planet (x y z vx vy vz m)
           (make-body x y z (* vx +days-per-year+) (* vy +days-per-year+)
                      (* vz +days-per-year+) (* m +solar-mass+))))
    (let ((bodies
            (vector (make-body 0d0 0d0 0d0 0d0 0d0 0d0 +solar-mass+)
                    (planet 4.84143144246472090d0 -1.16032004402742839d0 -1.03622044471123109d-1
                            1.66007664274403694d-3 7.69901118419740425d-3 -6.90460016972063023d-5
                            9.54791938424326609d-4)
                    (planet 8.34336671824457987d0 4.12479856412430479d0 -4.03523417114321381d-1
                            -2.76742510726862411d-3 4.99852801234917238d-3 2.30417297573763929d-5
                            2.85885980666130812d-4)
                    (planet 1.28943695621391310d1 -1.51111514016986312d1 -2.23307578892655734d-1
                            2.96460137564761618d-3 2.37847173959480950d-3 -2.96589568540237556d-5
                            4.36624404335156298d-5)
                    (planet 1.53796971148509165d1 -2.59193146099879641d1 1.79258772950371181d-1
                            2.68067772490389322d-3 1.62824170038242295d-3 -9.51592254519715870d-5
                            5.15138902046611451d-5))))
      ;; offset the sun's momentum
      (let ((px 0d0) (py 0d0) (pz 0d0) (sun (svref bodies 0)))
        (declare (double-float px py pz))
        (loop for b across bodies
              do (incf px (* (body-vx b) (body-mass b)))
                 (incf py (* (body-vy b) (body-mass b)))
                 (incf pz (* (body-vz b) (body-mass b))))
        (setf (body-vx sun) (/ (- px) +solar-mass+)
              (body-vy sun) (/ (- py) +solar-mass+)
              (body-vz sun) (/ (- pz) +solar-mass+)))
      bodies)))

(defun advance (bodies dt)
  (declare (simple-vector bodies) (double-float dt))
  (let ((n (length bodies)))
    (dotimes (i n)
      (let ((a (svref bodies i)))
        (declare (body a))
        (loop for j of-type fixnum from (1+ i) below n
              do (let* ((b (svref bodies j))
                        (dx (- (body-x a) (body-x b)))
                        (dy (- (body-y a) (body-y b)))
                        (dz (- (body-z a) (body-z b)))
                        (d2 (+ (* dx dx) (* dy dy) (* dz dz)))
                        (mag (/ dt (* d2 (sqrt d2))))
                        (ma (* (body-mass a) mag))
                        (mb (* (body-mass b) mag)))
                   (declare (body b) (double-float dx dy dz d2 mag ma mb))
                   (decf (body-vx a) (* dx mb)) (decf (body-vy a) (* dy mb)) (decf (body-vz a) (* dz mb))
                   (incf (body-vx b) (* dx ma)) (incf (body-vy b) (* dy ma)) (incf (body-vz b) (* dz ma))))))
    (loop for b across bodies
          do (let ((b b))
               (declare (body b))
               (incf (body-x b) (* dt (body-vx b)))
               (incf (body-y b) (* dt (body-vy b)))
               (incf (body-z b) (* dt (body-vz b)))))))

(defun energy (bodies)
  (declare (simple-vector bodies))
  (let ((e 0d0) (n (length bodies)))
    (declare (double-float e))
    (dotimes (i n e)
      (let ((a (svref bodies i)))
        (declare (body a))
        (incf e (* 0.5d0 (body-mass a)
                   (+ (* (body-vx a) (body-vx a)) (* (body-vy a) (body-vy a)) (* (body-vz a) (body-vz a)))))
        (loop for j of-type fixnum from (1+ i) below n
              do (let* ((b (svref bodies j))
                        (dx (- (body-x a) (body-x b)))
                        (dy (- (body-y a) (body-y b)))
                        (dz (- (body-z a) (body-z b))))
                   (declare (body b) (double-float dx dy dz))
                   (decf e (/ (* (body-mass a) (body-mass b))
                              (sqrt (+ (* dx dx) (* dy dy) (* dz dz)))))))))))

(defbench d-nbody (1)
  (let ((bodies (make-system)))
    (dotimes (i 5000000) (advance bodies 0.01d0))
    (round (* 1d9 (energy bodies)))))

;;; ----------------------------------------------------------- spectral-norm

(declaim (inline eval-a))
(defun eval-a (i j)
  (declare (fixnum i j))
  (/ 1d0 (the fixnum (+ (ash (the fixnum (* (+ i j) (+ i j 1))) -1) i 1))))

(defun eval-a-times-u (u au)
  (declare (type (simple-array double-float (*)) u au))
  (let ((n (length u)))
    (dotimes (i n)
      (let ((s 0d0))
        (declare (double-float s))
        (dotimes (j n) (incf s (* (eval-a i j) (aref u j))))
        (setf (aref au i) s)))))

(defun eval-at-times-u (u au)
  (declare (type (simple-array double-float (*)) u au))
  (let ((n (length u)))
    (dotimes (i n)
      (let ((s 0d0))
        (declare (double-float s))
        (dotimes (j n) (incf s (* (eval-a j i) (aref u j))))
        (setf (aref au i) s)))))

(defun eval-ata-times-u (u out tmp)
  (declare (type (simple-array double-float (*)) u out tmp))
  (eval-a-times-u u tmp)
  (eval-at-times-u tmp out))

(defun spectral-norm (n)
  (declare (type index n))
  (let ((u (make-array n :element-type 'double-float :initial-element 1d0))
        (v (make-array n :element-type 'double-float :initial-element 0d0))
        (tmp (make-array n :element-type 'double-float :initial-element 0d0))
        (vbv 0d0) (vv 0d0))
    (declare (double-float vbv vv))
    (dotimes (i 10)
      (eval-ata-times-u u v tmp)
      (eval-ata-times-u v u tmp))
    (dotimes (i n)
      (incf vbv (* (aref u i) (aref v i)))
      (incf vv (* (aref v i) (aref v i))))
    (sqrt (/ vbv vv))))

(defbench d-spectral-norm (1) (round (* 1d9 (spectral-norm 2000))))

;;; -------------------------------------------------------------- mandelbrot

(defun mandelbrot (size)
  (declare (type index size))
  (let* ((bytes-per-row (ceiling size 8))
         (out (make-array (* size bytes-per-row) :element-type '(unsigned-byte 8)))
         (inv (/ 2d0 size)))
    (declare (double-float inv))
    (dotimes (y size)
      (let ((ci (- (* y inv) 1d0)) (bits 0) (pos (* y bytes-per-row)))
        (declare (double-float ci) (type (unsigned-byte 8) bits) (type index pos))
        (dotimes (x size)
          (let ((cr (- (* x inv) 1.5d0)) (zr 0d0) (zi 0d0) (tr 0d0) (ti 0d0) (escaped nil))
            (declare (double-float cr zr zi tr ti))
            (dotimes (k 50)
              (setf zi (+ (* 2d0 zr zi) ci)
                    zr (+ (- tr ti) cr)
                    tr (* zr zr)
                    ti (* zi zi))
              (when (> (+ tr ti) 4d0) (setf escaped t) (return)))
            (setf bits (logior (logand (ash bits 1) #xff) (if escaped 0 1)))
            (when (= (logand x 7) 7)
              (setf (aref out pos) bits bits 0) (incf pos))))
        (let ((rem (logand size 7)))
          (unless (zerop rem)
            (setf (aref out pos) (logand (ash bits (- 8 rem)) #xff))))))
    (let ((sum 0))
      (declare (fixnum sum))
      (dotimes (i (length out) sum)
        (setf sum (logand (+ (* sum 31) (aref out i)) #xffffff))))))

(defbench d-mandelbrot (1) (mandelbrot 2000))

;;; ---------------------------------------------------------- fannkuch-redux

(defun fannkuch-redux (n)
  (declare (type (integer 3 12) n))
  (let ((perm (make-array n :element-type 'fixnum :initial-element 0))
        (perm1 (make-array n :element-type 'fixnum :initial-element 0))
        (count (make-array n :element-type 'fixnum :initial-element 0))
        (max-flips 0) (checksum 0) (perm-count 0) (r n))
    (declare (dynamic-extent perm perm1 count)
             (fixnum max-flips checksum perm-count r))
    (dotimes (i n) (setf (aref perm1 i) i))
    (loop
      (loop while (/= r 1) do (setf (aref count (1- r)) r) (decf r))
      (replace perm perm1)
      (let ((flips 0) (k (aref perm 0)))
        (declare (fixnum flips k))
        (loop until (= k 0)
              do (loop for i of-type fixnum from 0
                       for j of-type fixnum downfrom k
                       while (< i j)
                       do (rotatef (aref perm i) (aref perm j)))
                 (incf flips)
                 (setf k (aref perm 0)))
        (when (> flips max-flips) (setf max-flips flips))
        (if (evenp perm-count) (incf checksum flips) (decf checksum flips)))
      (loop
        (when (= r n)
          (return-from fannkuch-redux (+ (* checksum 100) max-flips)))
        (let ((perm0 (aref perm1 0)))
          (loop for i of-type fixnum from 0 below r
                do (setf (aref perm1 i) (aref perm1 (1+ i))))
          (setf (aref perm1 r) perm0))
        (decf (aref count r))
        (when (> (aref count r) 0) (return))
        (incf r))
      (incf perm-count))))

(defbench d-fannkuch (1) (fannkuch-redux 10))

;;; ------------------------------------------------------------ binary-trees

(defstruct (node (:constructor make-node (left right)))
  (left nil :type (or null node))
  (right nil :type (or null node)))

(declaim (ftype (function (fixnum) node) bottom-up-tree)
         (ftype (function (node) fixnum) item-check))
(defun bottom-up-tree (depth)
  (declare (fixnum depth))
  (if (= depth 0)
      (make-node nil nil)
      (make-node (bottom-up-tree (1- depth)) (bottom-up-tree (1- depth)))))

(defun item-check (tree)
  (declare (node tree))
  (let ((l (node-left tree)))
    (if l
        (the fixnum (+ 1 (item-check l) (item-check (node-right tree))))
        1)))

(defun binary-trees (max-depth)
  (declare (fixnum max-depth))
  (let ((total (item-check (bottom-up-tree (1+ max-depth))))
        (long-lived (bottom-up-tree max-depth)))
    (declare (fixnum total))
    (loop for depth of-type fixnum from 4 to max-depth by 2
          do (let ((iterations (ash 1 (+ (- max-depth depth) 4))) (check 0))
               (declare (fixnum iterations check))
               (dotimes (i iterations)
                 (incf check (item-check (bottom-up-tree depth))))
               (incf total check)))
    (+ total (item-check long-lived))))

(defbench d-binary-trees (1) (binary-trees 17))

;;; ------------------------------------------------------------------- fasta

(defconstant +im+ 139968)
(defconstant +ia+ 3877)
(defconstant +ic+ 29573)
(declaim (type (integer 0 139967) *seed*))
(defvar *seed* 42)

(declaim (inline next-random))
(defun next-random (max)
  (declare (double-float max))
  (let ((s (mod (+ (the (unsigned-byte 32) (* *seed* +ia+)) +ic+) +im+)))
    (setf *seed* s)
    (/ (* max s) #.(float +im+ 1d0))))

(defparameter *alu*
  (concatenate 'simple-string
               "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG"
               "GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA"
               "CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT"
               "ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA"
               "GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG"
               "AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC"
               "AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"))

(defun make-cumulative (pairs)
  (let* ((n (length pairs))
         (chars (make-string n))
         (probs (make-array n :element-type 'double-float))
         (acc 0d0))
    (declare (double-float acc))
    (loop for (c . p) in pairs for i of-type fixnum from 0
          do (incf acc (the double-float p))
             (setf (schar chars i) c (aref probs i) acc))
    (cons chars probs)))

(defparameter *iub*
  (make-cumulative '((#\a . 0.27d0) (#\c . 0.12d0) (#\g . 0.12d0) (#\t . 0.27d0)
                     (#\B . 0.02d0) (#\D . 0.02d0) (#\H . 0.02d0) (#\K . 0.02d0)
                     (#\M . 0.02d0) (#\N . 0.02d0) (#\R . 0.02d0) (#\S . 0.02d0)
                     (#\V . 0.02d0) (#\W . 0.02d0) (#\Y . 0.02d0))))
(defparameter *homo-sapiens*
  (make-cumulative '((#\a . 0.3029549426680d0) (#\c . 0.1979883004921d0)
                     (#\g . 0.1975473066391d0) (#\t . 0.3015094502008d0))))

(defun repeat-fasta (src out start n)
  (declare (simple-string src out) (type index start n))
  (let ((len (length src)) (k 0))
    (declare (type index len k))
    (dotimes (i n)
      (setf (schar out (+ start i)) (schar src k))
      (incf k)
      (when (= k len) (setf k 0)))))

(defun random-fasta (table out start n)
  (declare (cons table) (simple-string out) (type index start n))
  (let ((chars (car table)) (probs (cdr table)))
    (declare (simple-string chars) (type (simple-array double-float (*)) probs))
    (let ((last (1- (length probs))))
      (dotimes (i n)
        (let ((r (next-random 1d0)) (j 0))
          (declare (double-float r) (type index j))
          (loop while (and (< j last) (< (aref probs j) r)) do (incf j))
          (setf (schar out (+ start i)) (schar chars j)))))))

(defun fasta (n)
  (declare (type index n))
  (setf *seed* 42)
  (let ((out (make-string (* 10 n) :initial-element #\space)))
    (repeat-fasta *alu* out 0 (* 2 n))
    (random-fasta *iub* out (* 2 n) (* 3 n))
    (random-fasta *homo-sapiens* out (* 5 n) (* 5 n))
    out))

(defun string-checksum (s)
  (declare (simple-string s))
  (let ((h 0))
    (declare (type (unsigned-byte 32) h))
    (dotimes (i (length s) h)
      (setf h (logand (+ (* h 31) (char-code (schar s i))) #xffffff)))))

(defbench d-fasta (1) (string-checksum (fasta 1000000)))

;;; ------------------------------------------------------------- k-nucleotide

(defun knucleotide-sequence (n)
  (declare (type index n))
  (setf *seed* 7)
  (let ((out (make-string n)))
    (random-fasta *homo-sapiens* out 0 n)
    (dotimes (i n out)
      (setf (schar out i) (char-upcase (schar out i))))))

(defun frequencies (seq k)
  (declare (simple-string seq) (type index k))
  (let ((table (make-hash-table :test 'equal :size 1024)))
    (loop for i of-type index from 0 to (- (length seq) k)
          do (incf (the fixnum (gethash (subseq seq i (+ i k)) table 0))))
    table))

(defun k-nucleotide (n)
  (let ((seq (knucleotide-sequence n)) (result 0))
    (declare (fixnum result))
    (dolist (k '(1 2))
      (incf result (hash-table-count (frequencies seq k))))
    (dolist (s '("GGT" "GGTA" "GGTATT" "GGTATTTTAATT" "GGTATTTTAATTTATAGT") result)
      (let ((table (frequencies seq (length s))))
        (setf result (+ (* result 7) (the fixnum (gethash s table 0)) (hash-table-count table)))
        (setf result (logand result #xffffff))))))

(defbench d-k-nucleotide (1) (k-nucleotide 250000))

;;; ------------------------------------------------------- reverse-complement

(defparameter *complement*
  (let ((table (make-string 128 :initial-element #\?)))
    (loop for (a b) on '(#\A #\T #\C #\G #\G #\C #\T #\A #\U #\A #\M #\K #\R #\Y
                         #\W #\W #\S #\S #\Y #\R #\K #\M #\V #\B #\H #\D #\D #\H
                         #\B #\V #\N #\N) by #'cddr
          do (setf (schar table (char-code a)) b
                   (schar table (char-code (char-downcase a))) b))
    table))

(defun reverse-complement (s)
  (declare (simple-string s))
  (let ((table *complement*) (i 0) (j (1- (length s))))
    (declare (simple-string table) (fixnum i j))
    (loop while (< i j)
          do (let ((a (schar s i)) (b (schar s j)))
               (setf (schar s i) (schar table (char-code b))
                     (schar s j) (schar table (char-code a))))
             (incf i) (decf j))
    (when (= i j)
      (setf (schar s i) (schar table (char-code (schar s i)))))
    s))

(defparameter *revcomp-input* (fasta 100000))

(defbench d-revcomp (1)
  (let ((s (copy-seq (the simple-string *revcomp-input*))) (h 0))
    (declare (fixnum h))
    (dotimes (r 60)
      (reverse-complement s)
      (setf h (logand (+ (* h 3) (string-checksum s)) #xffffff)))
    h))

;;; ---------------------------------------------------------------- pidigits

(defun pidigits (n)
  (declare (type index n))
  ;; Gibbons' unbounded spigot, the version of the Benchmarks Game
  (let ((q 1) (r 0) (tt 1) (k 0) (sum 0) (tail 0))
    (declare (integer q r tt) (fixnum k sum tail))
    (flet ((extract (j) (floor (+ (* q j) r) tt)))
      (loop with i of-type fixnum = 0
            while (< i n)
            do (incf k)
               (let ((k2 (1+ (* 2 k))))
                 (declare (fixnum k2))
                 (psetf q (* q k)
                        r (* (+ (* 2 q) r) k2)
                        tt (* tt k2)))
               (unless (> q r)
                 (let ((d (extract 3)))
                   (when (= d (extract 4))
                     (setf sum (+ sum (the fixnum d))
                           tail (logand (+ (* tail 10) (the fixnum d)) #xffffff))
                     (incf i)
                     (psetf q (* q 10)
                            r (* 10 (- r (* d tt)))))))))
    (+ (* sum 100000000) tail)))

(defbench d-pidigits (1) (pidigits 3500))

;;; --------------------------------------------------------------------- tak

(declaim (ftype (function (fixnum fixnum fixnum) fixnum) tak))
(defun tak (x y z)
  (declare (fixnum x y z))
  (if (not (< y x))
      z
      (tak (tak (1- x) y z) (tak (1- y) z x) (tak (1- z) x y))))

(defbench d-tak (1) (let ((s 0)) (declare (fixnum s)) (dotimes (i 300 s) (incf s (tak 22 16 8)))))

;;; --------------------------------------------------------------------- fft

(defun fft (re im)
  (declare (type (simple-array double-float (*)) re im))
  (let* ((n (length re)) (j 0))
    (declare (type index n j))
    ;; bit-reversal permutation
    (dotimes (i n)
      (when (< i j)
        (rotatef (aref re i) (aref re j))
        (rotatef (aref im i) (aref im j)))
      (let ((m (ash n -1)))
        (declare (type index m))
        (loop while (and (>= m 1) (>= j m)) do (decf j m) (setf m (ash m -1)))
        (incf j m)))
    ;; butterflies, the twiddle factors by recurrence
    (let ((len 2))
      (declare (type index len))
      (loop while (<= len n)
            do (let* ((half (ash len -1))
                      (theta (/ (* -2d0 pi) len))
                      (wpr (cos theta)) (wpi (sin theta)))
                 (declare (type index half) (double-float theta wpr wpi))
                 (let ((wr 1d0) (wi 0d0))
                   (declare (double-float wr wi))
                   (dotimes (k half)
                     (loop for a of-type index from k below n by len
                           do (let* ((b (+ a half))
                                     (xr (- (* wr (aref re b)) (* wi (aref im b))))
                                     (xi (+ (* wr (aref im b)) (* wi (aref re b)))))
                                (declare (type index b) (double-float xr xi))
                                (setf (aref re b) (- (aref re a) xr)
                                      (aref im b) (- (aref im a) xi))
                                (incf (aref re a) xr)
                                (incf (aref im a) xi)))
                     (psetf wr (- (* wr wpr) (* wi wpi))
                            wi (+ (* wr wpi) (* wi wpr))))))
               (setf len (* len 2))))
    re))

(defbench d-fft (1)
  (let* ((n 65536)
         (re (make-array n :element-type 'double-float :initial-element 0d0))
         (im (make-array n :element-type 'double-float :initial-element 0d0))
         (s 0d0))
    (declare (double-float s))
    (dotimes (rep 12)
      (dotimes (i n)
        (setf (aref re i) (sin (* 0.01d0 (the fixnum (+ i rep))))
              (aref im i) 0d0))
      (fft re im)
      (dotimes (i 64) (incf s (+ (abs (aref re i)) (abs (aref im i))))))
    (round s)))

;;; ---------------------------------------------------------------- triangle

;;; Peg solitaire on a triangle of 15 holes (Gabriel's TRIANGLE): count the
;;; move sequences that leave one peg.  The moves are computed from the
;;; board's geometry.

(defun triangle-moves ()
  (let ((pos (make-array '(5 5) :initial-element nil)) (k 0) (moves '()))
    (dotimes (r 5) (dotimes (c (1+ r)) (setf (aref pos r c) k) (incf k)))
    (flet ((at (r c) (and (<= 0 r 4) (<= 0 c r) (aref pos r c))))
      (dotimes (r 5)
        (dotimes (c (1+ r))
          (loop for (dr dc) in '((0 1) (0 -1) (1 0) (-1 0) (1 1) (-1 -1))
                do (let ((over (at (+ r dr) (+ c dc)))
                         (to (at (+ r dr dr) (+ c dc dc))))
                     (when (and over to)
                       (push (list (aref pos r c) over to) moves)))))))
    (let* ((moves (nreverse moves)) (n (length moves))
           (from (make-array n :element-type 'fixnum))
           (over (make-array n :element-type 'fixnum))
           (to (make-array n :element-type 'fixnum)))
      (loop for (f o tt) in moves for i from 0
            do (setf (aref from i) f (aref over i) o (aref to i) tt))
      (vector from over to))))

(defparameter *triangle-moves* (triangle-moves))

(defun triangle-count (board from over to pegs)
  (declare (simple-vector board) (type (simple-array fixnum (*)) from over to)
           (fixnum pegs))
  (if (= pegs 1)
      1
      (let ((count 0))
        (declare (fixnum count))
        (dotimes (m (length from) count)
          (let ((f (aref from m)) (o (aref over m)) (tt (aref to m)))
            (when (and (eql (svref board f) 1) (eql (svref board o) 1)
                       (eql (svref board tt) 0))
              (setf (svref board f) 0 (svref board o) 0 (svref board tt) 1)
              (incf count (the fixnum (triangle-count board from over to (1- pegs))))
              (setf (svref board f) 1 (svref board o) 1 (svref board tt) 0)))))))

(defbench d-triangle (20)
  (let ((board (make-array 15 :initial-element 1))
        (moves *triangle-moves*))
    (setf (svref board 4) 0)
    (triangle-count board (svref moves 0) (svref moves 1) (svref moves 2) 14)))

;;; ------------------------------------------------------------------ matmul

(defun matmul (a b c)
  (declare (type (simple-array double-float (* *)) a b c))
  (let ((n (array-dimension a 0)) (m (array-dimension b 1)) (l (array-dimension a 1)))
    (declare (type index n m l))
    (dotimes (i n c)
      (dotimes (j m) (setf (aref c i j) 0d0))
      (dotimes (k l)
        (let ((aik (aref a i k)))
          (declare (double-float aik))
          (dotimes (j m)
            (incf (aref c i j) (* aik (aref b k j)))))))))

(defbench d-matmul (1)
  (let* ((n 300)
         (a (make-array (list n n) :element-type 'double-float))
         (b (make-array (list n n) :element-type 'double-float))
         (c (make-array (list n n) :element-type 'double-float :initial-element 0d0))
         (tr 0d0))
    (declare (double-float tr))
    (dotimes (i n)
      (dotimes (j n)
        (setf (aref a i j) (/ (float (mod (+ (* i 7) j) 13) 1d0) 13d0)
              (aref b i j) (/ (float (mod (- (* i 3) j) 11) 1d0) 11d0))))
    (dotimes (r 6) (matmul a b c) (dotimes (i n) (incf tr (aref c i i))))
    (round (* 1000 tr))))

;;; ------------------------------------------------------------------- sieve

(defun bit-sieve (n)
  (declare (type index n))
  (let ((composite (make-array (1+ n) :element-type 'bit :initial-element 0))
        (count 0))
    (declare (fixnum count))
    (loop for i of-type index from 2 to n
          do (when (zerop (sbit composite i))
               (incf count)
               (when (<= (* i i) n)
                 (loop for j of-type index from (* i i) to n by i
                       do (setf (sbit composite j) 1)))))
    count))

(defbench d-sieve (1) (let ((s 0)) (declare (fixnum s)) (dotimes (i 5 s) (incf s (bit-sieve 10000000)))))

;;; --------------------------------------------------------------- particles

(defstruct (particle (:constructor make-particle (x y vx vy)))
  (x 0d0 :type double-float) (y 0d0 :type double-float)
  (vx 0d0 :type double-float) (vy 0d0 :type double-float)
  (bounces 0 :type fixnum))

(declaim (inline reflect))
(defun reflect (p lo hi)
  (declare (particle p) (double-float lo hi))
  (cond ((< (particle-x p) lo)
         (setf (particle-x p) (- (* 2 lo) (particle-x p)) (particle-vx p) (- (particle-vx p)))
         (incf (particle-bounces p)))
        ((> (particle-x p) hi)
         (setf (particle-x p) (- (* 2 hi) (particle-x p)) (particle-vx p) (- (particle-vx p)))
         (incf (particle-bounces p))))
  (cond ((< (particle-y p) lo)
         (setf (particle-y p) (- (* 2 lo) (particle-y p)) (particle-vy p) (* -0.9d0 (particle-vy p)))
         (incf (particle-bounces p)))
        ((> (particle-y p) hi)
         (setf (particle-y p) (- (* 2 hi) (particle-y p)) (particle-vy p) (- (particle-vy p)))
         (incf (particle-bounces p)))))

(defun step-particles (ps dt)
  (declare (simple-vector ps) (double-float dt))
  (let ((cx 0d0) (cy 0d0) (n (length ps)))
    (declare (double-float cx cy))
    (loop for p across ps
          do (let ((p p)) (declare (particle p))
               (incf cx (particle-x p)) (incf cy (particle-y p))))
    (setf cx (/ cx n) cy (/ cy n))
    (loop for p across ps
          do (let ((p p)) (declare (particle p))
               (let ((ax (* 0.1d0 (- cx (particle-x p))))
                     (ay (- (* 0.1d0 (- cy (particle-y p))) 9.81d0)))
                 (declare (double-float ax ay))
                 (incf (particle-vx p) (* ax dt))
                 (incf (particle-vy p) (* ay dt))
                 (incf (particle-x p) (* (particle-vx p) dt))
                 (incf (particle-y p) (* (particle-vy p) dt))
                 (reflect p 0d0 100d0))))))

(defbench d-particles (1)
  (let ((ps (make-array 10000)) (seed 1))
    (declare (type (integer 0 2147483647) seed))
    (flet ((rnd ()
             (setf seed (mod (+ (* seed 48271) 11) 2147483647))
             (/ (float (mod seed 100000) 1d0) 1000d0)))
      (dotimes (i (length ps))
        (setf (svref ps i) (make-particle (rnd) (rnd) (- (rnd) 50d0) (- (rnd) 50d0)))))
    (dotimes (s 4000) (step-particles ps 0.01d0))
    (let ((b 0) (e 0d0))
      (declare (fixnum b) (double-float e))
      (loop for p across ps
            do (let ((p p)) (declare (particle p))
                 (incf b (particle-bounces p))
                 (incf e (+ (abs (particle-x p)) (abs (particle-y p))))))
      (+ (* b 1000) (mod (round e) 1000)))))

;;; ---------------------------------------------------------------- tokenize

(defun make-text (n)
  (declare (type index n))
  (let ((words #("lambda" "defun" "let" "x1" "car" "cdr" "loop" "foo-bar" "quux"))
        (s (make-string n :initial-element #\space)) (i 0) (k 1))
    (declare (simple-vector words) (type index i) (type (integer 0 1000000) k))
    (loop while (< i (- n 20))
          do (setf k (mod (+ (* k 1103) 12345) 1000000))
             (case (mod k 5)
               ((0 1) (let ((w (svref words (mod k 9))))
                        (declare (simple-string w))
                        (replace s w :start1 i) (incf i (length w))))
               (2 (let ((d (princ-to-string (mod k 99991))))
                    (replace s d :start1 i) (incf i (length d))))
               (3 (setf (schar s i) (schar "()'.,;" (mod k 6))) (incf i))
               (t (setf (schar s i) #\Newline) (incf i)))
             (setf (schar s i) #\space) (incf i))
    s))

(defparameter *text* (make-text 1000000))

(declaim (inline word-char-p))
(defun word-char-p (c)
  (declare (character c))
  (or (char<= #\a c #\z) (char<= #\A c #\Z) (char= c #\-) (char<= #\0 c #\9)))

(defun tokenize-count (s)
  (declare (simple-string s))
  (let ((n (length s)) (i 0) (words 0) (numbers 0) (sum 0) (punct 0) (lines 0) (h 0))
    (declare (type index n i) (fixnum words numbers punct lines) (type (unsigned-byte 32) sum h))
    (loop while (< i n)
          do (let ((c (schar s i)))
               (cond ((digit-char-p c)
                      (let ((v 0))
                        (declare (fixnum v))
                        (loop while (and (< i n) (digit-char-p (schar s i)))
                              do (setf v (+ (* v 10) (the fixnum (digit-char-p (schar s i))))) (incf i))
                        (incf numbers)
                        (setf sum (logand (+ sum v) #xffffff))))
                     ((word-char-p c)
                      (let ((start i))
                        (declare (type index start))
                        (loop while (and (< i n) (word-char-p (schar s i))) do (incf i))
                        (incf words)
                        (setf h (logand (+ (* h 33) (- i start) (char-code (schar s start))) #xffffff))))
                     ((char= c #\Newline) (incf lines) (incf i))
                     ((char= c #\space) (incf i))
                     (t (incf punct) (incf i)))))
    (logand (+ (* words 7) (* numbers 11) sum (* punct 13) (* lines 17) h) #xffffffff)))

(defbench d-tokenize (1)
  (let ((r 0)) (declare (type (unsigned-byte 32) r))
    (dotimes (i 40 r) (setf r (logand (+ r (tokenize-count *text*)) #xffffff)))))

;;; ---------------------------------------------------------- fixnum hashing

(defun fixnum-hash (n)
  (declare (type index n))
  (let ((table (make-hash-table :test 'eql)) (hits 0) (sum 0) (key 0))
    (declare (fixnum hits sum) (type (integer 0 1000003) key))
    (flet ((next (k step)
             (declare (type (integer 0 1000003) k step))
             (let ((k (+ k step))) (if (>= k 1000003) (- k 1000003) k))))
      (declare (inline next))
      (dotimes (i n)
        (setf (gethash key table) i key (next key 7919)))
      (setf key 0)
      (dotimes (i (* 2 n))
        (let ((v (gethash key table)))
          (when v (incf hits) (setf sum (logand (+ sum (the fixnum v)) #xffffff))))
        (setf key (next key 104729)))
      (setf key 0)
      (dotimes (i n)
        (when (evenp i) (remhash key table))
        (setf key (next key 7919))))
    (+ (* hits 1000) (hash-table-count table) sum)))

(defbench d-fixnum-hash (8) (fixnum-hash 300000))

;;; --------------------------------------------------------------- quicksort

(defun quicksort (v lo hi)
  (declare (type (simple-array fixnum (*)) v) (fixnum lo hi))
  (loop
    (when (< (- hi lo) 16)
      ;; insertion sort
      (loop for i of-type fixnum from (1+ lo) to hi
            do (let ((x (aref v i)) (j (1- i)))
                 (declare (fixnum x j))
                 (loop while (and (>= j lo) (> (aref v j) x))
                       do (setf (aref v (1+ j)) (aref v j)) (decf j))
                 (setf (aref v (1+ j)) x)))
      (return v))
    (let* ((mid (ash (+ lo hi) -1))
           (a (aref v lo)) (b (aref v mid)) (c (aref v hi))
           (pivot (if (< a b) (if (< b c) b (if (< a c) c a)) (if (< a c) a (if (< b c) c b))))
           (i lo) (j hi))
      (declare (fixnum mid a b c pivot i j))
      (loop
        (loop while (< (aref v i) pivot) do (incf i))
        (loop while (> (aref v j) pivot) do (decf j))
        (when (<= i j) (rotatef (aref v i) (aref v j)) (incf i) (decf j))
        (when (> i j) (return)))
      ;; recurse on the smaller part
      (if (< (- j lo) (- hi i))
          (progn (quicksort v lo j) (setf lo i))
          (progn (quicksort v i hi) (setf hi j))))))

(defbench d-quicksort (1)
  (let* ((n 1000000) (v (make-array n :element-type 'fixnum)) (x 1) (h 0))
    (declare (type (integer 0 2147483647) x) (fixnum h))
    (dotimes (r 3)
      (dotimes (i n)
        (setf x (mod (+ (* x 48271) r) 2147483647))
        (setf (aref v i) (mod x 100000000)))
      (quicksort v 0 (1- n))
      (dotimes (i (1- n))
        (when (> (aref v i) (aref v (1+ i))) (return-from d-quicksort :unsorted)))
      (loop for i of-type fixnum from 0 below n by 1000
            do (setf h (logand (+ (* h 17) (aref v i)) #xffffff))))
    h))

;;; ------------------------------------------------------------------- crc32

(defparameter *crc-table*
  (let ((table (make-array 256 :element-type '(unsigned-byte 32))))
    (dotimes (n 256 table)
      (let ((c n))
        (declare (type (unsigned-byte 32) c))
        (dotimes (k 8)
          (setf c (if (logbitp 0 c) (logxor #xedb88320 (ash c -1)) (ash c -1))))
        (setf (aref table n) c)))))

(defun crc32 (data)
  (declare (type (simple-array (unsigned-byte 8) (*)) data))
  (let ((table *crc-table*) (crc #xffffffff))
    (declare (type (simple-array (unsigned-byte 32) (256)) table)
             (type (unsigned-byte 32) crc))
    (dotimes (i (length data))
      (setf crc (logxor (aref table (logand (logxor crc (aref data i)) #xff))
                        (ash crc -8))))
    (logxor crc #xffffffff)))

(defbench d-crc32 (1)
  (let ((data (make-array 1000000 :element-type '(unsigned-byte 8))) (r 0))
    (declare (type (unsigned-byte 32) r))
    (dotimes (i (length data)) (setf (aref data i) (logand (* i 131) #xff)))
    (dotimes (k 80 r)
      (setf (aref data k) k)
      (setf r (logxor r (crc32 data))))))
