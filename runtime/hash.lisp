;;;; hash.lisp — hash tables
;;;;
;;;; Open hashing with index chains; entries are kept in insertion order
;;;; (as SBCL does), so MAPHASH iterates in insertion order.
;;;;
;;;; The entry I has its key and value in two simple vectors, and its hash and
;;;; the next entry of its chain in two vectors of 32-bit integers (the kind
;;;; bignums keep their digits in: they are made zeroed and the collector does
;;;; not scan them).  BUCKETS, of the same kind, has a power of two of chain
;;;; heads.  A chain link is the entry's index + 1, and 0 ends the chain.

;;; slots of the hash-table instance
(defconstant +ht-test+ 0)
(defconstant +ht-count+ 1)
(defconstant +ht-keys+ 2)
(defconstant +ht-vals+ 3)
(defconstant +ht-next+ 4)
(defconstant +ht-hashes+ 5)
(defconstant +ht-buckets+ 6)
(defconstant +ht-fill+ 7)
(defconstant +ht-rehash-size+ 8)
(defconstant +ht-rehash-threshold+ 9)
(defconstant +ht-kind+ 10)
(defconstant +ht-weakness+ 11)
(defconstant +ht-shift+ 12)           ; log2 of the number of buckets

(defvar *hash-table-class* (find-class 'hash-table))

(defun hash-table-p (x) (and (%instancep x) (eq (%instance-layout x) *hash-table-class*)))

(defmacro %ht (h i) `(%instance-ref ,h ,i))
(defmacro %ht-set (h i v) `(%set-instance-ref ,h ,i ,v))

;; the marker of a removed entry: an uninterned symbol, so that no key can be EQ to it
(defvar +deleted+ (make-symbol "DELETED-ENTRY"))

(defun %ht-kind-of (test)
  (cond ((or (eq test 'eq) (eq test #'eq)) 0)
        ((or (eq test 'eql) (eq test #'eql)) 1)
        ((or (eq test 'equal) (eq test #'equal)) 2)
        ((or (eq test 'equalp) (eq test #'equalp)) 3)
        (t (error "Unsupported hash table test ~S" test))))

(defun make-hash-table (&key (test 'eql) (size 16) (rehash-size 1.5) (rehash-threshold 1) weakness
                             synchronized)
  (declare (ignore synchronized))
  (let* ((kind (%ht-kind-of test))
         (cap (max 8 size))
         (h (%make-instance *hash-table-class* (%make-simple-vector 13 nil))))
    (%ht-set h +ht-test+ (svref #(eq eql equal equalp) kind))
    (%ht-set h +ht-kind+ kind)
    (%ht-set h +ht-rehash-size+ rehash-size)
    (%ht-set h +ht-rehash-threshold+ rehash-threshold)
    (%ht-set h +ht-weakness+ weakness)
    (%ht-init-storage h cap)
    h))

(defun %ht-init-storage (h cap)
  ;; twice as many chains as entries, a power of two
  (let ((nb 16) (bits 4) (want (* cap 2)))
    (do () ((or (>= nb want) (eql bits 28))) (setq nb (* nb 2) bits (+ bits 1)))
    (%ht-set h +ht-count+ 0)
    (%ht-set h +ht-fill+ 0)
    (%ht-set h +ht-keys+ (%make-simple-vector cap nil))
    (%ht-set h +ht-vals+ (%make-simple-vector cap nil))
    (%ht-set h +ht-next+ (%make-limbs cap))
    (%ht-set h +ht-hashes+ (%make-limbs cap))
    (%ht-set h +ht-buckets+ (%make-limbs nb))
    (%ht-set h +ht-shift+ bits)))

(defmacro %ht-bucket (hash bits)
  "The index of the chain of HASH among 2^BITS chains: the low BITS bits of
the hash plus odd multiples of its two higher pieces of BITS bits.
Consecutive hashes (fixnum keys) get consecutive chains, one each, so a table
of them is read in order; hashes that differ only above the low bits
(multiples of a power of two) are spread by the higher pieces; and since the
pieces are multiplied and added, not combined with XOR, keys with a repeated
bit pattern do not cancel."
  (let ((x (gensym)) (y (gensym)) (b (gensym)))
    `(let* ((,x ,hash) (,b ,bits) (,y (%fxash-right ,x ,b)))
       (%fxlogand (%fx+ (%fx+ ,x (%fx* ,y 40503)) (%fx* (%fxash-right ,y ,b) 31153))
                  (%fx- (%fxash-left 1 ,b) 1)))))

(defmacro %check-ht (h) `(unless (hash-table-p ,h) (%type-error ,h 'hash-table)))
(defun hash-table-count (h) (%check-ht h) (%ht h +ht-count+))
(defun hash-table-test (h) (%check-ht h) (%ht h +ht-test+))
(defun hash-table-size (h) (%check-ht h) (%svlength (%ht h +ht-keys+)))
(defun hash-table-rehash-size (h) (%check-ht h) (%ht h +ht-rehash-size+))
(defun hash-table-rehash-threshold (h) (%check-ht h) (%ht h +ht-rehash-threshold+))
(defun hash-table-weakness (h) (%check-ht h) (%ht h +ht-weakness+))

;;; hashing
(defun %eq-hash (x)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((characterp x) (%char-code x))
        ((symbolp x) (%symbol-hash x))
        (t (%identity-hash x))))

(defun %scramble (x)
  "A hash of 28 bits in which every bit of the fixnum X counts: two rounds of
multiplying by 2^28 / the golden ratio.  The result ends with a product (not
with a shift and XOR, which the folding of %HT-BUCKET could undo)."
  (let* ((y (%fxlogand (%fx* x 165902235) #xFFFFFFF))
         (z (%fxlogxor y (%fxash-right y 14))))
    (%fxlogand (%fx* z 165902235) #xFFFFFFF)))

(defun %number-hash (x)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ;; (over the base 2^28 digits, in Wasm: no limbs are made for an integer within 64 bits)
        ((%bignump x) (%bignum-hash x))
        ((%ratiop x) (%fxlogxor (%number-hash (%ratio-num x)) (%number-hash (%ratio-den x))))
        ((%double-float-p x)
         ;; the bits that differ between doubles of the same magnitude are
         ;; the high bits of the mantissa: scramble them over the hash
         (%scramble (%fxlogxor (%double-mant-hi x)
                               (%scramble (%fxlogxor (%double-mant-lo x) (%double-exponent x))))))
        ((%single-float-p x) (%number-hash (%single-to-double x)))
        ((%complexp x) (%fxlogxor (%number-hash (%complex-real x)) (%number-hash (%complex-imag x))))
        (t 0)))

(defun %eql-hash (x)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((symbolp x) (%symbol-hash x))
        ((numberp x) (%number-hash x))
        (t (%eq-hash x))))

;;; The hash so far H combined with the hash V of a part, in 28 bits: both
;;; are multiplied (by the FNV prime), so that small integers as parts do not
;;; cancel each other.
(defun %mix (h v)
  (%fxlogand (%fx* (%fxlogxor (%fxlogand (%fx* h 16777619) #xFFFFFFF) v) 16777619) #xFFFFFFF))

(defun sxhash (x) (%sxhash x 4))
(defun %sxhash (x depth)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((symbolp x) (%symbol-hash x))
        ((%simple-string-p x) (%string-hash x))
        ((stringp x) (%string-hash x))
        ((numberp x) (%number-hash x))
        ((characterp x) (%char-code x))
        ((consp x) (if (<= depth 0) 17
                       ;; the hash of the tail is multiplied again at each cons
                       (%mix (%sxhash (cdr x) (- depth 1)) (%sxhash (car x) (- depth 1)))))
        ((bit-vector-p x) (let ((h 7)) (dotimes (i (length x) h) (setq h (%mix h (bit x i))))))
        ((pathnamep x) (%sxhash (namestring x) depth))
        (t (%eq-hash x))))

;;; Numbers that are = hash alike: an integral value as the integer, another
;;; value as the double it is equal to (a ratio or a float that no double
;;; equals is equal to nothing else).
(defun %equalp-double-hash (d)
  (cond ((and (%d< -5d8 d) (%d< d 5d8))
         (let ((f (%double-to-fixnum d)))
           (if (%d= (%fixnum-to-double f) d) (%fxlogand f 536870911) (%number-hash d))))
        ((or (%double-nan-p d) (%float-infinite-p d)) 13)
        ((= d (truncate d)) (%number-hash (truncate d)))
        (t (%number-hash d))))

(defun %equalp-number-hash (x)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((%double-float-p x) (%equalp-double-hash x))
        ((%single-float-p x) (%equalp-double-hash (%single-to-double x)))
        ((%bignump x) (%number-hash x))
        ((%ratiop x)
         (if (and (fixnump (%ratio-num x)) (fixnump (%ratio-den x)))
             (%equalp-double-hash (%to-double x))
             13))
        ((and (%complexp x) (zerop (imagpart x))) (%equalp-number-hash (realpart x)))
        (t 13)))

(defun %equalp-hash (x depth)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((symbolp x) (%symbol-hash x))
        ((characterp x) (%char-code (char-upcase x)))
        ((numberp x) (%equalp-number-hash x))
        ((stringp x)
         (let ((h 5) (d (%array-storage x)) (off (%array-total-offset x)))
           (declare (fixnum h off))
           (dotimes (i (length x) h)
             (let ((c (%char-code (%schar d (+ i off)))))
               (declare (fixnum c))
               (setq h (%mix h (cond ((< c 97) c)
                                     ((<= c 122) (- c 32))
                                     ((< c 128) c)
                                     (t (%char-code (char-upcase (%schar d (+ i off))))))))))))
        ((consp x) (if (<= depth 0) 17
                       (%mix (%equalp-hash (cdr x) (- depth 1)) (%equalp-hash (car x) (- depth 1)))))
        ((vectorp x) (let ((h (length x)))
                       (if (<= depth 0) h
                           (dotimes (i (min 4 (length x)) h)
                             (setq h (%mix h (%equalp-hash (aref x i) (- depth 1))))))))
        ((hash-table-p x) 23)
        ((%instancep x) 29)
        (t (%sxhash x depth))))

(defmacro %ht-hash (h key)
  "The hash of KEY in the table H.  Fixnums and symbols hash the same way for
every test, without a call."
  (let ((k (gensym)) (x (gensym)))
    `(let ((,x ,key))
       (cond ((fixnump ,x) (%fxlogand ,x 536870911))
             ((symbolp ,x) (%symbol-hash ,x))
             (t (let ((,k (%ht ,h +ht-kind+)))
                  (cond ((%fx= ,k 0) (%eq-hash ,x))
                        ((%fx= ,k 1) (%eql-hash ,x))
                        ((%fx= ,k 2) (%sxhash ,x 4))
                        (t (%equalp-hash ,x 4)))))))))

(defun %ht-find-general (sv key hash kind)
  "Index of KEY or -1 in the table whose slots are SV, comparing with its test."
  (let* ((buckets (%svref sv +ht-buckets+))
         (keys (%svref sv +ht-keys+))
         (next (%svref sv +ht-next+))
         (hashes (%svref sv +ht-hashes+))
         (i (%lref buckets (%ht-bucket hash (%svref sv +ht-shift+)))))
    (do () ((%fx= i 0) -1)
      (let ((j (%fx- i 1)))
        (when (and (%fx= (%lref hashes j) hash)
                   (let ((k (%svref keys j)))
                     (or (eq k key)
                         (cond ((%fx= kind 0) nil)
                               ((%fx= kind 1) (eql k key))
                               ((%fx= kind 2)
                                ;; strings, the usual keys, are compared here
                                (if (and (%simple-string-p k) (%simple-string-p key))
                                    (%simple-string= k key)
                                    (equal k key)))
                               (t (equalp k key))))))
          (return j))
        (setq i (%lref next j))))))

(defmacro %ht-lookup (sv key hash)
  "Index of KEY (whose hash is HASH) or -1 in the table whose slots are SV.
A key that only an EQ key can match (any key of an EQ table, a symbol, a
fixnum or a non-number in an EQL table, a fixnum in an EQUAL table) is looked
for here, without a call."
  (let ((kind (gensym)) (buckets (gensym)) (keys (gensym)) (next (gensym)) (i (gensym)) (j (gensym)))
    `(let ((,kind (%svref ,sv +ht-kind+)))
       (if (or (%fx= ,kind 0)
               (symbolp ,key)
               (if (fixnump ,key) (not (%fx= ,kind 3)) (and (%fx= ,kind 1) (not (numberp ,key)))))
           (let* ((,buckets (%svref ,sv +ht-buckets+))
                  (,keys (%svref ,sv +ht-keys+))
                  (,next (%svref ,sv +ht-next+))
                  (,i (%lref ,buckets (%ht-bucket ,hash (%svref ,sv +ht-shift+)))))
             (do () ((%fx= ,i 0) -1)
               (let ((,j (%fx- ,i 1)))
                 (when (eq (%svref ,keys ,j) ,key) (return ,j))
                 (setq ,i (%lref ,next ,j)))))
           (%ht-find-general ,sv ,key ,hash ,kind)))))

(defun %ht-find (h key hash)
  "Index of KEY or -1."
  (let ((sv (%instance-slots h)))
    (%ht-lookup sv key hash)))

(defun gethash (key h &optional default)
  (%check-ht h)
  (let* ((sv (%instance-slots h))
         (hash (%ht-hash h key))
         (i (%ht-lookup sv key hash)))
    (if (%fx< i 0)
        (values default nil)
        (values (%svref (%svref sv +ht-vals+) i) t))))

(defun %gethash-hashed (key h hash)
  "GETHASH of KEY in H (an EQUAL table) whose hash HASH was computed already."
  (if (eql (%ht h +ht-count+) 0)
      (values nil nil)                  ; e.g. the external symbols of a package without any
      (let ((i (%ht-find-general (%instance-slots h) key hash 2)))
        (if (%fx< i 0) (values nil nil) (values (%svref (%ht h +ht-vals+) i) t)))))

(defun %puthash (key h &rest rest)
  ;; (setf (gethash key h [default]) value): value is the last argument
  (let ((value (car (last rest))))
    (%ht-put h key value)
    value))

(defun %ht-put (h key value)
  (%check-ht h)
  (let* ((sv (%instance-slots h))
         (hash (%ht-hash h key))
         (i (%ht-lookup sv key hash)))
    (if (%fx>= i 0)
        (%set-svref (%svref sv +ht-vals+) i value)
        (progn
          (when (%fx>= (%ht h +ht-fill+) (%svlength (%ht h +ht-keys+)))
            (%ht-grow h))
          (let* ((n (%ht h +ht-fill+))
                 (buckets (%ht h +ht-buckets+))
                 (b (%ht-bucket hash (%ht h +ht-shift+))))
            (%set-svref (%ht h +ht-keys+) n key)
            (%set-svref (%ht h +ht-vals+) n value)
            (%lset (%ht h +ht-hashes+) n hash)
            (%lset (%ht h +ht-next+) n (%lref buckets b))
            (%lset buckets b (%fx+ n 1))
            (%ht-set h +ht-fill+ (%fx+ n 1))
            (%ht-set h +ht-count+ (%fx+ (%ht h +ht-count+) 1)))))
    value))

(defun %ht-grow (h)
  (let* ((keys (%ht h +ht-keys+)) (vals (%ht h +ht-vals+)) (fill (%ht h +ht-fill+))
         (old-hashes (%ht h +ht-hashes+))
         (count (%ht h +ht-count+))
         (new-cap (if (< count (floor (%svlength keys) 2)) (%svlength keys) (* 2 (%svlength keys)))))
    (%ht-init-storage h new-cap)
    ;; reinsert with the stored hashes (the keys are distinct)
    (let* ((nk (%ht h +ht-keys+)) (nv (%ht h +ht-vals+)) (nh (%ht h +ht-hashes+))
           (nn (%ht h +ht-next+)) (nb (%ht h +ht-buckets+)) (bits (%ht h +ht-shift+)) (j 0))
      (declare (fixnum j))
      (dotimes (i fill)
        (let ((k (%svref keys i)))
          (unless (eq k +deleted+)
            (let* ((hash (%lref old-hashes i)) (b (%ht-bucket hash bits)))
              (%set-svref nk j k)
              (%set-svref nv j (%svref vals i))
              (%lset nh j hash)
              (%lset nn j (%lref nb b))
              (setq j (%fx+ j 1))
              (%lset nb b j)))))
      (%ht-set h +ht-fill+ j)
      (%ht-set h +ht-count+ j))))

(defun remhash (key h)
  (%check-ht h)
  (let* ((hash (%ht-hash h key))
         (buckets (%ht h +ht-buckets+))
         (b (%ht-bucket hash (%ht h +ht-shift+)))
         (next (%ht h +ht-next+))
         (hashes (%ht h +ht-hashes+))
         (keys (%ht h +ht-keys+))
         (kind (%ht h +ht-kind+))
         (i (%lref buckets b))
         (prev 0))
    (do () ((%fx= i 0) nil)
      (let ((j (%fx- i 1)))
        (when (and (%fx= (%lref hashes j) hash)
                   (let ((k (%svref keys j)))
                     (or (eq k key)
                         (cond ((%fx= kind 0) nil)
                               ((%fx= kind 1) (eql k key))
                               ((%fx= kind 2) (equal k key))
                               (t (equalp k key))))))
          (if (%fx= prev 0)
              (%lset buckets b (%lref next j))
              (%lset next (%fx- prev 1) (%lref next j)))
          (%set-svref keys j +deleted+)
          (%set-svref (%ht h +ht-vals+) j nil)
          (%ht-set h +ht-count+ (%fx- (%ht h +ht-count+) 1))
          (return t))
        (setq prev i i (%lref next j))))))

(defun clrhash (h)
  (%check-ht h)
  (%ht-init-storage h (%svlength (%ht h +ht-keys+)))
  h)

(defun maphash (fn h)
  (%check-ht h)
  (let ((keys (%ht h +ht-keys+)) (vals (%ht h +ht-vals+)) (fill (%ht h +ht-fill+)))
    (dotimes (i fill nil)
      (let ((k (%svref keys i)))
        (unless (eq k +deleted+)
          (funcall fn k (%svref vals i)))))))

(defun %ht-next-index (h i)
  "The index of the first entry of H at I or after it, or -1."
  (let ((keys (%ht h +ht-keys+)) (fill (%ht h +ht-fill+)))
    (declare (fixnum i fill))
    (do ((j i (+ j 1))) ((>= j fill) -1)
      (declare (fixnum j))
      (unless (eq (%svref keys j) +deleted+) (return j)))))

(defun %ht-checked (h) (%check-ht h) h)
(defun %ht-key-at (h i) (%svref (%ht h +ht-keys+) i))
(defun %ht-value-at (h i) (%svref (%ht h +ht-vals+) i))

(defmacro with-hash-table-iterator ((name table) &body body)
  (let ((h (gensym)) (i (gensym)) (j (gensym)))
    `(let ((,h ,table) (,i 0))
       (unless (hash-table-p ,h) (%type-error ,h 'hash-table))
       (macrolet ((,name () '(let ((,j (%ht-next-index ,h ,i)))
                               (if (< ,j 0)
                                   nil
                                   (progn (setq ,i (+ ,j 1))
                                          (values t (%ht-key-at ,h ,j) (%ht-value-at ,h ,j)))))))
         ,@body))))
