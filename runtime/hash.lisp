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
;;; The entry GETHASH last found in an EQUAL or EQUALP table (for a key that
;;; is not a fixnum, a character or a symbol):
;;; the key asked for, the index of the entry and its key in the table, so
;;; that (SETF GETHASH) of the same key right after (INCF (GETHASH K H)),
;;; (PUSH X (GETHASH K H)) stores without hashing it again.  The entry is
;;; still there when the key vector holds the same key at the index.
(defconstant +ht-last-key+ 13)
(defconstant +ht-last-index+ 14)
(defconstant +ht-last-tkey+ 15)

(defvar *hash-table-class* (find-class 'hash-table))

;; (no LOAD-TIME-VALUE: code compiled in the image then has a copy of it,
;; the test inline, as code walking JSON objects calls it for every value)
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
         (h (%make-instance *hash-table-class* (%make-simple-vector 16 nil))))
    (%ht-set h +ht-test+ (svref #(eq eql equal equalp) kind))
    (%ht-set h +ht-kind+ kind)
    (%ht-set h +ht-rehash-size+ rehash-size)
    (%ht-set h +ht-rehash-threshold+ rehash-threshold)
    (%ht-set h +ht-weakness+ weakness)
    (%ht-init-storage (%instance-slots h) cap)
    h))

;;; MAKE-HASH-TABLE with a test written as a constant and no option but
;;; :SIZE: the table made without parsing keywords and finding the test
;;; (a JSON reader makes one for every object).
(defun %make-hash-table-of-kind (kind size)
  (let ((h (%make-instance *hash-table-class* (%make-simple-vector 16 nil))))
    (%ht-set h +ht-test+ (svref #(eq eql equal equalp) kind))
    (%ht-set h +ht-kind+ kind)
    (%ht-set h +ht-rehash-size+ 1.5)
    (%ht-set h +ht-rehash-threshold+ 1)
    (%ht-set h +ht-weakness+ nil)
    (%ht-init-storage (%instance-slots h) (if (fixnump size) (if (%fx> size 8) size 8) (max 8 size)))
    h))

(define-compiler-macro make-hash-table (&whole form &rest keys)
  (unless (evenp (length keys)) (return-from make-hash-table form))
  (let* ((names (loop for (k) on keys by #'cddr collect k))
         (test (getf keys :test ''eql))
         (kind (and (consp test) (member (car test) '(quote function)) (consp (cdr test))
                    (position (second test) '(eq eql equal equalp)))))
    (if (and kind
             (every (lambda (k) (member k '(:test :size))) names)
             (= (length (remove-duplicates names)) (length names)))
        `(%make-hash-table-of-kind ,kind ,(getf keys :size 16))
        form)))

(defun %ht-init-storage (sv cap)
  ;; twice as many chains as entries, a power of two
  (let ((nb 16) (bits 4) (want (* cap 2)))
    (declare (fixnum nb bits))
    (do () ((or (>= nb want) (eql bits 28))) (setq nb (* nb 2) bits (+ bits 1)))
    (%set-svref sv +ht-count+ 0)
    (%set-svref sv +ht-fill+ 0)
    (%set-svref sv +ht-last-index+ -1)
    (%set-svref sv +ht-keys+ (%make-simple-vector cap nil))
    (%set-svref sv +ht-vals+ (%make-simple-vector cap nil))
    (%set-svref sv +ht-next+ (%make-limbs cap))
    (%set-svref sv +ht-hashes+ (%make-limbs cap))
    (%set-svref sv +ht-buckets+ (%make-limbs nb))
    (%set-svref sv +ht-shift+ bits)))

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

(defun %list-hash (x depth equalp)
  "The hash of the list X: its first DEPTH elements (each to DEPTH - 1) and
what ends it, combined in order by a loop (EQUALP: as %EQUALP-HASH, else as
%SXHASH)."
  (declare (fixnum depth))
  (if (%fx<= depth 0)
      17
      (let ((h 7) (l x) (k depth) (d (%fx- depth 1)))
        (declare (fixnum h k))
        (loop
          (unless (and (consp l) (%fx> k 0)) (return))
          (setq h (%mix h (let ((e (car l)))
                            (cond ((fixnump e) (%fxlogand e 536870911))
                                  ((symbolp e) (%symbol-hash e))
                                  ((and (not equalp) (%simple-string-p e)) (%string-hash e))
                                  (equalp (%equalp-hash e d))
                                  (t (%sxhash e d)))))
                l (cdr l) k (%fx- k 1)))
        (%mix h (cond ((null l) 3) ((consp l) 17) (equalp (%equalp-hash l d)) (t (%sxhash l d)))))))

(defun sxhash (x) (%sxhash x 4))
(defun %sxhash (x depth)
  ;; the tests that are a type test of the reference first
  (declare (fixnum depth))
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((symbolp x) (%symbol-hash x))
        ((characterp x) (%char-code x))
        ((consp x) (%list-hash x depth nil))
        ((%simple-string-p x) (%string-hash x))
        ((%double-float-p x) (%number-hash x))
        ((numberp x) (%number-hash x))
        ((stringp x) (%string-hash x))
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

(defun %equalp-vector-hash (x depth)
  ;; a vector of characters is EQUALP to a string: it hashes as one
  (let ((n (length x)))
    (if (dotimes (i n t) (unless (characterp (aref x i)) (return nil)))
        (let ((h 5))
          (dotimes (i n h) (setq h (%mix h (%char-code (char-upcase (aref x i)))))))
        (let ((h n))
          (if (<= depth 0) h
              (dotimes (i (min 4 n) h)
                (setq h (%mix h (%equalp-hash (aref x i) (- depth 1))))))))))

(defun %equalp-hash (x depth)
  (cond ((fixnump x) (%fxlogand x 536870911))
        ((symbolp x) (%symbol-hash x))
        ;; (the same hash as the general string case below)
        ((%simple-string-p x)
         (let ((h 5) (n (%string-length x)))
           (dotimes (i n h)
             (let ((c (%char-code (%schar x i))))
               (setq h (%mix h (cond ((%fx< c 97) c)
                                     ((%fx<= c 122) (%fx- c 32))
                                     ((%fx< c 128) c)
                                     (t (%char-code (char-upcase (%schar x i)))))))))))
        ;; (as VECTORP below, without the generic calls)
        ((%simple-vector-p x)
         (let* ((n (%svlength x)) (h n))
           (declare (fixnum n h))
           (if (or (%fx<= depth 0) (%fx= n 0) (characterp (%svref x 0)))
               (%equalp-vector-hash x depth)
               (dotimes (i (if (%fx< n 4) n 4) h)
                 (setq h (%mix h (let ((e (%svref x i)))
                                   (if (fixnump e) (%fxlogand e 536870911) (%equalp-hash e (%fx- depth 1))))))))))
        ((characterp x) (%char-code (char-upcase x)))
        ((%double-float-p x) (%equalp-double-hash x))
        ((%single-float-p x) (%equalp-double-hash (%single-to-double x)))
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
        ((consp x) (%list-hash x depth t))
        ((vectorp x) (%equalp-vector-hash x depth))
        ((hash-table-p x) 23)
        ((%instancep x) 29)
        (t (%sxhash x depth))))

(defmacro %ht-hash (h key)
  "The hash of KEY in the table H.  Fixnums and symbols hash the same way for
every test, characters for every test but EQUALP, without a call."
  (let ((k (gensym)) (x (gensym)))
    `(let ((,x ,key))
       (cond ((fixnump ,x) (%fxlogand ,x 536870911))
             ((symbolp ,x) (%symbol-hash ,x))
             (t (let ((,k (%ht ,h +ht-kind+)))
                  (cond ((and (characterp ,x) (%fx< ,k 3)) (%char-code ,x))   ; (EQUALP: of the upcased character)
                        ((%fx= ,k 0) (%eq-hash ,x))
                        ;; (an integer beyond the fixnums: as %NUMBER-HASH, without its calls)
                        ((and (%fx< ,k 3) (%bignump ,x)) (%bignum-hash ,x))
                        ((%fx= ,k 1) (%eql-hash ,x))
                        ((%fx= ,k 2) (%sxhash ,x 4))
                        (t (%equalp-hash ,x 4)))))))))

(defun %simple-string-ci= (a b)
  "EQUALP of two simple strings."
  (let ((n (%string-length a)))
    (and (eql n (%string-length b))
         (dotimes (i n t)
           (let ((c1 (%schar a i)) (c2 (%schar b i)))
             (unless (or (eq c1 c2) (%char-ci-eq c1 c2)) (return nil)))))))

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
                               ;; strings, the usual keys, are compared here
                               ((and (%simple-string-p k) (%simple-string-p key))
                                (%simple-string-ci= k key))
                               (t (equalp k key))))))
          (return j))
        (setq i (%lref next j))))))

(defmacro %ht-lookup (sv key hash)
  "Index of KEY (whose hash is HASH) or -1 in the table whose slots are SV.
A key that only an EQ key can match (any key of an EQ table, a symbol, a
fixnum or a non-number in an EQL table, a fixnum in an EQUAL table) is looked
for here, without a call."
  (let ((kind (gensym)))
    `(let ((,kind (%svref ,sv +ht-kind+)))
       (if (or (%fx= ,kind 0)
               (symbolp ,key)
               (if (fixnump ,key) (not (%fx= ,kind 3)) (and (%fx= ,kind 1) (not (numberp ,key)))))
           (%ht-eq-index ,sv ,key ,hash)
           (%ht-find-general ,sv ,key ,hash ,kind)))))

;;; A string key of an EQUAL table: hashed by the Wasm loop of strings, and
;;; a simple one looked up by the helper that compares simple strings (the
;;; keys of JSON objects, of symbol tables), without the dispatch on the
;;; key's type of %HT-HASH and %HT-FIND-GENERAL.
(defmacro %ht-string-key-p (sv key)
  `(and (eql (%svref ,sv +ht-kind+) 2) (stringp ,key)))
(defun %ht-string-lookup (sv key hash)
  "Index of the string KEY, of hash HASH (%STRING-HASH), or -1 in the EQUAL
table whose slots are SV.  A string with a fill pointer or displaced over a
simple string (a reader's buffer) is compared over its storage."
  (let ((i (if (%simple-string-p key)
               (%ht-string-index sv key hash)
               (let ((d (%mdarray-data key)))
                 (cond ((%simple-string-p d)
                        ;; (a fill pointer or a displacement into a simple string)
                        (%ht-region-index sv d hash (%mdarray-offset key) (length key)))
                       ((%simple-string-p (setq d (%array-storage key)))
                        (%ht-region-index sv d hash (%array-total-offset key) (length key)))
                       (t -2))))))
    (if (eql i -2) (%ht-find-general sv key hash 2) i)))

(defun %ht-find (h key hash)
  "Index of KEY or -1."
  (let ((sv (%instance-slots h)))
    (%ht-lookup sv key hash)))

(defun gethash (key h &optional default)
  ;; a fixnum, a character or a symbol: looked up by one helper
  (when (and (%immediate-or-symbol-p key) (%instancep h) (eq (%instance-layout h) (load-time-value *hash-table-class*)))
    (let* ((sv (%instance-slots h)) (i (%ht-fast-index sv key)))
      (cond ((%fx>= i 0) (return-from gethash (values (%svref (%svref sv +ht-vals+) i) t)))
            ((eql i -1) (return-from gethash (values default nil))))))
  (%gethash-general key h default))

(defmacro %gethash-index (key h)
  "The index of the entry of KEY (a variable) in the hash table H (a
variable; checked), or -1."
  `(progn
  (%check-ht ,h)
  (let* ((sv (%instance-slots ,h))
         (string-key (%ht-string-key-p sv ,key))
         (hash (if string-key (%string-hash ,key) (%ht-hash ,h ,key)))
         (i (if string-key (%ht-string-lookup sv ,key hash) (%ht-lookup sv ,key hash))))
    (when (and (%fx>= i 0)
               ;; (keys an EQUAL or EQUALP table hashes with a call)
               (not (or (fixnump ,key) (symbolp ,key) (characterp ,key) (%fx< (%svref sv +ht-kind+) 2))))
      (%set-svref sv +ht-last-key+ ,key)
      (%set-svref sv +ht-last-index+ i)
      (%set-svref sv +ht-last-tkey+ (%svref (%svref sv +ht-keys+) i)))
    i)))

(defun %gethash-general (key h default)
  "GETHASH for any key (without the helper of fixnums, characters and symbols)."
  (let ((i (%gethash-index key h)))
    (if (%fx< i 0)
        (values default nil)
        (values (%svref (%svref (%instance-slots h) +ht-vals+) i) t))))

;; a fixnum, a character or a symbol: looked up by one helper call, which
;; tests the table too (:HT-GET), so that the call site is small; any other
;; key calls %GETHASH-MARKED (and so does a key the helper cannot look up)
(define-compiler-macro gethash (key table &optional (default nil defaultp))
  (let ((k (gensym "KEY")) (h (gensym "H")) (d (gensym "DEFAULT")) (v (gensym "V")))
    `(let* ((,k ,key) (,h ,table) ,@(when defaultp `((,d ,default))))
       (let ((,v (if (%immediate-or-symbol-p ,k) (%ht-get ,h ,k (%gethash-marked ,k ,h)) (%gethash-marked ,k ,h))))
         (if (eq ,v (%ht-absent))
             (values ,(if defaultp d nil) nil)
             (values ,v t))))))

(defun %gethash-marked (key h)
  "The value of KEY in H, or +DELETED+ when it has none."
  (let ((i (%gethash-index key h)))
    (if (%fx< i 0) +deleted+ (%svref (%svref (%instance-slots h) +ht-vals+) i))))

;; a fixnum, a character or a symbol already in the table: the value is
;; stored by one helper call (:HT-PUT-OLD); anything else calls %HT-PUT-SLOW
(define-compiler-macro %ht-put (table key value)
  (let ((k (gensym "KEY")) (h (gensym "H")) (v (gensym "VALUE")))
    `(let* ((,h ,table) (,k ,key) (,v ,value))
       (if (%immediate-or-symbol-p ,k)
           (%ht-put-old ,h ,k ,v (%ht-put-slow ,h ,k ,v))
           (%ht-put-slow ,h ,k ,v)))))

(defun %gethash-hashed (key h hash)
  "GETHASH of KEY in H (an EQUAL table) whose hash HASH was computed already."
  (if (eql (%ht h +ht-count+) 0)
      (values nil nil)                  ; e.g. the external symbols of a package without any
      (let ((i (if (%simple-string-p key) (%ht-string-index (%instance-slots h) key hash) -2)))
        (when (eql i -2) (setq i (%ht-find-general (%instance-slots h) key hash 2)))
        (if (%fx< i 0) (values nil nil) (values (%svref (%ht h +ht-vals+) i) t)))))

(defun %puthash (key h &rest rest)
  ;; (setf (gethash key h [default]) value): value is the last argument
  (let ((value (car (last rest))))
    (%ht-put h key value)
    value))

(defun %ht-put-slow (h key value)
  "(SETF GETHASH) after :HT-PUT-OLD found no entry to store in: H is any
object."
  (%check-ht h)
  (let ((sv (%instance-slots h)))
    (cond ((and (%immediate-or-symbol-p key) (or (symbolp key) (%fx< (%svref sv +ht-kind+) 3)))
           ;; a key that :HT-PUT-OLD looked for: it has no entry
           (%ht-add sv key value (%ht-hash h key)))
          ((and (not (or (fixnump key) (symbolp key)))
                ;; the entry GETHASH has just found (never one of a fixnum or a symbol)
                (let ((li (%svref sv +ht-last-index+)))
                  (and (eq key (%svref sv +ht-last-key+)) (%fx>= li 0) (%fx< li (%svref sv +ht-fill+))
                       (eq (%svref (%svref sv +ht-keys+) li) (%svref sv +ht-last-tkey+))
                       (progn (%set-svref (%svref sv +ht-vals+) li value) t)))))
          (t (let* ((string-key (%ht-string-key-p sv key))
                    (hash (if string-key (%string-hash key) (%ht-hash h key)))
                    (i (if string-key (%ht-string-lookup sv key hash) (%ht-lookup sv key hash))))
               (if (%fx>= i 0)
                   (%set-svref (%svref sv +ht-vals+) i value)
                   (%ht-add sv key value hash)))))
    value))

(defun %ht-add (sv key value hash)
  "A new entry of KEY (of hash HASH) and VALUE in the table whose slot vector is SV."
  (declare (fixnum hash))
  (let ((n (%svref sv +ht-fill+)))
    (declare (fixnum n))
    (when (%fx>= n (%svlength (%svref sv +ht-keys+)))
      (%ht-grow sv)
      (setq n (%svref sv +ht-fill+)))
    (let* ((buckets (%svref sv +ht-buckets+))
           (b (%ht-bucket hash (the fixnum (%svref sv +ht-shift+)))))
      (%set-svref (%svref sv +ht-keys+) n key)
      (%set-svref (%svref sv +ht-vals+) n value)
      (%lset (%svref sv +ht-hashes+) n hash)
      (%lset (%svref sv +ht-next+) n (%lref buckets b))
      (%lset buckets b (%fx+ n 1))
      (%set-svref sv +ht-fill+ (%fx+ n 1))
      (%set-svref sv +ht-count+ (%fx+ (the fixnum (%svref sv +ht-count+)) 1)))))

(defun %ht-put (h key value) (%ht-put h key value))
(defun %ht-grow (sv)
  "Make room for an entry: the entries are moved to the front of the vectors
over the removed ones when they are less than half full, else the table
grows to twice the size."
  (let* ((keys (%svref sv +ht-keys+)) (count (%svref sv +ht-count+)) (cap (%svlength keys)))
    (if (%fx< count (%fxash-right cap 1))
        (%ht-compact sv)
        (%ht-rebuild sv (* 2 cap)))))

(defun %ht-rebuild (sv new-cap)
  (let* ((keys (%svref sv +ht-keys+)) (vals (%svref sv +ht-vals+)) (fill (%svref sv +ht-fill+))
         (count (%svref sv +ht-count+)) (old-hashes (%svref sv +ht-hashes+)) (del +deleted+))
    (declare (fixnum fill count))
    (%ht-init-storage sv new-cap)
    (let* ((nk (%svref sv +ht-keys+)) (nv (%svref sv +ht-vals+)) (nh (%svref sv +ht-hashes+))
           (nn (%svref sv +ht-next+)) (nb (%svref sv +ht-buckets+)) (bits (%svref sv +ht-shift+)) (j 0))
      (declare (fixnum j bits))
      (if (eql count fill)
          ;; no removed entries: the arrays copied at once, and the chains made
          (progn
            (%sv-copy nk 0 keys 0 fill)
            (%sv-copy nv 0 vals 0 fill)
            (%limbs-copy nh 0 old-hashes 0 fill)
            (dotimes (i fill)
              (let ((b (%ht-bucket (%lref nh i) bits)))
                (%lset nn i (%lref nb b))
                (%lset nb b (%fx+ i 1))))
            (setq j fill))
          ;; reinsert with the stored hashes (the keys are distinct)
          (dotimes (i fill)
            (let ((k (%svref keys i)))
              (unless (eq k del)
                (let* ((hash (%lref old-hashes i)) (b (%ht-bucket hash bits)))
                  (%set-svref nk j k)
                  (%set-svref nv j (%svref vals i))
                  (%lset nh j hash)
                  (%lset nn j (%lref nb b))
                  (setq j (%fx+ j 1))
                  (%lset nb b j))))))
      (%set-svref sv +ht-fill+ j)
      (%set-svref sv +ht-count+ j))))

(defun %ht-compact (sv)
  "Move the entries of H over its removed ones, in the same vectors (no
allocation), and chain them again."
  (let ((keys (%svref sv +ht-keys+)) (vals (%svref sv +ht-vals+)) (hashes (%svref sv +ht-hashes+))
        (next (%svref sv +ht-next+)) (buckets (%svref sv +ht-buckets+)) (bits (%svref sv +ht-shift+))
        (fill (%svref sv +ht-fill+)) (del +deleted+) (j 0))
    (declare (fixnum fill j bits))
    (dotimes (b (%llen buckets)) (%lset buckets b 0))
    (dotimes (i fill)
      (let ((k (%svref keys i)))
        (unless (eq k del)
          (let* ((hash (%lref hashes i)) (b (%ht-bucket hash bits)))
            (%set-svref keys j k)
            (%set-svref vals j (%svref vals i))
            (%lset hashes j hash)
            (%lset next j (%lref buckets b))
            (setq j (%fx+ j 1))
            (%lset buckets b j)))))
    (do ((i j (%fx+ i 1))) ((%fx>= i fill))
      (%set-svref keys i nil)
      (%set-svref vals i nil))
    (%set-svref sv +ht-fill+ j)
    (%set-svref sv +ht-count+ j)
    (%set-svref sv +ht-last-index+ -1)))

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
  (%ht-init-storage (%instance-slots h) (%svlength (%ht h +ht-keys+)))
  h)

(defmacro %ht-scan (keys fill del start)
  "The index of the first entry at START or after it in the key vector KEYS
filled up to FILL (DEL is +DELETED+), or -1."
  (let ((j (gensym "J")))
    `(do ((,j ,start (%fx+ ,j 1))) ((%fx>= ,j ,fill) -1)
       (unless (eq (%svref ,keys ,j) ,del) (return ,j)))))

(defmacro %with-ht-vectors ((h keys vals fill del) &body body)
  "Binds the key and value vectors of the hash table H (checked), the number
of entries filled in them and +DELETED+, which iterating over the entries in
place needs: the vectors are only replaced when an entry is added, which is
not allowed while iterating."
  `(let ((,keys (the simple-vector (%ht ,h +ht-keys+))) (,vals (the simple-vector (%ht ,h +ht-vals+)))
         (,fill (the fixnum (%ht ,h +ht-fill+))) (,del +deleted+))
     (declare (ignorable ,vals))
     ,@body))

(defun maphash (fn h)
  (%check-ht h)
  (let ((fn (%fn fn)))
    (%with-ht-vectors (h keys vals fill del)
      (dotimes (i fill nil)
        (let ((k (%svref keys i)))
          (unless (eq k del)
            (funcall (the function fn) k (%svref vals i))))))))

;; with a function written in place: a loop, so that the function is called
;; there (a lambda is inline) and an exit out of it (RETURN-FROM, GO) is a
;; branch, not an exception
(define-compiler-macro maphash (&whole form fn table)
  (if (not (%literal-function-form-p fn))
      form
      (let ((f (gensym "F")) (h (gensym "H")) (keys (gensym "KEYS")) (vals (gensym "VALS"))
            (fill (gensym "FILL")) (del (gensym "DEL")) (i (gensym "I")) (k (gensym "K")))
        `(let (,@(%function-binding f fn) (,h ,table))
           (%check-ht ,h)
           (%with-ht-vectors (,h ,keys ,vals ,fill ,del)
             (%do ((,i 0 (1+ ,i))) ((>= ,i ,fill) nil)
               (let ((,k (%svref ,keys ,i)))
                 (unless (eq ,k ,del) ,(%literal-call fn f `(,k (%svref ,vals ,i)))))))))))

(defun %ht-next-index (h i)
  "The index of the first entry of H at I or after it, or -1."
  (%with-ht-vectors (h keys vals fill del)
    (%ht-scan keys fill del (the fixnum i))))

(defun %ht-checked (h) (%check-ht h) h)
(defun %ht-key-at (h i) (%svref (%ht h +ht-keys+) i))
(defun %ht-value-at (h i) (%svref (%ht h +ht-vals+) i))

(defmacro with-hash-table-iterator ((name table) &body body)
  (let ((h (gensym "H")) (i (gensym "I")) (j (gensym "J")) (keys (gensym "KEYS")) (vals (gensym "VALS"))
        (fill (gensym "FILL")) (del (gensym "DEL")))
    `(let ((,h ,table) (,i 0))
       (unless (hash-table-p ,h) (%type-error ,h 'hash-table))
       (%with-ht-vectors (,h ,keys ,vals ,fill ,del)
         (macrolet ((,name () '(let ((,j (%ht-scan ,keys ,fill ,del ,i)))
                                 (if (%fx< ,j 0)
                                     nil
                                     (progn (setq ,i (%fx+ ,j 1))
                                            (values t (%svref ,keys ,j) (%svref ,vals ,j)))))))
           ,@body)))))
