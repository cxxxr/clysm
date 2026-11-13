;;;; numbers.lisp — the numeric tower
;;;;
;;;; fixnum (i31), bignum (sign + base 2^28 magnitude digits), ratio,
;;;; single-float (f32), double-float (f64), complex.

(defconstant most-positive-fixnum 536870911)
(defconstant most-negative-fixnum -536870912)
(defconstant +digit-bits+ 28)
(defconstant +digit-base+ 268435456)
(defconstant +digit-mask+ 268435455)

;;; ------------------------------------------------------------
;;; type predicates
;;; ------------------------------------------------------------

(defun bignump (x) (%bignump x))
(defun integerp (x) (or (fixnump x) (%bignump x)))
(defun rationalp (x) (or (fixnump x) (%bignump x) (%ratiop x)))
(defun floatp (x) (or (%double-float-p x) (%single-float-p x)))
(defun realp (x) (or (fixnump x) (%bignump x) (%ratiop x) (%double-float-p x) (%single-float-p x)))
(defun numberp (x) (or (realp x) (%complexp x)))
(defun complexp (x) (%complexp x))
(defun ratiop (x) (%ratiop x))
(defun single-float-p (x) (%single-float-p x))
(defun double-float-p (x) (%double-float-p x))

(defun %check-number (x) (if (numberp x) x (%type-error x 'number)))
(defun %check-real (x) (if (realp x) x (%type-error x 'real)))
(defun %check-integer (x) (if (integerp x) x (%type-error x 'integer)))

;;; ------------------------------------------------------------
;;; magnitudes: limbs arrays of base 2^28 digits, little endian
;;; ------------------------------------------------------------
;;;
;;; The kernels are Wasm helpers (src/prims.lisp): %MAKE-INTEGER (sign and
;;; magnitude to a fixnum, an integer within 64 bits or a bignum), %INT-MAG,
;;; %INT-SIGN, %MAG-CMP, %INT-ADD-BIG, %INT-SUB-BIG, %INT-MUL-BIG,
;;; %INT-LOGOP-BIG, %MAG-SHL, %MAG-SHR, %MAG-BITLEN, %MAG-POPCOUNT, %MAG-CTZ.
;;; Integers within 64 bits never reach the functions below from the
;;; arithmetic operators: the generic helpers compute them in i64.

(defun %mag-length (m)
  ;; length without leading zero digits
  (let ((n (%llen m)))
    (do () ((or (%fx= n 0) (not (%fx= (%lref m (%fx- n 1)) 0))) n)
      (setq n (%fx- n 1)))))

(defun %mag-trim (m)
  (let ((n (%mag-length m)))
    (if (%fx= n (%llen m)) m (%limbs-prefix m n))))

(defun %fixnum-mag (n) (%int-mag n))

(defun %mag-add (a b) (%mag-add-fast a b))

(defun %mag-sub (a b) (%mag-sub-fast a b))

(defun %mag-mul (a b) (%mag-mul-fast a b))

(defun %mag-divmod (u v)
  "Returns (values quotient-mag remainder-mag)."
  (let ((n (%mag-length v)) (m0 (%mag-length u)))
    (cond ((%fx= n 0) (error 'division-by-zero :operation 'truncate :operands nil))
          ((%fx< m0 n) (values (%make-limbs 0) u))
          ((%fx= n 1)
           (multiple-value-bind (q r) (%mag-divmod-small u (%lref v 0))
             (let ((rm (%make-limbs 1))) (%lset rm 0 r) (values q rm))))
          (t (%mag-divmod-knuth u v)))))

;;; ------------------------------------------------------------
;;; integer arithmetic
;;; ------------------------------------------------------------

(defun %int-add (a b) (or (%int64-op a b 0) (%int-add-big a b)))
(defun %int-sub (a b) (or (%int64-op a b 1) (%int-sub-big a b)))

(defun %int-negate (a)
  (if (fixnump a)
      (if (%fx= a most-negative-fixnum) 536870912 (%fx- 0 a))
      (%make-integer (%fx- 1 (%bignum-sign a)) (%bignum-mag a))))

(defun %int-mul (a b) (or (%int64-op a b 2) (%int-mul-big a b)))

(defun %int-truncate (a b)
  "Returns (values quotient remainder) of integers."
  (let ((q (%int64-op a b 6)))
    (when q (return-from %int-truncate (values q (%int64-op a b 7)))))
  (when (eql b 0) (error 'division-by-zero :operation 'truncate :operands (list a b)))
  (let ((sa (%int-sign a)) (sb (%int-sign b)) (ma (%int-mag a)) (mb (%int-mag b)))
    (if (%fx= (%mag-length mb) 1)
        ;; a divisor of one digit: no remainder magnitude
        (multiple-value-bind (q r) (%mag-divmod-small ma (%lref mb 0))
          (values (%make-integer (%fxlogxor sa sb) q) (if (%fx= sa 1) (%fx- 0 r) r)))
        (multiple-value-bind (q r) (%mag-divmod ma mb)
          (values (%make-integer (%fxlogxor sa sb) q) (%make-integer sa r))))))

(defun %int-cmp (a b)
  (or (%int64-op a b 8) (%int-cmp-big a b)))

(defun %int-zerop (x) (eql x 0))
(defun %int-minusp (x) (if (fixnump x) (%fx< x 0) (%fx= (%bignum-sign x) 1)))

(defun %ash2 (n count) (%int-ash (%check-integer n) (%check-integer count)))
(defun %int-ash (x count)
  ;; (an integer within 64 bits whose result is one too is shifted by the helper)
  (cond ((eql x 0) 0)
        ((or (not (fixnump count)) (%fx= count most-negative-fixnum))
         (if (%int-minusp count)
             (if (%int-minusp x) -1 0)
             (error "ASH: the shift count ~S is too large" count)))
        ((%fx> count 0) (%make-integer (%int-sign x) (%mag-shl (%int-mag x) count)))
        ((%fx= count 0) x)
        (t (let* ((n (%fx- 0 count)) (m (%int-mag x)) (q (%make-integer 0 (%mag-shr m n))))
             (if (%int-minusp x)
                 ;; floor: -(q + 1) when a 1 bit is shifted out
                 (if (%fx< (%mag-ctz m) n) (- -1 q) (- q))
                 q)))))

(defun %logand2 (a b) (%int-logop-big (%check-integer a) (%check-integer b) 0))
(defun %logior2 (a b) (%int-logop-big (%check-integer a) (%check-integer b) 1))
(defun %logxor2 (a b) (%int-logop-big (%check-integer a) (%check-integer b) 2))

;;; ------------------------------------------------------------
;;; generic dispatch
;;; ------------------------------------------------------------

(defun %num-rank (x)
  (cond ((fixnump x) 0) ((%bignump x) 0) ((%ratiop x) 1)
        ((%single-float-p x) 2) ((%double-float-p x) 3) ((%complexp x) 4)
        (t (%type-error x 'number))))

(defun %to-double (x)
  (cond ((%double-float-p x) x)
        ((fixnump x) (%fixnum-to-double x))
        ((%single-float-p x) (%single-to-double x))
        ((%bignump x)
         (if (%int-minusp x)
             (%dneg (%exact-rational-to-double (- x) 1))
             (%exact-rational-to-double x 1)))
        ((%ratiop x) (%ratio-to-double x))
        (t (%type-error x 'real))))

(defun %ratio-to-double (x)
  (let ((n (%ratio-num x)) (d (%ratio-den x)))
    (if (%int-minusp n)
        (%dneg (%exact-rational-to-double (- n) d))
        (%exact-rational-to-double n d))))

(defun %exact-rational-to-double (n d)
  "The double nearest to N/D (N, D positive integers), rounding to even."
  (let ((shift (min (- 53 (- (integer-length n) (integer-length d))) 1074)))
    (loop
      (multiple-value-bind (q r) (floor (if (>= shift 0) (ash n shift) n)
                                        (if (>= shift 0) d (ash d (- shift))))
        (if (and (> (integer-length q) 53) (> shift -2000))
            (setq shift (- shift 1))
            (let* ((twice (* 2 r))
                   (den (if (>= shift 0) d (ash d (- shift))))
                   (q (if (or (> twice den) (and (= twice den) (oddp q))) (+ q 1) q)))
              (return (%d* (%small-integer-to-double q) (%dexpt2 (- shift))))))))))

(defun %small-integer-to-double (q)
  ;; exact for |q| <= 2^53
  (if (fixnump q) (%fixnum-to-double q)
      (let* ((m (%bignum-mag q)) (r (%fixnum-to-double 0)) (base (%fixnum-to-double +digit-base+)))
        (do ((i (- (%llen m) 1) (- i 1))) ((< i 0) r)
          (setq r (%d+ (%d* r base) (%fixnum-to-double (%lref m i))))))))

(defun %dexpt2 (k)
  (let ((r (%fixnum-to-double 1)) (two (%fixnum-to-double 2)) (half (%d/ (%fixnum-to-double 1) (%fixnum-to-double 2))))
    (if (>= k 0)
        (dotimes (i k r) (setq r (%d* r two)))
        (dotimes (i (- k) r) (setq r (%d* r half))))))

(defun %to-single (x)
  (if (%single-float-p x) x (%double-to-single (%to-double x))))

(defun %float-infinite-p (d)
  (and (%d= d (%d* d 2.0d0)) (not (%d= d 0.0d0))))

(defun %float-result (d rank)
  (let ((r (if (= rank 2) (%double-to-single d) d)))
    (when (and (%float-infinite-p (%to-double r)))
      (error 'floating-point-overflow :operation nil :operands nil))
    r))

(defun %contagion (a b)
  (let ((ra (%num-rank a)) (rb (%num-rank b)))
    (if (> ra rb) ra rb)))

(defun %generic-add (a b)
  (when (and (integerp a) (integerp b)) (return-from %generic-add (%int-add-big a b)))
  (let ((r (%contagion a b)))
    (cond ((= r 0) (%int-add a b))
          ((= r 1) (%ratio-add a b))
          ((< r 4) (%float-result (%d+ (%to-double a) (%to-double b)) r))
          (t (%complex-add a b)))))

(defun %generic-sub (a b)
  (when (and (integerp a) (integerp b)) (return-from %generic-sub (%int-sub-big a b)))
  (let ((r (%contagion a b)))
    (cond ((= r 0) (%int-sub a b))
          ((= r 1) (%ratio-add a (%negate b)))
          ((< r 4) (%float-result (%d- (%to-double a) (%to-double b)) r))
          (t (%complex-add a (%negate b))))))

(defun %negate (x)
  (cond ((integerp x) (%int-negate x))
        ((%ratiop x) (%make-ratio (%int-negate (%ratio-num x)) (%ratio-den x)))
        ((%double-float-p x) (%dneg x))
        ((%single-float-p x) (%double-to-single (%dneg (%single-to-double x))))
        ((%complexp x) (%make-complex (%negate (%complex-real x)) (%negate (%complex-imag x))))
        (t (%type-error x 'number))))

(defun %check-underflow (result a b)
  "Signals FLOATING-POINT-UNDERFLOW when a float operation on nonzero operands gives zero."
  (when (and (floatp result) (zerop result) (not (zerop a)) (not (zerop b)))
    (error 'floating-point-underflow :operation nil :operands (list a b)))
  result)

(defun %generic-mul (a b)
  (when (and (integerp a) (integerp b)) (return-from %generic-mul (%int-mul-big a b)))
  (let ((r (%contagion a b)))
    (cond ((= r 0) (%int-mul a b))
          ((= r 1) (%ratio-mul a b))
          ((< r 4) (%check-underflow (%float-result (%d* (%to-double a) (%to-double b)) r) a b))
          (t (%complex-mul a b)))))

(defun %generic-div (a b)
  (let ((r (%contagion a b)))
    (cond ((<= r 1)
           (when (eql b 0) (error 'division-by-zero :operation '/ :operands (list a b)))
           (%ratio-mul a (%rational-reciprocal b)))
          ((< r 4)
           (let ((db (%to-double b)))
             (when (%d= db (%fixnum-to-double 0))
               (error 'division-by-zero :operation '/ :operands (list a b)))
             (%check-underflow (%float-result (%d/ (%to-double a) db) r) a b)))
          (t (%complex-div a b)))))

(defun %exact-compare-p (a b)
  "Rational vs float comparisons are done exactly (CLHS 12.1.4.1).  A fixnum
converts to a double exactly, so only bignums and ratios need it."
  (and (or (floatp a) (floatp b)) (not (and (floatp a) (floatp b)))
       (not (fixnump a)) (not (fixnump b))
       (realp a) (realp b)
       (not (%float-infinite-p (%to-double (if (floatp a) a b))))
       (not (%double-nan-p (%to-double (if (floatp a) a b))))))

(defun %generic-lt (a b)
  (let ((r (%contagion a b)))
    (cond ((and (< r 4) (%exact-compare-p a b)) (< (rational a) (rational b)))
          ((= r 0) (< (%int-cmp a b) 0))
          ((= r 1) (< (%int-cmp (* (numerator a) (denominator b)) (* (numerator b) (denominator a))) 0))
          ((< r 4) (%d< (%to-double a) (%to-double b)))
          (t (%type-error (if (complexp a) a b) 'real)))))

(defun %generic-num-eq (a b)
  (let ((r (%contagion a b)))
    (cond ((and (< r 4) (%exact-compare-p a b)) (= (rational a) (rational b)))
          ((= r 0) (= (%int-cmp a b) 0))
          ((= r 1) (and (= (numerator a) (numerator b)) (= (denominator a) (denominator b))))
          ((< r 4) (%d= (%to-double a) (%to-double b)))
          (t (and (= (realpart a) (realpart b)) (= (imagpart a) (imagpart b)))))))

(defun %eql-boxed (a b)
  (cond ((and (%bignump a) (%bignump b)) (%fx= (%int-cmp-big a b) 0))
        ((and (%double-float-p a) (%double-float-p b)) (%double-eql a b))
        ((and (%single-float-p a) (%single-float-p b)) (%single-eql a b))
        ((and (%ratiop a) (%ratiop b))
         (and (eql (%ratio-num a) (%ratio-num b)) (eql (%ratio-den a) (%ratio-den b))))
        ((and (%complexp a) (%complexp b))
         (and (eql (%complex-real a) (%complex-real b)) (eql (%complex-imag a) (%complex-imag b))))
        (t nil)))

;;; ratios
(defun numerator (x) (if (%ratiop x) (%ratio-num x) (%check-integer x)))
(defun denominator (x) (if (%ratiop x) (%ratio-den x) (progn (%check-integer x) 1)))

(defun %make-rational (n d)
  (when (eql d 0) (error 'division-by-zero :operation '/ :operands (list n d)))
  (when (%int-minusp d) (setq n (%int-negate n) d (%int-negate d)))
  (let ((g (gcd n d)))
    (unless (eql g 1)
      (setq n (truncate n g) d (truncate d g)))
    (if (eql d 1) n (%make-ratio n d))))

(defun %exact-quotient (a b) (if (eql b 1) a (values (%int-truncate a b))))
(defun %ratio-add (a b)
  ;; Knuth 4.5.1: the gcds are taken of the (small) common factors only
  (let* ((na (numerator a)) (da (denominator a)) (nb (numerator b)) (db (denominator b))
         (g1 (%gcd2 da db)))
    (if (eql g1 1)
        (%make-ratio-or-integer (+ (* na db) (* nb da)) (* da db))
        (let* ((da1 (%exact-quotient da g1))
               (tt (+ (* na (%exact-quotient db g1)) (* nb da1)))
               (g2 (%gcd2 tt g1)))
          (%make-ratio-or-integer (%exact-quotient tt g2) (* da1 (%exact-quotient db g2)))))))
(defun %make-ratio-or-integer (n d)
  ;; N/D in lowest terms with D > 0
  (if (eql d 1) n (%make-ratio n d)))
(defun %ratio-mul (a b)
  (let* ((na (numerator a)) (da (denominator a)) (nb (numerator b)) (db (denominator b))
         (g1 (%gcd2 na db)) (g2 (%gcd2 nb da)))
    (if (or (eql g1 0) (eql g2 0))
        0
        (%make-ratio-or-integer (* (%exact-quotient na g1) (%exact-quotient nb g2))
                                (* (%exact-quotient da g2) (%exact-quotient db g1))))))
(defun %rational-reciprocal (x)
  (%make-rational (denominator x) (numerator x)))

;;; complex
(defun complex (real &optional (imag 0))
  (cond ((and (rationalp real) (rationalp imag) (eql imag 0)) real)
        ((or (floatp real) (floatp imag))
         (let ((r (%contagion real imag)))
           (%make-complex (%float-result (%to-double real) r) (%float-result (%to-double imag) r))))
        (t (%make-complex real imag))))
(defun realpart (x) (if (%complexp x) (%complex-real x) (%check-real x)))
(defun imagpart (x) (if (%complexp x) (%complex-imag x) (* 0 (%check-real x))))
(defun conjugate (x) (if (%complexp x) (%make-complex (realpart x) (%negate (imagpart x))) x))
(defun %complex-add (a b) (complex (+ (realpart a) (realpart b)) (+ (imagpart a) (imagpart b))))
(defun %complex-mul (a b)
  (let ((ar (realpart a)) (ai (imagpart a)) (br (realpart b)) (bi (imagpart b)))
    (complex (- (* ar br) (* ai bi)) (+ (* ar bi) (* ai br)))))
(defun %complex-div (a b)
  (let ((ar (realpart a)) (ai (imagpart a)) (br (realpart b)) (bi (imagpart b)))
    (if (and (rationalp ar) (rationalp ai) (rationalp br) (rationalp bi))
        (let ((den (+ (* br br) (* bi bi))))
          (complex (/ (+ (* ar br) (* ai bi)) den) (/ (- (* ai br) (* ar bi)) den)))
        (if (>= (abs br) (abs bi))
            (let* ((r (/ bi br)) (den (+ br (* bi r))))
              (complex (/ (+ ar (* ai r)) den) (/ (- ai (* ar r)) den)))
            (let* ((r (/ br bi)) (den (+ (* br r) bi)))
              (complex (/ (+ (* ar r) ai) den) (/ (- (* ai r) ar) den)))))))

;;; ------------------------------------------------------------
;;; public arithmetic
;;; ------------------------------------------------------------

(defun + (&rest nums)
  (let ((acc 0)) (dolist (n nums acc) (setq acc (+ acc n)))))
(defun * (&rest nums)
  (let ((acc 1)) (dolist (n nums acc) (setq acc (* acc n)))))
(defun - (num &rest more)
  (if (null more) (- num)
      (let ((acc num)) (dolist (n more acc) (setq acc (- acc n))))))
(defun / (num &rest more)
  (if (null more) (/ 1 num)
      (let ((acc num)) (dolist (n more acc) (setq acc (/ acc n))))))

(defun %compare-chain (pred nums)
  (do ((l nums (cdr l))) ((null (cdr l)) t)
    (unless (funcall pred (car l) (cadr l)) (return nil))))
(defun = (n &rest more) (if more (%compare-chain (lambda (a b) (= a b)) (cons n more)) (progn (%check-number n) t)))
(defun < (n &rest more) (if more (%compare-chain (lambda (a b) (< a b)) (cons n more)) (progn (%check-real n) t)))
(defun > (n &rest more) (if more (%compare-chain (lambda (a b) (> a b)) (cons n more)) (progn (%check-real n) t)))
(defun <= (n &rest more) (if more (%compare-chain (lambda (a b) (<= a b)) (cons n more)) (progn (%check-real n) t)))
(defun >= (n &rest more) (if more (%compare-chain (lambda (a b) (>= a b)) (cons n more)) (progn (%check-real n) t)))
(defun /= (n &rest more)
  (do ((l (cons n more) (cdr l))) ((null l) t)
    (dolist (m (cdr l)) (when (= (car l) m) (return-from /= nil)))))

(defmacro %fast-entries (name one two)
  "Give the function NAME entries for one and two arguments (FUNCALL #'< from
SORT and the like make no argument list)."
  ;; (NOTINLINE: #'NAME is the new function, not the one DEFUN made)
  `(progn (declaim (notinline ,name))
          (setf (fdefinition ',name) (%make-gf-closure (fdefinition ',name) ,one ,two))))
(%fast-entries + (lambda (a) (%check-number a)) (lambda (a b) (+ a b)))
(%fast-entries * (lambda (a) (%check-number a)) (lambda (a b) (* a b)))
(%fast-entries - (lambda (a) (- a)) (lambda (a b) (- a b)))
(%fast-entries / (lambda (a) (/ 1 a)) (lambda (a b) (/ a b)))
(%fast-entries = (lambda (a) (%check-number a) t) (lambda (a b) (= a b)))
(%fast-entries < (lambda (a) (%check-real a) t) (lambda (a b) (< a b)))
(%fast-entries > (lambda (a) (%check-real a) t) (lambda (a b) (> a b)))
(%fast-entries <= (lambda (a) (%check-real a) t) (lambda (a b) (<= a b)))
(%fast-entries >= (lambda (a) (%check-real a) t) (lambda (a b) (>= a b)))

(defun max (n &rest more)
  (let ((m (%check-real n)))
    (dolist (x more m)
      (when (> (%check-real x) m) (setq m x)))))
(defun min (n &rest more)
  (let ((m (%check-real n)))
    (dolist (x more m)
      (when (< (%check-real x) m) (setq m x)))))

(defun abs (x)
  (cond ((fixnump x) (if (%fx< x 0) (- 0 x) x))
        ((%complexp x) (sqrt (+ (* (realpart x) (realpart x)) (* (imagpart x) (imagpart x)))))
        ((floatp x) (if (= (%double-sign (%to-double x)) 1) (%negate x) x))
        ((minusp x) (%negate x))
        (t x)))

(defun signum (x)
  (cond ((rationalp x) (cond ((plusp x) 1) ((minusp x) -1) (t 0)))
        ((floatp x) (cond ((plusp x) (float 1 x)) ((minusp x) (float -1 x)) (t x)))
        (t (if (zerop x) x (/ x (abs x))))))

(defun evenp (x) (if (fixnump x) (%fx= (%fxlogand x 1) 0) (zerop (logand (%check-integer x) 1))))
(defun oddp (x) (not (evenp x)))

(defun gcd (&rest nums)
  (dolist (n nums) (%check-integer n))
  (let ((acc 0))
    (dolist (n nums acc) (setq acc (%gcd2 acc n)))))
(defun %gcd2 (a b)
  (setq a (abs a) b (abs b))
  (do () ((eql b 0) a)
    (if (and (fixnump a) (fixnump b))
        (psetq a b b (%fxrem a b))
        (psetq a b b (nth-value 1 (%int-truncate a b))))))
(defun lcm (&rest nums)
  (dolist (n nums) (%check-integer n))
  (if (null nums) 1
      (let ((acc (abs (car nums))))
        (dolist (n (cdr nums) acc)
          (setq acc (if (or (zerop acc) (zerop n)) 0 (truncate (abs (* acc n)) (gcd acc n))))))))

;;; division family
;;;
;;; TRUNCATE, FLOOR, CEILING and ROUND are primitives: the helper :divmod
;;; (src/prims.lisp) computes fixnums, integers within 64 bits and floats, and
;;; calls %DIVMOD-SLOW for bignums, ratios, mixed rational / float arguments
;;; and whatever signals.  MODE: 0 truncate, 1 floor, 2 ceiling, 3 round.
(defun %float-to-integer-big (d)
  "D is an integral double of magnitude 2^63 or more (or not finite)."
  (let* ((expo (%double-exponent d))
         (mant (+ (* (+ (%double-mant-hi d) 67108864) 67108864) (%double-mant-lo d)))
         (shift (- expo 1075))
         (v (ash mant shift)))
    (when (= expo 2047) (error "Cannot convert ~S to an integer" d))
    (if (= (%double-sign d) 1) (- v) v)))

(defun %float-truncate-to-integer (d)
  "D is a double; returns the integer obtained by truncating it."
  (%double-to-integer d))

(defun %truncate-slow (number divisor)
  (cond ((and (integerp number) (integerp divisor)) (%int-truncate number divisor))
        ((and (rationalp number) (rationalp divisor))
         (when (eql divisor 0) (error 'division-by-zero :operation 'truncate :operands (list number divisor)))
         ;; n1/d1 / n2/d2: the integer quotient of n1*d2 by d1*n2
         (let ((iq (values (%int-truncate (* (numerator number) (denominator divisor))
                                          (* (denominator number) (numerator divisor))))))
           (values iq (- number (* iq divisor)))))
        (t (let* ((r (%contagion number divisor))
                  (dn (%to-double number)) (dd (%to-double divisor)))
             (when (%d= dd 0.0d0)
               (error 'division-by-zero :operation 'truncate :operands (list number divisor)))
             (let ((iq (%float-truncate-to-integer (%dtrunc (%d/ dn dd)))))
               (values iq (- number (* (float iq (%float-result (%fixnum-to-double 0) r)) divisor))))))))

(defun %divmod-slow (number divisor mode)
  (multiple-value-bind (q r) (%truncate-slow number divisor)
    (cond ((eql mode 0) (values q r))
          ((eql mode 1)
           (if (and (not (zerop r)) (if (minusp divisor) (plusp r) (minusp r)))
               (values (- q 1) (+ r divisor))
               (values q r)))
          ((eql mode 2)
           (if (and (not (zerop r)) (if (minusp divisor) (minusp r) (plusp r)))
               (values (+ q 1) (- r divisor))
               (values q r)))
          (t
           (let* ((twice (abs (* 2 r))) (ad (abs divisor)))
             (cond ((or (> twice ad) (and (= twice ad) (oddp q)))
                    (if (eq (minusp number) (minusp divisor))
                        (values (+ q 1) (- r divisor))
                        (values (- q 1) (+ r divisor))))
                   (t (values q r))))))))

(defun truncate (number &optional (divisor 1)) (truncate number divisor))
(defun floor (number &optional (divisor 1)) (floor number divisor))
(defun ceiling (number &optional (divisor 1)) (ceiling number divisor))
(defun round (number &optional (divisor 1)) (round number divisor))

(defun %mod-slow (n d) (nth-value 1 (floor n d)))
(defun %rem-slow (n d) (nth-value 1 (truncate n d)))
(defun mod (n d) (mod n d))
(defun rem (n d) (rem n d))

(defun %float-of (x proto) (if (and proto (%single-float-p proto)) (%to-single x) (%to-double x)))
;;; the quotient as a float of the format of the remainder (a single-float
;;; when both arguments are rational)
(defun %fquotient (q r) (if (%double-float-p r) (float q 1d0) (float q 1.0)))
(defun ffloor (n &optional (d 1)) (multiple-value-bind (q r) (floor n d) (values (%fquotient q r) r)))
(defun fceiling (n &optional (d 1)) (multiple-value-bind (q r) (ceiling n d) (values (%fquotient q r) r)))
(defun ftruncate (n &optional (d 1)) (multiple-value-bind (q r) (truncate n d) (values (%fquotient q r) r)))
(defun fround (n &optional (d 1)) (multiple-value-bind (q r) (round n d) (values (%fquotient q r) r)))

;;; logical
(defun logand (&rest ns) (let ((a -1)) (dolist (n ns a) (setq a (logand a (%check-integer n))))))
(defun logior (&rest ns) (let ((a 0)) (dolist (n ns a) (setq a (logior a (%check-integer n))))))
(defun logxor (&rest ns) (let ((a 0)) (dolist (n ns a) (setq a (logxor a (%check-integer n))))))
(%fast-entries logand (lambda (a) (%check-integer a)) (lambda (a b) (logand a b)))
(%fast-entries logior (lambda (a) (%check-integer a)) (lambda (a b) (logior a b)))
(%fast-entries logxor (lambda (a) (%check-integer a)) (lambda (a b) (logxor a b)))
(defun lognot (n) (- -1 (%check-integer n)))
(defun logeqv (&rest ns)
  (let ((acc -1)) (dolist (n ns acc) (setq acc (lognot (logxor acc (%check-integer n)))))))
(defun lognand (a b) (lognot (logand a b)))
(defun lognor (a b) (lognot (logior a b)))
(defun logandc1 (a b) (logand (lognot a) b))
(defun logandc2 (a b) (logand a (lognot b)))
(defun logorc1 (a b) (logior (lognot a) b))
(defun logorc2 (a b) (logior a (lognot b)))
(defun logtest (a b) (not (zerop (logand a b))))
(defun logbitp (index n)
  (if (and (fixnump index) (fixnump n) (%fx>= index 0))
      (if (%fx< index 30) (%fx= (%fxlogand (%fxash-right n index) 1) 1) (%fx< n 0))
      (progn
        (unless (and (integerp index) (>= index 0)) (%type-error index 'unsigned-byte))
        (oddp (ash (%check-integer n) (- index))))))
(defun ash (n count) (ash n count))

;;; INTEGER-LENGTH and LOGCOUNT of fixnums and integers within 64 bits are
;;; computed by the helpers (clz / popcnt)
(defun integer-length (n) (integer-length n))
(defun %integer-length-big (n)
  (unless (%bignump n) (%type-error n 'integer))
  (let* ((m (%bignum-mag n)) (len (%mag-bitlen m)))
    ;; of a negative integer: the length of |n| - 1
    (if (and (%fx= (%bignum-sign n) 1) (%fx= (%mag-popcount m) 1)) (%fx- len 1) len)))

(defun logcount (n) (logcount n))
(defun %logcount-big (n)
  (unless (%bignump n) (%type-error n 'integer))
  (let ((m (%bignum-mag n)))
    ;; of a negative integer: the 1 bits of |n| - 1
    (if (%fx= (%bignum-sign n) 1)
        (%fx+ (%fx- (%mag-popcount m) 1) (%mag-ctz m))
        (%mag-popcount m))))

(defconstant boole-clr 0) (defconstant boole-set 1) (defconstant boole-1 2) (defconstant boole-2 3)
(defconstant boole-c1 4) (defconstant boole-c2 5) (defconstant boole-and 6) (defconstant boole-ior 7)
(defconstant boole-xor 8) (defconstant boole-eqv 9) (defconstant boole-nand 10) (defconstant boole-nor 11)
(defconstant boole-andc1 12) (defconstant boole-andc2 13) (defconstant boole-orc1 14) (defconstant boole-orc2 15)
(defun boole (op a b)
  (%check-integer a) (%check-integer b)
  (unless (and (fixnump op) (<= 0 op 15)) (%type-error op '(integer 0 15)))
  (case op (0 0) (1 -1) (2 a) (3 b) (4 (lognot a)) (5 (lognot b)) (6 (logand a b)) (7 (logior a b))
    (8 (logxor a b)) (9 (logeqv a b)) (10 (lognand a b)) (11 (lognor a b)) (12 (logandc1 a b))
    (13 (logandc2 a b)) (14 (logorc1 a b)) (15 (logorc2 a b))))

;;; byte specifiers
(defun byte (size position) (cons size position))
(defun byte-size (b) (car b))
(defun byte-position (b) (cdr b))
(defun ldb (b n) (logand (ash n (- (byte-position b))) (- (ash 1 (byte-size b)) 1)))
(defun ldb-test (b n) (not (zerop (ldb b n))))
(defun mask-field (b n) (logand n (ash (- (ash 1 (byte-size b)) 1) (byte-position b))))
(defun dpb (new b n)
  (let ((mask (ash (- (ash 1 (byte-size b)) 1) (byte-position b))))
    (logior (logand n (lognot mask)) (logand (ash new (byte-position b)) mask))))
(defun deposit-field (new b n)
  (let ((mask (ash (- (ash 1 (byte-size b)) 1) (byte-position b))))
    (logior (logand n (lognot mask)) (logand new mask))))

;;; ------------------------------------------------------------
;;; floats
;;; ------------------------------------------------------------

(defun float (x &optional (proto nil protop)) (if protop (float x proto) (float x)))
(defun %float2 (x proto)
  (cond ((%double-float-p proto) (%to-double x))
        ((%single-float-p proto) (%to-single x))
        (t (%type-error proto 'float))))

(defun coerce-to-double (x) (%to-double x))

(defconstant pi 3.141592653589793d0)
(defconstant most-positive-double-float 1.7976931348623157d308)
(defconstant most-negative-double-float -1.7976931348623157d308)
(defconstant most-positive-single-float 3.4028235e38)
(defconstant most-negative-single-float -3.4028235e38)
(defconstant least-positive-double-float 4.9406564584124654d-324)
(defconstant least-positive-single-float 1.4012985e-45)
(defconstant least-positive-normalized-double-float 2.2250738585072014d-308)
(defconstant least-positive-normalized-single-float 1.1754944e-38)
(defconstant least-negative-double-float -4.9406564584124654d-324)
(defconstant least-negative-single-float -1.4012985e-45)
(defconstant double-float-epsilon 1.1102230246251568d-16)
(defconstant single-float-epsilon 5.960465e-8)
(defconstant double-float-negative-epsilon 5.551115123125784d-17)
(defconstant single-float-negative-epsilon 2.9802326e-8)
(defconstant most-positive-short-float 3.4028235e38)
(defconstant most-negative-short-float -3.4028235e38)
(defconstant most-positive-long-float 1.7976931348623157d308)
(defconstant most-negative-long-float -1.7976931348623157d308)

(defun %math-op (x op)
  (let* ((d (%math1 (%to-double x) op))
         (r (if (%double-float-p x) d (%double-to-single d))))
    (when (and (%float-infinite-p (%to-double r)) (not (%float-infinite-p (%to-double x))))
      (error 'floating-point-overflow :operation op :operands (list x)))
    (when (and (zerop r) (or (eql op 6) (not (zerop d))) (not (%float-infinite-p (%to-double x))))
      (error 'floating-point-underflow :operation op :operands (list x)))
    r))

(defun %fcomplex (z)
  "Z (a number) as a complex with float parts."
  (let* ((r (realpart z)) (i (imagpart z))
         (proto (if (or (%double-float-p r) (%double-float-p i)) 1d0 1.0)))
    (%make-complex (float r proto) (float i proto))))

(defun %csqrt (z)
  (let* ((z (%fcomplex z)) (x (realpart z)) (y (imagpart z)))
    (if (and (zerop x) (zerop y)) z
        (let* ((r (abs z)) (tt (sqrt (/ (+ r (abs x)) 2))))
          (if (>= (float-sign x) 0)
              (complex tt (/ y (* 2 tt)))
              (complex (/ (abs y) (* 2 tt)) (float-sign y tt)))))))

(defun %clog (z) (let ((z (%fcomplex z))) (complex (log (abs z)) (phase z))))

(defun sqrt (x)
  (cond ((%complexp x) (%csqrt x))
        ((minusp x) (complex 0 (sqrt (- x))))
        ((and (integerp x) (let ((r (isqrt x))) (and (= (* r r) x) (< x 16777216))))
         (float (isqrt x)))
        (t (let ((d (%dsqrt (%to-double x)))) (if (%double-float-p x) d (%double-to-single d))))))

(defun isqrt (n)
  (unless (and (integerp n) (>= n 0)) (%type-error n '(integer 0)))
  (if (< n 2) n
      (let ((x (ash 1 (ceiling (integer-length n) 2))))
        (loop
          (let ((y (floor (+ x (floor n x)) 2)))
            (when (>= y x) (return x))
            (setq x y))))))

(defun exp (x)
  (if (%complexp x)
      (* (exp (realpart x)) (cis (imagpart x)))
      (%math-op x 6)))

(defun log (x &optional base)
  (if base
      (/ (log x) (log base))
      (cond ((%complexp x) (complex (log (abs x)) (phase x)))
            ((minusp x) (complex (log (- x)) (float pi (if (floatp x) x 1.0))))
            (t (%math-op x 7)))))

(defun sin (x) (if (%complexp x) (let ((r (realpart x)) (i (imagpart x))) (complex (* (sin r) (cosh i)) (* (cos r) (sinh i)))) (%math-op x 0)))
(defun cos (x) (if (%complexp x) (let ((r (realpart x)) (i (imagpart x))) (complex (* (cos r) (cosh i)) (- (* (sin r) (sinh i))))) (%math-op x 1)))
(defun tan (x) (if (%complexp x) (/ (sin x) (cos x)) (%math-op x 2)))
(defun %casinh (z)
  ;; log(z + sqrt(1 + z^2)), using asinh(-z) = -asinh(z) to avoid cancellation
  (let* ((z (%fcomplex z)) (re (realpart z)) (im (imagpart z)))
    (if (or (minusp re) (and (zerop re) (minusp im)))
        (- (%casinh (- z)))
        (%clog (+ z (%csqrt (+ 1 (* z z))))))))
(defun %casin (z)
  ;; asin z = -i asinh(iz)
  (* #c(0 -1) (%casinh (* #c(0 1) (%fcomplex z)))))
(defun asin (x)
  (if (or (%complexp x) (> (abs (%check-real x)) 1)) (%casin x) (%math-op x 3)))
(defun acos (x)
  (if (or (%complexp x) (> (abs (%check-real x)) 1))
      (let ((z (%fcomplex x))) (- (float (/ pi 2) (realpart z)) (%casin z)))
      (%math-op x 4)))
(defun atan (y &optional (x nil xp))
  (cond (xp
         (%check-real y) (%check-real x)
         (let ((d (%math2 (%to-double y) (%to-double x) 1)))
           (if (or (%double-float-p x) (%double-float-p y)) d (%double-to-single d))))
        ((%complexp y)
         ;; (log(1+iz) - log(1-iz)) / 2i
         (let* ((z (%fcomplex y)) (iz (* #c(0 1) z)))
           (/ (- (%clog (+ 1 iz)) (%clog (- 1 iz))) #c(0 2))))
        (t (%math-op y 5))))
(defun sinh (x)
  (if (%complexp x)
      (let* ((z (%fcomplex x)) (a (realpart z)) (b (imagpart z)))
        (complex (* (sinh a) (cos b)) (* (cosh a) (sin b))))
      (%math-op x 9)))
(defun cosh (x)
  (if (%complexp x)
      (let* ((z (%fcomplex x)) (a (realpart z)) (b (imagpart z)))
        (complex (* (cosh a) (cos b)) (* (sinh a) (sin b))))
      (%math-op x 10)))
(defun tanh (x) (if (%complexp x) (/ (sinh x) (cosh x)) (%math-op x 11)))
(defun asinh (x)
  (if (%complexp x) (%casinh x) (%math-op x 12)))
(defun acosh (x)
  (if (or (%complexp x) (< (%check-real x) 1))
      (let ((z (%fcomplex x)))
        (* 2 (%clog (+ (%csqrt (/ (+ z 1) 2)) (%csqrt (/ (- z 1) 2))))))
      (%math-op x 13)))
(defun atanh (x)
  (if (or (%complexp x) (> (abs (%check-real x)) 1))
      (let ((z (%fcomplex x))) (/ (- (%clog (+ 1 z)) (%clog (- 1 z))) 2))
      (%math-op x 14)))
(defun cis (x) (complex (cos x) (sin x)))
(defun phase (x) (if (%complexp x) (atan (imagpart x) (realpart x)) (if (minusp x) (float pi (if (floatp x) x 1.0)) (* 0 (float x)))))

(defun expt (base power)
  (%check-number base) (%check-number power)
  (cond ((eql power 0)
         (cond ((%complexp base) (if (floatp (realpart base)) (complex (float 1 (realpart base)) 0) 1))
               ((floatp base) (float 1 base))
               (t 1)))
        ((and (zerop power) (not (rationalp power)))
         ;; a float or complex zero power: 1 in the contagion type
         (let ((one (* 1 (realpart power) (realpart base))))
           (if (or (%complexp base) (%complexp power)) (complex (float 1 one) (float 0 one)) (float 1 one))))
        ((and (integerp power) (rationalp base))
         (if (minusp power)
             (/ 1 (expt base (- power)))
             (let ((result 1) (b base) (p power))
               (do () ((eql p 0) result)
                 (when (oddp p) (setq result (* result b)))
                 (setq p (ash p -1))
                 (unless (eql p 0) (setq b (* b b)))))))
        ((and (integerp power) (floatp base))
         (if (minusp power) (/ 1 (expt base (- power)))
             (let ((result (float 1 base)) (b base) (p power))
               (do () ((eql p 0) result)
                 (when (oddp p) (setq result (* result b)))
                 (setq p (ash p -1))
                 (unless (eql p 0) (setq b (* b b)))))))
        ((and (integerp power) (%complexp base))
         (if (minusp power) (/ 1 (expt base (- power)))
             (let ((result 1)) (dotimes (i power result) (setq result (* result base))))))
        ((zerop base) (if (zerop power) 1 (* base power)))
        ((or (%complexp base) (%complexp power) (minusp base))
         (exp (* power (log base))))
        (t (let ((d (%math2 (%to-double base) (%to-double power) 0)))
             (if (or (%double-float-p base) (%double-float-p power)) d (%double-to-single d))))))

(defun decode-float (f)
  (if (%d= (%to-double f) 0.0d0)
      (values (float 0 f) 0 (float 1 f))
      (let* ((d (%to-double f))
             (expo (%double-exponent d))
             (e (- expo 1022))
             (m (%make-double-from-parts 0 1022 (%double-mant-hi d) (%double-mant-lo d))))
        (values (if (%single-float-p f) (%double-to-single m) m) e
                (if (= (%double-sign d) 1) (float -1 f) (float 1 f))))))

(defun integer-decode-float (f)
  (let ((d (%to-double f)))
    (if (%d= d 0.0d0) (values 0 0 1)
        (let* ((expo (%double-exponent d))
               (mant (+ (* (+ (%double-mant-hi d) (if (= expo 0) 0 67108864)) 67108864) (%double-mant-lo d)))
               (sign (if (= (%double-sign d) 1) -1 1)))
          (if (%single-float-p f)
              (values (ash mant -29) (- expo 1075 -29) sign)
              (values mant (- expo 1075) sign))))))

(defun scale-float (f k) (* f (expt (float 2 f) k)))
(defun float-radix (f) (declare (ignore f)) 2)
(defun float-sign (f &optional (g (float 1 f)))
  (if (= (%double-sign (%to-double f)) 1) (- (abs g)) (abs g)))
(defun float-digits (f) (if (%single-float-p f) 24 53))
(defun float-precision (f) (if (zerop f) 0 (float-digits f)))

(defun rational (x)
  (cond ((rationalp x) x)
        ((floatp x)
         (multiple-value-bind (m e s) (integer-decode-float x)
           (if (>= e 0) (* s m (ash 1 e)) (* s (/ m (ash 1 (- e)))))))
        (t (%type-error x 'real))))
(defun rationalize (x) (rational x))

;;; ------------------------------------------------------------
;;; integer <-> string
;;; ------------------------------------------------------------

(defun %fixnum-to-string (n base)
  "N a fixnum other than MOST-NEGATIVE-FIXNUM, BASE in [2, 36]."
  (let* ((neg (%fx< n 0)) (x (if neg (%fx- 0 n) n)) (d 1))
    (do ((q (%fxtruncate x base) (%fxtruncate q base))) ((%fx= q 0)) (setq d (%fx+ d 1)))
    (let* ((len (if neg (%fx+ d 1) d)) (s (%make-string len #\0)))
      (when neg (%set-schar s 0 #\-))
      (do ((i (%fx- len 1) (%fx- i 1)) (k 0 (%fx+ k 1))) ((%fx= k d) s)
        (%set-schar s i (%digit-char (%fxrem x base)))
        (setq x (%fxtruncate x base))))))

(defun %integer-to-string (n base)
  (when (and (fixnump n) (not (eql n most-negative-fixnum)))
    (return-from %integer-to-string (%fixnum-to-string n base)))
  (if (eql n 0) "0"
      (let ((digits nil) (neg (%int-minusp n)) (x (abs n)))
        (if (fixnump x)
            (do () ((eql x 0))
              (push (%digit-char (%fxrem x base)) digits)
              (setq x (%fxtruncate x base)))
            ;; bignum: divide by base^k chunks
            (let* ((chunk (if (= base 10) 10000 base)) (chunk-digits (if (= base 10) 4 1))
                   (m (%bignum-mag x)))
              (do () ((eql (%mag-length m) 0))
                (multiple-value-bind (q r) (%mag-divmod-small m chunk)
                  (setq m q)
                  (dotimes (i chunk-digits)
                    (push (%digit-char (%fxrem r base)) digits)
                    (setq r (%fxtruncate r base)))))
              (do () ((not (and digits (cdr digits) (eql (car digits) #\0)))) (pop digits))))
        (when neg (push #\- digits))
        (let ((s (%make-string (length-of-list digits) #\0)) (i 0))
          (dolist (c digits s) (%set-schar s i c) (setq i (+ i 1)))))))

(defun %digit-char (d) (code-char (if (< d 10) (+ 48 d) (+ 55 d))))

(defun parse-integer (string &key (start 0) end (radix 10) junk-allowed)
  (let* ((end (or end (length string)))
         (i start) (sign 1) (value 0) (any nil))
    (do () ((not (and (< i end) (%whitespace-char-p (char string i))))) (setq i (+ i 1)))
    (when (< i end)
      (cond ((char= (char string i) #\-) (setq sign -1 i (+ i 1)))
            ((char= (char string i) #\+) (setq i (+ i 1)))))
    (do () ((>= i end))
      (let ((d (digit-char-p (char string i) radix)))
        (if d
            (setq value (+ (* value radix) d) any t i (+ i 1))
            (return))))
    ;; with JUNK-ALLOWED the index is that of the character after the digits
    (unless (and any junk-allowed)
      (do () ((not (and (< i end) (%whitespace-char-p (char string i))))) (setq i (+ i 1))))
    (cond ((and any (or junk-allowed (= i end))) (values (* sign value) i))
          (junk-allowed (values nil i))
          (t (error 'parse-error-simple :format-control "Cannot parse integer from ~S"
                                         :format-arguments (list string))))))

(defun %whitespace-char-p (c)
  (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Linefeed)))

