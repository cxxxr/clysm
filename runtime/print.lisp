;;;; print.lisp — the printer
;;;;
;;;; OUTPUT-OBJECT is the central entry point (CLHS 22.1): it handles
;;;; *print-circle* (a first pass prints the object to a null stream to find
;;;; shared structure, the second pass prints #n= / #n# labels), dispatches to
;;;; the pretty printer through *print-pprint-dispatch* when *print-pretty* is
;;;; true, and otherwise prints the object "uglily" (OUTPUT-UGLY-OBJECT).

(defvar *print-escape* t)
(defvar *print-readably* nil)
(defvar *print-base* 10)
(defvar *print-radix* nil)
(defvar *print-case* :upcase)
(defvar *print-circle* nil)
(defvar *print-pretty* t)
(defvar *print-level* nil)
(defvar *print-length* nil)
(defvar *print-array* t)
(defvar *print-gensym* t)
(defvar *print-lines* nil)
(defvar *print-right-margin* nil)
(defvar *print-miser-width* nil)
(defvar *print-pprint-dispatch* nil)
(defvar *read-default-float-format* 'single-float)
(defvar *print-current-level* 0)
(defvar *print-object-hook* nil)
(defvar *readtable* nil)
(defvar *standard-readtable* nil)

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  ;; the body is called with the stream actually printed to (a pretty
  ;; stream when *print-pretty*), bound to STREAM when it is a variable
  (let* ((var (cond ((null stream) '*standard-output*)
                    ((eq stream t) '*terminal-io*)
                    ((symbolp stream) stream)
                    (t nil)))
         (s (gensym "STREAM")))
    `(print-unreadable-object* ,object ,stream ,type ,identity
                               ,(if body
                                    `(lambda (,s)
                                       (declare (ignorable ,s))
                                       ,(if var `(let ((,var ,s)) ,@body nil) `(progn ,@body nil)))
                                    nil))))

;;; ------------------------------------------------------------
;;; entry points
;;; ------------------------------------------------------------

(defun %write-with-keys (object &key (stream nil) (escape *print-escape*) (radix *print-radix*)
                          (base *print-base*) (circle *print-circle*) (pretty *print-pretty*)
                          (level *print-level*) (length *print-length*) (case *print-case*)
                          (gensym *print-gensym*) (array *print-array*) (readably *print-readably*)
                          (right-margin *print-right-margin*) (lines *print-lines*)
                          (miser-width *print-miser-width*) (pprint-dispatch *print-pprint-dispatch*))
  (let ((*print-escape* escape) (*print-radix* radix) (*print-base* base) (*print-circle* circle)
        (*print-pretty* pretty) (*print-level* level) (*print-length* length) (*print-case* case)
        (*print-gensym* gensym) (*print-array* array) (*print-readably* readably)
        (*print-right-margin* right-margin) (*print-lines* lines)
        (*print-miser-width* miser-width) (*print-pprint-dispatch* pprint-dispatch))
    (output-object object (%output-stream stream))
    object))

(defun write (object &rest keys)
  (cond ((null keys) (output-object object *standard-output*) object)
        ((and (eq (car keys) :stream) (consp (cdr keys)) (null (cddr keys)))
         (output-object object (%output-stream (cadr keys)))
         object)
        (t (apply #'%write-with-keys object keys))))

;;; WRITE and WRITE-TO-STRING with literal keywords bind only the printer
;;; variables that are given
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %write-key-variable (key)
    (case key
      (:escape '*print-escape*) (:radix '*print-radix*) (:base '*print-base*)
      (:circle '*print-circle*) (:pretty '*print-pretty*) (:level '*print-level*)
      (:length '*print-length*) (:case '*print-case*) (:gensym '*print-gensym*)
      (:array '*print-array*) (:readably '*print-readably*)
      (:right-margin '*print-right-margin*) (:lines '*print-lines*)
      (:miser-width '*print-miser-width*) (:pprint-dispatch '*print-pprint-dispatch*)
      (t nil)))
  (defun %write-keys-expansion (keys allow-stream)
    "For a WRITE keyword list with literal, distinct keywords: (values
temporaries-bindings special-bindings stream-form), else NIL."
    (let ((temps nil) (specials nil) (stream nil) (seen nil))
      (do ((l keys (cddr l))) ((null l) (values (nreverse temps) (nreverse specials) stream t))
        (let ((key (car l)))
          (unless (and (consp (cdr l)) (keywordp key) (not (member key seen))
                       (or (%write-key-variable key) (and allow-stream (eq key :stream))))
            (return nil))
          (push key seen)
          (let ((g (gensym "V")))
            (push (list g (cadr l)) temps)
            (if (eq key :stream) (setq stream g) (push (list (%write-key-variable key) g) specials))))))))

(define-compiler-macro write (&whole form object &rest keys)
  (multiple-value-bind (temps specials stream ok) (%write-keys-expansion keys t)
    (if (not ok)
        form
        (let ((o (gensym "OBJECT")))
          `(let* ((,o ,object) ,@temps)
             (let ,specials
               (output-object ,o (%output-stream ,stream)))
             ,o)))))

(defun prin1 (object &optional stream)
  (if *print-escape*
      (output-object object (%output-stream stream))
      (let ((*print-escape* t)) (output-object object (%output-stream stream))))
  object)
(defun princ (object &optional stream)
  (if (or *print-escape* *print-readably*)
      (let ((*print-escape* nil) (*print-readably* nil)) (output-object object (%output-stream stream)))
      (output-object object (%output-stream stream)))
  object)
(defun print (object &optional stream)
  (let ((s (%output-stream stream)))
    (terpri s) (prin1 object s) (write-char #\Space s) object))
(defun pprint (object &optional stream)
  (let ((s (%output-stream stream)) (*print-pretty* t) (*print-escape* t))
    (terpri s) (output-object object s) (values)))

(defun write-to-string (object &rest keys)
  (let ((s (%scratch-string-stream)))
    (if keys
        (apply #'%write-with-keys object :stream s keys)
        (output-object object s))
    (%scratch-string-result s)))

(define-compiler-macro write-to-string (&whole form object &rest keys)
  (multiple-value-bind (temps specials stream ok) (%write-keys-expansion keys nil)
    (declare (ignore stream))
    (if (not ok)
        form
        (let ((o (gensym "OBJECT")) (s (gensym "STREAM")))
          `(let* ((,o ,object) ,@temps (,s (%scratch-string-stream)))
             (let ,specials
               (output-object ,o ,s))
             (%scratch-string-result ,s))))))

(defun prin1-to-string (object)
  (let ((s (%scratch-string-stream)))
    (prin1 object s)
    (%scratch-string-result s)))
(defun princ-to-string (object)
  (if (and (%simple-string-p object) (not (and *print-pretty* (%ppd-user-entries-p))))
      (%simple-substring object 0 (%string-length object))
      (let ((s (%scratch-string-stream)))
        (princ object s)
        (%scratch-string-result s))))

;;; compatibility for other runtime files
(defun %write-object (x stream) (output-object x stream))
(defun %write-object-1 (x stream) (output-object x stream))

;;; ------------------------------------------------------------
;;; circularity detection (*print-circle*)
;;;
;;; *circularity-hash-table* maps objects to: T (seen once), 0 (seen more
;;; than once, label not yet assigned) or a label number.  During the first
;;; pass *circularity-counter* is NIL.
;;; ------------------------------------------------------------

(defvar *circularity-hash-table* nil)
(defvar *circularity-counter* nil)
;;; the object whose circularity OUTPUT-OBJECT has just checked; a logical
;;; block printing that same object must not check it again
(defvar *%circle-checked-object* nil)

(defun %uniquely-identified-by-print-p (x)
  (or (numberp x) (characterp x) (and (symbolp x) (symbol-package x))))

(defun %compound-object-p (x)
  (or (consp x) (%instancep x)
      (and (arrayp x) (not (stringp x)) (not (bit-vector-p x)))))

(defun %check-for-circularity (object &optional assign)
  "First pass: records OBJECT and returns true when it was seen before.
Second pass: NIL for objects printed once; for shared objects either a fresh
label (positive, when ASSIGN), T (shared, not yet labelled) or minus the label."
  (let ((table *circularity-hash-table*))
    (cond ((or (null *print-circle*) (null table)) nil)
          ((null *circularity-counter*)
           (let ((v (gethash object table)))
             (cond ((null v) (setf (gethash object table) t) nil)
                   ((eq v t) (setf (gethash object table) 0) t)
                   (t t))))
          (t
           (let ((v (gethash object table)))
             (cond ((or (null v) (eq v t)) nil)
                   ((eql v 0)
                    (if assign
                        (let ((n (+ *circularity-counter* 1)))
                          (setq *circularity-counter* n)
                          (setf (gethash object table) n)
                          n)
                        t))
                   (t (- v))))))))

(defun %handle-circularity (marker stream)
  "Prints a label for MARKER; returns true when the object itself must be printed."
  (cond ((eq marker t) nil)
        ((> marker 0)
         (write-char #\# stream) (%write-decimal marker stream) (write-char #\= stream)
         t)
        (t
         (write-char #\# stream) (%write-decimal (- marker) stream) (write-char #\# stream)
         nil)))

(defun output-object (x stream)
  (cond ((or (not *print-circle*) (%uniquely-identified-by-print-p x))
         (%print-dispatched x stream))
        (*circularity-hash-table*
         (let ((marker (%check-for-circularity x t)))
           (if marker
               (when (%handle-circularity marker stream)
                 (let ((*%circle-checked-object* x)) (%print-dispatched x stream)))
               (let ((*%circle-checked-object* x)) (%print-dispatched x stream)))))
        ((%compound-object-p x)
         (let ((*circularity-hash-table* (make-hash-table :test 'eq)))
           (let ((*circularity-counter* nil))
             (output-object x (make-broadcast-stream)))
           (let ((*circularity-counter* 0))
             (output-object x stream))))
        (t (%print-dispatched x stream))))

(defun %flat-atom-list-p (x)
  "X is a short proper list of numbers, characters, symbols and strings."
  (let ((n 0))
    (do ((l x (cdr l))) ((null l) t)
      (unless (and (consp l) (< (setq n (+ n 1)) 64)) (return nil))
      (let ((e (car l)))
        (unless (or (numberp e) (characterp e) (symbolp e) (stringp e)) (return nil))))))

(defun %ppd-user-entries-p ()
  "True when the current pprint dispatch table has entries made by the user."
  (let ((table *print-pprint-dispatch*))
    (and (pprint-dispatch-table-p table) (pprint-dispatch-table-entries table) t)))

(defun %print-dispatched (x stream)
  ;; only conses and arrays other than strings have standard pretty printing
  ;; functions
  (if (and *print-pretty*
           (or (consp x) (and (arrayp x) (not (%simple-string-p x))) (%ppd-user-entries-p)))
      (let ((fn (%pprint-dispatch-function x)))
        (cond ((null fn) (output-ugly-object x stream))
              ((and (consp x) (or (eq fn #'pprint-fill) (eq fn #'%pprint-fun-call))
                    (not (%pretty-stream-p stream)) (null *print-lines*) (null *print-circle*)
                    (%flat-atom-list-p x))
               ;; when it fits on the line, the pretty printer prints such a
               ;; list as the plain printer does
               (let ((str (let ((o (%scratch-string-stream)))
                            (output-ugly-object x o)
                            (%scratch-string-result o))))
                 (if (and (< (+ (or (%stream-column stream) 0) (length str)) (or *print-right-margin* 80))
                          (not (find #\Newline str)))
                     (write-string str stream)
                     (%with-pretty-stream stream (lambda (s) (funcall fn s x))))))
              (t (%with-pretty-stream stream (lambda (s) (funcall fn s x))))))
      (output-ugly-object x stream)))

;;; *print-level* handling: prints # instead of calling FN when too deep
(defun %descend-into (stream fn)
  (if (and *print-level* (not *print-readably*) (>= *print-current-level* *print-level*))
      (write-char #\# stream)
      (let ((*print-current-level* (+ *print-current-level* 1)))
        (funcall fn))))

(defun %print-length-exceeded-p (n)
  (and *print-length* (not *print-readably*) (>= n *print-length*)))

(defun %print-not-readable (object)
  (let ((*print-readably* nil))
    (error 'print-not-readable :object object)))

(defun output-ugly-object (x stream)
  (cond ((consp x) (%output-list x stream))
        ((fixnump x) (%write-integer x stream))
        ((symbolp x) (%output-symbol x stream))
        ((characterp x) (%write-character x stream))
        ((numberp x) (%write-number x stream))
        ((%instancep x) (%write-instance x stream))
        ((arrayp x) (%output-array x stream))
        ((functionp x) (%write-function x stream))
        ((%packagep x)
         (when *print-readably* (%print-not-readable x))
         (write-string "#<PACKAGE " stream)
         (if (package-name x) (%write-string-object (package-name x) stream) (write-string "(deleted)" stream))
         (write-string ">" stream))
        ((%cellp x)
         (when *print-readably* (%print-not-readable x))
         (write-string "#<CELL>" stream))
        ((%foreignp x)
         (when *print-readably* (%print-not-readable x))
         (write-string "#<JS-VALUE " stream)
         (write-string (symbol-name (%foreign-type-name x)) stream)
         (write-string ">" stream))
        (t
         (when *print-readably* (%print-not-readable x))
         (write-string "#<unknown object>" stream))))

;;; ------------------------------------------------------------
;;; numbers
;;; ------------------------------------------------------------

;;; Digits are made at the end of a scratch buffer and copied to the stream
;;; at once (nothing else runs between the two steps).
(defvar *%digit-buffer* (%make-string 80 #\0))

(defun %fixnum-digits (n base buf i)
  "Store the digits of the nonnegative fixnum N in BASE into the simple
string BUF, ending before index I; returns the index of the first digit."
  (declare (fixnum n base i))
  (if (eql base 10)
      (loop
        (let ((q (%fxtruncate n 10)))
          (declare (fixnum q))
          (setq i (- i 1))
          (%set-schar buf i (code-char (+ 48 (- n (* q 10)))))
          (when (eql q 0) (return i))
          (setq n q)))
      (loop
        (let* ((q (%fxtruncate n base)) (d (- n (* q base))))
          (declare (fixnum q d))
          (setq i (- i 1))
          (%set-schar buf i (code-char (if (< d 10) (+ 48 d) (+ 55 d))))
          (when (eql q 0) (return i))
          (setq n q)))))

(defun %fixnum-digits-padded (n buf i width)
  "Like %FIXNUM-DIGITS in base 10, with leading zeros up to WIDTH digits."
  (declare (fixnum n i width))
  (let ((start (- i width)))
    (declare (fixnum start))
    (setq i (%fixnum-digits n 10 buf i))
    (do () ((<= i start) i)
      (setq i (- i 1))
      (%set-schar buf i #\0))))

(defun %integer-digits (n base buf)
  "Store the digits of the magnitude of the integer N in BASE at the end of
the scratch buffer BUF and return the index of the first one, or NIL when N
is too large for the buffer (it has more than 62 bits)."
  (let ((end (%string-length buf)))
    (cond ((and (fixnump n) (not (eql n most-negative-fixnum)))
           (%fixnum-digits (if (< n 0) (- n) n) base buf end))
          ((and (eql base 10) (< (integer-length n) 63))
           ;; chunks of eight decimal digits, which are fixnums
           (let ((x (abs n)) (i end))
             (loop
               (when (fixnump x) (return (%fixnum-digits x 10 buf i)))
               (multiple-value-bind (q r) (truncate x 100000000)
                 (setq i (%fixnum-digits-padded r buf i 8))
                 (setq x q)))))
          (t nil))))

(defun %write-integer-digits (n base stream)
  "Write the integer N in BASE (with a minus sign when negative)."
  (let* ((buf *%digit-buffer*) (i (%integer-digits n base buf)))
    (if i
        (progn
          (when (< n 0) (setq i (- i 1)) (%set-schar buf i #\-))
          (%stream-write-scratch stream buf i (%string-length buf)))
        (%stream-write-simple-string-whole stream (%integer-to-string n base)))))

(defun %stream-write-scratch (stream buf start end)
  "Write the characters START..END of a scratch buffer.  Streams that may
run other code while they take the characters get a copy."
  (case (stream-kind stream)
    ((:string-out :string-fp :fd-out :file) (%stream-write-simple-string stream buf start end))
    (t (%stream-write-simple-string stream (%simple-substring buf start end) 0 (- end start)))))

(defun %stream-write-simple-string-whole (stream string)
  (%stream-write-simple-string stream string 0 (%string-length string)))

(defun %write-decimal (n stream) (%write-integer-digits n 10 stream))

(defun %write-radix-prefix (base stream)
  (case base
    (2 (write-string "#b" stream))
    (8 (write-string "#o" stream))
    (16 (write-string "#x" stream))
    (t (write-char #\# stream) (%write-decimal base stream) (write-char #\r stream))))

(defun %write-integer (n stream)
  (let ((base *print-base*))
    (if *print-radix*
        (progn
          (when (/= base 10) (%write-radix-prefix base stream))
          (%write-integer-digits n base stream)
          (when (= base 10) (write-char #\. stream)))
        (%write-integer-digits n base stream))))

(defun %write-number (x stream)
  (cond ((integerp x) (%write-integer x stream))
        ((%ratiop x)
         (when *print-radix* (%write-radix-prefix *print-base* stream))
         (write-string (%integer-to-string (%ratio-num x) *print-base*) stream)
         (write-char #\/ stream)
         (write-string (%integer-to-string (%ratio-den x) *print-base*) stream))
        ((floatp x) (%write-float x stream))
        ((%complexp x)
         (write-string "#C(" stream)
         (%write-number (%complex-real x) stream)
         (write-char #\Space stream)
         (%write-number (%complex-imag x) stream)
         (write-char #\) stream))))

(defvar *%float-digit-buffer* (%make-string 40 #\0))

;;; The shortest digits of a float.  The host makes them (JavaScript's
;;; Number.prototype.toExponential), but the usual cases are computed here
;;; with double arithmetic that is exact:
;;;
;;; * An integer below 2^53 and a power of ten up to 10^22 are exact doubles,
;;;   so one multiplication or division of them is the correctly rounded
;;;   double of a decimal number: that is how a candidate is checked to read
;;;   back as the float.
;;; * A double: decimals of 15 significant digits are farther apart than the
;;;   width of the interval of numbers that round to a double, so at most one
;;;   of them reads back as the double.  If there is one, it is the shortest
;;;   representation (with zeros at the end); if not, the double needs 16 or
;;;   17 digits and the host is asked.
;;; * A single: as the host does, the float is rounded to 1, 2 ... 9 digits
;;;   (halves up) until the decimal reads back as the float.  The product of
;;;   the float (24 bits) and a power of ten up to 10^12 is exact.  A
;;;   quotient is not, so when it is close to a half the float is compared
;;;   with the exact product of the half and the power of ten (up to 10^9);
;;;   with larger powers of ten, close to a half, the host is asked.

(defvar *%powers-of-ten*
  (vector 1d0 1d1 1d2 1d3 1d4 1d5 1d6 1d7 1d8 1d9 1d10 1d11 1d12 1d13 1d14 1d15 1d16 1d17 1d18 1d19
          1d20 1d21 1d22))

(defmacro %scale-by-power-of-ten (d j)
  "D * 10^J for |J| <= 22 (the variable TABLE holds the powers of ten)."
  `(if (>= ,j 0)
       (%d* ,d (%svref table ,j))
       (%d/ ,d (%svref table (- ,j)))))

(defun %decimal-exponent-estimate (d)
  "About floor(log10(D)) for the positive normal double D (it may be one too small)."
  (let ((e2 (- (%double-exponent d) 1023)))
    (declare (fixnum e2))
    (ash (* e2 1233) -12)))

(defun %integer-double-digits (c digits chunk chunk-digits)
  "Store the digits of the double C, a positive integer below CHUNK^2, in
DIGITS from index 0, without the zeros at the end; CHUNK is 10^CHUNK-DIGITS.
Returns (values number-of-digits-stored number-of-digits-of-c)."
  (declare (fixnum chunk chunk-digits) (double-float c))
  (let* ((dchunk (%fixnum-to-double chunk))
         (dhi (%dfloor (%d/ c dchunk)))
         (dlo (%d- c (%d* dhi dchunk))))
    ;; the quotient may be off by one
    (cond ((%d< dlo 0d0) (setq dhi (%d- dhi 1d0) dlo (%d+ dlo dchunk)))
          ((not (%d< dlo dchunk)) (setq dhi (%d+ dhi 1d0) dlo (%d- dlo dchunk))))
    (let* ((hi (%double-to-fixnum dhi)) (lo (%double-to-fixnum dlo))
           (buf *%digit-buffer*) (end (%string-length buf))
           (i (if (eql hi 0)
                  (%fixnum-digits lo 10 buf end)
                  (%fixnum-digits hi 10 buf (%fixnum-digits-padded lo buf end chunk-digits))))
           (n (- end i)) (nd n))
      (declare (fixnum hi lo end i n nd))
      (do () ((or (<= nd 1) (not (eq (%schar buf (+ i nd -1)) #\0)))) (setq nd (- nd 1)))
      (%string-copy digits 0 buf i nd)
      (values nd n))))

(defun %short-double-digits (d digits)
  "The shortest digits of the positive double D when they are at most 15:
(values number-of-digits exponent), else NIL."
  (declare (double-float d))
  (let ((raw (%double-exponent d)) (table *%powers-of-ten*))
    (declare (fixnum raw))
    (when (and (> raw 0) (< raw 2047))
      (let ((j (- 14 (%decimal-exponent-estimate d))))
        (declare (fixnum j))
        (when (<= -21 j 21)
          (let ((y (%scale-by-power-of-ten d j)))
            (declare (double-float y))
            (cond ((%d< y 1d14) (setq j (+ j 1) y (%scale-by-power-of-ten d j)))
                  ((not (%d< y 1d15)) (setq j (- j 1) y (%scale-by-power-of-ten d j))))
            (let ((c (%dround y)))
              (declare (double-float c))
              (macrolet ((try (c)
                           `(let ((c ,c))
                              (declare (double-float c))
                              (when (and (%d< c 1d15) (%d<= 1d0 c)
                                         (%d= (%scale-by-power-of-ten c (- j)) d))
                                (multiple-value-bind (nd n) (%integer-double-digits c digits 100000000 8)
                                  (declare (fixnum nd n))
                                  (return-from %short-double-digits (values nd (- n j))))))))
                ;; a decimal that reads back as D is within 0.12 of the true
                ;; product, and so is Y: it can only be the integer nearest to Y
                (try c)
                nil))))))))

(defun %short-single-digits (d digits)
  "The shortest digits of the single float whose value is the positive double
D: (values number-of-digits exponent), or NIL when the host must be asked."
  (declare (double-float d))
  (let ((raw (%double-exponent d)) (table *%powers-of-ten*))
    (declare (fixnum raw))
    (when (and (> raw 0) (< raw 2047))
      (let ((e10 (%decimal-exponent-estimate d)))
        (declare (fixnum e10))
        (unless (<= -13 e10 20) (return-from %short-single-digits nil))
        ;; 10^e10 <= d < 10^(e10+1) must hold
        (let ((y (%scale-by-power-of-ten d (- e10))))
          (declare (double-float y))
          (cond ((not (%d< y 10d0)) (setq e10 (+ e10 1) y (%scale-by-power-of-ten d (- e10))))
                ((%d< y 1d0) (setq e10 (- e10 1) y (%scale-by-power-of-ten d (- e10)))))
          (when (or (%d< y 1d0) (not (%d< y 10d0))) (return-from %short-single-digits nil))
          ;; a product that is very close to 1 or 10 may be on the wrong side
          (when (and (< e10 0)
                     (or (%d< y 1.000000001d0) (%d< 9.99999999d0 y)))
            (return-from %short-single-digits nil)))
        (do ((p 1 (+ p 1))) ((> p 9) nil)
          (declare (fixnum p))
          (let* ((j (- p 1 e10))
                 (y (%scale-by-power-of-ten d j))
                 (f (%dfloor y))
                 (frac (%d- y f))
                 (c (cond ((or (%d< frac 0.499999d0) (%d< 0.500001d0 frac))
                           (if (%d< 0.5d0 frac) (%d+ f 1d0) f))
                          ;; close to a half
                          ((<= 0 j 12) (if (%d< frac 0.5d0) f (%d+ f 1d0)))     ; the product is exact
                          ((<= -9 j -1)
                           (if (%d< d (%d* (%d+ f 0.5d0) (%svref table (- j)))) f (%d+ f 1d0)))
                          (t (return-from %short-single-digits nil)))))
            (declare (fixnum j) (double-float y f frac c))
            (when (%d= (%single-to-double (%double-to-single (%scale-by-power-of-ten c (- j)))) d)
              (multiple-value-bind (nd n) (%integer-double-digits c digits 100000 5)
                (declare (fixnum nd n))
                (return (values nd (- n j)))))))))))

(defun %float-digits-into (x digits)
  "Store the shortest digits that identify the finite float X in the simple
string DIGITS; returns (values number-of-digits exponent negative-p) with
|x| = 0.d1d2.. * 10^exponent."
  (let* ((single (%single-float-p x))
         (d (%to-double x)))
    (multiple-value-bind (nd expo)
        (if single
            (%short-single-digits (%dabs d) digits)
            (%short-double-digits (%dabs d) digits))
      (if nd
          (values nd expo (eql (%double-sign d) 1))
          (%host-float-digits-into d single digits)))))

(defun %host-float-digits-into (d single digits)
  (let* ((n (%float-to-chars d (if single 1 0)))
         (neg (and (> n 0) (char= (%host-char 0) #\-)))
         (nd 0) (i (if neg 1 0)) (expo 0) (eneg nil))
    (declare (fixnum n nd i expo))
    ;; mantissa digits up to the exponent marker, skipping the point
    (loop
      (when (>= i n) (return))
      (let ((c (%host-char i)))
        (when (char= c #\e) (return))
        (unless (char= c #\.) (%set-schar digits nd c) (setq nd (+ nd 1))))
      (setq i (+ i 1)))
    ;; the exponent: e+N or e-N
    (setq i (+ i 1))
    (when (< i n)
      (let ((c (%host-char i)))
        (cond ((char= c #\-) (setq eneg t i (+ i 1)))
              ((char= c #\+) (setq i (+ i 1))))))
    (do () ((>= i n)) (setq expo (+ (* expo 10) (- (char-code (%host-char i)) 48)) i (+ i 1)))
    (when eneg (setq expo (- expo)))
    ;; strip trailing zeros (keep at least one digit)
    (do () ((or (<= nd 1) (char/= (%schar digits (- nd 1)) #\0))) (setq nd (- nd 1)))
    (values nd (+ expo 1) neg)))

(defun %float-digits-and-exponent (x)
  "Returns (values digit-string exponent negative-p) with x = 0.d1d2.. * 10^exponent"
  (let ((buf (%make-string 24 #\0)))
    (multiple-value-bind (nd e neg) (%float-digits-into x buf)
      (values (%simple-substring buf 0 nd) e neg))))

(defun %write-float (x stream)
  (let ((d (%to-double x)))
    (cond ((%double-nan-p d)
           (when *print-readably* (%print-not-readable x))
           (write-string "#<NaN>" stream))
          ((and (%d= d (%d* d (%fixnum-to-double 2))) (not (%d= d (%fixnum-to-double 0))))
           (when (and *print-readably* (not *read-eval*)) (%print-not-readable x))
           (write-string (if (%single-float-p x)
                             (if (%d< d (%fixnum-to-double 0)) "#.SB-EXT:SINGLE-FLOAT-NEGATIVE-INFINITY"
                                 "#.SB-EXT:SINGLE-FLOAT-POSITIVE-INFINITY")
                             (if (%d< d (%fixnum-to-double 0)) "#.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY"
                                 "#.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY"))
                         stream))
          (t (%write-finite-float x stream)))))

(defun %float-exponent-marker (x)
  (if (if (%single-float-p x)
          (member *read-default-float-format* '(single-float short-float))
          (member *read-default-float-format* '(double-float long-float)))
      nil
      (if (%single-float-p x) "f" "d")))

(defun %write-finite-float (x stream)
  (if (zerop x)
      (progn
        (when (= (%double-sign (%to-double x)) 1) (write-char #\- stream))
        (write-string "0.0" stream)
        (let ((m (%float-exponent-marker x))) (when m (write-string m stream) (write-char #\0 stream))))
      ;; the text is put together in a scratch buffer: digits at 40.., text at 0..
      (let ((buf *%digit-buffer*) (o 0))
        (declare (fixnum o))
        (multiple-value-bind (nd e neg) (%float-digits-into x *%float-digit-buffer*)
          (declare (fixnum nd e))
          (let ((digits *%float-digit-buffer*)
                (marker (%float-exponent-marker x)))
            (macrolet ((put (c) `(progn (%set-schar buf o ,c) (setq o (+ o 1))))
                       (put-digits (start end)
                         `(let ((start ,start) (end ,end))
                            (declare (fixnum start end))
                            (%string-copy buf o digits start (- end start))
                            (setq o (+ o (- end start))))))
              (when neg (put #\-))
              (if (and (> e -3) (<= e 7))
                  ;; fixed notation: value = 0.digits * 10^e
                  (progn
                    (cond ((<= e 0)
                           (put #\0) (put #\.)
                           (dotimes (i (- e)) (put #\0))
                           (put-digits 0 nd))
                          ((>= e nd)
                           (put-digits 0 nd)
                           (dotimes (i (- e nd)) (put #\0))
                           (put #\.) (put #\0))
                          (t (put-digits 0 e)
                             (put #\.)
                             (put-digits e nd)))
                    (when marker (put (%schar marker 0)) (put #\0)))
                  ;; exponential: d.ddd e (e-1)
                  (let ((ex (- e 1)))
                    (declare (fixnum ex))
                    (put-digits 0 1)
                    (put #\.)
                    (if (> nd 1) (put-digits 1 nd) (put #\0))
                    (put (if marker (%schar marker 0) #\e))
                    (when (< ex 0) (put #\-) (setq ex (- ex)))
                    (let* ((end (+ o 4)) (i (%fixnum-digits ex 10 buf end)))
                      (declare (fixnum end i))
                      (%string-copy buf o buf i (- end i))
                      (setq o (+ o (- end i))))))
              (%stream-write-scratch stream buf 0 o)))))))

;;; ------------------------------------------------------------
;;; characters and strings
;;; ------------------------------------------------------------

(defun %write-character (c stream)
  (if (or *print-escape* *print-readably*)
      (progn
        (write-string "#\\" stream)
        (let ((name (and (not (graphic-char-p c)) (char-name c))))
          (if name (write-string name stream) (write-char c stream))))
      (write-char c stream)))

(defun %write-string-object (s stream)
  (if (or *print-escape* *print-readably*)
      (if (%simple-string-p s)
          ;; the parts between characters to escape are written at once
          (let ((n (%string-length s)) (start 0))
            (declare (fixnum n start))
            (write-char #\" stream)
            (dotimes (i n)
              (let ((c (%schar s i)))
                (when (or (eq c #\") (eq c #\\))
                  (when (< start i) (%stream-write-simple-string stream s start i))
                  (write-char #\\ stream)
                  (setq start i))))
            (when (< start n) (%stream-write-simple-string stream s start n))
            (write-char #\" stream))
          (progn
            (write-char #\" stream)
            (dotimes (i (length s))
              (let ((c (char s i)))
                (when (or (char= c #\") (char= c #\\)) (write-char #\\ stream))
                (write-char c stream)))
            (write-char #\" stream)))
      (write-string s stream)))

;;; ------------------------------------------------------------
;;; symbols (CLHS 22.1.3.3)
;;; ------------------------------------------------------------

(defun %potential-number-p (name base)
  "True when NAME could be read as a potential number (CLHS 2.3.1.1) in BASE."
  (let ((n (length name)) (has-dot nil))
    (when (= n 0) (return-from %potential-number-p nil))
    ;; a potential number has a digit
    (when (and (<= base 10)
               (dotimes (i n t) (let ((code (char-code (char name i))))
                                  (when (and (>= code 48) (<= code 57)) (return nil)))))
      (return-from %potential-number-p nil))
    (dotimes (i n)
      (let ((c (char name i)))
        (cond ((char= c #\.) (setq has-dot t))
              ((or (digit-char-p c 10) (member c '(#\+ #\- #\/ #\^ #\_))) nil)
              ((alpha-char-p c) nil)
              (t (return-from %potential-number-p nil)))))
    (flet ((digitp (c) (or (digit-char-p c 10) (and (not has-dot) (digit-char-p c base)))))
      (let ((has-digit nil))
        (dotimes (i n)
          (let ((c (char name i)))
            (cond ((digitp c) (setq has-digit t))
                  ((alpha-char-p c)
                   ;; a letter that is a number marker must not be adjacent to another letter
                   (when (or (and (> i 0) (alpha-char-p (char name (- i 1))) (not (digitp (char name (- i 1)))))
                             (and (< (+ i 1) n) (alpha-char-p (char name (+ i 1))) (not (digitp (char name (+ i 1))))))
                     (return-from %potential-number-p nil))))))
        (and has-digit
             (let ((c (char name 0)))
               (or (digitp c) (member c '(#\+ #\- #\. #\^ #\_))))
             (not (member (char name (- n 1)) '(#\+ #\-))))))))

(defun %symbol-quote-p (name readtable)
  "True when NAME must be printed with |...| to be read back as a symbol name."
  (let ((n (length name))
        (rcase (readtable-case readtable)))
    ;; the usual case: upper-case letters, digits and - * + only, starting with
    ;; a letter, all constituents (no potential number in bases up to 10)
    (when (and (eq rcase :upcase) (> n 0) (%simple-string-p name) (<= *print-base* 10)
               (<= 65 (char-code (%schar name 0)) 90))
      (let ((ascii (%rt-ascii readtable)))
        (when (dotimes (i n t)
                (let ((c (char-code (%schar name i))))
                  (unless (and (or (<= 65 c 90) (<= 48 c 57) (= c 45) (= c 42) (= c 43))
                               (eq (svref ascii c) :constituent))
                    (return nil))))
          (return-from %symbol-quote-p nil))))
    (or (= n 0)
        (dotimes (i n nil)
          (let ((c (char name i)))
            (when (or (member c '(#\Space #\Tab #\Newline #\Return #\Page #\Backspace #\Rubout
                                  #\| #\\ #\: #\( #\) #\' #\" #\; #\` #\,))
                      (not (graphic-char-p c))
                      (case rcase
                        (:upcase (lower-case-p c))
                        (:downcase (upper-case-p c))
                        (t nil))
                      (let ((syn (%syntax c readtable)))
                        (and (or (eq syn :terminating) (and (eq syn :non-terminating) (= i 0)))
                             (gethash c (%rt-macros readtable)))))
              (return t))))
        (dotimes (i n t) (unless (eql (char name i) #\.) (return nil)))
        (%potential-number-p name *print-base*))))

(defun %output-symbol-name-cased (name stream readtable)
  "Write NAME without escapes, applying *print-case* and the readtable case."
  (let ((rcase (readtable-case readtable)) (pcase *print-case*))
    (when (and (eq rcase :upcase) (eq pcase :upcase))     ; the usual case: no change
      (return-from %output-symbol-name-cased (write-string name stream)))
    (case rcase
      (:preserve (write-string name stream))
      (:invert
       (let ((up nil) (low nil))
         (dotimes (i (length name))
           (let ((c (char name i)))
             (when (upper-case-p c) (setq up t))
             (when (lower-case-p c) (setq low t))))
         (cond ((and up low) (write-string name stream))
               (up (write-string (string-downcase name) stream))
               (t (write-string (string-upcase name) stream)))))
      (t
       ;; :upcase affects upper-case chars; :downcase lower-case chars
       (let ((affected (if (eq rcase :downcase) #'lower-case-p #'upper-case-p))
             (prev-alnum nil))
         (dotimes (i (length name))
           (let ((c (char name i)))
             (write-char
              (if (funcall affected c)
                  (case pcase
                    (:upcase (char-upcase c))
                    (:downcase (char-downcase c))
                    (:capitalize (if prev-alnum (char-downcase c) (char-upcase c)))
                    (t c))
                  c)
              stream)
             (setq prev-alnum (alphanumericp c)))))))))

(defun %output-symbol-token (name stream readtable)
  (if (%symbol-quote-p name readtable)
      (progn
        (write-char #\| stream)
        (dotimes (i (length name))
          (let ((c (char name i)))
            (when (or (char= c #\|) (char= c #\\)) (write-char #\\ stream))
            (write-char c stream)))
        (write-char #\| stream))
      (%output-symbol-name-cased name stream readtable)))

(defun %print-readtable ()
  (if *print-readably* *standard-readtable* (or *readtable* *standard-readtable*)))

(defun %output-symbol (s stream)
  (let ((name (symbol-name s)))
    (if (or *print-escape* *print-readably*)
        (let ((pkg (%symbol-package s)) (rt (%print-readtable)))
          (cond ((eq pkg *keyword-package*) (write-char #\: stream))
                ((null pkg) (when (or *print-gensym* *print-readably*) (write-string "#:" stream)))
                ((eq pkg *package*) nil)
                (t (multiple-value-bind (found status) (find-symbol name *package*)
                     (unless (and status (eq found s))
                       (%output-symbol-token (package-name pkg) stream rt)
                       (multiple-value-bind (f2 st2) (find-symbol name pkg)
                         (declare (ignore f2))
                         (write-string (if (eq st2 :external) ":" "::") stream))))))
          (%output-symbol-token name stream rt))
        (%output-symbol-name-cased name stream (or *readtable* *standard-readtable*)))))

(defun %write-symbol (s stream) (%output-symbol s stream))

;;; ------------------------------------------------------------
;;; lists
;;; ------------------------------------------------------------

(defun %backquote-prefix (x)
  "The reader prefix for a backquote form (quasiquote x), (unquote x) ..."
  (and (consp (cdr x)) (null (cddr x))
       (case (car x)
         (quasiquote "`") (unquote ",") (unquote-splicing ",@")
         (t nil))))

(defun %quote-prefix (x)
  (and (consp (cdr x)) (null (cddr x))
       (case (car x)
         (quote "'") (function "#'") (quasiquote "`") (unquote ",") (unquote-splicing ",@")
         (t nil))))

(defun %output-list (x stream)
  (let ((prefix (%backquote-prefix x)))
    (if prefix
        (progn (write-string prefix stream) (output-object (cadr x) stream))
        (if (%print-level-exceeded-p)
            (write-char #\# stream)
            (let ((*print-current-level* (+ *print-current-level* 1)))
              (write-char #\( stream)
              (let ((n 0) (l x))
                (loop
                  (when (%print-length-exceeded-p n)
                    (write-string "..." stream) (return))
                  (output-object (car l) stream)
                  (setq l (cdr l))
                  (when (null l) (return))
                  (when (or (atom l) (%check-for-circularity l) (%backquote-prefix l))
                    (write-string " . " stream)
                    (output-object l stream)
                    (return))
                  (write-char #\Space stream)
                  (setq n (+ n 1))))
              (write-char #\) stream))))))

(defun %write-list (x stream) (%output-list x stream))

;;; ------------------------------------------------------------
;;; arrays
;;; ------------------------------------------------------------

(defun %string-like-p (x)
  (or (stringp x) (and (vectorp x) (null (array-element-type x)))))

(defun %array-readably-printable-p (a)
  (and (eq (array-element-type a) t)
       (let* ((dims (array-dimensions a))
              (zero (position 0 dims)))
         ;; a zero dimension followed by a non-zero one cannot be read back
         (or (null zero) (every #'zerop (nthcdr zero dims))))))

(defun %output-terse-array (a stream)
  (print-unreadable-object (a stream :type t :identity t)))

(defun %output-array (a stream)
  (cond ((%string-like-p a)
         (when (and *print-readably* (not (stringp a))) (%print-not-readable a))
         (%write-string-object (if (stringp a) a "") stream))
        ((not (or *print-array* *print-readably*))
         (%output-terse-array a stream))
        ((bit-vector-p a)
         (write-string "#*" stream)
         (dotimes (i (length a)) (write-char (if (eql (bit a i) 1) #\1 #\0) stream)))
        ((and *print-readably* (not (%array-readably-printable-p a)))
         (%print-not-readable a))
        ((vectorp a) (%write-vector a stream))
        (t (%write-array a stream))))

(defun %write-vector (v stream)
  (if (%print-level-exceeded-p)
      (write-char #\# stream)
      (let ((*print-current-level* (+ *print-current-level* 1)))
        (write-string "#(" stream)
        (dotimes (i (length v))
          (when (> i 0) (write-char #\Space stream))
          (when (%print-length-exceeded-p i)
            (write-string "..." stream) (return))
          (output-object (aref v i) stream))
        (write-char #\) stream))))

(defun %print-level-exceeded-p ()
  (and *print-level* (not *print-readably*) (>= *print-current-level* *print-level*)))

(defun %write-array (a stream)
  (if (%print-level-exceeded-p)
      (write-char #\# stream)
      (%write-array-1 a stream)))

(defun %write-array-1 (a stream)
  (let ((dims (array-dimensions a)))
    (write-char #\# stream)
    (%write-decimal (length dims) stream)
    (write-char #\A stream)
    (let ((idx 0))
      (labels ((walk (ds)
                 (if (null ds)
                     (progn (output-object (row-major-aref a idx) stream) (setq idx (+ idx 1)))
                     (%descend-into stream
                       (lambda ()
                         (write-char #\( stream)
                         (let ((step (reduce #'* (cdr ds))))
                           (dotimes (i (car ds))
                             (when (> i 0) (write-char #\Space stream))
                             (when (%print-length-exceeded-p i)
                               (write-string "..." stream) (return))
                             (let ((start idx))
                               (walk (cdr ds))
                               (setq idx (+ start step)))))
                         (write-char #\) stream))))))
        (walk dims)))))

;;; ------------------------------------------------------------
;;; functions and instances
;;; ------------------------------------------------------------

(defun %write-function (f stream)
  (when *print-readably* (%print-not-readable f))
  (let ((name (%closure-name f)))
    (cond ((%instancep name)
           (write-string "#<STANDARD-GENERIC-FUNCTION " stream)
           (let ((*print-escape* t)) (output-object (%gf-name name) stream))
           (write-string ">" stream))
          (t
           (write-string "#<FUNCTION " stream)
           (if name
               (let ((*print-escape* t)) (output-object name stream))
               (write-string "(LAMBDA)" stream))
           (write-string ">" stream)))))

(defun %gf-name (info) (%instance-ref info 0))

(defun %object-id (x) (%identity-hash x))

(defun %write-instance (x stream)
  (if (and *print-object-hook* (not (hash-table-p x)))
      (funcall *print-object-hook* x stream)
      (%default-print-instance x stream)))

(defun %default-print-instance (x stream)
  (let ((class (%instance-layout x)))
    (cond ((hash-table-p x)
           (print-unreadable-object (x stream :type t :identity t)
             (format stream ":TEST ~S :COUNT ~D" (hash-table-test x) (hash-table-count x))))
          ((%streamp x) (print-unreadable-object (x stream :type t :identity t)))
          ((restart-p x)
           (if (or *print-escape* (null (restart-report-function x)))
               (print-unreadable-object (x stream :type t :identity t)
                 (prin1 (restart-name x) stream))
               (funcall (restart-report-function x) stream)))
          ((%class-p x)
           (print-unreadable-object (x stream :type t)
             (prin1 (class-name x) stream)))
          ((eq (%class-kind class) :structure)
           (let ((printer (car (%class-slot class +class-extra+))))
             (if printer
                 (if (cdr (%class-slot class +class-extra+))
                     (funcall printer x stream)
                     (funcall printer x stream *print-current-level*))
                 (%write-structure x stream))))
          ((eq (%class-kind class) :condition)
           (if (or *print-escape* *print-readably*)
               (print-unreadable-object (x stream :type t :identity t))
               (%report-condition x stream)))
          (t (print-unreadable-object (x stream :type t :identity t))))))

(defun %report-condition (c stream)
  (let ((reporter nil))
    (dolist (class (%class-cpl (%instance-layout c)))
      (let ((r (get (class-name class) '%condition-report)))
        (when r (setq reporter r) (return))))
    (if reporter
        (funcall reporter c stream)
        (format stream "Condition of type ~S was signalled." (type-of c)))))
