;;;; print.lisp — the printer

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

;;; ------------------------------------------------------------
;;; entry points
;;; ------------------------------------------------------------

(defun write (object &key (stream nil) (escape *print-escape*) (radix *print-radix*)
                          (base *print-base*) (circle *print-circle*) (pretty *print-pretty*)
                          (level *print-level*) (length *print-length*) (case *print-case*)
                          (gensym *print-gensym*) (array *print-array*) (readably *print-readably*)
                          (right-margin *print-right-margin*) (lines *print-lines*)
                          (miser-width *print-miser-width*) (pprint-dispatch *print-pprint-dispatch*))
  (declare (ignore right-margin lines miser-width pprint-dispatch))
  (let ((*print-escape* escape) (*print-radix* radix) (*print-base* base) (*print-circle* circle)
        (*print-pretty* pretty) (*print-level* level) (*print-length* length) (*print-case* case)
        (*print-gensym* gensym) (*print-array* array) (*print-readably* readably))
    (%write-object object (%output-stream stream))
    object))

(defun prin1 (object &optional stream)
  (let ((*print-escape* t)) (%write-object object (%output-stream stream))) object)
(defun princ (object &optional stream)
  (let ((*print-escape* nil) (*print-readably* nil)) (%write-object object (%output-stream stream))) object)
(defun print (object &optional stream)
  (let ((s (%output-stream stream)))
    (terpri s) (prin1 object s) (write-char #\Space s) object))
(defun pprint (object &optional stream)
  (let ((s (%output-stream stream)) (*print-pretty* t))
    (terpri s) (prin1 object s) (values)))

(defun write-to-string (object &rest keys)
  (with-output-to-string (s) (apply #'write object :stream s keys)))
(defun prin1-to-string (object) (with-output-to-string (s) (prin1 object s)))
(defun princ-to-string (object) (with-output-to-string (s) (princ object s)))

;;; ------------------------------------------------------------
;;; circularity detection (*print-circle*)
;;; ------------------------------------------------------------

(defvar *circle-table* nil)
(defvar *circle-counter* 0)

(defun %circle-scan (x table)
  (when (or (consp x) (and (arrayp x) (not (stringp x))) (%instancep x))
    (multiple-value-bind (v found) (gethash x table)
      (if found
          (unless v (setf (gethash x table) t))
          (progn
            (setf (gethash x table) nil)
            (cond ((consp x) (%circle-scan (car x) table) (%circle-scan (cdr x) table))
                  ((and (arrayp x) (not (stringp x)))
                   (dotimes (i (array-total-size x)) (%circle-scan (row-major-aref x i) table)))))))))

(defun %circle-check (x stream)
  "Returns T if the object was printed as a #n# reference."
  (when (and *circle-table* (or (consp x) (and (arrayp x) (not (stringp x)))))
    (let ((v (gethash x *circle-table*)))
      (cond ((eq v t)
             (setq *circle-counter* (+ *circle-counter* 1))
             (setf (gethash x *circle-table*) *circle-counter*)
             (write-char #\# stream) (%write-decimal *circle-counter* stream) (write-char #\= stream)
             nil)
            ((integerp v)
             (write-char #\# stream) (%write-decimal v stream) (write-char #\# stream)
             t)
            (t nil)))))

(defun %write-object (x stream)
  (if (and *print-circle* (null *circle-table*)
           (or (consp x) (and (arrayp x) (not (stringp x))) (%instancep x)))
      (let ((*circle-table* (make-hash-table :test 'eq)) (*circle-counter* 0))
        (%circle-scan x *circle-table*)
        (%write-object-1 x stream))
      (%write-object-1 x stream)))

(defun %write-object-1 (x stream)
  (cond ((null x) (%write-symbol x stream))
        ((fixnump x) (%write-integer x stream))
        ((consp x) (if (and *print-pretty* (null *circle-table*))
                       (%pretty-write x stream)
                       (unless (%circle-check x stream) (%write-list x stream))))
        ((symbolp x) (%write-symbol x stream))
        ((characterp x) (%write-character x stream))
        ((stringp x) (%write-string-object x stream))
        ((numberp x) (%write-number x stream))
        ((%instancep x) (%write-instance x stream))
        ((vectorp x) (if (and *print-pretty* (null *circle-table*) (not (bit-vector-p x)))
                         (%pretty-write x stream)
                         (unless (%circle-check x stream) (%write-vector x stream))))
        ((arrayp x) (unless (%circle-check x stream) (%write-array x stream)))
        ((functionp x) (%write-function x stream))
        ((%packagep x)
         (write-string "#<PACKAGE " stream)
         (if (package-name x) (%write-string-object (package-name x) stream) (write-string "(deleted)" stream))
         (write-string ">" stream))
        ((%cellp x) (write-string "#<CELL>" stream))
        ((%foreignp x)
         (write-string "#<JS-VALUE " stream)
         (write-string (symbol-name (%foreign-type-name x)) stream)
         (write-string ">" stream))
        (t (write-string "#<unknown object>" stream))))

;;; ------------------------------------------------------------
;;; numbers
;;; ------------------------------------------------------------

(defun %write-decimal (n stream) (write-string (%integer-to-string n 10) stream))

(defun %write-integer (n stream)
  (let ((base *print-base*))
    (when *print-radix*
      (case base
        (2 (write-string "#b" stream))
        (8 (write-string "#o" stream))
        (16 (write-string "#x" stream))
        (10 nil)
        (t (write-char #\# stream) (%write-decimal base stream) (write-char #\r stream))))
    (write-string (%integer-to-string n base) stream)
    (when (and *print-radix* (= base 10)) (write-char #\. stream))))

(defun %write-number (x stream)
  (cond ((integerp x) (%write-integer x stream))
        ((%ratiop x)
         (when (and *print-radix* (/= *print-base* 10))
           (let ((*print-radix* nil))
             (case *print-base* (2 (write-string "#b" stream)) (8 (write-string "#o" stream))
               (16 (write-string "#x" stream)) (t (write-char #\# stream) (%write-decimal *print-base* stream) (write-char #\r stream)))))
         (let ((*print-radix* nil))
           (%write-integer (%ratio-num x) stream)
           (write-char #\/ stream)
           (%write-integer (%ratio-den x) stream)))
        ((floatp x) (%write-float x stream))
        ((%complexp x)
         (write-string "#C(" stream)
         (%write-number (%complex-real x) stream)
         (write-char #\Space stream)
         (%write-number (%complex-imag x) stream)
         (write-char #\) stream))))

(defun %float-digits-and-exponent (x)
  "Returns (values digit-string exponent negative-p) with x = 0.d1d2.. * 10^exponent"
  (let* ((single (%single-float-p x))
         (n (%float-to-chars (%to-double x) (if single 1 0)))
         (s (let ((str (%make-string n #\0))) (dotimes (i n str) (%set-schar str i (%host-char i)))))
         (neg (and (> n 0) (char= (char s 0) #\-)))
         (s (if neg (subseq s 1) s))
         (epos (position #\e s))
         (mant (subseq s 0 epos))
         (expo (parse-integer s :start (+ epos 1)))
         (digits (remove #\. mant)))
    ;; strip trailing zeros (keep at least one digit)
    (let ((end (length digits)))
      (do () ((or (<= end 1) (char/= (char digits (- end 1)) #\0))) (setq end (- end 1)))
      (values (subseq digits 0 end) (+ expo 1) neg))))

(defun %write-float (x stream)
  (let ((d (%to-double x)))
    (cond ((%double-nan-p d) (write-string "#<NaN>" stream))
          ((and (%d= d (%d* d (%fixnum-to-double 2))) (not (%d= d (%fixnum-to-double 0))))
           (write-string (if (%d< d (%fixnum-to-double 0)) "#.SB-EXT:DOUBLE-FLOAT-NEGATIVE-INFINITY"
                             "#.SB-EXT:DOUBLE-FLOAT-POSITIVE-INFINITY")
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
      (multiple-value-bind (digits e neg) (%float-digits-and-exponent x)
        (when neg (write-char #\- stream))
        (let ((marker (%float-exponent-marker x))
              (nd (length digits)))
          (if (and (> e -3) (<= e 7))
              ;; fixed notation: value = 0.digits * 10^e
              (progn
                (cond ((<= e 0)
                       (write-string "0." stream)
                       (dotimes (i (- e)) (write-char #\0 stream))
                       (write-string digits stream))
                      ((>= e nd)
                       (write-string digits stream)
                       (dotimes (i (- e nd)) (write-char #\0 stream))
                       (write-string ".0" stream))
                      (t (write-string (subseq digits 0 e) stream)
                         (write-char #\. stream)
                         (write-string (subseq digits e) stream)))
                (when marker (write-string marker stream) (write-char #\0 stream)))
              ;; exponential: d.ddd e (e-1)
              (progn
                (write-char (char digits 0) stream)
                (write-char #\. stream)
                (if (> nd 1) (write-string (subseq digits 1) stream) (write-char #\0 stream))
                (write-string (or marker "e") stream)
                (%write-decimal (- e 1) stream)))))))

;;; ------------------------------------------------------------
;;; characters and strings
;;; ------------------------------------------------------------

(defun %write-character (c stream)
  (if *print-escape*
      (progn
        (write-string "#\\" stream)
        (let ((name (and (not (graphic-char-p c)) (char-name c))))
          (if name (write-string name stream) (write-char c stream))))
      (write-char c stream)))

(defun %write-string-object (s stream)
  (if *print-escape*
      (progn
        (write-char #\" stream)
        (dotimes (i (length s))
          (let ((c (char s i)))
            (when (or (char= c #\") (char= c #\\)) (write-char #\\ stream))
            (write-char c stream)))
        (write-char #\" stream))
      (write-string s stream)))

;;; ------------------------------------------------------------
;;; symbols
;;; ------------------------------------------------------------

(defun %symbol-needs-escape-p (name)
  (or (= (length name) 0)
      (dotimes (i (length name) nil)
        (let ((c (char name i)))
          (when (or (lower-case-p c)
                    (member c '(#\( #\) #\' #\" #\; #\` #\, #\| #\\ #\: #\Space #\Tab #\Newline #\#))
                    (and (char= c #\#) (= i 0)))
            (return t))))
      (and (every (lambda (c) (char= c #\.)) name))
      (%potential-number-p name)))

(defun %potential-number-p (name)
  (let ((n (length name)) (i 0) (digits 0))
    (when (and (> n 0) (member (char name 0) '(#\+ #\-))) (setq i 1))
    (when (>= i n) (return-from %potential-number-p nil))
    (do ((j i (+ j 1))) ((>= j n))
      (let ((c (char name j)))
        (cond ((digit-char-p c) (setq digits (+ digits 1)))
              ((member c '(#\. #\/ #\E #\D #\F #\S #\L #\e #\d #\f #\s #\l)) nil)
              (t (return-from %potential-number-p nil)))))
    (and (> digits 0)
         (or (digit-char-p (char name (- n 1))) (char= (char name (- n 1)) #\.)
             (digit-char-p (char name i))))))

(defun %write-symbol-name (name stream)
  (if (and *print-escape* (%symbol-needs-escape-p name))
      (progn
        (write-char #\| stream)
        (dotimes (i (length name))
          (let ((c (char name i)))
            (when (or (char= c #\|) (char= c #\\)) (write-char #\\ stream))
            (write-char c stream)))
        (write-char #\| stream))
      (case *print-case*
        (:downcase (write-string (string-downcase name) stream))
        (:capitalize (write-string (string-capitalize name) stream))
        (t (write-string name stream)))))

(defun %write-symbol (s stream)
  (let ((name (symbol-name s)) (pkg (%symbol-package s)))
    (when *print-escape*
      (cond ((null pkg) (when *print-gensym* (write-string "#:" stream)))
            ((eq pkg *keyword-package*) (write-char #\: stream))
            (t (multiple-value-bind (found status) (find-symbol name *package*)
                 (unless (and status (eq found s))
                   (%write-symbol-name (package-name pkg) stream)
                   (multiple-value-bind (f2 st2) (find-symbol name pkg)
                     (declare (ignore f2))
                     (write-string (if (eq st2 :external) ":" "::") stream)))))))
    (%write-symbol-name name stream)))

;;; ------------------------------------------------------------
;;; lists and vectors
;;; ------------------------------------------------------------

(defun %quote-prefix (x)
  (and (consp (cdr x)) (null (cddr x))
       (case (car x)
         (quote "'") (function "#'") (quasiquote "`") (unquote ",") (unquote-splicing ",@")
         (t nil))))

(defun %write-list (x stream)
  (cond ((and (member (car x) '(quasiquote unquote unquote-splicing)) (%quote-prefix x))
         (write-string (%quote-prefix x) stream) (%write-object-1 (cadr x) stream))
        ((and (eq (car x) 'quote) (consp (cdr x)) (null (cddr x)))
         (write-char #\' stream) (%write-object-1 (cadr x) stream))
        ((and (eq (car x) 'function) (consp (cdr x)) (null (cddr x)))
         (write-string "#'" stream) (%write-object-1 (cadr x) stream))
        ((and *print-level* (>= *print-current-level* *print-level*))
         (write-char #\# stream))
        (t
         (let ((*print-current-level* (+ *print-current-level* 1)))
           (write-char #\( stream)
           (let ((n 0))
             (do ((l x (cdr l))) ((atom l) (when l (write-string " . " stream) (%write-object-1 l stream)))
               (when (> n 0) (write-char #\Space stream))
               (when (and *print-length* (>= n *print-length*))
                 (write-string "..." stream) (return))
               (%write-object-1 (car l) stream)
               (setq n (+ n 1))
               (when (and (consp (cdr l)) *circle-table* (gethash (cdr l) *circle-table*))
                 (write-string " . " stream)
                 (%write-object-1 (cdr l) stream)
                 (return))))
           (write-char #\) stream)))))

(defun %write-vector (v stream)
  (cond ((bit-vector-p v)
         (write-string "#*" stream)
         (dotimes (i (length v)) (write-char (if (eql (bit v i) 1) #\1 #\0) stream)))
        ((not *print-array*)
         (write-string "#<VECTOR>" stream))
        ((and *print-level* (>= *print-current-level* *print-level*)) (write-char #\# stream))
        (t
         (let ((*print-current-level* (+ *print-current-level* 1)))
           (write-string "#(" stream)
           (dotimes (i (length v))
             (when (> i 0) (write-char #\Space stream))
             (when (and *print-length* (>= i *print-length*))
               (write-string "..." stream) (return))
             (%write-object-1 (aref v i) stream))
           (write-char #\) stream)))))

(defun %write-array (a stream)
  (let ((dims (array-dimensions a)))
    (write-char #\# stream)
    (%write-decimal (length dims) stream)
    (write-char #\A stream)
    (if (null dims)
        (%write-object-1 (row-major-aref a 0) stream)
        (let ((idx 0))
          (labels ((walk (ds)
                     (if (null ds)
                         (progn (%write-object-1 (row-major-aref a idx) stream) (setq idx (+ idx 1)))
                         (progn
                           (write-char #\( stream)
                           (dotimes (i (car ds))
                             (when (> i 0) (write-char #\Space stream))
                             (walk (cdr ds)))
                           (write-char #\) stream)))))
            (walk dims))))))

;;; ------------------------------------------------------------
;;; functions and instances
;;; ------------------------------------------------------------

(defun %write-function (f stream)
  (let ((name (%closure-name f)))
    (cond ((%instancep name)
           (write-string "#<STANDARD-GENERIC-FUNCTION " stream)
           (%write-object-1 (%gf-name name) stream)
           (write-string ">" stream))
          (t
           (write-string "#<FUNCTION " stream)
           (if name
               (let ((*print-escape* t)) (%write-object-1 name stream))
               (write-string "(LAMBDA)" stream))
           (write-string ">" stream)))))

(defun %gf-name (info) (%instance-ref info 0))

(defun %object-id (x) (%identity-hash x))

(defun print-unreadable-object* (object stream type identity body-fn)
  (when *print-readably* (error 'print-not-readable :object object))
  (write-string "#<" stream)
  (when type
    (let ((*print-escape* t)) (%write-object-1 (type-of object) stream))
    (when (or body-fn identity) (write-char #\Space stream)))
  (when body-fn (funcall body-fn))
  (when identity
    (when body-fn (write-char #\Space stream))
    (write-char #\{ stream)
    (write-string (%integer-to-string (%object-id object) 16) stream)
    (write-char #\} stream))
  (write-char #\> stream)
  nil)

(defmacro print-unreadable-object ((object stream &key type identity) &body body)
  `(print-unreadable-object* ,object ,stream ,type ,identity
                             ,(if body `(lambda () ,@body) nil)))

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
           (if *print-escape*
               (print-unreadable-object (x stream :type t :identity t))
               (%report-condition x stream)))
          (t (print-unreadable-object (x stream :type t :identity t))))))

(defun %write-structure (x stream)
  (let ((class (%instance-layout x)))
    (write-string "#S(" stream)
    (let ((*print-escape* t)) (%write-object-1 (class-name class) stream))
    (dolist (sd (%class-effective-slots class))
      (write-char #\Space stream)
      (write-char #\: stream)
      (%write-symbol-name (symbol-name (%slotd-name sd)) stream)
      (write-char #\Space stream)
      (%write-object-1 (%instance-ref x (%slotd-location sd)) stream))
    (write-char #\) stream)))

(defun %report-condition (c stream)
  (let ((reporter nil))
    (dolist (class (%class-cpl (%instance-layout c)))
      (let ((r (get (class-name class) '%condition-report)))
        (when r (setq reporter r) (return))))
    (if reporter
        (funcall reporter c stream)
        (format stream "Condition of type ~S was signalled." (type-of c)))))
