;;;; types.lisp — TYPEP, TYPE-OF, SUBTYPEP, EQUAL, EQUALP, DEFTYPE

(defun equal (a b)
  ;; the CDR direction is a loop; objects that are EQUAL only when EQ
  ;; (symbols, fixnums, characters) are settled by the first test
  (loop
    (cond ((eq a b) (return t))
          ((consp a)
           (unless (consp b) (return nil))
           (let ((x (car a)) (y (car b)))
             ;; no call for elements that are EQ, or that only EQ makes EQUAL
             (unless (or (eq x y)
                         (and (not (symbolp x)) (not (fixnump x)) (equal x y)))
               (return nil)))
           (setq a (cdr a) b (cdr b)))
          ((or (symbolp a) (fixnump a) (characterp a)) (return nil))
          ((%simple-string-p a)
           (return (cond ((%simple-string-p b) (%simple-string= a b))
                         ((stringp b) (%string-equal-regions a b))
                         (t nil))))
          ((%simple-vector-p a) (return nil))
          ((numberp a) (return (eql a b)))
          ((stringp a) (return (and (stringp b) (%string-equal-regions a b))))
          ((bit-vector-p a)
           (return (and (bit-vector-p b) (= (length a) (length b))
                        (dotimes (i (length a) t) (unless (eql (bit a i) (bit b i)) (return nil))))))
          ((pathnamep a) (return (and (pathnamep b) (%pathname-equal a b))))
          (t (return nil)))))

;; (not given copies of its own by the typed copies of EQUAL that call it)
(declaim (notinline %pathname-equal))
(defun %pathname-equal (a b)
  ;; (component by component, CLHS EQUAL: not by their namestrings, whose
  ;; printer every program comparing lists would carry)
  (and (eq (%instance-layout a) (%instance-layout b))   ; (both logical or both not)
       (equal (%pathname-host a) (%pathname-host b))
       (equal (%pathname-device a) (%pathname-device b))
       (equal (%pathname-directory a) (%pathname-directory b))
       (equal (%pathname-name a) (%pathname-name b))
       (equal (%pathname-type a) (%pathname-type b))
       (equal (%pathname-version a) (%pathname-version b))))

(defun %equal-2 (a b) (equal a b))
;; (EQUAL A B): EQ, and the objects that only EQ makes EQUAL (symbols,
;; fixnums, characters), settled inline; the others by the function (in code
;; compiled in the image, a call of the image's function through its cell)
(define-compiler-macro equal (&whole form &rest args)
  (if (and (consp args) (consp (cdr args)) (null (cddr args)))
      (let ((a (gensym "A")) (b (gensym "B")))
        `(let ((,a ,(first args)) (,b ,(second args)))
           (cond ((eq ,a ,b) t)
                 ((%immediate-or-symbol-p ,a) nil)
                 (t (%equal-2 ,a ,b)))))
      form))

;;; STRING= of two strings, one of them not simple (a reader's buffer with a
;;; fill pointer, the key of a JSON object looked up in an EQUAL table):
;;; compared over their storage in Wasm when that is a simple string.
(defun %string-equal-regions (a b)
  (let ((r (%string-eq-fast a b 0 nil 0 nil nil)))
    (cond ((eq r 1) t)
          ((eq r 0) nil)
          (t (string= a b)))))

(defun %equalp-strings (a b)
  ;; two simple strings: by the loop over the codes of STRING-EQUAL, which
  ;; leaves a difference of characters beyond ASCII to this one
  (let ((r (%string-eq-fast a b 0 nil 0 nil t)))
    (cond ((eq r 1) t)
          ((eq r 0) nil)
          (t (let ((n (%string-length a)))
               (declare (fixnum n))
               (and (eql n (%string-length b))
                    (dotimes (i n t)
                      (let ((x (%schar a i)) (y (%schar b i)))
                        (unless (%char-ci-eq x y) (return nil))))))))))

(defun %equalp-vector-region (da oa db ob n)
  ;; N elements of the simple storages DA (from OA) and DB (from OB)
  (declare (fixnum oa ob n))
  (cond ((and (%simple-vector-p da) (%simple-vector-p db))
         (dotimes (i n t)
           (let ((x (%svref da (+ i oa))) (y (%svref db (+ i ob))))
             (unless (or (eq x y) (equalp x y)) (return nil)))))
        ((and (%simple-string-p da) (%simple-string-p db))
         (dotimes (i n t)
           (let ((x (%schar da (+ i oa))) (y (%schar db (+ i ob))))
             (unless (%char-ci-eq x y) (return nil)))))
        (t (dotimes (i n t)
             (unless (equalp (%vref da (+ i oa)) (%vref db (+ i ob))) (return nil))))))

(defmacro %equalp-storage (a)
  ;; the simple storage of the array A, without a call for a header over it
  (let ((x (gensym)) (d (gensym)))
    `(let ((,x ,a))
       (if (%mdarray-p ,x)
           (let ((,d (%mdarray-data ,x))) (if (%mdarray-p ,d) (%array-storage ,d) ,d))
           ,x))))
(defmacro %equalp-offset (a)
  (let ((x (gensym)))
    `(let ((,x ,a))
       (if (%mdarray-p ,x)
           (if (%mdarray-p (%mdarray-data ,x)) (%array-total-offset ,x) (%mdarray-offset ,x))
           0))))

(defmacro %equalp-vector-p (v)
  (let ((x (gensym)))
    `(let ((,x ,v))
       (or (%simple-vector-p ,x) (%simple-string-p ,x)
           (if (%mdarray-p ,x) (%header-vector-p ,x) (vectorp ,x))))))
(defmacro %equalp-vector-length (v)
  ;; the length of the vector V (its fill pointer, the dimension of a header)
  (let ((x (gensym)))
    `(let ((,x ,v))
       (cond ((%simple-vector-p ,x) (%svlength ,x))
             ((%simple-string-p ,x) (%string-length ,x))
             ((%mdarray-p ,x) (or (%mdarray-fill ,x) (%mdarray-dim1 ,x)))
             (t (length ,x))))))

(defmacro %equalp-view ((x data off len) &body body)
  ;; DATA, OFF and LEN (variables) set to the storage, the offset and the
  ;; length of X when it is a simple vector or string, or a vector whose
  ;; header is over one (a fill pointer, adjustable); else LEN stays -1
  `(progn
     (cond ((%simple-vector-p ,x) (setq ,len (%svlength ,x)))
           ((%simple-string-p ,x) (setq ,len (%string-length ,x)))
           ((and (%mdarray-p ,x) (not (eql 0 (%mdarray-dim1 ,x))))
            (let ((d (%mdarray-data ,x)) (o (%mdarray-offset ,x)))
              (when (and (or (%simple-vector-p d) (%simple-string-p d)) (fixnump o))
                (let ((f (%mdarray-fill ,x)))
                  (setq ,data d ,off o ,len (if (fixnump f) f (%mdarray-dim1 ,x))))))))
     ,@body))

(defun %equalp-arrays (a b)
  (let ((da a) (db b) (oa 0) (ob 0) (na -1) (nb -1))
    (declare (fixnum oa ob na nb))
    (%equalp-view (a da oa na)
      (%equalp-view (b db ob nb)
        (cond ((or (%fx< na 0) (%fx< nb 0)) (%equalp-arrays-general a b))
              ((not (%fx= na nb)) nil)
              ((%simple-vector-p da)
               (if (%simple-vector-p db)
                   (dotimes (i na t)
                     (let ((x (%svref da (%fx+ i oa))) (y (%svref db (%fx+ i ob))))
                       (unless (or (eq x y) (and (not (symbolp x)) (equalp x y))) (return nil))))
                   ;; (a vector of characters and a string)
                   (%equalp-vector-region da oa db ob na)))
              ((and (eq da a) (eq db b) (%simple-string-p da) (%simple-string-p db)) (%equalp-strings a b))
              (t (%equalp-vector-region da oa db ob na)))))))

(defun %equalp-arrays-general (a b)
  (cond ((%equalp-vector-p a)
         (and (%equalp-vector-p b)
              (let ((n (%equalp-vector-length a)))
                (declare (fixnum n))
                (and (eql n (%equalp-vector-length b))
                     (%equalp-vector-region (%equalp-storage a) (%equalp-offset a)
                                            (%equalp-storage b) (%equalp-offset b) n)))))
        (t
         (and (arrayp b) (not (vectorp b))
              (equal (array-dimensions a) (array-dimensions b))
              (%equalp-vector-region (%equalp-storage a) (%equalp-offset a)
                                     (%equalp-storage b) (%equalp-offset b) (array-total-size a))))))

;;; numbers of any types, = (no call for fixnums and floats: a fixnum and a
;;; single float are doubles exactly, so the comparison of doubles is =)
(defmacro %equalp-f64 (x)
  ;; X: a fixnum, a double or a single float
  (let ((v (gensym)))
    `(let ((,v ,x))
       (cond ((fixnump ,v) (float (the fixnum ,v) 1d0))
             ((%double-float-p ,v) (the double-float ,v))
             (t (float (the single-float ,v) 1d0))))))
(defmacro %equalp-flonum-p (x)
  (let ((v (gensym)))
    `(let ((,v ,x)) (or (fixnump ,v) (%double-float-p ,v) (%single-float-p ,v)))))

(defun %equalp-numbers (a b)
  (if (and (%equalp-flonum-p a) (%equalp-flonum-p b))
      (= (%equalp-f64 a) (%equalp-f64 b))
      (and (numberp b) (= a b))))

(defun %equalp-hash-tables (a b)
  (and (= (hash-table-count a) (hash-table-count b))
       (eq (hash-table-test a) (hash-table-test b))
       (block cmp
         (maphash (lambda (k v)
                    (multiple-value-bind (v2 found) (gethash k b)
                      (unless (and found (equalp v v2)) (return-from cmp nil))))
                  a)
         t)))

(defun %equalp-instances (a b)
  ;; (instances of one class: structures slot by slot, hash tables, pathnames
  ;; as EQUAL; other instances only when EQ)
  (cond ((not (%instancep b)) nil)
        ((not (eq (%instance-layout a) (%instance-layout b))) nil)
        ((hash-table-p a) (%equalp-hash-tables a b))
        ((pathnamep a) (equal a b))
        (t
         ;; (a structure of field slots: by the helper, which answers 0 for
         ;; anything else)
         (let ((r (%sinst-equalp a b)))
           (cond ((not (eql r 0)) r)
                 ((structurep a) (%equalp-slots a b))
                 (t nil))))))

(defun %equalp-slots (a b)
  ;; structures A and B of one class, slot by slot
  (let ((n (%instance-nslots a)))
    (and (eql n (%instance-nslots b))
         (dotimes (i n t)
           (let ((x (%instance-gref a i)) (y (%instance-gref b i)))
             (unless (or (eq x y) (equalp x y)) (return nil)))))))

(defun equalp (a b)
  ;; (the usual kinds of objects compared here, without a call: conses,
  ;; strings, characters, fixnums and doubles)
  (loop
    (cond ((eq a b) (return t))
          ((consp a)
           (unless (consp b) (return nil))
           (let ((x (car a)) (y (car b)))
             (unless (or (eq x y) (and (not (symbolp x)) (equalp x y))) (return nil)))
           (setq a (cdr a) b (cdr b)))
          ((%simple-string-p a)
           (return
             (cond ((%simple-string-p b) (%equalp-strings a b))
                   ((arrayp b) (%equalp-arrays a b))
                   (t nil))))
          ((%i31p a)
           ;; a fixnum or a character
           (return (cond ((not (fixnump a)) (and (characterp b) (%char-ci-eq a b)))
                         ((%i31p b) nil)          ; (not EQ)
                         ((%double-float-p b) (= (float (the fixnum a) 1d0) (the double-float b)))
                         ((%single-float-p b) (= (float (the fixnum a) 1d0) (float (the single-float b) 1d0)))
                         (t (%equalp-numbers a b)))))
          ((symbolp a) (return nil))
          ((%simple-vector-p a) (return (%equalp-arrays a b)))
          ((%double-float-p a)
           (return (cond ((%double-float-p b) (= (the double-float a) (the double-float b)))
                         ((fixnump b) (= (the double-float a) (float (the fixnum b) 1d0)))
                         (t (%equalp-numbers a b)))))
          ((%single-float-p a)
           (return (cond ((%single-float-p b) (= (the single-float a) (the single-float b)))
                         ((fixnump b) (= (float (the single-float a) 1d0) (float (the fixnum b) 1d0)))
                         (t (%equalp-numbers a b)))))
          ((%instancep a) (return (%equalp-instances a b)))
          ((%mdarray-p a) (return (%equalp-arrays a b)))
          ((numberp a) (return (%equalp-numbers a b)))
          ((arrayp a) (return (%equalp-arrays a b)))
          (t (return (equal a b))))))

(defun structurep (x)
  (and (%instancep x)
       (let ((c (%instance-layout x)))
         (and (%instancep c) (eq (%class-kind c) :structure)))))

;;; ------------------------------------------------------------
;;; deftype
;;; ------------------------------------------------------------

(defmacro deftype (name lambda-list &body body)
  (let ((args (gensym)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (%put ',name '%deftype
             (lambda (,args)
               (destructuring-bind ,(%deftype-ll lambda-list) ,args
                 ,@(%deftype-decls body)
                 (block ,name ,@(%deftype-forms body)))))
       ',name)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %deftype-decls (body)
    (let ((r nil))
      (dolist (x body (nreverse r))
        (cond ((and (consp x) (eq (car x) 'declare)) (push x r))
              ((and (stringp x) (not (eq x (car (last body))))) nil)
              (t (return (nreverse r)))))))
  (defun %deftype-forms (body)
    (do ((b body (cdr b)))
        ((not (or (and (consp (car b)) (eq (caar b) 'declare))
                  (and (stringp (car b)) (cdr b))))
         b)))

  (defun %deftype-ll (ll)
    ;; unsupplied optional parameters default to *
    (let ((state nil) (r nil))
      (dolist (x ll (reverse r))
        (cond ((member x '(&optional &key)) (setq state x) (push x r))
              ((member x lambda-list-keywords) (setq state nil) (push x r))
              ((and state (symbolp x)) (push (list x ''*) r))
              (t (push x r)))))))

(defun %expand-deftype (type)
  (let* ((name (if (consp type) (car type) type))
         (expander (and (symbolp name) (get name '%deftype))))
    (if expander
        (values (funcall expander (if (consp type) (cdr type) nil)) t)
        (values type nil))))

;;; ------------------------------------------------------------
;;; typep
;;; ------------------------------------------------------------

(defun %in-range (x lo hi)
  (and (or (eq lo '*) (if (consp lo) (> x (car lo)) (>= x lo)))
       (or (eq hi '*) (if (consp hi) (< x (car hi)) (<= x hi)))))

(defun %array-type-match (x et dims simple)
  (and (arrayp x)
       (or (not simple) (%simple-array-type-p x))
       (or (eq et '*) (equal (array-element-type x) (%normalize-element-type et))
           (and (eq (%normalize-element-type et) t) (eq (array-element-type x) t)))
       (cond ((eq dims '*) t)
             ((integerp dims) (= (array-rank x) dims))
             (t (and (= (array-rank x) (length dims))
                     (every (lambda (d ad) (or (eq d '*) (= d ad))) dims (array-dimensions x)))))))

(defun typep (x type &optional env)
  (declare (ignore env))
  (when (%class-p type)
    (return-from typep (%subclassp (class-of x) type)))
  (if (symbolp type)
      (case type
        ((t) t)
        ((nil) nil)
        (null (null x))
        (cons (consp x))
        (list (listp x))
        (atom (atom x))
        (symbol (symbolp x))
        (keyword (keywordp x))
        (boolean (or (eq x t) (eq x nil)))
        (number (numberp x))
        (integer (integerp x))
        (fixnum (fixnump x))
        (bignum (%bignump x))
        (rational (rationalp x))
        (ratio (%ratiop x))
        (real (realp x))
        (float (floatp x))
        ((single-float short-float) (%single-float-p x))
        ((double-float long-float) (%double-float-p x))
        (complex (%complexp x))
        (character (characterp x))
        (base-char (characterp x))
        (standard-char (and (characterp x) (standard-char-p x)))
        (extended-char nil)
        (string (stringp x))
        (simple-string (simple-string-p x))
        (simple-base-string (and (simple-string-p x) (eq (array-element-type x) 'character)))
        (base-string (and (stringp x) (eq (array-element-type x) 'character)))
        (vector (vectorp x))
        (simple-vector (%simple-vector-p x))
        (array (arrayp x))
        (simple-array (%simple-array-type-p x))
        (bit-vector (bit-vector-p x))
        (simple-bit-vector (%bits-p x))
        (sequence (or (listp x) (vectorp x)))
        ((function compiled-function) (functionp x))
        (hash-table (hash-table-p x))
        (package (%packagep x))
        (bit (or (eql x 0) (eql x 1)))
        (unsigned-byte (and (integerp x) (>= x 0)))
        (signed-byte (integerp x))
        (structure-object (structurep x))
        (standard-object (%subclassp (class-of x) (find-class 'standard-object)))
        (otherwise
         (let ((c (find-class type nil)))
           (if c
               (%subclassp (class-of x) c)
               (multiple-value-bind (exp found) (%expand-deftype type)
                 (if found (typep x exp)
                     (error "Unknown type specifier: ~S" type)))))))
      (let ((head (car type)) (args (cdr type)))
        (case head
          (or (some (lambda (ty) (typep x ty)) args))
          (and (every (lambda (ty) (typep x ty)) args))
          (not (not (typep x (car args))))
          (member (not (null (member x args))))
          (eql (eql x (car args)))
          (satisfies (not (null (funcall (car args) x))))
          ((integer) (and (integerp x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          ((rational) (and (rationalp x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          ((real) (and (realp x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          ((float) (and (floatp x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          ((single-float short-float) (and (%single-float-p x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          ((double-float long-float) (and (%double-float-p x) (%in-range x (if args (car args) '*) (if (cdr args) (cadr args) '*))))
          (mod (and (integerp x) (>= x 0) (< x (car args))))
          (unsigned-byte (and (integerp x) (>= x 0) (or (null args) (eq (car args) '*) (< x (ash 1 (car args))))))
          (signed-byte (and (integerp x)
                            (or (null args) (eq (car args) '*)
                                (let ((lim (ash 1 (- (car args) 1)))) (and (>= x (- lim)) (< x lim))))))
          (cons (and (consp x)
                     (or (null args) (eq (car args) '*) (typep (car x) (car args)))
                     (or (null (cdr args)) (eq (cadr args) '*) (typep (cdr x) (cadr args)))))
          (complex (and (%complexp x) (or (null args) (eq (car args) '*) (typep (realpart x) (car args)))))
          ((vector) (and (vectorp x)
                         (or (null args) (eq (car args) '*) (%array-type-match x (car args) '* nil))
                         (or (null (cdr args)) (eq (cadr args) '*) (= (length x) (cadr args)))))
          ((simple-vector) (and (%simple-vector-p x) (or (null args) (eq (car args) '*) (= (length x) (car args)))))
          ((string simple-string base-string simple-base-string)
           (and (if (member head '(simple-string simple-base-string)) (simple-string-p x) (stringp x))
                (or (member head '(string simple-string)) (eq (array-element-type x) 'character))
                (or (null args) (eq (car args) '*) (= (length x) (car args)))))
          ((bit-vector simple-bit-vector)
           (and (bit-vector-p x) (or (null args) (eq (car args) '*) (= (length x) (car args)))))
          ((array) (%array-type-match x (if args (car args) '*) (if (cdr args) (cadr args) '*) nil))
          ((simple-array) (%array-type-match x (if args (car args) '*) (if (cdr args) (cadr args) '*) t))
          ((function) (if args
                          (error 'simple-error :format-control "TYPEP: ~S is not a valid type specifier for TYPEP"
                                               :format-arguments (list type))
                          (functionp x)))
          ((values) (error 'simple-error :format-control "TYPEP: ~S is not a valid type specifier for TYPEP"
                                         :format-arguments (list type)))
          (otherwise
           (multiple-value-bind (exp found) (%expand-deftype type)
             (if found (typep x exp) (error "Unknown type specifier: ~S" type))))))))

;;; compile-time expansion of TYPEP with a constant type
;;;
;;; (typep x 'TYPE) with a constant TYPE compiles to direct tests: predicate
;;; calls for the standard atomic types, comparisons for numeric ranges,
;;; AND/OR/NOT of the expansions of compound types, the expansion of a DEFTYPE
;;; known at compile time, and for any other symbol (a class or structure
;;; name, or a type defined later) a test against the name's type cell, which
;;; holds the class once it exists.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *typep-predicates*
    '((null . null) (cons . consp) (list . listp) (atom . atom) (symbol . symbolp)
      (number . numberp) (integer . integerp) (fixnum . fixnump) (rational . rationalp)
      (real . realp) (float . floatp) (character . characterp) (string . stringp)
      (simple-string . simple-string-p) (vector . vectorp) (simple-vector . simple-vector-p)
      (array . arrayp) (function . functionp) (hash-table . hash-table-p) (keyword . keywordp)
      (single-float . single-float-p) (double-float . double-float-p) (complex . complexp)
      (bit-vector . bit-vector-p) (package . packagep) (sequence . sequencep)
      (structure-object . structurep) (ratio . ratiop) (bignum . bignump)
      (short-float . single-float-p) (long-float . double-float-p) (signed-byte . integerp)
      (base-char . characterp) (compiled-function . functionp)
      (simple-array . %simple-array-type-p) (simple-bit-vector . simple-bit-vector-p)))

  ;; the names TYPEP itself knows that are not classes or need more than a predicate
  (defparameter *typep-special-names*
    '(boolean bit unsigned-byte standard-char extended-char simple-base-string base-string))

  (defun %typep-bound-ok (b)
    (or (eq b '*) (realp b) (and (consp b) (null (cdr b)) (realp (car b)))))

  (defun %typep-range-expansion (x pred args)
    (let ((lo (if args (car args) '*)) (hi (if (cdr args) (cadr args) '*)))
      (if (and (%typep-bound-ok lo) (%typep-bound-ok hi) (null (cddr args)))
          `(and (,(if (and (eq pred 'integerp) (integerp lo) (integerp hi)
                           (<= -536870912 lo 536870911) (<= -536870912 hi 536870911))
                      'fixnump       ; a range within the fixnums: no generic comparison
                      pred)
                 ,x)
                ,@(cond ((eq lo '*) nil) ((consp lo) `((> ,x ,(car lo)))) (t `((>= ,x ,lo))))
                ,@(cond ((eq hi '*) nil) ((consp hi) `((< ,x ,(car hi)))) (t `((<= ,x ,hi)))))
          nil)))

  (defun %typep-size-ok (args)
    (and (null (cdr args)) (or (null args) (eq (car args) '*) (integerp (car args)))))

  (defun %typep-sized-expansion (x pred args)
    (if (or (null args) (eq (car args) '*))
        `(,pred ,x)
        `(and (,pred ,x) (eql (length ,x) ,(car args)))))

  (defun %typep-cell-expansion (x type)
    "The test of X for a class or structure name (or a type defined later):
an instance (by the class precedence list of its class), or an object that
is not an instance or a function when the class is not a built-in one, is
answered here; the rest by %TYPEP-CELL."
    (let ((cell (gensym "CELL")) (c (gensym "C")) (l (gensym "LAYOUT")) (g (gensym "GF")))
      (flet ((layout-test (lform)
               `(let ((,l ,lform))
                  (cond ((eq ,l ,c) t)
                        ((null ,c) (%typep-cell ,x ,cell))
                        ((%instancep ,l) (if (%memq ,c (%instance-ref ,l +class-cpl+)) t nil))
                        (t nil)))))
        `(let* ((,cell (load-time-value (%type-cell ',type))) (,c (car ,cell)))
           (cond ((%instancep ,x)
                  ;; (%STRUCTURE-TYPEP X C): its class or a class it inherits from
                  ,(layout-test `(%instance-layout ,x)))
                 ,@(if (get type '%defclass)
                       ;; a class defined by DEFCLASS or DEFINE-CONDITION (before,
                       ;; in this file or the image): only instances, and
                       ;; generic functions (funcallable instances), are of it
                       `(((functionp ,x)
                          (let ((,g (%closure-name ,x)))
                            (if (%instancep ,g) ,(layout-test `(%instance-layout ,g)) (if (null ,c) (%typep-cell ,x ,cell) nil))))
                         ((null ,c) (%typep-cell ,x ,cell))
                         (t nil))
                       `(((or (null ,c) (functionp ,x) (eq (%instance-ref ,c +class-kind+) :built-in))
                          (%typep-cell ,x ,cell))
                         (t nil))))))))

  (defun %typep-storage-code (et)
    "The kind of storage (%STORAGE-KIND) of the simple vectors of element type
ET, when ET is written as the element type of a kind; else NIL."
    (cond ((eq et t) 0) ((eq et 'character) 1) ((eq et 'bit) 2)
          ((eq et 'fixnum) 11) ((eq et 'single-float) 15) ((eq et 'double-float) 16)
          ((and (consp et) (consp (cdr et)) (null (cddr et)))
           (cond ((eq (car et) 'unsigned-byte)
                  (cond ((eql (cadr et) 8) 4) ((eql (cadr et) 16) 7) ((eql (cadr et) 32) 10) ((eql (cadr et) 64) 13)))
                 ((eq (car et) 'signed-byte)
                  (cond ((eql (cadr et) 8) 5) ((eql (cadr et) 16) 8) ((eql (cadr et) 32) 11) ((eql (cadr et) 64) 14)))))))

  (defun %typep-expansion (x type &optional (depth 0))
    "A form testing the variable X for the constant type specifier TYPE; a
(%typep X 'TYPE) form when the test is left to TYPEP.  DEPTH counts the DEFTYPE
expansions on the way: a type defined in terms of itself, such as
 (deftype tree () '(or null (cons tree tree))), is left to TYPEP after a few."
    ;; what is left to TYPEP calls %TYPEP, so that the form is not expanded again
    (let ((default `(%typep ,x ',type)))
      (cond ((eq type t) `(progn ,x t))
            ((null type) `(progn ,x nil))
            ((symbolp type)
             (cond ((assoc type *typep-predicates*) `(,(cdr (assoc type *typep-predicates*)) ,x))
                   ((eq type 'boolean) `(or (eq ,x t) (eq ,x nil)))
                   ((eq type 'bit) `(or (eql ,x 0) (eql ,x 1)))
                   ((eq type 'unsigned-byte) `(and (integerp ,x) (>= ,x 0)))
                   ((eq type 'standard-char) `(and (characterp ,x) (standard-char-p ,x)))
                   ((eq type 'extended-char) `(progn ,x nil))
                   ((member type *typep-special-names*) default)
                   ((get type '%deftype)
                    (let ((exp (and (< depth 6)
                                    (and (fboundp '%deftype-expansion-list) (funcall '%deftype-expansion-list type nil)))))
                      (if exp (%typep-expansion x (car exp) (+ depth 1)) default)))
                   ((or (keywordp type) (null (symbol-package type))) default)
                   ;; a structure defined before: its class (the compiler then
                   ;; knows the type of X where the test is true)
                   ((let ((d (get type '%struct-description))) (and (consp d) (null (car d))))
                    (let ((k (nth 4 (get type '%struct-description))))
                      (if k
                          `(%structure-typep ,x (load-time-value (find-class ',type)) ,k)
                          `(%structure-typep ,x (load-time-value (find-class ',type))))))
                   (t (%typep-cell-expansion x type))))
            ((not (and (consp type) (symbolp (car type)) (listp (cdr type)) (null (cdr (last type)))))
             default)
            (t
             (let ((head (car type)) (args (cdr type)))
               (case head
                 ((or and) `(,head ,@(mapcar (lambda (ty) (%typep-expansion x ty depth)) args)))
                 (not (if (and args (null (cdr args))) `(not ,(%typep-expansion x (car args) depth)) default))
                 (member (cond ((null args) `(progn ,x nil))
                               ((null (cdr args)) `(eql ,x ',(car args)))
                               (t `(if (member ,x ',args) t nil))))
                 (eql (if (and args (null (cdr args))) `(eql ,x ',(car args)) default))
                 (satisfies (if (and args (null (cdr args)) (symbolp (car args)))
                                `(if (,(car args) ,x) t nil)
                                default))
                 (integer (or (%typep-range-expansion x 'integerp args) default))
                 (rational (or (%typep-range-expansion x 'rationalp args) default))
                 (real (or (%typep-range-expansion x 'realp args) default))
                 (float (or (%typep-range-expansion x 'floatp args) default))
                 ((single-float short-float) (or (%typep-range-expansion x 'single-float-p args) default))
                 ((double-float long-float) (or (%typep-range-expansion x 'double-float-p args) default))
                 (mod (if (and args (null (cdr args)) (integerp (car args)) (plusp (car args)))
                          (%typep-range-expansion x 'integerp (list 0 (- (car args) 1)))
                          default))
                 (unsigned-byte
                  (cond ((or (null args) (and (eq (car args) '*) (null (cdr args)))) `(and (integerp ,x) (>= ,x 0)))
                        ((and (integerp (car args)) (plusp (car args)) (null (cdr args)))
                         (%typep-range-expansion x 'integerp (list 0 (- (ash 1 (car args)) 1))))
                        (t default)))
                 (signed-byte
                  (cond ((or (null args) (and (eq (car args) '*) (null (cdr args)))) `(integerp ,x))
                        ((and (integerp (car args)) (plusp (car args)) (null (cdr args)))
                         (let ((lim (ash 1 (- (car args) 1))))
                           (%typep-range-expansion x 'integerp (list (- lim) (- lim 1)))))
                        (t default)))
                 (cons
                  (if (null (cddr args))
                      (let ((a (if args (car args) '*)) (d (if (cdr args) (cadr args) '*)) (g (gensym)))
                        `(and (consp ,x)
                              ,@(if (member a '(* t)) nil `((let ((,g (car ,x))) ,(%typep-expansion g a depth))))
                              ,@(if (member d '(* t)) nil `((let ((,g (cdr ,x))) ,(%typep-expansion g d depth))))))
                      default))
                 (simple-vector (if (%typep-size-ok args) (%typep-sized-expansion x 'simple-vector-p args) default))
                 (string (if (%typep-size-ok args) (%typep-sized-expansion x 'stringp args) default))
                 (simple-string (if (%typep-size-ok args) (%typep-sized-expansion x 'simple-string-p args) default))
                 ((bit-vector simple-bit-vector)
                  (if (%typep-size-ok args) (%typep-sized-expansion x 'bit-vector-p args) default))
                 (vector
                  (if (and (null (cddr args)) (or (null (cdr args)) (eq (cadr args) '*) (integerp (cadr args))))
                      (let ((et (if args (car args) '*)))
                        `(and (vectorp ,x)
                              ,@(if (eq et '*) nil `((%array-type-match ,x ',et '* nil)))
                              ,@(if (or (null (cdr args)) (eq (cadr args) '*)) nil
                                    `((eql (length ,x) ,(cadr args))))))
                      default))
                 ((array simple-array)
                  (if (null (cddr args))
                      (let* ((et (if args (car args) '*)) (dims (if (cdr args) (cadr args) '*))
                             (general `(%array-type-match ,x ',et ',dims ,(eq head 'simple-array)))
                             (code (and (eq head 'simple-array) (consp dims) (null (cdr dims))
                                        (or (eq (car dims) '*) (typep (car dims) '(integer 0)))
                                        (%typep-storage-code et))))
                        ;; a simple vector of a kind of storage, (SIMPLE-ARRAY
                        ;; (UNSIGNED-BYTE 8) (*)): the kind of the object,
                        ;; one test (the general test finds the element
                        ;; type of the object and compares it with the
                        ;; type's, 90 ns; an array with a header is left to it);
                        ;; of a length, (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (4)),
                        ;; then the length of the storage
                        (if code
                            (let ((kind (if (eql code 4) `(%bytes-p ,x) `(eql (%storage-kind ,x) ,code))))
                              `(if (%mdarray-p ,x)
                                   ,general
                                   ,(if (eq (car dims) '*)
                                        kind
                                        `(and ,kind (eql ,(if (eql code 4) `(%bytes-length ,x) `(%storage-length ,x))
                                                         ,(car dims))))))
                            general))
                      default))
                 ((complex function values base-string simple-base-string) default)
                 (otherwise
                  (let ((exp (and (get head '%deftype) (< depth 6)
                                  (and (fboundp '%deftype-expansion-list) (funcall '%deftype-expansion-list head args)))))
                    (if exp (%typep-expansion x (car exp) (+ depth 1)) default))))))))))

;;; Type tests by the kind of the object.  %TYPE-CODE gives one of these
;;; codes for any object (src/prims.lisp, the helper :TYPE-CODE: a few type
;;; tests of the Wasm object, the common kinds first):
;;;   0 fixnum  1 character  2 cons  3 NIL  4 other symbol  5 instance
;;;   (structures, standard objects, hash tables...)  6 simple string
;;;   7 simple vector  8 double  9 function  10 bignum  11 ratio  12 single
;;;   13 complex  14 other specialized vector  15 array header  16 package
;;;   17 anything else
;;; A type is then two masks of codes: the kinds all of whose objects are of
;;; it, and those some of whose objects are (whose objects the full test,
;;; %TYPEP-EXPANSION, decides).  A TYPECASE computes the code once and tests
;;; each clause's masks (one shift and AND), with the full test only for the
;;; kinds that need it; TYPEP of a type of several kinds (NUMBER, SEQUENCE,
;;; (OR STRING SYMBOL)...) does the same.  (%KIND-IN X CODE MASK) tells the
;;; compiler X's type where it is true.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *type-code-all* (- (ash 1 18) 1))

  (defun %type-code-bits (&rest codes)
    (let ((m 0)) (dolist (c codes m) (setq m (logior m (ash 1 c))))))

  (defun %literal-type-code (x)
    (cond ((and (integerp x) (<= -536870912 x 536870911)) 0)
          ((integerp x) 10)
          ((characterp x) 1)
          ((null x) 3)
          ((symbolp x) 4)
          (t nil)))

  (defun %type-code-masks (type &optional (depth 0))
    "The masks (values SURE MAYBE) of the type specifier TYPE: the kinds all
of whose objects are of it, and the other kinds some of whose objects may be."
    (let ((all *type-code-all*))
      (flet ((bits (&rest c) (apply #'%type-code-bits c)))
        (cond
          ((> depth 8) (values 0 all))
          ((eq type t) (values all 0))
          ((null type) (values 0 0))
          ((symbolp type)
           (case type
             (fixnum (values (bits 0) 0))
             (bignum (values (bits 10) 0))
             ((integer signed-byte) (values (bits 0 10) 0))
             (unsigned-byte (values 0 (bits 0 10)))
             (bit (values 0 (bits 0)))
             (ratio (values (bits 11) 0))
             (rational (values (bits 0 10 11) 0))
             ((single-float short-float) (values (bits 12) 0))
             ((double-float long-float) (values (bits 8) 0))
             (float (values (bits 8 12) 0))
             (real (values (bits 0 10 11 8 12) 0))
             (complex (values (bits 13) 0))
             (number (values (bits 0 10 11 8 12 13) 0))
             ((character base-char) (values (bits 1) 0))
             ((standard-char extended-char) (values 0 (bits 1)))
             (cons (values (bits 2) 0))
             (null (values (bits 3) 0))
             (list (values (bits 2 3) 0))
             (symbol (values (bits 3 4) 0))
             (keyword (values 0 (bits 4)))
             (boolean (values (bits 3) (bits 4)))
             (atom (values (logandc2 all (bits 2)) 0))
             ((string simple-string) (values (bits 6) (bits 15)))
             ((base-string simple-base-string) (values 0 (bits 6 15)))
             (simple-vector (values (bits 7) 0))
             (vector (values (bits 6 7 14) (bits 15)))
             (simple-array (values (bits 6 7 14) (bits 15)))
             (array (values (bits 6 7 14 15) 0))
             ((bit-vector) (values 0 (bits 14 15)))
             ((simple-bit-vector) (values 0 (bits 14)))
             (sequence (values (bits 2 3 6 7 14) (bits 15)))
             ((function compiled-function) (values (bits 9) 0))
             (package (values (bits 16) 0))
             ((hash-table structure-object) (values 0 (bits 5)))
             (standard-object (values 0 (bits 5 9)))
             (otherwise
              (cond ((and (get type '%defclass) (not (get type '%deftype)))
                     ;; (a class defined by DEFCLASS or DEFINE-CONDITION:
                     ;; instances and funcallable instances)
                     (values 0 (bits 5 9)))
                    ((get type '%deftype)
                     (let ((exp (and (fboundp '%deftype-expansion-list) (funcall '%deftype-expansion-list type nil))))
                       (if exp (%type-code-masks (car exp) (+ depth 1)) (values 0 all))))
                    ((let ((d (get type '%struct-description))) (and (consp d) (null (car d))))
                     (values 0 (bits 5)))
                    (t (values 0 all))))))
          ((not (and (consp type) (symbolp (car type)) (listp (cdr type)))) (values 0 all))
          (t
           (let ((head (car type)) (args (cdr type)))
             (case head
               (or (let ((sure 0) (maybe 0))
                     (dolist (ty args)
                       (multiple-value-bind (s m) (%type-code-masks ty (+ depth 1))
                         (setq sure (logior sure s) maybe (logior maybe m))))
                     (values sure (logandc2 maybe sure))))
               (and (let ((sure all) (any all))
                      (dolist (ty args)
                        (multiple-value-bind (s m) (%type-code-masks ty (+ depth 1))
                          (setq sure (logand sure s) any (logand any (logior s m)))))
                      (values sure (logandc2 any sure))))
               (not (if (and args (null (cdr args)))
                        (multiple-value-bind (s m) (%type-code-masks (car args) (+ depth 1))
                          (values (logandc2 all (logior s m)) m))
                        (values 0 all)))
               ((member eql)
                (let ((m 0))
                  (dolist (x (if (eq head 'eql) (list (car args)) args) (values 0 m))
                    (let ((c (%literal-type-code x)))
                      (if c (setq m (logior m (ash 1 c))) (return (values 0 all)))))))
               ((integer mod unsigned-byte signed-byte)
                (let ((lo (car args)) (hi (cadr args)))
                  (if (and (eq head 'integer) (integerp lo) (integerp hi) (<= -536870912 lo) (<= hi 536870911))
                      (values 0 (bits 0))
                      (if (and (eq head 'mod) (integerp lo) (<= lo 536870912))
                          (values 0 (bits 0))
                          (values 0 (bits 0 10))))))
               (rational (values 0 (bits 0 10 11)))
               (real (values 0 (bits 0 10 11 8 12)))
               (float (values 0 (bits 8 12)))
               ((single-float short-float) (values 0 (bits 12)))
               ((double-float long-float) (values 0 (bits 8)))
               (complex (values 0 (bits 13)))
               (cons (if (every (lambda (a) (member a '(* t))) args) (values (bits 2) 0) (values 0 (bits 2))))
               ((string simple-string base-string simple-base-string) (values 0 (bits 6 15)))
               (simple-vector (values 0 (bits 7)))
               ((vector simple-array array) (values 0 (bits 6 7 14 15)))
               ((bit-vector simple-bit-vector) (values 0 (bits 14 15)))
               (otherwise
                (let ((exp (and (get head '%deftype) (fboundp '%deftype-expansion-list)
                                (funcall '%deftype-expansion-list head args))))
                  (if exp (%type-code-masks (car exp) (+ depth 1)) (values 0 all)))))))))))

  (defun %logcount18 (m) (let ((n 0)) (dotimes (i 18 n) (when (logbitp i m) (setq n (+ n 1))))))

  (defun %typep-code-test (x c type)
    "A form testing the variable X, whose %TYPE-CODE is the variable C, for
the constant type TYPE; :NONE when the masks do not help (an unknown type)."
    (multiple-value-bind (sure maybe) (%type-code-masks type)
      (let ((all *type-code-all*))
        (cond ((= sure all) t)
              ((= (logior sure maybe) 0) nil)
              ((= maybe 0) `(%kind-in ,x ,c ,sure))
              ((= (logior sure maybe) all) :none)
              (t (let ((full (%typep-expansion x type)))
                   (if (= sure 0)
                       `(and (%kind-in ,x ,c ,maybe) ,full)
                       `(or (%kind-in ,x ,c ,sure) (and (%kind-in ,x ,c ,maybe) ,full)))))))))

  (defun %typep-by-code-p (type)
    "Whether TYPEP of TYPE is tested by the kind of the object: a type of
several kinds whose predicates would test them one after the other, with
specialized vectors or complex numbers among them (NUMBER, REAL, SEQUENCE,
VECTOR, (AND NUMBER (NOT INTEGER))...: the kind tells them at once).  Not
(OR STRING SYMBOL), whose two tests cost less, nor ATOM, the complement of
one."
    (multiple-value-bind (sure maybe) (%type-code-masks type)
      (let ((any (logior sure maybe)))
        (and (/= any *type-code-all*) (/= sure *type-code-all*)
             (<= 3 (%logcount18 any) 12)
             (or (logbitp 13 any) (logbitp 14 any) (>= (%logcount18 any) 5)))))))

;;; TYPECASE by the kind, fused (runtime/boot.lisp, %TYPECASE-FORM): the
;;; clauses' tests (%TYPEP-CODE-TEST: of the kind code C by %KIND-IN, and
;;; a full test for the kinds that need one) are decided for each kind when
;;; the form is expanded, and the tests that tell the kinds apart branch to
;;; what is left for theirs -- the body of the first clause all objects of
;;; the kind are of, after the full tests of the clauses before it that some
;;; are of.  No kind code is computed and no clause tested for a kind it
;;; does not hold: a fixnum goes from its test to its clause.
;;;   (block out
;;;     (block body-2 (block body-1
;;;       (block arm-b (block arm-a          ; arms that several kinds share
;;;         (cond ((%i31p k) (if (fixnump k) <arm of 0> <arm of 1>))
;;;               ((%wasm-array-p k) (cond ((%simple-string-p k) <6>) ((%simple-vector-p k) <7>) (t <slow>)))
;;;               ((consp k) <2>) ((null k) <3>) ((symbolp k) <4>) ... (t <slow>)))
;;;         (cond (full-test (return-from body-1 nil)) (t (return-from body-2 nil))))  ; arm-a
;;;       ...)
;;;       (return-from out (progn . body-1)))
;;;     (return-from out (progn . body-2)))
;;; An arm that is one jump is written where the kind is found, and so is an
;;; arm of one kind.  A kind whose arm is that of the objects no test finds
;;; (kind 17) is not tested at all: a TYPECASE of FIXNUM, CONS and T tests
;;; for those two.  (%KIND-IS K MASK) is true, and tells the compiler the
;;; type of K in an arm or a body that is reached by a jump.  TYPEP of a
;;; type of several kinds is such a TYPECASE of one clause.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %tcf-simplify (form)
    (if (atom form)
        form
        (case (car form)
          (and (let ((args nil))
                 (dolist (a (cdr form))
                   (cond ((eq a t))
                         ((null a) (return-from %tcf-simplify nil))
                         (t (push a args))))
                 (cond ((null args) t)
                       ((null (cdr args)) (car args))
                       (t `(and ,@(nreverse args))))))
          (or (let ((args nil))
                (dolist (a (cdr form))
                  (cond ((null a))
                        ((eq a t) (push t args) (return))
                        (t (push a args))))
                (cond ((null args) nil)
                      ((null (cdr args)) (car args))
                      (t `(or ,@(nreverse args))))))
          (if (cond ((eq (second form) t) (third form))
                    ((null (second form)) (fourth form))
                    (t form)))
          (t form))))

  (defun %tcf-subst (form c v lay cpl)
    "FORM for an object of the kind V: the tests of the kind code C decided,
the layout and the list of classes (LAY, CPL) NIL for what is no instance."
    (cond ((and lay (not (eql v 5)) (or (eq form lay) (eq form cpl))) nil)
          ((atom form) form)
          ((eq (car form) 'quote) form)
          ((and (eq (car form) '%kind-in) (consp (cddr form)) (eq (third form) c))
           (if (logbitp v (fourth form)) t nil))
          (t (let ((new nil) (l form))
               (loop (when (atom l) (return))
                     (push (%tcf-subst (car l) c v lay cpl) new)
                     (setq l (cdr l)))
               (%tcf-simplify (nreconc new l))))))

  (defun %tcf-mentions-p (x form)
    (cond ((eq form x) t)
          ((atom form) nil)
          (t (or (%tcf-mentions-p x (car form)) (%tcf-mentions-p x (cdr form))))))

  (defun %typecase-fused-form (k c code-clauses bindings fail)
    "The TYPECASE of the variable K whose clauses, with their tests of the
kind code C (a variable no longer bound), are CODE-CLAUSES; BINDINGS: those
of the layout and its list of classes, or NIL; FAIL: the clause of ETYPECASE."
    (let* ((lay (car (first bindings))) (cpl (car (second bindings)))
           (out (gensym "TYPECASE"))
           (labels (mapcar (lambda (cl) (declare (ignore cl)) (gensym "BODY")) code-clauses))
           (fail-label (gensym "NONE"))
           (arms (make-array 18))
           (distinct nil))                ; (arm kinds...) of the arms that are no single jump
      (dotimes (v 18)
        (let ((arm nil) (sure nil))
          (loop for cl in code-clauses for label in labels
                do (let ((test (%tcf-subst (car cl) c v lay cpl)))
                     (cond ((null test))
                           ((eq test t) (push `(t (return-from ,label nil)) arm) (setq sure t) (return))
                           (t (push `(,test (return-from ,label nil)) arm)))))
          (unless sure (push `(t (return-from ,fail-label nil)) arm))
          (setq arm (nreverse arm))
          (setf (svref arms v) arm)
          (when (cdr arm)
            (let ((e (assoc arm distinct :test #'equal)))
              (if e (nconc e (list v)) (push (list arm v) distinct))))))
      (setq distinct (nreverse distinct))
      (let ((shared (mapcar (lambda (e) (list (car e) (gensym "ARM") (cdr e)))
                            (remove-if (lambda (e) (null (cddr e))) distinct))))
        (labels ((mask (kinds) (let ((m 0)) (dolist (v kinds m) (setq m (logior m (ash 1 v))))))
                 (arm-code (arm kinds)
                   ;; the tests of an arm, with the layout for an instance
                   (let ((code `(cond ,@arm)))
                     (if (and lay (member 5 kinds) (%tcf-mentions-p lay arm))
                         `(let* ((,lay ,(%tcf-subst (second (first bindings)) c 5 nil nil))
                                 (,cpl ,(%tcf-subst (second (second bindings)) c 5 nil nil)))
                            ,code)
                         code)))
                 (leaf (v)
                   (let* ((arm (svref arms v)) (s (assoc arm shared :test #'equal)))
                     (cond ((null (cdr arm)) (second (first arm)))
                           (s `(return-from ,(second s) nil))
                           (t (arm-code arm (list v))))))
                 (slow (kinds)
                   ;; the kinds that only the kind code tells apart
                   (let ((groups nil))
                     (dolist (v kinds)
                       (let* ((code (leaf v)) (e (assoc code groups :test #'equal)))
                         (if e (nconc e (list v)) (push (list code v) groups))))
                     (setq groups (nreverse groups))
                     (if (null (cdr groups))
                         (car (first groups))
                         `(let ((,c (%type-code ,k)))
                            (cond ,@(loop for (g . more) on groups
                                          collect (if more
                                                      `((%kind-in ,k ,c ,(mask (cdr g))) ,(car g))
                                                      `(t ,(car g))))))))))
          ;; The tests in the order of the kinds' frequency (as %TYPE-CODE's).
          ;; A kind whose arm is that of the objects left at the end (kind
          ;; 17) needs no test, it gets there by failing the others; it is
          ;; tested all the same while two tests or more of other arms
          ;; follow, which its objects then do not go through.
          (let* ((rest (leaf 17))
                 (arrays (let ((other (if (equal (leaf 14) rest)
                                          rest
                                          (slow '(14 17)))))
                           `(,@(unless (equal (leaf 6) other) `(((%simple-string-p ,k) ,(leaf 6))))
                             ,@(unless (equal (leaf 7) other) `(((%simple-vector-p ,k) ,(leaf 7))))
                             (t ,other))))
                 ;; (test arm differs-from-the-rest)
                 (entries
                   (list (list `(%i31p ,k)
                               (if (equal (leaf 0) (leaf 1)) (leaf 0) `(if (fixnump ,k) ,(leaf 0) ,(leaf 1)))
                               (not (and (equal (leaf 0) rest) (equal (leaf 1) rest))))
                         (list `(%wasm-array-p ,k)
                               (if (cdr arrays) `(cond ,@arrays) (second (first arrays)))
                               (not (and (null (cdr arrays)) (equal (second (first arrays)) rest))))
                         (list `(consp ,k) (leaf 2) (not (equal (leaf 2) rest)))
                         (list `(symbolp ,k) (leaf 4) (not (equal (leaf 4) rest)))
                         (list `(%instancep ,k) (leaf 5) (not (equal (leaf 5) rest)))
                         (list `(%double-float-p ,k) (leaf 8) (not (equal (leaf 8) rest)))
                         (list `(functionp ,k) (leaf 9) (not (equal (leaf 9) rest)))
                         (list `(%bignump ,k) (leaf 10) (not (equal (leaf 10) rest)))
                         (list `(%ratiop ,k) (leaf 11) (not (equal (leaf 11) rest)))
                         (list `(%single-float-p ,k) (leaf 12) (not (equal (leaf 12) rest)))
                         (list `(%mdarray-p ,k) (leaf 15) (not (equal (leaf 15) rest)))
                         (list `(%packagep ,k) (leaf 16) (not (equal (leaf 16) rest)))
                         (list `(%complexp ,k) (leaf 13) (not (equal (leaf 13) rest)))))
                 (kept (let ((after 0) (r nil))
                         (dolist (e (reverse entries) r)
                           (when (or (third e) (>= after 2)) (push e r))
                           (when (third e) (setq after (+ after 1))))))
                 (clauses (mapcan (lambda (e)
                                    (if (and (eq (car (first e)) 'symbolp) (not (equal (leaf 3) (leaf 4))))
                                        ;; (NIL is a symbol for the test of symbols: told apart before it)
                                        (list `((null ,k) ,(leaf 3)) (list (first e) (second e)))
                                        (list (list (first e) (second e)))))
                                  kept))
                 (form `(cond ,@clauses
                              ,@(unless (or (equal (leaf 3) rest)
                                            (find 'symbolp kept :key (lambda (e) (car (first e)))))
                                  ;; (the test of symbols left out: NIL alone)
                                  `(((null ,k) ,(leaf 3))))
                              (t ,rest))))
            (dolist (s shared)
              (setq form `(progn (block ,(second s) ,form)
                                 (if (%kind-is ,k ,(mask (third s))) ,(arm-code (first s) (third s)) nil))))
            (loop for cl in code-clauses for label in labels
                  do (setq form `(progn (block ,label ,form)
                                        (return-from ,out (progn ,@(cdr cl))))))
            `(block ,out
               (block ,fail-label ,form)
               ,@(if fail (cdr (first fail)) (list nil)))))))))

(define-compiler-macro typep (&whole form x type &optional env)
  (if (and (null env) (consp type) (eq (car type) 'quote) (consp (cdr type)) (null (cddr type)))
      (let ((ty (cadr type)))
        (if (%typep-by-code-p ty)
            (let ((g (if (symbolp x) x (gensym))) (c (gensym "CODE")))
              ;; (as a TYPECASE of one clause: the tests of the kinds that
              ;; are of the type, or may be, and no kind code)
              (let ((e (%typecase-fused-form g c (list (list (%typep-code-test g c ty) t)) nil nil)))
                (if (symbolp x) e `(let ((,g ,x)) ,e))))
        (if (symbolp x)
            (let ((e (%typep-expansion x ty)))
              (if (and (consp e) (eq (car e) '%typep)) form e))
            (let ((g (gensym)))
              (let ((e (%typep-expansion g ty)))
                (if (and (consp e) (eq (car e) '%typep)) form `(let ((,g ,x)) ,e)))))))
      form))

;; VECTORP and ARRAYP in code compiled after this file: by the kind (the
;; functions test the kinds of storage one after the other, in a helper)
(define-compiler-macro vectorp (x)
  (let ((g (if (symbolp x) x (gensym))) (c (gensym "CODE")))
    (let ((e (%typecase-fused-form
              g c `(((or (%kind-in ,g ,c ,(%type-code-bits 6 7 14))
                         (and (%kind-in ,g ,c ,(%type-code-bits 15)) (%header-vector-p ,g)))
                     t))
              nil nil)))
      (if (symbolp x) e `(let ((,g ,x)) ,e)))))
(define-compiler-macro arrayp (x)
  (let ((g (if (symbolp x) x (gensym))) (c (gensym "CODE")))
    (let ((e (%typecase-fused-form g c `(((%kind-in ,g ,c ,(%type-code-bits 6 7 14 15)) t)) nil nil)))
      (if (symbolp x) e `(let ((,g ,x)) ,e)))))

(defun %typep (x type) (typep x type))

;; (the primitives as functions, for FUNCALL and the evaluator)
(defun %type-code (x) (%type-code x))
(defun %kind-in (x c mask) (declare (ignore x)) (logbitp c mask))
(defun %kind-is (x mask) (declare (ignore x mask)) t)
(defun %wasm-array-p (x) (%wasm-array-p x))

;;; The type cell of a symbol: (class-or-nil . name).  %SET-FIND-CLASS keeps
;;; the class in it up to date, so a compiled test of a class or structure
;;; name costs one load instead of a FIND-CLASS.
(defun %type-cell (name)
  (or (get name '%type-cell)
      (let ((cell (cons (get name '%class) name)))
        (%put name '%type-cell cell)
        cell)))

(defun %typep-cell (x cell)
  (let ((c (car cell)))
    (cond ((null c) (typep x (cdr cell)))
          ((%instancep x) (%structure-typep x c))
          ;; only functions (generic functions) and instances have classes that are not built in
          ((and (not (eq (%class-kind c) :built-in)) (not (functionp x))) nil)
          (t (%subclassp (class-of x) c)))))

(defun packagep (x) (%packagep x))

(defun type-of (x)
  (cond ((null x) 'null)
        ((eq x t) 'boolean)
        ((fixnump x) (cond ((or (eql x 0) (eql x 1)) 'bit)
                           ((> x 0) (list 'integer 0 most-positive-fixnum))
                           (t 'fixnum)))
        ((%bignump x) (if (> x 0) (list 'integer (+ most-positive-fixnum 1)) 'bignum))
        ((%ratiop x) 'ratio)
        ((%single-float-p x) 'single-float)
        ((%double-float-p x) 'double-float)
        ((%complexp x) (list 'complex (cond ((%single-float-p (realpart x)) 'single-float)
                                            ((%double-float-p (realpart x)) 'double-float)
                                            (t 'rational))))
        ((characterp x) (if (standard-char-p x) 'standard-char 'character))
        ((keywordp x) 'keyword)
        ((symbolp x) 'symbol)
        ((consp x) 'cons)
        ((%simple-string-p x) (list 'simple-array 'character (list (length x))))
        ((%simple-vector-p x) (list 'simple-vector (length x)))
        ((%bits-p x) (list 'simple-bit-vector (length x)))
        ((arrayp x)
         (list (if (%simple-array-type-p x) 'simple-array 'array) (array-element-type x) (array-dimensions x)))
        ((functionp x) (let ((c (class-of x)))
                         (if (eq c (find-class 'compiled-function)) 'compiled-function (%type-of-class c))))
        ((%packagep x) 'package)
        (t (%type-of-class (class-of x)))))

(defun %type-of-class (c)
  (let ((name (%class-name c)))
    (if (and name (eq (find-class name nil) c)) name c)))

;;; very approximate SUBTYPEP
(defun %type-class-name (ty)
  "Class objects are type specifiers: use the class name when it names the class."
  (if (and (%class-p ty) (eq (find-class (%class-name ty) nil) ty)) (%class-name ty) ty))

(defun %type-empty-p (ty)
  "True when TY is known to denote the empty type."
  (cond ((null ty) t)
        ((and (consp ty) (eq (car ty) 'and))
         (let ((positive (remove-if (lambda (x) (and (consp x) (eq (car x) 'not))) (cdr ty)))
               (negative (mapcar #'cadr (remove-if-not (lambda (x) (and (consp x) (eq (car x) 'not))) (cdr ty)))))
           (or (some #'%type-empty-p positive)
               (some (lambda (p) (some (lambda (n) (subtypep p n)) negative)) positive))))
        ((and (consp ty) (eq (car ty) 'or)) (every #'%type-empty-p (cdr ty)))
        ((and (consp ty) (eq (car ty) 'member)) (null (cdr ty)))
        (t nil)))

(defun %standard-class-type (ty)
  "The class TY is or names when it is a standard, structure or condition
class whose precedence list is known (a type that is that class), else NIL."
  ;; (the types of COMMON-LISP, which COERCE and TYPEP of compound types ask
  ;; about at run time, are not looked up as classes)
  (let ((c (cond ((symbolp ty)
                  (and ty (not (eq (symbol-package ty) *cl-package*)) (find-class ty nil)))
                 ((%class-p ty) ty))))
    (and c (member (%class-kind c) '(:standard :structure :condition)) (consp (%class-cpl c)) c)))

(defun subtypep (t1 t2 &optional env)
  (declare (ignore env))
  ;; two classes of the user's (ASDF's COERCE-CLASS at every operation it
  ;; plans): a subclass when the second is in the first's precedence list
  (let ((c1 (%standard-class-type t1)))
    (when c1
      (let ((c2 (%standard-class-type t2)))
        (when c2 (return-from subtypep (values (%subclassp c1 c2) t))))))
  (setq t1 (%expand-type-fully (%type-class-name t1)) t2 (%expand-type-fully (%type-class-name t2)))
  (multiple-value-bind (r certain unknown) (%subtypep-by-algebra t1 t2)
    (unless unknown (return-from subtypep (values r certain))))
  (multiple-value-bind (r certain known) (%subtypep-arrays t1 t2)
    (when known (return-from subtypep (values r certain))))
  (%subtypep-fallback t1 t2))

(defun %subtypep-fallback (t1 t2)
  "SUBTYPEP for types the set algebra cannot represent: only certain answers
are returned as such."
  (cond ((equal t1 t2) (values t t))
        ((eq t2 t) (values t t))
        ((null t1) (values t t))
        ((and (consp t1) (eq (car t1) 'member))
         (values (every (lambda (x) (typep x t2)) (cdr t1)) t))
        ((and (consp t1) (eq (car t1) 'eql)) (values (typep (cadr t1) t2) t))
        ((and (consp t2) (eq (car t2) 'or))
         (if (some (lambda (x) (subtypep t1 x)) (cdr t2)) (values t t) (values nil nil)))
        ((and (consp t1) (eq (car t1) 'or))
         (if (every (lambda (x) (subtypep x t2)) (cdr t1)) (values t t) (values nil nil)))
        ((and (consp t2) (eq (car t2) 'and))
         (if (every (lambda (x) (subtypep t1 x)) (cdr t2)) (values t t) (values nil nil)))
        ((and (consp t1) (eq (car t1) 'and))
         (if (some (lambda (x) (subtypep x t2)) (cdr t1)) (values t t) (values nil nil)))
        ((and (consp t1) (eq (car t1) 'function))
         (if (subtypep 'function t2) (values t t) (values nil nil)))
        (t (values nil nil))))

(defun %expand-type-fully (ty)
  (multiple-value-bind (exp found) (%expand-deftype ty)
    (if found (%expand-type-fully exp) ty)))

(defparameter *subtype-table*
  '((fixnum integer rational real number atom)
    (bignum integer rational real number atom)
    (integer rational real number atom) (ratio rational real number atom)
    (rational real number atom) (float real number atom) (real number atom)
    (single-float float real number atom) (double-float float real number atom)
    (bit fixnum integer rational real number atom) (number atom)
    (character atom) (base-char character atom) (standard-char base-char character atom)
    (null symbol list sequence atom boolean) (symbol atom) (keyword symbol atom)
    (cons list sequence) (list sequence)
    (simple-string string vector array sequence atom) (string vector array sequence atom)
    (simple-vector vector array sequence atom) (vector array sequence atom)
    (bit-vector vector array sequence atom) (simple-bit-vector bit-vector vector array sequence atom)
    (hash-table atom) (function atom) (compiled-function function atom)))
