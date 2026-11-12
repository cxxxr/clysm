;;;; subtypep.lisp — SUBTYPEP by set algebra over a partition of the universe
;;;;
;;;; A type is represented as an alist from "categories" to constraints.  The
;;;; categories partition the universe:
;;;;   numeric (integer ratio single-float double-float): a list of intervals
;;;;       (lo lo-exclusive hi hi-exclusive), NIL bound = infinite
;;;;   set (base-char extended-char keyword symbol complex-*):
;;;;       (:in . objects) or (:out . objects) — (:out) is the whole category
;;;;   pattern categories (cons, (array element-type simple-p) and object, which
;;;;       holds everything else: functions, hash tables, instances, ...):
;;;;       a union of terms (pattern . excluded-patterns), see below.
;;;; Types that cannot be represented make CANON return :UNKNOWN.

(defparameter *numeric-categories* '(integer ratio single-float double-float))
(defparameter *set-categories* '(base-char extended-char keyword symbol
                                 complex-rational complex-single-float complex-double-float))
(defparameter *array-element-categories* '(t character bit (unsigned-byte 8) double-float))
(defparameter *array-categories*
  (let ((r nil))
    (dolist (e *array-element-categories* (nreverse r))
      (push (list 'array e t) r)
      (push (list 'array e nil) r))))
(defparameter *pattern-categories* (append '(cons object) *array-categories*))

(defun %numeric-cat-p (cat) (member cat *numeric-categories*))
(defun %set-cat-p (cat) (member cat *set-categories*))

(defun %iv (lo lox hi hix) (list lo lox hi hix))

(defun %iv-normalize-int (iv)
  "Integer intervals use inclusive integer bounds."
  (destructuring-bind (lo lox hi hix) iv
    (let ((lo (cond ((null lo) nil) (lox (+ (floor lo) 1)) (t (ceiling lo))))
          (hi (cond ((null hi) nil) (hix (- (ceiling hi) 1)) (t (floor hi)))))
      (if (and lo hi (> lo hi)) nil (list (%iv lo nil hi nil))))))

(defun %iv-empty-p (iv cat)
  (destructuring-bind (lo lox hi hix) iv
    (and lo hi
         (or (> lo hi)
             (and (= lo hi) (or lox hix))
             ;; no ratio is an integer
             (and (eq cat 'ratio) (= lo hi) (integerp lo))))))

(defun %iv-intersect (a b cat)
  (destructuring-bind (alo alox ahi ahix) a
    (destructuring-bind (blo blox bhi bhix) b
      (let* ((lo (cond ((null alo) blo) ((null blo) alo) ((> alo blo) alo) ((< alo blo) blo) (t alo)))
             (lox (cond ((null alo) blox) ((null blo) alox)
                        ((> alo blo) alox) ((< alo blo) blox) (t (or alox blox))))
             (hi (cond ((null ahi) bhi) ((null bhi) ahi) ((< ahi bhi) ahi) ((> ahi bhi) bhi) (t ahi)))
             (hix (cond ((null ahi) bhix) ((null bhi) ahix)
                        ((< ahi bhi) ahix) ((> ahi bhi) bhix) (t (or ahix bhix))))
             (iv (%iv lo lox hi hix)))
        (if (%iv-empty-p iv cat) nil (list iv))))))

(defun %ivs-intersect (as bs cat)
  (let ((r nil))
    (dolist (a as r)
      (dolist (b bs)
        (setq r (append r (%iv-intersect a b cat)))))))

(defun %ivs-complement (ivs cat)
  "Complement of a union of intervals within the category."
  (let ((result (list (%iv nil nil nil nil))))
    (dolist (iv ivs result)
      (destructuring-bind (lo lox hi hix) iv
        (let ((parts nil))
          (when lo (push (%iv nil nil lo (not lox)) parts))
          (when hi (push (%iv hi (not hix) nil nil) parts))
          (when (eq cat 'integer)
            (setq parts (mapcan #'%iv-normalize-int parts)))
          (setq result (%ivs-intersect result parts cat)))))))

;;; ------------------------------------------------------------
;;; pattern categories
;;; A constraint is a list of terms (pattern . excluded), meaning the union
;;; of (pattern minus the union of excluded).  Patterns:
;;;   :all                     the whole category
;;;   (:eql object)            one object
;;;   cons:   (:cons car cdr)  with canonical car and cdr types
;;;   arrays: (:dims rank . dims), rank an integer or :any, dims integers or *
;;;   object: (:class class)
;;; ------------------------------------------------------------

(defun %eql-in-pattern-p (x p)
  (cond ((eq p :all) t)
        ((eq (car p) :eql) (eql x (cadr p)))
        ((eq (car p) :class) (%subclassp (class-of x) (cadr p)))
        ((eq (car p) :dims)
         (let ((rank (cadr p)) (dims (cddr p)))
           (or (eq rank :any)
               (and (= (array-rank x) rank)
                    (every (lambda (d ad) (or (eq d '*) (= d ad))) dims (array-dimensions x))))))
        (t nil)))

(defun %common-subclasses (c1 c2)
  "The maximal classes that are subclasses of both C1 and C2."
  (let ((found nil) (seen nil))
    (labels ((walk (c)
               (unless (member c seen)
                 (push c seen)
                 (if (%subclassp c c2)
                     (push c found)
                     (dolist (s (%class-slot c +class-subclasses+)) (walk s))))))
      (dolist (s (%class-slot c1 +class-subclasses+)) (walk s)))
    (remove-if (lambda (c) (some (lambda (d) (and (not (eq c d)) (%subclassp c d))) found)) found)))

(defun %pat-intersect (p q)
  "List of patterns whose union is the intersection of P and Q."
  (cond ((eq p :all) (list q))
        ((eq q :all) (list p))
        ((eq (car p) :eql) (if (%eql-in-pattern-p (cadr p) q) (list p) nil))
        ((eq (car q) :eql) (if (%eql-in-pattern-p (cadr q) p) (list q) nil))
        ((eq (car p) :class)
         (let ((a (cadr p)) (b (cadr q)))
           (cond ((%subclassp a b) (list p))
                 ((%subclassp b a) (list q))
                 (t (mapcar (lambda (c) (list :class c)) (%common-subclasses a b))))))
        ((eq (car p) :cons)
         (let ((a (%type-and (cadr p) (cadr q))) (d (%type-and (caddr p) (caddr q))))
           (if (or (null a) (null d)) nil (list (list :cons a d)))))
        ((eq (car p) :dims)
         (let ((r1 (cadr p)) (r2 (cadr q)))
           (cond ((eq r1 :any) (list q))
                 ((eq r2 :any) (list p))
                 ((/= r1 r2) nil)
                 (t (let ((dims nil))
                      (do ((a (cddr p) (cdr a)) (b (cddr q) (cdr b)))
                          ((null a) (list (list* :dims r1 (nreverse dims))))
                        (cond ((eq (car a) '*) (push (car b) dims))
                              ((or (eq (car b) '*) (= (car a) (car b))) (push (car a) dims))
                              (t (return nil)))))))))
        (t nil)))

(defun %pat-empty-p (p)
  (and (consp p) (eq (car p) :cons) (or (null (cadr p)) (null (caddr p)))))

(defun %dims-covered-p (dims qs)
  "QS: lists of dims of the same length as DIMS."
  (cond ((null qs) nil)
        ((null dims) t)
        ((eq (car dims) '*)
         (%dims-covered-p (cdr dims) (mapcar #'cdr (remove-if-not (lambda (q) (eq (car q) '*)) qs))))
        (t (%dims-covered-p (cdr dims)
                            (mapcar #'cdr (remove-if-not (lambda (q) (or (eq (car q) '*) (eql (car q) (car dims))))
                                                         qs))))))

(defun %pat-covered-p (p qs)
  "True when pattern P is a subset of the union of the patterns QS."
  (cond ((%pat-empty-p p) t)
        ((member :all qs) t)
        ((eq p :all)
         ;; the whole category: all arrays of any rank, or all conses
         (cond ((some (lambda (q) (and (eq (car q) :dims) (eq (cadr q) :any))) qs) t)
               ((some (lambda (q) (eq (car q) :cons)) qs)
                (%pat-covered-p (list :cons (%canon-top) (%canon-top)) qs))
               (t nil)))
        ((eq (car p) :eql) (some (lambda (q) (%eql-in-pattern-p (cadr p) q)) qs))
        ((eq (car p) :class)
         (some (lambda (q) (and (eq (car q) :class) (%subclassp (cadr p) (cadr q)))) qs))
        ((eq (car p) :dims)
         (let ((rank (cadr p)) (qs (remove-if-not (lambda (q) (eq (car q) :dims)) qs)))
           (cond ((some (lambda (q) (eq (cadr q) :any)) qs) t)
                 ((eq rank :any) nil)
                 (t (%dims-covered-p (cddr p) (mapcar #'cddr (remove-if-not (lambda (q) (eql (cadr q) rank)) qs)))))))
        ((eq (car p) :cons)
         ;; split P along the first cons pattern that meets it
         (let ((qs (remove-if-not (lambda (q) (eq (car q) :cons)) qs)))
           (let ((q (find-if (lambda (q) (%pat-intersect p q)) qs)))
             (if (null q)
                 nil
                 (let* ((a (cadr p)) (b (caddr p)) (c (cadr q)) (d (caddr q))
                        (others (remove q qs :count 1)))
                   (and (%pat-covered-p (list :cons (%type-and a (%type-not c)) b) others)
                        (%pat-covered-p (list :cons (%type-and a c) (%type-and b (%type-not d))) others)))))))
        (t nil)))

(defun %term-empty-p (term) (%pat-covered-p (car term) (cdr term)))

(defun %terms-intersect (as bs)
  (let ((r nil))
    (dolist (a as (nreverse r))
      (dolist (b bs)
        (dolist (p (%pat-intersect (car a) (car b)))
          (let ((term (cons p (remove-if-not (lambda (n) (%pat-intersect p n)) (union (cdr a) (cdr b) :test #'equal)))))
            (unless (%term-empty-p term) (push term r))))))))

(defun %terms-complement (ts)
  (let ((result (list (list :all))))
    (dolist (term ts result)
      (setq result (%terms-intersect result
                                     (cons (list :all (car term))
                                           (mapcar #'list (cdr term))))))))

;;; ------------------------------------------------------------
;;; generic category operations
;;; ------------------------------------------------------------

(defun %cat-empty-p (cat c)
  (cond ((%numeric-cat-p cat) (null c))
        ((%set-cat-p cat) (or (null c) (and (eq (car c) :in) (null (cdr c)))))
        (t (every #'%term-empty-p c))))

(defun %cat-union (cat a b)
  (cond ((null a) b) ((null b) a)
        ((%numeric-cat-p cat) (append a b))
        ((%set-cat-p cat)
         (cond ((and (eq (car a) :in) (eq (car b) :in)) (cons :in (union (cdr a) (cdr b))))
               ((and (eq (car a) :out) (eq (car b) :out)) (cons :out (intersection (cdr a) (cdr b))))
               ((eq (car a) :in) (cons :out (set-difference (cdr b) (cdr a))))
               (t (cons :out (set-difference (cdr a) (cdr b))))))
        (t (append a b))))

(defun %cat-intersect (cat a b)
  (cond ((or (null a) (null b)) nil)
        ((%numeric-cat-p cat) (%ivs-intersect a b cat))
        ((%set-cat-p cat)
         (cond ((and (eq (car a) :in) (eq (car b) :in)) (cons :in (intersection (cdr a) (cdr b))))
               ((and (eq (car a) :out) (eq (car b) :out)) (cons :out (union (cdr a) (cdr b))))
               ((eq (car a) :in) (cons :in (set-difference (cdr a) (cdr b))))
               (t (cons :in (set-difference (cdr b) (cdr a))))))
        (t (%terms-intersect a b))))

(defun %cat-complement (cat c)
  (cond ((null c) (%cat-whole cat))
        ((%numeric-cat-p cat) (%ivs-complement c cat))
        ((%set-cat-p cat) (if (eq (car c) :in) (cons :out (cdr c)) (cons :in (cdr c))))
        (t (%terms-complement c))))

(defun %cat-whole (cat)
  (cond ((%numeric-cat-p cat) (list (%iv nil nil nil nil)))
        ((%set-cat-p cat) (list :out))
        (t (list (list :all)))))

(defun %all-categories () (append *numeric-categories* *set-categories* *pattern-categories*))

(defun %type-get (ty cat) (cdr (assoc cat ty :test #'equal)))

(defun %type-op (fn a b)
  (let ((r nil))
    (dolist (cat (%all-categories) r)
      (let ((x (%type-get a cat)) (y (%type-get b cat)))
        (when (or x y)
          (let ((c (funcall fn cat x y)))
            (unless (%cat-empty-p cat c) (push (cons cat c) r))))))))

(defun %type-and (a b) (%type-op #'%cat-intersect a b))
(defun %type-or (a b) (%type-op #'%cat-union a b))

(defun %type-not (a)
  (let ((r nil))
    (dolist (cat (%all-categories) r)
      (let ((c (%cat-complement cat (%type-get a cat))))
        (unless (%cat-empty-p cat c) (push (cons cat c) r))))))

(defun %canon-top ()
  (let ((r nil)) (dolist (c (%all-categories) r) (push (cons c (%cat-whole c)) r))))

;;; ------------------------------------------------------------
;;; canonicalization of type specifiers
;;; ------------------------------------------------------------

(defun %real-bounds (args)
  (let ((lo (if args (car args) '*)) (hi (if (cdr args) (cadr args) '*)))
    (flet ((b (x) (cond ((eq x '*) (values nil nil))
                        ((and (consp x) (realp (car x)) (null (cdr x))) (values (rational (car x)) t))
                        ((realp x) (values (rational x) nil))
                        (t (return-from %real-bounds :unknown)))))
      (multiple-value-bind (l lx) (b lo)
        (multiple-value-bind (h hx) (b hi)
          (%iv l lx h hx))))))

(defun %numeric-type (cats args)
  (let ((iv (%real-bounds args)) (r nil))
    (when (eq iv :unknown) (return-from %numeric-type :unknown))
    (dolist (cat cats r)
      (let ((ivs (if (eq cat 'integer) (%iv-normalize-int iv) (if (%iv-empty-p iv cat) nil (list iv)))))
        (when ivs (push (cons cat ivs) r))))))

(defun %array-elt-category (x)
  (let ((e (array-element-type x)))
    (if (member e *array-element-categories* :test #'equal) e t)))

(defun %object-category (x)
  "The category of the object X and its constraint as a member of a set."
  (cond ((integerp x) (values 'integer (list (%iv x nil x nil))))
        ((%ratiop x) (values 'ratio (list (%iv x nil x nil))))
        ((%single-float-p x) (values 'single-float (list (%iv (rational x) nil (rational x) nil))))
        ((%double-float-p x) (values 'double-float (list (%iv (rational x) nil (rational x) nil))))
        ((keywordp x) (values 'keyword (list :in x)))
        ((symbolp x) (values 'symbol (list :in x)))
        ((characterp x) (values (if (< (char-code x) 128) 'base-char 'extended-char) (list :in x)))
        ((%complexp x) (values (cond ((%single-float-p (realpart x)) 'complex-single-float)
                                     ((%double-float-p (realpart x)) 'complex-double-float)
                                     (t 'complex-rational))
                               (list :in x)))
        ((consp x) (values :unknown nil))
        ((arrayp x) (values (list 'array (%array-elt-category x) (not (null (%simple-array-p x))))
                            (list (list (list :eql x)))))
        (t (values 'object (list (list (list :eql x)))))))

(defun %member-type (objects)
  (let ((r nil))
    (dolist (x objects r)
      (multiple-value-bind (cat c) (%object-category x)
        (when (eq cat :unknown) (return-from %member-type :unknown))
        (setq r (%type-or r (list (cons cat c))))))))

(defun %element-categories (et)
  "The array element categories denoted by the element type ET."
  (if (eq et '*)
      *array-element-categories*
      (let ((u (upgraded-array-element-type et)))
        (list (if (member u *array-element-categories* :test #'equal) u t)))))

(defun %array-type (et dims simple-only)
  (let* ((pattern (cond ((eq dims '*) (list :dims :any))
                        ((integerp dims) (list* :dims dims (make-list dims :initial-element '*)))
                        ((listp dims)
                         (unless (every (lambda (d) (or (eq d '*) (integerp d))) dims)
                           (return-from %array-type :unknown))
                         (list* :dims (length dims) dims))
                        (t (return-from %array-type :unknown))))
         (r nil))
    (dolist (e (%element-categories et) r)
      (push (cons (list 'array e t) (list (list pattern))) r)
      (unless simple-only (push (cons (list 'array e nil) (list (list pattern))) r)))))

(defun %vector-type (et size simple-only)
  (%array-type et (list (if (or (null size) (eq size '*)) '* size)) simple-only))

(defun %complex-type (args)
  (if (or (null args) (eq (car args) '*))
      (list (cons 'complex-rational (list :out)) (cons 'complex-single-float (list :out))
            (cons 'complex-double-float (list :out)))
      (let ((part (%canon (car args))) (r nil))
        (when (eq part :unknown) (return-from %complex-type :unknown))
        (when (or (%type-get part 'integer) (%type-get part 'ratio))
          (push (cons 'complex-rational (list :out)) r))
        (when (%type-get part 'single-float) (push (cons 'complex-single-float (list :out)) r))
        (when (%type-get part 'double-float) (push (cons 'complex-double-float (list :out)) r))
        r)))

(defparameter *standard-chars*
  (let ((r (list #\Newline)))
    (do ((i 32 (+ i 1))) ((> i 126) r) (push (code-char i) r))))

(defun %class-type (class)
  (let ((name (%class-name class)))
    (if (and name (eq (find-class name nil) class) (member name '(t)))
        (%canon-top)
        (list (cons 'object (list (list (list :class class))))))))

(defun %canon (ty)
  "Canonical alist for TY, or :UNKNOWN."
  (setq ty (%expand-type-fully (%type-class-name ty)))
  (when (%class-p ty) (return-from %canon (%class-type ty)))
  (let ((args (if (consp ty) (cdr ty) nil))
        (head (if (consp ty) (car ty) ty)))
    (unless (listp args) (return-from %canon :unknown))
    (flet ((arg (i) (let ((a (nthcdr i args))) (if a (car a) '*)))
           (nargs () (length args)))
      (case head
        ((t) (%canon-top))
        ((nil) nil)
        (and (let ((members (find-if (lambda (x) (and (consp x) (member (car x) '(member eql)))) args)))
               (if members
                   ;; a finite set: keep the elements of the intersection
                   (%member-type (remove-if-not (lambda (x) (funcall '%safe-typep x ty))
                                                (if (eq (car members) 'eql) (cdr members) (cdr members))))
                   (let ((r (%canon-top)))
                     (dolist (x args r)
                       (let ((c (%canon x))) (when (eq c :unknown) (return :unknown))
                         (setq r (%type-and r c))))))))
        (or (let ((r nil))
              (dolist (x args r)
                (let ((c (%canon x))) (when (eq c :unknown) (return :unknown))
                  (setq r (%type-or r c))))))
        (not (if (/= (nargs) 1) :unknown
                 (let ((c (%canon (car args)))) (if (eq c :unknown) :unknown (%type-not c)))))
        (member (%member-type args))
        (eql (if (/= (nargs) 1) :unknown (%member-type args)))
        (integer (%numeric-type '(integer) args))
        ((fixnum) (if args :unknown (%numeric-type '(integer) (list most-negative-fixnum most-positive-fixnum))))
        ((bignum) (if args :unknown (%canon `(and integer (not fixnum)))))
        ((bit) (if args :unknown (%numeric-type '(integer) '(0 1))))
        (mod (if (and (= (nargs) 1) (integerp (car args)) (> (car args) 0))
                 (%numeric-type '(integer) (list 0 (list (car args))))
                 :unknown))
        (unsigned-byte (cond ((or (null args) (eq (car args) '*)) (%numeric-type '(integer) '(0 *)))
                             ((and (integerp (car args)) (> (car args) 0))
                              (%numeric-type '(integer) (list 0 (- (expt 2 (car args)) 1))))
                             (t :unknown)))
        (signed-byte (cond ((or (null args) (eq (car args) '*)) (%numeric-type '(integer) nil))
                           ((and (integerp (car args)) (> (car args) 0))
                            (let ((n (expt 2 (- (car args) 1)))) (%numeric-type '(integer) (list (- n) (- n 1)))))
                           (t :unknown)))
        (ratio (if args :unknown (%numeric-type '(ratio) nil)))
        (rational (%numeric-type '(integer ratio) args))
        ((single-float short-float) (%numeric-type '(single-float) args))
        ((double-float long-float) (%numeric-type '(double-float) args))
        (float (%numeric-type '(single-float double-float) args))
        (real (%numeric-type '(integer ratio single-float double-float) args))
        (number (if args :unknown
                    (%type-or (%numeric-type '(integer ratio single-float double-float) nil)
                              (%complex-type nil))))
        (complex (if (> (nargs) 1) :unknown (%complex-type args)))
        (character (if args :unknown (list (cons 'base-char (list :out)) (cons 'extended-char (list :out)))))
        ((base-char) (if args :unknown (list (cons 'base-char (list :out)))))
        ((standard-char) (if args :unknown (list (cons 'base-char (cons :in *standard-chars*)))))
        ((extended-char) (if args :unknown (list (cons 'extended-char (list :out)))))
        (symbol (if args :unknown (list (cons 'symbol (list :out)) (cons 'keyword (list :out)))))
        (keyword (if args :unknown (list (cons 'keyword (list :out)))))
        (null (if args :unknown (list (cons 'symbol (list :in nil)))))
        (boolean (if args :unknown (list (cons 'symbol (list :in nil t)))))
        (cons (if (> (nargs) 2) :unknown
                  (let ((a (if (eq (arg 0) '*) (%canon-top) (%canon (arg 0))))
                        (d (if (eq (arg 1) '*) (%canon-top) (%canon (arg 1)))))
                    (cond ((or (eq a :unknown) (eq d :unknown)) :unknown)
                          ((or (null a) (null d)) nil)
                          (t (list (cons 'cons (list (list (list :cons a d))))))))))
        (list (if args :unknown (list (cons 'cons (list (list :all))) (cons 'symbol (list :in nil)))))
        (atom (if args :unknown (%type-not (%canon 'cons))))
        (sequence (if args :unknown (%type-or (%canon 'list) (%array-type '* '(*) nil))))
        (array (if (> (nargs) 2) :unknown (%array-type (arg 0) (arg 1) nil)))
        (simple-array (if (> (nargs) 2) :unknown (%array-type (arg 0) (arg 1) t)))
        (vector (if (> (nargs) 2) :unknown (%vector-type (arg 0) (arg 1) nil)))
        (simple-vector (if (> (nargs) 1) :unknown (%vector-type t (arg 0) t)))
        ((string base-string) (if (> (nargs) 1) :unknown (%vector-type 'character (arg 0) nil)))
        ((simple-string simple-base-string) (if (> (nargs) 1) :unknown (%vector-type 'character (arg 0) t)))
        (bit-vector (if (> (nargs) 1) :unknown (%vector-type 'bit (arg 0) nil)))
        (simple-bit-vector (if (> (nargs) 1) :unknown (%vector-type 'bit (arg 0) t)))
        (otherwise
         (let ((c (and (symbolp ty) (find-class ty nil))))
           (if c (%class-type c) :unknown)))))))

(defun %subtypep-by-algebra (t1 t2)
  "Returns (values result certain) or NIL as the first value of a third :UNKNOWN."
  (let ((c1 (%canon t1)) (c2 (%canon t2)))
    (if (or (eq c1 :unknown) (eq c2 :unknown))
        (values nil nil :unknown)
        (values (null (%type-and c1 (%type-not c2))) t nil))))
