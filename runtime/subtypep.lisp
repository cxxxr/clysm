;;;; subtypep.lisp — SUBTYPEP by set algebra over a partition of the universe
;;;;
;;;; A type is represented as an alist from "categories" to constraints:
;;;;   numeric categories (integer ratio single-float double-float):
;;;;       a list of intervals (lo lo-exclusive hi hi-exclusive), NIL bound = infinite
;;;;   set categories (symbol base-char extended-char complex cons other...):
;;;;       (:in . objects) or (:out . objects)  — (:out) is the whole category
;;;; Types that cannot be represented make CANON return :UNKNOWN.

(defparameter *numeric-categories* '(integer ratio single-float double-float))
(defparameter *set-categories* '(symbol base-char extended-char complex cons other)
  "OTHER stands for every object outside the modeled categories (arrays, functions, ...).")

(defun %iv (lo lox hi hix) (list lo lox hi hix))

(defun %bound< (a b) (< a b))

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
             (and (eq cat 'integer) (> lo hi))))))

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

(defun %cat-empty-p (cat c)
  (if (member cat *numeric-categories*)
      (null c)
      (or (null c) (and (eq (car c) :in) (null (cdr c))))))

(defun %cat-union (cat a b)
  (cond ((null a) b) ((null b) a)
        ((member cat *numeric-categories*) (append a b))
        ((and (eq (car a) :in) (eq (car b) :in)) (cons :in (union (cdr a) (cdr b))))
        ((and (eq (car a) :out) (eq (car b) :out)) (cons :out (intersection (cdr a) (cdr b))))
        ((eq (car a) :in) (cons :out (set-difference (cdr b) (cdr a))))
        (t (cons :out (set-difference (cdr a) (cdr b))))))

(defun %cat-intersect (cat a b)
  (cond ((or (null a) (null b)) nil)
        ((member cat *numeric-categories*) (%ivs-intersect a b cat))
        ((and (eq (car a) :in) (eq (car b) :in)) (cons :in (intersection (cdr a) (cdr b))))
        ((and (eq (car a) :out) (eq (car b) :out)) (cons :out (union (cdr a) (cdr b))))
        ((eq (car a) :in) (cons :in (set-difference (cdr a) (cdr b))))
        (t (cons :in (set-difference (cdr b) (cdr a))))))

(defun %cat-complement (cat c)
  (cond ((null c) (%cat-whole cat))
        ((member cat *numeric-categories*) (%ivs-complement c cat))
        ((eq (car c) :in) (cons :out (cdr c)))
        (t (cons :in (cdr c)))))

(defun %cat-whole (cat)
  (if (member cat *numeric-categories*) (list (%iv nil nil nil nil)) (list :out)))

(defun %all-categories () (append *numeric-categories* *set-categories*))

(defun %type-make (alist) alist)
(defun %type-get (ty cat) (cdr (assoc cat ty)))

(defun %type-op (fn a b)
  (let ((r nil))
    (dolist (cat (%all-categories) r)
      (let ((c (funcall fn cat (%type-get a cat) (%type-get b cat))))
        (unless (%cat-empty-p cat c) (push (cons cat c) r))))))

(defun %type-not (a)
  (let ((r nil))
    (dolist (cat (%all-categories) r)
      (let ((c (%cat-complement cat (%type-get a cat))))
        (unless (%cat-empty-p cat c) (push (cons cat c) r))))))

(defun %type-empty-alist-p (a) (null a))

(defun %real-bounds (args)
  (let ((lo (if args (car args) '*)) (hi (if (cdr args) (cadr args) '*)))
    (flet ((b (x) (cond ((eq x '*) (values nil nil))
                        ((consp x) (values (rational (car x)) t))
                        (t (values (rational x) nil)))))
      (multiple-value-bind (l lx) (b lo)
        (multiple-value-bind (h hx) (b hi)
          (%iv l lx h hx))))))

(defun %numeric-type (cats args)
  (let ((iv (%real-bounds args)) (r nil))
    (dolist (cat cats r)
      (let ((ivs (if (eq cat 'integer) (%iv-normalize-int iv) (if (%iv-empty-p iv cat) nil (list iv)))))
        (when ivs (push (cons cat ivs) r))))))

(defun %member-type (objects)
  (let ((r nil))
    (dolist (x objects r)
      (let ((part (cond ((integerp x) (list (cons 'integer (list (%iv x nil x nil)))))
                        ((%ratiop x) (list (cons 'ratio (list (%iv x nil x nil)))))
                        ((%single-float-p x) (if (zerop x) (return-from %member-type :unknown)
                                                 (list (cons 'single-float (list (%iv (rational x) nil (rational x) nil))))))
                        ((%double-float-p x) (if (zerop x) (return-from %member-type :unknown)
                                                 (list (cons 'double-float (list (%iv (rational x) nil (rational x) nil))))))
                        ((symbolp x) (list (cons 'symbol (list :in x))))
                        ((characterp x) (list (cons 'base-char (list :in x))))
                        ((consp x) (list (cons 'cons (list :in x))))
                        ((complexp x) (list (cons 'complex (list :in x))))
                        (t (return-from %member-type :unknown)))))
        (setq r (%type-op #'%cat-union r part))))))

(defun %canon (ty)
  "Canonical alist for TY, or :UNKNOWN."
  (setq ty (%expand-type-fully (%type-class-name ty)))
  (flet ((args () (if (consp ty) (cdr ty) nil))
         (head () (if (consp ty) (car ty) ty)))
    (case (head)
      ((t) (let ((r nil)) (dolist (c (%all-categories) r) (push (cons c (%cat-whole c)) r))))
      ((nil) nil)
      (and (let ((r (%canon t)))
             (dolist (x (args) r)
               (let ((c (%canon x))) (when (eq c :unknown) (return :unknown))
                 (setq r (%type-op #'%cat-intersect r c))))))
      (or (let ((r nil))
            (dolist (x (args) r)
              (let ((c (%canon x))) (when (eq c :unknown) (return :unknown))
                (setq r (%type-op #'%cat-union r c))))))
      (not (let ((c (%canon (car (args))))) (if (eq c :unknown) :unknown (%type-not c))))
      (member (%member-type (args)))
      (eql (%member-type (args)))
      (integer (%numeric-type '(integer) (args)))
      ((fixnum) (%numeric-type '(integer) (list most-negative-fixnum most-positive-fixnum)))
      ((bignum) (%canon `(and integer (not fixnum))))
      ((bit) (%numeric-type '(integer) '(0 1)))
      (mod (%numeric-type '(integer) (list 0 (list (car (args))))))
      (unsigned-byte (if (or (null (args)) (eq (car (args)) '*))
                         (%numeric-type '(integer) '(0 *))
                         (%numeric-type '(integer) (list 0 (- (expt 2 (car (args))) 1)))))
      (signed-byte (if (or (null (args)) (eq (car (args)) '*))
                       (%numeric-type '(integer) nil)
                       (let ((n (expt 2 (- (car (args)) 1)))) (%numeric-type '(integer) (list (- n) (- n 1))))))
      (ratio (if (args) :unknown (%numeric-type '(ratio) nil)))
      (rational (%numeric-type '(integer ratio) (args)))
      ((single-float short-float) (%numeric-type '(single-float) (args)))
      ((double-float long-float) (%numeric-type '(double-float) (args)))
      (float (%numeric-type '(single-float double-float) (args)))
      (real (%numeric-type '(integer ratio single-float double-float) (args)))
      (number (if (args) :unknown
                  (%type-op #'%cat-union (%numeric-type '(integer ratio single-float double-float) nil)
                            (list (cons 'complex (list :out))))))
      (complex (if (and (args) (not (eq (car (args)) '*))) :unknown (list (cons 'complex (list :out)))))
      (character (if (args) :unknown (list (cons 'base-char (list :out)) (cons 'extended-char (list :out)))))
      ;; every character is a base character: EXTENDED-CHAR is empty
      ((base-char) (list (cons 'base-char (list :out)) (cons 'extended-char (list :out))))
      ((extended-char) nil)
      (symbol (list (cons 'symbol (list :out))))
      (null (list (cons 'symbol (list :in nil))))
      (boolean (list (cons 'symbol (list :in nil t))))
      (cons (if (and (args) (not (and (eq (car (args)) '*) (or (null (cdr (args))) (eq (cadr (args)) '*)))))
                :unknown
                (list (cons 'cons (list :out)))))
      (list (list (cons 'cons (list :out)) (cons 'symbol (list :in nil))))
      (atom (%canon '(not cons)))
      (otherwise :unknown))))

(defun %subtypep-by-algebra (t1 t2)
  "Returns (values result certain) or NIL as the first value of a third :UNKNOWN."
  (let ((c1 (%canon t1)) (c2 (%canon t2)))
    (if (or (eq c1 :unknown) (eq c2 :unknown))
        (values nil nil :unknown)
        (values (null (%type-op #'%cat-intersect c1 (%type-not c2))) t nil))))
