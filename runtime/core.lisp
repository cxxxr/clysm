;;;; core.lisp — primitive wrappers, lists, symbols, plists

;;; ------------------------------------------------------------
;;; Function versions of primitives (for FUNCALL/#')
;;; ------------------------------------------------------------

(defmacro define-primitive-functions (&rest specs)
  `(progn ,@(mapcar (lambda (s) `(defun ,(car s) ,(cdr s) (,(car s) ,@(cdr s)))) specs)))

(define-primitive-functions
  (eq a b) (eql a b) (null x) (not x) (consp x) (atom x) (listp x) (symbolp x)
  (functionp x) (characterp x) (fixnump x)
  (cons a b) (car x) (cdr x) (first x) (rest x)
  (caar x) (cadr x) (cdar x) (cddr x) (caddr x) (cdddr x) (caadr x) (cddar x) (cdadr x)
  (cadar x) (caaar x) (cdaar x) (cadddr x) (cddddr x)
  (second x) (third x) (fourth x)
  (rplaca c v) (rplacd c v) (%setcar c v) (%setcdr c v)
  (1+ x) (1- x) (zerop x) (plusp x) (minusp x)
  (char-code c) (symbol-name s) (symbol-plist s)
  (boundp s) (%set-symbol-value s v) (%set-symbol-plist s v) (%set-symbol-function s f)
  (values-list l))

(macrolet ((define-cxrs ()
             (let ((defs nil))
               (labels ((gen (path depth)
                          (when (<= 2 depth 4)
                            (let ((name (intern (concatenate 'string "C" path "R") "COMMON-LISP")))
                              (unless (member name '(caar cadr cdar cddr caddr cdddr caadr cddar cdadr
                                                     cadar caaar cdaar cadddr cddddr))
                                (push `(defun ,name (x)
                                         ,(let ((form 'x))
                                            (dolist (c (reverse (coerce path 'list)) form)
                                              (setq form (list (if (char= c #\A) 'car 'cdr) form)))))
                                      defs))))
                          (when (< depth 4)
                            (gen (concatenate 'string path "A") (+ depth 1))
                            (gen (concatenate 'string path "D") (+ depth 1)))))
                 (gen "" 0))
               `(progn ,@defs))))
  (define-cxrs))

(defun code-char (n)
  (if (and (fixnump n) (>= n 0) (< n #x110000)) (code-char n) nil))

(defun symbol-value (s) (symbol-value s))
(defun set (s v) (%set-symbol-value s v))
(defun makunbound (s) (%makunbound s))

(defun values (&rest vals) (values-list vals))
(defun identity (x) x)
(defun constantly (value) (lambda (&rest args) (declare (ignore args)) value))
(defun complement (f) (lambda (&rest args) (not (apply f args))))

(defun list (&rest args) args)
(defun list* (arg &rest others)
  (if (null others)
      arg
      (let* ((head (cons arg nil)) (tail head))
        (do ((l others (cdr l)))
            ((null (cdr l)) (%setcdr tail (car l)) head)
          (let ((c (cons (car l) nil)))
            (%setcdr tail c)
            (setq tail c))))))

(defun funcall (f &rest args) (apply f args))
(defun apply (f &rest args)
  (apply f (apply #'list* args)))

;;; strict end-of-list test: a non-NIL atom where a list was expected is an error
(defmacro %endp (x)
  `(cond ((consp ,x) nil) ((null ,x) t) (t (%type-error ,x 'list))))

;;; ------------------------------------------------------------
;;; Early error support (replaced by the condition system later)
;;; ------------------------------------------------------------

(defun %early-write-string (s fd)
  (dotimes (i (%string-length s)) (%write-char (%schar s i) fd)))

(defun %member-eql (x list)
  (do ((l list (cdr l))) ((%endp l) nil)
    (when (eql x (car l)) (return l))))

;;; ------------------------------------------------------------
;;; Lists
;;; ------------------------------------------------------------

(defun endp (x)
  (cond ((null x) t)
        ((consp x) nil)
        (t (%type-error x 'list))))

(defun length-of-list (l)
  (let ((n 0))
    (do ((x l (cdr x))) ((%endp x) n) (setq n (+ n 1)))))

(defun list-length (l)
  (do ((n 0 (+ n 2))
       (fast l (cddr fast))
       (slow l (cdr slow)))
      (nil)
    (when (endp fast) (return n))
    (when (endp (cdr fast)) (return (+ n 1)))
    (when (and (eq fast slow) (> n 0)) (return nil))))

(defun nth (n list)
  (do ((i n (1- i)) (l list (cdr l))) ((or (<= i 0) (%endp l)) (car l))))
(defun nthcdr (n list)
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (do ((i n (1- i)) (l list (cdr l))) ((or (<= i 0) (%endp l)) l)))
(defun %setnth (n list value) (%setcar (nthcdr n list) value))

(defun fifth (l) (nth 4 l)) (defun sixth (l) (nth 5 l)) (defun seventh (l) (nth 6 l))
(defun eighth (l) (nth 7 l)) (defun ninth (l) (nth 8 l)) (defun tenth (l) (nth 9 l))
(defsetf fifth (l) (v) `(%setnth 4 ,l ,v))
(defsetf sixth (l) (v) `(%setnth 5 ,l ,v))
(defsetf seventh (l) (v) `(%setnth 6 ,l ,v))
(defsetf eighth (l) (v) `(%setnth 7 ,l ,v))
(defsetf ninth (l) (v) `(%setnth 8 ,l ,v))
(defsetf tenth (l) (v) `(%setnth 9 ,l ,v))
(defsetf nthcdr (n l) (v) `(%setcdr (nthcdr (1- ,n) ,l) ,v))

(defun last (list &optional (n 1))
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (unless (listp list) (%type-error list 'list))
  (let ((count 0))
    (do ((l list (cdr l))) ((atom l)) (setq count (+ count 1)))
    (let ((l list))
      (dotimes (i (max 0 (- count n)) l) (setq l (cdr l))))))

(defun %dotted-length (l)
  (unless (listp l) (%type-error l 'list))
  (do ((n 0 (+ n 1)) (x l (cdr x))) ((atom x) n)))

(defun butlast (list &optional (n 1))
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (let ((len (%dotted-length list)))
    (if (<= len n) nil
        (let ((result nil) (l list))
          (dotimes (i (- len n)) (push (car l) result) (setq l (cdr l)))
          (nreverse result)))))

(defun nbutlast (list &optional (n 1))
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (let ((len (%dotted-length list)))
    (if (<= len n) nil
        (progn (%setcdr (nthcdr (- len n 1) list) nil) list))))

(defun copy-list (list)
  (if (atom list) list
      (let* ((head (cons (car list) nil)) (tail head))
        (do ((l (cdr list) (cdr l))) ((atom l) (%setcdr tail l) head)
          (let ((c (cons (car l) nil))) (%setcdr tail c) (setq tail c))))))

(defun copy-alist (alist)
  (mapcar (lambda (x) (if (consp x) (cons (car x) (cdr x)) x)) alist))

(defun copy-tree (tree)
  (if (consp tree) (cons (copy-tree (car tree)) (copy-tree (cdr tree))) tree))

(defun %append2 (a b)
  (if (null a) b
      (let* ((head (cons (car a) nil)) (tail head))
        (do ((l (cdr a) (cdr l))) ((%endp l) (%setcdr tail b) head)
          (let ((c (cons (car l) nil))) (%setcdr tail c) (setq tail c))))))

(defun append (&rest lists)
  (cond ((null lists) nil)
        ((null (cdr lists)) (car lists))
        (t (%append2 (car lists) (apply #'append (cdr lists))))))

(defun nconc (&rest lists)
  (let ((result nil) (tail nil))
    (dolist (l lists result)
      (when l
        (if tail
            (progn (%setcdr (last tail) l) (setq tail l))
            (setq result l tail l))))))

(defun revappend (x y)
  (do ((l x (cdr l)) (acc y (cons (car l) acc))) ((%endp l) acc)))
(defun nreconc (x y) (revappend x y))

(defun reverse-list (l)
  (let ((acc nil)) (do ((x l (cdr x))) ((%endp x) acc) (setq acc (cons (car x) acc)))))

(defun nreverse-list (l)
  (let ((prev nil))
    (do () ((%endp l) prev)
      (let ((next (cdr l)))
        (%setcdr l prev)
        (setq prev l l next)))))

(defun ldiff (list object)
  (unless (listp list) (%type-error list 'list))
  (let ((result nil))
    (do ((l list (cdr l)))
        ((or (atom l) (eql l object))
         (let ((r (nreverse-list result)))
           (when (and (not (eql l object)) l) (%setcdr (last r) l))
           r))
      (push (car l) result))))

(defun tailp (object list)
  (do ((l list (cdr l)))
      ((atom l) (eql l object))
    (when (eql l object) (return t))))

(defun make-list (n &key initial-element)
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (let ((r nil)) (dotimes (i n r) (setq r (cons initial-element r)))))

(defun %test-fn (test test-not)
  (cond (test (if (functionp test) test (fdefinition test)))
        (test-not (let ((tn (if (functionp test-not) test-not (fdefinition test-not))))
                    (lambda (a b) (not (funcall tn a b)))))
        (t #'eql)))

(defun %key-fn (key) (cond ((null key) nil) ((functionp key) key) (t (fdefinition key))))

(defmacro %with-key ((var key) x) `(if ,var (funcall ,var ,x) ,x))

(defun member (item list &key key test test-not)
  (if (and (null key) (null test) (null test-not))
      (%member-eql item list)
      (let ((tf (%test-fn test test-not)) (kf (%key-fn key)))
        (do ((l list (cdr l))) ((%endp l) nil)
          (when (funcall tf item (%with-key (kf key) (car l))) (return l))))))

(defun member-if (pred list &key key)
  (let ((kf (%key-fn key)))
    (do ((l list (cdr l))) ((%endp l) nil)
      (when (funcall pred (%with-key (kf key) (car l))) (return l)))))
(defun member-if-not (pred list &key key)
  (member-if (complement pred) list :key key))

(defmacro %alist-entry-p (e)
  `(cond ((consp ,e) t) ((null ,e) nil) (t (%type-error ,e 'list))))

(defun assoc (item alist &key key test test-not)
  (if (and (null key) (null test) (null test-not))
      (do ((l alist (cdr l))) ((%endp l) nil)
        (let ((e (car l))) (when (and (%alist-entry-p e) (eql item (car e))) (return e))))
      (let ((tf (%test-fn test test-not)) (kf (%key-fn key)))
        (do ((l alist (cdr l))) ((%endp l) nil)
          (let ((e (car l)))
            (when (and (%alist-entry-p e) (funcall tf item (%with-key (kf key) (car e)))) (return e)))))))

(defun assoc-if (pred alist &key key)
  (let ((kf (%key-fn key)))
    (do ((l alist (cdr l))) ((%endp l) nil)
      (let ((e (car l)))
        (when (and (%alist-entry-p e) (funcall pred (%with-key (kf key) (car e)))) (return e))))))
(defun assoc-if-not (pred alist &key key) (assoc-if (complement pred) alist :key key))

(defun rassoc (item alist &key key test test-not)
  (let ((tf (%test-fn test test-not)) (kf (%key-fn key)))
    (do ((l alist (cdr l))) ((%endp l) nil)
      (let ((e (car l)))
        (when (and (%alist-entry-p e) (funcall tf item (%with-key (kf key) (cdr e)))) (return e))))))
(defun rassoc-if (pred alist &key key)
  (let ((kf (%key-fn key)))
    (do ((l alist (cdr l))) ((%endp l) nil)
      (let ((e (car l)))
        (when (and (%alist-entry-p e) (funcall pred (%with-key (kf key) (cdr e)))) (return e))))))
(defun rassoc-if-not (pred alist &key key) (rassoc-if (complement pred) alist :key key))

(defun acons (key datum alist) (cons (cons key datum) alist))
(defun pairlis (keys data &optional alist)
  (do ((k keys (cdr k)) (d data (cdr d)) (r alist (cons (cons (car k) (car d)) r)))
      ((or (%endp k) (%endp d)) r)))

(defun adjoin (item list &key key test test-not)
  (if (member (%with-key ((%key-fn key) key) item) list :key key :test test :test-not test-not)
      list
      (cons item list)))

(defun union (l1 l2 &key key test test-not)
  (let ((result l2))
    (dolist (x l1 result)
      (unless (member (%with-key ((%key-fn key) key) x) l2 :key key :test test :test-not test-not)
        (push x result)))))
(defun nunion (l1 l2 &key key test test-not) (union l1 l2 :key key :test test :test-not test-not))

(defun intersection (l1 l2 &key key test test-not)
  (let ((result nil))
    (dolist (x l1 result)
      (when (member (%with-key ((%key-fn key) key) x) l2 :key key :test test :test-not test-not)
        (push x result)))))
(defun nintersection (l1 l2 &key key test test-not)
  (intersection l1 l2 :key key :test test :test-not test-not))

(defun set-difference (l1 l2 &key key test test-not)
  (if (null l2) l1
  (let ((result nil))
    (dolist (x l1 result)
      (unless (member (%with-key ((%key-fn key) key) x) l2 :key key :test test :test-not test-not)
        (push x result))))))
(defun nset-difference (l1 l2 &key key test test-not)
  (set-difference l1 l2 :key key :test test :test-not test-not))

(defun set-exclusive-or (l1 l2 &key key test test-not)
  (append (set-difference l1 l2 :key key :test test :test-not test-not)
          (set-difference l2 l1 :key key
                                :test (if test (lambda (a b) (funcall test b a)) nil)
                                :test-not (if test-not (lambda (a b) (funcall test-not b a)) nil))))
(defun nset-exclusive-or (l1 l2 &key key test test-not)
  (set-exclusive-or l1 l2 :key key :test test :test-not test-not))

(defun subsetp (l1 l2 &key key test test-not)
  (dolist (x l1 t)
    (unless (member (%with-key ((%key-fn key) key) x) l2 :key key :test test :test-not test-not)
      (return nil))))

(defun subst (new old tree &key key test test-not)
  (let ((tf (%test-fn test test-not)) (kf (%key-fn key)))
    (labels ((walk (x)
               (cond ((funcall tf old (%with-key (kf key) x)) new)
                     ((consp x) (let ((a (walk (car x))) (d (walk (cdr x))))
                                  (if (and (eq a (car x)) (eq d (cdr x))) x (cons a d))))
                     (t x))))
      (walk tree))))
(defun subst-if (new pred tree &key key)
  (let ((kf (%key-fn key)))
    (labels ((walk (x)
               (cond ((funcall pred (%with-key (kf key) x)) new)
                     ((consp x) (cons (walk (car x)) (walk (cdr x))))
                     (t x))))
      (walk tree))))
(defun subst-if-not (new pred tree &key key) (subst-if new (complement pred) tree :key key))
(defun nsubst (new old tree &key key test test-not) (subst new old tree :key key :test test :test-not test-not))
(defun nsubst-if (new pred tree &key key) (subst-if new pred tree :key key))
(defun nsubst-if-not (new pred tree &key key) (subst-if-not new pred tree :key key))

(defun sublis (alist tree &key key test test-not)
  (labels ((walk (x)
             (let ((e (assoc (%with-key ((%key-fn key) key) x) alist :test test :test-not test-not)))
               (cond (e (cdr e))
                     ((consp x) (cons (walk (car x)) (walk (cdr x))))
                     (t x)))))
    (walk tree)))
(defun nsublis (alist tree &key key test test-not) (sublis alist tree :key key :test test :test-not test-not))

(defun tree-equal (a b &key test test-not)
  (let ((tf (%test-fn test test-not)))
    (labels ((walk (x y)
               (cond ((and (consp x) (consp y)) (and (walk (car x) (car y)) (walk (cdr x) (cdr y))))
                     ((or (consp x) (consp y)) nil)
                     (t (if (funcall tf x y) t nil)))))
      (walk a b))))

;;; mapping
(defun %map1 (f list collect)
  (let* ((head (cons nil nil)) (tail head))
    (do ((l list (cdr l))) ((%endp l))
      (let ((v (funcall f (car l))))
        (when collect (let ((c (cons v nil))) (%setcdr tail c) (setq tail c)))))
    (cdr head)))

(defun %mapn (f lists collect on-tails)
  (let* ((head (cons nil nil)) (tail head) (ls (copy-list lists)))
    (do () ((dolist (l ls nil) (when (%endp l) (return t))))
      (let ((v (apply f (if on-tails ls (mapcar #'car ls)))))
        (when collect (let ((c (cons v nil))) (%setcdr tail c) (setq tail c))))
      (do ((x ls (cdr x))) ((null x)) (%setcar x (cdr (car x)))))
    (cdr head)))

(defun mapcar (f list &rest more)
  (if (null more)
      (let* ((head (cons nil nil)) (tail head))
        (do ((l list (cdr l))) ((%endp l) (cdr head))
          (let ((c (cons (funcall f (car l)) nil)))
            (%setcdr tail c) (setq tail c))))
      (%mapn f (cons list more) t nil)))
(defun mapc (f list &rest more)
  (if (null more)
      (do ((l list (cdr l))) ((%endp l) list) (funcall f (car l)))
      (progn (%mapn f (cons list more) nil nil) list)))
(defun mapcan (f list &rest more)
  (apply #'nconc (apply #'mapcar f list more)))
(defun maplist (f list &rest more) (%mapn f (cons list more) t t))
(defun mapl (f list &rest more) (%mapn f (cons list more) nil t) list)
(defun mapcon (f list &rest more) (apply #'nconc (apply #'maplist f list more)))

;;; property lists
(defun getf (plist indicator &optional default)
  (do ((l plist (cddr l))) ((%endp l) default)
    (when (eq (car l) indicator) (return (cadr l)))))

(defun %putf (plist indicator value)
  (do ((l plist (cddr l))) ((%endp l) (list* indicator value plist))
    (when (eq (car l) indicator) (%setcar (cdr l) value) (return plist))))

(defun get-properties (plist indicators)
  (do ((l plist (cddr l))) ((%endp l) (values nil nil nil))
    (when (%member-eql (car l) indicators) (return (values (car l) (cadr l) l)))))

(defun get (symbol indicator &optional default)
  (getf (symbol-plist symbol) indicator default))

(defun %put (symbol indicator value)
  (%set-symbol-plist symbol (%putf (symbol-plist symbol) indicator value))
  value)

(defun remprop (symbol indicator)
  (let ((plist (symbol-plist symbol)))
    (cond ((atom plist) nil)
          ((eq (car plist) indicator) (%set-symbol-plist symbol (cddr plist)) t)
          (t (do ((l plist (cddr l))) ((atom (cddr l)) nil)
               (when (eq (caddr l) indicator)
                 (%setcdr (cdr l) (cddddr l))
                 (return t)))))))

(defmacro remf (place indicator &environment env)
  (multiple-value-bind (temps vals stores setter getter) (%get-setf-expansion place env)
    (let ((ind (gensym)) (pl (gensym)) (found (gensym)))
      `(let* (,@(mapcar #'list temps vals) (,ind ,indicator))
         (multiple-value-bind (,pl ,found) (%remf ,getter ,ind)
           (let ((,(car stores) ,pl)) ,setter)
           ,found)))))

(defun %remf (plist indicator)
  (cond ((atom plist) (values plist nil))
        ((eq (car plist) indicator) (values (cddr plist) t))
        (t (do ((l plist (cddr l))) ((atom (cddr l)) (values plist nil))
             (when (eq (caddr l) indicator)
               (%setcdr (cdr l) (cddddr l))
               (return (values plist t)))))))

;;; destructuring-bind support
(defun %db-error (msg)
  (error 'simple-program-error :format-control "Error while destructuring: ~A" :format-arguments (list msg)))
(defun %db-getf (plist key)
  (do ((l plist (cddr l))) ((atom l) '%db-none)
    (when (atom (cdr l)) (%db-error "odd number of keyword arguments"))
    (when (eq (car l) key) (return (cadr l)))))
(defun %db-check-keys (plist keys aok)
  (unless (or aok (getf plist :allow-other-keys))
    (do ((l plist (cddr l))) ((atom l) nil)
      (unless (or (%member-eql (car l) keys) (eq (car l) :allow-other-keys))
        (error 'simple-program-error
               :format-control "Unknown keyword ~S in destructuring (expected one of ~S)"
               :format-arguments (list (car l) keys))))))

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defvar *gensym-counter* 1)

(defun make-symbol (name)
  (unless (stringp name) (%type-error name 'string))
  (%make-symbol (%copy-simple-string name) (%string-hash name)))

(defun gensym (&optional (x "G"))
  (unless (or (stringp x) (and (integerp x) (>= x 0)))
    (%type-error x '(or string unsigned-byte)))
  (unless (or (integerp x) (and (integerp *gensym-counter*) (>= *gensym-counter* 0)))
    (%type-error *gensym-counter* 'unsigned-byte))
  (let ((prefix (if (stringp x) x "G"))
        (n (if (integerp x) x *gensym-counter*)))
    (unless (integerp x) (setq *gensym-counter* (+ *gensym-counter* 1)))
    (make-symbol (concatenate 'string prefix (%integer-to-string n 10)))))

(defvar *gentemp-counter* 1)
(defun gentemp (&optional (prefix "T") (package *package*))
  (unless (stringp prefix) (%type-error prefix 'string))
  (setq package (%find-package-or-die package))
  (loop
    (let ((name (concatenate 'string prefix (%integer-to-string *gentemp-counter* 10))))
      (setq *gentemp-counter* (+ *gentemp-counter* 1))
      (unless (find-symbol name package)
        (return (intern name package))))))

(defun symbol-package (s) (%symbol-package s))
(defun keywordp (x) (and (symbolp x) (eq (%symbol-package x) *keyword-package*)))

(defun copy-symbol (s &optional copy-props)
  (let ((new (make-symbol (symbol-name s))))
    (when copy-props
      (when (boundp s) (%set-symbol-value new (symbol-value s)))
      (when (fboundp s) (%set-symbol-function new (symbol-function s)))
      (%set-symbol-plist new (copy-list (symbol-plist s))))
    new))

(defun %function-name-symbol (name)
  (cond ((symbolp name) name)
        ((and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (cadr name)) (null (cddr name)))
         (%setf-function-symbol (cadr name)))
        (t (%type-error name '(or symbol (cons (eql setf) (cons symbol null)))))))

(defun %setf-function-symbol (sym)
  (let ((pkg (%symbol-package sym)))
    (intern (concatenate 'string "(SETF " (if pkg (package-name pkg) "") "::" (symbol-name sym) ")")
            *system-package*)))

(defun %macro-or-special-p (s)
  (and (symbolp s) (or (get s '%macro-function) (special-operator-p s)) t))

(defun fboundp (name)
  (or (fboundp (%function-name-symbol name)) (%macro-or-special-p name)))

(defun %macro-definition-object (s)
  ;; what SYMBOL-FUNCTION returns for a macro or special operator: a function
  ;; that signals an error when called
  (lambda (&rest args)
    (declare (ignore args))
    (error 'simple-program-error :format-control "~S names a ~A and cannot be called as a function"
                                 :format-arguments (list s (if (special-operator-p s) "special operator" "macro")))))

(defun symbol-function (s)
  (let ((f (%symbol-function s)))
    (cond (f f)
          ((%macro-or-special-p s) (%macro-definition-object s))
          (t (%undefined-function-error s)))))

(defun fdefinition (name) (symbol-function (%function-name-symbol name)))

(defun %set-fdefinition (name f)
  (%set-symbol-function (%function-name-symbol name) f)
  f)

(defun fmakunbound (name)
  (%set-symbol-function (%function-name-symbol name) nil)
  (when (symbolp name) (remprop name '%macro-function))
  name)

(defun special-operator-p (s)
  (unless (symbolp s) (%type-error s 'symbol))
  (and (%member-eql s '(quote function if progn let let* setq flet labels macrolet symbol-macrolet
                        block return-from tagbody go catch throw unwind-protect
                        multiple-value-call multiple-value-prog1 the locally eval-when
                        load-time-value progv))
       t))

;;; macros at runtime
(defun macro-function (s &optional env)
  (let ((local (and env (%env-macro env s))))
    (if local local (get s '%macro-function))))
(defun %set-macro-function (s f)
  (%put s '%macro-function f)
  f)
(defun %note-compiler-macro (name lambda-list body)
  (let ((s (%function-name-symbol name)))
    (%put s '%compiler-macro-function nil)
    (%put s '%compiler-macro-source (list name lambda-list body))
    name))
(defun compiler-macro-function (name &optional env)
  (declare (ignore env))
  (let ((s (%function-name-symbol name)))
    (or (get s '%compiler-macro-function)
        (let ((src (get s '%compiler-macro-source)))
          (when src
            (let ((fn (%make-compiler-macro-function (first src) (second src) (third src))))
              (%put s '%compiler-macro-function fn)
              fn))))))
(defun %set-compiler-macro-function (name f)
  (let ((s (%function-name-symbol name)))
    (%put s '%compiler-macro-source nil)
    (%put s '%compiler-macro-function f)
    f))

(defun %defconstant (s v)
  (%set-symbol-value s v)
  (%set-symbol-flags s (%fxlogior (%symbol-flags s) 3))
  s)

(defun %proclaim-list (specs)
  (dolist (s specs) (proclaim s)))

(defun %proper-list-p (x)
  (loop (cond ((null x) (return t)) ((atom x) (return nil)) (t (setq x (cdr x))))))

(defun proclaim (spec)
  (unless (and (consp spec) (%proper-list-p spec)
               (or (not (member (car spec) '(type ftype))) (consp (cdr spec))))
    (error 'simple-type-error :datum spec :expected-type '(cons symbol list)
                              :format-control "Malformed proclamation ~S" :format-arguments (list spec)))
  (when (and (consp spec) (eq (car spec) 'special))
    (dolist (s (cdr spec))
      (%set-symbol-flags s (%fxlogior (%symbol-flags s) 1))))
  nil)

(defun %special-p (s) (not (zerop (%fxlogand (%symbol-flags s) 1))))
(defun constantp (form &optional env)
  (declare (ignore env))
  (cond ((symbolp form) (or (keywordp form) (null form) (eq form t)
                            (not (zerop (%fxlogand (%symbol-flags form) 2)))))
        ((consp form) (eq (car form) 'quote))
        (t t)))

(defun %define-symbol-macro (name expansion)
  (%put name '%symbol-macro (list expansion))
  name)
