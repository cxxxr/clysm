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

;;; The compositions of CAR and CDR as nested CARs and CDRs: each is then
;;; compiled with what is known of its argument (a cons tested inline, the
;;; helper only for NIL or the type error), not as calls of the helpers
(macrolet ((define-cxr-expansions (&rest specs)
             `(progn
                ,@(mapcar (lambda (spec)
                            (let ((name (car spec)) (path (cadr spec)))
                              `(define-compiler-macro ,name (x)
                                 ,(let ((form 'x))
                                    (dolist (c (reverse (coerce path 'list)))
                                      (setq form (list 'list (if (char= c #\A) ''car ''cdr) form)))
                                    form))))
                          specs))))
  (define-cxr-expansions
    (caar "AA") (cadr "AD") (cdar "DA") (cddr "DD") (caddr "ADD") (cdddr "DDD")
    (caadr "AAD") (cddar "DDA") (cdadr "DAD") (cadar "ADA") (caaar "AAA") (cdaar "DAA")
    (cadddr "ADDD") (cddddr "DDDD") (second "AD") (third "ADD") (fourth "ADDD")))

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
(define-compiler-macro endp (x)
  (let ((v (gensym "L"))) `(let ((,v ,x)) (%endp ,v))))

(defun length-of-list (l)
  (let ((n 0) (x l))
    (loop (if (consp x) (setq x (cdr x) n (%fx+ n 1)) (return)))
    (if (null x) n (progn (%endp x) n))))

(defun list-length (l)
  ;; Brent's cycle detection: one CDR and one comparison for each cons.  MARK
  ;; stays behind and is moved up to the current cons after 2, 4, 8, ...
  ;; steps; in a circular list the walk comes back to it.
  (let ((n 0) (x l) (mark l) (limit 2))
    (declare (fixnum n limit))
    (loop
      (cond ((consp x)
             (setq x (cdr x) n (%fx+ n 1))
             (when (eq x mark) (return nil))
             (when (eql n limit) (setq mark x limit (%fx* limit 2))))
            ((null x) (return n))
            (t (%endp x) (return n))))))

(defun nth (n list)
  (do ((i n (1- i)) (l list (cdr l))) ((or (<= i 0) (%endp l)) (car l))))
(defun nthcdr (n list)
  (unless (and (integerp n) (>= n 0)) (%type-error n 'unsigned-byte))
  (do ((i n (1- i)) (l list (cdr l))) ((or (<= i 0) (%endp l)) l)))
(defun %setnth (n list value) (%setcar (nthcdr n list) value))

(defun fifth (l) (car (cddddr l))) (defun sixth (l) (cadr (cddddr l))) (defun seventh (l) (caddr (cddddr l)))
(defun eighth (l) (cadddr (cddddr l))) (defun ninth (l) (car (cddddr (cddddr l))))
(defun tenth (l) (cadr (cddddr (cddddr l))))
(defsetf fifth (l) (v) `(%setnth 4 ,l ,v))
(defsetf sixth (l) (v) `(%setnth 5 ,l ,v))
(defsetf seventh (l) (v) `(%setnth 6 ,l ,v))
(defsetf eighth (l) (v) `(%setnth 7 ,l ,v))
(defsetf ninth (l) (v) `(%setnth 8 ,l ,v))
(defsetf tenth (l) (v) `(%setnth 9 ,l ,v))
(defsetf nthcdr (n l) (v) `(%setcdr (nthcdr (1- ,n) ,l) ,v))

(defun %last1 (list)
  "(LAST LIST): one pass, to the last cons."
  (unless (listp list) (%type-error list 'list))
  (if (atom list)
      list
      (do ((l list (cdr l))) ((atom (cdr l)) l))))
;; (LAST L): the function of one pass
(define-compiler-macro last (&whole form list &optional (n nil np))
  (if (or (not np) (eql n 1)) `(%last1 ,list) form))
(defun last (list &optional (n 1))
  (when (eql n 1) (return-from last (%last1 list)))
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
;; KEY and FN below are functions (the callers coerce designators with %KEYFN
;; and %FN), so the calls do not check again
(defmacro %key (key x) `(if ,key (funcall (the function ,key) ,x) ,x))

;;; How an element is selected by the functions that take an item or a
;;; predicate (MEMBER, ASSOC, the set functions, and the sequence functions in
;;; seq.lisp): MODE 0 compares it with the item by EQL, MODE 1 calls the
;;; two-argument test FN on the item and the element, MODE 2 calls the
;;; predicate FN on the element; NEG inverts the result (:TEST-NOT and the
;;; -IF-NOT functions).  The functions of a family share one implementation
;;; this way, and no closure is made to turn the one into the other.
(defmacro %with-test ((mode fn neg) (test test-not) &body body)
  `(let ((,mode 0) (,fn nil) (,neg nil))
     (cond (,test
            (when ,test-not (error "Both :TEST and :TEST-NOT were supplied"))
            (setq ,fn (%fn ,test))
            (unless (or (eq ,fn #'eql) (eq ,fn #'eq)) (setq ,fn (%two-arg-fn ,fn) ,mode 1)))
           (,test-not (setq ,fn (%two-arg-fn (%fn ,test-not)) ,mode 1 ,neg t)))
     ,@body))

(defmacro %match (mode fn neg item x)
  (let ((m (gensym)) (v (gensym)))
    `(let* ((,v ,x)
            (,m (cond ((eql ,mode 0) (eql ,item ,v))
                      ((eql ,mode 1) (funcall (the function ,fn) ,item ,v))
                      (t (funcall (the function ,fn) ,v)))))
       (if ,neg (not ,m) ,m))))

;;; The usual two-argument tests (FN, as %WITH-TEST leaves it, is EQUAL,
;;; EQUALP, EQ, =, or the CHAR=, CHAR-EQUAL, STRING=, STRING-EQUAL of two
;;; arguments) called inline in a loop: (SCAN ... TEST) with TEST the call
;;; of the test on ITEM and V for each of them; nothing for other tests (the
;;; caller goes on the general way).  A list searched with :TEST
;;; #'CHAR-EQUAL calls no function per element.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *known-two-arg-tests*
    '((equal equal) (%char=-2 char=) (%char-equal-2 char-equal)
      (%string=-2 %string=-2) (%string-equal-2 %string-equal-2)
      (eq eq) (= =))))
(defmacro %known-tests (mode fn neg item v (scan &rest args))
  `(when (and (eql ,mode 1) (not ,neg))
     (cond ,@(mapcar (lambda (c) `((eq ,fn #',(car c)) (,scan ,@args (,(cadr c) ,item ,v))))
                     *known-two-arg-tests*))))
(defun %member (item list mode fn neg key)
  "The tail of LIST whose first element is selected (see %MATCH)."
  (macrolet ((scan (test)
               `(if key
                    (do ((l list (cdr l))) ((%endp l) nil)
                      (when (let ((v (funcall (the function key) (car l)))) ,test) (return l)))
                    (do ((l list (cdr l))) ((%endp l) nil)
                      (when (let ((v (car l))) ,test) (return l))))))
    (cond ((or neg (not (eql mode 1)))
           (do ((l list (cdr l))) ((%endp l) nil)
             (when (%match mode fn neg item (%key key (car l))) (return l))))
          ((eq fn #'equal) (scan (equal item v)))
          ((eq fn #'eq) (scan (eq item v)))
          ((eq fn #'=) (scan (= item v)))
          ((eq fn #'%string=-2) (scan (%string=-2 item v)))
          ((eq fn #'%string-equal-2) (scan (%string-equal-2 item v)))
          ((eq fn #'%char=-2) (scan (char= item v)))
          ((eq fn #'%char-equal-2) (scan (char-equal item v)))
          (t (scan (funcall (the function fn) item v))))))

(defun member (item list &key key test test-not)
  (if (and (null key) (null test) (null test-not))
      (%member-eql item list)
      (%with-test (mode fn neg) (test test-not)
        (%member item list mode fn neg (%keyfn key)))))
;;; MEMBER with a list written as a constant, or with only :TEST: at the
;;; call site (local-time's (member c '(#\: #\T ...)), (member c chars :test
;;; #'char-equal)), without parsing keywords or calling the test through a
;;; function object.
(define-compiler-macro member (&whole form item list &rest keys)
  (cond ((and (null keys) (consp list) (eq (car list) 'quote) (consp (cdr list))
              (let ((l (second list)))
                (and (consp l) (handler-case (<= (list-length l) 8) (error () nil))
                     (every (lambda (e) (or (symbolp e) (characterp e) (typep e '(signed-byte 29)))) l))))
         (let ((v (gensym "ITEM")) (l (second list)))
           `(let ((,v ,item))
              (cond ,@(loop for tail on l
                            collect `((eq ,v ',(car tail)) ',tail))
                    (t nil)))))
        ((null keys) `(%member-eql ,item ,list))
        ((and (= (length keys) 2) (eq (first keys) :test))
         (let ((v (gensym "ITEM")) (f (gensym "TEST")) (l (gensym "LIST")) (b (gensym "MEMBER"))
               (test (second keys)))
           (if (and (consp test) (eq (car test) 'function) (symbolp (second test)) (null (cddr test)))
               `(let ((,v ,item))
                  (block ,b
                    (%do ((,l ,list (cdr ,l))) ((endp ,l) nil)
                      (when (,(second test) ,v (car ,l)) (return-from ,b ,l)))))
               (let ((l0 (gensym "LIST")))
                 `(let* ((,v ,item) (,l0 ,list) (,f (%fn ,test)))
                    (block ,b
                      (%do ((,l ,l0 (cdr ,l))) ((endp ,l) nil)
                        (when (funcall ,f ,v (car ,l)) (return-from ,b ,l)))))))))
        (t form)))

(defun member-if (pred list &key key)
  (%member nil list 2 (%fn pred) nil (%keyfn key)))
(defun member-if-not (pred list &key key)
  (%member nil list 2 (%fn pred) t (%keyfn key)))

(defmacro %alist-entry-p (e)
  `(cond ((consp ,e) t) ((null ,e) nil) (t (%type-error ,e 'list))))

(defun %assoc-eql (item alist)
  (do ((l alist (cdr l))) ((%endp l) nil)
    (let ((e (car l))) (when (and (%alist-entry-p e) (eql item (car e))) (return e)))))

(defun %rassoc-eql (item alist)
  (do ((l alist (cdr l))) ((%endp l) nil)
    (let ((e (car l))) (when (and (%alist-entry-p e) (eql item (cdr e))) (return e)))))

;;; MEMBER / ASSOC with :TEST #'EQUAL, #'EQUALP or #'STRING= (the frontend
;;; calls these): the test called directly, not through FUNCALL
(macrolet ((def (member-name assoc-name test)
             `(progn
                (defun ,member-name (item list)
                  (do ((l list (cdr l))) ((%endp l) nil)
                    (when (,test item (car l)) (return l))))
                (defun ,assoc-name (item alist)
                  (do ((l alist (cdr l))) ((%endp l) nil)
                    (let ((e (car l)))
                      (when (and (%alist-entry-p e) (,test item (car e))) (return e))))))))
  (def %member-equal %assoc-equal equal)
  (def %member-equalp %assoc-equalp equalp)
  (def %member-string= %assoc-string= string=))

(defun %assoc (item alist mode fn neg key by-cdr)
  "The first entry of ALIST whose CAR (CDR when BY-CDR) is selected."
  (macrolet ((scan (test)
               `(do ((l alist (cdr l))) ((%endp l) nil)
                  (let ((e (car l)))
                    (when (and (%alist-entry-p e)
                               (let ((v (car e))) ,test))
                      (return e))))))
    (cond ((or neg key by-cdr (not (eql mode 1)))
           (do ((l alist (cdr l))) ((%endp l) nil)
             (let ((e (car l)))
               (when (and (%alist-entry-p e)
                          (%match mode fn neg item (%key key (if by-cdr (cdr e) (car e)))))
                 (return e)))))
          ;; (the usual tests on the keys themselves: called directly)
          ((eq fn #'equal) (scan (equal item v)))
          ((eq fn #'eq) (scan (eq item v)))
          ((eq fn #'%string=-2) (scan (%string=-2 item v)))
          ((eq fn #'%string-equal-2) (scan (%string-equal-2 item v)))
          ((eq fn #'%char=-2) (scan (char= item v)))
          ((eq fn #'%char-equal-2) (scan (char-equal item v)))
          ((eq fn #'=) (scan (= item v)))
          (t (scan (funcall (the function fn) item v))))))

(defun assoc (item alist &key key test test-not)
  (if (and (null key) (null test) (null test-not))
      (do ((l alist (cdr l))) ((%endp l) nil)
        (let ((e (car l))) (when (and (%alist-entry-p e) (eql item (car e))) (return e))))
      (%with-test (mode fn neg) (test test-not)
        (%assoc item alist mode fn neg (%keyfn key) nil))))
(defun assoc-if (pred alist &key key) (%assoc nil alist 2 (%fn pred) nil (%keyfn key) nil))
(defun assoc-if-not (pred alist &key key) (%assoc nil alist 2 (%fn pred) t (%keyfn key) nil))

(defun rassoc (item alist &key key test test-not)
  (if (and (null key) (null test) (null test-not))
      (do ((l alist (cdr l))) ((%endp l) nil)
        (let ((e (car l))) (when (and (%alist-entry-p e) (eql item (cdr e))) (return e))))
      (%with-test (mode fn neg) (test test-not)
        (%assoc item alist mode fn neg (%keyfn key) t))))
(defun rassoc-if (pred alist &key key) (%assoc nil alist 2 (%fn pred) nil (%keyfn key) t))
(defun rassoc-if-not (pred alist &key key) (%assoc nil alist 2 (%fn pred) t (%keyfn key) t))

(defun acons (key datum alist) (cons (cons key datum) alist))
(defun pairlis (keys data &optional alist)
  (do ((k keys (cdr k)) (d data (cdr d)) (r alist (cons (cons (car k) (car d)) r)))
      ((or (%endp k) (%endp d)) r)))

(defun adjoin (item list &key key test test-not)
  (%with-test (mode fn neg) (test test-not)
    (let ((key (%keyfn key)))
      (if (%member (%key key item) list mode fn neg key) list (cons item list)))))

;;; The set functions.  Whether an element of the first list has a match in
;;; the second is asked of a hash table of the second list's keys when the
;;; test is EQ, EQL, EQUAL or EQUALP and both lists are long; of the list
;;; itself otherwise.
(defun %set-table (l1 l2 mode fn neg key)
  "A hash table of the keys of L2, or NIL when the lists are short or the
test is not one a hash table has."
  (let ((test (cond (neg nil)
                    ((eql mode 0) 'eql)
                    ((eq fn #'equal) 'equal)
                    ((eq fn #'equalp) 'equalp)
                    (t nil))))
    (if (and test (consp (nthcdr 15 l1)) (consp (nthcdr 15 l2)) (boundp '*hash-table-class*))
        (let ((table (make-hash-table :test test :size (length-of-list l2))))
          (dolist (y l2 table) (setf (gethash (%key key y) table) t)))
        nil)))

(defmacro %in-set-p (x l2 table mode fn neg key)
  "True when the element X of the first list has a match in L2."
  (let ((k (gensym)))
    `(let ((,k (%key ,key ,x)))
       (if ,table
           (nth-value 1 (gethash ,k ,table))
           (%member ,k ,l2 ,mode ,fn ,neg ,key)))))

(defun union (l1 l2 &key key test test-not)
  (%with-test (mode fn neg) (test test-not)
    (let* ((key (%keyfn key)) (table (%set-table l1 l2 mode fn neg key)) (result l2))
      (dolist (x l1 result)
        (unless (%in-set-p x l2 table mode fn neg key) (push x result))))))
(defun nunion (l1 l2 &key key test test-not) (union l1 l2 :key key :test test :test-not test-not))

(defun intersection (l1 l2 &key key test test-not)
  (%with-test (mode fn neg) (test test-not)
    (let* ((key (%keyfn key)) (table (%set-table l1 l2 mode fn neg key)) (result nil))
      (dolist (x l1 result)
        (when (%in-set-p x l2 table mode fn neg key) (push x result))))))
(defun nintersection (l1 l2 &key key test test-not)
  (intersection l1 l2 :key key :test test :test-not test-not))

(defun set-difference (l1 l2 &key key test test-not)
  (if (null l2)
      l1
      (%with-test (mode fn neg) (test test-not)
        (let* ((key (%keyfn key)) (table (%set-table l1 l2 mode fn neg key)) (result nil))
          (dolist (x l1 result)
            (unless (%in-set-p x l2 table mode fn neg key) (push x result)))))))
(defun nset-difference (l1 l2 &key key test test-not)
  (set-difference l1 l2 :key key :test test :test-not test-not))

(defun set-exclusive-or (l1 l2 &key key test test-not)
  (append (set-difference l1 l2 :key key :test test :test-not test-not)
          (if (and (null test-not)
                   (or (null test) (%member-eql test (list 'eq 'eql 'equal 'equalp #'eq #'eql #'equal #'equalp))))
              ;; a symmetric test: the order of its arguments does not matter
              (set-difference l2 l1 :key key :test test)
              (set-difference l2 l1 :key key
                                    :test (if test (lambda (a b) (funcall test b a)) nil)
                                    :test-not (if test-not (lambda (a b) (funcall test-not b a)) nil)))))
(defun nset-exclusive-or (l1 l2 &key key test test-not)
  (set-exclusive-or l1 l2 :key key :test test :test-not test-not))

(defun subsetp (l1 l2 &key key test test-not)
  (%with-test (mode fn neg) (test test-not)
    (let* ((key (%keyfn key)) (table (%set-table l1 l2 mode fn neg key)))
      (dolist (x l1 t)
        (unless (%in-set-p x l2 table mode fn neg key) (return nil))))))

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
  (if (or test test-not)
      (%tree-equal-test a b (%test-fn test test-not))
      (%tree-equal-eql a b)))

(defun %tree-equal-test (x y tf)
  ;; the CDR direction is a loop
  (loop
    (cond ((and (consp x) (consp y))
           (unless (%tree-equal-test (car x) (car y) tf) (return nil))
           (setq x (cdr x) y (cdr y)))
          ((or (consp x) (consp y)) (return nil))
          (t (return (if (funcall (the function tf) x y) t nil))))))

(defun %tree-equal-eql (x y)
  (loop
    (cond ((eq x y) (return t))
          ((and (consp x) (consp y))
           (unless (%tree-equal-eql (car x) (car y)) (return nil))
           (setq x (cdr x) y (cdr y)))
          ((or (consp x) (consp y)) (return nil))
          (t (return (eql x y))))))

;; without keywords, or with :TEST alone: no keyword list
(define-compiler-macro tree-equal (&whole form a b &rest keys)
  (cond ((null keys) `(%tree-equal-eql ,a ,b))
        ((and (eq (car keys) :test) (consp (cdr keys)) (null (cddr keys)))
         (let ((x (gensym "A")) (y (gensym "B")))
           `(let ((,x ,a) (,y ,b))
              (%tree-equal-test ,x ,y (%test-fn ,(cadr keys) nil)))))
        (t form)))

;;; mapping
(defun %map1 (f list collect)
  (let* ((head (cons nil nil)) (tail head))
    (do ((l list (cdr l))) ((%endp l))
      (let ((v (funcall f (car l))))
        (when collect (let ((c (cons v nil))) (%setcdr tail c) (setq tail c)))))
    (cdr head)))

(defun %mapn (f lists collect on-tails)
  (let* ((head (cons nil nil)) (tail head) (ls (copy-list lists))
         ;; the argument list is made once and refilled (APPLY does not keep it)
         (args (if on-tails ls (copy-list lists))))
    (do () ((dolist (l ls nil) (when (%endp l) (return t))))
      (unless on-tails
        (do ((a args (cdr a)) (x ls (cdr x))) ((null x)) (%setcar a (car (car x)))))
      (let ((v (apply f args)))
        (when collect (let ((c (cons v nil))) (%setcdr tail c) (setq tail c))))
      (do ((x ls (cdr x))) ((null x)) (%setcar x (cdr (car x)))))
    (cdr head)))

(defun %map2 (f l1 l2 collect)
  "F on the elements of two lists in step, without an argument list."
  (let* ((head (cons nil nil)) (tail head))
    (do ((a l1 (cdr a)) (b l2 (cdr b))) ((or (%endp a) (%endp b)) (cdr head))
      (let ((v (funcall f (car a) (car b))))
        (when collect (let ((c (cons v nil))) (%setcdr tail c) (setq tail c)))))))

(defun mapcar (f list &rest more)
  (cond ((null more)
         (let* ((head (cons nil nil)) (tail head))
           (do ((l list (cdr l))) ((%endp l) (cdr head))
             (let ((c (cons (funcall f (car l)) nil)))
               (%setcdr tail c) (setq tail c)))))
        ((null (cdr more)) (%map2 f list (car more) t))
        (t (%mapn f (cons list more) t nil))))
;;; The mapping functions with a function and lists written at the call
;;; site are loops there (as in SBCL), whatever the number of lists: the
;;; function, when it is #'name or a lambda, is called directly (a primitive
;;; such as + or CONS is inline) and no argument list is made.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %literal-function-form-p (f)
    (and (consp f) (member (car f) '(lambda function))))
  (defun %direct-function-form-p (f)
    "F is #'name or a lambda: it is called where it is written, not bound."
    (and (consp f)
         (or (eq (car f) 'lambda)
             (and (eq (car f) 'function) (consp (cdr f))
                  (or (symbolp (cadr f)) (and (consp (cadr f)) (eq (car (cadr f)) 'lambda)))))))
  (defun %function-binding (var f)
    "The binding of VAR to the function form F, unless F is called directly."
    (if (%direct-function-form-p f) nil (list (list var f))))
  (defun %arity-lambda (f n)
    "For F = #'NAME, a lambda of N parameters calling NAME (a call of a
known arity, where a primitive is inline and no &REST entry is taken); NIL
for any other F."
    (when (and (consp f) (eq (car f) 'function) (consp (cdr f)) (symbolp (cadr f)))
      (let ((args (loop repeat n collect (gensym "A"))))
        `(lambda ,args (,(cadr f) ,@args)))))
  (defun %literal-call (f var args)
    "A call of the function form F (bound to VAR unless %DIRECT-FUNCTION-FORM-P) on ARGS."
    (cond ((not (%direct-function-form-p f)) `(funcall ,var ,@args))
          ((eq (car f) 'lambda) `(,f ,@args))
          (t `(,(cadr f) ,@args))))
  (defun %mapfun-expansion (f lists on-tails accumulate)
    "ACCUMULATE: :LIST (MAPCAR, MAPLIST), :NCONC (MAPCAN, MAPCON) or NIL (MAPC,
MAPL: the value is the first list)."
    (let ((fn (gensym "F")) (l0 (gensym "L0")) (head (gensym "HEAD")) (tail (gensym "TAIL"))
          (v (gensym "V")) (vars (loop for nil in lists collect (gensym "L"))))
      `(let* (,@(%function-binding fn f) (,l0 ,(car lists))
              ,@(when accumulate `((,head (cons nil nil)) (,tail ,head))))
         (%do ((,(car vars) ,l0 (cdr ,(car vars)))
              ,@(loop for var in (cdr vars) for l in (cdr lists) collect `(,var ,l (cdr ,var))))
             ((or ,@(loop for var in vars collect `(%endp ,var))) ,(if accumulate `(cdr ,head) l0))
           ,(let ((call (%literal-call f fn (loop for var in vars collect (if on-tails var `(car ,var))))))
              (case accumulate
                (:list `(let ((,v (cons ,call nil))) (%setcdr ,tail ,v) (setq ,tail ,v)))
                (:nconc `(let ((,v ,call))
                           (when ,v
                             (%setcdr ,tail ,v)
                             (when (consp ,v)
                               (setq ,tail ,v)
                               (do () ((atom (cdr ,tail))) (setq ,tail (cdr ,tail)))))))
                (t call))))))))
(define-compiler-macro mapcar (f list &rest more) (%mapfun-expansion f (cons list more) nil :list))
(define-compiler-macro mapc (f list &rest more) (%mapfun-expansion f (cons list more) nil nil))
(define-compiler-macro mapcan (f list &rest more) (%mapfun-expansion f (cons list more) nil :nconc))
(define-compiler-macro maplist (f list &rest more) (%mapfun-expansion f (cons list more) t :list))
(define-compiler-macro mapl (f list &rest more) (%mapfun-expansion f (cons list more) t nil))
(define-compiler-macro mapcon (f list &rest more) (%mapfun-expansion f (cons list more) t :nconc))

(defun mapc (f list &rest more)
  (cond ((null more) (do ((l list (cdr l))) ((%endp l) list) (funcall f (car l))))
        ((null (cdr more)) (%map2 f list (car more) nil) list)
        (t (%mapn f (cons list more) nil nil) list)))
(defun mapcan (f list &rest more)
  (if (cdr more)
      (apply #'nconc (apply #'mapcar f list more))
      ;; one or two lists: each result is linked to the last cons so far
      (let* ((head (cons nil nil)) (tail head) (two (not (null more))) (l2 (car more)))
        (do ((l list (cdr l))) ((or (%endp l) (and two (%endp l2))) (cdr head))
          (let ((r (if two (funcall f (car l) (car l2)) (funcall f (car l)))))
            (when two (setq l2 (cdr l2)))
            (when r
              (%setcdr tail r)
              (when (consp r) (setq tail (last r)))))))))
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

;;; Call sites of generic functions cache the functions found for the classes
;;; of their arguments (clos.lisp); the caches are valid while this number is
;;; the one they were filled at.  It changes when the methods of a generic
;;; function or a class change, and when a name stops naming a generic function.
(defvar *clos-epoch* 1)
;;; the caches of call sites that have been used: they are emptied here,
;;; and so compare no epoch at each call
(defvar *gf-sites* nil)
(defun %bump-clos-epoch ()
  (setq *clos-epoch* (%fx+ *clos-epoch* 1))
  ;; (an emptied site leaves the list, and is noted again when it is filled:
  ;; loading a program adds methods one by one, each emptying the sites)
  (let ((sites *gf-sites*))
    (setq *gf-sites* nil)
    (dolist (c sites)
      (if (eql (%svlength c) 12)
          (progn (%set-svref c 1 nil) (%set-svref c 3 nil) (%set-svref c 5 nil) (%set-svref c 7 nil)
                 (%set-svref c 11 nil))
          (progn (do ((i 1 (%fx+ i 3))) ((%fx> i 10))
                   (%set-svref c i nil) (%set-svref c (%fx+ i 1) nil))
                 (%set-svref c 15 nil))))))
(defun %note-function-change (sym)
  (when (fboundp sym)
    (let ((old (symbol-function sym)))
      (when (and (functionp old) (%instancep (%closure-name old))) (%bump-clos-epoch)))))

;; (the compiler in the image: the function made by the definition whose source it recorded)
(defun %note-live-inline (sym) (%put sym '%inline-closure (symbol-function sym)) sym)
;; (a small function of a compiled file: its source and the function it made,
;; for the calls compiled after the fasl is loaded; src/frontend.lisp)
(defun %note-fasl-inline (sym lambda-list body mark)
  (%put sym '%fasl-inline (list lambda-list body mark (symbol-function sym)))
  sym)
;; the function a guarded copy of SYM's body compares SYM's function with:
;; that of the definition MARK while it is SYM's function, else no function
(defun %fasl-inline-function (sym mark)
  (let ((e (get sym '%fasl-inline)))
    (if (and (consp e) (equal (third e) mark) (fboundp sym) (eq (fourth e) (symbol-function sym)))
        (fourth e)
        (list mark))))

(defun %set-fdefinition (name f)
  (let ((sym (%function-name-symbol name)))
    (%note-function-change sym)
    (%set-symbol-function sym f))
  f)

(defun fmakunbound (name)
  (%note-function-change (%function-name-symbol name))
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
(defvar *foreign-env-macro-function-hook* nil
  "called with a symbol and an environment that is not an interpreter environment
(the embedded compiler's): the macro function the symbol has there, or NIL")
(defun macro-function (s &optional env)
  (if (and env (not (listp env)) *foreign-env-macro-function-hook*)
      (funcall *foreign-env-macro-function-hook* s env)
      (let ((local (and env (%env-macro env s))))
        (if local local (get s '%macro-function)))))
(defvar *env-epoch* 0
  "Counts the changes of what reading or compiling a form depends on: macros,
proclamations, readtables, packages, LOADs (%LOAD-BATCH reads and compiles
forms of a file before the forms before them have run, and reads them again
when one of those changed this).")
(defun %note-env-change () (setq *env-epoch* (%fx+ *env-epoch* 1)) nil)
(defun %set-macro-function (s f)
  (%note-env-change)
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

(defvar *proclaimed-safety* 1
  "The SAFETY of the OPTIMIZE proclamations, which the compiler in the image
reads (src/frontend.lisp); COMPILE-FILE binds it around a file.")

(defun %proclaim-list (specs)
  ;; (DECLAIM: the compiler has seen it when it compiled the forms after it)
  (dolist (s specs) (%proclaim-1 s)))

(defun %proper-list-p (x)
  (loop (cond ((null x) (return t)) ((atom x) (return nil)) (t (setq x (cdr x))))))

(defun proclaim (spec)
  (%note-env-change)
  (%proclaim-1 spec))

(defun %proclaim-1 (spec)
  (unless (and (consp spec) (%proper-list-p spec)
               (or (not (member (car spec) '(type ftype))) (consp (cdr spec))))
    (error 'simple-type-error :datum spec :expected-type '(cons symbol list)
                              :format-control "Malformed proclamation ~S" :format-arguments (list spec)))
  (when (and (consp spec) (eq (car spec) 'special))
    (dolist (s (cdr spec))
      (%set-symbol-flags s (%fxlogior (%symbol-flags s) 1))))
  (when (and (consp spec) (eq (car spec) 'optimize))
    (dolist (q (cdr spec))
      (cond ((eq q 'safety) (setq *proclaimed-safety* 3))
            ((and (consp q) (eq (car q) 'safety) (consp (cdr q)) (typep (cadr q) '(integer 0 3)))
             (setq *proclaimed-safety* (cadr q))))))
  ;; the type of global variables: references compiled in the image after it
  ;; are of that type (*LIVE-VAR-TYPE-HOOK*, src/frontend.lisp)
  (multiple-value-bind (type vars) (%proclaimed-var-type spec)
    (when type
      (dolist (v vars) (when (and v (symbolp v)) (%put v '%var-type type)))))
  ;; the compiler in the image does not copy the inline expansion of a
  ;; function proclaimed NOTINLINE (src/frontend.lisp)
  (when (and (consp spec) (member (car spec) '(inline notinline)))
    (dolist (f (cdr spec))
      (let ((s (if (symbolp f)
                   f
                   (and (consp f) (eq (car f) 'setf) (consp (cdr f)) (symbolp (cadr f))
                        (null (cddr f)) (%function-name-symbol f)))))
        (when (and s (symbolp s))
          (if (eq (car spec) 'notinline) (%put s '%notinline t) (remprop s '%notinline))))))
  ;; the compiler in the image checks the types of an FTYPE (src/frontend.lisp)
  (when (and (consp spec) (eq (car spec) 'ftype))
    (dolist (f (cddr spec))
      (let ((s (if (symbolp f)
                   f
                   (and (consp f) (eq (car f) 'setf) (consp (cdr f)) (symbolp (cadr f))
                        (null (cddr f)) (%function-name-symbol f)))))
        (when (and s (symbolp s)) (%put s '%ftype (cadr spec))))))
  nil)

(defun %proclaimed-var-type (spec)
  "The type and the variables of the type proclamation SPEC, or NIL (the
abbreviated form only for the type names the compiler knows in declarations)."
  (cond ((atom spec) nil)
        ((eq (car spec) 'type) (and (consp (cdr spec)) (values (cadr spec) (cddr spec))))
        ((member (car spec) '(fixnum integer float double-float single-float short-float long-float
                              character base-char string simple-string simple-vector cons list symbol
                              number real rational bit function unsigned-byte signed-byte null
                              simple-bit-vector simple-base-string))
         (values (car spec) (cdr spec)))
        ((and (consp (car spec))
              (member (caar spec) '(integer unsigned-byte signed-byte simple-array simple-vector
                                    simple-string simple-bit-vector)))
         (values (car spec) (cdr spec)))
        (t nil)))

(defun %special-p (s) (not (zerop (%fxlogand (%symbol-flags s) 1))))
(defun constantp (form &optional env)
  (declare (ignore env))
  (cond ((symbolp form) (or (keywordp form) (null form) (eq form t)
                            (not (zerop (%fxlogand (%symbol-flags form) 2)))))
        ((consp form) (eq (car form) 'quote))
        (t t)))

(defun %define-symbol-macro (name expansion)
  (%note-env-change)
  (%put name '%symbol-macro (list expansion))
  name)
