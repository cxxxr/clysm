;;;; hof.lisp — loops at the call site for the sequence functions taking a function
;;;;
;;;; A call of FIND-IF, POSITION-IF, COUNT-IF, MEMBER-IF, ASSOC-IF, REMOVE-IF,
;;;; DELETE-IF (and their -NOT forms), of FIND, POSITION, COUNT, ASSOC,
;;;; REMOVE and DELETE with :KEY or :TEST, and of REDUCE, over a list or a
;;;; vector, without :START, :END, :FROM-END or :COUNT, is a loop where it is
;;;; written: the function, when it is a lambda or #'NAME, is called there
;;;; (its body inline, a primitive such as < open-coded); any other function
;;;; is called from that loop, whose call the engine can inline (a call from
;;;; the runtime's function into a module loaded from source or a fasl never
;;;; is).  A sequence that is neither a list nor a vector goes to the
;;;; function, which signals the error.
;;;;
;;;; This file is the last of the runtime but the image's own: the runtime's
;;;; code, compiled before these compiler macros are defined, keeps its calls
;;;; (one loop at every call site of the runtime would make it larger).

(defvar *open-coded-function-p-hook* nil
  "In the image: a function of a name and a number of arguments telling
whether the compiler open-codes a call of it (lib/live-image.lisp).")

(%define-expander-functions
  (defun %hof-runtime-function-p (f nargs)
    "True in the image when the function form F is #'NAME of a function of the
runtime that a call does not open-code: the runtime's function, whose loop
calls it within the runtime's module (where the engine inlines it), is
faster than a loop at the call site calling it in another module."
    (and (consp f) (eq (car f) 'function) (consp (cdr f)) (symbolp (cadr f)) (null (cddr f))
         (member (symbol-package (cadr f)) (list (find-package "COMMON-LISP") (find-package "SYSTEM")))
         (boundp '*open-coded-function-p-hook*) (symbol-value '*open-coded-function-p-hook*)
         (not (funcall (symbol-value '*open-coded-function-p-hook*) (cadr f) nargs))))
  (defun %hof-user-code-p ()
    "True unless the code being compiled is the compiler's own (its packages'
names begin with CLYSM; on the host, after the prefix of the target's
packages): the compiler in the image keeps its calls, as the runtime does."
    (let* ((name (package-name *package*))
           (slash (position #\/ name))
           (name (if slash (subseq name (+ slash 1)) name)))
      (not (and (>= (length name) 5) (string= name "CLYSM" :end1 5)))))
  (defun %hof-keys (keys allowed)
    "The plist KEYS (keywords written at the call site) when its keywords are
among ALLOWED, each at most once, in user code (%HOF-USER-CODE-P); :NO
otherwise."
    (let ((seen nil))
      (unless (%hof-user-code-p) (return-from %hof-keys :no))
      (do ((l keys (cddr l))) ((null l) keys)
        (unless (and (consp (cdr l)) (keywordp (car l)) (member (car l) allowed) (not (member (car l) seen)))
          (return :no))
        (push (car l) seen))))

  (defun %hof-fn (var f)
    "(values bindings call-fn) for the function form F: CALL-FN makes the
call of F on argument forms (F itself when it is a lambda or #'NAME, else
a FUNCALL of VAR bound to it as a function)."
    (if (%direct-function-form-p f)
        (values nil (lambda (args) (%literal-call f nil args)))
        (values (list (list var `(%fn ,f))) (lambda (args) `(funcall (the function ,var) ,@args)))))

  (defun %hof-key (var key)
    "(values bindings key-fn) for the :KEY form KEY (NIL: none)."
    (cond ((or (null key) (equal key '(function identity)) (equal key ''identity))
           (values nil (lambda (x) x)))
          ((%direct-function-form-p key)
           (values nil (lambda (x) (%literal-call key nil (list x)))))
          (t (values (list (list var `(%keyfn ,key)))
                     (lambda (x) `(if ,var (funcall (the function ,var) ,x) ,x))))))

  (defun %hof-test (item-var keys tv)
    "(values bindings test-fn) of the item functions (FIND and the like): TEST-FN
makes the test of an element (already keyed) against the item in ITEM-VAR;
TV is the variable of a test that is not called directly."
    (let ((test (getf keys :test)) (test-not (getf keys :test-not)))
      (multiple-value-bind (b call) (if (or test test-not) (%hof-fn tv (or test test-not)) (values nil nil))
        (values b
                (cond (test (lambda (x) (funcall call (list item-var x))))
                      (test-not (lambda (x) `(not ,(funcall call (list item-var x)))))
                      (t (lambda (x) `(eql ,item-var ,x))))))))

  (defun %hof-scan (kind seq test-fn before after fallback)
    "The loop of KIND (:FIND :POSITION :COUNT :MEMBER :REMOVE :DELETE) over the
sequence form SEQ, (TEST-FN element-var) being the test of an element; the
bindings BEFORE are made before SEQ is evaluated, AFTER after it (the order
of the arguments as written); (FALLBACK seq-var) is the code for any
sequence that is not a list (or a vector, for the first three kinds)."
    (let ((s (gensym "SEQ")) (l (gensym "L")) (x (gensym "X")) (n (gensym "N")) (b (gensym "B"))
          (d (gensym "D")) (i (gensym "I")) (e (gensym "E")) (off (gensym "OFF"))
          (head (gensym "HEAD")) (tail (gensym "TAIL")))
      (let ((list-loop
              (ecase kind
                ((:find :member)
                 `(block ,b
                    (%do ((,l ,s (cdr ,l))) ((%endp ,l) nil)
                      (let ((,x (car ,l)))
                        (when ,(funcall test-fn x) (return-from ,b ,(if (eq kind :find) x l)))))))
                (:position
                 `(block ,b
                    (let ((,n 0))
                      (declare (fixnum ,n))
                      (%do ((,l ,s (cdr ,l))) ((%endp ,l) nil)
                        (let ((,x (car ,l)))
                          (when ,(funcall test-fn x) (return-from ,b ,n)))
                        (setq ,n (%fx+ ,n 1))))))
                (:count
                 `(let ((,n 0))
                    (declare (fixnum ,n))
                    (%do ((,l ,s (cdr ,l))) ((%endp ,l) ,n)
                      (let ((,x (car ,l)))
                        (when ,(funcall test-fn x) (setq ,n (%fx+ ,n 1)))))))
                (:remove
                 ;; (the elements after the last one removed are shared: the
                 ;; list itself when none is)
                 `(let* ((,head (cons nil nil)) (,tail ,head) (,e ,s))
                    (%do ((,l ,s (cdr ,l))) ((%endp ,l) (%setcdr ,tail ,e) (cdr ,head))
                      (let ((,x (car ,l)))
                        (when ,(funcall test-fn x)
                          (%do ((,i ,e (cdr ,i))) ((eq ,i ,l))
                            (let ((,n (cons (car ,i) nil))) (%setcdr ,tail ,n) (setq ,tail ,n)))
                          (setq ,e (cdr ,l)))))))
                (:delete
                 ;; (the conses of the elements kept are linked again)
                 `(let* ((,head (cons nil ,s)) (,tail ,head))
                    (%do ((,l ,s (cdr ,l))) ((%endp ,l) (cdr ,head))
                      (let ((,x (car ,l)))
                        (if ,(funcall test-fn x)
                            (%setcdr ,tail (cdr ,l))
                            (setq ,tail ,l))))))))
            (vector-loop
              (when (member kind '(:find :position :count))
                `(let* ((,d (if (%simple-vector-p ,s) ,s (%array-storage ,s)))
                        (,off (if (%simple-vector-p ,s) 0 (%array-total-offset ,s)))
                        (,e (+ ,off (length ,s))) ,@(when (eq kind :count) `((,n 0))))
                   (declare (fixnum ,off ,e ,@(when (eq kind :count) (list n))))
                   (block ,b
                     (%do ((,i ,off (%fx+ ,i 1))) ((%fx>= ,i ,e) ,(if (eq kind :count) n nil))
                       (declare (fixnum ,i))
                       (let ((,x (%vref ,d ,i)))
                         (when ,(funcall test-fn x)
                           ,(ecase kind
                              (:find `(return-from ,b ,x))
                              (:position `(return-from ,b (%fx- ,i ,off)))
                              (:count `(setq ,n (%fx+ ,n 1))))))))))))
        `(let* (,@before (,s ,seq) ,@after)
           (cond ((listp ,s) ,list-loop)
                 ,@(when vector-loop `(((vectorp ,s) ,vector-loop)))
                 (t ,(funcall fallback s)))))))

  (defun %hof-fallback (kind name args)
    "The code for a sequence the loops of KIND do not take: the call of the
function NAME on ARGS (REMOVE and DELETE of a vector), else the type error."
    (lambda (s)
      (case kind
        ((:remove :delete) `(locally (declare (notinline ,name)) (,name ,@(subst s :seq args))))
        (:member `(%type-error ,s 'list))
        (t `(%type-error ,s 'sequence)))))

  (defun %hof-pred-expansion (form kind pred seq keys negate)
    "FIND-IF and the like: the loop, or FORM."
    (let ((keys (%hof-keys keys '(:key))))
      (if (or (eq keys :no) (%hof-runtime-function-p pred 1) (%hof-runtime-function-p (getf keys :key) 1))
          form
          (let ((fv (gensym "PRED")) (kv (gensym "KEY")))
            (multiple-value-bind (fb call) (%hof-fn fv pred)
              (multiple-value-bind (kb keyfn) (%hof-key kv (getf keys :key))
                (%hof-scan kind seq
                           (lambda (x) (let ((c (funcall call (list (funcall keyfn x))))) (if negate `(not ,c) c)))
                           fb kb
                           (%hof-fallback kind (car form)
                                          `(,(if fb fv pred) :seq ,@(when kb `(:key ,kv))
                                            ,@(when (and (getf keys :key) (not kb)) `(:key ,(getf keys :key))))))))))))

  (defun %hof-item-expansion (kind name item seq keys)
    "FIND, POSITION, COUNT, REMOVE and DELETE of an item with :KEY or :TEST (or
:TEST-NOT): the loop, or NIL."
    (let ((keys (%hof-keys keys '(:key :test :test-not))))
      (when (and (not (eq keys :no)) keys (not (and (getf keys :test) (getf keys :test-not)))
                 (not (%hof-runtime-function-p (getf keys :key) 1))
                 (not (%hof-runtime-function-p (or (getf keys :test) (getf keys :test-not)) 2)))
        (let ((iv (gensym "ITEM")) (kv (gensym "KEY")) (tv (gensym "TEST")))
          (multiple-value-bind (tb test) (%hof-test iv keys tv)
            (multiple-value-bind (kb keyfn) (%hof-key kv (getf keys :key))
              (%hof-scan kind seq
                         (lambda (x) (funcall test (funcall keyfn x)))
                         `((,iv ,item))
                         ;; (the keyword arguments in their order)
                         (loop for (k nil) on keys by #'cddr append (if (eq k :key) kb tb))
                         (%hof-fallback kind name
                                        `(,iv :seq
                                          ,@(loop for (k v) on keys by #'cddr
                                                  append (list k (cond ((and (eq k :key) kb) kv)
                                                                       ((and (member k '(:test :test-not)) tb) tv)
                                                                       (t v)))))))))))))

  (defun %hof-assoc-expansion (item pred alist keys negate)
    "ASSOC of ITEM with :TEST, :TEST-NOT or :KEY, or ASSOC-IF of PRED (ITEM
NIL): the loop, or NIL."
    (let ((keys (%hof-keys keys (if pred '(:key) '(:key :test :test-not)))))
      (when (and (not (eq keys :no)) (or pred keys) (not (and (getf keys :test) (getf keys :test-not)))
                 (not (%hof-runtime-function-p pred 1)) (not (%hof-runtime-function-p (getf keys :key) 1))
                 (not (%hof-runtime-function-p (or (getf keys :test) (getf keys :test-not)) 2)))
        (let ((iv (gensym "ITEM")) (kv (gensym "KEY")) (tv (gensym "TEST"))
              (s (gensym "ALIST")) (l (gensym "L")) (p (gensym "P")) (b (gensym "B")))
          (multiple-value-bind (tb test)
              (if pred
                  (multiple-value-bind (fb call) (%hof-fn tv pred)
                    (values fb (lambda (x) (let ((c (funcall call (list x)))) (if negate `(not ,c) c)))))
                  (%hof-test iv keys tv))
            (multiple-value-bind (kb keyfn) (%hof-key kv (getf keys :key))
              `(let* (,@(if pred tb `((,iv ,item))) (,s ,alist)
                      ,@(loop for (k nil) on keys by #'cddr append (if (eq k :key) kb (unless pred tb))))
                 (block ,b
                   (%do ((,l ,s (cdr ,l))) ((%endp ,l) nil)
                     (let ((,p (car ,l)))
                       (when (and ,p ,(funcall test (funcall keyfn `(car ,p))))
                         (return-from ,b ,p)))))))))))))
(%define-image-compiler-macro find-if (&whole form pred seq &rest keys) (%hof-pred-expansion form :find pred seq keys nil))
(%define-image-compiler-macro find-if-not (&whole form pred seq &rest keys) (%hof-pred-expansion form :find pred seq keys t))
(%define-image-compiler-macro position-if (&whole form pred seq &rest keys) (%hof-pred-expansion form :position pred seq keys nil))
(%define-image-compiler-macro position-if-not (&whole form pred seq &rest keys) (%hof-pred-expansion form :position pred seq keys t))
(%define-image-compiler-macro count-if (&whole form pred seq &rest keys) (%hof-pred-expansion form :count pred seq keys nil))
(%define-image-compiler-macro count-if-not (&whole form pred seq &rest keys) (%hof-pred-expansion form :count pred seq keys t))
(%define-image-compiler-macro remove-if (&whole form pred seq &rest keys) (%hof-pred-expansion form :remove pred seq keys nil))
(%define-image-compiler-macro remove-if-not (&whole form pred seq &rest keys) (%hof-pred-expansion form :remove pred seq keys t))
(%define-image-compiler-macro delete-if (&whole form pred seq &rest keys) (%hof-pred-expansion form :delete pred seq keys nil))
(%define-image-compiler-macro delete-if-not (&whole form pred seq &rest keys) (%hof-pred-expansion form :delete pred seq keys t))
(%define-image-compiler-macro member-if (&whole form pred list &rest keys) (%hof-pred-expansion form :member pred list keys nil))
(%define-image-compiler-macro member-if-not (&whole form pred list &rest keys) (%hof-pred-expansion form :member pred list keys t))
(%define-image-compiler-macro assoc-if (&whole form pred alist &rest keys)
  (or (%hof-assoc-expansion nil pred alist keys nil) form))
(%define-image-compiler-macro assoc-if-not (&whole form pred alist &rest keys)
  (or (%hof-assoc-expansion nil pred alist keys t) form))
(%define-image-compiler-macro assoc (&whole form item alist &rest keys)
  (or (and keys (%hof-assoc-expansion item nil alist keys nil)) form))
(%define-image-compiler-macro count (&whole form item seq &rest keys)
  (or (%hof-item-expansion :count 'count item seq keys) form))
(%define-image-compiler-macro remove (&whole form item seq &rest keys)
  (or (%hof-item-expansion :remove 'remove item seq keys) form))
(%define-image-compiler-macro delete (&whole form item seq &rest keys)
  (or (%hof-item-expansion :delete 'delete item seq keys) form))
;; (the short literal sequences of the compiler macros of seq.lisp first)
(%define-image-compiler-macro find (&whole form item seq &rest keys)
  (or (and (eq (%short-constant-sequence-of keys seq) :no) (%hof-item-expansion :find 'find item seq keys))
      (%find-expansion form item seq keys)))
(%define-image-compiler-macro position (&whole form item seq &rest keys)
  (or (and (eq (%short-constant-sequence-of keys seq) :no) (%hof-item-expansion :position 'position item seq keys))
      (%position-expansion form item seq keys)))

;;; REDUCE of a function written in place (with :INITIAL-VALUE or :KEY, or
;;; over a list): the loop; #'+ and the like of a list without keywords as in
;;; seq.lisp
(%define-image-compiler-macro reduce (&whole form f seq &rest keys)
  (let ((op (and (null keys) (consp f) (member (car f) '(function quote)) (consp (cdr f))
                 (car (member (cadr f) '(+ * max min logand logior logxor)))))
        (keys (%hof-keys keys '(:key :initial-value))))
    (cond (op
           (let ((s (gensym "SEQ")) (acc (gensym "ACC")) (l (gensym "L")))
             `(let ((,s ,seq))
                (if (consp ,s)
                    (let ((,acc (car ,s)))
                      (do ((,l (cdr ,s) (cdr ,l))) ((endp ,l) ,acc)
                        (setq ,acc (,op ,acc (car ,l)))))
                    (%reduce-other #',op ,s)))))
          ((or (eq keys :no) (not (%direct-function-form-p f)) (%hof-runtime-function-p f 2)
               (%hof-runtime-function-p (getf keys :key) 1))
           (let ((l (and (not (eq keys :no)) (member :initial-value keys) (%arity-lambda f 2))))
             (if l `(reduce ,l ,seq ,@keys) form)))
          (t
           (let ((s (gensym "SEQ")) (acc (gensym "ACC")) (l (gensym "L")) (kv (gensym "KEY"))
                 (d (gensym "D")) (i (gensym "I")) (e (gensym "E")) (ivp (member :initial-value keys)))
             (multiple-value-bind (kb keyfn) (%hof-key kv (getf keys :key))
               (flet ((acc-step (x) `(setq ,acc ,(%literal-call f nil (list acc (funcall keyfn x))))))
                 ;; (the keywords are evaluated in their order, after the sequence)
                 `(let* ((,s ,seq)
                         ,@(loop for (k v) on keys by #'cddr
                                 collect (if (eq k :key) (or (car kb) (list (gensym "K") v)) (list acc v)))
                         ,@(unless ivp `((,acc nil))))
                    (declare (ignorable ,acc))
                    (cond ,@(if ivp
                                `(((listp ,s)
                                   (%do ((,l ,s (cdr ,l))) ((%endp ,l) ,acc)
                                     ,(acc-step `(car ,l))))
                                  ((vectorp ,s)
                                   (let* ((,d (if (%simple-vector-p ,s) ,s (%array-storage ,s)))
                                          (,i (if (%simple-vector-p ,s) 0 (%array-total-offset ,s)))
                                          (,e (+ ,i (length ,s))))
                                     (declare (fixnum ,i ,e))
                                     (%do () ((%fx>= ,i ,e) ,acc)
                                       ,(acc-step `(%vref ,d ,i))
                                       (setq ,i (%fx+ ,i 1))))))
                                `(((consp ,s)
                                   (setq ,acc ,(funcall keyfn `(car ,s)))
                                   (%do ((,l (cdr ,s) (cdr ,l))) ((%endp ,l) ,acc)
                                     ,(acc-step `(car ,l))))))
                          (t (locally (declare (notinline reduce))
                               (reduce ,f ,s ,@(loop for (k v) on keys by #'cddr
                                                     append (list k (if (eq k :key) (if kb kv v) acc)))))))))))))))
