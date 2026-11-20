;;;; clos.lisp — CLOS: classes, generic functions, methods, method combinations

;;; ------------------------------------------------------------
;;; metaobject layouts
;;; ------------------------------------------------------------
;;; A generic function is a closure whose name field holds its metaobject, an
;;; instance of (a subclass of) GENERIC-FUNCTION with the slots below.

;;; The world lock (runtime/thread.lisp): with green threads, one thread at
;;; a time compiles or changes the class system, and is not preempted
;;; meanwhile (it may still wait, e.g. in a macro expander or a method of the
;;; metaobject protocol).  Before a thread is made it costs a binding.
(defvar *threads-started* nil
  "True once a thread has been made: then blocking operations wait without
blocking the other threads (runtime/thread.lisp).")
(defvar *preemption-inhibited* nil
  "True while the thread must not be preempted (it holds the world lock);
it still waits where it waits.")
(defvar *world-lock* nil)

(defun %call-with-world-lock (function)
  (let ((*preemption-inhibited* t))
    (if (and *threads-started* (%can-wait-p))
        (let ((lock (or *world-lock* (setq *world-lock* (make-mutex :name "world lock"))))
              (got nil))
          (unwind-protect
               (progn (setq got (grab-mutex lock :recursive t))
                      (funcall function))
            (when got (release-mutex lock))))
        (funcall function))))

(defconstant +gf-name+ 0) (defconstant +gf-ll+ 1) (defconstant +gf-methods+ 2)
(defconstant +gf-combination+ 3) (defconstant +gf-cache+ 4) (defconstant +gf-nreq+ 5)
(defconstant +gf-eql+ 6) (defconstant +gf-doc+ 7) (defconstant +gf-function+ 8)
(defconstant +gf-apo+ 9) (defconstant +gf-declarations+ 10) (defconstant +gf-method-class+ 11)
(defconstant +gf-nopt+ 12) (defconstant +gf-restp+ 13) (defconstant +gf-keyp+ 14)
(defconstant +gf-keys+ 15) (defconstant +gf-initial-methods+ 16)
;;; +GF-FAST+: the number of required parameters (1..4) when the effective
;;; methods are functions of the arguments themselves, else 0.
;;; +GF-TABLE+: the part of the cache beyond the first entries.
;;; +GF-UNSPECIALIZED+: bit i is set when no method has a specializer other
;;; than T for the required parameter i.
(defconstant +gf-fast+ 17) (defconstant +gf-table+ 18) (defconstant +gf-unspecialized+ 19)
;;; +GF-KMAKER+: the keywords of the keyed main function of the generic
;;; function's function (%GF-KEYED-ENTRY), in the order of its slots, or NIL.
(defconstant +gf-kmaker+ 20)
;;; +GF-CORE+: the function of the spread arguments holding the dispatch of a
;;; generic function with optional or rest/keyword parameters (one copy,
;;; which its entries call: %GF-SCALL-CORE).
(defconstant +gf-core+ 21)

(defparameter *gf-slot-names*
  '(%name %lambda-list %methods %method-combination %cache %nreq %eql-specializers %documentation
    %function %argument-precedence-order %declarations %method-class %nopt %restp %keyp %keys
    %initial-methods %fast %table %unspecialized %kmaker %core))

;;; method: 0 gf 1 qualifiers 2 specializers 3 function 4 lambda-list 5 doc 6 keys
(defparameter *method-slot-names*
  '(%generic-function %qualifiers %specializers %function %lambda-list %documentation %keys))

;;; method combination: 0 name 1 options
(defparameter *method-combination-slot-names* '(%name %options))


(%define-raw-slots 'slot-definition
                   (let ((none (lambda () nil)))
                     (list (list '%name :name none) (list '%initargs :initargs none)
                           (list '%initform :initform none) (list '%initfunction :initfunction none)
                           (list '%readers :readers none) (list '%writers :writers none)
                           (list '%allocation :allocation (lambda () :instance))
                           (list '%type :type (lambda () t)) (list '%documentation :documentation none)
                           (list '%location nil none))))

(%define-raw-slots 'class
                   '(%name %direct-superclasses %direct-slots %class-precedence-list %slots
                     %direct-subclasses %finalized-p %kind %slot-index %default-initargs %documentation
                     %extra %shared %prototype %direct-default-initargs %protocol %plist %make-info
                     %instance-size %dispatch %inherits %shape-ops))

(setq *standard-metaclasses*
      (mapcar #'find-class '(standard-class funcallable-standard-class built-in-class structure-class
                             forward-referenced-class)))

(%define-raw-slots 'generic-function *gf-slot-names*)
(%define-raw-slots 'method *method-slot-names*)
(%define-raw-slots 'method-combination *method-combination-slot-names*)

(defun %method-qualifiers (m) (%instance-ref m 1))
(defun %method-specializers (m) (%instance-ref m 2))
(defun %method-function (m) (%instance-ref m 3))
(defun %method-gf (m) (%instance-ref m 0))

(defun %gf-info (f)
  (let ((info (and (functionp f) (%closure-name f))))
    (if (and (%instancep info) (%instancep (%instance-layout info))
             (%subclassp (%instance-layout info) (find-class 'generic-function)))
        info nil)))

(defun %generic-function-p (f) (not (null (%gf-info f))))

(defun %gf-slot (info i) (%instance-ref info i))
(defun %set-gf-slot (info i v) (%set-instance-ref info i v))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %program-error (control &rest args)
    (error 'simple-program-error :format-control control :format-arguments args)))

;;; ------------------------------------------------------------
;;; lambda lists
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %key-name (x)
    (cond ((symbolp x) (intern (symbol-name x) "KEYWORD"))
          ((consp (car x)) (caar x))
          (t (intern (symbol-name (car x)) "KEYWORD"))))

  (defun %ll-info (ll)
    "Returns (values nreq nopt restp keyp keys aokp)."
    (let ((state :req) (nreq 0) (nopt 0) (restp nil) (keyp nil) (keys nil) (aokp nil))
      (dolist (x ll)
        (cond ((eq x '&optional) (setq state :opt))
              ((or (eq x '&rest) (eq x '&body)) (setq state :rest restp t))
              ((eq x '&key) (setq state :key keyp t))
              ((eq x '&allow-other-keys) (setq aokp t))
              ((eq x '&aux) (setq state :aux))
              ((member x lambda-list-keywords) nil)
              ((eq state :req) (setq nreq (+ nreq 1)))
              ((eq state :opt) (setq nopt (+ nopt 1)))
              ((eq state :key) (push (%key-name x) keys))))
      (values nreq nopt restp keyp (nreverse keys) aokp)))

  (defun %parse-body (body doc-allowed)
    "Returns (values declarations doc forms)."
    (let ((decls nil) (doc nil))
      (loop
        (cond ((and (consp (car body)) (eq (caar body) 'declare)) (push (pop body) decls))
              ((and doc-allowed (stringp (car body)) (cdr body) (null doc)) (setq doc (pop body)))
              (t (return))))
      (values (nreverse decls) doc body)))

  (defun %safety-3-p (decls)
    "True when the DECLARE forms DECLS declare (OPTIMIZE (SAFETY 3))."
    (let ((safety nil))
      (dolist (d decls)
        (dolist (spec (cdr d))
          (when (and (consp spec) (eq (car spec) 'optimize))
            (dolist (q (cdr spec))
              (cond ((eq q 'safety) (setq safety 3))
                    ((and (consp q) (eq (car q) 'safety)) (setq safety (cadr q))))))))
      (eql safety 3))))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %check-gf-lambda-list (ll)
  (unless (listp ll) (%program-error "Invalid generic function lambda list ~S" ll))
  (let ((state :req) (vars nil))
    (dolist (x ll)
      (cond ((member x '(&optional &rest &key &allow-other-keys))
             (setq state x))
            ((member x lambda-list-keywords)
             (%program-error "~S is not allowed in a generic function lambda list ~S" x ll))
            ((symbolp x)
             (when (or (null x) (eq x t) (constantp x)) (%program-error "Invalid variable ~S in ~S" x ll))
             (when (member x vars) (%program-error "Duplicate variable ~S in ~S" x ll))
             (push x vars))
            ((and (consp x) (member state '(&optional &key)))
             (when (or (cdr x) (not (or (symbolp (car x)) (and (consp (car x)) (null (cddr (car x)))))))
               (%program-error "Default values are not allowed in a generic function lambda list ~S" ll))
             (push (if (consp (car x)) (cadr (car x)) (car x)) vars))
            (t (%program-error "Invalid generic function lambda list ~S" ll)))))))

(defun %congruent-p (gf-ll m-ll)
  "CLHS 7.6.4"
  (multiple-value-bind (r1 o1 rest1 key1 keys1) (%ll-info gf-ll)
    (multiple-value-bind (r2 o2 rest2 key2 keys2 aok2) (%ll-info m-ll)
      (and (= r1 r2) (= o1 o2)
           (eq (not (or rest1 key1)) (not (or rest2 key2)))
           (or (not key1) aok2 (and rest2 (not key2))
               (every (lambda (k) (member k keys2)) keys1))))))

(defun %gf-lambda-list-from-method (m-ll)
  "Lambda list of a generic function created by DEFMETHOD."
  (let ((r nil) (state :req))
    (dolist (x m-ll)
      (cond ((eq x '&optional) (setq state :opt) (push x r))
            ((or (eq x '&rest) (eq x '&body)) (setq state :rest) (push '&rest r))
            ((eq x '&key) (setq state :key) (push x r))
            ((eq x '&allow-other-keys) (setq state :aok))
            ((eq x '&aux) (setq state :aux))
            ((member x lambda-list-keywords) nil)
            ((eq state :req) (push (if (consp x) (car x) x) r))
            ((eq state :opt) (push (if (consp x) (car x) x) r))
            ((eq state :rest) (push x r))))
    (nreverse r)))

;;; ------------------------------------------------------------
;;; method combination objects
;;; ------------------------------------------------------------

(defun %make-method-combination (name options)
  (let ((mc (%allocate-instance (find-class 'method-combination))))
    (%set-instance-ref mc 0 name)
    (%set-instance-ref mc 1 options)
    mc))

(defvar *standard-method-combination* (%make-method-combination 'standard nil))

(defun %find-method-combination (spec)
  "SPEC is (name . options) or NIL."
  (cond ((null spec) *standard-method-combination*)
        ((and (eq (car spec) 'standard) (null (cdr spec))) *standard-method-combination*)
        ((and (symbolp (car spec)) (get (car spec) '%method-combination))
         (when (eq (car spec) 'standard)
           (%program-error "The STANDARD method combination accepts no options: ~S" spec))
         (%make-method-combination (car spec) (cdr spec)))
        ((%instancep spec) spec)
        (t (%program-error "Unknown method combination ~S" spec))))

;;; ------------------------------------------------------------
;;; generic function objects
;;; ------------------------------------------------------------

(defun %gf-set-lambda-list (info ll)
  (%set-gf-slot info +gf-ll+ ll)
  (multiple-value-bind (nreq nopt restp keyp keys) (%ll-info (if (eq ll :unspecified) nil ll))
    (%set-gf-slot info +gf-nreq+ nreq)
    (%set-gf-slot info +gf-nopt+ nopt)
    (%set-gf-slot info +gf-restp+ (or restp keyp (eq ll :unspecified)))
    (%set-gf-slot info +gf-keyp+ keyp)
    (%set-gf-slot info +gf-keys+ keys))
  (%gf-reset-cache info))

(defvar *the-standard-generic-function-class* nil)

;;; A function of the info making the function of a generic function, given
;;; by the expansions of DEFGENERIC and DEFMETHOD so that each generic
;;; function has its own dispatch code (whose calls of the methods the
;;; engine can then specialize).
(defvar *gf-function-maker* nil)

;;; N is the number of required parameters when there are one to four of them
;;; and no others: calls with N arguments are dispatched without an argument
;;; list (the other entries stay correct should the lambda list change).
;;; N may also be the shape (nreq nopt more) of a lambda list (%FAST-SHAPE):
;;; with optional or rest/keyword parameters, calls of one to four arguments
;;; are dispatched without an argument list too, the effective method being a
;;; function of the spread arguments (%GF-SCALL).
;;; :ALL gives every entry of one to four arguments such a dispatch.
(defvar *note-key-layout-hook* nil
  "Set by the compiler in the image: called with the name of a generic function
being compiled (DEFGENERIC, DEFMETHOD), the layout (nreq nopt . keywords) of
the keyed main function its function would have, and whether the form is a
DEFGENERIC; later calls compiled before the form is evaluated (COMPILE-FILE)
can then pass their arguments in its slots.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %note-key-layout (name ll defgeneric-p)
    (let ((hook (and (boundp '*note-key-layout-hook*) (symbol-value '*note-key-layout-hook*)))
          (shape (%fast-shape ll)))
      (when (and hook (symbolp name) shape (eql (third shape) 1))
        (let ((keys (nth-value 4 (%ll-info ll))))
          (when (and keys (<= (+ (first shape) (second shape) (length keys)) 8)
                     (<= (+ (first shape) (second shape) 1) 4))
            (funcall hook name (list* (first shape) (second shape) keys) defgeneric-p)))))))

(defmacro %gf-function-maker (&optional n keys)
  (when (and (consp n) (eql (second n) 0) (eql (third n) 0) (<= (first n) 4))
    (setq n (first n)))
  (cond ((and (consp n) (eql (third n) 1) keys (<= (+ (first n) (second n) (length keys)) 8)
              (<= (+ (first n) (second n) 1) 4))
         ;; with keyword parameters: also a keyed main function, which calls
         ;; with constant keywords compiled in the image call (%GF-KEYED-ENTRY)
         `(lambda (info)
            (%set-gf-slot info +gf-kmaker+ ',keys)
            (%set-gf-slot info +gf-core+ ,(%gf-spread-core n))
            (%make-gf-closure4k ,(%gf-spread-entry-n n)
                                ,@(%gf-spread-entries n)
                                ,(%gf-keyed-entry n keys))))
        ((and (consp n) (> (+ (first n) (second n) (third n)) 4))
         ;; more than four spread arguments: the dispatch is in the function
         ;; of the calls of more than four arguments (the others, which
         ;; leave out optional arguments, make the list of theirs)
         `(lambda (info)
            (%make-gf-closure4 ,(%gf-spread-entry-n n t)
                               (lambda (a) (%gf-invoke info (list a)))
                               (lambda (a b) (%gf-invoke info (list a b)))
                               (lambda (a b c) (%gf-invoke info (list a b c)))
                               (lambda (a b c d) (%gf-invoke info (list a b c d))))))
        ((consp n)
         ;; (the dispatch is copied once, into the core, which the entries call)
         `(lambda (info)
            (%set-gf-slot info +gf-core+ ,(%gf-spread-core n))
            (%make-gf-closure4 ,(%gf-spread-entry-n n)
                               ,@(%gf-spread-entries n t))))
        ((eq n :all)
         '(lambda (info)
           (%make-gf-closure4 (lambda (&rest args) (%gf-invoke info args))
            (lambda (a) (%gf-miss1 info a)) (lambda (a b) (%gf-miss2 info a b))
            (lambda (a b c) (%gf-miss3 info a b c)) (lambda (a b c d) (%gf-miss4 info a b c d)))))
        ((member n '(1 2 3 4))
         `(lambda (info)
            (let ((g (lambda (&rest args) (%gf-invoke info args))))
              (%make-gf-closure4 g
                                 ,(if (eql n 1) '(lambda (a) (%gf-call1 info a)) 'g)
                                 ,(if (eql n 2) '(lambda (a b) (%gf-call2 info a b)) 'g)
                                 ,(if (eql n 3) '(lambda (a b c) (%gf-call3 info a b c)) 'g)
                                 ,(if (eql n 4) '(lambda (a b c d) (%gf-call4 info a b c d)) 'g)))))
        (t '(lambda (info) (%make-list-gf-function info)))))

(defun %make-list-gf-function (info)
  "The function of a generic function called with a list of its arguments
(its lambda list has &optional, &rest or &key, or more than four required
parameters): calls with one to four arguments make the list directly."
  (%make-gf-closure4 (lambda (&rest args) (%gf-invoke info args))
                     (lambda (a) (%gf-invoke info (list a)))
                     (lambda (a b) (%gf-invoke info (list a b)))
                     (lambda (a b c) (%gf-invoke info (list a b c)))
                     (lambda (a b c d) (%gf-invoke info (list a b c d)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %fast-arity (ll)
    "The number of required parameters of the lambda list LL when a generic
function with it is dispatched by functions of the arguments (%GF-FUNCTION-MAKER)."
    (if (and (listp ll) (<= 1 (length ll) 4)
             (notany (lambda (x) (member x lambda-list-keywords)) ll))
        (length ll)
        nil))

  (defun %fast-shape (ll)
    "(nreq nopt more) of the (generic function or method) lambda list LL when
a generic function with it is dispatched by functions of the spread arguments:
the NREQ required ones, the NOPT optional ones (the unbound marker for those
not given) and, when MORE is 1 (&REST or &KEY), the list of the arguments after
them; at most four in all.  NIL otherwise."
    (when (and (listp ll) (null (cdr (last ll))))
      (let ((state :req) (nreq 0) (nopt 0) (more 0))
        (dolist (x ll)
          (cond ((eq x '&optional) (unless (eq state :req) (return-from %fast-shape nil)) (setq state :opt))
                ((member x '(&rest &body &key)) (setq more 1 state :more))
                ((eq x '&aux) (setq state :aux))
                ((member x lambda-list-keywords) (unless (eq x '&allow-other-keys) (return-from %fast-shape nil)))
                ((eq state :req) (setq nreq (+ nreq 1)))
                ((eq state :opt) (setq nopt (+ nopt 1)))))
        ;; (up to eight: the functions of more than four arguments are
        ;; called with a vector of them, but no list is made)
        (and (<= 1 nreq) (<= nopt 3) (<= (+ nreq nopt more) 8) (list nreq nopt more)))))

  (defun %shape-code (nreq nopt more)
    "The bits of the tag of the cache (+GF-CACHE+) that tell the shape."
    (+ nreq nopt more (* 256 nopt) (* 1024 more))))

;;; the shape bits of the tag of the cache
(defconstant +gf-shape-mask+ #x70f)

(defun %spread-list (nreq nopt more args)
  "The spread arguments (a list) of the arguments ARGS of a generic function
or method of the shape (NREQ NOPT MORE)."
  (let ((r nil) (a args))
    (dotimes (i nreq)
      (when (atom a) (error 'simple-program-error :format-control "Too few arguments: ~S"
                                                  :format-arguments (list args)))
      (push (car a) r) (setq a (cdr a)))
    (dotimes (i nopt)
      (if (consp a) (progn (push (car a) r) (setq a (cdr a))) (push (%unbound-marker) r)))
    ;; (the rest list may share the list of the arguments, CLHS 3.4.1.3)
    (cond ((eql more 1) (push a r))
          (a (error 'simple-program-error :format-control "Too many arguments: ~S"
                                          :format-arguments (list args))))
    (nreverse r)))

(defun %unspread (nreq nopt more spread)
  "The arguments whose spread arguments are SPREAD."
  (let ((r nil) (a spread))
    (dotimes (i nreq) (push (car a) r) (setq a (cdr a)))
    (dotimes (i nopt) (unless (%unboundp (car a)) (push (car a) r)) (setq a (cdr a)))
    (nreconc r (if (eql more 1) (car a) nil))))

(defun %gf-shape (info)
  "The shape (nreq nopt more) of a generic function dispatched by functions of
spread arguments that are not just its required ones, else NIL."
  (let ((nreq (%gf-slot info +gf-nreq+)) (fast (%gf-slot info +gf-fast+)))
    (if (or (eql fast 0) (eql fast nreq))
        nil
        (list nreq (%gf-slot info +gf-nopt+) (if (%gf-slot info +gf-restp+) 1 0)))))

;;; every generic function: their caches are emptied when a class is redefined
(defvar *generic-function-infos* nil)

(defun %make-generic-function (name lambda-list combination class)
  (let* ((info (%allocate-instance (or class (find-class 'standard-generic-function))))
         (f (progn
              ;; (the maker may set slots)
              (dotimes (i (length *gf-slot-names*)) (%set-gf-slot info i nil))
              (if *gf-function-maker*
                  (funcall *gf-function-maker* info)
                  (funcall (%gf-function-maker :all) info)))))
    (%set-gf-slot info +gf-name+ name)
    (%set-gf-slot info +gf-combination+ (%find-method-combination combination))
    (%set-gf-slot info +gf-method-class+ (find-class 'standard-method))
    (%gf-set-lambda-list info lambda-list)
    (%set-closure-name f info)
    (%set-gf-slot info +gf-function+ f)
    (push info *generic-function-infos*)
    f))

;;; The cache (+GF-CACHE+) is a simple-vector
;;;   #(tag fast eql unspecialized table  key-0 ... key-(nreq-1) value methods  key-0 ...)
;;; (FAST, EQL, UNSPECIALIZED: +GF-FAST+, +GF-EQL+, +GF-UNSPECIALIZED+; TABLE:
;;; the vector of +GF-TABLE+ or NIL -- what the dispatch needs, in one object)
;;; searched without consing.  A key is the class of a required argument, or
;;; the marker (object . hash) of the EQL specializer object the argument is
;;; (+GF-EQL+: NIL or a simple-vector of the markers per parameter), or 0 for
;;; a parameter that no method specializes (+GF-UNSPECIALIZED+).  VALUE is a
;;; function of the
;;; spread arguments when +GF-FAST+ (their number) is not 0, else
;;; (effective-method-function . valid-keys), a function of the argument list;
;;; METHODS are the applicable methods.  TAG is (%SHAPE-CODE nreq nopt more)
;;; + 16 * (the first four bits of +GF-UNSPECIALIZED+), or 0 when +GF-FAST+ is.  The dispatch inlined in the
;;; function of the generic function compares the classes of the arguments
;;; with the keys: it never finds the entries with EQL markers, and the value
;;; of an entry for a class that EQL specializer objects of that parameter
;;; have first tests whether the argument is one of them (%EQL-GUARDED-VALUE).
;;; With more than +GF-CACHE-ENTRIES+ entries they all move to +GF-TABLE+:
;;; (count . #(mask key... value methods ...)), an open-addressing hash table
;;; on the keys (the cache then has no entries).  The search of
;;; the entries is cheaper than a call of the function that probes the table
;;; up to some twenty entries; probing the table in the function of the
;;; generic function itself is slower than the search too until some thirty
;;; (the address of the method then depends on a chain of loads from the
;;; argument, which a mispredicted call waits for).
(defconstant +gf-cache-entries+ 32)
(defconstant +gf-cache-header+ 5)

(defun %gf-reset-cache (info)
  (%bump-clos-epoch)                    ; the caches of call sites
  (let* ((nreq (%gf-slot info +gf-nreq+))
         (nopt (%gf-slot info +gf-nopt+))
         (more (if (%gf-slot info +gf-restp+) 1 0))
         (fast (if (and (fixnump nreq) (<= 1 nreq) (fixnump nopt) (<= nopt 3) (<= (+ nreq nopt more) 8)
                        (not (eq (%gf-slot info +gf-ll+) :unspecified))
                        (or (null *the-standard-generic-function-class*)
                            (eq (%instance-layout info) *the-standard-generic-function-class*)))
                   (+ nreq nopt more) 0)))
    (%set-gf-slot info +gf-fast+ fast)
    (unless (%gf-slot info +gf-unspecialized+) (%set-gf-slot info +gf-unspecialized+ 0))
    (%set-gf-slot info +gf-cache+
                  (vector (if (eql fast 0)
                              0
                              (+ (%shape-code nreq nopt more)
                                 (* 16 (logand (%gf-slot info +gf-unspecialized+) 15))
                                 (if (%gf-slot info +gf-keyp+) 4096 0)
                                 ;; the keyed main function applies
                                 (let ((mk (%gf-slot info +gf-kmaker+)) (gfkeys (%gf-slot info +gf-keys+)))
                                   (if (and mk (eql more 1) (%gf-slot info +gf-keyp+)
                                            (<= (+ nreq nopt (length mk)) 8) (<= (+ nreq nopt 1) 4)
                                            (every (lambda (k) (member k gfkeys)) mk))
                                       8192 0))))
                          fast (%gf-slot info +gf-eql+) (%gf-slot info +gf-unspecialized+) nil))
    (%set-gf-slot info +gf-table+ nil)))

(defun %gf-invalidate (info)
  (%gf-reset-cache info)
  (%clear-initarg-caches))

(defvar *initarg-cache* nil)
(defvar *ctor-generation* 0)
(defun %clear-initarg-caches ()
  (setq *ctor-generation* (+ *ctor-generation* 1))
  (when *initarg-cache* (clrhash *initarg-cache*)))

(defun %apo-indices (gf-ll apo)
  (let ((req (let ((r nil))
               (dolist (x gf-ll (nreverse r))
                 (when (member x lambda-list-keywords) (return (nreverse r)))
                 (push x r)))))
    (unless (and (= (length apo) (length req))
                 (every (lambda (x) (member x req)) apo)
                 (= (length (remove-duplicates apo)) (length apo)))
      (%program-error "Invalid argument precedence order ~S for lambda list ~S" apo gf-ll))
    (mapcar (lambda (x) (position x req)) apo)))

(defun ensure-generic-function (name &rest keys
                                     &key (lambda-list nil llp) (method-combination nil mcp) documentation
                                       generic-function-class method-class
                                       (argument-precedence-order nil apop)
                                       declarations ((:declare declare-option)) environment
                                       &allow-other-keys)
  (declare (ignore environment))
  (unless (evenp (length keys))
    (%program-error "ENSURE-GENERIC-FUNCTION: odd number of keyword arguments ~S" keys))
  (when (and (symbolp name)
             (or (special-operator-p name) (macro-function name)
                 (and (fboundp name) (not (%generic-function-p (fdefinition name))))))
    (%program-error "~S already names an ordinary function, macro or special operator" name))
  (when (and (consp name) (fboundp name) (not (%generic-function-p (fdefinition name))))
    (%program-error "~S already names an ordinary function" name))
  (when llp (%check-gf-lambda-list lambda-list))
  (let* ((existing (and (fboundp name) (fdefinition name)))
         (gfclass (cond ((null generic-function-class) nil)
                        ((%class-p generic-function-class) generic-function-class)
                        (t (find-class generic-function-class))))
         (f (if (and existing (%generic-function-p existing))
                existing
                (let ((f (%make-generic-function name (if llp lambda-list :unspecified)
                                                 method-combination gfclass)))
                  (%set-fdefinition name f)
                  f)))
         (info (%gf-info f)))
    (when (and gfclass (not (eq gfclass (%instance-layout info))))
      (%set-instance-layout info gfclass))
    (when (and llp (not (equal lambda-list (%gf-slot info +gf-ll+))))
      (dolist (m (%gf-slot info +gf-methods+))
        (unless (%congruent-p lambda-list (%instance-ref m 4))
          (error 'simple-error
                 :format-control "The lambda list ~S of ~S is not congruent with its method ~S"
                 :format-arguments (list lambda-list name m))))
      (%gf-set-lambda-list info lambda-list))
    (when apop
      (%set-gf-slot info +gf-apo+ (%apo-indices (%gf-slot info +gf-ll+) argument-precedence-order)))
    (when mcp
      (%set-gf-slot info +gf-combination+ (%find-method-combination method-combination)))
    (when documentation (%set-gf-slot info +gf-doc+ documentation))
    (when (or declarations declare-option)
      (%set-gf-slot info +gf-declarations+ (or declarations declare-option)))
    (when method-class
      (%set-gf-slot info +gf-method-class+ (if (%class-p method-class) method-class (find-class method-class))))
    (%gf-invalidate info)
    f))

(defun %ensure-gf-for-method (name lambda-list)
  (let ((existing (and (fboundp name) (fdefinition name))))
    (if (and existing (%generic-function-p existing))
        existing
        (ensure-generic-function name :lambda-list (%gf-lambda-list-from-method lambda-list)))))

;;; ------------------------------------------------------------
;;; methods
;;; ------------------------------------------------------------

;;; specializers: class objects or (eql value)
(defun %parse-specializer (s)
  (cond ((and (consp s) (eq (car s) 'eql) (consp (cdr s)) (null (cddr s))) s)
        ((%class-p s) s)
        ((and (symbolp s) s)
         (or (find-class s nil)
             (%ensure-class-object s (find-class 'forward-referenced-class) nil nil :forward)))
        (t (error 'simple-error :format-control "Bad specializer ~S" :format-arguments (list s)))))

(defun %specializer-equal (a b)
  (if (consp a) (and (consp b) (eql (cadr a) (cadr b))) (eq a b)))

(defun %make-method-object (class qualifiers specializers function lambda-list doc)
  (let ((m (%allocate-instance (or class (find-class 'standard-method)))))
    (dotimes (i (length *method-slot-names*)) (%set-instance-ref m i nil))
    (%set-instance-ref m 1 qualifiers)
    (%set-instance-ref m 2 specializers)
    (%set-instance-ref m 3 function)
    (%set-instance-ref m 4 lambda-list)
    (%set-instance-ref m 5 doc)
    (%set-instance-ref m 6 (multiple-value-bind (nreq nopt restp keyp keys aokp) (%ll-info lambda-list)
                             (declare (ignore nreq nopt restp))
                             (cond (aokp :any) (keyp keys) (t nil))))
    m))

(defvar *eql-marker-count* 0)
(defun %update-dispatch-info (info)
  "Set +GF-EQL+ and +GF-UNSPECIALIZED+ from the methods."
  (let ((eqls nil) (specialized 0) (top (find-class t)))
    (dolist (m (%gf-slot info +gf-methods+))
      (let ((i 0))
        (dolist (s (%method-specializers m))
          (unless (eq s top)
            (when (< i 24) (setq specialized (logior specialized (ash 1 i)))))
          (when (consp s)
            (unless eqls (setq eqls (make-array (length (%method-specializers m)) :initial-element nil)))
            (let ((x (cadr s)))
              (unless (dolist (k (svref eqls i) nil) (when (eql (car k) x) (return t)))
                (setq *eql-marker-count* (%fxlogand (+ *eql-marker-count* 1) #xFFFFF))
                (push (cons x *eql-marker-count*) (svref eqls i)))))
          (setq i (+ i 1)))))
    (%set-gf-slot info +gf-eql+ eqls)
    (%set-gf-slot info +gf-unspecialized+ (logxor specialized #xFFFFFF))))

(defvar *add-method-hook* nil)

(defun %add-method-unlocked (gf method)
  (let* ((info (%gf-info gf))
         (specs (%method-specializers method))
         (quals (%method-qualifiers method)))
    (when (and (%method-gf method) (not (eq (%method-gf method) gf)))
      (error 'simple-error :format-control "The method ~S already belongs to the generic function ~S"
                           :format-arguments (list method (%method-gf method))))
    (if (eq (%gf-slot info +gf-ll+) :unspecified)
        (%gf-set-lambda-list info (%gf-lambda-list-from-method (%instance-ref method 4)))
        (unless (%congruent-p (%gf-slot info +gf-ll+) (%instance-ref method 4))
          (error 'simple-error
                 :format-control "The lambda list of method ~S is not congruent with that of ~S (~S)"
                 :format-arguments (list method (%gf-slot info +gf-name+) (%gf-slot info +gf-ll+)))))
    (let ((old (find-if (lambda (m) (and (equal (%method-qualifiers m) quals)
                                         (= (length (%method-specializers m)) (length specs))
                                         (every #'%specializer-equal (%method-specializers m) specs)))
                        (%gf-slot info +gf-methods+))))
      (when old (%set-instance-ref old 0 nil))
      (%set-instance-ref method 0 gf)
      (when (and old *add-method-hook*) (funcall *add-method-hook* old nil))
      (%set-gf-slot info +gf-methods+ (cons method (remove old (%gf-slot info +gf-methods+))))
      (when *add-method-hook* (funcall *add-method-hook* method t)))
    (%update-dispatch-info info)
    (%gf-invalidate info)
    gf))


(defun %add-method (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'%add-method-unlocked args))))

(defun %remove-method-unlocked (gf method)
  (let ((info (%gf-info gf)))
    (when (member method (%gf-slot info +gf-methods+))
      (%set-gf-slot info +gf-methods+ (remove method (%gf-slot info +gf-methods+)))
      (when *add-method-hook* (funcall *add-method-hook* method nil))
      (%set-instance-ref method 0 nil)
      (%update-dispatch-info info)
      (%gf-invalidate info))
    gf))


(defun %remove-method (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'%remove-method-unlocked args))))

(defun %find-method (gf qualifiers specializers errorp)
  (let ((info (%gf-info gf)))
    (unless (= (length specializers) (%gf-slot info +gf-nreq+))
      (error 'simple-error
             :format-control "FIND-METHOD: ~S specializers given, but ~S has ~S required parameters"
             :format-arguments (list (length specializers) gf (%gf-slot info +gf-nreq+))))
    (let* ((specs (mapcar (lambda (s) (if (and (symbolp s) (not (find-class s nil)))
                                          (error 'simple-error :format-control "No class named ~S"
                                                               :format-arguments (list s))
                                          (%parse-specializer s)))
                          specializers))
           (m (find-if (lambda (m) (and (equal (%method-qualifiers m) qualifiers)
                                        (every #'%specializer-equal (%method-specializers m) specs)))
                       (%gf-slot info +gf-methods+))))
      (if (or m (not errorp)) m
          (error 'simple-error :format-control "No method of ~S with qualifiers ~S and specializers ~S"
                               :format-arguments (list gf qualifiers specializers))))))

(defun %defmethod-unlocked (name qualifiers specializers lambda-list function doc cell)
  (let* ((gf (%ensure-gf-for-method name lambda-list))
         (info (%gf-info gf))
         (m (%make-method-object (%gf-slot info +gf-method-class+) qualifiers
                                 (mapcar #'%parse-specializer specializers) function lambda-list doc)))
    (when cell (setf (car cell) m))
    (%add-method gf m)
    m))

;;; ------------------------------------------------------------
;;; dispatch
;;; ------------------------------------------------------------


(defun %defmethod (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'%defmethod-unlocked args))))

(defun %specializer-applicable-p (spec arg)
  (if (consp spec)
      (eql arg (cadr spec))
      (%subclassp (class-of arg) spec)))

(defun %method-applicable-p (m args)
  (do ((specs (%method-specializers m) (cdr specs)) (a args (cdr a)))
      ((null specs) t)
    (unless (%specializer-applicable-p (car specs) (car a)) (return nil))))

(defun %specializer-more-specific-p (s1 s2 arg)
  (cond ((%specializer-equal s1 s2) nil)
        ((consp s1) t)
        ((consp s2) nil)
        (t (let ((cpl (%class-cpl (class-of arg))))
             (< (or (position s1 cpl) 1000) (or (position s2 cpl) 1000))))))

(defun %method-more-specific-p (m1 m2 args apo)
  (let ((s1 (%method-specializers m1)) (s2 (%method-specializers m2)))
    (if apo
        (dolist (i apo nil)
          (let ((a (nth i s1)) (b (nth i s2)) (arg (nth i args)))
            (cond ((%specializer-more-specific-p a b arg) (return t))
                  ((%specializer-more-specific-p b a arg) (return nil)))))
        (do ((s1 s1 (cdr s1)) (s2 s2 (cdr s2)) (a args (cdr a)))
            ((null s1) nil)
          (cond ((%specializer-more-specific-p (car s1) (car s2) (car a)) (return t))
                ((%specializer-more-specific-p (car s2) (car s1) (car a)) (return nil)))))))

(defun %compute-applicable-methods (gf args)
  (let* ((info (%gf-info gf))
         (ms (remove-if-not (lambda (m) (%method-applicable-p m args)) (%gf-slot info +gf-methods+)))
         (apo (%gf-slot info +gf-apo+)))
    (stable-sort (copy-list ms) (lambda (a b) (%method-more-specific-p a b args apo)))))

(defun %gf-arg-error (info control &rest args)
  (error 'simple-program-error
         :format-control "~S: ~A"
         :format-arguments (list (%gf-slot info +gf-name+) (apply #'format nil control args))))

;;; Obsolete instances (made before their class was redefined with other
;;; slots, see %ENSURE-CURRENT-INSTANCE) exist only for the classes that
;;; were.  The caches of slot locations (call sites of accessors, cached
;;; SLOT-VALUE) keep the locations of the other classes only, and use them
;;; without a check: when a class is redefined with other slots, the caches
;;; of its slots are emptied (*CLOS-EPOCH*, a new slot index).  The accessor
;;; functions of a redefined class compare the instance's number of slots
;;; with the class's.
(defun %class-redefined-p (class) (and (assoc class *obsolete-slot-indexes*) t))
(defmacro %slot-count-is-p (o n)
  `(%fx= (%svlength (%instance-slots ,o)) ,n))

(defmacro %fast-class-of (x)
  ;; The objects that generic functions of built-in classes (printers,
  ;; encoders) are given most find their class in CLASS-OF's cache without a
  ;; call of CLASS-OF, which is in another module for a function compiled in
  ;; the image (the engine does not inline it): fixnums, symbols, simple
  ;; strings, conses, doubles and strings with a fill pointer over a simple
  ;; string.  One index, one read of the cache and one call for the others,
  ;; as this is copied into the dispatch of every generic function.
  `(let ((x ,x))
     (if (%instancep x)
         (%instance-layout x)
         (let ((i (cond ((fixnump x) 0)
                        ((and x (symbolp x)) (if (eq (%symbol-package x) *keyword-package*) 3 4))
                        ((%simple-string-p x) 6)
                        ((consp x) 1)
                        ((%double-float-p x) 15)
                        ((and (%mdarray-p x) (%simple-string-p (%mdarray-data x))
                              (let ((d (%mdarray-dims x))) (and (consp d) (null (cdr d)))))
                         7)
                        (t -1))))
           (declare (fixnum i))
           (or (and (%fx>= i 0) (%svref *builtin-classes* i)) (class-of x))))))

(defun %eql-key (markers x)
  "The cache key of the argument X for a parameter with the EQL specializer
objects of MARKERS: the marker of the object that X is, else the class of X."
  (if (or (fixnump x) (symbolp x) (%instancep x))
      (dolist (k markers) (when (eq (car k) x) (return-from %eql-key k)))
      (dolist (k markers) (when (eql (car k) x) (return-from %eql-key k))))
  (%fast-class-of x))

;;; the cache key of the argument X at position I (UNSPEC: +GF-UNSPECIALIZED+,
;;; EQLS: +GF-EQL+)
(defmacro %arg-key (unspec eqls i x)
  `(cond ((not (eq (%fxlogand ,unspec ,(if (integerp i) (ash 1 i) `(ash 1 ,i))) 0)) 0)
         ((and ,eqls (%svref ,eqls ,i)) (%eql-key (%svref ,eqls ,i) ,x))
         (t (%fast-class-of ,x))))

;;; The hash of a key.  Identity hashes are numbers given in order (classes
;;; defined together have hashes at regular distances): higher bits are
;;; folded into the low ones, which index the table.
(defmacro %key-hash (k)
  `(let ((k ,k))
     (if (fixnump k)
         0
         (let ((h (%fxlogand (the fixnum (if (consp k)
                                             (cdr k)
                                             ;; (a class: its identity hash is cached in it)
                                             (let ((h (%instance-raw-hash k)))
                                               (if (eq h 0) (%identity-hash k) h))))
                             #xFFFFF)))
           (declare (fixnum h))
           (%fxlogxor h (%fxlogxor (%fxash-right h 3) (%fxash-right h 6)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %splice-args (tree vars)
    "TREE with the elements %ARGS of its lists replaced by the elements of VARS."
    (cond ((atom tree) tree)
          ((eq (car tree) '%args) (append vars (%splice-args (cdr tree) vars)))
          (t (cons (%splice-args (car tree) vars) (%splice-args (cdr tree) vars)))))
  (defun %arity-vars (n)
    (let ((r nil)) (dotimes (i n (nreverse r)) (push (nth i '(%a0 %a1 %a2 %a3 %a4 %a5 %a6 %a7)) r)))))

;;; (%ARITY-CASE n form): FORM for N = 1..8, with %ARGS in a list of FORM
;;; standing for N variables.
(defmacro %arity-case (n form)
  `(case ,n
     (1 ,(%splice-args form (%arity-vars 1)))
     (2 ,(%splice-args form (%arity-vars 2)))
     (3 ,(%splice-args form (%arity-vars 3)))
     (4 ,(%splice-args form (%arity-vars 4)))
     (5 ,(%splice-args form (%arity-vars 5)))
     (6 ,(%splice-args form (%arity-vars 6)))
     (7 ,(%splice-args form (%arity-vars 7)))
     (t ,(%splice-args form (%arity-vars 8)))))

(defun %gf-keys (info args nreq)
  (let ((eqls (%gf-slot info +gf-eql+)) (unspec (%gf-slot info +gf-unspecialized+)) (r nil) (a args))
    (dotimes (i nreq (nreverse r))
      (push (if (< i 24) (%arg-key unspec eqls i (car a)) (class-of (car a))) r)
      (setq a (cdr a)))))

;;; the hash of several keys: the low bits must depend on all the bits of the
;;; keys (the identity hashes of classes defined together are consecutive)
(defmacro %mix-hash (h k)
  `(let ((h ,h))
     (%fxlogand (%fxlogxor (%fxlogxor (%fx* h 32) (%fxash-right h 3)) ,k) #xFFFFF)))

(defun %keys-hash (keys)
  (let ((h (%key-hash (car keys))))
    (dolist (k (cdr keys) h) (setq h (%mix-hash h (%key-hash k))))))

(defun %gf-table-find (vec keys nreq)
  "The index of the value of KEYS in the table VEC, or NIL."
  (let* ((stride (+ nreq 2)) (mask (svref vec 0)) (i (logand (%keys-hash keys) mask)))
    (loop
      (let ((base (+ 1 (* i stride))))
        (cond ((null (svref vec base)) (return nil))
              ((do ((j base (+ j 1)) (l keys (cdr l))) ((null l) t)
                 (unless (eq (svref vec j) (car l)) (return nil)))
               (return (+ base nreq))))
        (setq i (logand (+ i 1) mask))))))

(defun %gf-table-insert (vec keys nreq value methods)
  (let* ((stride (+ nreq 2)) (mask (svref vec 0)) (i (logand (%keys-hash keys) mask)))
    (loop
      (let ((base (+ 1 (* i stride))))
        (when (null (svref vec base))
          (do ((j base (+ j 1)) (l keys (cdr l))) ((null l))
            (setf (svref vec j) (car l)))
          (setf (svref vec (+ base nreq)) value (svref vec (+ base nreq 1)) methods)
          (return))
        (setq i (logand (+ i 1) mask))))))

(defun %gf-make-table (capacity nreq)
  (let ((vec (make-array (+ 1 (* capacity (+ nreq 2))) :initial-element nil)))
    (setf (svref vec 0) (- capacity 1))
    vec))

(defun %gf-table-put (info keys nreq value methods)
  (let ((table (or (%gf-slot info +gf-table+)
                   (let ((tb (cons 0 (%gf-make-table 16 nreq)))) (%set-gf-slot info +gf-table+ tb) tb))))
    (when (> (* 2 (+ (car table) 1)) (svref (cdr table) 0))
      (let* ((old (cdr table)) (new (%gf-make-table (* 2 (+ (svref old 0) 1)) nreq)) (stride (+ nreq 2)))
        (do ((b 1 (+ b stride))) ((>= b (length old)))
          (unless (null (svref old b))
            (%gf-table-insert new (let ((r nil)) (dotimes (j nreq (nreverse r)) (push (svref old (+ b j)) r)))
                              nreq (svref old (+ b nreq)) (svref old (+ b nreq 1)))))
        (setf (cdr table) new)))
    (%gf-table-insert (cdr table) keys nreq value methods)
    (setf (car table) (+ (car table) 1))
    (setf (svref (%gf-slot info +gf-cache+) 4) (cdr table))))

;;; the variable of VARS at the position P
(defmacro %pick (p &rest vars)
  `(case ,p
     ,@(let ((i -1) (r nil))
         (dolist (v vars (nreverse r))
           (setq i (+ i 1))
           (push (list (if (cdr (member v vars)) i t) v) r)))))

(defun %eql-guarded-value (info keys value n)
  "VALUE (a function of the N arguments, the cache entry of the keys KEYS),
or a function that calls it unless an argument is an EQL specializer object
whose class is the key of its parameter: such an argument has an entry of its
own, which the dispatch on classes does not find."
  (let ((eqls (%gf-slot info +gf-eql+)) (guards nil) (p 0) (nkeys (%gf-slot info +gf-nreq+)))
    (dolist (k keys)
      (let ((markers (and (< p (length eqls)) (svref eqls p))))
        (when (and markers (%instancep k))
          (let ((objects nil))
            (dolist (m markers) (when (eq (class-of (car m)) k) (push (car m) objects)))
            (when objects (push (cons p objects) guards)))))
      (setq p (+ p 1)))
    (cond ((or (null guards) (> n 8) (< n 1)) value)
          ((null (cdr guards))
           ;; one such parameter: the entries of its objects are remembered
           (let ((p (caar guards)) (cache (mapcar (lambda (o) (cons o nil)) (cdar guards))))
             (macrolet ((guard (test)
                          `(%arity-case n
                             (lambda (%args)
                               (let ((x (%pick p %args)))
                                 (dolist (e cache (funcall (the function value) %args))
                                   (when (,test (car e) x)
                                     (return (funcall (the function
                                                           (or (cdr e)
                                                               (setf (cdr e) (%gf-lookup info (list %args) nkeys 0))))
                                                      %args)))))))))
               (if (every (lambda (o) (or (fixnump o) (symbolp o) (characterp o) (%instancep o))) (cdar guards))
                   (guard eq)
                   (guard eql)))))
          (t
           ;; several: the objects the arguments are make a number (0: none),
           ;; under which the function of their entry is remembered
           (let ((memo nil))
             (%arity-case n
               (lambda (%args)
                 (let ((code 0) (radix 1))
                   (declare (fixnum code radix))
                   (dolist (g guards)
                     (let ((x (%pick (car g) %args)) (i 1))
                       (declare (fixnum i))
                       (dolist (o (cdr g)) (when (eql o x) (setq code (%fx+ code (%fx* i radix))) (return))
                         (setq i (%fx+ i 1)))
                       (setq radix (%fx* radix (%fx+ 1 (length (cdr g)))))))
                   (if (eq code 0)
                       (funcall (the function value) %args)
                       (funcall (the function
                                     (let ((e (assoc code memo)))
                                       (if e
                                           (cdr e)
                                           (let ((f (%gf-lookup info (list %args) nkeys 0)))
                                             (push (cons code f) memo)
                                             f))))
                                %args))))))))))

(defmacro %single-key (&rest keys)
  "The one of the cache keys KEYS that is not 0 (the class of the one
specialized argument), or 0."
  (if (null (cdr keys))
      (car keys)
      `(cond ,@(mapcar (lambda (k)
                         `((and ,@(mapcar (lambda (o) `(eq ,o 0)) (remove k keys))) ,k))
                       keys)
             (t 0))))

;;; A generic function with more cache entries than are searched in its own
;;; code (+GF-CACHE-ENTRIES+, then the hash table) and one specialized
;;; argument keeps the function for each class of that argument in the class
;;; itself: +CLASS-DISPATCH+ holds #(info cache function ...), an entry being
;;; current while the cache (+GF-CACHE+) of the generic function is that
;;; vector.  The classes a call meets keep the few generic functions they are
;;; dispatched by in a short vector, which is searched more cheaply than the
;;; hash table of a generic function of many classes.
(defconstant +class-dispatch-entries+ 16)

(defun %gf-class-lookup (pic class)
  (and (%instancep class)
       (let ((v (%instance-ref class +class-dispatch+)))
         (and v
              (do ((j 1 (%fx+ j 3))) ((%fx>= j (%svlength v)) nil)
                (when (eq (%svref v j) pic) (return (%svref v (%fx+ j 1)))))))))

(defun %gf-class-note (info pic class value)
  "VALUE, remembered in CLASS as the function of the generic function INFO
(whose cache is PIC) for its instances."
  (when (and (%instancep class) (functionp value) (%class-p class))
    (let* ((v (%instance-ref class +class-dispatch+))
           (n (if v (length v) 0))
           (j (do ((j 0 (+ j 3))) ((>= j n) nil)
                ;; this generic function's entry, or one no longer current
                (let ((other (svref v j)))
                  (when (or (eq other info) (not (eq (%gf-slot other +gf-cache+) (svref v (+ j 1)))))
                    (return j))))))
      (unless j
        (if (< n (* 3 +class-dispatch-entries+))
            (let ((new (make-array (+ n 3))))
              (dotimes (i n) (setf (svref new i) (svref v i)))
              (%set-instance-ref class +class-dispatch+ new)
              (setq v new j n))
            (setq j 0)))
      (setf (svref v j) info (svref v (+ j 1)) pic (svref v (+ j 2)) value)))
  value)

(defun %gf-find-big (info spread nkeys n)
  "%GF-FINDn for more than four spread arguments (SPREAD, a list), of which
the first NKEYS are the required ones."
  (and (eq (%svref (%gf-slot info +gf-cache+) 1) n) (eql (%gf-slot info +gf-nreq+) nkeys)
       (%gf-lookup info spread nkeys 0)))

(defun %gf-fill (info keys args nreq which)
  "Compute the cache entry for ARGS (whose keys are KEYS) and store it."
  (let ((pic (%gf-slot info +gf-cache+)))
    (multiple-value-bind (value methods) (%gf-compute-value info args)
      (when (and (functionp value) (%gf-slot info +gf-eql+))
        (setq value (%eql-guarded-value info keys value (%gf-slot info +gf-fast+))))
      ;; computing the effective method may have invalidated the cache
      (when (eq pic (%gf-slot info +gf-cache+))
        (%gf-store info keys nreq value methods)
        (when (and (functionp value) (not (eql (%gf-slot info +gf-fast+) 0)))
          (%gf-add-eql-class-entry info keys args nreq)))
      (if (eql which 0) value methods))))

(defun %gf-store (info keys nreq value methods)
  "Add the entry of KEYS to the cache (or to its table)."
  (let* ((pic (%gf-slot info +gf-cache+)) (len (length pic)) (stride (+ nreq 2)))
    (cond ((%gf-slot info +gf-table+) (%gf-table-put info keys nreq value methods))
          ((< (- len +gf-cache-header+) (* +gf-cache-entries+ stride))
           ;; the dispatch inlined in the function of the generic function
           ;; searches from the first entry and never finds those of EQL
           ;; markers: the others go first
           (let* ((new (make-array (+ len stride)))
                  (front (notany #'consp keys))
                  (j (if front +gf-cache-header+ len)))
             (dotimes (i +gf-cache-header+) (setf (svref new i) (svref pic i)))
             (do ((i +gf-cache-header+ (+ i 1))) ((>= i len))
               (setf (svref new (if front (+ i stride) i)) (svref pic i)))
             (dolist (k keys) (setf (svref new j) k) (setq j (+ j 1)))
             (setf (svref new j) value (svref new (+ j 1)) methods)
             (%set-gf-slot info +gf-cache+ new)))
          (t
           (let ((new (vector (svref pic 0) (svref pic 1) (svref pic 2) (svref pic 3) nil)))
             (%set-gf-slot info +gf-cache+ new)
             (do ((b +gf-cache-header+ (+ b stride))) ((>= b len))
               (%gf-table-put info (let ((r nil)) (dotimes (j nreq (nreverse r)) (push (svref pic (+ b j)) r)))
                              nreq (svref pic (+ b nreq)) (svref pic (+ b nreq 1))))
             (%gf-table-put info keys nreq value methods))))))

(defun %gf-has-entry-p (info keys nreq)
  (let* ((pic (%gf-slot info +gf-cache+)) (len (length pic)) (stride (+ nreq 2))
         (table (%gf-slot info +gf-table+)))
    (or (do ((i +gf-cache-header+ (+ i stride))) ((>= i len) nil)
          (when (do ((j i (+ j 1)) (l keys (cdr l))) ((null l) t)
                  (unless (eq (svref pic j) (car l)) (return nil)))
            (return t)))
        (and table (%gf-table-find (cdr table) keys nreq) t))))

;;; The dispatch inlined in the function of a generic function looks for the
;;; classes of the arguments only: an argument that is an EQL specializer
;;; object would always miss it (its entry is keyed by its marker) until an
;;; argument of the same class that is not such an object made the guarded
;;; entry of the class.  So the entry of a marker also makes that entry, with
;;; the value for the other objects of the class computed when one first comes
;;; (and the applicable methods, which only CALL-NEXT-METHOD's check of its
;;; arguments asks for, computed each time: +GF-METHODS-UNKNOWN+).
(defconstant +gf-methods-unknown+ '%methods-unknown)

(defun %gf-add-eql-class-entry (info keys args nreq)
  (when (and (<= 1 nreq 8) (some #'consp keys))
    (let ((class-keys (mapcar (lambda (k a) (if (consp k) (%fast-class-of a) k)) keys args))
          (n (%gf-slot info +gf-fast+)))
      (unless (%gf-has-entry-p info class-keys nreq)
        (let* ((memo nil)
               (pic (%gf-slot info +gf-cache+))
               (other (%arity-case n
                        (lambda (%args)
                          (funcall (the function
                                        (or memo
                                            (setq memo (values (%gf-compute-value info (list %args))))))
                                   %args)))))
          (when (eq pic (%gf-slot info +gf-cache+))
            (%gf-store info class-keys nreq (%eql-guarded-value info class-keys other n)
                       +gf-methods-unknown+)))))))

(defun %gf-lookup-entry (info args nreq which)
  "The cache entry for ARGS: its value (WHICH = 0) or the applicable methods (1)."
  (declare (fixnum nreq which))
  (let* ((pic (%gf-slot info +gf-cache+)) (len (%svlength pic)) (stride (%fx+ nreq 2)))
    (declare (fixnum len stride))
    (when (%fx> len +gf-cache-header+)
      (let ((eqls (%svref pic 2)) (unspec (the fixnum (%svref pic 3))))
        (declare (fixnum unspec))
        (if (and (null eqls) (%fx<= nreq 4))
            ;; the keys are classes (or 0)
            (do ((i +gf-cache-header+ (%fx+ i stride))) ((%fx>= i len))
              (declare (fixnum i))
              (when (do ((j i (%fx+ j 1)) (p 0 (%fx+ p 1)) (bit 1 (%fx* bit 2)) (a args (cdr a)))
                        ((%fx>= p nreq) t)
                      (declare (fixnum j p bit))
                      (unless (eq (%svref pic j)
                                  (if (eq (%fxlogand unspec bit) 0) (%fast-class-of (car a)) 0))
                        (return nil)))
                (return-from %gf-lookup-entry (%svref pic (%fx+ i (%fx+ nreq which))))))
            (let ((keys (%gf-keys info args nreq)))
              (do ((i +gf-cache-header+ (%fx+ i stride))) ((%fx>= i len))
                (declare (fixnum i))
                (when (do ((j i (%fx+ j 1)) (l keys (cdr l))) ((null l) t)
                        (declare (fixnum j))
                        (unless (eq (%svref pic j) (car l)) (return nil)))
                  (return-from %gf-lookup-entry (%svref pic (%fx+ i (%fx+ nreq which))))))))))
    (let* ((keys (%gf-keys info args nreq))
           (table (%gf-slot info +gf-table+))
           (pos (and table (%gf-table-find (cdr table) keys nreq))))
      (if pos
          (svref (cdr table) (+ pos which))
          (%gf-fill info keys args nreq which)))))

(defun %gf-lookup (info args nreq which)
  "The cache entry for ARGS: its value (WHICH = 0) or the applicable methods (1)."
  (declare (fixnum nreq which))
  (let ((v (%gf-lookup-entry info args nreq which)))
    (if (and (eq which 1) (eq v +gf-methods-unknown+))
        (%gf-applicable-methods (%gf-slot info +gf-function+) info args)
        v)))

(defun %gf-compute-value (info args)
  "Returns the value of the cache entry for ARGS and the applicable methods."
  (let* ((gf (%gf-slot info +gf-function+))
         (methods (%gf-applicable-methods gf info args))
         (n (%gf-slot info +gf-fast+)))
    (values (if (eql n 0)
                (%compute-emf gf info methods args)
                (%compute-fast-emf gf info methods args n))
            methods)))

(defun %gf-invoke (info args)
  (let* ((nreq (the fixnum (%gf-slot info +gf-nreq+))) (nopt (the fixnum (%gf-slot info +gf-nopt+)))
         (rest args))
    (declare (fixnum nreq nopt))
    ;; REST: the arguments after the required and optional ones
    (do ((i 0 (%fx+ i 1))) ((%fx>= i nreq))
      (declare (fixnum i))
      (when (atom rest)
        (%gf-arg-error info "too few arguments (~S given, ~S required)" (length args) nreq))
      (setq rest (cdr rest)))
    (unless (eq nopt 0)
      (do ((i 0 (%fx+ i 1))) ((or (%fx>= i nopt) (atom rest)))
        (declare (fixnum i))
        (setq rest (cdr rest))))
    (when rest
      (if (%gf-slot info +gf-restp+)
          (when (%gf-slot info +gf-keyp+)
            (do ((l rest (cddr l))) ((null l))
              (when (null (cdr l)) (%gf-arg-error info "odd number of keyword arguments ~S" rest))
              (unless (symbolp (car l)) (%gf-arg-error info "~S is not a keyword" (car l)))))
          (%gf-arg-error info "too many arguments (~S given, at most ~S allowed)" (length args) (+ nreq nopt))))
    (let ((entry (%gf-lookup info args nreq 0)))
      (if (consp entry)
          (progn
            (when (and rest (not (eq (cdr entry) :any)) (%gf-slot info +gf-keyp+))
              (%check-call-keys info (cdr entry) rest))
            (funcall (the function (car entry)) args))
          (if (and (eq nopt 0) (not (%gf-slot info +gf-restp+)))
              (apply (the function entry) args)
              (apply (the function entry)
                     (%spread-list nreq nopt (if (%gf-slot info +gf-restp+) 1 0) args)))))))

;;; The dispatch on one to four required arguments.  %GF-CALLn is inlined in
;;; the function of a generic function: it searches the first cache entries
;;; for the classes of the arguments.  %GF-FINDn does the rest (the keys with
;;; EQL specializers, the table, the computation of the effective method) and
;;; returns the function to call, or NIL when the generic function is not
;;; dispatched on n arguments.  %GF-MISSn is the whole dispatch out of line.
(defmacro %define-gf-dispatch (n &optional (nkeys n))
  ;; N: the number of arguments of the functions; NKEYS: the number of them
  ;; that the dispatch looks at (the required ones).  With fewer keys than
  ;; arguments only %GF-FIND-n-k is defined (%GF-SCALL calls it).
  (let* ((args (subseq '(a b c d) 0 n))
         (kargs (subseq args 0 nkeys))
         (keys (subseq '(k0 k1 k2 k3) 0 nkeys))
         (call (nth (- n 1) '(%gf-call1 %gf-call2 %gf-call3 %gf-call4)))
         (miss (nth (- n 1) '(%gf-miss1 %gf-miss2 %gf-miss3 %gf-miss4)))
         (find (if (eql n nkeys)
                   (nth (- n 1) '(%gf-find1 %gf-find2 %gf-find3 %gf-find4))
                   (cdr (assoc (+ (* 10 n) nkeys) '((21 . %gf-find-2-1) (31 . %gf-find-3-1) (32 . %gf-find-3-2)
                                                     (41 . %gf-find-4-1) (42 . %gf-find-4-2) (43 . %gf-find-4-3))))))
         (stride (+ nkeys 2)))
    (flet ((match (vec base)
             (let ((i -1))
               `(and ,@(mapcar (lambda (k)
                                 (setq i (+ i 1))
                                 `(eq (%svref ,vec ,(if (eql i 0) base `(%fx+ ,base ,i))) ,k))
                               keys)))))
      `(progn
         (defun ,find (info ,@args)
           (if (and (eq (%svref (%gf-slot info +gf-cache+) 1) ,n) (eql (%gf-slot info +gf-nreq+) ,nkeys))
               (let* ((pic (%gf-slot info +gf-cache+))
                      (eqls (%svref pic 2)) (unspec (%svref pic 3))
                      ,@(let ((i -1))
                          (mapcar (lambda (k a) (setq i (+ i 1)) `(,k (%arg-key unspec eqls ,i ,a)))
                                  keys kargs))
                      (len (%svlength pic)))
                 (do ((i +gf-cache-header+ (%fx+ i ,stride))) ((%fx>= i len))
                   (when ,(match 'pic 'i)
                     (return-from ,find (%svref pic (%fx+ i ,nkeys)))))
                 (let ((vec (%svref pic 4)))
                   (when vec
                     (let* ((mask (%svref vec 0))
                            (i (%fxlogand ,(let ((h nil))
                                             (dolist (k keys h)
                                               (setq h (if h
                                                           `(%mix-hash ,h (%key-hash ,k))
                                                           `(%key-hash ,k)))))
                                          mask)))
                       (loop
                         (let ((base (%fx+ 1 (%fx* i ,stride))))
                           (cond ((null (%svref vec base)) (return))
                                 (,(match 'vec 'base)
                                  (return-from ,find
                                    (%gf-class-note info pic (%single-key ,@keys) (%svref vec (%fx+ base ,nkeys))))))
                           (setq i (%fxlogand (%fx+ i 1) mask)))))))
                 (let ((v (%gf-fill info (list ,@keys) (list ,@args) ,nkeys 0))
                       (pic (%gf-slot info +gf-cache+)))
                   (if (%svref pic 4) (%gf-class-note info pic (%single-key ,@keys) v) v)))
               nil))
         ,@(when (eql n nkeys)
             `((defun ,miss (info ,@args)
                 (let ((f (,find info ,@args)))
                   (if f
                       (funcall (the function f) ,@args)
                       (%gf-invoke info (list ,@args)))))
               ;; (a macro, not an inline function: the dispatch is copied into
               ;; each generic function's function also where that is compiled
               ;; in the image, whose compiler does not copy the runtime's functions)
               (defmacro ,call (info ,@args)
                 ;; The methods are called here, in the function of each generic
                 ;; function, and not in a function shared by all of them: the
                 ;; engine specializes the call per generic function (a method
                 ;; found among the first entries is called inside the loop, not in
                 ;; tail position: it is then inlined into the dispatch).
                 (list 'let* (list (list 'info info) ,@(mapcar (lambda (a) `(list ',a ,a)) args))
                       (%gf-dispatch-code ,n ,nkeys ',args '(list ,@args) ,(%shape-code n 0 0))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %gf-dispatch-code (n nkeys spread actual code)
    "The dispatch inlined in the function of a generic function: SPREAD are the
N variables holding the spread arguments, the first NKEYS of them the required
ones; ACTUAL is the form of the list of the arguments, for the general call.
INFO is the variable of the generic function metaobject, and the tag of the
cache has the shape bits CODE."
    (let* ((keys (subseq '(k0 k1 k2 k3 k4 k5 k6 k7) 0 nkeys))
           (kargs (subseq spread 0 nkeys))
           (stride (+ nkeys 2))
           (find (if (> n 4)
                     nil
                     (if (eql n nkeys)
                     (nth (- n 1) '(%gf-find1 %gf-find2 %gf-find3 %gf-find4))
                     (cdr (assoc (+ (* 10 n) nkeys) '((21 . %gf-find-2-1) (31 . %gf-find-3-1) (32 . %gf-find-3-2)
                                                     (41 . %gf-find-4-1) (42 . %gf-find-4-2) (43 . %gf-find-4-3))))))))
      `(block dispatch
         (let* ((pic (%instance-ref info +gf-cache+))
                (tag (%svref pic 0)))
           (when (eq (%fxlogand tag #x70f) ,code)
             (let (,@(let ((i -1))
                       (mapcar (lambda (k a)
                                 (setq i (+ i 1))
                                 `(,k (if (eq ,(if (< i 4)
                                                       `(%fxlogand tag ,(ash 16 i))
                                                       ;; (the tag has the bits of the first four)
                                                       `(%fxlogand (%svref pic 3) ,(ash 1 i)))
                                                  0)
                                              (%fast-class-of ,a)
                                              0)))
                               keys kargs))
                   (len (%svlength pic)))
               (do ((i +gf-cache-header+ (%fx+ i ,stride))) ((%fx>= i len))
                 (when (and ,@(let ((i -1))
                                (mapcar (lambda (k)
                                          (setq i (+ i 1))
                                          `(eq (%svref pic ,(if (eql i 0) 'i `(%fx+ i ,i))) ,k))
                                        keys)))
                   (return-from dispatch
                     ,(if (eql n nkeys)
                          `(funcall (the function (%svref pic (%fx+ i ,nkeys))) ,@spread)
                          ;; a generic function with &KEY (tag bit 4096): the
                          ;; check of the keywords is called from here, and
                          ;; only for a call with some (%KEY-CHECKED-FAST-EMF)
                          `(let ((f (%svref pic (%fx+ i ,nkeys))))
                             (if (or (eq (%fxlogand tag 4096) 0) (null ,(car (last spread))))
                                 (funcall (the function f) ,@spread)
                                 (let ((kn (%closure-name f)))
                                   (if (simple-vector-p kn)
                                       (progn
                                         (%check-more-keys info (%svref kn 1) ,(car (last spread)))
                                         (funcall (the function (%svref kn 0)) ,@spread))
                                       (funcall (the function f) ,@spread)))))))))
               ;; many classes: the class of the one specialized argument
               ;; may know the function (%GF-CLASS-NOTE)
               (when (%svref pic 4)
                 (let ((f (%gf-class-lookup pic (%single-key ,@keys))))
                   (when f (return-from dispatch (funcall (the function f) ,@spread)))))
               (let ((f ,(if find
                             `(,find info ,@spread)
                             `(%gf-find-big info (list ,@spread) ,nkeys ,n))))
                 (when f (return-from dispatch (funcall (the function f) ,@spread))))))
           (%gf-invoke info ,actual))))))

;;; (%GF-SCALL info (nreq nopt more) (arg...)): the dispatch inlined in the
;;; function of a generic function with optional or rest/keyword parameters,
;;; for a call with the arguments ARG...: they are spread (the unbound marker
;;; for the optional ones not given, the list of the rest) and the cache is
;;; searched by the required ones; its values are functions of the spread
;;; arguments.
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %gf-spread-entry-n (shape &optional inline)
    "The function of a generic function of the shape SHAPE called with more
than four arguments (or applied): its lambda list spreads them.  It calls the
core (+GF-CORE+), or, with INLINE, has the dispatch itself."
    (destructuring-bind (nreq nopt more) shape
      (let* ((vars '(s0 s1 s2 s3 s4 s5 s6 s7))
             (req (subseq vars 0 nreq))
             (opt (subseq vars nreq (+ nreq nopt)))
             (rest (if (eql more 1) (list (nth (+ nreq nopt) vars)) nil))
             (spread (append req opt rest)))
        `(lambda (,@req ,@(when opt `(&optional ,@(mapcar (lambda (o) `(,o (%unbound-marker))) opt)))
                  ,@(when rest `(&rest ,@rest)))
           ,(if inline
                (%gf-dispatch-code (+ nreq nopt more) nreq spread `(%unspread ,nreq ,nopt ,more (list ,@spread))
                                   (%shape-code nreq nopt more))
                `(funcall (the function (%instance-ref info +gf-core+)) ,@spread))))))

  (defun %gf-spread-entries (shape &optional rest-inline)
    "The entries of one to four arguments of a generic function of the shape
SHAPE: the call with the required arguments alone has the dispatch inline
(%GF-SCALL), and, with REST-INLINE (no keyword parameters), those with the
optional ones and up to two rest arguments; the others call the core
(%GF-SCALL-CORE)."
    (loop for args in '((a) (a b) (a b c) (a b c d))
          collect `(lambda ,args
                     ,(if (or (eql (length args) (first shape))
                              ;; without keywords: the calls with the optional
                              ;; arguments and up to two rest arguments too
                              (and rest-inline
                                   (<= (first shape) (length args)
                                       (+ (first shape) (second shape) (* 2 (third shape))))))
                          `(%gf-scall info ,shape ,args)
                          `(%gf-scall-core info ,shape ,args)))))

  (defun %gf-spread-core (shape)
    "The function of the spread arguments of a generic function of the shape
SHAPE holding its dispatch (+GF-CORE+)."
    (destructuring-bind (nreq nopt more) shape
      (let ((spread (subseq '(s0 s1 s2 s3) 0 (+ nreq nopt more))))
        `(lambda ,spread
           ,(%gf-dispatch-code (+ nreq nopt more) nreq spread `(%unspread ,nreq ,nopt ,more (list ,@spread))
                               (%shape-code nreq nopt more)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %gf-keyed-entry (shape keys)
    "The lambda whose keyed main function becomes that of a generic function of
the shape SHAPE (with &KEY) made by %GF-FUNCTION-MAKER: a call compiled in the
image with constant keywords among KEYS passes the arguments in its slots,
the keyword ones in the order of KEYS (the unbound marker, null, for those not
given).  It calls the keyed effective method of the cache entry (functions of
four arguments: the required, optional and keyword ones, padded with NIL;
past four in all, the fourth is a simple vector of the fourth and later ones,
*wide*; %GF-KEYED-EMF), or else the effective method of the spread arguments with the
list of the keyword arguments (which are valid: they are keywords of the
generic function)."
    (destructuring-bind (nreq nopt more) shape
      (declare (ignore more))
      (let* ((vars '(s0 s1 s2 s3 s4 s5 s6 s7))
             (npos (+ nreq nopt))
             (req (subseq vars 0 nreq))
             (opt (subseq vars nreq npos))
             (kvars (subseq vars npos (+ npos (length keys))))
             ;; (more than four in all: the fourth argument of the keyed
             ;; effective method is a vector of the fourth and later ones)
             (keyed-emf-p (<= npos 3))
             (wide (> (+ npos (length keys)) 4))
             (all (append req opt kvars))
             (pad (make-list (max 0 (- 4 (+ npos (length keys))))))
             (ks (subseq '(k0 k1 k2 k3) 0 nreq))
             (stride (+ nreq 2))
             (code (+ (%shape-code nreq nopt 1) 8192)))
        `(lambda (,@req ,@(when opt `(&optional ,@(mapcar (lambda (o) `(,o (%unbound-marker))) opt)))
                  &key ,@(mapcar (lambda (k v) `((,k ,v) (%unbound-marker))) keys kvars))
           (block kd
             (let* ((pic (%instance-ref info +gf-cache+)) (tag (%svref pic 0)))
               (when (eq (%fxlogand tag ,(logior #x70f 8192)) ,code)
                 (let (,@(let ((i -1))
                           (mapcar (lambda (k a)
                                     (setq i (+ i 1))
                                     `(,k (if (eq ,(if (< i 4)
                                                       `(%fxlogand tag ,(ash 16 i))
                                                       ;; (the tag has the bits of the first four)
                                                       `(%fxlogand (%svref pic 3) ,(ash 1 i)))
                                                  0)
                                              (%fast-class-of ,a)
                                              0)))
                                   ks req))
                       (len (%svlength pic)))
                   (do ((i +gf-cache-header+ (%fx+ i ,stride))) ((%fx>= i len))
                     (when (and ,@(let ((i -1))
                                    (mapcar (lambda (k)
                                              (setq i (+ i 1))
                                              `(eq (%svref pic ,(if (eql i 0) 'i `(%fx+ i ,i))) ,k))
                                            ks)))
                       (let* ((f (%svref pic (%fx+ i ,nreq))) (kn (%closure-name f)))
                         (when (simple-vector-p kn)
                           (let ((kemf ,(if keyed-emf-p
                                            `(or (%svref kn 2)
                                                 (%gf-make-keyed-emf info kn (%svref pic (%fx+ i ,(+ nreq 1)))))
                                            :none)))
                             (return-from kd
                               (if (functionp kemf)
                                   ,(cond ((not keyed-emf-p) nil)
                                          (wide `(funcall (the function kemf) ,@(subseq all 0 3)
                                                          (vector ,@(subseq all 3))))
                                          (t `(funcall (the function kemf) ,@all ,@pad)))
                                   (funcall (the function (%svref kn 0)) ,@req ,@opt
                                            (let ((r nil))
                                              ,@(reverse
                                                 (mapcar (lambda (k v) `(unless (%unboundp ,v) (setq r (list* ',k ,v r))))
                                                         keys kvars))
                                              r))))))
                         (return))))))
               (%gf-invoke info (%keyed-args ,nreq ,nopt ',keys (list ,@req ,@opt ,@kvars))))))))))

(defun %unwide-keyed (spread)
  "The keyed spread arguments of the four arguments SPREAD of a wide keyed
effective method (%GF-KEYED-ENTRY)."
  (append (subseq spread 0 3) (coerce (fourth spread) 'list)))

(defun %keyed-args (nreq nopt keys spread)
  "The arguments whose keyed spread arguments are SPREAD (the required, the
optional and the keyword ones in the order of KEYS)."
  (let ((r nil) (a spread))
    (dotimes (i nreq) (push (pop a) r))
    (dotimes (i nopt) (let ((x (pop a))) (unless (%unboundp x) (push x r))))
    (dolist (k keys) (let ((x (pop a))) (unless (%unboundp x) (push k r) (push x r))))
    (nreverse r)))

(defun %key-slot (keys key npos)
  "The slot of the keyword KEY among the four keyed spread arguments, or NIL."
  (let ((p (position key keys))) (and p (+ npos p))))

;;; methods known as functions of the keyed spread arguments: method -> maker
(defvar *method-keyed-makers* (make-hash-table :test 'eq))
(defun %note-method-keyed-maker (m maker)
  (setf (gethash m *method-keyed-makers*) maker)
  m)

(defun %gf-make-keyed-emf (info kn methods)
  "The keyed effective method of a cache entry whose effective method of the
spread arguments has the name KN, or :NONE; remembered in KN."
  (setf (svref kn 2) (or (%gf-keyed-emf info methods) :none)))

(defun %gf-keyed-emf (info methods)
  (let ((gfkeys (%gf-slot info +gf-kmaker+)))
    (when (and (consp methods)
               (eq (%gf-slot info +gf-combination+) *standard-method-combination*)
               (every (lambda (m) (gethash m *method-keyed-makers*)) methods))
      (let ((around nil) (before nil) (primary nil) (after nil))
        (dolist (m methods)
          (let ((q (%method-qualifiers m)))
            (cond ((null q) (push m primary))
                  ((equal q '(:around)) (push m around))
                  ((equal q '(:before)) (push m before))
                  ((equal q '(:after)) (push m after))
                  (t (return-from %gf-keyed-emf nil)))))
        (when primary
          (flet ((fn (m next) (funcall (the function (gethash m *method-keyed-makers*)) next gfkeys)))
            (let ((f nil))
              (dolist (m primary) (setq f (fn m f)))
              (when (or before after)
                (let ((bfns (mapcar (lambda (m) (fn m nil)) (nreverse before)))
                      (afns (mapcar (lambda (m) (fn m nil)) after))
                      (p f))
                  (declare (function p))
                  (setq f (if afns
                              (lambda (a b c d)
                                (dolist (x bfns) (funcall (the function x) a b c d))
                                (multiple-value-prog1 (funcall p a b c d)
                                  (dolist (x afns) (funcall (the function x) a b c d))))
                              (lambda (a b c d)
                                (dolist (x bfns) (funcall (the function x) a b c d))
                                (funcall p a b c d))))))
              (dolist (m around) (setq f (fn m f)))
              f)))))))

(defmacro %gf-scall-core (info shape actual)
  "A call of the arguments ACTUAL: the core function (+GF-CORE+) of their
spread form, or the general call when their number does not fit SHAPE."
  (destructuring-bind (nreq nopt more) shape
    (let ((k (length actual)))
      (if (or (< k nreq) (and (> k (+ nreq nopt)) (eql more 0)))
          `(%gf-invoke ,info (list ,@actual))
          `(funcall (the function (%instance-ref ,info +gf-core+))
                    ,@(subseq actual 0 (min k (+ nreq nopt)))
                    ,@(make-list (max 0 (- (+ nreq nopt) k)) :initial-element '(%unbound-marker))
                    ,@(if (eql more 1)
                          (list (if (> k (+ nreq nopt)) `(list ,@(nthcdr (+ nreq nopt) actual)) nil))
                          nil))))))

(defmacro %gf-scall (info shape actual)
  (destructuring-bind (nreq nopt more) shape
    (let ((k (length actual)) (m (+ nreq nopt more)))
      (if (or (< k nreq) (and (> k (+ nreq nopt)) (eql more 0)))
          `(%gf-invoke ,info (list ,@actual))
          (let* ((spread (subseq '(s0 s1 s2 s3) 0 m))
                 (values (append (subseq actual 0 (min k (+ nreq nopt)))
                                 (make-list (max 0 (- (+ nreq nopt) k)) :initial-element '(%unbound-marker))
                                 (if (eql more 1)
                                     (list (if (> k (+ nreq nopt)) `(list ,@(nthcdr (+ nreq nopt) actual)) nil))
                                     nil)))
                 (code (%gf-dispatch-code m nreq spread `(list ,@actual) (%shape-code nreq nopt more))))
            `(let* ((info ,info) ,@(mapcar #'list spread values))
               ,code))))))

(%define-gf-dispatch 1)
(%define-gf-dispatch 2)
(%define-gf-dispatch 3)
(%define-gf-dispatch 4)
(%define-gf-dispatch 2 1)
(%define-gf-dispatch 3 1)
(%define-gf-dispatch 3 2)
(%define-gf-dispatch 4 1)
(%define-gf-dispatch 4 2)
(%define-gf-dispatch 4 3)

;;; Methods of only required parameters (at most four) are also known as
;;; functions of the arguments: either such a function (a method that does not
;;; use CALL-NEXT-METHOD or NEXT-METHOD-P), or a function making one from the
;;; next method (a function of the arguments, or NIL) and the bits of the
;;; parameters that CALL-NEXT-METHOD may be given any argument for.
(defvar *method-fast-functions* (make-hash-table :test 'eq))
(defvar *method-fast-makers* (make-hash-table :test 'eq))
(defun %note-method-fast-function (m f)
  (setf (gethash m *method-fast-functions*) f)
  m)
(defun %note-method-fast-maker (m f)
  (setf (gethash m *method-fast-makers*) f)
  m)

(defun %spread-maker-method-function (maker nreq nopt more)
  "The method function (of the argument list and the next method functions) of
a method of optional or rest/keyword parameters known by MAKER."
  (lambda (args next)
    (apply (the function
                (funcall maker
                         (if next
                             (lambda (&rest spread)
                               (funcall (car next) (%unspread nreq nopt more spread) (cdr next)))
                             nil)
                         0))
           (%spread-list nreq nopt more args))))

(defun %call-next-method-spread (spread next cnm-args cell nreq nopt more check)
  "CALL-NEXT-METHOD with the arguments CNM-ARGS (or none) in a method of the
spread arguments SPREAD whose next method NEXT is a function of such."
  (when (and check cnm-args) (%check-cnm-args cell (%unspread nreq nopt more spread) cnm-args))
  (cond ((null next) (%no-next-method cell (or cnm-args (%unspread nreq nopt more spread))))
        (cnm-args (apply (the function next) (%spread-list nreq nopt more cnm-args)))
        (t (apply (the function next) spread))))

(defun %maker-method-function (maker)
  "The method function (of the argument list and the next method functions) of
a method known by MAKER."
  (lambda (args next)
    (apply (the function (funcall maker (if next (lambda (&rest a) (funcall (car next) a (cdr next))) nil) 0))
           args)))

(defun %method-fast-fn (m next n unspec &optional shape)
  "A function of the N (spread) arguments calling the method M, NEXT (a
function of the arguments, or NIL) being its next method; UNSPEC is
+GF-UNSPECIALIZED+ of the generic function, SHAPE its %GF-SHAPE."
  (let ((maker (gethash m *method-fast-makers*)))
    (if maker
        (funcall maker next unspec)
        (or (gethash m *method-fast-functions*)
            (let ((mf (%method-function m)))
              (if shape
                  (destructuring-bind (nreq nopt more) shape
                    (let ((nexts (if next
                                     (list (lambda (args n)
                                             (declare (ignore n))
                                             (apply next (%spread-list nreq nopt more args))))
                                     nil)))
                      (%arity-case n (lambda (%args)
                                       (funcall mf (%unspread nreq nopt more (list %args)) nexts)))))
                  (let ((nexts (if next (list (lambda (args n) (declare (ignore n)) (apply next args))) nil)))
                    (%arity-case n (lambda (%args) (funcall mf (list %args) nexts))))))))))

(defun %no-applicable-fast (gf n shape)
  (if shape
      (destructuring-bind (nreq nopt more) shape
        (%arity-case n (lambda (%args) (apply #'no-applicable-method gf (%unspread nreq nopt more (list %args))))))
      (%arity-case n (lambda (%args) (no-applicable-method gf %args)))))

(defun %check-more-keys (info valid more)
  "Check the keyword arguments MORE of a call of the generic function INFO
whose applicable methods accept the keywords VALID (or :ANY)."
  ;; one pass with EQ (this runs at every call with keyword arguments); the
  ;; errors and :ALLOW-OTHER-KEYS are looked at only when a key is not valid
  (let ((unknown nil) (l more))
    (loop
      (when (null l) (return))
      (when (or (atom l) (atom (cdr l))) (%gf-arg-error info "odd number of keyword arguments ~S" more))
      (let ((k (%car l)))
        (unless (symbolp k) (%gf-arg-error info "~S is not a keyword" k))
        (unless (or (eq valid :any)
                    (do ((v valid (%cdr v))) ((atom v) nil) (when (eq (%car v) k) (return t))))
          (unless (or unknown (eq k :allow-other-keys)) (setq unknown k))))
      (setq l (%cdr (%cdr l))))
    (when unknown
      (unless (let ((aok (%plist-find more :allow-other-keys))) (and aok (cadr aok)))
        (%gf-arg-error info "unknown keyword argument ~S (allowed: ~S)" unknown valid)))))

(defun %key-checked-fast-emf (info methods f n)
  "F, a function of the N spread arguments of a generic function with &KEY,
the last of them the list of the arguments after the optional ones, preceded
by the check of those keyword arguments."
  (let* ((gfll (%gf-slot info +gf-ll+))
         (valid (if (and (consp gfll) (member '&allow-other-keys gfll)) :any (%valid-call-keys info methods))))
    (declare (function f))
    (let ((g (case n
               (2 (lambda (a more) (when more (%check-more-keys info valid more)) (funcall f a more)))
               (3 (lambda (a b more) (when more (%check-more-keys info valid more)) (funcall f a b more)))
               (4 (lambda (a b c more) (when more (%check-more-keys info valid more)) (funcall f a b c more)))
               (t (lambda (&rest args)
                    (let ((more (car (last args))))
                      (when more (%check-more-keys info valid more))
                      (apply f args)))))))
      ;; (the dispatch inlined in the function of the generic function
      ;; finds F and VALID here and checks the keywords itself)
      ;; (and the keyed effective method, made when first called: %GF-KEYED-ENTRY)
      (%set-closure-name g (vector f valid nil))
      g)))

(defun %compute-fast-emf (gf info methods args n)
  (let ((shape (%gf-shape info)))
    (if (and shape methods (%gf-slot info +gf-keyp+))
        (%key-checked-fast-emf info methods (%compute-fast-emf-1 gf info methods args n shape) n)
        (%compute-fast-emf-1 gf info methods args n shape))))

(defun %compute-fast-emf-1 (gf info methods args n shape)
  (let* ((mc (%gf-slot info +gf-combination+))
         (def (if (eq mc *standard-method-combination*) nil (get (%instance-ref mc 0) '%method-combination))))
    (cond ((null methods) (%no-applicable-fast gf n shape))
          ((and (null shape) (%fast-accessor-fn info methods args)))
          ((eq mc *standard-method-combination*)
           (%standard-fast-emf gf methods n (%gf-slot info +gf-unspecialized+) shape))
          ((and (eq (car def) :short)
                (%short-fast-emf gf methods (%instance-ref mc 0) def (%instance-ref mc 1) n
                                 (%gf-slot info +gf-unspecialized+) shape)))
          (t (let ((emf (%effective-method-function gf info methods)))
               (if shape
                   (destructuring-bind (nreq nopt more) shape
                     (%arity-case n (lambda (%args) (funcall emf (%unspread nreq nopt more (list %args))))))
                   (%arity-case n (lambda (%args) (funcall emf (list %args))))))))))

(defun %short-fast-emf (gf methods name def options n unspec &optional shape)
  "The effective method of a short-form method combination as a function of
the N arguments, or NIL when its operator is not a function."
  (let ((op (cadr def)) (identity (caddr def)))
    (when (or (member op '(progn and or))
              (and (fboundp op) (not (macro-function op)) (not (special-operator-p op))))
      (multiple-value-bind (around primary) (%short-methods gf methods name options)
        (if (null primary)
            (%no-applicable-fast gf n shape)
            (let* ((pfns (mapcar (lambda (m) (%method-fast-fn m nil n unspec shape)) primary))
                   (f (cond ((and identity (null (cdr pfns))) (car pfns))
                            ((eq op 'progn)
                             (%arity-case n (lambda (%args)
                                              (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (the function (car l)) %args))
                                                (funcall (the function (car l)) %args)))))
                            ((eq op 'and)
                             (%arity-case n (lambda (%args)
                                              (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (the function (car l)) %args))
                                                (unless (funcall (the function (car l)) %args) (return nil))))))
                            ((eq op 'or)
                             (%arity-case n (lambda (%args)
                                              (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (the function (car l)) %args))
                                                (let ((v (funcall (the function (car l)) %args))) (when v (return v)))))))
                            ((eq op '+)
                             (%arity-case n (lambda (%args)
                                              (let ((acc (+ (funcall (the function (car pfns)) %args))))
                                                (dolist (p (cdr pfns) acc)
                                                  (setq acc (+ acc (funcall (the function p) %args))))))))
                            ((eq op 'list)
                             (%arity-case n (lambda (%args)
                                              (let ((r nil))
                                                (dolist (p pfns (nreverse r)) (push (funcall (the function p) %args) r))))))
                            ;; two or three methods of the other standard
                            ;; operators: no list of their values
                            ((and (member op '(append nconc max min)) (cdr pfns) (null (cdddr pfns)))
                             (let ((p1 (first pfns)) (p2 (second pfns)) (p3 (third pfns)))
                               (declare (function p1 p2))
                               (macrolet ((combine (operator)
                                            `(if p3
                                                 (%arity-case n (lambda (%args)
                                                                  (let* ((a (funcall p1 %args)) (b (funcall p2 %args))
                                                                         (c (funcall (the function p3) %args)))
                                                                    (,operator a b c))))
                                                 (%arity-case n (lambda (%args)
                                                                  (let* ((a (funcall p1 %args)) (b (funcall p2 %args)))
                                                                    (,operator a b)))))))
                                 (case op
                                   (append (combine append))
                                   (nconc (combine nconc))
                                   (max (combine max))
                                   (t (combine min))))))
                            (t
                             (%arity-case n (lambda (%args)
                                              (let ((r nil))
                                                (dolist (p pfns) (push (funcall (the function p) %args) r))
                                                (apply op (nreverse r)))))))))
              (dolist (m (reverse around)) (setq f (%method-fast-fn m f n unspec shape)))
              f))))))

(defun %standard-fast-emf (gf methods n unspec &optional shape)
  (let ((around nil) (before nil) (primary nil) (after nil))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((null q) (push m primary))
              ((equal q '(:around)) (push m around))
              ((equal q '(:before)) (push m before))
              ((equal q '(:after)) (push m after))
              (t (%invalid-qualifiers gf m 'standard)))))
    ;; AROUND, PRIMARY and AFTER are now least specific first
    (if (null primary)
        (%no-applicable-fast gf n shape)
        (let ((f nil))
          (dolist (m primary) (setq f (%method-fast-fn m f n unspec shape)))
          (when (or before after)
            (let ((bfns (mapcar (lambda (m) (%method-fast-fn m nil n unspec shape)) (nreverse before)))
                  (afns (mapcar (lambda (m) (%method-fast-fn m nil n unspec shape)) after))
                  (p f))
              (setq f (if afns
                          (%arity-case n (lambda (%args)
                                           (dolist (b bfns) (funcall (the function b) %args))
                                           (multiple-value-prog1 (funcall (the function p) %args)
                                             (dolist (a afns) (funcall (the function a) %args)))))
                          (%arity-case n (lambda (%args)
                                           (dolist (b bfns) (funcall (the function b) %args))
                                           (funcall (the function p) %args)))))))
          (dolist (m around) (setq f (%method-fast-fn m f n unspec shape)))
          f))))

(defun %check-call-keys (info valid keys)
  (unless (let ((aok (%plist-find keys :allow-other-keys))) (and aok (cadr aok)))
    (do ((l keys (cddr l))) ((null l))
      (unless (or (eq (car l) :allow-other-keys) (member (car l) valid))
        (%gf-arg-error info "unknown keyword argument ~S (allowed: ~S)" (car l) valid)))))

(defun %valid-call-keys (info methods)
  (let ((r (copy-list (%gf-slot info +gf-keys+))))
    (dolist (m methods r)
      (let ((k (%instance-ref m 6)))
        (when (eq k :any) (return :any))
        (dolist (x k) (pushnew x r))))))

(defun %gf-applicable-methods (gf info args)
  (if (or (eq (%instance-layout info) *the-standard-generic-function-class*)
          (null *the-standard-generic-function-class*))
      (%compute-applicable-methods gf args)
      ;; a generic function of a user-defined class (AMOP: the
      ;; discriminating function calls COMPUTE-APPLICABLE-METHODS-USING-CLASSES
      ;; first, then COMPUTE-APPLICABLE-METHODS)
      (multiple-value-bind (methods ok)
          (compute-applicable-methods-using-classes
           gf (mapcar #'class-of (subseq args 0 (min (length args) (%gf-slot info +gf-nreq+)))))
        (if ok methods (compute-applicable-methods gf args)))))


;;; Reader and writer methods made by DEFCLASS: method -> (slot-name . writerp).
;;; When such a method is the only applicable one, the effective method
;;; accesses the slot at its location in the instance's class directly.
(defvar *slot-accessor-methods* (make-hash-table :test 'eq))
(defun %note-slot-accessor (method slot-name writerp)
  (setf (gethash method *slot-accessor-methods*) (cons slot-name writerp))
  method)

(defun %fast-accessor-emf (info methods args)
  (let ((acc (and (null (cdr methods)) (null (%method-qualifiers (car methods)))
                  (eq (%gf-slot info +gf-combination+) *standard-method-combination*)
                  (gethash (car methods) *slot-accessor-methods*))))
    (when acc
      (let* ((writerp (cdr acc))
             (object (if writerp (cadr args) (car args)))
             (class (and (%instancep object) (%instance-layout object)))
             (index (and class (not (functionp object)) (%instancep class)
                         (not (%class-slot class +class-protocol+))
                         (%class-slot class +class-slot-index+)))
             (loc (cdr (assoc (car acc) index)))
             (count (and class (%class-redefined-p class) (%class-slot class +class-instance-size+)))
             (mf (%method-function (car methods))))
        (when (fixnump loc)
          (if writerp
              (lambda (args)
                (let ((o (cadr args)))
                  (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index)
                           (or (null count) (%slot-count-is-p o count)))
                      (%set-instance-ref o loc (car args))
                      (funcall mf args nil))))
              (lambda (args)
                (let ((o (car args)))
                  (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index)
                           (or (null count) (%slot-count-is-p o count)))
                      (let ((v (%instance-ref o loc)))
                        (if (%unboundp v) (funcall mf args nil) v))
                      (funcall mf args nil))))))))))

(defvar *reader-locations* (make-hash-table :test 'eq)
  "reader function made by %FAST-ACCESSOR-FN -> the location of the slot it reads")

(defvar *writer-locations* (make-hash-table :test 'eq)
  "The writer functions of %FAST-ACCESSOR-FN -> the location of their slot.")

(defun %fast-accessor-fn (info methods args)
  "Like %FAST-ACCESSOR-EMF for a function of the arguments themselves."
  (let ((acc (and (null (cdr methods)) (null (%method-qualifiers (car methods)))
                  (eq (%gf-slot info +gf-combination+) *standard-method-combination*)
                  (gethash (car methods) *slot-accessor-methods*))))
    (when acc
      (let* ((writerp (cdr acc))
             (object (if writerp (cadr args) (car args)))
             (class (and (%instancep object) (%instance-layout object)))
             (index (and class (not (functionp object)) (%instancep class)
                         (not (%class-slot class +class-protocol+))
                         (%class-slot class +class-slot-index+)))
             (loc (cdr (assoc (car acc) index)))
             (count (and class (%class-redefined-p class) (%class-slot class +class-instance-size+)))
             (mf (%method-function (car methods)))
             (nreq (%gf-slot info +gf-nreq+)))
        ;; the object is a direct instance of CLASS (the key of the cache
        ;; entry), whose slot locations stay while the entry does
        (when (and (fixnump loc) (eql nreq (if writerp 2 1)) (eql (%gf-slot info +gf-fast+) nreq))
          (if writerp
              (let ((writer (lambda (v o)
                              (if (and (%instancep o) (or (null count) (%slot-count-is-p o count)))
                                  (%set-instance-ref o loc v)
                                  (funcall mf (list v o) nil)))))
                (unless count (setf (gethash writer *writer-locations*) loc))
                writer)
              (let ((reader (lambda (o)
                              (if (and (%instancep o) (or (null count) (%slot-count-is-p o count)))
                                  (let ((v (%instance-ref o loc)))
                                    (if (%unboundp v) (funcall mf (list o) nil) v))
                                  (funcall mf (list o) nil)))))
                ;; (a call site caches the location instead of the function)
                (unless count (setf (gethash reader *reader-locations*) loc))
                reader)))))))

(defun %compute-emf (gf info methods args)
  "Returns (emf . valid-keys)."
  (let ((gfll (%gf-slot info +gf-ll+)))
    (cons (if (null methods)
              (lambda (args) (apply #'no-applicable-method gf args))
              (or (%fast-accessor-emf info methods args)
                  (%effective-method-function gf info methods)))
          (if (and (consp gfll) (member '&allow-other-keys gfll))
              :any
              (%valid-call-keys info methods)))))

(defun %effective-method-function (gf info methods)
  (let* ((mc (%gf-slot info +gf-combination+))
         (name (%instance-ref mc 0))
         (def (if (eq mc *standard-method-combination*) nil (get name '%method-combination))))
    (cond ((null def) (%standard-emf gf methods))
          ((eq (car def) :short) (%short-emf gf methods name def (%instance-ref mc 1)))
          ((eq (car def) :long) (%long-emf gf info methods def (%instance-ref mc 1)))
          (t (error "Bad method combination ~S" name)))))

(defun %invalid-qualifiers (gf m combination)
  (invalid-method-error m "The method ~S of ~S has qualifiers ~S, which are invalid for the ~S method combination"
                        m (generic-function-name gf) (%method-qualifiers m) combination))

(defun %no-primary-method (gf)
  (lambda (args) (apply #'no-applicable-method gf args)))

(defun %standard-emf (gf methods)
  (let ((around nil) (before nil) (primary nil) (after nil))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((null q) (push m primary))
              ((equal q '(:around)) (push m around))
              ((equal q '(:before)) (push m before))
              ((equal q '(:after)) (push m after))
              (t (%invalid-qualifiers gf m 'standard)))))
    (setq around (nreverse around) before (nreverse before) primary (nreverse primary))
    ;; AFTER stays reversed (least specific first)
    (if (null primary)
        (%no-primary-method gf)
        (let* ((pfns (mapcar #'%method-function primary))
               (inner (if (or before after)
                          (let ((bfns (mapcar #'%method-function before))
                                (afns (mapcar #'%method-function after)))
                            (lambda (args next)
                              (declare (ignore next))
                              (dolist (b bfns) (funcall b args nil))
                              (multiple-value-prog1 (funcall (car pfns) args (cdr pfns))
                                (dolist (a afns) (funcall a args nil)))))
                          nil)))
          (cond (around
                 (let ((chain (append (mapcar #'%method-function around)
                                      (list (or inner (lambda (args next)
                                                        (declare (ignore next))
                                                        (funcall (car pfns) args (cdr pfns))))))))
                   (lambda (args) (funcall (car chain) args (cdr chain)))))
                (inner (lambda (args) (funcall inner args nil)))
                (t (let ((first (car pfns)) (rest (cdr pfns)))
                     (lambda (args) (funcall first args rest)))))))))

;;; short-form method combinations: (:short operator identity-with-one-argument)
(defun %short-methods (gf methods name options)
  "Returns the :AROUND methods (most specific first) and the primary methods
in the order of the option."
  (let ((around nil) (primary nil)
        (order (if options (car options) :most-specific-first)))
    (unless (and (member order '(:most-specific-first :most-specific-last)) (null (cdr options)))
      (error 'simple-error :format-control "Invalid options ~S for the ~S method combination"
                           :format-arguments (list options name)))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((equal q '(:around)) (push m around))
              ((and (consp q) (eq (car q) name) (null (cdr q))) (push m primary))
              (t (%invalid-qualifiers gf m name)))))
    (values (nreverse around)
            (if (eq order :most-specific-last) primary (nreverse primary)))))

(defun %short-emf (gf methods name def options)
  (let ((op (cadr def)) (identity (caddr def)) (around nil) (primary nil))
    (multiple-value-setq (around primary) (%short-methods gf methods name options))
    (if (null primary)
        (%no-primary-method gf)
        (let* ((pfns (mapcar #'%method-function primary))
               (inner
                 (cond ((and identity (null (cdr pfns)))
                        (let ((f (car pfns))) (lambda (args) (funcall f args nil))))
                       ((eq op 'progn)
                        (lambda (args)
                          (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (car l) args nil))
                            (funcall (car l) args nil))))
                       ((eq op 'and)
                        (lambda (args)
                          (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (car l) args nil))
                            (unless (funcall (car l) args nil) (return nil)))))
                       ((eq op 'or)
                        (lambda (args)
                          (do ((l pfns (cdr l))) ((null (cdr l)) (funcall (car l) args nil))
                            (let ((v (funcall (car l) args nil))) (when v (return v))))))
                       ((and (fboundp op) (not (macro-function op)) (not (special-operator-p op)))
                        (lambda (args)
                          (apply op (mapcar (lambda (f) (funcall f args nil)) pfns))))
                       (t (eval `(lambda (%args)
                                   (,op ,@(mapcar (lambda (f) `(funcall ',f %args nil)) pfns))))))))
          (if around
              (let ((chain (append (mapcar #'%method-function around)
                                   (list (lambda (args next) (declare (ignore next)) (funcall inner args))))))
                (lambda (args) (funcall (car chain) args (cdr chain))))
              inner)))))

;;; long-form method combinations: (:long function arguments-lambda-list)
(defun %long-emf (gf info methods def options)
  (let* ((form (funcall (cadr def) gf methods options))
         (lam `(lambda (%emf-args)
                 (declare (ignorable %emf-args))
                 (let* ,(%arguments-bindings (caddr def) info '%emf-args)
                   (macrolet ((call-method (m &optional nexts) (%expand-call-method m nexts '%emf-args))
                              (make-method (form) (declare (ignore form))
                                (error "MAKE-METHOD is only allowed as an argument of CALL-METHOD")))
                     ,form)))))
    (eval lam)))

(defun %expand-call-method (m nexts argsvar)
  (flet ((next-form (x)
           (if (and (consp x) (eq (car x) 'make-method))
               `(lambda (%a %n) (declare (ignore %a %n)) ,(cadr x))
               `',(%method-function x))))
    (if (and (consp m) (eq (car m) 'make-method))
        (cadr m)
        `(funcall ',(%method-function m) ,argsvar (list ,@(mapcar #'next-form nexts))))))

(defun %arguments-bindings (args-ll info argsvar)
  "Bindings for the :ARGUMENTS lambda list of a long-form method combination."
  (if (null args-ll)
      nil
      (let ((nreq (%gf-slot info +gf-nreq+)) (nopt (%gf-slot info +gf-nopt+))
            (state :req) (i 0) (j 0) (r nil) (restvar (gensym "REST")) (rest-bound nil))
        (flet ((rest-form () `(nthcdr ,(+ nreq nopt) ,argsvar)))
          (dolist (x args-ll (nreverse r))
            (cond ((eq x '&whole) (setq state :whole))
                  ((eq x '&optional) (setq state :opt))
                  ((eq x '&rest) (setq state :rest))
                  ((eq x '&key) (setq state :key)
                   (unless rest-bound (push (list restvar (rest-form)) r) (setq rest-bound t)))
                  ((eq x '&allow-other-keys) nil)
                  ((eq x '&aux) (setq state :aux))
                  ((eq state :whole) (push (list x argsvar) r) (setq state :req))
                  ((eq state :req)
                   (push (list x (if (< i nreq) `(nth ,i ,argsvar) nil)) r)
                   (setq i (+ i 1)))
                  ((eq state :opt)
                   (let* ((var (if (consp x) (car x) x)) (default (if (consp x) (cadr x) nil))
                          (svar (if (consp x) (caddr x) nil)) (pos (+ nreq j)))
                     (if (< j nopt)
                         (progn
                           (push (list var `(if (nthcdr ,pos ,argsvar) (nth ,pos ,argsvar) ,default)) r)
                           (when svar (push (list svar `(not (null (nthcdr ,pos ,argsvar)))) r)))
                         (progn (push (list var default) r) (when svar (push (list svar nil) r))))
                     (setq j (+ j 1))))
                  ((eq state :rest)
                   (push (list x (rest-form)) r))
                  ((eq state :key)
                   (let* ((spec (if (consp x) (car x) x))
                          (var (if (consp spec) (cadr spec) spec))
                          (kw (%key-name x))
                          (default (if (consp x) (cadr x) nil))
                          (svar (if (consp x) (caddr x) nil))
                          (found (gensym "FOUND")))
                     (push (list found `(%plist-find ,restvar ',kw)) r)
                     (push (list var `(if ,found (cadr ,found) ,default)) r)
                     (when svar (push (list svar `(not (null ,found))) r))))
                  ((eq state :aux)
                   (push (if (consp x) (list (car x) (cadr x)) (list x nil)) r))))))))

(defun %plist-find (plist key)
  (do ((l plist (cddr l))) ((or (atom l) (atom (cdr l))) nil)
    (when (eq (car l) key) (return l))))

;;; call-next-method
(defun %no-next-method (cell args)
  (let ((m (car cell)))
    (apply #'no-next-method (and m (%method-gf m)) m args)))

(defun %call-next-method (args next cnm-args cell)
  (when cnm-args (%check-cnm-args cell args cnm-args))
  (%call-next-method-unchecked args next cnm-args cell))

(defun %call-next-method-unchecked (args next cnm-args cell)
  (if (null next)
      (%no-next-method cell (or cnm-args args))
      (funcall (car next) (or cnm-args args) (cdr next))))

(defun %call-next-method-fast (args next cnm-args cell)
  "CALL-NEXT-METHOD as a function in a method whose next method NEXT is a
function of the arguments (or NIL)."
  (when cnm-args (%check-cnm-args cell args cnm-args))
  (%call-next-method-fast-unchecked args next cnm-args cell))

(defun %call-next-method-fast-unchecked (args next cnm-args cell)
  (if (null next)
      (%no-next-method cell (or cnm-args args))
      (apply (the function next) (or cnm-args args))))

;;; CLHS CALL-NEXT-METHOD: with arguments, the ordered set of applicable
;;; methods must be the one of the original arguments.  As in SBCL, this is
;;; checked only in a method declared (OPTIMIZE (SAFETY 3)); in other methods
;;; the arguments are passed on unchecked (an :AROUND method that unwraps an
;;; argument, as Lem has, is not an error there).  The set is the same when every
;;; argument has the cache key (class or EQL specializer object) of the
;;; original one; otherwise the applicable methods of the two cache entries
;;; are compared.  CELL is (method . memo): the memo is #(cache old-keys
;;; new-keys) of the last such comparison that passed.
(defun %cnm-arg-ok (cell i old new)
  "True when the required argument NEW at position I, given to
CALL-NEXT-METHOD in place of OLD, has the same cache key."
  (let* ((m (car cell)) (gf (and m (%method-gf m))))
    (or (null gf)
        (let* ((info (%closure-name gf)) (eqls (%gf-slot info +gf-eql+)))
          (eq (%arg-key (%gf-slot info +gf-unspecialized+) eqls i old)
              (%arg-key (%gf-slot info +gf-unspecialized+) eqls i new))))))

(defun %check-cnm-args (cell args cnm-args)
  (let* ((method (car cell)) (gf (and method (%method-gf method))))
    (when gf
      (let* ((info (%gf-info gf)) (nreq (%gf-slot info +gf-nreq+)))
        (unless (< (length cnm-args) nreq)   ; else the call of the next method signals the error
          (let ((old (%gf-keys info args nreq)) (new (%gf-keys info cnm-args nreq))
                (memo (cdr cell)) (cache (%gf-slot info +gf-cache+)))
            (unless (or (do ((a old (cdr a)) (b new (cdr b))) ((null a) t)
                          (unless (eq (car a) (car b)) (return nil)))
                        (and memo (eq (svref memo 0) cache)
                             (do ((i 1 (+ i 1)) (a old (cdr a))) ((null a) t)
                               (unless (eq (svref memo i) (car a)) (return nil)))
                             (do ((i (+ nreq 1) (+ i 1)) (b new (cdr b))) ((null b) t)
                               (unless (eq (svref memo i) (car b)) (return nil)))))
              (if (equal (%gf-lookup info args nreq 1) (%gf-lookup info cnm-args nreq 1))
                  (setf (cdr cell) (apply #'vector (%gf-slot info +gf-cache+) (append old new)))
                  (error 'simple-error
                         :format-control "CALL-NEXT-METHOD: the arguments ~S change the set of applicable methods of ~S"
                         :format-arguments (list cnm-args gf))))))))))

;;; ------------------------------------------------------------
;;; defgeneric / defmethod
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-method-args (args)
    "Returns (values qualifiers specialized-lambda-list body)."
    (let ((quals nil))
      (do () ((or (null args) (listp (car args))))
        (push (pop args) quals))
      (when (null args)
        (error 'simple-program-error :format-control "DEFMETHOD: missing lambda list" :format-arguments nil))
      (values (reverse quals) (car args) (cdr args))))

  (defun %method-arg-bindings (ll argsvar)
    "Returns the LET* bindings of the variables of the lambda list LL (of a
method) to the elements of the argument list in ARGSVAR, and the variables
made for them.  The generic function has checked the number of arguments and
the keywords."
    (let ((state :req) (r nil) (tail argsvar) (temps nil))
      (dolist (x ll)
        (cond ((eq x '&optional) (setq state :opt))
              ((or (eq x '&rest) (eq x '&body)) (setq state :rest))
              ((eq x '&key) (setq state :key))
              ((eq x '&allow-other-keys) nil)
              ((eq x '&aux) (setq state :aux))
              ((eq state :req)
               (let ((next (gensym "ARGS")))
                 (push (list x `(car ,tail)) r)
                 (push (list next `(cdr ,tail)) r)
                 (push next temps)
                 (setq tail next)))
              ((eq state :opt)
               (let ((var (if (consp x) (car x) x)) (default (if (consp x) (cadr x) nil))
                     (svar (if (consp x) (caddr x) nil)) (next (gensym "ARGS")))
                 (push (list var `(if ,tail (car ,tail) ,default)) r)
                 (when svar (push (list svar `(not (null ,tail))) r))
                 (push (list next `(cdr ,tail)) r)
                 (push next temps)
                 (setq tail next)))
              ((eq state :rest) (push (list x tail) r))
              ((eq state :key)
               (let* ((spec (if (consp x) (car x) x))
                      (var (if (consp spec) (cadr spec) spec))
                      (default (if (consp x) (cadr x) nil))
                      (svar (if (consp x) (caddr x) nil))
                      (found (gensym "KEY")))
                 (push (list found `(%plist-find ,tail ',(%key-name x))) r)
                 (push found temps)
                 (push (list var `(if ,found (cadr ,found) ,default)) r)
                 (when svar (push (list svar `(not (null ,found))) r))))
              ((eq state :aux)
               (push (if (consp x) (list (car x) (cadr x)) (list x nil)) r))))
      (values (nreverse r) temps)))

  (defun %method-lambda (name ll body cell)
    "Returns (values lambda-form specializers plain-lambda-list doc)."
    (let ((plain-ll nil) (specs nil) (req t) (args (gensym "ARGS")) (next (gensym "NEXT")))
      (unless (listp ll)
        (error 'simple-program-error :format-control "Invalid specialized lambda list ~S" :format-arguments (list ll)))
      (dolist (x ll)
        (when (member x lambda-list-keywords) (setq req nil))
        (cond ((not req) (push x plain-ll))
              ((consp x)
               (unless (and (symbolp (car x)) (consp (cdr x)) (null (cddr x)))
                 (error 'simple-program-error :format-control "Invalid specialized parameter ~S"
                                              :format-arguments (list x)))
               (push (car x) plain-ll) (push (cadr x) specs))
              (t (push x plain-ll) (push t specs))))
      (setq plain-ll (reverse plain-ll) specs (reverse specs))
      (let ((req-vars (subseq plain-ll 0 (length specs))))
        (multiple-value-bind (decls doc forms) (%parse-body body t)
          (multiple-value-bind (bindings temps) (%method-arg-bindings plain-ll args)
            (values
             ;; the parameters are bound from the argument list (no APPLY)
             `(lambda (,args ,next)
                (declare (ignorable ,next))
                (flet ((call-next-method (&rest cnm-args)
                         (,(if (%safety-3-p decls) '%call-next-method '%call-next-method-unchecked)
                          ,args ,next cnm-args ,cell))
                       (next-method-p () (not (null ,next))))
                  (let* ,bindings
                    (declare (ignorable ,@temps ,@req-vars))
                    ,@decls
                    (block ,(if (consp name) (cadr name) name) ,@forms))))
             specs
             plain-ll
             doc)))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %tree-mentions-p (tree syms)
    (cond ((symbolp tree) (member tree syms))
          ((consp tree) (or (%tree-mentions-p (car tree) syms) (%tree-mentions-p (cdr tree) syms)))
          (t nil)))

  (defun %only-operators-p (tree syms)
    "True when the symbols SYMS occur in the list TREE only as first elements
of lists (so that binding them as local macros cannot change the meaning of
#'CALL-NEXT-METHOD and the like)."
    (and (let ((x (car tree))) (if (consp x) (%only-operators-p x syms) t))
         (do ((l (cdr tree) (cdr l))) ((atom l) (not (and l (member l syms))))
           (let ((x (car l)))
             (cond ((consp x) (unless (%only-operators-p x syms) (return nil)))
                   ((member x syms) (return nil)))))))

  (defun %expand-fast-cnm (cnm-args vars next cell unspec check)
    "The expansion of (CALL-NEXT-METHOD . CNM-ARGS) in a method whose required
arguments are in VARS and whose next method (a function of the arguments, or
NIL) is in NEXT.  UNSPEC holds the bits of the parameters that no method of
the generic function specializes: any argument will do for them.  CHECK is
true in a method declared (OPTIMIZE (SAFETY 3)): the arguments must then have
the applicable methods of the original ones."
    (cond ((null cnm-args)
           `(if ,next
                (funcall (the function ,next) ,@vars)
                (%no-next-method ,cell (list ,@vars))))
          ((not check)
           (if (= (length cnm-args) (length vars))
               (let ((new (mapcar (lambda (v) (declare (ignore v)) (gensym "ARG")) vars)))
                 `(let ,(mapcar #'list new cnm-args)
                    (if ,next
                        (funcall (the function ,next) ,@new)
                        (%no-next-method ,cell (list ,@new)))))
               `(%call-next-method-fast-unchecked (list ,@vars) ,next (list ,@cnm-args) ,cell)))
          ((= (length cnm-args) (length vars))
           (let ((new (mapcar (lambda (v) (declare (ignore v)) (gensym "ARG")) vars)) (i -1))
             `(let ,(mapcar #'list new cnm-args)
                (unless (and ,@(mapcar (lambda (v n)
                                         (setq i (+ i 1))
                                         `(or (eq ,v ,n) (not (eq (%fxlogand ,unspec ,(ash 1 i)) 0))
                                              (%cnm-arg-ok ,cell ,i ,v ,n)))
                                       vars new))
                  (%check-cnm-args ,cell (list ,@vars) (list ,@new)))
                (if ,next
                    (funcall (the function ,next) ,@new)
                    (%no-next-method ,cell (list ,@new))))))
          (t `(%call-next-method-fast (list ,@vars) ,next (list ,@cnm-args) ,cell))))

  (defun %fast-method-maker (name plain-ll body cell)
    "A form for a function making, from the next method, the function of the
arguments of a method with the required parameters PLAIN-LL whose BODY uses
CALL-NEXT-METHOD or NEXT-METHOD-P."
    (multiple-value-bind (decls doc forms) (%parse-body body t)
      (declare (ignore doc))
      (let* ((next (gensym "NEXT")) (unspec (gensym "UNSPEC")) (check (%safety-3-p decls))
             (vars (mapcar (lambda (v) (declare (ignore v)) (gensym "A")) plain-ll))
             (inner `(let ,(mapcar #'list plain-ll vars)
                       ,@decls
                       (declare (ignorable ,@plain-ll))
                       (block ,(if (consp name) (cadr name) name) ,@forms))))
        `(lambda (,next ,unspec)
           (declare (ignorable ,unspec))
           (lambda ,vars
             ,(if (%only-operators-p (cons 'progn body) '(call-next-method next-method-p))
                  `(macrolet ((call-next-method (&rest cnm-args)
                                (%expand-fast-cnm cnm-args ',vars ',next ',cell ',unspec ',check))
                              (next-method-p () '(not (null ,next))))
                     ,inner)
                  `(flet ((call-next-method (&rest cnm-args)
                            (,(if check '%call-next-method-fast '%call-next-method-fast-unchecked)
                             (list ,@vars) ,next cnm-args ,cell))
                          (next-method-p () (not (null ,next))))
                     ,inner))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %spread-bindings (ll vars)
    "The LET* bindings of the variables of the method lambda list LL from the
spread arguments in VARS (%FAST-SHAPE), and the temporary variables."
    (let ((state :req) (r nil) (temps nil) (v vars) (more nil))
      (dolist (x ll)
        (cond ((eq x '&optional) (setq state :opt))
              ((or (eq x '&rest) (eq x '&body)) (setq state :rest))
              ((eq x '&key) (setq state :key))
              ((eq x '&allow-other-keys) nil)
              ((eq x '&aux) (setq state :aux))
              ((eq state :req) (push (list x (pop v)) r))
              ((eq state :opt)
               (let ((var (if (consp x) (car x) x)) (default (if (consp x) (cadr x) nil))
                     (svar (if (consp x) (caddr x) nil)) (s (pop v)))
                 (push (list var `(if (%unboundp ,s) ,default ,s)) r)
                 (when svar (push (list svar `(not (%unboundp ,s))) r))))
              ((eq state :rest) (push (list x (or more (setq more (pop v)))) r))
              ((eq state :key)
               (let* ((spec (if (consp x) (car x) x))
                      (var (if (consp spec) (cadr spec) spec))
                      (default (if (consp x) (cadr x) nil))
                      (svar (if (consp x) (caddr x) nil))
                      (found (gensym "KEY")))
                 ;; (%PLIST-FIND inline: a method compiled in the image or
                 ;; into a fasl would call the runtime's function)
                 (push (list found `(%plist-find ,(or more (setq more (pop v))) ',(%key-name x))) r)
                 (push found temps)
                 (push (list var `(if ,found (cadr ,found) ,default)) r)
                 (when svar (push (list svar `(not (null ,found))) r))))
              ((eq state :aux)
               (push (if (consp x) (list (car x) (cadr x)) (list x nil)) r))))
      (values (nreverse r) temps)))

  (defun %expand-spread-cnm (cnm-args vars next cell shape check)
    "The expansion of (CALL-NEXT-METHOD . CNM-ARGS) in a method of the spread
arguments in VARS (shape SHAPE), whose next method is in NEXT."
    (destructuring-bind (nreq nopt more) shape
      (if cnm-args
          `(%call-next-method-spread (list ,@vars) ,next (list ,@cnm-args) ,cell ,nreq ,nopt ,more ,check)
          `(if ,next
               (funcall (the function ,next) ,@vars)
               (%no-next-method ,cell (%unspread ,nreq ,nopt ,more (list ,@vars)))))))

  (defun %spread-method-forms (name plain-ll body cell shape)
    "Returns (values fast cnm-p): the form of the function of the spread
arguments (shape SHAPE) of a method with the lambda list PLAIN-LL, or, when its
BODY uses CALL-NEXT-METHOD or NEXT-METHOD-P (CNM-P), the form of the function
making that function from the next method (as %FAST-METHOD-MAKER)."
    (destructuring-bind (nreq nopt more) shape
      (multiple-value-bind (decls doc forms) (%parse-body body t)
        (declare (ignore doc))
        (let* ((vars (mapcar (lambda (v) (declare (ignore v)) (gensym "S"))
                             (make-list (+ nreq nopt more))))
               (cnm-p (%tree-mentions-p body '(call-next-method next-method-p)))
               (all-vars (let ((r nil))
                           (dolist (x plain-ll (nreverse r))
                             (unless (member x lambda-list-keywords)
                               (let ((y (if (consp x) (car x) x)))
                                 (push (if (consp y) (cadr y) y) r)
                                 (when (and (consp x) (consp (cddr x))) (push (caddr x) r))))))))
          (multiple-value-bind (bindings temps) (%spread-bindings plain-ll vars)
            (let ((inner `(let* ,bindings
                            (declare (ignorable ,@temps ,@all-vars))
                            ,@decls
                            (block ,(if (consp name) (cadr name) name) ,@forms))))
              (if (not cnm-p)
                  (values `(lambda ,vars ,inner) nil)
                  (let ((next (gensym "NEXT")) (unspec (gensym "UNSPEC")) (check (%safety-3-p decls)))
                    (values
                     `(lambda (,next ,unspec)
                        (declare (ignorable ,unspec))
                        (lambda ,vars
                          ,(if (%only-operators-p (cons 'progn body) '(call-next-method next-method-p))
                               `(macrolet ((call-next-method (&rest cnm-args)
                                             (%expand-spread-cnm cnm-args ',vars ',next ',cell ',shape ',check))
                                           (next-method-p () '(not (null ,next))))
                                  ,inner)
                               `(flet ((call-next-method (&rest cnm-args)
                                         (if (or cnm-args (null ,next))
                                             (%call-next-method-spread (list ,@vars) ,next cnm-args ,cell
                                                                       ,nreq ,nopt ,more ,check)
                                             (funcall (the function ,next) ,@vars)))
                                       (next-method-p () (not (null ,next))))
                                  ,inner))))
                     t))))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %spread-registration (cnm-p name quals specs-form plain-ll shape doc cell fast args-var next)
    "The form defining a method of spread arguments whose function (or maker) is in FAST."
    (if cnm-p
        `(%note-method-fast-maker
          (%defmethod ',name ',quals ,specs-form ',plain-ll
                      (%spread-maker-method-function ,fast ,@shape) ,doc ,cell)
          ,fast)
        `(%note-method-fast-function
          (%defmethod ',name ',quals ,specs-form ',plain-ll
                      (lambda (,args-var ,next)
                        (declare (ignore ,next))
                        (apply ,fast (%spread-list ,@shape ,args-var)))
                      ,doc ,cell)
          ,fast)))

  (defun %cnm-without-arguments-p (tree)
    "True when every CALL-NEXT-METHOD in TREE is a call without arguments."
    (cond ((atom tree) t)
          ((eq (car tree) 'call-next-method) (null (cdr tree)))
          (t (do ((l tree (cdr l))) ((atom l) t)
               (unless (%cnm-without-arguments-p (car l)) (return nil))))))

  (defun %keyed-method-form (name plain-ll body cell shape)
    "The form of the function making, from the next method and the keywords of
the generic function's keyed main function, the method as a function of the
four keyed spread arguments (%GF-KEYED-ENTRY), or NIL when the method cannot
be one (&REST, more than three required and optional parameters, or
CALL-NEXT-METHOD with arguments or as a function)."
    (destructuring-bind (nreq nopt more) shape
      (declare (ignore more))
      (when (and (member '&key plain-ll)
                 (not (member '&rest plain-ll)) (not (member '&body plain-ll))
                 (<= (+ nreq nopt) 3)
                 (or (not (%tree-mentions-p body '(call-next-method next-method-p)))
                     (and (%only-operators-p (cons 'progn body) '(call-next-method next-method-p))
                          (%cnm-without-arguments-p body))))
        (multiple-value-bind (decls doc forms) (%parse-body body t)
          (declare (ignore doc))
          (let* ((vars (list (gensym "S") (gensym "S") (gensym "S") (gensym "S")))
                 (next (gensym "NEXT")) (gfkeys (gensym "GFKEYS")) (wide (gensym "WIDE"))
                 (npos (+ nreq nopt))
                 (slots nil) (bindings nil) (temps nil) (all-vars nil)
                 (state :req) (i 0))
            (dolist (x plain-ll)
              (cond ((eq x '&optional) (setq state :opt))
                    ((eq x '&key) (setq state :key))
                    ((eq x '&allow-other-keys) nil)
                    ((eq x '&aux) (setq state :aux))
                    ((eq state :req) (push (list x (nth i vars)) bindings) (push x all-vars) (setq i (+ i 1)))
                    ((eq state :opt)
                     (let ((var (if (consp x) (car x) x)) (default (if (consp x) (cadr x) nil))
                           (svar (if (consp x) (caddr x) nil)) (s (nth i vars)))
                       (push (list var `(if (%unboundp ,s) ,default ,s)) bindings) (push var all-vars)
                       (when svar (push (list svar `(not (%unboundp ,s))) bindings) (push svar all-vars))
                       (setq i (+ i 1))))
                    ((eq state :key)
                     (let* ((spec (if (consp x) (car x) x))
                            (var (if (consp spec) (cadr spec) spec))
                            (default (if (consp x) (cadr x) nil))
                            (svar (if (consp x) (caddr x) nil))
                            (slot (gensym "SLOT")) (raw (gensym "RAW")))
                       (push (list slot `(%key-slot ,gfkeys ',(%key-name x) ,npos)) slots)
                       ;; (a wide keyed effective method: the slots from
                       ;; the fourth on are in a vector, the fourth argument)
                       (push (list raw `(if ,wide
                                            (if (and ,slot (%fx>= ,slot 3))
                                                (%svref ,(nth 3 vars) (%fx- ,slot 3))
                                                (case ,slot
                                                  ,@(loop for j from 0 to 2 collect (list j (nth j vars)))
                                                  (t (%unbound-marker))))
                                            (case ,slot
                                              ,@(loop for j from 0 to 3 collect (list j (nth j vars)))
                                              (t (%unbound-marker)))))
                             bindings)
                       (push raw temps)
                       (push (list var `(if (%unboundp ,raw) ,default ,raw)) bindings) (push var all-vars)
                       (when svar (push (list svar `(not (%unboundp ,raw))) bindings) (push svar all-vars))))
                    ((eq state :aux)
                     (push (if (consp x) (list (car x) (cadr x)) (list x nil)) bindings)
                     (push (if (consp x) (car x) x) all-vars))))
            `(lambda (,next ,gfkeys)
               (declare (ignorable ,next))
               (let (,@(reverse slots) (,wide (> (+ ,npos (length ,gfkeys)) 4)))
                 (declare (ignorable ,wide))
                 (lambda ,vars
                   (declare (ignorable ,@vars))
                   (macrolet ((call-next-method ()
                                '(if ,next
                                  (funcall (the function ,next) ,@vars)
                                  (%no-next-method ,cell (%keyed-args ,nreq ,nopt ,gfkeys
                                                                      (if ,wide
                                                                          (%unwide-keyed (list ,@vars))
                                                                          (list ,@vars))))))
                              (next-method-p () '(not (null ,next))))
                     (let* ,(reverse bindings)
                       (declare (ignorable ,@temps ,@all-vars))
                       ,@decls
                       (block ,(if (consp name) (cadr name) name) ,@forms))))))))))))

(defmacro %defmethod-1 (name &rest args)
  (multiple-value-bind (quals ll body) (%split-method-args args)
    (let ((cell (gensym "CELL")) (fast (gensym "FAST")))
      (multiple-value-bind (lam specs plain-ll doc) (%method-lambda name ll body cell)
        (let ((specs-form `(list ,@(mapcar (lambda (s) (if (and (consp s) (eq (car s) 'eql))
                                                            `(list 'eql ,(cadr s))
                                                            `',s))
                                            specs)))
              (maker (progn
                       (%note-key-layout name plain-ll nil)
                       `(%gf-function-maker ,(%fast-shape plain-ll) ,(nth-value 4 (%ll-info plain-ll)))))
              (shape (%fast-shape plain-ll)))
          (cond ((and shape (not (%fast-arity plain-ll)))
                 ;; optional or rest/keyword parameters: the method is also
                 ;; known as a function of the spread arguments (%FAST-SHAPE)
                 (multiple-value-bind (form cnm-p) (%spread-method-forms name plain-ll body cell shape)
                   (let ((args-var (gensym "ARGS")) (next (gensym "NEXT"))
                         (keyed (%keyed-method-form name plain-ll body cell shape)))
                     `(let* ((,cell (list nil))
                             (*gf-function-maker* ,maker)
                             (,fast ,form))
                        ,(if keyed
                             `(%note-method-keyed-maker ,(%spread-registration cnm-p name quals specs-form plain-ll
                                                                               shape doc cell fast args-var next)
                                                        ,keyed)
                             (%spread-registration cnm-p name quals specs-form plain-ll shape doc cell fast
                                                   args-var next))))))
                ((and (listp ll) (notany (lambda (x) (member x lambda-list-keywords)) plain-ll)
                      (not (%tree-mentions-p body '(call-next-method next-method-p))))
                 ;; the method function applies a function of the arguments, which
                 ;; is also called directly by the dispatch of one to four arguments
                 (multiple-value-bind (decls doc2 forms) (%parse-body body t)
                   (declare (ignore doc2))
                   (let ((args-var (gensym "ARGS")) (next (gensym "NEXT")))
                     `(let ((,cell (list nil))
                            (*gf-function-maker* ,maker)
                            (,fast (lambda ,plain-ll
                                     ,@decls
                                     (declare (ignorable ,@plain-ll))
                                     (block ,(if (consp name) (cadr name) name) ,@forms))))
                        (%note-method-fast-function
                         (%defmethod ',name ',quals ,specs-form ',plain-ll
                                     (lambda (,args-var ,next) (declare (ignore ,next)) (apply ,fast ,args-var))
                                     ,doc ,cell)
                         ,fast)))))
                ((%fast-arity plain-ll)
                 ;; CALL-NEXT-METHOD calls the next method, a function of the arguments
                 `(let* ((,cell (list nil))
                         (*gf-function-maker* ,maker)
                         (,fast ,(%fast-method-maker name plain-ll body cell)))
                    (%note-method-fast-maker
                     (%defmethod ',name ',quals ,specs-form ',plain-ll (%maker-method-function ,fast) ,doc ,cell)
                     ,fast)))
                (t
                 `(let ((,cell (list nil)) (*gf-function-maker* ,maker))
                    (%defmethod ',name ',quals ,specs-form ',plain-ll (function ,lam) ,doc ,cell)))))))))

;;; ------------------------------------------------------------
;;; Caches at call sites
;;; ------------------------------------------------------------
;;; A call (F x) or (F x y) of a generic function F of one or two required
;;; parameters, compiled after its DEFGENERIC or a DEFMETHOD, first looks in
;;; a cache of its own: a vector with four entries of the classes of the
;;; arguments (0 for a parameter that no method specializes) and the function
;;; that the generic function found for them, or the location of the slot
;;; when that function is a slot reader (one argument) or writer (two: the
;;; new value and the object).  A hit calls that function directly, or reads
;;; or writes the slot; anything else (another class, a changed *CLOS-EPOCH*,
;;; a generic function with EQL specializers, a name that is no longer a
;;; generic function of that shape) calls the function of the name as usual.
;;; The function (SETF NAME) of a generic function has such call sites too:
;;; its compiler macro is that of the calls (FUNCALL #'(SETF NAME) ...) that
;;; SETF of (NAME object) expands to.
;;;   one argument:   #(epoch a fn-a b fn-b c fn-c d fn-d name misses registered)
;;;   two arguments:  #(epoch a0 a1 fn-a b0 b1 fn-b c0 c1 fn-c d0 d1 fn-d name misses registered)
;;; The call site compares the class of an instance with the four entries
;;; and then reads or writes the slot, or calls the function, in one place;
;;; an argument that is not an instance, an unbound slot and a miss are out
;;; of line (%GF-SITE-OTHER1, %GF-SITE-OTHER2), so that a call site stays
;;; small (about 55 Wasm instructions; 140 with an access for each entry).

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %gf-site-expansion (form args name n)
    (if (not (= (length args) n))
        form
        (let ((c (if (eql n 1)
                     (vector 0 nil nil nil nil nil nil nil nil name 0 nil)
                     (vector 0 nil nil nil nil nil nil nil nil nil nil nil nil name 0 nil)))
              (a (gensym "A")) (b (gensym "B")) (k (gensym "K")) (kb (gensym "KB"))
              (f (gensym "F")) (v (gensym "V")) (blk (gensym "SITE")))
          (if (eql n 1)
              ;; (no epoch is compared: %BUMP-CLOS-EPOCH empties the cache;
              ;; the key of an argument that is not an instance is 0, which
              ;; is the key of the entry of a parameter that no method
              ;; specializes, or no key: out of line)
              `(let* ((,a ,(car args))
                      (,k (if (%instancep ,a) (%instance-layout ,a) 0))
                      ;; the value of the entry: the function to call, or the
                      ;; location of the slot that a reader reads
                      (,f (cond ((eq ,k (%svref ',c 1)) (%svref ',c 2))
                                ((eq ,k (%svref ',c 3)) (%svref ',c 4))
                                ((eq ,k (%svref ',c 5)) (%svref ',c 6))
                                ((eq ,k (%svref ',c 7)) (%svref ',c 8))
                                (t nil))))
                 (block ,blk
                   (if (%i31p ,f)
                       (let ((,v (%instance-ref ,a ,f)))
                         (unless (%unboundp ,v) (return-from ,blk ,v)))
                       (when ,f (return-from ,blk (funcall (the function ,f) ,a))))
                   (if (eq (%svref ',c 0) -1)
                       ;; a site that is not cached (-1): the usual call
                       (locally (declare (notinline ,name)) (,name ,a))
                       (%gf-site-other1 ',c ,a))))
              ;; (a parameter that no method specializes has the key 0 in
              ;; every entry; an argument that is not an instance has no key
              ;; here, NIL: an empty entry, out of line)
              `(let ((,a ,(car args)) (,b ,(cadr args)))
                 (if (eq (%svref ',c 0) -1)
                     ;; a site that is not cached: the usual call
                     ,(if (symbolp name)
                          `(locally (declare (notinline ,name)) (,name ,a ,b))
                          `(%gf-site-call2 ',c ,a ,b))
                     (let* ((,k (cond ((eq (%svref ',c 1) 0) 0) ((%instancep ,a) (%instance-layout ,a)) (t nil)))
                            (,kb (cond ((eq (%svref ',c 2) 0) 0) ((%instancep ,b) (%instance-layout ,b)) (t nil)))
                            ;; the value of the entry: the function to call, or the
                            ;; location of the slot that a writer writes
                            (,f (cond ,@(loop for i in '(1 4 7 10)
                                              collect `((and (eq (%svref ',c ,i) ,k) (eq (%svref ',c ,(+ i 1)) ,kb))
                                                        (%svref ',c ,(+ i 2))))
                                      (t nil))))
                       (cond ((%i31p ,f) (%set-instance-ref ,b ,f ,a) ,a)
                             (,f (funcall (the function ,f) ,a ,b))
                             (t (%gf-site-other2 ',c ,a ,b))))))))))

  (defun %gf-site-macro-forms (name lambda-list)
    "Forms defining the compiler macro of the generic function NAME that
gives its call sites a cache, when LAMBDA-LIST has one or two required
parameters and nothing else, and NAME is a symbol outside the implementation's packages."
    (let* ((n (and (listp lambda-list) (length lambda-list)))
           ;; (SETF NAME): the flag is on NAME
           (setfp (and (consp name) (eq (car name) 'setf) (consp (cdr name)) (symbolp (cadr name))
                       (null (cddr name)) (eql n 2)))
           (sym (if setfp (cadr name) name))
           (flag (if setfp '%gf-setf-site-macro '%gf-site-macro)))
      (if (and (symbolp sym) sym n (<= 1 n 2)
               (notany (lambda (x) (member x lambda-list-keywords)) lambda-list)
               (symbol-package sym)
               (not (member (package-name (symbol-package sym)) '("COMMON-LISP" "SYSTEM" "CLYSM" "KEYWORD")
                            :test #'string=))
               (or (not (eql (get sym flag) n))
                   ;; in the image: the form setting the flag may have been
                   ;; expanded without being evaluated (a toplevel form of a
                   ;; loaded file is expanded more than once)
                   (and (boundp '*note-key-layout-hook*) (symbol-value '*note-key-layout-hook*)
                        (null (compiler-macro-function name)))))
          (progn
            (%put sym flag n)
            `((define-compiler-macro ,name (&whole form &rest args)
                (%gf-site-expansion form args ',name ,n))))
          nil))))

(defun %gf-site-info (name n)
  "The generic function metaobject of NAME when its call sites may cache by
the classes of N arguments."
  (and (fboundp name)
       (let ((info (%gf-info (fdefinition name))))
         (and info
              (let ((pic (%gf-slot info +gf-cache+)))
                (and (eq (%svref pic 1) n) (eql (%gf-slot info +gf-nreq+) n) (null (%svref pic 2))
                     info))))))

(defun %gf-site-miss-count (c i)
  "Count a miss of the site cache C (the count is at index I); a site that
keeps missing (more classes than entries) stops caching."
  (let ((m (%fx+ (%svref c i) 1)))
    (%set-svref c i m)
    (when (%fx> m 16) (%set-svref c 0 -1))))

(defun %gf-site-miss1 (c a)
  (let* ((name (%svref c 9)) (info (%gf-site-info name 1)))
    (if (null info)
        (progn (%set-svref c 0 -1) (funcall name a))
        (let ((f (%gf-find1 info a)))
          (if (null f)
              (funcall name a)
              (progn
                (%gf-site-register c 11)
                ;; (the search may have changed the epoch: the first call of a method computes things)
                ;; (the entries move one place down, the new one first)
                (if (eq (%svref c 0) *clos-epoch*)
                    (progn (when (%svref c 7) (%gf-site-miss-count c 10))
                           (do ((i 8 (- i 1))) ((< i 3)) (%set-svref c i (%svref c (- i 2)))))
                    (progn (do ((i 3 (+ i 1))) ((> i 8)) (%set-svref c i nil))
                           (%set-svref c 10 0)
                           (%set-svref c 0 *clos-epoch*)))
                (%set-svref c 1 (if (eq (%fxlogand (%svref (%gf-slot info +gf-cache+) 3) 1) 0) (%fast-class-of a) 0))
                (%set-svref c 2 (if (eq (%svref c 1) 0) f (or (gethash f *reader-locations*) f)))
                (funcall (the function f) a)))))))

(defun %gf-site-call1 (c a) (funcall (%svref c 9) a))
;;; What a call site does not do inline: the entries of arguments that
;;; are not instances, an unbound slot, a site that is not cached and the
;;; misses, in one function that every call site calls (the call sites stay
;;; small)
(defun %gf-site-other1 (c a)
  (let ((k (%fast-class-of a)))
    (macrolet ((entry (i)
                 `(when (eq (%svref c ,i) k)
                    (let ((f (%svref c ,(+ i 1))))
                      (return-from %gf-site-other1
                        (if (%i31p f)
                            (let ((v (%instance-ref a f)))
                              (if (%unboundp v) (%gf-site-call1 c a) v))
                            (funcall (the function f) a)))))))
      (entry 1) (entry 3) (entry 5) (entry 7))
    (if (eq (%svref c 0) -1)
        ;; a site that is not cached: the usual call
        (funcall (%svref c 9) a)
        (%gf-site-miss1 c a))))

(defun %gf-site-other2 (c a b)
  (if (eq (%svref c 0) -1)
      ;; a site that is not cached: the usual call
      (%gf-site-call2 c a b)
      ;; (a parameter that no method specializes has the key 0 in every entry)
      (let ((k (if (eq (%svref c 1) 0) 0 (%fast-class-of a)))
            (kb (if (eq (%svref c 2) 0) 0 (%fast-class-of b))))
        (macrolet ((entry (i)
                     `(when (and (eq (%svref c ,i) k) (eq (%svref c ,(+ i 1)) kb))
                        (let ((f (%svref c ,(+ i 2))))
                          (return-from %gf-site-other2
                            (if (%i31p f)
                                (progn (%set-instance-ref b f a) a)
                                (funcall (the function f) a b)))))))
          (entry 1) (entry 4) (entry 7) (entry 10))
        (%gf-site-miss2 c a b))))

(defun %gf-site-class (x)
  "The cache key of an argument that is not an instance."
  (%fast-class-of x))

(defun %gf-site-register (c i)
  "Note the call site cache C (its flag at I) for %BUMP-CLOS-EPOCH."
  (unless (%svref c i)
    (%set-svref c i t)
    (push c *gf-sites*)))

(defun %gf-site-call2 (c a b) (funcall (fdefinition (%svref c 13)) a b))

(defun %gf-site-miss2 (c a b)
  (let* ((name (%svref c 13)) (info (%gf-site-info name 2)))
    (if (null info)
        (progn (%set-svref c 0 -1) (%gf-site-call2 c a b))
        (let ((f (%gf-find2 info a b)))
          (if (null f)
              (%gf-site-call2 c a b)
              (let ((unspec (%svref (%gf-slot info +gf-cache+) 3)))
                (%gf-site-register c 15)
                ;; (the entries move one place down, the new one first)
                (if (eq (%svref c 0) *clos-epoch*)
                    (progn (when (%svref c 12) (%gf-site-miss-count c 14))
                           (do ((i 12 (- i 1))) ((< i 4)) (%set-svref c i (%svref c (- i 3)))))
                    (progn (do ((i 1 (+ i 1))) ((> i 12)) (%set-svref c i nil))
                           (%set-svref c 14 0)
                           (%set-svref c 0 *clos-epoch*)))
                (%set-svref c 1 (if (eq (%fxlogand unspec 1) 0) (%fast-class-of a) 0))
                (%set-svref c 2 (if (eq (%fxlogand unspec 2) 0) (%fast-class-of b) 0))
                (%set-svref c 3 (if (eq (%svref c 2) 0) f (or (gethash f *writer-locations*) f)))
                (funcall (the function f) a b)))))))

(defmacro defmethod (name &rest args)
  (multiple-value-bind (quals ll body) (%split-method-args args)
    (declare (ignore quals body))
    (let ((site (%gf-site-macro-forms name (and (listp ll) (mapcar (lambda (x) (if (consp x) (car x) x)) ll)))))
      (if site
          `(progn ,@site (%defmethod-1 ,name ,@args))
          `(%defmethod-1 ,name ,@args)))))

(defmacro defgeneric (name lambda-list &rest options)
  (let ((site (%gf-site-macro-forms name lambda-list)))
    (if site
        `(progn ,@site (%defgeneric-1 ,name ,lambda-list ,@options))
        `(%defgeneric-1 ,name ,lambda-list ,@options))))

(defmacro %defgeneric-1 (name lambda-list &rest options)
  (let ((combination nil) (methods nil) (doc nil) (apo nil) (gfclass nil) (mclass nil) (decls nil)
        (seen nil))
    (%check-gf-lambda-list lambda-list)
    (dolist (o options)
      (unless (and (consp o) (symbolp (car o)))
        (%program-error "DEFGENERIC ~S: invalid option ~S" name o))
      (unless (member (car o) '(:method declare))
        (when (member (car o) seen)
          (%program-error "DEFGENERIC ~S: option ~S given more than once" name (car o)))
        (push (car o) seen))
      (case (car o)
        (:method-combination (setq combination (cdr o)))
        (:method (push `(defmethod ,name ,@(cdr o)) methods))
        (:documentation
         (unless (and (stringp (cadr o)) (null (cddr o)))
           (%program-error "DEFGENERIC ~S: invalid :DOCUMENTATION option ~S" name o))
         (setq doc (cadr o)))
        (:argument-precedence-order (setq apo (list (cdr o))))
        (:generic-function-class (setq gfclass (cadr o)))
        (:method-class (setq mclass (cadr o)))
        (declare (setq decls (append decls (cdr o))))
        (t (%program-error "DEFGENERIC ~S: unknown option ~S" name o))))
    (when apo
      (multiple-value-bind (nreq) (%ll-info lambda-list)
        (let ((req (subseq lambda-list 0 nreq)))
          (unless (and (= (length (car apo)) nreq)
                       (every (lambda (x) (member x req)) (car apo))
                       (= (length (remove-duplicates (car apo))) nreq))
            (%program-error "DEFGENERIC ~S: invalid argument precedence order ~S" name (car apo))))))
    (let ((gf (gensym "GF")))
      (%note-key-layout name lambda-list t)
      `(let* ((*gf-function-maker* (%gf-function-maker ,(%fast-shape lambda-list)
                                                       ,(nth-value 4 (%ll-info lambda-list))))
              (,gf (%defgeneric ',name ',lambda-list ',combination ,doc ',apo ',gfclass ',mclass ',decls)))
         (%set-gf-initial-methods ,gf (list ,@(reverse methods)))
         ,gf))))

(defun %defgeneric-unlocked (name ll combination doc apo gfclass mclass decls)
  ;; methods defined by a previous DEFGENERIC form are removed
  (when (and (fboundp name) (%generic-function-p (fdefinition name)))
    (let* ((f (fdefinition name)) (info (%gf-info f)))
      (dolist (m (%gf-slot info +gf-initial-methods+)) (%remove-method f m))
      (%set-gf-slot info +gf-initial-methods+ nil)))
  (let ((f (apply #'ensure-generic-function name :lambda-list ll :method-combination combination
                  :documentation doc :declarations decls
                  (append (if apo (list :argument-precedence-order (car apo)) nil)
                          (if gfclass (list :generic-function-class gfclass) nil)
                          (if mclass (list :method-class mclass) nil)))))
    f))


(defun %defgeneric (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'%defgeneric-unlocked args))))

(defun %set-gf-initial-methods (gf methods)
  (%set-gf-slot (%gf-info gf) +gf-initial-methods+ methods))

;;; ------------------------------------------------------------
;;; define-method-combination
;;; ------------------------------------------------------------

(defun %define-short-method-combination (name operator identity doc)
  (%put name '%method-combination (list :short operator identity))
  (when doc (%set-doc name 'method-combination doc))
  name)

(defun %define-long-method-combination (name function args-ll doc)
  (%put name '%method-combination (list :long function args-ll))
  (when doc (%set-doc name 'method-combination doc))
  name)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %qualifier-pattern-test (pattern qvar)
    (cond ((eq pattern '*) t)
          ((null pattern) `(null ,qvar))
          (t `(%qualifiers-match-p ',pattern ,qvar))))

  (defun %method-group-clause (spec mvar qvar)
    (let ((var (car spec)) (rest (cdr spec)) (tests nil))
      (if (and rest (symbolp (car rest)) (car rest) (not (eq (car rest) '*)) (not (keywordp (car rest))))
          (progn (push `(funcall #',(car rest) ,qvar) tests) (setq rest (cdr rest)))
          (do () ((or (null rest) (keywordp (car rest))))
            (push (%qualifier-pattern-test (pop rest) qvar) tests)))
      `((or ,@(reverse tests)) (push ,mvar ,var))))

  (defun %rename-arguments-ll (ll)
    "The variables of an :ARGUMENTS lambda list are bound to forms (fresh
variables of the effective method) while the effective method form is computed."
    (let ((r nil) (bindings nil) (keyp nil))
      (flet ((ren (v) (let ((g (gensym (symbol-name v)))) (push (list v (list 'quote g)) bindings) g)))
        (dolist (x ll)
          (cond ((member x lambda-list-keywords) (unless (eq x '&allow-other-keys) (setq keyp (eq x '&key))) (push x r))
                ((and (symbolp x) keyp) (push (list (list (intern (symbol-name x) "KEYWORD") (ren x))) r))
                ((symbolp x) (push (ren x) r))
                (t (let* ((spec (car x))
                          (spec2 (cond ((and (symbolp spec) keyp)
                                        (list (intern (symbol-name spec) "KEYWORD") (ren spec)))
                                       ((symbolp spec) (ren spec))
                                       (t (list (car spec) (ren (cadr spec)))))))
                     (push (append (list spec2) (if (cdr x) (list (cadr x)) nil)
                                   (if (cddr x) (list (ren (caddr x))) nil))
                           r))))))
      (values (nreverse r) (nreverse bindings))))

  (defun %method-group-options (spec)
    (let ((rest (cdr spec)))
      (do () ((or (null rest) (keywordp (car rest)))) (pop rest))
      rest)))

(defun %qualifiers-match-p (pattern qualifiers)
  (cond ((eq pattern '*) t)
        ((null pattern) (null qualifiers))
        ((atom pattern) nil)
        ((null qualifiers) nil)
        ((or (eq (car pattern) '*) (equal (car pattern) (car qualifiers)))
         (%qualifiers-match-p (cdr pattern) (cdr qualifiers)))
        (t nil)))

(defmacro define-method-combination (name &rest options)
  (if (or (null options) (keywordp (car options)))
      ;; short form
      (let ((doc nil) (identity nil) (operator name))
        (do ((l options (cddr l))) ((null l))
          (case (car l)
            (:documentation (setq doc (cadr l)))
            (:identity-with-one-argument (setq identity (cadr l)))
            (:operator (setq operator (cadr l)))
            (t (%program-error "DEFINE-METHOD-COMBINATION: unknown option ~S" (car l)))))
        `(%define-short-method-combination ',name ',operator ',identity ,doc))
      ;; long form
      (destructuring-bind (lambda-list groups &rest body) options
        (let ((args-ll nil) (gfvar nil))
          (loop
            (cond ((and (consp (car body)) (eq (caar body) :arguments)) (setq args-ll (cdr (pop body))))
                  ((and (consp (car body)) (eq (caar body) :generic-function)) (setq gfvar (cadr (pop body))))
                  (t (return))))
          (multiple-value-bind (args-ll arg-bindings) (%rename-arguments-ll args-ll)
          (multiple-value-bind (decls doc forms) (%parse-body body t)
            (let* ((gf (gensym "GF")) (ms (gensym "METHODS")) (opts (gensym "OPTIONS"))
                   (m (gensym "M")) (q (gensym "Q"))
                   (vars (mapcar #'car groups)))
              `(%define-long-method-combination
                ',name
                (lambda (,gf ,ms ,opts)
                  (declare (ignorable ,gf))
                  (let (,@(if gfvar `((,gfvar ,gf)) nil) ,@arg-bindings)
                    (apply (lambda ,lambda-list
                             (let (,@vars)
                               ,@decls
                               (dolist (,m ,ms)
                                 (let ((,q (method-qualifiers ,m)))
                                   (cond ,@(mapcar (lambda (g) (%method-group-clause g m q)) groups)
                                         (t (invalid-method-error
                                             ,m "The method ~S has qualifiers ~S, which match no method group"
                                             ,m ,q)))))
                               ,@(mapcar (lambda (g)
                                           (let* ((o (%method-group-options g))
                                                  (order (getf o :order :most-specific-first))
                                                  (required (getf o :required nil))
                                                  (var (car g)))
                                             `(progn
                                                (let ((o ,order))
                                                  (cond ((eq o :most-specific-first) (setq ,var (nreverse ,var)))
                                                        ((eq o :most-specific-last))
                                                        (t (method-combination-error "Invalid :ORDER ~S" o))))
                                                ,@(when required
                                                    `((when (null ,var)
                                                        (method-combination-error
                                                         "No methods in the required method group ~S" ',var)))))))
                                         groups)
                               ,@forms))
                           ,opts)))
                ',args-ll ,doc))))))))

(defmacro call-method (method &optional next-methods)
  (declare (ignore method next-methods))
  (error "CALL-METHOD is only allowed in the body of an effective method"))

(defmacro make-method (form)
  (declare (ignore form))
  (error "MAKE-METHOD is only allowed as an argument of CALL-METHOD"))

;;; the built-in method combination types
(define-method-combination + :identity-with-one-argument t)
(define-method-combination and :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list :identity-with-one-argument nil)
(define-method-combination max :identity-with-one-argument t)
(define-method-combination min :identity-with-one-argument t)
(define-method-combination nconc :identity-with-one-argument nil)
(define-method-combination or :identity-with-one-argument t)
(define-method-combination progn :identity-with-one-argument t)

;;; ------------------------------------------------------------
;;; defclass
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %defclass-slot (class-name spec)
    "Returns (values canonical-slot-spec-form readers writers name)."
    (let* ((spec (if (consp spec) spec (list spec)))
           (name (car spec)) (opts (cdr spec))
           (initargs nil) (initform nil) (initformp nil) (readers nil) (writers nil)
           (allocation :instance) (type t) (doc nil) (seen nil) (others nil))
      (unless (and (symbolp name) name (not (constantp name)))
        (%program-error "DEFCLASS ~S: invalid slot name ~S" class-name name))
      (unless (evenp (length opts))
        (%program-error "DEFCLASS ~S: odd-length slot options in ~S" class-name spec))
      (do ((l opts (cddr l))) ((null l))
        (let ((key (car l)) (val (cadr l)))
          (when (member key '(:initform :type :documentation :allocation))
            (when (member key seen)
              (%program-error "DEFCLASS ~S: slot option ~S given more than once in ~S" class-name key spec))
            (push key seen))
          (case key
            (:initarg (unless (symbolp val) (%program-error "DEFCLASS ~S: invalid initarg ~S" class-name val))
             (push val initargs))
            (:initform (setq initform val initformp t))
            (:reader (unless (and val (symbolp val)) (%program-error "DEFCLASS ~S: invalid reader ~S" class-name val))
             (push val readers))
            (:writer (push val writers))
            (:accessor (unless (and val (symbolp val))
                         (%program-error "DEFCLASS ~S: invalid accessor ~S" class-name val))
             (push val readers) (push `(setf ,val) writers))
            (:allocation (setq allocation val))
            (:type (setq type val))
            (:documentation (unless (stringp val) (%program-error "DEFCLASS ~S: invalid documentation ~S" class-name val))
             (setq doc val))
            ;; other slot options are passed to the slot definition class
            ;; (AMOP: one value, or the list of values of repeated options)
            (t (let ((e (assoc key others)))
                 (if e (setf (cdr e) (append (cdr e) (list val))) (push (list key val) others)))))))
      (values `(list :name ',name :initargs ',(reverse initargs) :initform ',initform
                     :initfunction ,(if initformp `(lambda () ,initform) nil)
                     :readers ',(reverse readers) :writers ',(reverse writers) :allocation ',allocation
                     :type ',type :documentation ',doc
                     ,@(mapcan (lambda (e) (list `',(car e) (if (cddr e) `',(cdr e) `',(cadr e))))
                               (reverse others)))
              (reverse readers) (reverse writers) name))))

(defun %define-slot-accessor (name class-name slot-name writerp)
  "Define the reader (or writer) method NAME of the slot SLOT-NAME of the class
CLASS-NAME, as DEFMETHOD would."
  (let* ((*gf-function-maker* nil)
         (fast (if writerp
                   (lambda (new-value object) (%set-slot-value-slow object slot-name new-value))
                   (lambda (object) (%slot-value-slow object slot-name)))))
    (%note-slot-accessor
     (%note-method-fast-function
      (%defmethod name nil (if writerp (list t class-name) (list class-name))
                  (if writerp '(new-value object) '(object))
                  (lambda (args next) (declare (ignore next)) (apply fast args))
                  nil (list nil))
      fast)
     slot-name writerp)))

(defmacro defclass (name supers slots &rest options)
  (let ((slotd-forms nil) (accessor-forms nil) (default-initargs nil) (metaclass nil) (doc nil)
        (seen nil) (slot-names nil) (others nil) (site-forms nil))
    (unless (and (symbolp name) name) (%program-error "DEFCLASS: invalid class name ~S" name))
    (unless (and (listp supers) (every #'symbolp supers))
      (%program-error "DEFCLASS ~S: invalid superclass list ~S" name supers))
    (unless (listp slots) (%program-error "DEFCLASS ~S: invalid slot list ~S" name slots))
    (dolist (s slots)
      (multiple-value-bind (form readers writers sname) (%defclass-slot name s)
        (when (member sname slot-names) (%program-error "DEFCLASS ~S: duplicate slot name ~S" name sname))
        (push sname slot-names)
        (push form slotd-forms)
        (dolist (r readers)
          ;; (the compiler macro giving the reader's call sites a cache: a toplevel form)
          (setq site-forms (append (%gf-site-macro-forms r '(object)) site-forms))
          ;; (not DEFMETHOD: the generic function needs no dispatch of its
          ;; own, the call sites keep the slot locations)
          (push `(%define-slot-accessor ',r ',name ',sname nil) accessor-forms))
        (dolist (w writers)
          (setq site-forms (append (%gf-site-macro-forms w '(new-value object)) site-forms))
          (push `(%define-slot-accessor ',w ',name ',sname t) accessor-forms))))
    (dolist (o options)
      (unless (and (consp o) (symbolp (car o)))
        (%program-error "DEFCLASS ~S: invalid class option ~S" name o))
      (when (member (car o) seen) (%program-error "DEFCLASS ~S: class option ~S given more than once" name (car o)))
      (push (car o) seen)
      (case (car o)
        (:default-initargs
         (setq default-initargs (cdr o))
         (unless (evenp (length default-initargs))
           (%program-error "DEFCLASS ~S: odd-length :DEFAULT-INITARGS ~S" name o))
         (let ((keys nil))
           (do ((l default-initargs (cddr l))) ((null l))
             (when (member (car l) keys)
               (%program-error "DEFCLASS ~S: duplicate default initarg ~S" name (car l)))
             (push (car l) keys))))
        (:metaclass (setq metaclass (cadr o)))
        (:documentation (setq doc (cadr o)))
        ;; other class options are initialization arguments of the class
        (t (push `',(car o) others) (push `',(cdr o) others))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%put ',name '%defclass t))
       ,@site-forms
       (%ensure-class ',name ',supers (list ,@(reverse slotd-forms))
                      (list ,@(let ((r nil))
                                (do ((l default-initargs (cddr l))) ((null l) (reverse r))
                                  (push `',(car l) r) (push `(lambda () ,(cadr l)) r))))
                      ',metaclass ,doc
                      (list ,@(reverse others))
                      ',(let ((r nil))
                          (do ((l default-initargs (cddr l))) ((null l) (reverse r))
                            (push (list (car l) (cadr l)) r))))
       ,@(reverse accessor-forms)
       (find-class ',name))))

(defun %standard-metaclass-p (meta) (not (null (member meta *standard-metaclasses*))))

(defparameter *standard-slot-keys*
  '(:name :initargs :initform :initfunction :readers :writers :allocation :type :documentation))

(defun %canonical-slot-name (s) (if (consp s) (getf s :name) (%slotd-name s)))

(defun %standard-direct-slotd (class-name spec)
  "A slot definition of a class of a standard metaclass from a canonical slot
specification (or a slot definition)."
  (if (%slotd-p spec)
      spec
      (progn
        (do ((l spec (cddr l))) ((null l))
          (unless (member (car l) *standard-slot-keys*)
            (%program-error "DEFCLASS ~S: unknown slot option ~S" class-name (car l))))
        (%make-slotd (getf spec :name) :initargs (getf spec :initargs) :initform (getf spec :initform)
                                       :initfunction (getf spec :initfunction) :readers (getf spec :readers)
                                       :writers (getf spec :writers)
                                       :allocation (getf spec :allocation :instance)
                                       :type (getf spec :type t) :doc (getf spec :documentation)))))

(defun %mop-direct-slotd (class spec)
  (if (%slotd-p spec)
      spec
      (apply #'make-instance (apply #'direct-slot-definition-class class spec) spec)))

(defun %ensure-class-unlocked (name supers direct-slots default-initargs metaclass &optional doc extra direct-default-initargs)
  (let ((names (mapcar #'%canonical-slot-name direct-slots)))
    (unless (= (length names) (length (remove-duplicates names)))
      (%program-error "DEFCLASS ~S: duplicate slot names" name)))
  (let* ((super-classes (mapcar (lambda (s)
                                  (if (%class-p s) s
                                      (or (find-class s nil)
                                          (%ensure-class-object s (find-class 'forward-referenced-class)
                                                                nil nil :forward))))
                                (or supers
                                    (if (and metaclass (%subclassp (if (%class-p metaclass) metaclass (find-class metaclass))
                                                                   (find-class 'forward-referenced-class)))
                                        nil
                                        '(standard-object)))))
         (meta (cond ((null metaclass) *the-standard-class*)
                     ((%class-p metaclass) metaclass)
                     (t (find-class metaclass))))
         (standard (%standard-metaclass-p meta))
         (kind (cond ((%subclassp meta (find-class 'forward-referenced-class)) :forward)
                     ((some (lambda (c) (eq (%class-kind c) :condition)) super-classes) :condition)
                     ((some (lambda (c) (and (%subclassp c (find-class 'class)) (not (eq (%class-kind c) :forward))))
                            super-classes)
                      :metaclass)
                     (t :standard)))
         (existing (let ((c (find-class name nil))) (and c (eq (%class-name c) name) c))))
    (when (and standard extra)
      (%program-error "DEFCLASS ~S: unknown class option ~S" name (car extra)))
    (let ((class (%ensure-class-object
                  name meta super-classes nil kind
                  (lambda (class)
                    (unless (or standard (eq kind :forward))
                      (dolist (s super-classes)
                        (unless (validate-superclass class s)
                          (error 'simple-error
                                 :format-control "The class ~S is not a valid superclass of ~S (metaclass ~S)"
                                 :format-arguments (list s name (%class-name meta))))))
                    (mapcar (lambda (spec) (if standard (%standard-direct-slotd name spec) (%mop-direct-slotd class spec)))
                            direct-slots)))))
      (%set-class-slot class +class-default-initargs+ default-initargs)
      (%set-class-slot class +class-direct-default-initargs+
                       (let ((r nil) (l default-initargs))
                         (dolist (d direct-default-initargs (nreverse r))
                           (push (list (car d) (cadr d) (cadr (member (car d) l))) r))))
      (%set-class-slot class +class-doc+ doc)
      (unless standard
        (apply (if existing #'reinitialize-instance #'initialize-instance) class
               :name name :direct-superclasses super-classes
               :direct-slots (remove-if #'%slotd-p direct-slots)
               :direct-default-initargs (%class-slot class +class-direct-default-initargs+)
               :documentation doc
               extra))
      class)))


(defun %ensure-class (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'%ensure-class-unlocked args))))

(defun ensure-class-unlocked (name &rest initargs &key direct-superclasses direct-slots metaclass direct-default-initargs
                            documentation &allow-other-keys)
  (let ((extra (let ((r nil))
                 (do ((l initargs (cddr l))) ((null l) (nreverse r))
                   (unless (member (car l) '(:name :direct-superclasses :direct-slots :metaclass
                                             :direct-default-initargs :documentation))
                     (push (car l) r) (push (cadr l) r))))))
    (%ensure-class name direct-superclasses
                   (mapcar (lambda (s) (if (%slotd-p s) s (cons :name (cons (getf s :name) s)))) direct-slots)
                   (let ((r nil))
                     (dolist (d direct-default-initargs (nreverse r))
                       (push (car d) r) (push (or (caddr d) (let ((form (cadr d))) (lambda () (eval form)))) r)))
                   metaclass documentation extra
                   (mapcar (lambda (d) (list (car d) (cadr d))) direct-default-initargs))))

;;; a class finalization invalidates the initarg caches; when it changes the
;;; class precedence list or the slot locations of a class, the caches of the
;;; generic functions too (their keys are the classes)

(defun ensure-class (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'ensure-class-unlocked args))))

(setq *class-finalization-hook*
      (lambda (class changedp)
        (declare (ignore class))
        (when changedp
          (dolist (info *generic-function-infos*) (%gf-reset-cache info)))
        (%clear-initarg-caches)))

;;; ------------------------------------------------------------
;;; slots
;;; ------------------------------------------------------------

(defun %fi-object (x)
  "The instance holding the slots of X (a funcallable instance is a closure
whose name is that instance)."
  (if (functionp x)
      (let ((info (%closure-name x))) (if (%instancep info) info x))
      x))

(defun %protocol-class (object)
  "The class of OBJECT when slot access goes through the generic functions
(SLOT-VALUE-USING-CLASS ...), else NIL."
  (and (%instancep object)
       (let ((c (%instance-layout object)))
         (and (%instancep c) (%class-slot c +class-protocol+) c))))

(defun %find-effective-slotd (class name)
  (dolist (s (%class-effective-slots class) nil)
    (when (eq (%slotd-name s) name) (return s))))

(defun slot-value (object slot-name)
  (setq object (%fi-object object))
  (let ((class (%protocol-class object)))
    (if class
        (let ((sd (%find-effective-slotd class slot-name)))
          (if sd
              (slot-value-using-class class object sd)
              (values (slot-missing class object slot-name 'slot-value))))
        (%slot-value-by-name object slot-name))))

(defun %set-slot-value (object slot-name value)
  (setq object (%fi-object object))
  (let ((class (%protocol-class object)))
    (if class
        (let ((sd (%find-effective-slotd class slot-name)))
          (if sd
              (funcall #'(setf slot-value-using-class) value class object sd)
              (slot-missing class object slot-name 'setf value))
          value)
        (%set-slot-value-by-name object slot-name value))))

(defun slot-boundp (object slot-name)
  (setq object (%fi-object object))
  (let ((class (%protocol-class object)))
    (if class
        (let ((sd (%find-effective-slotd class slot-name)))
          (if sd
              (not (null (slot-boundp-using-class class object sd)))
              (values (slot-missing class object slot-name 'slot-boundp))))
        (let ((loc (if (%instancep object) (%slot-location object slot-name) nil)))
          (cond ((fixnump loc) (not (%unboundp (%instance-gref object loc))))
                ((consp loc) (not (%unboundp (cdr loc))))
                (t (values (%slot-missing object slot-name 'slot-boundp nil))))))))

(defun slot-makunbound (object slot-name)
  (setq object (%fi-object object))
  (let ((class (%protocol-class object)))
    (if class
        (let ((sd (%find-effective-slotd class slot-name)))
          (if sd
              (slot-makunbound-using-class class object sd)
              (slot-missing class object slot-name 'slot-makunbound))
          object)
        (let ((loc (if (%instancep object) (%slot-location object slot-name) nil)))
          (cond ((fixnump loc) (%set-instance-gref object loc (%unbound-marker)))
                ((consp loc) (setf (cdr loc) (%unbound-marker)))
                (t (%slot-missing object slot-name 'slot-makunbound nil)))
          object))))

(defun slot-exists-p (object slot-name)
  (setq object (%fi-object object))
  (and (%instancep object) (not (null (%slot-location object slot-name)))))

;;; Slot access by a constant name: (SLOT-VALUE x 'name), (SETF SLOT-VALUE),
;;; SLOT-BOUNDP (and so WITH-SLOTS) are compiled to code comparing the slot
;;; index of the instance's class with the entries of a cache of the form
;;; (%CACHED-SLOT-ACCESS), #(index location index location index location index location next):
;;; the slot indexes of four classes seen there (the slot index of a class is
;;; replaced when its slot locations change), with the location of the slot
;;; in their instances, and the entry to replace next.
;;; Two more elements hold the entry of a class redefined with other slots
;;; (see %CLASS-REDEFINED-P): its slot index and location, used only for an
;;; instance with as many slots as the class (the number is the third).
(defun %make-slot-cache () (vector 0 0 0 0 0 0 0 0 0 0 0 0))

(defun %slot-cache-miss (object name cache index)
  "The location of the slot NAME in the instance OBJECT, whose class has the
slot index INDEX, when it is a slot of the instance accessed without the slot
access protocol; else NIL."
  (declare (simple-vector cache))
  (flet ((checked (loc count)
           (and (%slot-count-is-p object count) loc)))
    (cond ((%sinstp object) nil)          ; (a structure of field slots: not cached)
          ((eq index (svref cache 4)) (svref cache 5))
          ((eq index (svref cache 6)) (svref cache 7))
          ((eq index (svref cache 9)) (checked (svref cache 10) (svref cache 11)))
          (t (let* ((class (%instance-layout object))
                    (loc (and (%instancep class) (not (%class-slot class +class-protocol+))
                              (cdr (assoc name index)))))
               (cond ((not (fixnump loc)) nil)
                     ((%class-redefined-p class)
                      (let ((count (%class-slot class +class-instance-size+)))
                        (setf (svref cache 9) index (svref cache 10) loc (svref cache 11) count)
                        (checked loc count)))
                     (t (let ((i (svref cache 8)))
                          (setf (svref cache i) index (svref cache (+ i 1)) loc
                                (svref cache 8) (logand (+ i 2) 7))
                          loc))))))))

(declaim (inline %cached-slot-location))
(defun %cached-slot-location (object name cache)
  (let ((index (%class-slot (%instance-layout object) +class-slot-index+)))
    (cond ((eq index (%svref cache 0)) (%svref cache 1))
          ((eq index (%svref cache 2)) (%svref cache 3))
          (t (%slot-cache-miss object name cache index)))))

(defun %slot-value-cached (object name cache)
  (let ((loc (and (%instancep object) (%cached-slot-location object name cache))))
    (if loc
        (let ((v (%instance-ref object loc)))
          (if (%unboundp v) (slot-value object name) v))
        (slot-value object name))))

(defun %set-slot-value-cached (object name value cache)
  (let ((loc (and (%instancep object) (%cached-slot-location object name cache))))
    (if loc
        (%set-instance-ref object loc value)
        (%set-slot-value object name value))))

(defun %slot-boundp-cached (object name cache)
  (let ((loc (and (%instancep object) (%cached-slot-location object name cache))))
    (if loc
        (not (%unboundp (%instance-ref object loc)))
        (slot-boundp object name))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %constant-slot-name-p (form)
    (and (consp form) (eq (car form) 'quote) (consp (cdr form)) (null (cddr form))
         (symbolp (cadr form)))))

(defun %slot-value-slow (object name) (slot-value object name))
(defun %set-slot-value-slow (object name value) (%set-slot-value object name value))
(defun %slot-boundp-slow (object name) (slot-boundp object name))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %cached-slot-access (object name kind &optional value)
    "The inline code of a slot access by the constant NAME (a quoted symbol):
the four entries of the cache are compared at the call site (also where the
call site is compiled apart from the runtime, in the image or a fasl, whose
calls of the runtime's functions are not inlined by the engine)."
    (let ((o (gensym "O")) (c (gensym "C")) (index (gensym "INDEX")) (loc (gensym "LOC"))
          (v (gensym "V")) (new (gensym "NEW")))
      `(let* ((,o ,object) ,@(when (eq kind :set) `((,new ,value)))
              (,c (load-time-value (%make-slot-cache)))
              (,loc (if (%instancep ,o)
                        (let ((,index (%instance-ref (%instance-layout ,o) +class-slot-index+)))
                          (cond ((eq ,index (%svref ,c 0)) (%svref ,c 1))
                                ((eq ,index (%svref ,c 2)) (%svref ,c 3))
                                ((eq ,index (%svref ,c 4)) (%svref ,c 5))
                                ((eq ,index (%svref ,c 6)) (%svref ,c 7))
                                (t (%slot-cache-miss ,o ,name ,c ,index))))
                        nil)))
         ,(ecase kind
            (:read `(if ,loc
                        (let ((,v (%instance-ref ,o ,loc)))
                          (if (%unboundp ,v) (%slot-value-slow ,o ,name) ,v))
                        (%slot-value-slow ,o ,name)))
            (:set `(if ,loc
                       (%set-instance-ref ,o ,loc ,new)
                       (%set-slot-value-slow ,o ,name ,new)))
            (:boundp `(if ,loc
                          (not (%unboundp (%instance-ref ,o ,loc)))
                          (%slot-boundp-slow ,o ,name))))))))

(define-compiler-macro slot-value (&whole form object name)
  (if (%constant-slot-name-p name)
      (%cached-slot-access object name :read)
      form))

(define-compiler-macro %set-slot-value (&whole form object name value)
  (if (%constant-slot-name-p name)
      (%cached-slot-access object name :set value)
      form))

;;; (setf (slot-value x 'name) v): the constant name reaches the compiler macro
;;; (the short DEFSETF form of boot.lisp binds it to a variable)
(defsetf slot-value (object slot-name) (value)
  `(%set-slot-value ,object ,slot-name ,value))

(define-compiler-macro slot-boundp (&whole form object name)
  (if (%constant-slot-name-p name)
      (%cached-slot-access object name :boundp)
      form))

;;; the slot access protocol (AMOP): applies to instances of classes whose
;;; metaclass is not one of the implementation's
(defun %slotd-raw-value (object slotd)
  (when (%instancep object) (%ensure-current-instance object))
  (let ((loc (%slotd-location slotd)))
    (if (fixnump loc) (%instance-ref object loc) (cdr loc))))

(defgeneric slot-value-using-class (class object slotd))
(defmethod slot-value-using-class ((class t) object slotd)
  (let ((v (%slotd-raw-value object slotd)))
    (if (%unboundp v) (values (slot-unbound class object (%slotd-name slotd))) v)))

(defgeneric (setf slot-value-using-class) (new-value class object slotd))
(defmethod (setf slot-value-using-class) (new-value (class t) object slotd)
  (when (%instancep object) (%ensure-current-instance object))
  (let ((loc (%slotd-location slotd)))
    (if (fixnump loc) (%set-instance-ref object loc new-value) (setf (cdr loc) new-value))
    new-value))

(defgeneric slot-boundp-using-class (class object slotd))
(defmethod slot-boundp-using-class ((class t) object slotd)
  (not (%unboundp (%slotd-raw-value object slotd))))

(defgeneric slot-makunbound-using-class (class object slotd))
(defmethod slot-makunbound-using-class ((class t) object slotd)
  (when (%instancep object) (%ensure-current-instance object))
  (let ((loc (%slotd-location slotd)))
    (if (fixnump loc) (%set-instance-ref object loc (%unbound-marker)) (setf (cdr loc) (%unbound-marker)))
    object))

(defun standard-instance-access (instance location) (%instance-ref instance location))
(defun (setf standard-instance-access) (value instance location) (%set-instance-ref instance location value))
(defun funcallable-standard-instance-access (instance location)
  (%instance-ref (if (functionp instance) (%closure-name instance) instance) location))
(defun (setf funcallable-standard-instance-access) (value instance location)
  (%set-instance-ref (if (functionp instance) (%closure-name instance) instance) location value))

(defmacro with-slots (slots instance &body body)
  (let ((g (gensym "INSTANCE")))
    `(let ((,g ,instance))
       (declare (ignorable ,g))
       (symbol-macrolet ,(mapcar (lambda (s)
                                   (let ((var (if (consp s) (car s) s)) (slot (if (consp s) (cadr s) s)))
                                     `(,var (slot-value ,g ',slot))))
                                 slots)
         ,@body))))

(defmacro with-accessors (bindings instance &body body)
  (let ((g (gensym "INSTANCE")))
    `(let ((,g ,instance))
       (declare (ignorable ,g))
       (symbol-macrolet ,(mapcar (lambda (b) `(,(car b) (,(cadr b) ,g))) bindings)
         ,@body))))

;;; ------------------------------------------------------------
;;; standard generic functions
;;; ------------------------------------------------------------

(defgeneric no-applicable-method (generic-function &rest function-arguments))
(defmethod no-applicable-method ((gf t) &rest args)
  (error 'simple-error
         :format-control "There is no applicable method for the generic function ~S when called with arguments ~S."
         :format-arguments (list gf args)))

(defgeneric no-next-method (generic-function method &rest args))
(defmethod no-next-method ((gf t) (method t) &rest args)
  (error 'simple-error :format-control "There is no next method for ~S (arguments ~S)."
                       :format-arguments (list method args)))

(defgeneric slot-missing (class object slot-name operation &optional new-value))
(defmethod slot-missing ((class t) object slot-name operation &optional new-value)
  (declare (ignore new-value))
  (error 'simple-error :format-control "The slot ~S is missing from ~S (operation ~S)."
                       :format-arguments (list slot-name object operation)))

(defgeneric slot-unbound (class instance slot-name))
(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :name slot-name :instance instance))

(defgeneric add-method (generic-function method))
(defmethod add-method ((gf generic-function) (method method)) (%add-method gf method))
(defgeneric remove-method (generic-function method))
(defmethod remove-method ((gf generic-function) (method method)) (%remove-method gf method))
(defgeneric find-method (generic-function qualifiers specializers &optional errorp))
(defmethod find-method ((gf generic-function) qualifiers specializers &optional (errorp t))
  (%find-method gf qualifiers specializers errorp))
(defgeneric compute-applicable-methods (generic-function function-arguments))
(defmethod compute-applicable-methods ((gf generic-function) args)
  (%compute-applicable-methods gf args))
(defgeneric method-qualifiers (method))
(defmethod method-qualifiers ((m method)) (%method-qualifiers m))

(defgeneric class-name (class))
(defmethod class-name ((class class)) (%class-name class))
(defgeneric (setf class-name) (new-value class))
(defmethod (setf class-name) (new-value (class class))
  (%set-class-slot class +class-name+ new-value)
  new-value)

;;; ------------------------------------------------------------
;;; instance creation and initialization (CLHS 7.1)
;;; ------------------------------------------------------------

(defun %check-plist (initargs who)
  (do ((l initargs (cddr l))) ((null l))
    (unless (and (consp l) (consp (cdr l)))
      (%program-error "~S: odd-length initialization argument list ~S" who initargs))
    (unless (symbolp (car l))
      (%program-error "~S: invalid initialization argument name ~S" who (car l)))))

(defun %class-prototype (class)
  (or (%class-slot class +class-prototype+)
      (let ((p (%allocate-instance class)))
        (%set-class-slot class +class-prototype+ p)
        p)))

(setq *initarg-cache* (make-hash-table :test 'equal))

(defun %valid-initargs (class op calls)
  "CALLS is a list of (generic-function . arguments) whose applicable methods'
keyword parameters are valid initialization arguments."
  (let ((key (cons class op)))
    (multiple-value-bind (v found) (gethash key *initarg-cache*)
      (if found v
          (setf (gethash key *initarg-cache*)
                (let ((r nil))
                  (dolist (sd (%class-effective-slots class))
                    (dolist (i (%slotd-initargs sd)) (pushnew i r)))
                  (dolist (c calls r)
                    (let ((k (%valid-call-keys (%gf-info (car c)) (%compute-applicable-methods (car c) (cdr c)))))
                      (when (eq k :any) (return :any))
                      (dolist (x k) (pushnew x r))))))))))

(defun %check-initargs (class op initargs calls)
  (let ((valid (%valid-initargs class op calls)))
    (unless (or (eq valid :any)
                (let ((aok (%plist-find initargs :allow-other-keys))) (and aok (cadr aok))))
      (do ((l initargs (cddr l))) ((null l))
        (unless (or (eq (car l) :allow-other-keys) (member (car l) valid))
          (%program-error "~S: invalid initialization argument ~S for ~S" op (car l) class))))))

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))
(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-instance class))
(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-instance class))
(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-instance class))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))

;;; What MAKE-INSTANCE of a class does is computed once per class and kept in
;;; the class (+CLASS-MAKE-INFO+) while *CTOR-GENERATION* stays the same (it
;;; changes when a method is added to or removed from any generic function
;;; and when a class is finalized):
;;;   #(generation valid-initargs defaults allocate initialize shared standard-make)
;;; VALID-INITARGS: a list of keys or :ANY.  DEFAULTS: ((key . function) ...),
;;; the default initargs.  ALLOCATE: true when ALLOCATE-INSTANCE must be
;;; called (NIL: only the standard method applies).  INITIALIZE, SHARED: NIL
;;; when only the standard method of INITIALIZE-INSTANCE (SHARED-INITIALIZE)
;;; applies, else the effective method (a function of the argument list), or
;;; T when the generic function must be called.  STANDARD-MAKE: true when
;;; only the standard method of MAKE-INSTANCE applies to the class.
(defvar *standard-make-instance-method* nil)
(defvar *standard-creation-methods* nil)

(defun %only-standard-methods-p (gf args)
  (let ((ms (%compute-applicable-methods gf args)))
    (and ms (null (cdr ms)) (member (car ms) *standard-creation-methods*))))

(defun %creation-emf (gf args)
  "The effective method of GF for ARGS (a function of the argument list), or T
when it depends on more than the classes of ARGS."
  (let ((info (%gf-info gf)))
    (if (or (%gf-slot info +gf-eql+)
            (not (eq (%instance-layout info) *the-standard-generic-function-class*)))
        t
        (let ((entry (%gf-lookup info args (%gf-slot info +gf-nreq+) 0))
              (shape (%gf-shape info)))
          (cond ((consp entry) (car entry))
                ;; a function of the spread arguments: of the instance (and
                ;; the slot names) and the list of the initargs
                ((and (functionp entry) (equal shape (if (eq gf #'initialize-instance) '(1 0 1) '(2 0 1))))
                 (list entry))
                (t t))))))

(defun %compute-make-info (class)
  (when (eq (%class-kind class) :forward)
    (error 'simple-error :format-control "Cannot instantiate the forward-referenced class ~S"
                         :format-arguments (list (%class-name class))))
  (unless (%class-slot class +class-finalized+)
    (finalize-inheritance class))
  (let* ((generation *ctor-generation*)
         (proto (%class-prototype class))
         (plain (not (%class-slot class +class-protocol+)))
         (mi (vector generation
                     (%valid-initargs class 'make-instance
                                      (list (list #'allocate-instance class)
                                            (list #'initialize-instance proto)
                                            (list #'shared-initialize proto t)))
                     (%class-default-initargs-list class)
                     (not (and plain (%only-standard-methods-p #'allocate-instance (list class))))
                     (if (%only-standard-methods-p #'initialize-instance (list proto))
                         nil
                         (%creation-emf #'initialize-instance (list proto)))
                     (if (and plain (%only-standard-methods-p #'shared-initialize (list proto t)))
                         nil
                         (%creation-emf #'shared-initialize (list proto t)))
                     (let ((ms (%compute-applicable-methods #'make-instance (list class))))
                       (and ms (null (cdr ms)) (eq (car ms) *standard-make-instance-method*))))))
    (when (eql generation *ctor-generation*)
      (%set-class-slot class +class-make-info+ mi))
    mi))

(defmacro %class-make-info (class)
  `(let* ((class ,class) (mi (%class-slot class +class-make-info+)))
     (if (and mi (eq (%svref mi 0) *ctor-generation*)) mi (%compute-make-info class))))

(defun %check-initarg-keys (class valid initargs)
  (unless (or (eq valid :any)
              (let ((aok (%plist-find initargs :allow-other-keys))) (and aok (cadr aok))))
    (do ((l initargs (cddr l))) ((null l))
      (let ((key (car l)))
        (unless (or (eq key :allow-other-keys)
                    (dolist (v valid nil) (when (eq v key) (return t))))
          (%program-error "~S: invalid initialization argument ~S for ~S" 'make-instance key class))))))

(defun %add-defaults (defaults initargs)
  "INITARGS followed by the default initargs DEFAULTS ((key . function) ...)
whose key is not supplied."
  (let ((extra nil))
    (dolist (d defaults)
      (unless (do ((l initargs (cddr l))) ((atom l) nil) (when (eq (car l) (car d)) (return t)))
        (push (car d) extra)
        (push (funcall (cdr d)) extra)))
    (if extra (append initargs (nreverse extra)) initargs)))

(defun %run-shared-initialize (instance class mi initargs)
  "(apply #'shared-initialize INSTANCE t INITARGS) for an instance of CLASS,
whose make-info is MI."
  (let ((shared (svref mi 5)))
    (cond ((null shared) (%std-shared-initialize instance class t initargs))
          ((functionp shared) (funcall shared (cons instance (cons t initargs))))
          ((consp shared) (funcall (the function (car shared)) instance t initargs))
          (t (apply #'shared-initialize instance t initargs)))))

(defun %make-instance-of (class initargs &optional checked)
  "CHECKED: INITARGS is a list of valid initargs (a constructor has checked them)."
  (let ((mi (%class-make-info class)))
    (unless checked (%check-plist initargs 'make-instance))
    (let* ((defaults (svref mi 2))
           (initargs (if defaults (%add-defaults defaults initargs) initargs)))
      (unless checked (%check-initarg-keys class (svref mi 1) initargs))
      (let ((instance (if (svref mi 3) (apply #'allocate-instance class initargs) (%allocate-instance class)))
            (init (svref mi 4)))
        (cond ((null init)
               (if (%instancep instance)
                   (%run-shared-initialize instance class mi initargs)
                   (apply #'shared-initialize instance t initargs)))
              ((functionp init) (funcall init (cons instance initargs)))
              ((consp init) (funcall (the function (car init)) instance initargs))
              (t (apply #'initialize-instance instance initargs)))
        instance))))

(defmethod make-instance ((class symbol) &rest initargs)
  (let ((c (find-class class)))
    (if (and (eq (%class-kind c) :standard) (svref (%class-make-info c) 6))
        (%make-instance-of c initargs)
        (apply #'make-instance c initargs))))

(defmethod make-instance ((class standard-class) &rest initargs) (%make-instance-of class initargs))
(setq *standard-make-instance-method* (car (%gf-slot (%gf-info #'make-instance) +gf-methods+)))
(defmethod make-instance ((class funcallable-standard-class) &rest initargs) (%make-instance-of class initargs))

(defgeneric initialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  ;; (apply #'shared-initialize instance t initargs)
  (if (%instancep instance)
      (let ((class (%instance-layout instance)))
        (%check-plist initargs 'shared-initialize)
        (%run-shared-initialize instance class (%class-make-info class) initargs)
        instance)
      (apply #'shared-initialize instance t initargs)))

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (%check-plist initargs 'reinitialize-instance)
  (%check-initargs (class-of instance) 'reinitialize-instance initargs
                   (list (list #'reinitialize-instance instance)
                         (list #'shared-initialize instance nil)))
  (apply #'shared-initialize instance nil initargs))

(defgeneric shared-initialize (instance slot-names &rest initargs &key &allow-other-keys))
(defun %protocol-shared-initialize (instance class slot-names initargs)
  (dolist (sd (%class-effective-slots class))
    (let ((found nil) (value nil))
      (do ((l initargs (cddr l))) ((or found (atom l)))
        (when (member (car l) (%slotd-initargs sd))
          (setq found t value (cadr l))))
      (cond (found (funcall #'(setf slot-value-using-class) value class instance sd))
            ((and (%slotd-initfunction sd)
                  (or (eq slot-names t) (member (%slotd-name sd) slot-names))
                  (not (slot-boundp-using-class class instance sd)))
             (funcall #'(setf slot-value-using-class) (funcall (%slotd-initfunction sd)) class instance sd)))))
  instance)

(defmethod shared-initialize ((original standard-object) slot-names &rest initargs)
  (%check-plist initargs 'shared-initialize)
  (let ((class (class-of original)) (instance (%fi-object original)))
    (when (%class-slot class +class-protocol+)
      (%protocol-shared-initialize instance class slot-names initargs)
      (return-from shared-initialize original))
    (%std-shared-initialize instance class slot-names initargs)
    original))

(defun %std-shared-initialize (instance class slot-names initargs)
  (%ensure-current-instance instance)
  (dolist (sd (%class-effective-slots class))
    (let ((loc (%slotd-location sd)) (keys (%slotd-initargs sd)) (found nil) (value nil))
      (when keys
        (if (cdr keys)
            (do ((l initargs (cddr l))) ((atom l))
              (when (let ((key (car l))) (dolist (k keys nil) (when (eq k key) (return t))))
                (setq found t value (cadr l))
                (return)))
            (let ((k (car keys)))
              (do ((l initargs (cddr l))) ((atom l))
                (when (eq (car l) k)
                  (setq found t value (cadr l))
                  (return))))))
      (cond (found
             (if (fixnump loc) (%set-instance-ref instance loc value) (setf (cdr loc) value)))
            ((and (%slotd-initfunction sd)
                  (or (eq slot-names t) (member (%slotd-name sd) slot-names))
                  (if (fixnump loc) (%unboundp (%instance-ref instance loc)) (%unboundp (cdr loc))))
             (let ((v (funcall (%slotd-initfunction sd))))
               (if (fixnump loc) (%set-instance-ref instance loc v) (setf (cdr loc) v)))))))
  instance)

(setq *standard-creation-methods*
      (append (%gf-slot (%gf-info #'allocate-instance) +gf-methods+)
              (%gf-slot (%gf-info #'initialize-instance) +gf-methods+)
              (%gf-slot (%gf-info #'shared-initialize) +gf-methods+)))

(defgeneric update-instance-for-different-class (previous current &rest initargs &key &allow-other-keys))
(defmethod update-instance-for-different-class ((previous standard-object) (current standard-object)
                                                &rest initargs)
  (%check-plist initargs 'update-instance-for-different-class)
  (let ((added nil))
    (dolist (sd (%class-effective-slots (class-of current)))
      (when (and (fixnump (%slotd-location sd))
                 (not (%slot-location previous (%slotd-name sd))))
        (push (%slotd-name sd) added)))
    (%check-initargs (class-of current) (cons 'change-class (class-of previous)) initargs
                     (list (list #'update-instance-for-different-class previous current)
                           (list #'shared-initialize current added)))
    (apply #'shared-initialize current added initargs)))

(defgeneric change-class (instance new-class &rest initargs &key &allow-other-keys))
(defmethod change-class ((instance t) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))
(defmethod change-class ((instance standard-object) (new-class standard-class) &rest initargs)
  (let* ((old-class (class-of instance))
         (copy (%make-instance old-class (copy-seq (%instance-slots instance))))
         (new (%allocate-instance new-class)))
    (dolist (sd (%class-effective-slots new-class))
      (let ((loc (%slotd-location sd)))
        (when (fixnump loc)
          (let ((old-loc (%slot-location copy (%slotd-name sd))))
            (cond ((fixnump old-loc) (%set-instance-ref new loc (%instance-ref copy old-loc)))
                  ((consp old-loc) (%set-instance-ref new loc (cdr old-loc))))))))
    (%set-instance-layout instance new-class)
    (%set-instance-slots instance (%instance-slots new))
    (apply #'update-instance-for-different-class copy instance initargs)
    instance))

(defgeneric update-instance-for-redefined-class (instance added-slots discarded-slots property-list
                                                 &rest initargs &key &allow-other-keys))
(defmethod update-instance-for-redefined-class ((instance standard-object) added discarded plist &rest initargs)
  (declare (ignore discarded plist))
  (apply #'shared-initialize instance added initargs))

(defgeneric make-instances-obsolete (class))
(defmethod make-instances-obsolete ((class standard-class)) class)
(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

;;; ------------------------------------------------------------
;;; printing, load forms
;;; ------------------------------------------------------------

(defgeneric print-object (object stream))
(defmethod print-object ((object t) stream)
  (%default-print-instance object stream)
  object)

(defgeneric make-load-form (object &optional environment))
(defmethod make-load-form ((object standard-object) &optional environment)
  (declare (ignore environment))
  (error 'simple-error :format-control "No MAKE-LOAD-FORM method is defined for ~S"
                       :format-arguments (list object)))
(defmethod make-load-form ((object structure-object) &optional environment)
  (declare (ignore environment))
  (error 'simple-error :format-control "No MAKE-LOAD-FORM method is defined for ~S"
                       :format-arguments (list object)))
(defmethod make-load-form ((object condition) &optional environment)
  (declare (ignore environment))
  (error 'simple-error :format-control "No MAKE-LOAD-FORM method is defined for ~S"
                       :format-arguments (list object)))
(defmethod make-load-form ((object class) &optional environment)
  (declare (ignore environment))
  (let ((name (%class-name object)))
    (if (and name (eq (find-class name nil) object))
        `(find-class ',name)
        (error 'simple-error :format-control "The class ~S has no proper name" :format-arguments (list object)))))

;;; A literal hash table (e.g. #.(make-table) in a source file) is made
;;; again with its test and filled with its entries, which are externalized
;;; like any other literal objects.
(defmethod make-load-form ((object hash-table) &optional environment)
  (declare (ignore environment))
  (values `(make-hash-table :test ',(hash-table-test object) :size ,(max 1 (hash-table-count object)))
          `(%fill-hash-table ',object
                             ',(let ((entries nil))
                                 (maphash (lambda (k v) (push v entries) (push k entries)) object)
                                 entries))))

(defun %fill-hash-table (table entries)
  (loop (when (null entries) (return table))
        (setf (gethash (car entries) table) (cadr entries))
        (setq entries (cddr entries))))

(defun make-load-form-saving-slots (object &key (slot-names nil slot-names-p) environment)
  (declare (ignore environment))
  (let* ((class (class-of object))
         (names (if slot-names-p slot-names
                    (mapcar #'%slotd-name (remove-if-not (lambda (s) (fixnump (%slotd-location s)))
                                                         (%class-effective-slots class))))))
    (values `(allocate-instance ',class)
            `(progn
               ,@(mapcar (lambda (n)
                           (if (slot-boundp object n)
                               `(setf (slot-value ',object ',n) ',(slot-value object n))
                               `(slot-makunbound ',object ',n)))
                         names)))))

;;; the printer calls PRINT-OBJECT for instances
(setq *print-object-hook*
      (lambda (x stream)
        (print-object x stream)))

;;; used by SUBTYPEP (defined before HANDLER-CASE exists)
(defun %safe-typep (x type) (handler-case (typep x type) (error () nil)))

;;; ------------------------------------------------------------
;;; documentation
;;; ------------------------------------------------------------

;;; DOCUMENTATION itself is defined in misc.lisp
(defun %set-doc (x doc-type value)
  (funcall (fdefinition '(setf documentation)) value x doc-type))

;;; ------------------------------------------------------------
;;; introspection (subset of the MOP)
;;; ------------------------------------------------------------

;;; the class definition protocol (AMOP), used for classes whose metaclass
;;; is not one of the implementation's
(defgeneric validate-superclass (class superclass))
(defmethod validate-superclass ((class class) (superclass class))
  (let ((m1 (class-of class)) (m2 (class-of superclass)) (std (find-class 'standard-class))
        (fsc (find-class 'funcallable-standard-class)))
    (or (eq superclass (find-class t))
        (eq m1 m2)
        (and (eq m1 std) (eq m2 fsc))
        (and (eq m1 fsc) (eq m2 std))
        (%subclassp m2 (find-class 'forward-referenced-class))
        ;; a subclass of a standard metaclass, as the standard metaclass
        (and (%subclassp m1 std) (eq m2 std))
        (and (%subclassp m1 fsc) (or (eq m2 fsc) (eq m2 std))))))

(defgeneric direct-slot-definition-class (class &rest initargs))
(defmethod direct-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  *the-direct-slotd-class*)

(defgeneric effective-slot-definition-class (class &rest initargs))
(defmethod effective-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  *the-effective-slotd-class*)

(defgeneric compute-effective-slot-definition (class name direct-slot-definitions))
(defmethod compute-effective-slot-definition ((class class) name direct-slotds)
  (let* ((v (%effective-slotd-values (mapcar (lambda (d) (cons nil d)) direct-slotds)))
         (initargs (list :name name :initargs (svref v 1) :initform (svref v 2) :initfunction (svref v 3)
                         :allocation (svref v 6) :type (svref v 7) :documentation (svref v 8))))
    (apply #'make-instance (apply #'effective-slot-definition-class class initargs) initargs)))

(defgeneric compute-slots (class))
(defmethod compute-slots ((class class))
  (let ((cpl (%class-cpl class)))
    (mapcar (lambda (name)
              (compute-effective-slot-definition class name (mapcar #'cdr (%direct-slotds-named cpl name))))
            (%effective-slot-names cpl))))

;;; the locations are assigned to the slots that COMPUTE-SLOTS returns (as
;;; by an :AROUND method in other implementations: methods may add slots)
(setq *compute-slots-hook* (lambda (class) (%assign-slot-locations class (compute-slots class))))

;;; class metaobjects accept the class definition initialization arguments
(defmethod shared-initialize ((class class) slot-names &rest initargs
                              &key name direct-superclasses direct-slots direct-default-initargs documentation
                              &allow-other-keys)
  (declare (ignore name direct-superclasses direct-slots direct-default-initargs documentation))
  (apply #'call-next-method class slot-names initargs))

(defun ensure-class-using-class (class name &rest initargs)
  (declare (ignore class))
  (apply #'ensure-class name initargs))

(defun compute-class-precedence-list (class) (%compute-cpl class (%class-slot class +class-supers+)))

;;; generic functions
(defgeneric compute-applicable-methods-using-classes (generic-function classes))
(defmethod compute-applicable-methods-using-classes ((gf generic-function) classes)
  (let* ((info (%gf-info gf)) (methods (%gf-slot info +gf-methods+)))
    (if (some (lambda (m) (some #'consp (%method-specializers m))) methods)
        (values nil nil)
        (let ((ms (remove-if-not (lambda (m)
                                   (do ((specs (%method-specializers m) (cdr specs)) (c classes (cdr c)))
                                       ((null specs) t)
                                     (unless (%subclassp (car c) (car specs)) (return nil))))
                                 methods))
              (apo (%gf-slot info +gf-apo+)))
          (values (stable-sort (copy-list ms)
                               (lambda (m1 m2)
                                 (let ((s1 (%method-specializers m1)) (s2 (%method-specializers m2)))
                                   (dolist (i (or apo (let ((r nil)) (dotimes (i (length classes) (nreverse r)) (push i r))))
                                              nil)
                                     (let ((a (nth i s1)) (b (nth i s2)) (cpl (%class-cpl (nth i classes))))
                                       (unless (eq a b)
                                         (return (< (or (position a cpl) 1000) (or (position b cpl) 1000)))))))))
                  t)))))

(setq *the-standard-generic-function-class* (find-class 'standard-generic-function))

(defun class-direct-superclasses (c) (%class-slot c +class-supers+))
(defun class-direct-subclasses (c) (%class-slot c +class-subclasses+))
(defun class-precedence-list (c) (%class-cpl c))
(defun class-slots (c) (%class-effective-slots c))
(defun class-direct-slots (c) (%class-slot c +class-direct-slots+))
(defun class-finalized-p (c) (%class-slot c +class-finalized+))
(defun finalize-inheritance-unlocked (c)
  (let ((fwd (find-if (lambda (k) (eq (%class-kind k) :forward)) (%compute-cpl c (%class-slot c +class-supers+)))))
    (when fwd
      (error 'simple-error :format-control "Cannot finalize ~S: its superclass ~S is not defined"
                           :format-arguments (list c (%class-name fwd))))
    (%finalize-class c)))

(defun finalize-inheritance (&rest args)
  ;; the class system is shared by the threads (runtime/thread.lisp)
  (%call-with-world-lock (lambda () (apply #'finalize-inheritance-unlocked args))))

(defun class-direct-default-initargs (c) (%class-slot c +class-direct-default-initargs+))
(defun class-default-initargs (c)
  (let ((r nil))
    (dolist (k (%class-cpl c) (nreverse r))
      (dolist (d (%class-slot k +class-direct-default-initargs+))
        (unless (assoc (car d) r) (push d r))))))
(defun classp (x) (%class-p x))
(defun slot-definition-documentation (s) (%slotd-doc s))
(defun slot-definition-name (s) (%slotd-name s))
(defun slot-definition-initargs (s) (%slotd-initargs s))
(defun slot-definition-initform (s) (%slotd-initform s))
(defun slot-definition-initfunction (s) (%slotd-initfunction s))
(defun slot-definition-allocation (s) (%slotd-allocation s))
(defun slot-definition-readers (s) (%slotd-readers s))
(defun slot-definition-writers (s) (%slotd-writers s))
(defun slot-definition-type (s) (%slotd-type s))
(defun slot-definition-location (s) (%slotd-location s))
(defun generic-function-name (gf) (%gf-slot (%gf-info gf) +gf-name+))
(defun generic-function-methods (gf) (%gf-slot (%gf-info gf) +gf-methods+))
(defun generic-function-lambda-list (gf)
  (let ((ll (%gf-slot (%gf-info gf) +gf-ll+))) (if (eq ll :unspecified) nil ll)))
(defun generic-function-method-combination (gf) (%gf-slot (%gf-info gf) +gf-combination+))
(defun generic-function-method-class (gf) (%gf-slot (%gf-info gf) +gf-method-class+))
(defun generic-function-declarations (gf) (%gf-slot (%gf-info gf) +gf-declarations+))
(defun generic-function-argument-precedence-order (gf)
  (let* ((info (%gf-info gf)) (ll (%gf-slot info +gf-ll+))
         (req (subseq ll 0 (%gf-slot info +gf-nreq+))))
    (if (%gf-slot info +gf-apo+) (mapcar (lambda (i) (nth i req)) (%gf-slot info +gf-apo+)) req)))
(defun method-specializers (m) (%method-specializers m))
(defun method-function (m) (%method-function m))
(defun method-lambda-list (m) (%instance-ref m 4))
(defun method-generic-function (m) (%method-gf m))
(defun class-prototype (c)
  (unless (%class-slot c +class-finalized+)
    (error 'simple-error :format-control "The class ~S is not finalized" :format-arguments (list c)))
  (%class-prototype c))
(defun eql-specializer-object (s) (cadr s))
(defun intern-eql-specializer (x) (list 'eql x))

(defmethod print-object ((c class) stream)
  (print-unreadable-object (c stream :type t :identity (null (%class-name c)))
    (prin1 (%class-name c) stream))
  c)
(defmethod print-object ((m method) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~S ~{~S ~}~S"
            (let ((gf (%method-gf m))) (if gf (generic-function-name gf) nil))
            (%method-qualifiers m)
            (mapcar (lambda (s) (if (consp s) s (%class-name s))) (%method-specializers m))))
  m)
(defmethod print-object ((mc method-combination) stream)
  (print-unreadable-object (mc stream :type t)
    (prin1 (%instance-ref mc 0) stream))
  mc)

;;; ------------------------------------------------------------
;;; file compilation: literal objects are externalized with MAKE-LOAD-FORM
;;; ------------------------------------------------------------
;;; The output file is Lisp source.  An externalized object is created by a
;;; form (%FASL-SET id index creation-form) and referenced as #.(%FASL-REF id index).

(defvar *fasl-objects* (make-hash-table :test 'equal))

(defun %fasl-set (id index value)
  (let ((table (or (gethash id *fasl-objects*)
                   (setf (gethash id *fasl-objects*) (make-hash-table)))))
    (setf (gethash index table) value)))

(defun %fasl-ref (id index)
  (values (gethash index (gethash id *fasl-objects*))))

(defstruct (%fasl-marker (:constructor %make-fasl-marker (id index))) id index)
(defmethod print-object ((m %fasl-marker) stream)
  (format stream "#.(~S ~S ~S)" '%fasl-ref (%fasl-marker-id m) (%fasl-marker-index m))
  m)

(defun %externalizable-p (x)
  (and (%instancep x)
       (not (pathnamep x))
       (not (%fasl-marker-p x))
       (or (typep x 'standard-object) (typep x 'structure-object) (hash-table-p x))))

(defun %literal-objects (form exclude)
  "The externalizable objects in FORM, in order of appearance."
  (let ((seen (make-hash-table :test 'eq)) (r nil))
    (labels ((walk (x)
               (cond ((%externalizable-p x)
                      (unless (or (eq x exclude) (member x r)) (push x r)))
                     ((consp x)
                      (unless (gethash x seen)
                        (setf (gethash x seen) t)
                        (walk (car x)) (walk (cdr x))))
                     ((and (simple-vector-p x) (not (gethash x seen)))
                      (setf (gethash x seen) t)
                      (dotimes (i (length x)) (walk (svref x i)))))))
      (walk form))
    (nreverse r)))

(defun %make-externalizer (id)
  "A function from a top-level form to the list of forms to write in its place:
creation and initialization forms of the literal objects it references (CLHS
MAKE-LOAD-FORM ordering), then the form, objects replaced by references."
  (let ((out nil) (state (make-hash-table :test 'eq)) (index (make-hash-table :test 'eq))
        (counter 0) (pending nil))
    (labels ((replace-objects (form)
               (let ((memo (make-hash-table :test 'eq)))
                 (labels ((copy (x)
                            (cond ((%externalizable-p x)
                                   (%make-fasl-marker id (or (gethash x index)
                                                             (error "Unexternalized object ~S" x))))
                                  ((consp x)
                                   (or (gethash x memo)
                                       (let ((c (cons nil nil)))
                                         (setf (gethash x memo) c)
                                         (setf (car c) (copy (car x)) (cdr c) (copy (cdr x)))
                                         c)))
                                  ((simple-vector-p x)
                                   (or (gethash x memo)
                                       (let ((v (make-array (length x))))
                                         (setf (gethash x memo) v)
                                         (dotimes (i (length x) v) (setf (svref v i) (copy (svref x i)))))))
                                  (t x))))
                   (copy form))))
             (emit (form) (push (replace-objects form) out))
             (createdp (x) (member (gethash x state) '(:created :done)))
             (flush ()
               ;; deferred initialization forms whose objects now all exist
               (loop
                 (let ((ready (find-if (lambda (p) (every #'createdp (cdr (cdr p)))) pending)))
                   (unless ready (return))
                   (setq pending (remove ready pending))
                   (emit (cadr ready)))))
             (externalize (x)
               (unless (gethash x state)
                 (setf (gethash x state) :creating)
                 (multiple-value-bind (creation init) (make-load-form x)
                   (dolist (y (%literal-objects creation x)) (externalize y))
                   (setf (gethash x index) counter)
                   (setq counter (+ counter 1))
                   (emit `(%fasl-set ,id ,(gethash x index) ,creation))
                   (setf (gethash x state) :created)
                   (when init
                     (let ((deps (%literal-objects init x)))
                       (dolist (y deps) (externalize y))
                       (if (every #'createdp deps)
                           (emit init)
                           ;; most recently deferred first
                           (push (list* x init deps) pending))))
                   (setf (gethash x state) :done)
                   (flush)))))
      (lambda (form)
        (setq out nil)
        (dolist (y (%literal-objects form nil)) (externalize y))
        (dolist (p pending) (emit (cadr p)))
        (setq pending nil)
        (emit form)
        (nreverse out)))))

(defvar *compile-file-counter* 0)

(defun %new-externalizer (input)
  (%make-externalizer (format nil "~A-~D-~D" (namestring input) (get-universal-time)
                              (setq *compile-file-counter* (+ *compile-file-counter* 1)))))

;;; slot readers and writers of conditions defined from now on are generic
;;; functions (CLHS DEFINE-CONDITION)
(setq *condition-accessor-hook*
      (lambda (name class fn writerp &optional slot-name)
        (when (and (fboundp name) (not (%generic-function-p (fdefinition name))))
          (fmakunbound name))
        (let ((m (%note-method-fast-function
                  (%defmethod name nil (if writerp (list t class) (list class))
                              (if writerp '(new-value condition) '(condition))
                              (lambda (args next) (declare (ignore next)) (apply fn args))
                              nil nil)
                  fn)))
          ;; (the effective method of the reader alone reads the slot itself)
          (when slot-name (%note-slot-accessor m slot-name writerp))
          m)))

;;; ------------------------------------------------------------
;;; constructors: (MAKE-INSTANCE 'NAME :KEY value ...) with constant initarg
;;; keys compiles to a call of a per-site constructor, which precomputes the
;;; slot locations while only the standard methods apply.
;;; A ctor is #(generation name keys class plan default-initarg-functions
;;; standard-class valid template arg-plan initform-plan default-plan);
;;; PLAN is a list of (location . n) for slots taking the n-th argument,
;;; (location . (i)) for the i-th default initarg, (location . function) for
;;; slots taking their initform.  STANDARD-CLASS is the class when only the
;;; standard method of MAKE-INSTANCE applies to it (then %MAKE-INSTANCE-OF is
;;; called without going through the generic function); VALID is true when
;;; the keys and the default initargs are then valid initargs.
;;; The new instance's slots are a copy of TEMPLATE, which holds the values of
;;; the constant initforms (and the unbound marker); then the slots get the
;;; values of the default initargs (DEFAULT-PLAN: #(function locations ...),
;;; in order), of the arguments (ARG-PLAN: #(n location ...)) and of the other
;;; initforms (INITFORM-PLAN: #(location function ...), in slot order).
;;; ------------------------------------------------------------

(defun %make-ctor (name keys) (vector -1 name keys nil nil nil nil nil nil nil nil nil))

(defun %constant-initform-p (form)
  "True when the initform FORM always has the same value (which an
initfunction returning it would always return)."
  (or (and (atom form) (or (not (symbolp form)) (null form) (eq form t) (keywordp form)))
      (and (consp form) (eq (car form) 'quote) (consp (cdr form)) (null (cddr form)))))

(defun %ctor-plans (ctor class plan defaults)
  "Set the template and the plans of CTOR from its PLAN."
  (let ((template (%make-simple-vector (or (%class-slot class +class-instance-size+) (%instance-slot-count class))
                                       (%unbound-marker)))
        (args nil) (forms nil) (dplan nil)
        (slotds (%class-effective-slots class)))
    (dotimes (i (length defaults))
      (let ((locs nil))
        (dolist (p plan) (when (and (consp (cdr p)) (eql (cadr p) i)) (push (car p) locs)))
        (push (nth i defaults) dplan) (push locs dplan)))
    (dolist (p plan)
      (let ((loc (car p)) (src (cdr p)))
        (cond ((fixnump src) (push src args) (push loc args))
              ((consp src))
              (t (let ((sd (find loc slotds :key #'%slotd-location)))
                   (if (and sd (eq (%slotd-initfunction sd) src) (%constant-initform-p (%slotd-initform sd)))
                       (setf (svref template loc) (funcall src))
                       (progn (push loc forms) (push src forms))))))))
    (setf (svref ctor 8) template
          (svref ctor 9) (coerce (nreverse args) 'simple-vector)
          (svref ctor 10) (coerce (nreverse forms) 'simple-vector)
          (svref ctor 11) (coerce (nreverse dplan) 'simple-vector))))

(defun %ctor-update (ctor)
  "Recompute CTOR's plan; NIL when the fast path does not apply."
  (let* ((name (svref ctor 1)) (keys (svref ctor 2))
         (class (find-class name nil))
         (mi (and class (%instancep class) (eq (%class-kind class) :standard)
                  (%class-make-info class)))
         ;; the valid initargs when only the standard methods apply
         (fast (and mi (svref mi 6) (not (svref mi 3)) (null (svref mi 4)) (null (svref mi 5))
                    (svref mi 1))))
    (setf (svref ctor 0) *ctor-generation*)
    (setf (svref ctor 6) (if (and mi (svref mi 6)) class nil))
    (setf (svref ctor 7)
          (and mi (not (member :allow-other-keys keys))
               (let ((valid (svref mi 1)))
                 (or (eq valid :any)
                     (and (every (lambda (k) (member k valid)) keys)
                          (every (lambda (d) (member (car d) valid)) (svref mi 2)))))))
    (if (or (null fast) (member :allow-other-keys keys)
            (and (not (eq fast :any)) (notevery (lambda (k) (member k fast)) keys)))
        (setf (svref ctor 3) nil)
        (let* ((defaults (remove-if (lambda (d) (member (car d) keys)) (%class-default-initargs-list class)))
               (plan nil))
          (dolist (sd (%class-effective-slots class))
            (let* ((loc (%slotd-location sd)) (initargs (%slotd-initargs sd))
                   (n (position-if (lambda (k) (member k initargs)) keys))
                   (d (and (null n) (position-if (lambda (d) (member (car d) initargs)) defaults))))
              (cond ((not (fixnump loc)) (setq plan :slow) (return))
                    (n (push (cons loc n) plan))
                    (d (push (cons loc (list d)) plan))
                    ((%slotd-initfunction sd) (push (cons loc (%slotd-initfunction sd)) plan)))))
          ;; default initargs not stored in any slot are still evaluated
          (if (or (eq plan :slow)
                  (some (lambda (d) (notany (lambda (sd) (member (car d) (%slotd-initargs sd)))
                                            (%class-effective-slots class)))
                        defaults))
              (setf (svref ctor 3) nil)
              (progn
                (setf (svref ctor 3) class (svref ctor 4) (nreverse plan)
                      (svref ctor 5) (mapcar #'cdr defaults))
                (%ctor-plans ctor class (svref ctor 4) (svref ctor 5))))))
    (svref ctor 3)))

(defun %ctor-make-instance (ctor &rest args)
  (declare (simple-vector ctor))
  (let ((class (if (eql (svref ctor 0) *ctor-generation*) (svref ctor 3) (%ctor-update ctor))))
    (if (null class)
        (let ((initargs (let ((r nil) (a args))
                          (dolist (k (svref ctor 2) (nreverse r)) (push k r) (push (car a) r) (setq a (cdr a))))))
          (if (svref ctor 6)
              (%make-instance-of (svref ctor 6) initargs (svref ctor 7))
              (apply #'make-instance (svref ctor 1) initargs)))
        (let* ((defaults (let ((fns (svref ctor 5)))   ; default initargs first, in order
                           (and fns (mapcar #'funcall fns))))
               (inst (%allocate-instance class)))
          (dolist (p (svref ctor 4) inst)
            (let ((src (cdr p)))
              (%set-instance-ref inst (car p)
                                 (cond ((fixnump src) (nth src args))
                                       ((consp src) (nth (car src) defaults))
                                       (t (funcall src))))))))))

;;; Up to four initargs: no argument list (the general function when the
;;; constructor is not up to date)
(defmacro %define-ctor-entry (name &rest params)
  `(defun ,name (ctor ,@params)
     (declare (simple-vector ctor))
     (if (and (eql (%svref ctor 0) *ctor-generation*) (%svref ctor 3))
         (let* ((slots (%copy-simple-vector (%svref ctor 8)))
                (inst (%make-instance (%svref ctor 3) slots)))
           ;; the default initargs first, in order
           (let ((dp (%svref ctor 11)))
             (do ((i 0 (%fx+ i 2))) ((%fx>= i (%svlength dp)))
               (let ((v (funcall (the function (%svref dp i)))))
                 (dolist (loc (%svref dp (%fx+ i 1))) (%set-svref slots loc v)))))
           ,@(when params
               `((let ((ap (%svref ctor 9)))
                   (do ((i 0 (%fx+ i 2))) ((%fx>= i (%svlength ap)))
                     (%set-svref slots (%svref ap (%fx+ i 1))
                                 (case (%svref ap i)
                                   ,@(let ((i -1)) (mapcar (lambda (v) (setq i (+ i 1)) (list i v)) (butlast params)))
                                   (t ,(car (last params)))))))))
           (let ((fp (%svref ctor 10)))
             (do ((i 0 (%fx+ i 2))) ((%fx>= i (%svlength fp)))
               (%set-svref slots (%svref fp i) (funcall (the function (%svref fp (%fx+ i 1)))))))
           (when *track-instance-hook* (funcall *track-instance-hook* (%svref ctor 3) inst))
           inst)
         (%ctor-make-instance ctor ,@params))))
(%define-ctor-entry %ctor-make-instance-0)
(%define-ctor-entry %ctor-make-instance-1 a)
(%define-ctor-entry %ctor-make-instance-2 a b)
(%define-ctor-entry %ctor-make-instance-3 a b c)
(%define-ctor-entry %ctor-make-instance-4 a b c d)
;;; (five to eight: a call of more than four arguments passes them in a
;;; vector, but no list of them is made and they are not looked up by NTH)
(%define-ctor-entry %ctor-make-instance-5 a b c d e)
(%define-ctor-entry %ctor-make-instance-6 a b c d e f)
(%define-ctor-entry %ctor-make-instance-7 a b c d e f g)
(%define-ctor-entry %ctor-make-instance-8 a b c d e f g h)

;;; (MAKE-INSTANCE class :KEY value ...) with constant keys and a class known
;;; only at run time: the call site keeps the constructors of the classes it
;;; was called with, #(keys (class-or-name . ctor) ...) in a list, at most
;;; +DYNAMIC-CTOR-CLASSES+.  A constructor is that of the class's name: it is
;;; used for a class only while it makes that very class (the name may have
;;; been given to another class); otherwise, and for the classes past the
;;; limit, MAKE-INSTANCE is called.
(defconstant +dynamic-ctor-classes+ 8)

(defun %dynamic-ctor (site class)
  "The constructor of CLASS (a class or a class name) at the call site SITE, or NIL."
  (let ((e (dolist (e (cdr site) nil) (when (eq (car e) class) (return e)))))
    (if e
        (cdr e)
        (let ((ctor (cond ((and (symbolp class) class) (%make-ctor class (car site)))
                          ((and (%instancep class) (typep class 'class))
                           (let ((name (%class-name class)))
                             (and name (symbolp name) (eq (find-class name nil) class)
                                  (%make-ctor name (car site)))))
                          (t nil))))
          (when (< (length (cdr site)) +dynamic-ctor-classes+)
            (push (cons class ctor) (cdr site)))
          ctor))))

(defun %dynamic-ctor-usable-p (ctor class)
  "True when the constructor CTOR, found for CLASS, makes its instances."
  (or (symbolp class)
      (eq class (if (eql (svref ctor 0) *ctor-generation*) (svref ctor 3) (%ctor-update ctor)))))

(defmacro %define-dynamic-ctor-entry (name entry &rest params)
  `(defun ,name (site class ,@params)
     (let ((ctor (%dynamic-ctor site class)))
       (if (and ctor (%dynamic-ctor-usable-p ctor class))
           (,entry ctor ,@params)
           (apply #'make-instance class
                  (let ((r nil) (a (list ,@params)))
                    (dolist (k (car site) (nreverse r)) (push k r) (push (car a) r) (setq a (cdr a)))))))))
(%define-dynamic-ctor-entry %dynamic-make-instance-0 %ctor-make-instance-0)
(%define-dynamic-ctor-entry %dynamic-make-instance-1 %ctor-make-instance-1 a)
(%define-dynamic-ctor-entry %dynamic-make-instance-2 %ctor-make-instance-2 a b)
(%define-dynamic-ctor-entry %dynamic-make-instance-3 %ctor-make-instance-3 a b c)
(%define-dynamic-ctor-entry %dynamic-make-instance-4 %ctor-make-instance-4 a b c d)
(%define-dynamic-ctor-entry %dynamic-make-instance-5 %ctor-make-instance-5 a b c d e)
(%define-dynamic-ctor-entry %dynamic-make-instance-6 %ctor-make-instance-6 a b c d e f)
(%define-dynamic-ctor-entry %dynamic-make-instance-7 %ctor-make-instance-7 a b c d e f g)
(%define-dynamic-ctor-entry %dynamic-make-instance-8 %ctor-make-instance-8 a b c d e f g h)

(define-compiler-macro make-instance (&whole form class &rest initargs)
  (cond ((not (and (evenp (length initargs))
                   (do ((l initargs (cddr l))) ((null l) t) (unless (keywordp (car l)) (return nil)))))
         form)
        ((and (consp class) (eq (car class) 'quote) (consp (cdr class)) (symbolp (cadr class)))
         (let ((keys nil) (vals nil))
           (do ((l initargs (cddr l))) ((null l)) (push (car l) keys) (push (cadr l) vals))
           `(,(case (length vals)
                (0 '%ctor-make-instance-0) (1 '%ctor-make-instance-1) (2 '%ctor-make-instance-2)
                (3 '%ctor-make-instance-3) (4 '%ctor-make-instance-4) (5 '%ctor-make-instance-5)
                (6 '%ctor-make-instance-6) (7 '%ctor-make-instance-7) (8 '%ctor-make-instance-8)
                (t '%ctor-make-instance))
             (load-time-value (%make-ctor ',(cadr class) ',(reverse keys)))
             ,@(reverse vals))))
        ;; (a constant that is not a quoted symbol: a class object, an error)
        ((or (and (atom class) (or (not (symbolp class)) (constantp class)))
             (and (consp class) (eq (car class) 'quote))
             (> (length initargs) 16))
         form)
        (t
         (let ((keys nil) (vals nil))
           (do ((l initargs (cddr l))) ((null l)) (push (car l) keys) (push (cadr l) vals))
           `(,(nth (length vals) '(%dynamic-make-instance-0 %dynamic-make-instance-1 %dynamic-make-instance-2
                                    %dynamic-make-instance-3 %dynamic-make-instance-4 %dynamic-make-instance-5
                                    %dynamic-make-instance-6 %dynamic-make-instance-7 %dynamic-make-instance-8))
             (load-time-value (list ',(reverse keys)))
             ,class
             ,@(reverse vals))))))
