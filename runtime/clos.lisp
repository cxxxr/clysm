;;;; clos.lisp — CLOS: classes, generic functions, methods, method combinations

;;; ------------------------------------------------------------
;;; metaobject layouts
;;; ------------------------------------------------------------
;;; A generic function is a closure whose name field holds its metaobject, an
;;; instance of (a subclass of) GENERIC-FUNCTION with the slots below.

(defconstant +gf-name+ 0) (defconstant +gf-ll+ 1) (defconstant +gf-methods+ 2)
(defconstant +gf-combination+ 3) (defconstant +gf-cache+ 4) (defconstant +gf-nreq+ 5)
(defconstant +gf-eql+ 6) (defconstant +gf-doc+ 7) (defconstant +gf-function+ 8)
(defconstant +gf-apo+ 9) (defconstant +gf-declarations+ 10) (defconstant +gf-method-class+ 11)
(defconstant +gf-nopt+ 12) (defconstant +gf-restp+ 13) (defconstant +gf-keyp+ 14)
(defconstant +gf-keys+ 15) (defconstant +gf-initial-methods+ 16)
;;; +GF-FAST+: the number of required parameters (1..4) when the effective
;;; methods are functions of the arguments themselves, else 0.
;;; +GF-TABLE+: the part of the cache beyond the first entries.
(defconstant +gf-fast+ 17) (defconstant +gf-table+ 18)

(defparameter *gf-slot-names*
  '(%name %lambda-list %methods %method-combination %cache %nreq %eql-specializers %documentation
    %function %argument-precedence-order %declarations %method-class %nopt %restp %keyp %keys
    %initial-methods %fast %table))

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
                     %extra %shared %prototype %direct-default-initargs %protocol %plist))

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
      (values (nreverse decls) doc body))))

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
;;; :ALL gives every entry of one to four arguments such a dispatch.
(defmacro %gf-function-maker (&optional n)
  (cond ((eq n :all)
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
        (t '(lambda (info) (lambda (&rest args) (%gf-invoke info args))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %fast-arity (ll)
    "The number of required parameters of the lambda list LL when a generic
function with it is dispatched by functions of the arguments (%GF-FUNCTION-MAKER)."
    (if (and (listp ll) (<= 1 (length ll) 4)
             (notany (lambda (x) (member x lambda-list-keywords)) ll))
        (length ll)
        nil)))

(defun %make-generic-function (name lambda-list combination class)
  (let* ((info (%allocate-instance (or class (find-class 'standard-generic-function))))
         (f (if *gf-function-maker*
                (funcall *gf-function-maker* info)
                (funcall (%gf-function-maker :all) info))))
    (dotimes (i (length *gf-slot-names*)) (%set-gf-slot info i nil))
    (%set-gf-slot info +gf-name+ name)
    (%set-gf-slot info +gf-combination+ (%find-method-combination combination))
    (%set-gf-slot info +gf-method-class+ (find-class 'standard-method))
    (%gf-set-lambda-list info lambda-list)
    (%set-closure-name f info)
    (%set-gf-slot info +gf-function+ f)
    f))

;;; The cache (+GF-CACHE+) is a simple-vector
;;;   #(tag  key-0 ... key-(nreq-1) value methods  key-0 ...)
;;; searched without consing.  A key is the class of a required argument, or
;;; the marker (object . hash) of the EQL specializer object the argument is
;;; (+GF-EQL+: a list (position marker...)).  VALUE is a function of the
;;; required arguments when +GF-FAST+ is not 0, else (effective-method-function
;;; . valid-keys), a function of the argument list; METHODS are the applicable
;;; methods.  TAG is +GF-FAST+ when the keys are all classes (the dispatch
;;; inlined in the function of the generic function compares the classes of
;;; the arguments), else 0.  After +GF-CACHE-ENTRIES+ entries the others go to
;;; +GF-TABLE+: (count . #(mask key... value methods ...)), an open-addressing
;;; hash table on the keys.
(defconstant +gf-cache-entries+ 6)

(defun %gf-reset-cache (info)
  (let* ((nreq (%gf-slot info +gf-nreq+))
         (fast (if (and (fixnump nreq) (<= 1 nreq 4) (eql (%gf-slot info +gf-nopt+) 0)
                        (not (%gf-slot info +gf-restp+))
                        (or (null *the-standard-generic-function-class*)
                            (eq (%instance-layout info) *the-standard-generic-function-class*)))
                   nreq 0)))
    (%set-gf-slot info +gf-fast+ fast)
    (%set-gf-slot info +gf-cache+ (vector (if (%gf-slot info +gf-eql+) 0 fast)))
    (%set-gf-slot info +gf-table+ nil)))

(defun %gf-invalidate (info)
  (%gf-reset-cache info)
  (%clear-initarg-caches))

(defvar *initarg-cache* nil)
(defvar *fast-make-instance* nil)
(defvar *ctor-generation* 0)
(defun %clear-initarg-caches ()
  (setq *ctor-generation* (+ *ctor-generation* 1))
  (when *initarg-cache* (clrhash *initarg-cache*))
  (when *fast-make-instance* (clrhash *fast-make-instance*)))

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
(defun %update-eql-info (info)
  (let ((r nil))
    (dolist (m (%gf-slot info +gf-methods+))
      (let ((i 0))
        (dolist (s (%method-specializers m))
          (when (consp s)
            (let ((e (assoc i r)) (x (cadr s)))
              (unless e (setq e (list i)) (push e r))
              (unless (dolist (k (cdr e) nil) (when (eql (car k) x) (return t)))
                (setq *eql-marker-count* (%fxlogand (+ *eql-marker-count* 1) #xFFFFF))
                (push (cons x *eql-marker-count*) (cdr e)))))
          (setq i (+ i 1)))))
    (%set-gf-slot info +gf-eql+ r)))

(defvar *add-method-hook* nil)

(defun %add-method (gf method)
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
    (%update-eql-info info)
    (%gf-invalidate info)
    gf))

(defun %remove-method (gf method)
  (let ((info (%gf-info gf)))
    (when (member method (%gf-slot info +gf-methods+))
      (%set-gf-slot info +gf-methods+ (remove method (%gf-slot info +gf-methods+)))
      (when *add-method-hook* (funcall *add-method-hook* method nil))
      (%set-instance-ref method 0 nil)
      (%update-eql-info info)
      (%gf-invalidate info))
    gf))

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

(defun %defmethod (name qualifiers specializers lambda-list function doc cell)
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

(defmacro %fast-class-of (x)
  `(let ((x ,x)) (if (%instancep x) (%instance-layout x) (class-of x))))

(defun %eql-key (eqls i x)
  "The cache key of the argument X at position I: the marker of the EQL
specializer object that X is, else the class of X."
  (dolist (e eqls (%fast-class-of x))
    (when (eq (car e) i)
      (return (dolist (k (cdr e) (%fast-class-of x))
                (when (eql (car k) x) (return k)))))))

(defmacro %key-hash (k)
  `(let ((k ,k)) (%fxlogand (if (consp k) (cdr k) (%identity-hash k)) #xFFFFF)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %splice-args (tree vars)
    "TREE with the elements %ARGS of its lists replaced by the elements of VARS."
    (cond ((atom tree) tree)
          ((eq (car tree) '%args) (append vars (%splice-args (cdr tree) vars)))
          (t (cons (%splice-args (car tree) vars) (%splice-args (cdr tree) vars)))))
  (defun %arity-vars (n)
    (let ((r nil)) (dotimes (i n (nreverse r)) (push (nth i '(%a0 %a1 %a2 %a3)) r)))))

;;; (%ARITY-CASE n form): FORM for N = 1..4, with %ARGS in a list of FORM
;;; standing for N variables.
(defmacro %arity-case (n form)
  `(case ,n
     (1 ,(%splice-args form (%arity-vars 1)))
     (2 ,(%splice-args form (%arity-vars 2)))
     (3 ,(%splice-args form (%arity-vars 3)))
     (t ,(%splice-args form (%arity-vars 4)))))

(defun %gf-keys (info args nreq)
  (let ((eqls (%gf-slot info +gf-eql+)) (r nil) (a args))
    (dotimes (i nreq (nreverse r))
      (push (if eqls (%eql-key eqls i (car a)) (class-of (car a))) r)
      (setq a (cdr a)))))

(defun %keys-hash (keys)
  (let ((h 0))
    (dolist (k keys h) (setq h (%fxlogand (%fx+ (%fx* h 31) (%key-hash k)) #xFFFFF)))))

(defun %gf-table-find (vec keys nreq)
  "The index of the value of KEYS in the table VEC, or NIL."
  (let* ((stride (+ nreq 2)) (mask (svref vec 0)) (i (logand (%keys-hash keys) mask)))
    (loop
      (let ((base (+ 1 (* i stride))))
        (cond ((eq (svref vec base) 0) (return nil))
              ((do ((j base (+ j 1)) (l keys (cdr l))) ((null l) t)
                 (unless (eq (svref vec j) (car l)) (return nil)))
               (return (+ base nreq))))
        (setq i (logand (+ i 1) mask))))))

(defun %gf-table-insert (vec keys nreq value methods)
  (let* ((stride (+ nreq 2)) (mask (svref vec 0)) (i (logand (%keys-hash keys) mask)))
    (loop
      (let ((base (+ 1 (* i stride))))
        (when (eq (svref vec base) 0)
          (do ((j base (+ j 1)) (l keys (cdr l))) ((null l))
            (setf (svref vec j) (car l)))
          (setf (svref vec (+ base nreq)) value (svref vec (+ base nreq 1)) methods)
          (return))
        (setq i (logand (+ i 1) mask))))))

(defun %gf-make-table (capacity nreq)
  (let ((vec (make-array (+ 1 (* capacity (+ nreq 2))) :initial-element 0)))
    (setf (svref vec 0) (- capacity 1))
    vec))

(defun %gf-table-put (info keys nreq value methods)
  (let ((table (or (%gf-slot info +gf-table+)
                   (let ((tb (cons 0 (%gf-make-table 16 nreq)))) (%set-gf-slot info +gf-table+ tb) tb))))
    (when (> (* 2 (+ (car table) 1)) (svref (cdr table) 0))
      (let* ((old (cdr table)) (new (%gf-make-table (* 2 (+ (svref old 0) 1)) nreq)) (stride (+ nreq 2)))
        (do ((b 1 (+ b stride))) ((>= b (length old)))
          (unless (eq (svref old b) 0)
            (%gf-table-insert new (let ((r nil)) (dotimes (j nreq (nreverse r)) (push (svref old (+ b j)) r)))
                              nreq (svref old (+ b nreq)) (svref old (+ b nreq 1)))))
        (setf (cdr table) new)))
    (%gf-table-insert (cdr table) keys nreq value methods)
    (setf (car table) (+ (car table) 1))))

(defun %gf-fill (info keys args nreq which)
  "Compute the cache entry for ARGS (whose keys are KEYS) and store it."
  (let ((pic (%gf-slot info +gf-cache+)))
    (multiple-value-bind (value methods) (%gf-compute-value info args)
      ;; computing the effective method may have invalidated the cache
      (when (eq pic (%gf-slot info +gf-cache+))
        (let ((len (length pic)))
          (if (< len (* +gf-cache-entries+ (+ nreq 2)))
              (let ((new (make-array (+ len nreq 2))) (j len))
                (dotimes (i len) (setf (svref new i) (svref pic i)))
                (dolist (k keys) (setf (svref new j) k) (setq j (+ j 1)))
                (setf (svref new j) value (svref new (+ j 1)) methods)
                (%set-gf-slot info +gf-cache+ new))
              (%gf-table-put info keys nreq value methods))))
      (if (eql which 0) value methods))))

(defun %gf-lookup (info args nreq which)
  "The cache entry for ARGS: its value (WHICH = 0) or the applicable methods (1)."
  (let* ((eqls (%gf-slot info +gf-eql+))
         (keys (if eqls (%gf-keys info args nreq) nil))
         (pic (%gf-slot info +gf-cache+)) (len (length pic)) (stride (+ nreq 2)))
    (declare (simple-vector pic))
    (do ((i 1 (+ i stride))) ((>= i len))
      (when (if eqls
                (do ((j i (+ j 1)) (l keys (cdr l))) ((null l) t)
                  (unless (eq (svref pic j) (car l)) (return nil)))
                (do ((j i (+ j 1)) (p 0 (+ p 1)) (a args (cdr a))) ((>= p nreq) t)
                  (unless (eq (svref pic j) (%fast-class-of (car a))) (return nil))))
        (return-from %gf-lookup (svref pic (+ i nreq which)))))
    (let ((keys (or keys (%gf-keys info args nreq)))
          (table (%gf-slot info +gf-table+)))
      (let ((pos (and table (%gf-table-find (cdr table) keys nreq))))
        (if pos
            (svref (cdr table) (+ pos which))
            (%gf-fill info keys args nreq which))))))

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
  (let* ((nreq (%gf-slot info +gf-nreq+))
         (nargs (length args)))
    (when (< nargs nreq)
      (%gf-arg-error info "too few arguments (~S given, ~S required)" nargs nreq))
    (let ((nfixed (+ nreq (%gf-slot info +gf-nopt+))))
      (if (%gf-slot info +gf-restp+)
          (when (and (%gf-slot info +gf-keyp+) (> nargs nfixed))
            (let ((keys (nthcdr nfixed args)))
              (when (oddp (length keys)) (%gf-arg-error info "odd number of keyword arguments ~S" keys))
              (do ((l keys (cddr l))) ((null l))
                (unless (symbolp (car l)) (%gf-arg-error info "~S is not a keyword" (car l))))))
          (when (> nargs nfixed)
            (%gf-arg-error info "too many arguments (~S given, at most ~S allowed)" nargs nfixed)))
      (let ((entry (%gf-lookup info args nreq 0)))
        (if (consp entry)
            (progn
              (when (and (%gf-slot info +gf-keyp+) (not (eq (cdr entry) :any)) (> nargs nfixed))
                (%check-call-keys info (cdr entry) (nthcdr nfixed args)))
              (funcall (the function (car entry)) args))
            (apply (the function entry) args))))))

;;; The dispatch on one to four required arguments.  %GF-CALLn is inlined in
;;; the function of a generic function: it searches the first cache entries
;;; for the classes of the arguments.  %GF-MISSn does the rest: the keys with
;;; EQL specializers, the table, the computation of the effective method.
(defmacro %define-gf-dispatch (n)
  (let* ((args (subseq '(a b c d) 0 n))
         (keys (subseq '(k0 k1 k2 k3) 0 n))
         (call (nth (- n 1) '(%gf-call1 %gf-call2 %gf-call3 %gf-call4)))
         (miss (nth (- n 1) '(%gf-miss1 %gf-miss2 %gf-miss3 %gf-miss4)))
         (stride (+ n 2)))
    (flet ((match (vec base)
             (let ((i -1))
               `(and ,@(mapcar (lambda (k)
                                 (setq i (+ i 1))
                                 `(eq (%svref ,vec ,(if (eql i 0) base `(%fx+ ,base ,i))) ,k))
                               keys)))))
      `(progn
         (declaim (inline ,call))
         (defun ,call (info ,@args)
           (let ((pic (%gf-slot info +gf-cache+)))
             (when (eq (%svref pic 0) ,n)
               (let (,@(mapcar (lambda (k a) `(,k (%fast-class-of ,a))) keys args)
                     (len (%svlength pic)))
                 (do ((i 1 (%fx+ i ,stride))) ((%fx>= i len))
                   (when ,(match 'pic 'i)
                     (return-from ,call (funcall (the function (%svref pic (%fx+ i ,n))) ,@args))))))
             (,miss info ,@args)))
         (defun ,miss (info ,@args)
           (if (eql (%gf-slot info +gf-fast+) ,n)
               (let* ((eqls (%gf-slot info +gf-eql+))
                      ,@(let ((i -1))
                          (mapcar (lambda (k a)
                                    (setq i (+ i 1))
                                    `(,k (if eqls (%eql-key eqls ,i ,a) (%fast-class-of ,a))))
                                  keys args))
                      (pic (%gf-slot info +gf-cache+)) (len (%svlength pic)))
                 (do ((i 1 (%fx+ i ,stride))) ((%fx>= i len))
                   (when ,(match 'pic 'i)
                     (return-from ,miss (funcall (the function (%svref pic (%fx+ i ,n))) ,@args))))
                 (let ((table (%gf-slot info +gf-table+)))
                   (when table
                     (let* ((vec (cdr table)) (mask (%svref vec 0))
                            (i (%fxlogand ,(let ((h nil))
                                             (dolist (k keys h)
                                               (setq h (if h
                                                           `(%fxlogand (%fx+ (%fx* ,h 31) (%key-hash ,k)) #xFFFFF)
                                                           `(%key-hash ,k)))))
                                          mask)))
                       (loop
                         (let ((base (%fx+ 1 (%fx* i ,stride))))
                           (cond ((eq (%svref vec base) 0) (return))
                                 (,(match 'vec 'base)
                                  (return-from ,miss
                                    (funcall (the function (%svref vec (%fx+ base ,n))) ,@args))))
                           (setq i (%fxlogand (%fx+ i 1) mask)))))))
                 (funcall (the function (%gf-fill info (list ,@keys) (list ,@args) ,n 0)) ,@args))
               (%gf-invoke info (list ,@args))))))))

(%define-gf-dispatch 1)
(%define-gf-dispatch 2)
(%define-gf-dispatch 3)
(%define-gf-dispatch 4)

;;; Methods of only required parameters (at most four) are also known as
;;; functions of the arguments: either such a function (a method that does not
;;; use CALL-NEXT-METHOD or NEXT-METHOD-P), or a function making one from the
;;; next method (a function of the arguments, or NIL).
(defvar *method-fast-functions* (make-hash-table :test 'eq))
(defvar *method-fast-makers* (make-hash-table :test 'eq))
(defun %note-method-fast-function (m f)
  (setf (gethash m *method-fast-functions*) f)
  m)
(defun %note-method-fast-maker (m f)
  (setf (gethash m *method-fast-makers*) f)
  m)

(defun %maker-method-function (maker)
  "The method function (of the argument list and the next method functions) of
a method known by MAKER."
  (lambda (args next)
    (apply (the function (funcall maker (if next (lambda (&rest a) (funcall (car next) a (cdr next))) nil)))
           args)))

(defun %method-fast-fn (m next n)
  "A function of the N required arguments calling the method M, NEXT (a function
of the arguments, or NIL) being its next method."
  (let ((maker (gethash m *method-fast-makers*)))
    (if maker
        (funcall maker next)
        (or (gethash m *method-fast-functions*)
            (let ((mf (%method-function m))
                  (nexts (if next (list (lambda (args n) (declare (ignore n)) (apply next args))) nil)))
              (%arity-case n (lambda (%args) (funcall mf (list %args) nexts))))))))

(defun %compute-fast-emf (gf info methods args n)
  (cond ((null methods) (%arity-case n (lambda (%args) (no-applicable-method gf %args))))
        ((%fast-accessor-fn info methods args))
        ((eq (%gf-slot info +gf-combination+) *standard-method-combination*)
         (%standard-fast-emf gf methods n))
        (t (let ((emf (%effective-method-function gf info methods)))
             (%arity-case n (lambda (%args) (funcall emf (list %args))))))))

(defun %standard-fast-emf (gf methods n)
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
        (%arity-case n (lambda (%args) (no-applicable-method gf %args)))
        (let ((f nil))
          (dolist (m primary) (setq f (%method-fast-fn m f n)))
          (when (or before after)
            (let ((bfns (mapcar (lambda (m) (%method-fast-fn m nil n)) (nreverse before)))
                  (afns (mapcar (lambda (m) (%method-fast-fn m nil n)) after))
                  (p f))
              (setq f (if afns
                          (%arity-case n (lambda (%args)
                                           (dolist (b bfns) (funcall (the function b) %args))
                                           (multiple-value-prog1 (funcall (the function p) %args)
                                             (dolist (a afns) (funcall (the function a) %args)))))
                          (%arity-case n (lambda (%args)
                                           (dolist (b bfns) (funcall (the function b) %args))
                                           (funcall (the function p) %args)))))))
          (dolist (m around) (setq f (%method-fast-fn m f n)))
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
             (mf (%method-function (car methods))))
        (when (fixnump loc)
          (if writerp
              (lambda (args)
                (let ((o (cadr args)))
                  (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index))
                      (%set-instance-ref o loc (car args))
                      (funcall mf args nil))))
              (lambda (args)
                (let ((o (car args)))
                  (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index))
                      (let ((v (%instance-ref o loc)))
                        (if (%unboundp v) (funcall mf args nil) v))
                      (funcall mf args nil))))))))))

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
             (mf (%method-function (car methods)))
             (nreq (%gf-slot info +gf-nreq+)))
        (when (and (fixnump loc) (eql nreq (if writerp 2 1)))
          (if writerp
              (lambda (v o)
                (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index))
                    (%set-instance-ref o loc v)
                    (funcall mf (list v o) nil)))
              (lambda (o)
                (if (and (%instancep o) (eq (%class-slot (%instance-layout o) +class-slot-index+) index))
                    (let ((v (%instance-ref o loc)))
                      (if (%unboundp v) (funcall mf (list o) nil) v))
                    (funcall mf (list o) nil)))))))))

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
(defun %short-emf (gf methods name def options)
  (let ((op (cadr def)) (identity (caddr def)) (around nil) (primary nil)
        (order (if options (car options) :most-specific-first)))
    (unless (and (member order '(:most-specific-first :most-specific-last)) (null (cdr options)))
      (error 'simple-error :format-control "Invalid options ~S for the ~S method combination"
                           :format-arguments (list options name)))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((equal q '(:around)) (push m around))
              ((and (consp q) (eq (car q) name) (null (cdr q))) (push m primary))
              (t (%invalid-qualifiers gf m name)))))
    (setq around (nreverse around))
    (unless (eq order :most-specific-last) (setq primary (nreverse primary)))
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
  (when (and cnm-args (car cell))
    (%check-cnm-args (car cell) args cnm-args))
  (if (null next)
      (%no-next-method cell (or cnm-args args))
      (funcall (car next) (or cnm-args args) (cdr next))))

(defun %call-next-method-fast (args next cnm-args cell)
  "CALL-NEXT-METHOD as a function in a method whose next method NEXT is a
function of the arguments (or NIL)."
  (when (and cnm-args (car cell))
    (%check-cnm-args (car cell) args cnm-args))
  (if (null next)
      (%no-next-method cell (or cnm-args args))
      (apply (the function next) (or cnm-args args))))

;;; CLHS CALL-NEXT-METHOD: with arguments, the ordered set of applicable
;;; methods must be the one of the original arguments.  It is when every
;;; argument has the cache key (class or EQL specializer object) of the
;;; original one; otherwise the applicable methods of the two cache entries
;;; are compared.
(defun %cnm-arg-ok (cell i old new)
  "True when the required argument NEW at position I, given to
CALL-NEXT-METHOD in place of OLD, has the same cache key."
  (let* ((m (car cell)) (gf (and m (%method-gf m))))
    (or (null gf)
        (let ((eqls (%gf-slot (%closure-name gf) +gf-eql+)))
          (if eqls
              (eq (%eql-key eqls i old) (%eql-key eqls i new))
              (eq (%fast-class-of old) (%fast-class-of new)))))))

(defun %check-cnm-args (method args cnm-args)
  (let ((gf (%method-gf method)))
    (when gf
      (let* ((info (%gf-info gf)) (nreq (%gf-slot info +gf-nreq+)) (eqls (%gf-slot info +gf-eql+)))
        (unless (or (do ((i 0 (+ i 1)) (a args (cdr a)) (b cnm-args (cdr b))) ((>= i nreq) t)
                      (when (or (atom a) (atom b)) (return (and (atom a) (atom b))))
                      (unless (or (eq (car a) (car b))
                                  (if eqls
                                      (eq (%eql-key eqls i (car a)) (%eql-key eqls i (car b)))
                                      (eq (%fast-class-of (car a)) (%fast-class-of (car b)))))
                        (return nil)))
                    (< (length cnm-args) nreq)   ; the call of the next method signals the error
                    (equal (%gf-lookup info args nreq 1) (%gf-lookup info cnm-args nreq 1)))
          (error 'simple-error
                 :format-control "CALL-NEXT-METHOD: the arguments ~S change the set of applicable methods of ~S"
                 :format-arguments (list cnm-args gf)))))))

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
      (let ((lambda-ll (if (and (member '&key plain-ll) (not (member '&allow-other-keys plain-ll)))
                           (let ((aux (member '&aux plain-ll)))
                             (append (ldiff plain-ll aux) '(&allow-other-keys) aux))
                           plain-ll))
            (req-vars (subseq plain-ll 0 (length specs))))
        (multiple-value-bind (decls doc forms) (%parse-body body t)
          (values
           `(lambda (,args ,next)
              (declare (ignorable ,next))
              (flet ((call-next-method (&rest cnm-args) (%call-next-method ,args ,next cnm-args ,cell))
                     (next-method-p () (not (null ,next))))
                (apply (lambda ,lambda-ll
                         ,@decls
                         (declare (ignorable ,@req-vars))
                         (block ,(if (consp name) (cadr name) name) ,@forms))
                       ,args)))
           specs
           plain-ll
           doc))))))

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

  (defun %expand-fast-cnm (cnm-args vars next cell)
    "The expansion of (CALL-NEXT-METHOD . CNM-ARGS) in a method whose required
arguments are in VARS and whose next method (a function of the arguments, or
NIL) is in NEXT."
    (cond ((null cnm-args)
           `(if ,next
                (funcall (the function ,next) ,@vars)
                (%no-next-method ,cell (list ,@vars))))
          ((= (length cnm-args) (length vars))
           (let ((new (mapcar (lambda (v) (declare (ignore v)) (gensym "ARG")) vars)) (i -1))
             `(let ,(mapcar #'list new cnm-args)
                (unless (and ,@(mapcar (lambda (v n)
                                         (setq i (+ i 1))
                                         `(or (eq ,v ,n) (%cnm-arg-ok ,cell ,i ,v ,n)))
                                       vars new))
                  (%check-cnm-args (car ,cell) (list ,@vars) (list ,@new)))
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
      (let* ((next (gensym "NEXT"))
             (vars (mapcar (lambda (v) (declare (ignore v)) (gensym "A")) plain-ll))
             (inner `(let ,(mapcar #'list plain-ll vars)
                       ,@decls
                       (declare (ignorable ,@plain-ll))
                       (block ,(if (consp name) (cadr name) name) ,@forms))))
        `(lambda (,next)
           (lambda ,vars
             ,(if (%only-operators-p (cons 'progn body) '(call-next-method next-method-p))
                  `(macrolet ((call-next-method (&rest cnm-args)
                                (%expand-fast-cnm cnm-args ',vars ',next ',cell))
                              (next-method-p () '(not (null ,next))))
                     ,inner)
                  `(flet ((call-next-method (&rest cnm-args)
                            (%call-next-method-fast (list ,@vars) ,next cnm-args ,cell))
                          (next-method-p () (not (null ,next))))
                     ,inner))))))))

(defmacro defmethod (name &rest args)
  (multiple-value-bind (quals ll body) (%split-method-args args)
    (let ((cell (gensym "CELL")) (fast (gensym "FAST")))
      (multiple-value-bind (lam specs plain-ll doc) (%method-lambda name ll body cell)
        (let ((specs-form `(list ,@(mapcar (lambda (s) (if (and (consp s) (eq (car s) 'eql))
                                                            `(list 'eql ,(cadr s))
                                                            `',s))
                                            specs)))
              (maker `(%gf-function-maker ,(%fast-arity plain-ll))))
          (cond ((and (listp ll) (notany (lambda (x) (member x lambda-list-keywords)) plain-ll)
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

(defmacro defgeneric (name lambda-list &rest options)
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
      `(let* ((*gf-function-maker* (%gf-function-maker ,(%fast-arity lambda-list)))
              (,gf (%defgeneric ',name ',lambda-list ',combination ,doc ',apo ',gfclass ',mclass ',decls)))
         (%set-gf-initial-methods ,gf (list ,@(reverse methods)))
         ,gf))))

(defun %defgeneric (name ll combination doc apo gfclass mclass decls)
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

(defmacro defclass (name supers slots &rest options)
  (let ((slotd-forms nil) (accessor-forms nil) (default-initargs nil) (metaclass nil) (doc nil)
        (seen nil) (slot-names nil) (others nil))
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
          (push `(%note-slot-accessor (defmethod ,r ((object ,name)) (slot-value object ',sname)) ',sname nil)
                accessor-forms))
        (dolist (w writers)
          (push `(%note-slot-accessor
                  (defmethod ,w (new-value (object ,name)) (setf (slot-value object ',sname) new-value))
                  ',sname t)
                accessor-forms))))
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

(defun %ensure-class (name supers direct-slots default-initargs metaclass &optional doc extra direct-default-initargs)
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

(defun ensure-class (name &rest initargs &key direct-superclasses direct-slots metaclass direct-default-initargs
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

;;; a class finalization invalidates the initarg caches
(setq *class-finalization-hook*
      (lambda (class) (declare (ignore class)) (%clear-initarg-caches)))

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
          (cond ((fixnump loc) (not (%unboundp (%instance-ref object loc))))
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
          (cond ((fixnump loc) (%set-instance-ref object loc (%unbound-marker)))
                ((consp loc) (setf (cdr loc) (%unbound-marker)))
                (t (%slot-missing object slot-name 'slot-makunbound nil)))
          object))))

(defun slot-exists-p (object slot-name)
  (setq object (%fi-object object))
  (and (%instancep object) (not (null (%slot-location object slot-name)))))

;;; the slot access protocol (AMOP): applies to instances of classes whose
;;; metaclass is not one of the implementation's
(defun %slotd-raw-value (object slotd)
  (let ((loc (%slotd-location slotd)))
    (if (fixnump loc) (%instance-ref object loc) (cdr loc))))

(defgeneric slot-value-using-class (class object slotd))
(defmethod slot-value-using-class ((class t) object slotd)
  (let ((v (%slotd-raw-value object slotd)))
    (if (%unboundp v) (values (slot-unbound class object (%slotd-name slotd))) v)))

(defgeneric (setf slot-value-using-class) (new-value class object slotd))
(defmethod (setf slot-value-using-class) (new-value (class t) object slotd)
  (let ((loc (%slotd-location slotd)))
    (if (fixnump loc) (%set-instance-ref object loc new-value) (setf (cdr loc) new-value))
    new-value))

(defgeneric slot-boundp-using-class (class object slotd))
(defmethod slot-boundp-using-class ((class t) object slotd)
  (not (%unboundp (%slotd-raw-value object slotd))))

(defgeneric slot-makunbound-using-class (class object slotd))
(defmethod slot-makunbound-using-class ((class t) object slotd)
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
  (unless (and (listp initargs) (evenp (length initargs)))
    (%program-error "~S: odd-length initialization argument list ~S" who initargs))
  (do ((l initargs (cddr l))) ((null l))
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
(defmethod make-instance ((class symbol) &rest initargs)
  (let ((c (find-class class)))
    (if (and *fast-make-instance* (%class-slot c +class-finalized+)
             (let ((f (gethash c *fast-make-instance*))) (and f (not (eq f :slow)))))
        (%make-instance-of c initargs)
        (apply #'make-instance c initargs))))

(defvar *standard-make-instance-method* nil)
(defun %make-instance-of (class initargs)
  (let ((fast (and (%class-slot class +class-finalized+) *fast-make-instance*
                   (%fast-make-instance-info class))))
    (when (and fast (not (eq fast :slow)))
      (%check-plist initargs 'make-instance)
      (let ((initargs (%add-default-initargs class initargs)))
        (unless (or (eq fast :any)
                    (let ((aok (%plist-find initargs :allow-other-keys))) (and aok (cadr aok))))
          (do ((l initargs (cddr l))) ((null l))
            (unless (or (eq (car l) :allow-other-keys) (member (car l) fast))
              (%program-error "~S: invalid initialization argument ~S for ~S" 'make-instance (car l) class))))
        (return-from %make-instance-of
          (%std-shared-initialize (%allocate-instance class) class t initargs)))))
  (when (eq (%class-kind class) :forward)
    (error 'simple-error :format-control "Cannot instantiate the forward-referenced class ~S"
                         :format-arguments (list (%class-name class))))
  (unless (%class-slot class +class-finalized+)
    (finalize-inheritance class))
  (%check-plist initargs 'make-instance)
  (let* ((initargs (%add-default-initargs class initargs))
         (proto (%class-prototype class)))
    (%check-initargs class 'make-instance initargs
                     (list (list #'allocate-instance class)
                           (list #'initialize-instance proto)
                           (list #'shared-initialize proto t)))
    (let ((instance (apply #'allocate-instance class initargs)))
      (apply #'initialize-instance instance initargs)
      instance)))

(defmethod make-instance ((class standard-class) &rest initargs) (%make-instance-of class initargs))
(setq *standard-make-instance-method* (car (%gf-slot (%gf-info #'make-instance) +gf-methods+)))
(defmethod make-instance ((class funcallable-standard-class) &rest initargs) (%make-instance-of class initargs))

(defgeneric initialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

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
  (progn
    (dolist (sd (%class-effective-slots class))
      (let ((loc (%slotd-location sd)) (found nil) (value nil))
        (do ((l initargs (cddr l))) ((or found (atom l)))
          (when (member (car l) (%slotd-initargs sd))
            (setq found t value (cadr l))))
        (cond (found
               (if (fixnump loc) (%set-instance-ref instance loc value) (setf (cdr loc) value)))
              ((and (%slotd-initfunction sd)
                    (or (eq slot-names t) (member (%slotd-name sd) slot-names))
                    (if (fixnump loc) (%unboundp (%instance-ref instance loc)) (%unboundp (cdr loc))))
               (let ((v (funcall (%slotd-initfunction sd))))
                 (if (fixnump loc) (%set-instance-ref instance loc v) (setf (cdr loc) v)))))))
    instance))

;;; MAKE-INSTANCE without the generic function calls when only the standard
;;; methods of MAKE-INSTANCE, ALLOCATE-INSTANCE, INITIALIZE-INSTANCE and
;;; SHARED-INITIALIZE apply.  Per class: the valid initargs, or :SLOW.
(setq *fast-make-instance* (make-hash-table :test 'eq))
(defvar *standard-creation-methods*
  (append (%gf-slot (%gf-info #'allocate-instance) +gf-methods+)
          (%gf-slot (%gf-info #'initialize-instance) +gf-methods+)
          (%gf-slot (%gf-info #'shared-initialize) +gf-methods+)))

(defun %only-standard-methods-p (gf args)
  (let ((ms (%compute-applicable-methods gf args)))
    (and ms (null (cdr ms)) (member (car ms) *standard-creation-methods*))))

(defun %fast-make-instance-info (class)
  (multiple-value-bind (v found) (gethash class *fast-make-instance*)
    (if found v
        (setf (gethash class *fast-make-instance*)
              (let ((proto (%class-prototype class)))
                (if (and (eq (%class-kind class) :standard)
                         (not (%class-slot class +class-protocol+))
                         (not (functionp proto))
                         (%only-standard-methods-p #'allocate-instance (list class))
                         (%only-standard-methods-p #'initialize-instance (list proto))
                         (%only-standard-methods-p #'shared-initialize (list proto t))
                         (let ((ms (%compute-applicable-methods #'make-instance (list class))))
                           (and ms (null (cdr ms)) (eq (car ms) *standard-make-instance-method*))))
                    (%valid-initargs class 'make-instance
                                     (list (list #'allocate-instance class)
                                           (list #'initialize-instance proto)
                                           (list #'shared-initialize proto t)))
                    :slow))))))

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
(defun finalize-inheritance (c)
  (let ((fwd (find-if (lambda (k) (eq (%class-kind k) :forward)) (%compute-cpl c (%class-slot c +class-supers+)))))
    (when fwd
      (error 'simple-error :format-control "Cannot finalize ~S: its superclass ~S is not defined"
                           :format-arguments (list c (%class-name fwd))))
    (%finalize-class c)))
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
       (or (typep x 'standard-object) (typep x 'structure-object))))

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
      (lambda (name class fn writerp)
        (when (and (fboundp name) (not (%generic-function-p (fdefinition name))))
          (fmakunbound name))
        (%note-method-fast-function
         (%defmethod name nil (if writerp (list t class) (list class))
                     (if writerp '(new-value condition) '(condition))
                     (lambda (args next) (declare (ignore next)) (apply fn args))
                     nil nil)
         fn)))

;;; ------------------------------------------------------------
;;; constructors: (MAKE-INSTANCE 'NAME :KEY value ...) with constant initarg
;;; keys compiles to a call of a per-site constructor, which precomputes the
;;; slot locations while only the standard methods apply.
;;; A ctor is #(generation name keys class plan default-initarg-functions);
;;; PLAN is a list of (location . n) for slots taking the n-th argument,
;;; (location . (i)) for the i-th default initarg, (location . function) for
;;; slots taking their initform.
;;; ------------------------------------------------------------

(defun %make-ctor (name keys) (vector -1 name keys nil nil nil))

(defun %ctor-update (ctor)
  "Recompute CTOR's plan; NIL when the fast path does not apply."
  (let* ((name (svref ctor 1)) (keys (svref ctor 2))
         (class (find-class name nil))
         (fast (and class (%instancep class) (eq (%class-kind class) :standard)
                    (progn (unless (%class-slot class +class-finalized+) (finalize-inheritance class))
                           (%class-slot class +class-finalized+))
                    (%fast-make-instance-info class))))
    (setf (svref ctor 0) *ctor-generation*)
    (if (or (null fast) (eq fast :slow) (member :allow-other-keys keys)
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
              (setf (svref ctor 3) class (svref ctor 4) (nreverse plan)
                    (svref ctor 5) (mapcar #'cdr defaults)))))
    (svref ctor 3)))

(defun %ctor-make-instance (ctor &rest args)
  (declare (simple-vector ctor))
  (let ((class (if (eql (svref ctor 0) *ctor-generation*) (svref ctor 3) (%ctor-update ctor))))
    (if (null class)
        (apply #'make-instance (svref ctor 1)
               (let ((r nil) (a args))
                 (dolist (k (svref ctor 2) (nreverse r)) (push k r) (push (car a) r) (setq a (cdr a)))))
        (let* ((defaults (let ((fns (svref ctor 5)))   ; default initargs first, in order
                           (and fns (mapcar #'funcall fns))))
               (inst (%allocate-instance class)))
          (dolist (p (svref ctor 4) inst)
            (let ((src (cdr p)))
              (%set-instance-ref inst (car p)
                                 (cond ((fixnump src) (nth src args))
                                       ((consp src) (nth (car src) defaults))
                                       (t (funcall src))))))))))

(define-compiler-macro make-instance (&whole form class &rest initargs)
  (if (and (consp class) (eq (car class) 'quote) (consp (cdr class)) (symbolp (cadr class))
           (evenp (length initargs))
           (do ((l initargs (cddr l))) ((null l) t) (unless (keywordp (car l)) (return nil))))
      (let ((keys nil) (vals nil))
        (do ((l initargs (cddr l))) ((null l)) (push (car l) keys) (push (cadr l) vals))
        `(%ctor-make-instance (load-time-value (%make-ctor ',(cadr class) ',(reverse keys)))
                              ,@(reverse vals)))
      form))
