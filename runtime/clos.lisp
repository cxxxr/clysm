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

(defparameter *gf-slot-names*
  '(%name %lambda-list %methods %method-combination %cache %nreq %eql-specializers %documentation
    %function %argument-precedence-order %declarations %method-class %nopt %restp %keyp %keys
    %initial-methods))

;;; method: 0 gf 1 qualifiers 2 specializers 3 function 4 lambda-list 5 doc 6 keys
(defparameter *method-slot-names*
  '(%generic-function %qualifiers %specializers %function %lambda-list %documentation %keys))

;;; method combination: 0 name 1 options
(defparameter *method-combination-slot-names* '(%name %options))

(defun %define-raw-slots (class-name names)
  (let ((class (find-class class-name)))
    (%set-class-slot class +class-direct-slots+ (mapcar (lambda (n) (%make-slotd n)) names))
    (%finalize-class class)))

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
    (%set-gf-slot info +gf-keys+ keys)))

(defun %make-generic-function (name lambda-list combination class)
  (let* ((info (%allocate-instance (or class (find-class 'standard-generic-function))))
         (f (lambda (&rest args) (%gf-invoke info args))))
    (dotimes (i (length *gf-slot-names*)) (%set-gf-slot info i nil))
    (%set-gf-slot info +gf-name+ name)
    (%set-gf-slot info +gf-combination+ (%find-method-combination combination))
    (%set-gf-slot info +gf-cache+ (make-hash-table :test 'equal))
    (%set-gf-slot info +gf-method-class+ (find-class 'standard-method))
    (%gf-set-lambda-list info lambda-list)
    (%set-closure-name f info)
    (%set-gf-slot info +gf-function+ f)
    f))

(defun %gf-invalidate (info)
  (clrhash (%gf-slot info +gf-cache+))
  (when *initarg-cache* (clrhash *initarg-cache*)))

(defvar *initarg-cache* nil)

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

(defun %update-eql-info (info)
  (let ((r nil))
    (dolist (m (%gf-slot info +gf-methods+))
      (let ((i 0))
        (dolist (s (%method-specializers m))
          (when (consp s)
            (let ((e (assoc i r)))
              (if e (pushnew (cadr s) (cdr e)) (push (list i (cadr s)) r))))
          (setq i (+ i 1)))))
    (%set-gf-slot info +gf-eql+ r)))

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
      (%set-gf-slot info +gf-methods+ (cons method (remove old (%gf-slot info +gf-methods+)))))
    (%update-eql-info info)
    (%gf-invalidate info)
    gf))

(defun %remove-method (gf method)
  (let ((info (%gf-info gf)))
    (when (member method (%gf-slot info +gf-methods+))
      (%set-gf-slot info +gf-methods+ (remove method (%gf-slot info +gf-methods+)))
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

(defun %gf-cache-key (info args nreq)
  (let ((eqls (%gf-slot info +gf-eql+)) (r nil) (a args))
    (dotimes (i nreq (nreverse r))
      (let ((x (car a)))
        (push (let ((e (and eqls (assoc i eqls))))
                (if (and e (member x (cdr e))) (list 'eql x) (class-of x)))
              r))
      (setq a (cdr a)))))

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
      (let* ((key (%gf-cache-key info args nreq))
             (cache (%gf-slot info +gf-cache+))
             (entry (or (gethash key cache)
                        (setf (gethash key cache) (%compute-emf info args)))))
        (when (and (%gf-slot info +gf-keyp+) (not (eq (cdr entry) :any)) (> nargs nfixed))
          (%check-call-keys info (cdr entry) (nthcdr nfixed args)))
        (funcall (car entry) args)))))

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

(defun %compute-emf (info args)
  "Returns (emf . valid-keys)."
  (let* ((gf (%gf-slot info +gf-function+))
         (methods (%compute-applicable-methods gf args))
         (gfll (%gf-slot info +gf-ll+)))
    (cons (if (null methods)
              (lambda (args) (apply #'no-applicable-method gf args))
              (%effective-method-function gf info methods))
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
(defun %call-next-method (args next cnm-args cell)
  (when (and cnm-args (car cell))
    (%check-cnm-args (car cell) args cnm-args))
  (if (null next)
      (let ((m (car cell)))
        (apply #'no-next-method (and m (%method-gf m)) m (or cnm-args args)))
      (funcall (car next) (or cnm-args args) (cdr next))))

(defun %check-cnm-args (method args cnm-args)
  (let ((gf (%method-gf method)))
    (when gf
      (unless (equal (%compute-applicable-methods gf args) (%compute-applicable-methods gf cnm-args))
        (error 'simple-error
               :format-control "CALL-NEXT-METHOD: the arguments ~S change the set of applicable methods of ~S"
               :format-arguments (list cnm-args gf))))))

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

(defmacro defmethod (name &rest args)
  (multiple-value-bind (quals ll body) (%split-method-args args)
    (let ((cell (gensym "CELL")))
      (multiple-value-bind (lam specs plain-ll doc) (%method-lambda name ll body cell)
        `(let ((,cell (list nil)))
           (%defmethod ',name ',quals
                       (list ,@(mapcar (lambda (s) (if (and (consp s) (eq (car s) 'eql))
                                                       `(list 'eql ,(cadr s))
                                                       `',s))
                                       specs))
                       ',plain-ll
                       (function ,lam)
                       ,doc
                       ,cell))))))

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
      `(let ((,gf (%defgeneric ',name ',lambda-list ',combination ,doc ',apo ',gfclass ',mclass ',decls)))
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
    "Returns (values slotd-form readers writers name)."
    (let* ((spec (if (consp spec) spec (list spec)))
           (name (car spec)) (opts (cdr spec))
           (initargs nil) (initform nil) (initformp nil) (readers nil) (writers nil)
           (allocation :instance) (type t) (doc nil) (seen nil))
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
            (t (%program-error "DEFCLASS ~S: unknown slot option ~S" class-name key)))))
      (values `(%make-slotd ',name :initargs ',(reverse initargs) :initform ',initform
                            :initfunction ,(if initformp `(lambda () ,initform) nil)
                            :readers ',(reverse readers) :writers ',(reverse writers) :allocation ',allocation
                            :type ',type :doc ',doc)
              (reverse readers) (reverse writers) name))))

(defmacro defclass (name supers slots &rest options)
  (let ((slotd-forms nil) (accessor-forms nil) (default-initargs nil) (metaclass nil) (doc nil)
        (seen nil) (slot-names nil))
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
          (push `(defmethod ,r ((object ,name)) (slot-value object ',sname)) accessor-forms))
        (dolist (w writers)
          (push `(defmethod ,w (new-value (object ,name)) (setf (slot-value object ',sname) new-value))
                accessor-forms))))
    (dolist (o options)
      (unless (and (consp o) (member (car o) '(:default-initargs :metaclass :documentation)))
        (%program-error "DEFCLASS ~S: unknown class option ~S" name o))
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
        (:documentation (setq doc (cadr o)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%put ',name '%defclass t))
       (%ensure-class ',name ',supers (list ,@(reverse slotd-forms))
                      (list ,@(let ((r nil))
                                (do ((l default-initargs (cddr l))) ((null l) (reverse r))
                                  (push `',(car l) r) (push `(lambda () ,(cadr l)) r))))
                      ',metaclass ,doc)
       ,@(reverse accessor-forms)
       (find-class ',name))))

(defun %ensure-class (name supers direct-slots default-initargs metaclass &optional doc)
  (let ((names (mapcar #'%slotd-name direct-slots)))
    (unless (= (length names) (length (remove-duplicates names)))
      (%program-error "DEFCLASS ~S: duplicate slot names" name)))
  (let* ((super-classes (mapcar (lambda (s)
                                  (if (%class-p s) s
                                      (or (find-class s nil)
                                          (%ensure-class-object s (find-class 'forward-referenced-class)
                                                                nil nil :forward))))
                                (or supers '(standard-object))))
         (meta (cond ((null metaclass) *the-standard-class*)
                     ((%class-p metaclass) metaclass)
                     (t (find-class metaclass))))
         (kind (cond ((some (lambda (c) (eq (%class-kind c) :condition)) super-classes) :condition)
                     ((some (lambda (c) (and (%subclassp c (find-class 'class)) (not (eq (%class-kind c) :forward))))
                            super-classes)
                      :metaclass)
                     (t :standard)))
         (existing (find-class name nil)))
    (when (and existing (eq (%class-kind existing) :forward) (not (eq meta (%instance-layout existing))))
      (%set-instance-layout existing meta))
    (let ((class (%ensure-class-object name meta super-classes direct-slots kind)))
      (%set-class-slot class +class-default-initargs+ default-initargs)
      (%set-class-slot class +class-doc+ doc)
      class)))

(defun ensure-class (name &key direct-superclasses direct-slots metaclass direct-default-initargs
                            documentation &allow-other-keys)
  (%ensure-class name (mapcar (lambda (c) (if (%class-p c) c c)) direct-superclasses)
                 (mapcar (lambda (s) (if (vectorp s) s (apply #'%make-slotd (getf s :name) :allow-other-keys t s)))
                         direct-slots)
                 (let ((r nil))
                   (dolist (d direct-default-initargs (nreverse r))
                     (push (car d) r) (push (or (caddr d) (let ((form (cadr d))) (lambda () (eval form)))) r)))
                 metaclass documentation))

;;; a class finalization invalidates the initarg caches
(setq *class-finalization-hook*
      (lambda (class) (declare (ignore class)) (when *initarg-cache* (clrhash *initarg-cache*))))

;;; ------------------------------------------------------------
;;; slots
;;; ------------------------------------------------------------

(defun slot-value (object slot-name) (%slot-value-by-name object slot-name))
(defun %set-slot-value (object slot-name value) (%set-slot-value-by-name object slot-name value))

(defun slot-boundp (object slot-name)
  (let ((loc (if (%instancep object) (%slot-location object slot-name) nil)))
    (cond ((fixnump loc) (not (%unboundp (%instance-ref object loc))))
          ((consp loc) (not (%unboundp (cdr loc))))
          (t (values (%slot-missing object slot-name 'slot-boundp nil))))))

(defun slot-makunbound (object slot-name)
  (let ((loc (if (%instancep object) (%slot-location object slot-name) nil)))
    (cond ((fixnump loc) (%set-instance-ref object loc (%unbound-marker)))
          ((consp loc) (setf (cdr loc) (%unbound-marker)))
          (t (%slot-missing object slot-name 'slot-makunbound nil)))
    object))

(defun slot-exists-p (object slot-name)
  (and (%instancep object) (not (null (%slot-location object slot-name)))))

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
  (apply #'make-instance (find-class class) initargs))

(defun %make-instance-of (class initargs)
  (when (eq (%class-kind class) :forward)
    (error 'simple-error :format-control "Cannot instantiate the forward-referenced class ~S"
                         :format-arguments (list (%class-name class))))
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
(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (%check-plist initargs 'shared-initialize)
  (let ((class (class-of instance)))
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

;;; ------------------------------------------------------------
;;; documentation
;;; ------------------------------------------------------------

(defvar *documentation-table* (make-hash-table :test 'eq))

(defun %doc-key (x doc-type)
  (if (and (functionp x) (eq doc-type 'function)) t doc-type))

(defun %get-doc (x doc-type)
  (let ((doc-type (%doc-key x doc-type)))
    (if (symbolp x)
        (let ((d (get x '%documentation)))
          (cdr (assoc doc-type (if (listp d) d nil))))
        (cdr (assoc doc-type (gethash x *documentation-table*))))))

(defun %set-doc (x doc-type value)
  (let ((doc-type (%doc-key x doc-type)))
    (if (symbolp x)
        (let ((d (get x '%documentation)))
          (%put x '%documentation (acons doc-type value (remove doc-type (if (listp d) d nil) :key #'car))))
        (setf (gethash x *documentation-table*)
              (acons doc-type value (remove doc-type (gethash x *documentation-table*) :key #'car))))
    value))

(defgeneric documentation (x doc-type))
(defmethod documentation ((x t) doc-type) (%get-doc x doc-type))
(defmethod documentation ((x generic-function) doc-type)
  (or (%get-doc x doc-type)
      (and (member doc-type '(t function)) (%gf-slot (%gf-info x) +gf-doc+))))
(defmethod documentation ((x class) doc-type)
  (or (%get-doc x doc-type)
      (and (member doc-type '(t type)) (%class-slot x +class-doc+))))
(defmethod documentation ((x method) doc-type)
  (or (%get-doc x doc-type)
      (and (eq doc-type t) (%instance-ref x 5))))
(defmethod documentation ((x symbol) doc-type)
  (or (%get-doc x doc-type)
      (and (eq doc-type 'type) (find-class x nil) (%class-slot (find-class x) +class-doc+))
      (and (eq doc-type 'function) (fboundp x) (%generic-function-p (fdefinition x))
           (%gf-slot (%gf-info (fdefinition x)) +gf-doc+))))

(defgeneric (setf documentation) (new-value x doc-type))
(defmethod (setf documentation) (new-value (x t) doc-type) (%set-doc x doc-type new-value))

(defun %set-documentation (x doc-type value)
  (funcall #'(setf documentation) value x doc-type))

;;; ------------------------------------------------------------
;;; introspection (subset of the MOP)
;;; ------------------------------------------------------------

(defun class-direct-superclasses (c) (%class-slot c +class-supers+))
(defun class-direct-subclasses (c) (%class-slot c +class-subclasses+))
(defun class-precedence-list (c) (%class-cpl c))
(defun class-slots (c) (%class-effective-slots c))
(defun class-direct-slots (c) (%class-slot c +class-direct-slots+))
(defun class-finalized-p (c) (%class-slot c +class-finalized+))
(defun finalize-inheritance (c) (%finalize-class c))
(defun class-default-initargs (c) (%class-default-initargs-list c))
(defun class-direct-default-initargs (c) (%class-slot c +class-default-initargs+))
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
(defun class-prototype (c) (%class-prototype c))
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
