;;;; clos.lisp — CLOS: classes, generic functions, methods

;;; ------------------------------------------------------------
;;; generic function metaobjects
;;; ------------------------------------------------------------
;;; A generic function is a closure whose name field holds a GF-INFO
;;; instance (class standard-generic-function) with slots:
;;;  0 name 1 lambda-list 2 methods 3 combination 4 cache 5 nreq 6 has-eql 7 doc 8 function

(defconstant +gf-name+ 0) (defconstant +gf-ll+ 1) (defconstant +gf-methods+ 2)
(defconstant +gf-combination+ 3) (defconstant +gf-cache+ 4) (defconstant +gf-nreq+ 5)
(defconstant +gf-has-eql+ 6) (defconstant +gf-doc+ 7) (defconstant +gf-function+ 8)

;;; method: 0 gf 1 qualifiers 2 specializers 3 function 4 lambda-list 5 doc
(defun %method-qualifiers (m) (%instance-ref m 1))
(defun %method-specializers (m) (%instance-ref m 2))
(defun %method-function (m) (%instance-ref m 3))

(defun %make-method-object (qualifiers specializers function lambda-list)
  (%make-instance (find-class 'standard-method)
                  (vector nil qualifiers specializers function lambda-list nil)))

(defun %gf-info (f)
  (let ((info (and (functionp f) (%closure-name f))))
    (if (and (%instancep info) (eq (%instance-layout info) (find-class 'standard-generic-function)))
        info nil)))

(defun %generic-function-p (f) (not (null (%gf-info f))))

(defun %lambda-list-nreq (ll)
  (let ((n 0))
    (dolist (x ll n)
      (when (member x lambda-list-keywords) (return n))
      (setq n (+ n 1)))))

(defun %make-generic-function (name lambda-list combination)
  (let* ((info (%make-instance (find-class 'standard-generic-function)
                               (vector name lambda-list nil (or combination '(standard))
                                       (make-hash-table :test 'equal) (%lambda-list-nreq lambda-list)
                                       nil nil nil)))
         (f (lambda (&rest args) (%gf-invoke info args))))
    (%set-closure-name f info)
    (%set-instance-ref info +gf-function+ f)
    f))

(defun ensure-generic-function (name &key lambda-list method-combination documentation
                                       generic-function-class method-class argument-precedence-order
                                       declarations environment)
  (declare (ignore generic-function-class method-class argument-precedence-order declarations environment))
  (when (and (symbolp name)
             (or (special-operator-p name) (macro-function name)
                 (and (fboundp name) (not (%generic-function-p (fdefinition name))))))
    (error 'simple-program-error
           :format-control "~S already names an ordinary function, macro or special operator"
           :format-arguments (list name)))
  (when (and (consp name) (fboundp name) (not (%generic-function-p (fdefinition name))))
    (error 'simple-program-error :format-control "~S already names an ordinary function"
                                 :format-arguments (list name)))
  (let ((existing (and (fboundp name) (fdefinition name))))
    (if (and existing (%generic-function-p existing))
        (let ((info (%gf-info existing)))
          (when lambda-list
            (%set-instance-ref info +gf-ll+ lambda-list)
            (%set-instance-ref info +gf-nreq+ (%lambda-list-nreq lambda-list)))
          (when method-combination (%set-instance-ref info +gf-combination+ method-combination))
          (when documentation (%set-instance-ref info +gf-doc+ documentation))
          existing)
        (let ((f (%make-generic-function name lambda-list method-combination)))
          (%set-fdefinition name f)
          f))))

(defun %ensure-gf-for-method (name lambda-list)
  (let ((existing (and (fboundp name) (fdefinition name))))
    (if (and existing (%generic-function-p existing))
        existing
        (ensure-generic-function name :lambda-list (%generic-lambda-list lambda-list)))))

(defun %generic-lambda-list (specialized-ll)
  (let ((r nil) (req t))
    (dolist (x specialized-ll (reverse r))
      (when (member x lambda-list-keywords) (setq req nil))
      (push (if (and req (consp x)) (car x) (if (consp x) (car x) x)) r))))

;;; specializers: class objects or (eql value)
(defun %parse-specializer (s)
  (cond ((and (consp s) (eq (car s) 'eql)) s)
        ((%class-p s) s)
        ((symbolp s) (or (find-class s nil)
                         (%ensure-class-object s *the-standard-class* (list (find-class 'standard-object)) nil :forward)))
        (t (error "Bad specializer ~S" s))))

(defun add-method (gf method)
  (let* ((info (%gf-info gf))
         (specs (%method-specializers method))
         (quals (%method-qualifiers method))
         (old (find-if (lambda (m) (and (equal (%method-qualifiers m) quals)
                                        (every #'%specializer-equal (%method-specializers m) specs)))
                       (%instance-ref info +gf-methods+))))
    (%set-instance-ref method 0 gf)
    (%set-instance-ref info +gf-methods+
                       (cons method (remove old (%instance-ref info +gf-methods+))))
    (when (some #'consp specs) (%set-instance-ref info +gf-has-eql+ t))
    (clrhash (%instance-ref info +gf-cache+))
    gf))

(defun %specializer-equal (a b)
  (if (consp a) (and (consp b) (eql (cadr a) (cadr b))) (eq a b)))

(defun remove-method (gf method)
  (let ((info (%gf-info gf)))
    (%set-instance-ref info +gf-methods+ (remove method (%instance-ref info +gf-methods+)))
    (clrhash (%instance-ref info +gf-cache+))
    gf))

(defun find-method (gf qualifiers specializers &optional (errorp t))
  (let* ((specs (mapcar #'%parse-specializer specializers))
         (m (find-if (lambda (m) (and (equal (%method-qualifiers m) qualifiers)
                                      (every #'%specializer-equal (%method-specializers m) specs)))
                     (%instance-ref (%gf-info gf) +gf-methods+))))
    (if (or m (not errorp)) m (error "No method found"))))

(defun %add-method-to (name qualifiers specializers lambda-list function)
  (let ((gf (%ensure-gf-for-method name lambda-list))
        (m (%make-method-object qualifiers (mapcar #'%parse-specializer specializers) function lambda-list)))
    (add-method gf m)
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

(defun %method-more-specific-p (m1 m2 args)
  (do ((s1 (%method-specializers m1) (cdr s1)) (s2 (%method-specializers m2) (cdr s2)) (a args (cdr a)))
      ((null s1) nil)
    (cond ((%specializer-more-specific-p (car s1) (car s2) (car a)) (return t))
          ((%specializer-more-specific-p (car s2) (car s1) (car a)) (return nil)))))

(defun compute-applicable-methods (gf args)
  (let ((ms (remove-if-not (lambda (m) (%method-applicable-p m args))
                           (%instance-ref (%gf-info gf) +gf-methods+))))
    (sort (copy-list ms) (lambda (a b) (%method-more-specific-p a b args)))))

(defun %gf-invoke (info args)
  (let* ((nreq (%instance-ref info +gf-nreq+))
         (emf (if (%instance-ref info +gf-has-eql+)
                  (%compute-emf info args)
                  (let ((key (let ((r nil) (a args))
                               (dotimes (i nreq) (push (class-of (car a)) r) (setq a (cdr a)))
                               r))
                        (cache (%instance-ref info +gf-cache+)))
                    (or (gethash key cache)
                        (setf (gethash key cache) (%compute-emf info args)))))))
    (funcall emf args)))

(defun %compute-emf (info args)
  (let* ((gf (%instance-ref info +gf-function+))
         (methods (compute-applicable-methods gf args)))
    (if (null methods)
        (lambda (args) (apply #'no-applicable-method gf args))
        (let ((comb (%instance-ref info +gf-combination+)))
          (if (eq (car comb) 'standard)
              (%standard-emf gf methods)
              (%operator-emf gf methods (car comb) (member :most-specific-last (cdr comb))))))))

(defun %standard-emf (gf methods)
  (let ((around nil) (before nil) (primary nil) (after nil))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((null q) (push m primary))
              ((equal q '(:around)) (push m around))
              ((equal q '(:before)) (push m before))
              ((equal q '(:after)) (push m after))
              (t (error "Unsupported method qualifiers ~S" q)))))
    (setq around (nreverse around) before (nreverse before) primary (nreverse primary))
    ;; AFTER stays reversed (least specific first)
    (if (null primary)
        (lambda (args) (apply #'no-applicable-method gf args))
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

(defun %operator-emf (gf methods op most-specific-last)
  (let ((around nil) (primary nil))
    (dolist (m methods)
      (let ((q (%method-qualifiers m)))
        (cond ((equal q '(:around)) (push m around))
              ((equal q (list op)) (push m primary))
              (t (error "Method ~S has qualifiers ~S, which are invalid for the ~S method combination"
                        m q op)))))
    (setq around (nreverse around) primary (nreverse primary))
    (when most-specific-last (setq primary (reverse primary)))
    (if (null primary)
        (lambda (args) (apply #'no-applicable-method gf args))
        (let* ((pfns (mapcar #'%method-function primary))
               (inner (lambda (args next)
                        (declare (ignore next))
                        (let ((vals (mapcar (lambda (f) (funcall f args nil)) pfns)))
                          (case op
                            (progn (car (last vals)))
                            (and (every #'identity vals) (car (last vals)))
                            (or (some #'identity vals))
                            (t (apply op vals)))))))
          (if around
              (let ((chain (append (mapcar #'%method-function around) (list inner))))
                (lambda (args) (funcall (car chain) args (cdr chain))))
              (lambda (args) (funcall inner args nil)))))))

(defun %call-next-method (args next cnm-args)
  (if (null next)
      (apply #'no-next-method nil nil (or cnm-args args))
      (funcall (car next) (or cnm-args args) (cdr next))))

;;; ------------------------------------------------------------
;;; defgeneric / defmethod
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %split-method-args (args)
    "Returns (values qualifiers specialized-lambda-list body)."
    (let ((quals nil))
      (do () ((listp (car args)))
        (push (pop args) quals))
      (values (reverse quals) (car args) (cdr args))))

  (defun %method-lambda (name ll body)
    (let ((plain-ll nil) (specs nil) (req t) (args (gensym "ARGS")) (next (gensym "NEXT")))
      (dolist (x ll)
        (when (member x lambda-list-keywords) (setq req nil))
        (if (and req (consp x))
            (progn (push (car x) plain-ll) (push (cadr x) specs))
            (progn (push x plain-ll) (when req (push t specs)))))
      (setq plain-ll (reverse plain-ll) specs (reverse specs))
      (multiple-value-bind (decls forms) (%split-decls body)
        (let ((doc (when (and (stringp (car forms)) (cdr forms)) (car forms))))
          (when doc (setq forms (cdr forms)))
          (values
           `(lambda (,args ,next)
              (flet ((call-next-method (&rest cnm-args) (%call-next-method ,args ,next cnm-args))
                     (next-method-p () (not (null ,next))))
                (apply (lambda ,plain-ll
                         ,@decls
                         (declare (ignorable ,@(remove-if (lambda (v) (member v lambda-list-keywords))
                                                          (mapcar (lambda (v) (if (consp v) (car v) v))
                                                                  (subseq plain-ll 0 (length specs))))))
                         (block ,(if (consp name) (cadr name) name) ,@forms))
                       ,args)))
           specs
           plain-ll))))))

(defmacro defmethod (name &rest args)
  (multiple-value-bind (quals ll body) (%split-method-args args)
    (multiple-value-bind (lam specs plain-ll) (%method-lambda name ll body)
      `(%add-method-to ',name ',quals
                       (list ,@(mapcar (lambda (s) (if (and (consp s) (eq (car s) 'eql))
                                                       `(list 'eql ,(cadr s))
                                                       `',s))
                                       specs))
                       ',plain-ll
                       (function ,lam)))))

(defmacro defgeneric (name lambda-list &rest options)
  (let ((combination nil) (methods nil) (doc nil))
    (dolist (o options)
      (case (car o)
        (:method-combination (setq combination (cdr o)))
        (:method (push `(defmethod ,name ,@(cdr o)) methods))
        (:documentation (setq doc (cadr o)))))
    `(progn
       (ensure-generic-function ',name :lambda-list ',lambda-list
                                       :method-combination ',combination
                                       :documentation ,doc)
       ,@(reverse methods)
       (fdefinition ',name))))

(defmacro define-method-combination (name &rest options)
  (declare (ignore options))
  `',name)

(defmacro call-method (method &optional next-methods)
  (declare (ignore next-methods))
  `(funcall (%method-function ,method) args nil))

;;; ------------------------------------------------------------
;;; defclass
;;; ------------------------------------------------------------

(defmacro defclass (name supers slots &rest options)
  (let ((slotd-forms nil) (accessor-forms nil) (default-initargs nil) (metaclass nil) (doc nil))
    (dolist (s slots)
      (multiple-value-bind (form readers writers sname) (%parse-slot-spec s)
        (push form slotd-forms)
        (dolist (r readers)
          (push `(defmethod ,r ((object ,name)) (slot-value object ',sname)) accessor-forms))
        (dolist (w writers)
          (push `(defmethod ,w (new-value (object ,name)) (setf (slot-value object ',sname) new-value))
                accessor-forms))))
    (dolist (o options)
      (case (car o)
        (:default-initargs (setq default-initargs (cdr o)))
        (:metaclass (setq metaclass (cadr o)))
        (:documentation (setq doc (cadr o)))))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%put ',name '%defclass t))
       (%ensure-class ',name ',supers (list ,@(reverse slotd-forms))
                      (list ,@(let ((r nil))
                                (do ((l default-initargs (cddr l))) ((null l) (reverse r))
                                  (push `',(car l) r) (push `(lambda () ,(cadr l)) r))))
                      ',metaclass)
       ,@(reverse accessor-forms)
       (find-class ',name))))

(defun %ensure-class (name supers direct-slots default-initargs metaclass)
  (let ((names (mapcar #'%slotd-name direct-slots)))
    (unless (= (length names) (length (remove-duplicates names)))
      (error 'simple-program-error :format-control "DEFCLASS ~S: duplicate slot names"
                                   :format-arguments (list name))))
  (let* ((super-classes (mapcar (lambda (s)
                                  (or (find-class s nil)
                                      (%ensure-class-object s *the-standard-class*
                                                            (list (find-class 'standard-object)) nil :forward)))
                                (or supers '(standard-object))))
         (kind (if (some (lambda (c) (eq (%class-kind c) :condition)) super-classes) :condition :standard))
         (meta (if metaclass (find-class metaclass) *the-standard-class*))
         (class (%ensure-class-object name meta super-classes direct-slots kind)))
    (%set-class-slot class +class-default-initargs+ default-initargs)
    class))

(defun ensure-class (name &key direct-superclasses direct-slots metaclass &allow-other-keys)
  (%ensure-class name (mapcar (lambda (c) (if (%class-p c) (class-name c) c)) direct-superclasses)
                 (mapcar (lambda (s) (apply #'%make-slotd (getf s :name) s)) direct-slots)
                 nil metaclass))

;;; ------------------------------------------------------------
;;; slots
;;; ------------------------------------------------------------

(defun slot-value (object slot-name) (%slot-value-by-name object slot-name))
(defun %set-slot-value (object slot-name value) (%set-slot-value-by-name object slot-name value))
(defun slot-boundp (object slot-name)
  (let ((loc (%slot-location object slot-name)))
    (cond ((fixnump loc) (not (%unboundp (%instance-ref object loc))))
          ((consp loc) (not (%unboundp (%svref (%class-slot (%instance-layout object) +class-extra+) (cdr loc)))))
          (t (%slot-missing object slot-name 'slot-boundp)))))
(defun slot-makunbound (object slot-name)
  (let ((loc (%slot-location object slot-name)))
    (when (fixnump loc) (%set-instance-ref object loc (%unbound-marker)))
    object))
(defun slot-exists-p (object slot-name)
  (and (%instancep object) (not (null (%slot-location object slot-name)))))

(defmacro with-slots (slots instance &body body)
  (let ((g (gensym "INSTANCE")))
    `(let ((,g ,instance))
       (symbol-macrolet ,(mapcar (lambda (s)
                                   (let ((var (if (consp s) (car s) s)) (slot (if (consp s) (cadr s) s)))
                                     `(,var (slot-value ,g ',slot))))
                                 slots)
         ,@body))))

(defmacro with-accessors (bindings instance &body body)
  (let ((g (gensym "INSTANCE")))
    `(let ((,g ,instance))
       (symbol-macrolet ,(mapcar (lambda (b) `(,(car b) (,(cadr b) ,g))) bindings)
         ,@body))))

;;; ------------------------------------------------------------
;;; standard generic functions
;;; ------------------------------------------------------------

(defgeneric no-applicable-method (gf &rest args))
(defmethod no-applicable-method (gf &rest args)
  (error "There is no applicable method for the generic function ~S when called with arguments ~S."
         gf args))

(defgeneric no-next-method (gf method &rest args))
(defmethod no-next-method (gf method &rest args)
  (declare (ignore gf method))
  (error "There is no next method (arguments ~S)." args))

(defgeneric allocate-instance (class &rest initargs))
(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (%allocate-instance class))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))
(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))
(defmethod make-instance ((class standard-class) &rest initargs)
  (when (eq (%class-kind class) :forward)
    (error "Cannot instantiate the forward-referenced class ~S" (class-name class)))
  (let* ((all-initargs (append initargs (%class-default-initargs-all class)))
         (instance (apply #'allocate-instance class all-initargs)))
    (apply #'initialize-instance instance all-initargs)
    instance))

(defgeneric initialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod initialize-instance ((instance t) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &rest initargs &key &allow-other-keys))
(defmethod reinitialize-instance ((instance t) &rest initargs)
  (apply #'shared-initialize instance nil initargs))

(defgeneric shared-initialize (instance slot-names &rest initargs &key &allow-other-keys))
(defmethod shared-initialize ((instance t) slot-names &rest initargs)
  (let ((class (%instance-layout instance)))
    (dolist (sd (%class-effective-slots class))
      (let ((loc (%slotd-location sd)) (found nil) (value nil))
        (do ((l initargs (cddr l))) ((or found (atom l)))
          (when (member (car l) (%slotd-initargs sd))
            (setq found t value (cadr l))))
        (cond (found
               (if (fixnump loc) (%set-instance-ref instance loc value)
                   (%set-svref (%class-slot class +class-extra+) (cdr loc) value)))
              ((and (fixnump loc) (%unboundp (%instance-ref instance loc)) (%slotd-initfunction sd)
                    (or (eq slot-names t) (member (%slotd-name sd) slot-names)))
               (%set-instance-ref instance loc (funcall (%slotd-initfunction sd)))))))
    instance))

(defgeneric change-class (instance new-class &rest initargs))
(defmethod change-class ((instance t) new-class &rest initargs)
  (let* ((new-class (if (symbolp new-class) (find-class new-class) new-class))
         (new (%allocate-instance new-class)))
    (dolist (sd (%class-effective-slots new-class))
      (let ((name (%slotd-name sd)))
        (when (and (slot-exists-p instance name) (slot-boundp instance name))
          (%set-slot-value-by-name new name (slot-value instance name)))))
    (%set-instance-layout instance new-class)
    (%set-instance-slots instance (%instance-slots new))
    (apply #'shared-initialize instance t initargs)
    instance))

(defgeneric print-object (object stream))
(defmethod print-object ((object t) stream)
  (%default-print-instance object stream)
  object)


(defgeneric make-load-form (object &optional environment))
(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore slot-names environment))
  (values `(allocate-instance (class-of ',object)) nil))


;;; the printer calls PRINT-OBJECT for instances
(setq *print-object-hook*
      (lambda (x stream)
        (print-object x stream)))

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
(defun class-default-initargs (c) (%class-slot c +class-default-initargs+))
(defun slot-definition-name (s) (%slotd-name s))
(defun slot-definition-initargs (s) (%slotd-initargs s))
(defun slot-definition-initform (s) (%slotd-initform s))
(defun slot-definition-allocation (s) (%slotd-allocation s))
(defun slot-definition-readers (s) (%slotd-readers s))
(defun slot-definition-writers (s) (%slotd-writers s))
(defun slot-definition-type (s) (%slotd-type s))
(defun generic-function-name (gf) (%instance-ref (%gf-info gf) +gf-name+))
(defun generic-function-methods (gf) (%instance-ref (%gf-info gf) +gf-methods+))
(defun generic-function-lambda-list (gf) (%instance-ref (%gf-info gf) +gf-ll+))
(defun method-qualifiers (m) (%method-qualifiers m))
(defun method-specializers (m) (%method-specializers m))
(defun method-function (m) (%method-function m))
(defun method-generic-function (m) (%instance-ref m 0))
(defun class-prototype (c) (%allocate-instance c))

(defmethod print-object ((c class) stream)
  (print-unreadable-object (c stream :type t)
    (prin1 (class-name c) stream))
  c)
(defmethod print-object ((m standard-method) stream)
  (print-unreadable-object (m stream :type t)
    (format stream "~S ~{~S ~}~S"
            (let ((gf (%instance-ref m 0))) (if gf (generic-function-name gf) nil))
            (%method-qualifiers m)
            (mapcar (lambda (s) (if (consp s) s (class-name s))) (%method-specializers m))))
  m)
