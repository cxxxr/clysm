;;;; class0.lisp — class objects (shared by defstruct, conditions and CLOS)
;;;;
;;;; Every instance is a WasmGC struct $instance {layout, slots}.  The layout is
;;;; the class object, itself an instance whose layout is its metaclass.

(defconstant +class-name+ 0)
(defconstant +class-supers+ 1)
(defconstant +class-direct-slots+ 2)
(defconstant +class-cpl+ 3)
(defconstant +class-slots+ 4)
(defconstant +class-subclasses+ 5)
(defconstant +class-finalized+ 6)
(defconstant +class-kind+ 7)
(defconstant +class-slot-index+ 8)
(defconstant +class-default-initargs+ 9)
(defconstant +class-doc+ 10)
(defconstant +class-extra+ 11)
(defconstant +class-shared+ 12)     ; alist slot-name -> cell (name . value) of slots this class allocates
(defconstant +class-prototype+ 13)
(defconstant +class-direct-default-initargs+ 14) ; ((key form fn) ...) as given to DEFCLASS
(defconstant +class-protocol+ 15)   ; true: the metaclass is not a standard one (MOP generic protocols apply)
(defconstant +class-plist+ 16)
(defconstant +class-make-info+ 17)  ; what MAKE-INSTANCE does for the class (clos.lisp)
(defconstant +class-instance-size+ 18) ; the number of slots in an instance (set by the finalization)
(defconstant +class-size+ 19)

(defvar *the-standard-class* nil)
(defvar *the-direct-slotd-class* nil)
(defvar *the-effective-slotd-class* nil)
;;; classes whose instances have a fixed layout of raw slots (they come first)
(defvar *raw-layout-classes* nil)
;;; the metaclasses defined by the implementation; classes of other
;;; metaclasses go through the generic functions of the metaobject protocol
(defvar *standard-metaclasses* nil)
(defvar *compute-slots-hook* nil)

(defun %class-p (x)
  (and (%instancep x)
       (let ((meta (%instance-layout x)))
         (and (%instancep meta) (not (null (%class-slot meta +class-kind+)))
              (eq (%class-slot meta +class-kind+) :metaclass)))))

(defun %class-slot (c i) (%instance-ref c i))
(defun %set-class-slot (c i v) (%set-instance-ref c i v))

(defun %allocate-class-object (metaclass)
  (let ((n (if (%class-slot metaclass +class-slots+)
               (max +class-size+ (%instance-slot-count metaclass))
               +class-size+)))
    (let ((v (%make-simple-vector n nil)))
      ;; the slots of a user-defined metaclass start unbound
      (do ((i +class-size+ (+ i 1))) ((>= i n)) (setf (svref v i) (%unbound-marker)))
      (%make-instance metaclass v))))

(defun %class-name (c) (%class-slot c +class-name+))
(defun %class-cpl (c) (%class-slot c +class-cpl+))
(defun %class-kind (c) (%class-slot c +class-kind+))
(defun %class-effective-slots (c) (%class-slot c +class-slots+))

;;; slot definitions: instances of (subclasses of) SLOT-DEFINITION whose
;;; first slots are
;;; name initargs initform initfunction readers writers allocation type documentation location
(defconstant +slotd-size+ 10)
(defun %make-slotd (name &key initargs initform initfunction readers writers (allocation :instance) (type t) doc)
  (%make-instance *the-direct-slotd-class*
                  (vector name initargs initform initfunction readers writers allocation type doc nil)))
(defun %slotd-name (s) (%instance-ref s 0))
(defun %slotd-initargs (s) (%instance-ref s 1))
(defun %slotd-initform (s) (%instance-ref s 2))
(defun %slotd-initfunction (s) (%instance-ref s 3))
(defun %slotd-readers (s) (%instance-ref s 4))
(defun %slotd-writers (s) (%instance-ref s 5))
(defun %slotd-allocation (s) (%instance-ref s 6))
(defun %slotd-type (s) (%instance-ref s 7))
(defun %slotd-doc (s) (%instance-ref s 8))
(defun %slotd-location (s) (%instance-ref s 9))
(defun %set-slotd (s i v) (%set-instance-ref s i v))
(defun %slotd-p (x)
  (and (%instancep x) (%instancep (%instance-layout x))
       (not (null (member (find-class 'slot-definition nil) (%class-cpl (%instance-layout x)))))))

(defun find-class (name &optional (errorp t) env)
  (declare (ignore env))
  (let ((c (and (symbolp name) (get name '%class))))
    (cond (c c)
          (errorp (error 'simple-error :format-control "There is no class named ~S."
                                       :format-arguments (list name)))
          (t nil))))

;;; (setf (find-class name [errorp [env]]) class): the new value comes last
(defvar *builtin-classes* (make-array 20 :initial-element nil))
(defun %set-find-class (name &rest args)
  (let ((class (car (last args))))
    (if (null class)
        (%remprop-class name)
        (%put name '%class class))
    (fill *builtin-classes* nil)
    (when (boundp '*ctor-generation*) (set '*ctor-generation* (+ (symbol-value '*ctor-generation*) 1)))
    class))

(defun %remprop-class (name)
  (let ((plist (symbol-plist name)) (r nil))
    (do ((l plist (cddr l))) ((null l))
      (unless (eq (car l) '%class) (push (car l) r) (push (cadr l) r)))
    (%set-symbol-plist name (nreverse r))))

;;; class precedence list: the CLOS topological sort (CLHS 4.3.5)
(defun %compute-cpl (class supers)
  (let ((classes (list class)) (pairs nil))
    (labels ((direct (c) (if (eq c class) supers (%class-slot c +class-supers+)))
             (walk (c)
               (dolist (s (direct c))
                 (unless (member s classes)
                   (setq classes (append classes (list s)))
                   (walk s)))))
      (walk class)
      (dolist (c classes)
        (let ((prev c))
          (dolist (s (direct c))
            (push (cons prev s) pairs)
            (setq prev s))))
      (let ((result nil) (remaining classes))
        (do () ((null remaining) (nreverse result))
          (let ((candidates
                  (remove-if (lambda (c)
                               (some (lambda (p) (and (eq (cdr p) c) (member (car p) remaining))) pairs))
                             remaining)))
            (when (null candidates)
              (error 'simple-error :format-control "Inconsistent class precedence list for ~S"
                                   :format-arguments (list (%class-name class))))
            (let ((choice (if (null (cdr candidates))
                              (car candidates)
                              (or (dolist (r result nil)
                                    ;; RESULT is reversed: most recently added first
                                    (let ((c (find-if (lambda (cand) (member cand (direct r))) candidates)))
                                      (when c (return c))))
                                  (car candidates)))))
              (push choice result)
              (setq remaining (remove choice remaining)))))))))

;;; effective slots (CLHS 7.5.3).  Inherited slots come first so that the
;;; locations of a superclass's slots are kept under single inheritance;
;;; the raw slots of metaobject classes come before all others.
(defun %effective-slot-names (cpl)
  (let ((names nil) (rcpl (reverse cpl)))
    (dolist (pass '(t nil) names)
      (dolist (c rcpl)
        (when (eq pass (not (null (member c *raw-layout-classes*))))
          (dolist (sd (%class-slot c +class-direct-slots+))
            (unless (member (%slotd-name sd) names) (setq names (append names (list (%slotd-name sd)))))))))))

(defun %direct-slotds-named (cpl name)
  "(class . direct-slotd) for the slot NAME, most specific first."
  (let ((direct nil))
    (dolist (c cpl)
      (let ((sd (find name (%class-slot c +class-direct-slots+) :key #'%slotd-name)))
        (when sd (push (cons c sd) direct))))
    (nreverse direct)))

(defun %effective-slotd-values (direct)
  "The raw slot values (a simple vector) of the effective slot computed from
DIRECT, a list (class . direct-slotd), most specific first.  The location is
not set."
  (let* ((first (cdar direct))
         (v (subseq (%instance-slots first) 0 +slotd-size+))
         (init (find-if (lambda (e) (%slotd-initfunction (cdr e))) direct))
         (docd (find-if (lambda (e) (%slotd-doc (cdr e))) direct))
         (types (remove t (remove-duplicates (mapcar (lambda (e) (%slotd-type (cdr e))) direct)
                                             :test #'equal))))
    (setf (svref v 1) (remove-duplicates (mapcan (lambda (e) (copy-list (%slotd-initargs (cdr e)))) direct)
                                         :from-end t))
    (setf (svref v 2) (if init (%slotd-initform (cdr init)) nil)
          (svref v 3) (if init (%slotd-initfunction (cdr init)) nil))
    (setf (svref v 7) (cond ((null types) t) ((null (cdr types)) (car types)) (t (cons 'and types))))
    (setf (svref v 8) (if docd (%slotd-doc (cdr docd)) nil))
    (setf (svref v 4) nil (svref v 5) nil (svref v 9) nil)
    v))

(defun %assign-slot-locations (class slots)
  "Instance slots get consecutive indices, class slots the shared cell of the
class that defines them."
  (let ((i 0) (cpl (%class-cpl class)))
    (dolist (eff slots slots)
      (let ((name (%slotd-name eff)))
        (if (eq (%slotd-allocation eff) :instance)
            (progn (%set-slotd eff 9 i) (setq i (+ i 1)))
            (let ((owner (car (find-if (lambda (e) (eq (%slotd-allocation (cdr e)) (%slotd-allocation eff)))
                                       (%direct-slotds-named cpl name)))))
              (%set-slotd eff 9 (%class-shared-cell (or owner class) name))))))))

(defun %compute-effective-slots (cpl)
  (let ((slots nil))
    (dolist (name (%effective-slot-names cpl))
      (push (%make-instance *the-effective-slotd-class* (%effective-slotd-values (%direct-slotds-named cpl name)))
            slots))
    (%assign-slot-locations (car cpl) (nreverse slots))))

(defun %class-shared-cell (class name)
  (or (cdr (assoc name (%class-slot class +class-shared+)))
      (let ((cell (cons name (%unbound-marker))))
        (%set-class-slot class +class-shared+ (acons name cell (%class-slot class +class-shared+)))
        cell)))

(defun %instance-slot-count (class)
  (let ((n 0))
    (dolist (s (%class-effective-slots class) n)
      (when (fixnump (%slotd-location s)) (setq n (%fx+ n 1))))))

(defun %change-layout (object class)
  "Make OBJECT an instance of CLASS, keeping its slot values (the slot vector
grows when CLASS has more instance slots)."
  (let* ((old (%instance-slots object))
         (n (if (%class-slot class +class-slots+) (%instance-slot-count class) 0)))
    (when (< (length old) n)
      (let ((new (%make-simple-vector n (%unbound-marker))))
        (dotimes (i (length old)) (setf (svref new i) (svref old i)))
        (%set-instance-slots object new)))
    (%set-instance-layout object class)
    object))

;;; PREPARE, when given, is called with the class object before its direct
;;; slots are set, and returns them (DIRECT-SLOTS is then ignored).
(defun %ensure-class-object (name metaclass supers direct-slots kind &optional prepare)
  (let ((class (or (let ((c (and name (find-class name nil))))
                     ;; reuse the class only while NAME is its proper name
                     (and c (eq (%class-name c) name) c))
                   (%allocate-class-object metaclass))))
    (if (eq (%instance-layout class) metaclass)
        nil
        (if (%instance-layout class)
            (%change-layout class metaclass)
            (%set-instance-layout class metaclass)))
    (%set-class-slot class +class-name+ name)
    (when prepare (setq direct-slots (funcall prepare class)))
    ;; drop the class from the subclass lists of its former superclasses
    (dolist (s (%class-slot class +class-supers+))
      (unless (member s supers)
        (%set-class-slot s +class-subclasses+ (remove class (%class-slot s +class-subclasses+)))))
    (%set-class-slot class +class-supers+ supers)
    (%set-class-slot class +class-direct-slots+ direct-slots)
    (%set-class-slot class +class-kind+ kind)
    (dolist (s supers)
      (unless (member class (%class-slot s +class-subclasses+))
        (%set-class-slot s +class-subclasses+ (cons class (%class-slot s +class-subclasses+)))))
    (%finalize-class class)
    (when name (%set-find-class name class))
    class))

(defvar *class-finalization-hook* nil)

(defun %same-slot-index-p (a b)
  (do ((a a (cdr a)) (b b (cdr b))) ((or (null a) (null b)) (and (null a) (null b)))
    (unless (and (eq (caar a) (caar b)) (eq (cdar a) (cdar b))) (return nil))))

(defun %finalize-class (class)
  (let* ((cpl (if (eq (%class-name class) t) (list class)
                  (%compute-cpl class (%class-slot class +class-supers+))))
         (old-cpl (%class-slot class +class-cpl+))
         (old-index (%class-slot class +class-slot-index+))
         ;; a class that was finalized gets another precedence list or other slot locations
         (changed (and old-cpl (not (equal cpl old-cpl)))))
    (%set-class-slot class +class-cpl+ cpl)
    ;; shared slots allocated by this class: keep the cells (and values) of
    ;; slots that stay shared, initialize new ones from their initforms
    (let ((old (%class-slot class +class-shared+)) (new nil))
      (dolist (sd (%class-slot class +class-direct-slots+))
        (unless (eq (%slotd-allocation sd) :instance)
          (let ((cell (cdr (assoc (%slotd-name sd) old))))
            (unless cell
              (setq cell (cons (%slotd-name sd) (%unbound-marker)))
              (when (%slotd-initfunction sd)
                (setf (cdr cell) (funcall (%slotd-initfunction sd)))))
            (push (cons (%slotd-name sd) cell) new))))
      (%set-class-slot class +class-shared+ (nreverse new)))
    (%set-class-slot class +class-protocol+
                     (and *standard-metaclasses*
                          (not (member (%instance-layout class) *standard-metaclasses*))))
    (let ((slots (if (and *compute-slots-hook* (%class-slot class +class-protocol+))
                     (funcall *compute-slots-hook* class)
                     (%compute-effective-slots cpl))))
      (%set-class-slot class +class-slots+ slots)
      ;; slot index: alist name -> location (the same object as long as the
      ;; locations are the same: caches of slot locations compare it)
      (let ((index (mapcar (lambda (s) (cons (%slotd-name s) (%slotd-location s))) slots)))
        (if (and old-cpl (%same-slot-index-p index old-index))
            nil
            (progn (when old-cpl (setq changed t))
                   (%set-class-slot class +class-slot-index+ index)))))
    (%set-class-slot class +class-instance-size+ (%instance-slot-count class))
    ;; a class with a forward-referenced superclass is not finalized (AMOP)
    (%set-class-slot class +class-finalized+
                     (not (some (lambda (c) (eq (%class-slot c +class-kind+) :forward)) cpl)))
    (%set-class-slot class +class-prototype+ nil)
    (when *class-finalization-hook* (funcall *class-finalization-hook* class changed))
    ;; refinalize subclasses
    (dolist (sub (%class-slot class +class-subclasses+))
      (%finalize-class sub))
    class))

(defun %subclassp (c1 c2) (or (eq c1 c2) (not (null (member c2 (%class-cpl c1))))))

;;; ------------------------------------------------------------
;;; slot access by name (used by conditions and CLOS)
;;; A location is a fixnum (index in the instance's slot vector) or the
;;; cons cell (name . value) holding a shared slot.
;;; ------------------------------------------------------------

(defun %slot-location (instance name)
  (let ((e (assoc name (%class-slot (%instance-layout instance) +class-slot-index+))))
    (if e (cdr e) nil)))

(defun %slot-value-by-name (instance name)
  (let ((loc (if (%instancep instance) (%slot-location instance name) nil)))
    (cond ((fixnump loc)
           (let ((v (%instance-ref instance loc)))
             (if (%unboundp v) (%slot-unbound instance name) v)))
          ((consp loc)
           (let ((v (cdr loc)))
             (if (%unboundp v) (%slot-unbound instance name) v)))
          (t (values (%slot-missing instance name 'slot-value nil))))))

(defun %set-slot-value-by-name (instance name value)
  (let ((loc (if (%instancep instance) (%slot-location instance name) nil)))
    (cond ((fixnump loc) (%set-instance-ref instance loc value))
          ((consp loc) (setf (cdr loc) value))
          (t (%slot-missing instance name 'setf value)))
    value))

(defun %slot-unbound (instance name)
  (if (fboundp 'slot-unbound)
      (values (funcall 'slot-unbound (class-of instance) instance name))
      (error 'unbound-slot :name name :instance instance)))

(defun %slot-missing (instance name op value)
  (if (fboundp 'slot-missing)
      (if (eq op 'setf)
          (funcall 'slot-missing (class-of instance) instance name op value)
          (funcall 'slot-missing (class-of instance) instance name op))
      (error 'simple-error :format-control "The slot ~S is missing from the object ~S."
                           :format-arguments (list name instance))))

(defun %allocate-instance (class)
  (let* ((n (or (%class-slot class +class-instance-size+) (%instance-slot-count class)))
         (v (%make-simple-vector n (%unbound-marker))))
    (%make-instance class v)))

(defun %initialize-slots-from-initargs (instance initargs)
  (let ((class (%instance-layout instance)))
    (dolist (sd (%class-effective-slots class))
      (let ((loc (%slotd-location sd)) (found nil) (value nil))
        (do ((l initargs (cddr l))) ((or found (atom l)))
          (when (member (car l) (%slotd-initargs sd))
            (setq found t value (cadr l))))
        (cond (found (if (fixnump loc) (%set-instance-ref instance loc value)
                         (setf (cdr loc) value)))
              ((and (fixnump loc) (%unboundp (%instance-ref instance loc)) (%slotd-initfunction sd))
               (%set-instance-ref instance loc (funcall (%slotd-initfunction sd)))))))
    instance))

;;; default initargs (CLHS 7.1.3): the most specific class's default for a key
;;; wins; only defaults whose key is not supplied are evaluated, in order.
(defun %class-default-initargs-list (class)
  "((key . function) ...) for CLASS, most specific first, without duplicate keys."
  (let ((r nil))
    (dolist (c (%class-cpl class) (nreverse r))
      (do ((l (%class-slot c +class-default-initargs+) (cddr l))) ((null l))
        (unless (assoc (car l) r) (push (cons (car l) (cadr l)) r))))))

(defun %add-default-initargs (class initargs)
  (let ((extra nil))
    (dolist (d (%class-default-initargs-list class))
      (unless (do ((l initargs (cddr l))) ((atom l) nil) (when (eq (car l) (car d)) (return t)))
        (push (car d) extra)
        (push (funcall (cdr d)) extra)))
    (if extra (append initargs (nreverse extra)) initargs)))

;;; bootstrap metaclasses: standard-class is an instance of itself
(defun %bootstrap-classes ()
  (let ((sc (%make-instance nil (%make-simple-vector +class-size+ nil))))
    (%set-instance-layout sc sc)
    (%set-class-slot sc +class-kind+ :metaclass)
    (%set-class-slot sc +class-name+ 'standard-class)
    (setq *the-standard-class* sc)
    (%set-find-class 'standard-class sc)
    (let ((top (%ensure-class-object 't sc nil nil :built-in)))
      (%ensure-class-object 'standard-object sc (list top) nil :standard)
      (%ensure-class-object 'structure-object sc (list top) nil :structure)
      (%ensure-class-object 'function sc (list top) nil :built-in)
      (%ensure-class-object 'metaobject sc (list (find-class 'standard-object)) nil :standard)
      (%ensure-class-object 'specializer sc (list (find-class 'metaobject)) nil :standard)
      (%ensure-class-object 'class sc (list (find-class 'specializer)) nil :standard)
      ;; standard-class proper
      (%set-class-slot sc +class-supers+ (list (find-class 'class)))
      (%set-class-slot (find-class 'class) +class-subclasses+ (list sc))
      (%finalize-class sc)
      (%set-class-slot sc +class-kind+ :metaclass)
      (dolist (m '(built-in-class structure-class funcallable-standard-class forward-referenced-class))
        (%ensure-class-object m sc (list (find-class 'class)) nil :metaclass))
      (let ((bic (find-class 'built-in-class)))
        (%set-instance-layout top bic)
        (%set-instance-layout (find-class 'function) bic))
      (%set-instance-layout (find-class 'structure-object) (find-class 'structure-class))
      (%ensure-class-object 'compiled-function (find-class 'built-in-class) (list (find-class 'function)) nil :built-in)
      (%ensure-class-object 'funcallable-standard-object sc
                            (list (find-class 'standard-object) (find-class 'compiled-function)) nil :standard)
      (%ensure-class-object 'generic-function (find-class 'funcallable-standard-class)
                            (list (find-class 'metaobject) (find-class 'funcallable-standard-object)) nil :standard)
      (%ensure-class-object 'standard-generic-function (find-class 'funcallable-standard-class)
                            (list (find-class 'generic-function)) nil :standard)
      (%ensure-class-object 'method sc (list (find-class 'metaobject)) nil :standard)
      (%ensure-class-object 'standard-method sc (list (find-class 'method)) nil :standard)
      (%ensure-class-object 'method-combination sc (list (find-class 'metaobject)) nil :standard)
      (%ensure-class-object 'eql-specializer sc (list (find-class 'specializer)) nil :standard)
      (%ensure-class-object 'slot-definition sc (list (find-class 'metaobject)) nil :standard)
      (%ensure-class-object 'standard-slot-definition sc (list (find-class 'slot-definition)) nil :standard)
      (%ensure-class-object 'direct-slot-definition sc (list (find-class 'slot-definition)) nil :standard)
      (%ensure-class-object 'effective-slot-definition sc (list (find-class 'slot-definition)) nil :standard)
      (%ensure-class-object 'standard-direct-slot-definition sc
                            (list (find-class 'standard-slot-definition) (find-class 'direct-slot-definition))
                            nil :standard)
      (%ensure-class-object 'standard-effective-slot-definition sc
                            (list (find-class 'standard-slot-definition) (find-class 'effective-slot-definition))
                            nil :standard)
      (setq *the-direct-slotd-class* (find-class 'standard-direct-slot-definition)
            *the-effective-slotd-class* (find-class 'standard-effective-slot-definition))
      (%define-built-in-classes))))

(defun %define-built-in-classes ()
  (let ((bic (find-class 'built-in-class)))
    (dolist (spec '((number t) (real number) (rational real) (integer rational) (fixnum integer)
                    (bignum integer) (ratio rational) (float real) (single-float float)
                    (double-float float) (complex number) (character t)
                    (symbol t) (sequence t) (list sequence) (null symbol list) (keyword symbol) (cons list)
                    (array t) (vector array sequence) (string vector) (simple-string string)
                    (bit-vector vector) (simple-vector vector) (hash-table t) (package t)
))
      (%ensure-class-object (car spec) bic (mapcar #'find-class (cdr spec)) nil :built-in))))

(%bootstrap-classes)

;;; raw slots: SPECS are names or (name initarg [initfunction])
(defun %define-raw-slots (class-name specs)
  (let ((class (find-class class-name)))
    (pushnew class *raw-layout-classes*)
    (%set-class-slot class +class-direct-slots+
                     (mapcar (lambda (s)
                               (if (consp s)
                                   (%make-slotd (car s) :initargs (if (cadr s) (list (cadr s)) nil)
                                                        :initfunction (caddr s))
                                   (%make-slotd s)))
                             specs))
    (%finalize-class class)))


;;; class-of: built-in classes are cached (cleared by (SETF FIND-CLASS))
(defmacro %builtin-class (i name)
  `(or (svref *builtin-classes* ,i) (setf (svref *builtin-classes* ,i) (find-class ',name))))
;;; (the tests of one representation come first: STRINGP, VECTORP, ARRAYP and
;;; BIT-VECTOR-P look into array headers)
(defun class-of (x)
  (cond ((%instancep x) (%instance-layout x))
        ((fixnump x) (%builtin-class 0 fixnum))
        ((consp x) (%builtin-class 1 cons))
        ((null x) (%builtin-class 2 null))
        ((symbolp x) (if (keywordp x) (%builtin-class 3 keyword) (%builtin-class 4 symbol)))
        ((characterp x) (%builtin-class 5 character))
        ((%simple-string-p x) (%builtin-class 6 simple-string))
        ((%double-float-p x) (%builtin-class 15 double-float))
        ((%single-float-p x) (%builtin-class 16 single-float))
        ((%simple-vector-p x) (%builtin-class 8 simple-vector))
        ((functionp x) (let ((gf (%closure-name x)))
                         (if (%instancep gf) (%instance-layout gf) (%builtin-class 12 compiled-function))))
        ((%bignump x) (%builtin-class 13 bignum))
        ((%ratiop x) (%builtin-class 14 ratio))
        ((%complexp x) (%builtin-class 17 complex))
        ((%packagep x) (%builtin-class 18 package))
        ((stringp x) (%builtin-class 7 string))
        ((bit-vector-p x) (%builtin-class 9 bit-vector))
        ((vectorp x) (%builtin-class 10 vector))
        ((arrayp x) (%builtin-class 11 array))
        (t (%builtin-class 19 t))))
