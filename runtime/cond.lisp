;;;; cond.lisp — the condition system

;;; ------------------------------------------------------------
;;; define-condition
;;; ------------------------------------------------------------

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %parse-slot-spec (spec)
    "Returns (values slotd-form readers writers)"
    (let* ((spec (if (consp spec) spec (list spec)))
           (name (car spec)) (opts (cdr spec))
           (initargs nil) (initform nil) (initformp nil) (readers nil) (writers nil)
           (allocation :instance) (type t) (doc nil))
      (do ((l opts (cddr l))) ((null l))
        (case (car l)
          (:initarg (push (cadr l) initargs))
          (:initform (setq initform (cadr l) initformp t))
          (:reader (push (cadr l) readers))
          (:writer (push (cadr l) writers))
          (:accessor (push (cadr l) readers) (push `(setf ,(cadr l)) writers))
          (:allocation (setq allocation (cadr l)))
          (:type (setq type (cadr l)))
          (:documentation (setq doc (cadr l)))))
      (values `(%make-slotd ',name :initargs ',(reverse initargs) :initform ',initform
                            :initfunction ,(if initformp `(lambda () ,initform) nil)
                            :readers ',readers :writers ',writers :allocation ',allocation
                            :type ',type :doc ',doc)
              readers writers name))))

(defmacro define-condition (name supers slots &rest options)
  (let ((slotd-forms nil) (accessor-forms nil) (report nil) (default-initargs nil))
    (dolist (s slots)
      (multiple-value-bind (form readers writers sname) (%parse-slot-spec s)
        (push form slotd-forms)
        ;; with CLOS present (in the image) readers and writers are methods;
        ;; when the runtime itself is compiled they are plain functions
        (let ((generic (and (boundp '*condition-accessor-hook*) (symbol-value '*condition-accessor-hook*))))
          (dolist (r readers)
            (push (if generic
                      `(%define-condition-accessor ',r ',name (lambda (c) (%slot-value-by-name c ',sname)) nil ',sname)
                      `(defun ,r (c) (%slot-value-by-name c ',sname)))
                  accessor-forms)
            ;; (the caches of call sites of CLOS, as for DEFCLASS)
            (when (and generic (fboundp '%gf-site-macro-forms))
              (dolist (f (funcall '%gf-site-macro-forms r '(object) t)) (push f accessor-forms))))
          (dolist (w writers)
            (push (if generic
                      `(%define-condition-accessor ',w ',name (lambda (v c) (%set-slot-value-by-name c ',sname v)) t ',sname)
                      `(defun ,w (v c) (%set-slot-value-by-name c ',sname v)))
                  accessor-forms)
            (when (and generic (fboundp '%gf-site-macro-forms))
              (dolist (f (funcall '%gf-site-macro-forms w '(new-value object) t)) (push f accessor-forms)))))))
    (dolist (o options)
      (case (car o)
        (:report (setq report (cadr o)))
        (:default-initargs (setq default-initargs (cdr o)))))
    `(progn
       (%define-condition-class ',name ',(or supers '(condition)) (list ,@(reverse slotd-forms))
                                (list ,@(let ((r nil))
                                          (do ((l default-initargs (cddr l))) ((null l) (reverse r))
                                            (push `',(car l) r) (push `(lambda () ,(cadr l)) r)))))
       ,@(when report
           `((%put ',name '%condition-report
                   ,(if (stringp report)
                        `(lambda (c s) (declare (ignore c)) (write-string ,report s))
                        (if (symbolp report)
                            ;; looked up when reporting (it may be defined later)
                            `(lambda (c s) (funcall ',report c s))
                            `(function ,report))))))
       ,@(reverse accessor-forms)
       ',name)))

(defvar *condition-accessor-hook* nil
  "set by CLOS: makes condition slot readers and writers generic functions")

(defun %define-condition-accessor (name class fn writerp &optional slot-name)
  (if *condition-accessor-hook*
      (funcall *condition-accessor-hook* name class fn writerp slot-name)
      (%set-fdefinition name fn))
  name)

(defun %define-condition-class (name supers direct-slots default-initargs)
  (let ((class (%ensure-class-object name *the-standard-class* (mapcar #'find-class supers)
                                     direct-slots :condition)))
    (%set-class-slot class +class-default-initargs+ default-initargs)
    (%forget-condition-plans class)
    class))

(defun %forget-condition-plans (class)
  "CLASS was (re)defined: it and its subclasses compute how they are made again."
  (when (eq (%class-kind class) :condition) (%set-class-slot class +class-extra+ nil))
  (dolist (sub (%class-slot class +class-subclasses+)) (%forget-condition-plans sub)))

(defun %plist-key-p (plist key)
  (do ((l plist (cddr l))) ((atom l) nil)
    (when (eq (car l) key) (return t))))

(defun %class-default-initargs-all (class &optional supplied)
  "The default initargs of CLASS not in SUPPLIED, with their forms evaluated."
  (let ((r nil))
    (dolist (c (%class-cpl class) r)
      (let ((d (%class-slot c +class-default-initargs+)))
        (do ((l d (cddr l))) ((null l))
          (unless (or (%plist-key-p r (car l)) (%plist-key-p supplied (car l)))
            (setq r (append r (list (car l) (funcall (cadr l)))))))))))

(defun %condition-class-for-type (type &optional (errorp t))
  (cond ((%class-p type) type)
        ((symbolp type) (find-class type errorp))
        ((and (consp type) (member (car type) '(or and)))
         ;; a compound type specifier: a class among its members (for AND,
         ;; preferably one that is a subtype of all the members)
         (let ((classes (remove nil (mapcar (lambda (x) (%condition-class-for-type x nil))
                                            (cdr type)))))
           (or (and (eq (car type) 'and)
                    (find-if (lambda (c) (every (lambda (x) (subtypep c x)) (cdr type))) classes))
               (car classes)
               (if errorp
                   (error 'simple-type-error :datum type :expected-type '(satisfies subtypep-condition)
                                             :format-control "~S is not a condition type"
                                             :format-arguments (list type))
                   nil))))
        (errorp (%type-error type 'symbol))
        (t nil)))

;;; A condition class caches how its instances are made in its EXTRA slot:
;;;   #(effective-slots nslots keys initfunctions)
;;; NSLOTS is the size of the slot vector, KEYS an alist (initarg . locations)
;;; and INITFUNCTIONS a list of (location . function) for the slots with an
;;; initform.  NSLOTS is NIL when this way does not apply (default initargs,
;;; shared slots).  The plan is valid while the class's list of effective
;;; slots is the one it was made from.
(defun %compute-condition-plan (class)
  (let ((slots (%class-effective-slots class)) (n 0) (keys nil) (initfns nil) (ok t))
    (dolist (c (%class-cpl class))
      (when (%class-slot c +class-default-initargs+) (setq ok nil)))
    (dolist (sd slots)
      (let ((loc (%slotd-location sd)))
        (if (fixnump loc)
            (progn
              (setq n (+ n 1))
              (dolist (k (%slotd-initargs sd))
                (let ((e (assoc k keys)))
                  (if e
                      (setf (cdr e) (append (cdr e) (list loc)))
                      (setq keys (append keys (list (list k loc)))))))
              (when (%slotd-initfunction sd)
                (setq initfns (append initfns (list (cons loc (%slotd-initfunction sd)))))))
            (setq ok nil))))
    (let ((plan (vector slots (if ok n nil) keys initfns)))
      (%set-class-slot class +class-extra+ plan)
      plan)))

(defun %make-condition (type initargs)
  (let* ((class (or (and (symbolp type) (get type '%class)) (%condition-class-for-type type)))
         (plan (and (eq (%class-kind class) :condition)
                    (let ((plan (%class-slot class +class-extra+)))
                      (if (and plan (eq (%svref plan 0) (%class-effective-slots class)))
                          plan
                          (%compute-condition-plan class)))))
         (n (and plan (%svref plan 1))))
    (if n
        (let ((v (%make-simple-vector n (%unbound-marker))))
          ;; the leftmost initarg of a slot wins
          (do ((l initargs (cddr l))) ((atom l))
            (let ((key (car l)))
              (do ((e (%svref plan 2) (cdr e))) ((null e))
                (when (eq (car (car e)) key)
                  (dolist (loc (cdr (car e)))
                    (when (%unboundp (%svref v loc)) (%set-svref v loc (cadr l))))
                  (return)))))
          (dolist (e (%svref plan 3))
            (when (%unboundp (%svref v (car e))) (%set-svref v (car e) (funcall (cdr e)))))
          (%make-instance class v))
        (let ((c (%allocate-instance class)))
          (%initialize-slots-from-initargs c (append initargs (%class-default-initargs-all class initargs)))
          c))))

(defun make-condition (type &rest initargs)
  (%make-condition type initargs))

(defun conditionp (x)
  (and (%instancep x) (let ((c (%instance-layout x))) (and (%instancep c) (eq (%class-kind c) :condition)))))

;;; base classes
(%ensure-class-object 'condition *the-standard-class* (list (find-class 'standard-object)) nil :condition)

(define-condition serious-condition (condition) ())
(define-condition error (serious-condition) ())
(define-condition warning (condition) ())
(define-condition style-warning (warning) ())
(define-condition storage-condition (serious-condition) ())
(define-condition simple-condition (condition)
  ((format-control :initarg :format-control :initform nil :reader simple-condition-format-control)
   (format-arguments :initarg :format-arguments :initform nil :reader simple-condition-format-arguments))
  (:report (lambda (c s)
             (apply #'format s (or (simple-condition-format-control c) "")
                    (simple-condition-format-arguments c)))))
(define-condition simple-error (simple-condition error) ())
;; conditions that carry a format control without being SIMPLE-CONDITIONs
(define-condition %formatted-condition (condition)
  ((format-control :initarg :format-control :initform nil)
   (format-arguments :initarg :format-arguments :initform nil))
  (:report (lambda (c s)
             (apply #'format s (or (simple-condition-format-control c) "")
                    (simple-condition-format-arguments c)))))
(define-condition simple-warning (simple-condition warning) ())
(define-condition type-error (error)
  ((datum :initarg :datum :initform nil :reader type-error-datum)
   (expected-type :initarg :expected-type :initform t :reader type-error-expected-type))
  (:report (lambda (c s)
             (format s "The value~%  ~S~%is not of type~%  ~S"
                     (type-error-datum c) (type-error-expected-type c)))))
(define-condition simple-type-error (simple-condition type-error) ()
  (:report (lambda (c s)
             (apply #'format s (or (simple-condition-format-control c) "")
                    (simple-condition-format-arguments c)))))
(define-condition index-out-of-bounds (type-error)
  ((sequence :initarg :sequence :initform nil))
  (:report (lambda (c s)
             (format s "Index ~S is out of bounds (expected ~S)"
                     (type-error-datum c) (type-error-expected-type c)))))
(define-condition program-error (error) ())
(define-condition simple-program-error (simple-condition program-error) ())
(define-condition control-error (error) ())
(define-condition simple-control-error (simple-condition control-error) ())
(define-condition package-error (error %formatted-condition)
  ((package :initarg :package :initform nil :reader package-error-package)))
(define-condition stream-error (error)
  ((stream :initarg :stream :initform nil :reader stream-error-stream)))
(define-condition end-of-file (stream-error) ()
  (:report (lambda (c s) (format s "end of file on ~S" (stream-error-stream c)))))
(define-condition file-error (error %formatted-condition)
  ((pathname :initarg :pathname :initform nil :reader file-error-pathname))
  (:report (lambda (c s)
             (if (simple-condition-format-control c)
                 (apply #'format s (simple-condition-format-control c) (simple-condition-format-arguments c))
                 (format s "error on file ~A" (file-error-pathname c))))))
(define-condition parse-error (error) ())
(define-condition parse-error-simple (simple-condition parse-error) ())
(define-condition reader-error (parse-error stream-error %formatted-condition) ()
  (:report (lambda (c s)
             (apply #'format s (or (simple-condition-format-control c) "reader error")
                    (simple-condition-format-arguments c)))))
(define-condition cell-error (error)
  ((name :initarg :name :initform nil :reader cell-error-name)))
(define-condition unbound-variable (cell-error) ()
  (:report (lambda (c s) (format s "The variable ~S is unbound." (cell-error-name c)))))
(define-condition undefined-function (cell-error) ()
  (:report (lambda (c s) (format s "The function ~S is undefined." (cell-error-name c)))))
(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance :initform nil :reader unbound-slot-instance))
  (:report (lambda (c s) (format s "The slot ~S is unbound in the object ~S."
                                 (cell-error-name c) (unbound-slot-instance c)))))
(define-condition arithmetic-error (error)
  ((operation :initarg :operation :initform nil :reader arithmetic-error-operation)
   (operands :initarg :operands :initform nil :reader arithmetic-error-operands))
  (:report (lambda (c s) (format s "arithmetic error ~S signalled" (type-of c)))))
(define-condition division-by-zero (arithmetic-error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "arithmetic error DIVISION-BY-ZERO signalled"))))
(define-condition floating-point-overflow (arithmetic-error) ())
(define-condition floating-point-underflow (arithmetic-error) ())
(define-condition floating-point-inexact (arithmetic-error) ())
(define-condition floating-point-invalid-operation (arithmetic-error) ())
(define-condition print-not-readable (error)
  ((object :initarg :object :initform nil :reader print-not-readable-object)))

;;; ------------------------------------------------------------
;;; restarts
;;; ------------------------------------------------------------

;;; A restart of RESTART-BIND has a FUNCTION.  A restart of RESTART-CASE has
;;; none: INVOKE-RESTART stores its INDEX (the number of its clause) in the
;;; cluster of the RESTART-CASE (its TAG), which is the tag of the %CATCH
;;; around the form, and throws the arguments to it.
(defstruct (restart (:constructor %make-restart (name function report-function interactive-function test-function))
                    (:constructor %make-case-restart (name index report-function interactive-function test-function tag)))
  name function report-function interactive-function test-function
  (conditions nil)                      ; the conditions it is associated with
  (index 0) (tag nil))

;;; *RESTART-CLUSTERS* is a chain of simple vectors, innermost first:
;;;   #(next clauses chosen restarts function...)
;;; A cluster of RESTART-BIND has CLAUSES = :BIND and its RESTARTS (a list).
;;; A cluster of RESTART-CASE is all that entering it makes (a restart that
;;; is not asked for costs nothing): CLAUSES, a constant vector of
;;; #(name report interactive test) (REPORT a string, or like INTERACTIVE and
;;; TEST NIL or the index in the cluster of the function given), CHOSEN the
;;; index of the clause INVOKE-RESTART chose, and the functions; RESTARTS is
;;; made when the restarts are first asked for (%CLUSTER-RESTARTS).
(defvar *restart-clusters* nil)
(defvar *condition-restarts* nil)

(defun %cluster-restarts (cluster)
  "The restarts of CLUSTER, an element of *RESTART-CLUSTERS* (in order), or
of no cluster (NIL)."
  (let ((clauses (and cluster (%svref cluster 1))))
    (if (or (null cluster) (eq clauses :bind))
        (and cluster (%svref cluster 3))
        (or (%svref cluster 3)
            (%set-svref
             cluster 3
             (let ((r nil))
               (dotimes (i (length clauses) (nreverse r))
                 (let* ((c (%svref clauses i))
                        (report (%svref c 1))
                        (slot (lambda (x) (and x (%svref cluster x)))))
                   (push (%make-case-restart
                          (%svref c 0) i
                          (if (stringp report) (lambda (s) (write-string report s)) (funcall slot report))
                          (funcall slot (%svref c 2)) (funcall slot (%svref c 3)) cluster)
                         r)))))))))

(defun %restart-visible-p (restart condition)
  (and (or (null condition) (null (restart-conditions restart))
           (member condition (restart-conditions restart)))
       (or (null (restart-test-function restart))
           (funcall (restart-test-function restart) condition))))

(defun compute-restarts (&optional condition)
  (let ((r nil))
    (do ((cluster *restart-clusters* (%svref cluster 0))) ((null cluster) (nreverse r))
      (dolist (restart (%cluster-restarts cluster))
        (when (%restart-visible-p restart condition)
          (push restart r))))))

(defun %cluster-has-name-p (cluster name)
  (let ((clauses (%svref cluster 1)))
    (or (eq clauses :bind)
        (dotimes (i (length clauses) nil)
          (when (eq (%svref (%svref clauses i) 0) name) (return t))))))

(defun find-restart (name &optional condition)
  (if (restart-p name) name
      (do ((cluster *restart-clusters* (%svref cluster 0))) ((null cluster) nil)
        ;; (the restarts of a RESTART-CASE are made only if one has the name)
        (when (%cluster-has-name-p cluster name)
          (dolist (restart (%cluster-restarts cluster))
            (when (and (eq (restart-name restart) name) (%restart-visible-p restart condition))
              (return-from find-restart restart)))))))

(defun %find-active-restart (restart)
  (let ((r (if (restart-p restart)
               (do ((cluster *restart-clusters* (%svref cluster 0))) ((null cluster) nil)
                 (when (member restart (%cluster-restarts cluster)) (return restart)))
               (find-restart restart))))
    (unless r
      (error 'simple-control-error :format-control "The restart ~S is not active."
                                   :format-arguments (list restart)))
    r))

(defun invoke-restart (restart &rest args)
  (let* ((r (%find-active-restart restart)) (f (restart-function r)))
    (if f
        (apply f args)
        (let ((tag (restart-tag r)))
          (%set-svref tag 2 (restart-index r))
          (%throw tag args)))))

(defun invoke-restart-interactively (restart)
  (let ((r (%find-active-restart restart)))
    (apply #'invoke-restart r
           (if (restart-interactive-function r) (funcall (restart-interactive-function r)) nil))))

(defmacro restart-bind (bindings &body body)
  `(let ((*restart-clusters*
           (vector *restart-clusters* :bind nil
                   (list ,@(mapcar (lambda (b)
                                     (destructuring-bind (name fn &key report-function interactive-function test-function) b
                                       `(%make-restart ',name ,fn ,report-function ,interactive-function ,test-function)))
                                   bindings)))))
     ,@body))

(defun %associate-restarts (condition restarts)
  "Associate RESTARTS with CONDITION for the rest of their extent."
  (dolist (r restarts) (setf (restart-conditions r) (cons condition (restart-conditions r))))
  condition)


(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %restart-case-signal-form (form env)
  "CLHS RESTART-CASE: when FORM is a call to SIGNAL, ERROR, CERROR or WARN,
the restarts are associated with the condition being signaled."
  ;; the compiler's %MACROEXPAND understands its environments (when the
  ;; compiler is in the image or on the host)
  ;; (named through a variable: a program without the compiler has no such
  ;; function, and a call of it by name would be reported as undefined)
  (let* ((expander '%macroexpand)
         (exp (if (or (listp env) (not (fboundp expander)))
                  (macroexpand form env)
                  (funcall expander form env))))
    (if (and (consp exp) (member (car exp) '(signal error cerror warn)))
        (let ((c (gensym "C")))
          (multiple-value-bind (datum args fmt)
              (if (eq (car exp) 'cerror)
                  (values (third exp) (cdddr exp) (second exp))
                  (values (second exp) (cddr exp) nil))
            ;; the restarts are out of reach once the form is left, so the
            ;; association is not undone
            `(let ((,c (%associate-restarts
                        (%coerce-to-condition ,datum (list ,@args)
                                              ',(case (car exp) (signal 'simple-condition)
                                                  (warn 'simple-warning) (t 'simple-error)))
                        (%cluster-restarts *restart-clusters*))))
               ,(if (eq (car exp) 'cerror)
                    `(cerror ,fmt ,c ,@args)
                    `(,(car exp) ,c)))))
        form))))

(defmacro restart-case (form &rest clauses &environment env)
  (setq form (%restart-case-signal-form form env))
  (if (null clauses)
      form
      (let* ((block (gensym "RC")) (args (gensym "ARGS")) (first (gensym "CLUSTER"))
             ;; the functions of the clauses, from slot 4 of the cluster
             (functions nil)
             (slot (lambda (form)
                     (push form functions)
                     (+ 3 (length functions))))
             (descriptions
               (mapcar (lambda (c)
                         (let* ((name (car c)) (rest (cddr c))
                                (report nil) (interactive nil) (test nil))
                           (do () ((not (member (car rest) '(:report :interactive :test))))
                             (case (car rest)
                               (:report (setq report (cadr rest)))
                               (:interactive (setq interactive (cadr rest)))
                               (:test (setq test (cadr rest))))
                             (setq rest (cddr rest)))
                           (vector name
                                   (cond ((null report) nil)
                                         ((stringp report) report)
                                         (t (funcall slot `(function ,report))))
                                   (if interactive (funcall slot `(function ,interactive)) nil)
                                   (if test (funcall slot `(function ,test)) nil))))
                       clauses))
             (bodies (mapcar (lambda (c)
                               (let* ((ll (cadr c)) (body (cddr c)))
                                 (do () ((not (member (car body) '(:report :interactive :test))))
                                   (setq body (cddr body)))
                                 `(apply (lambda ,ll ,@body) ,args)))
                             clauses)))
        ;; the cluster is the tag of the %CATCH: INVOKE-RESTART throws the
        ;; arguments to it after storing the clause number in it
        `(block ,block
           (let* ((,first (vector *restart-clusters* ',(coerce descriptions 'simple-vector) nil nil
                                  ,@(reverse functions)))
                  (,args (%catch ,first
                           (return-from ,block
                             (let ((*restart-clusters* ,first))
                               ,form)))))
             (declare (ignorable ,args))
             ,(if (null (cdr bodies))
                  (car bodies)
                  (let ((i -1))
                    `(case (%svref ,first 2)
                       ,@(mapcar (lambda (b) (list (if (cdr (member b bodies)) (setq i (+ i 1)) t) b))
                                 bodies)))))))))

(defmacro with-simple-restart ((name format-control &rest args) &body body)
  `(restart-case (progn ,@body)
     (,name () :report (lambda (s) (format s ,format-control ,@args)) (values nil t))))

(defmacro with-condition-restarts (condition restarts &body body)
  (let ((c (gensym "C")) (rs (gensym "RESTARTS")))
    `(let ((,c ,condition) (,rs ,restarts))
       (dolist (r ,rs) (setf (restart-conditions r) (cons ,c (restart-conditions r))))
       (unwind-protect (progn ,@body)
         (dolist (r ,rs) (setf (restart-conditions r) (remove ,c (restart-conditions r) :count 1)))))))

(defun abort (&optional condition)
  (let ((r (find-restart 'abort condition)))
    (if r (invoke-restart r)
        (error 'simple-control-error :format-control "There is no active ABORT restart."
                                     :format-arguments nil))))
(defun continue (&optional condition)
  (let ((r (find-restart 'continue condition))) (when r (invoke-restart r))))
(defun muffle-warning (&optional condition)
  (let ((r (find-restart 'muffle-warning condition)))
    (if r (invoke-restart r) (error 'control-error))))
(defun use-value (value &optional condition)
  (let ((r (find-restart 'use-value condition))) (when r (invoke-restart r value))))
(defun store-value (value &optional condition)
  (let ((r (find-restart 'store-value condition))) (when r (invoke-restart r value))))

;;; ------------------------------------------------------------
;;; signalling
;;; ------------------------------------------------------------

(defvar *handler-clusters* nil)
(defvar *break-on-signals* nil)
(defvar *debugger-hook* nil)
(defvar *invoke-debugger-hook* nil)
(defvar *erroring* nil
  "The condition that ERROR or CERROR is signalling (which enters the
debugger if no handler takes it, unlike one given to SIGNAL).")
(defvar *debugger-fallback* nil
  "What the REPL, a script, a thread or JavaScript calling Lisp do with an
error that would enter the debugger (the innermost binding): a function of
the condition, called by INVOKE-DEBUGGER after *DEBUGGER-HOOK* and
*INVOKE-DEBUGGER-HOOK*.")

(defun %coerce-to-condition (datum args default-type &optional allow-args)
  (cond ((conditionp datum)
         (when (and args (not allow-args))
           (error 'simple-type-error :datum args :expected-type 'null
                                     :format-control "Arguments ~S given with the condition ~S"
                                     :format-arguments (list args datum)))
         datum)
        ((or (stringp datum) (functionp datum))
         (%make-condition default-type (list :format-control datum :format-arguments args)))
        ((symbolp datum) (%make-condition datum args))
        ((%class-p datum) (%make-condition datum args))
        (t (error 'simple-type-error :datum datum :expected-type '(or condition string symbol)
                                     :format-control "Bad condition designator ~S"
                                     :format-arguments (list datum)))))

;;; *HANDLER-CLUSTERS* is a chain of simple vectors, innermost first:
;;;   #(next types kind function...)
;;; NEXT is the enclosing cluster, TYPES a simple vector of type specifiers.
;;; A HANDLER-BIND cluster has KIND = T and one function per type, or, when
;;; its handlers are all LAMBDA forms, KIND = :ONE and one function of the
;;; index of the type and the condition (making one closure on entry instead
;;; of one for each handler).  A
;;; HANDLER-CASE cluster has no functions: it is the tag of the %CATCH around
;;; the form, and SIGNAL stores the number of the chosen clause in KIND before
;;; it throws the condition to it.

(defun %condition-of-type-p (condition type)
  (if (symbolp type)
      (let ((class (get type '%class)))
        (if class
            ;; the usual case, a class name: is the class in the condition's
            ;; class precedence list?
            (do ((l (%class-cpl (%instance-layout condition)) (cdr l))) ((null l) nil)
              (when (eq (car l) class) (return t)))
            (typep condition type)))
      (typep condition type)))

;;; the body of SIGNAL, also expanded in ERROR and WARN: the frames between
;;; the signalling function and the handler make a transfer of control slower
(defmacro %signal-condition (condition)
  `(let ((cluster *handler-clusters*))
    (do () ((null cluster))
      (let* ((next (%svref cluster 0)) (types (%svref cluster 1)) (n (length types)))
        (case (%svref cluster 2)
          ((t)
            (do ((i 0 (+ i 1))) ((>= i n))
              (when (%condition-of-type-p ,condition (%svref types i))
                (let ((*handler-clusters* next))
                  (funcall (%svref cluster (+ i 3)) ,condition)))))
          (:one
            (do ((i 0 (+ i 1))) ((>= i n))
              (when (%condition-of-type-p ,condition (%svref types i))
                (let ((*handler-clusters* next))
                  (funcall (%svref cluster 3) i ,condition)))))
          (t
            (do ((i 0 (+ i 1))) ((>= i n))
              (when (%condition-of-type-p ,condition (%svref types i))
                (%set-svref cluster 2 i)
                (%throw cluster ,condition)))))
        (setq cluster next)))))

(defun %throw (tag value) (%throw tag value))

;;; The pending non-local exit (src/codegen.lisp, "Non-local exits as
;;; returns"): unbound when there is none.
(defvar %exit-tag)
(defvar %exit-value)

(defun signal (datum &rest args)
  (let ((condition (%coerce-to-condition datum args 'simple-condition)))
    (%signal-condition condition)
    nil))

(defvar *error-depth* 0)

(defun %error-panic (datum args)
  (%early-write-string "Fatal error during error handling: " 2)
  (cond ((stringp datum) (%early-write-string datum 2))
        ((symbolp datum) (%early-write-string (symbol-name datum) 2)))
  (dolist (a args)
    (%early-write-string " " 2)
    (cond ((stringp a) (%early-write-string a 2))
          ((symbolp a) (%early-write-string (symbol-name a) 2))
          ((fixnump a) (%early-write-string (%integer-to-string a 10) 2))
          (t (%early-write-string "?" 2))))
  (%write-char #\Newline 2)
  ;; trap so that the host prints a stack trace
  (%wasm nil () :unreachable))

(defun error (datum &rest args)
  (when (or (> *error-depth* 4) (null (get 'simple-error '%class))) (%error-panic datum args))
  (let* ((*error-depth* (+ *error-depth* 1))
         (condition (%coerce-to-condition datum args 'simple-error))
         (*erroring* condition))
    (%signal-condition condition)
    (invoke-debugger condition)))

(defun cerror (continue-format datum &rest args)
  (when (or (> *error-depth* 4) (null (get 'simple-error '%class))) (%error-panic datum args))
  (let ((c (%coerce-to-condition datum args 'simple-error t)))
    (restart-case (progn
                    (%associate-restarts c (%cluster-restarts *restart-clusters*))
                    (let ((*error-depth* (+ *error-depth* 1)) (*erroring* c))
                      (%signal-condition c))
                    (invoke-debugger c))
      (continue () :report (lambda (s) (apply #'format s continue-format args)) nil)))
  nil)

(defun warn (datum &rest args)
  (let ((condition (%coerce-to-condition datum args 'simple-warning)))
    (unless (typep condition 'warning)
      (error 'simple-type-error :datum condition :expected-type 'warning
                                :format-control "~S is not a warning" :format-arguments (list condition)))
    (restart-case (progn
                    (%signal-condition condition)
                    (format *error-output* "~&WARNING: ~A~%" condition))
      (muffle-warning () :report "Skip warning." nil))
    nil))

(defun invoke-debugger (condition)
  (let ((hook *debugger-hook*))
    (when hook
      (let ((*debugger-hook* nil))
        (funcall hook condition hook))))
  (when *invoke-debugger-hook*
    (funcall *invoke-debugger-hook* condition))
  (when *debugger-fallback*
    (funcall *debugger-fallback* condition))
  (%report-and-exit condition))

(defun break (&optional (format-control "Break") &rest args)
  (format *error-output* "~&BREAK: ~?~%" format-control args)
  nil)

;;; handler macros

(defmacro handler-bind (bindings &body body)
  (flet ((lambda-form-p (f)
           (and (consp f)
                (or (eq (car f) 'lambda)
                    (and (eq (car f) 'function) (consp (cdr f)) (consp (cadr f))
                         (eq (car (cadr f)) 'lambda))))))
    (if (and (cdr bindings) (every #'lambda-form-p (mapcar #'cadr bindings)))
        (let ((i (gensym "I")) (c (gensym "C")) (k -1))
          `(let ((*handler-clusters*
                   (vector *handler-clusters* ',(apply #'vector (mapcar #'car bindings)) :one
                           (lambda (,i ,c)
                             (case ,i
                               ,@(mapcar (lambda (b)
                                           (let ((f (cadr b)))
                                             `(,(setq k (+ k 1))
                                               (funcall ,(if (eq (car f) 'function) f `(function ,f)) ,c))))
                                         bindings))))))
             ,@body))
        `(let ((*handler-clusters*
                 (vector *handler-clusters* ',(apply #'vector (mapcar #'car bindings)) t
                         ,@(mapcar #'cadr bindings))))
           ,@body))))

(defmacro handler-case (form &rest clauses)
  (let* ((no-error (find :no-error clauses :key #'car))
         (clauses (remove :no-error clauses :key #'car))
         (block (gensym "HC")) (cvar (gensym "C")) (cluster (gensym "CLUSTER")))
    (cond (no-error
           (let ((normal (gensym "NORMAL")))
             `(block ,normal
                (multiple-value-call (lambda ,(cadr no-error) ,@(cddr no-error))
                  (block ,block
                    (return-from ,normal
                      (handler-case (return-from ,block ,form)
                        ,@clauses)))))))
          ((null clauses) form)
          (t
           ;; the cluster is the tag of the %CATCH: SIGNAL throws the
           ;; condition to it after storing the clause number in it
           (let ((bodies (mapcar (lambda (c)
                                   (let ((var (car (cadr c))) (body (cddr c)))
                                     (if var
                                         `(let ((,var ,cvar)) ,@body)
                                         `(locally ,@body))))
                                 clauses)))
             `(block ,block
                (let* ((,cluster (vector *handler-clusters* ',(apply #'vector (mapcar #'car clauses)) nil))
                       (,cvar (%catch ,cluster
                                (return-from ,block
                                  (let ((*handler-clusters* ,cluster)) ,form)))))
                  (declare (ignorable ,cvar))
                  ,(if (null (cdr bodies))
                       (car bodies)
                       (let ((i -1))
                         `(case (%svref ,cluster 2)
                            ,@(mapcar (lambda (b) (list (if (cdr (member b bodies)) (setq i (+ i 1)) t) b))
                                      bodies)))))))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) (values nil condition))))

;;; ------------------------------------------------------------
;;; errors signalled by the compiler-generated code
;;; ------------------------------------------------------------

(defun %type-error (datum expected)
  (error 'type-error :datum datum :expected-type expected))

;; the value of a declared parameter that the inline test of its type does
;; not decide (src/codegen.lisp, DECL-CHECK-CODE)
(defun %untyped (x) x)
(defun %check-declared-type (datum expected)
  (if (typep datum expected) datum (%type-error datum expected)))

(defun %read-new-value ()
  (format *query-io* "~&Enter a form to evaluate: ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))

(defun %undefined-function-error (name)
  (restart-case (error 'undefined-function :name name)
    (use-value (f) :report "Call a different function." :interactive %read-new-value f)))

(defun %unbound-variable-error (name)
  (restart-case (error 'unbound-variable :name name)
    (use-value (v) :report "Use a value instead." :interactive %read-new-value v)
    (store-value (v) :report "Set the variable and use the value." :interactive %read-new-value
      (setf (symbol-value name) v) v)))

(defun %check-arg-list (l n)
  "L is a proper list of N elements (APPLY of a lambda of N required parameters)."
  (let ((x l))
    (dotimes (i n) (if (consp x) (setq x (cdr x)) (return-from %check-arg-list (%arg-list-error l n))))
    (when x (%arg-list-error l n))))

(defun %arg-list-error (l n)
  (error 'simple-program-error
         :format-control "invalid number of arguments: ~S (applying a function of ~S arguments)"
         :format-arguments (list (if (listp l) (length l) l) n)))

(defun %arg-count-error (f nargs)
  (error 'simple-program-error
         :format-control "invalid number of arguments: ~S (calling ~S)"
         :format-arguments (list nargs (%closure-name f))))

(defun %correctable-package-error (package report control &rest args)
  "Signal a PACKAGE-ERROR with a CONTINUE restart; returns T if continued."
  (restart-case (progn (error 'package-error :package package :format-control control
                                             :format-arguments args)
                       nil)
    (continue () :report (lambda (s) (write-string report s)) t)))

(defun %values-list-error (list)
  (error 'type-error :datum list :expected-type 'list))

(defun %no-catch-error (tag)
  (error 'simple-control-error :format-control "attempt to THROW to a tag that does not exist: ~S"
                               :format-arguments (list tag)))

(defun %odd-keys-error (args start)
  (declare (ignore args start))
  (error 'simple-program-error :format-control "odd number of &KEY arguments" :format-arguments nil))

(defun %unknown-key-error (key keys)
  (error 'simple-program-error :format-control "Unknown &KEY argument: ~S (allowed: ~S)"
                               :format-arguments (list key keys)))

(defun %case-failure (key keys)
  (error 'simple-type-error :datum key :expected-type `(member ,@keys)
                            :format-control "~S fell through ECASE expression. Wanted one of ~S."
                            :format-arguments (list key keys)))

(defun %etypecase-failure (key type)
  (error 'simple-type-error :datum key :expected-type type
                            :format-control "~S fell through ETYPECASE expression. Wanted one of ~S."
                            :format-arguments (list key (cdr type))))

(defun %ccase-failure (key type place)
  (restart-case (error 'simple-type-error :datum key :expected-type type
                                          :format-control "The value of ~S, ~S, is not of type ~S."
                                          :format-arguments (list place key type))
    (store-value (v)
      :report (lambda (s) (format s "Supply a new value for ~S." place))
      :interactive (lambda () (format *query-io* "~&New value: ") (list (eval (read *query-io*))))
      v)))

(defun %assert-error (test &optional (datum nil datum-p) &rest args)
  "The failure of (ASSERT TEST ...): signal the error, returning when the
CONTINUE restart is chosen (the assertion is then tried again)."
  (restart-case (if datum-p
                    (apply #'error datum args)
                    (error "The assertion ~S failed." test))
    (continue () :report "Retry the assertion." nil)))

(defun %check-type-error (place value type &optional type-string)
  (declare (ignore type-string))
  (restart-case (error 'simple-type-error :datum value :expected-type type
                                          :format-control "The value of ~S is ~S, which is not of type ~S."
                                          :format-arguments (list place value type))
    (store-value (v) :report "Supply a new value." v)))

(defun %report-and-exit (condition)
  (let ((msg (handler-case (princ-to-string condition)
               (error () "<error while printing condition>"))))
    (%early-write-string "Unhandled " 2)
    (%early-write-string (handler-case (symbol-name (type-of condition)) (error () "condition")) 2)
    (%early-write-string ": " 2)
    (%early-write-string msg 2)
    (%write-char #\Newline 2)
    (%backtrace 2)
    (when (eql (%debug-mode) 1) (%wasm nil () :unreachable))
    (%exit 1)))


;;; (for the TYPEP compiler macro in types.lisp, which is compiled before HANDLER-CASE is defined)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun %deftype-expansion-list (name args)
    "A list of the expansion of the type (NAME . ARGS) defined by DEFTYPE, or NIL when the expander signals an error."
    (handler-case (list (funcall (get name '%deftype) args)) (error () nil))))
