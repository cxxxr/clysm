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
       ;; (a class name: the compiled TYPEP tests of it, runtime/types.lisp)
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (%put ',name '%defclass t))
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

(defun %condition-class-subtype-p (c x)
  ;; (a class against a class or its name without SUBTYPEP, which a shaken
  ;; program may not have: %WEAK-SUBTYPEP)
  (let ((k (cond ((%class-p x) x) ((symbolp x) (find-class x nil)))))
    (cond (k (%subclassp c k))
          ((%weak-subtypep) (values (funcall (the function (%weak-subtypep)) c x)))
          (t nil))))

(defun %condition-class-for-type (type &optional (errorp t))
  (cond ((%class-p type) type)
        ((symbolp type) (find-class type errorp))
        ((and (consp type) (member (car type) '(or and)))
         ;; a compound type specifier: a class among its members (for AND,
         ;; preferably one that is a subtype of all the members)
         (let ((classes (remove nil (mapcar (lambda (x) (%condition-class-for-type x nil))
                                            (cdr type)))))
           (or (and (eq (car type) 'and)
                    (dolist (c classes nil)
                      (when (every (lambda (x) (%condition-class-subtype-p c x)) (cdr type)) (return c))))
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
   (format-arguments :initarg :format-arguments :initform nil :reader simple-condition-format-arguments)))
(define-condition simple-error (simple-condition error) ())
;; conditions that carry a format control without being SIMPLE-CONDITIONs
(define-condition %formatted-condition (condition)
  ((format-control :initarg :format-control :initform nil)
   (format-arguments :initarg :format-arguments :initform nil)))
(define-condition simple-warning (simple-condition warning) ())
(define-condition type-error (error)
  ((datum :initarg :datum :initform nil :reader type-error-datum)
   (expected-type :initarg :expected-type :initform t :reader type-error-expected-type)))
(define-condition simple-type-error (simple-condition type-error) ())
(define-condition index-out-of-bounds (type-error)
  ((sequence :initarg :sequence :initform nil)))
(define-condition program-error (error) ())
(define-condition simple-program-error (simple-condition program-error) ())
(define-condition control-error (error) ())
(define-condition simple-control-error (simple-condition control-error) ())
(define-condition package-error (error %formatted-condition)
  ((package :initarg :package :initform nil :reader package-error-package)))
(define-condition stream-error (error)
  ((stream :initarg :stream :initform nil :reader stream-error-stream)))
(define-condition end-of-file (stream-error) ())
(define-condition file-error (error %formatted-condition)
  ((pathname :initarg :pathname :initform nil :reader file-error-pathname)))
(define-condition parse-error (error) ())
(define-condition parse-error-simple (simple-condition parse-error) ())
(define-condition reader-error (parse-error stream-error %formatted-condition) ())
(define-condition cell-error (error)
  ((name :initarg :name :initform nil :reader cell-error-name)))
(define-condition unbound-variable (cell-error) ())
(define-condition undefined-function (cell-error) ())
(define-condition unbound-slot (cell-error)
  ((instance :initarg :instance :initform nil :reader unbound-slot-instance)))
(define-condition arithmetic-error (error)
  ((operation :initarg :operation :initform nil :reader arithmetic-error-operation)
   (operands :initarg :operands :initform nil :reader arithmetic-error-operands)))
(define-condition division-by-zero (arithmetic-error) ())
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

;;; The restarts in effect are, like the handlers (see *HANDLER-FRAMES*
;;; below), frames that stay in a vector: the first *RESTART-CLUSTERS* (a
;;; fixnum) elements of *RESTART-FRAMES*, the innermost last, each a simple
;;; vector #(clauses chosen restarts function...).  RESTART-CASE fills the
;;; frame at the current depth and binds the depth to one more: entering it
;;; allocates nothing (but the closures of its report, interactive and test
;;; functions that are not the ones the frame holds).  The frame is all
;;; that entering makes (a restart that is not asked for costs nothing):
;;; CLAUSES, a constant vector of #(name report interactive test) (REPORT a
;;; string, or like INTERACTIVE and TEST NIL or the index in the frame of
;;; the function given), CHOSEN the index of the clause INVOKE-RESTART
;;; chose, and the functions; RESTARTS is made when the restarts are first
;;; asked for (%CLUSTER-RESTARTS) and emptied when the frame is entered
;;; again: a restart object kept from an earlier use of the frame is not
;;; among them, and so not active.  The frame is the tag of the %CATCH.
;;; A frame of RESTART-BIND has CLAUSES = :BIND and its RESTARTS (a list).
;;; CLAUSES = (frames . depth): the restarts continue in another vector
;;; (those around a computation of JS:ASYNC-FUNCTION).
(defun %make-restart-frames (n old)
  (let ((new (%make-simple-vector n nil)) (m (if old (%svlength old) 0)))
    (do ((i 0 (+ i 1))) ((>= i n) new)
      (%set-svref new i (if (< i m) (%svref old i) (%make-simple-vector 9 nil))))))
(defvar *restart-clusters* 0)
(declaim (type fixnum *restart-clusters*))
(defvar *restart-frames* (%make-restart-frames 16 nil))
(defvar *condition-restarts* nil)

(defun %grow-restart-frames (d)
  (let ((new (%make-restart-frames (* 2 (+ d 8)) *restart-frames*)))
    (setq *restart-frames* new)
    (%svref new d)))

;;; the frame at DEPTH with CLAUSES in it and no restarts made: the helper
;;; :RESTART-FRAME of src/prims.lisp
(defun %restart-frame (depth clauses) (%restart-frame depth clauses))

(defun %wide-restart-frame (depth clauses n)
  "The frame at DEPTH for a RESTART-CASE of more functions than a frame has room for."
  (let ((frame (%restart-frame depth clauses)))
    (if (< (%svlength frame) n)
        (let ((new (%make-simple-vector n nil)))
          (%set-svref new 0 clauses)
          (%set-svref *restart-frames* depth new)
          new)
        frame)))

(defun %linked-restart-frames ()
  (let ((frames (%make-restart-frames 8 nil)))
    (%set-svref (%svref frames 0) 0 (cons *restart-frames* *restart-clusters*))
    frames))

;;; BODY with FRAME bound to each frame in effect, the innermost first
(defmacro %do-restart-frames ((frame &optional result) &body body)
  `(let ((frames *restart-frames*))
     (do ((i *restart-clusters*)) ((<= i 0) ,result)
       (setq i (- i 1))
       (let ((,frame (%svref frames i)))
         (if (consp (%svref ,frame 0))
             (let ((link (%svref ,frame 0))) (setq frames (car link) i (cdr link)))
             (progn ,@body))))))

(defun %innermost-restart-frame ()
  (%do-restart-frames (frame nil) (return frame)))

(defun %cluster-restarts (cluster)
  "The restarts of CLUSTER, a restart frame (in order), or of no frame (NIL)."
  (let ((clauses (and cluster (%svref cluster 0))))
    (if (or (null cluster) (eq clauses :bind))
        (and cluster (%svref cluster 2))
        (or (%svref cluster 2)
            (%set-svref
             cluster 2
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
    (%do-restart-frames (cluster (nreverse r))
      (dolist (restart (%cluster-restarts cluster))
        (when (%restart-visible-p restart condition)
          (push restart r))))))

(defun %cluster-has-name-p (cluster name)
  (let ((clauses (%svref cluster 0)))
    (or (eq clauses :bind)
        (dotimes (i (length clauses) nil)
          (when (eq (%svref (%svref clauses i) 0) name) (return t))))))

(defun find-restart (name &optional condition)
  (if (restart-p name) name
      (%do-restart-frames (cluster nil)
        ;; (the restarts of a RESTART-CASE are made only if one has the name)
        (when (%cluster-has-name-p cluster name)
          (dolist (restart (%cluster-restarts cluster))
            (when (and (eq (restart-name restart) name) (%restart-visible-p restart condition))
              (return-from find-restart restart)))))))

(defun %find-active-restart (restart)
  (let ((r (if (restart-p restart)
               (%do-restart-frames (cluster nil)
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
          (%set-svref tag 1 (restart-index r))
          (%throw tag args)))))

(defun invoke-restart-interactively (restart)
  (let* ((r (%find-active-restart restart))
         (f (restart-interactive-function r)))
    (apply #'invoke-restart r
           (cond ((null f) nil)
                 ((eq f (%weak-symbol %read-new-value)) (%read-new-value))
                 ((eq f (%weak-symbol %read-store-value)) (%read-store-value))
                 (t (funcall f))))))

(defmacro restart-bind (bindings &body body)
  (let ((depth (gensym "DEPTH")) (frame (gensym "FRAME")))
    `(let* ((,depth *restart-clusters*)
            (,frame (%restart-frame ,depth :bind)))
       (%set-svref ,frame 2
                   (list ,@(mapcar (lambda (b)
                                     (destructuring-bind (name fn &key report-function interactive-function test-function) b
                                       `(%make-restart ',name ,fn ,report-function ,interactive-function ,test-function)))
                                   bindings)))
       (let ((*restart-clusters* (+ ,depth 1)))
         ,@body))))

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
                        (%cluster-restarts (%innermost-restart-frame)))))
               ,(if (eq (car exp) 'cerror)
                    `(cerror ,fmt ,c ,@args)
                    `(,(car exp) ,c)))))
        form))))

(defmacro restart-case (form &rest clauses &environment env)
  (setq form (%restart-case-signal-form form env))
  (if (null clauses)
      form
      (let* ((block (gensym "RC")) (args (gensym "ARGS")) (first (gensym "CLUSTER")) (depth (gensym "DEPTH"))
             (fn (gensym "FN"))
             ;; the functions of the clauses, from slot 3 of the frame
             (functions nil)
             (slot (lambda (form)
                     (push form functions)
                     (+ 2 (length functions))))
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
                                   (if interactive
                                       (funcall slot (if (member interactive '(%read-new-value %read-store-value))
                                                         ;; (the runtime's: kept by
                                                         ;; INVOKE-RESTART-INTERACTIVELY, not by the
                                                         ;; code that makes the restart)
                                                         `(%weak-symbol ,interactive)
                                                         `(function ,interactive)))
                                       nil)
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
        ;; (a function is the closure the frame holds when that is one of
        ;; the same function over the same cells and values, as HANDLER-BIND's)
        `(block ,block
           (let* ((,depth *restart-clusters*)
                  (,first ,(if (> (length functions) 6)
                               `(%wide-restart-frame ,depth ',(coerce descriptions 'simple-vector)
                                                     ,(+ 3 (length functions)))
                               `(%restart-frame ,depth ',(coerce descriptions 'simple-vector))))
                  (,args (progn
                           ,@(let ((k 2))
                               (mapcar (lambda (f)
                                         (setq k (+ k 1))
                                         `(let ((,fn (%reuse-closure (%svref ,first ,k) ,f)))
                                            (if (eq ,fn (%svref ,first ,k)) nil (%set-svref ,first ,k ,fn))))
                                       (reverse functions)))
                           (%catch ,first
                             (return-from ,block
                               (let ((*restart-clusters* (+ ,depth 1)))
                                 ,form))))))
             (declare (ignorable ,args))
             ,(if (null (cdr bodies))
                  (car bodies)
                  (let ((i -1))
                    `(case (%svref ,first 1)
                       ,@(mapcar (lambda (b) (list (if (cdr (member b bodies)) (setq i (+ i 1)) t) b))
                                 bodies)))))))))

(defmacro with-simple-restart ((name format-control &rest args) &body body)
  `(restart-case (progn ,@body)
     (,name () :report (lambda (s) (%message-format s ,format-control ,@args)) (values nil t))))

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

(defun %make-handler-frames (n old)
  (let ((new (%make-simple-vector n nil)) (m (if old (%svlength old) 0)))
    (do ((i 0 (+ i 1))) ((>= i n) new)
      (%set-svref new i (if (< i m) (%svref old i) (vector nil nil nil))))))
;;; the number of handler frames in use, and the frames (see below)
(defvar *handler-clusters* 0)
(declaim (type fixnum *handler-clusters*))
(defvar *handler-frames* (%make-handler-frames 16 nil))
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

;;; The handlers in effect are the first *HANDLER-CLUSTERS* elements of the
;;; simple vector *HANDLER-FRAMES*, the innermost last.  A frame is a simple
;;; vector #(types clause function) that stays in its place: HANDLER-BIND and
;;; HANDLER-CASE fill the frame at the current depth and bind
;;; *HANDLER-CLUSTERS* to one more, so that entering one allocates nothing and
;;; stores no new object into an old one (a store of a young object into an
;;; old one takes the slow path of the engine's write barrier: a cluster
;;; made at each entry and bound to the variable cost that).  Each slot is
;;; stored only when it holds another object (a loop finds its own there).
;;; TYPES is a constant of the form:
;;;   #(T type)          HANDLER-BIND of one handler: FUNCTION takes the condition
;;;   #(:ONE type...)    HANDLER-BIND: FUNCTION takes the index of the type and
;;;                      the condition (one closure for all the handlers)
;;;   #(NIL type...)     HANDLER-CASE: the frame is the tag of the %CATCH around
;;;                      the form, and SIGNAL stores the index of the chosen
;;;                      clause in CLAUSE before it throws the condition to it
;;;   an integer I       (made by SIGNAL around the call of a handler, which
;;;                      runs with the handlers outside its own) the frames
;;;                      below this one down to I are skipped
;;;   (frames . depth)   the handlers continue in another vector (those around
;;;                      a computation of JS:ASYNC-FUNCTION, which has frames
;;;                      of its own: another computation runs while it waits)

(defun %grow-handler-frames (d)
  (let ((new (%make-handler-frames (* 2 (+ d 8)) *handler-frames*)))
    (setq *handler-frames* new)
    (%svref new d)))

;;; the frame at DEPTH with TYPES in it: the helper :HANDLER-FRAME of
;;; src/prims.lisp (the frames' vector, the frame or %GROW-HANDLER-FRAMES,
;;; and the store of TYPES unless they are there)
(defun %handler-frame (depth types) (%handler-frame depth types))

(defun %call-handler (function index condition depth below)
  ;; the handler runs with the handlers below its own frame
  (%handler-frame depth below)
  (let ((*handler-clusters* (+ depth 1)))
    (if index (funcall function index condition) (funcall function condition))))

(defun %linked-handler-frames ()
  "Frames for a computation that can be suspended, whose first frame continues
with the handlers in effect now."
  (let ((frames (%make-handler-frames 8 nil)))
    (%set-svref (%svref frames 0) 0 (cons *handler-frames* *handler-clusters*))
    frames))

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
  `(let ((depth *handler-clusters*) (frames *handler-frames*))
    (do ((i depth)) ((<= i 0))
      (setq i (- i 1))
      (let* ((frame (%svref frames i)) (types (%svref frame 0)))
        (cond ((fixnump types) (setq i types))
              ((consp types) (setq frames (car types) i (cdr types)))
              (t
               (let ((n (%svlength types)) (kind (%svref types 0)))
                 (do ((k 1 (+ k 1))) ((>= k n))
                   (when (%condition-of-type-p ,condition (%svref types k))
                     (cond ((eq kind t) (%call-handler (%svref frame 2) nil ,condition depth i))
                           (kind (%call-handler (%svref frame 2) (- k 1) ,condition depth i))
                           (t (%set-svref frame 1 (- k 1))
                              (%throw frame ,condition))))))))))))

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
                    (%associate-restarts c (%cluster-restarts (%innermost-restart-frame)))
                    (let ((*error-depth* (+ *error-depth* 1)) (*erroring* c))
                      (%signal-condition c))
                    (invoke-debugger c))
      (continue () :report (lambda (s) (apply #'%message-format s continue-format args)) nil)))
  nil)

(defun warn (datum &rest args)
  (let ((condition (%coerce-to-condition datum args 'simple-warning)))
    (unless (typep condition 'warning)
      (error 'simple-type-error :datum condition :expected-type 'warning
                                :format-control "~S is not a warning" :format-arguments (list condition)))
    (restart-case (progn
                    (%signal-condition condition)
                    ;; (as FORMAT "~&WARNING: ~A~%", without the printer
                    ;; when the program has not it: %CONDITION-MESSAGE)
                    (let ((s *error-output*))
                      (fresh-line s)
                      (write-string "WARNING: " s)
                      (write-string (%condition-message condition) s)
                      (terpri s)))
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

(defun %reuse-closure (old new) (declare (ignore old)) new)

(defmacro handler-bind (bindings &body body)
  (flet ((lambda-form-p (f)
           (and (consp f)
                (or (eq (car f) 'lambda)
                    (and (eq (car f) 'function) (consp (cdr f)) (consp (cadr f))
                         (eq (car (cadr f)) 'lambda))))))
    (if (null bindings)
        `(progn ,@body)
        (let* ((depth (gensym "DEPTH")) (frame (gensym "FRAME")) (fn (gensym "FN"))
               (i (gensym "I")) (c (gensym "C")) (k -1) (temps '())
               (function
                 ;; one handler: its function; more: one function of the
                 ;; index of the type and the condition (one closure on
                 ;; entry instead of one for each handler)
                 (if (null (cdr bindings))
                     (cadr (car bindings))
                     `(lambda (,i ,c)
                        (case ,i
                          ,@(mapcar (lambda (b)
                                      (let ((f (cadr b)))
                                        `(,(setq k (+ k 1))
                                          (funcall ,(cond ((not (lambda-form-p f))
                                                           ;; (evaluated on entry)
                                                           (let ((g (gensym "H")))
                                                             (push (list g f) temps)
                                                             g))
                                                          ((eq (car f) 'function) f)
                                                          (t `(function ,f)))
                                                   ,c))))
                                    bindings))))))
          ;; (the closure of the frame is used again when it is of this
          ;; function with the same captured cells and values: a loop that
          ;; binds handlers makes none)
          `(let* ((,depth *handler-clusters*)
                  (,frame (%handler-frame ,depth ',(apply #'vector (if (cdr bindings) :one t) (mapcar #'car bindings))))
                  ,@(reverse temps)
                  (,fn (%reuse-closure (%svref ,frame 2) ,function)))
             (if (eq ,fn (%svref ,frame 2)) nil (%set-svref ,frame 2 ,fn))
             (let ((*handler-clusters* (+ ,depth 1)))
               ,@body))))))

(defmacro handler-case (form &rest clauses)
  (let* ((no-error (find :no-error clauses :key #'car))
         (clauses (remove :no-error clauses :key #'car))
         (block (gensym "HC")) (cvar (gensym "C")) (cluster (gensym "CLUSTER")) (depth (gensym "DEPTH")))
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
           ;; the frame is the tag of the %CATCH: SIGNAL throws the
           ;; condition to it after storing the clause number in it
           (let ((bodies (mapcar (lambda (c)
                                   (let ((var (car (cadr c))) (body (cddr c)))
                                     (if var
                                         `(let ((,var ,cvar)) ,@body)
                                         `(locally ,@body))))
                                 clauses)))
             `(block ,block
                (let* ((,depth *handler-clusters*)
                       (,cluster (%handler-frame ,depth ',(apply #'vector nil (mapcar #'car clauses))))
                       (,cvar (%catch ,cluster
                                (return-from ,block
                                  (let ((*handler-clusters* (+ ,depth 1))) ,form)))))
                  (declare (ignorable ,cvar))
                  ,(if (null (cdr bodies))
                       (car bodies)
                       (let ((i -1))
                         `(case (%svref ,cluster 1)
                            ,@(mapcar (lambda (b) (list (if (cdr (member b bodies)) (setq i (+ i 1)) t) b))
                                      bodies)))))))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) (values nil condition))))

(defun %loop-deftype-expansion (type)
  "(EXPANSION) when TYPE names a type defined by DEFTYPE and expands without
error into something else, else NIL (for LOOP's default values)."
  (let* ((name (if (consp type) (car type) type))
         (expander (and (symbolp name) name (get name '%deftype))))
    (when expander
      (let ((e (ignore-errors (list (funcall expander (if (consp type) (cdr type) nil))))))
        (and e (not (equal (car e) type)) e)))))

;;; ------------------------------------------------------------
;;; errors signalled by the compiler-generated code
;;; ------------------------------------------------------------


(defun %type-error (datum expected)
  (error 'type-error :datum datum :expected-type expected))

;; the value of a declared parameter that the inline test of its type does
;; not decide (src/codegen.lisp, DECL-CHECK-CODE)
(defun %untyped (x) x)
(defun %check-declared-type (datum expected)
  ;; (a compound FUNCTION type is not a type for TYPEP, CLHS 4.2.3: what a
  ;; declaration of one can be checked for is that the value is a function)
  (if (typep datum (if (and (consp expected) (eq (car expected) 'function)) 'function expected))
      datum
      (%type-error datum expected)))

;;; (called by INVOKE-RESTART-INTERACTIVELY: the restarts refer to them by
;;; SYS:%WEAK-SYMBOL, so that only a program that invokes restarts
;;; interactively has the reader and the evaluator for them)
(defun %read-new-value ()
  (format *query-io* "~&Enter a form to evaluate: ")
  (finish-output *query-io*)
  (list (eval (read *query-io*))))
(defun %read-store-value ()
  (format *query-io* "~&New value: ")
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
  "Signal a PACKAGE-ERROR with a CONTINUE restart; returns T if continued.
REPORT, the restart's report, is a string, or a control and its arguments
(written by %MESSAGE-FORMAT: not by FORMAT, which a shaken program that only
makes packages would keep for it)."
  (restart-case (progn (error 'package-error :package package :format-control control
                                             :format-arguments args)
                       nil)
    (continue () :report (lambda (s) (if (stringp report) (write-string report s) (apply #'%message-format s report)))
      t)))

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
      :report (lambda (s) (%message-format s "Supply a new value for ~S." place))
      :interactive %read-store-value
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

;;; ------------------------------------------------------------
;;; The message of a condition nobody handled
;;;
;;; An error that reaches JavaScript (a LispError) or ends a program is
;;; reported with its message, PRINC-TO-STRING of the condition.  In a
;;; shaken program that does not print itself, the printer, the pretty
;;; printer and FORMAT (and the reader, which the printer's readtable keeps)
;;; would be there for that alone: %CONDITION-MESSAGE uses them when the
;;; program has them anyway (print.lisp puts the printer's functions on the
;;; plist of PRINT-OBJECT, which a shaken program keeps only if it refers
;;; to that symbol: the printer does, src/preinit.lisp), and otherwise a
;;; small printer of its own (%SMALL-PRINC, %SMALL-FORMAT): strings,
;;; symbols, numbers, characters, lists and vectors, other objects as
;;; #<TYPE>, and the directives of FORMAT that error messages use.  The
;;; standard conditions' reports are their %CONDITION-MESSAGE properties, a
;;; control and arguments that both print.
;;; ------------------------------------------------------------

(defmacro %define-condition-message (name (var) control &rest args)
  "The report of the condition NAME: FORMAT of CONTROL and ARGS, with VAR the condition."
  `(%put ',name '%condition-message (lambda (,var) (declare (ignorable ,var)) (list* ,control (list ,@args)))))

(defun %condition-slot (c name) (%slot-value-by-name c name))
;; (the name of a slot: by a reference that does not keep the function of
;; that name, as PATHNAME, in a shaken program)
(define-compiler-macro %condition-slot (&whole form c name)
  (if (and (consp name) (eq (car name) 'quote) (consp (cdr name)) (symbolp (cadr name)) (null (cddr name)))
      `(%slot-value-by-name ,c (%weak-symbol ,(cadr name)))
      form))

(defun %formatted-condition-message (c)
  (list* (or (%condition-slot c 'format-control) "") (%condition-slot c 'format-arguments)))
(%put 'simple-condition '%condition-message #'%formatted-condition-message)
(%put '%formatted-condition '%condition-message #'%formatted-condition-message)
(%put 'simple-type-error '%condition-message #'%formatted-condition-message)
(%define-condition-message type-error (c)
  "The value~%  ~S~%is not of type~%  ~S" (%condition-slot c 'datum) (%condition-slot c 'expected-type))
(%define-condition-message index-out-of-bounds (c)
  "Index ~S is out of bounds (expected ~S)" (%condition-slot c 'datum) (%condition-slot c 'expected-type))
(%define-condition-message end-of-file (c) "end of file on ~S" (%condition-slot c 'stream))
(%put 'file-error '%condition-message
      (lambda (c)
        (if (%condition-slot c 'format-control)
            (list* (%condition-slot c 'format-control) (%condition-slot c 'format-arguments))
            (list "error on file ~A" (%condition-slot c 'pathname)))))
(%put 'reader-error '%condition-message
      (lambda (c) (list* (or (%condition-slot c 'format-control) "reader error") (%condition-slot c 'format-arguments))))
(%define-condition-message unbound-variable (c) "The variable ~S is unbound." (%condition-slot c 'name))
(%define-condition-message undefined-function (c) "The function ~S is undefined." (%condition-slot c 'name))
(%define-condition-message unbound-slot (c)
  "The slot ~S is unbound in the object ~S." (%condition-slot c 'name) (%condition-slot c 'instance))
(%define-condition-message arithmetic-error (c) "arithmetic error ~S signalled" (%class-name (%instance-layout c)))
(%define-condition-message division-by-zero (c) "arithmetic error DIVISION-BY-ZERO signalled")

(defun %condition-report-args (c)
  "How the condition C is reported: (values FUNCTION nil) for a report
function of DEFINE-CONDITION, (values nil CONTROL-AND-ARGUMENTS) for a
%CONDITION-MESSAGE."
  (dolist (class (%class-cpl (%instance-layout c)) (values nil (list "Condition of type ~S was signalled." (%class-name (%instance-layout c)))))
    (let* ((name (%class-name class))
           (r (get name '%condition-report)))
      (when r (return (values r nil)))
      (let ((m (get name '%condition-message)))
        (when m (return (values nil (funcall m c))))))))

(defun %condition-message (c)
  "PRINC-TO-STRING of the condition C, by the small printer when the program has not the full one."
  (handler-case
      (let ((full (get (%weak-symbol print-object) '%full-message)))
        (if full
            (funcall full c)
            (%small-condition-message c)))
    (error () "<error while printing condition>")))

(defun %condition-type-string (c)
  "PRIN1-TO-STRING of the type of the condition C."
  (handler-case
      (let ((full (get (%weak-symbol print-object) '%full-prin1)))
        (if full
            (funcall full (type-of c))
            (let ((out (make-string-output-stream)))
              (%small-print (%class-name (%instance-layout c)) out t)
              (get-output-stream-string out))))
    (error () "ERROR")))

(defun %message-format (stream control &rest args)
  "FORMAT to the stream STREAM, by the small printer when the program has not
the full one: for the reports of the runtime's restarts, which only the
printer prints."
  (let ((full (get (%weak-symbol print-object) '%full-format)))
    (if full
        (apply full stream control args)
        (%small-format stream control args))))

(defun %small-condition-message (c)
  (let ((out (make-string-output-stream)))
    (if (conditionp c)
        (multiple-value-bind (fn args) (%condition-report-args c)
          (if fn
              (funcall fn c out)
              (%small-format out (car args) (cdr args))))
        (%small-print c out nil))
    (get-output-stream-string out)))

(defun %small-symbol-quote-p (name)
  (let ((n (length name)))
    (or (= n 0)
        (dotimes (i n nil)
          (let ((c (char name i)))
            (when (or (%small-position c " |\\:()'\";`,#" 0 nil) (not (graphic-char-p c)) (lower-case-p c)
                      (and (= i 0) (find c "#")))
              (return t))))
        (dotimes (i n t) (unless (eql (char name i) #\.) (return nil)))
        (dotimes (i n t)
          (let ((c (char name i)))
            (unless (or (digit-char-p c) (and (%small-position c "+-./" 0 nil) (> n 1)))
              (return nil)))))))

(defun %small-write-token (name out)
  (if (%small-symbol-quote-p name)
      (progn
        (write-char #\| out)
        (dotimes (i (length name))
          (let ((c (char name i)))
            (when (or (char= c #\|) (char= c #\\)) (write-char #\\ out))
            (write-char c out)))
        (write-char #\| out))
      (write-string name out)))

(defvar *%small-depth* 0)

;;; CHAR-NAME when the program has it; otherwise the names of the first 256
;;; characters and U+XXXX: CHAR-NAME of a character above Latin-1 reads
;;; the names of Unicode from a file (%UNICODE-NAMES), which would keep
;;; pathnames, OPEN and the reader of lines in every shaken module.
(defun %small-char-name (c)
  (let ((f (%symbol-function (%weak-symbol char-name))) (code (char-code c)))
    (cond (f (funcall (the function f) c))
          ((< code 256) (svref *char-names* code))
          (t (concatenate 'string "U+" (%integer-to-string code 16))))))

(defun %small-print-1 (x out escape)
  (let ((*%small-depth* (+ *%small-depth* 1))) (%small-print x out escape)))

(defun %small-print (x out escape)
  "Print X to the string stream OUT as PRIN1 (ESCAPE) or PRINC would in the
usual settings, without the printer: lists and vectors to a few levels."
  (cond ((stringp x)
         (if escape
             (progn (write-char #\" out)
                    (dotimes (i (length x))
                      (let ((c (char x i)))
                        (when (or (char= c #\") (char= c #\\)) (write-char #\\ out))
                        (write-char c out)))
                    (write-char #\" out))
             (write-string x out)))
        ((symbolp x)
         (let ((name (symbol-name x)) (pkg (symbol-package x)))
           (if escape
               (progn
                 (cond ((eq pkg *keyword-package*) (write-char #\: out))
                       ((null pkg) (write-string "#:" out))
                       ((eq pkg *package*))
                       (t (multiple-value-bind (found status) (find-symbol name *package*)
                            (unless (and status (eq found x))
                              (%small-write-token (package-name pkg) out)
                              (write-string (if (eq (nth-value 1 (find-symbol name pkg)) :external) ":" "::")
                                            out)))))
                 (%small-write-token name out))
               (write-string name out))))
        ((characterp x)
         (if escape
             (let ((name (and (not (graphic-char-p x)) (%small-char-name x))))
               (write-string "#\\" out)
               (if name (write-string name out) (write-char x out)))
             (write-char x out)))
        ((numberp x)
         (let ((*print-base* 10) (*print-radix* nil)) (%write-number x out)))
        ((consp x)
         (let ((prefix (and (consp (cdr x)) (null (cddr x))
                            (cdr (assoc (car x) '((quote . "'") (function . "#'")))))))
           (cond ((>= *%small-depth* 8) (write-char #\# out))
                 (prefix (write-string prefix out) (%small-print-1 (cadr x) out escape))
                 (t (write-char #\( out)
                    (let ((l x) (n 0))
                      (loop
                        (when (>= n 50) (write-string "..." out) (return))
                        (%small-print-1 (car l) out escape)
                        (setq l (cdr l) n (+ n 1))
                        (cond ((null l) (return))
                              ((atom l) (write-string " . " out) (%small-print-1 l out escape) (return)))
                        (write-char #\Space out)))
                    (write-char #\) out)))))
        ((and (vectorp x) (not (bit-vector-p x)))
         (if (>= *%small-depth* 8)
             (write-char #\# out)
             (progn
               (write-string "#(" out)
               (dotimes (i (min (length x) 50))
                 (when (> i 0) (write-char #\Space out))
                 (%small-print-1 (aref x i) out escape))
               (when (> (length x) 50) (write-string " ..." out))
               (write-char #\) out))))
        ((bit-vector-p x)
         (write-string "#*" out)
         (dotimes (i (length x)) (write-char (if (zerop (aref x i)) #\0 #\1) out)))
        ((and (functionp x) (%instancep (%closure-name x)))
         ;; (a generic function, as its PRINT-OBJECT: its class and name)
         (let ((info (%closure-name x)))
           (write-string "#<" out)
           (%small-print-1 (%class-name (%instance-layout info)) out t)
           (write-char #\Space out)
           (%small-print-1 (%instance-ref info 0) out t)
           (write-char #\> out)))
        ((functionp x)
         (write-string "#<FUNCTION " out)
         (let ((name (%closure-name x)))
           (if name (%small-print-1 name out t) (write-string "(LAMBDA)" out)))
         (write-char #\> out))
        ((%instancep x)
         (let ((class (%instance-layout x)))
           (cond
             ((and (conditionp x) (not escape))
              (write-string (%small-condition-message x) out))
             ((and (eq (%class-kind class) :structure) (null (car (%class-slot class +class-extra+))))
              (write-string "#S(" out)
              (%small-print-1 (%class-name class) out t)
              (dolist (sd (%class-effective-slots class))
                (write-string " :" out)
                (%small-write-token (symbol-name (%slotd-name sd)) out)
                (write-char #\Space out)
                (%small-print-1 (%instance-gref x (%slotd-location sd)) out escape))
              (write-char #\) out))
             (t
                 (write-string "#<" out)
                 (%small-print-1 (%class-name class) out t)
                 (write-string " {" out)
                 (%write-integer-digits (%identity-hash x) 16 out)
                 (write-string "}>" out)))))
        (t (write-string "#<" out)
           (%small-print-1 (type-of x) out t)
           (write-char #\> out))))

(defun %small-directive-end (control i)
  "The index after the directive that starts at I (after the tilde), and its character, parameters and modifiers."
  (let ((n (length control)) (params nil) (colon nil) (at nil) (cur nil))
    (loop
      (when (>= i n) (return (values i nil (nreverse params) colon at)))
      (let ((c (char control i)))
        (cond ((digit-char-p c) (setq cur (+ (* (or cur 0) 10) (digit-char-p c))) (setq i (+ i 1)))
              ((char= c #\,) (push cur params) (setq cur nil i (+ i 1)))
              ((and (char= c #\') (< (+ i 1) n)) (setq cur (char control (+ i 1)) i (+ i 2)))
              ((char= c #\:) (setq colon t i (+ i 1)))
              ((char= c #\@) (setq at t i (+ i 1)))
              (t (when cur (push cur params))
                 (return (values (+ i 1) c (nreverse params) colon at))))))))

;;; (a loop over a string, for the small printer: POSITION and POSITION-IF
;;; are generic, and their worker, %FIND-POSITION, is 50 KB)
(defun %small-position-if (pred s start end)
  (let ((end (or end (length s))))
    (do ((i start (+ i 1))) ((>= i end) nil)
      (when (funcall pred (char s i)) (return i)))))
(defun %small-position (c s start end)
  (%small-position-if (lambda (x) (eql x c)) s start end))

(defun %small-find-close (control start close)
  "The places of the directive CLOSE (a character) that ends the group after
START, and of the ~; at its level: (values close-start close-end separators)."
  (let ((i start) (depth 0) (seps nil) (n (length control)))
    (loop
      (let ((p (%small-position #\~ control i nil)))
        (when (null p) (return (values nil n (nreverse seps))))
        (multiple-value-bind (end c) (%small-directive-end control (+ p 1))
          (let ((c (and c (char-upcase c))))
            (cond ((member c '(#\[ #\( #\{ #\<)) (setq depth (+ depth 1)))
                  ((and (= depth 0) (eql c close)) (return (values p end (nreverse seps))))
                  ((member c '(#\] #\) #\} #\>)) (setq depth (- depth 1)))
                  ((and (= depth 0) (eql c #\;)) (push (cons p end) seps))))
          (setq i end))))))

(defun %small-format (out control args)
  "FORMAT to OUT of the string CONTROL with ARGS, by %SMALL-PRINT."
  (if (stringp control)
      (%small-format-1 out control 0 (length control) args args)
      (progn (%small-print control out nil)
             (dolist (a args) (write-char #\Space out) (%small-print a out t))))
  nil)

(defun %small-format-1 (out control start end args all)
  "Run the directives of CONTROL from START to END; returns the arguments
left, and true if ~^ ended it."
  (let ((i start))
    (macrolet ((next () `(if args (pop args) (error "no more arguments for ~S" control))))
      (loop
        (let ((p (%small-position #\~ control i end)))
          (write-string control out :start i :end (or p end))
          (when (null p) (return (values args nil)))
          (multiple-value-bind (j c params colon at) (%small-directive-end control (+ p 1))
            (setq i j)
            (case (and c (char-upcase c))
              ((#\A #\S) (let ((a (next)))
                           (if (and colon (null a))
                               (write-string (if (char-equal c #\A) "NIL" "()") out)
                               (%small-print a out (char-equal c #\S)))))
              ((#\D #\B #\O #\X)
               (let ((a (next)) (base (cdr (assoc (char-upcase c) '((#\D . 10) (#\B . 2) (#\O . 8) (#\X . 16))))))
                 (if (integerp a)
                     (let ((digits (let ((o (make-string-output-stream)))
                                     (%write-integer-digits (abs a) base o)
                                     (get-output-stream-string o))))
                       (when colon
                         (let ((r nil) (n (length digits)))
                           (dotimes (k n)
                             (when (and (> k 0) (zerop (mod (- n k) 3))) (push #\, r))
                             (push (char digits k) r))
                           (setq digits (coerce (nreverse r) 'string))))
                       (let ((text (concatenate 'string (cond ((minusp a) "-") (at "+") (t "")) digits)))
                         (dotimes (k (- (or (first params) 0) (length text))) (write-char #\Space out))
                         (write-string text out)))
                     (%small-print a out nil))))
              (#\C (let ((a (next)))
                     (if (and (characterp a) (or colon at))
                         (if at
                             (%small-print a out t)
                             (write-string (or (and (not (graphic-char-p a)) (%small-char-name a)) (string a)) out))
                         (%small-print a out nil))))
              (#\% (dotimes (k (or (first params) 1)) (terpri out)))
              (#\& (fresh-line out) (dotimes (k (- (or (first params) 1) 1)) (terpri out)))
              (#\~ (write-char #\~ out))
              (#\Newline (unless colon
                           (setq i (or (%small-position-if (lambda (ch) (not (member ch '(#\Space #\Tab)))) control i end)
                                       end)))
                         (when at (terpri out)))
              (#\P (when colon (setq args (cons (or (nth (- (length all) (length args) 1) all) 0) args)))
                   (let ((a (next)))
                     (if at
                         (write-string (if (eql a 1) "y" "ies") out)
                         (unless (eql a 1) (write-char #\s out)))))
              (#\* (if colon
                       (setq args (nthcdr (max 0 (- (length all) (length args) (or (first params) 1))) all))
                       (dotimes (k (or (first params) 1)) (next))))
              (#\? (let ((ctl (next)))
                     (if at
                         (setq args (%small-format-1 out ctl 0 (length ctl) args args))
                         (let ((sub (next))) (%small-format-1 out ctl 0 (length ctl) sub sub)))))
              (#\^ (when (null args) (return (values args t))))
              (#\( (multiple-value-bind (cs ce) (%small-find-close control i #\))
                     (let ((sub (make-string-output-stream)))
                       (setq args (%small-format-1 sub control i (or cs end) args all))
                       (let ((s (get-output-stream-string sub)))
                         (write-string (cond ((and colon at) (string-upcase s))
                                             (colon (string-capitalize s))
                                             (at (let ((d (string-downcase s))
                                                       (k (%small-position-if #'alpha-char-p s 0 nil)))
                                                   (when k (setf (char d k) (char-upcase (char d k))))
                                                   d))
                                             (t (string-downcase s)))
                                       out)))
                     (setq i ce)))
              (#\[ (multiple-value-bind (cs ce seps) (%small-find-close control i #\])
                     (let* ((starts (cons i (mapcar #'cdr seps)))
                            (ends (append (mapcar #'car seps) (list (or cs end))))
                            (clause
                              (cond (at (if (car args) 0 (progn (pop args) nil)))
                                    (colon (if (next) 1 0))
                                    (t (let ((k (or (first params) (next))))
                                         (if (and (integerp k) (< -1 k (length starts)))
                                             k
                                             ;; ~:; makes the last clause the default
                                             (and seps (let ((last (car (last seps))))
                                                         (eql (char control (- (cdr last) 2)) #\:))
                                                  (- (length starts) 1))))))))
                       (when clause
                         (multiple-value-bind (rest escaped)
                             (%small-format-1 out control (nth clause starts) (nth clause ends) args all)
                           (setq args rest)
                           (when escaped (return (values args t)))))
                       (setq i ce))))
              (#\{ (multiple-value-bind (cs ce) (%small-find-close control i #\})
                     (let* ((items (if at args (next))) (count 0))
                       (loop
                         (when (or (and (null items) (not (and colon (= count 0) nil))) (> count 1000))
                           (return))
                         (setq count (+ count 1))
                         (if colon
                             (let ((sub (pop items)))
                               (%small-format-1 out control i (or cs end) sub sub))
                             (multiple-value-bind (rest escaped)
                                 (%small-format-1 out control i (or cs end) items items)
                               (setq items rest)
                               (when escaped (return)))))
                       (when at (setq args items))
                       (setq i ce))))
              (t
               ;; (a directive that it does not know: written as it is)
               (write-string control out :start p :end j)))))))))

(defun %report-and-exit (condition)
  (let ((msg (%condition-message condition)))
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
