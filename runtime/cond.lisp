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
                      `(%define-condition-accessor ',r ',name (lambda (c) (%slot-value-by-name c ',sname)) nil)
                      `(defun ,r (c) (%slot-value-by-name c ',sname)))
                  accessor-forms))
          (dolist (w writers)
            (push (if generic
                      `(%define-condition-accessor ',w ',name (lambda (v c) (%set-slot-value-by-name c ',sname v)) t)
                      `(defun ,w (v c) (%set-slot-value-by-name c ',sname v)))
                  accessor-forms)))))
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

(defun %define-condition-accessor (name class fn writerp)
  (if *condition-accessor-hook*
      (funcall *condition-accessor-hook* name class fn writerp)
      (%set-fdefinition name fn))
  name)

(defun %define-condition-class (name supers direct-slots default-initargs)
  (let ((class (%ensure-class-object name *the-standard-class* (mapcar #'find-class supers)
                                     direct-slots :condition)))
    (%set-class-slot class +class-default-initargs+ default-initargs)
    class))

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

(defun make-condition (type &rest initargs)
  (let* ((class (%condition-class-for-type type))
         (c (%allocate-instance class)))
    (%initialize-slots-from-initargs c (append initargs (%class-default-initargs-all class initargs)))
    c))

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

(defstruct (restart (:constructor %make-restart (name function report-function interactive-function test-function)))
  name function report-function interactive-function test-function
  (conditions nil))                     ; the conditions it is associated with


(defvar *restart-clusters* nil)
(defvar *condition-restarts* nil)

(defun compute-restarts (&optional condition)
  (let ((r nil))
    (dolist (cluster *restart-clusters* (nreverse r))
      (dolist (restart cluster)
        (when (and (or (null condition) (null (restart-conditions restart))
                       (member condition (restart-conditions restart)))
                   (or (null (restart-test-function restart))
                       (funcall (restart-test-function restart) condition)))
          (push restart r))))))

(defun find-restart (name &optional condition)
  (if (restart-p name) name
      (dolist (r (compute-restarts condition) nil)
        (when (eq (restart-name r) name) (return r)))))

(defun %find-active-restart (restart)
  (let ((r (if (restart-p restart)
               (dolist (cluster *restart-clusters* nil)
                 (when (member restart cluster) (return restart)))
               (find-restart restart))))
    (unless r
      (error 'simple-control-error :format-control "The restart ~S is not active."
                                   :format-arguments (list restart)))
    r))

(defun invoke-restart (restart &rest args)
  (let ((r (%find-active-restart restart)))
    (apply (restart-function r) args)))

(defun invoke-restart-interactively (restart)
  (let ((r (%find-active-restart restart)))
    (apply (restart-function r)
           (if (restart-interactive-function r) (funcall (restart-interactive-function r)) nil))))

(defmacro restart-bind (bindings &body body)
  `(let ((*restart-clusters*
           (cons (list ,@(mapcar (lambda (b)
                                   (destructuring-bind (name fn &key report-function interactive-function test-function) b
                                     `(%make-restart ',name ,fn ,report-function ,interactive-function ,test-function)))
                                 bindings))
                 *restart-clusters*)))
     ,@body))

(eval-when (:compile-toplevel :load-toplevel :execute)
(defun %restart-case-signal-form (form env)
  "CLHS RESTART-CASE: when FORM is a call to SIGNAL, ERROR, CERROR or WARN,
the restarts are associated with the condition being signaled."
  ;; the compiler's %MACROEXPAND understands its environments (when the
  ;; compiler is in the image or on the host)
  (let ((exp (if (or (listp env) (not (fboundp '%macroexpand)))
                 (macroexpand form env)
                 (funcall '%macroexpand form env))))
    (if (and (consp exp) (member (car exp) '(signal error cerror warn)))
        (let ((c (gensym "C")))
          (multiple-value-bind (datum args fmt)
              (if (eq (car exp) 'cerror)
                  (values (third exp) (cdddr exp) (second exp))
                  (values (second exp) (cddr exp) nil))
            `(let ((,c (%coerce-to-condition ,datum (list ,@args)
                                             ',(case (car exp) (signal 'simple-condition)
                                                 (warn 'simple-warning) (t 'simple-error)))))
               (with-condition-restarts ,c (car *restart-clusters*)
                 ,(if (eq (car exp) 'cerror)
                      `(cerror ,fmt ,c ,@args)
                      `(,(car exp) ,c))))))
        form))))

(defmacro restart-case (form &rest clauses &environment env)
  (setq form (%restart-case-signal-form form env))
  (let* ((block (gensym "RC")) (args (gensym "ARGS"))
         (tags (mapcar (lambda (c) (declare (ignore c)) (gensym "R")) clauses)))
    `(block ,block
       (let ((,args nil))
         (declare (ignorable ,args))
         (tagbody
            (return-from ,block
              (restart-bind
                  ,(mapcar (lambda (c tag)
                             (let* ((name (car c)) (rest (cddr c))
                                    (report nil) (interactive nil) (test nil))
                               (do () ((not (member (car rest) '(:report :interactive :test))))
                                 (case (car rest)
                                   (:report (setq report (cadr rest)))
                                   (:interactive (setq interactive (cadr rest)))
                                   (:test (setq test (cadr rest))))
                                 (setq rest (cddr rest)))
                               `(,name (lambda (&rest temp) (setq ,args temp) (go ,tag))
                                       ,@(when report
                                           `(:report-function
                                             ,(if (stringp report)
                                                  `(lambda (s) (write-string ,report s))
                                                  `(function ,report))))
                                       ,@(when interactive `(:interactive-function (function ,interactive)))
                                       ,@(when test `(:test-function (function ,test))))))
                           clauses tags)
                ,form))
            ,@(mapcan (lambda (c tag)
                        (let* ((ll (cadr c)) (body (cddr c)))
                          (do () ((not (member (car body) '(:report :interactive :test))))
                            (setq body (cddr body)))
                          (list tag `(return-from ,block (apply (lambda ,ll ,@body) ,args)))))
                      clauses tags))))))

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

(defun %coerce-to-condition (datum args default-type &optional allow-args)
  (cond ((conditionp datum)
         (when (and args (not allow-args))
           (error 'simple-type-error :datum args :expected-type 'null
                                     :format-control "Arguments ~S given with the condition ~S"
                                     :format-arguments (list args datum)))
         datum)
        ((or (stringp datum) (functionp datum))
         (make-condition default-type :format-control datum :format-arguments args))
        ((symbolp datum) (apply #'make-condition datum args))
        ((%class-p datum) (apply #'make-condition datum args))
        (t (error 'simple-type-error :datum datum :expected-type '(or condition string symbol)
                                     :format-control "Bad condition designator ~S"
                                     :format-arguments (list datum)))))

(defun signal (datum &rest args)
  (let ((condition (%coerce-to-condition datum args 'simple-condition)))
    (let ((clusters *handler-clusters*))
      (do () ((null clusters))
        (let ((cluster (car clusters)))
          (setq clusters (cdr clusters))
          (dolist (binding cluster)
            (when (typep condition (car binding))
              (let ((*handler-clusters* clusters))
                (funcall (cdr binding) condition)))))))
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
         (condition (%coerce-to-condition datum args 'simple-error)))
    (signal condition)
    (invoke-debugger condition)))

(defun cerror (continue-format datum &rest args)
  (let ((c (%coerce-to-condition datum args 'simple-error t)))
    (restart-case (error c)
      (continue () :report (lambda (s) (apply #'format s continue-format args)) nil)))
  nil)

(defun warn (datum &rest args)
  (let ((condition (%coerce-to-condition datum args 'simple-warning)))
    (unless (typep condition 'warning)
      (error 'simple-type-error :datum condition :expected-type 'warning
                                :format-control "~S is not a warning" :format-arguments (list condition)))
    (restart-case (progn
                    (signal condition)
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
  (%report-and-exit condition))

(defun break (&optional (format-control "Break") &rest args)
  (format *error-output* "~&BREAK: ~?~%" format-control args)
  nil)

;;; handler macros
(defmacro handler-bind (bindings &body body)
  `(let ((*handler-clusters*
           (cons (list ,@(mapcar (lambda (b) `(cons ',(car b) ,(cadr b))) bindings))
                 *handler-clusters*)))
     ,@body))

(defmacro handler-case (form &rest clauses)
  (let* ((no-error (find :no-error clauses :key #'car))
         (clauses (remove :no-error clauses :key #'car))
         (block (gensym "HC")) (cvar (gensym "C"))
         (tags (mapcar (lambda (c) (declare (ignore c)) (gensym "TAG")) clauses)))
    (if no-error
        (let ((normal (gensym "NORMAL")))
          `(block ,normal
             (multiple-value-call (lambda ,(cadr no-error) ,@(cddr no-error))
               (block ,block
                 (return-from ,normal
                   (handler-case (return-from ,block ,form)
                     ,@clauses))))))
        `(block ,block
           (let ((,cvar nil))
             (declare (ignorable ,cvar))
             (tagbody
                (return-from ,block
                  (handler-bind ,(mapcar (lambda (c tag)
                                           `(,(car c) (lambda (temp) (setq ,cvar temp) (go ,tag))))
                                         clauses tags)
                    ,form))
                ,@(mapcan (lambda (c tag)
                            (let ((var (car (cadr c))) (body (cddr c)))
                              (list tag
                                    `(return-from ,block
                                       ,(if var
                                            `(let ((,var ,cvar)) ,@body)
                                            `(locally ,@body))))))
                          clauses tags)))))))

(defmacro ignore-errors (&body body)
  `(handler-case (progn ,@body)
     (error (condition) (values nil condition))))

;;; ------------------------------------------------------------
;;; errors signalled by the compiler-generated code
;;; ------------------------------------------------------------

(defun %type-error (datum expected)
  (error 'type-error :datum datum :expected-type expected))

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

