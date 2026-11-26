;;;; driver.lisp — toplevel form processing and module assembly

(in-package :clysm)

(defvar *deferred-macros* nil)
(defvar *macros-ready* nil)
(defvar *initial-symbol-values* (make-hash-table :test 'eq))
(defparameter *live-imported-helpers*
  (append '(:undefined-function :argerr-n)
          (loop for k from 0 to +max-fixed-args+ collect (list :argerr k)))
  "Runtime helpers that a module evaluated at once imports from the image
(which exports them, *EXPORT-LIVE-HELPERS*) instead of carrying a copy: they
are called on errors only.  (The helpers of exits are on the path of every
exit; importing the functions of checked casts, called only when a cast fails,
made a THROW through 100 frames three times as slow.)")
(defun live-helper-export-name (name)
  (let ((*package* (find-package :keyword))) (format nil "h:~(~S~)" name)))

(defvar *export-live-helpers* nil
  "Building an image that embeds the compiler: it exports the helpers that the
modules compiled in it import (*LIVE-IMPORTED-HELPERS*).")
(defvar *toplevel-lams* nil "reverse list of toplevel lams")
(defvar *current-toplevel* nil)
(defvar *toplevel-nodes* nil)
(defparameter *toplevel-batch* 40)
(defvar *toplevel-weight* 0
  "The size of the nodes of the current toplevel function (see TOPLEVEL-WEIGHT).")
(defparameter *toplevel-max-weight* 4000
  "A toplevel function gets no more forms once its nodes have this many conses.")

(defun reset-compiler ()
  (setf *check-declared-integers* nil)  ; until FINISH-RUNTIME
  (setf *macros* (make-hash-table :test 'eq)
        *symbol-macros* (make-hash-table :test 'eq)
        *specials* (make-hash-table :test 'eq)
        *constants* (make-hash-table :test 'eq)
        *var-types* (make-hash-table :test 'eq)
        *notinline* (make-hash-table :test 'eq)
        *not-copied* (make-hash-table :test 'eq)
        *compiler-macros* (make-hash-table :test 'eq)
        *global-defuns* (make-hash-table :test 'eq)
        *lam-counter* 0
        *warnings* nil
        *toplevel-lams* nil
        *current-toplevel* nil
        *toplevel-nodes* nil
        *toplevel-weight* 0
        *symbol-globals* nil
        *symbol-global-table* (make-hash-table :test 'eq)
        *const-globals* nil
        *const-table* (make-hash-table :test 'eql)
        *const-string-table* (make-hash-table :test 'equal)
        *data-strings* nil
        *literal-scopes* (make-hash-table :test 'eq)
        *const-counter* 0
        *closure-globals* nil
        *closure-global-names* (make-hash-table :test 'equal)
        *misc-globals* nil
        *pending-lams* nil
        *compiled-lams* (make-hash-table :test 'eq)
        *extra-types* (make-hash-table :test 'equal)
        *ltv-counter* 0
        *called-functions* (make-hash-table :test 'eq)
        *initial-symbol-values* (make-hash-table :test 'eq)
        *deferred-macros* nil
        *macros-ready* nil
        *dynamic-functions* (make-hash-table :test 'eq)
        *runtime-consts* nil
        *js-name-cell-globals* (make-hash-table :test 'equal)
        *node-types* (make-hash-table :test 'eq)
        *ti-analyzed* (make-hash-table :test 'eq)
        *spec-clones* (make-hash-table :test 'equal)
        *raw-clones* (make-hash-table :test 'equal)
        *spec-cloneable* (make-hash-table :test 'eq)
        *inline-candidates* (make-hash-table :test 'eq)
        *inlined-into* (make-hash-table :test 'eq)
        *declared-inline* (make-hash-table :test 'eq)
        *declared-ftypes* (make-hash-table :test 'eq)
        *lvar-lams* (make-hash-table :test 'eq)
        *program-inline-expansions* nil)
  (setf (gethash 'clysm-sys::*catch-tags* *specials*) t)
  (init-target-packages)
  (setf *read-package* (find-target-package "COMMON-LISP-USER")))

;;; ------------------------------------------------------------
;;; Toplevel processing
;;; ------------------------------------------------------------

(defun flush-toplevel ()
  (when *current-toplevel*
    (setf (lam-body *current-toplevel*) `(:progn ,@(reverse *toplevel-nodes*) (:const nil)))
    (push *current-toplevel* *toplevel-lams*)
    (setf *current-toplevel* nil *toplevel-nodes* nil *toplevel-weight* 0)))

;;; Consecutive toplevel forms share a function, up to *TOPLEVEL-BATCH* of
;;; them, or fewer when they are large: their code (not that of the
;;; functions they define, which have functions of their own) is in that
;;; function, with locals of its own for each.  A function of thirty
;;; expansions of a macro that builds tables inline (Inquisitor's
;;; DEFINE-DFA) had 400 KB of code and 11000 locals, and the engine took
;;; 1.5 s to compile it (V8's baseline compiler takes time in the number of
;;; locals at every branch).

(defun toplevel-weight (node limit)
  "The number of conses of NODE, counting up to LIMIT."
  (let ((n 0) (stack (list node)))
    (loop while (and stack (< n limit))
          do (let ((x (pop stack)))
               (loop while (and (consp x) (< n limit))
                     do (incf n)
                        (when (consp (car x)) (push (car x) stack))
                        (setq x (cdr x)))))
    n))

(defun ensure-toplevel-lam ()
  (when (and *current-toplevel*
             (or (>= (length *toplevel-nodes*) *toplevel-batch*)
                 (>= *toplevel-weight* *toplevel-max-weight*)))
    (flush-toplevel))
  (unless *current-toplevel*
    (setf *current-toplevel* (make-lam :id (incf *lam-counter*) :name nil :toplevel t)))
  *current-toplevel*)

(defun emit-pending-forms ()
  "The toplevel forms that the form just converted needs before it (a
program built in the image: the functions that make its literal objects,
CONSTANT-NODE), into the same toplevel function."
  (loop while *static-pending-forms*
        do (let ((forms (reverse *static-pending-forms*))
                 (*toplevel-batch* most-positive-fixnum) (*toplevel-max-weight* most-positive-fixnum))
             (setq *static-pending-forms* nil)
             (dolist (f forms)
               (let ((*current-lam* *current-toplevel*))
                 (push (convert f (make-cenv)) *toplevel-nodes*))))))

(defun emit-toplevel-form (form env)
  (let* ((lam (ensure-toplevel-lam))
         (*current-lam* lam)
         (node (convert form env)))
    (when *static-pending-forms* (emit-pending-forms))
    (incf *toplevel-weight* (toplevel-weight node *toplevel-max-weight*))
    (push node *toplevel-nodes*)))

;;; The definitions of a program are evaluated here for its macros to call,
;;; and SBCL compiles what it evaluates.  Its compiler does not finish
;;; (minutes for one function, where it takes a moment) with some functions
;;; that are large only in how deep their forms nest: a loop in a loop in a
;;; LET, several times over, with a call of a function inside
;;; (tests/cases/deep-forms.lisp; the time goes to its propagation of
;;; constraints).  With DEBUG above SPEED it compiles them at once, but
;;; everything else many times slower, so that is not how all is compiled: a
;;; definition is given a while, and only one that takes longer is compiled
;;; again that way.

(defvar *host-definition-seconds* 20
  "How long the host may take to compile one definition of a program before
it is compiled again in the way that always ends.")

(defparameter *host-definers* '(defun defmacro defmethod define-compiler-macro)
  "The operators whose evaluation is the compiling of a function and nothing
that may not be done twice.")

(defun host-eval-form (form)
  #+sbcl
  (if (and (consp form) (member (car form) *host-definers*))
      (handler-case (sb-ext:with-timeout *host-definition-seconds* (eval form))
        (sb-ext:timeout ()
          ;; said, so that a build that became slow can be told why
          (format *error-output*
                  "~&; note: the host did not compile ~(~A~) ~S in ~D seconds; compiled again without its optimizations~%"
                  (car form) (if (consp (cdr form)) (second form) nil) *host-definition-seconds*)
          (with-compilation-unit (:policy '(optimize (debug 2) (speed 0)))
            (eval form))))
      (eval form))
  #-sbcl
  (eval form))

(defun host-eval (form)
  (setf form (substitute-host-functions form))
  (let ((*package* *read-package*))
    (handler-bind ((warning #'muffle-warning))
      (if *static-image*
          ;; (a program built in this image: the form is evaluated as
          ;; COMPILE-FILE evaluates it, and may change the package)
          (let ((package nil))
            (call-in-live-state (lambda ()
                                  ;; (the compiler's functions the form calls name
                                  ;; packages as they are here)
                                  (let ((*live-packages* t)) (host-eval-form form))
                                  (setq package *package*)))
            (when (and (packagep package) (not (eq package *read-package*)))
              (setq *read-package* package)))
          (host-eval-form form)))))

(defparameter *toplevel-operators*
  '(progn eval-when locally macrolet symbol-macrolet defmacro in-package defpackage declaim))

(defun process-toplevel (form env &optional compile-time-too)
  ;; With COMPILE-TIME-TOO, an ordinary form is first evaluated on the host
  ;; (using host macros), then processed for the target.
  (when (and compile-time-too
             (not (and (consp form) (member (car form) *toplevel-operators*))))
    ;; (a file of the runtime compiled in an image of that runtime: what the
    ;; form defines is there)
    (unless (member *static-image* '(:runtime :loaded)) (host-eval form))
    (setf compile-time-too nil))
  (loop
    (unless (consp form)
      (return (emit-toplevel-form form env)))
    (case (car form)
      (progn (dolist (f (cdr form)) (process-toplevel f env compile-time-too))
             (return))
      (eval-when
       (when *live-mode*
         ;; forms compiled into the running image are evaluated, not file-compiled
         (when (or (member :execute (second form)) (member 'eval (second form)))
           (dolist (f (cddr form)) (process-toplevel f env nil)))
         (return))
       (let* ((sits (second form))
              (ct (or (member :compile-toplevel sits) (member 'compile sits)))
              (lt (or (member :load-toplevel sits) (member 'load sits)))
              (ex (or (member :execute sits) (member 'eval sits))))
         (cond ((and lt ct) (dolist (f (cddr form)) (process-toplevel f env t)))
               ((and lt ex compile-time-too) (dolist (f (cddr form)) (process-toplevel f env t)))
               (lt (dolist (f (cddr form)) (process-toplevel f env nil)))
               ((or ct (and ex compile-time-too))
                (dolist (f (cddr form)) (host-eval f)))))
       (return))
      (locally
       (multiple-value-bind (decls forms) (parse-body (cdr form) :doc-allowed nil)
         (let ((env (if (declared-specials decls)
                        (apply #'env-extend env (mapcar (lambda (s) (list :special s)) (declared-specials decls)))
                        env)))
           (dolist (f forms) (process-toplevel f env compile-time-too))))
       (return))
      (macrolet
       (let ((new-env (apply #'env-extend env
                             (loop for (name ll . mbody) in (second form)
                                   collect (list :macro name
                                                 (local-macro-expander (make-host-macro-expander name ll mbody)))))))
         (dolist (f (cddr form)) (process-toplevel f new-env compile-time-too)))
       (return))
      (symbol-macrolet
       (let ((new-env (apply #'env-extend env
                             (loop for (s exp) in (second form) collect (list :smacro s exp)))))
         (dolist (f (cddr form)) (process-toplevel f new-env compile-time-too)))
       (return))
      (defmacro (process-defmacro form env) (return))
      ((in-package defpackage)
       (when *live-mode*
         (return (emit-toplevel-form form env)))
       ;; a program built in this image: the form is evaluated here, as
       ;; COMPILE-FILE does, and the program runs it as it is (the packages
       ;; of the program are the image's: SYMBOL-TARGET-PACKAGE-NAME)
       (when (eq *static-image* :user)
         (host-eval form)
         (return (emit-toplevel-form form env)))
       ;; (a file that is loaded in the image, :LOADED: its packages are there)
       (when (eq *static-image* :loaded)
         (when (eq (car form) 'in-package)
           (setf *read-package* (or (find-package (second form)) *read-package*)))
         (return (emit-toplevel-form form env)))
       (when (eq (car form) 'defpackage)
         (process-defpackage form env)
         (return))
       (let ((pkg (find-target-package (string (second form)))))
         (setf *read-package* pkg)
         (return (emit-toplevel-form `(clysm-sys::%in-package ,(target-package-name pkg)) env))))
      (declaim (dolist (d (cdr form)) (compile-time-proclaim d))
               ;; (a program built in this image: the image's compiler knows
               ;; the proclamation too, as after COMPILE-FILE)
               (when (eq *static-image* :user)
                 (call-in-live-state
                  (lambda ()
                    (let ((*live-mode* t) (*live-packages* t))
                      (dolist (d (cdr form))
                        (unless (and (consp d) (eq (car d) 'optimize)) (compile-time-proclaim d)))))))
               (return (emit-toplevel-form `(clysm-sys::%proclaim-list ',(cdr form)) env)))
      (clysm-sys::%defun (return (process-defun form env nil)))
      (clysm-sys::%note-compiler-macro
       ;; In the image (LOAD of a source file, COMPILE-FILE), the compiler
       ;; macro is in effect for the forms processed after it, also those of
       ;; the same toplevel PROGN (a DEFCLASS or DEFGENERIC giving the call
       ;; sites of a reader or generic function a cache, and code using them,
       ;; made by one macro), which are compiled before it is evaluated.
       (when (and *live-mode* (every (lambda (a) (and (consp a) (eq (car a) 'quote) (consp (cdr a))))
                                     (cdr form))
                  (eql (length (cdr form)) 3))
         (apply 'clysm-sys::%note-compiler-macro (mapcar #'second (cdr form))))
       (return (emit-toplevel-form form env)))
      (clysm-sys::%emit-deferred-macros (return (clysm-sys::%emit-deferred-macros-now env)))
      (t
       ;; a program built in this image: the definitions that COMPILE-FILE
       ;; evaluates when it compiles a file are evaluated here too, so that
       ;; the rest of the file is compiled with them as it would be there
       ;; (a structure's class for the type tests of its instances, a type,
       ;; a constant whose value is a number, character, symbol or string
       ;; for FIND in it, arithmetic on it).  Without this the program was
       ;; compiled with them only when COMPILE-FILE had just compiled the
       ;; same file for its fasl (the first build after the image changed),
       ;; and otherwise ran slower (cl-ppcre 13 to 25%).
       (when (and (eq *static-image* :user) (symbolp (car form)) (not (env-lookup-fun env (car form))))
         (cond ((member (car form) *static-compile-time-definers*) (host-eval form))
               ((eq (car form) 'defconstant) (static-note-defconstant form))))
       (multiple-value-bind (new changed) (if (get (car form) 'clysm-native)
                                              (values form nil)
                                              (cmacroexpand-1 form env))
         (unless changed
           (return (emit-toplevel-form form env)))
         (setf form new))))))

(defparameter *static-compile-time-definers*
  '(deftype defstruct defclass define-condition defsetf define-setf-expander define-modify-macro
    define-symbol-macro define-compiler-macro define-method-combination)
  "The operators of the toplevel forms that COMPILE-FILE evaluates when it
compiles them (SYS::%COMPILE-TIME-DEFINERS, but those PROCESS-TOPLEVEL
treats itself).")

(defun static-note-defconstant (form)
  "FORM, a toplevel DEFCONSTANT of a program built in this image: as
COMPILE-FILE does (SYS::%CF-NOTE-DEFCONSTANT), a value that can be computed
now and is a number, character, symbol or string defines the constant in
the image now."
  (when (and (consp (cdr form)) (symbolp (second form)) (second form)
             (not (keywordp (second form))) (consp (cddr form)))
    (let ((sym (second form)) (value-form (substitute-host-functions (third form))))
      (call-in-live-state
       (lambda ()
         (unless (boundp sym)
           (multiple-value-bind (v ok)
               (let ((*package* *read-package*))
                 (handler-bind ((warning #'muffle-warning))
                   (ignore-errors (values (eval value-form) t))))
             (when (and (eq ok t) (not (boundp sym))
                        (or (numberp v) (characterp v) (symbolp v) (stringp v)))
               (clysm-sys::%defconstant sym v)))))))))

(defun compile-time-proclaim (d)
  (when (consp d)
    (case (car d)
      (special (dolist (s (cdr d)) (setf (gethash s *specials*) t)))
      (notinline (dolist (s (cdr d)) (setf (gethash (notinline-name s) *notinline*) t)))
      (clysm-sys::not-copied (dolist (s (cdr d)) (setf (gethash (notinline-name s) *not-copied*) t)))
      (inline (dolist (s (cdr d))
                (let ((s (notinline-name s)))
                  (remhash s *notinline*) (remhash s *not-copied*) (setf (gethash s *declared-inline*) t))))
      (optimize (let ((safety (decls-safety (list d))))
                  (when safety (setf clysm-sys::*proclaimed-safety* safety))))
      (ftype (when (consp (cdr d))
               (dolist (s (cddr d))
                 (let ((sym (notinline-name s)))
                   (when (symbolp sym) (setf (gethash sym *declared-ftypes*) (second d)))))))
      (t (loop for (spec . vars) in (type-decl-specs (list d))
               do (dolist (v vars) (when (and v (symbolp v)) (setf (gethash v *var-types*) spec))))))))

(defun clysm-sys::%compile-time-special (sym) (table-put '*specials* sym t))
(defun clysm-sys::%compile-time-constant (sym value)
  (table-put '*specials* sym t)
  ;; (a program built in this image: the constant is the program's; the
  ;; image has it when the file is loaded, as after COMPILE-FILE)
  (table-put '*constants* sym (list value) (and *static-build* t)))
(defun clysm-sys::%host-macro-function (sym) (gethash sym *macros*))

(defun process-defmacro (form env)
  ;; (a file that is loaded in the image that builds the program, :LOADED:
  ;; its macros are the image's)
  (when (eq *static-image* :loaded)
    (destructuring-bind (name ll &rest body) (cdr form)
      (return-from process-defmacro (emit-target-macro name ll body env))))
  (destructuring-bind (name ll &rest body) (cdr form)
    (let* ((expander-form (make-host-macro-expander name ll body))
           (fn (global-macro-expander expander-form)))
      (setf (gethash name *macros*) fn)
      ;; non-CL symbols also become host macros, so that host-evaluated
      ;; code (macro bodies, eval-when) can use them
      (unless (or (eq (symbol-package name) (find-package :cl))
                  ;; (the image that builds the program has the macros of its runtime)
                  (eq *static-image* :runtime))
        (setf (macro-function name)
              ;; (what the host expands, when it pleases: SBCL when it
              ;; compiles a definition, Clysm's evaluator each time it runs
              ;; one; the symbols made then are not counted, CGENSYM)
              (lambda (form env)
                (let ((*cgensym-counter* *cgensym-counter*))
                  (funcall fn form env)))))
      ;; target side: install a macro function at load time.  Until the
      ;; runtime has defined DESTRUCTURING-BIND, definitions are deferred.
      (if (target-macros-ready-p)
          (emit-target-macro name ll body env)
          (push (list name ll body) *deferred-macros*)))))

(defun target-macros-ready-p () *macros-ready*)

(defun emit-target-macro (name ll body env)
  ;; (in a shaken program the expander is there only if something refers to
  ;; NAME: compiled code has expanded the macro, and only EVAL, COMPILE or
  ;; MACROEXPAND of a form made at run time would call it; the value of the
  ;; form is NAME, which only the definition refers to)
  (emit-toplevel-form
   `(clysm-sys::%when-reached
     ',name
     (progn
       (clysm-sys::%set-macro-function ',name
                                       (function (clysm-sys::named-lambda ,name ,@(cdr (target-macro-lambda name ll body)))))
       ,@(unless (eq (symbol-package name) (find-package :cl))
           ;; the pretty printer lays out &BODY macros like SBCL
           `((clysm-sys::%put ',name 'clysm-sys::%macro-lambda-list ',ll)))))
   env))

(defun clysm-sys::%emit-deferred-macros-now (env)
  (setf *macros-ready* t)
  (dolist (d (reverse *deferred-macros*))
    (destructuring-bind (name ll body) d
      (emit-target-macro name ll body env)))
  (setf *deferred-macros* nil))

(defun target-macro-lambda (name ll body)
  "A (lambda (form env) ...) form for the target, using the target's
destructuring-bind (defined by the runtime)."
  (declare (ignore name))
  (let ((form (cgensym "FORM")) (envv (cgensym "ENV"))
        (whole nil) (envp nil) (clean nil))
    (loop while ll
          do (let ((x (if (consp ll) (pop ll) (prog1 ll (setf ll nil) (push '&rest clean)))))
               (cond ((eq x '&whole) (setf whole (pop ll)))
                     ((eq x '&environment) (setf envp (pop ll)))
                     (t (push x clean)))))
    (setf clean (nreverse clean))
    (multiple-value-bind (decls forms) (parse-body body)
      (let ((inner `(destructuring-bind ,clean (cdr ,form)
                      (declare ,@decls)
                      ,@forms)))
        ;; &whole may be a destructuring pattern
        (when (consp whole)
          (setf inner `(destructuring-bind ,whole ,form ,inner) whole nil))
        `(lambda (,form ,envv)
           (declare (ignorable ,form ,envv))
           (let (,@(when whole `((,whole ,form)))
                 ,@(when envp `((,envp ,envv))))
             ,inner))))))

(defun process-defun (form env compile-time-too)
  (declare (ignore compile-time-too))
  ;; (sys:%defun 'name #'(sys:named-lambda name ll . body))
  (destructuring-bind (qname fn-form) (cdr form)
    (let* ((name (second qname))
           (sym (if (consp name) (setf-function-symbol (second name)) name))
           (lam (ensure-toplevel-lam))
           (*current-lam* lam)
           (*defun-param-specs*
             (and (consp fn-form) (eq (car fn-form) 'function) (consp (second fn-form))
                  (eq (car (second fn-form)) 'clysm-sys::named-lambda)
                  (consp (cdr (second fn-form))) (listp (third (second fn-form)))
                  (cons sym (param-decl-specs (third (second fn-form))
                                              (parse-body (cdddr (second fn-form)))))))
           (fnode (convert (ftype-checked-function sym fn-form) env)))
      (note-batch-defun sym)
      ;; a function of a whole program declaimed INLINE that the copies of
      ;; nodes do not serve (&OPTIONAL, &KEY, &REST): its source is kept
      ;; for its calls (INLINE-EXPANSION-CALL)
      (when (and *program-inline-expansions* (not *live-mode*) (symbolp name))
        (if (and (gethash sym *declared-inline*) (not (not-copied-p sym)) (eq (car fnode) :lambda)
                 (not (lam-simple-p (second fnode))) (null (cenv-entries env))
                 (not (tree-mentions-p (cdddr (second fn-form)) sym)))
            (let ((src (inline-expansion-source fn-form)))
              (if src
                  (setf (gethash sym *program-inline-expansions*) src)
                  (remhash sym *program-inline-expansions*)))
            (remhash sym *program-inline-expansions*)))
      (when (and (eq (car fnode) :lambda) (null (lam-free-vars (second fnode))))
        (setf (lam-static-p (second fnode)) t)
        (push (second fnode) (gethash sym *global-defuns*))
        ;; calls compiled before the definition is evaluated (later forms of
        ;; a file given to COMPILE-FILE) know its key layout
        (when (and *live-mode* (lam-keyp (second fnode)))
          (setf (gethash sym *compiled-key-layouts*) (key-layout-object (lam-key-layout (second fnode)))))
        ;; and the layout of one of required parameters and &REST (APPLY)
        (when (and *live-mode* (lam-rest-only-p (second fnode)))
          (setf (gethash sym *compiled-key-layouts*) (rest-layout-object (length (lam-req (second fnode))))))
        ;; in a development image the function may be redefined at run time
        (when *redefinable-functions* (setf (gethash sym *notinline*) t)))
      ;; in the image, the lambda list is kept for the development
      ;; environment (SWANK's argument lists)
      (when (and *live-mode* (consp fn-form) (eq (car fn-form) 'function)
                 (consp (second fn-form)) (eq (car (second fn-form)) 'clysm-sys::named-lambda)
                 (consp (cdr (second fn-form))) (consp (cddr (second fn-form))))
        (emit-toplevel-form `(clysm-sys::%put ',sym 'clysm-sys::%arglist ',(third (second fn-form))) env))
      (when (and *record-definitions* (symbolp name))
        (setf (gethash sym *runtime-inline-sources*)
              (and (eq (car fnode) :lambda) (null (cenv-entries env))
                   (runtime-inline-source sym fn-form))))
      ;; in the image, a function declaimed INLINE keeps its source for the
      ;; calls compiled later (INLINE-EXPANSION-CALL)
      (when (and *live-mode* (symbolp sym))
        (let ((src (and (gethash sym *declared-inline*) (eq (car fnode) :lambda)
                        (null (cenv-entries env))
                        (inline-expansion-source fn-form))))
          ;; (that of a SETF function, kept under its symbol, has the block
          ;; of its name: (RETURN-FROM X) in (SETF X))
          (when (and src (consp name))
            (multiple-value-bind (decls forms doc) (parse-body (cddr src))
              (setq src `(lambda ,(second src) ,@(when doc (list doc)) ,@(when decls `((declare ,@decls)))
                           (block ,(second name) ,@forms)))))
          (when (and src
                     (not (ignore-errors (clysm-sys::%call-with-fasl-syntax
                                          (lambda () (prin1-to-string src) t)))))
            (setq src nil))
          (cond (src (setf (get sym 'clysm-sys::%inline-expansion) src)
                     (emit-toplevel-form `(clysm-sys::%put ',sym 'clysm-sys::%inline-expansion ',src) env))
                ((get sym 'clysm-sys::%inline-expansion)
                 (remprop sym 'clysm-sys::%inline-expansion)
                 (emit-toplevel-form `(remprop ',sym 'clysm-sys::%inline-expansion) env)))))
      (when *static-pending-forms* (emit-pending-forms))
      (push `(:defun ,sym ,fnode) *toplevel-nodes*)
      ;; a small function defined at top level in the image: later calls may
      ;; contain a copy of its body (LIVE-INLINE-CALL)
      (when (and *live-inline-ok* (symbolp name) (eq (car fnode) :lambda)
                 (null (cenv-entries env)) (not (not-copied-p sym)) (live-inline-record sym fn-form))
        (emit-toplevel-form `(clysm-sys::%note-live-inline ',sym) env))
      ;; a small function of a file given to COMPILE-FILE: its source is kept
      ;; in the fasl for the calls compiled after it is loaded (LIVE-INLINE-CALL)
      (when (and *unit-env* (symbolp name) (eq (car fnode) :lambda) (null (cenv-entries env))
                 (not (gethash sym *notinline*)) (not (gethash sym *declared-inline*))
                 (not (not-copied-p sym)))
        (let ((src (let ((*live-inline-max-size* *fasl-inline-max-size*)) (live-inline-source sym fn-form))))
          (when (and src
                     (ignore-errors (clysm-sys::%call-with-fasl-syntax
                                     (lambda () (prin1-to-string src) t))))
            ;; (a mark of this compilation: the time, and a hash of the
            ;; source, since every session of the image starts from the
            ;; same random state)
            (let ((mark (format nil "~36R-~36R-~36R" (get-universal-time) (get-internal-real-time)
                                (sxhash (clysm-sys::%call-with-fasl-syntax
                                         (lambda () (prin1-to-string (cons sym src))))))))
              (emit-toplevel-form
               `(clysm-sys::%note-fasl-inline ',sym ',(first src) ',(second src) ,mark)
               env))))))))

(defun process-defpackage (form env)
  (destructuring-bind (name &rest options) (cdr form)
    (let* ((name (string name))
           (hp (ensure-host-package-for name))
           (nicknames nil) (uses nil) (exports nil) (shadows nil) (imports nil) (interns nil))
      (dolist (opt options)
        (case (car opt)
          (:nicknames (setf nicknames (mapcar #'string (cdr opt))))
          (:use (setf uses (mapcar #'string (cdr opt))))
          (:export (setf exports (append exports (mapcar #'string (cdr opt)))))
          (:shadow (setf shadows (append shadows (mapcar #'string (cdr opt)))))
          (:intern (setf interns (append interns (mapcar #'string (cdr opt)))))
          (:shadowing-import-from (push opt imports))
          (:import-from (push opt imports))))
      (dolist (n nicknames) (setf (gethash n *target-packages*) hp))
      (add-host-nicknames hp nicknames)
      (setf (gethash name *target-nicknames*) nicknames)
      (dolist (s shadows) (shadow s hp))
      (dolist (opt imports)
        (let ((from (find-target-package (string (second opt)))))
          (dolist (s (cddr opt))
            (let ((sym (or (find-symbol (string s) from) (intern (string s) from))))
              (if (eq (car opt) :shadowing-import-from)
                  (shadowing-import sym hp)
                  (import sym hp))))))
      (dolist (u uses) (use-package (find-target-package u) hp))
      (dolist (i interns) (intern i hp))
      (dolist (e exports) (export (intern e hp) hp))
      (emit-toplevel-form `(clysm-sys::%defpackage ,name ',nicknames ',uses ',exports ',shadows
                                                   ',(mapcar (lambda (o) (list* (car o) (string (second o))
                                                                                (mapcar #'string (cddr o))))
                                                             imports)
                                                   ',interns)
                          env))))


(defun directify-helper-calls (instrs)
  "Replace (global.get (:sym S)) (call (:callsym K)) by a direct call stub when
S names a statically known function (not in a compilation unit of COMPILE-FILE,
whose functions run after the segment that defines them, src/live.lisp)."
  (when *unit-env* (return-from directify-helper-calls instrs))
  ;; (nor in a batch of LOAD, whose functions may be redefined)
  (when *batch-forms* (return-from directify-helper-calls instrs))
  (let ((out nil))
    (loop while instrs
          do (let ((i (pop instrs)))
               (cond ((and (consp i) (eq (car i) :global.get) (consp (second i)) (eq (car (second i)) :sym)
                           (consp (car instrs)) (eq (car (car instrs)) :call)
                           (consp (second (car instrs))) (eq (car (second (car instrs))) :callsym)
                           (let ((lam (known-global-lam (second (second i)))))
                             (and lam (lam-simple-p lam)
                                  (= (length (lam-req lam)) (second (second (car instrs)))))))
                      (let* ((sym (second (second i))) (k (second (second (pop instrs))))
                             (stub (list :direct sym k)))
                        (ensure-direct-stub sym k)
                        (push `(:call ,stub) out)))
                     ((and (consp i) (member (car i) '(:block :loop)))
                      (push `(,(first i) ,(second i) ,(third i) ,@(directify-helper-calls (cdddr i))) out))
                     ((and (consp i) (eq (car i) :if))
                      (push `(:if ,(second i) ,(third i) ,(directify-helper-calls (fourth i))
                                  ,(directify-helper-calls (fifth i)))
                            out))
                     ((and (consp i) (eq (car i) :try_table))
                      (push `(:try_table ,(second i) ,(third i) ,(fourth i) ,@(directify-helper-calls (nthcdr 4 i))) out))
                     (t (push i out)))))
    (nreverse out)))

(defun ensure-direct-stub (sym k)
  (let ((name (list :direct sym k)))
    (unless (func-defined-p *m* name)
      (let ((lam (known-global-lam sym)))
        (ensure-lam-compiled lam)
        (let ((tn (list :direct-type k)))
          (unless (gethash tn *extra-types*)
            (setf (gethash tn *extra-types*) t)
            (add-type *m* tn `(:func ,(make-list k :initial-element :eqref) (:eqref)))))
        ;; (as (:CALLSYM K): an exit the function left pending is thrown)
        (add-func *m* name (list :direct-type k)
                  :body `((:block ok :eqref
                            (:global.get ,(static-closure-global lam))
                            ,@(loop for i below k collect `(:local.get ,i))
                            (:call (:main ,(lam-id lam)))
                            (:br_on_non_null ok)
                            (:call :exit-pending-p)
                            (:if nil nil ((:call :throw-pending-exit)))
                            (:ref.null :eq))))))))

(defvar *runtime-package-names* nil
  "The names of the packages there are once the runtime is compiled
(FINISH-RUNTIME).")

(defun finish-runtime ()
  "Called after the runtime sources have been processed."
  (let ((*read-package* (find-target-package "SYSTEM"))) (emit-primitive-wrappers))
  (setf *runtime-package-names* (loop for k being the hash-values of *host->target-name* collect k))
  (setf *program-inline-expansions* (make-hash-table :test 'eq))
  (setf *check-declared-integers* t))

(defun emit-primitive-wrappers ()
  "Define functions for primitives that have no function definition, so that
they can be FUNCALLed and used by the interpreter."
  (let ((env (make-cenv)))
    (maphash (lambda (name prims)
               (unless (or (gethash name *global-defuns*) (gethash name *macros*)
                           (get name 'clysm-native))
                 (let ((p (find-if (lambda (p) (integerp (prim-nargs p))) prims)))
                   (when (and p (= (length prims) 1))
                     (let ((args (loop for i below (prim-nargs p)
                                       collect (intern (format nil "A~D" i) :clysm-sys))))
                       (process-toplevel
                        `(clysm-sys::%defun ',name (function (clysm-sys::named-lambda ,name ,args (,name ,@args))))
                        env))))))
             *prims*)))

(defvar *host-load-defuns* nil
  "True while compiling user files ahead of time: after each file, its toplevel
DEFUNs are also defined on the host, as if the file had been compiled and
loaded before the next one, so that later files' macros can call them.")

(defun host-loadable-defun-p (form)
  (and (consp form) (eq (car form) 'defun) (consp (cdr form))
       (symbolp (second form))
       (let ((target (symbol-target-package-name (second form))))
         (and target (not (member target '("COMMON-LISP" "SYSTEM" "KEYWORD") :test #'string=))))))

(defun compile-source-file (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((env (make-cenv)) (*read-package* *read-package*) (defuns nil)
          ;; a DECLAIM of OPTIMIZE holds to the end of the file (as SBCL's COMPILE-FILE)
          (clysm-sys::*proclaimed-safety* clysm-sys::*proclaimed-safety*))
      (unwind-protect
           (compile-source-forms s path env (lambda (form)
                                              (when (and *host-load-defuns* (host-loadable-defun-p form))
                                                (push (cons form *read-package*) defuns))))
        (dolist (d (reverse defuns))
          (let ((*read-package* (cdr d)))
            (ignore-errors (host-eval (car d)))))))))

(defun source-line-finder (path)
  "A function of a file position of the file PATH (an octet offset) that
returns the line of the first form at or after it (whitespace and comments
skipped)."
  (let* ((octets (with-open-file (in path :element-type '(unsigned-byte 8))
                   (let ((v (make-array (file-length in) :element-type '(unsigned-byte 8))))
                     (read-sequence v in) v)))
         (n (length octets))
         ;; the positions of the newlines, in order (a line is found by a
         ;; binary search, not by counting from the start of the file)
         (newlines (let ((v (make-array 64 :adjustable t :fill-pointer 0)))
                     (dotimes (i n v) (when (= (aref octets i) 10) (vector-push-extend i v))))))
    (lambda (pos)
      (let ((p (or pos 0)))
        (loop
          (cond ((>= p n) (return))
                ((member (aref octets p) '(32 9 10 13 12)) (incf p))
                ((= (aref octets p) 59)          ; ;
                 (loop while (and (< p n) (/= (aref octets p) 10)) do (incf p)))
                ((and (= (aref octets p) 35) (< (1+ p) n) (= (aref octets (1+ p)) 124))   ; #|
                 (let ((e (search #(124 35) octets :start2 (+ p 2))))
                   (setf p (if e (+ e 2) n))))
                (t (return))))
        ;; 1 + the number of newlines before (MIN P N)
        (let ((end (min p n)) (lo 0) (hi (length newlines)))
          (loop while (< lo hi)
                do (let ((mid (floor (+ lo hi) 2)))
                     (if (< (aref newlines mid) end) (setf lo (1+ mid)) (setf hi mid))))
          (1+ lo))))))

(defvar *source-root* nil
  "A directory: the source files under it are named in stack traces by their
path relative to it (the Clysm installation, or the working directory).")

(defun source-file-name (path)
  (let* ((true (ignore-errors (namestring (truename path))))
         (roots (remove nil (list *source-root* (namestring *default-pathname-defaults*)))))
    (or (and true
             (loop for r in roots
                   for rs = (namestring r)
                   when (and (> (length true) (length rs)) (string= rs true :end2 (length rs)))
                     return (subseq true (length rs))))
        (namestring path))))

(defvar *record-definitions* nil
  "When true, the definitions of functions and macros named by symbols of
COMMON-LISP in the files compiled are collected in *RECORDED-DEFINITIONS*
(for the REPL image: argument lists and source locations for SWANK).")
(defvar *recorded-definitions* nil "((symbol arglist dspec file position) ...), last first")

(defun record-definition (form path pos)
  (when (and (consp form) (symbolp (car form)) (consp (cdr form)) (consp (cddr form))
             (string= "DEF" (symbol-name (car form)) :end2 (min 3 (length (symbol-name (car form))))))
    (let* ((op (car form)) (name (second form))
           (sym (cond ((symbolp name) name)
                      ((and (consp name) (eq (car name) 'setf) (symbolp (second name))) (second name)))))
      (when (and sym (eq (symbol-package sym) (find-package :common-lisp))
                 (not (eq op 'defmethod)))
        (let ((arglist (and (member (symbol-name op) '("DEFUN" "DEFMACRO") :test #'string=)
                            (symbolp name) (listp (third form))
                            (list (third form)))))
          (push (list (if (symbolp name) name (list 'setf sym))
                      arglist (list op name)
                      (ignore-errors (namestring (truename path))) pos)
                *recorded-definitions*))))))

(defun recorded-definitions-form ()
  "A form that gives the image the definitions collected (see
*RECORD-DEFINITIONS*), and the sources of its small functions."
  `(progn
     (clysm-sys::%install-definitions ',(reverse *recorded-definitions*))
     (let ((l ',(selected-runtime-inline-sources)))
       (loop (when (null l) (return))
             (clysm-sys::%put (car (car l)) 'clysm-sys::%inline-expansion (cdr (car l)))
             (setq l (cdr l))))))

;;; The small functions of COMMON-LISP in the REPL image keep their source,
;;; so that code compiled in the image contains a copy of them, as a whole
;;; program does (INLINE-EXPANSION-CALL; they are not redefined).
(defvar *runtime-inline-sources* (make-hash-table :test 'eq)
  "While the runtime of the REPL image is compiled: symbol -> the source of its small function, or NIL.")
(defparameter *runtime-inline-max-size* 100)
(defparameter *runtime-inline-also* '(clysm-sys::%gethash-marked clysm-sys::%ht-put-other)
  "Functions of the runtime (not of COMMON-LISP) that keep their source too:
the dispatch of GETHASH and of (SETF GETHASH) on the kind of the key, which
the compiler macros of both call for a key that is no fixnum, character or
symbol; copied, a string or a list goes to its helper without a call into
the image's module.")

(defun noreturn-source-p (source)
  "SOURCE, (lambda lambda-list . body), is a call of ERROR: a function that
signals an error (SYS::%TYPE-ERROR).  A copy of it in each caller makes the
caller larger for a path that does not run."
  (let ((last (car (last (cddr source)))))
    (and (consp last) (eq (car last) 'error))))

(defun selected-runtime-inline-sources ()
  "((symbol . source) ...): the small functions of COMMON-LISP (and
*RUNTIME-INLINE-ALSO*), and the small functions of the runtime that their
sources call (so that a copy of NUMERATOR has a copy of %CHECK-INTEGER,
whose type then flows), transitively; not those that only signal an error
(NORETURN-SOURCE-P)."
  (let ((selected (make-hash-table :test 'eq)) (queue nil))
    (maphash (lambda (k v)
               (when (and v (or (eq (symbol-package k) (find-package :common-lisp))
                                (member k *runtime-inline-also*)))
                 (setf (gethash k selected) v) (push v queue)))
             *runtime-inline-sources*)
    (labels ((walk (x)
               (when (consp x)
                 (let ((op (car x)))
                   (when (and (symbolp op) (not (gethash op selected)))
                     (let ((v (gethash op *runtime-inline-sources*)))
                       (when (and v (not (noreturn-source-p v)))
                         (setf (gethash op selected) v) (push v queue)))))
                 (loop while (consp x) do (walk (car x)) (setq x (cdr x))))))
      (loop while queue do (walk (cddr (pop queue)))))
    (loop for k being the hash-keys of selected using (hash-value v) collect (cons k v))))

(defun runtime-inline-source (sym fn-form)
  (when (and (consp fn-form) (eq (car fn-form) 'function) (consp (second fn-form))
             (eq (car (second fn-form)) 'clysm-sys::named-lambda))
    (let* ((l (second fn-form)) (ll (third l)) (body (cdddr l)))
      (and (listp ll) (every #'symbolp ll)
           (notany (lambda (x) (member x lambda-list-keywords)) ll)
           (not (tree-mentions-p body sym))
           (notany (lambda (op) (tree-mentions-p body op))
                   '(dotimes dolist do do* loop tagbody go prog prog* lambda function flet labels
                     macrolet symbol-macrolet return-from catch throw unwind-protect handler-case
                     handler-bind restart-case load-time-value clysm-sys::%wasm clysm-sys::%wasm-test))
           (<= (form-size body *runtime-inline-max-size*) *runtime-inline-max-size*)
           (list* 'lambda ll body)))))

(defun compile-source-forms (s path env note &optional (reader (lambda (s) (target-read s nil s))))
  (let ((line-of (or (ignore-errors (source-line-finder path)) (constantly nil)))
        (file (source-file-name path))
        (*current-source* nil))
    (loop
      (let* ((pos (ignore-errors (file-position s)))
             (line (ignore-errors (funcall line-of pos)))
             (form (funcall reader s)))
        (when (eq form s) (return))
        (setf *current-source* (and line (cons file line)))
        (when (and *record-definitions* pos) (record-definition form path pos))
        (funcall note form)
        (handler-bind ((error (lambda (c)
                                (format *error-output* "~&;; error while compiling ~A~%;;   form: ~S~%;;   ~A~%"
                                        path (if (consp form) (list (car form) (if (consp (cdr form)) (cadr form))) form) c))))
          (process-toplevel form env))))))

;;; ------------------------------------------------------------
;;; Module assembly
;;; ------------------------------------------------------------

(defun symbol-target-package-name (sym)
  (let ((p (symbol-package sym)))
    (and p (or (gethash p *host->target-name*)
               (and *image-packages* (image-target-package p))))))

;;; A symbol whose name is of at most 255 characters of codes below 256 (all
;;; but a few) is a global made empty ((:STRUCT.NEW_DEFAULT :SYMBOL)), whose
;;; name, hash, property list and flags :INIT-SYMBOLS sets from the data of
;;; the symbol table when the module starts (SYMBOL-TABLE-DATA): the
;;; initializer that held them was 57 bytes a symbol, a third of the global
;;; section, mostly the characters of the name as an (:I32.CONST C) each.
(defun data-named-symbol-p (sym)
  (let ((name (symbol-name sym)))
    (and (<= (length name) 255) (every (lambda (c) (< (char-code c) 256)) name))))

(defun symbol-global-init (sym)
  (if (data-named-symbol-p sym)
      '((:struct.new_default :symbol))
      (let ((name (symbol-name sym)))
        `(,@(string-array-code name)
          (:ref.null :package)
          ,@(multiple-value-bind (v found) (gethash sym *initial-symbol-values*)
              (if (and found (immediate-initial-value-p sym (car v)))
                  (const-init-code (car v))
                  '((:ref.null :eq))))
          (:ref.null :closure)
          ,@(if (null sym) '((:ref.null :eq)) `((:global.get ,(sym-global nil))))
          (:i32.const ,(string-hash name))
          (:i32.const ,(if (gethash sym *specials*) +flag-special+ 0))
          (:struct.new :symbol)))))

(defun immediate-initial-value-p (sym v)
  (and (not (member sym '(nil t)))
       (or (fixnump* v) (characterp v) (null v) (eq v t))))

(defun initial-value-prologue ()
  "The code setting the initial values of symbols that their initializers do
not set.  With *SHAKE-SYMBOLS*, each is a function (:INSTALL SYMBOL :VALUE)
of its own, kept only if something refers to SYMBOL (SHAKE-MODULE)."
  (let ((code nil) (n 0))
    (maphash (lambda (sym v)
               (unless (and (immediate-initial-value-p sym (car v)) (not (data-named-symbol-p sym)))
                 (let ((set `((:global.get ,(sym-global sym))
                              ,@(const-init-code (car v))
                              (:struct.set :symbol ,+sym-value+))))
                   (if *shake-symbols*
                       (let ((name (list :install sym :value (incf n))))
                         (add-func *m* name :v-e :body `(,@set (:global.get ,(sym-global sym))))
                         (push `((:call ,name) :drop) code))
                       (push set code)))))
             *initial-symbol-values*)
    (apply #'append (nreverse code))))

(defun symbol-table-data (syms pkg-index)
  "The data of the symbols SYMS (the order of the symbol table) for
:INIT-SYMBOLS and %INIT-PACKAGES: (values NAMES INFO), octet vectors.  NAMES
has the characters of the names of the DATA-NAMED-SYMBOL-P symbols, one after
the other; INFO three octets a symbol: its package's index in the package
table plus 1 (0: none, 255: read an unsigned LEB128 index from the tail),
its flags (1: external, 2: special, 4: its initializer sets its name) and
the length of its name. The tail follows all three-octet records."
  (let ((names (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))
        (info (make-array (* 3 (length syms)) :element-type '(unsigned-byte 8)
                          :adjustable t :fill-pointer (* 3 (length syms)))))
    (loop for s in syms for k from 0
          do (let* ((pn (symbol-target-package-name s))
                    (ext (and pn (eq (nth-value 1 (find-symbol (symbol-name s) (symbol-package s))) :external)))
                    (data (data-named-symbol-p s))
                    (pidx (if pn (1+ (funcall pkg-index pn)) 0)))
               (when (>= pidx 255)
                 (loop for rest = pidx then (ash rest -7)
                       do (vector-push-extend (logior (logand rest 127) (if (> rest 127) 128 0)) info)
                       while (> rest 127)))
               (setf (aref info (* 3 k)) (min pidx 255)
                     (aref info (+ (* 3 k) 1)) (logior (if ext 1 0) (if (gethash s *specials*) 2 0) (if data 0 4))
                     (aref info (+ (* 3 k) 2)) (if data (length (symbol-name s)) 0))
               (when data
                 (loop for c across (symbol-name s) do (vector-push-extend (char-code c) names)))))
    (values (coerce names '(simple-array (unsigned-byte 8) (*)))
            (coerce info '(simple-array (unsigned-byte 8) (*))))))

(defun symtab-init (syms)
  "The initializer of :SYMTAB: a vector of vectors of SYMS (array.new_fixed
takes at most 10000 operands)."
  (let ((chunks (loop for rest = syms then (nthcdr 9000 rest)
                      while rest
                      collect (subseq rest 0 (min 9000 (length rest))))))
    `(,@(loop for chunk in chunks
              append `(,@(loop for s in chunk collect `(:global.get (:sym ,s)))
                       (:array.new_fixed :vector ,(length chunk))))
      (:array.new_fixed :vector ,(length chunks)))))

(defun init-symbols-body (names-seg names-len info-seg info-len)
  "The body of :INIT-SYMBOLS, which _START calls first: the names, hashes,
property lists and flags of the symbols of :SYMTAB from the data segments
of SYMBOL-TABLE-DATA."
  `((:i32.const 0) (:i32.const ,names-len) (:array.new_data :bytes ,names-seg) (:local.set names)
    (:i32.const 0) (:i32.const ,info-len) (:array.new_data :bytes ,info-seg) (:local.set info)
    (:block done nil
      (:loop chunks nil
        (:local.get ci) (:global.get :symtab) :array.len :i32.ge_u (:br_if done)
        (:global.get :symtab) (:local.get ci) (:array.get :vector) (:ref.cast! :vector) (:local.set chunk)
        (:i32.const 0) (:local.set i)
        (:block chunk-done nil
          (:loop syms nil
            (:local.get i) (:local.get chunk) :array.len :i32.ge_u (:br_if chunk-done)
            (:local.get chunk) (:local.get i) (:array.get :vector) (:ref.cast! :symbol) (:local.set s)
            (:local.get info) (:local.get k) (:i32.const 3) :i32.mul (:i32.const 1) :i32.add
            (:array.get_u :bytes) (:local.set flags)
            (:local.get info) (:local.get k) (:i32.const 3) :i32.mul (:i32.const 2) :i32.add
            (:array.get_u :bytes) (:local.set len)
            (:local.get flags) (:i32.const 4) :i32.and :i32.eqz
            (:if nil nil
                 ((:local.get len) (:array.new_default :string) (:local.set str)
                  (:i32.const 0) (:local.set j)
                  (:block copied nil
                    (:loop copy nil
                      (:local.get j) (:local.get len) :i32.ge_u (:br_if copied)
                      (:local.get str) (:local.get j)
                      (:local.get names) (:local.get pos) (:local.get j) :i32.add (:array.get_u :bytes)
                      (:array.set :string)
                      (:local.get j) (:i32.const 1) :i32.add (:local.set j)
                      (:br copy)))
                  (:local.get s) (:local.get str) (:struct.set :symbol ,+sym-name+)
                  (:local.get s) (:local.get str) (:call :string-hash) (:ref.cast! :i31) :i31.get_s
                  (:struct.set :symbol ,+sym-hash+)
                  (:local.get s) (:global.get (:sym nil)) (:struct.set :symbol ,+sym-plist+)
                  (:local.get flags) (:i32.const 2) :i32.and
                  (:if nil nil ((:local.get s) (:i32.const ,+flag-special+) (:struct.set :symbol ,+sym-flags+)))))
            (:local.get pos) (:local.get len) :i32.add (:local.set pos)
            (:local.get k) (:i32.const 1) :i32.add (:local.set k)
            (:local.get i) (:i32.const 1) :i32.add (:local.set i)
            (:br syms)))
        (:local.get ci) (:i32.const 1) :i32.add (:local.set ci)
        (:br chunks)))))

(defvar *string-data-segment* nil
  "While BUILD-MODULE runs: the data segment of SET-STRING-TABLE.")

(defun set-string-table (m)
  "Make :STRTAB the vector (of vectors of at most 9000) of the strings of
*DATA-STRINGS* whose globals M has, the data segment their characters (one
octet each, in that order), and :INIT-STRINGS copy them into the strings."
  (let* ((names (remove-if-not (lambda (n) (global-defined-p m n)) (reverse (mapcar #'car *data-strings*))))
         (texts (let ((h (make-hash-table :test 'equal)))
                  (dolist (d *data-strings*) (setf (gethash (car d) h) (cdr d)))
                  h))
         (octets (make-array (loop for n in names sum (length (gethash n texts)))
                             :element-type '(unsigned-byte 8)))
         (chunks (loop for rest = names then (nthcdr 9000 rest)
                       while rest
                       collect (subseq rest 0 (min 9000 (length rest))))))
    (let ((i 0))
      (dolist (n names)
        (loop for c across (gethash n texts) do (setf (aref octets i) (char-code c)) (incf i))))
    (setf (aref (wasm-module-data m) *string-data-segment*) octets)
    (setf (fourth (find :strtab (wasm-module-globals m) :key #'first))
          `(,@(loop for chunk in chunks
                    append `(,@(loop for n in chunk collect `(:global.get ,n))
                             (:array.new_fixed :vector ,(length chunk))))
            (:array.new_fixed :vector ,(length chunks))))
    (let ((f (find-func m :init-strings)))
      (setf (wasm-func-ref-list f) :unknown
            (wasm-func-body f)
            `((:i32.const 0) (:i32.const ,(length octets)) (:array.new_data :bytes ,*string-data-segment*)
              (:local.set chars)
              (:block done nil
                (:loop chunks nil
                  (:local.get ci) (:global.get :strtab) :array.len :i32.ge_u (:br_if done)
                  (:global.get :strtab) (:local.get ci) (:array.get :vector) (:ref.cast! :vector) (:local.set chunk)
                  (:i32.const 0) (:local.set i)
                  (:block chunk-done nil
                    (:loop strs nil
                      (:local.get i) (:local.get chunk) :array.len :i32.ge_u (:br_if chunk-done)
                      (:local.get chunk) (:local.get i) (:array.get :vector) (:ref.cast! :string) (:local.set str)
                      (:local.get str) :array.len (:local.set len)
                      (:i32.const 0) (:local.set j)
                      (:block copied nil
                        (:loop copy nil
                          (:local.get j) (:local.get len) :i32.ge_u (:br_if copied)
                          (:local.get str) (:local.get j)
                          (:local.get chars) (:local.get pos) (:local.get j) :i32.add (:array.get_u :bytes)
                          (:array.set :string)
                          (:local.get j) (:i32.const 1) :i32.add (:local.set j)
                          (:br copy)))
                      (:local.get pos) (:local.get len) :i32.add (:local.set pos)
                      (:local.get i) (:i32.const 1) :i32.add (:local.set i)
                      (:br strs)))
                  (:local.get ci) (:i32.const 1) :i32.add (:local.set ci)
                  (:br chunks))))))))

(defvar *symbol-data-segments* nil
  "While BUILD-MODULE runs: (names-segment info-segment dropped-segment
pkg-index), the data segments of SET-SYMBOL-TABLE and the function giving the
index of a package (by name) in the package table.")

(defun dropped-symbols-data (syms pkg-index)
  "The octets of the symbols SYMS that a module made with *SHAKE-SYMBOLS*
left out, for an error when they are looked up (*%DROPPED-SYMBOLS* in
runtime/package.lisp): those of COMMON-LISP and of the packages the program
made (those of the runtime, but COMMON-LISP-USER, are its own), whose names
are DATA-NAMED-SYMBOL-P.  For each, its package's index plus 1, 1 if it is
external (else 0), the length of its name and its characters. A package
index of 255 escapes an unsigned LEB128 index immediately after that octet."
  (let ((out (make-array 0 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (dolist (s syms)
      (let ((pn (symbol-target-package-name s)))
        (when (and pn (data-named-symbol-p s)
                   (or (member pn '("COMMON-LISP" "COMMON-LISP-USER") :test #'string=)
                       (not (member pn *runtime-package-names* :test #'string=))))
          (let ((pidx (1+ (funcall pkg-index pn))))
            (vector-push-extend (min pidx 255) out)
            (when (>= pidx 255)
              (loop for rest = pidx then (ash rest -7)
                    do (vector-push-extend (logior (logand rest 127) (if (> rest 127) 128 0)) out)
                    while (> rest 127))))
          (vector-push-extend (if (eq (nth-value 1 (find-symbol (symbol-name s) (symbol-package s))) :external) 1 0)
                              out)
          (vector-push-extend (length (symbol-name s)) out)
          (loop for c across (symbol-name s) do (vector-push-extend (char-code c) out)))))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun set-symbol-table (m syms &optional dropped)
  "Make :SYMTAB the table of SYMS, and :INIT-SYMBOLS, :SYMBOL-INFO and
:DROPPED-SYMBOLS read their data (DROPPED: the symbols left out;
:DROPPED-SYMBOLS is NIL without *SHAKE-SYMBOLS*).  (Those of the functions
that a shaken module has.)"
  (flet ((set-body (name body)
           (let ((f (find-func m name)))
             (when f (setf (wasm-func-body f) body (wasm-func-ref-list f) :unknown)))))
    (destructuring-bind (names-seg info-seg dropped-seg pkg-index) *symbol-data-segments*
      (multiple-value-bind (names info) (symbol-table-data syms pkg-index)
        (let ((dropped (dropped-symbols-data dropped pkg-index)))
          (setf (aref (wasm-module-data m) dropped-seg) dropped)
          (set-body :dropped-symbols
                    (if *shake-symbols*
                        `((:i32.const 0) (:i32.const ,(length dropped)) (:array.new_data :bytes ,dropped-seg))
                        `((:global.get (:sym nil))))))
        (setf (aref (wasm-module-data m) names-seg) names
              (aref (wasm-module-data m) info-seg) info)
        (setf (fourth (find :symtab (wasm-module-globals m) :key #'first)) (symtab-init syms))
        (set-body :init-symbols (init-symbols-body names-seg (length names) info-seg (length info)))
        (set-body :symbol-info `((:i32.const 0) (:i32.const ,(length info)) (:array.new_data :bytes ,info-seg)))))))

(defun add-identity-hash-cache ()
  "The globals of the :IDENTITY-HASH helper: the last object it hashed with the
host's identity hash, and that hash."
  (add-global *m* :ih-key :eqref t '((:ref.null :eq)))
  (add-global *m* :ih-hash :i32 t '((:i32.const 0)))
  ;; the i64 (or the bits of the f64) that the helper :acc-step leaves for
  ;; the step of an accumulator that called it (typed.lisp, DUAL-LSET-CODE)
  (add-global *m* :acc-bits :i64 t '((:i64.const 0))))

(defvar *preinit-function* nil
  "NIL, or the function (src/preinit.lisp, loaded with the compiler on SBCL)
that shakes a program whose runtime's state is made from a heap saved when
it is compiled, instead of by the runtime's toplevel forms: called with the
module and *PREINIT-PARTS*.")
(defvar *runtime-top-count* nil
  "The number of toplevel functions of the runtime, once compiled (when *PREINIT-FUNCTION* is used).")
(defvar *preinit-through* nil
  "NIL, or the last source file whose toplevel forms run when compiling a shaken program.")
(defvar *preinit-parts* nil
  "(PROLOGUE RUNTIME-TOPS PROGRAM-TOPS): the code of the start function, in parts.")
(defvar *preinit-root* nil "The directory of Clysm (for host/run.mjs).")
(defvar *preinit-table-check* nil
  "The number of functions in the table when src/preinit.lisp placed the functions of the heap's closures.")

(defvar *shake-why-file* nil
  "NIL, or the file that BUILD-MODULE writes WRITE-SHAKE-WHY's report to.")

(defvar *built-module-hook* nil
  "A function called with the module of a program when its functions are
compiled, before it is encoded (STATIC-FINISH: *LIVE-DUMP*).")

(defun build-module (&key (entry "_start"))
  (let ((*m* (make-wasm-module
              ;; (a shaken program's functions are walked and rewritten after)
              :early (if (or *shake* *shake-why-file* *keep-instructions*) :uses :program))))
    (loop for (name def) in *type-defs* do (add-type *m* name def))
    (setf (wasm-module-base-types *m*) (length *type-defs*))
    (loop for (name type mod field) in *imports* do (add-import-func *m* name type mod field))
    (add-tag *m* :lisp-throw :exn)
    ;; make sure basic symbols exist
    (sym-global nil) (sym-global t)
    (do-external-symbols (s :cl) (sym-global s))
    (define-standard-helpers)
    (define-argerr-helpers)
    (define-checked-casts)
    ;; toplevel code
    (flush-toplevel)
    (let ((tops (reverse *toplevel-lams*)))
      (dolist (l tops) (ensure-lam-compiled l) (static-closure-global l))
      (compile-pending-lams)
      ;; helpers (they may reference new symbols)
      (let ((*fs* (make-fstate :lam nil :nparams 0)))
        (dolist (h (reverse *helpers*))
          (destructuring-bind (name type locals body-fn) h
            (let ((body (directify-helper-calls
                         (loop for item in (funcall body-fn)
                               if (and (consp item) (eq (car item) :progn-marker))
                                 append (cdr item)
                               else collect item))))
              (add-func *m* name type
                        :locals (mapcar #'second locals)
                        :local-names (mapcar #'first locals)
                        :body (remove nil body))))))
      (compile-pending-lams)
      ;; the helpers that modules compiled in the image import (src/live.lisp)
      (when *export-live-helpers*
        (dolist (name *live-imported-helpers*)
          (when (func-defined-p *m* name)
            (add-export *m* (live-helper-export-name name) :func name))))
      (when (and *export-live-helpers* (func-defined-p *m* :direct-entry))
        (add-export *m* "h:direct-entry" :func :direct-entry))
      (add-export *m* "lisp_apply" :func :lisp-apply)
      (add-export *m* "lisp_apply_async" :func :lisp-apply-async)
      (add-export *m* "lisp_recover" :func :lisp-recover)
      (add-export *m* "mv_count" :global :mv-count)
      (add-export *m* "mv_buf" :global :mv-buf)
      (add-export *m* "empty_slots" :global :empty-slots)
      (add-export *m* "dyn" :global :dyn)
      (add-export *m* "nil_symbol" :global (list :sym nil))
      ;; (for the hosts: the binding stack's top and vector)
      (add-func *m* :get-bsp :v-i :export "get_bsp" :body (bsp-code))
      (add-func *m* :get-bstack :v-e :export "get_bstack" :body (bstack-code))
      (add-export *m* "poll_count" :global :poll-count)
      (add-export *m* "ih_next" :global :ih-next)
      (add-export *m* "lisp_throw" :tag :lisp-throw)
      ;; the memory through which strings and octets cross to the host at once
      (setf (wasm-module-memory *m*) 1)
      (add-export *m* "memory" :memory nil)
      (add-export *m* "eval_string" :func :eval-string)
      ;; _start
      (flet ((calls (lams)
               (loop for l in lams
                     append `((:global.get ,(static-closure-global l))
                              (:call (:main ,(lam-id l))) :drop))))
        (let ((init '((:call :init-symbols) (:call :init-strings)))
              (prologue (append (runtime-consts-prologue) (initial-value-prologue))))
          (add-func *m* :start :v-v :export entry :body (append init prologue (calls tops)))
          ;; (a shaken program made from the heap of its runtime: src/preinit.lisp)
          (when (and *shake* *preinit-function* *runtime-top-count*)
            (setf *preinit-parts*
                  (list init prologue (calls (subseq tops 0 *runtime-top-count*))
                        (calls (nthcdr *runtime-top-count* tops))))))))
    ;; globals in dependency order
    ;; NIL first, then T, then the other symbols
    (let ((syms (reverse *symbol-globals*)))
      (setf syms (list* nil t (remove-if (lambda (s) (member s '(nil t))) syms)))
      ;; also intern symbols referenced by the symbol table itself
      (dolist (s syms)
        (add-global *m* (list :sym s) '(:ref :symbol) nil (symbol-global-init s)))
      (dolist (g (inline-single-use-constants (reverse *const-globals*)))
        (destructuring-bind (name type init) g
          (add-global *m* name (if (eq type :eqref) '(:ref :eq) type) nil init)))
      (dolist (g (reverse *closure-globals*))
        (destructuring-bind (name type init) g
          (add-global *m* name type nil init)))
      ;; constants built at startup (they contain long strings)
      (dolist (g (reverse *runtime-consts*))
        (add-global *m* (first g) :eqref t '((:ref.null :eq))))
      ;; runtime state
      (add-global *m* :mv-count :i32 t '((:i32.const 1)))
      (add-global *m* :mv-buf '(:ref :vector) nil `((:i32.const ,+mv-limit+) (:array.new_default :vector)))
      ;; the slot vector of structures whose slots are fields of their own
      (add-global *m* :empty-slots '(:ref :vector) nil '((:array.new_fixed :vector 0)))
      (add-global *m* :dyn '(:ref :dynstate) nil
                  '((:i32.const 0) (:i32.const 1024) (:array.new_default :vector) (:struct.new :dynstate)))
      (add-global *m* :poll-count :i32 t '((:i32.const 0)))
      ;; (the last identity hash given to an instance: the :IDENTITY-HASH helper)
      (add-global *m* :ih-next :i32 t '((:i32.const 0)))
      (add-identity-hash-cache)
      ;; loading a heap image (runtime/image.lisp)
      (add-global *m* :img-objs '(:ref :null :vector) t '((:ref.null :vector)))
      (add-global *m* :img-pos :i32 t '((:i32.const 0)))
      (add-global *m* :img-roots :eqref t '((:ref.null :eq)))
      (dolist (g (reverse *misc-globals*))
        (destructuring-bind (name type mutable init) g
          (add-global *m* name type mutable init)))
      ;; (a program built in the image: the packages of its symbols)
      (when *image-packages* (dolist (s syms) (symbol-target-package-name s)))
      ;; symbol table: #(sym pkgindex external-p ...) and package table
      (let* ((pkgs (remove-duplicates
                    (loop for k being the hash-values of *host->target-name* collect k) :test #'string=))
             (pkg-index (lambda (name) (position name pkgs :test #'string=))))
        ;; the symbol table (SET-SYMBOL-TABLE; again after SHAKE-MODULE
        ;; with *SHAKE-SYMBOLS*, which leaves out symbols)
        (setf *symbol-data-segments*
              (list (add-data *m* (make-array 0 :element-type '(unsigned-byte 8)))
                    (add-data *m* (make-array 0 :element-type '(unsigned-byte 8)))
                    (add-data *m* (make-array 0 :element-type '(unsigned-byte 8)))
                    pkg-index))
        (add-global *m* :symtab '(:ref :vector) nil nil)
        (add-func *m* :init-symbols :v-v
                  :locals '((:ref :null :bytes) (:ref :null :bytes) (:ref :null :vector) :i32 :i32 :i32 :i32
                            :i32 :i32 (:ref :null :symbol) (:ref :null :string) :i32)
                  :local-names '(names info chunk ci i k pos len flags s str j))
        (setf *string-data-segment* (add-data *m* (make-array 0 :element-type '(unsigned-byte 8))))
        (add-global *m* :strtab '(:ref :vector) nil nil)
        (add-func *m* :init-strings :v-v
                  :locals '((:ref :null :bytes) (:ref :null :vector) :i32 :i32 :i32 :i32 :i32
                            (:ref :null :string))
                  :local-names '(chars chunk ci i j pos len str))
        (set-string-table *m*)
        (add-func *m* :symbol-info :v-e)
        (add-func *m* :dropped-symbols :v-e)
        (set-symbol-table *m* syms)
        ;; pkgtab: vector of (name nicknames uses) built as constant lists of strings
        (let ((pkg-inits
                (loop for pn in pkgs
                      collect (let* ((hp (find-target-package pn))
                                     (uses (loop for u in (package-use-list hp)
                                                 for n = (gethash u *host->target-name*)
                                                 when n collect n))
                                     (nicks (gethash pn *target-nicknames*))
                                     ;; its shadowing symbols, as (name home-package):
                                     ;; known before its DEFPACKAGE form runs, since
                                     ;; the packages it uses are (an EXPORT from one
                                     ;; of them must not see a conflict)
                                     (shadows (loop for s in (package-shadowing-symbols hp)
                                                    for home = (symbol-target-package-name s)
                                                    when home collect (list (symbol-name s) home))))
                                (list pn nicks uses shadows)))))
          (let ((*const-globals* nil))
            ;; build init code directly (strings/lists) as constant expressions
            (labels ((lst (items)
                       (if (null items) `((:global.get (:sym nil)))
                           `(,@(car items) ,@(lst (cdr items)) (:struct.new :cons))))
                     (str (s) (string-array-code s)))
              (add-global *m* :pkgtab '(:ref :vector) nil
                          `(,@(loop for (pn nicks uses shadows) in pkg-inits
                                    append (lst (list (str pn)
                                                      (lst (mapcar #'str nicks))
                                                      (lst (mapcar #'str uses))
                                                      (lst (loop for (name home) in shadows
                                                                 collect (lst (list (str name) (str home))))))))
                            (:array.new_fixed :vector ,(length pkg-inits)))))))))
    ;; heap images (runtime/image.lisp): the table of functions, the objects
    ;; of the immutable globals, and the values of the mutable ones
    (add-image-functions *m*)
    (report-undefined-functions)
    (when *shake*
      (if *preinit-parts*
          (funcall *preinit-function* *m* *preinit-parts* #'after-shake)
          (progn (shake-module *m* :drop-symbols *shake-symbols* :keep-globals '(:symtab :pkgtab :strtab))
                 (after-shake *m*))))
    (when *built-module-hook* (funcall *built-module-hook* *m*))
    (order-functions-by-use *m*)
    (prog1 (let ((bytes (encode-module *m*)))
             ;; (the places of the functions in the table are those that
             ;; src/preinit.lisp gave the closures of the heap)
             (when (and *preinit-parts* *preinit-table-check*)
               (unless (= (hash-table-count (wasm-module-declared-refs *m*)) *preinit-table-check*)
                 (error "preinit: the table of functions changed while the module was encoded")))
             (with-build-section bytes))
      (when *shake-why-file*
        (with-open-file (o *shake-why-file* :direction :output :if-exists :supersede)
          (write-shake-why *m* *shake-why* *func-body-sizes* o))))))

(defun after-shake (m)
  "What a module needs once SHAKE-MODULE has shaken it: the table of the
strings left, and with *SHAKE-SYMBOLS* that of the symbols left."
  (set-string-table m)
  (when *shake-symbols*
    (let ((kept (loop for g across (wasm-module-globals m)
                      when (and (consp (first g)) (eq (car (first g)) :sym))
                        collect (second (first g))))
          (seen (make-hash-table :test 'eq)))
      (dolist (s kept) (setf (gethash s seen) t))
      (set-symbol-table m kept (remove-if (lambda (s) (gethash s seen)) (reverse *symbol-globals*))))))

(defun inline-single-use-constants (consts)
  "CONSTS ((name type init), in dependency order) without those that only
the initializer of one other constant refers to, whose initializers are put
in that one's instead (the cells of a quoted list but its first are then
not globals of their own).  Each global costs V8 the evaluation of its
initializer when it compiles and instantiates the image: of the 80000 of the
REPL image, 59000 were the conses of constant lists."
  (let ((pinned (make-hash-table :test 'equal))
        (uses (make-hash-table :test 'equal))
        (names (make-hash-table :test 'equal)))
    (dolist (c consts) (setf (gethash (first c) names) t))
    (labels ((walk (x table)
               (when (consp x)
                 (if (and (eq (car x) :global.get) (consp (cdr x)) (gethash (second x) names))
                     (incf (gethash (second x) table 0))
                     (do ((l x (cdr l))) ((atom l)) (walk (car l) table))))))
      ;; (the strings that :INIT-STRINGS fills are globals of their own)
      (dolist (d *data-strings*) (setf (gethash (car d) pinned) 1))
      ;; what refers to them otherwise: the code, the other globals
      (loop for f across (wasm-module-funcs *m*)
            do (dolist (name (nth-value 1 (wasm-func-uses f)))
                 (when (gethash name names) (incf (gethash name pinned 0)))))
      ;; (and the functions encoded when they were added)
      (maphash (lambda (name ref)
                 (when (and (plusp (nref-gets ref)) (gethash name names))
                   (incf (gethash name pinned 0) (nref-gets ref))))
               (wasm-module-global-refs *m*))
      (loop for g across (wasm-module-globals *m*) do (walk (fourth g) pinned))
      (dolist (g *closure-globals*) (walk (third g) pinned))
      (dolist (g *misc-globals*) (walk (fourth g) pinned))
      (dolist (c consts) (walk (third c) uses)))
    ;; (each instruction is put in its place once: copying the initializer
    ;; of the rest of a list into that of each cons before it took time and
    ;; space in the square of the length, 4 GB for a list of some ten
    ;; thousand elements)
    (let ((inlined (make-hash-table :test 'equal)) (out nil))
      (flet ((subst-init (init)
               (let ((r nil) (stack nil) (l init))
                 (loop
                   (cond (l (let* ((i (pop l))
                                   (v (and (consp i) (eq (car i) :global.get) (consp (cdr i))
                                           (gethash (second i) inlined))))
                              (if v
                                  (progn (when l (push l stack)) (setq l (car v)))
                                  (push i r))))
                         (stack (setq l (pop stack)))
                         (t (return (nreverse r))))))))
        (dolist (c consts)
          (destructuring-bind (name type init) c
            (if (and (not (gethash name pinned)) (eql (gethash name uses) 1))
                ;; (in a list: an initializer may be empty)
                (setf (gethash name inlined) (list init))
                (push (list name type (subst-init init)) out)))))
      (nreverse out))))

(defun with-build-section (bytes)
  "BYTES with a custom section clysm.build at the end that names the image by
a hash of its octets: a heap image (runtime/image.lisp) is valid only for
the image it names."
  ;; (BYTES declared where it is bound: a declaration of it in the LET below
  ;; is a free one, and each reference in the loop would test the type)
  (declare (type (simple-array (unsigned-byte 8) (*)) bytes))
  (let ((a 2166136261) (b 3735928559) (n (length bytes)))
    (declare (type (unsigned-byte 32) a b) (fixnum n))
    (dotimes (i n)
      (let ((x (aref bytes i)))
        (setf a (ldb (byte 32 0) (* (logxor a x) 16777619))
              b (ldb (byte 32 0) (* (logxor b x) 2246822519)))))
    (let* ((name "clysm.build")
           (text (format nil "~8,'0X~8,'0X" a b))
           (payload (+ 1 (length name) (length text)))
           (out (make-array (+ (length bytes) 2 payload) :element-type '(unsigned-byte 8)))
           (i (length bytes)))
      (replace out bytes)
      (flet ((put (x) (setf (aref out i) x) (incf i)))
        (put 0) (put payload) (put (length name))
        (loop for c across name do (put (char-code c)))
        (loop for c across text do (put (char-code c))))
      out)))

(defun vector-of-globals-code (globals)
  "Instructions making a simple vector of simple vectors of the values of
GLOBALS (names), in pieces of about the square root of their number (at least
100, at most 5000: array.new_fixed is limited to 10000 operands)."
  ;; Every value of a piece is on the operand stack until its array.new_fixed,
  ;; and the pieces until the last one: Firefox's baseline compiler takes time
  ;; that grows faster than the depth of the stack (pieces of 5000 took 56 ms of
  ;; its 71 ms on a page's 2.2 MB module, pieces of about 110 took 8 ms).
  ;; (the size in integers only, the same on every host: the ceiling of the
  ;; square root)
  (let* ((n (length globals))
         (size (min 5000 (max 100 (if (zerop n) 0 (1+ (isqrt (1- n)))))))
         (chunks (loop for rest = globals then (nthcdr size rest)
                       while rest
                       collect (loop repeat size for g in rest collect g))))
    `(,@(loop for chunk in chunks
              append `(,@(loop for g in chunk collect `(:global.get ,g))
                       (:array.new_fixed :vector ,(length chunk))))
      (:array.new_fixed :vector ,(length chunks)))))

(defun image-globals (m)
  "The names of the globals of M whose objects a heap image refers to by
their places (its immutable globals of references), and of those whose
values it saves (its mutable globals of type eqref): runtime/image.lisp."
  (let ((objects nil) (mutables nil))
    ;; (not :STRTAB, which :IMAGE-LOAD reads before the objects are made)
    (loop for (name type mutable) across (wasm-module-globals m)
          do (cond ((member name '(:mv-buf :dyn :img-objs :img-roots :empty-slots :strtab)))
                   ((not (and (consp type) (eq (car type) :ref)))
                    (when (and mutable (eq type :eqref)) (push name mutables)))
                   ((not mutable) (push name objects))))
    (values (nreverse objects) (nreverse mutables))))

(defun add-image-functions (m)
  "The image's table of functions, which every function referred to by a
closure is placed in (from 0 for the image's own, from their code base for a
module compiled in it), and :IMAGE-OBJECTS, :IMAGE-MUTABLES and
:SET-IMAGE-MUTABLES (runtime/image.lisp)."
  (setf (wasm-module-table m) (list :define))
  (add-export m "codes" :table nil)
  (multiple-value-bind (objects mutables) (image-globals m)
    ;; the conses, simple vectors and symbols among them: a heap image writes
    ;; all their fields (the records of runtime/image.lisp), so an image
    ;; started from one can make them empty (host/common.mjs imageVariant):
    ;; their contents were two thirds of the initializers that V8 decodes
    ;; and evaluates
    ;; (not in a whole program without the compiler, which does not load
    ;; heap images)
    (unless *shake*
     (let ((inits (make-hash-table :test 'equal)))
      (loop for (name nil nil init) across (wasm-module-globals m)
            do (setf (gethash name inits) init))
      (setf (wasm-module-image-defaults m)
            (loop for name in objects
                  for last = (car (last (gethash name inits)))
                  when (equal last '(:struct.new :cons))
                    collect (list name '(:struct.new_default :cons))
                  when (and (consp last) (eq (first last) :array.new_fixed) (eq (second last) :vector))
                    collect (list name `(:i32.const ,(third last)) '(:array.new_default :vector))
                  when (equal last '(:struct.new :symbol))
                    collect (list name '(:i32.const 0) '(:array.new_default :string)
                                  '(:ref.null :package) '(:ref.null :eq) '(:ref.null :closure) '(:ref.null :eq)
                                  '(:i32.const 0) '(:i32.const 0) '(:struct.new :symbol))))))
    (add-func m :image-objects :v-e :body (vector-of-globals-code objects))
    (add-func m :image-mutables :v-e :body (vector-of-globals-code mutables))
    ;; (a whole program without the compiler does not load heap images)
    (unless *shake*
      (add-export m "image_load" :func :image-load)
      (add-export m "image_module_snap" :func :image-module-snap)
      (add-export m "image_resume" :func :image-resume))
    (add-func m :set-image-mutables :e-v :locals '((:ref :null :vector)) :local-names '(v)
              :body `((:local.get 0) (:ref.cast! :vector) (:local.set v)
                      ,@(loop for g in mutables for i from 0
                              append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                       (:global.set ,g)))))))

(defun report-undefined-functions ()
  (let ((undefined nil))
    (loop for s being the hash-keys of *called-functions*
          when (gethash s *macros*)
            do (cwarn "~S was called as a function before being defined as a macro" s)
          unless (or (gethash s *global-defuns*) (gethash s *macros*) (gethash s *dynamic-functions*))
            do (push s undefined))
    (when undefined
      (format *error-output* "~&; undefined functions (~D):~%;  ~{~S~^ ~}~%"
              (length undefined) (sort undefined #'string< :key #'symbol-name)))))

(defun runtime-source-files (runtime-dir)
  (with-open-file (s (merge-pathnames "../tools/runtime-files.lisp" runtime-dir))
    (mapcar (lambda (f) (namestring (merge-pathnames (concatenate 'string f ".lisp") runtime-dir)))
            (read s))))

(defun clysm-version (runtime-dir)
  "\"0.1.N (COMMIT)\" of the git checkout of RUNTIME-DIR's Clysm, else the
line of its VERSION file (tools/build-npm.sh writes it), else, in the
self-hosted compiler (which runs no git), the version of the compiler itself
(of the same checkout when it was built: tools/build-selfhost.sh), or NIL.
A program compiled by either compiler has the same version (a form of its
own, which takes a place among the toplevel forms)."
  (or (clysm-git-version runtime-dir)
      (ignore-errors
       (with-open-file (in (merge-pathnames "../VERSION" runtime-dir))
         (read-line in)))
      ;; (the runtime's variable, in SYSTEM: CLYSM-SYS is a package of the
      ;; compiler's own in clysmc.wasm)
      #+clysm (let ((sym (find-symbol "*CLYSM-VERSION*" "SYSTEM")))
                (and sym (boundp sym) (symbol-value sym)))))

(defun clysm-git-version (runtime-dir)
  (ignore-errors
   (let* ((dir (truename (make-pathname :name nil :type nil :version nil :defaults runtime-dir)))
          (root (namestring (make-pathname :name nil :type nil :version nil
                                           :directory (butlast (pathname-directory dir))
                                           :defaults dir)))
          ;; (what git printed, or NIL when it failed: outside a checkout it
          ;; says so, and that was taken for the version)
          (git (lambda (&rest args)
                 (let* ((code nil)
                        (out (with-output-to-string (s)
                               #+sbcl (setq code (sb-ext:process-exit-code
                                                  (sb-ext:run-program "git" (list* "-C" root args)
                                                                      :search t :output s :error nil)))
                               #-sbcl (progn s args nil))))
                   (and (eql code 0) (string-trim '(#\Newline #\Space) out))))))
     (let ((count (funcall git "rev-list" "--count" "HEAD")) (commit (funcall git "rev-parse" "--short" "HEAD")))
       (and (plusp (length count)) (plusp (length commit))
            (format nil "0.1.~A (~A)" count commit))))))

(defun compile-program (files &key (output "out.wasm") runtime-dir ((:shake *shake*) *shake*)
                                   ((:preinit-through *preinit-through*) nil)
                                   ((:shake-symbols *shake-symbols*) *shake-symbols*)
                                   ((:shake-why *shake-why-file*) nil)
                                   ((:eval-string *eval-string*) *eval-string*)
                                   ((:source-root *source-root*)
                                    (and runtime-dir
                                         (ignore-errors
                                          (let ((dir (truename (make-pathname :name nil :type nil :version nil
                                                                              :defaults runtime-dir))))
                                            (make-pathname :name nil :type nil :version nil
                                                           :directory (butlast (pathname-directory dir))
                                                           :defaults dir))))))
  "Compile the runtime and FILES into the WebAssembly module OUTPUT.  With
SHAKE, the global functions that nothing refers to by name are left out
(SHAKE-MODULE): the program must not call functions by names it makes at
run time (INTERN, READ, EVAL of new code).  With SHAKE-SYMBOLS (and SHAKE),
the symbols that nothing refers to are left out too: the program must not
look up by name (INTERN, FIND-SYMBOL, READ) a symbol that its code does not
refer to.  With SHAKE-WHY (a file), the size of each kept function and what kept it are written there
(WRITE-SHAKE-WHY).  PREINIT-THROUGH names a source file in FILES: with SHAKE
on the SBCL compiler, run the program's toplevel forms through that file
when compiling, and save their heap with the runtime's.  Later files run
when the module loads.  Returns the number of bytes written."
  (when *preinit-through*
    (unless (and *shake* *preinit-function* runtime-dir)
      (error "PREINIT-THROUGH requires --shake and the SBCL compiler with its runtime"))
    (setf *preinit-through* (truename *preinit-through*))
    (unless (find *preinit-through* files :key #'truename :test #'equal)
      (error "PREINIT-THROUGH must name one of the program's source files")))
  (reset-compiler)
  ;; (the uninterned symbols of the module are named alike on SBCL and in
  ;; the self-hosted compiler: CGENSYM)
  (setf *cgensym-counter* 0)
  (setf *runtime-top-count* nil *preinit-parts* nil *preinit-table-check* nil)
  ;; (the prologue of _start makes the cells of the literal names of
  ;; JavaScript unless the module is shaken without the heap of
  ;; src/preinit.lisp: JS-NAME-CELL-GLOBAL.  When that heap cannot be made,
  ;; PREINIT-SHAKE shakes the module with its prologue, which keeps every
  ;; cell; it says so in a warning)
  (setf *name-cells-at-start* (or (not *shake*) (and *preinit-function* runtime-dir t)))
  (let* ((runtime-prefix (and *shake* runtime-dir
                              (ignore-errors
                               ;; (as the functions' sources are named, SOURCE-FILE-NAME)
                               (source-file-name (make-pathname :name nil :type nil :version nil
                                                                :defaults runtime-dir)))))
         ;; (in a shaken module, no typed copies of the runtime's large
         ;; functions of five required parameters or more: the workers of
         ;; the sequence functions)
         (*runtime-copy-limit* (and runtime-prefix (list runtime-prefix 5 300))))
    (compile-program-1 files output runtime-dir runtime-prefix)))

(defvar clysm-sys::%program-key-layout-hook nil
  "While the files of a whole program are compiled on the host: called by
the expansions of DEFGENERIC and DEFMETHOD (SYS::%NOTE-KEY-LAYOUT) with the
name of a generic function, the layout (nreq nopt . keywords) of its keyed
main function and whether the form is a DEFGENERIC.")

(defun compile-program-1 (files output runtime-dir runtime-prefix)
  ;; A call with keywords of a generic function of the program passes its
  ;; arguments in the slots of the function's keyed entry, its layout tested
  ;; when the call runs, as code compiled in the image does
  ;; (LIVE-KEYED-CALL-CODE); it made a vector of them for the generic
  ;; entry, which searched it.  The layouts are those the program's own
  ;; DEFGENERIC and DEFMETHOD forms give (the runtime's are not noted: its
  ;; code is as before).
  (let ((*compiled-key-layouts* (make-hash-table :test 'eq))
        (*live-key-layout-hook* (lambda (sym) (declare (ignore sym)) nil)))
    (compile-program-2 files output runtime-dir runtime-prefix)))

(defvar *after-runtime-hook* nil
  "When true, a function that COMPILE-PROGRAM calls with its FILES and
OUTPUT once the runtime is compiled; the program is then compiled with the
files and output it returns (two values).  tools/build-many.lisp forks
there, to compile many programs on one compiled runtime.")

(defun compile-program-2 (files output runtime-dir runtime-prefix)
  (let ((*read-package* (find-target-package "SYSTEM")))
    (dolist (f (runtime-source-files runtime-dir)) (compile-source-file f))
    (let ((version (clysm-version runtime-dir)))
      (when version (process-toplevel `(setq clysm-sys::*clysm-version* ,version) (make-cenv))))
    (finish-runtime))
  ;; (the runtime's toplevel functions, which src/preinit.lisp runs when
  ;; the program is compiled)
  (when (and *shake* *preinit-function* runtime-dir)
    (flush-toplevel)
    (setf *runtime-top-count* (length *toplevel-lams*)
          *preinit-root* (merge-pathnames "../" (make-pathname :name nil :type nil :version nil
                                                               :defaults runtime-dir))))
  (when *after-runtime-hook*
    (multiple-value-setq (files output) (funcall *after-runtime-hook* files output)))
  (let ((*host-load-defuns* t)
        (clysm-sys::%program-key-layout-hook
          (lambda (name layout defgeneric-p)
            (when (or defgeneric-p (not (gethash name *compiled-key-layouts*)))
              (setf (gethash name *compiled-key-layouts*) (key-layout-object layout)))))
        ;; (the program's generic functions: the runtime's call sites and
        ;; methods stay as they are, so that a program without generic
        ;; functions of its own carries no dispatch)
        (clysm-sys::*gf-unit* (clysm-sys::%make-gf-unit)))
    (dolist (f files)
      (compile-source-file f)
      (when (and *preinit-through* (equal (truename f) *preinit-through*))
        (gf-unit-flush (make-cenv))
        (flush-toplevel)
        (setf *runtime-top-count* (length *toplevel-lams*))))
    (gf-unit-flush (make-cenv)))
  (let ((bytes (let ((*shake-why* (and *shake-why-file* (make-hash-table :test 'equal)))
                      (*func-body-sizes* (and *shake-why-file* (make-hash-table :test 'equal)))
                      ;; (no branch hints in the runtime's large functions)
                      (clysm-wasm::*runtime-hint-limit*
                        (and runtime-prefix (cons runtime-prefix 800))))
                  (build-module))))
    (with-open-file (o output :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence bytes o))
    (length bytes)))
