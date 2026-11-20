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
        *node-types* (make-hash-table :test 'eq)
        *ti-analyzed* (make-hash-table :test 'eq)
        *spec-clones* (make-hash-table :test 'equal)
        *spec-cloneable* (make-hash-table :test 'eq)
        *inline-candidates* (make-hash-table :test 'eq)
        *inlined-into* (make-hash-table :test 'eq)
        *declared-inline* (make-hash-table :test 'eq)
        *declared-ftypes* (make-hash-table :test 'eq)
        *lvar-lams* (make-hash-table :test 'eq))
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

(defun emit-toplevel-form (form env)
  (let* ((lam (ensure-toplevel-lam))
         (*current-lam* lam)
         (node (convert form env)))
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
      (host-eval-form form))))

(defparameter *toplevel-operators*
  '(progn eval-when locally macrolet symbol-macrolet defmacro in-package defpackage declaim))

(defun process-toplevel (form env &optional compile-time-too)
  ;; With COMPILE-TIME-TOO, an ordinary form is first evaluated on the host
  ;; (using host macros), then processed for the target.
  (when (and compile-time-too
             (not (and (consp form) (member (car form) *toplevel-operators*))))
    (host-eval form)
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
       (when (eq (car form) 'defpackage)
         (process-defpackage form env)
         (return))
       (let ((pkg (find-target-package (string (second form)))))
         (setf *read-package* pkg)
         (return (emit-toplevel-form `(clysm-sys::%in-package ,(target-package-name pkg)) env))))
      (declaim (dolist (d (cdr form)) (compile-time-proclaim d))
               (return (emit-toplevel-form `(clysm-sys::%proclaim-list ',(cdr form)) env)))
      (clysm-sys::%defun (return (process-defun form env nil)))
      (clysm-sys::%emit-deferred-macros (return (clysm-sys::%emit-deferred-macros-now env)))
      (t
       (multiple-value-bind (new changed) (if (get (car form) 'clysm-native)
                                              (values form nil)
                                              (cmacroexpand-1 form env))
         (unless changed
           (return (emit-toplevel-form form env)))
         (setf form new))))))

(defun compile-time-proclaim (d)
  (when (consp d)
    (case (car d)
      (special (dolist (s (cdr d)) (setf (gethash s *specials*) t)))
      (notinline (dolist (s (cdr d)) (setf (gethash (notinline-name s) *notinline*) t)))
      (inline (dolist (s (cdr d)) (remhash s *notinline*) (setf (gethash s *declared-inline*) t)))
      (ftype (when (consp (cdr d))
               (dolist (s (cddr d))
                 (let ((sym (notinline-name s)))
                   (when (symbolp sym) (setf (gethash sym *declared-ftypes*) (second d)))))))
      (t (loop for (spec . vars) in (type-decl-specs (list d))
               do (dolist (v vars) (when (and v (symbolp v)) (setf (gethash v *var-types*) spec))))))))

(defun clysm-sys::%compile-time-special (sym) (setf (gethash sym *specials*) t))
(defun clysm-sys::%compile-time-constant (sym value)
  (setf (gethash sym *specials*) t)
  (setf (gethash sym *constants*) (list value)))
(defun clysm-sys::%host-macro-function (sym) (gethash sym *macros*))

(defun process-defmacro (form env)
  (destructuring-bind (name ll &rest body) (cdr form)
    (let* ((expander-form (make-host-macro-expander name ll body))
           (fn (host-compile expander-form)))
      (setf (gethash name *macros*) fn)
      ;; non-CL symbols also become host macros, so that host-evaluated
      ;; code (macro bodies, eval-when) can use them
      (unless (eq (symbol-package name) (find-package :cl))
        (setf (macro-function name) fn))
      ;; target side: install a macro function at load time.  Until the
      ;; runtime has defined DESTRUCTURING-BIND, definitions are deferred.
      (if (target-macros-ready-p)
          (emit-target-macro name ll body env)
          (push (list name ll body) *deferred-macros*)))))

(defun target-macros-ready-p () *macros-ready*)

(defun emit-target-macro (name ll body env)
  (emit-toplevel-form
   `(progn
      (clysm-sys::%set-macro-function ',name
                                      (function (clysm-sys::named-lambda ,name ,@(cdr (target-macro-lambda name ll body)))))
      ,@(unless (eq (symbol-package name) (find-package :cl))
          ;; the pretty printer lays out &BODY macros like SBCL
          `((clysm-sys::%put ',name 'clysm-sys::%macro-lambda-list ',ll)))
      ',name)
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
  (let ((form (gensym "FORM")) (envv (gensym "ENV"))
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
           (fnode (convert (ftype-checked-function sym fn-form) env)))
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
      (when (and *live-mode* (symbolp name))
        (let ((src (and (gethash sym *declared-inline*) (eq (car fnode) :lambda)
                        (null (cenv-entries env))
                        (inline-expansion-source fn-form))))
          (when (and src
                     (not (ignore-errors (clysm-sys::%call-with-fasl-syntax
                                          (lambda () (prin1-to-string src) t)))))
            (setq src nil))
          (cond (src (setf (get sym 'clysm-sys::%inline-expansion) src)
                     (emit-toplevel-form `(clysm-sys::%put ',sym 'clysm-sys::%inline-expansion ',src) env))
                ((get sym 'clysm-sys::%inline-expansion)
                 (remprop sym 'clysm-sys::%inline-expansion)
                 (emit-toplevel-form `(remprop ',sym 'clysm-sys::%inline-expansion) env)))))
      (push `(:defun ,sym ,fnode) *toplevel-nodes*)
      ;; a small function defined at top level in the image: later calls may
      ;; contain a copy of its body (LIVE-INLINE-CALL)
      (when (and *live-inline-ok* (symbolp name) (eq (car fnode) :lambda)
                 (null (cenv-entries env)) (live-inline-record sym fn-form))
        (emit-toplevel-form `(clysm-sys::%note-live-inline ',sym) env)))))

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

(defvar *unit-env* nil
  "The environment of the toplevel forms of the compilation unit (COMPILE-FILE)
being compiled, or NIL (src/live.lisp).")

(defun directify-helper-calls (instrs)
  "Replace (global.get (:sym S)) (call (:callsym K)) by a direct call stub when
S names a statically known function (not in a compilation unit of COMPILE-FILE,
whose functions run after the segment that defines them, src/live.lisp)."
  (when *unit-env* (return-from directify-helper-calls instrs))
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

(defun finish-runtime ()
  "Called after the runtime sources have been processed."
  (let ((*read-package* (find-target-package "SYSTEM"))) (emit-primitive-wrappers))
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
    (let ((env (make-cenv)) (*read-package* *read-package*) (defuns nil))
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

(defun selected-runtime-inline-sources ()
  "((symbol . source) ...): the small functions of COMMON-LISP, and the small
functions of the runtime that their sources call (so that a copy of
NUMERATOR has a copy of %CHECK-INTEGER, whose type then flows), transitively."
  (let ((selected (make-hash-table :test 'eq)) (queue nil))
    (maphash (lambda (k v)
               (when (and v (eq (symbol-package k) (find-package :common-lisp)))
                 (setf (gethash k selected) v) (push v queue)))
             *runtime-inline-sources*)
    (labels ((walk (x)
               (when (consp x)
                 (let ((op (car x)))
                   (when (and (symbolp op) (not (gethash op selected)))
                     (let ((v (gethash op *runtime-inline-sources*)))
                       (when v (setf (gethash op selected) v) (push v queue)))))
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

(defun compile-source-forms (s path env note)
  (let ((line-of (or (ignore-errors (source-line-finder path)) (constantly nil)))
        (file (source-file-name path))
        (*current-source* nil))
    (loop
      (let* ((pos (ignore-errors (file-position s)))
             (line (ignore-errors (funcall line-of pos)))
             (form (target-read s nil s)))
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
    (and p (gethash p *host->target-name*))))

(defun symbol-global-init (sym)
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
      (:struct.new :symbol))))

(defun immediate-initial-value-p (sym v)
  (and (not (member sym '(nil t)))
       (or (fixnump* v) (characterp v) (null v) (eq v t))))

(defun initial-value-prologue ()
  (let ((code nil))
    (maphash (lambda (sym v)
               (unless (immediate-initial-value-p sym (car v))
                 (setf code (append code `((:global.get ,(sym-global sym))
                                           ,@(const-init-code (car v))
                                           (:struct.set :symbol ,+sym-value+))))))
             *initial-symbol-values*)
    code))

(defun add-identity-hash-cache ()
  "The globals of the :IDENTITY-HASH helper: the last object it hashed with the
host's identity hash, and that hash."
  (add-global *m* :ih-key :eqref t '((:ref.null :eq)))
  (add-global *m* :ih-hash :i32 t '((:i32.const 0))))

(defun build-module (&key (entry "_start"))
  (let ((*m* (make-wasm-module)))
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
      (add-export *m* "lisp_throw" :tag :lisp-throw)
      ;; the memory through which strings and octets cross to the host at once
      (setf (wasm-module-memory *m*) 1)
      (add-export *m* "memory" :memory nil)
      (add-export *m* "eval_string" :func :eval-string)
      ;; _start
      (add-func *m* :start :v-v :export entry
                :body (append (runtime-consts-prologue) (initial-value-prologue)
                       (loop for l in tops
                            append `((:global.get ,(static-closure-global l))
                                     (:call (:main ,(lam-id l))) :drop)))))
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
      (add-identity-hash-cache)
      ;; loading a heap image (runtime/image.lisp)
      (add-global *m* :img-objs '(:ref :null :vector) t '((:ref.null :vector)))
      (add-global *m* :img-pos :i32 t '((:i32.const 0)))
      (add-global *m* :img-roots :eqref t '((:ref.null :eq)))
      (dolist (g (reverse *misc-globals*))
        (destructuring-bind (name type mutable init) g
          (add-global *m* name type mutable init)))
      ;; symbol table: #(sym pkgindex external-p ...) and package table
      (let* ((pkgs (remove-duplicates
                    (loop for k being the hash-values of *host->target-name* collect k) :test #'string=))
             (pkg-index (lambda (name) (position name pkgs :test #'string=))))
        (let ((entries
                (loop for s in syms
                      collect (let* ((pn (symbol-target-package-name s))
                                    (ext (and pn (eq (nth-value 1 (find-symbol (symbol-name s) (symbol-package s)))
                                                     :external))))
                               `((:global.get (:sym ,s))
                                 ,@(fixnum-code (if pn (funcall pkg-index pn) -1))
                                 ,@(if ext `((:global.get (:sym t))) `((:global.get (:sym nil)))))))))
          ;; array.new_fixed is limited to 10000 operands: emit chunks of entries
          (let ((chunks (loop for rest = entries then (nthcdr 3000 rest)
                              while rest
                              collect (subseq rest 0 (min 3000 (length rest))))))
            (add-global *m* :symtab '(:ref :vector) nil
                        `(,@(loop for chunk in chunks
                                  append `(,@(apply #'append chunk)
                                           (:array.new_fixed :vector ,(* 3 (length chunk)))))
                          (:array.new_fixed :vector ,(length chunks))))))
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
    (when *shake* (shake-module *m*))
    (with-build-section (encode-module *m*))))

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
      ;; what refers to them otherwise: the code, the other globals
      (loop for f across (wasm-module-funcs *m*) do (walk (wasm-func-body f) pinned))
      (loop for g across (wasm-module-globals *m*) do (walk (fourth g) pinned))
      (dolist (g *closure-globals*) (walk (third g) pinned))
      (dolist (g *misc-globals*) (walk (fourth g) pinned))
      (dolist (c consts) (walk (third c) uses)))
    (let ((inlined (make-hash-table :test 'equal)) (out nil))
      (labels ((subst-init (init)
                 (loop for i in init
                       for v = (and (consp i) (eq (car i) :global.get) (consp (cdr i))
                                    (gethash (second i) inlined))
                       if v append v else collect i)))
        (dolist (c consts)
          (destructuring-bind (name type init) c
            (let ((init (subst-init init)))
              (if (and (not (gethash name pinned)) (eql (gethash name uses) 1))
                  (setf (gethash name inlined) init)
                  (push (list name type init) out))))))
      (nreverse out))))

(defun with-build-section (bytes)
  "BYTES with a custom section clysm.build at the end that names the image by
a hash of its octets: a heap image (runtime/image.lisp) is valid only for
the image it names."
  (let ((a 2166136261) (b 3735928559) (n (length bytes)))
    (declare (type (unsigned-byte 32) a b) (fixnum n)
             (type (simple-array (unsigned-byte 8) (*)) bytes))
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
GLOBALS (names), in pieces of at most 5000 (array.new_fixed is limited to
10000 operands)."
  (let ((chunks (loop for rest = globals then (nthcdr 5000 rest)
                      while rest
                      collect (subseq rest 0 (min 5000 (length rest))))))
    `(,@(loop for chunk in chunks
              append `(,@(loop for g in chunk collect `(:global.get ,g))
                       (:array.new_fixed :vector ,(length chunk))))
      (:array.new_fixed :vector ,(length chunks)))))

(defun add-image-functions (m)
  "The image's table of functions, which every function referred to by a
closure is placed in (from 0 for the image's own, from their code base for a
module compiled in it), and :IMAGE-OBJECTS, :IMAGE-MUTABLES and
:SET-IMAGE-MUTABLES (runtime/image.lisp)."
  (setf (wasm-module-table m) (list :define))
  (add-export m "codes" :table nil)
  (let ((objects nil) (mutables nil))
    (loop for (name type mutable) across (wasm-module-globals m)
          do (cond ((member name '(:mv-buf :dyn :img-objs :img-roots :empty-slots)))
                   ((not (and (consp type) (eq (car type) :ref)))
                    (when (and mutable (eq type :eqref)) (push name mutables)))
                   ((not mutable) (push name objects))))
    (setf objects (nreverse objects) mutables (nreverse mutables))
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
  "\"0.1.N (COMMIT)\" of the git checkout of RUNTIME-DIR's Clysm, or NIL."
  (ignore-errors
   (let* ((dir (truename (make-pathname :name nil :type nil :version nil :defaults runtime-dir)))
          (root (namestring (make-pathname :name nil :type nil :version nil
                                           :directory (butlast (pathname-directory dir))
                                           :defaults dir)))
          (git (lambda (&rest args)
                 (string-trim '(#\Newline #\Space)
                              (with-output-to-string (s)
                                #+sbcl (sb-ext:run-program "git" (list* "-C" root args) :search t :output s)
                                #-sbcl (progn s args nil))))))
     (let ((count (funcall git "rev-list" "--count" "HEAD")) (commit (funcall git "rev-parse" "--short" "HEAD")))
       (and (plusp (length count)) (plusp (length commit))
            (format nil "0.1.~A (~A)" count commit))))))

(defun compile-program (files &key (output "out.wasm") runtime-dir ((:shake *shake*) *shake*)
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
run time (INTERN, READ, EVAL of new code).  Returns the number of bytes written."
  (reset-compiler)
  (let ((*read-package* (find-target-package "SYSTEM")))
    (dolist (f (runtime-source-files runtime-dir)) (compile-source-file f))
    (let ((version (clysm-version runtime-dir)))
      (when version (process-toplevel `(setq clysm-sys::*clysm-version* ,version) (make-cenv))))
    (finish-runtime))
  (let ((*host-load-defuns* t))
    (dolist (f files) (compile-source-file f)))
  (let ((bytes (build-module)))
    (with-open-file (o output :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence bytes o))
    (length bytes)))
