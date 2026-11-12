;;;; driver.lisp — toplevel form processing and module assembly

(in-package :clysm)

(defvar *deferred-macros* nil)
(defvar *macros-ready* nil)
(defvar *initial-symbol-values* (make-hash-table :test 'eq))
(defvar *toplevel-lams* nil "reverse list of toplevel lams")
(defvar *current-toplevel* nil)
(defvar *toplevel-nodes* nil)
(defparameter *toplevel-batch* 40)

(defun reset-compiler ()
  (setf *macros* (make-hash-table :test 'eq)
        *symbol-macros* (make-hash-table :test 'eq)
        *specials* (make-hash-table :test 'eq)
        *constants* (make-hash-table :test 'eq)
        *notinline* (make-hash-table :test 'eq)
        *compiler-macros* (make-hash-table :test 'eq)
        *global-defuns* (make-hash-table :test 'eq)
        *lam-counter* 0
        *warnings* nil
        *toplevel-lams* nil
        *current-toplevel* nil
        *toplevel-nodes* nil
        *symbol-globals* nil
        *symbol-global-table* (make-hash-table :test 'eq)
        *const-globals* nil
        *const-table* (make-hash-table :test 'eql)
        *const-string-table* (make-hash-table :test 'equal)
        *const-counter* 0
        *closure-globals* nil
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
        *runtime-consts* nil)
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
    (setf *current-toplevel* nil *toplevel-nodes* nil)))

(defun ensure-toplevel-lam ()
  (when (and *current-toplevel* (>= (length *toplevel-nodes*) *toplevel-batch*))
    (flush-toplevel))
  (unless *current-toplevel*
    (setf *current-toplevel* (make-lam :id (incf *lam-counter*) :name nil :toplevel t)))
  *current-toplevel*)

(defun emit-toplevel-form (form env)
  (let* ((lam (ensure-toplevel-lam))
         (*current-lam* lam)
         (node (convert form env)))
    (push node *toplevel-nodes*)))

(defun host-eval (form)
  #-clysm (setf form (sublis *env-function-substitutions* form))
  (let ((*package* *read-package*))
    (handler-bind ((warning #'muffle-warning))
      (eval form))))

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
                                                 (host-compile (make-host-macro-expander name ll mbody)))))))
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
      (notinline (dolist (s (cdr d)) (setf (gethash s *notinline*) t)))
      (inline (dolist (s (cdr d)) (remhash s *notinline*))))))

(defun clysm-sys::%compile-time-special (sym) (setf (gethash sym *specials*) t))
(defun clysm-sys::%compile-time-constant (sym value)
  (setf (gethash sym *specials*) t)
  (setf (gethash sym *constants*) (list value)))
(defun clysm-sys::%compile-time-symbol-macro (sym exp) (setf (gethash sym *symbol-macros*) exp))
(defun clysm-sys::%compile-time-compiler-macro (sym fn) (setf (gethash sym *compiler-macros*) fn))
(defun clysm-sys::%compile-time-macro (sym fn) (setf (gethash sym *macros*) fn))
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
      `(lambda (,form ,envv)
         (declare (ignorable ,form ,envv))
         (let (,@(when whole `((,whole ,form)))
               ,@(when envp `((,envp ,envv))))
           (destructuring-bind ,clean (cdr ,form)
             (declare ,@decls)
             ,@forms))))))

(defun process-defun (form env compile-time-too)
  (declare (ignore compile-time-too))
  ;; (sys:%defun 'name #'(sys:named-lambda name ll . body))
  (destructuring-bind (qname fn-form) (cdr form)
    (let* ((name (second qname))
           (sym (if (consp name) (setf-function-symbol (second name)) name))
           (lam (ensure-toplevel-lam))
           (*current-lam* lam)
           (fnode (convert fn-form env)))
      (when (and (eq (car fnode) :lambda) (null (lam-free-vars (second fnode))))
        (setf (lam-static-p (second fnode)) t)
        (push (second fnode) (gethash sym *global-defuns*)))
      (push `(:defun ,sym ,fnode) *toplevel-nodes*))))

(defun process-defpackage (form env)
  (destructuring-bind (name &rest options) (cdr form)
    (let* ((name (string name))
           (hp (ensure-host-package-for name))
           (nicknames nil) (uses nil) (exports nil) (shadows nil) (imports nil))
      (dolist (opt options)
        (case (car opt)
          (:nicknames (setf nicknames (mapcar #'string (cdr opt))))
          (:use (setf uses (mapcar #'string (cdr opt))))
          (:export (setf exports (append exports (mapcar #'string (cdr opt)))))
          (:shadow (setf shadows (append shadows (mapcar #'string (cdr opt)))))
          (:shadowing-import-from (push opt imports))
          (:import-from (push opt imports))))
      (dolist (n nicknames) (setf (gethash n *target-packages*) hp))
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
      (dolist (e exports) (export (intern e hp) hp))
      (emit-toplevel-form `(clysm-sys::%defpackage ,name ',nicknames ',uses ',exports ',shadows
                                                   ',(mapcar (lambda (o) (list* (car o) (string (second o))
                                                                                (mapcar #'string (cddr o))))
                                                             imports))
                          env))))

(defun directify-helper-calls (instrs)
  "Replace (global.get (:sym S)) (call (:callsym K)) by a direct call stub when
S names a statically known function."
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
        (add-func *m* name (list :direct-type k)
                  :body `((:global.get ,(static-closure-global lam))
                          ,@(loop for i below k collect `(:local.get ,i))
                          (:return_call (:main ,(lam-id lam)))))))))

(defun finish-runtime ()
  "Called after the runtime sources have been processed."
  (let ((*read-package* (find-target-package "SYSTEM"))) (emit-primitive-wrappers)))

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

(defun compile-forms (forms)
  (let ((env (make-cenv)))
    (dolist (f forms) (process-toplevel f env))))

(defun compile-source-file (path)
  (with-open-file (s path :external-format :utf-8)
    (let ((env (make-cenv)) (*read-package* *read-package*))
      (loop
        (let ((form (target-read s nil s)))
          (when (eq form s) (return))
          (handler-bind ((error (lambda (c)
                                  (format *error-output* "~&;; error while compiling ~A~%;;   form: ~S~%;;   ~A~%"
                                          path (if (consp form) (list (car form) (if (consp (cdr form)) (cadr form))) form) c))))
            (process-toplevel form env)))))))

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
      (add-export *m* "lisp_apply" :func :lisp-apply)
      (add-export *m* "mv_count" :global :mv-count)
      (add-export *m* "mv_buf" :global :mv-buf)
      (add-export *m* "bstack" :global :bstack)
      (add-export *m* "bsp" :global :bsp)
      (add-export *m* "lisp_throw" :tag :lisp-throw)
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
      (dolist (g (reverse *const-globals*))
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
      (add-global *m* :bstack '(:ref :vector) t '((:i32.const 1024) (:array.new_default :vector)))
      (add-global *m* :bsp :i32 t '((:i32.const 0)))
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
                                     (nicks (gethash pn *target-nicknames*)))
                                (list pn nicks uses)))))
          (let ((*const-globals* nil))
            ;; build init code directly (strings/lists) as constant expressions
            (labels ((lst (items)
                       (if (null items) `((:global.get (:sym nil)))
                           `(,@(car items) ,@(lst (cdr items)) (:struct.new :cons))))
                     (str (s) (string-array-code s)))
              (add-global *m* :pkgtab '(:ref :vector) nil
                          `(,@(loop for (pn nicks uses) in pkg-inits
                                    append (lst (list (str pn)
                                                      (lst (mapcar #'str nicks))
                                                      (lst (mapcar #'str uses)))))
                            (:array.new_fixed :vector ,(length pkg-inits)))))))))
    (report-undefined-functions)
    (encode-module *m*)))

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

(defun compile-program (files &key (output "out.wasm"))
  (reset-compiler)
  (dolist (f files) (compile-source-file f))
  (let ((bytes (build-module)))
    (with-open-file (o output :direction :output :element-type '(unsigned-byte 8) :if-exists :supersede)
      (write-sequence bytes o))
    (format t "~&; wrote ~A (~D bytes)~%" output (length bytes))
    output))
