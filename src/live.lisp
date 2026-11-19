;;;; live.lisp — compiling code into a running image ("live" modules)
;;;;
;;;; When the compiler is embedded in a Clysm image, COMPILE and the REPL can
;;;; turn forms into new WebAssembly modules that are linked with the running
;;;; image:
;;;;  * the base types form an identical rec group, so struct/array/function
;;;;    types are shared between the modules;
;;;;  * the multiple-value buffer, binding stack and exception tag are imported
;;;;    from the image;
;;;;  * symbols and literal objects are passed in at instantiation as a vector
;;;;    of the live objects themselves.

(in-package :clysm)

(defmacro with-fresh-codegen-state (() &body body)
  "Bind all per-module code generation state, so that compilations can nest
(e.g. a macro expander being compiled while a module is being built)."
  `(let ((*toplevel-lams* nil)
         (*current-toplevel* nil)
         (*toplevel-nodes* nil)
         (*toplevel-weight* 0)
         (*symbol-globals* nil)
         (*symbol-global-table* (make-hash-table :test 'eq))
         (*const-globals* nil)
         (*const-table* (make-hash-table :test 'eql))
         (*const-string-table* (make-hash-table :test 'equal))
         (*closure-globals* nil)
         (*closure-global-names* (make-hash-table :test 'equal))
         (*misc-globals* nil)
         (*pending-lams* nil)
         (*compiled-lams* (make-hash-table :test 'eq))
         (*extra-types* (make-hash-table :test 'equal))
         (*global-defuns* (make-hash-table :test 'eq))
         (*called-functions* (make-hash-table :test 'eq))
         (*runtime-consts* nil)
         (*live-constants* nil)
         (*helpers* *helpers*)
         (*current-lam* nil)
         ;; (tables of the lambdas, variables and nodes of this module: kept
         ;; in the image, they grew with every form compiled in it)
         (*node-types* (make-hash-table :test 'eq))
         (*ti-analyzed* (make-hash-table :test 'eq))
         (*inline-candidates* (make-hash-table :test 'eq))
         (*inlined-into* (make-hash-table :test 'eq))
         (*lvar-lams* (make-hash-table :test 'eq))
         (*spec-cloneable* (make-hash-table :test 'eq))
         (*spec-clones* (make-hash-table :test 'equal))
         (*catch-clones* (make-hash-table :test 'equal))
         (*macros-ready* t))
     ,@body))

(defun live-compile-forms (forms)
  "Compile FORMS into a module linked with the running image.
Returns (values bytes constants-vector)."
  (let ((*live-mode* t)
        (*live-packages* t)
        (*read-package* *package*)
        (*warnings* nil)
        (*error-output* (make-broadcast-stream)))
    (with-fresh-codegen-state ()
      (let ((env (make-cenv)))
        (dolist (f forms) (process-toplevel f env)))
      (when *current-toplevel*
        ;; keep the value of the last form
        (setf (lam-body *current-toplevel*) `(:progn (:const nil) ,@(reverse *toplevel-nodes*)))
        (push *current-toplevel* *toplevel-lams*)
        (setf *current-toplevel* nil *toplevel-nodes* nil *toplevel-weight* 0))
      (build-live-module))))

(defun live-used-globals (tops)
  "The names of the globals (and functions) that the code of a live module
can reach from its toplevel functions TOPS, the closures they refer to and
the other globals: a hash table."
  (let ((used (make-hash-table :test 'equal)) (queue nil)
        (closure-inits (make-hash-table :test 'equal))
        (rewrites (wasm-module-cast-rewrites *m*)))
    (dolist (g *closure-globals*) (setf (gethash (first g) closure-inits) (third g)))
    (labels ((mark (name)
               (unless (gethash name used)
                 (setf (gethash name used) t)
                 (let ((f (find-func *m* name)))
                   (when f (push (wasm-func-body f) queue)))
                 (multiple-value-bind (init found) (gethash name closure-inits)
                   (when found (push init queue)))))
             (walk (x)
               (when (consp x)
                 (case (car x)
                   ((:call :return_call :ref.func :global.get :global.set)
                    (when (consp (cdr x)) (mark (second x))))
                   (:ref.cast (when (and (consp (cdr x)) (not (eq (second x) :null)))
                                (let ((fn (cdr (assoc (second x) rewrites))))
                                  (when fn (mark fn))))))
                 (do ((l x (cdr l))) ((atom l)) (walk (car l))))))
      (dolist (l tops) (mark (list :main (lam-id l))) (mark (static-closure-global l)))
      (dolist (g *misc-globals*) (mark (first g)) (push (fourth g) queue))
      ;; (RUN casts the symbols it is given)
      (push '((:ref.cast :symbol)) queue)
      (loop while queue do (walk (pop queue))))
    used))

(defun add-called-helpers (castfns)
  "Add to the module the runtime helpers its functions call, and those they
call (a module made at run time uses few of them: making the code of all of
them, to drop most again, was most of the time of compiling a small form),
and the functions of the checked casts they make (CASTFNS, from
DEFINE-CHECKED-CASTS)."
  (let ((helpers (make-hash-table :test 'equal)) (next 0)
        (funcs (wasm-module-funcs *m*))
        (*fs* (make-fstate :lam nil :nparams 0)))
    (dolist (h *helpers*) (setf (gethash (first h) helpers) h))
    (labels ((add-helper (h)
               (destructuring-bind (name type locals body-fn) h
                 (let ((body (directify-helper-calls
                              (loop for item in (funcall body-fn)
                                    if (and (consp item) (eq (car item) :progn-marker))
                                      append (cdr item)
                                    else collect item))))
                   (add-func *m* name type
                             :locals (mapcar #'second locals)
                             :local-names (mapcar #'first locals)
                             :body (remove nil body)))))
             (need (name)
               (let ((h (gethash name helpers)))
                 (when (and h (not (func-defined-p *m* name)))
                   (add-helper h))))
             (need-cast (type)
               (let* ((name (cdr (assoc type (wasm-module-cast-rewrites *m*))))
                      (add (and name (gethash name castfns))))
                 (when add
                   (remhash name castfns)
                   (funcall add))))
             (walk (x)
               (when (consp x)
                 (case (car x)
                   ((:call :return_call :ref.func) (when (consp (cdr x)) (need (second x))))
                   (:ref.cast (when (and (consp (cdr x)) (not (eq (second x) :null)))
                                (need-cast (second x)))))
                 (do ((l x (cdr l))) ((atom l)) (walk (car l))))))
      ;; (RUN casts the symbols it is given)
      (need-cast :symbol)
      ;; (the helpers' code can make further functions be compiled: the
      ;; direct calls of DIRECTIFY-HELPER-CALLS)
      (loop (loop while (< next (fill-pointer funcs))
                  do (walk (wasm-func-body (aref funcs next))) (incf next))
            ;; (the closures of the globals refer to entries such as (:ARGERR k))
            (dolist (g *closure-globals*) (walk (third g)))
            (dolist (g *misc-globals*) (walk (fourth g)))
            (if (or *pending-lams* (< next (fill-pointer funcs)))
                (compile-pending-lams)
                (return))))))

(defun build-live-module ()
  (let ((*m* (make-wasm-module)))
    (loop for (name def) in *type-defs* do (add-type *m* name def))
    (setf (wasm-module-base-types *m*) (length *type-defs*))
    (loop for (name type mod field) in *imports* do (add-import-func *m* name type mod field))
    (add-import-global *m* :mv-count :i32 t "clysm" "mv_count")
    (add-import-global *m* :mv-buf '(:ref :vector) nil "clysm" "mv_buf")
    (add-import-global *m* :bstack '(:ref :vector) t "clysm" "bstack")
    (add-import-global *m* :bsp :i32 t "clysm" "bsp")
    (add-import-global *m* :poll-count :i32 t "clysm" "poll_count")
    (add-import-tag *m* :lisp-throw :exn "clysm" "lisp_throw")
    (setf (wasm-module-memory-import *m*) (list "clysm" "memory" 1))
    ;; its functions are placed in the image's table of functions, from
    ;; CODE_BASE (runtime/image.lisp)
    (add-import-global *m* :code-base :i32 nil "clysm" "code_base")
    (setf (wasm-module-table *m*) (list :import "clysm" "codes")
          (wasm-module-code-base *m*) :code-base)
    (sym-global nil) (sym-global t)
    (define-standard-helpers)
    (define-argerr-helpers)
    (let ((tops (reverse *toplevel-lams*))
          (castfns (define-checked-casts :lazy t)))
      (dolist (l tops) (ensure-lam-compiled l) (static-closure-global l))
      (compile-pending-lams)
      (add-called-helpers castfns)
      ;; globals: symbols, live constants and closures are filled in by RUN;
      ;; only those the code that can run refers to (every helper's symbols
      ;; were made, also those of the helpers it does not use)
      (let* ((used (live-used-globals tops))
             (syms (remove-if-not (lambda (s) (gethash (list :sym s) used)) (reverse *symbol-globals*)))
             (consts (remove-if-not (lambda (c) (gethash (car c) used)) (reverse *live-constants*)))
             (closures (remove-if-not (lambda (g) (gethash (first g) used)) (reverse *closure-globals*)))
             (objects (append syms (mapcar #'cdr consts))))
        (dolist (s syms)
          (add-global *m* (list :sym s) '(:ref :null :symbol) t '((:ref.null :symbol)))
          (setf (gethash (list :sym s) (wasm-module-nonnull-globals *m*)) t))
        (dolist (c consts)
          (add-global *m* (car c) :eqref t '((:ref.null :eq))))
        (dolist (g closures)
          (add-global *m* (first g) '(:ref :null :closure) t '((:ref.null :closure)))
          (setf (gethash (first g) (wasm-module-nonnull-globals *m*)) t))
        (dolist (g (reverse *misc-globals*))
          (destructuring-bind (name type mutable init) g
            (add-global *m* name type mutable init)))
        (add-func *m* :run :x-e :export "run"
                  :locals '((:ref :null :vector) :eqref) :local-names '(v result)
                  :body `((:local.get 0) :any.convert_extern (:ref.cast! :vector) (:local.set v)
                          ,@(loop for s in syms for i from 0
                                  append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                           (:ref.cast :symbol) (:global.set (:sym ,s))))
                          ,@(loop for c in consts for i from (length syms)
                                  append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                           (:global.set ,(car c))))
                          ,@(loop for g in closures
                                  append `(,@(third g) (:global.set ,(first g))))
                          ;; a form that leaves with an exit pending: RUN
                          ;; returns null too (its caller, %COMPILE-MODULE, looks)
                          (:block pend nil
                            ,@(loop for l in tops
                                    append `((:global.get ,(static-closure-global l))
                                             (:call (:main ,(lam-id l))) (:local.tee result)
                                             :ref.is_null
                                             (:if nil nil ((:call :exit-pending-p) (:br_if pend)))))
                            (:local.get result) :return)
                          (:ref.null :eq)))
        ;; for heap images (runtime/image.lisp): SNAP returns the values of
        ;; its globals, which RESTORE sets again in a new instance (whose RUN
        ;; is not called)
        (let ((globals (append (mapcar (lambda (s) (list :sym s)) syms)
                               (mapcar #'car consts)
                               (mapcar #'first closures)
                               (loop for (name type) in (reverse *misc-globals*)
                                     when (eq type :eqref) collect name))))
          (add-func *m* :snap :v-e :export "snap"
                    :locals '((:ref :null :vector)) :local-names '(v)
                    :body `((:i32.const ,(length globals)) (:array.new_default :vector) (:local.set v)
                            ,@(loop for g in globals for i from 0
                                    append `((:local.get v) (:i32.const ,i) (:global.get ,g)
                                             (:array.set :vector)))
                            (:local.get v)))
          (add-func *m* :restore :e-v :export "restore"
                    :locals '((:ref :null :vector)) :local-names '(v)
                    :body `((:local.get 0) (:ref.cast! :vector) (:local.set v)
                            ,@(loop for g in globals for i from 0
                                    append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                             ;; (plain casts: the values are
                                                             ;; those SNAP returned)
                                             ,@(case (car g)
                                                 (:sym '((:ref.cast! :symbol)))
                                                 (t (if (member g closures :key #'first)
                                                        '((:ref.cast! :closure))
                                                        nil)))
                                             (:global.set ,g))))))
        ;; a module made at run time only needs the helpers it calls
        (remove-unreachable-functions *m* used)
        (values (encode-module *m*) (coerce objects 'simple-vector))))))
