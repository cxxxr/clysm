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

(defvar *live-helper-imports* nil
  "True in an image that exports *LIVE-IMPORTED-HELPERS* (tools/build-repl.lisp).")

(defun live-helper-type (name)
  (cond ((and (consp name) (eq (car name) :argerr)) (fixed-entry-type (second name)))
        ((and (consp name) (eq (car name) :callsym)) (list :callsym-type (second name)))
        ((and (consp name) (eq (car name) :castfn)) (list :cast-type (second name)))
        (t (second (assoc name *helpers* :test #'equal)))))

(defvar *live-snapshots* t
  "Whether the module being made has the functions SNAP and RESTORE of heap
images: NIL for a form evaluated at once in an image that does not record its
modules for SAVE-IMAGE (a module of a fasl may be loaded into one that does).")

(defun live-compile-forms (forms)
  "Compile FORMS into a module linked with the running image.
Returns (values bytes constants-vector)."
  (let ((*live-mode* t)
        (*live-packages* t)
        (*read-package* *package*)
        (*warnings* nil)
        (*unit-env* nil)
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
                 (when (and add (not (func-defined-p *m* name)))
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

(defun close-live-toplevel ()
  "End the current toplevel function; its value is that of its last form."
  (when *current-toplevel*
    (setf (lam-body *current-toplevel*) `(:progn (:const nil) ,@(reverse *toplevel-nodes*)))
    (push *current-toplevel* *toplevel-lams*)
    (setf *current-toplevel* nil *toplevel-nodes* nil *toplevel-weight* 0)))

(defun build-live-module ()
  (let ((*m* (make-wasm-module)))
    (let ((castfns (live-module-begin))
          (tops (reverse *toplevel-lams*)))
      (live-compile-tops tops)
      (multiple-value-bind (octets vectors) (live-module-finish (list tops) castfns nil)
        (values octets (first vectors))))))

(defun live-module-begin ()
  "The types, imports and standard helpers of a module compiled in the image
(in *M*); returns the functions of the checked casts (DEFINE-CHECKED-CASTS)."
  (loop for (name def) in *type-defs* do (add-type *m* name def))
  (setf (wasm-module-base-types *m*) (length *type-defs*))
  (loop for (name type mod field) in *imports* do (add-import-func *m* name type mod field))
  ;; (those not called are dropped with the unreachable functions)
  (when (and *live-helper-imports* (not *live-snapshots*))
    (dolist (name *live-imported-helpers*)
      (add-import-func *m* name (live-helper-type name) "clysm" (live-helper-export-name name))))
  (add-import-global *m* :mv-count :i32 t "clysm" "mv_count")
  (add-import-global *m* :mv-buf '(:ref :vector) nil "clysm" "mv_buf")
  (add-import-global *m* :empty-slots '(:ref :vector) nil "clysm" "empty_slots")
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
  (let ((*func-segment* 0))
    (sym-global nil) (sym-global t)
    (define-standard-helpers)
    (define-argerr-helpers)
    (define-checked-casts :lazy t)))

(defun live-compile-tops (tops)
  "Compile the toplevel functions TOPS and the functions they need."
  (dolist (l tops) (ensure-lam-compiled l) (static-closure-global l))
  (compile-pending-lams))

(defun global-segments (globals closures segments)
  "A table: global name -> the segment whose run sets it: the first of the
segments whose functions refer to it (a function runs only after the run of
its segment), or the last.  The symbols that the closures of a segment are
made with are set by the same run, before them."
  (let ((seg (make-hash-table :test 'equal)) (nsegs (length segments)))
    (dolist (g globals) (setf (gethash g seg) nsegs))
    ;; (the closures of the toplevel functions are called by the run of their segment)
    (loop for tops in segments for k from 0
          do (dolist (l tops) (setf (gethash (static-closure-global l) seg) k)))
    (labels ((walk (x s)
               (loop while (consp x)
                     do (let ((i (car x)))
                          (when (consp i)
                            (if (and (member (car i) '(:global.get :global.set)) (consp (cdr i)))
                                (multiple-value-bind (old found) (gethash (second i) seg)
                                  (when (and found (< s old)) (setf (gethash (second i) seg) s)))
                                (walk i s))))
                        (setq x (cdr x)))))
      (loop for f across (wasm-module-funcs *m*)
            do (walk (wasm-func-body f) (wasm-func-segment f)))
      (dolist (g closures)
        (when (= (gethash (first g) seg) nsegs) (setf (gethash (first g) seg) (1- nsegs)))
        (walk (third g) (gethash (first g) seg))))
    (dolist (g globals) (when (= (gethash g seg) nsegs) (setf (gethash g seg) (1- nsegs))))
    seg))

(defun live-module-finish (segments castfns dispatch)
  "Add the helpers, the globals and the functions that run the module to *M*,
and encode it.  SEGMENTS: the toplevel functions of each segment, in order.
The exported function RUN sets the globals of the first segment from the
vector of objects it is given and calls its toplevel functions; the functions
(:RUN-SEG K) do the same for the others, and with DISPATCH,
(:UNIT-DISPATCH K VECTOR) calls the one of segment K.  Returns (values octets
vectors), the vector of objects of each segment."
  (let ((*func-segment* 0))
    (add-called-helpers castfns))
  (let* ((nsegs (length segments))
         (all-tops (apply #'append segments))
         ;; globals: symbols, live constants and closures are filled in by
         ;; the runs; only those the code that can run refers to
         (used (live-used-globals all-tops))
         (syms (remove-if-not (lambda (s) (gethash (list :sym s) used)) (reverse *symbol-globals*)))
         (consts (remove-if-not (lambda (c) (gethash (car c) used)) (reverse *live-constants*)))
         (closures (remove-if-not (lambda (g) (gethash (first g) used)) (reverse *closure-globals*)))
         (seg (if (= nsegs 1)
                  nil
                  (global-segments (append (mapcar (lambda (s) (list :sym s)) syms)
                                           (mapcar #'car consts)
                                           (mapcar #'first closures))
                                   closures segments)))
         (vectors nil))
    (flet ((in-segment-p (g k) (or (null seg) (= k (gethash g seg)))))
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
      (add-identity-hash-cache)
      (loop for tops in segments for k from 0
            do (let* ((ssyms (remove-if-not (lambda (s) (in-segment-p (list :sym s) k)) syms))
                      (sconsts (remove-if-not (lambda (c) (in-segment-p (car c) k)) consts))
                      (sclosures (remove-if-not (lambda (g) (in-segment-p (first g) k)) closures))
                      (body `(,@(loop for s in ssyms for i from 0
                                      append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                               (:ref.cast :symbol) (:global.set (:sym ,s))))
                              ,@(loop for c in sconsts for i from (length ssyms)
                                      append `((:local.get v) (:i32.const ,i) (:array.get :vector)
                                               (:global.set ,(car c))))
                              ,@(loop for g in sclosures
                                      append `(,@(third g) (:global.set ,(first g))))
                              ;; a form that leaves with an exit pending: the
                              ;; run returns null too (its caller looks)
                              (:block pend nil
                                ,@(loop for l in tops
                                        append `((:global.get ,(static-closure-global l))
                                                 (:call (:main ,(lam-id l))) (:local.tee result)
                                                 :ref.is_null
                                                 (:if nil nil ((:call :exit-pending-p) (:br_if pend)))))
                                (:local.get result) :return)
                              (:ref.null :eq))))
                 (push (coerce (append ssyms (mapcar #'cdr sconsts)) 'simple-vector) vectors)
                 (if (= k 0)
                     (add-func *m* :run :x-e :export "run"
                               :locals '((:ref :null :vector) :eqref) :local-names '(v result)
                               :body `((:local.get 0) :any.convert_extern (:ref.cast! :vector) (:local.set v)
                                       ,@body))
                     (progn
                       (add-func *m* (list :run-seg k) :e-e
                                 :locals '((:ref :null :vector) :eqref) :local-names '(v result)
                                 :body `((:local.get 0) (:ref.cast! :vector) (:local.set v) ,@body))
                       (setf (gethash (list :run-seg k) used) t))))))
    (when dispatch
      (setf (gethash :unit-dispatch used) t)
      (add-func *m* :unit-dispatch :ee-e
                :body (labels ((chain (k)
                                 (if (>= k nsegs)
                                     (nil-code)
                                     `((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const ,k) :i32.eq
                                       (:if nil :eqref ((:local.get 1) (:call (:run-seg ,k))) ,(chain (1+ k)))))))
                        (chain 1))))
    ;; for heap images (runtime/image.lisp): SNAP returns the values of
    ;; its globals, which RESTORE sets again in a new instance (whose RUN
    ;; is not called); not in a module that cannot be saved in one
    (when *live-snapshots*
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
                                         (:global.set ,g)))))))
    ;; a module made at run time only needs the helpers it calls
    (remove-unreachable-functions *m* used)
    (when *live-dump* (dump-live-functions *m*))
    (values (encode-module *m*) (nreverse vectors))))

(defvar *live-dump* nil
  "Names (strings) of functions whose Wasm code is printed to *TRACE-OUTPUT*
when code compiled in the image (the REPL, LOAD, COMPILE-FILE) defines them,
as tools/dump-func.lisp prints that of a program: NAME is the function's name
as in stack traces (FOO, or FOO#123 for one of them).")

(defun dump-live-functions (module)
  (loop for f across (clysm-wasm::wasm-module-funcs module)
        for disp = (format nil "~A" (or (clysm-wasm::wasm-func-display f) (wasm-func-name f)))
        when (some (lambda (n) (or (string-equal n disp)
                                   (eql 0 (search (format nil "~A#" n) disp :test #'char-equal))))
                   *live-dump*)
          do (let ((*print-right-margin* 120) (*print-case* :downcase) (*print-pretty* t))
               (format *trace-output* "~&;;; ~A  locals ~S~%" disp (wasm-func-locals f))
               (dolist (i (wasm-func-body f)) (format *trace-output* "  ~S~%" i)))))

;;; ------------------------------------------------------------
;;; Compilation units: COMPILE-FILE
;;; ------------------------------------------------------------
;;;
;;; COMPILE-FILE compiles a whole file into one module (CLHS 3.2.2.3: a call
;;; of a function defined in the same file may be assumed to call that
;;; definition), so that calls between the functions of a file are direct
;;; calls, small functions are copied into their callers, and the types of
;;; arguments and values flow between them, as in a whole program.
;;;
;;; The forms are converted when COMPILE-FILE processes them, in the
;;; environment that the forms before them made (macros, special
;;; proclamations).  The module is cut into segments where the file's
;;; compile-time side effects may change how the constants of the forms
;;; after them are read when the file is loaded (a DEFPACKAGE; see
;;; %CF-PROCESS): the constants of a segment (symbols, literal objects) are
;;; read just before it runs.  A segment's functions are compiled when it
;;; ends, so a function calls the functions of the segments before it and of
;;; its own segment directly, and those of the segments after it (not
;;; defined yet when it is compiled) through their symbol.  The first segment
;;; sets SYS::*FASL-UNIT-RUNNER* to a function of a segment number and a
;;; vector of objects that runs that segment (LOAD calls it for the others).

(defvar *unit-segments* nil "the toplevel functions of each segment of the unit, reversed")
(defvar *unit-castfns* nil "the functions of the checked casts of the unit's module")

(defun call-with-compilation-unit (thunk)
  "Call THUNK with a new compilation unit being compiled."
  (let ((*live-mode* t) (*live-packages* t) (*warnings* nil))
    (with-fresh-codegen-state ()
      (let ((*m* (make-wasm-module)) (*unit-env* (make-cenv)) (*unit-segments* nil) (*unit-castfns* nil))
        (clysm-sys::%call-with-world-lock
         (lambda ()
           (let ((*error-output* (make-broadcast-stream)))
             (setq *unit-castfns* (live-module-begin)))
           (unit-add-form '(setq clysm-sys::*fasl-unit-runner*
                            (lambda (k v) (clysm-sys::%wasm :eqref (k v) (:call :unit-dispatch)))))))
        (funcall thunk)))))

(defun unit-add-form (form)
  "Process FORM, a toplevel form of the unit."
  (let ((*read-package* *package*) (*error-output* (make-broadcast-stream))
        (*func-segment* (length *unit-segments*))
        (current *current-toplevel*) (nodes *toplevel-nodes*) (weight *toplevel-weight*)
        (lams *toplevel-lams*) (done nil))
    (unwind-protect
         (progn (process-toplevel form *unit-env*) (setq done t))
      ;; a form the compiler failed on leaves no code (COMPILE-FILE writes
      ;; its source instead)
      (unless done
        (setf *current-toplevel* current *toplevel-nodes* nodes *toplevel-weight* weight
              *toplevel-lams* lams)))
    nil))

(defun unit-end-segment ()
  "End the current segment and compile its functions; its number, or NIL
when it has no forms."
  (let ((*error-output* (make-broadcast-stream))
        (*func-segment* (length *unit-segments*)))
    (close-live-toplevel)
    (let ((tops (reverse *toplevel-lams*)))
      (when tops
        (setf *toplevel-lams* nil)
        (live-compile-tops tops)
        (push tops *unit-segments*)
        (1- (length *unit-segments*))))))

(defun unit-finish ()
  "The module of the unit: (values octets vectors), the vector of objects of each segment."
  (unit-end-segment)
  (let ((*error-output* (make-broadcast-stream)))
    (live-module-finish (reverse *unit-segments*) *unit-castfns* t)))
