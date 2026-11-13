;;;; codegen.lisp — AST -> WasmGC instructions

(in-package :clysm)

;;; ------------------------------------------------------------
;;; Module-level state
;;; ------------------------------------------------------------

(defvar *m* nil "the wasm module being built")
(defvar *symbol-globals* nil "ordered list of symbols (reverse)")
(defvar *symbol-global-table* (make-hash-table :test 'eq))
(defvar *const-globals* nil "list of (name type init) in reverse order")
(defvar *const-table* (make-hash-table :test 'eql))
(defvar *const-string-table* (make-hash-table :test 'equal))
(defvar *const-counter* 0)
(defvar *closure-globals* nil)
(defvar *misc-globals* nil)
(defvar *pending-lams* nil)
(defvar *compiled-lams* (make-hash-table :test 'eq))
(defvar *extra-types* (make-hash-table :test 'equal))
(defvar *ltv-counter* 0)

(defun sym-global (sym)
  (unless (gethash sym *symbol-global-table*)
    (setf (gethash sym *symbol-global-table*) t)
    (push sym *symbol-globals*))
  (list :sym sym))

(defun nil-code () (list (list :global.get (sym-global nil))))
(defun t-code () (list (list :global.get (sym-global t))))

(defun main-type (n)
  (if (<= n +max-fixed-args+)
      (fixed-entry-type n)
      (let ((key (list :fm n)))
        (unless (gethash key *extra-types*)
          (setf (gethash key *extra-types*) t)
          (add-type *m* key `(:func ((:ref :closure) ,@(make-list n :initial-element :eqref)) (:eqref))))
        key)))

;;; ------------------------------------------------------------
;;; Constants
;;; ------------------------------------------------------------

(defun fixnump* (x) (and (integerp x) (<= +most-negative-fixnum+ x +most-positive-fixnum+)))

(defun string-hash (s)
  "FNV-1a over code points, 29-bit result. Must match the runtime."
  (let ((h 2166136261))
    (loop for c across s
          do (setf h (ldb (byte 32 0) (* (logxor h (char-code c)) 16777619))))
    (ldb (byte 29 0) h)))

(defun string-array-code (s)
  (append (loop for c across s collect `(:i32.const ,(char-code c)))
          `((:array.new_fixed :string ,(length s)))))

(defun bignum-digits (n)
  "Base 2^28 little-endian digits of a non-negative integer."
  (let ((digits nil))
    (loop while (plusp n) do (push (ldb (byte +limb-bits+ 0) n) digits) (setf n (ash n (- +limb-bits+))))
    (nreverse digits)))

(defun new-const-global (init &optional (type :eqref))
  (let ((name (list :const (incf *const-counter*))))
    (push (list name type init) *const-globals*)
    name))

(defun const-init-code (obj)
  "Constant expression (list of instrs) producing OBJ."
  (cond ((fixnump* obj) `((:i32.const ,(ldb (byte 32 0) obj)) :ref.i31))
        ((characterp obj) `((:i32.const ,(+ +char-tag+ (char-code obj))) :ref.i31))
        ((symbolp obj) `((:global.get ,(sym-global obj))))
        (t `((:global.get ,(const-global obj))))))

(defvar *runtime-consts* nil "list of (global-name init-code) built in _start")
(defconstant +max-fixed-elements+ 9000)

(defun long-constant-p (obj &optional (depth 0))
  (cond ((> depth 1000) nil)
        ((stringp obj) (> (length obj) +max-fixed-elements+))
        ((consp obj) (or (long-constant-p (car obj) (1+ depth)) (long-constant-p (cdr obj) (1+ depth))))
        ((simple-vector-p obj) (or (> (length obj) +max-fixed-elements+)
                                   (some (lambda (x) (long-constant-p x (1+ depth))) obj)))
        (t nil)))

(defun runtime-const-code (obj)
  "Instructions (valid in a function body) that build OBJ."
  (cond ((and (stringp obj) (> (length obj) +max-fixed-elements+))
         (let ((bytes (make-array (* 4 (length obj)) :element-type '(unsigned-byte 8))))
           (loop for c across obj for i from 0
                 do (let ((code (char-code c)))
                      (dotimes (k 4) (setf (aref bytes (+ (* 4 i) k)) (ldb (byte 8 (* 8 k)) code)))))
           (let ((seg (add-data *m* bytes)))
             `((:i32.const 0) (:i32.const ,(length obj)) (:array.new_data :string ,seg)))))
        ((not (long-constant-p obj)) (const-init-code obj))
        ((consp obj) `(,@(runtime-const-code (car obj)) ,@(runtime-const-code (cdr obj)) (:struct.new :cons)))
        ((simple-vector-p obj)
         (let ((tmp-elts (coerce obj 'list)))
           `(,@(loop for x in tmp-elts append (runtime-const-code x))
             ,@(if (> (length obj) +max-fixed-elements+)
                   (error "vector constant too long (~D elements)" (length obj))
                   `((:array.new_fixed :vector ,(length obj)))))))
        (t (const-init-code obj))))

(defun runtime-consts-prologue ()
  (loop for (name code) in (reverse *runtime-consts*)
        append `(,@code (:global.set ,name))))

(defvar *live-constants* nil "live mode: list of (global-name . object), reversed")

(defun const-global (obj)
  (when *live-mode*
    ;; the constant is the live object itself, passed in at instantiation
    (return-from const-global
      (or (gethash obj *const-table*)
          (let ((name (list :lconst (incf *const-counter*))))
            (push (cons name obj) *live-constants*)
            (setf (gethash obj *const-table*) name)))))
  (let ((table (if (stringp obj) *const-string-table* *const-table*)))
    (when (and (not (gethash obj table)) (long-constant-p obj))
      (let ((name (list :rconst (incf *const-counter*))))
        (setf (gethash obj table) name)
        (push (list name (runtime-const-code obj)) *runtime-consts*)
        (return-from const-global name)))
    (or (gethash obj table)
        (setf (gethash obj table)
              (new-const-global
               (typecase obj
                 (integer (if (typep obj '(signed-byte 64))
                              ;; within 64 bits: the value, no limbs (as :i64-int makes them)
                              `((:i32.const ,(if (minusp obj) 1 0)) (:ref.null :limbs) (:i64.const ,obj)
                                (:struct.new :bignum))
                              (let ((digits (bignum-digits (abs obj))))
                                `((:i32.const ,(if (minusp obj) 1 0))
                                  ,@(loop for d in digits collect `(:i32.const ,d))
                                  (:array.new_fixed :limbs ,(length digits))
                                  (:i64.const 0)
                                  (:struct.new :bignum)))))
                 (string (string-array-code obj))
                 (cons `(,@(const-init-code (car obj)) ,@(const-init-code (cdr obj)) (:struct.new :cons)))
                 (simple-vector `(,@(loop for x across obj append (const-init-code x))
                                  (:array.new_fixed :vector ,(length obj))))
                 ((and array (not vector))
                  (let ((elts (loop for i below (array-total-size obj) collect (row-major-aref obj i))))
                    `(,@(loop for x in elts append (const-init-code x))
                      (:array.new_fixed :vector ,(length elts))
                      ,@(const-init-code nil)
                      ,@(const-init-code (array-dimensions obj))
                      ,@(const-init-code 0)
                      ,@(const-init-code (list t nil))
                      ,@(let ((d (array-dimensions obj)))
                          (if (= (length d) 2)
                              `((:i32.const ,(first d)) (:i32.const ,(second d)))
                              '((:i32.const 0) (:i32.const 0))))
                      (:struct.new :mdarray))))
                 (bit-vector `(,@(loop for x across obj collect `(:i32.const ,x))
                               (:array.new_fixed :bits ,(length obj))))
                 (single-float `((:f32.const ,obj) (:struct.new :single)))
                 (double-float `((:f64.const ,obj) (:struct.new :double)))
                 (ratio `(,@(const-init-code (numerator obj)) ,@(const-init-code (denominator obj))
                          (:struct.new :ratio)))
                 (complex `(,@(const-init-code (realpart obj)) ,@(const-init-code (imagpart obj))
                            (:struct.new :complex)))
                 (t (error "cannot compile constant ~S" obj))))))))

(defun cg-const (obj) (const-init-code obj))

;;; ------------------------------------------------------------
;;; Function state
;;; ------------------------------------------------------------

(defstruct fstate lam nparams (locals nil) (nlocals 0) (locs (make-hash-table :test 'eq)) env-local)

(defvar *fs* nil)
(defvar *tail-ok* nil "true when a call in tail position may become return_call")
(defvar *dyn-depth* 0 "number of dynamic-binding scopes active in the current function")
(defvar *uwp-depth* 0 "number of unwind-protect/catch regions active in the current function")
(defvar *uwp-frames* nil
  "The unwind-protect (a UWPF) and catch (:CATCH) regions active in the current
function, innermost first.  A local exit through unwind-protect regions jumps
to the innermost one's cleanup, which then continues the exit.")
(defstruct uwpf code res landing (exits nil) (nexits 0) dyn-depth uwp-depth parent mv-used)

(defun new-local (&optional (type :eqref))
  (let ((idx (+ (fstate-nparams *fs*) (fstate-nlocals *fs*))))
    (push type (fstate-locals *fs*))
    (incf (fstate-nlocals *fs*))
    idx))

(defun boxed-p (lv) (and (lvar-captured lv) (lvar-mutated lv)))

(defun set-loc (lv loc) (setf (gethash lv (fstate-locs *fs*)) loc))
(defun get-loc (lv)
  (or (gethash lv (fstate-locs *fs*))
      (error "codegen: no location for ~S in ~S" lv (fstate-lam *fs*))))

(defun lvar-rep* (lv) (or (lvar-rep lv) :eqref))
(defun rep-wasm-type (rep) (ecase rep (:eqref :eqref) (:f64 :f64) (:i32 :i32) (:i64 :i64)))
(defun box-rep-code (rep &optional ty)
  "Box a value of REP; for :i32, TY tells whether it is a fixnum (else it may be a bignum)."
  (ecase rep
    (:eqref nil)
    (:f64 '((:struct.new :double)))
    (:i32 (if (or (null ty) (ty-fixnum-p ty)) '(:ref.i31) '((:call :box-i32))))
    (:i64 '((:call :i64-int)))))
(defun unbox-rep-code (rep &optional ty)
  (ecase rep
    (:eqref nil)
    (:f64 '((:ref.cast! :double) (:struct.get :double 0)))
    (:i32 (if (or (null ty) (ty-fixnum-p ty))
              '((:ref.cast! :i31) :i31.get_s)
              '((:call :int-i64) :i32.wrap_i64)))
    (:i64 (if (and ty (ty-fixnum-p ty))
              '((:ref.cast! :i31) :i31.get_s :i64.extend_i32_s)
              '((:call :int-i64))))))

(defun unboxed-local-p (lv)
  "LV is held in a local of the function being compiled in its representation."
  (and (lvar-rep lv) (not (boxed-p lv)) (eq (car (get-loc lv)) :local)))

(defun var-raw-code (lv)
  "Push the raw content of a variable's location (cell if boxed)."
  (let ((loc (get-loc lv)))
    (ecase (car loc)
      (:local `((:local.get ,(cdr loc)) ,@(box-rep-code (lvar-rep* lv) (lvar-type lv))))
      (:env `((:local.get ,(fstate-env-local *fs*)) (:i32.const ,(cdr loc)) (:array.get :vector))))))

(defun var-get-code (lv)
  (if (boxed-p lv)
      `(,@(var-raw-code lv) (:ref.cast :cell) (:struct.get :cell 0))
      (var-raw-code lv)))

(defun var-set-code (lv value-code)
  "Store; leaves nothing on the stack."
  (let ((loc (get-loc lv)))
    (cond ((boxed-p lv)
           `(,@(var-raw-code lv) (:ref.cast :cell) ,@value-code (:struct.set :cell 0)))
          ((eq (car loc) :local) `(,@value-code ,@(unbox-rep-code (lvar-rep* lv) (lvar-type lv)) (:local.set ,(cdr loc))))
          (t `((:local.get ,(fstate-env-local *fs*)) (:i32.const ,(cdr loc)) ,@value-code
               (:array.set :vector))))))

(defun bind-new-var-code (lv value-code)
  "Initialize a fresh lexical variable (allocating its local)."
  (when (and (lvar-rep lv) (not (boxed-p lv)))
    (let ((idx (new-local (rep-wasm-type (lvar-rep lv)))))
      (set-loc lv (cons :local idx))
      (return-from bind-new-var-code `(,@value-code ,@(unbox-rep-code (lvar-rep lv) (lvar-type lv)) (:local.set ,idx)))))
  (let ((idx (new-local)))
    (set-loc lv (cons :local idx))
    (if (boxed-p lv)
        `(,@value-code (:struct.new :cell) (:local.set ,idx))
        `(,@value-code (:local.set ,idx)))))

(defun mv-one () '((:i32.const 1) (:global.set :mv-count)))

;;; ------------------------------------------------------------
;;; Main dispatcher
;;; ------------------------------------------------------------

(defun cg (node &optional mv tail)
  "Instructions leaving one eqref on the stack.  MV: the consumer wants all
values (mv-count must be valid).  TAIL: value is returned from the function."
  (let ((tail (and tail *tail-ok*)))
    (ecase (car node)
      (:const (append (cg-const (second node)) (when mv (mv-one))))
      (:lref (append (var-get-code (second node)) (when mv (mv-one))))
      (:lset (if (unboxed-local-p (second node))
                 (let ((rep (lvar-rep (second node))))
                   `(,@(cg-rep (third node) rep) (:local.tee ,(cdr (get-loc (second node))))
                     ,@(box-rep-code rep (lvar-type (second node))) ,@(when mv (mv-one))))
               (let ((tmp (new-local)))
               `(,@(var-set-code (second node) `(,@(cg (third node)) (:local.tee ,tmp)))
                 (:local.get ,tmp) ,@(when mv (mv-one))))))
      (:sref (let ((ok (gensym "BOUND")))
               `((:block ,ok (:ref :eq)
                   (:global.get ,(sym-global (second node))) (:struct.get :symbol ,+sym-value+) (:br_on_non_null ,ok)
                   (:global.get ,(sym-global (second node))) (:call :symbol-value) (:ref.as_non_null))
                 ,@(when mv (mv-one)))))
      (:sset (let ((tmp (new-local)))
               `((:global.get ,(sym-global (second node)))
                 ,@(cg (third node)) (:local.tee ,tmp)
                 (:struct.set :symbol ,+sym-value+)
                 (:local.get ,tmp) ,@(when mv (mv-one)))))
      (:if (cg-if node mv tail))
      (:progn (cg-progn (cdr node) mv tail))
      (:let (if (and (or-let-p node) (eq (car (cdr (first (second node)))) :prim)
                     (eq (prim-kind (second (cdr (first (second node))))) :test))
                ;; (or test b): T when the test holds, else B
                `(,@(cg-test (cdr (first (second node))))
                  (:if nil :eqref (,@(t-code) ,@(when mv (mv-one))) ,(cg (fourth (third node)) mv tail)))
                (cg-let node mv tail)))
      (:mvbind (cg-mvbind node mv tail))
      (:lambda (append (cg-closure (second node)) (when mv (mv-one))))
      (:fref (let ((lam (and (not (gethash (second node) *dynamic-functions*))
                             (known-global-lam (second node)))))
               (if lam
                   ;; a known function: its closure (as with direct calls, the
                   ;; definition is assumed not to change)
                   (progn (ensure-lam-compiled lam)
                          `((:global.get ,(static-closure-global lam)) ,@(when mv (mv-one))))
                   `((:global.get ,(sym-global (second node))) (:call :fdefinition) ,@(when mv (mv-one))))))
      (:call (or (typed-cg node mv) (cg-global-call (second node) (cddr node) mv tail)))
      (:funcall (cg-funcall (second node) (cddr node) mv tail))
      (:lcall (cg-local-call (second node) (cddr node) mv tail))
      (:prim (or (typed-cg node mv) (cg-prim (second node) (cddr node) mv)))
      ((:flet :labels) (cg-flet node mv tail))
      (:block (cg-block node mv tail))
      (:return-from (cg-return-from node))
      (:tagbody (cg-tagbody node mv))
      (:go (cg-go node))
      (:catch (cg-catch node mv))
      (:throw (cg-throw node))
      (:unwind-protect (cg-unwind-protect node mv))
      (:mvcall (cg-mvcall node mv tail))
      (:mvprog1 (cg-mvprog1 node mv))
      (:the (cg (third node) mv tail))
      (:ltv (cg-ltv node mv))
      (:progv (cg-progv node mv))
      (:wasm (cg-wasm node mv))
      (:wasm-test (append (cg-test node) (bool-code) (when mv (mv-one))))
      (:symtab `((:global.get :symtab) (:global.get :pkgtab) (:struct.new :cons) ,@(when mv (mv-one))))
      (:defun (cg-defun node mv))
      (:values (cg-values (cdr node) mv))
      (:apply `(,@(cg (second node)) ,@(cg (third node))
                (,(if tail :return_call :call) :apply-list))))))

(defun cg-values (args mv)
  (cond ((null args) `(,@(nil-code) ,@(when mv '((:i32.const 0) (:global.set :mv-count)))))
        ((not mv) `(,@(cg (car args)) ,@(loop for a in (cdr args) append `(,@(cg a) :drop))))
        (t (let ((tmps (loop for nil in args collect (new-local))))
             `(,@(loop for a in args for tmp in tmps append `(,@(cg a) (:local.set ,tmp)))
               ,@(loop for tmp in (cdr tmps) for i from 0
                       append `((:global.get :mv-buf) (:i32.const ,i) (:local.get ,tmp) (:array.set :vector)))
               (:i32.const ,(length args)) (:global.set :mv-count)
               (:local.get ,(car tmps)))))))

(defun bool-code ()
  "Convert an i32 on the stack to T/NIL."
  `((:if nil :eqref ,(t-code) ,(nil-code))))

(defun cg-test (node)
  "Instructions leaving an i32 (0 = false)."
  (let ((typed (and (member (car node) '(:prim :call)) (typed-test-code node))))
    (when typed (return-from cg-test typed)))
  ;; (not x) / (null x): the test of X, negated (no T/NIL in between)
  (when (and (eq (car node) :prim) (member (prim-name (second node)) '(not null)))
    (return-from cg-test `(,@(cg-test (third node)) :i32.eqz)))
  ;; (or a b) is (let ((g a)) (if g g b)): as a test, A's test or B's
  (when (or-let-p node)
    (return-from cg-test
      `(,@(cg-test (cdr (first (second node))))
        (:if nil :i32 ((:i32.const 1)) ,(cg-test (fourth (third node)))))))
  (case (car node)
    (:const (if (second node) '((:i32.const 1)) '((:i32.const 0))))
    (:prim (let ((p (second node)))
             (if (eq (prim-kind p) :test)
                 (funcall (prim-fn p) (mapcar #'cg (cddr node)))
                 `(,@(cg node) ,@(nil-code) :ref.eq :i32.eqz))))
    (:wasm-test `(,@(loop for a in (second node) append (cg a))
                  ,@(resolve-wasm-instrs (third node))))
    (:if `(,@(cg-test (second node))
           (:if nil :i32 ,(cg-test (third node)) ,(cg-test (fourth node)))))
    (:progn (if (cdr node)
                `(,@(loop for n in (butlast (cdr node)) append `(,@(cg n) :drop))
                  ,@(cg-test (car (last node))))
                '((:i32.const 0))))
    (t `(,@(cg node) ,@(nil-code) :ref.eq :i32.eqz))))

(defun or-let-p (node)
  "NODE is (let ((g a)) (if g g b)), as OR expands."
  (and (eq (car node) :let) (= (length (second node)) 1)
       (let ((lv (car (first (second node)))) (body (third node)))
         (and (not (lvar-special lv)) (eq (car body) :if)
              (eq (car (second body)) :lref) (eq (second (second body)) lv)
              (eq (car (third body)) :lref) (eq (second (third body)) lv)))))

(defun cg-if (node mv tail)
  (destructuring-bind (c a b) (cdr node)
    `(,@(cg-test c)
      (:if nil :eqref ,(cg a mv tail) ,(cg b mv tail)))))

(defun cg-progn (nodes mv tail)
  (if (null nodes)
      (append (nil-code) (when mv (mv-one)))
      `(,@(loop for n in (butlast nodes) append (cg-effect n))
        ,@(cg (car (last nodes)) mv tail))))

(defun cg-effect (node)
  "Instructions evaluating NODE for its effects (leaving nothing)."
  (case (car node)
    ((:const :lref) nil)
    (:lset (let ((lv (second node)))
             (if (unboxed-local-p lv)
                 `(,@(cg-rep (third node) (lvar-rep lv)) (:local.set ,(cdr (get-loc lv))))
                 (var-set-code lv (cg (third node))))))
    (:progn (loop for n in (cdr node) append (cg-effect n)))
    (:if `(,@(cg-test (second node)) (:if nil nil ,(cg-effect (third node)) ,(cg-effect (fourth node)))))
    (:let (cg-let node nil nil :effect))
    (t `(,@(cg node) :drop))))

;;; ------------------------------------------------------------
;;; Dynamic binding helpers
;;; ------------------------------------------------------------

(defun save-bsp-code (local) `((:global.get :bsp) (:local.set ,local)))
(defun restore-bsp-code (local) `((:local.get ,local) (:call :unbind-to)))

(defmacro with-dynamic-scope (() &body body)
  `(let ((*dyn-depth* (1+ *dyn-depth*)) (*tail-ok* nil)) ,@body))

;;; ------------------------------------------------------------
;;; LET
;;; ------------------------------------------------------------

(defun cg-let (node mv tail &optional (rep :eqref))
  "REP: :eqref (the value as usual), :f64 / :i32 (the value in that
representation) or :effect (no value)."
  (destructuring-bind (bindings body) (cdr node)
    (let ((code nil) (specials nil))
      ;; evaluate inits in order
      (dolist (b bindings)
        (let ((lv (car b)) (init (cdr b)))
          (cond ((and (eq (car init) :lambda) (zerop (lvar-refs lv)) (not (lvar-special lv)))
                 ;; a lambda whose variable is never referenced (its calls were inlined)
                 nil)
                ((lvar-special lv)
                 (let ((tmp (new-local)))
                   (setf code (append code (cg init) `((:local.set ,tmp))))
                   (push (cons lv tmp) specials)))
                ((and (lvar-rep lv) (not (boxed-p lv)))
                 (let ((idx (new-local (rep-wasm-type (lvar-rep lv)))))
                   ;; the init is compiled before the variable has a location
                   (setf code (append code (cg-rep init (lvar-rep lv)) `((:local.set ,idx))))
                   (set-loc lv (cons :local idx))))
                (t (setf code (append code (bind-new-var-code lv (cg init))))))
          ;; a variable that is always a known lambda: FUNCALLs of it are direct calls
          (when (and (eq (car init) :lambda) (not (lvar-mutated lv)) (not (lvar-special lv)))
            (setf (gethash lv *lvar-lams*) (second init)))))
      (flet ((body-code (tail)
               (case rep
                 (:eqref (cg body mv tail))
                 (:effect (cg-effect body))
                 (t (cg-rep body rep)))))
        (if (null specials)
            (append code (body-code tail))
            (let ((saved (new-local :i32)) (res (unless (eq rep :effect)
                                                 (new-local (if (eq rep :eqref) :eqref (rep-wasm-type rep))))))
              (append code
                      (save-bsp-code saved)
                      (loop for (lv . tmp) in (reverse specials)
                            append `((:global.get ,(sym-global (lvar-name lv))) (:local.get ,tmp) (:call :bind)))
                      (with-dynamic-scope () (body-code nil))
                      (when res `((:local.set ,res)))
                      (restore-bsp-code saved)
                      (when res `((:local.get ,res))))))))))

(defun cg-mvbind (node mv tail)
  (destructuring-bind (lvars vnode body) (cdr node)
    ;; a division whose values are computed inline: the quotient on the
    ;; stack, the remainder in REM-LOCAL (of representation REM-REP when it
    ;; is not an eqref)
    (multiple-value-bind (divmod rem-local rem-rep)
        (if lvars
            (multiple-value-bind (c l) (typed-divmod-code vnode)
              (if c (values c l nil) (numeric-divmod-code vnode)))
            nil)
      (let* ((code (or divmod (cg vnode t)))
             (tmps (loop for nil in lvars collect (new-local)))
             (specials nil)
             ;; the second variable takes the unboxed remainder as it is
             (direct (and divmod rem-rep (second lvars)
                          (let ((lv (second lvars)))
                            (and (eq (lvar-rep lv) rem-rep) (not (boxed-p lv)) (not (lvar-special lv)))))))
        (when (null lvars) (setf code (append code '(:drop))))
        ;; primary
        (when lvars
          (setf code (append code `((:local.set ,(first tmps))))))
        (loop for i from 1 for tmp in (rest tmps)
              do (setf code (append code
                                    (cond ((and divmod (= i 1) direct) nil)
                                          ((and divmod (= i 1))
                                           `((:local.get ,rem-local) ,@(when rem-rep (box-rep-code rem-rep))
                                             (:local.set ,tmp)))
                                          (divmod `(,@(nil-code) (:local.set ,tmp)))
                                          (t `((:global.get :mv-count) (:i32.const ,i) :i32.gt_s
                                               (:if nil :eqref
                                                    ((:global.get :mv-buf) (:i32.const ,(1- i)) (:array.get :vector))
                                                    ,(nil-code))
                                               (:local.set ,tmp)))))))
      (loop for lv in lvars for tmp in tmps for i from 0
            do (cond ((lvar-special lv) (push (cons lv tmp) specials))
                     ((and direct (= i 1))
                      (let ((idx (new-local (rep-wasm-type rem-rep))))
                        (set-loc lv (cons :local idx))
                        (setf code (append code `((:local.get ,rem-local) (:local.set ,idx))))))
                     (t (setf code (append code (bind-new-var-code lv `((:local.get ,tmp))))))))
      (if (null specials)
          (append code (cg body mv tail))
          (let ((saved (new-local :i32)) (res (new-local)))
            (append code (save-bsp-code saved)
                    (loop for (lv . tmp) in (reverse specials)
                          append `((:global.get ,(sym-global (lvar-name lv))) (:local.get ,tmp) (:call :bind)))
                    (with-dynamic-scope () (cg body mv nil))
                    `((:local.set ,res))
                    (restore-bsp-code saved)
                    `((:local.get ,res)))))))))

;;; ------------------------------------------------------------
;;; Closures
;;; ------------------------------------------------------------

(defun lam-rest-only-p (lam)
  "Required parameters and &REST, nothing else."
  (and (lam-rest lam) (null (lam-opt lam)) (not (lam-keyp lam))))

(defun lam-opt-only-p (lam)
  "Required and &OPTIONAL parameters only."
  (and (lam-keyed-p lam) (not (lam-keyp lam))))

(defun lam-entry-names (lam)
  (let ((simple (lam-simple-p lam)) (n (length (lam-req lam))) (id (lam-id lam)))
    (append
     (loop for k from 0 to +max-fixed-args+
           collect (cond ((lam-rest-only-p lam) (if (< k n) (list :argerr k) (list :rentry id k)))
                         ((lam-opt-only-p lam)
                          (if (<= n k (+ n (length (lam-opt lam)))) (list :oentry id k) (list :argerr k)))
                         ((not simple) (list :pack k))
                         ((= k n) (list :main id))
                         (t (list :argerr k))))
     (list (if simple (list :entn id) (list :main id))))))

(defun lam-name-code (lam)
  (let ((name (lam-name lam)))
    (cond ((symbolp name) `((:global.get ,(sym-global name))))
          (t (const-init-code name)))))

(defun ensure-lam-compiled (lam)
  (unless (gethash lam *compiled-lams*)
    (setf (gethash lam *compiled-lams*) :pending)
    (push lam *pending-lams*)))

(defun static-closure-global (lam)
  (let ((name (list :clo (lam-id lam))))
    (unless (global-defined-p *m* name)
      (unless (find name *closure-globals* :key #'first :test #'equal)
        (push (list name `(:ref :closure)
                    `(,@(loop for e in (lam-entry-names lam) collect `(:ref.func ,e))
                      (:ref.null :vector)
                      ,@(lam-name-code lam)
                      (:struct.new :closure)))
              *closure-globals*)))
    name))

(defun cg-closure (lam)
  (ensure-lam-compiled lam)
  (if (null (lam-free-vars lam))
      `((:global.get ,(static-closure-global lam)))
      `(,@(loop for e in (lam-entry-names lam) collect `(:ref.func ,e))
        ,@(loop for fv in (lam-free-vars lam) append (var-raw-code fv))
        (:array.new_fixed :vector ,(length (lam-free-vars lam)))
        ,@(lam-name-code lam)
        (:struct.new :closure))))

;;; ------------------------------------------------------------
;;; Calls
;;; ------------------------------------------------------------

(defun call-closure-code (closure-code arg-codes mv tail)
  "CLOSURE-CODE pushes a (ref $closure)."
  (declare (ignore mv))
  (let ((n (length arg-codes)) (tmp (new-local '(:ref :null :closure))))
    (if (<= n +max-fixed-args+)
        `(,@closure-code (:local.tee ,tmp) (:ref.as_non_null)
          ,@(apply #'append arg-codes)
          (:local.get ,tmp) (:struct.get :closure ,n)
          (,(if tail :return_call_ref :call_ref) ,(fixed-entry-type n)))
        `(,@closure-code (:local.tee ,tmp) (:ref.as_non_null)
          ,@(apply #'append arg-codes)
          (:array.new_fixed :vector ,n)
          (:local.get ,tmp) (:struct.get :closure ,+clo-entry-n+)
          (,(if tail :return_call_ref :call_ref) :fN)))))

(defun lam-accepts-p (lam n)
  (let ((req (length (lam-req lam))))
    (and (>= n req)
         (or (lam-rest lam) (lam-keyp lam) (<= n (+ req (length (lam-opt lam))))))))

(defun lam-keyed-p (lam)
  "Complex lambdas without &rest are compiled with a fixed argument layout:
#(req... opt... key...) where unsupplied optional/key slots are null."
  (and (not (lam-simple-p lam)) (null (lam-rest lam))))

(defun lam-fixed-size (lam)
  (+ (length (lam-req lam)) (length (lam-opt lam)) (length (lam-keys lam))))

(defun keyed-call-layout (lam nodes)
  "For a direct call, map each fixed slot to the index of the argument node
supplying it (or NIL).  Returns NIL when the call cannot be laid out statically."
  (let* ((nreq (length (lam-req lam))) (nopt (length (lam-opt lam)))
         (npos (+ nreq nopt)) (keys (mapcar #'first (lam-keys lam)))
         (n (length nodes))
         (layout (make-array (lam-fixed-size lam) :initial-element nil)))
    (cond ((< n nreq) nil)
          ((and (> n npos) (not (lam-keyp lam))) nil)
          ((oddp (max 0 (- n npos))) nil)
          (t
           (dotimes (i (min n npos)) (setf (aref layout i) i))
           (loop for i from npos below n by 2
                 for k = (nth i nodes)
                 do (unless (and (eq (car k) :const) (member (second k) keys))
                      (return-from keyed-call-layout nil))
                    (let ((slot (+ npos (position (second k) keys))))
                      (unless (aref layout slot) (setf (aref layout slot) (1+ i)))))
           layout))))

(defun direct-call-code (lam closure-code arg-codes tail &optional arg-nodes)
  "Call LAM's main function directly."
  (ensure-lam-compiled lam)
  (let ((call (if tail :return_call :call)))
    (cond ((lam-simple-p lam)
           `(,@closure-code ,@(apply #'append arg-codes) (,call (:main ,(lam-id lam)))))
          ((lam-rest-only-p lam)
           ;; the rest list is made by the caller
           (let ((n (length (lam-req lam))))
             `(,@closure-code ,@(apply #'append arg-codes) ,@(nil-code)
               ,@(loop repeat (- (length arg-codes) n) collect '(:struct.new :cons))
               (,call (:rmain ,(lam-id lam))))))
          ((and arg-nodes (lam-keyed-p lam) (keyed-call-layout lam arg-nodes))
           (let* ((layout (keyed-call-layout lam arg-nodes))
                  (temps (loop for nil in arg-codes collect (new-local))))
             `(,@closure-code
               ,@(loop for code in arg-codes for tmp in temps for node in arg-nodes
                       unless (and (eq (car node) :const) (= 1 (length (cg-const (second node)))))
                         append `(,@code (:local.set ,tmp)))
               ,@(loop for idx across layout
                       collect (if idx
                                   (let ((node (nth idx arg-nodes)))
                                     (if (and (eq (car node) :const)
                                              (= 1 (length (cg-const (second node)))))
                                         (car (cg-const (second node)))
                                         `(:local.get ,(nth idx temps))))
                                   '(:ref.null :eq)))
               (,call (:kmain ,(lam-id lam))))))
          (t
           `(,@closure-code ,@(apply #'append arg-codes)
             (:array.new_fixed :vector ,(length arg-codes))
             (,call (:main ,(lam-id lam))))))))

(defun known-global-lam (sym)
  (let ((lams (gethash sym *global-defuns*)))
    (and lams (null (cdr lams))
         (lam-static-p (car lams))
         (not (gethash sym *notinline*))
         (car lams))))

(defvar *called-functions* (make-hash-table :test 'eq))

(defun fdef-code (sym)
  (setf (gethash sym *called-functions*) t)
  (let ((l (gensym "FD")))
    `((:block ,l (:ref :closure)
        (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-function+)
        (:br_on_non_null ,l)
        (:global.get ,(sym-global sym)) (:call :undefined-function)))))

(defun cg-global-call (sym args mv tail)
  (let ((lam (known-global-lam sym))
        (arg-codes (mapcar #'cg args)))
    ;; in a catch clone, a tail call under no CATCH or UNWIND-PROTECT may call
    ;; the callee's clone for the same tag: its throws to the tag are returns
    (when (and lam tail *tail-ok* *catching-tag* (null *uwp-frames*) (lam-simple-p lam)
               (null (lam-free-vars lam)) (= (length args) (length (lam-req lam))))
      (ensure-lam-compiled lam)
      (return-from cg-global-call
        `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
          (:return_call ,(ensure-catch-clone lam *catching-tag*)))))
    (if (and lam (lam-accepts-p lam (length args)))
        (let ((sig (spec-signature lam args)))
          (if sig
              (progn
                (ensure-lam-compiled lam)
                `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
                  (,(if tail :return_call :call) ,(spec-clone-name lam sig))))
              (direct-call-code lam `((:global.get ,(static-closure-global lam))) arg-codes tail args)))
        (progn
          (when (and lam (not (lam-accepts-p lam (length args))))
            (cwarn "~S called with ~D arguments" sym (length args)))
          (call-closure-code (fdef-code sym) arg-codes mv tail)))))

(defvar *lvar-lams* (make-hash-table :test 'eq) "lvar -> the lam it is always bound to")

(defun cg-funcall (fnode args mv tail)
  (let ((lam (and (eq (car fnode) :lref) (gethash (second fnode) *lvar-lams*))))
    (when (and lam (lam-accepts-p lam (length args)))
      (return-from cg-funcall
        (direct-call-code lam `(,@(var-get-code (second fnode)) (:ref.cast! :closure))
                          (mapcar #'cg args) tail args))))
  (if (eq (car fnode) :lambda)
      (let ((lam (second fnode)))
        (if (lam-accepts-p lam (length args))
            (direct-call-code lam (cg-closure lam) (mapcar #'cg args) tail args)
            (call-closure-code (cg-closure lam) (mapcar #'cg args) mv tail)))
      (call-closure-code (if (ty-only-p (node-ty fnode) +k-func+)
                             `(,@(cg fnode) (:ref.cast! :closure))
                             `(,@(cg fnode) (:call :coerce-function)))
                         (mapcar #'cg args) mv tail)))

(defun cg-local-call (fun args mv tail)
  (let* ((lam (fun-lam fun))
         (closure-code (var-get-code (fun-lvar fun))))
    (if (lam-accepts-p lam (length args))
        (direct-call-code lam `(,@closure-code (:ref.cast! :closure)) (mapcar #'cg args) tail args)
        (progn (cwarn "local function ~S called with ~D arguments" (fun-name fun) (length args))
               (call-closure-code `(,@closure-code (:ref.cast! :closure)) (mapcar #'cg args) mv tail)))))

(defun cg-defun (node mv)
  (destructuring-bind (sym fnode) (cdr node)
    `((:global.get ,(sym-global sym))
      ,@(cg fnode) (:ref.cast! :closure)
      (:struct.set :symbol ,+sym-function+)
      (:global.get ,(sym-global sym))
      ,@(when mv (mv-one)))))

;;; ------------------------------------------------------------
;;; FLET / LABELS
;;; ------------------------------------------------------------

(defun cg-flet (node mv tail)
  (destructuring-bind (op all-funs body) node
    ;; functions that are never referenced need no closure
    (let ((code nil) (funs (remove-if (lambda (f) (zerop (lvar-refs (fun-lvar f)))) all-funs)))
      (if (eq op :flet)
          (let ((tmps (loop for f in funs collect (let ((tmp (new-local)))
                                                    (setf code (append code (cg-closure (fun-lam f))
                                                                       `((:local.set ,tmp))))
                                                    tmp))))
            (loop for f in funs for tmp in tmps
                  do (setf code (append code (bind-new-var-code (fun-lvar f) `((:local.get ,tmp)))))))
          (progn
            ;; allocate locations first so siblings can refer to each other
            (dolist (f funs)
              (set-loc (fun-lvar f) (cons :local (new-local))))
            (dolist (f funs)
              (setf code (append code (cg-closure (fun-lam f))
                                 `((:local.set ,(cdr (get-loc (fun-lvar f))))))))
            ;; patch environments
            (dolist (f funs)
              (let ((lam (fun-lam f)))
                (loop for fv in (lam-free-vars lam) for i from 0
                      when (find fv funs :key #'fun-lvar)
                        do (setf code
                                 (append code
                                         `((:local.get ,(cdr (get-loc (fun-lvar f))))
                                           (:ref.cast! :closure)
                                           (:struct.get :closure ,+clo-env+)
                                           (:i32.const ,i)
                                           (:local.get ,(cdr (get-loc fv)))
                                           (:array.set :vector)))))))))
      (append code (cg body mv tail)))))

;;; ------------------------------------------------------------
;;; BLOCK / RETURN-FROM
;;; ------------------------------------------------------------

(defun cg-block (node mv tail)
  (destructuring-bind (blk body) (cdr node)
    (let ((label (gensym "BLOCK")))
      (setf (blk-label blk) label
            (blk-mv blk) mv
            (blk-dyn-depth blk) *dyn-depth*
            (blk-uwp-depth blk) *uwp-depth*
            (blk-uwp-frames blk) *uwp-frames*
            (blk-saved-bsp blk) (new-local :i32))
      (set-loc (blk-tag-var blk) (cons :local (new-local)))
      (let ((body-code (let ((*tail-ok* (and *tail-ok* (not (blk-nonlocal blk)))))
                         (cg body mv tail))))
        (if (not (blk-nonlocal blk))
            `(,@(save-bsp-code (blk-saved-bsp blk))
              (:block ,label :eqref ,@body-code))
            ;; non-local: a fresh tag object, try_table
            (let ((h (gensym "H")) (vtmp (new-local)) (ttmp (new-local)) (etmp (new-local :exnref)))
              ;; The body was generated with *tail-ok* possibly true; regenerate is costly,
              ;; so we generated it with tail disabled above when nonlocal.
              `(,@(save-bsp-code (blk-saved-bsp blk))
                ,@(var-set-code (blk-tag-var blk) `(,@(nil-code) (:struct.new :cell)))
                (:block ,label :eqref
                  (:block ,h (:type :bt-catch)
                    (:try_table nil :eqref ((:catch_ref :lisp-throw ,h))
                      ,@body-code)
                    (:br ,label))
                  (:local.set ,etmp) (:local.set ,vtmp) (:local.set ,ttmp)
                  (:local.get ,ttmp) ,@(var-get-code (blk-tag-var blk)) :ref.eq
                  (:if nil nil
                       (,@(restore-bsp-code (blk-saved-bsp blk))
                        (:local.get ,vtmp) (:br ,label))
                       ((:local.get ,etmp) :throw_ref))
                  :unreachable))))))))

(defun innermost-uwpf-between (frames outer)
  "The innermost unwind-protect frame in FRAMES above the tail OUTER, or NIL."
  (do ((f frames (cdr f))) ((or (null f) (eq f outer)) nil)
    (when (uwpf-p (car f)) (return (car f)))))

(defun register-uwp-exit (frame continuation)
  "Register a local exit through FRAME; CONTINUATION generates the code that
continues the exit after the cleanup (in the context outside FRAME).  Returns
the code that starts the exit, the value (if any) being in (uwpf-res FRAME)."
  (let ((k (incf (uwpf-nexits frame))))
    (push (cons k continuation) (uwpf-exits frame))
    `((:i32.const ,k) (:local.set ,(uwpf-code frame)) (:br ,(uwpf-landing frame)))))

(defun local-exit-to-block (blk value-code)
  "Code for a return-from BLK in the current function, VALUE-CODE pushing the value."
  (let ((frame (innermost-uwpf-between *uwp-frames* (blk-uwp-frames blk))))
    (if (null frame)
        `(,@value-code
          ,@(when (> *dyn-depth* (blk-dyn-depth blk))
              (restore-bsp-code (blk-saved-bsp blk)))
          (:br ,(blk-label blk)))
        (progn
          (when (blk-mv blk) (setf (uwpf-mv-used frame) t))
          `(,@value-code (:local.set ,(uwpf-res frame))
            ,@(register-uwp-exit
               frame (lambda () (local-exit-to-block blk `((:local.get ,(uwpf-res frame)))))))))))

(defun cg-return-from (node)
  (destructuring-bind (blk value) (cdr node)
    (let ((local-p (eq (blk-lam blk) (fstate-lam *fs*))))
      (if local-p
          (local-exit-to-block blk (cg value (blk-mv blk)))
          (progn
            (let ((tmp (new-local)))
              `(,@(cg value t) (:local.set ,tmp)
                ,@(var-get-code (blk-tag-var blk))
                (:local.get ,tmp)
                (:throw :lisp-throw))))))))

;;; ------------------------------------------------------------
;;; TAGBODY / GO
;;; ------------------------------------------------------------

(defun tagbody-go-modes (tb segments)
  "For each tag of TB: :FAR when a local GO jumps to it from a later segment,
:SELF when only from its own segment (and earlier ones), else NIL."
  (let ((modes (make-array (length segments) :initial-element nil)))
    (loop for (k . forms) in segments
          do (labels ((walk (n)
                        (when (and (eq (car n) :go) (eq (second n) tb))
                          (let ((idx (third n)))
                            (cond ((< idx k) (setf (aref modes idx) :far))
                                  ((= idx k) (unless (aref modes idx) (setf (aref modes idx) :self))))))
                        (map-subnodes #'walk n)))
               (mapc #'walk forms)))
    modes))

(defun cg-tagbody (node mv)
  "Segment K starts after the end of block K, which encloses the segments
before it: a GO forward is a BR to that block.  A segment that is the
target of GOs from within itself is a LOOP.  Only GOs back from a later
segment (and from closures) set a segment number and branch to a dispatch
loop with a BR_TABLE."
  (destructuring-bind (tb segments) (cdr node)
    (let* ((n (length segments))
           (labels (loop for i below n collect (gensym (format nil "SEG~D-" i))))
           (dispatch (gensym "DISPATCH"))
           (end (gensym "TBEND"))
           (pc (new-local :i32))
           (modes (tagbody-go-modes tb segments))
           (need-dispatch (or (tb-nonlocal tb) (find :far modes)))
           (loop-labels (loop for i below n collect (and (eq (aref modes i) :self) (gensym "TAGLOOP")))))
      (setf (tb-labels tb) labels (tb-dispatch tb) dispatch (tb-pc tb) pc
            (tb-modes tb) modes (tb-loop-labels tb) loop-labels
            (tb-dyn-depth tb) *dyn-depth* (tb-uwp-depth tb) *uwp-depth* (tb-uwp-frames tb) *uwp-frames*
            (tb-saved-bsp tb) (new-local :i32))
      (set-loc (tb-tag-var tb) (cons :local (new-local)))
      (let* ((seg-codes (let ((*tail-ok* nil))
                          (loop for (k . forms) in segments for ll in loop-labels
                                collect (progn
                                          (setf (tb-cur tb) k)
                                          (let ((code (loop for f in forms append (cg-effect f))))
                                            (if ll `((:loop ,ll nil ,@code)) code))))))
             ;; build nested blocks: innermost has br_table
             (inner (when need-dispatch `((:local.get ,pc) (:br_table ,@labels ,(car (last labels))))))
             (blocks (let ((acc inner))
                       (loop for label in labels for code in seg-codes
                             do (setf acc `((:block ,label nil ,@acc) ,@code)))
                       acc))
             (loop-body (if (tb-nonlocal tb)
                            (let ((h (gensym "H")) (vtmp (new-local)) (ttmp (new-local))
                                  (etmp (new-local :exnref)))
                              `((:block ,h (:type :bt-catch)
                                  (:try_table nil nil ((:catch_ref :lisp-throw ,h))
                                    ,@blocks
                                    (:br ,end))
                                  :unreachable)
                                (:local.set ,etmp) (:local.set ,vtmp) (:local.set ,ttmp)
                                (:local.get ,ttmp) ,@(var-get-code (tb-tag-var tb)) :ref.eq
                                (:if nil nil
                                     (,@(restore-bsp-code (tb-saved-bsp tb))
                                      (:local.get ,vtmp) (:ref.cast :i31) :i31.get_s (:local.set ,pc)
                                      (:br ,dispatch))
                                     ((:local.get ,etmp) :throw_ref))))
                            `(,@blocks (:br ,end)))))
        `(,@(when need-dispatch `((:i32.const 0) (:local.set ,pc)))
          ,@(save-bsp-code (tb-saved-bsp tb))
          ,@(when (tb-nonlocal tb)
              (var-set-code (tb-tag-var tb) `(,@(nil-code) (:struct.new :cell))))
          (:block ,end nil
            ,@(if need-dispatch `((:loop ,dispatch nil ,@loop-body)) loop-body))
          ,@(nil-code)
          ,@(when mv (mv-one)))))))

(defun local-go (tb index)
  (let ((frame (innermost-uwpf-between *uwp-frames* (tb-uwp-frames tb))))
    (if (null frame)
        `(,@(when (> *dyn-depth* (tb-dyn-depth tb)) (restore-bsp-code (tb-saved-bsp tb)))
          ,@(case (aref (tb-modes tb) index)
              (:far `((:i32.const ,index) (:local.set ,(tb-pc tb)) (:br ,(tb-dispatch tb))))
              (:self `((:br ,(if (eql (tb-cur tb) index)
                                 (nth index (tb-loop-labels tb))
                                 (nth index (tb-labels tb))))))
              (t `((:br ,(nth index (tb-labels tb)))))))
        (register-uwp-exit frame (lambda () (local-go tb index))))))

(defun cg-go (node)
  (destructuring-bind (tb index) (cdr node)
    (let ((local-p (eq (tb-lam tb) (fstate-lam *fs*))))
      (if local-p
          (local-go tb index)
          (progn
            `(,@(var-get-code (tb-tag-var tb))
              (:i32.const ,index) :ref.i31
              (:throw :lisp-throw)))))))

;;; ------------------------------------------------------------
;;; CATCH / THROW / UNWIND-PROTECT
;;; ------------------------------------------------------------

(defvar *local-catches* nil
  "The CATCH forms around the code being compiled in the current function,
innermost first: (tag-symbol-or-:UNKNOWN done-label saved-bsp-local frames).")

(defun cg-catch (node mv)
  (destructuring-bind (tag-node body) (cdr node)
    (let ((tag (new-local)) (saved (new-local :i32))
          (vtmp (new-local)) (ttmp (new-local)) (etmp (new-local :exnref))
          (done (gensym "CATCH")) (h (gensym "H")))
      `(,@(cg tag-node) (:local.set ,tag)
        ,@(save-bsp-code saved)
        ;; bind *catch-tags* to the tag: the active tags are its value and the
        ;; values saved by its bindings on the binding stack
        (:global.get ,(sym-global 'clysm-sys::*catch-tags*))
        (:local.get ,tag)
        (:call :bind)
        (:block ,done :eqref
          (:block ,h (:type :bt-catch)
            (:try_table nil :eqref ((:catch_ref :lisp-throw ,h))
              ,@(let* ((*tail-ok* nil) (*uwp-depth* (1+ *uwp-depth*)) (*dyn-depth* (1+ *dyn-depth*))
                       (*uwp-frames* (cons :catch *uwp-frames*))
                       (*local-catches*
                         (cons (list (if (and (eq (car tag-node) :const) (symbolp (second tag-node)))
                                         (second tag-node) :unknown)
                                     done saved *uwp-frames*)
                               *local-catches*)))
                  (or (catch-clone-call tag-node body) (cg body mv))))
            ,@(restore-bsp-code saved)
            (:br ,done))
          (:local.set ,etmp) (:local.set ,vtmp) (:local.set ,ttmp)
          (:local.get ,ttmp) (:local.get ,tag) :ref.eq
          (:if nil nil
               (,@(restore-bsp-code saved) (:local.get ,vtmp) (:br ,done))
               ((:local.get ,etmp) :throw_ref))
          :unreachable)))))

(defun cg-throw (node)
  (destructuring-bind (tag-node value) (cdr node)
    (when (and *catching-tag* (null *uwp-frames*) (eq (car tag-node) :const)
               (eq (second tag-node) *catching-tag*))
      (return-from cg-throw `(,@(cg value t) :return)))
    ;; a throw to a CATCH of the same constant tag around it in this function
    ;; (with no UNWIND-PROTECT in between) is a branch
    (when (and (eq (car tag-node) :const) (symbolp (second tag-node)))
      (let ((entry (dolist (e *local-catches*)
                     (cond ((eq (first e) (second tag-node)) (return e))
                           ((eq (first e) :unknown) (return nil))))))
        (when (and entry (null (innermost-uwpf-between *uwp-frames* (fourth entry))))
          (return-from cg-throw
            `(,@(cg value t) ,@(restore-bsp-code (third entry)) (:br ,(second entry)))))))
    (let ((tag (new-local)) (v (new-local)))
      `(,@(cg tag-node) (:local.tee ,tag) (:call :check-catch-tag)
        ,@(cg value t) (:local.set ,v)
        (:local.get ,tag) (:local.get ,v)
        (:throw :lisp-throw)))))

(defun cg-unwind-protect (node mv)
  (destructuring-bind (form cleanup) (cdr node)
    (let* ((saved (new-local :i32)) (res (new-local)) (etmp (new-local :exnref))
           (mvsave (new-local)) (code (new-local :i32))
           (done (gensym "UWP")) (h (gensym "H")) (land (gensym "LAND"))
           (frame (make-uwpf :code code :res res :landing land :dyn-depth *dyn-depth*
                             :uwp-depth *uwp-depth* :parent *uwp-frames*))
           (body-code (let ((*tail-ok* nil) (*uwp-depth* (1+ *uwp-depth*))
                            (*uwp-frames* (cons frame *uwp-frames*)))
                        (cg form mv))))
      (let* ((cleanup-code (let ((*tail-ok* nil)) `(,@(cg cleanup) :drop)))
             (normal-code (if mv
                             `((:local.get ,res) (:call :mv-save) (:local.set ,mvsave)
                               ,@cleanup-code
                               (:local.get ,mvsave) (:call :mv-restore))
                             `(,@cleanup-code (:local.get ,res)))))
        `(,@(save-bsp-code saved)
          ,@(when (uwpf-exits frame) `((:i32.const 0) (:local.set ,code)))
          (:block ,done :eqref
            (:block ,h :exnref
              ,@(if (null (uwpf-exits frame))
                    `((:try_table nil :eqref ((:catch_all_ref ,h)) ,@body-code)
                      ;; normal exit
                      (:local.set ,res)
                      ,@normal-code)
                    ;; local exits through the cleanup: they set CODE and RES, and branch to LAND
                    `((:block ,land nil
                        (:try_table nil :eqref ((:catch_all_ref ,h)) ,@body-code)
                        (:local.set ,res)
                        (:br ,land))
                      (:local.get ,code) :i32.eqz
                      (:if nil :eqref
                           ,normal-code
                           (,@(restore-bsp-code saved)
                            ,@(when (uwpf-mv-used frame)
                                `((:local.get ,res) (:call :mv-save) (:local.set ,mvsave)))
                            ,@cleanup-code
                            ,@(when (uwpf-mv-used frame)
                                `((:local.get ,mvsave) (:call :mv-restore) :drop))
                            ,@(let ((*uwp-frames* (uwpf-parent frame)) (*uwp-depth* (uwpf-uwp-depth frame))
                                    (*dyn-depth* (uwpf-dyn-depth frame)) (*tail-ok* nil))
                                (loop for (k . cont) in (reverse (uwpf-exits frame))
                                      append `((:local.get ,code) (:i32.const ,k) :i32.eq
                                               (:if nil nil ,(funcall cont)))))
                            :unreachable))))
              (:br ,done))
            (:local.set ,etmp)
            ,@(restore-bsp-code saved)
            (:ref.null :eq) (:call :mv-save) (:local.set ,mvsave)
            ,@cleanup-code
            (:local.get ,mvsave) (:call :mv-restore) :drop
            (:local.get ,etmp) :throw_ref))))))

;;; ------------------------------------------------------------
;;; Multiple values
;;; ------------------------------------------------------------

(defun cg-mvcall (node mv tail)
  (declare (ignore mv))
  (destructuring-bind (fnode &rest args) (cdr node)
    (let ((ftmp (new-local '(:ref :null :closure)))
          (acc (new-local)))
      `(,@(cg fnode) (:call :coerce-function) (:local.set ,ftmp)
        ,@(nil-code) (:local.set ,acc)
        ,@(loop for a in args
                append `(,@(cg a t) (:call :mv-to-list) (:local.get ,acc) (:call :revappend-onto)
                         (:local.set ,acc)))
        (:local.get ,ftmp) (:ref.as_non_null)
        (:local.get ,acc) (:call :reverse-list-to-vector)
        (:local.get ,ftmp) (:struct.get :closure ,+clo-entry-n+)
        (,(if tail :return_call_ref :call_ref) :fN)))))

(defun cg-mvprog1 (node mv)
  (destructuring-bind (form body) (cdr node)
    (let ((res (new-local)) (save (new-local)))
      (if mv
          `(,@(cg form t) (:call :mv-save) (:local.set ,save)
            ,@(cg body) :drop
            (:local.get ,save) (:call :mv-restore))
          `(,@(cg form) (:local.set ,res) ,@(cg body) :drop (:local.get ,res))))))

;;; ------------------------------------------------------------
;;; misc
;;; ------------------------------------------------------------

(defun cg-ltv (node mv)
  (let ((g (list :ltv (incf *ltv-counter*))) (l (gensym "LTV")))
    (push (list g :eqref t '((:ref.null :eq))) *misc-globals*)
    (let ((value-code (let ((*fs* *fs*)) (cg (second node)))))
      `((:block ,l :eqref
          (:global.get ,g) (:br_on_non_null ,l)
          ,@value-code (:global.set ,g) (:global.get ,g))
        ,@(when mv (mv-one))))))

(defun cg-progv (node mv)
  (destructuring-bind (syms vals body) (cdr node)
    (let ((saved (new-local :i32)) (res (new-local)))
      `(,@(save-bsp-code saved)
        ,@(cg syms) ,@(cg vals) (:call :progv-bind)
        ,@(with-dynamic-scope () (cg body mv))
        (:local.set ,res)
        ,@(restore-bsp-code saved)
        (:local.get ,res)))))

(defun resolve-wasm-instrs (instrs)
  "Register symbol/const references inside inline wasm."
  (labels ((walk (x)
             (cond ((and (consp x) (eq (car x) :sym)) (sym-global (second x)))
                   ((and (consp x) (eq (car x) :quote)) (const-global (second x)))
                   ((consp x) (cons (walk (car x)) (walk (cdr x))))
                   (t x))))
    (mapcar (lambda (i)
              (cond ((and (consp i) (eq (car i) :nil)) (car (nil-code)))
                    ((and (consp i) (eq (car i) :t)) (car (t-code)))
                    ((and (consp i) (eq (car i) :lisp-const)) (car (const-init-code (second i))))
                    (t (walk i))))
            instrs)))

(defun cg-wasm (node mv)
  (destructuring-bind (rtype args instrs) (cdr node)
    `(,@(loop for a in args append (cg a))
      ,@(resolve-wasm-instrs instrs)
      ,@(ecase rtype
          (:eqref nil)
          ((nil) (nil-code))
          (:i32 '(:ref.i31))
          (:bool (bool-code)))
      ,@(when mv (mv-one)))))

;;; ------------------------------------------------------------
;;; Lambda bodies
;;; ------------------------------------------------------------

(defun compile-pending-lams ()
  (loop while *pending-lams*
        do (let ((lam (pop *pending-lams*)) (*local-catches* nil))
             (cond ((and (consp lam) (eq (car lam) :spec))
                    (compile-spec-clone (second lam) (third lam)))
                   ((consp lam)
                    (compile-catch-clone (car lam) (cdr lam)))
                   (t
                    (compile-lam lam)
                    (setf (gethash lam *compiled-lams*) t))))))

;;; (CATCH 'TAG (F ...)) where F is a known function of required parameters
;;; calls a copy of F in which a (THROW 'TAG value) not inside a CATCH,
;;; UNWIND-PROTECT or local function of F is a plain return: the catch is F's
;;; caller, and the value of either way out is the value of the CATCH.
(defvar *catching-tag* nil "in a catch clone: the tag whose throws return")
(defvar *catch-clones* (make-hash-table :test 'equal))

(defun catch-clone-name (lam tag) (list :catch-main (lam-id lam) (sym-global tag)))

(defun ensure-catch-clone (lam tag)
  (let ((key (cons (lam-id lam) tag)))
    (unless (gethash key *catch-clones*)
      (setf (gethash key *catch-clones*) t)
      (push (cons lam tag) *pending-lams*))
    (catch-clone-name lam tag)))

(defun compile-catch-clone (lam tag)
  (inline-calls lam)
  (infer-lam lam)
  (let* ((n (length (lam-req lam)))
         (*fs* (make-fstate :lam lam :nparams (1+ n)))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil)
         (prologue (setup-free-vars lam))
         (any-special nil))
    (loop for lv in (lam-req lam) for i from 1
          do (multiple-value-bind (code sp) (param-setup-code lv i)
               (setf prologue (append prologue code))
               (when sp (setf any-special t))))
    (let ((body (let ((*catching-tag* tag)) (finish-body lam prologue any-special))))
      (add-func *m* (catch-clone-name lam tag) (main-type n)
                :locals (reverse (fstate-locals *fs*)) :body body
                :display (format nil "~A/catch" (lam-display-name lam))))))

(defun catch-clone-call (tag-node body)
  "The catch-clone call code for (CATCH TAG-NODE BODY), or NIL."
  (let ((tag (and (eq (car tag-node) :const) (symbolp (second tag-node)) (second tag-node))))
    (when (and tag (eq (car body) :call))
      (let ((lam (known-global-lam (second body))) (args (cddr body)))
        (when (and lam (lam-simple-p lam) (null (lam-free-vars lam))
                   (= (length args) (length (lam-req lam))))
          (let ((arg-codes (mapcar #'cg args)))
            (ensure-lam-compiled lam)
            `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
              (:call ,(ensure-catch-clone lam tag)))))))))

(defun setup-free-vars (lam)
  (when (lam-free-vars lam)
    (let ((env (new-local '(:ref :null :vector))))
      (setf (fstate-env-local *fs*) env)
      (loop for fv in (lam-free-vars lam) for i from 0
            do (set-loc fv (cons :env i)))
      `((:local.get 0) (:struct.get :closure ,+clo-env+) (:local.set ,env)))))

(defun param-setup-code (lv local-idx)
  "LV was passed in LOCAL-IDX.  Returns (values code special-p)."
  (cond ((lvar-special lv)
         (values `((:global.get ,(sym-global (lvar-name lv))) (:local.get ,local-idx) (:call :bind)) t))
        ((boxed-p lv)
         (set-loc lv (cons :local local-idx))
         (values `((:local.get ,local-idx) (:struct.new :cell) (:local.set ,local-idx)) nil))
        ((lvar-rep lv)
         ;; known to be a fixnum / double (in a specialized clone): unboxed copy
         (let ((idx (new-local (rep-wasm-type (lvar-rep lv)))))
           (set-loc lv (cons :local idx))
           (values `((:local.get ,local-idx) ,@(unbox-rep-code (lvar-rep lv) (lvar-type lv)) (:local.set ,idx)) nil)))
        (t (set-loc lv (cons :local local-idx)) (values nil nil))))

(defun finish-body (lam prologue any-special)
  "Compile LAM's body after PROLOGUE.  If ANY-SPECIAL, unbind at the end."
  (if any-special
      (let ((saved (new-local :i32)) (res (new-local)))
        ;; saved bsp must be captured before binding: prologue is wrapped
        (values `((:global.get :bsp) (:local.set ,saved)
                  ,@prologue
                  ,@(with-dynamic-scope () (cg (lam-body lam) t nil))
                  (:local.set ,res)
                  ,@(restore-bsp-code saved)
                  (:local.get ,res))))
      `(,@prologue ,@(let ((*tail-ok* t)) (cg (lam-body lam) t t)))))

(defun compile-lam (lam &aux (*catching-tag* nil))
  (inline-calls lam)
  (infer-lam lam)
  (cond ((lam-simple-p lam) (compile-simple-lam lam))
        ((lam-keyed-p lam) (compile-keyed-lam lam))
        ((lam-rest-only-p lam) (compile-rest-lam lam))
        (t (compile-complex-lam lam))))

(defun compile-simple-lam (lam)
  (compile-simple-main lam (list :main (lam-id lam)) (lam-display-name lam))
  (let ((n (length (lam-req lam))))
    ;; entryN adapter
    (add-func *m* (list :entn (lam-id lam)) :fN
              :body `((:local.get 1) :array.len (:i32.const ,n) :i32.ne
                      (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :argerr-n)))
                      (:local.get 0)
                      ,@(loop for i below n
                              append `((:local.get 1) (:i32.const ,i) (:array.get :vector)))
                      (:return_call (:main ,(lam-id lam)))))))

(defun compile-simple-main (lam name display)
  (let* ((n (length (lam-req lam)))
         (*fs* (make-fstate :lam lam :nparams (1+ n)))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil)
         (prologue (setup-free-vars lam))
         (any-special nil))
    (loop for lv in (lam-req lam) for i from 1
          do (multiple-value-bind (code sp) (param-setup-code lv i)
               (setf prologue (append prologue code))
               (when sp (setf any-special t))))
    (let ((body (finish-body lam prologue any-special)))
      (add-func *m* name (main-type n)
                :locals (reverse (fstate-locals *fs*)) :body body :display display))))

;;; A direct call of a known function with required parameters whose
;;; arguments include fixnums or doubles calls a copy of the function
;;; compiled for those argument types (its own type analysis, the parameters
;;; unboxed).
(defvar *spec-clones* (make-hash-table :test 'equal))
(defvar *spec-cloneable* (make-hash-table :test 'eq))
(defparameter *spec-max-nodes* 3000)

(defun spec-cloneable-p (lam)
  (multiple-value-bind (v found) (gethash lam *spec-cloneable*)
    (if found v
        (setf (gethash lam *spec-cloneable*)
              (and (lam-simple-p lam) (null (lam-free-vars lam)) (lam-req lam)
                   (notany #'lvar-special (lam-req lam))
                   (let ((n 0))
                     (labels ((walk (node)
                                (when (> (incf n) *spec-max-nodes*) (return-from spec-cloneable-p
                                                                       (setf (gethash lam *spec-cloneable*) nil)))
                                (when (member (car node) '(:lambda :flet :labels :ltv))
                                  (return-from spec-cloneable-p (setf (gethash lam *spec-cloneable*) nil)))
                                (map-subnodes #'walk node)))
                       (walk (lam-body lam))
                       t)))))))

(defun arg-class (node)
  (let ((ty (node-ty node)))
    (cond ((ty-fixnum-p ty) :fixnum)
          ((ty-double-p ty) :double))))

(defun class-ty (class)
  (ecase class
    (:fixnum *ty-fixnum*)
    (:double (mkty +k-double+))
    ((nil) *ty-top*)))

(defun spec-signature (lam args)
  (when (and (spec-cloneable-p lam) (= (length args) (length (lam-req lam))))
    (let ((sig (mapcar #'arg-class args)))
      (when (some #'identity sig) sig))))

(defun spec-clone-name (lam sig)
  (let ((key (list (lam-id lam) sig)))
    (unless (gethash key *spec-clones*)
      (setf (gethash key *spec-clones*) t)
      (push (list :spec lam sig) *pending-lams*))
    (list :spec (lam-id lam) sig)))

(defun lam-all-lvars (lam)
  (let ((r (copy-list (lam-req lam))))
    (labels ((walk (n)
               (case (car n)
                 (:let (dolist (b (second n)) (push (car b) r)))
                 (:mvbind (dolist (lv (second n)) (push lv r))))
               (map-subnodes #'walk n)))
      (walk (lam-body lam)))
    r))

(defun compile-spec-clone (lam sig)
  (inline-calls lam)
  (let* ((lvars (lam-all-lvars lam))
         (saved (mapcar (lambda (lv) (cons (lvar-type lv) (lvar-rep lv))) lvars)))
    (dolist (lv lvars) (setf (lvar-type lv) nil (lvar-rep lv) nil))
    (unwind-protect
         (let ((*node-types* (make-hash-table :test 'eq))
               (*ti-analyzed* (make-hash-table :test 'eq))
               (*catching-tag* nil))
           (infer-lam lam (mapcar #'class-ty sig))
           (compile-simple-main lam (list :spec (lam-id lam) sig)
                                (format nil "~A/~{~(~A~)~^,~}" (lam-display-name lam)
                                        (mapcar (lambda (c) (or c "t")) sig))))
      (loop for lv in lvars for (ty . rep) in saved
            do (setf (lvar-type lv) ty (lvar-rep lv) rep)))))

(defun compile-rest-lam (lam)
  "Required parameters and &REST: the body (:RMAIN) takes the rest list as
its last parameter; the entries for few arguments make the list."
  (let ((n (length (lam-req lam))) (id (lam-id lam)))
    (let* ((*fs* (make-fstate :lam lam :nparams (+ n 2)))
           (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil)
           (prologue (setup-free-vars lam))
           (any-special nil))
      (loop for lv in (append (lam-req lam) (list (lam-rest lam))) for i from 1
            do (multiple-value-bind (code sp) (param-setup-code lv i)
                 (setf prologue (append prologue code))
                 (when sp (setf any-special t))))
      (let ((body (finish-body lam prologue any-special)))
        (add-func *m* (list :rmain id) (main-type (1+ n))
                  :locals (reverse (fstate-locals *fs*)) :body body :display (lam-display-name lam))))
    (add-func *m* (list :main id) :fN
              :body `((:local.get 1) :array.len (:i32.const ,n) :i32.lt_s
                      (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :argerr-n)))
                      (:local.get 0)
                      ,@(loop for i below n append `((:local.get 1) (:i32.const ,i) (:array.get :vector)))
                      (:local.get 1) (:i32.const ,n) (:call :vector-tail-to-list)
                      (:return_call (:rmain ,id)))
              :display (format nil "~A/generic" (lam-display-name lam)))
    (loop for k from n to +max-fixed-args+
          do (add-func *m* (list :rentry id k) (fixed-entry-type k)
                       :body `((:local.get 0)
                               ,@(loop for i from 1 to k collect `(:local.get ,i))
                               ,@(nil-code)
                               ,@(loop repeat (- k n) collect '(:struct.new :cons))
                               (:return_call (:rmain ,id)))))))

(defun compile-complex-lam (lam)
  (let* ((*fs* (make-fstate :lam lam :nparams 2))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil)
         (prologue (setup-free-vars lam))
         (nargs (new-local :i32))
         (nreq (length (lam-req lam)))
         (nopt (length (lam-opt lam)))
         (specials nil)
         (saved (new-local :i32))
         (code nil))
    (labels ((emit (&rest instrs) (setf code (append code (apply #'append instrs))))
             (bind (lv value-code)
               (if (lvar-special lv)
                   (progn (push lv specials)
                          (emit `((:global.get ,(sym-global (lvar-name lv))) ,@value-code (:call :bind))))
                   (emit (bind-new-var-code lv value-code))))
             (arg (i) `((:local.get 1) (:i32.const ,i) (:array.get :vector))))
      (emit prologue
            `((:local.get 1) :array.len (:local.set ,nargs))
            `((:global.get :bsp) (:local.set ,saved))
            ;; arg count check
            `((:local.get ,nargs) (:i32.const ,nreq) :i32.lt_s
              ,@(unless (or (lam-rest lam) (lam-keyp lam))
                  `((:local.get ,nargs) (:i32.const ,(+ nreq nopt)) :i32.gt_s :i32.or))
              (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :argerr-n)))))
      (loop for lv in (lam-req lam) for i from 0
            do (bind lv (arg i)))
      (loop for (lv init sv) in (lam-opt lam) for i from nreq
            do (let ((*tail-ok* nil))
                 (bind lv `((:local.get ,nargs) (:i32.const ,i) :i32.gt_s
                            (:if nil :eqref ,(arg i)
                                 ,(let ((*dyn-depth* (+ *dyn-depth* (if specials 1 0)))) (cg init)))))
                 (when sv
                   (bind sv `((:local.get ,nargs) (:i32.const ,i) :i32.gt_s ,@(bool-code))))))
      (let ((pos (+ nreq nopt)))
        (when (lam-rest lam)
          (bind (lam-rest lam) `((:local.get 1) (:i32.const ,pos) (:call :vector-tail-to-list))))
        (when (lam-keyp lam)
          (unless (lam-allow-other-keys lam)
            (emit `((:local.get 1) (:i32.const ,pos)
                    ,@(const-init-code (mapcar #'first (lam-keys lam)))
                    (:call :check-keys))))
          (loop for (kw lv init sv) in (lam-keys lam)
                do (let ((tmp (new-local)) (*tail-ok* nil))
                     (emit `((:local.get 1) (:i32.const ,pos) ,@(const-init-code kw)
                             (:call :find-key) (:local.set ,tmp)))
                     (bind lv `((:local.get ,tmp) :ref.is_null
                                (:if nil :eqref ,(cg init) ((:local.get ,tmp)))))
                     (when sv
                       (bind sv `((:local.get ,tmp) :ref.is_null :i32.eqz ,@(bool-code))))))))
      (let ((body
              (if specials
                  (let ((res (new-local)))
                    `(,@code
                      ,@(with-dynamic-scope () (cg (lam-body lam) t nil))
                      (:local.set ,res)
                      ,@(restore-bsp-code saved)
                      (:local.get ,res)))
                  `(,@code ,@(let ((*tail-ok* t)) (cg (lam-body lam) t t))))))
        (add-func *m* (list :main (lam-id lam)) :fN
                  :locals (reverse (fstate-locals *fs*)) :body body :display (lam-display-name lam))))))

(defun lam-display-name (lam)
  (let ((*package* (find-package :keyword)))
    (format nil "~A#~D" (or (lam-name lam) (if (lam-toplevel lam) "toplevel" "lambda")) (lam-id lam))))

(defun compile-keyed-lam (lam)
  "Body takes a fixed-layout vector; MAIN is an adapter from a generic argument vector."
  (let* ((nreq (length (lam-req lam))) (nopt (length (lam-opt lam)))
         (npos (+ nreq nopt)) (size (lam-fixed-size lam)))
    ;; generic entry: argument checks, key lookup, then call the keyed body
    (let* ((*fs* (make-fstate :lam lam :nparams 2))
           (nargs (new-local :i32))
           (body
             `((:local.get 1) :array.len (:local.set ,nargs)
               (:local.get ,nargs) (:i32.const ,nreq) :i32.lt_s
               ,@(unless (lam-keyp lam)
                   `((:local.get ,nargs) (:i32.const ,npos) :i32.gt_s :i32.or))
               (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :argerr-n)))
               ,@(when (and (lam-keyp lam) (not (lam-allow-other-keys lam)))
                   `((:local.get 1) (:i32.const ,npos)
                     ,@(const-init-code (mapcar #'first (lam-keys lam)))
                     (:call :check-keys)))
               (:local.get 0)
               ,@(loop for i below nreq
                       append `((:local.get 1) (:i32.const ,i) (:array.get :vector)))
               ,@(loop for i from nreq below npos
                       append `((:local.get ,nargs) (:i32.const ,i) :i32.gt_s
                                (:if nil :eqref ((:local.get 1) (:i32.const ,i) (:array.get :vector))
                                     ((:ref.null :eq)))))
               ,@(loop for (kw) in (lam-keys lam)
                       append `((:local.get 1) (:i32.const ,npos) ,@(const-init-code kw) (:call :find-key)))
               (:return_call (:kmain ,(lam-id lam))))))
      (add-func *m* (list :main (lam-id lam)) :fN
                :locals (reverse (fstate-locals *fs*)) :body body
                :display (format nil "~A/generic" (lam-display-name lam))))
    ;; the body
    (let* ((*fs* (make-fstate :lam lam :nparams (1+ size)))
           (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil)
           (prologue (setup-free-vars lam))
           (specials nil)
           (saved (new-local :i32))
           (code nil))
      (labels ((emit (&rest instrs) (setf code (append code (apply #'append instrs))))
               (bind (lv value-code)
                 (if (lvar-special lv)
                     (progn (push lv specials)
                            (emit `((:global.get ,(sym-global (lvar-name lv))) ,@value-code (:call :bind))))
                     (emit (bind-new-var-code lv value-code))))
               (slot (i) `((:local.get ,(1+ i))))
               (bind-defaulted (lv init sv i)
                 (let ((tmp (new-local)) (*tail-ok* nil))
                   (emit `(,@(slot i) (:local.set ,tmp)))
                   (bind lv `((:local.get ,tmp) :ref.is_null
                              (:if nil :eqref ,(let ((*dyn-depth* (+ *dyn-depth* (if specials 1 0)))) (cg init))
                                   ((:local.get ,tmp)))))
                   (when sv (bind sv `((:local.get ,tmp) :ref.is_null :i32.eqz ,@(bool-code)))))))
        (emit prologue `((:global.get :bsp) (:local.set ,saved)))
        (loop for lv in (lam-req lam) for i from 0 do (bind lv (slot i)))
        (loop for (lv init sv) in (lam-opt lam) for i from nreq do (bind-defaulted lv init sv i))
        (loop for (nil lv init sv) in (lam-keys lam) for i from npos do (bind-defaulted lv init sv i))
        (let ((body
                (if specials
                    (let ((res (new-local)))
                      `(,@code
                        ,@(with-dynamic-scope () (cg (lam-body lam) t nil))
                        (:local.set ,res)
                        ,@(restore-bsp-code saved)
                        (:local.get ,res)))
                    `(,@code ,@(let ((*tail-ok* t)) (cg (lam-body lam) t t))))))
          (add-func *m* (list :kmain (lam-id lam)) (main-type size)
                    :locals (reverse (fstate-locals *fs*)) :body body :display (lam-display-name lam)))))
    ;; entries for a fixed number of arguments: the missing optionals are null
    (when (lam-opt-only-p lam)
      (loop for k from nreq to (min +max-fixed-args+ npos)
            do (add-func *m* (list :oentry (lam-id lam) k) (fixed-entry-type k)
                         :body `((:local.get 0)
                                 ,@(loop for i from 1 to k collect `(:local.get ,i))
                                 ,@(loop repeat (- size k) collect '(:ref.null :eq))
                                 (:return_call (:kmain ,(lam-id lam)))))))))
