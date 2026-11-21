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
(defvar *closure-global-names* (make-hash-table :test 'equal)
  "The names of *CLOSURE-GLOBALS*, for finding them without a search.")
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
  "FNV-1a over code points, from 8 characters on in four lanes of every
fourth character combined at the end; 29-bit result.  Must match the
runtime (:STRING-HASH in src/prims.lisp)."
  (flet ((fnv (h c) (ldb (byte 32 0) (* (logxor h c) 16777619))))
    (let* ((n (length s)) (h 2166136261) (i 0))
      (if (< n 8)
          (loop while (< i n) do (setf h (fnv h (char-code (char s i))) i (+ i 1)))
          (let ((h1 (+ h 1)) (h2 (+ h 2)) (h3 (+ h 3)))
            (loop while (<= (+ i 4) n)
                  do (setf h (fnv h (char-code (char s i)))
                           h1 (fnv h1 (char-code (char s (+ i 1))))
                           h2 (fnv h2 (char-code (char s (+ i 2))))
                           h3 (fnv h3 (char-code (char s (+ i 3))))
                           i (+ i 4)))
            (loop while (< i n) do (setf h (fnv h (char-code (char s i))) i (+ i 1)))
            (setf h (fnv (fnv (fnv h h1) h2) h3))))
      (ldb (byte 29 0) h))))

(defun string-array-code (s)
  (append (loop for c across s collect `(:i32.const ,(char-code c)))
          `((:array.new_fixed :string ,(length s)))))

(defun bignum-digits (n)
  "Base 2^32 little-endian digits of a non-negative integer."
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

;;; A constant string of characters below 256 is a global made of its length
;;; only ((array.new_default $string)), whose characters :INIT-STRINGS copies
;;; from a data segment of one octet a character when the module starts, or
;;; when a heap image is loaded into it (SET-STRING-TABLE in src/driver.lisp):
;;; an (:I32.CONST C) a character was two or three octets.
(defvar *data-strings* nil "((global-name . string) ...) of DATA-STRING-P constants, reversed")

(defun data-string-p (s)
  (and (simple-string-p s) (every (lambda (c) (< (char-code c) 256)) s)))

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
    (when (and (stringp obj) (not (gethash obj table)) (data-string-p obj))
      (let ((name (new-const-global `((:i32.const ,(length obj)) (:array.new_default :string)))))
        (push (cons name obj) *data-strings*)
        (return-from const-global (setf (gethash obj table) name))))
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
                      (:i32.const 0)    ; (not one-dimensional)
                      (:struct.new :mdarray))))
                 (bit-vector `(,@(loop for x across obj collect `(:i32.const ,x))
                               (:array.new_fixed :bits ,(length obj))))
                 (single-float `((:f32.const ,obj) (:struct.new :single)))
                 (double-float `((:f64.const ,obj) (:struct.new :double)))
                 (ratio `(,@(const-init-code (numerator obj)) ,@(const-init-code (denominator obj))
                          (:struct.new :ratio)))
                 (complex (typecase (realpart obj)
                            (double-float `((:f64.const ,(realpart obj)) (:f64.const ,(imagpart obj))
                                            (:struct.new :complex-d)))
                            (single-float `((:f32.const ,(realpart obj)) (:f32.const ,(imagpart obj))
                                            (:struct.new :complex-s)))
                            (t `(,@(const-init-code (realpart obj)) ,@(const-init-code (imagpart obj))
                                 (:struct.new :complex)))))
                 (t (error "cannot compile constant ~S" obj)))
               ;; a simple vector of its type (an element read only checks
               ;; the bounds: the caches of call sites of readers)
               (if (simple-vector-p obj) '(:ref :vector) :eqref))))))

(defun cg-const (obj) (const-init-code obj))

;;; ------------------------------------------------------------
;;; Function state
;;; ------------------------------------------------------------

(defstruct fstate lam nparams (locals nil) (nlocals 0) (locs (make-hash-table :test 'eq)) env-local
  env-type                              ; the closure type in ENV-LOCAL (NIL: the vector of a :ENVCLOV)
  (shadows (make-hash-table :test 'eq)))   ; lvar -> the locals of an (:MD2 kind) variable's storage, dimensions, offset

(defvar *fs* nil)
(defvar *tail-ok* nil "true when a call in tail position may become return_call")
(defvar *dyn-depth* 0 "number of dynamic-binding scopes active in the current function")
(defvar *uwp-depth* 0 "number of unwind-protect/catch regions active in the current function")
(defvar *uwp-frames* nil
  "The unwind-protect (a UWPF) and catch (:CATCH) regions active in the current
function, innermost first.  A local exit through unwind-protect regions jumps
to the innermost one's cleanup, which then continues the exit.")
(defstruct uwpf code res landing (exits nil) (nexits 0) dyn-depth uwp-depth parent mv-used)
(defvar *fexit* nil
  "In a function being compiled: (blk tag-local value-local pctx); exits through
the function's own cleanups branch to BLK with the pending exit in the two
locals; PCTX is the context of the calls outside every catcher (a pending
exit returns from the function).")
(defstruct pctx label fs used tmp)
(defvar *pend* nil
  "The PCTX of the calls being compiled when they are inside a catcher or an
UNWIND-PROTECT of the current function.")
(defmacro with-pctx ((type) &body body)
  "BODY (code leaving TYPE) as a context of calls of its own; call where the
dynamic environment (*CATCHERS*, *UWP-FRAMES*) is the context's."
  (let ((ctx (gensym)))
    `(let ((,ctx (new-pctx)))
       (wrap-pctx ,ctx (let ((*pend* ,ctx)) ,@body) ,type #'pend-handler-code))))
(defvar *catchers* nil
  "The constructs around the code being compiled in the current function that
catch non-local exits, innermost first: (:block blk), (:tagbody tb) or
(:catch tag-local done-label saved-bsp-local frames tag-symbol-or-:unknown).")

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
(defun rep-wasm-type (rep)
  (if (consp rep)
      (if (eq (first rep) :md2)
          '(:ref :null :mdarray)        ; (:md2 kind): an array header
          `(:ref :null ,(second rep)))  ; (:ref kind): a reference to a storage type
      (ecase rep (:eqref :eqref) (:f64 :f64) (:f32 :f32) (:i32 :i32) (:i64 :i64))))

(defun rep-block-type (rep)
  "The block type of a block leaving a value of REP."
  (case rep (:c64 '(:type :v-dd)) (:c32 '(:type :v-ff)) (t (rep-wasm-type rep))))

(defun new-rep-local (rep)
  "A new local for a value of REP (two consecutive locals for the parts of a complex number)."
  (case rep
    (:c64 (prog1 (new-local :f64) (new-local :f64)))
    (:c32 (prog1 (new-local :f32) (new-local :f32)))
    (t (new-local (rep-wasm-type rep)))))

(defun rep-get-code (rep idx)
  (if (member rep '(:c64 :c32)) `((:local.get ,idx) (:local.get ,(1+ idx))) `((:local.get ,idx))))
(defun rep-set-code (rep idx)
  (if (member rep '(:c64 :c32)) `((:local.set ,(1+ idx)) (:local.set ,idx)) `((:local.set ,idx))))

(defun md2-shadows (lv)
  "The locals (data dim0 dim1 offset) of the (:MD2 kind) variable LV."
  (or (gethash lv (fstate-shadows *fs*))
      (setf (gethash lv (fstate-shadows *fs*))
            (list (new-local `(:ref :null ,(second (lvar-rep lv)))) (new-local :i32) (new-local :i32) (new-local :i32)))))

(defvar *cg-loop* nil "The innermost TAGBODY node with a loop around the code being compiled.")

(defun innermost-loop-p ()
  "The code being compiled is in a loop that has no loop inside it."
  (and *cg-loop*
       (let ((inner nil))
         (labels ((walk (n) (when (and (consp n) (not inner))
                              (if (and (eq (car n) :tagbody) (not (eq n *cg-loop*)))
                                  (setf inner t)
                                  (map-subnodes #'walk n)))))
           (walk *cg-loop*))
         (not inner))))

(defun md2-alias-src (lv init)
  (let ((src (loop while (eq (car init) :the) do (setf init (third init))
                   finally (return (and (eq (car init) :lref) (second init))))))
    ;; (only in an innermost loop: elsewhere the locals of the array, live
    ;; across the inner loops, cost the engine more registers than the
    ;; loads of its header they save)
    (and src (innermost-loop-p) (not (lvar-special lv)) (not (boxed-p lv)) (not (lvar-mutated lv)) (not (lvar-mutated src))
         (consp (lvar-rep lv)) (eq (first (lvar-rep lv)) :md2)
         (eq (lvar-rep src) (lvar-rep lv)) (unboxed-local-p src)
         src)))

;;; A special variable used as the array of two-dimensional accesses in a
;;; loop (a program's *GRID*): on entry to the outermost such loop its value,
;;; when a simple array of element type T, is kept in a local with its
;;; storage, dimensions and offset; each access compares the variable's value
;;; with it (one load and a REF.EQ: the loop may assign or bind the variable,
;;; a function it calls too) and takes those locals when it is the same, the
;;; access of an array of unknown type otherwise.
(defvar *sref-md2-caches* nil "((symbol header data dim0 dim1 offset) ...)")
(defvar *sref-md2-aliases* nil "((lvar . symbol) ...): variables bound to such a special variable")

(defun strip-the (node)
  (loop while (eq (car node) :the) do (setf node (third node)))
  node)

(defun sref-md2-symbols (node)
  "The special variables (not cached yet) used as the array of a two-dimensional access in NODE."
  (let ((syms nil) (aliases nil))
    (labels ((arr-sym (a)
               (let ((a (strip-the a)))
                 (case (car a)
                   (:sref (second a))
                   (:lref (cdr (assoc (second a) aliases))))))
             (walk (n)
               (case (car n)
                 ((:lambda :flet :labels :defun) (return-from walk))
                 (:let (dolist (b (second n))
                         (let ((a (strip-the (cdr b))))
                           (when (and (eq (car a) :sref) (not (lvar-mutated (car b))) (not (lvar-special (car b))))
                             (push (cons (car b) (second a)) aliases)))))
                 ((:prim :call)
                  (when (member (node-op-name n) '(clysm-sys::%aref2 clysm-sys::%aset2))
                    (let ((sym (arr-sym (third n))))
                      (when (and sym (symbolp sym) (not (assoc sym *sref-md2-caches*)))
                        (pushnew sym syms))))))
               (map-subnodes #'walk n)))
      (walk node))
    syms))

(defun sref-md2-wrap (node body-fn)
  (let ((syms (and *fs* (sref-md2-symbols node))))
    (if (null syms)
        (funcall body-fn)
        (let* ((caches (loop for sym in syms
                             collect (list sym (new-local '(:ref :null :mdarray)) (new-local '(:ref :null :vector))
                                           (new-local :i32) (new-local :i32) (new-local :i32))))
               (init (loop for (sym c data d0 d1 off) in caches
                           append (let ((e (new-local)))
                                    `((:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-value+) (:local.tee ,e)
                                      (:call :simple-md2-p)
                                      (:if nil nil
                                           ((:local.get ,e) (:ref.cast :mdarray) (:local.tee ,c)
                                            (:struct.get :mdarray 0) (:ref.cast :vector) (:local.set ,data)
                                            (:local.get ,c) (:struct.get :mdarray 5) (:local.set ,d0)
                                            (:local.get ,c) (:struct.get :mdarray 6) (:local.set ,d1)
                                            (:local.get ,c) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s
                                            (:local.set ,off))
                                           ;; (an object no value is EQ to)
                                           ((:struct.new_default :mdarray) (:local.set ,c))))))))
          (let ((*sref-md2-caches* (append caches *sref-md2-caches*)) (*sref-md2-aliases* *sref-md2-aliases*))
            (append init (funcall body-fn)))))))

(defun sref-md2-cache-of (a)
  "The cache of the special variable that the array node A reads (directly or through a variable bound to it), or NIL."
  (when *sref-md2-caches*
    (let* ((a (strip-the a))
           (sym (case (car a)
                  (:sref (second a))
                  (:lref (cdr (assoc (second a) *sref-md2-aliases*))))))
      (and sym (assoc sym *sref-md2-caches*)))))

;;; A LET binding a variable of unknown type used as the array of
;;; two-dimensional accesses in a loop of its body (a slot's value, a
;;; function's result) is compiled twice: when the value is a simple array of
;;; element type T (neither adjustable nor displaced, so that its header does
;;; not change), the body with the variable held as (:MD2 :VECTOR), its
;;; storage, dimensions and offset in locals loaded once; else the body as
;;; usual.  (A parameter is tested on entry instead: ENTRY-VERSION-SIGNATURE.)
(defvar *md2-version-vars* nil
  "(variables) in a cons while the version of a LET for a simple two-dimensional
array is compiled: the array's variable and the variables bound to it.")
(defparameter *md2-version-max-nodes* 1200)

(defun md2-version-alias-src (lv init)
  (when *md2-version-vars*
    (let ((src (loop while (eq (car init) :the) do (setf init (third init))
                     finally (return (and (eq (car init) :lref) (second init))))))
      (and src (member src (car *md2-version-vars*))
           (null (lvar-rep lv)) (not (lvar-special lv)) (not (boxed-p lv)) (not (lvar-mutated lv))
           src))))

(defun md2-version-body-p (lv body)
  "BODY has a two-dimensional access to LV (or a variable bound to it) in a
loop, nothing that may not be compiled twice, and not many nodes."
  (let ((n 0) (found nil) (vars (list lv)))
    (labels ((arr-p (node)
               (loop while (eq (car node) :the) do (setf node (third node)))
               (and (eq (car node) :lref) (member (second node) vars)))
             (walk (node in-loop)
               (when (> (incf n) *md2-version-max-nodes*) (return-from md2-version-body-p nil))
               (case (car node)
                 ((:lambda :flet :labels :ltv :defun) (return-from md2-version-body-p nil))
                 (:lset (when (member (second node) vars) (return-from md2-version-body-p nil)))
                 (:let (dolist (b (second node))
                         (when (arr-p (cdr b)) (push (car b) vars))))
                 (:tagbody (setf in-loop t))
                 ((:prim :call)
                  (when (and in-loop (member (node-op-name node) '(clysm-sys::%aref2 clysm-sys::%aset2))
                         (arr-p (third node)))
                    (setf found t))))
               (map-subnodes (lambda (x) (walk x in-loop)) node)))
      (walk body nil))
    found))

(defun md2-version-var (bindings body rep)
  "The variable of BINDINGS for which the LET is compiled in two versions, or NIL."
  (unless (or (consp rep) (member rep '(:c64 :c32)))
    (loop for b in bindings
          for lv = (car b)
          when (and (null (lvar-rep lv)) (not (lvar-special lv)) (not (lvar-mutated lv)) (not (lvar-captured lv))
                    (not (boxed-p lv)) (eq (car (gethash lv (fstate-locs *fs*))) :local)
                    (not (and *md2-version-vars* (member lv (car *md2-version-vars*))))
                    ;; (of a type that may be an array header)
                    (let ((ty (lvar-type lv))) (or (null ty) (not (ty-only-p ty (lognot +k-other+)))))
                    (md2-version-body-p lv body))
            return lv)))

(defun md2-version-code (lv rep body-fn tail)
  (let* ((e (cdr (get-loc lv)))
         (hdr (new-local '(:ref :null :mdarray)))
         (block-type (case rep (:effect nil) (:test :i32) (t (rep-block-type rep))))
         (fast (let ((*md2-version-vars* (list (list lv))))
                 (setf (lvar-rep lv) (md2-rep :vector))
                 (remhash lv (fstate-shadows *fs*))
                 (set-loc lv (cons :local hdr))
                 (unwind-protect
                      (append `((:local.get ,e) (:ref.cast :mdarray)) (rep-local-set-code lv hdr)
                              (funcall body-fn tail))
                   (dolist (v (car *md2-version-vars*))
                     (setf (lvar-rep v) nil)
                     (remhash v (fstate-shadows *fs*)))
                   (set-loc lv (cons :local e)))))
         (general (funcall body-fn tail)))
    `((:local.get ,e) (:call :simple-md2-p)
      (:if nil ,block-type ,fast ,general))))

(defun md2-copy-code (lv idx init)
  "When LV, a two-dimensional array variable (:MD2 kind) whose local is IDX,
is bound to another one of the same representation (the variable of a SETF
expansion, the parameter of an inline function): code copying its header
and the locals of its storage, dimensions and offset (instead of loading
them from the header again, in a loop), else NIL."
  (let ((src (loop while (eq (car init) :the) do (setf init (third init))
                   finally (return (and (eq (car init) :lref) (second init))))))
    (when (and src (innermost-loop-p) (consp (lvar-rep lv)) (eq (first (lvar-rep lv)) :md2)
               (eq (lvar-rep src) (lvar-rep lv)) (unboxed-local-p src))
      `((:local.get ,(cdr (get-loc src))) (:local.set ,idx)
        ,@(loop for from in (md2-shadows src) for to in (md2-shadows lv)
                append `((:local.get ,from) (:local.set ,to)))))))

(defun rep-local-set-code (lv idx &optional tee)
  "Store the value on the stack (in LV's representation) in LV's local IDX
(with TEE the value stays on the stack)."
  (if (and (consp (lvar-rep lv)) (eq (first (lvar-rep lv)) :md2))
      (destructuring-bind (data d0 d1 off) (md2-shadows lv)
        `((:local.set ,idx)
          (:local.get ,idx) (:struct.get :mdarray 0) (:ref.cast ,(second (lvar-rep lv))) (:local.set ,data)
          (:local.get ,idx) (:struct.get :mdarray 5) (:local.set ,d0)
          (:local.get ,idx) (:struct.get :mdarray 6) (:local.set ,d1)
          (:local.get ,idx) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s (:local.set ,off)
          ,@(when tee `((:local.get ,idx)))))
      (if (member (lvar-rep lv) '(:c64 :c32))
          `((:local.set ,(1+ idx)) (:local.set ,idx) ,@(when tee `((:local.get ,idx) (:local.get ,(1+ idx)))))
          `((,(if tee :local.tee :local.set) ,idx)))))
(defun box-rep-code (rep &optional ty)
  "Box a value of REP; for :i32, TY tells whether it is a fixnum (else it may be a bignum)."
  (when (consp rep) (return-from box-rep-code nil))
  (ecase rep
    (:eqref nil)
    (:f64 '((:struct.new :double)))
    (:f32 '((:struct.new :single)))
    (:c64 '((:struct.new :complex-d)))
    (:c32 '((:struct.new :complex-s)))
    (:i32 (if (or (null ty) (ty-fixnum-p ty)) '(:ref.i31) '((:call :box-i32))))
    (:i64 '((:call :i64-int)))))
(defun unbox-rep-code (rep &optional ty)
  (when (consp rep)
    (return-from unbox-rep-code `((:ref.cast ,(if (eq (first rep) :md2) :mdarray (second rep))))))
  (ecase rep
    (:eqref nil)
    (:f64 '((:ref.cast! :double) (:struct.get :double 0)))
    (:f32 '((:ref.cast! :single) (:struct.get :single 0)))
    ((:c64 :c32)
     (let* ((st (if (eq rep :c64) :complex-d :complex-s)) (tmp (new-local `(:ref :null ,st))))
       `((:ref.cast ,st) (:local.tee ,tmp) (:struct.get ,st 0) (:local.get ,tmp) (:struct.get ,st 1))))
    ;; (an integer of unknown size: a fixnum inline, the helper for a bignum)
    (:i32 (if (or (null ty) (ty-fixnum-p ty))
              '((:ref.cast! :i31) :i31.get_s)
              (let ((tmp (new-local)))
                `((:local.tee ,tmp) (:ref.test :i31)
                  (:if nil :i32 ((:local.get ,tmp) (:ref.cast! :i31) :i31.get_s)
                       ((:local.get ,tmp) (:call :int-i64) :i32.wrap_i64))))))
    (:i64 (if (and ty (ty-fixnum-p ty))
              '((:ref.cast! :i31) :i31.get_s :i64.extend_i32_s)
              (let ((tmp (new-local)))
                `((:local.tee ,tmp) (:ref.test :i31)
                  (:if nil :i64 ((:local.get ,tmp) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s)
                       ((:local.get ,tmp) (:call :int-i64)))))))))

(defun unboxed-local-p (lv)
  "LV is held in a local of the function being compiled in its representation."
  (and (lvar-rep lv) (not (boxed-p lv)) (eq (car (get-loc lv)) :local)))

(defun var-raw-code (lv)
  "Push the raw content of a variable's location (cell if boxed)."
  (let ((loc (get-loc lv)))
    (ecase (car loc)
      (:local (cond ((dual-local lv) (dual-get-code lv)) ; an integer accumulator (typed.lisp)
                    ((fdual-local lv) (fdual-materialize-code (cdr loc) (fdual-local lv)))
                    (t
                  `(,@(rep-get-code (lvar-rep* lv) (cdr loc)) ,@(box-rep-code (lvar-rep* lv) (lvar-type lv))))))
      (:env (env-get-code (cdr loc)))
      ;; a captured and assigned variable's cell, in a local of its type
      (:cell `((:local.get ,(cdr loc)))))))

(defun env-get-code (i)
  (let ((type (fstate-env-type *fs*)))
    (if type
        `((:local.get ,(fstate-env-local *fs*)) (:struct.get ,type ,(+ +clo-env+ i)))
        `((:local.get ,(fstate-env-local *fs*)) (:i32.const ,i) (:array.get :vector)))))

(defun var-get-code (lv)
  (cond ((not (boxed-p lv)) (var-raw-code lv))
        ((eq (car (get-loc lv)) :cell) `((:local.get ,(cdr (get-loc lv))) (:struct.get :cell 0)))
        (t `(,@(var-raw-code lv) (:ref.cast :cell) (:struct.get :cell 0)))))

(defun var-set-code (lv value-code)
  "Store; leaves nothing on the stack."
  (let ((loc (get-loc lv)))
    (cond ((and (boxed-p lv) (eq (car loc) :cell))
           `((:local.get ,(cdr loc)) ,@value-code (:struct.set :cell 0)))
          ((boxed-p lv)
           `(,@(var-raw-code lv) (:ref.cast :cell) ,@value-code (:struct.set :cell 0)))
          ((eq (car loc) :local) `(,@value-code ,@(unbox-rep-code (lvar-rep* lv) (lvar-type lv))
                                    ,@(rep-local-set-code lv (cdr loc))))
          ((fstate-env-type *fs*)
           `((:local.get ,(fstate-env-local *fs*)) ,@value-code
             (:struct.set ,(fstate-env-type *fs*) ,(+ +clo-env+ (cdr loc)))))
          (t `((:local.get ,(fstate-env-local *fs*)) (:i32.const ,(cdr loc)) ,@value-code
               (:array.set :vector))))))

(defvar *binding-init* nil "The node of the value BIND-NEW-VAR-CODE binds, when known.")

(defun bind-new-var-code (lv value-code)
  "Initialize a fresh lexical variable (allocating its local)."
  (note-dual-var lv)                    ; integer accumulators (typed.lisp)
  (when (and (lvar-rep lv) (not (boxed-p lv)))
    (let ((idx (new-rep-local (lvar-rep lv))))
      (set-loc lv (cons :local idx))
      (return-from bind-new-var-code `(,@value-code ,@(unbox-rep-code (lvar-rep lv) (lvar-type lv))
                                       ,@(rep-local-set-code lv idx)))))
  (if (boxed-p lv)
      (let ((idx (new-local '(:ref :null :cell))))
        (set-loc lv (cons :cell idx))
        `(,@value-code (:struct.new :cell) (:local.set ,idx)))
      (let ((idx (new-local)))
        (set-loc lv (cons :local idx))
        `(,@value-code (:local.set ,idx) ,@(struct-shadow-init-code lv idx)))))

(defun mv-one () '((:i32.const 1) (:global.set :mv-count)))

;;; Declared parameters.  The type declared for a parameter is that of the
;;; variable from the start of the function's body (type inference, and so
;;; its representation: an i32, an f64, a vector of a kind of storage),
;;; with no code making it so: a value of another type passed to it trapped
;;; at the cast of the prologue or of the code using it.  At SAFETY 1 and
;;; more the prologue tests it (DECL-CHECK-CODE), as SBCL does: one test of
;;; the object (a fixnum with its range, a double, a vector of the storage
;;; kind...), a TYPE-ERROR for anything else.
(defun decl-check-ty (spec)
  "The type the declaration of a variable of type SPEC gives it, when the
code may rely on it so that a value of another type would trap; else NIL
(a structure class, whose accessors test the object: STRUCT-SHADOW-INIT-CODE;
a type of objects the code handles generically)."
  (let ((ty (ignore-errors (declared-ty spec))))
    (and ty (not (ty-bottom-p ty)) (/= (ty-bits ty) +k-all+)
         (cond ((and (ty-cls ty) (symbolp (ty-cls ty)) (struct-class-type-p (ty-cls ty))) nil)
               ((ty-array-kind ty) ty)
               ((logtest (ty-bits ty) (logior +k-other+ +k-limbs+)) nil)
               (t ty)))))

(defun decl-test-code (l ty)
  "Code leaving 1 when the object in the local L is of the type TY (from
DECL-CHECK-TY), else 0; second value, whether 0 means it is not (else it
may be: a bignum beyond 64 bits within the range, an array of another rank)."
  (let ((tests nil) (exact t) (bits (ty-bits ty)))
    (flet ((add (code) (push code tests))
           (obj () `(:local.get ,l)))
      (when (logtest bits +k-int+)
        (let* ((lo (ty-lo ty)) (hi (ty-hi ty))
               (flo (if lo (max lo +most-negative-fixnum+) +most-negative-fixnum+))
               (fhi (if hi (min hi +most-positive-fixnum+) +most-positive-fixnum+))
               (big (or (null lo) (null hi) (< lo +most-negative-fixnum+) (> hi +most-positive-fixnum+)))
               (v (new-local :i32)))
          ;; (an i31 is a fixnum or a character, 2^29 + its code: beyond the fixnums)
          (add `(,(obj) (:ref.test :i31)
                 (:if nil :i32
                      ,(cond ((and (= flo +most-negative-fixnum+) (= fhi +most-positive-fixnum+))
                              `(,(obj) (:ref.cast :i31) :i31.get_s (:local.set ,v) ,@(fixnum-test-instrs v)))
                             ((> flo fhi) '((:i32.const 0)))
                             (t `(,(obj) (:ref.cast :i31) :i31.get_s (:local.tee ,v) (:i32.const ,flo) :i32.ge_s
                                  (:local.get ,v) (:i32.const ,fhi) :i32.le_s :i32.and)))
                      ,(cond ((not big) '((:i32.const 0)))
                             ((and (null lo) (null hi)) `(,(obj) (:ref.test :bignum)))
                             (t
                              ;; a bignum within 64 bits: its value
                              (let ((w (new-local :i64))
                                    (llo (and lo (> lo (- (expt 2 63))) lo))
                                    (lhi (and hi (< hi (1- (expt 2 63))) hi)))
                                ;; (beyond 64 bits: within the range unless it is bounded within them)
                                (when (or (null llo) (null lhi)) (setf exact nil))
                                `(,(obj) (:ref.test :bignum)
                                  (:if nil :i32
                                       (,(obj) (:ref.cast :bignum) (:struct.get :bignum 1) :ref.is_null
                                        (:if nil :i32
                                             (,(obj) (:ref.cast :bignum) (:struct.get :bignum 2) (:local.set ,w)
                                              ,@(if llo `((:local.get ,w) (:i64.const ,llo) :i64.ge_s) '((:i32.const 1)))
                                              ,@(if lhi `((:local.get ,w) (:i64.const ,lhi) :i64.le_s) '((:i32.const 1)))
                                              :i32.and)
                                             ((:i32.const 0))))
                                       ((:i32.const 0))))))))))))
      (when (logtest bits +k-ratio+) (add `(,(obj) (:ref.test :ratio))))
      (when (logtest bits +k-single+) (add `(,(obj) (:ref.test :single))))
      (when (logtest bits +k-double+)
        (unless (= (logand bits +k-double+) +k-double+) (setf exact nil))
        (add `(,(obj) (:ref.test :double))))
      (when (logtest bits +k-complex+) (add `(,(obj) (:ref.test :complex))))
      (when (logtest bits +k-cdouble+) (add `(,(obj) (:ref.test :complex-d))))
      (when (logtest bits +k-csingle+) (add `(,(obj) (:ref.test :complex-s))))
      (when (logtest bits +k-char+)
        (let ((v (new-local :i32)))
          (add `(,(obj) (:ref.test :i31)
                 (:if nil :i32 (,(obj) (:ref.cast :i31) :i31.get_s (:local.set ,v)
                                (:local.get ,v) (:i32.const ,+char-tag+) :i32.ge_s)
                      ((:i32.const 0)))))))
      (cond ((= (logand bits +k-bool+) +k-bool+) (add `(,(obj) (:ref.test :symbol))))
            ((logtest bits +k-null+) (add `(,(obj) ,@(nil-code) :ref.eq)))
            ((logtest bits +k-sym+)
             (add `(,(obj) (:ref.test :symbol) ,(obj) ,@(nil-code) :ref.eq :i32.eqz :i32.and))))
      (when (logtest bits +k-cons+) (add `(,(obj) (:ref.test :cons))))
      (when (logtest bits +k-string+) (add `(,(obj) (:ref.test :string))))
      (when (logtest bits +k-svec+) (add `(,(obj) (:ref.test :vector))))
      (when (logtest bits +k-func+) (add `(,(obj) (:ref.test :closure))))
      (when (logtest bits +k-other+)
        (let ((kind (ty-vec-kind ty)))
          (if (and kind (not (member kind '(:vector :string))))
              (add `(,(obj) (:ref.test ,kind)))
              (setf exact nil))))
      (values (if tests
                  (reduce (lambda (a b) `(,@a (:if nil :i32 ((:i32.const 1)) ,b)))
                          (nreverse tests))
                  '((:i32.const 0)))
              exact))))

(defun decl-check-code (l spec)
  "At SAFETY 1 and more (in code that is not the runtime's): the test that
the object in the local L, given to a variable declared of type SPEC, is of
it, signalling a TYPE-ERROR when it is not; else NIL."
  (let ((ty (and (not (eql (cg-safety) 0))
                 (fstate-lam *fs*) (lam-p (fstate-lam *fs*)) (lam-safety (fstate-lam *fs*))
                 (decl-check-ty spec))))
    (when ty
      (multiple-value-bind (test exact) (decl-test-code l ty)
        `(,@test :i32.eqz
          (:if nil nil
               ((:local.get ,l) ,@(const-init-code spec)
                ,@(if exact
                      `((:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2)) :unreachable)
                      `((:global.get ,(sym-global 'clysm-sys::%check-declared-type)) (:call (:callsym 2)) :drop)))))))))

(defun checked-param-value-code (lv value-code)
  "VALUE-CODE, the object given to the parameter LV, with its test (PARAM-CHECK-CODE)."
  (if (and (lvar-decl lv) (decl-check-ty (lvar-decl lv)))
      (let* ((tmp (new-local)) (check (param-check-code lv tmp)))
        (if check
            `(,@value-code (:local.set ,tmp) ,@check (:local.get ,tmp))
            value-code))
      value-code))

(defun param-check-code (lv local-idx)
  "The test of the object passed in LOCAL-IDX for the declared parameter LV, or NIL."
  (and (lvar-decl lv) (not (lvar-special lv)) (not (member lv *proven-params*))
       (decl-check-code local-idx (lvar-decl lv))))

(defun struct-slot-check-code (node)
  "After the code of NODE, a read of a structure slot declared a simple
array of one kind of storage, whose value the inferred types take to be
one (STRUCT-SLOT-NODE-TY): the test of its storage kind, a TYPE-ERROR when
it is another (a slot written by code that did not check it), so that the
casts of the code using it never trap.  Nothing at (SAFETY 0)."
  (multiple-value-bind (kind spec) (struct-slot-array-kind node)
    (when (and kind (not (eql (cg-safety) 0)))
      (let ((tmp (new-local)))
        `((:local.tee ,tmp) (:ref.test ,kind)
          (:if nil :eqref ((:local.get ,tmp))
               ((:local.get ,tmp) ,@(const-init-code spec)
                (:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2)) :unreachable)))))))

(defun cg-safety ()
  "The SAFETY of the function being compiled (its LAM-SAFETY; 1 by default)."
  (let ((lam (and *fs* (fstate-lam *fs*))))
    (or (and (lam-p lam) (lam-safety lam)) 1)))

(defparameter *i64-arith-ops* '(+ - * 1+ 1- ash logand logior logxor lognot abs)
  "Operations computed in an i64 when their operands and result are within 64 bits.")

(defun trusted-fixnum-the-p (node)
  "NODE is a (THE type X) converted at (SAFETY 0) whose type is a fixnum,
with X integer arithmetic within 64 bits not known to give a fixnum: its
value is X's wrapped to 32 bits (TRUSTED-FIXNUM-CODE)."
  (and (eq (fourth node) :trust)
       (ty-fixnum-p (node-ty node))
       (let ((x (third node)))
         (and (not (ty-fixnum-p (node-ty x))) (ty-int64-p (node-ty x))
              (member (car x) '(:prim :call)) (member (node-op-name x) *i64-arith-ops*)))))

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
                   `(,@(cg-rep (third node) rep) ,@(rep-local-set-code (second node) (cdr (get-loc (second node))) t)
                     ,@(box-rep-code rep (lvar-type (second node))) ,@(when mv (mv-one))))
               (let ((tmp (new-local)) (dual (dual-lset-code node nil)))
                 (when dual (return-from cg `(,@dual ,@(when mv (mv-one)))))
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
      (:prim `(,@(or (typed-cg node mv) (cg-prim (second node) (cddr node) mv)) ,@(struct-slot-check-code node)))
      ((:flet :labels) (cg-flet node mv tail))
      (:block (cg-block node mv tail))
      (:return-from (cg-return-from node))
      (:tagbody (sref-md2-wrap node (lambda () (cg-tagbody node mv))))
      (:go (cg-go node))
      (:catch (cg-catch node mv))
      (:throw (cg-throw node))
      (:unwind-protect (cg-unwind-protect node mv))
      (:mvcall (cg-mvcall node mv tail))
      (:mvprog1 (cg-mvprog1 node mv))
      (:the (if (trusted-fixnum-the-p node)
                ;; (SAFETY 0): an integer declared a fixnum is one (wrapped to 32 bits)
                `(,@(cg-rep node :i32) :ref.i31 ,@(when mv (mv-one)))
                (cg (third node) mv tail)))
      (:ltv (cg-ltv node mv))
      (:progv (cg-progv node mv))
      (:wasm (cg-wasm node mv))
      (:wasm-test (append (cg-test node) (bool-code) (when mv (mv-one))))
      (:symtab `((:global.get :symtab) (:global.get :pkgtab) (:call :symbol-info) (:call :dropped-symbols)
                 (:struct.new :cons) (:struct.new :cons) (:struct.new :cons) ,@(when mv (mv-one))))
      (:defun (cg-defun node mv))
      (:values (cg-values (cdr node) mv))
      (:apply (maybe-checked-call tail (cg-apply (second node) (cddr node) tail))))))

(defun live-apply-code (fnode args tail)
  "In the image, (APPLY 'F SPREAD... LIST) of a global function that has
(or that this compilation defines with) required parameters and &REST, as
many as there are spread arguments or fewer: when the symbol's function is
still one with that rest layout, its main function is called with them and
the rest list, as for a known function; else the generic way."
  (let* ((sym (and (member (car fnode) '(:fref :const)) (second fnode)))
         (layout (and sym (symbolp sym) sym *live-key-layout-hook*
                      (not (gethash sym *notinline*)) (not (member sym *local-notinline*))
                      (or (funcall *live-key-layout-hook* sym)
                          (gethash sym *compiled-key-layouts*))))
         (nreq (rest-layout-nreq layout))
         (spread (butlast args)))
    (when (and nreq (<= nreq (length spread)))
      (let* ((temps (loop for nil in args collect (new-local)))
             (f (new-local '(:ref :null :closure)))
             (type (main-type (1+ nreq)))
             (call (if tail :return_call_ref :call_ref))
             (slow (gensym "GENERIC")) (done (gensym "ACALL")))
        `(,@(loop for a in args for tmp in temps append `(,@(cg a) (:local.set ,tmp)))
          (:block ,done :eqref
            (:block ,slow nil
              (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-function+) (:local.tee ,f)
              :ref.is_null (:br_if ,slow)
              (:local.get ,f) ,@(clo-field-code +clo-klayout+)
              ,@(const-init-code layout) :ref.eq :i32.eqz (:br_if ,slow)
              (:local.get ,f) :ref.as_non_null
              ,@(loop for tmp in (subseq temps 0 nreq) collect `(:local.get ,tmp))
              ,@(loop for tmp in (nthcdr nreq temps) collect `(:local.get ,tmp))
              ,@(loop repeat (- (length temps) nreq 1) collect '(:struct.new :cons))
              (:local.get ,f) ,@(clo-field-code +clo-kmain+) (:ref.cast! ,type)
              (,call ,type)
              (:br ,done))
            (:global.get ,(sym-global sym)) (:call :fdefinition)
            ,@(loop for tmp in temps collect `(:local.get ,tmp))
            ,@(loop repeat (1- (length temps)) collect '(:struct.new :cons))
            (,(if tail :return_call :call) :apply-list)))))))

(defun cg-apply (fnode args tail)
  "(APPLY F SPREAD... LIST): the entry of F taking a list, called directly
when F is a known global function; a known function of required parameters
and &REST that the spread arguments fill takes them directly and only the
others are consed onto the list for its rest list."
  (let* ((call (if tail :return_call :call))
         (spread (butlast args))
         (lam (and (eq (car fnode) :fref)
                   (not (gethash (second fnode) *dynamic-functions*))
                   (known-global-lam (second fnode))))
         (nreq (and lam (length (lam-req lam)))))
    (flet ((list*-code (nodes)
             ;; the values of NODES consed onto the last one
             `(,@(loop for n in nodes append (cg n))
               ,@(loop repeat (1- (length nodes)) collect '(:struct.new :cons)))))
      (cond ((and lam (lam-rest-only-p lam) (<= nreq (length spread)))
             (ensure-lam-compiled lam)
             `((:global.get ,(static-closure-global lam))
               ,@(loop for n in (subseq spread 0 nreq) append (cg n))
               ,@(list*-code (nthcdr nreq args))
               (,call (:rmain ,(lam-id lam)))))
            (lam
             (ensure-lam-compiled lam)
             `((:global.get ,(static-closure-global lam)) ,@(list*-code args)
               (,call ,(lam-apply-entry-name lam))))
            ((live-apply-code fnode args tail))
            (t `(,@(cg fnode) ,@(list*-code args) (,call :apply-list)))))))

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

(defvar *fixnum-known-codes* nil
  "The operand codes (the lists themselves) given to a primitive that push a
value known to be a fixnum: its i32 followed by :REF.I31 (CG-PRIM, CG-TEST).")

(defun cg-test (node)
  "Instructions leaving an i32 (0 = false)."
  (when (assumed-guard-p node) (return-from cg-test '((:i32.const 1))))
  (let ((h (hoisted-guard-local node)))
    (when h (return-from cg-test `((:local.get ,h)))))
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
                 (multiple-value-bind (codes known) (prim-arg-codes p (cddr node))
                   (let ((*fixnum-known-codes* known)) (funcall (prim-fn p) codes)))
                 `(,@(cg node) ,@(nil-code) :ref.eq :i32.eqz))))
    (:wasm-test `(,@(loop for a in (second node) append (cg a))
                  ,@(resolve-wasm-instrs (third node))))
    (:if `(,@(cg-test (second node))
           ,@(if-branch-hint (third node) (fourth node) t)
           (:if nil :i32 ,(cg-test (third node)) ,(cg-test (fourth node)))))
    (:progn (if (cdr node)
                `(,@(loop for n in (butlast (cdr node)) append `(,@(cg n) :drop))
                  ,@(cg-test (car (last node))))
                '((:i32.const 0))))
    ;; a LET binding no special variable (a chained comparison (< a b c)
    ;; binds its arguments): the test of its body
    (:let (if (notany (lambda (b) (lvar-special (car b))) (second node))
              (cg-let node nil nil :test)
              `(,@(cg node) ,@(nil-code) :ref.eq :i32.eqz)))
    (t `(,@(cg node) ,@(nil-code) :ref.eq :i32.eqz))))

(defun or-let-p (node)
  "NODE is (let ((g a)) (if g g b)), as OR expands."
  (and (eq (car node) :let) (= (length (second node)) 1)
       (let ((lv (car (first (second node)))) (body (third node)))
         (and (not (lvar-special lv)) (eq (car body) :if)
              (eq (car (second body)) :lref) (eq (second (second body)) lv)
              (eq (car (third body)) :lref) (eq (second (third body)) lv)))))

(defparameter *cold-functions*
  '(error cerror warn signal break invoke-debugger
    clysm-sys::%type-error clysm-sys::%signal-type-error clysm-sys::%assert-error
    clysm-sys::%check-type-error)
  "Functions whose calls are not expected to happen often (they signal
errors, warnings or conditions): an IF of which one branch calls one is
hinted the other way, so that V8 makes that branch a deferred block,
spilling what lives across the call there only.")

(defvar *cold-cache* (make-hash-table :test 'eq)
  "Node -> :YES / :NO, what COLD-BRANCH-P found (each node walked once: the
branches of nested IFs are asked again).")

(defun cold-function-p (sym)
  (and (symbolp sym)
       (or (member sym *cold-functions*) (gethash sym *ti-noreturn*)
           ;; a function of the file that never returns (an error helper)
           (let ((l (known-global-lam sym))) (and l (lam-noreturn-p l))))
       t))

(defun cold-branch-p (node)
  "NODE, a branch of an IF, signals (calls a function of COLD-FUNCTION-P) or
is marked with %COLD on every way through it: the branch is not expected to
be taken.  An IF counts when its test does or both its branches do; any
other node when one of its parts does (not exact for blocks and loops,
but close)."
  (when (> (hash-table-count *cold-cache*) 200000) (clrhash *cold-cache*))
  (labels ((cold (n)
             (and (consp n)
                  (let ((c (gethash n *cold-cache*)))
                    (if c
                        (eq c :yes)
                        (let ((r (case (car n)
                                   (:call (or (cold-function-p (second n)) (some-sub n)))
                                   (:prim (or (eq (prim-name (second n)) 'clysm-sys::%cold) (some-sub n)))
                                   (:if (or (cold (second n)) (and (cold (third n)) (cold (fourth n)))))
                                   (t (some-sub n)))))
                          (setf (gethash n *cold-cache*) (if r :yes :no))
                          r)))))
           (some-sub (n)
             (block sub
               (map-subnodes (lambda (x) (when (cold x) (return-from sub t))) n)
               nil)))
    (cold node)))

(defvar *recurse-cache* (make-hash-table :test 'eq)
  "Node -> what ARM-RECURSES found (:EXIT, :CALL or :NONE).")

(defun arm-recurses (node lam)
  "What NODE, a branch of an IF in LAM, does: :EXIT when it may leave the
code around it (a RETURN-FROM, a GO, a THROW), else :CALL when it calls LAM
itself (by its global name or as a local function), else NIL."
  (when (> (hash-table-count *recurse-cache*) 200000) (clrhash *recurse-cache*))
  (labels ((walk (n)
             (if (not (consp n))
                 :none
                 (or (gethash n *recurse-cache*)
                     (setf (gethash n *recurse-cache*)
                           (case (car n)
                             ((:return-from :go :throw) :exit)
                             (:lambda :none)
                             (t (let ((r (if (or (and (eq (car n) :lcall) (eq (fun-lam (second n)) lam))
                                                 (and (eq (car n) :call) (lam-name lam) (eq (second n) (lam-name lam))))
                                            :call :none)))
                                  (map-subnodes (lambda (x)
                                                  (let ((v (walk x)))
                                                    (cond ((eq v :exit) (setf r :exit))
                                                          ((and (eq v :call) (eq r :none)) (setf r :call)))))
                                                n)
                                  r))))))))
    (let ((v (walk node))) (if (eq v :none) nil v))))

(defun if-branch-hint (a b &optional test)
  "The branch hint of an IF whose branches are A and B: a cold one is not
expected to be taken, so that the engine lays it out of the way of the other.
In a loop, a branch that calls the function itself is not expected either
when the other does not and does not leave the loop: a search that tries
each move and recurses on the few that apply (TRIANGLE, QUEENS).  The
engine then makes the branch a deferred block and keeps
the loop's values in registers on the way without the call and spills them
around the call, where the call costs more than the spills.  (Any call
taken as unlikely, the call heuristic of static branch prediction, was
measured too: it misses the method call of a site cache whose other branch
is a slot's writer, 20% slower.)  Not for an IF that is part of a TEST (an
OR, an AND): there the call decides whether the loop goes on, as in (DO
(...) ((OR (= J 36) (TRY J)))), and is expected."
  (cond ((cold-branch-p a) '((:hint 0)))
        ((cold-branch-p b) '((:hint 1)))
        ((and *cg-loop* (not test) *fs* (lam-p (fstate-lam *fs*)))
         (let* ((lam (fstate-lam *fs*)) (ca (arm-recurses a lam)) (cb (arm-recurses b lam)))
           (cond ((and (eq ca :call) (null cb)) '((:hint 0)))
                 ((and (eq cb :call) (null ca)) '((:hint 1))))))))

;;; A test that is itself an IF (COND, AND, %ENDP's (cond ((consp x) nil)
;;; ((null x) t) (t error))), an OR or a NOT of one is compiled as branches
;;; to the two arms instead of an i32 computed by nested IFs and tested
;;; again: each leaf test ends in a BR_IF to the arm (or to the next test),
;;; so that the engine sees the REF.TEST of a leaf as the condition of the
;;; branch into the arm, and knows the type of the value there (the cast of
;;; the CAR after (%ENDP L) goes away), and no i32 is materialized.
(defun compound-test-p (c)
  (and (not (assumed-guard-p c)) (not (hoisted-guard-local c))
       (case (car c)
         (:if t)
         (:let (or-let-p c))
         (:prim (and (member (prim-name (second c)) '(not null)) (compound-test-p (third c)))))))

(defun effect-branch-test-p (c)
  "C, the test of an IF for effect, is compiled by BRANCH-CODE: a test of
COMPOUND-TEST-P whose own first test is one too (an OR of %ENDPs, as in
MAPC over two lists). The end test of a loop over one list, (%ENDP L),
keeps the IF of an i32: V8 peels the first iteration of such a loop when
it is compiled as branches, which made short loops around a call slower
(b-deriv 1.08 times its time)."
  (and (compound-test-p c)
       (case (car c)
         (:let t)
         (:if (compound-test-p (second c)))
         (t (effect-branch-test-p (third c))))))

(defun flip-hint (hint)
  (cond ((equal hint '((:hint 0))) '((:hint 1)))
        ((equal hint '((:hint 1))) '((:hint 0)))))

(defun leaf-branch-test (test)
  "TEST, an i32 for a BR_IF, without a final pair of I32.EQZ."
  (let ((n (length test)))
    (if (and (>= n 3) (eq (nth (- n 1) test) :i32.eqz) (eq (nth (- n 2) test) :i32.eqz))
        (butlast test 2)
        test)))

(defun branch-code (node then else &optional hint)
  "Instructions that branch to the label THEN when the test NODE is true and
to the label ELSE when it is false. Either may be :NEXT instead, the code
that follows (they fall through to it). HINT, ((:HINT 0|1)) or NIL, is how
likely THEN is."
  (cond ((not (compound-test-p node))
         (let ((test (cg-test node)))
           (flet ((to (l) (unless (eq l :next) `((:br ,l)))))
             (cond ((equal test '((:i32.const 1))) (to then))
                   ((equal test '((:i32.const 0))) (to else))
                   ((eq else :next) `(,@(leaf-branch-test test) ,@hint (:br_if ,then)))
                   ((eq then :next) `(,@(leaf-branch-test `(,@test :i32.eqz)) ,@(flip-hint hint) (:br_if ,else)))
                   (t `(,@(leaf-branch-test test) ,@hint (:br_if ,then) (:br ,else)))))))
        ((eq (car node) :prim) (branch-code (third node) else then (flip-hint hint)))
        ((eq (car node) :let)           ; (or x y): (if x t y)
         (branch-if-code (cdr (first (second node))) '(:const t) (fourth (third node)) then else hint))
        (t (destructuring-bind (c a b) (cdr node) (branch-if-code c a b then else hint)))))

(defun branch-if-code (c a b then else hint)
  "BRANCH-CODE of the test (IF C A B)."
  (flet ((target (n) (when (eq (car n) :const) (if (second n) then else))))
    (let ((ta (target a)) (tb (target b)) (h (if-branch-hint a b t)))
      (cond
        ;; an arm that is a constant: C branches to its target, the other arm follows
        (ta (if (eq ta :next)
                (let ((lx (gensym "NEXT")))
                  `((:block ,lx nil ,@(branch-code c lx :next h) ,@(branch-code b then else hint))))
                `(,@(branch-code c ta :next h) ,@(branch-code b then else hint))))
        (tb (if (eq tb :next)
                (let ((lx (gensym "NEXT")))
                  `((:block ,lx nil ,@(branch-code c :next lx h) ,@(branch-code a then else hint))))
                `(,@(branch-code c :next tb h) ,@(branch-code a then else hint))))
        ;; C falls through into A; A must not fall through (into B), so a
        ;; :NEXT of A is the end of a block around the whole
        (t (let* ((lb (gensym "ELSE"))
                  (lx (and (or (eq then :next) (eq else :next)) (gensym "NEXT")))
                  (code `((:block ,lb nil ,@(branch-code c :next lb h)
                            ,@(branch-code a (if (eq then :next) lx then) (if (eq else :next) lx else) hint))
                          ,@(branch-code b then else hint))))
             (if lx `((:block ,lx nil ,@code)) code)))))))

(defun cg-if (node mv tail)
  (destructuring-bind (c a b) (cdr node)
    ;; (a guard taken as true in a loop's version: the copy only)
    (when (assumed-guard-p c) (return-from cg-if (cg a mv tail)))
    (when (compound-test-p c)
      (return-from cg-if
        (let ((lf (gensym "ELSE")) (end (gensym "IF")))
          `((:block ,end :eqref
              (:block ,lf nil
                ,@(branch-code c :next lf (if-branch-hint a b))
                ,@(cg a mv tail) (:br ,end))
              ,@(cg b mv tail) ,@(guard-else-epilogue c))))))
    (let ((test (cg-test c)))
      ;; a test the types decide (a version of a loop for a fixnum count): one branch
      (cond ((equal test '((:i32.const 1))) (return-from cg-if (cg a mv tail)))
            ((equal test '((:i32.const 0)))
             (return-from cg-if `(,@(cg b mv tail) ,@(guard-else-epilogue c)))))
      `(,@test
        ;; the guard of a copied function body is expected to hold; a branch
        ;; that never returns (an error) or is marked cold is not expected to
        ;; be taken, so that the engine lays it out of the way of the other
        ,@(cond ((and (eq (car c) :prim) (member (prim-name (second c)) '(clysm-sys::%function-cell-eq clysm-sys::%fasl-inline-guard clysm-sys::%defun-guard)))
                 '((:hint 1)))
                (t (if-branch-hint a b)))
        (:if nil :eqref ,(cg a mv tail) (,@(cg b mv tail) ,@(guard-else-epilogue c)))))))

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
                 `(,@(cg-rep (third node) (lvar-rep lv)) ,@(rep-local-set-code lv (cdr (get-loc lv))))
                 (or (dual-lset-code node t)
                     (var-set-code lv (cg (third node)))))))
    (:progn (loop for n in (cdr node) append (cg-effect n)))
    (:if (when (effect-branch-test-p (second node))
           (return-from cg-effect
             (let* ((lf (gensym "ELSE")) (end (gensym "IF"))
                    (b (cg-effect (fourth node)))
                    (a `(:block ,lf nil
                          ,@(branch-code (second node) :next lf (if-branch-hint (third node) (fourth node)))
                          ,@(cg-effect (third node)) ,@(when b `((:br ,end))))))
               (if b `((:block ,end nil ,a ,@b)) (list a)))))
         (let ((test (cg-test (second node))))
           (cond ((equal test '((:i32.const 1))) (cg-effect (third node)))
                 ((equal test '((:i32.const 0))) (cg-effect (fourth node)))
                 (t `(,@test ,@(if-branch-hint (third node) (fourth node))
                      (:if nil nil ,(cg-effect (third node)) ,(cg-effect (fourth node))))))))
    (:let (cg-let node nil nil :effect))
    (:mvbind (cg-mvbind node nil nil t))
    ((:prim :call)
     (or (and (member (node-op-name node) '(clysm-sys::%aset1 clysm-sys::%svset clysm-sys::%row-major-aset
                                            clysm-sys::%aset2 clysm-sys::%charset clysm-sys::%scharset))
              (typed-store-code node t))
         (and (eq (node-op-name node) 'clysm-sys::%struct-tset) (tstruct-set-code node t))
         `(,@(cg node) :drop)))
    (t `(,@(cg node) :drop))))

;;; ------------------------------------------------------------
;;; Dynamic binding helpers
;;; ------------------------------------------------------------

(defun bsp-code () '((:global.get :dyn) (:struct.get :dynstate 0)))
(defun bstack-code () '((:global.get :dyn) (:struct.get :dynstate 1)))
(defun set-bsp-code (value-code) `((:global.get :dyn) ,@value-code (:struct.set :dynstate 0)))
(defun save-bsp-code (local) `(,@(bsp-code) (:local.set ,local)))
(defun restore-bsp-code (local) `((:local.get ,local) (:call :unbind-to)))

;;; Binding special variables whose symbols are known: the entries
;;; (symbol, old value) are written to the binding stack inline, with one
;;; check of its size.  At the normal exit of the form the old values are
;;; stored back from the known positions.  (Non-local exits use :UNBIND-TO,
;;; which reads the symbols from the stack.)
(defparameter *inline-bind-max* 6 "more variables than this are bound by calls of :BIND")

(defun bind-specials-code (saved bindings)
  "Save the binding stack pointer in the local SAVED and bind the symbols of
BINDINGS, a list of (symbol . local holding the new value), in order."
  (let ((n (length bindings)))
    (if (or (zerop n) (> n *inline-bind-max*))
        `(,@(save-bsp-code saved)
          ,@(loop for (sym . tmp) in bindings
                  append `((:global.get ,(sym-global sym)) (:local.get ,tmp) (:call :bind))))
        (let ((st (new-local '(:ref :null :vector))))
          `(,@(bsp-code) (:local.tee ,saved) (:i32.const ,(* 2 n)) :i32.add
            ,@(bstack-code) :array.len :i32.gt_u
            (:if nil nil ((:call :grow-bstack)))
            ,@(bstack-code) (:local.set ,st)
            ,@(loop for (sym . tmp) in bindings for i from 0 by 2
                    ;; (the symbol is stored only when its slot holds
                    ;; another: a binding in a loop finds it there, and a
                    ;; store of an object made after the engine's last
                    ;; full collection, such as a symbol of code just
                    ;; loaded, into the old binding stack costs a slow
                    ;; write barrier: b-special in a script 2 times as long)
                    append `((:local.get ,st) (:local.get ,saved) ,@(unless (zerop i) `((:i32.const ,i) :i32.add))
                             (:array.get :vector) (:global.get ,(sym-global sym)) :ref.eq
                             (:if nil nil nil
                              ((:local.get ,st) (:local.get ,saved) ,@(unless (zerop i) `((:i32.const ,i) :i32.add))
                               (:global.get ,(sym-global sym)) (:array.set :vector)))
                             ;; (so is the old value)
                             (:local.get ,st) (:local.get ,saved) (:i32.const ,(1+ i)) :i32.add (:array.get :vector)
                             (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-value+) :ref.eq
                             (:if nil nil nil
                              ((:local.get ,st) (:local.get ,saved) (:i32.const ,(1+ i)) :i32.add
                               (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-value+) (:array.set :vector)))
                             (:global.get ,(sym-global sym)) (:local.get ,tmp) (:struct.set :symbol ,+sym-value+)))
            ,@(set-bsp-code `((:local.get ,saved) (:i32.const ,(* 2 n)) :i32.add)))))))

(defun unbind-specials-code (saved symbols)
  "Undo, at the normal exit, the bindings of SYMBOLS made by BIND-SPECIALS-CODE."
  (let ((n (length symbols)))
    (if (or (zerop n) (> n *inline-bind-max*))
        (restore-bsp-code saved)
        (let ((st (new-local '(:ref :null :vector))))
          `(,@(bstack-code) (:local.set ,st)
            ,@(loop for sym in (reverse symbols) for i downfrom (1- (* 2 n)) by 2
                    append `((:global.get ,(sym-global sym))
                             (:local.get ,st) (:local.get ,saved) (:i32.const ,i) :i32.add (:array.get :vector)
                             (:struct.set :symbol ,+sym-value+)))
            ,@(set-bsp-code `((:local.get ,saved))))))))

(defmacro with-dynamic-scope (() &body body)
  `(let ((*dyn-depth* (1+ *dyn-depth*)) (*tail-ok* nil)) ,@body))

;;; ------------------------------------------------------------
;;; LET
;;; ------------------------------------------------------------

(defun cg-let (node mv tail &optional (rep :eqref))
  "REP: :eqref (the value as usual), :f64 / :i32 (the value in that
representation), :effect (no value) or :test (an i32 that is 0 for false,
as CG-TEST; only without special variables)."
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
                ((md2-version-alias-src lv init)
                 ;; (bound to the array of a let's version: its locals)
                 (let ((src (md2-version-alias-src lv init)))
                   (push lv (car *md2-version-vars*))
                   (setf (lvar-rep lv) (lvar-rep src))
                   (set-loc lv (get-loc src))
                   (setf (gethash lv (fstate-shadows *fs*)) (md2-shadows src))))
                ((md2-alias-src lv init)
                 ;; (bound to an array variable neither of which is assigned: its locals)
                 (let ((src (md2-alias-src lv init)))
                   (set-loc lv (get-loc src))
                   (setf (gethash lv (fstate-shadows *fs*)) (md2-shadows src))))
                ((and (lvar-rep lv) (not (boxed-p lv)))
                 (let ((idx (new-rep-local (lvar-rep lv))))
                   ;; the init is compiled before the variable has a location
                   (setf code (append code (or (md2-copy-code lv idx init)
                                               (append (cg-rep init (lvar-rep lv)) (rep-local-set-code lv idx)))))
                   (set-loc lv (cons :local idx))))
                ((fdual-candidate-p lv init body)
                 ;; a double of unknown type kept unboxed (typed.lisp)
                 (let ((b (new-local)) (f (new-local :f64)))
                   (setf code (append code (fdual-bind-code lv init b f)))
                   (set-loc lv (cons :local b))
                   (remhash lv *dual-vars*)
                   (setf (gethash lv *fdual-vars*) f)))
                ((qdual-candidate-p lv init)
                 ;; the integer quotient of a division of doubles: an i64
                 ;; beside the eqref local, as an integer accumulator's (typed.lisp)
                 (let ((b (new-local)) (v (new-local :i64)))
                   (setf code (append code (values (typed-float-divmod init :pair nil (cons b v)))))
                   (set-loc lv (cons :local b))
                   (remhash lv *fdual-vars*)
                   (setf (gethash lv *dual-vars*) v)))
                (t (remhash lv *fdual-vars*)
                   (setf code (append code (let ((*binding-init* init)) (bind-new-var-code lv (cg init)))))))
          ;; (bound to a special variable whose array is cached: SREF-MD2-WRAP)
          (when (and *sref-md2-caches* (not (lvar-mutated lv)) (not (lvar-special lv)) (null (lvar-rep lv))
                     (let ((a (strip-the init))) (and (eq (car a) :sref) (assoc (second a) *sref-md2-caches*))))
            (push (cons lv (second (strip-the init))) *sref-md2-aliases*))
          ;; a variable that is always a known lambda: FUNCALLs of it are direct calls
          (when (and (not (lvar-mutated lv)) (not (lvar-special lv)))
            (cond ((eq (car init) :lambda)
                   (setf (gethash lv *lvar-lams*) (second init)))
                  ;; (a variable bound to one that is always a known lambda:
                  ;; the parameter of a copy of an inline function given a
                  ;; lambda as its argument)
                  ((and (eq (car init) :lref) (not (lvar-mutated (second init)))
                        (gethash (second init) *lvar-lams*))
                   (setf (gethash lv *lvar-lams*) (gethash (second init) *lvar-lams*)))))))
      (flet ((body-code (tail)
               (cond ((eq rep :eqref) (cg body mv tail))
                     ((eq rep :effect) (cg-effect body))
                     ((eq rep :test) (cg-test body))
                     ;; (:mvinto tmps): the values into locals (CG-MV-INTO)
                     ((and (consp rep) (eq (car rep) :mvinto)) (cg-mv-into body (second rep)))
                     ;; (:dual b f): into a pair of locals (CG-DUAL-F64, typed.lisp)
                     ((and (consp rep) (eq (car rep) :dual)) (cg-dual-f64 body (second rep) (third rep)))
                     (t (cg-rep body rep)))))
        (if (null specials)
            (let ((lv (md2-version-var bindings body rep)))
              (if lv
                  (append code (md2-version-code lv rep #'body-code tail))
                  (append code (body-code tail))))
            (let ((saved (new-local :i32)) (res (unless (or (eq rep :effect) (consp rep)) (new-rep-local rep))))
              (setf specials (loop for (lv . tmp) in (reverse specials) collect (cons (lvar-name lv) tmp)))
              (append code
                      (bind-specials-code saved specials)
                      (with-dynamic-scope () (body-code nil))
                      (when res (rep-set-code rep res))
                      (unbind-specials-code saved (mapcar #'car specials))
                      (when res (rep-get-code rep res)))))))))

;;; MULTIPLE-VALUE-BIND of a form whose values are written in it: an IF,
;;; PROGN or LET whose branches end in VALUES or constants (the NEXT of
;;; WITH-HASH-TABLE-ITERATOR, (if x (values a b) (values c d))).  Each
;;; branch stores its values into the variables' locals, without the values
;;; buffer; a branch of another kind goes through it as before.
(defun mv-into-worth-p (node)
  "NODE ends, in some branch, in a VALUES form written there."
  (case (car node)
    (:values t)
    (:if (or (mv-into-worth-p (third node)) (mv-into-worth-p (fourth node))))
    (:progn (and (cdr node) (mv-into-worth-p (car (last node)))))
    (:let (and (notany (lambda (b) (lvar-special (car b))) (second node))
               (mv-into-worth-p (third node))))
    (t nil)))

(defun cg-mv-into (node tmps)
  "Instructions storing the values of NODE into the eqref locals TMPS (NIL
for the missing ones), leaving nothing."
  (flet ((nils (ts) (loop for tmp in ts append `(,@(nil-code) (:local.set ,tmp)))))
    (case (car node)
      (:values
       (let ((args (cdr node)))
         `(,@(loop for a in args for i from 0
                   append (if (< i (length tmps))
                              `(,@(cg a) (:local.set ,(nth i tmps)))
                              (cg-effect a)))
           ,@(nils (nthcdr (length args) tmps)))))
      (:const `(,@(cg node) (:local.set ,(first tmps)) ,@(nils (rest tmps))))
      (:if
       (destructuring-bind (c a b) (cdr node)
         (if (assumed-guard-p c)
             (cg-mv-into a tmps)
             (let ((test (cg-test c)))
               (cond ((equal test '((:i32.const 1))) (cg-mv-into a tmps))
                     ((equal test '((:i32.const 0))) `(,@(cg-mv-into b tmps) ,@(guard-else-epilogue c)))
                     (t `(,@test
                          (:if nil nil ,(cg-mv-into a tmps)
                               (,@(cg-mv-into b tmps) ,@(guard-else-epilogue c))))))))))
      (:progn
       (if (cdr node)
           `(,@(loop for n in (butlast (cdr node)) append (cg-effect n))
             ,@(cg-mv-into (car (last node)) tmps))
           (cg-mv-into '(:const nil) tmps)))
      (:let
       (if (notany (lambda (b) (lvar-special (car b))) (second node))
           (cg-let node nil nil (list :mvinto tmps))
           (cg-mv-into-generic node tmps)))
      (t (cg-mv-into-generic node tmps)))))

(defun cg-mv-into-generic (node tmps)
  `(,@(cg node t) (:local.set ,(first tmps))
    ,@(loop for i from 1 for tmp in (rest tmps)
            append `((:global.get :mv-count) (:i32.const ,i) :i32.gt_s
                     (:if nil :eqref
                          ((:global.get :mv-buf) (:i32.const ,(1- i)) (:array.get :vector))
                          ,(nil-code))
                     (:local.set ,tmp)))))

(defun cg-mvbind (node mv tail &optional effect)
  "EFFECT: the body is compiled for its effects, nothing is left."
  (destructuring-bind (lvars vnode body) (cdr node)
    ;; a division whose values are computed inline: the quotient on the
    ;; stack, the remainder in REM-LOCAL (of representation REM-REP when it
    ;; is not an eqref)
    (multiple-value-bind (divmod rem-local rem-rep qpair)
        (cond ((null lvars) nil)
              ((qdual-candidate-p (first lvars) vnode)
               ;; the quotient of doubles into the pair of locals of the
               ;; first variable (as in CG-LET); nothing is left
               (let ((b (new-local)) (v (new-local :i64)))
                 (multiple-value-bind (c r) (typed-float-divmod vnode :pair t (cons b v))
                   (values c r :f64 (cons b v)))))
              (t (multiple-value-bind (c l) (typed-divmod-code vnode)
                   (if c (values c l nil) (numeric-divmod-code vnode)))))
      (let* ((into (and (not divmod) lvars (mv-into-worth-p vnode)))
             (tmps (loop for nil in lvars collect (new-local)))
             (code (cond (divmod) (into (cg-mv-into vnode tmps)) (t (cg vnode t))))
             (specials nil)
             ;; the second variable takes the unboxed remainder as it is
             (direct (and divmod rem-rep (second lvars)
                          (let ((lv (second lvars)))
                            (and (eq (lvar-rep lv) rem-rep) (not (boxed-p lv)) (not (lvar-special lv)))))))
        (when (null lvars) (setf code (append code '(:drop))))
        ;; primary
        (when (and lvars (not qpair) (not into))
          (setf code (append code `((:local.set ,(first tmps))))))
        (loop for i from 1 for tmp in (and (not into) (rest tmps))
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
            do (cond ((and qpair (= i 0))
                      (set-loc lv (cons :local (car qpair)))
                      (remhash lv *fdual-vars*)
                      (setf (gethash lv *dual-vars*) (cdr qpair)))
                     ((lvar-special lv) (push (cons lv tmp) specials))
                     ((and direct (= i 1))
                      (let ((idx (new-local (rep-wasm-type rem-rep))))
                        (set-loc lv (cons :local idx))
                        (setf code (append code `((:local.get ,rem-local) (:local.set ,idx))))))
                     (t (setf code (append code (bind-new-var-code lv `((:local.get ,tmp))))))))
      (if (null specials)
          (append code (if effect (cg-effect body) (cg body mv tail)))
          (let ((saved (new-local :i32)) (res (unless effect (new-local))))
            (setf specials (loop for (lv . tmp) in (reverse specials) collect (cons (lvar-name lv) tmp)))
            (append code
                    (bind-specials-code saved specials)
                    (with-dynamic-scope () (if effect (cg-effect body) (cg body mv nil)))
                    (when res `((:local.set ,res)))
                    (unbind-specials-code saved (mapcar #'car specials))
                    (when res `((:local.get ,res))))))))))

;;; ------------------------------------------------------------
;;; Closures
;;; ------------------------------------------------------------

(defun lam-rest-only-p (lam)
  "Required parameters and &REST, nothing else."
  (and (lam-rest lam) (null (lam-opt lam)) (not (lam-keyp lam))))

(defun lam-opt-only-p (lam)
  "Required and &OPTIONAL parameters only."
  (and (lam-keyed-p lam) (not (lam-keyp lam))))

(defparameter *arity-entries*
  (let ((s (lambda (name) (intern name (symbol-package 'clysm-sys::%car)))))
    (append
     `((max (2 . ,(funcall s "%MAX-2"))) (min (2 . ,(funcall s "%MIN-2")))
       (append (2 . ,(funcall s "%APPEND2"))) (gcd (2 . ,(funcall s "%GCD2"))) (lcm (2 . ,(funcall s "%LCM2"))))
     (loop for name in '(string= string< string-equal char= char-equal string> string-lessp char< char>
                         string<= string>= string/= string-greaterp string-not-greaterp string-not-lessp
                         string-not-equal char/= char<= char>= char-lessp char-greaterp char-not-equal
                         char-not-greaterp char-not-lessp)
           collect `(,name (2 . ,(funcall s (format nil "%~A-2" (symbol-name name))))))))
  "Global functions of the runtime taking any number of arguments or keywords
whose entry for some number of arguments K is the main function of another,
of K required parameters, that does the same (CHAR= of two characters,
STRING= of two strings without keywords): FUNCALL #'CHAR-EQUAL, a :TEST
#'STRING= or REDUCE #'MAX call it with no argument list or keyword search.")

(defun arity-entry-name (lam k)
  "The entry of LAM for K arguments from *ARITY-ENTRIES*, or NIL."
  (let* ((name (lam-name lam))
         (fast (and name (symbolp name) (assoc name *arity-entries*) (eq lam (known-global-lam name))
                    (cdr (assoc k (cdr (assoc name *arity-entries*))))))
         (flam (and fast (known-global-lam fast))))
    (when (and flam (lam-simple-p flam) (= (length (lam-req flam)) k) (null (lam-free-vars flam))
               (lam-accepts-p lam k))
      (ensure-lam-compiled flam)
      (list :main (lam-id flam)))))

(defun lam-entry-names (lam)
  (let ((simple (lam-simple-p lam)) (n (length (lam-req lam))) (id (lam-id lam)))
    (append
     (loop for k from 0 to +max-fixed-args+
           collect (cond ((arity-entry-name lam k))
                         ((lam-rest-only-p lam) (if (< k n) (list :argerr k) (list :rentry id k)))
                         ((lam-opt-only-p lam)
                          (cond ((not (<= n k (+ n (length (lam-opt lam))))) (list :argerr k))
                                ;; all of them: the keyed main function itself
                                ((= k (+ n (length (lam-opt lam)))) (list :kmain id))
                                (t (list :oentry id k))))
                         ((and (lam-keyed-p lam) (lam-keyp lam))
                          (cond ((< k n) (list :argerr k))
                                ((kentry-p lam k) (list :kentry id k))
                                (t (list :pack k))))
                         ((not simple) (list :pack k))
                         ((= k n) (list :main id))
                         (t (list :argerr k))))
     (list (if simple (list :entn id) (list :main id))))))

(defun lam-apply-entry-name (lam)
  "The entry of LAM taking its arguments as a list: a function with &REST
and required parameters only takes the rest of the list as its rest list."
  (if (lam-rest-only-p lam) (list :aentry (lam-id lam)) :apply-generic))

(defun lam-key-layout (lam)
  "(required optional . keywords): how the keyed main function of LAM takes its arguments."
  (list* (length (lam-req lam)) (length (lam-opt lam)) (mapcar #'first (lam-keys lam))))

(defun key-layout-object (layout)
  "The object standing for the key layout LAYOUT in closures, which calls
compare (EQ) with the one they were compiled for: a symbol named after it, so
that a call read from a fasl (or compiled by another module) finds the same
object; LAYOUT itself when a key's name would not survive in that name."
  (if (every (lambda (k)
               (and (symbolp k) (symbol-package k)
                    (notany (lambda (c) (member c '(#\Space #\:))) (symbol-name k))))
             (cddr layout))
      (intern (format nil "%KEY-LAYOUT ~D ~D~{ ~A~}" (first layout) (second layout)
                      (mapcar (lambda (k) (format nil "~A:~A" (target-package-name (symbol-package k)) (symbol-name k)))
                              (cddr layout)))
              (symbol-package 'clysm-sys::%car))
      layout))

(defun key-layout-list (object)
  "The layout (required optional . keywords) of a key layout object, or NIL."
  (cond ((consp object) object)
        ((and object (symbolp object))
         (let* ((name (symbol-name object)) (parts nil) (start 0))
           (loop (let ((sp (position #\Space name :start start)))
                   (push (subseq name start sp) parts)
                   (if sp (setq start (1+ sp)) (return))))
           (setq parts (nreverse parts))
           (and (string= (first parts) "%KEY-LAYOUT")
                (list* (parse-integer (second parts)) (parse-integer (third parts))
                       (mapcar (lambda (k)
                                 (let* ((c (position #\: k))
                                        (pkg (find-package (subseq k 0 c))))
                                   (if pkg
                                       (multiple-value-bind (s found) (find-symbol (subseq k (1+ c)) pkg)
                                         (if found s (return-from key-layout-list nil)))
                                       (return-from key-layout-list nil))))
                               (cdddr parts))))))
        (t nil)))

(defun lam-kmain-code (lam)
  "The kmain and klayout fields of LAM's code: for a function with keyword
parameters its keyed main function and key layout; for one of required
parameters and &REST its main function taking the rest list and a rest
layout (REST-LAYOUT-OBJECT), for APPLY in the image (CG-APPLY)."
  (cond ((and (lam-keyed-p lam) (lam-keyp lam))
         `((:ref.func (:kmain ,(lam-id lam))) ,@(const-init-code (key-layout-object (lam-key-layout lam)))))
        ((lam-rest-only-p lam)
         `((:ref.func (:rmain ,(lam-id lam))) ,@(const-init-code (rest-layout-object (length (lam-req lam))))))
        ((and (lam-simple-p lam) (< +max-fixed-args+ (length (lam-req lam)) (1+ +max-req-layout-args+)))
         `((:ref.func (:main ,(lam-id lam))) ,@(const-init-code (req-layout-object (length (lam-req lam))))))
        (t `((:ref.null :func) (:ref.null :eq)))))

;;; A function of N required parameters only, N from 5 to
;;; +MAX-REQ-LAYOUT-ARGS+, has its main function (taking them as Wasm
;;; parameters) in the kmain field of its code and a "req layout" in
;;; klayout: a call of N arguments of a function not known where it is
;;; compiled (another file's, through the symbol, or FUNCALL) compares the
;;; layout and calls the main function, with no argument vector for entryN to
;;; take apart (esrap's RECALL, five arguments, called for each rule).
(defun req-layout-object (n)
  (intern (format nil "%REQ-LAYOUT ~D" n) (symbol-package 'clysm-sys::%car)))

(defun rest-layout-object (nreq)
  "The object in the klayout field of the code of a function of NREQ
required parameters and &REST (its kmain field is then its main function,
taking the rest list): a symbol, the same in every module."
  (intern (format nil "%REST-LAYOUT ~D" nreq) (symbol-package 'clysm-sys::%car)))

(defun rest-layout-nreq (object)
  "The number of required parameters of a rest layout object, or NIL."
  (and object (symbolp object)
       (let ((name (symbol-name object)))
         (and (> (length name) 13) (string= "%REST-LAYOUT " name :end2 13)
              (parse-integer name :start 13 :junk-allowed t)))))

(defun lam-name-code (lam)
  (let ((name (lam-name lam)))
    (cond ((symbolp name) `((:global.get ,(sym-global name))))
          (t (const-init-code name)))))

(defun ensure-lam-compiled (lam)
  (unless (gethash lam *compiled-lams*)
    (setf (gethash lam *compiled-lams*) :pending)
    (push lam *pending-lams*)))

(defun static-closure-global (lam)
  "The global holding LAM's closure without environment, whose code (a
$code struct of its entries) every closure of LAM shares."
  (let ((name (list :clo (lam-id lam))))
    (unless (global-defined-p *m* name)
      (unless (gethash name *closure-global-names*)
        (setf (gethash name *closure-global-names*) t)
        (push (list name `(:ref :closure)
                    `(,@(loop for e in (lam-entry-names lam) collect `(:ref.func ,e))
                      ,@(lam-kmain-code lam)
                      (:ref.func ,(lam-apply-entry-name lam))
                      (:struct.new :code)
                      ,@(lam-name-code lam)
                      (:struct.new :closure)))
              *closure-globals*)))
    name))

(defun cg-closure (lam)
  (ensure-lam-compiled lam)
  (if (null (lam-free-vars lam))
      `((:global.get ,(static-closure-global lam)))
      (let ((n (length (lam-free-vars lam))))
        ;; the values of the free variables are fields of the closure
        `((:global.get ,(static-closure-global lam)) (:struct.get :closure ,+clo-code+)
          ,@(lam-name-code lam)
          ,@(loop for fv in (lam-free-vars lam) append (var-raw-code fv))
          ,@(when (> n +clo-env-max+) `((:array.new_fixed :vector ,n)))
          (:struct.new ,(envclo-type n))))))

(defun clo-field-code (i)
  "Instructions taking a closure to field I of its code (an entry, +CLO-KMAIN+
or +CLO-KLAYOUT+)."
  `((:struct.get :closure ,+clo-code+) (:struct.get :code ,i)))

;;; ------------------------------------------------------------
;;; Calls
;;; ------------------------------------------------------------

(defun call-closure-code-1 (closure-code arg-codes mv tail)
  "CLOSURE-CODE pushes a (ref $closure)."
  (declare (ignore mv))
  (let ((n (length arg-codes)) (tmp (new-local '(:ref :null :closure))))
    (maybe-checked-call
     tail
     (if (<= n +max-fixed-args+)
         `(,@closure-code (:local.tee ,tmp) (:ref.as_non_null)
           ,@(apply #'append arg-codes)
           (:local.get ,tmp) ,@(clo-field-code n)
           (,(if tail :return_call_ref :call_ref) ,(fixed-entry-type n)))
         `(,@closure-code (:local.tee ,tmp) (:ref.as_non_null)
           ,@(apply #'append arg-codes)
           (:array.new_fixed :vector ,n)
           (:local.get ,tmp) ,@(clo-field-code +clo-entry-n+)
           (,(if tail :return_call_ref :call_ref) :fN))))))

(defun call-closure-code (closure-code arg-codes mv tail)
  "CLOSURE-CODE pushes a (ref $closure)."
  (let ((n (length arg-codes)))
    (if (or (<= n +max-fixed-args+) (> n +max-req-layout-args+))
        (call-closure-code-1 closure-code arg-codes mv tail)
        ;; more arguments: the main function in kmain when the function has the
        ;; req layout of N (see REQ-LAYOUT-OBJECT), else entryN
        (let* ((tmp (new-local '(:ref :null :closure)))
               (temps (loop repeat n collect (new-local)))
               (gets (loop for tm in temps collect `(:local.get ,tm)))
               (type (main-type n))
               (slow (gensym "SLOWN")) (done (gensym "CALLN")))
          `(,@closure-code (:local.set ,tmp)
            ,@(loop for code in arg-codes for tm in temps append `(,@code (:local.set ,tm)))
            (:block ,done :eqref
              (:block ,slow nil
                (:local.get ,tmp) ,@(clo-field-code +clo-klayout+)
                ,@(const-init-code (req-layout-object n)) :ref.eq :i32.eqz (:br_if ,slow)
                ,@(maybe-checked-call
                   tail
                   `((:local.get ,tmp) :ref.as_non_null ,@gets
                     (:local.get ,tmp) ,@(clo-field-code +clo-kmain+) (:ref.cast! ,type)
                     (,(if tail :return_call_ref :call_ref) ,type)))
                (:br ,done))
              ,@(maybe-checked-call
                 tail
                 `((:local.get ,tmp) :ref.as_non_null ,@gets (:array.new_fixed :vector ,n)
                   (:local.get ,tmp) ,@(clo-field-code +clo-entry-n+)
                   (,(if tail :return_call_ref :call_ref) :fN)))))))))

(defvar *call-noreturn* nil
  "True while the call of a function that never returns normally (ERROR,
through its symbol) is compiled: MAYBE-CHECKED-CALL ends it with a branch to
the exit handler.")

(defun maybe-checked-call (tail code)
  (if tail code (checked-call code *call-noreturn*)))

;;; A global function whose code contains no exit of its own (THROW, a
;;; RETURN-FROM or GO out of a closure) and calls no function that is not
;;; known not to leave with one never returns with an exit pending: a direct
;;; call of it is not checked.
(defparameter *exiting-prims* '(clysm-sys::%throw clysm-sys::%apply-vector clysm-sys::%compile-module))

(defvar *may-exit-assumed* nil "set when a result depended on a function still being analyzed")

(defun lam-may-exit-p (lam)
  (let ((v (lam-may-exit lam)))
    (cond ((eq v :unknown)
           (setf (lam-may-exit lam) :in-progress)
           (let* ((*may-exit-assumed* nil)
                  (r (lam-may-exit-1 lam)))
             (setf (lam-may-exit lam) (if (or r (not *may-exit-assumed*)) r :unknown))
             r))
          ((eq v :in-progress) (setf *may-exit-assumed* t) nil)
          (t v))))

(defun lam-may-exit-1 (lam)
  (let ((root lam))
    (labels ((walk-lam (l)
               (loop for (nil init) in (lam-opt l) do (walk init))
               (loop for (nil nil init) in (lam-keys l) do (walk init))
               (walk (lam-body l)))
             (walk (n)
               (when (case (car n)
                       ((:throw :funcall :mvcall :apply) t)
                       (:go (not (eq (tb-lam (second n)) root)))
                       (:return-from (not (eq (blk-lam (second n)) root)))
                       (:prim (member (prim-name (second n)) *exiting-prims*))
                       (:call (and (not (inline-length-call-p n))
                                   (let ((callee (and (not (gethash (second n) *notinline*))
                                                      (known-global-lam (second n)))))
                                     (or (null callee) (lam-may-exit-p callee)))))
                       (:lambda (walk-lam (second n)) nil)
                       ((:flet :labels) (dolist (f (second n)) (walk-lam (fun-lam f))) nil))
                 (return-from lam-may-exit-1 t))
               (map-subnodes #'walk n)))
      (walk-lam lam)
      nil)))

(defun inline-length-call-p (n)
  "N is a call of LENGTH that I32-OP-CODE compiles as the length of a Wasm
array, with no call: of a variable declared a simple vector or string."
  (and (eq (second n) 'length) (= (length (cddr n)) 1)
       (let ((a (third n)))
         (and (eq (car a) :lref) (lvar-decl (second a))
              (let ((ty (declared-ty (lvar-decl (second a)))))
                (or (ty-vec-kind ty) (ty-only-p ty +k-svec+) (ty-only-p ty +k-string+)))))))

(defun direct-call-checked (lam tail code)
  "CODE: a direct call of LAM."
  ;; (a global function is a lambda of the toplevel form defining it, without
  ;; free variables: what LAM-MAY-EXIT-P finds is all it can do)
  (let ((global (or (null (lam-parent lam)) (and (lam-static-p lam) (null (lam-free-vars lam))))))
    (if (or tail (and global (not (lam-may-exit-p lam))))
        code
        (checked-call code (and global (lam-noreturn-p lam))))))

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
  (direct-call-checked lam tail (direct-call-code-1 lam closure-code arg-codes tail arg-nodes)))

(defun direct-call-code-1 (lam closure-code arg-codes tail arg-nodes)
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
         (not (and *batch-no-direct* (gethash sym *batch-no-direct*)))
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
  (cg-global-call-1 sym args mv tail))

(defun cg-global-call-1 (sym args mv tail)
  (let ((raw (raw-call-code (known-global-lam sym) args mv tail)))
    (when raw (return-from cg-global-call-1 raw)))
  (let ((lam (known-global-lam sym))
        (arg-codes (mapcar #'cg args)))
    ;; in a catch clone, a tail call under no CATCH or UNWIND-PROTECT may call
    ;; the callee's clone for the same tag: its throws to the tag are returns
    (when (and lam tail *tail-ok* *catching-tag* (null *uwp-frames*) (lam-simple-p lam)
               (null (lam-free-vars lam)) (= (length args) (length (lam-req lam))))
      (ensure-lam-compiled lam)
      (return-from cg-global-call-1
        `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
          (:return_call ,(ensure-catch-clone lam *catching-tag*)))))
    (if (and lam (lam-accepts-p lam (length args)))
        (let ((sig (spec-signature lam args)))
          (if sig
              (progn
                (ensure-lam-compiled lam)
                (direct-call-checked
                 lam tail
                 `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
                   (,(if tail :return_call :call) ,(spec-clone-name lam sig)))))
              (direct-call-code lam `((:global.get ,(static-closure-global lam))) arg-codes tail args)))
        (progn
          (when (and lam (not (lam-accepts-p lam (length args))))
            (cwarn "~S called with ~D arguments" sym (length args)))
          (or (and (null lam) (live-keyed-call-code sym args arg-codes tail))
              ;; (a call of ERROR: what follows it is not reached when it
              ;; returns, so that nothing the function computed before is
              ;; kept across the call; V8 otherwise spills the variables of
              ;; a loop whose error branch calls it on every iteration)
              (let ((*call-noreturn* (and (gethash sym *ti-noreturn*) t)))
                (call-closure-code (fdef-code sym) arg-codes mv tail)))))))

;;; In the image, a call with constant keywords of a global function that has
;;; keyword parameters passes the arguments in the fixed slots of the
;;; function's keyed main function (no argument vector, no search for the
;;; keys) as long as the symbol's function is one with the layout it had when
;;; the call was compiled (the layout object in the closure is compared).
(defvar *live-key-layout-hook* nil "symbol -> the key layout of its global function, or NIL")

(defvar *compiled-key-layouts* (make-hash-table :test 'eq)
  "symbol -> the key layout object of the global function last compiled for it in the image")

(defun live-keyed-call-code (sym args arg-codes tail)
  (let* ((layout-object (and *live-key-layout-hook* (not (gethash sym *notinline*))
                             (not (member sym *local-notinline*))
                             (or (funcall *live-key-layout-hook* sym)
                                 (gethash sym *compiled-key-layouts*))))
         (layout-list (key-layout-list layout-object)))
    (when (and (consp layout-list) (cddr layout-list))
      (destructuring-bind (nreq nopt &rest keys) layout-list
        (let* ((stub (make-lam :id -1 :name sym :keyp t
                               :req (make-list nreq) :opt (make-list nopt)
                               :keys (mapcar #'list keys)))
               (layout (keyed-call-layout stub args)))
          (when layout
            (let* ((temps (loop for nil in args collect (new-local)))
                   (f (new-local '(:ref :null :closure)))
                   (type (main-type (length layout)))
                   (slow (gensym "GENERIC")) (done (gensym "KCALL")))
              `(,@(loop for code in arg-codes for tmp in temps append `(,@code (:local.set ,tmp)))
                (:block ,done :eqref
                  (:block ,slow nil
                    (:global.get ,(sym-global sym)) (:struct.get :symbol ,+sym-function+) (:local.tee ,f)
                    :ref.is_null (:br_if ,slow)
                    (:local.get ,f) ,@(clo-field-code +clo-klayout+)
                    ,@(const-init-code layout-object) :ref.eq :i32.eqz (:br_if ,slow)
                    ,@(maybe-checked-call
                       tail
                       `((:local.get ,f) :ref.as_non_null
                         ,@(loop for idx across layout
                                 collect (if idx `(:local.get ,(nth idx temps)) '(:ref.null :eq)))
                         (:local.get ,f) ,@(clo-field-code +clo-kmain+) (:ref.cast! ,type)
                         (,(if tail :return_call_ref :call_ref) ,type)))
                    (:br ,done))
                  ,@(call-closure-code (fdef-code sym)
                                       (loop for tmp in temps collect `((:local.get ,tmp)))
                                       nil tail))))))))))

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
                             (coerce-function-code (cg fnode)))
                         (mapcar #'cg args) mv tail)))

(defun coerce-function-code (code)
  "CODE pushes a function designator; push the function, a (ref $closure):
a closure is taken as it is and the function of a symbol read from it at the
call site; only other objects (an error) go to the helper."
  (let ((l (gensym "FN")) (slow (gensym "SLOW")) (tmp (new-local)))
    `((:block ,l (:ref :closure)
        ,@code (:local.tee ,tmp) (:br_on_cast ,l :eqref (:ref :closure)) :drop
        (:block ,slow :eqref
          (:local.get ,tmp) (:br_on_cast_fail ,slow :eqref (:ref :symbol))
          (:struct.get :symbol ,+sym-function+) (:br_on_non_null ,l)
          (:local.get ,tmp))
        (:call :coerce-function) :ref.as_non_null))))

(defvar *spec-self* nil
  "While an entry version of a function is compiled: (lam sig name), its
function, signature and name.")

(defun self-clone-call-code (lam closure-code args tail)
  "In LAM's entry version (*SPEC-SELF*), a call of LAM itself with its own
closure: the version called directly when the arguments are of its classes
(a 32-bit integer tested to be a fixnum here), the general version
otherwise; NIL when the classes cannot hold."
  (destructuring-bind (self sig name) *spec-self*
    (when (and (eq lam self) (lam-simple-p lam) (= (length args) (length (lam-req lam)))
               (equal closure-code '((:local.get 0))))
      (let ((tests nil) (codes nil) (boxed nil))
        (loop for a in args for class in sig
              do (let ((ty (node-ty a)))
                   (cond ((or (null class) (and (eq class :fixnum) (ty-fixnum-p ty))
                              (and (eq class :double) (ty-double-p ty)))
                          (let ((tmp (new-local)))
                            (push `(,@(cg a) (:local.set ,tmp)) codes)
                            (push `(:local.get ,tmp) boxed)))
                         ((and (eq class :fixnum) (ty-int32-p ty))
                          (let ((tmp (new-local :i32)))
                            (push `(,@(cg-rep a :i32) (:local.set ,tmp)) codes)
                            (push `((:local.get ,tmp) (:i32.const ,+char-tag+) :i32.add
                                    (:i32.const ,(* 2 +char-tag+)) :i32.lt_u)
                                  tests)
                            (push `(:local.get ,tmp) boxed)
                            (setf (car boxed) (list :box tmp))))
                         (t (return-from self-clone-call-code nil)))))
        (setf codes (nreverse codes) boxed (nreverse boxed) tests (nreverse tests))
        (let* ((call (if tail :return_call :call))
               (fast `((:local.get 0) (:ref.cast! :closure)
                       ,@(loop for b in boxed
                               append (if (eq (car b) :box) `((:local.get ,(second b)) :ref.i31) (list b)))
                       (,call ,name)))
               (slow `((:local.get 0) (:ref.cast! :closure)
                       ,@(loop for b in boxed
                               append (if (eq (car b) :box) `((:local.get ,(second b)) (:call :box-i32)) (list b)))
                       (,call (:main ,(lam-id lam))))))
          (ensure-lam-compiled lam)
          (direct-call-checked
           lam tail
           `(,@(apply #'append codes)
             ,@(if tests
                   `(,@(car tests) ,@(loop for tc in (cdr tests) append `(,@tc :i32.and))
                     (:if nil :eqref ,fast ,slow))
                   fast))))))))

(defun cg-local-call (fun args mv tail)
  (let* ((lam (fun-lam fun))
         (closure-code (var-get-code (fun-lvar fun))))
    (when (and *spec-self* (lam-accepts-p lam (length args)))
      (let ((code (self-clone-call-code lam closure-code args tail)))
        (when code (return-from cg-local-call code))))
    (if (lam-accepts-p lam (length args))
        (direct-call-code lam `(,@closure-code (:ref.cast! :closure)) (mapcar #'cg args) tail args)
        (progn (cwarn "local function ~S called with ~D arguments" (fun-name fun) (length args))
               (call-closure-code `(,@closure-code (:ref.cast! :closure)) (mapcar #'cg args) mv tail)))))

(defvar *shake* nil
  "True when the whole program is shaken (SHAKE-MODULE): a global definition
of a function without free variables is a function (:install SYMBOL N) of its
own, which the shaker keeps only if something refers to SYMBOL.")
(defvar *shake-symbols* nil
  "True when SHAKE-MODULE also leaves out the symbols that nothing refers to
(bin/clysm library --shake-symbols): the program promises not to look up by
name a symbol that its code does not refer to.")
(defvar *install-counter* 0)

(defun cg-defun (node mv)
  "(:DEFUN SYMBOL FNODE): set the function of SYMBOL to the closure of FNODE.
(:DEFUN SYMBOL (:LAMBDA LAM) :FORM), made by SYS:%WHEN-REACHED with *SHAKE*:
call LAM (of no arguments nor free variables).  Either is the value SYMBOL,
and with *SHAKE* it is done only if something refers to SYMBOL
(SHAKE-MODULE)."
  (destructuring-bind (sym fnode &optional form) (cdr node)
    (let ((install (if form
                       (let ((lam (second fnode)))
                         (ensure-lam-compiled lam)
                         `((:global.get ,(static-closure-global lam))
                           (:call (:main ,(lam-id lam))) :drop))
                       `((:global.get ,(sym-global sym))
                         ,@(cg fnode) (:ref.cast! :closure)
                         (:struct.set :symbol ,+sym-function+)))))
      (if (and *shake* (not (tree-find-if (lambda (x) (member x '(:local.get :local.set :local.tee)))
                                          install)))
          (let ((name (list :install sym (incf *install-counter*))))
            ;; (it returns the symbol, the value of the definition: a
            ;; reference to the symbol here would keep the function)
            (add-func *m* name :v-e :body `(,@install (:global.get ,(sym-global sym))))
            `((:call ,name)
              ,@(when mv (mv-one))))
          `(,@install
            (:global.get ,(sym-global sym))
            ,@(when mv (mv-one)))))))

(defun tree-find-if (pred tree)
  (cond ((funcall pred tree) t)
        ((consp tree) (or (tree-find-if pred (car tree)) (tree-find-if pred (cdr tree))))
        (t nil)))

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
                (loop with n = (length (lam-free-vars lam)) with type = (envclo-type n)
                      for fv in (lam-free-vars lam) for i from 0
                      when (find fv funs :key #'fun-lvar)
                        do (setf code
                                 (append code
                                         (if (<= n +clo-env-max+)
                                             `((:local.get ,(cdr (get-loc (fun-lvar f))))
                                               (:ref.cast! ,type)
                                               (:local.get ,(cdr (get-loc fv)))
                                               (:struct.set ,type ,(+ +clo-env+ i)))
                                             `((:local.get ,(cdr (get-loc (fun-lvar f))))
                                               (:ref.cast! :envclov)
                                               (:struct.get :envclov ,+clo-env+)
                                               (:i32.const ,i)
                                               (:local.get ,(cdr (get-loc fv)))
                                               (:array.set :vector))))))))))
      (append code (cg body mv tail)))))

;;; ------------------------------------------------------------
;;; BLOCK / RETURN-FROM
;;; ------------------------------------------------------------

(defun cg-block (node mv tail &optional (rep :eqref))
  "REP other than :EQREF (from CG-REP, a local block only): the block and the
values its RETURN-FROMs pass are raw (an i32, i64 or f64)."
  (destructuring-bind (blk body) (cdr node)
    (let ((label (gensym "BLOCK")))
      (setf (blk-label blk) label
            (blk-rep blk) rep
            (blk-mv blk) mv
            (blk-dyn-depth blk) *dyn-depth*
            (blk-uwp-depth blk) *uwp-depth*
            (blk-uwp-frames blk) *uwp-frames*
            (blk-saved-bsp blk) (new-local :i32))
      (set-loc (blk-tag-var blk) (cons :local (new-local)))
      (let ((body-code (let ((*tail-ok* (and *tail-ok* (not (blk-nonlocal blk))))
                             (*catchers* (if (blk-nonlocal blk) (cons (list :block blk) *catchers*) *catchers*)))
                         (cond ((blk-nonlocal blk) (with-pctx (:eqref) (cg body mv tail)))
                               ((not (eq rep :eqref)) (cg-rep body rep))
                               (t (cg body mv tail))))))
        (if (not (blk-nonlocal blk))
            `(,@(save-bsp-code (blk-saved-bsp blk))
              (:block ,label ,(rep-wasm-type rep) ,@body-code))
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

(defun register-uwp-exit (frame continuation &optional mv)
  "Register a local exit through FRAME; CONTINUATION generates the code that
continues the exit after the cleanup (in the context outside FRAME).  Returns
the code that starts the exit, the value (if any) being in (uwpf-res FRAME).
MV: the exit carries multiple values (saved around the cleanup)."
  (let ((k (incf (uwpf-nexits frame))))
    (push (cons k continuation) (uwpf-exits frame))
    (when mv (push k (uwpf-mv-used frame)))
    `((:i32.const ,k) (:local.set ,(uwpf-code frame)) (:br ,(uwpf-landing frame)))))

(defun uwp-mv-save-code (frame res mvsave)
  "In the landing of FRAME: save the values of the exits that carry several."
  (let ((ks (uwpf-mv-used frame)))
    (cond ((null ks) nil)
          ((= (length ks) (uwpf-nexits frame))
           `((:local.get ,res) (:call :mv-save) (:local.set ,mvsave)))
          (t `((:ref.null :eq) (:local.set ,mvsave)
               ,@(loop for k in ks
                       append `((:local.get ,(uwpf-code frame)) (:i32.const ,k) :i32.eq
                                (:if nil nil ((:local.get ,res) (:call :mv-save) (:local.set ,mvsave))))))))))

(defun uwp-mv-restore-code (frame mvsave)
  (let ((ks (uwpf-mv-used frame)))
    (cond ((null ks) nil)
          ((= (length ks) (uwpf-nexits frame))
           `((:local.get ,mvsave) (:call :mv-restore) :drop))
          (t `((:local.get ,mvsave) :ref.is_null :i32.eqz
               (:if nil nil ((:local.get ,mvsave) (:call :mv-restore) :drop)))))))

(defun local-exit-to-block (blk value-code &optional boxed)
  "Code for a return-from BLK in the current function, VALUE-CODE pushing the
value (in the block's representation, unless BOXED)."
  (let ((frame (innermost-uwpf-between *uwp-frames* (blk-uwp-frames blk)))
        (rep (blk-rep blk)))
    (if (null frame)
        `(,@value-code
          ,@(when boxed (unbox-rep-code rep *ty-top*))
          ,@(when (> *dyn-depth* (blk-dyn-depth blk))
              (restore-bsp-code (blk-saved-bsp blk)))
          (:br ,(blk-label blk)))
        (progn
          ;; (through the cleanups the value is kept boxed)
          `(,@value-code ,@(unless boxed (box-rep-code rep *ty-top*)) (:local.set ,(uwpf-res frame))
            ,@(register-uwp-exit
               frame (lambda () (local-exit-to-block blk `((:local.get ,(uwpf-res frame))) t))
               (blk-mv blk)))))))

(defun cg-return-from (node)
  (destructuring-bind (blk value) (cdr node)
    (let ((local-p (eq (blk-lam blk) (fstate-lam *fs*))))
      (cond (local-p
             (local-exit-to-block blk (if (eq (blk-rep blk) :eqref)
                                          (cg value (blk-mv blk))
                                          (cg-rep value (blk-rep blk)))))
            (*fexit*
             (flagged-exit-code (var-get-code (blk-tag-var blk)) (cg value t)))
            (t
             (let ((tmp (new-local)))
               `(,@(cg value t) (:local.set ,tmp)
                 ,@(var-get-code (blk-tag-var blk))
                 (:local.get ,tmp)
                 (:throw :lisp-throw))))))))

;;; ------------------------------------------------------------
;;; Non-local exits as returns
;;;
;;; A Wasm exception costs about 120 ns in V8 plus 33 ns per frame it
;;; unwinds (SBCL's THROW: about 7 ns), so non-local exits are not thrown.
;;; An exit out of a function (a THROW, a RETURN-FROM or GO out of a
;;; closure, SYS:%THROW of a HANDLER-CASE or RESTART-CASE) stores the tag
;;; and the value in the symbols SYS::%EXIT-TAG and SYS::%EXIT-VALUE (the
;;; pending exit; unbound when there is none) and the function returns null,
;;; which no Lisp value is (an unbound marker that a function returns is
;;; told apart by the tag).  After every call that is not a tail call the
;;; code tests for null (CHECKED-CALL) and branches to the handler of the
;;; call's context: the catchers around the call in the function
;;; (non-local blocks and tagbodies, CATCH and %CATCH forms) take the exit
;;; if the tag is theirs; otherwise the function returns null in turn,
;;; through its UNWIND-PROTECT cleanups (while they run the exit is kept in
;;; locals).  The bindings of special variables are undone by the catcher
;;; that takes the exit.
;;;
;;; The code that does not test - the Wasm helpers that call Lisp functions
;;; through (:CALLSYM K) or a direct stub, and JavaScript - gets the exit
;;; thrown as the exception $LISP-THROW, which the catchers also take
;;; (TRY_TABLE).  A direct call of a function that cannot leave with an exit
;;; pending (LAM-MAY-EXIT-P) is not tested.
;;; ------------------------------------------------------------

(defun pending-tag-code () `((:global.get ,(sym-global 'clysm-sys::%exit-tag)) (:struct.get :symbol ,+sym-value+)))

(defun refine-nonlocal (lam)
  "Call after the calls in LAM are inlined: a block or tagbody of LAM to which
no other function exits any more (the lambdas that did were inlined) is local."
  (let ((own nil) (exited nil))
    (labels ((own-walk (n)
               (case (car n)
                 (:block (when (blk-nonlocal (second n)) (push (second n) own)))
                 (:tagbody (when (tb-nonlocal (second n)) (push (second n) own))))
               (map-subnodes #'own-walk n))
             (walk-lam (l)
               (loop for (nil init) in (lam-opt l) do (walk init l))
               (loop for (nil nil init) in (lam-keys l) do (walk init l))
               (walk (lam-body l) l))
             (walk (n cur)
               (case (car n)
                 (:lambda (walk-lam (second n)))
                 ;; as in CG-FLET and CG-LET: functions that are not referenced are not compiled
                 ((:flet :labels)
                  (dolist (f (second n))
                    (unless (zerop (lvar-refs (fun-lvar f))) (walk-lam (fun-lam f))))
                  (walk (third n) cur))
                 (:let
                  (loop for (lv . init) in (second n)
                        unless (and (eq (car init) :lambda) (zerop (lvar-refs lv)) (not (lvar-special lv)))
                          do (walk init cur))
                  (walk (third n) cur))
                 (:go (unless (eq (tb-lam (second n)) cur) (pushnew (second n) exited)))
                 (t
                  (when (and (eq (car n) :return-from) (not (eq (blk-lam (second n)) cur)))
                    (pushnew (second n) exited))
                  (map-subnodes (lambda (x) (walk x cur)) n)))))
      (loop for (nil init) in (lam-opt lam) do (own-walk init))
      (loop for (nil nil init) in (lam-keys lam) do (own-walk init))
      (own-walk (lam-body lam))
      (when own
        (walk-lam lam)
        (dolist (b own)
          (unless (member b exited)
            (if (blk-p b) (setf (blk-nonlocal b) nil) (setf (tb-nonlocal b) nil))))))))

(defun make-fexit (lam &optional force)
  "The exit state of the function being compiled (*FEXIT*).  Call when *FS*
is set up, before any code of the function is generated."
  (declare (ignore force))
  (list (make-blk :name nil :lam lam :label (gensym "FEXIT") :mv t :dyn-depth 0 :uwp-depth 0
                  :uwp-frames nil :saved-bsp (new-local :i32))
        (new-local) (new-local)
        (make-pctx :label (gensym "FPEND") :fs *fs*)))

(defun exit-globals-code (tag-code value-code)
  "Make the exit (TAG-CODE, VALUE-CODE: one instruction list each, without
calls) the pending exit."
  `((:global.get ,(sym-global 'clysm-sys::%exit-value)) ,@value-code (:struct.set :symbol ,+sym-value+)
    (:global.get ,(sym-global 'clysm-sys::%exit-tag)) ,@tag-code (:struct.set :symbol ,+sym-value+)))

(defun wrap-fexit (body)
  "BODY: the whole code of the function being compiled.  Its exits through
its own cleanups end at the FEXIT block with the exit in two locals, which
are made the pending exit; a call outside every catcher that returned with
an exit pending returns null in turn."
  (if (null *fexit*)
      body
      (destructuring-bind (blk tag val ctx) *fexit*
        (let ((code body))
          (when (tree-member `(:br ,(blk-label blk)) code)
            (let ((normal (gensym "NORMAL")))
              (setf code `(,@(when (tree-member `(:local.get ,(blk-saved-bsp blk)) code)
                               (save-bsp-code (blk-saved-bsp blk)))
                           (:block ,normal :eqref
                             (:block ,(blk-label blk) :eqref ,@code (:br ,normal))
                             :drop
                             ,@(exit-globals-code `((:local.get ,tag)) `((:local.get ,val)))
                             (:ref.null :eq))))))
          (when (pctx-used ctx)
            (setf code `((:block ,(pctx-label ctx) nil ,@code :return)
                         (:ref.null :eq))))
          code))))

(defun tree-member (x tree)
  (cond ((equal x tree) t)
        ((consp tree) (or (tree-member x (car tree)) (tree-member x (cdr tree))))))

(defun flagged-exit-code (tag-code value-code)
  "Leave the function being compiled with a pending exit (TAG-CODE: code
without calls), through its cleanups."
  (if (innermost-uwpf-between *uwp-frames* nil)
      (destructuring-bind (blk tag val ctx) *fexit*
        (declare (ignore ctx))
        `(,@value-code (:local.set ,val)
          ,@tag-code (:local.set ,tag)
          ,@(local-exit-to-block blk (nil-code))))
      (let ((v (new-local)) (tg (new-local)))
        `(,@value-code (:local.set ,v) ,@tag-code (:local.set ,tg)
          ,@(exit-globals-code `((:local.get ,tg)) `((:local.get ,v)))
          (:ref.null :eq) :return))))

(defun local-exit-to-catch (entry value-code)
  "Code for a throw to the CATCH form ENTRY (of *CATCHERS*) of the current
function, VALUE-CODE pushing the value."
  (destructuring-bind (tag done saved frames &rest r) (cdr entry)
    (declare (ignore tag r))
    (let ((frame (innermost-uwpf-between *uwp-frames* frames)))
      (if (null frame)
          `(,@value-code ,@(restore-bsp-code saved) (:br ,done))
          (progn
            `(,@value-code (:local.set ,(uwpf-res frame))
              ,@(register-uwp-exit
                 frame (lambda () (local-exit-to-catch entry `((:local.get ,(uwpf-res frame)))))
                 t)))))))

(defun local-go-pc (tb)
  "Code for a GO to the segment whose number is in TB's pc local."
  (let ((frame (innermost-uwpf-between *uwp-frames* (tb-uwp-frames tb))))
    (if (null frame)
        `(,@(when (> *dyn-depth* (tb-dyn-depth tb)) (restore-bsp-code (tb-saved-bsp tb)))
          (:br ,(tb-dispatch tb)))
        (register-uwp-exit frame (lambda () (local-go-pc tb))))))

(defun catcher-tag-code (c)
  (ecase (car c)
    (:block (var-get-code (blk-tag-var (second c))))
    (:tagbody (var-get-code (tb-tag-var (second c))))
    (:catch `((:local.get ,(second c))))))

(defun catcher-exit-code (c value-local)
  "Code for the exit, whose value is in VALUE-LOCAL, to the catcher C."
  (ecase (car c)
    (:block (local-exit-to-block (second c) `((:local.get ,value-local))))
    (:tagbody `((:local.get ,value-local) (:ref.cast :i31) :i31.get_s (:local.set ,(tb-pc (second c)))
                ,@(local-go-pc (second c))))
    (:catch (local-exit-to-catch c `((:local.get ,value-local))))))

;;; The calls.  A call of a Lisp function returns null when it left with an
;;; exit pending (and only then, unless the function returned an unbound
;;; marker: the pending tag tells them apart); the code after it branches to
;;; the handler of the context of the call.

(defun current-pctx ()
  (cond ((and *pend* (eq (pctx-fs *pend*) *fs*)) *pend*)
        (*fexit* (fourth *fexit*))))

(defun checked-call (code &optional noreturn)
  "CODE: instructions that push the result of a call of a Lisp function (not
a tail call).  When it returns with an exit pending, the handler of the
context takes it up.  NORETURN: the function never returns normally (ERROR),
so that it returns at all is an exit."
  (let ((ctx (current-pctx)))
    (cond
      ((null ctx) code)
      (noreturn
       (setf (pctx-used ctx) t)
       `(,@code (:br ,(pctx-label ctx))))
      (t
        (let* ((fctx (fourth *fexit*))
               (tmp (or (pctx-tmp fctx) (setf (pctx-tmp fctx) (new-local)))))
          (setf (pctx-used ctx) t)
          `(,@code
            (:local.tee ,tmp) :ref.is_null (:hint 0)
            (:if nil nil ((:call :exit-pending-p) (:br_if ,(pctx-label ctx))))
            (:local.get ,tmp)))))))

(defun lam-noreturn-p (lam)
  "LAM never returns normally: a global function the type inference found
returns no value (every way out of it is an error or an exit), or ERROR."
  (or (and (symbolp (lam-name lam)) (gethash (lam-name lam) *ti-noreturn*))
      (let ((ty (gethash (lam-body lam) *node-types*)))
        (and ty (ty-bottom-p ty)))))

(defun take-pending-code (val)
  "The pending exit's value to VAL; no exit is pending any more."
  `((:global.get ,(sym-global 'clysm-sys::%exit-value)) (:struct.get :symbol ,+sym-value+) (:local.set ,val)
    (:global.get ,(sym-global 'clysm-sys::%exit-tag)) (:ref.null :eq) (:struct.set :symbol ,+sym-value+)
    (:global.get ,(sym-global 'clysm-sys::%exit-value)) (:ref.null :eq) (:struct.set :symbol ,+sym-value+)))

(defun pend-handler-code ()
  "The handler of a context of calls (in the context's dynamic environment):
the catchers around it take the pending exit if it is theirs; else the
function returns null with the exit pending, through its cleanups."
  (let ((tag (new-local)) (val (new-local))
        ;; the bindings made since the catcher are undone (the call may
        ;; have been in a LET of a special variable inside the context)
        (*dyn-depth* most-positive-fixnum)
        (*tail-ok* nil))
    `(,@(when *catchers*
          `(,@(pending-tag-code) (:local.set ,tag)
            ,@(loop for c in *catchers*
                    append `((:local.get ,tag) ,@(catcher-tag-code c) :ref.eq
                             (:if nil nil (,@(take-pending-code val) ,@(catcher-exit-code c val)))))))
      ,@(if (innermost-uwpf-between *uwp-frames* nil)
            (destructuring-bind (blk ftag fval ctx) *fexit*
              (declare (ignore ctx))
              `(,@(pending-tag-code) (:local.set ,ftag)
                ,@(take-pending-code fval)
                ,@(local-exit-to-block blk (nil-code))))
            '((:ref.null :eq) :return))
      :unreachable)))

(defun new-pctx () (make-pctx :label (gensym "PEND") :fs *fs*))

(defun wrap-pctx (ctx code type handler)
  "CODE (leaving a value of TYPE, :EQREF or NIL) in the context CTX, whose
handler is HANDLER (a thunk run in the context's dynamic environment)."
  (if (not (pctx-used ctx))
      code
      (let ((done (gensym "PDONE")))
        `((:block ,done ,type
            (:block ,(pctx-label ctx) nil ,@code (:br ,done))
            ,@(funcall handler))))))


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

;;; The guards of the copies of functions (LIVE-INLINE-CALL) in a loop that
;;; calls nothing else are tested once before the loop, into locals: nothing
;;; in the loop can redefine the function, except the call that a failed
;;; guard makes and the interrupt check (POLL-CODE), after which they are
;;; tested again.  (Primitives may signal errors, whose handlers run user
;;; code, but none of their errors can be continued into the loop.)
(defvar *hoisted-guards* nil "((guard-node . key) . i32 local) of the enclosing loops")
(defvar *hoisted-guards-fs* nil "the function whose locals *HOISTED-GUARDS* are")
(defvar *assumed-guards* nil "keys of the guards taken as true in the code being compiled")
(defvar *assumed-guards-fs* nil)

(defun current-hoisted-guards ()
  (and (eq *hoisted-guards-fs* *fs*) *hoisted-guards*))

(defun guard-node-p (c)
  (and (consp c) (eq (car c) :prim)
       (member (prim-name (second c)) '(clysm-sys::%function-cell-eq clysm-sys::%fasl-inline-guard clysm-sys::%defun-guard))
       (every (lambda (a) (eq (car a) :const)) (cddr c))))

(defun guard-key (c) (cons (prim-name (second c)) (mapcar #'second (cddr c))))

(defun hoisted-guard-local (c)
  (and (current-hoisted-guards) (guard-node-p c)
       (cdr (assoc (guard-key c) *hoisted-guards* :key #'cdr :test #'equal))))

(defun hoisted-guards-test-code ()
  "Code testing again the guards hoisted out of the enclosing loops."
  (let ((guards (current-hoisted-guards)))
    (let ((*hoisted-guards* nil) (*assumed-guards* nil))
      (loop for ((node . nil) . loc) in guards
            append `(,@(cg-test node) (:local.set ,loc))))))

(defun guard-else-epilogue (c)
  "After the call that a failed guard made: the hoisted guards tested again."
  (when (hoisted-guard-local c) (hoisted-guards-test-code)))

(defparameter *guard-loop-safe-calls*
  '(aref svref char schar row-major-aref length elt car cdr first rest nth nthcdr cons list vector
    sqrt expt exp log sin cos tan atan abs floor ceiling truncate round ffloor fceiling ftruncate fround
    mod rem min max float + - * / = < > <= >= /= 1+ 1- zerop plusp minusp evenp oddp
    logand logior logxor lognot ash ldb isqrt gcd lcm signum numberp integerp floatp realp rationalp
    complex realpart imagpart conjugate phase char-code code-char gethash
    clysm-sys::%aref1 clysm-sys::%aset1 clysm-sys::%svref clysm-sys::%set-svref clysm-sys::%svset
    clysm-sys::%row-major-aset clysm-sys::%aref2 clysm-sys::%aset2 clysm-sys::%charset clysm-sys::%scharset
    clysm-sys::%struct-ref clysm-sys::%struct-set clysm-sys::%struct-tref clysm-sys::%struct-tset
    clysm-sys::%struct-iref clysm-sys::%struct-iset clysm-sys::%lref clysm-sys::%lset))

(defun loop-guards (segments)
  "The guards (nodes) of copies in the forms SEGMENTS of a loop when it calls
nothing but the functions guarded (when the guard fails) and primitives; else NIL."
  (let ((guards nil))
    (labels ((walk (n)
               (when (consp n)
                 (case (car n)
                   ((:funcall :mvcall :apply :lcall :catch :throw :unwind-protect :progv :defun
                     :flet :labels :symtab :ltv :lambda)
                    (return-from loop-guards nil))
                   (:call (unless (member (second n) *guard-loop-safe-calls*)
                            (return-from loop-guards nil))
                          (mapc #'walk (cddr n)))
                   (:prim (when (member (prim-name (second n))
                                        '(clysm-sys::%fasl-inline-guard-set clysm-sys::%image-resume))
                            (return-from loop-guards nil))
                          (mapc #'walk (cddr n)))
                   (:if (if (guard-node-p (second n))
                            (progn (pushnew (cons (second n) (guard-key (second n))) guards
                                            :key #'cdr :test #'equal)
                                   ;; (the else branch is the call of the function)
                                   (walk (third n)))
                            (mapc #'walk (cdr n))))
                   (t (map-subnodes #'walk n))))))
      (dolist (seg segments) (mapc #'walk (cdr seg)))
      guards)))

;;; An innermost loop whose guards all hold when it starts runs in a version
;;; compiled with the guards taken as true (no call at all: V8 keeps its
;;; values in registers); an interrupt check whose interrupt redefined a
;;; function goes on in the other version, compiled with the guards, from
;;; the head of the same segment (both versions share their variables, and a
;;; segment's head is where an iteration starts).
(defun assumed-guards-active-p () (and *assumed-guards* (eq *assumed-guards-fs* *fs*)))

(defun assumed-guard-p (c)
  (and *assumed-guards* (eq *assumed-guards-fs* *fs*) (guard-node-p c)
       (member (guard-key c) *assumed-guards* :test #'equal)))

(defun nested-loop-p (segments)
  (labels ((walk (n)
             (when (consp n)
               (when (and (eq (car n) :tagbody)
                          (let ((m (tagbody-go-modes (second n) (third n))))
                            (or (find :self m) (find :far m))))
                 (return-from nested-loop-p t))
               (map-subnodes #'walk n))))
    (dolist (seg segments) (mapc #'walk (cdr seg)))
    nil))

(defun cg-tagbody (node mv)
  (let* ((segments (third node))
         (outer (current-hoisted-guards))
         (modes (and *live-mode* (consp segments) (tagbody-go-modes (second node) segments)))
         (all (and modes (or (find :self modes) (find :far modes)) (loop-guards segments)))
         (guards (remove-if (lambda (g) (assoc (cdr g) outer :key #'cdr :test #'equal)) all)))
    (if (null all)
        (cg-tagbody-1 node mv)
        (let* ((new (loop for g in guards collect (cons g (new-local :i32))))
               (init (let ((*hoisted-guards* nil) (*assumed-guards* nil))
                       (loop for ((gn . nil) . loc) in new append `(,@(cg-test gn) (:local.set ,loc))))))
          (let ((*hoisted-guards* (append new outer)) (*hoisted-guards-fs* *fs*))
            (if (or (tb-nonlocal (second node)) (nested-loop-p segments))
                `(,@init ,@(cg-tagbody-1 node mv))
                (let* ((keys (mapcar #'cdr all))
                       (flags (let ((locs (loop for k in keys
                                                collect (cdr (assoc k *hoisted-guards* :key #'cdr :test #'equal)))))
                                `((:local.get ,(car locs))
                                  ,@(loop for l in (cdr locs) append `((:local.get ,l) :i32.and)))))
                       (pc (new-local :i32)) (slow-entry (gensym "SLOWLOOP")) (done (gensym "LOOPV"))
                       (fast-poll (lambda (set-pc)
                                    (subst `((:call :poll-slow) ,@(hoisted-guards-test-code)
                                             ,@flags :i32.eqz
                                             (:if nil nil (,@set-pc (:br ,slow-entry))))
                                           '((:call :poll-slow)) (poll-code) :test #'equal)))
                       (entry nil)
                       (fast (let ((*assumed-guards* keys) (*assumed-guards-fs* *fs*))
                               ;; the variables that are doubles in this version
                               (multiple-value-bind (inner outer) (version-typed-vars segments)
                                 (setf entry (version-entry-code outer))
                                 (let ((saved (mapcar (lambda (lv) (list lv (lvar-rep lv) (lvar-type lv))) inner))
                                       (*version-lvar-tys* (mapcar (lambda (lv) (cons lv (mkty +k-double+)))
                                                                   (append inner outer)))
                                       (*version-fdual-vars* outer))
                                   (dolist (lv inner) (setf (lvar-rep lv) :f64 (lvar-type lv) (mkty +k-double+)))
                                   (unwind-protect
                                        (cg-tagbody-1 node nil :pc pc :bare t
                                                      :seg-poll (lambda (k) (funcall fast-poll `((:i32.const ,k) (:local.set ,pc))))
                                                      :dispatch-poll (lambda () (funcall fast-poll nil)))
                                     (loop for (lv rep ty) in saved
                                           do (setf (lvar-rep lv) rep (lvar-type lv) ty)))))))
                       (slow (cg-tagbody-1 node nil :pc pc :bare t :force-dispatch t :no-pc-init t)))
                  `(,@init
                    (:block ,done nil
                      (:block ,slow-entry nil
                        ,@flags
                        ,@(unless (equal entry '((:i32.const 1))) `(,@entry :i32.and))
                        (:if nil nil (,@fast (:br ,done)) ((:i32.const 0) (:local.set ,pc))))
                      ,@slow)
                    ,@(nil-code)
                    ,@(when mv (mv-one))))))))))

(defun version-typed-fdual-p (lv)
  (and *version-fdual-vars* (assumed-guards-active-p) (member lv *version-fdual-vars*)
       (fdual-local lv) (eq (car (get-loc lv)) :local)))

(defun version-typed-vars (segments)
  "The variables that are doubles in the version of a loop (SEGMENTS) whose
guards hold (to be called with the guards assumed): (values inner outer),
INNER the variables bound in the loop (to get an f64 local in that version),
OUTER the unboxed double variables bound outside it and assigned in it.  The
types are found by a fixed point: all taken as doubles, a variable dropped
when its value or one of its assignments is not a double with the others'
types (the copies' types given by ASSUMED-NODE-TY)."
  (let ((inits nil) (sets nil) (excluded nil))
    (labels ((walk (n)
               (when (consp n)
                 (case (car n)
                   (:let (dolist (b (second n)) (push (cons (car b) (cdr b)) inits)))
                   (:mvbind (dolist (lv (second n)) (push lv excluded)))
                   (:lset (push (cons (second n) (third n)) sets)))
                 (map-subnodes #'walk n))))
      (dolist (seg segments) (mapc #'walk (cdr seg))))
    (let* ((inner (loop for (lv . nil) in inits
                        when (and (not (lvar-special lv)) (not (boxed-p lv)) (not (lvar-captured lv))
                                  (null (lvar-rep lv)) (not (member lv excluded)))
                          collect lv))
           (outer (remove-duplicates
                   (loop for (lv . nil) in sets
                         when (and (not (assoc lv inits)) (not (lvar-special lv)) (not (boxed-p lv))
                                   (fdual-local lv) (eq (car (get-loc lv)) :local))
                           collect lv)))
           (cands (append (remove-duplicates inner) outer))
           (double (mkty +k-double+)))
      (loop
        (let* ((*version-lvar-tys* (mapcar (lambda (lv) (cons lv double)) cands))
               (bad (remove-if (lambda (lv)
                                 (and (let ((i (assoc lv inits))) (or (null i) (ty-double-p (node-ty (cdr i)))))
                                      (loop for (v . e) in sets
                                            always (or (not (eq v lv)) (ty-double-p (node-ty e))))))
                               cands)))
          (when (or (null bad) (null cands)) (return))
          (setf cands (set-difference cands bad))))
      (values (remove-if-not (lambda (lv) (member lv cands)) inner)
              (remove-if-not (lambda (lv) (member lv cands)) outer)))))

(defun version-entry-code (outer)
  "An i32: 1 when every variable of OUTER holds a double (its f64 local
then holds it), else 0."
  (if (null outer)
      '((:i32.const 1))
      (loop for lv in outer for i from 0
            for b = (cdr (get-loc lv)) for f = (fdual-local lv)
            append `((:local.get ,b) :ref.is_null
                     (:if nil :i32 ((:i32.const 1))
                          ((:local.get ,b) (:ref.test :double)
                           (:if nil :i32 ((:local.get ,b) (:ref.cast! :double) (:struct.get :double 0)
                                          (:local.set ,f) (:i32.const 1))
                                ((:i32.const 0)))))
                     ,@(when (> i 0) '(:i32.and))))))

(defun poll-code-p () (and (poll-code) t))

(defun loop-poll-code ()
  (let ((p (poll-code)))
    (if (and p (current-hoisted-guards))
        ;; (an interrupt may have redefined a function)
        (let ((again (hoisted-guards-test-code)))
          (subst (cons '(:call :poll-slow) again) '((:call :poll-slow)) p :test #'equal))
        p)))

(defun cg-tagbody-1 (node mv &key pc force-dispatch no-pc-init bare seg-poll dispatch-poll)
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
           (pc (or pc (new-local :i32)))
           (modes (tagbody-go-modes tb segments))
           (need-dispatch (or force-dispatch (tb-nonlocal tb) (find :far modes)))
           (loop-labels (loop for i below n collect (and (eq (aref modes i) :self) (gensym "TAGLOOP")))))
      (setf (tb-labels tb) labels (tb-dispatch tb) dispatch (tb-pc tb) pc
            (tb-modes tb) modes (tb-loop-labels tb) loop-labels
            (tb-dyn-depth tb) *dyn-depth* (tb-uwp-depth tb) *uwp-depth* (tb-uwp-frames tb) *uwp-frames*
            (tb-saved-bsp tb) (new-local :i32))
      (set-loc (tb-tag-var tb) (cons :local (new-local)))
      (let* ((ctx (and (tb-nonlocal tb) (new-pctx)))
             (seg-codes (let ((*tail-ok* nil) (*pend* (or ctx *pend*))
                              (*cg-loop* (if (or need-dispatch (some #'identity loop-labels)) node *cg-loop*))
                              (*catchers* (if (tb-nonlocal tb) (cons (list :tagbody tb) *catchers*) *catchers*)))
                          (loop for (k . forms) in segments for ll in loop-labels
                                collect (progn
                                          (setf (tb-cur tb) k)
                                          (let ((code (loop for f in forms append (cg-effect f))))
                                            (if ll `((:loop ,ll nil ,@(if seg-poll (funcall seg-poll k) (loop-poll-code)) ,@code)) code))))))
             ;; build nested blocks: innermost has br_table
             (inner (when need-dispatch `((:local.get ,pc) (:br_table ,@labels ,(car (last labels))))))
             (blocks (let ((acc inner))
                       (loop for label in labels for code in seg-codes
                             do (setf acc `((:block ,label nil ,@acc) ,@code)))
                       (if ctx
                           (let ((*tail-ok* nil) (*catchers* (cons (list :tagbody tb) *catchers*)))
                             (wrap-pctx ctx acc nil #'pend-handler-code))
                           acc)))
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
        `(,@(when (and need-dispatch (not no-pc-init)) `((:i32.const 0) (:local.set ,pc)))
          ,@(save-bsp-code (tb-saved-bsp tb))
          ,@(when (tb-nonlocal tb)
              (var-set-code (tb-tag-var tb) `(,@(nil-code) (:struct.new :cell))))
          (:block ,end nil
            ,@(if need-dispatch
                  `((:loop ,dispatch nil ,@(if dispatch-poll (funcall dispatch-poll) (loop-poll-code)) ,@loop-body))
                  loop-body))
          ,@(unless bare (nil-code))
          ,@(when (and mv (not bare)) (mv-one)))))))

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
          (if *fexit*
              (flagged-exit-code (var-get-code (tb-tag-var tb)) `((:i32.const ,index) :ref.i31))
              `(,@(var-get-code (tb-tag-var tb))
                (:i32.const ,index) :ref.i31
                (:throw :lisp-throw)))))))

;;; ------------------------------------------------------------
;;; CATCH / THROW / UNWIND-PROTECT
;;; ------------------------------------------------------------

(defun cg-catch (node mv)
  "(:catch tag body [unchecked]): UNCHECKED (SYS:%CATCH) does not record the
tag in *CATCH-TAGS* (SYS:%THROW does not look for it)."
  (destructuring-bind (tag-node body &optional unchecked) (cdr node)
    (let ((tag (new-local)) (saved (new-local :i32))
          (vtmp (new-local)) (ttmp (new-local)) (etmp (new-local :exnref))
          (done (gensym "CATCH")) (h (gensym "H")))
      `(,@(cg tag-node) (:local.set ,tag)
        ,@(save-bsp-code saved)
        ;; bind *catch-tags* to the tag: the active tags are its value and the
        ;; values saved by its bindings on the binding stack
        ,@(unless unchecked
            `((:global.get ,(sym-global 'clysm-sys::*catch-tags*))
              (:local.get ,tag)
              (:call :bind)))
        (:block ,done :eqref
          (:block ,h (:type :bt-catch)
            (:try_table nil :eqref ((:catch_ref :lisp-throw ,h))
              ,@(let* ((*tail-ok* nil) (*uwp-depth* (1+ *uwp-depth*))
                       (*dyn-depth* (if unchecked *dyn-depth* (1+ *dyn-depth*)))
                       (*uwp-frames* (cons :catch *uwp-frames*))
                       (*catchers*
                         (cons (list :catch tag done saved *uwp-frames*
                                     (if (and (not unchecked) (eq (car tag-node) :const) (symbolp (second tag-node)))
                                         (second tag-node) :unknown))
                               *catchers*)))
                  (with-pctx (:eqref)
                    (or (and (not unchecked) (catch-clone-call tag-node body)) (cg body mv)))))
            ,@(unless unchecked (restore-bsp-code saved))
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
    ;; is a branch (through the cleanups of the UNWIND-PROTECTs in between)
    (when (and (eq (car tag-node) :const) (symbolp (second tag-node)))
      (let ((entry (dolist (c *catchers*)
                     (when (eq (car c) :catch)
                       (cond ((eq (sixth c) (second tag-node)) (return c))
                             ((eq (sixth c) :unknown) (return nil)))))))
        (when entry
          (return-from cg-throw (local-exit-to-catch entry (cg value t))))))
    (let ((tag (new-local)) (v (new-local)))
      `(,@(cg tag-node) (:local.tee ,tag) (:call :check-catch-tag)
        ,@(cg value t) (:local.set ,v)
        ;; the CATCH forms around the throw in this function take it with a branch
        ,@(loop for c in *catchers*
                when (eq (car c) :catch)
                  append `((:local.get ,tag) ,@(catcher-tag-code c) :ref.eq
                           (:if nil nil ,(catcher-exit-code c v))))
        ,@(if *fexit*
              ;; a return with the exit pending
              `(,@(flagged-exit-code `((:local.get ,tag)) `((:local.get ,v)))
                :unreachable)
              `((:local.get ,tag) (:local.get ,v)
                (:throw :lisp-throw)))))))

(defun cg-unwind-protect (node mv)
  (destructuring-bind (form cleanup) (cdr node)
    (let* ((saved (new-local :i32)) (res (new-local)) (etmp (new-local :exnref))
           (mvsave (new-local)) (code (new-local :i32))
           (done (gensym "UWP")) (h (gensym "H")) (land (gensym "LAND"))
           (frame (make-uwpf :code code :res res :landing land :dyn-depth *dyn-depth*
                             :uwp-depth *uwp-depth* :parent *uwp-frames*))
           (body-code (let ((*tail-ok* nil) (*uwp-depth* (1+ *uwp-depth*))
                            (*uwp-frames* (cons frame *uwp-frames*)))
                        (with-pctx (:eqref) (cg form mv)))))
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
                            ,@(uwp-mv-save-code frame res mvsave)
                            ,@cleanup-code
                            ,@(uwp-mv-restore-code frame mvsave)
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

(defun mvcall-simple-arg-p (n)
  "N gives one value and evaluating it leaves the multiple values as they are."
  (member (car n) '(:const :lref)))

(defun cg-mvcall-fixed (fnode args tail)
  "(MULTIPLE-VALUE-CALL F ... FORM ...) where every argument form but FORM
is a constant or a variable: when the values number at most four in all,
F's entry for that many is called with them (from the multiple-value
buffer), with no list or argument vector; else the generic way."
  (let* ((pos (position-if-not #'mvcall-simple-arg-p args))
         (before (subseq args 0 pos)) (form (nth pos args)) (after (nthcdr (1+ pos) args))
         (nsimple (+ (length before) (length after)))
         (ftmp (new-local '(:ref :null :closure)))
         (btemps (loop for nil in before collect (new-local)))
         (v (new-local)) (cnt (new-local :i32)) (acc (new-local))
         (done (gensym "MVCALL")) (generic (gensym "MVGEN"))
         (call (if tail :return_call_ref :call_ref)))
    `(,@(coerce-function-code (cg fnode)) (:local.set ,ftmp)
      ,@(loop for a in before for tm in btemps append `(,@(cg a) (:local.set ,tm)))
      ,@(cg form t) (:local.set ,v) (:global.get :mv-count) (:local.set ,cnt)
      (:block ,done :eqref
        (:block ,generic nil
          ,@(loop for c from 0 to (- +max-fixed-args+ nsimple)
                  for k = (+ nsimple c)
                  append `((:local.get ,cnt) (:i32.const ,c) :i32.eq
                           (:if nil nil
                                (,@(maybe-checked-call
                                    tail
                                    `((:local.get ,ftmp) :ref.as_non_null
                                      ,@(loop for tm in btemps collect `(:local.get ,tm))
                                      ,@(when (> c 0) `((:local.get ,v)))
                                      ,@(loop for i from 0 below (1- c)
                                              append `((:global.get :mv-buf) (:i32.const ,i) (:array.get :vector)))
                                      ,@(loop for a in after append (cg a))
                                      (:local.get ,ftmp) ,@(clo-field-code k)
                                      (,call ,(fixed-entry-type k))))
                                 (:br ,done)))))
          (:br ,generic))
        ;; more values: all of them in a vector for entryN
        ;; (the multiple values are still those of FORM)
        ,@(nil-code)
        ,@(loop for tm in btemps append `((:local.set ,acc) (:local.get ,tm) (:local.get ,acc) (:struct.new :cons)))
        (:local.set ,acc)
        (:local.get ,v) (:call :mv-to-list) (:local.get ,acc) (:call :revappend-onto)
        ,@(loop for a in after append `((:local.set ,acc) ,@(cg a) (:local.get ,acc) (:struct.new :cons)))
        (:local.set ,acc)
        ,@(maybe-checked-call
           tail
           `((:local.get ,ftmp) (:ref.as_non_null)
             (:local.get ,acc) (:call :reverse-list-to-vector)
             (:local.get ,ftmp) ,@(clo-field-code +clo-entry-n+)
             (,call :fN)))))))

(defun cg-mvcall (node mv tail)
  (declare (ignore mv))
  (destructuring-bind (fnode &rest args) (cdr node)
    (when (= (count-if-not #'mvcall-simple-arg-p args) 1)
      (return-from cg-mvcall (cg-mvcall-fixed fnode args tail)))
    ;; the values of the arguments are appended to a vector of them, given
    ;; to the function's entry of any number of arguments
    (let ((ftmp (new-local '(:ref :null :closure)))
          (acc (new-local)))
      `(,@(coerce-function-code (cg fnode)) (:local.set ,ftmp)
        (:ref.null :eq) (:local.set ,acc)
        ,@(loop for a in args
                append `(,@(cg a t) (:local.get ,acc) (:call :mv-append) (:local.set ,acc)))
        ,@(maybe-checked-call
           tail
           `((:local.get ,ftmp) (:ref.as_non_null)
             ,@(if args
                   `((:local.get ,acc) (:ref.cast :vector))
                   '((:i32.const 0) (:array.new_default :vector)))
             (:local.get ,ftmp) ,@(clo-field-code +clo-entry-n+)
             (,(if tail :return_call_ref :call_ref) :fN)))))))

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
        do (let* ((lam (pop *pending-lams*)) (*catchers* nil)
                  (*func-source* (let ((l (cond ((not (consp lam)) lam)
                                                ((member (car lam) '(:spec :raw)) (second lam))
                                                (t (car lam)))))
                                   (and (lam-p l) (lam-source l)))))
             (cond ((and (consp lam) (eq (car lam) :spec))
                    (compile-spec-clone (second lam) (third lam) (fourth lam)))
                   ((and (consp lam) (eq (car lam) :raw))
                    (compile-raw-clone (second lam) (third lam) (fourth lam)))
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
  ;; (it is a flag clone as well: the other exits that leave it, such as a
  ;; throw to the tag from a recursive call, return to the CATCH pending)
  (inline-calls lam)
  (refine-nonlocal lam)
  (infer-lam lam)
  (let* ((n (length (lam-req lam)))
         (*fs* (make-fstate :lam lam :nparams (1+ n)))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam t)) (*catchers* nil)
         (prologue (setup-free-vars lam))
         (any-special nil))
    (loop for lv in (lam-req lam) for i from 1
          do (multiple-value-bind (code sp) (param-setup-code lv i)
               (setf prologue (append prologue code))
               (when sp (setf any-special (append any-special (list sp))))))
    (let ((body (wrap-fexit (let ((*catching-tag* tag)) (finish-body lam prologue any-special)))))
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
            (checked-call
             `((:global.get ,(static-closure-global lam)) ,@(apply #'append arg-codes)
               (:call ,(ensure-catch-clone lam tag))))))))))

(defun setup-free-vars (lam &aux free-reps)
  (when (lam-self lam)
    (set-loc (lam-self lam) (cons :local 0)))
  (when (lam-free-vars lam)
    (let* ((n (length (lam-free-vars lam)))
           (type (envclo-type n))
           (env (new-local (if (<= n +clo-env-max+) `(:ref :null ,type) '(:ref :null :vector)))))
      (setf (fstate-env-local *fs*) env
            (fstate-env-type *fs*) (and (<= n +clo-env-max+) type))
      (loop for fv in (lam-free-vars lam) for i from 0
            do (set-loc fv (cons :env i)))
      ;; (an entry version of the closure: free variables known to be
      ;; fixnums or doubles, unboxed into locals)
      (setf free-reps
            (loop for (fv . rep) in *clone-free-reps*
                  for i = (position fv (lam-free-vars lam))
                  when (and i (eq (lvar-rep fv) rep))
                    append (let ((idx (new-rep-local rep)))
                             (prog1 `(,@(env-get-code i) ,@(unbox-rep-code rep (lvar-type fv))
                                      ,@(rep-local-set-code fv idx))
                               (set-loc fv (cons :local idx))))))
      `(,@(if (<= n +clo-env-max+)
              `((:local.get 0) (:ref.cast! ,type) (:local.set ,env))
              `((:local.get 0) (:ref.cast! :envclov) (:struct.get :envclov ,+clo-env+) (:local.set ,env)))
        ;; the cells of captured and assigned variables: cast once, held in
        ;; locals of their type (not cast at each reference)
        ,@(loop for fv in (lam-free-vars lam) for i from 0
                when (boxed-p fv)
                  append (let ((idx (new-local '(:ref :null :cell))))
                           (prog1 `(,@(env-get-code i) (:ref.cast :cell) (:local.set ,idx))
                             (set-loc fv (cons :cell idx)))))
        ,@free-reps))))

(defun param-setup-code (lv local-idx)
  "LV was passed in LOCAL-IDX.  Returns (values code special-p); a special
parameter has no code here: FINISH-BODY binds it."
  (let ((check (param-check-code lv local-idx)))
    (multiple-value-bind (code sp) (param-setup-code-1 lv local-idx)
      (values (append check code) sp))))

(defun param-setup-code-1 (lv local-idx)
  (cond ((lvar-special lv)
         (values nil (cons (lvar-name lv) local-idx)))
        ((boxed-p lv)
         (let ((idx (new-local '(:ref :null :cell))))
           (set-loc lv (cons :cell idx))
           (values `((:local.get ,local-idx) (:struct.new :cell) (:local.set ,idx)) nil)))
        ((lvar-rep lv)
         ;; known to be a fixnum / double (in a specialized clone): unboxed copy
         (let ((idx (new-rep-local (lvar-rep lv))))
           (set-loc lv (cons :local idx))
           (values `((:local.get ,local-idx) ,@(unbox-rep-code (lvar-rep lv) (lvar-type lv))
                     ,@(rep-local-set-code lv idx))
                   nil)))
        (t (note-dual-var lv t)          ; integer accumulators (typed.lisp)
           (set-loc lv (cons :local local-idx))
           (values (struct-shadow-init-code lv local-idx) nil))))

(defvar clysm-sys::*interrupt-checks* nil
  "When true, code compiled in the image (live mode) checks for interrupts
on function entry and at the head of loops (POLL-CODE).")

(defun poll-code ()
  "An interrupt check: a counter shared by the modules of the image is
decremented, and when it is negative, :POLL-SLOW calls
SYSTEM::%INTERRUPT-POLL (a development environment's chance to interrupt the
computation, e.g. SWANK's)."
  (when (and *live-mode* clysm-sys::*interrupt-checks*)
    '((:global.get :poll-count) (:i32.const 1) :i32.sub (:global.set :poll-count)
      (:global.get :poll-count) (:i32.const 0) :i32.lt_s
      (:if nil nil ((:call :poll-slow))))))

(defun finish-body (lam prologue specials)
  "Compile LAM's body after PROLOGUE.  SPECIALS: the special parameters, a
list of (symbol . local), which are bound around the body."
  (setq prologue (append (poll-code) prologue))
  (if specials
      (let ((saved (new-local :i32)) (res (new-local)))
        (values `(,@prologue
                  ,@(bind-specials-code saved specials)
                  ,@(with-dynamic-scope () (cg (lam-body lam) t nil))
                  (:local.set ,res)
                  ,@(unbind-specials-code saved (mapcar #'car specials))
                  (:local.get ,res))))
      `(,@prologue ,@(let ((*tail-ok* t)) (cg (lam-body lam) t t)))))

(defun compile-lam (lam &aux (*catching-tag* nil))
  (inline-calls lam)
  (refine-nonlocal lam)
  (infer-lam lam)
  (cond ((lam-simple-p lam) (compile-simple-lam lam))
        ((lam-keyed-p lam) (compile-keyed-lam lam))
        ((lam-rest-only-p lam) (compile-rest-lam lam))
        (t (compile-complex-lam lam))))

(defun compile-simple-lam (lam)
  (compile-simple-main lam (list :main (lam-id lam)) (lam-display-name lam) nil (entry-version-code lam))
  (let ((n (length (lam-req lam))))
    ;; entryN adapter
    (add-func *m* (list :entn (lam-id lam)) :fN
              :body `((:local.get 1) :array.len (:i32.const ,n) :i32.ne
                      (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :argerr-n)))
                      (:local.get 0)
                      ,@(loop for i below n
                              append `((:local.get 1) (:i32.const ,i) (:array.get :vector)))
                      (:return_call (:main ,(lam-id lam)))))))

(defun compile-simple-main (lam name display &optional flag entry-code)
  "ENTRY-CODE: instructions run first (they may leave by a tail call)."
  (let* ((n (length (lam-req lam)))
         (*fs* (make-fstate :lam lam :nparams (1+ n)))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam flag)) (*catchers* nil)
         (prologue (append entry-code (setup-free-vars lam)))
         (any-special nil))
    (loop for lv in (lam-req lam) for i from 1
          do (multiple-value-bind (code sp) (param-setup-code lv i)
               (setf prologue (append prologue code))
               (when sp (setf any-special (append any-special (list sp))))))
    (let ((body (wrap-fexit (finish-body lam prologue any-special))))
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
  (and (null (lam-free-vars lam)) (spec-body-cloneable-p lam)))

(defun entry-clone-free-vars (lam)
  "The free variables of LAM that its entry version may take as fixnums or
doubles: neither special nor assigned (their value is that of the closure
for the whole call)."
  (remove-if (lambda (fv) (or (lvar-special fv) (boxed-p fv) (lvar-mutated fv))) (lam-free-vars lam)))

(defun entry-cloneable-p (lam)
  "LAM may have an entry version: a function without free variables, or a
local function or closure (whose free variables are tested on entry too)."
  (and (or (lam-req lam) (entry-clone-free-vars lam))
       (or (null (lam-free-vars lam)) (<= (length (lam-free-vars lam)) +clo-env-max+))
       (spec-body-cloneable-p lam)))

(defun spec-body-cloneable-p (lam)
  (multiple-value-bind (v found) (gethash lam *spec-cloneable*)
    (if found v
        (setf (gethash lam *spec-cloneable*)
              (and (lam-simple-p lam)
                   (notany #'lvar-special (lam-req lam))
                   (let ((n 0))
                     (labels ((walk (node)
                                (when (> (incf n) *spec-max-nodes*) (return-from spec-body-cloneable-p
                                                                       (setf (gethash lam *spec-cloneable*) nil)))
                                (when (and (member (car node) '(:lambda :flet :labels :ltv))
                                           ;; (the class of a structure: the accessors' test; evaluated
                                           ;; once more in the copy, with the same value)
                                           (not (and (eq (car node) :ltv) (struct-class-node-name node))))
                                  (return-from spec-body-cloneable-p (setf (gethash lam *spec-cloneable*) nil)))
                                (map-subnodes #'walk node)))
                       (walk (lam-body lam))
                       t)))))))

(defun arg-class (node)
  (let ((ty (node-ty node)))
    (cond ((ty-fixnum-p ty) :fixnum)
          ((ty-double-p ty) :double)
          ;; an integer within 64 bits computed as an i64 (a mask, a
          ;; variable declared (UNSIGNED-BYTE 32)): passed as one (not one
          ;; read from data, which the callee would make again)
          ((and (ty-int64-p ty) (i64-natural-node-p node)) :int64)
          ;; a two-dimensional simple array of element type T
          ((eq (ty-md2-kind ty) :vector) :md2))))

(defun i64-natural-node-p (node)
  "NODE's value is computed as an i64 (not read as an object)."
  (case (car node)
    (:const t)
    (:lref (let ((lv (second node))) (or (eq (lvar-rep lv) :i64) (dual-local lv))))
    (:the (i64-natural-node-p (third node)))
    ((:prim :call) (or (member (node-op-name node) *i64-arith-ops*)
                       (member (node-op-name node) '(mod rem floor truncate ceiling round))
                       ;; a call of a copy returning an i64
                       (and (eq (car node) :call)
                            (let* ((lam (known-global-lam (second node))) (sig (raw-call-sig lam (cddr node))))
                              (and sig (eq (raw-clone-rrep lam sig) :i64))))))
    (t nil)))

(defun class-ty (class)
  (ecase class
    (:fixnum *ty-fixnum*)
    (:double (mkty +k-double+))
    (:int64 (mkty +k-int+ (- (expt 2 63)) (1- (expt 2 63))))
    (:md2 (md2-ty :vector))
    ;; (an object the caller proved of the parameter's declared type: the
    ;; declaration gives the type)
    ((nil :checked) *ty-top*)))

;;; A function with a loop or a recursive call whose parameters are used as
;;; numbers is compiled twice: the general version tests the arguments on entry
;;; and, when they are fixnums (or doubles, where the parameter is used with
;;; double constants), continues in a copy compiled for those types.  This
;;; gives the specialized code to callers that do not know the types (calls
;;; through FUNCALL, from code compiled separately at the REPL or by LOAD).
(defparameter *numeric-ops*
  '(+ - * / 1+ 1- < > <= >= = /= zerop plusp minusp evenp oddp logand logior logxor ash mod rem floor
    truncate ceiling round min max abs sqrt float
    clysm-sys::%fx+ clysm-sys::%fx- clysm-sys::%fx* clysm-sys::%fx< clysm-sys::%fx> clysm-sys::%fx<=
    clysm-sys::%fx>= clysm-sys::%fx=))
(defparameter *integer-only-ops* '(evenp oddp logand logior logxor ash mod rem))
(defparameter *index-ops*
  ;; (operator . positions of its subscript arguments)
  '((clysm-sys::%aref1 1) (clysm-sys::%aset1 1) (svref 1) (clysm-sys::%svset 1) (char 1) (schar 1)
    (clysm-sys::%charset 1) (clysm-sys::%scharset 1) (clysm-sys::%aref2 1 2) (clysm-sys::%aset2 1 2)
    (clysm-sys::%svref 1) (clysm-sys::%set-svref 1) (row-major-aref 1) (nth 0) (nthcdr 0) (elt 1)))

(defun entry-version-signature (lam &optional doubles-p)
  "The classes (:FIXNUM, :DOUBLE or NIL per required parameter) of the
specialized version of LAM that its general version tries on entry, or NIL.
DOUBLES-P: the second version tried, with doubles for the parameters used as
numbers but not as integers (not with integer-only operations, as indexes,
or compared with a variable known to be an integer: a count)."
  (when (and (entry-cloneable-p lam) (or (lam-name lam) (lam-free-vars lam)))
    (let* ((params (append (lam-req lam) (entry-clone-free-vars lam)))
           (alias (make-hash-table :test 'eq))       ; variable -> the parameter it is a copy of
           (ints (make-hash-table :test 'eq)) (doubles (make-hash-table :test 'eq))
           (counts (make-hash-table :test 'eq))
           (nums (make-hash-table :test 'eq)) (arrays (make-hash-table :test 'eq))
           (self (lam-name lam)) (loop-p nil) (self-call nil)
           (double-vars (make-hash-table :test 'eq)))  ; variables bound to a double (as DOUBLE-P sees)
      (dolist (p params) (setf (gethash p alias) p))
      (labels ((param-of (node) (and (eq (car node) :lref) (gethash (second node) alias)))
               ;; NODE is a double: a constant, a slot of a structure declared
               ;; DOUBLE-FLOAT, an operation on such, a variable bound to one
               (double-p (node)
                 (case (car node)
                   (:const (typep (second node) 'double-float))
                   (:lref (gethash (second node) double-vars))
                   ((:prim :call)
                    (let ((name (node-op-name node)) (args (cddr node)))
                      (cond ((eq name 'clysm-sys::%struct-tref)
                             (let ((i (third args)) (kinds (fourth args)))
                               (and (eq (car i) :const) (eq (car kinds) :const) (integerp (second i))
                                    (listp (second kinds)) (eq (nth (second i) (second kinds)) :d))))
                            ((member name '(+ - * / sqrt abs 1+ 1-)) (some #'double-p args)))))))
               (walk (node)
                 (case (car node)
                   ;; a variable bound to a parameter, even one assigned later
                   ;; (the counter of (LOOP FOR I FROM START ...)): its uses as a
                   ;; number count as uses of the parameter
                   (:let (dolist (b (second node))
                           (let ((p (param-of (cdr b))))
                             (when (and p (not (lvar-special (car b))))
                               (setf (gethash (car b) alias) p))
                             (when (and (not (lvar-mutated (car b))) (double-p (cdr b)))
                               (setf (gethash (car b) double-vars) t)))))
                   (:tagbody (setf loop-p t))
                   ((:prim :call)
                    (let ((name (node-op-name node)) (args (cddr node)))
                      (when (and (eq (car node) :call) (eq name self)) (setf self-call t))
                      (when (and (member name '(< > <= >= = /=)) (= (length args) 2))
                        ;; compared with an integer variable (a loop counter): a count
                        (loop for (x y) in (list args (reverse args))
                              do (let ((p (param-of x)))
                                   (when (and p (eq (car y) :lref) (not (param-of y))
                                              (ty-only-p (node-ty y) +k-int+))
                                     (setf (gethash p counts) t)))))
                      (when (member name *numeric-ops*)
                        (let ((double-const (some #'double-p args)))
                          (dolist (a args)
                            (let ((p (param-of a)))
                              (when p
                                (setf (gethash p nums) t)
                                (when (member name *integer-only-ops*) (setf (gethash p ints) t))
                                (when double-const (setf (gethash p doubles) t)))))))
                      ;; the array of a two-dimensional access
                      (when (member name '(clysm-sys::%aref2 clysm-sys::%aset2))
                        (let ((p (param-of (first args))))
                          (when p (setf (gethash p arrays) t))))
                      (let ((positions (cdr (assoc name *index-ops*))))
                        (dolist (i positions)
                          (let ((p (and (nth i args) (param-of (nth i args)))))
                            (when p (setf (gethash p nums) t (gethash p ints) t))))))))
                 (map-subnodes #'walk node)))
        (walk (lam-body lam)))
      (when (or loop-p self-call)
        (let ((sig (mapcar (lambda (p)
                             (cond ((lvar-decl p) nil)        ; declared: already typed
                                   ;; a free variable: a number (no array header copied)
                                   ((and (not (member p (lam-req lam))) (not (gethash p nums))) nil)
                                   ;; the array of two-dimensional accesses in a loop: a
                                   ;; simple array of element type T (its storage and
                                   ;; dimensions in locals, loaded once)
                                   ((and loop-p (gethash p arrays) (not (gethash p nums))) :md2)
                                   ((not (gethash p nums)) nil)
                                   ((and (or doubles-p (gethash p doubles))
                                         (not (gethash p ints)) (not (gethash p counts)))
                                    :double)
                                   (t :fixnum)))
                           params)))
          ;; (a free variable's class is that of its value as the closure holds it:
          ;; fixnums and doubles only)
          (setf sig (loop for c in sig for i from 0
                          collect (if (and (>= i (length (lam-req lam))) (eq c :md2)) nil c)))
          ;; (no trailing NILs of free variables: the signature of a function
          ;; without them is as it was)
          (let ((n (length (lam-req lam))))
            (loop while (and (> (length sig) n) (null (car (last sig)))) do (setf sig (butlast sig))))
          (when (and (some #'identity sig) (or (not doubles-p) (member :double sig)))
            sig))))))

(defun entry-version-code (lam)
  "Instructions for the entry of LAM's general version: when the arguments
are of the classes of its specialized version, a tail call of that version."
  (let* ((sig (entry-version-signature lam))
         (sig2 (and sig (entry-version-signature lam t))))
    (loop for s in (remove-duplicates
                    (remove-if-not (lambda (s) (some #'identity s))
                                   (list sig sig2
                                         ;; (the numbers without the arrays: an array that is
                                         ;; not a simple one of element type T)
                                         (substitute nil :md2 sig) (substitute nil :md2 sig2)))
                    :test #'equal :from-end t)
          append (let ((skip (gensym "GENERAL")) (n (length (lam-req lam)))
                       (fvs (entry-clone-free-vars lam)))
                   `((:block ,skip nil
                       ,@(loop for class in s for i from 1
                               for get = (if (<= i n)
                                             `((:local.get ,i))
                                             (closure-field-code lam (nth (- i n 1) fvs)))
                               append (ecase class
                                        (:fixnum `(,@get (:call :fixnump) :i32.eqz (:br_if ,skip)))
                                        (:double `(,@get (:ref.test :double) :i32.eqz (:br_if ,skip)))
                                        (:md2 `(,@get (:call :simple-md2-p) :i32.eqz (:br_if ,skip)))
                                        ((nil) nil)))
                       ,@(loop for i from 0 to n collect `(:local.get ,i))
                       (:return_call ,(spec-clone-name lam s))))))))

(defun closure-field-code (lam fv)
  "Push the value of the free variable FV of LAM from its closure (local 0),
before the closure's environment is set up."
  (let ((i (position fv (lam-free-vars lam))) (type (envclo-type (length (lam-free-vars lam)))))
    `((:local.get 0) (:ref.cast! ,type) (:struct.get ,type ,(+ +clo-env+ i)))))

(defun spec-signature (lam args)
  (when (and (spec-cloneable-p lam) (= (length args) (length (lam-req lam))))
    (let ((sig (call-arg-classes lam args)))
      (when (some #'identity sig) sig))))

(defun call-arg-classes (lam args)
  "The classes of the arguments ARGS of a call of LAM (ARG-CLASS), as a copy
of LAM for them takes them; :CHECKED for an argument of no class whose type
shows it is of the type of LAM's parameter, which then has no test of it
(PARAM-CHECK-CODE): a call of a function with declared parameters from code
that proved them skips the tests."
  (let ((sig (mapcar #'arg-class args)))
    ;; (an array argument only where the function's own version tests it:
    ;; no copy of every function given a two-dimensional array)
    (when (member :md2 sig)
      (let ((own (or (entry-version-signature lam) (mapcar (constantly nil) sig))))
        (setf sig (loop for c in sig for o in own collect (if (and (eq c :md2) (not (eq o :md2))) nil c)))))
    (if (and (lam-safety lam) (not (eql (lam-safety lam) 0)))
        (loop for c in sig for a in args for p in (lam-req lam)
              collect (or c (and (param-proven-p p (node-ty a)) :checked)))
        sig)))

(defun param-proven-p (lv ty)
  "Whether a value of type TY given to the parameter LV needs no test of its declared type."
  (let ((dty (and (lvar-decl lv) (not (lvar-special lv)) (decl-check-ty (lvar-decl lv)))))
    (and dty ty (not (ty-bottom-p ty)) (ty-equal (ty-intersect ty dty) ty))))

(defvar *proven-params* nil
  "The parameters of the copy being compiled whose arguments the callers
proved of their declared types (CALL-ARG-CLASSES): no test in its prologue.")

(defun clone-proven-params (lam sig)
  (loop for lv in (lam-req lam) for class in sig
        when (or (eq class :checked)
                 (and class (not (eq class :checked)) (param-proven-p lv (class-ty class))))
          collect lv))

(defun spec-clone-name (lam sig &optional flag)
  "FLAG: the clone is also a flag clone."
  (let ((key (if flag (list (lam-id lam) sig :flag) (list (lam-id lam) sig))))
    (unless (gethash key *spec-clones*)
      (setf (gethash key *spec-clones*) t)
      (push (list :spec lam sig flag) *pending-lams*))
    (cons :spec key)))

(defun lam-all-lvars (lam)
  (let ((r (copy-list (lam-req lam))))
    (labels ((walk (n)
               (case (car n)
                 (:let (dolist (b (second n)) (push (car b) r)))
                 (:mvbind (dolist (lv (second n)) (push lv r))))
               (map-subnodes #'walk n)))
      (walk (lam-body lam)))
    r))

(defvar *clone-free-reps* nil
  "In an entry version of a closure: (free variable . representation) of
the free variables it takes as fixnums or doubles (copied from the closure
into locals of that representation on entry).")

(defun compile-spec-clone (lam sig flag)
  (inline-calls lam)
  (refine-nonlocal lam)
  (let* ((n (length (lam-req lam)))
         (fv-classes (loop for fv in (entry-clone-free-vars lam) for c in (nthcdr n sig)
                           when c collect (cons fv c)))
         (lvars (append (lam-all-lvars lam) (mapcar #'car fv-classes)))
         (saved (mapcar (lambda (lv) (cons (lvar-type lv) (lvar-rep lv))) lvars)))
    (dolist (lv lvars) (setf (lvar-type lv) nil (lvar-rep lv) nil))
    ;; the free variables tested on entry: of that type in the copy
    (loop for (fv . c) in fv-classes
          do (setf (lvar-type fv) (class-ty c) (lvar-rep fv) (class-rep c)))
    (unwind-protect
         (let ((*ti-shared* (cons *node-types* *ti-analyzed*))
               (*node-types* (make-hash-table :test 'eq))
               (*ti-analyzed* (make-hash-table :test 'eq))
               (*catching-tag* nil)
               (*clone-free-reps* (loop for (fv . c) in fv-classes collect (cons fv (class-rep c))))
               (*spec-self* (list lam (subseq sig 0 n) (spec-clone-name lam sig flag))))
           (infer-lam lam (mapcar #'class-ty (subseq sig 0 n)))
           (let ((*proven-params* (clone-proven-params lam (subseq sig 0 n))))
           (compile-simple-main lam (spec-clone-name lam sig flag)
                                (format nil "~A/~{~(~A~)~^,~}~:[~;/flag~]" (lam-display-name lam)
                                        (mapcar (lambda (c) (or c "t")) sig) flag)
                                flag)))
      (loop for lv in lvars for (ty . rep) in saved
            do (setf (lvar-type lv) ty (lvar-rep lv) rep)))))

(defun compile-rest-lam (lam)
  "Required parameters and &REST: the body (:RMAIN) takes the rest list as
its last parameter; the entries for few arguments make the list."
  (let ((n (length (lam-req lam))) (id (lam-id lam)))
    (let* ((*fs* (make-fstate :lam lam :nparams (+ n 2)))
           (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam)) (*catchers* nil)
           (prologue (setup-free-vars lam))
           (any-special nil))
      (loop for lv in (append (lam-req lam) (list (lam-rest lam))) for i from 1
            do (multiple-value-bind (code sp) (param-setup-code lv i)
                 (setf prologue (append prologue code))
                 (when sp (setf any-special (append any-special (list sp))))))
      (let ((body (wrap-fexit (finish-body lam prologue any-special))))
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
    ;; APPLY: the required arguments are taken from the list, whose rest is
    ;; the rest list (CLHS 3.4.1.3 lets it share structure with the last
    ;; argument of APPLY); a list too short goes to the generic entry, which
    ;; signals the error
    (add-func *m* (list :aentry id) :fA
              :locals '(:eqref)
              :body `((:local.get 0)
                      (:local.get 1) (:local.set 2)
                      ,@(loop repeat n
                              append `((:local.get 2) (:ref.test :cons) :i32.eqz
                                       (:if nil nil ((:local.get 0) (:local.get 1) (:return_call :apply-generic)))
                                       (:local.get 2) (:ref.cast! :cons) (:struct.get :cons 0)
                                       (:local.get 2) (:ref.cast! :cons) (:struct.get :cons 1) (:local.set 2)))
                      (:local.get 2)
                      (:return_call (:rmain ,id)))
              :display (format nil "~A/apply" (lam-display-name lam)))
    (loop for k from n to +max-fixed-args+
          do (add-func *m* (list :rentry id k) (fixed-entry-type k)
                       :body `((:local.get 0)
                               ,@(loop for i from 1 to k collect `(:local.get ,i))
                               ,@(nil-code)
                               ,@(loop repeat (- k n) collect '(:struct.new :cons))
                               (:return_call (:rmain ,id)))))))

(defun compile-complex-lam (lam)
  (let* ((*fs* (make-fstate :lam lam :nparams 2))
         (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam)) (*catchers* nil)
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
                   (emit (bind-new-var-code lv (checked-param-value-code lv value-code)))))
             (arg (i) `((:local.get 1) (:i32.const ,i) (:array.get :vector))))
      (emit prologue
            `((:local.get 1) :array.len (:local.set ,nargs))
            (save-bsp-code saved)
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
                  :body (wrap-fexit body) :locals (reverse (fstate-locals *fs*)) :display (lam-display-name lam))))))

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
           (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam)) (*catchers* nil)
           (prologue (setup-free-vars lam))
           (specials nil)
           (saved (new-local :i32))
           (code nil))
      (labels ((emit (&rest instrs) (setf code (append code (apply #'append instrs))))
               (bind (lv value-code)
                 (if (lvar-special lv)
                     (progn (push lv specials)
                            (emit `((:global.get ,(sym-global (lvar-name lv))) ,@value-code (:call :bind))))
                     (emit (bind-new-var-code lv (checked-param-value-code lv value-code)))))
               (slot (i) `((:local.get ,(1+ i))))
               (bind-defaulted (lv init sv i)
                 (let ((tmp (new-local)) (*tail-ok* nil))
                   (emit `(,@(slot i) (:local.set ,tmp)))
                   (bind lv `((:local.get ,tmp) :ref.is_null
                              (:if nil :eqref ,(let ((*dyn-depth* (+ *dyn-depth* (if specials 1 0)))) (cg init))
                                   ((:local.get ,tmp)))))
                   (when sv (bind sv `((:local.get ,tmp) :ref.is_null :i32.eqz ,@(bool-code)))))))
        (emit prologue
              ;; (the depth is restored only when a parameter is special)
              (when (some (lambda (lv) (and lv (lvar-special lv)))
                          (append (lam-req lam)
                                  (loop for (lv nil sv) in (lam-opt lam) collect lv collect sv)
                                  (loop for (nil lv nil sv) in (lam-keys lam) collect lv collect sv)))
                (save-bsp-code saved)))
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
                    :body (wrap-fexit body) :locals (reverse (fstate-locals *fs*)) :display (lam-display-name lam)))))
    ;; entries for a fixed number of arguments: the missing optionals are null
    (when (lam-opt-only-p lam)
      (loop for k from nreq to (min +max-fixed-args+ npos)
            do (add-func *m* (list :oentry (lam-id lam) k) (fixed-entry-type k)
                         :body `((:local.get 0)
                                 ,@(loop for i from 1 to k collect `(:local.get ,i))
                                 ,@(loop repeat (- size k) collect '(:ref.null :eq))
                                 (:return_call (:kmain ,(lam-id lam)))))))
    ;; with keywords: an entry for each number of arguments up to four that
    ;; finds the keys among its parameters (KEYED-ENTRY-BODY)
    (when (lam-keyp lam)
      (loop for k from nreq to +max-fixed-args+
            when (kentry-p lam k)
            do (add-func *m* (list :kentry (lam-id lam) k) (fixed-entry-type k)
                         :locals (make-list (length (lam-keys lam)) :initial-element :eqref)
                         :body (keyed-entry-body lam k))))))

(defun kentry-p (lam k)
  "LAM (with keyword parameters, no &REST) has an entry of its own for K
arguments: not when the keyword arguments would be odd in number (the
shared stub making the argument vector for the generic entry, which signals
the error)."
  (evenp (max 0 (- k (length (lam-req lam)) (length (lam-opt lam))))))

(defun keyed-entry-body (lam k)
  "The entry of LAM (with keyword parameters, no &REST) for K arguments: the
positional ones and the keyword arguments go to the slots of its keyed main
function, the first of a repeated key winning; :ALLOW-OTHER-KEYS or a key
LAM does not take go to the generic entry (which signals the error or allows
the keys).  (KENTRY-P: the keyword arguments are even in number.)"
  (let* ((nreq (length (lam-req lam))) (nopt (length (lam-opt lam)))
         (npos (+ nreq nopt)) (keys (mapcar #'first (lam-keys lam)))
         (slot0 (1+ k))                 ; the locals of the key slots
         (generic `((:local.get 0)
                    ,@(loop for i from 1 to k collect `(:local.get ,i))
                    (:array.new_fixed :vector ,k)
                    (:return_call (:main ,(lam-id lam)))))
         (nkeyargs (max 0 (- k npos))))
    `(;; the pairs from the last, so that the first of a key wins
          ,@(loop for j from (1- (floor nkeyargs 2)) downto 0
                  for kp = (+ 1 npos (* 2 j))
                  for found = (gensym "KEY")
                  append `((:block ,found nil
                             ,@(loop for kw in keys for i from 0
                                     append `((:local.get ,kp) ,@(const-init-code kw) :ref.eq
                                              (:if nil nil ((:local.get ,(1+ kp)) (:local.set ,(+ slot0 i))
                                                            (:br ,found)))))
                             ,@(if (and (lam-allow-other-keys lam) (not (member :allow-other-keys keys)))
                                   `((:local.get ,kp) ,@(const-init-code :allow-other-keys) :ref.eq :i32.eqz
                                     (:br_if ,found))
                                   nil)
                             ,@generic)))
          (:local.get 0)
          ,@(loop for i from 1 to (min k npos) collect `(:local.get ,i))
          ,@(loop repeat (max 0 (- npos k)) collect '(:ref.null :eq))
          ,@(loop for i from 0 below (length keys) collect `(:local.get ,(+ slot0 i)))
          (:return_call (:kmain ,(lam-id lam))))))

;;; ------------------------------------------------------------
;;; Raw clones: a known function called with fixnum or double arguments
;;; (SPEC-SIGNATURE) is called in a copy that takes those arguments as i32 /
;;; f64 parameters, (:RAW id sig); when its result is a fixnum (a double) on
;;; every path and it cannot return with an exit pending, the copy returns it
;;; as an i32 (an f64).  Such a copy's only tail calls (it would not keep
;;; others) are calls of itself with arguments of the same classes, which
;;; are return_calls of itself.  A call whose value is used as an i32 / f64
;;; (CG-REP) takes the raw result; a call in a tail position of a function
;;; returning an eqref goes to the copy only when that returns an eqref.
;;; ------------------------------------------------------------

(defvar *raw-clones* (make-hash-table :test 'equal) "(lam-id . sig) -> the result rep of its raw clone")
(defvar *raw-tail-calls* nil "in a raw clone returning an i32 / f64: its tail calls of itself")

(defun class-rep (class) (ecase class (:fixnum :i32) (:double :f64) (:int64 :i64) ((:md2 :checked nil) nil)))

(defun raw-clone-name (lam sig rrep)
  (let ((key (list (lam-id lam) sig :raw)))
    (unless (gethash key *spec-clones*)
      (setf (gethash key *spec-clones*) t)
      (push (list :raw lam sig rrep) *pending-lams*))
    (list :raw (lam-id lam) sig)))

(defun raw-func-type (sig rrep closure)
  "CLOSURE: the clone takes its closure (as its first parameter)."
  (let ((key (list* (if closure :rawfn :rawfn-noclo) rrep sig)))
    (unless (gethash key *extra-types*)
      (setf (gethash key *extra-types*) t)
      (add-type *m* key `(:func (,@(when closure '((:ref :closure)))
                                 ,@(mapcar (lambda (c) (or (class-rep c) :eqref)) sig))
                                (,rrep))))
    key))

;;; A raw clone of a function without free variables that does not refer
;;; to itself as a closure takes no closure: its calls pass only the
;;; arguments (the global of the closure was loaded, and kept across the
;;; calls of a recursive function, at each call).
(defun raw-clone-closure-p (lam rrep)
  (or (eq rrep :eqref) (lam-self lam) (lam-free-vars lam)))

(defun raw-closure-code (lam rrep)
  (when (raw-clone-closure-p lam rrep) `((:global.get ,(static-closure-global lam)))))

(defun shift-locals-down (instrs)
  "INSTRS with every local index lowered by one (the closure parameter, local
0, removed; an error if it is used)."
  (labels ((walk (x)
             (cond ((atom x) x)
                   ((and (member (car x) '(:local.get :local.set :local.tee)) (integerp (second x)))
                    (when (zerop (second x)) (error "codegen: a raw clone uses its closure"))
                    (list* (car x) (1- (second x)) (cddr x)))
                   (t (mapcar #'walk x)))))
    (mapcar #'walk instrs)))

(defmacro with-clone-inference ((lam sig) &body body)
  "Run BODY with LAM's types inferred for arguments of the classes SIG (as
COMPILE-SPEC-CLONE does); the types before are restored after."
  (let ((lvars (gensym "LVARS")) (saved (gensym "SAVED")) (l (gensym "LAM")))
    `(let* ((,l ,lam)
            (,lvars (lam-all-lvars ,l))
            (,saved (mapcar (lambda (lv) (cons (lvar-type lv) (lvar-rep lv))) ,lvars)))
       (dolist (lv ,lvars) (setf (lvar-type lv) nil (lvar-rep lv) nil))
       (unwind-protect
            (let ((*ti-shared* (cons *node-types* *ti-analyzed*))
                  (*node-types* (make-hash-table :test 'eq))
                  (*ti-analyzed* (make-hash-table :test 'eq)))
              (infer-lam ,l (mapcar #'class-ty ,sig))
              ,@body)
         (loop for lv in ,lvars for (ty . rep) in ,saved
               do (setf (lvar-type lv) ty (lvar-rep lv) rep))))))

(defun raw-tail-self-calls (lam sig)
  "With LAM's types inferred for SIG: the calls of LAM itself in tail
positions of its body, or :FAIL when a tail position holds another call (or
one with arguments of other classes), or a form that a raw result cannot
leave through (a binding of a special variable, a CATCH...).  Second value:
the other forms in tail positions (whose values are the function's)."
  (let ((found nil) (leaves nil))
    (labels ((fail () (return-from raw-tail-self-calls :fail))
             (forbidden (n)
               (when (member (car n) '(:catch :unwind-protect :progv :throw)) (fail))
               (map-subnodes #'forbidden n))
             (returns-to (blk n values)
               (when (and (eq (car n) :return-from) (eq (second n) blk))
                 (when values (push (third n) leaves))
                 (tail (third n) nil))
               (map-subnodes (lambda (x) (returns-to blk x values)) n))
             (no-fall-p (n)
               ;; N never ends normally (a loop's TAGBODY left by RETURN-FROM)
               (case (car n)
                 ((:go :return-from :throw) t)
                 (:progn (no-fall-p (car (last n))))
                 ((:let :the) (no-fall-p (third n)))
                 (:if (and (no-fall-p (third n)) (no-fall-p (fourth n))))
                 (:tagbody (let ((last-seg (car (last (third n)))))
                             (and (cdr last-seg) (no-fall-p (car (last last-seg))))))))
             ;; TOP: a tail position of the function (else of a form that
             ;; CG-REP compiles as a whole, where a call is not a tail call)
             (tail (n top)
               (case (car n)
                 (:if (tail (third n) top) (tail (fourth n) top))
                 (:progn (tail (car (last n)) top))
                 (:let (when (some (lambda (b) (lvar-special (car b))) (second n)) (fail))
                  (tail (third n) top))
                 (:the (tail (third n) top))
                 (:call (let ((callee (known-global-lam (second n))))
                          (cond ((not top) (fail))
                                ((and (eq callee lam)
                                      (= (length (cddr n)) (length sig))
                                      (equal (call-arg-classes lam (cddr n)) sig))
                                 (push n found))
                                ;; a call of another function whose raw clone
                                ;; returns a fixnum or a double: a return_call
                                ;; of it when that is what this one returns
                                ;; (its value, typed by RAW-CALL-RETURN-TY, a leaf)
                                ((and callee (not (eq callee lam))
                                      (let ((csig (raw-call-sig callee (cddr n))))
                                        (and csig (member (raw-clone-rrep callee csig) '(:i32 :f64 :i64)))))
                                 (push n found) (push n leaves))
                                (t (fail)))))
                 ((:lcall :funcall :mvcall :apply) (fail))
                 (:block (let ((by-returns (and top (no-fall-p (third n)))))
                           ;; (its values: those of its RETURN-FROMs when its body does not end)
                           (when (and top (not by-returns)) (push n leaves))
                           (tail (third n) nil)
                           (returns-to (second n) (third n) by-returns)))
                 (:mvbind (when top (push n leaves)) (tail (fourth n) nil))
                 ((:flet :labels) (when top (push n leaves)) (tail (third n) nil))
                 (t (when top (push n leaves))))))
      (forbidden (lam-body lam))
      (tail (lam-body lam) t)
      (values found leaves))))

(defun raw-clone-rrep (lam sig)
  "The result rep of LAM's raw clone for SIG: :I32, :F64 or :EQREF."
  (let* ((key (cons (lam-id lam) sig)) (v (gethash key *raw-clones*)))
    (cond
      ((eq v :in-progress) :eqref)        ; (a call of it in its own analysis)
      (v v)
      (t
        (setf (gethash key *raw-clones*) :in-progress)
        (setf (gethash key *raw-clones*)
              (progn
                (inline-calls lam)
                (refine-nonlocal lam)
                (if (lam-may-exit-p lam)
                    :eqref
                    (with-clone-inference (lam sig)
                      ;; (the values of its tail calls of itself are its values)
                      (multiple-value-bind (calls leaves) (raw-tail-self-calls lam sig)
                        (cond ((or (eq calls :fail) (null leaves)) :eqref)
                              ((every (lambda (n) (ty-fixnum-p (node-ty n))) leaves) :i32)
                              ((every (lambda (n) (ty-double-p (node-ty n))) leaves) :f64)
                              ((every (lambda (n) (ty-int64-p (node-ty n))) leaves) :i64)
                              (t :eqref)))))))))))

(defun raw-call-return-ty (lam tys)
  "During type inference: the type of the value of a call of the known
function LAM with arguments of the types TYS when its raw clone for them
returns a fixnum or a double (RAW-CLONE-RREP), else NIL."
  (when (and (not (eq lam *ti-lam*)) (not (member lam *ti-in-progress*))
             (not *catching-tag*) (= (length tys) (length (lam-req lam)))
             (spec-cloneable-p lam))
    (let ((sig (mapcar (lambda (ty) (cond ((ty-fixnum-p ty) :fixnum) ((ty-double-p ty) :double)
                                          ((ty-int64-p ty) :int64)))
                       tys)))
      (when (some #'identity sig)
        (let ((*ti-in-progress* (cons *ti-lam* *ti-in-progress*)))
          (case (raw-clone-rrep lam sig)
            (:i32 *ty-fixnum*)
            (:f64 (mkty +k-double+))
            (:i64 (class-ty :int64))))))))

(defun raw-call-sig (lam args)
  (and lam (not *catching-tag*) (lam-accepts-p lam (length args))
       (spec-signature lam args)))

(defun raw-args-code (sig args)
  (loop for class in sig for a in args
        append (if (class-rep class) (cg-rep a (class-rep class)) (cg a))))

(defun raw-call-code (lam args mv tail)
  "A call of LAM's raw clone leaving an eqref (CG-GLOBAL-CALL), or NIL."
  (let ((sig (raw-call-sig lam args)))
    (when sig
      (let ((rrep (raw-clone-rrep lam sig)))
        ;; (a tail call returns what the caller returns: an eqref; a raw
        ;; result is the first value only)
        (unless (and (or tail mv) (not (eq rrep :eqref)))
          (ensure-lam-compiled lam)
          (let ((call `(,@(raw-closure-code lam rrep) ,@(raw-args-code sig args)
                        (,(if tail :return_call :call) ,(raw-clone-name lam sig rrep)))))
            (if (eq rrep :eqref)
                (direct-call-checked lam tail call)
                `(,@call ,@(box-rep-code rrep)))))))))

(defun raw-call-rep-code (node rep)
  "NODE, a :CALL whose value is wanted in REP (:I32, :I64, :F64): a call of
the raw clone returning that, or NIL."
  (let* ((lam (known-global-lam (second node))) (args (cddr node))
         (sig (raw-call-sig lam args)))
    (when sig
      (let ((rrep (raw-clone-rrep lam sig)))
        (when (or (eq rrep rep) (and (eq rrep :i32) (eq rep :i64)))
          (ensure-lam-compiled lam)
          (let ((self-tail (and *raw-tail-calls* (gethash node *raw-tail-calls*) (eq rrep rep))))
            `(,@(raw-closure-code lam rrep) ,@(raw-args-code sig args)
              (,(if self-tail :return_call :call) ,(raw-clone-name lam sig rrep))
              ,@(when (and (eq rrep :i32) (eq rep :i64)) '(:i64.extend_i32_s)))))))))

(defun raw-param-setup-code (lv local-idx class)
  "LV was passed in LOCAL-IDX as the raw value of CLASS (or an eqref)."
  (let ((r (class-rep class)))
    (if (and r (lvar-decl lv) (decl-check-ty (lvar-decl lv))
             (let ((cty (class-ty class)) (dty (decl-check-ty (lvar-decl lv))))
               (not (ty-equal (ty-intersect cty dty) cty))))
        ;; (a value of the class that may not be of the declared type: tested boxed)
        (let ((tmp (new-local)))
          `((:local.get ,local-idx) ,@(box-rep-code r) (:local.set ,tmp)
            ,@(param-check-code lv tmp)
            ,@(raw-param-setup-code-1 lv local-idx class)))
        (raw-param-setup-code-1 lv local-idx class))))

(defun raw-param-setup-code-1 (lv local-idx class)
  (let ((r (class-rep class)))
    (cond ((null r) (values (param-setup-code lv local-idx)))
          ((eq (lvar-rep lv) r)
           (let ((idx (new-rep-local r)))
             (set-loc lv (cons :local idx))
             `((:local.get ,local-idx) ,@(rep-local-set-code lv idx))))
          ((lvar-rep lv)
           (let ((idx (new-rep-local (lvar-rep lv))))
             (set-loc lv (cons :local idx))
             `((:local.get ,local-idx) ,@(box-rep-code r) ,@(unbox-rep-code (lvar-rep lv) (lvar-type lv))
               ,@(rep-local-set-code lv idx))))
          (t
           (let ((idx (new-local)))
             (set-loc lv (cons :local idx))
             ;; (an integer accumulator gets its i64 local in this function,
             ;; or none: *DUAL-VARS* may hold that of another compiled before)
             (note-dual-var lv t)
             `((:local.get ,local-idx) ,@(box-rep-code r) (:local.set ,idx)))))))

(defun compile-raw-clone (lam sig rrep)
  (inline-calls lam)
  (refine-nonlocal lam)
  (with-clone-inference (lam sig)
    (let* ((n (length (lam-req lam)))
           (*catching-tag* nil)
           (*fs* (make-fstate :lam lam :nparams (1+ n)))
           (*dyn-depth* 0) (*uwp-depth* 0) (*uwp-frames* nil) (*fexit* (make-fexit lam nil)) (*catchers* nil)
           (*raw-tail-calls* (unless (eq rrep :eqref)
                               (let ((h (make-hash-table :test 'eq)) (calls (raw-tail-self-calls lam sig)))
                                 (when (eq calls :fail)
                                   (error "raw clone of ~S: its tail calls changed" (lam-name lam)))
                                 (dolist (c calls h) (setf (gethash c h) t)))))
           (prologue (setup-free-vars lam)))
      (let ((*proven-params* (clone-proven-params lam sig)))
        (loop for lv in (lam-req lam) for i from 1 for class in sig
              do (setf prologue (append prologue (raw-param-setup-code lv i class)))))
      (let ((body (if (eq rrep :eqref)
                      (wrap-fexit (finish-body lam prologue nil))
                      (let ((code `(,@(poll-code) ,@prologue ,@(cg-rep (lam-body lam) rrep))))
                        (destructuring-bind (blk tag val ctx) *fexit*
                          (declare (ignore tag val))
                          (when (or (pctx-used ctx) (tree-member `(:br ,(blk-label blk)) code))
                            (error "raw clone of ~S: an exit may be pending" (lam-name lam))))
                        code))))
        (add-func *m* (list :raw (lam-id lam) sig) (raw-func-type sig rrep (raw-clone-closure-p lam rrep))
                  :locals (reverse (fstate-locals *fs*))
                  :body (if (raw-clone-closure-p lam rrep) body (shift-locals-down body))
                  :display (format nil "~A/raw~{,~(~A~)~}~@[->~(~A~)~]" (lam-display-name lam)
                                   (mapcar (lambda (c) (or c "t")) sig) (unless (eq rrep :eqref) rrep)))))))
