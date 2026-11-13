;;;; prims.lisp — inline primitives and hand-written wasm runtime helpers

(in-package :clysm)

;;; ------------------------------------------------------------
;;; Primitive table
;;; ------------------------------------------------------------

(defstruct prim name nargs kind fn)
(defvar *prims* (make-hash-table :test 'eq))

(defun find-primitive (name nargs)
  (let ((entries (and (symbolp name) (gethash name *prims*))))
    (find-if (lambda (p) (or (eql (prim-nargs p) nargs) (eq (prim-nargs p) :any))) entries)))

(defmacro defprim (name args kind &body body)
  "Define a primitive.  BODY receives the arg instruction lists bound to ARGS
and returns instructions.  KIND is :value (eqref) or :test (i32)."
  (let ((argv (gensym)))
    `(let ((p (make-prim :name ',name :nargs ,(if (eq args :any) :any (length args)) :kind ,kind
                         :fn (lambda (,argv)
                               (declare (ignorable ,argv))
                               ,(if (eq args :any)
                                    `(let ((args ,argv)) (declare (ignorable args)) ,@body)
                                    `(destructuring-bind ,args ,argv ,@body))))))
       (setf (gethash ',name *prims*)
             (cons p (remove ,(if (eq args :any) :any (length args)) (gethash ',name *prims*)
                             :key #'prim-nargs)))
       ',name)))

;;; Nested integer arithmetic whose leaves are variables and integer constants
;;; is first tried in i64 (no intermediate boxes); when a leaf is not a small
;;; integer or a step overflows, the usual generic code runs.
(defparameter *fusable-ops* '((+ . :add) (- . :sub) (* . :mul) (logand . :and) (logior . :or) (logxor . :xor)))
(defvar *no-fuse* nil)

(defun fusable-op-node-p (node)
  (and (eq (car node) :prim) (assoc (prim-name (second node)) *fusable-ops*) (= (length (cddr node)) 2)))
(defun fused-leaf-p (node)
  (or (eq (car node) :lref)
      (and (eq (car node) :const) (integerp (second node)) (typep (second node) '(signed-byte 62)))))
(defun fusable-tree-p (node)
  (and (fusable-op-node-p node)
       (every (lambda (a) (or (fused-leaf-p a) (fusable-tree-p a))) (cddr node))))

(defun fused-i64-code (node fail)
  "Code leaving the i64 value of NODE, branching to FAIL when that is not possible."
  (cond ((eq (car node) :const) `((:i64.const ,(second node))))
        ((and (ty-fixnum-p (node-ty node)) (not (fusable-op-node-p node)))
         `(,@(cg-rep node :i32) :i64.extend_i32_s))
        ((and (ty-fixnum-p (node-ty node)) (i32-op-code node))
         `(,@(i32-op-code node) :i64.extend_i32_s))
        ((eq (car node) :lref)
         (let ((e (new-local)) (v (new-local :i32)))
           `(,@(cg node) (:local.tee ,e) (:ref.test :i31)
             (:if nil :i64
                  ((:local.get ,e) (:ref.cast! :i31) :i31.get_s (:local.tee ,v)
                   (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
                   (:local.get ,v) :i64.extend_i32_s)
                  ((:local.get ,e) (:call :small-int-p) :i32.eqz (:br_if ,fail)
                   (:local.get ,e) (:call :int-i64))))))
        (t
         (let ((x (new-local :i64)) (y (new-local :i64)) (r (new-local :i64))
               (op (cdr (assoc (prim-name (second node)) *fusable-ops*))))
           `(,@(fused-i64-code (third node) fail) (:local.set ,x)
             ,@(fused-i64-code (fourth node) fail) (:local.set ,y)
             ,@(ecase op
                 (:add `((:local.get ,x) (:local.get ,y) :i64.add (:local.tee ,r) (:local.get ,x) :i64.xor
                         (:local.get ,r) (:local.get ,y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if ,fail)))
                 (:sub `((:local.get ,x) (:local.get ,y) :i64.sub (:local.tee ,r) (:local.get ,x) :i64.xor
                         (:local.get ,x) (:local.get ,y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if ,fail)))
                 (:mul `((:local.get ,x) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if ,fail)
                         (:local.get ,y) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if ,fail)
                         (:local.get ,x) (:local.get ,y) :i64.mul (:local.set ,r)
                         (:local.get ,x) :i64.eqz :i32.eqz
                         (:if nil nil ((:local.get ,r) (:local.get ,x) :i64.div_s (:local.get ,y) :i64.ne
                                       (:br_if ,fail)))))
                 (:and `((:local.get ,x) (:local.get ,y) :i64.and (:local.set ,r)))
                 (:or `((:local.get ,x) (:local.get ,y) :i64.or (:local.set ,r)))
                 (:xor `((:local.get ,x) (:local.get ,y) :i64.xor (:local.set ,r))))
             (:local.get ,r))))))

(defun fused-tree-node-p (node)
  (and (not *no-fuse*) (eq (car node) :prim) (assoc (prim-name (second node)) *fusable-ops*)
       (fusable-tree-p node) (some #'fusable-op-node-p (cddr node))))

(defun fused-code (node rep)
  "NODE (a fused tree) computed in i64 and left as REP (:eqref, or :i32 when
its type is a fixnum), the generic code when that fails."
  (let ((done (gensym "FUSED")) (fail (gensym "SLOW")) (r (new-local :i64)))
    `((:block ,done ,rep
        (:block ,fail nil
          ,@(fused-i64-code node fail)
          ,@(if (or (eq rep :i32) (ty-fixnum-p (node-ty node)))
                `(:i32.wrap_i64 ,@(when (eq rep :eqref) '(:ref.i31)))
                `((:local.tee ,r) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                  (:if nil :eqref ((:local.get ,r) :i32.wrap_i64 :ref.i31) ((:local.get ,r) (:call :i64-int)))))
          (:br ,done))
        ,@(let ((*no-fuse* t)) (cg-prim (second node) (cddr node) nil))
        ,@(when (eq rep :i32) (unbox-rep-code :i32))))))

(defun cg-prim (p args mv)
  (when (fused-tree-node-p `(:prim ,p ,@args))
    (return-from cg-prim
      `(,@(fused-code `(:prim ,p ,@args) :eqref) ,@(when mv (mv-one)))))
  (let ((codes (mapcar #'cg args)))
    (append (funcall (prim-fn p) codes)
            (when (eq (prim-kind p) :test) (bool-code))
            (when (and mv (not (eq (prim-kind p) :mv))) (mv-one)))))

(defun fixnum-code (n) `((:i32.const ,(ldb (byte 32 0) n)) :ref.i31))

;;; ------------------------------------------------------------
;;; Tests
;;; ------------------------------------------------------------

(defprim eq (a b) :test `(,@a ,@b :ref.eq))
(defprim eql (a b) :test `(,@a ,@b (:call :eql)))
(defprim null (x) :test `(,@x ,@(nil-code) :ref.eq))
(defprim not (x) :test `(,@x ,@(nil-code) :ref.eq))
(defprim consp (x) :test `(,@x (:ref.test :cons)))
(defprim atom (x) :test `(,@x (:ref.test :cons) :i32.eqz))
(defprim listp (x) :test `(,@x (:call :listp)))
(defprim symbolp (x) :test `(,@x (:ref.test :symbol)))
(defprim functionp (x) :test `(,@x (:ref.test :closure)))
(defprim characterp (x) :test `(,@x (:call :characterp)))
(defprim clysm-sys::fixnump (x) :test `(,@x (:call :fixnump)))
(defprim clysm-sys::%bignump (x) :test `(,@x (:ref.test :bignum)))
(defprim clysm-sys::%ratiop (x) :test `(,@x (:ref.test :ratio)))
(defprim clysm-sys::%single-float-p (x) :test `(,@x (:ref.test :single)))
(defprim clysm-sys::%double-float-p (x) :test `(,@x (:ref.test :double)))
(defprim clysm-sys::%complexp (x) :test `(,@x (:ref.test :complex)))
(defprim clysm-sys::%simple-string-p (x) :test `(,@x (:ref.test :string)))
(defprim clysm-sys::%simple-vector-p (x) :test `(,@x (:ref.test :vector)))
(defprim clysm-sys::%bytes-p (x) :test `(,@x (:ref.test :bytes)))
(defprim clysm-sys::%bits-p (x) :test `(,@x (:ref.test :bits)))
(defprim clysm-sys::%f64vec-p (x) :test `(,@x (:ref.test :f64vec)))
(defprim clysm-sys::%mdarray-p (x) :test `(,@x (:ref.test :mdarray)))
(defprim clysm-sys::%instancep (x) :test `(,@x (:ref.test :instance)))
(defprim clysm-sys::%packagep (x) :test `(,@x (:ref.test :package)))
(defprim clysm-sys::%foreignp (x) :test `(,@x (:ref.test :foreign)))
(defprim clysm-sys::%cellp (x) :test `(,@x (:ref.test :cell)))
(defprim clysm-sys::%unboundp (x) :test `(,@x :ref.is_null))

;;; ------------------------------------------------------------
;;; Conses
;;; ------------------------------------------------------------

(defprim cons (a b) :value `(,@a ,@b (:struct.new :cons)))
(defprim car (x) :value `(,@x (:call :car)))
(defprim cdr (x) :value `(,@x (:call :cdr)))
(defprim first (x) :value `(,@x (:call :car)))
(defprim rest (x) :value `(,@x (:call :cdr)))
(macrolet ((cxr (name path)
             `(defprim ,name (x) :value
                `(,@x ,@',(loop for c across (reverse path)
                               collect (if (char= c #\A) '(:call :car) '(:call :cdr)))))))
  (cxr caar "AA") (cxr cadr "AD") (cxr cdar "DA") (cxr cddr "DD")
  (cxr caddr "ADD") (cxr cdddr "DDD") (cxr caadr "AAD") (cxr cddar "DDA")
  (cxr cdadr "DAD") (cxr cadar "ADA") (cxr caaar "AAA") (cxr cdaar "DAA")
  (cxr cadddr "ADDD") (cxr cddddr "DDDD")
  (cxr second "AD") (cxr third "ADD") (cxr fourth "ADDD"))
(defprim clysm-sys::%car (x) :value `(,@x (:ref.cast :cons) (:struct.get :cons 0)))
(defprim clysm-sys::%cdr (x) :value `(,@x (:ref.cast :cons) (:struct.get :cons 1)))
(defprim rplaca (c v) :value
  (let ((tmp (new-local)))
    `(,@c (:local.tee ,tmp) (:call :check-cons) ,@v (:struct.set :cons 0) (:local.get ,tmp))))
(defprim rplacd (c v) :value
  (let ((tmp (new-local)))
    `(,@c (:local.tee ,tmp) (:call :check-cons) ,@v (:struct.set :cons 1) (:local.get ,tmp))))
;; (setf car) helpers: return the new value
(defprim clysm-sys::%setcar (c v) :value
  (let ((tmp (new-local)))
    `(,@c (:call :check-cons) ,@v (:local.tee ,tmp) (:struct.set :cons 0) (:local.get ,tmp))))
(defprim clysm-sys::%setcdr (c v) :value
  (let ((tmp (new-local)))
    `(,@c (:call :check-cons) ,@v (:local.tee ,tmp) (:struct.set :cons 1) (:local.get ,tmp))))

;;; ------------------------------------------------------------
;;; Fixnum / numbers
;;; ------------------------------------------------------------

(defun inline-fixnum-op (a b op helper)
  "Inline fixnum fast path; falls back to HELPER."
  (let ((ta (new-local)) (tb (new-local)) (x (new-local :i32)) (y (new-local :i32)))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:local.get ,ta) (:ref.test :i31) (:local.get ,tb) (:ref.test :i31) :i32.and
      (:if nil :eqref
           ((:local.get ,ta) (:ref.cast! :i31) :i31.get_s (:local.tee ,x)
            (:i32.const ,+char-tag+) :i32.add
            (:local.get ,tb) (:ref.cast! :i31) :i31.get_s (:local.tee ,y)
            (:i32.const ,+char-tag+) :i32.add :i32.or
            (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
            (:if nil :eqref
                 ((:local.get ,x) (:local.get ,y) ,op (:local.tee ,x)
                  (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                  (:if nil :eqref ((:local.get ,x) :ref.i31)
                       ((:local.get ,ta) (:local.get ,tb) (:call ,helper))))
                 ((:local.get ,ta) (:local.get ,tb) (:call ,helper))))
           ((:local.get ,ta) (:local.get ,tb) (:call ,helper))))))

(defun inline-fixnum-mul (a b)
  "Inline fixnum product (in i64); falls back to the :mul helper."
  (let ((ta (new-local)) (tb (new-local)) (r (new-local :i64)))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:local.get ,ta) (:ref.test :i31) (:local.get ,tb) (:ref.test :i31) :i32.and
      (:if nil :eqref
           ((:local.get ,ta) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s
            (:local.get ,tb) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s :i64.mul (:local.tee ,r)
            ;; characters are i31s outside the fixnum range: their products are too
            (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
            (:local.get ,ta) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :i32.and
            (:local.get ,tb) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :i32.and
            (:if nil :eqref ((:local.get ,r) :i32.wrap_i64 :ref.i31)
                 ((:local.get ,ta) (:local.get ,tb) (:call :mul))))
           ((:local.get ,ta) (:local.get ,tb) (:call :mul))))))

(defun inline-fixnum-cmp (a b op helper-code &optional swap)
  "Inline comparison when both arguments are i31 (characters never reach here
as the generic helper is used for anything that is not a fixnum pair... the
i31 order is the fixnum order); falls back to HELPER-CODE."
  (let ((ta (new-local)) (tb (new-local)) (x (new-local :i32)) (y (new-local :i32)))
    `(,@a (:local.set ,ta) ,@b (:local.set ,tb)
      (:local.get ,ta) (:ref.test :i31) (:local.get ,tb) (:ref.test :i31) :i32.and
      (:if nil :i32
           ((:local.get ,ta) (:ref.cast! :i31) :i31.get_s (:local.tee ,x)
            (:i32.const ,+char-tag+) :i32.add
            (:local.get ,tb) (:ref.cast! :i31) :i31.get_s (:local.tee ,y)
            (:i32.const ,+char-tag+) :i32.add :i32.or
            (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
            (:if nil :i32
                 ((:local.get ,x) (:local.get ,y) ,op)
                 (,@(if swap `((:local.get ,tb) (:local.get ,ta)) `((:local.get ,ta) (:local.get ,tb)))
                  ,@helper-code)))
           (,@(if swap `((:local.get ,tb) (:local.get ,ta)) `((:local.get ,ta) (:local.get ,tb)))
            ,@helper-code)))))

(defprim + (a b) :value (inline-fixnum-op a b :i32.add :add))
(defprim - (a b) :value (inline-fixnum-op a b :i32.sub :sub))
(defprim * (a b) :value (inline-fixnum-mul a b))
(defprim - (a) :value `(,@a (:call :negate)))
(defprim 1+ (a) :value (inline-fixnum-op a (fixnum-code 1) :i32.add :add))
(defprim 1- (a) :value (inline-fixnum-op a (fixnum-code 1) :i32.sub :sub))
(defprim < (a b) :test (inline-fixnum-cmp a b :i32.lt_s '((:call :lt))))
(defprim > (a b) :test (inline-fixnum-cmp a b :i32.gt_s '((:call :lt)) t))
(defprim <= (a b) :test (inline-fixnum-cmp a b :i32.le_s '((:call :lt) :i32.eqz) t))
(defprim >= (a b) :test (inline-fixnum-cmp a b :i32.ge_s '((:call :lt) :i32.eqz)))
(defprim = (a b) :test (inline-fixnum-cmp a b :i32.eq '((:call :num-eq))))
(defprim /= (a b) :test `(,@a ,@b (:call :num-eq) :i32.eqz))
(defprim zerop (a) :test `(,@a ,@(fixnum-code 0) (:call :num-eq)))
(defprim plusp (a) :test `(,@(fixnum-code 0) ,@a (:call :lt)))
(defprim minusp (a) :test `(,@a ,@(fixnum-code 0) (:call :lt)))

;; unchecked fixnum ops for the runtime: (sys:%fx+ a b) etc. operate on i31 directly
(macrolet ((fxop (name op)
             `(defprim ,name (a b) :value
                `(,@a (:ref.cast! :i31) :i31.get_s ,@b (:ref.cast! :i31) :i31.get_s ,',op :ref.i31)))
           (fxcmp (name op)
             `(defprim ,name (a b) :test
                `(,@a (:ref.cast! :i31) :i31.get_s ,@b (:ref.cast! :i31) :i31.get_s ,',op))))
  (fxop clysm-sys::%fx+ :i32.add) (fxop clysm-sys::%fx- :i32.sub) (fxop clysm-sys::%fx* :i32.mul)
  (fxop clysm-sys::%fxlogand :i32.and) (fxop clysm-sys::%fxlogior :i32.or)
  (fxop clysm-sys::%fxlogxor :i32.xor)
  (fxop clysm-sys::%fxtruncate :i32.div_s) (fxop clysm-sys::%fxrem :i32.rem_s)
  (fxop clysm-sys::%fxash-left :i32.shl) (fxop clysm-sys::%fxash-right :i32.shr_s)
  (fxcmp clysm-sys::%fx< :i32.lt_s) (fxcmp clysm-sys::%fx> :i32.gt_s)
  (fxcmp clysm-sys::%fx<= :i32.le_s) (fxcmp clysm-sys::%fx>= :i32.ge_s)
  (fxcmp clysm-sys::%fx= :i32.eq))

;;; ------------------------------------------------------------
;;; Characters
;;; ------------------------------------------------------------

(defprim char-code (c) :value `(,@c (:call :check-char) (:i32.const ,+char-tag+) :i32.sub :ref.i31))
(defprim code-char (n) :value `(,@n (:ref.cast :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add :ref.i31))
(defprim clysm-sys::%char-code (c) :value `(,@c (:ref.cast :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.sub :ref.i31))
(defprim char= (a b) :test `(,@a (:call :check-char) ,@b (:call :check-char) :i32.eq))

;;; ------------------------------------------------------------
;;; Symbols
;;; ------------------------------------------------------------

(defprim symbol-name (s) :value `(,@s (:call :to-symbol) (:struct.get :symbol ,+sym-name+)))
(defprim symbol-value (s) :value `(,@s (:call :to-symbol) (:call :symbol-value)))
(defprim symbol-plist (s) :value `(,@s (:call :to-symbol) (:struct.get :symbol ,+sym-plist+)))
(defprim clysm-sys::%symbol-function (s) :value
  ;; NIL when not fbound
  (let ((l (gensym)))
    `((:block ,l :eqref ,@s (:call :to-symbol) (:struct.get :symbol ,+sym-function+)
        (:br_on_non_null ,l) ,@(nil-code)))))
(defprim clysm-sys::%symbol-package (s) :value
  (let ((l (gensym)))
    `((:block ,l :eqref ,@s (:call :to-symbol) (:struct.get :symbol ,+sym-package+)
        (:br_on_non_null ,l) ,@(nil-code)))))
(defprim clysm-sys::%set-symbol-package (s p) :value
  (let ((tmp (new-local)))
    `(,@s (:call :to-symbol) ,@p (:local.tee ,tmp) (:call :to-package-or-null)
      (:struct.set :symbol ,+sym-package+) (:local.get ,tmp))))
(defprim clysm-sys::%set-symbol-value (s v) :value
  (let ((tmp (new-local)))
    `(,@s (:call :to-symbol) ,@v (:local.tee ,tmp) (:struct.set :symbol ,+sym-value+) (:local.get ,tmp))))
(defprim clysm-sys::%makunbound (s) :value
  (let ((tmp (new-local)))
    `(,@s (:local.tee ,tmp) (:call :to-symbol) (:ref.null :eq) (:struct.set :symbol ,+sym-value+) (:local.get ,tmp))))
(defprim clysm-sys::%set-symbol-plist (s v) :value
  (let ((tmp (new-local)))
    `(,@s (:call :to-symbol) ,@v (:local.tee ,tmp) (:struct.set :symbol ,+sym-plist+) (:local.get ,tmp))))
(defprim clysm-sys::%set-symbol-function (s f) :value
  (let ((tmp (new-local)) (l (gensym)))
    `(,@s (:call :to-symbol) ,@f (:local.tee ,tmp)
      ,@(nil-code) :ref.eq
      (:if nil (:ref :null :closure) ((:ref.null :closure)) ((:local.get ,tmp) (:ref.cast :closure)))
      (:struct.set :symbol ,+sym-function+) (:local.get ,tmp))))
(defprim boundp (s) :test `(,@s (:call :to-symbol) (:struct.get :symbol ,+sym-value+) :ref.is_null :i32.eqz))
(defprim fboundp (s) :test `(,@s (:call :to-function-name) (:struct.get :symbol ,+sym-function+) :ref.is_null :i32.eqz))
(defprim clysm-sys::%symbol-hash (s) :value `(,@s (:ref.cast :symbol) (:struct.get :symbol ,+sym-hash+) :ref.i31))
(defprim clysm-sys::%symbol-flags (s) :value `(,@s (:call :to-symbol) (:struct.get :symbol ,+sym-flags+) :ref.i31))
(defprim clysm-sys::%set-symbol-flags (s f) :value
  (let ((tmp (new-local)))
    `(,@s (:call :to-symbol) ,@f (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s
      (:struct.set :symbol ,+sym-flags+) (:local.get ,tmp))))
(defprim clysm-sys::%make-symbol (name hash) :value
  `(,@name (:ref.cast :string) (:ref.null :package) (:ref.null :eq) (:ref.null :closure)
    ,@(nil-code) ,@hash (:ref.cast :i31) :i31.get_s (:i32.const 0) (:struct.new :symbol)))

;;; ------------------------------------------------------------
;;; Functions / closures
;;; ------------------------------------------------------------

(defprim clysm-sys::%make-gf-closure (g f1 f2) :value
  ;; the entries of G, except for one and two arguments those of F1 and F2
  ;; (the three closures have the same environment layout)
  (let ((tg (new-local '(:ref :null :closure))) (t1 (new-local '(:ref :null :closure)))
        (t2 (new-local '(:ref :null :closure))))
    `(,@g (:ref.cast :closure) (:local.set ,tg) ,@f1 (:ref.cast :closure) (:local.set ,t1)
      ,@f2 (:ref.cast :closure) (:local.set ,t2)
      (:local.get ,tg) (:struct.get :closure 0)
      (:local.get ,t1) (:struct.get :closure 1)
      (:local.get ,t2) (:struct.get :closure 2)
      (:local.get ,tg) (:struct.get :closure 3)
      (:local.get ,tg) (:struct.get :closure 4)
      (:local.get ,tg) (:struct.get :closure ,+clo-entry-n+)
      (:local.get ,tg) (:struct.get :closure ,+clo-env+)
      (:local.get ,tg) (:struct.get :closure ,+clo-name+)
      (:struct.new :closure))))
(defprim clysm-sys::%closure-name (f) :value `(,@f (:ref.cast :closure) (:struct.get :closure ,+clo-name+)))
(defprim clysm-sys::%set-closure-name (f n) :value
  (let ((tmp (new-local)))
    `(,@f (:ref.cast :closure) ,@n (:local.tee ,tmp) (:struct.set :closure ,+clo-name+) (:local.get ,tmp))))
(defprim clysm-sys::%defun-set (sym f) :value
  ;; set the function cell of SYM to closure F; return SYM
  (let ((tmp (new-local)))
    `(,@sym (:local.tee ,tmp) (:ref.cast :symbol) ,@f (:ref.cast :closure)
      (:struct.set :symbol ,+sym-function+) (:local.get ,tmp))))

;;; ------------------------------------------------------------
;;; Strings and vectors (simple)
;;; ------------------------------------------------------------

(defun idx-code (i) `(,@i (:ref.cast! :i31) :i31.get_s))

(defprim clysm-sys::%make-string (n c) :value
  `(,@c (:call :check-char) (:i32.const ,+char-tag+) :i32.sub ,@(idx-code n) (:array.new :string)))
(defprim clysm-sys::%string-length (s) :value `(,@s (:ref.cast :string) :array.len :ref.i31))
(defprim clysm-sys::%schar (s i) :value
  `(,@s (:ref.cast :string) ,@(idx-code i) (:array.get :string) (:i32.const ,+char-tag+) :i32.add :ref.i31))
(defprim clysm-sys::%set-schar (s i c) :value
  (let ((tmp (new-local)))
    `(,@s (:ref.cast :string) ,@(idx-code i) ,@c (:local.tee ,tmp) (:call :check-char)
      (:i32.const ,+char-tag+) :i32.sub (:array.set :string) (:local.get ,tmp))))
(defprim clysm-sys::%make-simple-vector (n init) :value
  `(,@init ,@(idx-code n) (:array.new :vector)))
(defprim clysm-sys::%svlength (v) :value `(,@v (:ref.cast :vector) :array.len :ref.i31))
(defprim clysm-sys::%svref (v i) :value `(,@v (:ref.cast :vector) ,@(idx-code i) (:array.get :vector)))
(defprim clysm-sys::%set-svref (v i x) :value
  (let ((tmp (new-local)))
    `(,@v (:ref.cast :vector) ,@(idx-code i) ,@x (:local.tee ,tmp) (:array.set :vector) (:local.get ,tmp))))
(defprim clysm-sys::%make-bytes (n) :value `(,@(idx-code n) (:array.new_default :bytes)))
(defprim clysm-sys::%bytes-length (v) :value `(,@v (:ref.cast :bytes) :array.len :ref.i31))
(defprim clysm-sys::%bytes-ref (v i) :value `(,@v (:ref.cast :bytes) ,@(idx-code i) (:array.get_u :bytes) :ref.i31))
(defprim clysm-sys::%set-bytes-ref (v i x) :value
  (let ((tmp (new-local)))
    `(,@v (:ref.cast :bytes) ,@(idx-code i) ,@x (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s
      (:array.set :bytes) (:local.get ,tmp))))
(defprim clysm-sys::%make-bits (n) :value `(,@(idx-code n) (:array.new_default :bits)))
(defprim clysm-sys::%bits-length (v) :value `(,@v (:ref.cast :bits) :array.len :ref.i31))
(defprim clysm-sys::%bits-ref (v i) :value `(,@v (:ref.cast :bits) ,@(idx-code i) (:array.get_u :bits) :ref.i31))
(defprim clysm-sys::%set-bits-ref (v i x) :value
  (let ((tmp (new-local)))
    `(,@v (:ref.cast :bits) ,@(idx-code i) ,@x (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s
      (:array.set :bits) (:local.get ,tmp))))
(defprim clysm-sys::%make-f64vec (n) :value `(,@(idx-code n) (:array.new_default :f64vec)))
(defprim clysm-sys::%f64vec-length (v) :value `(,@v (:ref.cast :f64vec) :array.len :ref.i31))
(defprim clysm-sys::%f64vec-ref (v i) :value
  `(,@v (:ref.cast :f64vec) ,@(idx-code i) (:array.get :f64vec) (:struct.new :double)))
(defprim clysm-sys::%set-f64vec-ref (v i x) :value
  (let ((tmp (new-local)))
    `(,@v (:ref.cast :f64vec) ,@(idx-code i) ,@x (:local.tee ,tmp) (:call :to-f64)
      (:array.set :f64vec) (:local.get ,tmp))))

;; generic array copy for simple arrays of the same kind: (%replace-simple dst dstart src sstart n)
;;; array headers
(defprim clysm-sys::%make-mdarray (data fill dims offset info) :value
  `(,@data ,@fill ,@dims ,@offset ,@info (:struct.new :mdarray)))
(macrolet ((acc (name idx)
             `(progn
                (defprim ,name (a) :value `(,@a (:ref.cast :mdarray) (:struct.get :mdarray ,,idx)))
                (defprim ,(intern (format nil "%SET-~A" (subseq (symbol-name name) 1)) (symbol-package name)) (a v) :value
                  (let ((tmp (new-local)))
                    `(,@a (:ref.cast :mdarray) ,@v (:local.tee ,tmp) (:struct.set :mdarray ,,idx)
                      (:local.get ,tmp)))))))
  (acc clysm-sys::%mdarray-data 0) (acc clysm-sys::%mdarray-fill 1)
  (acc clysm-sys::%mdarray-dims 2) (acc clysm-sys::%mdarray-offset 3) (acc clysm-sys::%mdarray-info 4))

(defun struct-check-code (o c)
  "Push 1 when local O is a structure of class (local C): its layout is C, or C
is the layout's direct superclass, or (out of line) C is in its precedence list."
  (let ((l (new-local)) (cpl (new-local)))
    `((:local.get ,o) (:ref.test :instance)
      (:if nil :i32
           ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 0) (:local.tee ,l) (:local.get ,c) :ref.eq
            (:if nil :i32 ((:i32.const 1))
                 ((:local.get ,l) (:ref.test :instance)
                  (:if nil :i32
                       ((:local.get ,l) (:ref.cast! :instance) (:struct.get :instance 1) (:i32.const 3)
                        (:array.get :vector) (:local.tee ,cpl) (:ref.test :cons)
                        (:if nil :i32
                             ((:local.get ,cpl) (:ref.cast! :cons) (:struct.get :cons 1) (:local.tee ,cpl)
                              (:ref.test :cons)
                              (:if nil :i32
                                   ((:local.get ,cpl) (:ref.cast! :cons) (:struct.get :cons 0) (:local.get ,c) :ref.eq)
                                   ((:i32.const 0))))
                             ((:i32.const 0))))
                       ((:i32.const 0)))
                  (:if nil :i32 ((:i32.const 1)) ((:local.get ,o) (:local.get ,c) (:call :struct-typep))))))
           ((:i32.const 0))))))

;;; structure slot access: inline when the layout is exactly CLASS, then a
;;; search of the layout's class precedence list, else the Lisp function
;;; (which signals the type error)

(defprim clysm-sys::%struct-ref (x class i) :value
  (let ((o (new-local)) (c (new-local)) (k (new-local)))
    `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,k)
      ,@(struct-check-code o c)
      (:if nil :eqref
           ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 1)
            (:local.get ,k) (:ref.cast! :i31) :i31.get_s (:array.get :vector))
           ((:local.get ,o) (:local.get ,c) (:local.get ,k)
            ,@(lisp-call-instrs 'clysm-sys::%struct-ref 3))))))
(defprim clysm-sys::%struct-set (x class i v) :value
  (let ((o (new-local)) (c (new-local)) (k (new-local)) (val (new-local)))
    `(,@x (:local.set ,o) ,@class (:local.set ,c) ,@i (:local.set ,k) ,@v (:local.set ,val)
      ,@(struct-check-code o c)
      (:if nil :eqref
           ((:local.get ,o) (:ref.cast! :instance) (:struct.get :instance 1)
            (:local.get ,k) (:ref.cast! :i31) :i31.get_s (:local.get ,val) (:array.set :vector)
            (:local.get ,val))
           ((:local.get ,o) (:local.get ,c) (:local.get ,k) (:local.get ,val)
            ,@(lisp-call-instrs 'clysm-sys::%struct-set 4))))))

;;; instances
(defprim clysm-sys::%make-instance (layout slots) :value
  `(,@layout ,@slots (:ref.cast :vector) (:i32.const 0) (:struct.new :instance)))
(defprim clysm-sys::%instance-layout (x) :value `(,@x (:ref.cast :instance) (:struct.get :instance 0)))
(defprim clysm-sys::%instance-slots (x) :value `(,@x (:ref.cast :instance) (:struct.get :instance 1)))
(defprim clysm-sys::%set-instance-layout (x l) :value
  (let ((tmp (new-local)))
    `(,@x (:ref.cast :instance) ,@l (:local.tee ,tmp) (:struct.set :instance 0) (:local.get ,tmp))))
(defprim clysm-sys::%set-instance-slots (x l) :value
  (let ((tmp (new-local)))
    `(,@x (:ref.cast :instance) ,@l (:local.tee ,tmp) (:ref.cast :vector) (:struct.set :instance 1) (:local.get ,tmp))))
(defprim clysm-sys::%instance-ref (x i) :value
  `(,@x (:ref.cast :instance) (:struct.get :instance 1) ,@(idx-code i) (:array.get :vector)))
(defprim clysm-sys::%set-instance-ref (x i v) :value
  (let ((tmp (new-local)))
    `(,@x (:ref.cast :instance) (:struct.get :instance 1) ,@(idx-code i) ,@v (:local.tee ,tmp)
      (:array.set :vector) (:local.get ,tmp))))

;;; packages
(defprim clysm-sys::%make-package () :value
  `(,@(loop repeat 8 append (nil-code)) (:struct.new :package)))
(defprim clysm-sys::%package-ref (p i) :value
  ;; field index must be a literal fixnum; use dispatch helper
  `(,@p ,@i (:call :package-ref)))
(defprim clysm-sys::%set-package-ref (p i v) :value
  `(,@p ,@i ,@v (:call :set-package-ref)))

;;; cells
(defprim clysm-sys::%make-cell (v) :value `(,@v (:struct.new :cell)))
(defprim clysm-sys::%cell-value (c) :value `(,@c (:ref.cast :cell) (:struct.get :cell 0)))
(defprim clysm-sys::%set-cell-value (c v) :value
  (let ((tmp (new-local)))
    `(,@c (:ref.cast :cell) ,@v (:local.tee ,tmp) (:struct.set :cell 0) (:local.get ,tmp))))

;;; misc
(defprim clysm-sys::%unbound-marker () :value `((:ref.null :eq)))
(defprim values-list (l) :mv `(,@l (:call :values-list)))
(defprim clysm-sys::%mv-count () :value `((:global.get :mv-count) :ref.i31))
(defprim clysm-sys::%bsp () :value `((:global.get :bsp) :ref.i31))
(defprim clysm-sys::%write-char (c fd) :value
  (let ((tmp (new-local)))
    `(,@fd (:ref.cast :i31) :i31.get_s ,@c (:local.tee ,tmp) (:call :check-char) (:i32.const ,+char-tag+) :i32.sub (:call :host-write-char)
      (:local.get ,tmp))))
(defprim clysm-sys::%read-char () :value `((:call :host-read-char) :ref.i31))
(defprim clysm-sys::%exit (code) :value `(,@code (:ref.cast :i31) :i31.get_s (:call :host-exit) ,@(nil-code)))
(defprim clysm-sys::%apply-vector (f v) :value
  (let ((tmp (new-local '(:ref :null :closure))))
    `(,@f (:call :coerce-function) (:local.tee ,tmp) :ref.as_non_null ,@v (:ref.cast :vector)
      (:local.get ,tmp) (:struct.get :closure ,+clo-entry-n+) (:call_ref :fN))))
(defprim clysm-sys::%identity-hash (x) :value
  `(,@x (:call :identity-hash) :ref.i31))
(defprim clysm-sys::%mv-buf-ref (i) :value `((:global.get :mv-buf) ,@(idx-code i) (:array.get :vector)))

;;; ------------------------------------------------------------
;;; Natives that need AST access: values, list, list*, apply
;;; ------------------------------------------------------------

(defnative values (form env)
  (let ((args (convert-args (cdr form) env)))
    `(:values ,@args)))

(defnative list (form env)
  `(:wasm :eqref ,(convert-args (cdr form) env)
          ((:nil) ,@(loop repeat (length (cdr form)) collect '(:struct.new :cons)))))

(defnative list* (form env)
  (if (null (cdr form))
      (convert '(list*) env)
      `(:wasm :eqref ,(convert-args (cdr form) env)
              (,@(loop repeat (1- (length (cdr form))) collect '(:struct.new :cons))))))

(defnative apply (form env)
  (block apply-native
    (let ((args (cdr form)))
      ;; (apply (lambda (a b) . body) list) binds the elements of the list
      (let ((f (first args)))
        (when (and (= (length args) 2) (consp f)
                   (or (eq (car f) 'lambda)
                       (and (eq (car f) 'function) (consp (second f)) (eq (car (second f)) 'lambda))))
          (let* ((lam (if (eq (car f) 'lambda) f (second f))) (ll (second lam)))
            (when (and (listp ll) (every #'symbolp ll)
                       (notany (lambda (x) (member x lambda-list-keywords)) ll))
              (multiple-value-bind (decls forms) (parse-body (cddr lam))
                (let ((l (gensym "ARGS")))
                  (return-from apply-native
                    (convert `(let ((,l ,(second args)))
                                (clysm-sys::%check-arg-list ,l ,(length ll))
                                (let* ,(loop for v in ll
                                             collect `(,v (prog1 (car ,l) (setq ,l (cdr ,l)))))
                                  (declare ,@decls)
                                  ,@forms))
                             env))))))))
      (if (null (cddr args))
          `(:apply ,@(convert-args args env))
          `(:apply ,@(convert-args (list (first args) `(list* ,@(rest args))) env))))))

;;; ------------------------------------------------------------
;;; Hand-written wasm runtime helpers
;;; ------------------------------------------------------------

(defvar *helpers* nil "list of (name type params locals body-fn)")

(defmacro defhelper (name type (&rest locals) &body body)
  "LOCALS: list of (name type).  Params are referred to by index (0..)."
  `(progn
     (setf *helpers* (remove ',name *helpers* :key #'first :test #'equal))
     (push (list ',name ',type ',locals
                 ;; an element that is a list of instructions is spliced
                 (lambda () (loop for x in (list ,@body)
                                  if (and (consp x) (consp (car x))) append x else collect x)))
           *helpers*)
     ',name))

(defvar *imports*
  '((:host-write-char :ii-v "env" "write_char")
    (:host-read-char :v-i "env" "read_char")
    (:host-exit :i-v "env" "exit")
    (:host-identity-hash :x-i "env" "identity_hash")
    (:host-float-to-chars :di-i "env" "float_to_chars")
    (:host-get-char :i-i "env" "get_char")
    (:host-math1 :di-d "env" "math1")
    (:host-math2 :ddi-d "env" "math2")
    (:host-time :i-d "env" "time")
    (:host-file-open :i-i "env" "file_open")
    (:host-put-char-buf :i-v "env" "put_char_buf")
    (:host-argc :v-i "env" "argc")
    (:host-debug-mode :v-i "env" "debug_mode")
    (:host-backtrace :i-v "env" "backtrace")
    (:host-is-tty :v-i "env" "is_tty")
    (:host-flush :v-v "env" "flush")
    (:host-compile-module :xx-x "env" "compile_module")
    (:js-global :v-x "js" "global")
    (:js-get :xx-x "js" "get")
    (:js-set :xxx-v "js" "set")
    (:js-apply :xxx-x "js" "apply")
    (:js-new :xx-x "js" "construct")
    (:js-array :v-x "js" "array")
    (:js-push :xx-v "js" "push")
    (:js-number :d-x "js" "number")
    (:js-to-number :x-d "js" "to_number")
    (:js-typeof :x-i "js" "typeof")
    (:js-take-string :v-x "js" "take_string")
    (:js-str-len :x-i "js" "str_len")
    (:js-str-char :xi-i "js" "str_char")
    (:js-bool :i-x "js" "bool")
    (:js-special :i-x "js" "special")
    (:js-wrap-fn :x-x "js" "wrap_fn")
    (:js-truthy :x-i "js" "truthy")
    (:js-is-lisp :x-i "js" "is_lisp")
    (:host-arg-len :i-i "env" "arg_len")
    (:host-arg-char :ii-i "env" "arg_char")))

(defun lisp-call-instrs (sym nargs)
  "Call Lisp function SYM with NARGS eqref args already pushed (helper context)."
  `((:global.get ,(sym-global sym)) (:call (:callsym ,nargs))))

(defun i31 (x) `((:local.get ,x) (:ref.cast :i31) :i31.get_s))

(defun fixnum-test-instrs (local)
  "local holds i32 value; push i32 (value is in fixnum range)"
  `((:local.get ,local) (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u))

(defun define-standard-helpers ()
  ;; (:callsym k): (sym, a1..ak) -> eqref, calling through the function cell
  (loop for k from 0 to +max-fixed-args+
        do (let ((k k))
             (setf *helpers* (remove (list :callsym k) *helpers* :key #'first :test #'equal))
             (push (list (list :callsym k)
                         (let ((tn (list :callsym-type k)))
                           (unless (gethash tn *extra-types*)
                             (setf (gethash tn *extra-types*) t)
                             (add-type *m* tn `(:func (,@(make-list k :initial-element :eqref) (:ref :symbol))
                                                      (:eqref))))
                           tn)
                         '((clo (:ref :null :closure)))
                         (lambda ()
                           (let ((l (gensym)))
                             `((:block ,l (:ref :closure)
                                 (:local.get ,k) (:struct.get :symbol ,+sym-function+) (:br_on_non_null ,l)
                                 (:local.get ,k) (:call :undefined-function))
                               (:local.set clo)
                               (:local.get clo) (:ref.as_non_null)
                               ,@(loop for i below k collect `(:local.get ,i))
                               (:local.get clo) (:struct.get :closure ,k)
                               (:return_call_ref ,(fixed-entry-type k))))))
                   *helpers*))))

(defun type-error-instrs (datum-local type-sym)
  `((:local.get ,datum-local) (:global.get ,(sym-global type-sym))
    (:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2))))

(defhelper :car :e-e ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :cons)) (:struct.get :cons 0) :return)
  :drop
  '(:local.get 0) (car (nil-code)) :ref.eq
  `(:if nil :eqref (,@(nil-code)) ,(type-error-instrs 0 'list)))

(defhelper :cdr :e-e ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :cons)) (:struct.get :cons 1) :return)
  :drop
  '(:local.get 0) (car (nil-code)) :ref.eq
  `(:if nil :eqref (,@(nil-code)) ,(type-error-instrs 0 'list)))

(defhelper :check-cons :e-cons ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :cons)) :return)
  :drop
  `(:block x :eqref ,@(type-error-instrs 0 'cons))
  '(:ref.cast :cons))

(defhelper :listp :e-i ()
  '(:local.get 0) '(:ref.test :cons)
  '(:local.get 0) (car (nil-code)) :ref.eq :i32.or)

(defhelper :characterp :e-i ()
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil :i32
        ((:local.get 0) (:ref.cast :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.ge_s)
        ((:i32.const 0))))

(defhelper :fixnump :e-i ((v :i32))
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil :i32
        ((:local.get 0) (:ref.cast :i31) :i31.get_s (:local.set v) ,@(fixnum-test-instrs 'v))
        ((:i32.const 0))))

(defhelper :check-char :e-i ((v :i32))
  ;; returns the i31 payload of a character (2^29+code), or signals type-error
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 0) (:ref.cast :i31) :i31.get_s (:local.tee v) (:i32.const ,+char-tag+) :i32.ge_s
         (:if nil nil ((:local.get v) :return))))
  `(:block x :eqref ,@(type-error-instrs 0 'character))
  :drop
  '(:i32.const 0))

(defhelper :to-symbol :e-sym ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :symbol)) :return)
  :drop
  `(:block x :eqref ,@(type-error-instrs 0 'symbol))
  '(:ref.cast :symbol))

(defhelper :to-function-name :e-sym ()
  ;; symbol or (setf sym) -> symbol holding the function
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :symbol)) :return)
  :drop
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%function-name-symbol)) '(:call (:callsym 1))
  '(:ref.cast :symbol))

(defhelper :to-package-or-null :e-pkgn ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :package)) :return)
  :drop
  '(:ref.null :package))

(defhelper :symbol-value :sym-e ()
  `(:block ok :eqref
     (:local.get 0) (:struct.get :symbol ,+sym-value+) (:br_on_non_null ok)
     (:local.get 0) (:global.get ,(sym-global 'clysm-sys::%unbound-variable-error)) (:call (:callsym 1))))

(defhelper :fdefinition :sym-e ()
  `(:block ok :eqref
     (:local.get 0) (:struct.get :symbol ,+sym-function+) (:br_on_non_null ok)
     (:local.get 0) (:call :undefined-function)))

(defhelper :undefined-function :sym-clo ((name (:ref :null :string)) (i :i32))
  ;; call the Lisp error function; it may return a function (via a restart)
  `(:block fail nil
     (:global.get ,(sym-global 'clysm-sys::%undefined-function-error))
     (:struct.get :symbol ,+sym-function+) (:br_on_null fail)
     :drop
     (:local.get 0) (:global.get ,(sym-global 'clysm-sys::%undefined-function-error)) (:call (:callsym 1))
     (:call :coerce-function) :return)
  ;; no error function yet: print the name on stderr and exit
  '(:local.get 0) `(:struct.get :symbol ,+sym-name+) '(:local.set name)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get name) :array.len :i32.ge_s (:br_if done)
       (:i32.const 2) (:local.get name) (:local.get i) (:array.get :string) (:call :host-write-char)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:i32.const 2) '(:i32.const 10) '(:call :host-write-char)
  '(:i32.const 99) '(:call :host-exit)
  :unreachable)

(defhelper :coerce-function :e-clo ()
  `(:block fail :eqref
     (:local.get 0) (:br_on_cast_fail fail :eqref (:ref :closure)) :return)
  :drop
  `(:block fail2 :eqref
     (:local.get 0) (:br_on_cast_fail fail2 :eqref (:ref :symbol))
     (:call :fdefinition) (:ref.cast :closure) :return)
  :drop
  `(:block x :eqref ,@(type-error-instrs 0 'function))
  '(:call :coerce-function))

(defhelper :eql :ee-i ()
  '(:local.get 0) '(:local.get 1) :ref.eq
  '(:if nil nil ((:i32.const 1) :return))
  ;; both numbers of the same boxed type?
  '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.or
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.test :cons) '(:local.get 0) '(:ref.test :symbol) :i32.or
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:local.get 1) (car (lisp-call-instrs 'clysm-sys::%eql-boxed 2))
  (cadr (lisp-call-instrs 'clysm-sys::%eql-boxed 2))
  (car (nil-code)) :ref.eq :i32.eqz)

;;; integers of up to 56 bits (fixnums and bignums of at most 4 limbs) are
;;; computed with i64 arithmetic
(defhelper :small-int-p :e-i ()
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add
                 (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :return))
  '(:local.get 0) '(:ref.test :bignum)
  '(:if nil nil ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) :ref.is_null
                 (:if nil nil ((:i32.const 1) :return))
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) :array.len
                 (:i32.const 5) :i32.lt_u :return))
  '(:i32.const 0))

(defhelper :int-i64 :e-l ((m (:ref :null :limbs)) (i :i32) (r :i64))
  '(:local.get 0) '(:ref.test :i31)
  '(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s :return))
  '(:local.get 0) '(:ref.cast! :bignum) '(:struct.get :bignum 1) '(:local.tee m) :ref.is_null
  '(:if nil nil ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) :return))
  '(:local.get m) :array.len '(:local.set i)
  '(:i64.const 0) '(:local.set r)
  '(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:local.get r) (:i64.const 14) :i64.shl
       (:local.get m) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.set r)
       (:br again)))
  '(:local.get 0) '(:ref.cast! :bignum) '(:struct.get :bignum 0)
  '(:if nil :i64 ((:i64.const 0) (:local.get r) :i64.sub) ((:local.get r))))

(defhelper :i64-int :l-e ()
  ;; a fixnum, or a bignum holding the value (its limbs are made when needed)
  `(:local.get 0) `(:i64.const ,+char-tag+) :i64.add `(:i64.const ,(* 2 +char-tag+)) :i64.lt_u
  '(:if nil nil ((:local.get 0) :i32.wrap_i64 :ref.i31 :return))
  '(:local.get 0) '(:i64.const 0) :i64.lt_s '(:ref.null :limbs) '(:local.get 0) '(:struct.new :bignum))

;;; the limbs of a bignum built by :i64-int
(defhelper :bignum-limbs :e-e ((b (:ref :null :bignum)) (m :i64) (tt :i64) (n :i32) (a (:ref :null :limbs)) (i :i32))
  '(:local.get 0) '(:ref.cast :bignum) '(:local.tee b) '(:struct.get :bignum 1) '(:local.tee a) :ref.is_null :i32.eqz
  '(:if nil nil ((:local.get a) :ref.as_non_null :return))
  '(:local.get b) '(:struct.get :bignum 2) '(:local.tee m) '(:i64.const 0) :i64.lt_s
  '(:if nil :i64 ((:i64.const 0) (:local.get m) :i64.sub) ((:local.get m))) '(:local.tee m) '(:local.set tt)
  '(:block done nil
     (:loop again nil
       (:local.get tt) :i64.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.add (:local.set n)
       (:local.get tt) (:i64.const 14) :i64.shr_u (:local.set tt)
       (:br again)))
  '(:local.get n) '(:array.new_default :limbs) '(:local.set a)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:local.get m) (:i64.const 16383) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get m) (:i64.const 14) :i64.shr_u (:local.set m)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get b) '(:local.get a) '(:struct.set :bignum 1)
  '(:local.get a) :ref.as_non_null)

;;; OP: 0 + 1 - 2 * 3 logand 4 logior 5 logxor 6 truncate 7 rem 8 compare 9 ash.
;;; Returns NIL when an operand is not a small integer or the result may not fit.
(defhelper :int64-op :eei-e ((x :i64) (y :i64) (r :i64) (v :i32) (m (:ref :null :limbs)))
  (loop for (param dst) in '((0 x) (1 y))
          collect
          `(:block ok nil
             (:block slow nil
               (:local.get ,param) (:ref.test :i31) :i32.eqz (:br_if slow)
               (:local.get ,param) (:ref.cast! :i31) :i31.get_s (:local.tee v)
               (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :i32.eqz
               (:if nil nil (,@(nil-code) :return))
               (:local.get v) :i64.extend_i32_s (:local.set ,dst) (:br ok))
             (:local.get ,param) (:ref.test :bignum) :i32.eqz (:if nil nil (,@(nil-code) :return))
             (:local.get ,param) (:ref.cast! :bignum) (:struct.get :bignum 1) (:local.tee m) :ref.is_null
             (:if nil nil ((:local.get ,param) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.set ,dst)
                           (:br ok)))
             (:local.get m) :array.len (:i32.const 5) :i32.lt_u :i32.eqz (:if nil nil (,@(nil-code) :return))
             (:local.get ,param) (:call :int-i64) (:local.set ,dst)))
  `(:block fail nil
     (:block b9 nil (:block b8 nil (:block b7 nil (:block b6 nil (:block b5 nil (:block b4 nil
       (:block b3 nil (:block b2 nil (:block b1 nil (:block b0 nil
         (:local.get 2) (:br_table b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 fail))
         ;; overflow when both operands differ in sign from the result
         (:local.get x) (:local.get y) :i64.add (:local.tee r) (:local.get x) :i64.xor
         (:local.get r) (:local.get y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if fail)
         (:local.get r) (:call :i64-int) :return)
         (:local.get x) (:local.get y) :i64.sub (:local.tee r) (:local.get x) :i64.xor
         (:local.get x) (:local.get y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if fail)
         (:local.get r) (:call :i64-int) :return)
         (:local.get x) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if fail)
         (:local.get y) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if fail)
         (:local.get x) (:local.get y) :i64.mul (:local.set r)
         (:local.get x) :i64.eqz
         (:if nil nil ((:local.get r) (:call :i64-int) :return))
         (:local.get r) (:local.get x) :i64.div_s (:local.get y) :i64.ne (:br_if fail)
         (:local.get r) (:call :i64-int) :return)
         (:local.get x) (:local.get y) :i64.and (:call :i64-int) :return)
         (:local.get x) (:local.get y) :i64.or (:call :i64-int) :return)
         (:local.get x) (:local.get y) :i64.xor (:call :i64-int) :return)
         (:local.get y) :i64.eqz (:br_if fail)
         (:local.get x) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if fail)
         (:local.get x) (:local.get y) :i64.div_s (:call :i64-int) :return)
         (:local.get y) :i64.eqz (:br_if fail)
         (:local.get x) (:local.get y) :i64.rem_s (:call :i64-int) :return)
         (:local.get x) (:local.get y) :i64.lt_s
         (:if nil :eqref ((:i32.const -1) :ref.i31)
              ((:local.get x) (:local.get y) :i64.gt_s :ref.i31))
         :return)
     ;; ash
     (:local.get y) (:i64.const 0) :i64.ge_s
     (:if nil nil
          ((:local.get y) (:i64.const 62) :i64.gt_s (:br_if fail)
           (:local.get x) (:local.get y) :i64.shl (:local.tee r) (:local.get y) :i64.shr_s
           (:local.get x) :i64.ne (:br_if fail)
           (:local.get r) (:call :i64-int) :return))
     (:local.get x)
     (:i64.const 0) (:local.get y) :i64.sub (:local.tee r) (:i64.const 63) :i64.gt_s
     (:if nil :i64 ((:i64.const 63)) ((:local.get r)))
     :i64.shr_s (:call :i64-int) :return)
  (car (nil-code)))

(defprim clysm-sys::%int64-op (a b op) :value
  `(,@a ,@b ,@op (:ref.cast :i31) :i31.get_s (:call :int64-op)))

;;; logical operations: fixnum pairs inline, then small integers, then the
;;; Lisp functions (which check the argument types)
(macrolet ((logop (name opcode lisp-fn)
             `(defhelper ,name :ee-e ((e :eqref))
                '(:local.get 0) '(:local.get 1) '(:i32.const ,opcode) '(:call :int64-op) '(:local.tee e)
                (car (nil-code)) :ref.eq :i32.eqz '(:if nil nil ((:local.get e) :return))
                '(:local.get 0) '(:local.get 1)
                `(:global.get ,(sym-global ',lisp-fn)) '(:call (:callsym 2)))))
  (logop :logand2 3 logand)
  (logop :logior2 4 logior)
  (logop :logxor2 5 logxor)
  (logop :ash2 9 ash))

(defhelper :ash-fx :ee-e ((x :i64) (c :i32))
  ;; fixnum X, fixnum count
  '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.and
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add
         (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee c) (:i32.const ,+char-tag+) :i32.add :i32.or
         (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:if nil nil
              ((:local.get 0) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s (:local.set x)
               (:local.get c) (:i32.const 0) :i32.lt_s
               (:if nil nil
                    ((:local.get x)
                     (:i32.const 0) (:local.get c) :i32.sub (:local.tee c) (:i32.const 40) :i32.gt_u
                     (:if nil :i64 ((:i64.const 40)) ((:local.get c) :i64.extend_i32_u))
                     :i64.shr_s :i32.wrap_i64 :ref.i31 :return))
               (:local.get c) (:i32.const 33) :i32.lt_u
               (:if nil nil
                    ((:local.get x) (:local.get c) :i64.extend_i32_u :i64.shl (:call :i64-int) :return))))))
  '(:local.get 0) '(:local.get 1) '(:call :ash2))

(defprim logand (a b) :value (inline-fixnum-op a b :i32.and :logand2))
(defprim logior (a b) :value (inline-fixnum-op a b :i32.or :logior2))
(defprim logxor (a b) :value (inline-fixnum-op a b :i32.xor :logxor2))
(defprim ash (a b) :value `(,@a ,@b (:call :ash-fx)))

(defhelper :struct-typep :ee-i ((l :eqref) (cpl :eqref))
  '(:local.get 0) '(:ref.test :instance) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :instance) '(:struct.get :instance 0) '(:local.tee l)
  '(:local.get 1) :ref.eq '(:if nil nil ((:i32.const 1) :return))
  '(:local.get l) '(:ref.test :instance) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
  '(:local.get l) '(:ref.cast! :instance) '(:struct.get :instance 1) '(:i32.const 3) '(:array.get :vector)
  '(:local.set cpl)
  '(:block done nil
     (:loop again nil
       (:local.get cpl) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get cpl) (:ref.cast! :cons) (:struct.get :cons 0) (:local.get 1) :ref.eq
       (:if nil nil ((:i32.const 1) :return))
       (:local.get cpl) (:ref.cast! :cons) (:struct.get :cons 1) (:local.set cpl)
       (:br again)))
  '(:i32.const 0))

(defhelper :memq-tail :ee-e ((l :eqref))
  ;; MEMBER with EQ on a proper list
  '(:local.get 1) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get l) (:ref.cast! :cons) (:struct.get :cons 0) (:local.get 0) :ref.eq
       (:if nil nil ((:local.get l) :return))
       (:local.get l) (:ref.cast! :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  (car (nil-code)))
(defprim clysm-sys::%memq (x l) :value `(,@x ,@l (:call :memq-tail)))

;;; bignum magnitude kernels (base 2^14 limbs, little endian)
(defhelper :mag-len :e-i ((m (:ref :null :limbs)) (n :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee m) :array.len '(:local.set n)
  '(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       (:local.get m) (:local.get n) (:i32.const 1) :i32.sub (:array.get :limbs) (:br_if done)
       (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
       (:br again)))
  '(:local.get n))

(defhelper :mag-mul :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (i :i32) (j :i32) (ai :i32) (carry :i32) (tt :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) :i32.add '(:array.new_default :limbs) '(:local.set r)
  '(:block done nil
     (:loop outer nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) (:local.tee ai)
       (:if nil nil
            ((:i32.const 0) (:local.set carry) (:i32.const 0) (:local.set j)
             (:block idone nil
               (:loop inner nil
                 (:local.get j) (:local.get lb) :i32.ge_u (:br_if idone)
                 (:local.get ai) (:local.get b) (:local.get j) (:array.get :limbs) :i32.mul
                 (:local.get r) (:local.get i) (:local.get j) :i32.add (:array.get :limbs) :i32.add
                 (:local.get carry) :i32.add (:local.set tt)
                 (:local.get r) (:local.get i) (:local.get j) :i32.add
                 (:local.get tt) (:i32.const 16383) :i32.and (:array.set :limbs)
                 (:local.get tt) (:i32.const 14) :i32.shr_u (:local.set carry)
                 (:local.get j) (:i32.const 1) :i32.add (:local.set j)
                 (:br inner)))
             (:local.get r) (:local.get i) (:local.get lb) :i32.add (:local.get carry) (:array.set :limbs)))
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br outer)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-mul-fast (a b) :value `(,@a ,@b (:call :mag-mul)))

;;; a + b (magnitudes)
(defhelper :mag-add :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (n :i32) (i :i32) (s :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) '(:local.get la) '(:local.get lb) :i32.gt_u :select
  '(:i32.const 1) :i32.add '(:local.tee n) '(:array.new_default :limbs) '(:local.set r)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get s)
       (:local.get i) (:local.get la) :i32.lt_u
       (:if nil :i32 ((:local.get a) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.add
       (:local.get i) (:local.get lb) :i32.lt_u
       (:if nil :i32 ((:local.get b) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.add
       (:local.set s)
       (:local.get r) (:local.get i) (:local.get s) (:i32.const 16383) :i32.and (:array.set :limbs)
       (:local.get s) (:i32.const 14) :i32.shr_u (:local.set s)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-add-fast (a b) :value `(,@a ,@b (:call :mag-add)))

;;; a - b (magnitudes, a >= b)
(defhelper :mag-sub :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (i :i32) (d :i32) (borrow :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:array.new_default :limbs) '(:local.set r)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs)
       (:local.get i) (:local.get lb) :i32.lt_u
       (:if nil :i32 ((:local.get b) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.sub
       (:local.get borrow) :i32.sub (:local.tee d)
       (:i32.const 0) :i32.lt_s (:local.set borrow)
       (:local.get r) (:local.get i)
       (:local.get d) (:local.get borrow) (:i32.const 14) :i32.shl :i32.add (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-sub-fast (a b) :value `(,@a ,@b (:call :mag-sub)))

(defhelper :struct-typep-parent :ee-i ((cpl :eqref))
  ;; the layout's direct superclass first (included structures), then the full search
  '(:local.get 0) '(:ref.test :instance) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :instance) '(:struct.get :instance 0) '(:ref.test :instance) :i32.eqz
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.cast! :instance) '(:struct.get :instance 0) '(:ref.cast! :instance)
  '(:struct.get :instance 1) '(:i32.const 3) '(:array.get :vector) '(:local.tee cpl) '(:ref.test :cons)
  '(:if nil nil
        ((:local.get cpl) (:ref.cast! :cons) (:struct.get :cons 1) (:local.tee cpl) (:ref.test :cons)
         (:if nil nil
              ((:local.get cpl) (:ref.cast! :cons) (:struct.get :cons 0) (:local.get 1) :ref.eq
               (:if nil nil ((:i32.const 1) :return))))))
  '(:local.get 0) '(:local.get 1) '(:call :struct-typep))
(defprim clysm-sys::%structure-typep (x c) :test `(,@x ,@c (:call :struct-typep)))

;;; the simple storage of a one-dimensional array header at offset 0 when I is
;;; below its dimension, else null
(defhelper :md1-data :ei-e ((m (:ref :null :mdarray)) (dims :eqref))
  '(:local.get 0) '(:ref.test :mdarray) :i32.eqz '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get 0) '(:ref.cast! :mdarray) '(:local.tee m) '(:struct.get :mdarray 3) '(:i32.const 0) :ref.i31 :ref.eq
  :i32.eqz '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get m) '(:struct.get :mdarray 2) '(:local.tee dims) '(:ref.test :cons) :i32.eqz
  '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get dims) '(:ref.cast! :cons) '(:struct.get :cons 1) (car (nil-code)) :ref.eq :i32.eqz
  '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get dims) '(:ref.cast! :cons) '(:struct.get :cons 0) '(:ref.test :i31) :i32.eqz
  '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get dims) '(:ref.cast! :cons) '(:struct.get :cons 0) '(:ref.cast! :i31) :i31.get_s
  '(:local.get 1) :i32.gt_u :i32.eqz '(:if nil nil ((:ref.null :eq) :return))
  '(:local.get m) '(:struct.get :mdarray 0))

(defun md1-ref-code (d i strings-only)
  "Helper-body code: when local 0 is an array header over simple storage, return
element (local I) of it.  D is a scratch eqref local."
  `((:local.get 1) (:ref.test :i31)
    (:if nil nil
         ((:local.get 0) (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee ,i) (:call :md1-data)
          (:local.tee ,d)
          ,@(unless strings-only
              `((:ref.test :vector)
                (:if nil nil ((:local.get ,d) (:ref.cast! :vector) (:local.get ,i) (:array.get :vector) :return))
                (:local.get ,d)))
          (:ref.test :string)
          (:if nil nil ((:local.get ,d) (:ref.cast! :string) (:local.get ,i) (:array.get :string)
                        (:i32.const ,+char-tag+) :i32.add :ref.i31 :return))))))

(defun md1-set-code (d i c strings-only)
  "Like MD1-REF-CODE for setting element (local I) to local 2."
  `((:local.get 1) (:ref.test :i31)
    (:if nil nil
         ((:local.get 0) (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee ,i) (:call :md1-data)
          (:local.tee ,d)
          ,@(unless strings-only
              `((:ref.test :vector)
                (:if nil nil ((:local.get ,d) (:ref.cast! :vector) (:local.get ,i) (:local.get 2) (:array.set :vector)
                              (:local.get 2) :return))
                (:local.get ,d)))
          (:ref.test :string)
          (:local.get 2) (:ref.test :i31) :i32.and
          (:if nil nil
               ((:local.get 2) (:ref.cast! :i31) :i31.get_s (:local.tee ,c) (:i32.const ,+char-tag+) :i32.ge_s
                (:if nil nil ((:local.get ,d) (:ref.cast! :string) (:local.get ,i)
                              (:local.get ,c) (:i32.const ,+char-tag+) :i32.sub (:array.set :string)
                              (:local.get 2) :return))))))))

;;; CHAR / SCHAR on simple strings inline, else the Lisp functions
(macrolet ((charref (name fallback)
             `(defhelper ,name :ee-e ((s (:ref :null :string)) (i :i32) (d :eqref))
                '(:local.get 1) '(:ref.test :i31)
                `(:if nil nil
                      ((:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set i)
                       (:block notstr :eqref
                         (:local.get 0) (:br_on_cast_fail notstr :eqref (:ref :string)) (:local.tee s)
                         :array.len (:local.get i) :i32.gt_u
                         (:if nil nil ((:local.get s) (:local.get i) (:array.get :string)
                                       (:i32.const ,+char-tag+) :i32.add :ref.i31 :return))
                         (:local.get 0))
                       :drop))
                ,@(when (eq name :char-ref) `((md1-ref-code 'd 'i t)))
                '(:local.get 0) '(:local.get 1)
                `(:global.get ,(sym-global ',fallback)) '(:call (:callsym 2))))
           (charset (name fallback)
             `(defhelper ,name :eee-e ((s (:ref :null :string)) (i :i32) (c :i32) (d :eqref))
                '(:local.get 1) '(:ref.test :i31) '(:local.get 2) '(:ref.test :i31) :i32.and
                `(:if nil nil
                      ((:local.get 2) (:ref.cast! :i31) :i31.get_s (:local.tee c)
                       (:i32.const ,+char-tag+) :i32.ge_s
                       (:if nil nil
                            ((:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set i)
                             (:block notstr :eqref
                               (:local.get 0) (:br_on_cast_fail notstr :eqref (:ref :string)) (:local.tee s)
                               :array.len (:local.get i) :i32.gt_u
                               (:if nil nil ((:local.get s) (:local.get i)
                                             (:local.get c) (:i32.const ,+char-tag+) :i32.sub
                                             (:array.set :string) (:local.get 2) :return))
                               (:local.get 0))
                             :drop))))
                ,@(when (eq name :char-set) `((md1-set-code 'd 'i 'c t)))
                '(:local.get 0) '(:local.get 1) '(:local.get 2)
                `(:global.get ,(sym-global ',fallback)) '(:call (:callsym 3)))))
  (charref :char-ref char)
  (charref :schar-ref schar)
  (charset :char-set clysm-sys::%charset)
  (charset :schar-set clysm-sys::%scharset))
(defprim char (s i) :value `(,@s ,@i (:call :char-ref)))
(defprim schar (s i) :value `(,@s ,@i (:call :schar-ref)))
(defprim clysm-sys::%charset (s i c) :value `(,@s ,@i ,@c (:call :char-set)))
(defprim clysm-sys::%scharset (s i c) :value `(,@s ,@i ,@c (:call :schar-set)))

;;; string hashing (FNV-1a, 29 bits, as STRING-HASH in the compiler) and comparison
(defhelper :string-hash :e-e ((str (:ref :null :string)) (i :i32) (n :i32) (h :i32))
  '(:local.get 0) '(:ref.test :string)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :string) (:local.tee str) :array.len (:local.set n)
         (:i32.const ,(- #x811C9DC5 (expt 2 32))) (:local.set h)
         (:block done nil
           (:loop again nil
             (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
             (:local.get h) (:local.get str) (:local.get i) (:array.get :string) :i32.xor
             (:i32.const ,#x01000193) :i32.mul (:local.set h)
             (:local.get i) (:i32.const 1) :i32.add (:local.set i)
             (:br again)))
         (:local.get h) (:i32.const ,#x1FFFFFFF) :i32.and :ref.i31 :return))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%string-hash)) '(:call (:callsym 1)))
(defprim clysm-sys::%string-hash (s) :value `(,@s (:call :string-hash)))

(defhelper :simple-string= :ee-i ((a (:ref :null :string)) (b (:ref :null :string)) (i :i32) (n :i32))
  '(:local.get 0) '(:ref.cast! :string) '(:local.tee a) :array.len '(:local.tee n)
  '(:local.get 1) '(:ref.cast! :string) '(:local.tee b) :array.len :i32.ne '(:if nil nil ((:i32.const 0) :return))
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :string) (:local.get b) (:local.get i) (:array.get :string) :i32.ne
       (:if nil nil ((:i32.const 0) :return))
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:i32.const 1))
(defprim clysm-sys::%simple-string= (a b) :test `(,@a ,@b (:call :simple-string=)))

;;; arithmetic fast paths
(defun f64-of-param-code (p v fail)
  "Push param P (a double or a fixnum) as an f64, or branch to FAIL."
  `((:local.get ,p) (:ref.test :double)
    (:if nil :f64 ((:local.get ,p) (:ref.cast! :double) (:struct.get :double 0))
         ((:local.get ,p) (:ref.test :i31) :i32.eqz (:br_if ,fail)
          (:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee ,v)
          (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
          (:local.get ,v) :f64.convert_i32_s))))

(defun double-fast-path-code (body)
  "When one argument is a double and the other a double or a fixnum: BODY
with the arguments as f64s in locals DA and DB (it may branch to SLOW)."
  `((:local.get 0) (:ref.test :double) (:local.get 1) (:ref.test :double) :i32.or
    (:if nil nil
         ((:block slow nil
            ,@(f64-of-param-code 0 'v 'slow) (:local.set da)
            ,@(f64-of-param-code 1 'v 'slow) (:local.set db)
            ,@body)))))

(defun double-arith-code (op zero-check)
  (double-fast-path-code
   `((:local.get da) (:local.get db) ,op (:local.tee dr)
     ;; infinite / NaN (and zero for *): the generic function decides
     (:local.get dr) :f64.sub (:f64.const 0) :f64.ne
     ,@(when zero-check '((:local.get dr) (:f64.const 0) :f64.eq :i32.or))
     (:br_if slow)
     (:local.get dr) (:struct.new :double) :return)))

(macrolet ((arith (name op lisp-fn &key mul opcode)
             `(defhelper ,name :ee-e ((x :i32) (y :i32) (r :i64) (e :eqref) (v :i32) (da :f64) (db :f64) (dr :f64))
                '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.and
                `(:if nil nil
                      ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.set x)
                       (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set y)
                       (:local.get x) (:i32.const ,+char-tag+) :i32.add
                       (:local.get y) (:i32.const ,+char-tag+) :i32.add :i32.or
                       (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                       (:if nil nil
                            (,@',(if mul
                                     `((:local.get x) :i64.extend_i32_s (:local.get y) :i64.extend_i32_s
                                       :i64.mul (:local.tee r)
                                       (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                                       (:if nil nil ((:local.get r) :i32.wrap_i64 :ref.i31 :return)))
                                     `((:local.get x) (:local.get y) ,op (:local.tee x)
                                       (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                                       (:if nil nil ((:local.get x) :ref.i31 :return))))))))
                (double-arith-code ,(case opcode (0 :f64.add) (1 :f64.sub) (2 :f64.mul)) ,mul)
                '(:local.get 0) '(:local.get 1) '(:i32.const ,opcode) '(:call :int64-op) '(:local.tee e)
                (car (nil-code)) :ref.eq :i32.eqz '(:if nil nil ((:local.get e) :return))
                '(:local.get 0) '(:local.get 1)
                `(:global.get ,(sym-global ',lisp-fn)) '(:call (:callsym 2)))))
  (arith :add :i32.add clysm-sys::%generic-add :opcode 0)
  (arith :sub :i32.sub clysm-sys::%generic-sub :opcode 1)
  (arith :mul nil clysm-sys::%generic-mul :mul t :opcode 2))

(defhelper :negate :e-e ((x :i32))
  ;; fixnums other than most-negative-fixnum negate inline; (- 0.0) must be -0.0
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee x)
         (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:local.get x) (:i32.const ,(- +char-tag+)) :i32.ne :i32.and
         (:if nil nil ((:i32.const 0) (:local.get x) :i32.sub :ref.i31 :return))))
  '(:local.get 0)
  `(:global.get ,(sym-global 'clysm-sys::%negate)) '(:call (:callsym 1)))

(defhelper :lt :ee-i ((x :i32) (y :i32) (v :i32) (da :f64) (db :f64))
  ;; fast path for two fixnums (characters are i31 too and must go the slow way)
  '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.and
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.set x)
         (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set y)
         (:local.get x) (:i32.const ,+char-tag+) :i32.add
         (:local.get y) (:i32.const ,+char-tag+) :i32.add :i32.or
         (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:if nil nil ((:local.get x) (:local.get y) :i32.lt_s :return))))
  (double-fast-path-code '((:local.get da) (:local.get db) :f64.lt :return))
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%generic-lt)) '(:call (:callsym 2))
  (car (nil-code)) :ref.eq :i32.eqz)

(defhelper :num-eq :ee-i ((x :i32) (y :i32) (v :i32) (da :f64) (db :f64))
  ;; fast path for two fixnums (characters are i31 too and must go the slow way)
  '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.and
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.set x)
         (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set y)
         (:local.get x) (:i32.const ,+char-tag+) :i32.add
         (:local.get y) (:i32.const ,+char-tag+) :i32.add :i32.or
         (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:if nil nil ((:local.get x) (:local.get y) :i32.eq :return))))
  (double-fast-path-code '((:local.get da) (:local.get db) :f64.eq :return))
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%generic-num-eq)) '(:call (:callsym 2))
  (car (nil-code)) :ref.eq :i32.eqz)

;;; dynamic binding
(defhelper :bind :ee-v ((sp :i32) (new (:ref :vector)))
  ;; grow if needed
  '(:global.get :bsp) '(:i32.const 2) :i32.add '(:global.get :bstack) :array.len :i32.gt_u
  '(:if nil nil
        ((:ref.null :eq) (:global.get :bstack) :array.len (:i32.const 2) :i32.mul
         (:array.new :vector) (:local.set new)
         (:local.get new) (:i32.const 0) (:global.get :bstack) (:i32.const 0) (:global.get :bsp)
         (:array.copy :vector :vector)
         (:local.get new) (:global.set :bstack)))
  '(:global.get :bstack) '(:global.get :bsp) '(:local.get 0) '(:array.set :vector)
  '(:global.get :bstack) '(:global.get :bsp) '(:i32.const 1) :i32.add
  '(:local.get 0) '(:ref.cast :symbol) `(:struct.get :symbol ,+sym-value+) '(:array.set :vector)
  '(:local.get 0) '(:ref.cast :symbol) '(:local.get 1) `(:struct.set :symbol ,+sym-value+)
  '(:global.get :bsp) '(:i32.const 2) :i32.add '(:global.set :bsp))

(defhelper :unbind-to :i-v ((sp :i32))
  '(:block done nil
     (:loop again nil
       (:global.get :bsp) (:local.get 0) :i32.le_s (:br_if done)
       (:global.get :bsp) (:i32.const 2) :i32.sub (:local.tee sp) (:global.set :bsp)
       ;; sym.value = old
       (:global.get :bstack) (:local.get sp) (:array.get :vector) (:ref.cast :symbol)
       (:global.get :bstack) (:local.get sp) (:i32.const 1) :i32.add (:array.get :vector)
       (:struct.set :symbol 2)
       (:global.get :bstack) (:local.get sp) (:ref.null :eq) (:array.set :vector)
       (:global.get :bstack) (:local.get sp) (:i32.const 1) :i32.add (:ref.null :eq) (:array.set :vector)
       (:br again))))

(defhelper :progv-bind :ee-v ((syms :eqref) (vals :eqref))
  '(:local.get 0) '(:local.set syms) '(:local.get 1) '(:local.set vals)
  `(:block done nil
     (:loop again nil
       (:local.get syms) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get syms) (:ref.cast :cons) (:struct.get :cons 0)
       (:local.get vals) (:ref.test :cons)
       (:if nil :eqref ((:local.get vals) (:ref.cast :cons) (:struct.get :cons 0)) ((:ref.null :eq)))
       (:call :bind)
       (:local.get syms) (:ref.cast :cons) (:struct.get :cons 1) (:local.set syms)
       (:local.get vals) (:ref.test :cons)
       (:if nil nil ((:local.get vals) (:ref.cast :cons) (:struct.get :cons 1) (:local.set vals)))
       (:br again))))

;;; catch tags
(defhelper :check-catch-tag :e-v ((l :eqref))
  `(:global.get ,(sym-global 'clysm-sys::*catch-tags*)) `(:struct.get :symbol ,+sym-value+) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:local.get 0) :ref.eq
       (:if nil nil (:return))
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%no-catch-error)) '(:call (:callsym 1)) :drop)

;;; multiple values
(defhelper :mv-save :e-e ((v (:ref :null :vector)) (n :i32))
  ;; returns vector [primary, count, v1 ... v(n-1)]
  '(:global.get :mv-count) '(:local.set n)
  ;; n+2 slots: with zero values the count slot is still needed
  '(:ref.null :eq) '(:local.get n) '(:i32.const 2) :i32.add '(:array.new :vector) '(:local.tee v)
  '(:i32.const 0) '(:local.get 0) '(:array.set :vector)
  '(:local.get v) '(:i32.const 1) '(:local.get n) :ref.i31 '(:array.set :vector)
  '(:local.get n) '(:i32.const 1) :i32.gt_s
  '(:if nil nil ((:local.get v) (:i32.const 2) (:global.get :mv-buf) (:i32.const 0)
                 (:local.get n) (:i32.const 1) :i32.sub (:array.copy :vector :vector)))
  '(:local.get v))

(defhelper :mv-restore :e-e ((v (:ref :null :vector)) (n :i32))
  '(:local.get 0) '(:ref.cast :vector) '(:local.tee v)
  '(:i32.const 1) '(:array.get :vector) '(:ref.cast :i31) :i31.get_s '(:local.tee n) '(:global.set :mv-count)
  '(:local.get n) '(:i32.const 1) :i32.gt_s
  '(:if nil nil ((:global.get :mv-buf) (:i32.const 0) (:local.get v) (:i32.const 2)
                 (:local.get n) (:i32.const 1) :i32.sub (:array.copy :vector :vector)))
  '(:local.get v) '(:i32.const 0) '(:array.get :vector))

(defhelper :mv-to-list :e-e ((l :eqref) (i :i32))
  ;; primary + mv-buf -> list
  '(:global.get :mv-count) '(:i32.eqz)
  `(:if nil nil (,@(nil-code) :return))
  (car (nil-code)) '(:local.set l)
  '(:global.get :mv-count) '(:i32.const 1) :i32.sub '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:i32.const 0) :i32.le_s (:br_if done)
       (:global.get :mv-buf) (:local.get i) (:i32.const 1) :i32.sub (:array.get :vector)
       (:local.get l) (:struct.new :cons) (:local.set l)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:br again)))
  '(:local.get 0) '(:local.get l) '(:struct.new :cons))

(defhelper :values-list :e-e ((l :eqref) (i :i32) (first :eqref))
  '(:local.get 0) '(:ref.test :cons) :i32.eqz
  `(:if nil nil ((:local.get 0) ,@(nil-code) :ref.eq :i32.eqz
                 (:if nil nil ((:local.get 0) (:global.get ,(sym-global 'clysm-sys::%values-list-error))
                               (:call (:callsym 1)) :drop))
                 (:i32.const 0) (:global.set :mv-count) ,@(nil-code) :return))
  '(:local.get 0) '(:ref.cast :cons) '(:struct.get :cons 0) '(:local.set first)
  '(:local.get 0) '(:ref.cast :cons) '(:struct.get :cons 1) '(:local.set l)
  '(:i32.const 1) '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:global.get :mv-buf) (:local.get i) (:i32.const 1) :i32.sub
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:array.set :vector)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  ;; a dotted list
  '(:local.get l) (car (nil-code)) :ref.eq :i32.eqz
  `(:if nil nil ((:local.get l) (:global.get ,(sym-global 'clysm-sys::%values-list-error))
                 (:call (:callsym 1)) :drop))
  '(:local.get i) '(:global.set :mv-count)
  '(:local.get first))

(defhelper :revappend-onto :ee-e ((l :eqref) (acc :eqref))
  '(:local.get 0) '(:local.set l) '(:local.get 1) '(:local.set acc)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:local.get acc) (:struct.new :cons)
       (:local.set acc)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:local.get acc))

(defhelper :list-length-i32 :e-i ((l :eqref) (n :i32))
  '(:local.get 0) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.add (:local.set n)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:local.get n))

(defhelper :reverse-list-to-vector :l-v ((n :i32) (v (:ref :null :vector)) (l :eqref))
  ;; list is in reverse order
  '(:local.get 0) '(:call :list-length-i32) '(:local.tee n)
  '(:array.new_default :vector) '(:local.set v)
  '(:local.get 0) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
       (:local.get v) (:local.get n) (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:array.set :vector)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:local.get v) '(:ref.as_non_null))

(defhelper :list-to-vector :l-v ((n :i32) (v (:ref :null :vector)) (l :eqref) (i :i32))
  '(:local.get 0) '(:call :list-length-i32) '(:local.tee n)
  '(:array.new_default :vector) '(:local.set v)
  '(:local.get 0) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get v) (:local.get i) (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:array.set :vector)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:local.get v) '(:ref.as_non_null))

(defhelper :apply-list :ee-e ((f (:ref :null :closure)) (v (:ref :null :vector)) (n :i32))
  ;; (apply f list): dispatch to fixed entries when possible
  '(:local.get 0) '(:call :coerce-function) '(:local.set f)
  '(:local.get 1) '(:call :list-to-vector) '(:local.tee v) :array.len '(:local.set n)
  `(:block generic nil
     (:block a4 nil (:block a3 nil (:block a2 nil (:block a1 nil (:block a0 nil
       (:local.get n) (:br_table a0 a1 a2 a3 a4 generic))
       (:local.get f) :ref.as_non_null (:local.get f) (:struct.get :closure 0) (:return_call_ref :f0))
       (:local.get f) :ref.as_non_null (:local.get v) (:i32.const 0) (:array.get :vector)
       (:local.get f) (:struct.get :closure 1) (:return_call_ref :f1))
       (:local.get f) :ref.as_non_null
       (:local.get v) (:i32.const 0) (:array.get :vector) (:local.get v) (:i32.const 1) (:array.get :vector)
       (:local.get f) (:struct.get :closure 2) (:return_call_ref :f2))
       (:local.get f) :ref.as_non_null
       (:local.get v) (:i32.const 0) (:array.get :vector) (:local.get v) (:i32.const 1) (:array.get :vector)
       (:local.get v) (:i32.const 2) (:array.get :vector)
       (:local.get f) (:struct.get :closure 3) (:return_call_ref :f3))
     (:local.get f) :ref.as_non_null
     (:local.get v) (:i32.const 0) (:array.get :vector) (:local.get v) (:i32.const 1) (:array.get :vector)
     (:local.get v) (:i32.const 2) (:array.get :vector) (:local.get v) (:i32.const 3) (:array.get :vector)
     (:local.get f) (:struct.get :closure 4) (:return_call_ref :f4))
  '(:local.get f) :ref.as_non_null '(:local.get v) :ref.as_non_null
  '(:local.get f) `(:struct.get :closure ,+clo-entry-n+) '(:return_call_ref :fN))

;;; lambda-list support
(defhelper :vector-tail-to-list :vi-e ((l :eqref) (i :i32))
  (car (nil-code)) '(:local.set l)
  '(:local.get 0) :array.len '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:local.get 1) :i32.le_s (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:local.get 0) (:local.get i) (:array.get :vector) (:local.get l) (:struct.new :cons) (:local.set l)
       (:br again)))
  '(:local.get l))

(defhelper :find-key :vie-e ((i :i32) (n :i32))
  ;; search keyword in args[start..]; returns value or null
  '(:local.get 0) :array.len '(:local.set n)
  '(:local.get 1) '(:local.set i)
  '(:block done nil
     (:loop again nil
       (:local.get i) (:i32.const 1) :i32.add (:local.get n) :i32.ge_s (:br_if done)
       (:local.get 0) (:local.get i) (:array.get :vector) (:local.get 2) :ref.eq
       (:if nil nil ((:local.get 0) (:local.get i) (:i32.const 1) :i32.add (:array.get :vector) :return))
       (:local.get i) (:i32.const 2) :i32.add (:local.set i)
       (:br again)))
  '(:ref.null :eq))

(defhelper :check-keys :vie-v ((i :i32) (n :i32) (k :eqref) (aok :i32))
  ;; verify even count and that every key is in the list (unless :allow-other-keys t)
  '(:local.get 0) :array.len '(:local.set n)
  '(:local.get n) '(:local.get 1) :i32.le_s
  '(:if nil nil (:return))
  '(:local.get n) '(:local.get 1) :i32.sub '(:i32.const 1) :i32.and
  `(:if nil nil ((:local.get 0) (:local.get 1) :ref.i31 (:global.get ,(sym-global 'clysm-sys::%odd-keys-error))
                 (:call (:callsym 2)) :drop :return))
  ;; allow-other-keys?
  `(:local.get 0) '(:local.get 1) `(:global.get ,(sym-global :allow-other-keys)) '(:call :find-key) '(:local.tee k)
  :ref.is_null :i32.eqz
  `(:if nil nil ((:local.get k) ,@(nil-code) :ref.eq :i32.eqz (:if nil nil (:return))))
  '(:local.get 1) '(:local.set i)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_s (:br_if done)
       (:local.get 0) (:local.get i) (:array.get :vector) (:local.set k)
       (:local.get k) (:global.get ,(sym-global :allow-other-keys)) :ref.eq
       (:local.get k) (:local.get 2) (:call :memq) :i32.or :i32.eqz
       (:if nil nil ((:local.get k) (:local.get 2) (:global.get ,(sym-global 'clysm-sys::%unknown-key-error))
                     (:call (:callsym 2)) :drop :return))
       (:local.get i) (:i32.const 2) :i32.add (:local.set i)
       (:br again))))

(defhelper :memq :ee-i ((l :eqref))
  '(:local.get 1) '(:local.set l)
  '(:block done nil
     (:loop again nil
       (:local.get l) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 0) (:local.get 0) :ref.eq
       (:if nil nil ((:i32.const 1) :return))
       (:local.get l) (:ref.cast :cons) (:struct.get :cons 1) (:local.set l)
       (:br again)))
  '(:i32.const 0))

;;; arg count errors
(defun argerr-body (k)
  `((:local.get 0)
    ,@(fixnum-code k)
    (:global.get ,(sym-global 'clysm-sys::%arg-count-error)) (:call (:callsym 2))))

(defun define-argerr-helpers ()
  (loop for k from 0 to +max-fixed-args+
        do (let ((k k))
             (setf *helpers* (remove (list :argerr k) *helpers* :key #'first :test #'equal))
             (push (list (list :argerr k) (fixed-entry-type k) nil (lambda () (argerr-body k))) *helpers*)
             ;; pack entries for complex lambdas: build vector and call entryN
             (setf *helpers* (remove (list :pack k) *helpers* :key #'first :test #'equal))
             (push (list (list :pack k) (fixed-entry-type k) nil
                         (lambda ()
                           `((:local.get 0)
                             ,@(loop for i from 1 to k collect `(:local.get ,i))
                             (:array.new_fixed :vector ,k)
                             (:local.get 0) (:struct.get :closure ,+clo-entry-n+)
                             (:return_call_ref :fN))))
                   *helpers*))))

(defhelper :argerr-n :fN ()
  '(:local.get 0) '(:local.get 1) :array.len :ref.i31
  `(:global.get ,(sym-global 'clysm-sys::%arg-count-error)) '(:call (:callsym 2)))

;;; packages: dynamic field access
(defhelper :package-ref :ee-e ((p (:ref :null :package)))
  '(:local.get 0) '(:ref.cast :package) '(:local.set p)
  '(:block f7 nil (:block f6 nil (:block f5 nil (:block f4 nil (:block f3 nil (:block f2 nil (:block f1 nil (:block f0 nil
     (:local.get 1) (:ref.cast :i31) :i31.get_s (:br_table f0 f1 f2 f3 f4 f5 f6 f7 f7))
     (:local.get p) (:struct.get :package 0) :return) (:local.get p) (:struct.get :package 1) :return)
     (:local.get p) (:struct.get :package 2) :return) (:local.get p) (:struct.get :package 3) :return)
     (:local.get p) (:struct.get :package 4) :return) (:local.get p) (:struct.get :package 5) :return)
     (:local.get p) (:struct.get :package 6) :return)
  '(:local.get p) '(:struct.get :package 7))

(defhelper :set-package-ref :eee-e ((p (:ref :null :package)))
  '(:local.get 0) '(:ref.cast :package) '(:local.set p)
  '(:block f7 nil (:block f6 nil (:block f5 nil (:block f4 nil (:block f3 nil (:block f2 nil (:block f1 nil (:block f0 nil
     (:local.get 1) (:ref.cast :i31) :i31.get_s (:br_table f0 f1 f2 f3 f4 f5 f6 f7 f7))
     (:local.get p) (:local.get 2) (:struct.set :package 0) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 1) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 2) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 3) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 4) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 5) (:local.get 2) :return)
     (:local.get p) (:local.get 2) (:struct.set :package 6) (:local.get 2) :return)
  '(:local.get p) '(:local.get 2) '(:struct.set :package 7) '(:local.get 2))

(defhelper :identity-hash :e-i ((h :i32))
  ;; instances cache the host's identity hash in their third field
  '(:local.get 0) '(:ref.test :instance)
  '(:if nil nil
        ((:local.get 0) (:ref.cast :instance) (:struct.get :instance 2) (:local.tee h)
         (:if nil nil ((:local.get h) :return))
         (:local.get 0) (:ref.cast :instance)
         (:local.get 0) :extern.convert_any (:call :host-identity-hash) (:local.tee h)
         (:struct.set :instance 2)
         (:local.get h) :return))
  '(:local.get 0) :extern.convert_any '(:call :host-identity-hash))

(defhelper :to-f64 :e-d ()
  `(:block notd :eqref
     (:local.get 0) (:br_on_cast_fail notd :eqref (:ref :double)) (:struct.get :double 0) :return)
  :drop
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%to-double)) '(:call (:callsym 1))
  '(:ref.cast :double) '(:struct.get :double 0))

;;; ------------------------------------------------------------
;;; Bignum, ratio, complex, float primitives
;;; ------------------------------------------------------------

(defprim clysm-sys::%make-limbs (n) :value `(,@(idx-code n) (:array.new_default :limbs)))
(defprim clysm-sys::%llen (v) :value `(,@v (:ref.cast :limbs) :array.len :ref.i31))
(defprim clysm-sys::%lref (v i) :value `(,@v (:ref.cast :limbs) ,@(idx-code i) (:array.get :limbs) :ref.i31))
(defprim clysm-sys::%lset (v i x) :value
  (let ((tmp (new-local)))
    `(,@v (:ref.cast :limbs) ,@(idx-code i) ,@x (:local.tee ,tmp) (:ref.cast :i31) :i31.get_s
      (:array.set :limbs) (:local.get ,tmp))))
(defprim clysm-sys::%make-bignum (sign mag) :value
  `(,@sign (:ref.cast :i31) :i31.get_s ,@mag (:ref.cast :limbs) (:i64.const 0) (:struct.new :bignum)))
(defprim clysm-sys::%bignum-sign (b) :value `(,@b (:ref.cast :bignum) (:struct.get :bignum 0) :ref.i31))
(defprim clysm-sys::%bignum-mag (b) :value `(,@b (:call :bignum-limbs)))

(defprim clysm-sys::%make-ratio (n d) :value `(,@n ,@d (:struct.new :ratio)))
(defprim clysm-sys::%ratio-num (r) :value `(,@r (:ref.cast :ratio) (:struct.get :ratio 0)))
(defprim clysm-sys::%ratio-den (r) :value `(,@r (:ref.cast :ratio) (:struct.get :ratio 1)))
(defprim clysm-sys::%make-complex (r i) :value `(,@r ,@i (:struct.new :complex)))
(defprim clysm-sys::%complex-real (c) :value `(,@c (:ref.cast :complex) (:struct.get :complex 0)))
(defprim clysm-sys::%complex-imag (c) :value `(,@c (:ref.cast :complex) (:struct.get :complex 1)))

(defun f64-code (x) `(,@x (:ref.cast :double) (:struct.get :double 0)))
(defun box-f64 () '((:struct.new :double)))
(macrolet ((dop (name op)
             `(defprim ,name (a b) :value `(,@(f64-code a) ,@(f64-code b) ,',op ,@(box-f64))))
           (dcmp (name op)
             `(defprim ,name (a b) :test `(,@(f64-code a) ,@(f64-code b) ,',op)))
           (d1 (name op)
             `(defprim ,name (a) :value `(,@(f64-code a) ,',op ,@(box-f64)))))
  (dop clysm-sys::%d+ :f64.add) (dop clysm-sys::%d- :f64.sub) (dop clysm-sys::%d* :f64.mul)
  (dop clysm-sys::%d/ :f64.div) (dop clysm-sys::%dmin :f64.min) (dop clysm-sys::%dmax :f64.max)
  (dcmp clysm-sys::%d< :f64.lt) (dcmp clysm-sys::%d= :f64.eq) (dcmp clysm-sys::%d<= :f64.le)
  (d1 clysm-sys::%dneg :f64.neg) (d1 clysm-sys::%dabs :f64.abs) (d1 clysm-sys::%dsqrt :f64.sqrt)
  (d1 clysm-sys::%dfloor :f64.floor) (d1 clysm-sys::%dceil :f64.ceil) (d1 clysm-sys::%dtrunc :f64.trunc)
  (d1 clysm-sys::%dround :f64.nearest))

(defprim clysm-sys::%double-eql (a b) :test
  `(,@(f64-code a) :i64.reinterpret_f64 ,@(f64-code b) :i64.reinterpret_f64 :i64.eq))
(defprim clysm-sys::%single-eql (a b) :test
  `(,@a (:ref.cast :single) (:struct.get :single 0) :i32.reinterpret_f32
    ,@b (:ref.cast :single) (:struct.get :single 0) :i32.reinterpret_f32 :i32.eq))
(defprim clysm-sys::%fixnum-to-double (n) :value `(,@(idx-code n) :f64.convert_i32_s ,@(box-f64)))
(defprim clysm-sys::%double-to-fixnum (d) :value
  ;; caller guarantees the truncated value fits in a fixnum
  `(,@(f64-code d) :i32.trunc_sat_f64_s :ref.i31))
(defprim clysm-sys::%double-to-single (d) :value `(,@(f64-code d) :f32.demote_f64 (:struct.new :single)))
(defprim clysm-sys::%single-to-double (s) :value
  `(,@s (:ref.cast :single) (:struct.get :single 0) :f64.promote_f32 ,@(box-f64)))
(defprim clysm-sys::%double-nan-p (d) :test
  (let ((tmp (new-local :f64)))
    `(,@(f64-code d) (:local.tee ,tmp) (:local.get ,tmp) :f64.ne)))
;; decode double: fields as fixnums
(defprim clysm-sys::%double-sign (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 63) :i64.shr_u :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-exponent (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 52) :i64.shr_u (:i64.const #x7FF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-mant-hi (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const 26) :i64.shr_u (:i64.const #x3FFFFFF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%double-mant-lo (d) :value
  `(,@(f64-code d) :i64.reinterpret_f64 (:i64.const #x3FFFFFF) :i64.and :i32.wrap_i64 :ref.i31))
(defprim clysm-sys::%make-double-from-parts (sign expo hi lo) :value
  ;; sign: 0/1, expo: raw 11 bits, hi: 26 bits, lo: 26 bits
  `(,@sign (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 63) :i64.shl
    ,@expo (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 52) :i64.shl :i64.or
    ,@hi (:ref.cast :i31) :i31.get_s :i64.extend_i32_u (:i64.const 26) :i64.shl :i64.or
    ,@lo (:ref.cast :i31) :i31.get_s :i64.extend_i32_u :i64.or
    :f64.reinterpret_i64 ,@(box-f64)))
(defprim clysm-sys::%float-to-chars (d mode) :value
  `(,@(f64-code d) ,@(idx-code mode) (:call :host-float-to-chars) :ref.i31))
(defprim clysm-sys::%host-char (i) :value
  `(,@(idx-code i) (:call :host-get-char) (:i32.const ,+char-tag+) :i32.add :ref.i31))
(defprim clysm-sys::%math1 (d op) :value `(,@(f64-code d) ,@(idx-code op) (:call :host-math1) ,@(box-f64)))
(defprim clysm-sys::%math2 (a b op) :value
  `(,@(f64-code a) ,@(f64-code b) ,@(idx-code op) (:call :host-math2) ,@(box-f64)))
(defprim clysm-sys::%host-time (k) :value `(,@(idx-code k) (:call :host-time) ,@(box-f64)))
(defprim clysm-sys::%put-char-buf (c) :value
  `(,@(idx-code c) (:call :host-put-char-buf) ,@(nil-code)))
(defprim clysm-sys::%file-open (mode) :value `(,@(idx-code mode) (:call :host-file-open) :ref.i31))

(defprim clysm-sys::%argc () :value `((:call :host-argc) :ref.i31))
(defprim clysm-sys::%arg-len (i) :value `(,@(idx-code i) (:call :host-arg-len) :ref.i31))
(defprim clysm-sys::%arg-char (i j) :value
  `(,@(idx-code i) ,@(idx-code j) (:call :host-arg-char) (:i32.const ,+char-tag+) :i32.add :ref.i31))
(defprim clysm-sys::%debug-mode () :value `((:call :host-debug-mode) :ref.i31))

;;; ------------------------------------------------------------
;;; JavaScript interop: externref values are wrapped in $foreign structs
;;; ------------------------------------------------------------

(defun ext-code (x) `(,@x (:ref.cast :foreign) (:struct.get :foreign 0)))
(defun wrap-ext () '((:struct.new :foreign)))

(defprim clysm-sys::%js-global () :value `((:call :js-global) ,@(wrap-ext)))
(defprim clysm-sys::%js-get (o k) :value `(,@(ext-code o) ,@(ext-code k) (:call :js-get) ,@(wrap-ext)))
(defprim clysm-sys::%js-set (o k v) :value
  `(,@(ext-code o) ,@(ext-code k) ,@(ext-code v) (:call :js-set) ,@(nil-code)))
(defprim clysm-sys::%js-apply (f this args) :value
  `(,@(ext-code f) ,@(ext-code this) ,@(ext-code args) (:call :js-apply) ,@(wrap-ext)))
(defprim clysm-sys::%js-new (f args) :value `(,@(ext-code f) ,@(ext-code args) (:call :js-new) ,@(wrap-ext)))
(defprim clysm-sys::%js-array () :value `((:call :js-array) ,@(wrap-ext)))
(defprim clysm-sys::%js-push (a v) :value `(,@(ext-code a) ,@(ext-code v) (:call :js-push) ,@(nil-code)))
(defprim clysm-sys::%js-number (d) :value `(,@(f64-code d) (:call :js-number) ,@(wrap-ext)))
(defprim clysm-sys::%js-to-number (o) :value `(,@(ext-code o) (:call :js-to-number) ,@(box-f64)))
(defprim clysm-sys::%js-typeof (o) :value `(,@(ext-code o) (:call :js-typeof) :ref.i31))
(defprim clysm-sys::%js-take-string () :value `((:call :js-take-string) ,@(wrap-ext)))
(defprim clysm-sys::%js-str-len (o) :value `(,@(ext-code o) (:call :js-str-len) :ref.i31))
(defprim clysm-sys::%js-str-char (o i) :value
  `(,@(ext-code o) ,@(idx-code i) (:call :js-str-char) (:i32.const ,+char-tag+) :i32.add :ref.i31))
(defprim clysm-sys::%js-bool (b) :value `(,@(idx-code b) (:call :js-bool) ,@(wrap-ext)))
(defprim clysm-sys::%js-special (k) :value `(,@(idx-code k) (:call :js-special) ,@(wrap-ext)))
(defprim clysm-sys::%js-wrap-fn (f) :value `(,@f (:ref.cast :closure) :extern.convert_any (:call :js-wrap-fn) ,@(wrap-ext)))
(defprim clysm-sys::%js-truthy (o) :test `(,@(ext-code o) (:call :js-truthy)))
(defprim clysm-sys::%js-lisp-object-p (o) :test `(,@(ext-code o) (:call :js-is-lisp)))
(defprim clysm-sys::%js-wrap-lisp (x) :value `(,@x :extern.convert_any ,@(wrap-ext)))
(defprim clysm-sys::%js-unwrap-lisp (o) :value `(,@(ext-code o) :any.convert_extern (:ref.cast :null :eq)))

;; exported entry used by JS to call Lisp closures
(defhelper :lisp-apply :xx-x ()
  '(:local.get 0) '(:struct.new :foreign) '(:local.get 1) '(:struct.new :foreign)
  `(:global.get ,(sym-global 'clysm-sys::%js-callback)) '(:call (:callsym 2))
  '(:ref.cast :foreign) '(:struct.get :foreign 0))

(defnative vector (form env)
  `(:wasm :eqref ,(convert-args (cdr form) env)
          ((:array.new_fixed :vector ,(length (cdr form))))))

(defhelper :eval-string :x-x ()
  '(:local.get 0) '(:struct.new :foreign)
  `(:global.get ,(sym-global 'clysm-sys::%host-eval-string)) '(:call (:callsym 1))
  '(:ref.cast :foreign) '(:struct.get :foreign 0))

;;; fast paths for 1-D AREF / (SETF AREF) on simple vectors and strings
(defhelper :aref1 :ee-e ((v (:ref :null :vector)) (s (:ref :null :string)) (i :i32) (d :eqref))
  '(:local.get 1) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set i)
         (:block notvec :eqref
           (:local.get 0) (:br_on_cast_fail notvec :eqref (:ref :vector)) (:local.tee v)
           :array.len (:local.get i) :i32.gt_u
           (:if nil nil ((:local.get v) (:local.get i) (:array.get :vector) :return))
           (:local.get 0))
         :drop
         (:block notstr :eqref
           (:local.get 0) (:br_on_cast_fail notstr :eqref (:ref :string)) (:local.tee s)
           :array.len (:local.get i) :i32.gt_u
           (:if nil nil ((:local.get s) (:local.get i) (:array.get :string)
                         (:i32.const ,+char-tag+) :i32.add :ref.i31 :return))
           (:local.get 0))
         :drop))
  (md1-ref-code 'd 'i nil)
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%aref1-slow)) '(:call (:callsym 2)))

(defhelper :aset1 :eee-e ((v (:ref :null :vector)) (i :i32) (d :eqref) (c :i32))
  '(:local.get 1) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set i)
         (:block notvec :eqref
           (:local.get 0) (:br_on_cast_fail notvec :eqref (:ref :vector)) (:local.tee v)
           :array.len (:local.get i) :i32.gt_u
           (:if nil nil ((:local.get v) (:local.get i) (:local.get 2) (:array.set :vector)
                         (:local.get 2) :return))
           (:local.get 0))
         :drop))
  (md1-set-code 'd 'i 'c nil)
  '(:local.get 0) '(:local.get 1) '(:local.get 2)
  `(:global.get ,(sym-global 'clysm-sys::%aset1-slow)) '(:call (:callsym 3)))

(defprim clysm-sys::%aref1 (a i) :value `(,@a ,@i (:call :aref1)))

;;; two-dimensional arrays over a simple vector inline, else the Lisp functions
(defun aref2-index-code (fail)
  "Helper-body code: when local 0 is a two-dimensional array over a simple
vector and locals 1, 2 are indices within its dimensions, leave the vector
in local V and the index in local K; else branch to FAIL."
  `((:local.get 0) (:ref.test :mdarray) :i32.eqz (:br_if ,fail)
    (:local.get 0) (:ref.cast! :mdarray) (:local.set m)
    (:local.get m) (:struct.get :mdarray 2) (:local.tee dims) (:ref.test :cons) :i32.eqz (:br_if ,fail)
    (:local.get dims) (:ref.cast! :cons) (:struct.get :cons 1) (:local.tee rest) (:ref.test :cons) :i32.eqz (:br_if ,fail)
    (:local.get rest) (:ref.cast! :cons) (:struct.get :cons 1) ,@(nil-code) :ref.eq :i32.eqz (:br_if ,fail)
    (:local.get 1) (:ref.test :i31) :i32.eqz (:br_if ,fail)
    (:local.get 2) (:ref.test :i31) :i32.eqz (:br_if ,fail)
    (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee x)
    (:local.get dims) (:ref.cast! :cons) (:struct.get :cons 0) (:ref.cast :i31) :i31.get_s :i32.ge_u (:br_if ,fail)
    (:local.get 2) (:ref.cast! :i31) :i31.get_s (:local.tee y)
    (:local.get rest) (:ref.cast! :cons) (:struct.get :cons 0) (:ref.cast :i31) :i31.get_s (:local.tee d1)
    :i32.ge_u (:br_if ,fail)
    (:local.get m) (:struct.get :mdarray 0) (:ref.test :vector) :i32.eqz (:br_if ,fail)
    (:local.get m) (:struct.get :mdarray 0) (:ref.cast! :vector) (:local.set v)
    (:local.get m) (:struct.get :mdarray 3) (:ref.cast :i31) :i31.get_s
    (:local.get x) (:local.get d1) :i32.mul :i32.add (:local.get y) :i32.add (:local.set k)))

(defhelper :aref2 :eee-e ((m (:ref :null :mdarray)) (dims :eqref) (rest :eqref) (x :i32) (y :i32) (d1 :i32)
                          (v (:ref :null :vector)) (k :i32))
  `(:block slow nil ,@(aref2-index-code 'slow)
     (:local.get v) (:local.get k) (:array.get :vector) :return)
  '(:local.get 0) '(:local.get 1) '(:local.get 2)
  `(:global.get ,(sym-global 'clysm-sys::%aref2)) '(:call (:callsym 3)))

(defhelper :aset2 :eeee-e ((m (:ref :null :mdarray)) (dims :eqref) (rest :eqref) (x :i32) (y :i32) (d1 :i32)
                           (v (:ref :null :vector)) (k :i32))
  `(:block slow nil ,@(aref2-index-code 'slow)
     (:local.get v) (:local.get k) (:local.get 3) (:array.set :vector) (:local.get 3) :return)
  '(:local.get 0) '(:local.get 1) '(:local.get 2) '(:local.get 3)
  `(:global.get ,(sym-global 'clysm-sys::%aset2)) '(:call (:callsym 4)))

(defprim clysm-sys::%aref2 (a i j) :value `(,@a ,@i ,@j (:call :aref2)))
(defprim clysm-sys::%aset2 (a i j v) :value `(,@a ,@i ,@j ,@v (:call :aset2)))
(defprim clysm-sys::%aset1 (a i v) :value `(,@a ,@i ,@v (:call :aset1)))
(defprim svref (v i) :value `(,@v ,@i (:call :aref1)))

;;; MOD / REM fast paths
(defun divop-helper-body (lisp-fn modp)
  `((:local.get 0) (:ref.test :i31) (:local.get 1) (:ref.test :i31) :i32.and
    (:if nil nil
         ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee x)
          (:i32.const ,+char-tag+) :i32.add
          (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee y)
          (:i32.const ,+char-tag+) :i32.add :i32.or
          (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
          (:local.get y) :i32.eqz :i32.eqz :i32.and
          (:if nil nil
               ((:local.get x) (:local.get y) :i32.rem_s (:local.set r)
                ,@(when modp
                    '((:local.get r) :i32.eqz :i32.eqz
                      (:local.get r) (:local.get y) :i32.xor (:i32.const 0) :i32.lt_s :i32.and
                      (:if nil nil ((:local.get r) (:local.get y) :i32.add (:local.set r)))))
                (:local.get r) :ref.i31 :return))))
    (:local.get 0) (:local.get 1)
    (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))))

(defhelper :mod :ee-e ((x :i32) (y :i32) (r :i32))
  (cons :progn-marker (divop-helper-body 'clysm-sys::%mod-slow t)))
(defhelper :rem :ee-e ((x :i32) (y :i32) (r :i32))
  (cons :progn-marker (divop-helper-body 'clysm-sys::%rem-slow nil)))
(defprim mod (a b) :value `(,@a ,@b (:call :mod)))
(defprim rem (a b) :value `(,@a ,@b (:call :rem)))

(defprim clysm-sys::%backtrace (fd) :value `(,@(idx-code fd) (:call :host-backtrace) ,@(nil-code)))
(defprim clysm-sys::%is-tty () :value `((:call :host-is-tty) :ref.i31))

;; instantiate a module compiled in the image and run it (see src/live.lisp)
(defprim clysm-sys::%compile-module (bytes consts) :mv
  `(,@(ext-code bytes) ,@(ext-code consts) (:call :host-compile-module)
    :any.convert_extern (:ref.cast :null :eq)))
(defprim clysm-sys::%flush () :value `((:call :host-flush) ,@(nil-code)))

;;; ------------------------------------------------------------
;;; checked casts: ref.cast to these types signals TYPE-ERROR on failure
;;; ------------------------------------------------------------

(defparameter *checked-cast-types*
  '((:i31 . fixnum) (:cons . cons) (:symbol . symbol) (:string . simple-string)
    (:vector . simple-vector) (:instance . structure-object) (:mdarray . array)
    (:double . double-float) (:single . single-float) (:bignum . integer) (:ratio . ratio)
    (:complex . complex) (:package . package) (:bytes . (simple-array (unsigned-byte 8) (*)))
    (:bits . simple-bit-vector) (:f64vec . (simple-array double-float (*))) (:foreign . t)
    (:limbs . t) (:closure . function)))

(defun define-checked-casts ()
  (dolist (e *checked-cast-types*)
    (let* ((type (car e)) (lisp-type (cdr e))
           (fname (list :castfn type))
           (tname (list :cast-type type)))
      (unless (gethash tname *extra-types*)
        (setf (gethash tname *extra-types*) t)
        (add-type *m* tname `(:func (:eqref) ((:ref ,type)))))
      (add-func *m* fname tname
                :body `((:block fail :eqref
                          (:local.get 0) (:br_on_cast_fail fail :eqref (:ref ,type)) :return)
                        :drop
                        (:local.get 0) ,@(const-init-code lisp-type)
                        (:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2))
                        (:ref.cast ,type)))
      (push (cons type fname) (wasm-module-cast-rewrites *m*)))))
