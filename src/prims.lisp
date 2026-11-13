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

;;; Nested integer arithmetic (+ - * 1+ 1- LOGAND LOGIOR LOGXOR and ASH by a
;;; constant) is first tried in i64, without boxes for the intermediate
;;; values.  The leaves of the tree (anything else: variables, constants,
;;; calls) are evaluated once, in order, into locals; when a leaf is not an
;;; integer within 64 bits or a step overflows, the generic helpers compute
;;; the tree from the same locals.
;;;
;;; Under a LOGAND with an operand known to be within [0, 2^63) only the low
;;; 64 bits of the other operand matter, so + - * LOGAND LOGIOR LOGXOR and
;;; left shifts below it are computed modulo 2^64 without overflow checks:
;;; (logand (* h 16777619) #xFFFFFFFF), (logand (+ (* x a) c) mask) and the
;;; like never make a bignum.
(defparameter *fusable-ops*
  '((+ . :add) (- . :sub) (* . :mul) (logand . :and) (logior . :or) (logxor . :xor) (1+ . :add) (1- . :sub)))
(defparameter *fused-helpers*
  '((:add . :add) (:sub . :sub) (:mul . :mul) (:and . :logand2) (:or . :logior2) (:xor . :logxor2)))
(defvar *no-fuse* nil)
(defvar *fused-rest* nil "the records of the leaves not yet visited by the walk in progress")

(defun fusable-ash-node-p (node)
  "(ASH x constant) with the count within -63 .. 63."
  (and (eq (car node) :prim) (eq (prim-name (second node)) 'ash) (= (length (cddr node)) 2)
       (let ((c (fourth node))) (and (eq (car c) :const) (integerp (second c)) (<= -63 (second c) 63)))))
(defun fusable-op-node-p (node)
  (and (eq (car node) :prim)
       (let ((name (prim-name (second node))) (n (length (cddr node))))
         (or (and (member name '(+ - * logand logior logxor)) (= n 2))
             (and (member name '(1+ 1-)) (= n 1))
             (fusable-ash-node-p node)))))
(defun fused-operands (node)
  "The operation (:add ... or :ash) and the two operands of a fusable node."
  (let ((name (prim-name (second node))))
    (cond ((eq name 'ash) (values :ash (third node) (fourth node)))
          ((member name '(1+ 1-)) (values (cdr (assoc name *fusable-ops*)) (third node) '(:const 1)))
          (t (values (cdr (assoc name *fusable-ops*)) (third node) (fourth node))))))

(defun fused-int-node-p (node)
  (if (eq (car node) :const) (integerp (second node)) (ty-only-p (node-ty node) +k-int+)))

(defun fused-plan (root alloc)
  "The leaves of the arithmetic tree ROOT in evaluation order, as records
\(node kind local...) with their locals when ALLOC; or :NO when the tree
cannot be fused: after an operation that may signal (an operand not known to
be an integer) only variables and constants may be evaluated, so that the
operations can all be done after the leaves."
  (let ((recs nil) (tainted nil))
    (labels ((leaf (n)
               (when (and tainted (not (member (car n) '(:lref :const)))) (return-from fused-plan :no))
               (push (cond ((and (eq (car n) :const) (typep (second n) '(signed-byte 64))) (list n :const))
                           ((and (eq (car n) :lref) (dual-local (second n)))
                            (list* n :dual (when alloc (list (new-local) (new-local :i64)))))
                           ((ty-int64-p (node-ty n)) (list* n :i64 (when alloc (list (new-local :i64)))))
                           (t (list* n :eqref (when alloc (list (new-local))))))
                     recs))
             (walk (n top)
               (cond ((and (not top) (not (eq (car n) :const)) (ty-int64-p (node-ty n))) (leaf n))
                     ((fusable-op-node-p n)
                      (multiple-value-bind (op a b) (fused-operands n)
                        (walk a nil)
                        (unless (eq op :ash) (walk b nil))
                        (unless (and (fused-int-node-p a) (fused-int-node-p b)) (setf tainted t))))
                     (t (leaf n)))))
      (walk root t)
      (nreverse recs))))

(defun fused-leaf-eval-code (rec)
  (destructuring-bind (node kind &rest locs) rec
    (ecase kind
      (:const nil)
      (:dual (let ((lv (second node)))
               `((:local.get ,(cdr (get-loc lv))) (:local.set ,(first locs))
                 (:local.get ,(dual-local lv)) (:local.set ,(second locs)))))
      (:i64 `(,@(cg-rep node :i64) (:local.set ,(first locs))))
      (:eqref `(,@(cg node) (:local.set ,(first locs)))))))

(defun fused-mask-p (node)
  "NODE is known to be an integer within [0, 2^63)."
  (or (and (eq (car node) :const) (integerp (second node)) (<= 0 (second node) (1- (expt 2 63))))
      (and (not (eq (car node) :const)) (ty-in-range-p (node-ty node) 0 (1- (expt 2 63))))))

(defun i64-decode-code (value-code fail)
  "The i64 value of the eqref left by VALUE-CODE when it is a fixnum or an
integer within 64 bits; anything else branches to FAIL."
  (let ((e (new-local)) (v (new-local :i32)) (w (new-local :i64)))
    `(,@value-code (:local.tee ,e) (:ref.test :i31)
      (:if nil :i64
           ((:local.get ,e) (:ref.cast! :i31) :i31.get_s (:local.tee ,v)
            (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,fail)
            (:local.get ,v) :i64.extend_i32_s)
           ;; a bignum holding its value as an i64 (nonzero), else the helpers
           ((:local.get ,e) (:ref.test :bignum) :i32.eqz (:br_if ,fail)
            (:local.get ,e) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee ,w) :i64.eqz
            (:if nil :i64
                 ((:local.get ,e) (:call :small-int-p) :i32.eqz (:br_if ,fail)
                  (:local.get ,e) (:call :int-i64))
                 ((:local.get ,w))))))))

(defun i64-checked-op-code (op x y r fail)
  "OP (:add :sub :mul) of the i64 locals X and Y into the local R, branching
to FAIL when the result does not fit."
  (ecase op
    (:add `((:local.get ,x) (:local.get ,y) :i64.add (:local.tee ,r) (:local.get ,x) :i64.xor
            (:local.get ,r) (:local.get ,y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if ,fail)))
    (:sub `((:local.get ,x) (:local.get ,y) :i64.sub (:local.tee ,r) (:local.get ,x) :i64.xor
            (:local.get ,x) (:local.get ,y) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if ,fail)))
    (:mul `(;; both within 32 bits: the product fits
            (:local.get ,x) (:i64.const ,(expt 2 31)) :i64.add
            (:local.get ,y) (:i64.const ,(expt 2 31)) :i64.add :i64.or (:i64.const 32) :i64.shr_u :i64.eqz
            (:if nil nil
                 ((:local.get ,x) (:local.get ,y) :i64.mul (:local.set ,r))
                 ((:local.get ,x) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if ,fail)
                  (:local.get ,y) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if ,fail)
                  (:local.get ,x) :i64.eqz
                  (:if nil nil
                       ((:i64.const 0) (:local.set ,r))
                       ((:local.get ,x) (:local.get ,y) :i64.mul (:local.tee ,r)
                        (:local.get ,x) :i64.div_s (:local.get ,y) :i64.ne (:br_if ,fail)))))))))

(defun fused-i64-code (node fail &optional wrap top)
  "Code leaving the i64 value of NODE (with WRAP: its low 64 bits),
branching to FAIL when that is not possible.  The leaves are taken from
*FUSED-REST* in order."
  (cond ((and (fusable-op-node-p node) (or top (not (ty-int64-p (node-ty node)))))
         (multiple-value-bind (op a b) (fused-operands node)
           (cond ((eq op :ash)
                  (let ((c (second b)) (x (new-local :i64)) (r (new-local :i64)))
                    (cond ((< c 0) `(,@(fused-i64-code a fail nil) (:i64.const ,(- c)) :i64.shr_s))
                          (wrap `(,@(fused-i64-code a fail t) (:i64.const ,c) :i64.shl))
                          (t `(,@(fused-i64-code a fail nil) (:local.tee ,x) (:i64.const ,c) :i64.shl (:local.tee ,r)
                               (:i64.const ,c) :i64.shr_s (:local.get ,x) :i64.ne (:br_if ,fail)
                               (:local.get ,r))))))
                 ((member op '(:and :or :xor))
                  ;; under a mask the other operand is needed modulo 2^64 only
                  (let* ((wrap-a (or wrap (and (eq op :and) (fused-mask-p b))))
                         (wrap-b (or wrap (and (eq op :and) (fused-mask-p a))))
                         (ca (fused-i64-code a fail wrap-a))
                         (cb (fused-i64-code b fail wrap-b)))
                    `(,@ca ,@cb ,(ecase op (:and :i64.and) (:or :i64.or) (:xor :i64.xor)))))
                 (t
                  (let* ((x (new-local :i64)) (y (new-local :i64)) (r (new-local :i64))
                         (ca (fused-i64-code a fail wrap))
                         (cb (fused-i64-code b fail wrap)))
                    `(,@ca (:local.set ,x) ,@cb (:local.set ,y)
                      ,@(if wrap
                            `((:local.get ,x) (:local.get ,y) ,(ecase op (:add :i64.add) (:sub :i64.sub) (:mul :i64.mul)))
                            `(,@(i64-checked-op-code op x y r fail) (:local.get ,r)))))))))
        (t
         ;; a leaf
         (destructuring-bind (leaf kind &rest locs) (pop *fused-rest*)
           (ecase kind
             (:const `((:i64.const ,(second leaf))))
             (:dual (dual-pair-i64-code (first locs) (second locs) fail))
             (:i64 `((:local.get ,(first locs))))
             (:eqref (i64-decode-code `((:local.get ,(first locs))) fail)))))))

(defun fused-generic-code (node &optional top)
  "The value of the tree NODE by the generic helpers, from the locals of its leaves."
  (cond ((and (fusable-op-node-p node) (or top (not (ty-int64-p (node-ty node)))))
         (multiple-value-bind (op a b) (fused-operands node)
           (if (eq op :ash)
               `(,@(fused-generic-code a) ,@(cg b) (:call :ash-fx))
               (let* ((ca (fused-generic-code a)) (cb (fused-generic-code b)))
                 `(,@ca ,@cb (:call ,(cdr (assoc op *fused-helpers*))))))))
        (t
         (destructuring-bind (leaf kind &rest locs) (pop *fused-rest*)
           (ecase kind
             (:const (cg leaf))
             (:dual (dual-materialize-code (first locs) (second locs)))
             (:i64 `((:local.get ,(first locs)) ,@(box-rep-code :i64 (node-ty leaf))))
             (:eqref `((:local.get ,(first locs)))))))))

(defun fused-tree-node-p (node)
  (and (not *no-fuse*) (fusable-op-node-p node) (some #'fusable-op-node-p (cddr node))
       (not (eq (fused-plan node nil) :no))))

(defun fused-parts (node)
  "For the fusable tree NODE: the code evaluating its leaves, a function of a
label giving the i64 code, and the generic code."
  (let ((recs (fused-plan node t)))
    (values (loop for rec in recs append (fused-leaf-eval-code rec))
            (lambda (fail) (let ((*fused-rest* recs)) (fused-i64-code node fail nil t)))
            (let ((*fused-rest* recs)) (fused-generic-code node t)))))

(defun fused-code (node rep)
  "NODE (a fused tree) computed in i64 and left as REP (:eqref, or :i32 when
its type is a fixnum), the generic code when that fails."
  (let ((done (gensym "FUSED")) (fail (gensym "SLOW")) (r (new-local :i64)))
    (multiple-value-bind (leaves i64-code generic) (fused-parts node)
      `(,@leaves
        (:block ,done ,rep
          (:block ,fail nil
            ,@(funcall i64-code fail)
            ,@(if (or (eq rep :i32) (ty-fixnum-p (node-ty node)))
                  `(:i32.wrap_i64 ,@(when (eq rep :eqref) '(:ref.i31)))
                  `((:local.tee ,r) (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                    (:if nil :eqref ((:local.get ,r) :i32.wrap_i64 :ref.i31) ((:local.get ,r) (:call :i64-int)))))
            (:br ,done))
          ,@generic
          ,@(when (eq rep :i32) (unbox-rep-code :i32 (node-ty node))))))))

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
  (let ((d (new-local)))
    `(,@data ,@fill ,@dims (:local.tee ,d) ,@offset ,@info
      (:local.get ,d) (:call :dims-2d-0) (:local.get ,d) (:call :dims-2d-1)
      (:struct.new :mdarray))))
(macrolet ((acc (name idx)
             `(progn
                (defprim ,name (a) :value `(,@a (:ref.cast :mdarray) (:struct.get :mdarray ,,idx)))
                (defprim ,(intern (format nil "%SET-~A" (subseq (symbol-name name) 1)) (symbol-package name)) (a v) :value
                  (let ((tmp (new-local)))
                    `(,@a (:ref.cast :mdarray) ,@v (:local.tee ,tmp) (:struct.set :mdarray ,,idx)
                      (:local.get ,tmp)))))))
  (acc clysm-sys::%mdarray-data 0) (acc clysm-sys::%mdarray-fill 1)
  (acc clysm-sys::%mdarray-dims 2) (acc clysm-sys::%mdarray-offset 3) (acc clysm-sys::%mdarray-info 4))
;; setting the dimensions also sets the rank-2 dimension fields
(defprim clysm-sys::%set-mdarray-dims (a v) :value
  (let ((m (new-local '(:ref :null :mdarray))) (tmp (new-local)))
    `(,@a (:ref.cast :mdarray) (:local.tee ,m) ,@v (:local.tee ,tmp) (:struct.set :mdarray 2)
      (:local.get ,m) (:local.get ,tmp) (:call :dims-2d-0) (:struct.set :mdarray 5)
      (:local.get ,m) (:local.get ,tmp) (:call :dims-2d-1) (:struct.set :mdarray 6)
      (:local.get ,tmp))))

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
(defprim clysm-sys::%unbind-to (sp) :value `(,@sp (:ref.cast :i31) :i31.get_s (:call :unbind-to) ,@(nil-code)))
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
      ;; (apply f a (list b c)) is (funcall f a b c)
      (let ((last (car (last args))))
        (when (and (cdr args) (consp last) (eq (car last) 'list) (not (env-lookup-fun env 'list)))
          (return-from apply-native
            (convert `(funcall ,@(butlast args) ,@(cdr last)) env))))
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
    (:host-arg-char :ii-i "env" "arg_char")
    ;; the host's Math functions, imported one by one (a direct call each)
    (:math-sin :d-d "math" "sin") (:math-cos :d-d "math" "cos") (:math-tan :d-d "math" "tan")
    (:math-asin :d-d "math" "asin") (:math-acos :d-d "math" "acos") (:math-atan :d-d "math" "atan")
    (:math-exp :d-d "math" "exp") (:math-log :d-d "math" "log")
    (:math-sinh :d-d "math" "sinh") (:math-cosh :d-d "math" "cosh") (:math-tanh :d-d "math" "tanh")
    (:math-asinh :d-d "math" "asinh") (:math-acosh :d-d "math" "acosh") (:math-atanh :d-d "math" "atanh")
    (:math-pow :dd-d "math" "pow") (:math-atan2 :dd-d "math" "atan2")))

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

(defhelper :eql :ee-i ((wa :i64) (wb :i64))
  '(:local.get 0) '(:local.get 1) :ref.eq
  '(:if nil nil ((:i32.const 1) :return))
  ;; both numbers of the same boxed type?
  '(:local.get 0) '(:ref.test :i31) '(:local.get 1) '(:ref.test :i31) :i32.or
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:ref.test :double)
  '(:if nil nil
        ((:local.get 1) (:ref.test :double) :i32.eqz (:if nil nil ((:i32.const 0) :return))
         (:local.get 0) (:ref.cast! :double) (:struct.get :double 0) :i64.reinterpret_f64
         (:local.get 1) (:ref.cast! :double) (:struct.get :double 0) :i64.reinterpret_f64 :i64.eq :return))
  '(:local.get 0) '(:ref.test :bignum)
  '(:if nil nil
        ((:local.get 1) (:ref.test :bignum) :i32.eqz (:if nil nil ((:i32.const 0) :return))
         ;; integers within 64 bits: the values
         (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee wa) :i64.eqz :i32.eqz
         (:local.get 1) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee wb) :i64.eqz :i32.eqz :i32.and
         (:if nil nil ((:local.get wa) (:local.get wb) :i64.eq :return))))
  '(:local.get 0) '(:ref.test :single)
  '(:if nil nil
        ((:local.get 1) (:ref.test :single) :i32.eqz (:if nil nil ((:i32.const 0) :return))
         (:local.get 0) (:ref.cast! :single) (:struct.get :single 0) :i32.reinterpret_f32
         (:local.get 1) (:ref.cast! :single) (:struct.get :single 0) :i32.reinterpret_f32 :i32.eq :return))
  '(:local.get 0) '(:ref.test :bignum) '(:local.get 0) '(:ref.test :ratio) :i32.or
  '(:local.get 0) '(:ref.test :complex) :i32.or :i32.eqz
  '(:if nil nil ((:i32.const 0) :return))
  '(:local.get 0) '(:local.get 1) (car (lisp-call-instrs 'clysm-sys::%eql-boxed 2))
  (cadr (lisp-call-instrs 'clysm-sys::%eql-boxed 2))
  (car (nil-code)) :ref.eq :i32.eqz)

;;; Integers within 64 bits are computed with i64 arithmetic.  A bignum whose
;;; VALUE field is not zero holds its value there (it may have limbs as well,
;;; made when the general bignum code asked for them); one made from limbs has
;;; VALUE = 0 (the normalizer :make-int never makes such a one below 2^63).
(defhelper :small-int-p :e-i ()
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add
                 (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :return))
  '(:local.get 0) '(:ref.test :bignum)
  '(:if nil nil ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) :i64.eqz :i32.eqz
                 (:if nil nil ((:i32.const 1) :return))
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) :ref.is_null
                 (:if nil nil ((:i32.const 1) :return))
                 (:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 1) :array.len
                 (:i32.const 3) :i32.lt_u :return))
  '(:i32.const 0))

(defhelper :int-i64 :e-l ((m (:ref :null :limbs)) (i :i32) (r :i64))
  '(:local.get 0) '(:ref.test :i31)
  '(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s :i64.extend_i32_s :return))
  '(:local.get 0) '(:ref.cast! :bignum) '(:struct.get :bignum 2) '(:local.tee r) :i64.eqz :i32.eqz
  '(:if nil nil ((:local.get r) :return))
  '(:local.get 0) '(:ref.cast! :bignum) '(:struct.get :bignum 1) '(:local.tee m) :ref.is_null
  '(:if nil nil ((:i64.const 0) :return))
  '(:local.get m) :array.len '(:local.set i)
  `(:block done nil
     (:loop again nil
       (:local.get i) :i32.eqz (:br_if done)
       (:local.get i) (:i32.const 1) :i32.sub (:local.set i)
       (:local.get r) (:i64.const ,+limb-bits+) :i64.shl
       (:local.get m) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.set r)
       (:br again)))
  '(:local.get 0) '(:ref.cast! :bignum) '(:struct.get :bignum 0)
  '(:if nil :i64 ((:i64.const 0) (:local.get r) :i64.sub) ((:local.get r))))

(defhelper :box-i32 :i-e ()
  ;; an i32 as a fixnum, or a bignum outside the fixnum range
  `(:local.get 0) `(:i32.const ,+char-tag+) :i32.add `(:i32.const ,(* 2 +char-tag+)) :i32.lt_u
  '(:if nil nil ((:local.get 0) :ref.i31 :return))
  '(:local.get 0) :i64.extend_i32_s '(:call :i64-int))

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
  `(:block done nil
     (:loop again nil
       (:local.get tt) :i64.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.add (:local.set n)
       (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set tt)
       (:br again)))
  '(:local.get n) '(:array.new_default :limbs) '(:local.set a)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:local.get m) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get m) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set m)
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
             (:local.get ,param) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee ,dst) :i64.eqz :i32.eqz
             (:br_if ok)
             (:local.get ,param) (:ref.cast! :bignum) (:struct.get :bignum 1) (:local.tee m) :ref.is_null
             (:br_if ok)
             (:local.get m) :array.len (:i32.const 3) :i32.lt_u :i32.eqz (:if nil nil (,@(nil-code) :return))
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

;;; logical operations: fixnum pairs inline, then integers within 64 bits,
;;; then the Lisp functions (which check the argument types)
(macrolet ((logop (name op lisp-fn)
             `(defhelper ,name :ee-e ((v :i32) (xa :i64) (xb :i64) (ka :i32))
                `(:block slow nil
                   ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
                   ,@(num-decode-code 1 'xb nil 'ka 'slow nil)
                   (:local.get xa) (:local.get xb) ,',op (:call :i64-int) :return)
                '(:local.get 0) '(:local.get 1)
                `(:global.get ,(sym-global ',lisp-fn)) '(:call (:callsym 2)))))
  (logop :logand2 :i64.and clysm-sys::%logand2)
  (logop :logior2 :i64.or clysm-sys::%logior2)
  (logop :logxor2 :i64.xor clysm-sys::%logxor2))

(defhelper :ash-fx :ee-e ((v :i32) (xa :i64) (ka :i32) (c :i32) (r :i64))
  ;; an integer within 64 bits shifted by a fixnum count
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     (:local.get 1) (:ref.test :i31) :i32.eqz (:br_if slow)
     (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee c)
     (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if slow)
     (:local.get c) (:i32.const 0) :i32.lt_s
     (:if nil nil
          ((:local.get xa)
           (:i32.const 0) (:local.get c) :i32.sub (:local.tee c) (:i32.const 63) :i32.gt_u
           (:if nil :i64 ((:i64.const 63)) ((:local.get c) :i64.extend_i32_u))
           :i64.shr_s (:call :i64-int) :return))
     (:local.get c) (:i32.const 62) :i32.gt_u (:br_if slow)
     (:local.get xa) (:local.get c) :i64.extend_i32_u :i64.shl (:local.tee r)
     (:local.get c) :i64.extend_i32_u :i64.shr_s (:local.get xa) :i64.ne (:br_if slow)
     (:local.get r) (:call :i64-int) :return)
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%ash2)) '(:call (:callsym 2)))

(defprim logand (a b) :value (inline-fixnum-op a b :i32.and :logand2))
(defprim logior (a b) :value (inline-fixnum-op a b :i32.or :logior2))
(defprim logxor (a b) :value (inline-fixnum-op a b :i32.xor :logxor2))
(defprim ash (a b) :value `(,@a ,@b (:call :ash-fx)))

;;; INTEGER-LENGTH and LOGCOUNT of fixnums and integers within 64 bits
(defhelper :integer-length :e-e ((v :i32) (xa :i64) (ka :i32))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     (:i32.const 64) (:local.get xa) (:local.get xa) (:i64.const 63) :i64.shr_s :i64.xor :i64.clz :i32.wrap_i64
     :i32.sub :ref.i31 :return)
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%integer-length-big)) '(:call (:callsym 1)))
(defhelper :logcount :e-e ((v :i32) (xa :i64) (ka :i32))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     (:local.get xa) (:local.get xa) (:i64.const 63) :i64.shr_s :i64.xor :i64.popcnt :i32.wrap_i64
     :ref.i31 :return)
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%logcount-big)) '(:call (:callsym 1)))
(defprim integer-length (a) :value `(,@a (:call :integer-length)))
(defprim logcount (a) :value `(,@a (:call :logcount)))

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

;;; An integer from a sign (0 / 1) and a magnitude: a fixnum, an integer
;;; within 64 bits (held in VALUE, no limbs), or a bignum of the limbs without
;;; leading zeros (the array itself when it has none).
(defhelper :make-int :ei-e ((m (:ref :null :limbs)) (n :i32) (w :i64) (r (:ref :null :limbs)))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set m)
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) :i32.eqz
  '(:if nil nil ((:i32.const 0) :ref.i31 :return))
  `(:block big nil
     (:local.get n) (:i32.const 3) :i32.gt_u (:br_if big)
     (:local.get m) (:i32.const 0) (:array.get :limbs) :i64.extend_i32_u (:local.set w)
     (:local.get n) (:i32.const 1) :i32.gt_u
     (:if nil nil ((:local.get m) (:i32.const 1) (:array.get :limbs) :i64.extend_i32_u (:i64.const ,+limb-bits+) :i64.shl
                   (:local.get w) :i64.or (:local.set w)))
     (:local.get n) (:i32.const 2) :i32.gt_u
     (:if nil nil
          ((:local.get m) (:i32.const 2) (:array.get :limbs) (:i32.const 128) :i32.ge_u
           (:if nil nil
                (;; -2^63 is the one more value an i64 holds
                 (:local.get m) (:i32.const 2) (:array.get :limbs) (:i32.const 128) :i32.eq
                 (:local.get w) :i64.eqz :i32.and (:local.get 1) :i32.and :i32.eqz (:br_if big)
                 (:i64.const ,(- (expt 2 63))) (:call :i64-int) :return))
           (:local.get m) (:i32.const 2) (:array.get :limbs) :i64.extend_i32_u (:i64.const ,(* 2 +limb-bits+)) :i64.shl
           (:local.get w) :i64.or (:local.set w)))
     (:local.get 1) (:if nil :i64 ((:i64.const 0) (:local.get w) :i64.sub) ((:local.get w)))
     (:call :i64-int) :return)
  '(:local.get n) '(:local.get m) :array.len :i32.eq
  '(:if nil nil ((:local.get 1) (:local.get m) (:i64.const 0) (:struct.new :bignum) :return))
  '(:local.get n) '(:array.new_default :limbs) '(:local.tee r)
  '(:i32.const 0) '(:local.get m) '(:i32.const 0) '(:local.get n) '(:array.copy :limbs :limbs)
  '(:local.get 1) '(:local.get r) '(:i64.const 0) '(:struct.new :bignum))
(defprim clysm-sys::%make-integer (sign mag) :value
  `(,@mag ,@sign (:ref.cast :i31) :i31.get_s (:call :make-int)))

;;; the magnitude of an integer (a fresh array for a fixnum)
(defhelper :int-limbs :e-e ((v :i32) (m (:ref :null :limbs)))
  '(:local.get 0) '(:ref.test :i31) :i32.eqz
  '(:if nil nil ((:local.get 0) (:call :bignum-limbs) :return))
  '(:local.get 0) '(:ref.cast! :i31) :i31.get_s '(:local.tee v) '(:i32.const 0) :i32.lt_s
  '(:if nil nil ((:i32.const 0) (:local.get v) :i32.sub (:local.set v)))
  '(:local.get v) :i32.eqz '(:if nil nil ((:i32.const 0) (:array.new_default :limbs) :return))
  `(:local.get v) `(:i32.const ,(expt 2 +limb-bits+)) :i32.lt_u
  '(:if nil nil ((:local.get v) (:array.new_fixed :limbs 1) :return))
  `(:local.get v) `(:i32.const ,+limb-mask+) :i32.and '(:local.get v) `(:i32.const ,+limb-bits+) :i32.shr_u
  '(:array.new_fixed :limbs 2))
(defprim clysm-sys::%int-mag (x) :value `(,@x (:call :int-limbs)))

(defhelper :int-neg :e-i ()
  '(:local.get 0) '(:ref.test :i31)
  '(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const 0) :i32.lt_s :return))
  '(:local.get 0) '(:ref.cast :bignum) '(:struct.get :bignum 0))
(defprim clysm-sys::%int-sign (x) :value `(,@x (:call :int-neg) :ref.i31))

(defhelper :integerp :e-i ()
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:i32.const ,+char-tag+) :i32.add
                 (:i32.const ,(* 2 +char-tag+)) :i32.lt_u :return))
  '(:local.get 0) '(:ref.test :bignum))

;;; compare magnitudes: -1, 0, 1
(defhelper :mag-cmp :ee-i ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (la :i32) (lb :i32) (x :i32) (y :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) :i32.ne
  '(:if nil nil ((:i32.const -1) (:i32.const 1) (:local.get la) (:local.get lb) :i32.lt_u :select :return))
  '(:block done nil
     (:loop again nil
       (:local.get la) :i32.eqz (:br_if done)
       (:local.get la) (:i32.const 1) :i32.sub (:local.set la)
       (:local.get a) (:local.get la) (:array.get :limbs) (:local.tee x)
       (:local.get b) (:local.get la) (:array.get :limbs) (:local.tee y) :i32.eq (:br_if again)
       (:i32.const -1) (:i32.const 1) (:local.get x) (:local.get y) :i32.lt_u :select :return))
  '(:i32.const 0))
(defprim clysm-sys::%mag-cmp (a b) :value `(,@a ,@b (:call :mag-cmp) :ref.i31))

;;; compare integers: -1, 0, 1
(defhelper :int-cmp :ee-i ((sa :i32) (sb :i32) (c :i32))
  '(:local.get 0) '(:call :int-neg) '(:local.tee sa) '(:local.get 1) '(:call :int-neg) '(:local.tee sb) :i32.ne
  '(:if nil nil ((:i32.const -1) (:i32.const 1) (:local.get sa) :select :return))
  '(:local.get 0) '(:call :int-limbs) '(:local.get 1) '(:call :int-limbs) '(:call :mag-cmp) '(:local.set c)
  '(:i32.const 0) '(:local.get c) :i32.sub '(:local.get c) '(:local.get sa) :select)
(defprim clysm-sys::%int-cmp-big (a b) :value `(,@a ,@b (:call :int-cmp) :ref.i31))

;;; A + B, or A - B when the third parameter is 1 (integers of any size)
(defhelper :big-add :eei-e ((sa :i32) (sb :i32) (ma :eqref) (mb :eqref) (c :i32))
  '(:local.get 0) '(:call :int-neg) '(:local.set sa)
  '(:local.get 1) '(:call :int-neg) '(:local.get 2) :i32.xor '(:local.set sb)
  '(:local.get 0) '(:call :int-limbs) '(:local.set ma) '(:local.get 1) '(:call :int-limbs) '(:local.set mb)
  '(:local.get sa) '(:local.get sb) :i32.eq
  '(:if nil nil ((:local.get ma) (:local.get mb) (:call :mag-add) (:local.get sa) (:call :make-int) :return))
  '(:local.get ma) '(:local.get mb) '(:call :mag-cmp) '(:local.tee c) :i32.eqz
  '(:if nil nil ((:i32.const 0) :ref.i31 :return))
  '(:local.get c) '(:i32.const 0) :i32.gt_s
  '(:if nil :eqref
        ((:local.get ma) (:local.get mb) (:call :mag-sub) (:local.get sa) (:call :make-int))
        ((:local.get mb) (:local.get ma) (:call :mag-sub) (:local.get sb) (:call :make-int))))
(defprim clysm-sys::%int-add-big (a b) :value `(,@a ,@b (:i32.const 0) (:call :big-add)))
(defprim clysm-sys::%int-sub-big (a b) :value `(,@a ,@b (:i32.const 1) (:call :big-add)))

;;; A * B (integers of any size)
(defhelper :big-mul :ee-e ((v :i32) (s :i32))
  '(:local.get 0) '(:call :int-neg) '(:local.get 1) '(:call :int-neg) :i32.xor '(:local.set s)
  ;; a fixnum below the limb base: one pass
  (loop for (p q) in '((1 0) (0 1))
        collect `(:block notfx nil
                   (:local.get ,p) (:ref.test :i31) :i32.eqz (:br_if notfx)
                   (:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee v) :i32.eqz
                   (:if nil nil ((:i32.const 0) :ref.i31 :return))
                   (:local.get v) (:i32.const 0) :i32.lt_s
                   (:if nil nil ((:i32.const 0) (:local.get v) :i32.sub (:local.set v)))
                   (:local.get v) (:i32.const ,(expt 2 +limb-bits+)) :i32.ge_u (:br_if notfx)
                   (:local.get ,q) (:call :int-limbs) (:local.get v) :ref.i31 (:call :mag-mul1)
                   (:local.get s) (:call :make-int) :return))
  '(:local.get 0) '(:call :int-limbs) '(:local.get 1) '(:call :int-limbs) '(:call :mag-mul)
  '(:local.get s) '(:call :make-int))
(defprim clysm-sys::%int-mul-big (a b) :value `(,@a ,@b (:call :big-mul)))

;;; LOGAND (0) / LOGIOR (1) / LOGXOR (2) of integers of any size, digit by
;;; digit in two's complement
(defhelper :big-logop :eei-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                              (la :i32) (lb :i32) (n :i32) (i :i32) (sa :i32) (sb :i32) (sr :i32)
                              (ca :i32) (cb :i32) (cr :i32) (x :i32) (y :i32) (d :i32))
  '(:local.get 0) '(:call :int-limbs) '(:ref.cast! :limbs) '(:local.tee a) :array.len '(:local.set la)
  '(:local.get 1) '(:call :int-limbs) '(:ref.cast! :limbs) '(:local.tee b) :array.len '(:local.set lb)
  '(:local.get 0) '(:call :int-neg) '(:local.set sa) '(:local.get 1) '(:call :int-neg) '(:local.set sb)
  '(:block b2 nil (:block b1 nil (:block b0 nil
     (:local.get 2) (:br_table b0 b1 b2))
     (:local.get sa) (:local.get sb) :i32.and (:local.set sr) (:br b2))
     (:local.get sa) (:local.get sb) :i32.or (:local.set sr) (:br b2))
  '(:local.get 2) '(:i32.const 2) :i32.eq '(:if nil nil ((:local.get sa) (:local.get sb) :i32.xor (:local.set sr)))
  '(:local.get la) '(:local.get lb) '(:local.get la) '(:local.get lb) :i32.gt_u :select
  '(:i32.const 1) :i32.add '(:local.tee n) '(:array.new_default :limbs) '(:local.set r)
  '(:i32.const 1) '(:local.set ca) '(:i32.const 1) '(:local.set cb) '(:i32.const 1) '(:local.set cr)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       ,@(loop for (m len sg carry dst) in '((a la sa ca x) (b lb sb cb y))
               append `((:local.get i) (:local.get ,len) :i32.lt_u
                        (:if nil :i32 ((:local.get ,m) (:local.get i) (:array.get :limbs)) ((:i32.const 0)))
                        (:local.set ,dst)
                        (:local.get ,sg)
                        (:if nil nil
                             ((:local.get ,dst) (:i32.const ,+limb-mask+) :i32.xor (:local.get ,carry) :i32.add
                              (:local.tee ,dst) (:i32.const ,+limb-bits+) :i32.shr_u (:local.set ,carry)
                              (:local.get ,dst) (:i32.const ,+limb-mask+) :i32.and (:local.set ,dst)))))
       (:block o2 nil (:block o1 nil (:block o0 nil
          (:local.get 2) (:br_table o0 o1 o2))
          (:local.get x) (:local.get y) :i32.and (:local.set d) (:br o2))
          (:local.get x) (:local.get y) :i32.or (:local.set d) (:br o2))
       (:local.get 2) (:i32.const 2) :i32.eq (:if nil nil ((:local.get x) (:local.get y) :i32.xor (:local.set d)))
       (:local.get sr)
       (:if nil nil
            ((:local.get d) (:i32.const ,+limb-mask+) :i32.xor (:local.get cr) :i32.add
             (:local.tee d) (:i32.const ,+limb-bits+) :i32.shr_u (:local.set cr)
             (:local.get d) (:i32.const ,+limb-mask+) :i32.and (:local.set d)))
       (:local.get r) (:local.get i) (:local.get d) (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) '(:local.get sr) '(:call :make-int))
(defprim clysm-sys::%int-logop-big (a b op) :value
  `(,@a ,@b ,@op (:ref.cast :i31) :i31.get_s (:call :big-logop)))

;;; magnitude measures: the bit length, the number of 1 bits, the number of
;;; trailing 0 bits (of a magnitude that is not zero)
(defhelper :mag-bitlen :e-i ((n :i32))
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
  '(:local.get n) '(:i32.const 1) :i32.sub `(:i32.const ,+limb-bits+) :i32.mul
  '(:i32.const 32) '(:local.get 0) '(:ref.cast! :limbs) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :limbs)
  :i32.clz :i32.sub :i32.add)
(defhelper :mag-popcount :e-i ((m (:ref :null :limbs)) (n :i32) (c :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee m) :array.len '(:local.set n)
  '(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
       (:local.get m) (:local.get n) (:array.get :limbs) :i32.popcnt (:local.get c) :i32.add (:local.set c)
       (:br again)))
  '(:local.get c))
(defhelper :mag-ctz :e-i ((m (:ref :null :limbs)) (n :i32) (i :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee m) :array.len '(:local.set n)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get m) (:local.get i) (:array.get :limbs)
       (:if nil nil ((:local.get i) (:i32.const ,+limb-bits+) :i32.mul
                     (:local.get m) (:local.get i) (:array.get :limbs) :i32.ctz :i32.add :return))
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:i32.const 0))
(defprim clysm-sys::%mag-bitlen (m) :value `(,@m (:call :mag-bitlen) :ref.i31))
(defprim clysm-sys::%mag-popcount (m) :value `(,@m (:call :mag-popcount) :ref.i31))
(defprim clysm-sys::%mag-ctz (m) :value `(,@m (:call :mag-ctz) :ref.i31))

;;; a magnitude shifted left / right by any number of bits (a fixnum >= 0)
(defhelper :mag-shl-n :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (dg :i32) (s :i64)
                             (v :i64) (carry :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 0) '(:call :mag-len) '(:local.set n)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s `(:i32.const ,+limb-bits+) :i32.div_u '(:local.set dg)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s `(:i32.const ,+limb-bits+) :i32.rem_u :i64.extend_i32_u '(:local.set s)
  '(:local.get n) '(:local.get dg) :i32.add '(:i32.const 1) :i32.add '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get s) :i64.shl
       (:local.get carry) :i64.or (:local.set v)
       (:local.get r) (:local.get i) (:local.get dg) :i32.add
       (:local.get v) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get v) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) '(:local.get n) '(:local.get dg) :i32.add '(:local.get carry) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r) :ref.as_non_null)
(defhelper :mag-shr-n :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (dg :i32) (s :i32)
                             (k :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 0) '(:call :mag-len) '(:local.set n)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s `(:i32.const ,+limb-bits+) :i32.div_u '(:local.set dg)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s `(:i32.const ,+limb-bits+) :i32.rem_u '(:local.set s)
  '(:local.get dg) '(:local.get n) :i32.ge_u '(:if nil nil ((:i32.const 0) (:array.new_default :limbs) :return))
  '(:local.get n) '(:local.get dg) :i32.sub '(:local.tee k) '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get k) :i32.ge_u (:br_if done)
       (:local.get r) (:local.get i)
       (:local.get a) (:local.get i) (:local.get dg) :i32.add (:array.get :limbs) (:local.get s) :i32.shr_u
       (:local.get i) (:i32.const 1) :i32.add (:local.get k) :i32.lt_u
       (:if nil :i32
            ((:local.get a) (:local.get i) (:local.get dg) :i32.add (:i32.const 1) :i32.add (:array.get :limbs)
             ;; (when S = 0 the shift by 28 leaves no bits within the mask)
             (:i32.const ,+limb-bits+) (:local.get s) :i32.sub :i32.shl (:i32.const ,+limb-mask+) :i32.and)
            ((:i32.const 0)))
       :i32.or (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-shl (a n) :value `(,@a ,@n (:call :mag-shl-n)))
(defprim clysm-sys::%mag-shr (a n) :value `(,@a ,@n (:call :mag-shr-n)))

(defhelper :mag-mul :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (i :i32) (j :i32) (ai :i64) (carry :i64) (tt :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) :i32.add '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop outer nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.tee ai) :i64.eqz :i32.eqz
       (:if nil nil
            ((:i64.const 0) (:local.set carry) (:i32.const 0) (:local.set j)
             (:block idone nil
               (:loop inner nil
                 (:local.get j) (:local.get lb) :i32.ge_u (:br_if idone)
                 (:local.get ai) (:local.get b) (:local.get j) (:array.get :limbs) :i64.extend_i32_u :i64.mul
                 (:local.get r) (:local.get i) (:local.get j) :i32.add (:array.get :limbs) :i64.extend_i32_u :i64.add
                 (:local.get carry) :i64.add (:local.set tt)
                 (:local.get r) (:local.get i) (:local.get j) :i32.add
                 (:local.get tt) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
                 (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
                 (:local.get j) (:i32.const 1) :i32.add (:local.set j)
                 (:br inner)))
             (:local.get r) (:local.get i) (:local.get lb) :i32.add (:local.get carry) :i32.wrap_i64 (:array.set :limbs)))
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br outer)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-mul-fast (a b) :value `(,@a ,@b (:call :mag-mul)))

;;; A times one digit 0 < D < 2^28: the trimmed product
(defhelper :mag-mul1 :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (d :i64)
                            (carry :i64) (tt :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a)
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) '(:i32.const 1) :i32.add '(:array.new_default :limbs) '(:local.set r)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s :i64.extend_i32_s '(:local.set d)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get d) :i64.mul
       (:local.get carry) :i64.add (:local.set tt)
       (:local.get r) (:local.get i) (:local.get tt) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get carry) :i64.eqz
  `(:if nil nil
        (;; no carry out: the first N limbs
         (:local.get n) (:array.new_default :limbs) (:local.set a)
         (:local.get a) (:i32.const 0) (:local.get r) (:i32.const 0) (:local.get n) (:array.copy :limbs :limbs)
         (:local.get a) :ref.as_non_null :return))
  '(:local.get r) '(:local.get n) '(:local.get carry) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-mul-digit (a d) :value `(,@a ,@d (:call :mag-mul1)))

;;; a + b (magnitudes)
(defhelper :mag-add :ee-e ((a (:ref :null :limbs)) (b (:ref :null :limbs)) (r (:ref :null :limbs))
                           (la :i32) (lb :i32) (n :i32) (i :i32) (s :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set b)
  '(:local.get 0) '(:call :mag-len) '(:local.set la) '(:local.get 1) '(:call :mag-len) '(:local.set lb)
  '(:local.get la) '(:local.get lb) '(:local.get la) '(:local.get lb) :i32.gt_u :select
  '(:i32.const 1) :i32.add '(:local.tee n) '(:array.new_default :limbs) '(:local.set r)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get s)
       (:local.get i) (:local.get la) :i32.lt_u
       (:if nil :i32 ((:local.get a) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.add
       (:local.get i) (:local.get lb) :i32.lt_u
       (:if nil :i32 ((:local.get b) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.add
       (:local.set s)
       (:local.get r) (:local.get i) (:local.get s) (:i32.const ,+limb-mask+) :i32.and (:array.set :limbs)
       (:local.get s) (:i32.const ,+limb-bits+) :i32.shr_u (:local.set s)
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
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get la) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs)
       (:local.get i) (:local.get lb) :i32.lt_u
       (:if nil :i32 ((:local.get b) (:local.get i) (:array.get :limbs)) ((:i32.const 0))) :i32.sub
       (:local.get borrow) :i32.sub (:local.tee d)
       (:i32.const 0) :i32.lt_s (:local.set borrow)
       (:local.get r) (:local.get i)
       (:local.get d) (:local.get borrow) (:i32.const ,+limb-bits+) :i32.shl :i32.add (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-sub-fast (a b) :value `(,@a ,@b (:call :mag-sub)))

;;; A divided by 0 < D < 2^28: the quotient, the remainder as the second value
(defhelper :mag-divmod-small :ee-e ((a (:ref :null :limbs)) (q (:ref :null :limbs)) (n :i32) (d :i64)
                                    (rem :i64) (cur :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set a)
  '(:local.get 0) '(:call :mag-len) '(:local.tee n) '(:array.new_default :limbs) '(:local.set q)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s :i64.extend_i32_s '(:local.set d)
  `(:block done nil
     (:loop again nil
       (:local.get n) :i32.eqz (:br_if done)
       (:local.get n) (:i32.const 1) :i32.sub (:local.set n)
       (:local.get rem) (:i64.const ,+limb-bits+) :i64.shl
       (:local.get a) (:local.get n) (:array.get :limbs) :i64.extend_i32_u :i64.or (:local.set cur)
       (:local.get q) (:local.get n) (:local.get cur) (:local.get d) :i64.div_u :i32.wrap_i64 (:array.set :limbs)
       (:local.get cur) (:local.get d) :i64.rem_u (:local.set rem)
       (:br again)))
  '(:global.get :mv-buf) '(:i32.const 0) '(:local.get rem) :i32.wrap_i64 :ref.i31 '(:array.set :vector)
  '(:i32.const 2) '(:global.set :mv-count)
  '(:local.get q) :ref.as_non_null)
(defprim clysm-sys::%mag-divmod-small (a d) :mv `(,@a ,@d (:call :mag-divmod-small)))

;;; magnitude shifts by 0 <= S < 28 bits (left: one more limb)
(defhelper :mag-shl :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (s :i64)
                           (v :i64) (carry :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee a) :array.len '(:local.tee n)
  '(:i32.const 1) :i32.add '(:array.new_default :limbs) '(:local.set r)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s :i64.extend_i32_s '(:local.set s)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get a) (:local.get i) (:array.get :limbs) :i64.extend_i32_u (:local.get s) :i64.shl
       (:local.get carry) :i64.or (:local.set v)
       (:local.get r) (:local.get i) (:local.get v) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
       (:local.get v) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) '(:local.get n) '(:local.get carry) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-shift-left-bits (a s) :value `(,@a ,@s (:call :mag-shl)))

(defhelper :mag-shr :ee-e ((a (:ref :null :limbs)) (r (:ref :null :limbs)) (n :i32) (i :i32) (s :i32))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.tee a) :array.len '(:local.tee n)
  '(:array.new_default :limbs) '(:local.set r)
  '(:local.get 1) '(:ref.cast! :i31) :i31.get_s '(:local.set s)
  `(:block done nil
     (:loop again nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if done)
       (:local.get r) (:local.get i)
       (:local.get a) (:local.get i) (:array.get :limbs) (:local.get s) :i32.shr_u
       (:local.get i) (:i32.const 1) :i32.add (:local.get n) :i32.lt_u
       (:if nil :i32
            ((:local.get a) (:local.get i) (:i32.const 1) :i32.add (:array.get :limbs)
             ;; (when S = 0 the shift by 28 leaves no bits within the mask)
             (:i32.const ,+limb-bits+) (:local.get s) :i32.sub :i32.shl (:i32.const ,+limb-mask+) :i32.and)
            ((:i32.const 0)))
       :i32.or (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br again)))
  '(:local.get r) :ref.as_non_null)
(defprim clysm-sys::%mag-shift-right-bits (a s) :value `(,@a ,@s (:call :mag-shr)))

;;; Knuth's algorithm D for magnitudes U, V (V of at least two limbs, U at
;;; least as long): the quotient, the remainder as the second value
(defhelper :mag-divmod :ee-e ((u (:ref :null :limbs)) (v (:ref :null :limbs)) (un (:ref :null :limbs))
                              (vn (:ref :null :limbs)) (q (:ref :null :limbs)) (r (:ref :null :limbs))
                              (n :i32) (m0 :i32) (m :i32) (s :i32) (i :i32) (j :i32)
                              (vtop :i64) (vnext :i64) (num :i64) (qhat :i64) (rhat :i64)
                              (p :i64) (carry :i64) (borrow :i64) (tt :i64))
  '(:local.get 0) '(:ref.cast! :limbs) '(:local.set u) '(:local.get 1) '(:ref.cast! :limbs) '(:local.set v)
  '(:local.get 0) '(:call :mag-len) '(:local.set m0) '(:local.get 1) '(:call :mag-len) '(:local.set n)
  ;; normalize: the top limb of vn has its bit 27 set
  '(:local.get v) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :limbs) :i32.clz
  `(:i32.const ,(- 32 +limb-bits+)) :i32.sub '(:local.set s)
  '(:local.get n) '(:array.new_default :limbs) '(:local.set vn)
  `(:local.get m0) '(:i32.const 1) :i32.add '(:array.new_default :limbs) '(:local.set un)
  ;; vn = v << s, un = u << s (un one limb longer)
  (loop for (src dst len) in '((v vn n) (u un m0))
        collect `(:block sdone nil
                   (:i32.const 0) (:local.set i) (:i64.const 0) (:local.set carry)
                   (:loop sagain nil
                     (:local.get i) (:local.get ,len) :i32.ge_u (:br_if sdone)
                     (:local.get ,src) (:local.get i) (:array.get :limbs) :i64.extend_i32_u
                     (:local.get s) :i64.extend_i32_u :i64.shl (:local.get carry) :i64.or (:local.set tt)
                     (:local.get ,dst) (:local.get i) (:local.get tt) (:i64.const ,+limb-mask+) :i64.and
                     :i32.wrap_i64 (:array.set :limbs)
                     (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
                     (:local.get i) (:i32.const 1) :i32.add (:local.set i)
                     (:br sagain))))
  '(:local.get un) '(:local.get m0) '(:local.get carry) :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get m0) '(:local.get n) :i32.sub '(:local.tee m) '(:i32.const 1) :i32.add '(:array.new_default :limbs)
  '(:local.set q)
  '(:local.get vn) '(:local.get n) '(:i32.const 1) :i32.sub '(:array.get :limbs) :i64.extend_i32_u '(:local.set vtop)
  '(:local.get vn) '(:local.get n) '(:i32.const 2) :i32.sub '(:array.get :limbs) :i64.extend_i32_u '(:local.set vnext)
  '(:local.get m) '(:local.set j)
  `(:block jdone nil
     (:loop jagain nil
       (:local.get j) (:i32.const 0) :i32.lt_s (:br_if jdone)
       (:local.get un) (:local.get j) (:local.get n) :i32.add (:array.get :limbs) :i64.extend_i32_u
       (:i64.const ,+limb-bits+) :i64.shl
       (:local.get un) (:local.get j) (:local.get n) :i32.add (:i32.const 1) :i32.sub (:array.get :limbs)
       :i64.extend_i32_u :i64.or (:local.tee num) (:local.get vtop) :i64.div_u (:local.set qhat)
       (:local.get num) (:local.get vtop) :i64.rem_u (:local.set rhat)
       ;; correct qhat (at most twice)
       (:block cdone nil
         (:loop cagain nil
           (:local.get rhat) (:i64.const ,(expt 2 +limb-bits+)) :i64.ge_u (:br_if cdone)
           (:local.get qhat) (:i64.const ,(expt 2 +limb-bits+)) :i64.ge_u
           (:local.get qhat) (:local.get vnext) :i64.mul
           (:local.get rhat) (:i64.const ,+limb-bits+) :i64.shl
           (:local.get un) (:local.get j) (:local.get n) :i32.add (:i32.const 2) :i32.sub (:array.get :limbs)
           :i64.extend_i32_u :i64.or :i64.gt_u :i32.or :i32.eqz (:br_if cdone)
           (:local.get qhat) (:i64.const 1) :i64.sub (:local.set qhat)
           (:local.get rhat) (:local.get vtop) :i64.add (:local.set rhat)
           (:br cagain)))
       ;; un[j..j+n] -= qhat * vn
       (:i64.const 0) (:local.set borrow) (:i64.const 0) (:local.set carry) (:i32.const 0) (:local.set i)
       (:block mdone nil
         (:loop magain nil
           (:local.get i) (:local.get n) :i32.ge_u (:br_if mdone)
           (:local.get qhat) (:local.get vn) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.mul
           (:local.get carry) :i64.add (:local.tee p) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
           (:local.get un) (:local.get i) (:local.get j) :i32.add (:array.get :limbs) :i64.extend_i32_u
           (:local.get p) (:i64.const ,+limb-mask+) :i64.and :i64.sub (:local.get borrow) :i64.sub (:local.tee tt)
           (:i64.const 0) :i64.lt_s
           (:if nil nil
                ((:local.get tt) (:i64.const ,(expt 2 +limb-bits+)) :i64.add (:local.set tt) (:i64.const 1) (:local.set borrow))
                ((:i64.const 0) (:local.set borrow)))
           (:local.get un) (:local.get i) (:local.get j) :i32.add (:local.get tt) :i32.wrap_i64 (:array.set :limbs)
           (:local.get i) (:i32.const 1) :i32.add (:local.set i)
           (:br magain)))
       (:local.get un) (:local.get j) (:local.get n) :i32.add (:array.get :limbs) :i64.extend_i32_u
       (:local.get carry) :i64.sub (:local.get borrow) :i64.sub (:local.tee tt) (:i64.const 0) :i64.lt_s
       (:if nil nil
            (;; add back
             (:local.get un) (:local.get j) (:local.get n) :i32.add
             (:local.get tt) (:i64.const ,(expt 2 +limb-bits+)) :i64.add :i32.wrap_i64 (:array.set :limbs)
             (:local.get qhat) (:i64.const 1) :i64.sub (:local.set qhat)
             (:i64.const 0) (:local.set carry) (:i32.const 0) (:local.set i)
             (:block adone nil
               (:loop aagain nil
                 (:local.get i) (:local.get n) :i32.ge_u (:br_if adone)
                 (:local.get un) (:local.get i) (:local.get j) :i32.add (:array.get :limbs) :i64.extend_i32_u
                 (:local.get vn) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.add
                 (:local.get carry) :i64.add (:local.set tt)
                 (:local.get un) (:local.get i) (:local.get j) :i32.add
                 (:local.get tt) (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)
                 (:local.get tt) (:i64.const ,+limb-bits+) :i64.shr_u (:local.set carry)
                 (:local.get i) (:i32.const 1) :i32.add (:local.set i)
                 (:br aagain)))
             (:local.get un) (:local.get j) (:local.get n) :i32.add
             (:local.get un) (:local.get j) (:local.get n) :i32.add (:array.get :limbs) :i64.extend_i32_u
             (:local.get carry) :i64.add (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs))
            ((:local.get un) (:local.get j) (:local.get n) :i32.add (:local.get tt) :i32.wrap_i64 (:array.set :limbs)))
       (:local.get q) (:local.get j) (:local.get qhat) :i32.wrap_i64 (:array.set :limbs)
       (:local.get j) (:i32.const 1) :i32.sub (:local.set j)
       (:br jagain)))
  ;; remainder: the low n limbs of un >> s
  '(:local.get n) '(:array.new_default :limbs) '(:local.set r)
  '(:i32.const 0) '(:local.set i)
  `(:block rdone nil
     (:loop ragain nil
       (:local.get i) (:local.get n) :i32.ge_u (:br_if rdone)
       (:local.get r) (:local.get i)
       (:local.get un) (:local.get i) (:i32.const 1) :i32.add (:array.get :limbs) :i64.extend_i32_u
       (:i64.const ,+limb-bits+) :i64.shl
       (:local.get un) (:local.get i) (:array.get :limbs) :i64.extend_i32_u :i64.or
       (:local.get s) :i64.extend_i32_u :i64.shr_u (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64
       (:array.set :limbs)
       (:local.get i) (:i32.const 1) :i32.add (:local.set i)
       (:br ragain)))
  '(:global.get :mv-buf) '(:i32.const 0) '(:local.get r) '(:array.set :vector)
  '(:i32.const 2) '(:global.set :mv-count)
  '(:local.get q) :ref.as_non_null)
(defprim clysm-sys::%mag-divmod-knuth (u v) :mv `(,@u ,@v (:call :mag-divmod)))

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
;;;
;;; The generic helpers work on the common representations without calling
;;; Lisp: fixnums, integers within 64 bits (a $bignum whose VALUE is set),
;;; single and double floats.  Everything else (ratios, complex numbers,
;;; larger bignums, results that overflow, are not finite or may be an
;;; underflow, and arguments that are not numbers) goes to the Lisp function,
;;; which signals as before.
(defun num-decode-code (p x d k slow &optional (floats t))
  "Decode parameter P: a fixnum or an integer within 64 bits into the i64
local X (K := 0), a single / double float into the f64 local D (K := 1 / 2);
anything else branches to SLOW.  Uses the i32 local V."
  (let ((done (gensym "DEC")))
    `((:block ,done nil
        (:local.get ,p) (:ref.test :i31)
        (:if nil nil
             ((:local.get ,p) (:ref.cast! :i31) :i31.get_s (:local.tee v)
              (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.ge_u (:br_if ,slow)
              (:local.get v) :i64.extend_i32_s (:local.set ,x) (:i32.const 0) (:local.set ,k) (:br ,done)))
        (:local.get ,p) (:ref.test :bignum)
        (:if nil nil
             ((:local.get ,p) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee ,x) :i64.eqz (:br_if ,slow)
              (:i32.const 0) (:local.set ,k) (:br ,done)))
        ,@(when floats
            `((:local.get ,p) (:ref.test :double)
              (:if nil nil
                   ((:local.get ,p) (:ref.cast! :double) (:struct.get :double 0) (:local.set ,d)
                    (:i32.const 2) (:local.set ,k) (:br ,done)))
              (:local.get ,p) (:ref.test :single)
              (:if nil nil
                   ((:local.get ,p) (:ref.cast! :single) (:struct.get :single 0) :f64.promote_f32 (:local.set ,d)
                    (:i32.const 1) (:local.set ,k) (:br ,done)))))
        (:br ,slow)))))

(defun fixnum-pair-code (body)
  "When both parameters are fixnums: BODY with their values in the i32 locals X and Y."
  `((:local.get 0) (:ref.test :i31) (:local.get 1) (:ref.test :i31) :i32.and
    (:if nil nil
         ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.set x)
          (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.set y)
          (:local.get x) (:i32.const ,+char-tag+) :i32.add
          (:local.get y) (:i32.const ,+char-tag+) :i32.add :i32.or
          (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
          (:if nil nil ,body)))))

;;; the product of two i64s (neither -2^63) that does not fit in an i64
(defhelper :mul-wide :ll-e ((a :i64) (b :i64) (p00 :i64) (mid :i64) (mid2 :i64) (lo :i64) (hi :i64)
                            (n :i32) (m (:ref :null :limbs)))
  '(:local.get 0) '(:i64.const 0) :i64.lt_s '(:if nil :i64 ((:i64.const 0) (:local.get 0) :i64.sub) ((:local.get 0)))
  '(:local.set a)
  '(:local.get 1) '(:i64.const 0) :i64.lt_s '(:if nil :i64 ((:i64.const 0) (:local.get 1) :i64.sub) ((:local.get 1)))
  '(:local.set b)
  ;; 64 x 64 -> 128 bits (hi, lo)
  '(:local.get a) '(:i64.const #xFFFFFFFF) :i64.and '(:local.get b) '(:i64.const #xFFFFFFFF) :i64.and :i64.mul
  '(:local.set p00)
  '(:local.get a) '(:i64.const #xFFFFFFFF) :i64.and '(:local.get b) '(:i64.const 32) :i64.shr_u :i64.mul
  '(:local.get p00) '(:i64.const 32) :i64.shr_u :i64.add '(:local.set mid)
  '(:local.get a) '(:i64.const 32) :i64.shr_u '(:local.get b) '(:i64.const #xFFFFFFFF) :i64.and :i64.mul
  '(:local.get mid) '(:i64.const #xFFFFFFFF) :i64.and :i64.add '(:local.set mid2)
  '(:local.get mid2) '(:i64.const 32) :i64.shl '(:local.get p00) '(:i64.const #xFFFFFFFF) :i64.and :i64.or
  '(:local.set lo)
  '(:local.get a) '(:i64.const 32) :i64.shr_u '(:local.get b) '(:i64.const 32) :i64.shr_u :i64.mul
  '(:local.get mid) '(:i64.const 32) :i64.shr_u :i64.add '(:local.get mid2) '(:i64.const 32) :i64.shr_u :i64.add
  '(:local.set hi)
  ;; limbs: 3 (bits 56..83 are not all zero), 4 or 5
  '(:i32.const 3) '(:i32.const 4) '(:local.get hi) '(:i64.const 20) :i64.shr_u :i64.eqz :select
  '(:i32.const 5) '(:local.get hi) '(:i64.const 48) :i64.shr_u :i64.eqz :select
  '(:local.tee n) '(:array.new_default :limbs) '(:local.set m)
  `(:local.get m) '(:i32.const 0) '(:local.get lo) `(:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 '(:array.set :limbs)
  `(:local.get m) '(:i32.const 1) '(:local.get lo) `(:i64.const ,+limb-bits+) :i64.shr_u
  `(:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 '(:array.set :limbs)
  `(:local.get m) '(:i32.const 2) '(:local.get lo) '(:i64.const 56) :i64.shr_u '(:local.get hi) '(:i64.const 8) :i64.shl
  :i64.or `(:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 '(:array.set :limbs)
  '(:local.get n) '(:i32.const 3) :i32.gt_u
  `(:if nil nil ((:local.get m) (:i32.const 3) (:local.get hi) (:i64.const 20) :i64.shr_u
                 (:i64.const ,+limb-mask+) :i64.and :i32.wrap_i64 (:array.set :limbs)))
  '(:local.get n) '(:i32.const 4) :i32.gt_u
  `(:if nil nil ((:local.get m) (:i32.const 4) (:local.get hi) (:i64.const 48) :i64.shr_u :i32.wrap_i64
                 (:array.set :limbs)))
  '(:local.get 0) '(:local.get 1) :i64.xor '(:i64.const 0) :i64.lt_s
  '(:local.get m) '(:i64.const 0) '(:struct.new :bignum))

(defun arith-helper-body (opcode lisp-fn)
  "OPCODE: 0 + 1 - 2 * 3 /."
  (let ((min64 (- (expt 2 63)))
        (fop (ecase opcode (0 :f64.add) (1 :f64.sub) (2 :f64.mul) (3 :f64.div))))
    (flet ((zero-check (r zero eq)
             ;; a zero result of nonzero operands may be an underflow
             (case opcode
               (2 `((:local.get ,r) ,zero ,eq (:local.get da) (:f64.const 0) :f64.ne :i32.and
                    (:local.get db) (:f64.const 0) :f64.ne :i32.and (:br_if slow)))
               (3 `((:local.get ,r) ,zero ,eq (:local.get da) (:f64.const 0) :f64.ne :i32.and (:br_if slow)))))
           (finite-check (r sub zero ne)
             `((:local.get ,r) (:local.get ,r) ,sub ,zero ,ne (:br_if slow))))
      `(,@(fixnum-pair-code
           (ecase opcode
             ((0 1) `((:local.get x) (:local.get y) ,(if (= opcode 0) :i32.add :i32.sub) (:local.tee x)
                      (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                      (:if nil nil ((:local.get x) :ref.i31 :return))
                      (:local.get x) :i64.extend_i32_s (:call :i64-int) :return))
             (2 `((:local.get x) :i64.extend_i32_s (:local.get y) :i64.extend_i32_s :i64.mul (:local.tee r)
                  (:i64.const ,+char-tag+) :i64.add (:i64.const ,(* 2 +char-tag+)) :i64.lt_u
                  (:if nil nil ((:local.get r) :i32.wrap_i64 :ref.i31 :return))
                  (:local.get r) (:call :i64-int) :return))
             (3 `((:local.get y)
                  (:if nil nil
                       ((:local.get x) (:local.get y) :i32.rem_s :i32.eqz
                        (:if nil nil ((:local.get x) :i64.extend_i32_s (:local.get y) :i64.extend_i32_s :i64.div_s
                                      (:call :i64-int) :return))))))))
        (:block slow nil
          ,@(num-decode-code 0 'xa 'da 'ka 'slow)
          ,@(num-decode-code 1 'xb 'db 'kb 'slow)
          (:local.get ka) (:local.get kb) :i32.or :i32.eqz
          (:if nil nil
               ,(ecase opcode
                  (0 `((:local.get xa) (:local.get xb) :i64.add (:local.tee r) (:local.get xa) :i64.xor
                       (:local.get r) (:local.get xb) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if slow)
                       (:local.get r) (:call :i64-int) :return))
                  (1 `((:local.get xa) (:local.get xb) :i64.sub (:local.tee r) (:local.get xa) :i64.xor
                       (:local.get xa) (:local.get xb) :i64.xor :i64.and (:i64.const 0) :i64.lt_s (:br_if slow)
                       (:local.get r) (:call :i64-int) :return))
                  (2 `(;; both within 32 bits: the product fits
                       (:local.get xa) (:i64.const ,(expt 2 31)) :i64.add
                       (:local.get xb) (:i64.const ,(expt 2 31)) :i64.add :i64.or (:i64.const 32) :i64.shr_u :i64.eqz
                       (:if nil nil ((:local.get xa) (:local.get xb) :i64.mul (:call :i64-int) :return))
                       (:local.get xa) (:i64.const ,min64) :i64.eq (:br_if slow)
                       (:local.get xb) (:i64.const ,min64) :i64.eq (:br_if slow)
                       (:local.get xa) :i64.eqz (:if nil nil ((:i32.const 0) :ref.i31 :return))
                       (:local.get xa) (:local.get xb) :i64.mul (:local.tee r)
                       (:local.get xa) :i64.div_s (:local.get xb) :i64.eq
                       (:if nil nil ((:local.get r) (:call :i64-int) :return))
                       (:local.get xa) (:local.get xb) (:call :mul-wide) :return))
                  (3 `((:local.get xb) :i64.eqz (:br_if slow)
                       (:local.get xa) (:i64.const ,min64) :i64.eq (:br_if slow)
                       (:local.get xa) (:local.get xb) :i64.rem_s :i64.eqz :i32.eqz (:br_if slow)
                       (:local.get xa) (:local.get xb) :i64.div_s (:call :i64-int) :return))))
          ;; floats (an integer operand is converted)
          (:local.get ka) :i32.eqz (:if nil nil ((:local.get xa) :f64.convert_i64_s (:local.set da)))
          (:local.get kb) :i32.eqz (:if nil nil ((:local.get xb) :f64.convert_i64_s (:local.set db)))
          ,@(when (= opcode 3) '((:local.get db) (:f64.const 0) :f64.eq (:br_if slow)))
          (:local.get da) (:local.get db) ,fop (:local.set dr)
          (:local.get ka) (:local.get kb) :i32.or (:i32.const 2) :i32.and
          (:if nil nil
               (,@(finite-check 'dr :f64.sub '(:f64.const 0) :f64.ne)
                ,@(zero-check 'dr '(:f64.const 0) :f64.eq)
                (:local.get dr) (:struct.new :double) :return))
          (:local.get dr) :f32.demote_f64 (:local.set fr)
          ,@(finite-check 'fr :f32.sub '(:f32.const 0) :f32.ne)
          ,@(zero-check 'fr '(:f32.const 0) :f32.eq)
          (:local.get fr) (:struct.new :single) :return)
        (:local.get 0) (:local.get 1)
        (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))))))

(macrolet ((arith (name opcode lisp-fn)
             `(defhelper ,name :ee-e ((x :i32) (y :i32) (r :i64) (v :i32) (xa :i64) (xb :i64)
                                      (ka :i32) (kb :i32) (da :f64) (db :f64) (dr :f64) (fr :f32))
                (cons :progn-marker (arith-helper-body ,opcode ',lisp-fn)))))
  (arith :add 0 clysm-sys::%generic-add)
  (arith :sub 1 clysm-sys::%generic-sub)
  (arith :mul 2 clysm-sys::%generic-mul)
  (arith :div 3 clysm-sys::%generic-div))

(defprim / (a b) :value `(,@a ,@b (:call :div)))
(defprim / (a) :value `(,@(fixnum-code 1) ,@a (:call :div)))

(defhelper :negate :e-e ((x :i32) (w :i64))
  ;; fixnums other than most-negative-fixnum negate inline; (- 0.0) must be -0.0
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee x)
         (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:local.get x) (:i32.const ,(- +char-tag+)) :i32.ne :i32.and
         (:if nil nil ((:i32.const 0) (:local.get x) :i32.sub :ref.i31 :return))))
  '(:local.get 0) '(:ref.test :double)
  '(:if nil nil ((:local.get 0) (:ref.cast! :double) (:struct.get :double 0) :f64.neg (:struct.new :double) :return))
  '(:local.get 0) '(:ref.test :bignum)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee w) :i64.eqz
         (:local.get w) (:i64.const ,(- (expt 2 63))) :i64.eq :i32.or :i32.eqz
         (:if nil nil ((:i64.const 0) (:local.get w) :i64.sub (:call :i64-int) :return))))
  '(:local.get 0)
  `(:global.get ,(sym-global 'clysm-sys::%negate)) '(:call (:callsym 1)))

(defun compare-helper-body (iop fop lisp-fn)
  ;; a comparison of an integer with a float is exact (CLHS 12.1.4.1): an
  ;; integer beyond 2^53 goes to the Lisp function
  (flet ((to-f64 (k x d)
           `((:local.get ,k) :i32.eqz
             (:if nil nil
                  ((:local.get ,x) (:i64.const ,(expt 2 53)) :i64.add (:i64.const ,(expt 2 54)) :i64.gt_u (:br_if slow)
                   (:local.get ,x) :f64.convert_i64_s (:local.set ,d))))))
    `(,@(fixnum-pair-code `((:local.get x) (:local.get y) ,(if (eq iop :i64.lt_s) :i32.lt_s :i32.eq) :return))
      (:block slow nil
        ,@(num-decode-code 0 'xa 'da 'ka 'slow)
        ,@(num-decode-code 1 'xb 'db 'kb 'slow)
        (:local.get ka) (:local.get kb) :i32.or :i32.eqz
        (:if nil nil ((:local.get xa) (:local.get xb) ,iop :return))
        ,@(to-f64 'ka 'xa 'da)
        ,@(to-f64 'kb 'xb 'db)
        (:local.get da) (:local.get db) ,fop :return)
      (:local.get 0) (:local.get 1)
      (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))
      ,(car (nil-code)) :ref.eq :i32.eqz)))

(defhelper :lt :ee-i ((x :i32) (y :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-helper-body :i64.lt_s :f64.lt 'clysm-sys::%generic-lt)))

(defhelper :num-eq :ee-i ((x :i32) (y :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64))
  (cons :progn-marker (compare-helper-body :i64.eq :f64.eq 'clysm-sys::%generic-num-eq)))

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
  '(:local.get 0) '(:ref.cast! :symbol) `(:struct.get :symbol ,+sym-value+) '(:array.set :vector)
  '(:local.get 0) '(:ref.cast! :symbol) '(:local.get 1) `(:struct.set :symbol ,+sym-value+)
  '(:global.get :bsp) '(:i32.const 2) :i32.add '(:global.set :bsp))

(defhelper :unbind-to :i-v ((sp :i32))
  '(:block done nil
     (:loop again nil
       (:global.get :bsp) (:local.get 0) :i32.le_s (:br_if done)
       (:global.get :bsp) (:i32.const 2) :i32.sub (:local.tee sp) (:global.set :bsp)
       ;; sym.value = old
       (:global.get :bstack) (:local.get sp) (:array.get :vector) (:ref.cast! :symbol)
       (:global.get :bstack) (:local.get sp) (:i32.const 1) :i32.add (:array.get :vector)
       (:struct.set :symbol 2)
       ;; (the popped entries are not cleared: the next bindings overwrite them)
       (:br again))))

(defhelper :progv-bind :ee-v ((syms :eqref) (vals :eqref))
  '(:local.get 0) '(:local.set syms) '(:local.get 1) '(:local.set vals)
  `(:block done nil
     (:loop again nil
       (:local.get syms) (:ref.test :cons) :i32.eqz (:br_if done)
       (:local.get syms) (:ref.cast :cons) (:struct.get :cons 0) (:ref.cast :symbol)
       (:local.get vals) (:ref.test :cons)
       (:if nil :eqref ((:local.get vals) (:ref.cast :cons) (:struct.get :cons 0)) ((:ref.null :eq)))
       (:call :bind)
       (:local.get syms) (:ref.cast :cons) (:struct.get :cons 1) (:local.set syms)
       (:local.get vals) (:ref.test :cons)
       (:if nil nil ((:local.get vals) (:ref.cast :cons) (:struct.get :cons 1) (:local.set vals)))
       (:br again))))

;;; catch tags
(defhelper :check-catch-tag :e-v ((sp :i32))
  ;; the innermost catch's tag is the value of *catch-tags*; the bindings of
  ;; *catch-tags* on the binding stack saved the others (the outermost saved
  ;; its unbound value, which no tag is EQ to)
  `(:global.get ,(sym-global 'clysm-sys::*catch-tags*)) `(:struct.get :symbol ,+sym-value+) '(:local.get 0) :ref.eq
  '(:if nil nil (:return))
  '(:global.get :bsp) '(:local.set sp)
  `(:block done nil
     (:loop again nil
       (:local.get sp) (:i32.const 2) :i32.lt_s (:br_if done)
       (:local.get sp) (:i32.const 2) :i32.sub (:local.set sp)
       (:global.get :bstack) (:local.get sp) (:array.get :vector)
       (:global.get ,(sym-global 'clysm-sys::*catch-tags*)) :ref.eq
       (:if nil nil
            ((:global.get :bstack) (:local.get sp) (:i32.const 1) :i32.add (:array.get :vector)
             (:local.get 0) :ref.eq (:if nil nil (:return))))
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

(defhelper :to-f64 :e-d ((v :i32) (w :i64))
  `(:block notd :eqref
     (:local.get 0) (:br_on_cast_fail notd :eqref (:ref :double)) (:struct.get :double 0) :return)
  :drop
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee v)
                 (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                 (:if nil nil ((:local.get v) :f64.convert_i32_s :return))))
  '(:local.get 0) '(:ref.test :single)
  '(:if nil nil ((:local.get 0) (:ref.cast! :single) (:struct.get :single 0) :f64.promote_f32 :return))
  '(:local.get 0) '(:ref.test :bignum)
  '(:if nil nil ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee w) :i64.eqz :i32.eqz
                 (:if nil nil ((:local.get w) :f64.convert_i64_s :return))))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%to-double)) '(:call (:callsym 1))
  '(:ref.cast :double) '(:struct.get :double 0))

;;; FLOAT: (float x) of a float or a fixnum, (float x prototype)
(defhelper :float1 :e-e ((v :i32))
  '(:local.get 0) '(:ref.test :double) '(:local.get 0) '(:ref.test :single) :i32.or
  '(:if nil nil ((:local.get 0) :return))
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee v)
                 (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                 (:if nil nil ((:local.get v) :f32.convert_i32_s (:struct.new :single) :return))))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%to-single)) '(:call (:callsym 1)))
(defhelper :float2 :ee-e ((v :i32))
  '(:local.get 1) '(:ref.test :double)
  '(:if nil nil ((:local.get 0) (:ref.test :double) (:if nil nil ((:local.get 0) :return))
                 (:local.get 0) (:call :to-f64) (:struct.new :double) :return))
  '(:local.get 1) '(:ref.test :single)
  `(:if nil nil ((:local.get 0) (:ref.test :single) (:if nil nil ((:local.get 0) :return))
                 (:local.get 0) (:ref.test :i31)
                 (:if nil nil ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee v)
                               (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
                               (:if nil nil ((:local.get v) :f32.convert_i32_s (:struct.new :single) :return))))))
  '(:local.get 0) '(:local.get 1) `(:global.get ,(sym-global 'clysm-sys::%float2)) '(:call (:callsym 2)))
(defprim float (x) :value `(,@x (:call :float1)))
(defprim float (x proto) :value `(,@x ,@proto (:call :float2)))

;;; an integral f64 as an integer
(defhelper :f64-int :d-e ()
  '(:local.get 0) :f64.abs '(:f64.const 9.223372036854775d18) :f64.lt
  '(:if nil nil ((:local.get 0) :i64.trunc_sat_f64_s (:call :i64-int) :return))
  '(:local.get 0) '(:struct.new :double)
  `(:global.get ,(sym-global 'clysm-sys::%float-to-integer-big)) '(:call (:callsym 1)))
(defprim clysm-sys::%double-to-integer (d) :value `(,@(f64-code d) :f64.trunc (:call :f64-int)))

;;; ------------------------------------------------------------
;;; TRUNCATE / FLOOR / CEILING / ROUND
;;; ------------------------------------------------------------
;;; Modes: 0 truncate, 1 floor, 2 ceiling, 3 round.  The helper :divmod
;;; returns the quotient and leaves the remainder as the second value.  It
;;; computes fixnums, integers within 64 bits, and single / double floats
;;; (with integers up to 2^53) itself, by the algorithm of the Lisp
;;; functions (truncate, then adjust by the remainder); everything else, and
;;; whatever would signal, goes to %DIVMOD-SLOW.

(defun ity (ty name)
  (intern (concatenate 'string (symbol-name ty) "." (symbol-name name)) :keyword))

(defun int-divmod-adjust-code (ty mode x y q r tmp tmp2)
  "Adjust the truncated quotient Q and remainder R of X / Y (locals of type TY, :i32 or :i64) for MODE."
  (flet ((o (n) (ity ty n)) (c (n) `(,(ity ty :const) ,n)))
    (let ((up `((:local.get ,q) ,(c 1) ,(o :add) (:local.set ,q)
                (:local.get ,r) (:local.get ,y) ,(o :sub) (:local.set ,r)))
          (down `((:local.get ,q) ,(c 1) ,(o :sub) (:local.set ,q)
                  (:local.get ,r) (:local.get ,y) ,(o :add) (:local.set ,r))))
      (ecase mode
        (0 nil)
        (1 `((:local.get ,r) ,(o :eqz) :i32.eqz
             (:local.get ,r) (:local.get ,y) ,(o :xor) ,(c 0) ,(o :lt_s) :i32.and (:if nil nil ,down)))
        (2 `((:local.get ,r) ,(o :eqz) :i32.eqz
             (:local.get ,r) (:local.get ,y) ,(o :xor) ,(c 0) ,(o :ge_s) :i32.and (:if nil nil ,up)))
        (3 `(;; |r| against |y| - |r| (unsigned): twice the remainder against the divisor
             (:local.get ,r) ,(c 0) ,(o :lt_s)
             (:if nil ,ty (,(c 0) (:local.get ,r) ,(o :sub)) ((:local.get ,r))) (:local.set ,tmp)
             (:local.get ,y) ,(c 0) ,(o :lt_s)
             (:if nil ,ty (,(c 0) (:local.get ,y) ,(o :sub)) ((:local.get ,y)))
             (:local.get ,tmp) ,(o :sub) (:local.set ,tmp2)
             (:local.get ,tmp) (:local.get ,tmp2) ,(o :gt_u)
             (:local.get ,tmp) (:local.get ,tmp2) ,(o :eq)
             (:local.get ,q) ,@(when (eq ty :i64) '(:i32.wrap_i64)) (:i32.const 1) :i32.and :i32.and :i32.or
             (:if nil nil ((:local.get ,x) (:local.get ,y) ,(o :xor) ,(c 0) ,(o :ge_s) (:if nil nil ,up ,down)))))))))

(defun float-divmod-code (mode slow da db q r adj single tmp &optional unit)
  "From the f64 locals DA and DB: the truncated quotient in Q (integral), the
adjustment of the quotient for MODE in the i32 local ADJ (-1, 0 or 1) and the
remainder in R.  SINGLE (an i32 local, or NIL when the result is a double):
when set, every step is rounded to single precision.  Branches to SLOW when
the divisor is zero or a value is not finite.  UNIT: DB is known to be 1 (the
division and the product are the identity)."
  (flet ((rnd (l) (when single
                    `((:local.get ,single)
                      (:if nil nil ((:local.get ,l) :f32.demote_f64 :f64.promote_f32 (:local.set ,l)))))))
    (let ((up `((:i32.const 1) (:local.set ,adj)
                (:local.get ,r) (:local.get ,db) :f64.sub (:local.set ,r) ,@(rnd r)))
          (down `((:i32.const -1) (:local.set ,adj)
                  (:local.get ,r) (:local.get ,db) :f64.add (:local.set ,r) ,@(rnd r))))
      `(;; (+ 0: the quotient -0.0 is the integer 0)
        (:local.get ,da) ,@(unless unit `((:local.get ,db) :f64.div)) :f64.trunc (:f64.const 0) :f64.add (:local.set ,q)
        ;; remainder: number - quotient * divisor (not finite when the
        ;; quotient is not: a zero divisor, an infinity, a NaN)
        (:local.get ,q) (:local.set ,tmp) ,@(rnd tmp)
        ,@(unless unit `((:local.get ,tmp) (:local.get ,db) :f64.mul (:local.set ,tmp) ,@(rnd tmp)))
        (:local.get ,da) (:local.get ,tmp) :f64.sub (:local.set ,r) ,@(rnd r)
        (:local.get ,r) (:local.get ,r) :f64.sub (:f64.const 0) :f64.ne (:br_if ,slow)
        (:i32.const 0) (:local.set ,adj)
        ,@(ecase mode
            (0 nil)
            (1 `((:local.get ,r) (:f64.const 0) :f64.ne
                 (:local.get ,r) (:f64.const 0) :f64.gt (:local.get ,r) (:f64.const 0) :f64.lt
                 (:local.get ,db) (:f64.const 0) :f64.lt :select :i32.and
                 (:if nil nil ,down)))
            (2 `((:local.get ,r) (:f64.const 0) :f64.ne
                 (:local.get ,r) (:f64.const 0) :f64.lt (:local.get ,r) (:f64.const 0) :f64.gt
                 (:local.get ,db) (:f64.const 0) :f64.lt :select :i32.and
                 (:if nil nil ,up)))
            (3 `((:local.get ,r) (:f64.const 2d0) :f64.mul (:local.set ,tmp) ,@(rnd tmp)
                 (:local.get ,tmp) :f64.abs (:local.tee ,tmp) (:local.get ,tmp) :f64.sub (:f64.const 0) :f64.ne
                 (:br_if ,slow)
                 (:local.get ,tmp) (:local.get ,db) :f64.abs :f64.gt
                 (:local.get ,tmp) (:local.get ,db) :f64.abs :f64.eq
                 (:local.get ,q) :f64.abs (:f64.const 9007199254740992d0) :f64.lt :i32.and
                 (:local.get ,q) :i64.trunc_sat_f64_s :i32.wrap_i64 (:i32.const 1) :i32.and :i32.and :i32.or
                 (:if nil nil ((:local.get ,da) (:f64.const 0) :f64.lt (:local.get ,db) (:f64.const 0) :f64.lt :i32.eq
                               (:if nil nil ,up ,down))))))))))

(defhelper :divmod :eei-e ((x :i32) (y :i32) (qi :i32) (ri :i32) (ti :i32) (ti2 :i32)
                           (v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64)
                           (w :i64) (wr :i64) (wt :i64) (wt2 :i64)
                           (q :f64) (r :f64) (tmp :f64) (adj :i32) (single :i32))
  ;; two fixnums
  (fixnum-pair-code
   `((:local.get y) :i32.eqz :i32.eqz
     (:if nil nil
          ((:local.get x) (:local.get y) :i32.div_s (:local.set qi)
           (:local.get x) (:local.get y) :i32.rem_s (:local.set ri)
           ,@(loop for mode from 1 to 3
                   collect `(:local.get 2) collect `(:i32.const ,mode) collect :i32.eq
                   collect `(:if nil nil ,(int-divmod-adjust-code :i32 mode 'x 'y 'qi 'ri 'ti 'ti2)))
           (:global.get :mv-buf) (:i32.const 0) (:local.get ri) :ref.i31 (:array.set :vector)
           (:i32.const 2) (:global.set :mv-count)
           (:local.get qi) (:call :box-i32) :return))))
  `(:block slow nil
     ,@(num-decode-code 0 'xa 'da 'ka 'slow)
     ,@(num-decode-code 1 'xb 'db 'kb 'slow)
     (:local.get ka) (:local.get kb) :i32.or :i32.eqz
     (:if nil nil
          (;; integers within 64 bits
           (:local.get xb) :i64.eqz (:br_if slow)
           (:local.get xa) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if slow)
           (:local.get xa) (:local.get xb) :i64.div_s (:local.set w)
           (:local.get xa) (:local.get xb) :i64.rem_s (:local.set wr)
           ,@(loop for mode from 1 to 3
                   collect `(:local.get 2) collect `(:i32.const ,mode) collect :i32.eq
                   collect `(:if nil nil ,(int-divmod-adjust-code :i64 mode 'xa 'xb 'w 'wr 'wt 'wt2)))
           (:global.get :mv-buf) (:i32.const 0) (:local.get wr) (:call :i64-int) (:array.set :vector)
           (:i32.const 2) (:global.set :mv-count)
           (:local.get w) (:call :i64-int) :return))
     ;; floats; an integer operand must convert exactly
     ,@(loop for (k xx d) in '((ka xa da) (kb xb db))
             append `((:local.get ,k) :i32.eqz
                      (:if nil nil
                           ((:local.get ,xx) (:i64.const ,(expt 2 53)) :i64.add (:i64.const ,(expt 2 54)) :i64.gt_u
                            (:br_if slow)
                            (:local.get ,xx) :f64.convert_i64_s (:local.set ,d)))))
     (:local.get ka) (:local.get kb) :i32.or (:i32.const 2) :i32.and :i32.eqz (:local.set single)
     ,@(loop for mode from 0 to 3
             collect `(:local.get 2) collect `(:i32.const ,mode) collect :i32.eq
             collect `(:if nil nil ,(float-divmod-code mode 'slow 'da 'db 'q 'r 'adj 'single 'tmp)))
     (:local.get q) :f64.abs (:f64.const 4.611686018427388d18) :f64.lt :i32.eqz (:br_if slow)
     (:global.get :mv-buf) (:i32.const 0)
     (:local.get single)
     (:if nil :eqref ((:local.get r) :f32.demote_f64 (:struct.new :single)) ((:local.get r) (:struct.new :double)))
     (:array.set :vector)
     (:i32.const 2) (:global.set :mv-count)
     (:local.get q) :i64.trunc_sat_f64_s (:local.get adj) :i64.extend_i32_s :i64.add (:call :i64-int) :return)
  '(:local.get 0) '(:local.get 1) '(:local.get 2) :ref.i31
  `(:global.get ,(sym-global 'clysm-sys::%divmod-slow)) '(:call (:callsym 3)))

(macrolet ((div (name mode)
             `(progn
                (defprim ,name (a b) :mv `(,@a ,@b (:i32.const ,,mode) (:call :divmod)))
                (defprim ,name (a) :mv `(,@a ,@(fixnum-code 1) (:i32.const ,,mode) (:call :divmod))))))
  (div truncate 0) (div floor 1) (div ceiling 2) (div round 3))

;;; ------------------------------------------------------------
;;; Bignum, ratio, complex, float primitives
;;; ------------------------------------------------------------

(defprim clysm-sys::%make-limbs (n) :value `(,@(idx-code n) (:array.new_default :limbs)))
(defprim clysm-sys::%limbs-prefix (v n) :value
  ;; a fresh limbs array of the first N elements of V
  (let ((src (new-local '(:ref :null :limbs))) (k (new-local :i32)) (r (new-local '(:ref :null :limbs))))
    `(,@v (:ref.cast :limbs) (:local.set ,src) ,@(idx-code n) (:local.tee ,k)
      (:array.new_default :limbs) (:local.tee ,r)
      (:i32.const 0) (:local.get ,src) (:i32.const 0) (:local.get ,k) (:array.copy :limbs :limbs)
      (:local.get ,r))))
(defprim clysm-sys::%string-copy (dst dstart src sstart n) :value
  ;; copy N characters of simple string SRC from SSTART into DST at DSTART
  (let ((d (new-local)))
    `(,@dst (:local.tee ,d) (:ref.cast :string) ,@(idx-code dstart) ,@src (:ref.cast :string) ,@(idx-code sstart)
      ,@(idx-code n) (:array.copy :string :string) (:local.get ,d))))
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

(macrolet ((dim-of-2d (name second)
             ;; dimension 0 / 1 of a rank-2 dimension list, else 0
             `(defhelper ,name :e-i ((r :eqref))
                '(:local.get 0) '(:ref.test :cons) :i32.eqz '(:if nil nil ((:i32.const 0) :return))
                '(:local.get 0) '(:ref.cast! :cons) '(:struct.get :cons 1) '(:local.tee r) '(:ref.test :cons) :i32.eqz
                '(:if nil nil ((:i32.const 0) :return))
                '(:local.get r) '(:ref.cast! :cons) '(:struct.get :cons 1) (car (nil-code)) :ref.eq :i32.eqz
                '(:if nil nil ((:i32.const 0) :return))
                ,(if second
                     ''(:local.get r)
                     ''(:local.get 0))
                '(:ref.cast! :cons) '(:struct.get :cons 0) '(:ref.cast :i31) :i31.get_s)))
  (dim-of-2d :dims-2d-0 nil)
  (dim-of-2d :dims-2d-1 t))

;;; two-dimensional arrays over a simple vector inline, else the Lisp functions
(defun aref2-index-code (fail)
  "Helper-body code: when local 0 is a two-dimensional array over a simple
vector and locals 1, 2 are indices within its dimensions, leave the vector
in local V and the index in local K; else branch to FAIL."
  `((:local.get 0) (:ref.test :mdarray) :i32.eqz (:br_if ,fail)
    (:local.get 0) (:ref.cast! :mdarray) (:local.set m)
    (:local.get 1) (:ref.test :i31) :i32.eqz (:br_if ,fail)
    (:local.get 2) (:ref.test :i31) :i32.eqz (:br_if ,fail)
    (:local.get 1) (:ref.cast! :i31) :i31.get_s (:local.tee x) (:local.get m) (:struct.get :mdarray 5)
    :i32.ge_u (:br_if ,fail)
    (:local.get 2) (:ref.cast! :i31) :i31.get_s (:local.tee y) (:local.get m) (:struct.get :mdarray 6) (:local.tee d1)
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

;;; ------------------------------------------------------------
;;; Irrational functions
;;; ------------------------------------------------------------
;;; SIN COS TAN ASIN ACOS ATAN EXP LOG SINH COSH TANH ASINH ACOSH ATANH of
;;; one argument are primitives: for a double, a single or an integer within
;;; 64 bits the helper calls the host function and returns a float of the
;;; argument's format (a single for an integer).  A result that is not
;;; finite (outside the domain: the result is complex; an overflow), a zero
;;; of EXP or a single zero of a nonzero double (an underflow), and every
;;; other argument go to the Lisp function %NAME-SLOW.
(defparameter *math1-functions* '(sin cos tan asin acos atan exp log sinh cosh tanh asinh acosh atanh))
(defun math-import (name) (intern (concatenate 'string "MATH-" (symbol-name name)) :keyword))
(defun math-helper (name) (intern (concatenate 'string "FN-" (symbol-name name)) :keyword))
(defun math-slow-function (name)
  (intern (concatenate 'string "%" (symbol-name name) "-SLOW") (find-package :clysm-sys)))

(defun math1-helper-body (name)
  `((:block slow nil
      ,@(num-decode-code 0 'xa 'da 'ka 'slow)
      (:local.get ka) :i32.eqz (:if nil nil ((:local.get xa) :f64.convert_i64_s (:local.set da)))
      (:local.get da) (:call ,(math-import name)) (:local.tee dr)
      (:local.get dr) :f64.sub (:f64.const 0) :f64.ne (:br_if slow)
      (:local.get ka) (:i32.const 2) :i32.eq
      (:if nil nil
           (,@(when (eq name 'exp) '((:local.get dr) (:f64.const 0) :f64.eq (:br_if slow)))
            (:local.get dr) (:struct.new :double) :return))
      (:local.get dr) :f32.demote_f64 (:local.tee fr) (:local.get fr) :f32.sub (:f32.const 0) :f32.ne (:br_if slow)
      (:local.get fr) (:f32.const 0) :f32.eq
      ,@(unless (eq name 'exp) '((:local.get dr) (:f64.const 0) :f64.ne :i32.and))
      (:br_if slow)
      (:local.get fr) (:struct.new :single) :return)
    (:local.get 0) (:global.get ,(sym-global (math-slow-function name))) (:call (:callsym 1))))

(defun define-math-helpers ()
  (dolist (name *math1-functions*)
    (let ((name name) (helper (math-helper name)))
      (setf *helpers* (remove helper *helpers* :key #'first :test #'equal))
      (push (list helper :e-e '((v :i32) (xa :i64) (ka :i32) (da :f64) (dr :f64) (fr :f32))
                  (lambda () (math1-helper-body name)))
            *helpers*)
      (let ((p (make-prim :name name :nargs 1 :kind :value
                          :fn (lambda (argv) `(,@(first argv) (:call ,helper))))))
        (setf (gethash name *prims*) (cons p (remove 1 (gethash name *prims*) :key #'prim-nargs)))))))
(define-math-helpers)

;;; (ATAN y x): floats and integers within 64 bits
(defhelper :fn-atan2 :ee-e ((v :i32) (xa :i64) (xb :i64) (ka :i32) (kb :i32) (da :f64) (db :f64))
  `(:block slow nil
     ,@(num-decode-code 0 'xa 'da 'ka 'slow)
     ,@(num-decode-code 1 'xb 'db 'kb 'slow)
     (:local.get ka) :i32.eqz (:if nil nil ((:local.get xa) :f64.convert_i64_s (:local.set da)))
     (:local.get kb) :i32.eqz (:if nil nil ((:local.get xb) :f64.convert_i64_s (:local.set db)))
     (:local.get da) (:local.get db) (:call :math-atan2) (:local.set da)
     (:local.get ka) (:local.get kb) :i32.or (:i32.const 2) :i32.and
     (:if nil nil ((:local.get da) (:struct.new :double) :return))
     (:local.get da) :f32.demote_f64 (:struct.new :single) :return)
  '(:local.get 0) '(:local.get 1) `(:global.get ,(sym-global 'clysm-sys::%atan2-slow)) '(:call (:callsym 2)))
(defprim atan (y x) :value `(,@y ,@x (:call :fn-atan2)))

;;; (EXPT base power) of doubles with a positive base
(defhelper :fn-expt :ee-e ((da :f64) (dr :f64))
  '(:block slow nil
     (:local.get 0) (:ref.test :double) (:local.get 1) (:ref.test :double) :i32.and :i32.eqz (:br_if slow)
     (:local.get 0) (:ref.cast! :double) (:struct.get :double 0) (:local.tee da) (:f64.const 0) :f64.gt :i32.eqz
     (:br_if slow)
     (:local.get da) (:local.get 1) (:ref.cast! :double) (:struct.get :double 0) (:call :math-pow) (:local.tee dr)
     (:local.get dr) :f64.sub (:f64.const 0) :f64.ne (:br_if slow)
     (:local.get dr) (:struct.new :double) :return)
  '(:local.get 0) '(:local.get 1) `(:global.get ,(sym-global 'clysm-sys::%expt-slow)) '(:call (:callsym 2)))
(defprim expt (b p) :value `(,@b ,@p (:call :fn-expt)))

;;; SQRT of a float that is not negative
(defhelper :fn-sqrt :e-e ((da :f64))
  '(:local.get 0) '(:ref.test :double)
  '(:if nil nil
        ((:local.get 0) (:ref.cast! :double) (:struct.get :double 0) (:local.tee da) (:f64.const 0) :f64.ge
         (:if nil nil ((:local.get da) :f64.sqrt (:struct.new :double) :return))))
  '(:local.get 0) '(:ref.test :single)
  '(:if nil nil
        ((:local.get 0) (:ref.cast! :single) (:struct.get :single 0) :f64.promote_f32 (:local.tee da) (:f64.const 0) :f64.ge
         (:if nil nil ((:local.get da) :f64.sqrt :f32.demote_f64 (:struct.new :single) :return))))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%sqrt-slow)) '(:call (:callsym 1)))
(defprim sqrt (x) :value `(,@x (:call :fn-sqrt)))

;;; ISQRT of an integer within 64 bits
(defhelper :isqrt :e-e ((v :i32) (xa :i64) (ka :i32) (r :i64))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     (:local.get xa) (:i64.const 0) :i64.lt_s (:br_if slow)
     (:local.get xa) :f64.convert_i64_s :f64.sqrt :i64.trunc_sat_f64_s (:local.set r)
     ;; (beyond 2^53 the conversion rounds: correct the estimate)
     (:block d1 nil
       (:loop l1 nil
         (:local.get r) (:local.get r) :i64.mul (:local.get xa) :i64.le_u (:br_if d1)
         (:local.get r) (:i64.const 1) :i64.sub (:local.set r)
         (:br l1)))
     (:block d2 nil
       (:loop l2 nil
         (:local.get r) (:i64.const 1) :i64.add (:local.get r) (:i64.const 1) :i64.add :i64.mul
         (:local.get xa) :i64.gt_u (:br_if d2)
         (:local.get r) (:i64.const 1) :i64.add (:local.set r)
         (:br l2)))
     (:local.get r) (:call :i64-int) :return)
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%isqrt-big)) '(:call (:callsym 1)))
(defprim isqrt (n) :value `(,@n (:call :isqrt)))

;;; GCD of integers within 64 bits (Euclid)
(defhelper :gcd :ee-e ((v :i32) (xa :i64) (xb :i64) (ka :i32) (tt :i64))
  `(:block slow nil
     ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
     ,@(num-decode-code 1 'xb nil 'ka 'slow nil)
     (:local.get xa) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if slow)
     (:local.get xb) (:i64.const ,(- (expt 2 63))) :i64.eq (:br_if slow)
     (:local.get xa) (:i64.const 0) :i64.lt_s (:if nil nil ((:i64.const 0) (:local.get xa) :i64.sub (:local.set xa)))
     (:local.get xb) (:i64.const 0) :i64.lt_s (:if nil nil ((:i64.const 0) (:local.get xb) :i64.sub (:local.set xb)))
     (:block done nil
       (:loop again nil
         (:local.get xb) :i64.eqz (:br_if done)
         (:local.get xa) (:local.get xb) :i64.rem_u (:local.set tt)
         (:local.get xb) (:local.set xa) (:local.get tt) (:local.set xb)
         (:br again)))
     (:local.get xa) (:call :i64-int) :return)
  '(:local.get 0) '(:local.get 1) `(:global.get ,(sym-global 'clysm-sys::%gcd-big)) '(:call (:callsym 2)))
(defprim clysm-sys::%gcd (a b) :value `(,@a ,@b (:call :gcd)))

;;; ABS of a fixnum, an integer within 64 bits or a double
(defhelper :abs :e-e ((v :i32) (w :i64))
  '(:local.get 0) '(:ref.test :i31)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee v)
         (:i32.const ,+char-tag+) :i32.add (:i32.const ,(* 2 +char-tag+)) :i32.lt_u
         (:if nil nil
              ((:local.get v) (:i32.const 0) :i32.ge_s (:if nil nil ((:local.get 0) :return))
               (:i32.const 0) (:local.get v) :i32.sub (:call :box-i32) :return))))
  '(:local.get 0) '(:ref.test :double)
  '(:if nil nil
        ((:local.get 0) (:ref.cast! :double) (:struct.get :double 0) :i64.reinterpret_f64 (:i64.const 0) :i64.ge_s
         (:if nil nil ((:local.get 0) :return))
         (:local.get 0) (:ref.cast! :double) (:struct.get :double 0) :f64.abs (:struct.new :double) :return))
  '(:local.get 0) '(:ref.test :bignum)
  `(:if nil nil
        ((:local.get 0) (:ref.cast! :bignum) (:struct.get :bignum 2) (:local.tee w) (:i64.const 0) :i64.gt_s
         (:if nil nil ((:local.get 0) :return))
         (:local.get w) :i64.eqz (:local.get w) (:i64.const ,(- (expt 2 63))) :i64.eq :i32.or :i32.eqz
         (:if nil nil ((:i64.const 0) (:local.get w) :i64.sub (:call :i64-int) :return))))
  '(:local.get 0) `(:global.get ,(sym-global 'clysm-sys::%abs-slow)) '(:call (:callsym 1)))
(defprim abs (x) :value `(,@x (:call :abs)))

;;; LOGBITP with a fixnum index and an integer within 64 bits
(defhelper :logbitp :ee-i ((v :i32) (xa :i64) (ka :i32) (i :i32))
  `(:block slow nil
     (:local.get 0) (:ref.test :i31) :i32.eqz (:br_if slow)
     ;; 0 <= index (not a character)
     (:local.get 0) (:ref.cast! :i31) :i31.get_s (:local.tee i) (:i32.const ,+char-tag+) :i32.ge_u (:br_if slow)
     ,@(num-decode-code 1 'xa nil 'ka 'slow nil)
     (:local.get xa)
     (:local.get i) (:i32.const 63) (:local.get i) (:i32.const 63) :i32.lt_u :select :i64.extend_i32_u
     :i64.shr_s :i32.wrap_i64 (:i32.const 1) :i32.and :return)
  '(:local.get 0) '(:local.get 1)
  `(:global.get ,(sym-global 'clysm-sys::%logbitp-slow)) '(:call (:callsym 2))
  (car (nil-code)) :ref.eq :i32.eqz)
(defprim logbitp (i n) :test `(,@i ,@n (:call :logbitp)))

;;; MOD / REM fast paths: fixnums, then integers within 64 bits
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
    (:block slow nil
      ,@(num-decode-code 0 'xa nil 'ka 'slow nil)
      ,@(num-decode-code 1 'xb nil 'ka 'slow nil)
      (:local.get xb) :i64.eqz (:br_if slow)
      ;; (the remainder of a division by -1 is 0; i64.rem_s of -2^63 by -1 is defined as 0)
      (:local.get xa) (:local.get xb) :i64.rem_s (:local.set w)
      ,@(when modp
          '((:local.get w) :i64.eqz :i32.eqz
            (:local.get w) (:local.get xb) :i64.xor (:i64.const 0) :i64.lt_s :i32.and
            (:if nil nil ((:local.get w) (:local.get xb) :i64.add (:local.set w)))))
      (:local.get w) (:call :i64-int) :return)
    (:local.get 0) (:local.get 1)
    (:global.get ,(sym-global lisp-fn)) (:call (:callsym 2))))

(defhelper :mod :ee-e ((x :i32) (y :i32) (r :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (w :i64))
  (cons :progn-marker (divop-helper-body 'clysm-sys::%mod-slow t)))
(defhelper :rem :ee-e ((x :i32) (y :i32) (r :i32) (v :i32) (xa :i64) (xb :i64) (ka :i32) (w :i64))
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
        (add-type *m* tname `(:func (:eqref) ((:ref ,type))))
        ;; the block type of the inline cast (wasm.lisp)
        (add-type *m* (list :cast-bt type) `(:func (:eqref) ((:ref ,type)))))
      (add-func *m* fname tname
                :body `((:block fail :eqref
                          (:local.get 0) (:br_on_cast_fail fail :eqref (:ref ,type)) :return)
                        :drop
                        (:local.get 0) ,@(const-init-code lisp-type)
                        (:global.get ,(sym-global 'clysm-sys::%type-error)) (:call (:callsym 2))
                        (:ref.cast ,type)))
      (push (cons type fname) (wasm-module-cast-rewrites *m*)))))
